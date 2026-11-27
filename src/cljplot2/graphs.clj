(ns cljplot2.graphs
  "Plot builders of graphs and trees: a layout from [[cljplot2.data.graph]] or [[cljplot2.data.tree]] drawn with ordinary charts and returned as a plot map.

  The returned map is data: edit it, add charts, or build the plot from the layout yourself. A builder's `:scales` option is merged into the builder's own scales key by key (see [[cljplot2.theme/deep-merge]]), so `{:color {:range ..}}` changes the palette and keeps the builder's domain and legend setting. Marks draw in named theme styles (`:theme-style`, see [[cljplot2.theme/named-styles]]) and labels in builder text roles, so a theme restyles every plot of a builder; the per-call style options win over them. Every builder validates its options first (its `*-options` schema) and throws `ex-info {:key k}` for an invalid value or an unknown key.

  Graphs: [[network]] (node-link), [[chord]] (ring sectors and ribbons), [[arc]] (nodes on a line), [[sankey]] (layered flows), [[alluvial]] (flows between categorical columns).

  Trees: [[dendrogram]] (elbow branches at merge heights), [[tree]] (node-link tree), [[clustered-heatmap]] (tiles ordered by clustering, with dendrograms), [[bundle]] (hierarchical edge bundling), [[sequences]] (flow sunburst).

  Option types: [[annotation-schema]] (clustered heatmap annotations). Option schemas: [[network-options]], [[chord-options]], [[arc-options]], [[sankey-options]], [[alluvial-options]], [[dendrogram-options]], [[tree-options]], [[clustered-heatmap-options]], [[bundle-options]], [[sequences-options]]."
  (:require [clojure2d.color :as c]
            [fastmath.core :as m]
            [fastmath.ml.clustering :as cl]
            [fastmath.vector :as v]
            [cljplot2.chart :as chart]
            [cljplot2.content.label :as label]
            [cljplot2.data.graph :as graph]
            [cljplot2.data.hierarchy :as hierarchy]
            [cljplot2.data.tree :as tree]
            [cljplot2.multiplot :as plot]
            [cljplot2.data.rows :as data]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme]
            [malli.core :as malli]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private node-size-range
  "Size scale attributes of a mapped network node `:size`: pixel range and area-proportional sizes."
  {:range [6 24] :area true})

(def ^{:private true :const true :tag 'double} frame-padding
  "Room around a network's unit square layout, a fraction of its side."
  0.06)

(def ^{:private true :const true :tag 'double} frame-padding+
  "Upper end of a network axis domain: 1 plus [[frame-padding]]."
  (m/inc frame-padding))

(def ^{:private true :const true :tag 'double} frame-padding3
  "Room right of a labelled network: three times [[frame-padding]]."
  (m/* 3.0 frame-padding))

(def ^{:private true :const true :tag 'double} -frame-padding
  "Lower end of a network axis domain: minus [[frame-padding]]."
  (m/- frame-padding))

(def ^{:private true :const true :tag 'double} min-arrow
  "Smallest arrow head of a directed network, in pixels."
  7.0)

(defn- keys-of
  "Keys of the map entries `entries`.

  Parameters:

  - `entries` (sequence): malli map entries `[k ...]`.

  Returns a vector of the entries' keys, in order."
  [entries]
  (mapv first entries))

(def ^:private graph-keys
  "Option keys forwarded to [[cljplot2.data.graph/graph]]."
  (keys-of graph/graph-entries))

(def ^:private node-map
  "Schema of a mapping or a chart options map passed on to a chart (validated by the chart, at build)."
  [:map-of keyword? any?])

(defn- node-radius-fn
  "Function giving the pixel radius of a network node (a shape `:size` is its bounding box side).

  Parameters:

  - `nodes` (sequence of maps): the laid-out nodes.
  - `mapping` (map): node aesthetics; `:size` - an accessor of the nodes.
  - `size-spec` (map): the plot's `:scales :size`; its domain comes from the nodes' values.
  - `merged-theme` (map): the merged theme, whose palette gives the size range.
  - `default-size` (double): the node size when `:size` isn't mapped (or a value has no size).

  Returns a function of a node returning half its size in pixels: from the node size scale when `mapping` maps `:size`, else from `default-size`."
  [nodes mapping size-spec [merged-theme ^double default-size]]
  (if-let [acc (:size mapping)]
    (let [size-of (data/row-accessor :size acc)
          spec (aesthetic/scale-spec :size size-spec (aesthetic/value-extent :size size-spec (keep size-of nodes)) false
                                     (:palette merged-theme))
          attribute (if spec (aesthetic/attribute-fn :size spec default-size) (constantly default-size))]
      (fn [node] (m/* 0.5 (double (or (attribute (size-of node)) default-size)))))
    (constantly (m/* 0.5 default-size))))

(defn- widest-edge
  "Width in pixels of the widest network edge.

  Parameters:

  - `edge-mapping` (map): edge aesthetics; a mapped `:size` sets the widths.
  - `size-spec` (map): the plot's `:scales :size`; its `:range` bounds the widths.
  - `edge-style` (map): the style the edges draw with (theme style and per-call options).
  - `merged-theme` (map): the merged theme, whose palette `:size` is the default range.

  Returns the top of the size range when `:size` is mapped (`size-spec` `:range`, else the palette's), else the `:line-width` of `edge-style`."
  ^double [edge-mapping size-spec edge-style merged-theme]
  (if (:size edge-mapping)
    (v/mx (or (:range size-spec) (get-in merged-theme [:palette :size])))
    (:line-width edge-style)))

(defn- check-mappings!
  "Check that no aesthetic is mapped for both nodes and edges (one scale can't show two variables).

  Parameters:

  - `mapping` (map): node aesthetics.
  - `edge-mapping` (map): edge aesthetics.

  Returns `nil`; throws `ex-info {:key :edge-mapping}` (`:value` - the aesthetics in both) otherwise."
  [mapping edge-mapping]
  (when-let [both (seq (filter #(and (contains? mapping %) (contains? edge-mapping %)) aesthetic/dims))]
    (schema/fail! "An aesthetic is mapped for both nodes and edges - one scale can't show two variables."
                  {:key :edge-mapping :value (vec both)})))

(defn- effective-style
  "Style a builder's chart draws with, for the sizes the builder computes itself (node radii, arrow heads).

  Parameters:

  - `merged-theme` (map): the merged theme.
  - `kind` (keyword): the chart kind (`:scatter`, `:segment` ...).
  - `style-name` (keyword): the builder's named theme style (`:network-node` ...).
  - `style` (map): the user's per-call style options, or `nil`.

  Returns the named style over the kind's (see [[cljplot2.theme/chart-style]]) with `style` merged over both."
  [merged-theme kind style-name style]
  (merge (theme/chart-style merged-theme kind {:theme-style style-name}) style))

(def network-options
  "Options schema of [[network]]: its own entries, those of [[cljplot2.data.graph/graph]] and [[cljplot2.data.graph/layout]] (forwarded) and the builder's."
  (schema/options-map
   [(schema/nilable :mapping {:description "Maps node aesthetics (`:color`, `:size`, `:shape`) to accessors of the nodes: their rows' columns, `:degree`, `:in` or `:out`."
                             :cljplot2/default-doc "No node aesthetics." :cljplot2/example {:size :degree}}
                    node-map)
    (schema/nilable :edge-mapping {:description "Maps edge aesthetics (`:color`, and `:size` for the width) to accessors of the edges: their rows' columns or `:value`; an aesthetic can't be mapped for nodes too."
                                  :cljplot2/default-doc "No edge aesthetics." :cljplot2/example {:color :value}}
                    node-map)
    (schema/nilable :node-style {:description "Scatter options of the nodes, merged over the theme style `[:chart :network-node]`."
                                 :cljplot2/default-doc "The theme style `[:chart :network-node]`." :cljplot2/example {:size 16}}
                    node-map)
    (schema/nilable :edge-style {:description "Segment options of the edges, merged over the theme style `[:chart :network-edge]`; a mapped colour wins, and `:arrow` styles a directed graph's arrows."
                                 :cljplot2/default-doc "The theme style `[:chart :network-edge]`." :cljplot2/example {:line-width 4}}
                    node-map)
    (schema/nilable :labels {:description "Writes a label right of each node."
                             :default false
                             :cljplot2/value-meanings {true "The node's id." false "No labels."}
                             :cljplot2/form-meanings {::schema/accessor "An accessor of the node, e.g. `:name`; a `nil` or `\"\"` result writes no label."}}
                    [:or schema/bool schema/accessor])]
   graph/graph-entries
   graph/layout-entries
   plot/builder-entries))

(defn network
  "Builds a network plot of edge rows as a plot map: edges as segments under the nodes (scatter), labels beside them, equal units on both axes (`:aspect :iso`) and no axes.

  Directed graphs get arrows ending at the target node's edge (a per-edge `:end-gap` of its radius, from the node size scale - its spec with the nodes' values as the domain); the head is at least 7 px and 3 times the widest edge (`:edge-style {:arrow ..}` wins). Opposite edges of a pair lie on one line. Node radii, arrow gaps and heads are computed when the builder runs, from the theme given as `:theme` (and the style options): a theme set on the returned plot later restyles the marks but leaves the arrow ends where they were.

  Parameters:

  - `edges` (sequence of maps): edge rows, see [[cljplot2.data.graph/graph]].
  - `options` (map, optional), validated first against [[network-options]]:
    - graph - `:from`, `:to`, `:value`, `:nodes`, `:id`, `:directed` (see [[cljplot2.data.graph/graph]]);
    - layout - `:layout` (`:force` default, `:circle`, `:random`), `:iterations`, `:gravity`, `:rng` (see [[cljplot2.data.graph/layout]]); without `:rng` every call lays the graph out anew (the returned plot map keeps its layout);
    - `:mapping` - node aesthetics (`:color`, `:size`, `:shape`; nodes have `:degree`, `:in`, `:out` and their row's columns); a mapped node `:size` gets the size scale `{:range [6 24] :area true}` unless `:scales :size` sets them;
    - `:edge-mapping` - edge aesthetics (`:color`, `:size` - the width; edges have `:value` and their row's columns);
    - `:node-style` - scatter options of the nodes over the theme style `[:chart :network-node]` (larger than plain points, outlined, opaque);
    - `:edge-style` - segment options of the edges over the theme style `[:chart :network-edge]` (a translucent grey; a mapped colour wins); `:arrow` - the arrows of a directed graph;
    - `:labels` - `true` (the node id) or an accessor of the node (`nil` or an empty string - no label), in the text role `:network-label` (white boxes) right of the nodes, offset by the largest node radius, a label overlapping that of a larger node left out; default `false`;
    - `:scales` (e.g. `:color`, `:size`), `:title`, `:theme`.

  Returns a plot map.

  Throws `ex-info {:key :edge-mapping}` for an aesthetic mapped for both nodes and edges, `ex-info {:key k}` for an invalid option.

  Equivalents: igraph `plot(graph, layout = layout_with_fr)`, ggraph `geom_edge_link` + `geom_node_point`. A correlation network: the pairs of a correlation matrix above a threshold, e.g. `(graph/adjacency->edges (mapv (fn [row] (mapv #(if (> (abs %) 0.7) % 0) row)) r) names {:whole-matrix false})` with `|r|` as the `:value` (weights are non-negative) and `:edge-mapping {:color ..}` by the sign (the diagonal's self-loops aren't drawn).

  See also [[arc]], [[chord]], [[cljplot2.data.graph/layout]]."
  {:cljplot2/options network-options}
  ([edges] (network edges {}))
  ([edges {:keys [mapping edge-mapping labels title node-style edge-style] directed? :directed user-scales :scales user-theme :theme :as options}]
   (schema/check-options! network-options options)
   (check-mappings! mapping edge-mapping)
   (let [{:keys [nodes] :as laid} (graph/layout (graph/graph edges (select-keys options graph-keys))
                                                (select-keys options (keys-of graph/layout-entries)))
         merged-theme (theme/deep-merge theme/default-theme user-theme)
         node-st (effective-style merged-theme :scatter :network-node node-style)
         scales (cond-> user-scales (:size mapping) (update :size #(merge node-size-range %)))
         radius (node-radius-fn nodes mapping (:size scales) [merged-theme (:size node-st)])
         radius-of (zipmap (map :id nodes) (map radius nodes))
         max-radius (double (reduce m/max 0.0 (vals radius-of)))
         edge-st (effective-style merged-theme :segment :network-edge edge-style)
         arrow (merge {:size (m/max min-arrow (m/* 3.0 (widest-edge edge-mapping (:size scales) edge-st merged-theme)))}
                      (:arrow edge-style))
         edge-rows (cond->> (:edges laid) directed? (mapv #(assoc % ::gap (m/inc (double (radius-of (:to %)))))))
         label-of (cond (true? labels) :id labels (data/row-accessor :label labels))
         ;; labels right of the nodes: more room on the right
         x-axis {:domain [-frame-padding (m/inc (if label-of frame-padding3 frame-padding))]
                 :nice false :expand [0.0 0.0] :axis false}
         y-axis {:domain [-frame-padding frame-padding+] :nice false :expand [0.0 0.0] :axis false}]
     (cond-> {:charts (cond-> [(chart/segment (cond-> (merge {:theme-style :network-edge}
                                                             edge-style
                                                             {:data edge-rows
                                                              :mapping (cond-> (merge {:x :x :y :y :xend :xend :yend :yend} edge-mapping)
                                                                         directed? (assoc :end-gap ::gap))})
                                                directed? (assoc :arrow arrow)))
                               (chart/scatter (merge {:theme-style :network-node} node-style
                                                     {:data nodes :mapping (merge {:x :x :y :y} mapping)}))]
                        label-of (conj (chart/text {:role :network-label
                                                    ;; larger nodes first: their labels win collisions
                                                    :data (vec (sort-by #(m/- (double (radius-of (:id %)))) nodes))
                                                    :mapping {:x :x :y :y :label label-of}
                                                    :anchor [:left :middle]
                                                    :offset [(m/+ max-radius 3.0) 0]
                                                    :overlapping-labels :drop})))
              :aspect :iso
              :scales (theme/deep-merge {:x x-axis :y y-axis} scales)
              :labels {:title title :x :none :y :none}}
       user-theme (assoc :theme user-theme)))))

;; trees

(defn- as-tree
  "Gives a tree of [[cljplot2.data.tree]] from a clustering or a tree.

  Parameters:

  - `source` (map): a fastmath `hclust` result (it has `:tree`) or a tree.
  - `leaf-names` (sequence): names of the `hclust` observations, in order (`nil` - their indices).

  Returns the tree of the clustering (see [[cljplot2.data.tree/from-hclust]]), or `source` as it is. Throws `ex-info {:key :leaf-names}` when `leaf-names` doesn't name every observation."
  [source leaf-names]
  (if (contains? source :tree) (tree/from-hclust source leaf-names) source))

(defn- leaf-axis
  "Position scale of a leaf axis: leaves at 0..n-1 with half a step around.

  Parameters:

  - `leaves` (sequence of maps): the leaves in axis order, each with a `:name`.
  - `labels?` (boolean): ticks at the leaves labelled by their names, without grid lines, every name drawn even when crowded (`:overlapping-labels :keep`); `false` - no axis.

  Returns the scale spec map."
  [leaves labels?]
  (let [n (count leaves)
        names (mapv (comp str :name) leaves)]
    (cond-> {:domain [-0.5 (m/- n 0.5)] :nice false :expand [0.0 0.0]}
      labels? (assoc :ticks (vec (range n)) :formatter (fn [^double v] (get names (m/round v) "")) :axis :no-grid
                     :overlapping-labels :keep)
      (not labels?) (assoc :axis false))))

(defn- branch-chart
  "Line chart of a dendrogram's elbow paths in the theme style `[:chart :dendrogram-branch]`.

  Parameters:

  - `paths` (sequence of maps): path rows `{:id :x :y :cluster}`, see [[cljplot2.data.tree/layout]].
  - `clustered?` (boolean): branches coloured by cluster (branches above the cut in the NA colour).
  - `line-style` (map): the user's line options over the theme style, or `nil`.

  Returns a line chart value, a path per branch."
  [paths clustered? line-style]
  (chart/line (merge {:theme-style :dendrogram-branch}
                     line-style
                     {:data paths :order :data
                      ;; cluster numbers are levels, not quantities
                      :mapping (cond-> {:x :x :y :y :group :id} clustered? (assoc :color #(some-> (:cluster %) str)))})))

(defn- radial-labels
  "Text charts of the leaf names around a circle, each turned along its angle and reading outward.

  Labels sit just outside each leaf's radius: the right half anchored left, the left half anchored right; they draw in the text role `:radial-label`, the font smaller for many leaves (down to the role's `:min-size`, never above its size; no `:min-size` - the role's size); pixel room for the labels is kept around the circle (`:reserve-room`).

  Parameters:

  - `nodes` (sequence of maps): laid-out nodes with `:angle` (degrees), `:leaf`, `:name` and an optional `:radius` (default 1).
  - `user-theme` (map or `nil`): the builder's `:theme` option, merged over the default theme for the role's size.

  Returns a vector of up to two text chart values (empty without leaves).

  See also [[circle-axis]]."
  [nodes user-theme]
  (let [leaves (filterv :leaf nodes)
        {role-size :size min-size :min-size} (get-in (theme/deep-merge theme/default-theme user-theme) [:text :radial-label])
        size (m/min (double role-size) (m/max (double (or min-size role-size)) (m// 1400.0 (m/max 1 (count leaves)))))
        row (fn [{leaf-name :name :keys [^double angle ^double radius] :or {radius 1.0}}]
              (let [a (m/radians angle)
                    r (m/+ radius 0.03)]
                {:x (m/* r (m/sin a))
                 :y (m/* r (m/cos a))
                 :label (str leaf-name)
                 :angle (if (m/<= angle 180.0) (m/- angle 90.0) (m/- angle 270.0))}))
        [right left] ((juxt filter remove) #(m/<= (double (:angle %)) 180.0) leaves)]
    (cond-> []
      (seq right) (conj (chart/text {:data (mapv row right) :mapping {:x :x :y :y :label :label :angle :angle}
                                     :anchor [:left :middle] :anchor-frame :text :role :radial-label :size size :reserve-room true}))
      (seq left) (conj (chart/text {:data (mapv row left) :mapping {:x :x :y :y :label :label :angle :angle}
                                    :anchor [:right :middle] :anchor-frame :text :role :radial-label :size size :reserve-room true})))))

(def ^{:private true :const true :tag 'double} root-room
  "Room past the root on a dendrogram's height axis (a fraction of the root height): its cross line stays inside the panel instead of on (and clipped at) its border."
  0.03)

(def ^{:private true :const true :tag 'double} root-room+
  "Factor of the root height giving the top of a dendrogram's height axis: 1 plus [[root-room]]."
  (m/inc root-room))

(defn- height-axis
  "Height scale of a dendrogram, from 0 exactly (leaves on the axis) to the root plus [[root-room]], without nice extension or chart padding.

  Parameters:

  - `top` (double): the root height; `0` (a single leaf) - the axis reaches 1.

  Returns the scale spec map."
  [^double top]
  {:domain [0.0 (m/* root-room+ (if (m/pos? top) top 1.0))] :nice false :expand [0.0 0.0]})

(def ^:private circle-axis
  "Both axes of a circular plot: the unit circle and the radial labels' start (radius 1.03), hidden; the labels keep their own pixel room past it (text `:reserve-room`, see [[radial-labels]]), whatever their length and the panel size."
  {:domain [-1.05 1.05] :nice false :expand [0.0 0.0] :axis false})

(def dendrogram-options
  "Options schema of [[dendrogram]]: its own entries, those of [[cljplot2.data.tree/layout]] (forwarded) and the builder's."
  (schema/options-map
   [(schema/nilable :leaf-names {:description "The names of an `hclust` result's observations, in order, one per observation."
                             :cljplot2/default-doc "The observations' indices." :cljplot2/example ["a" "b" "c"]}
                    [:sequential any?])
    (schema/nilable :horizontal {:description "Whether the leaves sit on the left with heights growing right; a circular dendrogram ignores it."
                                 :default false :cljplot2/value-meanings {true "Leaves on the left." false "Leaves at the bottom, heights growing up."}}
                    schema/bool)
    (schema/optional :labels {:description "Writes the leaf names along the leaf axis, outward around a circular dendrogram."
                              :default true :cljplot2/value-meanings {true "Writes the names." false "No names."}}
                     schema/bool)
    (schema/nilable :group-labels {:description "Writes the names of named groups at their nodes. Only trees built from levels or parents name their groups; an `hclust` result's groups have no names."
                                   :default false :cljplot2/value-meanings {true "Writes the names." false "No names."}}
                    schema/bool)
    (schema/nilable :line-style {:description "Line options of the branches, merged over the theme style `[:chart :dendrogram-branch]`."
                                 :cljplot2/default-doc "The theme style `[:chart :dendrogram-branch]`." :cljplot2/example {:line-width 4}}
                    node-map)]
   tree/layout-entries
   plot/builder-entries))

(defn dendrogram
  "Builds a dendrogram plot of a clustering or a tree as a plot map: branches as elbow paths (`chart/line`), leaves at 0..n-1 along the leaf axis labelled by their names.

  The height axis is shown for `hclust` trees (merge heights), hidden otherwise.

  Parameters:

  - `source` (map): a fastmath `hclust` result or a tree of [[cljplot2.data.tree]].
  - `options` (map, optional), validated first against [[dendrogram-options]]:
    - `:leaf-names` - names of an `hclust` result's observations (default their indices);
    - layout - `:k` (cluster colours; branches above the cut in the NA colour, no legend), `:circular` (radial, labels outward, equal axes), `:span` (its degrees), see [[cljplot2.data.tree/layout]];
    - `:horizontal` - leaves on the left, heights growing right (not circular), default `false`;
    - `:labels` - leaf names, default `true`;
    - `:group-labels` - names of the named groups at their nodes (level and parent trees) in the text role `:dendrogram-group-label`, default `false`;
    - `:line-style` - line options of the branches over the theme style `[:chart :dendrogram-branch]`;
    - `:scales`, `:title`, `:theme`.

  Returns a plot map.

  Throws `ex-info {:key k}` for an invalid option.

  Equivalents: R `plot(hclust)`, `dendextend` `color_branches(k)`, ggraph `geom_edge_elbow` (`circular = TRUE`).

  See also [[tree]], [[clustered-heatmap]]."
  {:cljplot2/options dendrogram-options}
  ([source] (dendrogram source {}))
  ([source {:keys [leaf-names k circular horizontal scales title line-style group-labels] user-theme :theme :as options}]
   (schema/check-options! dendrogram-options options)
   (let [labels? (data/option options :labels true)
         t (as-tree source leaf-names)
         {:keys [nodes paths]} (tree/layout t (select-keys options (keys-of tree/layout-entries)))
         leaves (tree/leaves t)
         heights? (contains? source :tree)
         top (double (:height t))]
     (cond-> {:charts (cond-> [(branch-chart paths (some? k) line-style)]
                        (and circular labels?) (into (radial-labels nodes user-theme))
                        group-labels (conj (chart/text {:data (filterv #(and (not (:leaf %)) (some? (:name %))) nodes)
                                                         :mapping {:x :x :y :y :label #(str (:name %))}
                                                         :anchor [:left :bottom] :offset [3 3]
                                                         :role :dendrogram-group-label})))
              :scales (theme/deep-merge (cond-> (if circular
                                                  {:x circle-axis :y circle-axis}
                                                  {:x (leaf-axis leaves labels?)
                                                   :y (cond-> (height-axis top)
                                                        (not heights?) (assoc :axis false))})
                                          k (assoc :color {:legend false}))
                                        scales)
              :labels {:title title :x :none :y (if (and heights? (not circular)) "height" :none)}}
       circular (assoc :aspect :iso)
       (and horizontal (not circular)) (assoc :flip true)
       (and labels? (not circular) (not horizontal)) (assoc-in [:theme :axis :x :label-rotation] -90)
       user-theme (update :theme #(theme/deep-merge % user-theme))))))

;; node-link tree

(defn- tree-labels
  "Text charts of the node names of a cartesian node-link tree (root on the left) in the text role `:tree-label`.

  Leaves are labelled right of their node, groups left of theirs (Vega `tree`); unnamed nodes get no label; room is kept only where the names reach (`:reserve-room :anchor`).

  Parameters:

  - `nodes` (sequence of maps): laid-out nodes with `:x`, `:y`, `:leaf` and an optional `:name`.

  Returns a vector of up to two text chart values."
  [nodes]
  (let [named (filterv #(some? (:name %)) nodes)
        [leaves groups] ((juxt filter remove) :leaf named)
        text (fn [rows anchor dx]
               (chart/text {:data rows :mapping {:x :x :y :y :label #(str (:name %))} :anchor anchor :offset [dx 0]
                            :role :tree-label :reserve-room :anchor}))]
    (cond-> []
      (seq leaves) (conj (text leaves [:left :middle] 6))
      (seq groups) (conj (text groups [:right :middle] -6)))))

(def tree-options
  "Options schema of [[tree]]: its own entries, those of [[cljplot2.data.tree/node-link-layout]] (forwarded) and the builder's."
  (schema/options-map
   [(schema/optional :labels {:description "Writes the node names. A cartesian tree writes leaf names right of their nodes and group names left of them; a circular tree writes leaf names outward."
                               :default true :cljplot2/value-meanings {true "Writes the names." false "No names."}}
                     schema/bool)
    (schema/optional :node-points {:description "Whether the plot draws a point per node." :default true
                                   :cljplot2/value-meanings {true "Draws a point per node." false "Draws no points."}}
                     schema/bool)
    (schema/nilable :mapping {:description "Maps node aesthetics (`:color`, `:size`, `:shape`) to accessors of the nodes: their rows' columns, `:depth`, `:leaf` or `:name`."
                             :cljplot2/default-doc "No node aesthetics." :cljplot2/example {:color :depth}}
                    node-map)
    (schema/nilable :node-style {:description "Scatter options of the nodes, merged over the theme style `[:chart :tree-node]`."
                                 :cljplot2/default-doc "The theme style `[:chart :tree-node]`." :cljplot2/example {:size 14}}
                    node-map)
    (schema/nilable :link-style {:description "Line options of the links, merged over the theme style `[:chart :tree-link]`."
                                 :cljplot2/default-doc "The theme style `[:chart :tree-link]`." :cljplot2/example {:line-width 4}}
                    node-map)
    (schema/nilable :leaf-names {:description "The names of an `hclust` result's observations, in order, one per observation."
                             :cljplot2/default-doc "The observations' indices." :cljplot2/example ["a" "b" "c"]}
                    [:sequential any?])]
   tree/node-link-layout-entries
   plot/builder-entries))

(defn tree
  "Builds a node-link tree plot of a tree or a clustering as a plot map: links as paths (`chart/line`) under a point per node (scatter), node names beside them, no axes and no legend.

  Unlike [[dendrogram]] (heights, elbows), depth places the nodes. Cartesian: the root on the left, depth growing right, the first child on top.

  Parameters:

  - `source` (map): a tree of [[cljplot2.data.tree]] (e.g. `from-parents`, `from-levels`) or a fastmath `hclust` result.
  - `options` (map, optional), validated first against [[tree-options]]:
    - layout - `:layout` (`:tidy` default, `:cluster`), `:links` (`:diagonal` default, `:line`), `:circular` (radial, equal axes; leaf names outward, group names left out), `:span` (its degrees), see [[cljplot2.data.tree/node-link-layout]];
    - `:labels` - node names in the text role `:tree-label` (cartesian - leaves right of their node, groups left), default `true`;
    - `:node-points` - node points, default `true`;
    - `:mapping` - node aesthetics (`:color`, `:size`, `:shape`; nodes have `:depth`, `:leaf`, `:name` and their row's columns - e.g. `{:color #(str (:depth %))}`);
    - `:node-style` - scatter options over the theme style `[:chart :tree-node]`;
    - `:link-style` - line options over the theme style `[:chart :tree-link]` (a mid grey, so the many leaf links don't merge into a block);
    - `:leaf-names` - names of an `hclust` result's observations;
    - `:scales`, `:title`, `:theme`.

  Returns a plot map.

  Throws `ex-info {:key k}` for an invalid option.

  Equivalents: Vega `tree-layout` / `radial-tree-layout`, d3 `tree` / `cluster` with `linkHorizontal` / `linkRadial`, ggraph `layout = 'tree'` + `geom_edge_diagonal`."
  {:cljplot2/options tree-options}
  ([source] (tree source {}))
  ([source {:keys [circular mapping node-style link-style leaf-names scales title] user-theme :theme :as options}]
   (schema/check-options! tree-options options)
   (let [labels? (data/option options :labels true)
         nodes? (data/option options :node-points true)
         t (as-tree source leaf-names)
         {:keys [nodes] laid-links :links} (tree/node-link-layout t (select-keys options (keys-of tree/node-link-layout-entries)))
         max-depth (double (reduce m/max 0.0 (map :x nodes)))
         min-y (double (reduce m/min 0.0 (map :y nodes)))]
     (cond-> {:charts (cond-> [(chart/line (merge {:theme-style :tree-link} link-style
                                                  {:data laid-links :order :data :mapping {:x :x :y :y :group :id}}))]
                        nodes? (conj (chart/scatter (merge {:theme-style :tree-node} node-style
                                                           {:data nodes :mapping (merge {:x :x :y :y} mapping)})))
                        (and labels? circular) (into (radial-labels nodes user-theme))
                        (and labels? (not circular)) (into (tree-labels nodes)))
              :scales (theme/deep-merge
                       (if circular
                         {:x circle-axis :y circle-axis}
                         {:x {:domain [0.0 (m/max max-depth 1.0)] :nice false :axis false}
                          :y {:domain [(m/- min-y 0.5) 0.5] :nice false :expand [0.0 0.0] :axis false}})
                       scales)
              :theme {:panel {:grid {:x :none :y :none}}}
              :legend false
              :labels {:title title :x :none :y :none}}
       circular (assoc :aspect :iso)
       user-theme (update :theme #(theme/deep-merge % user-theme))))))

(defn- value-matrix
  "Row levels, column levels and the value matrix of long rows of a clustered heatmap.

  Parameters:

  - `rows` (sequence of maps): long rows, one per cell.
  - `options` (map): `:row`, `:col`, `:value` - accessors of a row's tile row, column and value.

  Returns `[row-levels col-levels matrix]`: the levels in first-appearance order, `matrix` a vector per row level of the doubles per column level.

  Throws `ex-info {:key :rows}` for fewer than 2 rows or columns, `ex-info {:key :value}` (`:value` - `[row col]`) for a missing or non-finite cell."
  [rows {:keys [row col value]}]
  (let [[row-of col-of value-of] (map #(data/row-accessor %1 %2) [:row :col :value] [row col value])
        rs (vec (distinct (map row-of rows)))
        cs (vec (distinct (map col-of rows)))
        cell (into {} (map (fn [r] [[(row-of r) (col-of r)] (value-of r)])) rows)]
    (when (or (m/< (count rs) 2) (m/< (count cs) 2))
      (schema/fail! "A clustered heatmap needs at least 2 rows and 2 columns." {:key :rows}))
    [rs cs (mapv (fn [r] (mapv (fn [c] (let [v (cell [r c])]
                                       (if (schema/finite? v)
                                         (double v)
                                         (schema/fail! "Every row x column cell needs a finite value." {:key :value :value [r c]}))))
                              cs))
                 rs)]))

(defn- transpose
  "Transpose of `matrix`.

  Parameters:

  - `matrix` (vector of vectors): rows of equal length.

  Returns a vector of the columns of `matrix`, each a vector."
  [matrix]
  (apply mapv vector matrix))

(def ^:private annotation-shape
  "Schema of one clustered heatmap annotation: an accessor and a closed colour scale spec (or `nil`)."
  [:tuple schema/accessor [:maybe (aesthetic/scale-spec-schema :color)]])

(defn- annotation-problem
  "Gives the error message of a value that isn't an annotation.

  Parameters:

  - `v`: the invalid value.

  Returns `should be an annotation [accessor color-scale-spec]`, followed by the first problem inside the pair (its place and its message); a value that isn't a pair at all gets the first part only."
  [v]
  (let [{:keys [in message]} (first (schema/errors annotation-shape v []))]
    (str "should be an annotation [accessor color-scale-spec]" (when (and message (seq in)) (str ", at " (pr-str (vec in)) ": " message)))))

(def annotation-schema
  "Schema of one clustered heatmap annotation `[accessor colour-scale-spec]`, a named type reporting the first problem inside the pair."
  (schema/named-type ::annotation (malli/validator annotation-shape) annotation-problem
                     {:cljplot2/type-name "annotation"
                      :cljplot2/type-doc "[accessor colour-scale-spec]"
                      :cljplot2/type-meaning "A pair `[accessor colour-scale-spec]` that adds a strip beside a clustered heatmap: the accessor returns each row's value and the spec sets the strip's colour scale (`nil` for the default), e.g. `[:group {:range :set2}]`."}))

(def ^:private annotations-schema
  "Schema of clustered heatmap annotations: a sequence of [[annotation-schema]]."
  [:sequential annotation-schema])

(def ^:private heatmap-example-keys
  "The required options of every clustered heatmap option example: the tile row, column and value of the example scores."
  {:row :car :col :variable :value :score})

(def clustered-heatmap-options
  "Options schema of [[clustered-heatmap]]: its own entries and the builder's."
  (schema/options-map
   (map (fn [[k props s]] [k (update props :cljplot2/example-context #(merge heatmap-example-keys %)) s])
        [[:row {:description "The accessor that returns each data row's tile row, e.g. `:car`." :cljplot2/example :car} schema/accessor]
         [:col {:description "The accessor that returns each data row's tile column, e.g. `:variable`." :cljplot2/example :variable} schema/accessor]
         [:value {:description "The accessor that returns each data row's tile value, a finite number." :cljplot2/example :score} schema/accessor]
         (schema/optional :method {:description "The fastmath `hclust` linkage of the rows and columns: how it measures the distance between two clusters from the distances between their members."
                                   :default :complete
                                   :cljplot2/value-meanings {:single "The nearest pair of members." :complete "The farthest pair of members."
                                                             :average "The mean distance between members (UPGMA)." :centroid "The distance between the clusters' centroids."
                                                             :ward "Ward's minimum variance on squared distances (R `ward.D2`)." :diana "Divisive clustering: splits from one cluster down."}
                                   :cljplot2/other-values "Other fastmath linkages: the mean of the two merged clusters' distances (WPGMA), the distance between weighted centroids (WPGMC), and Ward's update on unsquared distances (R `ward.D`)."}
                          [:enum :single :complete :average :weighted :centroid :median :ward :ward-d :diana])
         (schema/optional :distance {:description "The fastmath `hclust` distance between two rows or two columns." :default :euclidean
                                     :cljplot2/value-meanings {:euclidean "The straight-line distance." :chebyshev "The largest difference in any position."
                                                               :manhattan "The sum of the absolute differences." :canberra "The sum of absolute differences, each over the sum of the two values' absolute values."
                                                               :earth-movers "The earth mover's distance between the two profiles."}
                                     :cljplot2/form-meanings {::schema/function "A function of two double arrays that returns their distance."}}
                          [:or [:enum :euclidean :chebyshev :manhattan :canberra :earth-movers] schema/function])
         (schema/optional :row-tree {:description "Whether the plot draws the rows' dendrogram right of the tiles." :default true
                                     :cljplot2/value-meanings {true "Draws the dendrogram." false "Only orders the rows."}}
                          schema/bool)
         (schema/optional :col-tree {:description "Whether the plot draws the columns' dendrogram above the tiles." :default true
                                     :cljplot2/value-meanings {true "Draws the dendrogram." false "Only orders the columns."}}
                          schema/bool)
         (schema/optional :tree-size {:description "The fractions `[w h]` of the plot the dendrograms take: `w` of the width for the rows' tree, `h` of the height for the columns' tree."
                                      :default [0.2 0.2] :cljplot2/example [0.4 0.4]}
                          [:tuple schema/open-fraction schema/open-fraction])
         (schema/nilable :row-annotations {:description "Adds a coloured strip per annotation, in order, between the tiles and the rows' tree. Each strip is coloured by its values and gets a legend."
                                           :cljplot2/default-doc "No strips." :cljplot2/example [[:origin nil]]}
                         annotations-schema)
         (schema/nilable :col-annotations {:description "Adds a coloured strip per annotation, in order, between the columns' tree and the tiles. Each strip is coloured by its values and gets a legend."
                                           :cljplot2/default-doc "No strips." :cljplot2/example [[:unit nil]]}
                         annotations-schema)
         (schema/nilable :annotation-size {:description "The thickness of an annotation strip in pixels." :default 16
                                           :cljplot2/example 40 :cljplot2/example-context {:row-annotations [[:origin nil]]}}
                         schema/pos)
         (schema/nilable :legend-span {:description "How far down the plot the legend column reaches; with annotations it stacks the value legend over the annotations' legends."
                                       :cljplot2/default-doc "`:matrix` with annotations, else `:row`."
                                       :cljplot2/value-meanings {:row "One legend cell beside the tiles' row only." :matrix "One legend cell spanning every row, beside the trees and strips too."}
                                       :cljplot2/example :matrix}
                         [:enum :row :matrix])])
   plot/builder-entries))

(defn- level-annotations
  "Gives the annotation value of every row or column level of a clustered heatmap.

  Parameters:

  - `rows` (sequence of maps): the long rows.
  - `level-of` (fn): a row's level (its tile row or column).
  - `acc` (fn): the annotation's row accessor.
  - `option` (keyword): the option naming the annotations, for errors.

  Returns `{level value}` - the one value the level's rows give (`nil` a value too).

  Throws `ex-info {:key option :value [level values]}` when the rows of one level give different values."
  [rows level-of acc option]
  (into {} (map (fn [[level level-rows]]
                  (let [vs (distinct (map acc level-rows))]
                    (when (next vs)
                      (schema/fail! (str "Every row of one heatmap " (if (= option :row-annotations) "row" "column")
                                         " needs one annotation value, " level " has " (vec vs) ".")
                                    {:key option :value [level (vec vs)]}))
                    [level (first vs)])))
        (group-by level-of rows)))

(defn- annotation-level
  "Gives the tile row or column level of a row of a clustered heatmap annotation strip (its level-axis accessor - a function, so a sub-plot error names no internal key).

  Parameters:

  - `row` (map): an annotation row.

  Returns the level."
  [row]
  (::level row))

(defn- annotation-plots
  "Gives the strip and legend plots of the annotations of one side of a clustered heatmap.

  Each annotation is a one-band tile strip along the tiles' rows (`:row-annotations`) or columns (`:col-annotations`), coloured by the level's value with the annotation's own colour scale (categorical or continuous by the values, as any colour mapping); its band label (the scale's string `:title`, else the accessor's name, else `annotation i`) names it beside the strip. Strips have no plot margin (also under a user `[:plot :margin]`): a strip is as thick as its cell.

  Parameters:

  - `rows` (sequence of maps): the long rows.
  - `annotations` (sequence): `[accessor colour-scale-spec]` pairs (spec `nil` - the default scale), in order.
  - `side` (keyword): `:row` or `:col`.
  - `context` (map): `:level-of` (a row's level), `:order` (the levels in tile order), `:option` (the option, for errors), `:theme` (the user theme or `nil`).

  Returns a vector of `{:strip plot :legend plot}`, one per annotation."
  [rows annotations side {:keys [level-of order option] user-theme :theme}]
  (vec (map-indexed
        (fn [i [k spec]]
          (let [title (let [t (:title spec)] (cond (string? t) t (keyword? k) (name k) :else (str "annotation " (m/long-inc i))))
                values (level-annotations rows level-of (data/row-accessor option k) option)
                level-rows (mapv (fn [level] {::level level ::value (values level)}) order)
                [level-dim band-dim] (if (= side :row) [:y :x] [:x :y])
                base (cond-> {:data level-rows :mapping {level-dim annotation-level band-dim (constantly title) :color ::value}
                              :charts [(chart/tile)]
                              :scales {:color (merge {:title title} spec)}}
                       user-theme (assoc :theme user-theme))]
            ;; the name as an axis title (tick label role) beside the strip: a band tick label would overhang a thin strip
            {:strip (-> base
                        (assoc-in [:scales level-dim] {:domain order :axis false})
                        (assoc-in [:scales band-dim] {:axis false})
                        (assoc :legend false :labels {level-dim :none band-dim {:text title :role :tick-label :rotation (if (= side :row) 270 0)}})
                        ;; no plot margin, also under a user margin: the strip is as thick as its cell
                        (update :theme theme/deep-merge {:plot {:margin 0}}))
             :legend (assoc base :legend-only true)}))
        annotations)))

(defn clustered-heatmap
  "Builds a clustered heatmap of long rows as an arrange plot map: tiles of the values, rows and columns ordered by fastmath `hclust` of the value matrix, with their dendrograms.

  The dendrograms sit right of (rows) and above (columns) the tiles, each leaf at its tile's centre, their branches in the theme style `[:chart :dendrogram-branch]`; the colour legend is in a column of its own on the right.

  Parameters:

  - `rows` (sequence of maps): long rows, one per cell.
  - `options` (map), validated first against [[clustered-heatmap-options]]:
    - `:row`, `:col`, `:value` - accessors of a row's tile row, column and value (required);
    - `:method` - the `hclust` linkage, default `:complete`;
    - `:distance` - the `hclust` distance, a keyword or a function of two double arrays, default `:euclidean`;
    - `:row-tree`, `:col-tree` - the rows' and the columns' dendrogram (`false` - order only), default `true`;
    - `:tree-size` - `[w h]`, the dendrograms' share of the width and the height (fractions), default `[0.2 0.2]`;
    - `:row-annotations`, `:col-annotations` - `[[accessor colour-scale-spec] ...]` (a `nil` spec - the default scale; specs checked as plot colour scales), in order: per accessor of the long rows a strip along the tile rows (between the tiles and the rows' tree) or columns (between the columns' tree and the tiles), coloured by the values - categorical or continuous (a gradient), as any colour mapping -, named beside it (the spec's string `:title`, else the accessor's name, else `annotation i`), with its own colour scale and legend;
    - `:annotation-size` - the strips' thickness in pixels (not a fraction, unlike `:tree-size`), default 16;
    - `:legend-span` - the legend column's height: `:row` - one cell beside the tiles' row; `:matrix` - one cell spanning every row (`:row-spans`, beside the trees and the strips too); default `:matrix` with annotations, else `:row`. With annotations the legend cell stacks the value legend over the annotations' legends, each row as tall as its legend (arrange `:heights :measured`; scaled down when they don't fit, the legends then wrap into further columns and the column widens);
    - `:scales` - the tile plot's scales (e.g. `:color`), `:title`, `:theme`.

  Returns an arrange plot map (see [[cljplot2.multiplot/arrange]]).

  Throws `ex-info {:key :value}` for a missing or non-finite cell, `ex-info {:key :rows}` for fewer than 2 rows or columns, `ex-info {:key k}` for an invalid or missing option.

  Throws `ex-info {:key :row-annotations}` / `{:key :col-annotations}` (`:value` - `[level values]`) when the rows of one tile row / column give different annotation values.

  Equivalents: R `heatmap` (`RowSideColors`, `ColSideColors`), `pheatmap` (`annotation_row`, `annotation_col`; the row tree on the right).

  See also [[dendrogram]]."
  {:cljplot2/options clustered-heatmap-options}
  [rows {:keys [method distance scales title tree-size] row-tree? :row-tree col-tree? :col-tree user-theme :theme
         :or {method :complete distance :euclidean row-tree? true col-tree? true tree-size [0.2 0.2]} :as options}]
  (schema/check-options! clustered-heatmap-options options)
  (let [[^double tw ^double th] tree-size
        [rs cs matrix] (value-matrix rows options)
        opts {:method method :distance distance}
        row-tree (tree/from-hclust (cl/hclust matrix opts) rs)
        col-tree (tree/from-hclust (cl/hclust (transpose matrix) opts) cs)
        cells (vec (for [[i r] (map-indexed vector rs) [j c] (map-indexed vector cs)]
                     {:row r :col c :value ((matrix i) j)}))
        tile-plot (fn [extra] (cond-> (merge {:data cells :mapping {:x :col :y :row :color :value}
                                             :scales (theme/deep-merge {:x {:domain (mapv :name (tree/leaves col-tree))}
                                                                        :y {:domain (mapv :name (tree/leaves row-tree))}}
                                                                       scales)
                                             :charts [(chart/tile)]}
                                            extra)
                               user-theme (update :theme theme/deep-merge user-theme)))
        heatmap (tile-plot {:legend false :theme {:axis {:x {:label-rotation -90}}} :labels {:x :none :y :none}})
        legend (tile-plot {:legend-only true})
        tree-plot (fn [t flip?]
                    (let [{:keys [paths]} (tree/layout t {})
                          top (double (:height t))
                          n (count (tree/leaves t))]
                      (cond-> {:charts [(branch-chart paths false nil)]
                               :scales {:x {:domain [-0.5 (m/- n 0.5)] :nice false :expand [0.0 0.0] :axis false}
                                        :y (assoc (height-axis top) :axis false)}
                               :legend false :labels {:x :none :y :none}}
                        flip? (assoc :flip true)
                        user-theme (assoc :theme user-theme))))
        blank {:charts [] :scales {:x {:domain [0 1] :axis false} :y {:domain [0 1] :axis false}} :labels {:x :none :y :none}}
        empty-legend {:legend-only true}
        annotated (fn [side option level-of order]
                    (annotation-plots rows (get options option) side {:level-of (data/row-accessor side level-of) :order order
                                                                      :option option :theme user-theme}))
        row-annotations (annotated :row :row-annotations (:row options) (mapv :name (tree/leaves row-tree)))
        col-annotations (annotated :col :col-annotations (:col options) (mapv :name (tree/leaves col-tree)))
        annotations? (or (seq row-annotations) (seq col-annotations))
        legend-span (data/option options :legend-span (if annotations? :matrix :row))]
    (if (and (not annotations?) (= :row legend-span))
      (let [main-row (cond-> [heatmap] row-tree? (conj (tree-plot row-tree true)) true (conj legend))
            top-row (when col-tree? (cond-> [(tree-plot col-tree false)] row-tree? (conj blank) true (conj empty-legend)))
            columns (count main-row)]
        (plot/arrange (vec (concat top-row main-row))
                      (cond-> {:columns columns
                               :widths (cond-> [(m/- 1.0 (if row-tree? tw 0.0))] row-tree? (conj tw) true (conj :measured))}
                        col-tree? (assoc :heights [th (m/- 1.0 th)])
                        title (assoc :title title))))
      ;; annotation strips between the tiles and the trees; the legend cell beside the tiles' row or spanning every
      ;; row, with annotations a stack of the value legend over the annotations' legends, each row as tall as its legend
      (let [size [:fixed (data/option options :annotation-size 16)]
            ;; no plot margin: a blank fits a strip's thickness
            tight-blank (assoc blank :theme {:plot {:margin 0}})
            right-blanks (cond-> (vec (repeat (count row-annotations) tight-blank)) row-tree? (conj tight-blank))
            legends (into [legend] (map :legend) (concat col-annotations row-annotations))
            legend-cell (if annotations?
                          (plot/arrange legends {:columns 1 :heights (into [1] (repeat (dec (count legends)) :measured)) :theme {:plot {:margin 0}}})
                          legend)
            ;; with strips the tiles and trees have no plot margin either (aligned panels take the largest margin of
            ;; their row and column): tiles, strips and trees are evenly the arrange gap apart
            tight (fn [p] (cond-> p annotations? (update :theme theme/deep-merge {:plot {:margin 0}})))
            tree-rows (cond-> [] col-tree? (conj (into [(tight (tree-plot col-tree false))] right-blanks)))
            top-rows (into tree-rows (map #(into [(:strip %)] right-blanks)) col-annotations)
            main-row (cond-> (into [(tight heatmap)] (map :strip) row-annotations) row-tree? (conj (tight (tree-plot row-tree true))))
            columns (m/long-inc (count main-row))
            n-rows (m/long-inc (count top-rows))
            spanning? (and (= :matrix legend-span) (m/> n-rows 1))
            rows-of-cells (if spanning?
                            (into [(conj (first top-rows) legend-cell)] (conj (vec (rest top-rows)) main-row))
                            (conj (mapv #(conj % (assoc empty-legend :theme {:plot {:margin 0}})) top-rows) (conj main-row legend-cell)))]
        (plot/arrange (vec (apply concat rows-of-cells))
                      (cond-> {:columns columns
                               :widths (-> [(m/- 1.0 (if row-tree? tw 0.0))]
                                           (into (repeat (count row-annotations) size))
                                           (cond-> row-tree? (conj tw))
                                           (conj :measured))
                               :heights (-> (if col-tree? [th] [])
                                            (into (repeat (count col-annotations) size))
                                            (conj (if col-tree? (m/- 1.0 th) 1.0)))}
                        spanning? (assoc :row-spans {(m/long-dec columns) n-rows})
                        title (assoc :title title)))))))

(def ^:private bundle-edge-color
  "Default colour of bundled edges not coloured by group (`:edge-color`): the theme's default colour."
  theme/default-color)

(def bundle-options
  "Options schema of [[bundle]]: its own entries, those of [[cljplot2.data.tree/bundle-paths]] (forwarded) and the builder's."
  (schema/options-map
   [(schema/optional :alpha {:description "The opacity of the edges; leaf points and legend keys stay opaque." :default 0.4 :cljplot2/example 1.0}
                     schema/fraction)
    (schema/nilable :color-by {:description "How the plot colours the edges and leaves."
                               :default :group
                               :cljplot2/value-meanings {:group "Colours each edge by the top-level group of its source leaf and each leaf by its own group, with a legend."
                                                         :single "Colours every edge in `:edge-color` and the leaves in the theme colour, without a legend."}}
                     [:enum :group :single])
    (schema/optional :edge-color {:description "The colour of the edges with `:color-by :single`." :default bundle-edge-color
                                  :cljplot2/value-meanings {:none "No edges."} :cljplot2/example :darkorange :cljplot2/example-context {:color-by :single}}
                     schema/color)
    (schema/optional :labels {:description "Writes the leaf names outward around the circle." :default true
                              :cljplot2/value-meanings {true "Writes the names." false "No names."}}
                     schema/bool)]
   tree/bundle-paths-entries
   plot/builder-entries))

(defn bundle
  "Builds a hierarchical edge bundling plot of edges between the leaves of a tree as a plot map: leaves around a circle (the radial layout of the tree), each edge a B-spline along the tree path between its leaves.

  Edges are coloured by the top-level group of their source leaf, drawn in the theme style `[:chart :bundle-edge]`; leaves are points in `[:chart :bundle-leaf]`. Edges with an end that isn't a leaf are dropped (see [[cljplot2.data.tree/bundle-paths]] for the count).

  Parameters:

  - `edges` (sequence of maps): edge rows between leaves, each leaf named by its `:id` in a `from-parents` tree, else by its name.
  - `leaf-tree` (map): a tree of [[cljplot2.data.tree]].
  - `options` (map, optional), validated first against [[bundle-options]]:
    - `:from`, `:to` - accessors of the edge ends (default `:from`, `:to`); `:beta` - the bundling strength, straighter towards 0, default 0.85 (see [[cljplot2.data.tree/bundle-paths]]);
    - `:color-by` - `:group` (default): edges and leaves coloured by the top-level group of their (source) leaf, with a legend; `:single` - edges in `:edge-color`, no legend;
    - `:edge-color` - the edges' colour when not coloured by group, default the theme's default colour (see [[cljplot2.theme/default-color]]);
    - `:alpha` - the edges' opacity (leaf points and legend keys stay opaque), default 0.4;
    - `:labels` - leaf names outward around the circle, default `true`;
    - `:scales` (e.g. `{:color {:legend false}}`), `:title`, `:theme`.

  Returns a plot map.

  Throws `ex-info {:key :edges}` when every edge is dropped (nothing would be drawn), `ex-info {:key k}` for an invalid option.

  Equivalents: ggraph `geom_conn_bundle` (`tension`), d3 `curveBundle.beta`; Holten (2006)."
  {:cljplot2/options bundle-options}
  ([edges leaf-tree] (bundle edges leaf-tree {}))
  ([edges leaf-tree {:keys [from to ^double beta ^double alpha edge-color scales title] user-theme :theme
                :or {beta 0.85 alpha 0.4 edge-color bundle-edge-color} :as options}]
   (schema/check-options! bundle-options options)
   (let [labels? (data/option options :labels true)
         by-group? (= :group (data/option options :color-by :group))
         laid (tree/layout leaf-tree {:circular true})
         nodes (:nodes laid)
         top-group (fn [i] (let [chain (vec (take-while some? (iterate #(:parent (nodes %)) i)))]
                            (:name (nodes (nth chain (m/long-max 0 (m/long-sub (count chain) 2)))))))
         leaf-group (into {} (keep-indexed (fn [i n] (when (:leaf n) [(tree/leaf-key n) (str (top-group i))])) nodes))
         {bundled :paths} (tree/bundle-paths edges laid (cond-> {:beta beta} from (assoc :from from) to (assoc :to to)))
         _ (when (and (seq edges) (empty? bundled))
             (schema/fail! (str "No bundle edge joins two different leaves of the tree: edge ends should name leaves - "
                                "the :id of a from-parents node, else the leaf name.")
                           {:key :edges :value (count edges)}))
         from-of (data/row-accessor :from (or from :from))
         paths (mapv #(assoc % ::group (leaf-group (from-of %))) bundled)
         leaves (mapv #(assoc % ::group (leaf-group (tree/leaf-key %))) (filter :leaf nodes))]
     (cond-> {:charts (cond-> [(chart/line (cond-> {:theme-style :bundle-edge :data paths :order :data :stroke-alpha-factor alpha
                                                    :mapping (cond-> {:x :x :y :y :group :id} by-group? (assoc :color ::group))}
                                             (not by-group?) (assoc :color edge-color)))
                               (chart/scatter {:theme-style :bundle-leaf :data leaves :mapping (cond-> {:x :x :y :y} by-group? (assoc :color ::group))})]
                        labels? (into (radial-labels nodes user-theme)))
              :aspect :iso
              :scales (theme/deep-merge (cond-> {:x circle-axis :y circle-axis} by-group? (assoc :color {:title :none})) scales)
              :labels {:title title :x :none :y :none}}
       user-theme (update :theme #(theme/deep-merge % user-theme))))))

;; ordered layouts

(def chord-options
  "Options schema of [[chord]]: its own entries, those of [[cljplot2.data.graph/graph]] and [[cljplot2.data.graph/chord-layout]] (forwarded) and the builder's."
  (schema/options-map
   [(schema/optional :alpha {:description "The opacity of the ribbons." :default 0.6 :cljplot2/example 1.0} schema/fraction)
    (schema/optional :labels {:description "Writes each node's name outward from the middle of its sector; a node without flow gets none." :default true
                              :cljplot2/value-meanings {true "Writes the names." false "No names."}}
                     schema/bool)]
   graph/graph-entries
   graph/chord-layout-entries
   plot/builder-entries))

(defn chord
  "Builds a chord diagram of edge rows as a plot map: a ring sector per node coloured by node (`chart/polygon`), ribbons in their source node's colour, equal axes, no axes or legend.

  The layout is [[cljplot2.data.graph/chord-layout]] - d3 `chord` for undirected graphs, `chordDirected` with `:directed true`.

  Parameters:

  - `edges` (sequence of maps): edge rows, see [[cljplot2.data.graph/graph]].
  - `options` (map, optional), validated first against [[chord-options]]:
    - graph - `:from`, `:to`, `:value`, `:nodes`, `:id`, `:directed`;
    - layout - `:order` (`nil`, ids, `:cluster`; see [[cljplot2.data.graph/node-order]]), `:gap-angle` (degrees, default 2), `:inner-radius` (default 0.92), `:min-value` (weaker flows left out - summed per pair);
    - `:alpha` - the ribbons' opacity, default 0.6;
    - `:labels` - node names outward from the sectors' middles (nodes without flow unlabelled), default `true`;
    - `:scales` (e.g. `:color`), `:title`, `:theme`.

  Returns a plot map.

  Throws `ex-info {:key k}` for an invalid option.

  Equivalents: d3 `chord` / `chordDirected` + `ribbon`, R `circlize::chordDiagram` (without tracks or ticks).

  See also [[network]], [[arc]]."
  {:cljplot2/options chord-options}
  ([edges] (chord edges {}))
  ([edges {:keys [alpha scales title] user-theme :theme :or {alpha 0.6} :as options}]
   (schema/check-options! chord-options options)
   (let [labels? (data/option options :labels true)
         g (graph/graph edges (select-keys options graph-keys))
         {:keys [groups arcs ribbons]} (graph/chord-layout g (select-keys options (keys-of graph/chord-layout-entries)))
         label-nodes (vec (for [{:keys [id ^double start ^double end ^double value]} groups :when (m/pos? value)]
                            {:name id :leaf true :angle (m/* 0.5 (m/+ start end))}))]
     (cond-> {:charts (cond-> [(chart/polygon {:data ribbons :alpha alpha :mapping {:x :x :y :y :group :ribbon :color :from}})
                               (chart/polygon {:data arcs :mapping {:x :x :y :y :color :group}})]
                        labels? (into (radial-labels label-nodes user-theme)))
              :aspect :iso
              ;; colours in ring order, the same for sectors and ribbons
              :scales (theme/deep-merge {:x circle-axis :y circle-axis :color {:domain (mapv :id groups) :legend false}} scales)
              :labels {:title title :x :none :y :none}}
       user-theme (assoc :theme user-theme)))))

(def ^:private arc-width-range
  "Size scale range of mapped arc widths, in pixels."
  {:range [0.5 4]})

(def ^{:private true :const true :tag 'double} arc-room
  "Room below the node line and above the highest arc (a fraction of its height): nodes and the top arc stay inside the panel."
  0.04)

(def ^{:private true :const true :tag 'double} arc-room+
  "Factor of the highest arc giving the top of an arc diagram's y domain: 1 plus [[arc-room]]."
  (m/inc arc-room))

(def arc-options
  "Options schema of [[arc]]: its own entries, those of [[cljplot2.data.graph/graph]] and [[cljplot2.data.graph/arc-layout]] (forwarded) and the builder's."
  (schema/options-map
   [(schema/nilable :mapping {:description "Maps node aesthetics (`:color`, `:size`, `:shape`) to accessors of the nodes: their rows' columns, `:degree`, `:in` or `:out`."
                             :cljplot2/default-doc "No node aesthetics." :cljplot2/example {:size :degree}}
                    node-map)
    (schema/nilable :edge-mapping {:description "Maps edge aesthetics (`:color`, and `:size` for the width) to accessors of the edges: their rows' columns or `:value`; an aesthetic can't be mapped for nodes too."
                                   :cljplot2/example {:color :value}
                                   :cljplot2/default-doc "`{:size :value}` (widths from a size scale of range `[0.5 4]`) when the edge weights differ and `:mapping` has no `:size`."}
                    node-map)
    (schema/optional :alpha {:description "The opacity of the arcs." :default 0.5 :cljplot2/example 1.0} schema/fraction)
    (schema/nilable :node-style {:description "Scatter options of the nodes, merged over the theme style `[:chart :arc-node]`."
                                 :cljplot2/default-doc "The theme style `[:chart :arc-node]`." :cljplot2/example {:size 16}}
                    node-map)
    (schema/nilable :edge-style {:description "Line options of the arcs, merged over the theme style `[:chart :arc-edge]`."
                                 :cljplot2/default-doc "The theme style `[:chart :arc-edge]`." :cljplot2/example {:color :black}}
                    node-map)
    (schema/optional :labels {:description "Writes a label under the line for each node, turned to read downward."
                              :default true
                              :cljplot2/value-meanings {true "The node's id." false "No labels."}
                              :cljplot2/form-meanings {::schema/accessor "An accessor of the node, e.g. `:name`; a `nil` or `\"\"` result writes no label."}}
                     [:or :boolean schema/accessor])]
   graph/graph-entries
   graph/arc-layout-entries
   plot/builder-entries))

(defn arc
  "Builds an arc diagram of edge rows as a plot map: nodes on a line in node order (scatter), every edge a semicircle above it (a line chart path), node labels under the line, no y axis.

  The layout is [[cljplot2.data.graph/arc-layout]]. Node labels are x tick labels rotated to read downward; to read upward set `:theme {:axis {:x {:label-rotation -90}}}`.

  Parameters:

  - `edges` (sequence of maps): edge rows, see [[cljplot2.data.graph/graph]].
  - `options` (map, optional), validated first against [[arc-options]]:
    - graph - `:from`, `:to`, `:value`, `:nodes`, `:id`, `:directed`;
    - `:order` - node order (`nil`, ids, `:cluster`; see [[cljplot2.data.graph/node-order]]);
    - `:mapping` - node aesthetics (`:color`, `:size`, `:shape`; nodes have `:degree`, `:in`, `:out` and their row's columns);
    - `:edge-mapping` - edge aesthetics; default `{:size :value}` - widths from the size scale `{:range [0.5 4]}` - when the edge weights differ and the nodes don't map `:size`;
    - `:alpha` - the arcs' stroke opacity, default 0.5;
    - `:node-style` - scatter options over the theme style `[:chart :arc-node]`;
    - `:edge-style` - line options over the theme style `[:chart :arc-edge]`;
    - `:labels` - `true` (the node id, default), `false` (no labels) or an accessor of the node, e.g. `:name` - a column of `:nodes` (`nil` or an empty string - an empty label);
    - `:scales`, `:title`, `:theme`.

  Returns a plot map.

  Throws `ex-info {:key :edge-mapping}` for an aesthetic mapped for both nodes and edges, `ex-info {:key k}` for an invalid option.

  Equivalents: ggraph `layout = 'linear'` + `geom_edge_arc`, arcdiagram (R).

  See also [[network]], [[chord]]."
  {:cljplot2/options arc-options}
  ([edges] (arc edges {}))
  ([edges {:keys [order mapping alpha scales title node-style edge-style labels] user-edge-mapping :edge-mapping user-theme :theme
           :or {alpha 0.5 labels true} :as options}]
   (schema/check-options! arc-options options)
   (let [labels? (not (false? labels))
         label-of (if (boolean? labels) :id (data/row-accessor :labels labels))
         g (graph/graph edges (select-keys options graph-keys))
         weighted? (m/< 1 (count (distinct (map :value (:edges g)))))
         edge-mapping (if (contains? options :edge-mapping)
                        user-edge-mapping
                        (when (and weighted? (not (contains? mapping :size))) {:size :value}))
         _ (check-mappings! mapping edge-mapping)
         {:keys [nodes arcs]} (graph/arc-layout g {:order order})
         highest (double (reduce m/max 0.0 (map :y arcs)))
         top (if (m/pos? highest) highest 1.0)]
     (cond-> {:charts [(chart/line (merge {:theme-style :arc-edge}
                                          edge-style
                                          {:data arcs :order :data :stroke-alpha-factor alpha
                                           :mapping (merge {:x :x :y :y :group :edge} edge-mapping)}))
                       (chart/scatter (merge {:theme-style :arc-node} node-style
                                             {:data nodes :mapping (merge {:x :x :y :y} mapping)}))]
              :scales (cond-> (theme/deep-merge {:x (leaf-axis (mapv (fn [n] {:name (or (label-of n) "")}) nodes) labels?)
                                                 :y {:domain [(m/- (m/* arc-room top))
                                                              (m/* arc-room+ top)] :nice false
                                                     :expand [0.0 0.0] :axis false}}
                                                scales)
                        (:size edge-mapping) (update :size #(merge arc-width-range %)))
              :labels {:title title :x :none :y :none}}
       labels? (assoc-in [:theme :axis :x :label-rotation] 90)
       user-theme (update :theme #(theme/deep-merge % user-theme))))))

;; sankey, alluvial

(def ^:private unit-axis
  "An axis of the unit square of a sankey, hidden, with a little room around."
  {:domain [-0.01 1.01] :nice false :expand [0.0 0.0] :axis false})

(defn- node-label
  "Label text of a laid-out sankey or alluvial node.

  Parameters:

  - `node` (map): a laid-out node with `:id` and an optional `:name`.

  Returns its `:name` (also when `nil`) as a string, else its `:id`."
  [node]
  (str (if (contains? node :name) (:name node) (:id node))))

(def ^{:private true :const true :tag 'double} label-line
  "Half the height of a sankey label's line, a fraction of the unit square's height: nodes within it of the label's centre limit its room (layout geometry, about half a label's height in a plot 600 px high)."
  0.02)

(defn- label-room
  "Gives the room of a sankey node's label along x: up to the nearest node on its side in the label's line - half way to it when that node's label faces this one -, else up to the frame.

  Nodes in the left half (`:x0` below 0.5) are labelled right of the node, the others left of it, so the labels of a left-half node and a right-half node facing each other share the gap between the nodes (neither reaches past its middle); a node is in the label's line when its vertical span comes within [[label-line]] of the label's centre.

  Parameters:

  - `nodes` (sequence of maps): all laid-out nodes, `:x0`, `:x1`, `:y0`, `:y1` in the unit square.
  - `node` (map): the labelled node.

  Returns the room in x units of the unit square."
  ^double [nodes {:keys [^double x0 ^double x1 ^double y0 ^double y1]}]
  (let [yc (m/* 0.5 (m/+ y0 y1))
        in-line (filter (fn [{^double a :y0 ^double b :y1}]
                          (and (m/< (m/- yc label-line) (m/max a b)) (m/> (m/+ yc label-line) (m/min a b))))
                        nodes)]
    (if (m/< x0 0.5)
      (let [ahead (filter (fn [{^double s :x0}] (m/> s x1)) in-line)]
        (if-let [{^double s :x0} (first (sort-by :x0 ahead))]
          ;; a right-half node labels leftwards, towards this label
          (m/* (if (m/< s 0.5) 1.0 0.5) (m/- s x1))
          (m/- 1.0 x1)))
      (let [behind (filter (fn [{^double e :x1}] (m/< e x0)) in-line)]
        (if-let [{^double e :x1 ^double s :x0} (last (sort-by :x1 behind))]
          ;; a left-half node labels rightwards, towards this label
          (m/* (if (m/< s 0.5) 0.5 1.0) (m/- x0 e))
          x0)))))

(defn- side-labels
  "Text charts of the sankey node names at the middle of their sides, in the text role `:sankey-label` (white boxes), both inside the frame.

  Each label is fitted to its room (see [[label-room]]; labels facing each other across a gap share it): shrunk to fit, down to the role's `:min-size` (the text kept whole, so a label longer than its room even at that size still reaches past it); labels of one side overlapping one of a larger node are left out (text `:x-span` and `:overlapping-labels :drop`).

  Parameters:

  - `nodes` (sequence of maps): laid-out nodes with `:x0`, `:x1`, `:y0`, `:y1`, `:value` in the unit square.

  Returns a vector of up to two text chart values: names right of the nodes in the left half, left of those in the right half."
  [nodes]
  (let [row (fn [x {:keys [^double y0 ^double y1] :as node}]
              {:x x :y (m/* 0.5 (m/+ y0 y1)) :label (node-label node) :room (label-room nodes node)})
        ;; larger nodes first: their labels win collisions
        [left right] ((juxt filter remove) #(m/< (double (:x0 %)) 0.5) (sort-by #(m/- (double (:value %))) nodes))
        text (fn [rows anchor dx]
               (chart/text {:role :sankey-label :data rows :mapping {:x :x :y :y :label :label :x-span :room}
                            :anchor anchor :offset [dx 0] :overlapping-labels :drop}))]
    (cond-> []
      (seq left) (conj (text (mapv #(row (:x1 %) %) left) [:left :middle] 4))
      (seq right) (conj (text (mapv #(row (:x0 %) %) right) [:right :middle] -4)))))

(def sankey-options
  "Options schema of [[sankey]]: its own entries, those of [[cljplot2.data.graph/graph]] (but `:directed` - flows are directed) and [[cljplot2.data.graph/sankey-layout]] (forwarded) and the builder's."
  (schema/options-map
   [(schema/nilable :mapping {:description "Maps the node colour to an accessor of the node rows; sankey maps only `:color`."}
                    [:map {:closed true}
                     (schema/nilable :color {:description "An accessor of the node rows that returns each node's colour; mapping it adds a legend."
                                             :cljplot2/default-doc "The node id, without a legend." :cljplot2/example :kind
                                             :cljplot2/example-context {:nodes [{:id "a" :kind "x"} {:id "b" :kind "y"} {:id "c" :kind "x"} {:id "d" :kind "y"}]}}
                                     schema/accessor)])
    (schema/nilable :link-color {:description "The colour of each flow band." :default :source
                                 :cljplot2/value-meanings {:source "The colour of its source node." :target "The colour of its target node."
                                                           :theme "The colour of the theme style `[:chart :sankey-link]`, grey by default."}}
                    [:enum :source :target :theme])
    (schema/optional :alpha {:description "The opacity of the flow bands." :default 0.5 :cljplot2/example 1.0} schema/fraction)
    (schema/optional :labels {:description "Writes each node's name beside it, inside the frame." :default true
                              :cljplot2/value-meanings {true "Writes the names." false "No names."}}
                     schema/bool)]
   (remove #(= :directed (first %)) graph/graph-entries)
   graph/sankey-layout-entries
   plot/builder-entries))

(defn sankey
  "Builds a sankey diagram of flow rows as a plot map: nodes as rectangles coloured by node (`chart/polygon`), flows as bands in the theme style `[:chart :sankey-link]`, node names beside the nodes, no axes.

  The layout is [[cljplot2.data.graph/sankey-layout]], a d3-sankey port.

  Parameters:

  - `edges` (sequence of maps): flow rows `{:from :to :value}`, see [[cljplot2.data.graph/graph]].
  - `options` (map, optional), validated first against [[sankey-options]]:
    - graph - `:from`, `:to`, `:value`, `:nodes`, `:id`;
    - layout - `:align`, `:layer`, `:node-width`, `:gap`, `:iterations`, `:order`;
    - `:mapping {:color acc}` - the node colour, an accessor of node rows (default the id; a legend unless it's the id);
    - `:link-color` - the band colour: that of its `:source` (default) or `:target` node, or `:theme` (the colour of the theme style `[:chart :sankey-link]`, grey);
    - `:alpha` - the bands' opacity, default 0.5;
    - `:labels` - node names beside the nodes, inside the frame, in the text role `:sankey-label`, default `true`; each fitted to its room - up to the nearest node on its side in its line, else the frame - shrunk, down to the role's `:min-size` (the text kept whole) - and left out when it overlaps the label of a larger node;
    - `:scales`, `:title`, `:theme`.

  Returns a plot map.

  Throws `ex-info {:key k}` for an invalid option.

  Equivalents: d3-sankey, networkD3 `sankeyNetwork`, plotly sankey.

  See also [[alluvial]]."
  {:cljplot2/options sankey-options}
  ([edges] (sankey edges {}))
  ([edges {:keys [mapping alpha scales title] user-theme :theme :or {alpha 0.5} :as options}]
   (schema/check-options! sankey-options options)
   (let [labels? (data/option options :labels true)
         link-color (data/option options :link-color :source)
         node-colored? (not= :theme link-color)
         {:keys [nodes rects] laid-bands :bands} (graph/sankey-layout (graph/graph edges (assoc (select-keys options graph-keys) :directed true))
                                                          (select-keys options (keys-of graph/sankey-layout-entries)))
         color-of (data/row-accessor :color (or (:color mapping) :id))
         node-color (into {} (map (juxt :id color-of)) nodes)
         bands (cond->> laid-bands
                 node-colored? (mapv #(assoc % ::color (node-color (if (= :source link-color) (:from %) (:to %))))))]
     (cond-> {:charts (cond-> [(chart/polygon (cond-> {:theme-style :sankey-link :data bands :alpha alpha
                                                       :mapping {:x :x :y :y :group :link}}
                                                node-colored? (assoc-in [:mapping :color] ::color)))
                               (chart/polygon {:data (mapv #(assoc % ::color (node-color (:node %))) rects)
                                               :mapping {:x :x :y :y :group :rect :color ::color}})]
                        labels? (into (side-labels nodes)))
              :scales (theme/deep-merge {:x unit-axis :y unit-axis
                                         :color {:domain (vec (distinct (map color-of nodes)))
                                                 :legend (some? (:color mapping))}}
                                        scales)
              :labels {:title title :x :none :y :none}}
       user-theme (assoc :theme user-theme)))))

(def ^:private alluvial-example-keys
  "The required options of every alluvial option example: the variables and weight of the example rows."
  {:variables [:class :sex] :value :n})

(def alluvial-options
  "Options schema of [[alluvial]]: its own entries, those of [[cljplot2.data.graph/alluvial-graph]] (forwarded) and the builder's."
  (schema/options-map
   (map (fn [[k props s]] [k (update props :cljplot2/example-context #(merge alluvial-example-keys %)) s])
    [(schema/optional :alpha {:description "The opacity of the strands." :default 0.6 :cljplot2/example 1.0} schema/fraction)
     (schema/optional :node-width {:description "The width of each stratum, as a fraction of the plot width." :default 0.12 :cljplot2/example 0.3} schema/open-fraction)
     (schema/optional :gap {:description "The smallest gap between the strata of a column, as a fraction of the plot height." :default 0.03 :cljplot2/example 0.2}
                      schema/finite-non-neg)
     (schema/nilable :variable-names {:description "The names under the columns, one per variable."
                                      :cljplot2/default-doc "The variables' accessor names: a keyword's name, else its text." :cljplot2/example ["Class" "Sex"]}
                     [:sequential any?])])
   graph/alluvial-graph-entries
   plot/builder-entries))

(defn alluvial
  "Builds an alluvial diagram of wide rows as a plot map: a column per variable with a stratum per level, strands between consecutive columns as bands coloured by the `:strand-color` level, variable names under the columns, no y axis.

  Strata draw in the theme style `[:chart :alluvial-stratum]` (grey), the level's name centred in the text role `:alluvial-label` (a white box; a label overlapping that of a larger stratum is left out); levels are in natural order. The layout is [[cljplot2.data.graph/alluvial-graph]] and [[cljplot2.data.graph/sankey-layout]] (fixed columns, strata in level order, spread evenly).

  Parameters:

  - `rows` (sequence of maps): wide rows, one per observation or group.
  - `options` (map), validated first against [[alluvial-options]]:
    - `:variables` - accessors of the categorical variables, one column each, at least 2 (required);
    - `:strand-color` - the accessor colouring the strands, default the first variable; `:value` - an accessor of the row weight, default 1 per row;
    - `:alpha` - the strands' opacity, default 0.6;
    - `:node-width` - the stratum width, a fraction of the plot width, default 0.12;
    - `:gap` - the least gap between the strata of an axis, a fraction of the plot height, default 0.03;
    - `:variable-names` - names under the columns, one per variable (default the accessors' names);
    - `:scales` (e.g. `:color`), `:title`, `:theme`.

  Returns a plot map.

  Throws `ex-info {:key k}` for an invalid option.

  Equivalents: ggalluvial `geom_alluvium` + `geom_stratum`, d3 parallel sets.

  See also [[sankey]]."
  {:cljplot2/options alluvial-options}
  [rows {:keys [variables strand-color alpha ^double node-width gap variable-names scales title] user-theme :theme
         :or {alpha 0.6 node-width 0.12 gap 0.03} :as options}]
  (schema/check-options! alluvial-options options)
  (let [g (graph/alluvial-graph rows (select-keys options (keys-of graph/alluvial-graph-entries)))
        {:keys [nodes rects bands]} (graph/sankey-layout g {:layer :layer :order :input :iterations 0
                                                            :node-width node-width :gap gap})
        variable-count (count variables)
        names (vec (or variable-names (map label/name-text variables)))
        centres (mapv (fn [^double v] (m/+ (m/* (m// (m/- 1.0 node-width) (m/dec variable-count)) v) (m/* 0.5 node-width)))
                      (range variable-count))
        nearest (fn [^double v] (apply min-key #(m/abs (m/- (double (centres %)) v)) (range variable-count)))]
    (cond-> {:charts [(chart/polygon {:data bands :alpha alpha :mapping {:x :x :y :y :group :link :color :strand-color}})
                      (chart/polygon {:theme-style :alluvial-stratum :data rects :mapping {:x :x :y :y :group :rect}})
                      (chart/text {:data (mapv (fn [{:keys [^double x0 ^double x1 ^double y0 ^double y1] :as node}]
                                                 {:x (m/* 0.5 (m/+ x0 x1))
                                                  :y (m/* 0.5 (m/+ y0 y1))
                                                  :label (node-label node)})
                                               ;; larger strata first: their labels win collisions
                                               (sort-by #(m/- (double (:value %))) nodes))
                                   :mapping {:x :x :y :y :label :label} :role :alluvial-label :overlapping-labels :drop})]
             :scales (theme/deep-merge {:x (assoc (dissoc unit-axis :axis) :ticks centres :formatter #(get names (nearest %) "")
                                                  :overlapping-labels :keep)
                                        :y unit-axis
                                        :color {:title (label/name-text (or strand-color (first variables)))}}
                                       scales)
             :labels {:title title :x :none :y :none}}
      user-theme (assoc :theme user-theme))))

;; sequences

(def sequences-options
  "Options schema of [[sequences]]: its own entries, those of [[cljplot2.data.hierarchy/sequence-levels]] (forwarded) and the builder's."
  (schema/options-map
   [(schema/optional :inner-radius {:description "The radius of the centre hole, as a fraction of the outer radius." :default 0.3 :cljplot2/example 0.6}
                     [:and schema/finite [:>= 0] [:< 1]])
    (schema/optional :labels {:description "Writes the step names in the cells they fit in." :default false
                              :cljplot2/value-meanings {true "Writes the names." false "No names."}}
                     schema/bool)
    (schema/optional :keep-end {:description "Whether the end marker is a step of its own." :default true
                                :cljplot2/value-meanings {true "A step of its own, coloured and in the legend." false "Sequences that end leave their group's grey remainder."}}
                     schema/bool)]
   hierarchy/sequence-levels-entries
   plot/builder-entries))

(defn sequences
  "Builds a flow (sequence) sunburst of rows as a plot map: the sequences' steps as rings from the centre, every step coloured by its name wherever it occurs, with a legend of the names.

  The cells are `chart/partition` (`:color-by :name`) under a polar coord, in the theme style `[:chart :sequences-cell]` (thin borders: deep rings have many narrow cells).

  Parameters:

  - `rows` (sequence of maps): rows holding a sequence of steps each.
  - `options` (map, optional), validated first against [[sequences-options]]:
    - `:sequence`, `:value`, `:separator`, `:end` - see [[cljplot2.data.hierarchy/sequence-levels]];
    - `:keep-end` - the end marker a step of its own, coloured and in the legend, default `true` (`false` - sequences that end are their group's grey remainder);
    - `:inner-radius` - radius of the centre hole, a fraction of the outer one, default 0.3;
    - `:labels` - step names in the cells that fit, default `false`;
    - `:scales`, `:title`, `:theme`.

  Returns a plot map.

  Throws `ex-info {:key k}` for an invalid option.

  Equivalents: d3 sequences sunburst (Kerry Rodden), sunburstR."
  {:cljplot2/options sequences-options}
  ([rows] (sequences rows {}))
  ([rows {:keys [scales title] inner :inner-radius user-theme :theme :or {inner 0.3} :as options}]
   (schema/check-options! sequences-options options)
   (let [labels? (data/option options :labels false)
         {level-rows :rows :keys [hierarchy]} (hierarchy/sequence-levels rows (select-keys options (keys-of hierarchy/sequence-levels-entries)))]
     (cond-> {:data level-rows
              :mapping {:value :value :hierarchy hierarchy}
              ;; theme style :sequences-cell - thin borders: deep rings have many narrow cells
              :charts [(chart/partition {:theme-style :sequences-cell :color-by :name :order :value
                                         :group-labels labels? :leaf-labels labels?})]
              :coord {:kind :polar :inner inner}
              :scales (theme/deep-merge {:color {:title :none}} scales)
              :labels {:title title}}
       user-theme (assoc :theme user-theme)))))
