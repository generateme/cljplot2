(ns cljplot2.graphs
  "Plot builders of graphs and trees: a layout from [[cljplot2.data.graph]] or [[cljplot2.data.tree]] drawn with ordinary charts and returned as a plot map.

  The returned map is data: edit it, add charts, or build the plot from the layout yourself. A builder's `:scales` option is merged into the builder's own scales key by key (see [[cljplot2.theme/deep-merge]]), so `{:color {:range ..}}` changes the palette and keeps the builder's domain and legend setting. Marks draw in named theme styles (`:theme-style`, see [[cljplot2.theme/named-styles]]) and labels in builder text roles, so a theme restyles every plot of a builder; the per-call style options win over them. Every builder validates its options first (its `*-options` schema) and throws `ex-info {:key k}` for an invalid value or an unknown key.

  Graphs: [[network]] (node-link), [[chord]] (ring sectors and ribbons), [[arc]] (nodes on a line), [[sankey]] (layered flows), [[alluvial]] (flows between categorical columns).

  Trees: [[dendrogram]] (elbow branches at merge heights), [[tree]] (node-link tree), [[clustered-heatmap]] (tiles ordered by clustering, with dendrograms), [[bundle]] (hierarchical edge bundling), [[sequences]] (flow sunburst).

  Option schemas: [[network-options]], [[chord-options]], [[arc-options]], [[sankey-options]], [[alluvial-options]], [[dendrogram-options]], [[tree-options]], [[clustered-heatmap-options]], [[bundle-options]], [[sequences-options]]."
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
            [cljplot2.theme :as theme]))

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
   [(schema/nilable :mapping {:description "node aesthetics (:color, :size, :shape): accessors of the nodes (their row's columns, :degree, :in, :out)"}
                    node-map)
    (schema/nilable :edge-mapping {:description "edge aesthetics (:color, :size - the width): accessors of the edges (their row's columns, :value); none mapped in :mapping too"}
                    node-map)
    (schema/nilable :node-style {:description "scatter options of the nodes, merged over the default"
                                 :cljplot2/default-doc "theme [:chart :network-node]"}
                    node-map)
    (schema/nilable :edge-style {:description "segment options of the edges, merged over the default (a mapped colour wins); :arrow - the arrows of a directed graph"
                                 :cljplot2/default-doc "theme [:chart :network-edge]"}
                    node-map)
    (schema/nilable :labels {:description "node labels right of the nodes: true - the id, an accessor of the node, false - none"
                             :default false}
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
    - `:labels` - `true` (the node id) or an accessor of the node (`nil` or an empty string - no label), in the text role `:network-label` (white boxes) right of the nodes, offset by the largest node radius; default `false`;
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
                                                    :data nodes
                                                    :mapping {:x :x :y :y :label label-of}
                                                    :anchor [:left :middle]
                                                    :offset [(m/+ max-radius 3.0) 0]})))
              :aspect :iso
              :scales (theme/deep-merge {:x x-axis :y y-axis} scales)
              :labels {:title title :x nil :y nil}}
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

  Labels sit just outside each leaf's radius: the right half anchored left, the left half anchored right; they draw in the text role `:radial-label`, the font smaller for many leaves (down to 5 pt, never above the role's size); pixel room for the labels is kept around the circle (`:reserve-room`).

  Parameters:

  - `nodes` (sequence of maps): laid-out nodes with `:angle` (degrees), `:leaf?`, `:name` and an optional `:radius` (default 1).
  - `user-theme` (map or `nil`): the builder's `:theme` option, merged over the default theme for the role's size.

  Returns a vector of up to two text chart values (empty without leaves).

  See also [[circle-axis]]."
  [nodes user-theme]
  (let [leaves (filterv :leaf? nodes)
        role-size (double (get-in (theme/deep-merge theme/default-theme user-theme) [:text :radial-label :size]))
        size (m/min role-size (m/max 5.0 (m// 1400.0 (m/max 1 (count leaves)))))
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
   [(schema/nilable :leaf-names {:description "names of an hclust result's observations, in order (one per observation)"
                             :cljplot2/default-doc "the observations' indices"}
                    [:sequential any?])
    (schema/nilable :horizontal {:description "leaves on the left, heights growing right (not circular)" :default false}
                    schema/bool)
    (schema/optional :labels {:description "leaf names along the leaf axis (circular - outward around the circle)" :default true}
                     schema/bool)
    (schema/nilable :group-labels {:description "names of the named groups at their nodes (level and parent trees)" :default false}
                    schema/bool)
    (schema/nilable :line-style {:description "line options of the branches, merged over the default"
                                 :cljplot2/default-doc "theme [:chart :dendrogram-branch]"}
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
                        group-labels (conj (chart/text {:data (filterv #(and (not (:leaf? %)) (some? (:name %))) nodes)
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
              :labels {:title title :x nil :y (when (and heights? (not circular)) "height")}}
       circular (assoc :aspect :iso)
       (and horizontal (not circular)) (assoc :flip true)
       (and labels? (not circular) (not horizontal)) (assoc-in [:theme :axis :x :label-rotation] -90)
       user-theme (update :theme #(theme/deep-merge % user-theme))))))

;; node-link tree

(defn- tree-labels
  "Text charts of the node names of a cartesian node-link tree (root on the left) in the text role `:tree-label`.

  Leaves are labelled right of their node, groups left of theirs (Vega `tree`); unnamed nodes get no label; room is kept only where the names reach (`:reserve-room :anchor`).

  Parameters:

  - `nodes` (sequence of maps): laid-out nodes with `:x`, `:y`, `:leaf?` and an optional `:name`.

  Returns a vector of up to two text chart values."
  [nodes]
  (let [named (filterv #(some? (:name %)) nodes)
        [leaves groups] ((juxt filter remove) :leaf? named)
        text (fn [rows anchor dx]
               (chart/text {:data rows :mapping {:x :x :y :y :label #(str (:name %))} :anchor anchor :offset [dx 0]
                            :role :tree-label :reserve-room :anchor}))]
    (cond-> []
      (seq leaves) (conj (text leaves [:left :middle] 6))
      (seq groups) (conj (text groups [:right :middle] -6)))))

(def tree-options
  "Options schema of [[tree]]: its own entries, those of [[cljplot2.data.tree/node-link-layout]] (forwarded) and the builder's."
  (schema/options-map
   [(schema/optional :labels {:description "node names (cartesian - leaves right of their node, groups left; circular - leaf names outward)"
                               :default true}
                     schema/bool)
    (schema/optional :node-points {:description "a point per node" :default true} schema/bool)
    (schema/nilable :mapping {:description "node aesthetics (:color, :size, :shape): accessors of the nodes (their row's columns, :depth, :leaf?, :name)"}
                    node-map)
    (schema/nilable :node-style {:description "scatter options of the nodes, merged over the default"
                                 :cljplot2/default-doc "theme [:chart :tree-node]"}
                    node-map)
    (schema/nilable :link-style {:description "line options of the links, merged over the default"
                                 :cljplot2/default-doc "theme [:chart :tree-link]"}
                    node-map)
    (schema/nilable :leaf-names {:description "names of an hclust result's observations, in order (one per observation)"
                             :cljplot2/default-doc "the observations' indices"}
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
    - `:mapping` - node aesthetics (`:color`, `:size`, `:shape`; nodes have `:depth`, `:leaf?`, `:name` and their row's columns - e.g. `{:color #(str (:depth %))}`);
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
              :labels {:title title :x nil :y nil}}
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

(def clustered-heatmap-options
  "Options schema of [[clustered-heatmap]]: its own entries and the builder's."
  (schema/options-map
   [[:row {:description "accessor of a row's tile row"} schema/accessor]
    [:col {:description "accessor of a row's tile column"} schema/accessor]
    [:value {:description "accessor of a row's tile value, a finite number"} schema/accessor]
    (schema/optional :method {:description "fastmath hclust linkage of the rows and columns" :default :complete}
                     [:enum :single :complete :average :weighted :centroid :median :ward :ward-d :diana])
    (schema/optional :distance {:description "fastmath hclust distance: a keyword or a function of two double arrays" :default :euclidean}
                     [:or [:enum :euclidean :chebyshev :manhattan :canberra :earth-movers] schema/function])
    (schema/optional :row-tree {:description "the rows' dendrogram right of the tiles (false - order only)" :default true}
                     schema/bool)
    (schema/optional :col-tree {:description "the columns' dendrogram above the tiles (false - order only)" :default true}
                     schema/bool)
    (schema/optional :tree-size {:description "[w h] - the dendrograms' share of the width (rows) and the height (columns), fractions in (0, 1)"
                                 :default [0.2 0.2]}
                     [:tuple schema/open-fraction schema/open-fraction])]
   plot/builder-entries))

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
    - `:tree-size` - `[w h]`, the dendrograms' share of the width and the height, default `[0.2 0.2]`;
    - `:scales` - the tile plot's scales (e.g. `:color`), `:title`, `:theme`.

  Returns an arrange plot map (see [[cljplot2.multiplot/arrange]]).

  Throws `ex-info {:key :value}` for a missing or non-finite cell, `ex-info {:key :rows}` for fewer than 2 rows or columns, `ex-info {:key k}` for an invalid or missing option.

  Equivalents: R `heatmap`, `pheatmap` (without annotations; the row tree on the right as cljplot2 margins).

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
        heatmap (tile-plot {:legend false :theme {:axis {:x {:label-rotation -90}}} :labels {:x nil :y nil}})
        legend (tile-plot {:legend-only true})
        tree-plot (fn [t flip?]
                    (let [{:keys [paths]} (tree/layout t {})
                          top (double (:height t))
                          n (count (tree/leaves t))]
                      (cond-> {:charts [(branch-chart paths false nil)]
                               :scales {:x {:domain [-0.5 (m/- n 0.5)] :nice false :expand [0.0 0.0] :axis false}
                                        :y (assoc (height-axis top) :axis false)}
                               :legend false :labels {:x nil :y nil}}
                        flip? (assoc :flip true)
                        user-theme (assoc :theme user-theme))))
        blank {:charts [] :scales {:x {:domain [0 1] :axis false} :y {:domain [0 1] :axis false}} :labels {:x nil :y nil}}
        empty-legend {:legend-only true}
        main-row (cond-> [heatmap] row-tree? (conj (tree-plot row-tree true)) true (conj legend))
        top-row (when col-tree? (cond-> [(tree-plot col-tree false)] row-tree? (conj blank) true (conj empty-legend)))
        columns (count main-row)]
    (plot/arrange (vec (concat top-row main-row))
                  (cond-> {:columns columns
                           :widths (cond-> [(m/- 1.0 (if row-tree? tw 0.0))] row-tree? (conj tw) true (conj :measured))}
                    col-tree? (assoc :heights [th (m/- 1.0 th)])
                    title (assoc :title title)))))

(def ^:private bundle-edge-color
  "Default colour of bundled edges not coloured by group (`:edge-color`)."
  (c/color 70 110 180))

(def bundle-options
  "Options schema of [[bundle]]: its own entries, those of [[cljplot2.data.tree/bundle-paths]] (forwarded) and the builder's."
  (schema/options-map
   [(schema/optional :alpha {:description "opacity of the edges (leaf points and legend keys stay opaque)" :default 0.4}
                     schema/fraction)
    (schema/nilable :color-by {:description "edge and leaf colours: :group - the top-level group of their (source) leaf, with a legend; :none - one :edge-color, no legend"
                                 :default :group}
                     [:enum :group :none])
    (schema/optional :edge-color {:description "colour of the edges when not coloured by group" :default bundle-edge-color}
                     schema/color)
    (schema/optional :labels {:description "leaf names outward around the circle" :default true} schema/bool)]
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
    - `:color-by` - `:group` (default): edges and leaves coloured by the top-level group of their (source) leaf, with a legend; `:none` - edges in `:edge-color`, no legend;
    - `:edge-color` - the edges' colour when not coloured by group, default `[70 110 180]`;
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
         leaf-group (into {} (keep-indexed (fn [i n] (when (:leaf? n) [(tree/leaf-key n) (str (top-group i))])) nodes))
         {bundled :paths} (tree/bundle-paths edges laid (cond-> {:beta beta} from (assoc :from from) to (assoc :to to)))
         _ (when (and (seq edges) (empty? bundled))
             (schema/fail! (str "No bundle edge joins two different leaves of the tree: edge ends should name leaves - "
                                "the :id of a from-parents node, else the leaf name.")
                           {:key :edges :value (count edges)}))
         from-of (data/row-accessor :from (or from :from))
         paths (mapv #(assoc % ::group (leaf-group (from-of %))) bundled)
         leaves (mapv #(assoc % ::group (leaf-group (tree/leaf-key %))) (filter :leaf? nodes))]
     (cond-> {:charts (cond-> [(chart/line (cond-> {:theme-style :bundle-edge :data paths :order :data :stroke-alpha-factor alpha
                                                    :mapping (cond-> {:x :x :y :y :group :id} by-group? (assoc :color ::group))}
                                             (not by-group?) (assoc :color edge-color)))
                               (chart/scatter {:theme-style :bundle-leaf :data leaves :mapping (cond-> {:x :x :y :y} by-group? (assoc :color ::group))})]
                        labels? (into (radial-labels nodes user-theme)))
              :aspect :iso
              :scales (theme/deep-merge (cond-> {:x circle-axis :y circle-axis} by-group? (assoc :color {:title nil})) scales)
              :labels {:title title :x nil :y nil}}
       user-theme (update :theme #(theme/deep-merge % user-theme))))))

;; ordered layouts

(def chord-options
  "Options schema of [[chord]]: its own entries, those of [[cljplot2.data.graph/graph]] and [[cljplot2.data.graph/chord-layout]] (forwarded) and the builder's."
  (schema/options-map
   [(schema/optional :alpha {:description "opacity of the ribbons" :default 0.6} schema/fraction)
    (schema/optional :labels {:description "node names outward from the sectors' middles (nodes without flow unlabelled)" :default true}
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
                            {:name id :leaf? true :angle (m/* 0.5 (m/+ start end))}))]
     (cond-> {:charts (cond-> [(chart/polygon {:data ribbons :alpha alpha :mapping {:x :x :y :y :group :ribbon :color :from}})
                               (chart/polygon {:data arcs :mapping {:x :x :y :y :color :group}})]
                        labels? (into (radial-labels label-nodes user-theme)))
              :aspect :iso
              ;; colours in ring order, the same for sectors and ribbons
              :scales (theme/deep-merge {:x circle-axis :y circle-axis :color {:domain (mapv :id groups) :legend false}} scales)
              :labels {:title title :x nil :y nil}}
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
   [(schema/nilable :mapping {:description "node aesthetics (:color, :size, :shape): accessors of the nodes (their row's columns, :degree, :in, :out)"}
                    node-map)
    (schema/nilable :edge-mapping {:description "edge aesthetics (:color, :size - the width): accessors of the edges (their row's columns, :value); none mapped in :mapping too"
                                   :cljplot2/default-doc "{:size :value} (widths from the size scale {:range [0.5 4]}) when the edge weights differ and :mapping has no :size"}
                    node-map)
    (schema/optional :alpha {:description "opacity of the arcs' strokes" :default 0.5} schema/fraction)
    (schema/nilable :node-style {:description "scatter options of the nodes, merged over the default"
                                 :cljplot2/default-doc "theme [:chart :arc-node]"}
                    node-map)
    (schema/nilable :edge-style {:description "line options of the arcs, merged over the default"
                                 :cljplot2/default-doc "theme [:chart :arc-edge]"}
                    node-map)
    (schema/optional :labels {:description "node labels under the line, rotated to read downward: true - the id, false - none, or an accessor of the node (a nil or \"\" result - an empty label)"
                              :default true}
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
              :labels {:title title :x nil :y nil}}
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

(defn- side-labels
  "Text charts of the sankey node names at the middle of their sides, in the text role `:sankey-label` (white boxes), both inside the frame.

  Parameters:

  - `nodes` (sequence of maps): laid-out nodes with `:x0`, `:x1`, `:y0`, `:y1` in the unit square.

  Returns a vector of up to two text chart values: names right of the nodes in the left half, left of those in the right half."
  [nodes]
  (let [row (fn [x {:keys [^double y0 ^double y1] :as node}] {:x x :y (m/* 0.5 (m/+ y0 y1)) :label (node-label node)})
        [left right] ((juxt filter remove) #(m/< (double (:x0 %)) 0.5) nodes)]
    (cond-> []
      (seq left) (conj (chart/text {:role :sankey-label :data (mapv #(row (:x1 %) %) left) :mapping {:x :x :y :y :label :label}
                                    :anchor [:left :middle] :offset [4 0]}))
      (seq right) (conj (chart/text {:role :sankey-label :data (mapv #(row (:x0 %) %) right) :mapping {:x :x :y :y :label :label}
                                     :anchor [:right :middle] :offset [-4 0]})))))

(def sankey-options
  "Options schema of [[sankey]]: its own entries, those of [[cljplot2.data.graph/graph]] (but `:directed` - flows are directed) and [[cljplot2.data.graph/sankey-layout]] (forwarded) and the builder's."
  (schema/options-map
   [(schema/nilable :mapping {:description "node aesthetics: only :color"}
                    [:map {:closed true}
                     (schema/nilable :color {:description "accessor of the node rows giving the node colour (a legend unless it's the id)"
                                             :cljplot2/default-doc "the node id, no legend"}
                                     schema/accessor)])
    (schema/nilable :link-color {:description "the flow colour: that of its :source or :target node, or :theme (the theme style [:chart :sankey-link], grey)" :default :source}
                    [:enum :source :target :theme])
    (schema/optional :alpha {:description "opacity of the flow bands" :default 0.5} schema/fraction)
    (schema/optional :labels {:description "node names beside the nodes, inside the frame" :default true} schema/bool)]
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
    - `:labels` - node names beside the nodes, inside the frame, in the text role `:sankey-label`, default `true`;
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
              :labels {:title title :x nil :y nil}}
       user-theme (assoc :theme user-theme)))))

(def alluvial-options
  "Options schema of [[alluvial]]: its own entries, those of [[cljplot2.data.graph/alluvial-graph]] (forwarded) and the builder's."
  (schema/options-map
   [(schema/optional :alpha {:description "opacity of the strands" :default 0.6} schema/fraction)
    (schema/optional :node-width {:description "stratum width, a fraction of the plot width" :default 0.12} schema/open-fraction)
    (schema/optional :gap {:description "least gap between the strata of an axis, a fraction of the plot height" :default 0.03}
                     schema/finite-non-neg)
    (schema/nilable :variable-names {:description "names under the columns, one per variable"
                                 :cljplot2/default-doc "the variables' accessor names (a keyword's name, else its text)"}
                    [:sequential any?])]
   graph/alluvial-graph-entries
   plot/builder-entries))

(defn alluvial
  "Builds an alluvial diagram of wide rows as a plot map: a column per variable with a stratum per level, strands between consecutive columns as bands coloured by the `:strand-color` level, variable names under the columns, no y axis.

  Strata draw in the theme style `[:chart :alluvial-stratum]` (grey), the level's name centred in the text role `:alluvial-label` (a white box); levels are in natural order. The layout is [[cljplot2.data.graph/alluvial-graph]] and [[cljplot2.data.graph/sankey-layout]] (fixed columns, strata in level order, spread evenly).

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
                                               nodes)
                                   :mapping {:x :x :y :y :label :label} :role :alluvial-label})]
             :scales (theme/deep-merge {:x (assoc (dissoc unit-axis :axis) :ticks centres :formatter #(get names (nearest %) "")
                                                  :overlapping-labels :keep)
                                        :y unit-axis
                                        :color {:title (label/name-text (or strand-color (first variables)))}}
                                       scales)
             :labels {:title title :x nil :y nil}}
      user-theme (assoc :theme user-theme))))

;; sequences

(def sequences-options
  "Options schema of [[sequences]]: its own entries, those of [[cljplot2.data.hierarchy/sequence-levels]] (forwarded) and the builder's."
  (schema/options-map
   [(schema/optional :inner-radius {:description "radius of the centre hole, a fraction of the outer one" :default 0.3}
                     [:and schema/finite [:>= 0] [:< 1]])
    (schema/optional :labels {:description "step names in the cells that fit" :default false} schema/bool)
    (schema/optional :keep-end {:description "the end marker a step of its own, coloured and in the legend (false - sequences that end are their group's grey remainder)"
                                 :default true}
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
              :scales (theme/deep-merge {:color {:title nil}} scales)
              :labels {:title title}}
       user-theme (assoc :theme user-theme)))))
