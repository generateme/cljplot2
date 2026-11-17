(ns cljplot2.graphs
  "Plot builders of graphs and trees: a layout from [[cljplot2.data.graph]] / [[cljplot2.data.tree]] drawn with
  ordinary charts, returned as a plot map (edit it, add charts, or build the plot from the layout yourself).

  A builder's `:scales` option is merged into the builder's own scales key by key (see [[cljplot2.theme/deep-merge]]):
  `{:color {:range ..}}` changes the palette and keeps the builder's domain and legend setting."
  (:require [clojure2d.color :as c]
            [fastmath.core :as m]
            [fastmath.ml.clustering :as cl]
            [cljplot2.chart :as chart]
            [cljplot2.data.graph :as graph]
            [cljplot2.data.tree :as tree]
            [cljplot2.multiplot :as plot]
            [cljplot2.data.rows :as data]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private node-defaults
  "Node style of the builder: larger than plain scatter points, outlined (igraph-like); the fill - the theme scatter
  colour made opaque (edges don't show through)."
  {:size 12 :stroke-color :white :stroke-width 1})

(def ^:private edge-defaults {:color [120 120 120 200]})
(def ^:private node-size-range {:range [6 24] :area true})
(def ^:private ^:const frame-padding 0.06)
(def ^:private ^:const min-arrow 7.0)

(defn- node-radius-fn
  "Pixel radius of a node (a shape `size` is its bounding box side): from the node size scale of `mapping` `:size`
  (`size-spec` - the plot's `:scales :size`, its domain the nodes' values; range from the merged `merged-theme`), else
  `default-size`."
  [nodes mapping size-spec [merged-theme default-size]]
  (let [default (double default-size)]
    (if-let [acc (:size mapping)]
      (let [size-of (data/row-accessor :size acc)
            spec (aesthetic/scale-spec :size size-spec (aesthetic/value-extent :size size-spec (keep size-of nodes)) false
                                       (:palette merged-theme))
            attribute (if spec (aesthetic/attribute-fn :size spec default) (constantly default))]
        (fn [node] (* 0.5 (double (or (attribute (size-of node)) default)))))
      (constantly (* 0.5 default)))))

(defn- widest-edge
  "Width (pixels) of the widest edge: the top of the size scale's range when `edge-mapping` maps `:size` (the plot's
  `size-spec` `:range`, else the theme palette's), else the edge style's or the theme segment width."
  ^double [edge-mapping size-spec edge-style merged-theme]
  (double (if (:size edge-mapping)
            (reduce max (or (:range size-spec) (get-in merged-theme [:palette :size])))
            (or (:width edge-style) (get-in merged-theme [:chart :segment :width])))))

(defn- check-mappings!
  "Throw `ex-info {:key :edge-mapping}` when an aesthetic is mapped for both nodes (`mapping`) and edges
  (`edge-mapping`)."
  [mapping edge-mapping]
  (when-let [both (seq (filter #(and (contains? mapping %) (contains? edge-mapping %)) aesthetic/dims))]
    (schema/fail! "An aesthetic is mapped for both nodes and edges - one scale can't show two variables."
                  {:key :edge-mapping :value (vec both)})))

(defn- node-options
  "Scatter options of the nodes: the builder's defaults (see [[node-defaults]]) with the theme scatter colour of
  `merged-theme` made opaque, under the user's `style`."
  [merged-theme style]
  (merge node-defaults {:color (c/set-alpha (c/to-color (get-in merged-theme [:chart :scatter :color])) 255)} style))

(defn network
  "Network plot of `edges` (rows; see [[cljplot2.data.graph/graph]]) as a plot map: edges as segments under the nodes
  (scatter), labels beside them; equal units on both axes (`:aspect :iso`), no axes.

  `options`:

  * graph - `:from`, `:to`, `:value`, `:nodes`, `:id`, `:directed?` (see [[cljplot2.data.graph/graph]])
  * layout - `:layout` (`:force` default, `:circle`, `:random`), `:iterations`, `:gravity`, `:rng` (see
    [[cljplot2.data.graph/layout]]); without `:rng` every call lays the graph out anew (the returned plot map keeps
    its layout)
  * `:mapping` - node aesthetics (`:color`, `:size`, `:shape`; nodes have `:degree`, `:in`, `:out` and their row's
    columns), `:edge-mapping` - edge aesthetics (`:color`, `:size` - the width; edges have `:value` and their row's
    columns); an aesthetic in both throws `ex-info {:key :edge-mapping}` (one scale can't show two variables)
  * `:node-style` - scatter options of the nodes over the builder's `{:size 12 :stroke-color :white :stroke-width 1}`
    and the theme scatter colour made opaque;
    a mapped node `:size` gets the size scale `{:range [6 24] :area true}` unless `:scales :size` sets them
  * `:edge-style` - segment options of the edges over `{:color [120 120 120 200]}` (the colour only when not mapped)
  * `:labels` - `true` (the node id) or an accessor of the node (`nil` / `\"\"` - no label), on white boxes right of
    the nodes (offset by the largest node radius)
  * `:scales` - plot scales (e.g. `:color`, `:size`), `:title`, `:theme`

  Directed graphs get arrows ending at the target node's edge (a per-edge `:end-gap` of its radius, from the node size
  scale - its spec with the nodes' values as the domain); the head is at least 7 px and 3 times the widest edge
  (`:edge-style {:arrow ..}` wins). Opposite edges of a pair lie on one line.

  Equivalents: igraph `plot(graph, layout = layout_with_fr)`, ggraph `geom_edge_link` + `geom_node_point`. A
  correlation network: the pairs of a correlation matrix above a threshold, e.g.
  `(graph/adjacency->edges (mapv (fn [row] (mapv #(if (> (abs %) 0.7) % 0) row)) r) names {:directed? false})`
  with `|r|` as the `:value` (weights are non-negative) and `:edge-mapping {:color ..}` by the sign (the diagonal's
  self-loops aren't drawn)."
  [edges {:keys [mapping edge-mapping labels scales title directed? node-style edge-style] user-theme :theme :as options}]
  (check-mappings! mapping edge-mapping)
  (let [{:keys [nodes] :as laid} (graph/layout (graph/graph edges options) options)
        merged-theme (theme/deep-merge theme/default-theme user-theme)
        node-st (node-options merged-theme node-style)
        scales (cond-> scales (:size mapping) (update :size #(merge node-size-range %)))
        radius (node-radius-fn nodes mapping (:size scales) [merged-theme (:size node-st)])
        radius-of (zipmap (map :id nodes) (map radius nodes))
        max-radius (double (reduce max 0.0 (vals radius-of)))
        edge-st (merge (when-not (contains? edge-mapping :color) edge-defaults) edge-style)
        arrow (merge {:size (max min-arrow (* 3.0 (widest-edge edge-mapping (:size scales) edge-st merged-theme)))}
                     (:arrow edge-style))
        edge-rows (cond->> (:edges laid) directed? (mapv #(assoc % ::gap (+ 1.0 (double (radius-of (:to %)))))))
        label-of (cond (true? labels) :id labels (data/row-accessor :label labels))
        ;; labels right of the nodes: more room on the right
        x-axis {:domain [(- frame-padding) (+ 1.0 (if label-of (* 3.0 frame-padding) frame-padding))]
                :nice false :expand [0.0 0.0] :axis false}
        y-axis {:domain [(- frame-padding) (+ 1.0 frame-padding)] :nice false :expand [0.0 0.0] :axis false}]
    (cond-> {:charts (cond-> [(chart/segment (cond-> (merge edge-st
                                                            {:data edge-rows
                                                             :mapping (cond-> (merge {:x :x :y :y :xend :xend :yend :yend} edge-mapping)
                                                                        directed? (assoc :end-gap ::gap))})
                                               directed? (assoc :arrow arrow)))
                              (chart/scatter (merge node-st {:data nodes :mapping (merge {:x :x :y :y} mapping)}))]
                       label-of (conj (chart/text {:data nodes :mapping {:x :x :y :y :label label-of}
                                                   :anchor [:left :middle] :offset [(+ max-radius 3.0) 0] :size 8
                                                   :background [255 255 255 170] :padding 1})))
             :aspect :iso
             :scales (theme/deep-merge {:x x-axis :y y-axis} scales)
             :labels {:title title :x nil :y nil}}
      user-theme (assoc :theme user-theme))))

;; trees

(def ^:private branch-color [70 70 70])

(defn- as-tree
  "A tree (see [[cljplot2.data.tree]]) from `source`: an `hclust` result (named by `labels`) or a tree as it is."
  [source labels]
  (if (contains? source :tree) (tree/from-hclust source labels) source))

(defn- leaf-axis
  "Position scale of the leaf axis: leaves at 0..n-1 with half a step around, ticks at the leaves labelled by their
  names (`labels?` false - no axis)."
  [leaves labels?]
  (let [n (count leaves)
        names (mapv #(str (:name %)) leaves)]
    (cond-> {:domain [-0.5 (- (double n) 0.5)] :nice false :expand [0.0 0.0]}
      labels? (assoc :ticks (vec (range n)) :formatter #(get names (long (Math/round (double %))) ""))
      (not labels?) (assoc :axis false))))

(defn- branch-chart
  "Line chart of the tree's elbow `paths` (rows `{:id :x :y :cluster}`): a path per branch, coloured by cluster when
  clustered (branches above the cut in the NA colour)."
  [paths clustered? line-style]
  (chart/line (merge {:color branch-color :width 1}
                     line-style
                     {:data paths :sort false
                      ;; cluster numbers are levels, not quantities
                      :mapping (cond-> {:x :x :y :y :group :id} clustered? (assoc :color #(some-> (:cluster %) str)))})))

(defn- radial-labels
  "Two text charts of the leaf names around a circle (`nodes` with `:angle` and `:leaf?`), just outside radius 1,
  each turned along its angle and reading outward: the right half anchored left, the left half anchored right; the
  font smaller for many leaves (9 pt down to 5 pt)."
  [nodes]
  (let [leaves (filterv :leaf? nodes)
        size (m/constrain (/ 1400.0 (double (max 1 (count leaves)))) 5.0 9.0)
        row (fn [{:keys [name angle]}]
              (let [a (m/radians (double angle)) r 1.03]
                {:x (* r (m/sin a)) :y (* r (m/cos a)) :label (str name)
                 :angle (if (<= (double angle) 180.0) (- (double angle) 90.0) (- (double angle) 270.0))}))
        [right left] ((juxt filter remove) #(<= (double (:angle %)) 180.0) leaves)]
    (cond-> []
      (seq right) (conj (chart/text {:data (mapv row right) :mapping {:x :x :y :y :label :label :angle :angle}
                                     :anchor [:left :middle] :anchor-frame :text :size size}))
      (seq left) (conj (chart/text {:data (mapv row left) :mapping {:x :x :y :y :label :label :angle :angle}
                                    :anchor [:right :middle] :anchor-frame :text :size size})))))

(def ^:private ^:const root-room
  "Room past the root on a dendrogram's height axis (a fraction of the root height): its cross line stays inside the
  panel instead of on (and clipped at) its border."
  0.03)

(defn- height-axis
  "Height scale of a dendrogram whose root is at `top` (`0` - a single leaf: up to 1): from 0 exactly (leaves on the
  axis) to the root plus [[root-room]], no nice extension or chart padding."
  [top]
  (let [top (double top)]
    {:domain [0.0 (* (+ 1.0 root-room) (if (pos? top) top 1.0))] :nice false :expand [0.0 0.0]}))

(def ^:private circle-axis
  "Both axes of a circular plot: the unit circle plus room for the leaf labels."
  {:domain [-1.5 1.5] :nice false :expand [0.0 0.0] :axis false})

(defn dendrogram
  "Dendrogram plot of `source` - a fastmath `hclust` result or a tree of [[cljplot2.data.tree]] - as a plot map:
  branches as elbow paths (`chart/line`), leaves at 0..n-1 along the leaf axis labelled by their names.

  `options`: `:labels` (names of an `hclust` result's observations), `:k` (cluster colours, see
  [[cljplot2.data.tree/layout]]; branches above the cut in the NA colour, no legend), `:circular` (radial, labels
  outward, equal axes), `:horizontal` (leaves on the left, heights growing right), `:labels?` (default `true`),
  `:group-labels?` (names of the named groups at their nodes - level and parent trees; default `false`),
  `:line-style` (line options of the branches), `:title`, `:theme`. The height axis is shown for `hclust` trees
  (merge heights), hidden otherwise.

  Equivalents: R `plot(hclust)`, `dendextend` `color_branches(k)`, ggraph `geom_edge_elbow` (`circular = TRUE`)."
  [source {:keys [labels k circular horizontal title line-style group-labels?] user-theme :theme :as options}]
  (let [labels? (get options :labels? true)
        t (as-tree source labels)
        {:keys [nodes paths]} (tree/layout t (cond-> {:circular circular} k (assoc :k k)))
        leaves (tree/leaves t)
        heights? (contains? source :tree)
        top (double (:height t))]
    (cond-> {:charts (cond-> [(branch-chart paths (some? k) line-style)]
                       (and circular labels?) (into (radial-labels nodes))
                       group-labels? (conj (chart/text {:data (filterv #(and (not (:leaf? %)) (some? (:name %))) nodes)
                                                        :mapping {:x :x :y :y :label #(str (:name %))}
                                                        :anchor [:left :bottom] :offset [3 3] :size 8
                                                        :background [255 255 255 170] :padding 1})))
             :scales (cond-> (if circular
                               {:x circle-axis :y circle-axis}
                               {:x (leaf-axis leaves labels?)
                                :y (cond-> (height-axis top)
                                     (not heights?) (assoc :axis false))})
                       k (assoc :color {:legend false}))
             :labels {:title title :x nil :y (when (and heights? (not circular)) "height")}}
      circular (assoc :aspect :iso)
      (and horizontal (not circular)) (assoc :flip true)
      (and labels? (not circular) (not horizontal)) (assoc-in [:theme :axis :x :label-rotation] -90)
      user-theme (update :theme #(theme/deep-merge % user-theme)))))

(defn- value-matrix
  "`[row-levels col-levels matrix]` of long `rows` (`:row`, `:col`, `:value` accessors; levels in first-appearance
  order); throws `ex-info` for a missing or non-finite cell (`:value`), fewer than 2 rows or columns (`:rows`)."
  [rows {:keys [row col value]}]
  (let [[row-of col-of value-of] (map #(data/row-accessor %1 %2) [:row :col :value] [row col value])
        rs (vec (distinct (map row-of rows)))
        cs (vec (distinct (map col-of rows)))
        cell (into {} (map (fn [r] [[(row-of r) (col-of r)] (value-of r)])) rows)]
    (when (or (< (count rs) 2) (< (count cs) 2))
      (schema/fail! "A clustered heatmap needs at least 2 rows and 2 columns." {:key :rows}))
    [rs cs (mapv (fn [r] (mapv (fn [c] (let [v (cell [r c])]
                                         (if (and (number? v) (m/valid-double? (double v)))
                                           (double v)
                                           (schema/fail! "Every row x column cell needs a finite value." {:key :value :value [r c]}))))
                               cs))
                 rs)]))

(defn- transpose [matrix] (apply mapv vector matrix))

(defn- tree-size!
  "`[w h]` of `:tree-size` - fractions in (0, 1) of the width / height for the row / column dendrograms; throws
  `ex-info {:key :tree-size}` otherwise."
  [size]
  (when-not (and (vector? size) (= 2 (count size)) (every? #(and (number? %) (< 0.0 (double %) 1.0)) size))
    (schema/fail! "Clustered heatmap :tree-size should be [w h], fractions in (0, 1)." {:key :tree-size :value size}))
  size)

(defn clustered-heatmap
  "Clustered heatmap of long `rows` as an arrange plot map: tiles of `:value` at `:row` x `:col` (accessors), rows and
  columns ordered by fastmath `hclust` of the value matrix's rows and columns (`:method` default `:complete`,
  `:distance` default `:euclidean`), their dendrograms right of (rows) and above (columns) the tiles - as plot margins
  - each leaf at its tile's centre (`:row-tree? false` / `:col-tree? false` - order only); the colour legend in a
  column of its own on the right. `:tree-size` - `[w h]`, the dendrograms' share of the width / height (default
  `[0.2 0.2]`). `:scales` - the tile plot's scales (e.g. `:color`), `:title`, `:theme`. Throws `ex-info` for a
  missing cell (`:value`), fewer than 2 rows or columns (`:rows`), a bad `:tree-size`.

  Equivalents: R `heatmap`, `pheatmap` (without annotations; the row tree on the right as cljplot2 margins)."
  [rows {:keys [method distance scales title row-tree? col-tree? tree-size] user-theme :theme
         :or {method :complete distance :euclidean row-tree? true col-tree? true tree-size [0.2 0.2]} :as options}]
  (let [[tw th] (tree-size! tree-size)
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
                               :scales {:x {:domain [-0.5 (- (double n) 0.5)] :nice false :expand [0.0 0.0] :axis false}
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
                           :widths (cond-> [(- 1.0 (double (if row-tree? tw 0.0)))] row-tree? (conj (double tw)) true (conj :measured))}
                    col-tree? (assoc :heights [(double th) (- 1.0 (double th))])
                    title (assoc :title title)))))

(defn bundle
  "Hierarchical edge bundling plot of `edges` (rows between leaves of `tree` - a [[cljplot2.data.tree/from-parents]]
  tree, leaves by `:id`; `:from` / `:to` accessors) as a plot map: leaves around a circle (the radial layout of the
  tree), each edge a B-spline along the tree path between its leaves, straightened by `:beta` (default 0.85, see
  [[cljplot2.data.tree/bundle-paths]]); edges coloured by the top-level group of their source leaf (`:color-by-group?`,
  default `true`; else `:edge-color`), with `:alpha` (default 0.4; the edges only - leaf points and legend keys stay
  opaque); leaf names outward around the circle
  (`:labels?`, default `true`). Edges with an end that isn't a leaf are dropped (see `bundle-paths` for the count).
  `:title`, `:theme`.

  Equivalents: ggraph `geom_conn_bundle` (`tension`), d3 `curveBundle.beta`; Holten (2006)."
  [edges tree {:keys [from to beta alpha edge-color title] user-theme :theme
               :or {beta 0.85 alpha 0.4 edge-color [70 110 180]} :as options}]
  (let [labels? (get options :labels? true)
        by-group? (get options :color-by-group? true)
        laid (tree/layout tree {:circular true})
        nodes (:nodes laid)
        top-group (fn [i] (let [chain (vec (take-while some? (iterate #(:parent (nodes %)) i)))]
                            (:name (nodes (nth chain (max 0 (- (count chain) 2)))))))
        leaf-group (into {} (keep-indexed (fn [i n] (when (:leaf? n) [(:id n) (str (top-group i))])) nodes))
        {:keys [paths]} (tree/bundle-paths edges laid (cond-> {:beta beta} from (assoc :from from) to (assoc :to to)))
        from-of (data/row-accessor :from (or from :from))
        paths (mapv #(assoc % ::group (leaf-group (from-of %))) paths)
        leaves (mapv #(assoc % ::group (leaf-group (:id %))) (filter :leaf? nodes))]
    (cond-> {:charts (cond-> [(chart/line (cond-> {:data paths :sort false :width 1 :stroke-alpha (double alpha)
                                                   :mapping (cond-> {:x :x :y :y :group :id} by-group? (assoc :color ::group))}
                                            (not by-group?) (assoc :color edge-color)))
                              (chart/scatter {:data leaves :size 3 :mapping (cond-> {:x :x :y :y} by-group? (assoc :color ::group))})]
                       labels? (into (radial-labels nodes)))
             :aspect :iso
             :scales (cond-> {:x circle-axis :y circle-axis} by-group? (assoc :color {:title nil}))
             :labels {:title title :x nil :y nil}}
      user-theme (update :theme #(theme/deep-merge % user-theme)))))

;; ordered layouts

(defn chord
  "Chord diagram of `edges` (rows; see [[cljplot2.data.graph/graph]]) as a plot map (see
  [[cljplot2.data.graph/chord-layout]] - d3 `chord` for undirected graphs, `chordDirected` with `:directed? true`):
  a ring sector per node coloured by node (`chart/polygon`), ribbons in their source node's colour at `:alpha`
  (default 0.6), node names outward from the sectors' middles (`:labels?`, default `true`; nodes without flow
  unlabelled); equal axes, no axes or legend.

  `options`: graph - `:from`, `:to`, `:value`, `:nodes`, `:id`, `:directed?`; layout - `:order` (`nil`, ids,
  `:cluster`; see [[cljplot2.data.graph/node-order]]), `:pad` (degrees, default 2), `:inner` (default 0.92),
  `:min-value` (weaker flows left out - summed per pair, see `chord-layout`); `:scales` (e.g. `:color`), `:title`, `:theme`.

  Equivalents: d3 `chord` / `chordDirected` + `ribbon`, R `circlize::chordDiagram` (without tracks or ticks)."
  [edges {:keys [alpha scales title] user-theme :theme :or {alpha 0.6} :as options}]
  (let [labels? (get options :labels? true)
        g (graph/graph edges options)
        {:keys [groups arcs ribbons]} (graph/chord-layout g (select-keys options [:order :pad :inner :min-value]))
        label-nodes (vec (for [{:keys [id start end value]} groups :when (pos? (double value))]
                           {:name id :leaf? true :angle (* 0.5 (+ (double start) (double end)))}))]
    (cond-> {:charts (cond-> [(chart/polygon {:data ribbons :alpha (double alpha) :mapping {:x :x :y :y :group :ribbon :color :from}})
                              (chart/polygon {:data arcs :mapping {:x :x :y :y :color :group}})]
                       labels? (into (radial-labels label-nodes)))
             :aspect :iso
             ;; colours in ring order, the same for sectors and ribbons
             :scales (theme/deep-merge {:x circle-axis :y circle-axis :color {:domain (mapv :id groups) :legend false}} scales)
             :labels {:title title :x nil :y nil}}
      user-theme (assoc :theme user-theme))))

(def ^:private arc-edge-color [120 120 120])
(def ^:private arc-width-range {:range [0.5 4]})
(def ^:private ^:const arc-room
  "Room below the node line and above the highest arc (a fraction of its height): nodes and the top arc stay inside
  the panel."
  0.04)

(defn arc
  "Arc diagram of `edges` (rows; see [[cljplot2.data.graph/graph]]) as a plot map (see
  [[cljplot2.data.graph/arc-layout]]): nodes on a line in node order (scatter), every edge a semicircle above it (a
  line chart path) at `:alpha` (default 0.5, the stroke only), node names under the line as x tick labels rotated to
  read downward (`:labels?`, default `true`); no y axis.

  `options`: graph - `:from`, `:to`, `:value`, `:nodes`, `:id`, `:directed?`; `:order` (`nil`, ids, `:cluster`; see
  [[cljplot2.data.graph/node-order]]); `:mapping` - node aesthetics (`:color`, `:size`, `:shape`; nodes have
  `:degree`, `:in`, `:out` and their row's columns), `:edge-mapping` - edge aesthetics (default `{:size :value}` -
  widths from the size scale `{:range [0.5 4]}` - when the edge weights differ and the nodes don't map `:size`); an
  aesthetic in both throws `ex-info {:key :edge-mapping}`; `:node-style` (scatter options over `{:size 8 :stroke-color :white :stroke-width 1}`),
  `:edge-style` (line options over `{:color [120 120 120]}`), `:scales`, `:title`, `:theme`.

  Equivalents: ggraph `layout = \"linear\"` + `geom_edge_arc`, arcdiagram (R)."
  [edges {:keys [order mapping edge-mapping alpha scales title node-style edge-style] user-theme :theme
          :or {alpha 0.5} :as options}]
  (let [labels? (get options :labels? true)
        g (graph/graph edges options)
        weighted? (< 1 (count (distinct (map :value (:edges g)))))
        edge-mapping (if (contains? options :edge-mapping)
                       edge-mapping
                       (when (and weighted? (not (contains? mapping :size))) {:size :value}))
        _ (check-mappings! mapping edge-mapping)
        {:keys [nodes arcs]} (graph/arc-layout g {:order order})
        merged-theme (theme/deep-merge theme/default-theme user-theme)
        top (double (reduce max 0.0 (map :y arcs)))
        top (if (pos? top) top 1.0)]
    (cond-> {:charts [(chart/line (merge {:color arc-edge-color :width 1}
                                         edge-style
                                         {:data arcs :sort false :stroke-alpha (double alpha)
                                          :mapping (merge {:x :x :y :y :group :edge} edge-mapping)}))
                      (chart/scatter (merge (node-options merged-theme (merge {:size 8} node-style))
                                            {:data nodes :mapping (merge {:x :x :y :y} mapping)}))]
             :scales (cond-> (theme/deep-merge {:x (leaf-axis (mapv (fn [n] {:name (:id n)}) nodes) labels?)
                                                :y {:domain [(- (* arc-room top)) (* (+ 1.0 arc-room) top)] :nice false
                                                    :expand [0.0 0.0] :axis false}}
                                               scales)
                       (:size edge-mapping) (update :size #(merge arc-width-range %)))
             :labels {:title title :x nil :y nil}}
      labels? (assoc-in [:theme :axis :x :label-rotation] 90)
      user-theme (update :theme #(theme/deep-merge % user-theme)))))
