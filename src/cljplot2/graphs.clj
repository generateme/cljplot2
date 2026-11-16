(ns cljplot2.graphs
  "Plot builders of graphs: a layout from [[cljplot2.data.graph]] drawn with ordinary charts, returned as a plot map
  (edit it, add charts, or build the plot from the layout yourself)."
  (:require [clojure2d.color :as c]
            [cljplot2.chart :as chart]
            [cljplot2.data.graph :as graph]
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
  (when-let [both (seq (filter #(and (contains? mapping %) (contains? edge-mapping %)) aesthetic/dims))]
    (schema/fail! "An aesthetic is mapped for both nodes and edges - one scale can't show two variables."
                  {:key :edge-mapping :value (vec both)}))
  (let [{:keys [nodes] :as laid} (graph/layout (graph/graph edges options) options)
        merged-theme (theme/deep-merge theme/default-theme user-theme)
        node-st (merge node-defaults {:color (c/set-alpha (c/to-color (get-in merged-theme [:chart :scatter :color])) 255)}
                       node-style)
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
             :scales (merge {:x x-axis :y y-axis} scales)
             :labels {:title title :x nil :y nil}}
      user-theme (assoc :theme user-theme))))
