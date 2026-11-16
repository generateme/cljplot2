(ns cljplot2.graphs
  "Plot builders of graphs: a layout from [[cljplot2.data.graph]] drawn with ordinary charts, returned as a plot map
  (edit it, add charts, or build the plot from the layout yourself)."
  (:require [cljplot2.chart :as chart]
            [cljplot2.data.graph :as graph]
            [cljplot2.data.rows :as data]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private edge-color [150 150 150 180])
(def ^:private ^:const frame-padding 0.06)

(defn- node-radius-fn
  "Pixel radius of a node (a shape `size` is its bounding box side): from the node size scale of `mapping` `:size`
  (`size-spec` - the plot's `:scales :size`, its domain the nodes' values; range from the merged `merged-theme`), else
  the scatter size of the theme."
  [nodes mapping size-spec merged-theme]
  (let [default (double (get-in merged-theme [:chart :scatter :size]))]
    (if-let [acc (:size mapping)]
      (let [size-of (data/row-accessor :size acc)
            values (keep size-of nodes)
            spec (aesthetic/scale-spec :size size-spec (aesthetic/value-extent :size size-spec values) false
                                       (:palette merged-theme))
            attribute (if spec (aesthetic/attribute-fn :size spec default) (constantly default))]
        (fn [node] (* 0.5 (double (or (attribute (size-of node)) default)))))
      (constantly (* 0.5 default)))))

(defn network
  "Network plot of `edges` (rows; see [[cljplot2.data.graph/graph]]) as a plot map: edges as segments under the nodes
  (scatter), labels beside them; equal units on both axes (`:aspect :iso`), no axes.

  `options`:

  * graph - `:from`, `:to`, `:value`, `:nodes`, `:id`, `:directed?` (see [[cljplot2.data.graph/graph]])
  * layout - `:layout` (`:force` default, `:circle`, `:random`), `:iterations`, `:rng` (see
    [[cljplot2.data.graph/layout]])
  * `:mapping` - node aesthetics (`:color`, `:size`, `:shape`; nodes have `:degree`, `:in`, `:out` and their row's
    columns), `:edge-mapping` - edge aesthetics (`:color`, `:size` - the width; edges have `:value` and their row's
    columns); an aesthetic in both throws `ex-info {:key :edge-mapping}` (one scale can't show two variables)
  * `:labels` - `true` (the node id) or an accessor of the node (`nil` / `\"\"` - no label), on white boxes right of
    the node
  * `:scales` - plot scales (e.g. `:color`, `:size`), `:title`, `:theme`

  Directed graphs get arrows ending at the target node's edge (a per-edge `:end-gap` of its radius, from the node size
  scale - the plot's `:scales :size` spec with the nodes' values as the domain).

  Equivalents: igraph `plot(graph, layout = layout_with_fr)`, ggraph `geom_edge_link` + `geom_node_point`. A
  correlation network: the pairs of a correlation matrix above a threshold, e.g.
  `(graph/adjacency->edges (mapv (fn [row] (mapv #(if (> (abs %) 0.7) % 0) row)) r) names {:directed? false})`
  with `:edge-mapping {:color #(if (pos? (:value %)) \"+\" \"-\")}` (drop the diagonal's self-loops - they aren't
  drawn anyway)."
  [edges {:keys [mapping edge-mapping labels scales title directed?] user-theme :theme :as options}]
  (when-let [both (seq (filter #(and (contains? mapping %) (contains? edge-mapping %)) aesthetic/dims))]
    (schema/fail! "An aesthetic is mapped for both nodes and edges - one scale can't show two variables."
                  {:key :edge-mapping :value (vec both)}))
  (let [{:keys [nodes] :as laid} (graph/layout (graph/graph edges options) options)
        merged-theme (theme/deep-merge theme/default-theme user-theme)
        radius (node-radius-fn nodes mapping (:size scales) merged-theme)
        radius-of (zipmap (map :id nodes) (map radius nodes))
        edge-rows (cond->> (:edges laid) directed? (mapv #(assoc % ::gap (+ 1.0 (double (radius-of (:to %)))))))
        label-of (cond (true? labels) :id labels (data/row-accessor :label labels))
        ;; labels right of the nodes: more room on the right
        x-axis {:domain [(- frame-padding) (+ 1.0 (if label-of (* 3.0 frame-padding) frame-padding))]
                :nice false :expand [0.0 0.0] :axis false}
        y-axis {:domain [(- frame-padding) (+ 1.0 frame-padding)] :nice false :expand [0.0 0.0] :axis false}]
    (cond-> {:charts (cond-> [(chart/segment (cond-> {:data edge-rows
                                                      :mapping (cond-> (merge {:x :x :y :y :xend :xend :yend :yend} edge-mapping)
                                                                 directed? (assoc :end-gap ::gap))}
                                               (not (contains? edge-mapping :color)) (assoc :color edge-color)
                                               directed? (assoc :arrow {:size 7})))
                              (chart/scatter {:data nodes :mapping (merge {:x :x :y :y} mapping)})]
                       label-of (conj (chart/text {:data nodes :mapping {:x :x :y :y :label label-of}
                                                   :anchor [:left :middle] :offset [6 0] :size 8
                                                   :background [255 255 255 170] :padding 1})))
             :aspect :iso
             :scales (merge {:x x-axis :y y-axis} scales)
             :labels {:title title :x nil :y nil}}
      user-theme (assoc :theme user-theme))))
