(ns cljplot2.data.hierarchy
  "Hierarchies of rows for the treemap and the partition: a tree from a `:value` and the `:hierarchy` levels.

  Row path: the level values of a row up to the first `nil` or `\"\"` - the row ends at that path's last node. Rows with
  an identical path merge (values summed); with a mapped `:color` rows with the same path but different colors stay
  separate leaves. A row whose path stops at a group that also has deeper rows becomes an unnamed leaf of that group
  (after its named children).

  Node: a group `{:name :value :children}` or a leaf `{:name :value :color}`; `:value` is the sum of the leaves below.
  The root is always a group with `:name nil`; its children carry `:top-index` (see [[panel-trees]])."
  (:require [fastmath.core :as m]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.data.rows :as data]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def mapping-schema
  "Schema of a hierarchy chart's merged mapping: `:value` and a non-empty `:hierarchy` vector."
  [:map [:value schema/accessor] [:hierarchy [:vector {:min 1} schema/accessor]]])

(def label-option
  "Schema of `:group-labels` / `:leaf-labels`: a boolean or a function of the node info (see [[node-info]]) returning
  the label text."
  [:or schema/bool schema/function])

(defn level-accessors
  "Row accessors of the `:hierarchy` mapping (a vector of accessors, outer level first)."
  [hierarchy]
  (mapv #(data/row-accessor :hierarchy %) hierarchy))

(defn path-of
  "Level values of `row` up to the first `nil` / `\"\"`."
  [accessors row]
  (vec (take-while #(not (or (nil? %) (= "" %))) (map #(% row) accessors))))

(defn- leaves
  "Leaves named `name` from `entries` ending here: one per distinct color value (first-appearance order), values
  summed - rows differing only in color stay separate rectangles."
  [name entries]
  (let [by-color (group-by :color entries)]
    (mapv (fn [col] {:name name :value (reduce + 0.0 (map :value (by-color col))) :color col})
          (distinct (map :color entries)))))

(defn- nodes
  "Nodes named `name` from `entries` (`{:path rest-of-path :value v :color c}`): the leaves of `name` when no entry
  goes deeper, otherwise one group whose children are the deeper groups (first-appearance order) followed by unnamed
  leaves for the entries ending at the group."
  [name entries]
  (let [ends (filter #(empty? (:path %)) entries)
        deeper (remove #(empty? (:path %)) entries)]
    (if (empty? deeper)
      (leaves name ends)
      (let [by-first (group-by #(first (:path %)) deeper)
            children (-> (into [] (mapcat (fn [k] (nodes k (map #(update % :path subvec 1) (by-first k)))))
                               (distinct (map #(first (:path %)) deeper)))
                         (into (leaves nil ends)))]
        [{:name name :value (reduce + 0.0 (map :value children)) :children children}]))))

(defn root
  "Root group of `entries` (`{:path :value :color}`, at least one): its children are the top-level nodes (always a
  group, also for a single leaf)."
  [entries]
  (let [[n & more] (nodes nil entries)]
    (if (and (:children n) (nil? more))
      n
      {:name nil :value (reduce + 0.0 (map :value (cons n more))) :children (vec (cons n more))})))

(defn panel-trees
  "Trees of a hierarchy chart's panels from `groups` (panel -> rows) and the merged `mapping` (`:value`,
  `:hierarchy`, optional `:color`): rows with a positive finite value (and a kept color) only.
  `{:trees {panel root} :top-count n :extents {panel {:color e}} :na #{..} :dropped {:value n :color n}}` - panels
  without kept rows left out; top-level children carry `:top-index` (first-appearance order over all panels, so
  default colors agree across panels; `:top-count` of them). `env` - `:plot-scales`, `:na-level?`."
  [groups mapping {:keys [na-level? plot-scales] :or {na-level? true}}]
  (let [value-of (data/row-accessor :value (:value mapping))
        accessors (level-accessors (:hierarchy mapping))
        color-of (when (some? (:color mapping)) (data/row-accessor :color (:color mapping)))
        color-spec (get plot-scales :color {})
        value-ok? #(let [v (value-of %)] (and (number? v) (m/valid-double? (double v)) (pos? (double v))))
        color-ok? (fn [row] (or (nil? color-of)
                                (let [cv (color-of row)]
                                  (if (nil? cv)
                                    na-level?
                                    (aesthetic/kept-value? :color color-spec cv)))))
        kept? #(and (value-ok? %) (color-ok? %))
        all-rows (mapcat val groups)
        trees (into {} (keep (fn [[panel rows]]
                               (when-let [kept (seq (filter kept? rows))]
                                 [panel (root (map (fn [r] {:path (path-of accessors r) :value (double (value-of r))
                                                            :color (when color-of (color-of r))})
                                                   kept))])))
                    groups)
        top-index (zipmap (distinct (map #(first (path-of accessors %)) (filter kept? all-rows))) (range))
        index-tops (fn [tree] (update tree :children (fn [cs] (mapv #(assoc % :top-index (top-index (:name %) 0)) cs))))
        colors (fn [tree] (when color-of (keep :color (tree-seq :children :children tree))))]
    {:trees (update-vals trees index-tops)
     :top-count (count top-index)
     :extents (into {} (keep (fn [[panel tree]]
                               (when-let [e (some->> tree colors seq (aesthetic/value-extent :color color-spec))]
                                 [panel {:color e}])))
                    trees)
     :na (if (and color-of (some (fn [tree] (some #(and (contains? % :color) (nil? (:color %)) (not (:children %)))
                                                  (tree-seq :children :children tree)))
                                 (vals trees)))
           #{:color} #{})
     :dropped (cond-> {:value (count (remove value-ok? all-rows))}
                color-of (assoc :color (count (remove color-ok? (filter value-ok? all-rows)))))}))

(defn top-color-fn
  "Default fill of the top-level node with `:top-index` i (`(f i)`): the theme's `[:palette :color]` over `top-count`
  top-level nodes."
  [theme top-count]
  (aesthetic/attribute-fn :color (aesthetic/scale-spec :color {:kind :ordinal :domain (vec (range (max 1 (long top-count))))}
                                                       nil false (:palette theme))
                          (get-in theme [:palette :na])))

(defn ordered-children
  "Children of group `node` in `order`: `:value` - descending values (ties in data order), `:data` - first
  appearance (an unnamed leaf last)."
  [node order]
  (let [cs (:children node)]
    (if (= order :data)
      cs
      (vec (sort-by (comp - double :value) cs)))))

(defn node-info
  "What label functions get for `node` at `depth` (root 0) with level values `path` from the root, in a tree of
  `total` value under a parent of `parent-value`: `{:name :value :depth :path :share :parent-share}` - `:share` of
  the total, `:parent-share` of the parent (1.0 for the root)."
  [node depth path parent-value total]
  (let [v (double (:value node))]
    {:name (:name node) :value v :depth depth :path path
     :share (/ v (double total))
     :parent-share (/ v (double parent-value))}))

(defn label-text
  "Text of a node label for a `:leaf-labels` / `:group-labels` option `labels` (`true` - the name, a function of
  `info` (see [[node-info]]) - its result, `false` / `nil` - none): `nil` for no label, also for an unnamed node,
  `nil` or `\"\"` results."
  [labels node info]
  (when (some? (:name node))
    (let [t (cond (fn? labels) (labels info)
                  labels (:name node))]
      (when (and (some? t) (not= "" t))
        (str t)))))
