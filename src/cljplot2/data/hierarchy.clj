(ns cljplot2.data.hierarchy
  "Hierarchies of rows for the treemap and the partition: a tree from a `:value` and the `:hierarchy` levels.

  Row path: the level values of a row up to the first `nil` or `\"\"` - the row ends at that path's last node. Rows with
  an identical path merge (values summed); with a mapped `:color` rows with the same path but different colors stay
  separate leaves. A row whose path stops at a group that also has deeper rows becomes an unnamed leaf of that group
  (after its named children).

  Node: a group `{:name :value :children}` or a leaf `{:name :value :color}`; `:value` is the sum of the leaves below.
  The root is always a group with `:name nil`; its children carry `:top-index` (see [[panel-trees]]).

  Sequences (e.g. page visits) become such level rows with [[sequence-levels]] - a flow sunburst."
  (:require [clojure.string :as str]
            [clojure2d.color :as c]
            [fastmath.core :as m]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.data.rows :as data]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec4]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def mapping-schema
  "Schema of a hierarchy chart's merged mapping: `:value` and a non-empty `:hierarchy` vector."
  [:map
   [:value {:description "node size: a positive finite number per row (other rows are dropped)"} schema/accessor]
   [:hierarchy {:description "the levels of the tree, outer level first (a row's path ends at its first nil or \"\")"}
    [:vector {:min 1} schema/accessor]]
   (schema/nilable :color {:description "leaf colour (rows with the same path but different colours stay separate leaves)"}
                   schema/accessor)])

(def label-option
  "Schema of `:group-labels` / `:leaf-labels`: a boolean or a function of the node info (see [[node-info]]) returning
  the label text."
  [:or schema/bool schema/function])

(def label-entries
  "Option entries `:group-labels` / `:leaf-labels` of the hierarchy charts (see [[label-option]])."
  (schema/in-fragment
   :hierarchy-labels
   [(schema/nilable :group-labels {:description (str "group labels: true - the node's name, false - none, or a function of the node info "
                                                     "{:name :value :depth :path :share :parent-share} returning the text")
                                   :default true}
                    label-option)
    (schema/nilable :leaf-labels {:description "leaf labels: as :group-labels, for the leaves"
                                  :default true}
                    label-option)]))

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
  (aesthetic/palette-color-fn theme top-count))

(def ^:private ^:const lightness-ceiling
  "CIELAB lightness L* that deeper levels approach (white is 100)."
  90.0)

(def ^:private ^:const lightening-per-level
  "Fraction of the way to [[lightness-ceiling]] each level below the top one goes."
  0.25)

(defn depth-shade
  "`color` of a top-level node shaded for `depth` (1 - the top level, `color` itself): L* moves towards
  [[lightness-ceiling]] by [[lightening-per-level]] per level (colors lighter than the ceiling stay)."
  [color depth]
  (if (<= (long depth) 1)
    color
    (let [^Vec4 lab (c/to-LAB color)
          l (.x lab)
          f (- 1.0 (m/pow (- 1.0 lightening-per-level) (dec (long depth))))]
      (if (< l lightness-ceiling)
        (c/clamp (c/from-LAB (Vec4. (+ l (* (- lightness-ceiling l) f)) (.y lab) (.z lab) (.w lab))))
        color))))

(defn fill-fn
  "Fill of a drawn node `{:depth :top-index :leaf? :color :name}` of a panel `{:top-count :colored? :named?}`
  (`:colored?` - a mapped `:color`, `:named?` - colors by node name): without a mapped color the top-level node's
  palette color (see [[top-color-fn]]) shaded by depth ([[depth-shade]]), the root `[:palette :na]`; with one leaves
  take their mapped color, groups and the root `[:palette :na]`; by name - every named node its name's color under
  the color scale, the root and unnamed nodes `[:palette :na]`."
  [{:keys [top-count colored? named?]} scales theme]
  (let [na (get-in theme [:palette :na])]
    (cond
      named?
      (let [color-fn (aesthetic/attribute-fn :color (:color scales) na)]
        (fn [{:keys [depth name]}] (if (or (zero? (long depth)) (nil? name)) na (or (color-fn name) na))))
      colored?
      (let [color-fn (aesthetic/attribute-fn :color (:color scales) na)]
        (fn [{:keys [leaf? color]}] (if leaf? (color-fn color) na)))
      :else
      (let [top-color (top-color-fn theme top-count)]
        (fn [{:keys [depth top-index]}]
          (if (zero? (long depth))
            na
            (depth-shade (top-color top-index) depth)))))))

(defn ordered-children
  "Children of group `node` in `order`: `:value` - descending values (ties in data order), `:data` - first
  appearance (an unnamed leaf last)."
  [node order]
  (let [cs (:children node)]
    (if (= order :data)
      cs
      (vec (sort-by (comp - double :value) cs)))))

(defn ordered-tree
  "`tree` with every group's children in `order` (see [[ordered-children]])."
  [tree order]
  (if (:children tree)
    (assoc tree :children (mapv #(ordered-tree % order) (ordered-children tree order)))
    tree))

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

;; sequences

(defn- sequence-steps
  "Steps of sequence `s`: a string split at the literal `separator`, or the elements of a sequential value; empty
  (`nil`, `\"\"`) steps removed, a final `end` marker too unless `keep-end?`."
  [s separator [end keep-end?]]
  (let [steps (cond
                (string? s) (str/split s (re-pattern (java.util.regex.Pattern/quote (str separator))))
                (sequential? s) s
                :else [])
        steps (filterv #(not (or (nil? %) (= "" %))) steps)]
    (if (and (not keep-end?) (seq steps) (= end (peek steps))) (pop steps) steps)))

(def sequence-levels-entries
  "Option entries of [[sequence-levels]] (a shared group, `:hierarchy/sequence-levels`; [[cljplot2.graphs/sequences]]
  forwards them)."
  (schema/in-fragment
   :hierarchy/sequence-levels
   [(schema/optional :sequence {:description "accessor of a row's sequence: a string split at :separator or a sequence of steps"
                                :default :sequence}
                     schema/accessor)
    (schema/nilable :value {:description "accessor of a row's weight, a finite number >= 0 (nil - 1; identical sequences summed)"
                            :cljplot2/default-doc "1 per row"}
                    schema/accessor)
    (schema/optional :separator {:description "literal text splitting a string sequence into steps" :default "-"} :string)
    (schema/optional :end {:description "the end marker (nil - none): a final step equal to it is removed unless :keep-end?" :default "end"}
                      any?)
    (schema/nilable :keep-end? {:description "keep a final end marker as a step of its own (false - a sequence ending before others continue is its group's unnamed leaf)"
                                :default false}
                    schema/bool)]))

(def sequence-levels-options
  "Options schema of [[sequence-levels]]."
  (into [:map {:closed true}] sequence-levels-entries))

(defn sequence-levels
  "Hierarchy rows of sequences (e.g. page visits) for a flow sunburst (`chart/partition` under polar, see
  [[cljplot2.graphs/sequences]]): every row's `:sequence` (an accessor of a string split at `:separator`, default
  `\"-\"`, or of a sequence of steps; empty steps left out; a final `:end` marker, default `\"end\"`, removed - a
  sequence ending before others continue becomes its group's unnamed leaf - or kept as a step of its own with
  `:keep-end? true`) with `:value` (an accessor, default 1 per row; see [[cljplot2.data.rows/weight]]; summed over
  identical sequences). Returns `{:rows [{:step-1 s1 .. :step-k sk :value v}] :hierarchy [:step-1 ..
  :step-k] :dropped n}` - `k` the longest sequence, shorter ones padded with `nil`, rows in first-appearance order;
  `:dropped` - rows without steps. Throws `ex-info {:key :value}` for an invalid weight; `{:key k}` for an invalid
  option `k` (checked first, see [[sequence-levels-options]])."
  {:cljplot2/api true :cljplot2/options sequence-levels-options}
  [rows {:keys [sequence value separator end keep-end?] :or {sequence :sequence separator "-" end "end"} :as options}]
  (schema/check-options! sequence-levels-options options)
  (let [sequence-of (data/row-accessor :sequence sequence)
        value-of (if value (data/row-accessor :value value) (constantly nil))
        keyed (mapv (fn [row] [(sequence-steps (sequence-of row) separator [end keep-end?]) (data/weight (value-of row))]) rows)
        kept (filter (comp seq first) keyed)
        sums (reduce (fn [acc [steps v]] (update acc steps (fnil + 0.0) (double v))) {} kept)
        order (distinct (map first kept))
        depth (long (reduce max 0 (map count order)))
        ks (mapv #(keyword (str "step-" %)) (range 1 (inc depth)))]
    {:rows (mapv (fn [steps] (assoc (zipmap ks (concat steps (repeat nil))) :value (sums steps))) order)
     :hierarchy ks
     :dropped (- (count keyed) (count kept))}))
