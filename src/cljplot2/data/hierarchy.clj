(ns cljplot2.data.hierarchy
  "Hierarchies of rows for the hierarchy charts (treemap, partition, pack): a tree from a `:value` and the `:hierarchy` levels.

  A row's path is its level values up to the first `nil` or empty text - the row ends at that path's last node. Rows with an identical path merge (values summed); with a mapped `:color` rows with the same path but different colors stay separate leaves. A row whose path stops at a group that also has deeper rows becomes an unnamed leaf of that group (after its named children).

  A node is a group `{:name :value :children}` or a leaf `{:name :value :color}`; `:value` is the sum of the leaves below. The root is always a group with `:name nil`; its children carry `:top-index` (see [[panel-trees]]).

  Functions:

  - trees: [[level-accessors]], [[path-of]], [[root]], [[panel-trees]] (a chart's trees per panel), [[ordered-children]], [[ordered-tree]].
  - fills: [[top-color-fn]], [[depth-shade]], [[fill-fn]].
  - labels: [[node-info]], [[label-text]]; schemas `label-option`, [[label-entries]], [[mapping-schema]].
  - sequences: [[sequence-levels]] - sequences (e.g. page visits) as level rows for a flow sunburst; options [[sequence-levels-options]]."
  (:require [clojure.string :as str]
            [clojure2d.color :as c]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.data.rows :as data]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec4]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def mapping-schema
  "Schema of a hierarchy chart's merged mapping: `:value` and a non-empty `:hierarchy` vector."
  [:map
   [:value {:description "The size of each row's node, a positive finite number; the chart drops other rows."} schema/accessor]
   [:hierarchy {:description "The levels of the tree, outer level first; a row's path ends at its first `nil` or `\"\"`."}
    [:vector {:min 1} schema/accessor]]
   (schema/nilable :color {:description "Colours each leaf; rows with the same path but different colours stay separate leaves."}
                   schema/accessor)])

(def ^:private label-option
  "Schema of `:group-labels` / `:leaf-labels`: a boolean or a function of the node info (see [[node-info]]) returning
  the label text."
  [:or schema/bool schema/function])

(def label-entries
  "Option entries `:group-labels` / `:leaf-labels` of the hierarchy charts (see `label-option`)."
  (schema/in-fragment
   :hierarchy-labels
   [(schema/nilable :group-labels {:description "The labels of the groups."
                                   :default true
                                   :cljplot2/value-meanings {true "The group's name." false "No group labels."}
                                   :cljplot2/form-meanings {::schema/function "A function of the node `{:name :value :depth :path :share :parent-share}` that returns the label text."}}
                    label-option)
    (schema/nilable :leaf-labels {:description "The labels of the leaves, as `:group-labels` for groups."
                                  :default true
                                  :cljplot2/value-meanings {true "The leaf's name." false "No leaf labels."}
                                  :cljplot2/form-meanings {::schema/function "A function of the node `{:name :value :depth :path :share :parent-share}` that returns the label text."}}
                    label-option)]))

(defn level-accessors
  "Makes the row accessors of the hierarchy levels.

  Parameters:

  - `hierarchy` (sequence): the `:hierarchy` mapping - level accessors, outer level first.

  Returns a vector of row functions, one per level (see [[cljplot2.data.rows/row-accessor]]).

  See also [[path-of]]."
  [hierarchy]
  (mapv #(data/row-accessor :hierarchy %) hierarchy))

(defn path-of
  "Reads a row's path in the hierarchy.

  Parameters:

  - `accessors` (sequence of functions): the level accessors (see [[level-accessors]]).
  - `row` (map): a data row.

  Returns the vector of the row's level values up to the first `nil` or empty text (excluded); empty when the first level has none."
  [accessors row]
  (vec (take-while #(not (or (nil? %) (= "" %))) (map #(% row) accessors))))

(defn- leaves
  "Makes the leaves of the entries ending at a node.

  Parameters:

  - `node-name`: the node's name.
  - `entries` (sequence of maps): `{:value v :color c}` entries ending here.

  Returns a vector of leaves `{:name node-name :value v :color c}`, one per distinct colour value in first-appearance order with the values summed - rows differing only in colour stay separate rectangles."
  [node-name entries]
  (let [by-color (group-by :color entries)]
    (mapv (fn [col] {:name node-name :value (v/sum (map :value (by-color col))) :color col})
          (distinct (map :color entries)))))

(defn- nodes
  "Builds the nodes of the entries below a name.

  Parameters:

  - `node-name`: the nodes' name.
  - `entries` (sequence of maps): `{:path rest-of-path :value v :color c}` entries, paths relative to the node.

  Returns a vector: the leaves of `node-name` when no entry goes deeper (see [[leaves]]), otherwise one group `{:name :value :children}` whose children are the deeper nodes (first-appearance order) followed by unnamed leaves for the entries ending at the group."
  [node-name entries]
  (let [ends (filter #(empty? (:path %)) entries)
        deeper (remove #(empty? (:path %)) entries)]
    (if (empty? deeper)
      (leaves node-name ends)
      (let [by-first (group-by #(first (:path %)) deeper)
            children (-> (into [] (mapcat (fn [k] (nodes k (map #(update % :path subvec 1) (by-first k)))))
                               (distinct (map #(first (:path %)) deeper)))
                         (into (leaves nil ends)))]
        [{:name node-name :value (v/sum (map :value children)) :children children}]))))

(defn root
  "Builds the root group of hierarchy entries.

  Parameters:

  - `entries` (sequence of maps, at least one): `{:path p :value v :color c}`, `p` a path from the root (see [[path-of]]).

  Returns the root group `{:name nil :value v :children [..]}` with the top-level nodes as its children - always a group, also for a single leaf.

  See also [[panel-trees]]."
  [entries]
  (let [[n & more] (nodes nil entries)]
    (if (and (:children n) (nil? more))
      n
      {:name nil :value (v/sum (map :value (cons n more))) :children (vec (cons n more))})))

(defn panel-trees
  "Builds the trees of a hierarchy chart's panels.

  Only rows with a positive finite value (and a kept colour) count.

  Parameters:

  - `groups` (map): panel -> rows.
  - `mapping` (map): the merged mapping - `:value`, `:hierarchy`, optional `:color`.
  - `env` (map): `:plot-scales` - the plot's scale specs (the `:color` one decides which colours are kept); `:na-level` - keep rows with a `nil` colour (default true).

  Returns `{:trees {panel root} :top-count n :extents {panel {:color e}} :na #{..} :dropped {:value n :color n}}` - panels without kept rows left out; top-level children carry `:top-index` (first-appearance order over all panels, so default colours agree across panels, `:top-count` of them); `:na` - `#{:color}` when a leaf has a `nil` colour; `:dropped` - rows without a valid value, and (with a mapped colour) rows whose colour isn't kept.

  Throws `ex-info {:key :mapping}` when rows exist but none has a finite mapped `:value`, or none a valid mapped `:color` (see [[cljplot2.scale.aesthetic/valid-value?]]).

  See also [[root]], [[fill-fn]]."
  [groups mapping {:keys [na-level plot-scales] :or {na-level true}}]
  (let [value-of (data/row-accessor :value (:value mapping))
        accessors (level-accessors (:hierarchy mapping))
        color-of (when (some? (:color mapping)) (data/row-accessor :color (:color mapping)))
        color-spec (get plot-scales :color {})
        value-ok? #(let [value (value-of %)] (and (schema/finite? value) (m/pos? (double value))))
        color-ok? (fn [row] (or (nil? color-of)
                                (let [cv (color-of row)]
                                  (if (nil? cv)
                                    na-level
                                    (aesthetic/kept-value? :color color-spec cv)))))
        kept? #(and (value-ok? %) (color-ok? %))
        all-rows (mapcat val groups)
        _ (data/check-mapped! all-rows (cond-> [] (some? (:value mapping)) (conj [:value value-of]) color-of (conj [:color color-of]))
                              (fn [k v] (if (= :value k) (schema/finite? v) (aesthetic/valid-value? :color color-spec v))))
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
  "Makes the default fill function of the top-level nodes.

  Parameters:

  - `theme` (map): the merged theme; its `[:palette :color]` gives the colours.
  - `top-count` (long): the number of top-level nodes.

  Returns a function of a node's `:top-index` giving its palette colour.

  See also [[fill-fn]], [[depth-shade]]."
  [theme top-count]
  (aesthetic/palette-color-fn theme top-count))

(def ^{:private true :const true :tag 'double} lightness-ceiling
  "CIELAB lightness L* that deeper levels approach (white is 100)."
  90.0)

(def ^{:private true :const true :tag 'double} lightening-per-level
  "Fraction of the way to [[lightness-ceiling]] each level below the top one goes."
  0.25)

(def ^{:private true :const true :tag 'double} remaining-per-level
  "Fraction of the way to [[lightness-ceiling]] left after each level below the top one."
  (m/- 1.0 lightening-per-level))

(defn depth-shade
  "Shades a top-level node's colour for a deeper level.

  CIELAB lightness L* moves towards 90 by a quarter of the remaining way per level below the top one.

  Parameters:

  - `color`: the top-level node's colour.
  - `depth` (long): the node's depth (1 - the top level).

  Returns `color` itself at depth 1 or less, or when it is lighter than the ceiling; else the lightened colour, alpha kept.

  See also [[fill-fn]]."
  [color ^long depth]
  (if (m/<= depth 1)
    color
    (let [^Vec4 lab (c/to-LAB color)
          l (.x lab)
          f (m/- 1.0 (m/pow remaining-per-level (m/long-dec depth)))]
      (if (m/< l lightness-ceiling)
        (c/clamp (c/from-LAB (Vec4. (m/+ l (m/* (m/- lightness-ceiling l) f)) (.y lab) (.z lab) (.w lab))))
        color))))

(defn fill-fn
  "Makes the fill function of a hierarchy chart's drawn nodes.

  Parameters:

  - `panel` (map): `:top-count` - the number of top-level nodes; `:colored` - `:color` is mapped; `:named` - colours by node name.
  - `scales` (map): the plot's aesthetic scales; `:color` used when colouring by a mapped colour or by name.
  - `theme` (map): the merged theme; `[:palette :na]` fills the root and nodes without a colour.

  Returns a function of a drawn node `{:depth :top-index :leaf :color :name}` giving its fill: by name - every named node its name's colour under the colour scale, the root and unnamed nodes the NA colour; with a mapped colour - leaves their colour under the scale, groups and the root the NA colour; else the top-level node's palette colour ([[top-color-fn]]) shaded by depth ([[depth-shade]]), the root the NA colour."
  [{:keys [top-count colored named]} scales theme]
  (let [na (get-in theme [:palette :na])]
    (cond
      named
      (let [color-fn (aesthetic/attribute-fn :color (:color scales) na)]
        (fn [{:keys [^long depth] node-name :name}] (if (or (m/zero? depth) (nil? node-name)) na (or (color-fn node-name) na))))
      colored
      (let [color-fn (aesthetic/attribute-fn :color (:color scales) na)]
        (fn [{:keys [leaf color]}] (if leaf (color-fn color) na)))
      :else
      (let [top-color (top-color-fn theme top-count)]
        (fn [{:keys [^long depth top-index]}]
          (if (m/zero? depth)
            na
            (depth-shade (top-color top-index) depth)))))))

(defn ordered-children
  "Orders a group's children.

  Parameters:

  - `node` (map): a group.
  - `order` (keyword): `:value` - descending values (ties in data order), `:data` - first appearance (an unnamed leaf last).

  Returns the vector of the children in that order.

  See also [[ordered-tree]]."
  [node order]
  (let [cs (:children node)]
    (if (= order :data)
      cs
      (vec (sort-by #(m/- (double (:value %))) cs)))))

(defn ordered-tree
  "Orders the children of every group of a tree.

  Parameters:

  - `tree` (map): the root.
  - `order` (keyword): `:value` or `:data`, see [[ordered-children]].

  Returns `tree` with every group's children in `order`."
  [tree order]
  (if (:children tree)
    (assoc tree :children (mapv #(ordered-tree % order) (ordered-children tree order)))
    tree))

(defn node-info
  "Describes a node for label functions.

  Parameters:

  - `node` (map): the node.
  - `depth` (long): its depth (the root 0).
  - `path` (vector): its level values from the root.
  - `parent-value` (double): its parent's value (the root's own value for the root).
  - `total` (double): the tree's total value.

  Returns `{:name :value :depth :path :share :parent-share}` - `:share` of the total, `:parent-share` of the parent (1.0 for the root).

  See also [[label-text]]."
  [node depth path parent-value total]
  (let [value (double (:value node))]
    {:name (:name node) :value value :depth depth :path path
     :share (m// value (double total))
     :parent-share (m// value (double parent-value))}))

(defn label-text
  "Gives the text of a node's label.

  Parameters:

  - `labels`: the `:leaf-labels` / `:group-labels` option - `true` (the node's name), a function of `info` (its result), `false` or `nil` (none).
  - `node` (map): the node.
  - `info` (map): the node's description (see [[node-info]]).

  Returns the label as text; `nil` for no label - also for an unnamed node and for `nil` or empty results."
  [labels node info]
  (when (some? (:name node))
    (let [t (cond (fn? labels) (labels info)
                  labels (:name node))]
      (when (and (some? t) (not= "" t))
        (str t)))))

;; sequences

(defn- sequence-steps
  "Splits a sequence into its steps.

  Parameters:

  - `s`: the sequence - a string or a sequential value (anything else has no steps).
  - `separator`: the literal text a string is split at.
  - `[end keep-end?]`: the end marker and whether a final one is kept.

  Returns the vector of steps without empty ones (`nil`, empty text) and without a final `end` marker unless `keep-end?`."
  [s separator [end keep-end?]]
  (let [raw-steps (cond
                    (string? s) (str/split s (re-pattern (java.util.regex.Pattern/quote (str separator))))
                    (sequential? s) s
                    :else [])
        steps (filterv #(not (or (nil? %) (= "" %))) raw-steps)]
    (if (and (not keep-end?) (seq steps) (= end (peek steps))) (pop steps) steps)))

(def sequence-levels-entries
  "Option entries of [[sequence-levels]] (a shared group, `:hierarchy/sequence-levels`; [[cljplot2.graphs/sequences]]
  forwards them)."
  (schema/in-fragment
   :hierarchy/sequence-levels
   [(schema/optional :sequence {:description "The accessor of a row's sequence: a string the function splits at `:separator`, or a sequence of steps."
                                :default :sequence :cljplot2/example :path}
                     schema/accessor)
    (schema/nilable :value {:description "The accessor of a row's weight, a finite number zero or greater; a `nil` weight counts as 1. The function sums the weights of identical sequences."
                            :cljplot2/default-doc "Every row weighs 1."
                            :cljplot2/example :n}
                    schema/accessor)
    (schema/optional :separator {:description "The literal text that splits a string sequence into steps." :default "-" :cljplot2/example "-b-"}
                     :string)
    (schema/optional :end {:description "The text of a final step that marks a sequence's end; it acts only with `:keep-end false`, which removes it. `nil` means no end marker."
                           :default "end" :cljplot2/example "c" :cljplot2/example-context {:keep-end false}}
                     any?)
    (schema/nilable :keep-end {:description "Whether a final end marker stays a step of its own."
                               :default true
                               :cljplot2/value-meanings {true "The marker stays a step: a sequence ending before others continue gets a leaf of its own."
                                                         false "The function removes the marker: a sequence ending before others continue is its group's unnamed leaf."}}
                    schema/bool)]))

(def sequence-levels-options
  "Options schema of [[sequence-levels]]."
  (into [:map {:closed true}] sequence-levels-entries))

(defn sequence-levels
  "Turns sequences (e.g. page visits) into hierarchy rows for a flow sunburst (`chart/partition` under polar, see [[cljplot2.graphs/sequences]]).

  Parameters:

  - `rows`: rows or columns (see [[cljplot2.data.rows/rows]]), one sequence per row.
  - `options` (map, optional):
    - `:sequence` - accessor of a row's sequence: a string split at `:separator`, or a sequence of steps; empty steps left out (default `:sequence`).
    - `:value` - accessor of a row's weight, a finite number >= 0 (see [[cljplot2.data.rows/weight]]); identical sequences summed (default 1 per row).
    - `:separator` - literal text splitting a string sequence (default `-`).
    - `:end` - the end marker, the text of a final step that ends a sequence (default `end`; `nil` - no end marker); see `:keep-end` for what becomes of it.
    - `:keep-end` - a final end marker stays a step of its own (default true); `false` - it is removed, so a sequence ending before others continue becomes its group's unnamed leaf.

  Returns `{:rows [{:step-1 s1 .. :step-k sk :value v} ..] :hierarchy [:step-1 .. :step-k] :dropped n}` - `k` the longest sequence, shorter ones padded with `nil`, rows in first-appearance order; `:dropped` - rows without steps.

  Throws `ex-info {:key :value}` for an invalid weight; `{:key k}` for an invalid option `k` (checked first, see [[sequence-levels-options]]). Data that are neither rows (a sequence of maps) nor columns throw `{:key :data}` (see [[cljplot2.data.rows/rows]])."
  {:cljplot2/api true
   :cljplot2/arguments [{:name 'rows :description "The data, each row holding one sequence: rows (a sequence of maps) or columns (a map of equal-length sequences)." :columns {:sequence "The sequence: a string of steps joined by `:separator`, or a sequence of steps; the option `:sequence` names another column." "a weight column" "The row's weight, named by the option `:value`; optional."}}]
   :cljplot2/returns "`{:rows :hierarchy :dropped}`: a row per distinct sequence with a column per step (`:step-1` ...) and the summed `:value`, the step keys, and the number of rows without steps."
   :cljplot2/options sequence-levels-options}
  ([rows] (sequence-levels rows {}))
  ([rows {:keys [value separator end] sequence-accessor :sequence :or {sequence-accessor :sequence separator "-" end "end"} :as options}]
   (schema/check-options! sequence-levels-options options)
   (let [rs (data/rows rows)
         keep-end? (data/option options :keep-end true)
         sequence-of (data/row-accessor :sequence sequence-accessor)
         value-of (if value (data/row-accessor :value value) (constantly nil))
         keyed (mapv (fn [row] [(sequence-steps (sequence-of row) separator [end keep-end?]) (data/weight (value-of row))]) rs)
         kept (filter (comp seq first) keyed)
         sums (reduce (fn [acc [steps weight]] (update acc steps (fnil m/+ 0.0) (double weight))) {} kept)
         order (distinct (map first kept))
         depth (long (reduce m/max 0 (map count order)))
         ks (mapv #(keyword (str "step-" %)) (range 1 (m/long-inc depth)))]
     {:rows (mapv (fn [steps] (assoc (zipmap ks (concat steps (repeat nil))) :value (sums steps))) order)
      :hierarchy ks
      :dropped (m/- (count keyed) (count kept))})))
