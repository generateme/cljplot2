(ns cljplot2.data.sets
  "Set membership rows (`:set`, `:item` - one row per item in a set; repeated rows count once) and their exact
  intersections: the region counts of a Venn diagram and the rows of an UpSet plot.

  An exact combination is a set of sets: its size counts the items in exactly those sets (in no other). Sets come in
  first-appearance order unless `:sets` (a vector) chooses and orders them; rows of other sets are left out. Rows
  with a `nil` set or item are dropped.

  UpSet recipe (bars of [[upset-rows]] over a dot matrix of [[membership-rows]], one column per combination: light
  dots everywhere, dark ones for the member sets):

      (let [m {:set :artist :item :word}
            dots (sets/membership-rows data m)]
        (plot/arrange [{:data (sets/upset-rows data m) :mapping {:x :combination :y :size}
                        :charts [(chart/bar {:value-label true})]}
                       {:data dots :mapping {:x :combination :y :set}
                        :charts [(chart/scatter {:color [210 210 210]})
                                 (chart/scatter {:data (filter :member? dots) :color :black})]}]
                      {:columns 1}))

  Functions: [[memberships]] - each item's sets; [[intersections]] - exact combinations and their sizes; [[set-sizes]] - items per set; [[upset-rows]] and [[membership-rows]] - the bar and dot-matrix rows of an UpSet plot; [[set-order]] - the chosen sets in order."
  (:require [fastmath.core :as m]
            [cljplot2.data.rows :as data]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- accessors
  "Gives the row accessors of a set membership mapping.

  Parameters:

  - `mapping` (map): `:set` and `:item` accessors (keywords or functions).

  Returns `[set-of item-of]`, functions of a row."
  [{set-accessor :set item-accessor :item}]
  [(data/row-accessor :set set-accessor) (data/row-accessor :item item-accessor)])

(defn set-order
  "Gives the chosen sets in their order.

  Parameters:

  - `rows` (sequence of maps): set membership rows.
  - `set-of` (function): a row's set.
  - `sets` (vector or `nil`): the sets chosen and ordered by the user.

  Returns `sets` when given, else the sets of `rows` in first-appearance order (`nil` left out), as a vector."
  [rows set-of sets]
  (or sets (vec (distinct (keep set-of rows)))))

(defn memberships
  "Collects every item's sets among the chosen ones.

  Parameters:

  - `rows` (sequence of maps): set membership rows; repeated rows count once.
  - `mapping` (map): `:set` and `:item` accessors.
  - `sets` (vector, optional): the chosen sets in order, see [[set-order]]; rows of other sets are left out.

  Returns `{:sets [s ..] :members {item #{s ..}} :items [item ..] :dropped n}`: `:sets` - the chosen sets in order, `:items` - the items of `:members` in first-appearance order, `:dropped` - the count of rows with a `nil` set or item.

  See also [[intersections]], [[set-sizes]]."
  ([rows mapping] (memberships rows mapping nil))
  ([rows mapping sets]
   (let [[set-of item-of] (accessors mapping)
         order (set-order rows set-of sets)
         chosen (set order)
         valid (filter #(and (some? (set-of %)) (some? (item-of %))) rows)
         in-chosen (filter #(chosen (set-of %)) valid)]
     {:sets order
      :members (reduce (fn [acc row] (update acc (item-of row) (fnil conj #{}) (set-of row))) {} in-chosen)
      :items (vec (distinct (map item-of in-chosen)))
      :dropped (m/- (count rows) (count valid))})))

(defn intersections
  "Computes the exact combinations of sets and their sizes.

  An exact combination is a set of sets; its size counts the items in exactly those sets (see the namespace doc).

  Parameters:

  - `rows`, `mapping`, `sets` (optional): as in [[memberships]].

  Returns `[{:sets [s ..] :size n} ..]` - non-empty combinations only, sets in set order, combinations in first-appearance order of their items.

  See also [[upset-rows]]."
  ([rows mapping] (intersections rows mapping nil))
  ([rows mapping sets]
   (let [{order :sets members :members items :items} (memberships rows mapping sets)
         rank (zipmap order (range))
         combinations (map #(vec (sort-by rank (members %))) items)
         sizes (frequencies combinations)]
     (mapv (fn [c] {:sets c :size (sizes c)}) (distinct combinations)))))

(defn set-sizes
  "Counts the distinct items of every chosen set.

  Parameters:

  - `rows`, `mapping`, `sets` (optional): as in [[memberships]].

  Returns `{s n}` over the chosen sets (see [[set-order]]); a set without items counts 0."
  ([rows mapping] (set-sizes rows mapping nil))
  ([rows mapping sets]
   (let [{order :sets members :members} (memberships rows mapping sets)]
     (merge (zipmap order (repeat 0)) (frequencies (mapcat val members))))))

(defn upset-rows
  "Computes the bar rows of an UpSet plot.

  Parameters:

  - `rows`, `mapping`, `sets` (optional): as in [[memberships]].

  Returns `[{:combination c :sets [..] :size n :degree k} ..]` - one row per exact combination (see [[intersections]]), `c` its sets joined with ` & `, `k` the count of its sets; sorted by size descending, then degree, then set order.

  See also [[membership-rows]]."
  {:cljplot2/api true}
  ([rows mapping] (upset-rows rows mapping nil))
  ([rows mapping sets]
   (let [{order :sets} (memberships rows mapping sets)
         rank (zipmap order (range))]
     (->> (intersections rows mapping sets)
          (map (fn [{ss :sets n :size}] {:combination (apply str (interpose " & " ss)) :sets ss :size n :degree (count ss)}))
          (sort-by (fn [{:keys [size degree] combination-sets :sets}] [(m/- (long size)) degree (mapv rank combination-sets)]))
          vec))))

(defn membership-rows
  "Computes the dot-matrix rows of an UpSet plot.

  Parameters:

  - `rows`, `mapping`, `sets` (optional): as in [[memberships]].

  Returns a vector of `{:combination c :set s :member? bool}` for every combination of [[upset-rows]] (in its order) and every chosen set (in set order); `:member?` - whether `s` is in the combination."
  {:cljplot2/api true}
  ([rows mapping] (membership-rows rows mapping nil))
  ([rows mapping sets]
   (let [{order :sets} (memberships rows mapping sets)]
     (vec (for [{:keys [combination] ss :sets} (upset-rows rows mapping sets)
                :let [in (set ss)]
                s order]
            {:combination combination :set s :member? (contains? in s)})))))
