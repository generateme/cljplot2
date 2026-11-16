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
                      {:columns 1}))"
  (:require [cljplot2.data.rows :as data]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- accessors
  [{:keys [set item]}]
  [(data/row-accessor :set set) (data/row-accessor :item item)])

(defn set-order
  "Sets of `rows` in first-appearance order (`nil` left out), or `sets` (a vector) when given."
  [rows set-of sets]
  (or sets (vec (distinct (keep set-of rows)))))

(defn memberships
  "`{:sets [s ..] :members {item #{s ..}} :items [item ..] :dropped n}` of `rows` under `mapping` (`:set`, `:item`
  accessors): every item's sets among the chosen ones (see [[set-order]]), `:items` - the items of `:members` in
  first-appearance order; `:dropped` - rows with a `nil` set or item."
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
      :dropped (- (count rows) (count valid))})))

(defn intersections
  "Exact combinations of the sets of `rows` (see the namespace doc): `[{:sets [s ..] :size n} ..]` - non-empty ones
  only, sets in set order, combinations in first-appearance order of their items."
  ([rows mapping] (intersections rows mapping nil))
  ([rows mapping sets]
   (let [{order :sets members :members items :items} (memberships rows mapping sets)
         rank (zipmap order (range))
         combinations (map #(vec (sort-by rank (members %))) items)
         sizes (frequencies combinations)]
     (mapv (fn [c] {:sets c :size (sizes c)}) (distinct combinations)))))

(defn set-sizes
  "Distinct items per set of `rows`: `{s n}` over the chosen sets (see [[set-order]]; a set without items - 0)."
  ([rows mapping] (set-sizes rows mapping nil))
  ([rows mapping sets]
   (let [{order :sets members :members} (memberships rows mapping sets)]
     (merge (zipmap order (repeat 0)) (frequencies (mapcat val members))))))

(defn upset-rows
  "Rows of an UpSet plot: `[{:combination \"A & B\" :sets [..] :size n :degree k} ..]` (see [[intersections]]), by
  size descending, then degree, then set order."
  ([rows mapping] (upset-rows rows mapping nil))
  ([rows mapping sets]
   (let [{order :sets} (memberships rows mapping sets)
         rank (zipmap order (range))]
     (->> (intersections rows mapping sets)
          (map (fn [{ss :sets n :size}] {:combination (apply str (interpose " & " ss)) :sets ss :size n :degree (count ss)}))
          (sort-by (fn [{:keys [size degree sets]}] [(- (long size)) degree (mapv rank sets)]))
          vec))))

(defn membership-rows
  "Dot-matrix rows of an UpSet plot: `{:combination c :set s :member? bool}` for every combination of [[upset-rows]]
  (in its order) and every chosen set (set order)."
  ([rows mapping] (membership-rows rows mapping nil))
  ([rows mapping sets]
   (let [{order :sets} (memberships rows mapping sets)]
     (vec (for [{:keys [combination] ss :sets} (upset-rows rows mapping sets)
                :let [in (set ss)]
                s order]
            {:combination combination :set s :member? (contains? in s)})))))
