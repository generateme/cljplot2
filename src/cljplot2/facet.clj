(ns cljplot2.facet
  "Facets: splitting rows into panels by facet values.

  Mapping keys: `:facet` (wrap: one dimension laid out row by row) or `:facet-col` / `:facet-row` (grid: one
  column per `:facet-col` value, one row per `:facet-row` value; either alone gives one row or one column).

  Levels (facet values in panel order) come from an explicit `:scales {k {:domain [...]}}` or from the data,
  sorted. `nil` values go to an NA level placed last (`:facets {:na-panel false}` drops them instead). Values
  outside an explicit domain, NaN and infinite numbers are dropped (and counted). Strip text is `(str value)` (`\"NA\"` for the NA level) unless the
  facet scale has a `:formatter`.

  Group keys: the level for wrap, `[col-level row-level]` for grid. Charts whose mapping has no facet key get
  the group key [[all-panels]] and are drawn in every panel."
  (:require [fastmath.core :as m]
            [cljplot2.data :as data]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def facet-keys #{:facet :facet-col :facet-row})
(def all-panels
  "Group key of a chart without facet mapping: its data is drawn in every panel."
  ::all-panels)
(def single-panel
  "Group key of the only panel of an unfaceted plot."
  :panel)

(def ^:private na-level
  "The NA level: rows with a `nil` facet value."
  nil)

(defn- invalid [message data] (throw (ex-info message data)))

(defn facet-dims
  "Facet keys with a non-nil accessor in `mapping`: `[:facet]`, `[:facet-col :facet-row]` (or one of them) or `[]`.
  A `nil` accessor counts as absent (a chart can opt out of the plot's faceting with `{:facet nil}`).
  `:facet` with a grid key throws `ex-info {:key :mapping}`."
  [mapping]
  (let [ks (filterv #(some? (get mapping %)) [:facet :facet-col :facet-row])]
    (when (and (some #{:facet} ks) (> (count ks) 1))
      (invalid "Mapping can't combine :facet with :facet-col / :facet-row." {:key :mapping :value ks}))
    ks))

(defn valid-facet-value?
  "Can `v` be a facet level: anything except NaN or infinite numbers (`nil` is the NA level)."
  [v]
  (or (not (number? v)) (data/valid-value? v)))

(defn levels
  "Panel-ordered levels of facet dimension `k` from the facet values `values` (`nil` allowed) and the facet scale
  `spec` (`:domain`), with NA handling per `na-panel?`."
  [k values spec na-panel?]
  (let [explicit (:domain spec)
        base (if explicit
               (do (when-not (and (sequential? explicit) (seq explicit) (apply distinct? explicit))
                     (invalid "Facet :domain must be a non-empty sequence of distinct values." {:key :domain :scale k}))
                   (vec explicit))
               (data/sorted-levels k (filter #(and (some? %) (valid-facet-value? %)) values) :facet))]
    (if (and na-panel? (some nil? values)) (conj base na-level) base)))

(def ^:private facet-spec-keys #{:domain :formatter})

(defn validate-spec!
  "Check the facet scale `spec` of dimension `k`: a map with `:domain` and/or `:formatter` (a function)."
  [k spec]
  (when-not (map? spec)
    (invalid "Facet scale spec must be a map." {:key :scales :scale k}))
  (when-let [bad (first (remove facet-spec-keys (keys spec)))]
    (invalid "Unknown facet scale attribute." {:key :scales :scale k :value bad :allowed facet-spec-keys}))
  (when (and (contains? spec :formatter) (not (fn? (:formatter spec))))
    (invalid "Facet scale :formatter must be a function." {:key :formatter :scale k})))

(defn label
  "Strip text of `level` of facet dimension `k` with facet scale `spec`."
  [spec level]
  (cond
    (nil? level) "NA"
    (:formatter spec) (str ((:formatter spec) level))
    :else (str level)))

(defn group-rows
  "Rows grouped by facet key for facet `dims` with accessors `accessors` (dim -> fn) and `levels` (dim -> vector).
  Rows whose value isn't a level are dropped; returns `{:groups {key rows} :dropped n}`."
  [rows dims accessors levels]
  (let [level-sets (into {} (map (fn [d] [d (set (levels d))]) dims))
        keyed (map (fn [row]
                     (let [vs (mapv #((accessors %) row) dims)]
                       (when (every? true? (map (fn [d v] (contains? (level-sets d) v)) dims vs))
                         [(if (= 1 (count dims)) (first vs) vs) row])))
                   rows)]
    {:groups (reduce (fn [acc [k row]] (update acc k (fnil conj []) row)) {} (remove nil? keyed))
     :dropped (count (filter nil? keyed))}))

(defn most-squared-shape
  "`[rows cols]` with `rows * cols >= n` closest to a square, `rows <= cols` (old cljplot rule: minimal
  `2 (x y - n) + (sqrt n - x)^2 + (sqrt n - y)^2`, ties by smaller x then y). For each x only y from
  `ceil(n / x)` up to `max(ceil(n / x), ceil(sqrt n) + 1)` can be minimal, so the search is `O(n)`."
  [^long n]
  (if (<= n 1)
    [1 1]
    (let [s (m/sqrt n)
          top (inc (long (m/ceil s)))
          [_ ^long x ^long y] (first (sort (for [x (range 1 (inc n))
                                                 :let [x (long x) y0 (long (m/ceil (/ (double n) (double x))))]
                                                 y (range y0 (inc (m/max y0 top)))
                                                 :let [y (long y) d1 (- (* x y) n)]]
                                             [(+ (* 2.0 d1) (m/sq (- s x)) (m/sq (- s y))) x y])))]
      [(m/min x y) (m/max x y)])))

(defn wrap-shape
  "`[rows cols]` for `n` wrapped panels with optional `columns` / `rows` from plot `:facets`."
  [^long n {:keys [columns rows]}]
  (doseq [[k v] [[:columns columns] [:rows rows]]]
    (when-not (or (nil? v) (and (integer? v) (pos? (long v))))
      (invalid "Facet :columns / :rows must be positive integers." {:key :facets :value k})))
  (let [shape (cond
                (and rows columns) [(long rows) (long columns)]
                rows [(long rows) (long (m/ceil (/ (double n) (double rows))))]
                columns [(long (m/ceil (/ (double n) (double columns)))) (long columns)]
                :else (most-squared-shape n))
        [^long r ^long c] shape]
    (when (< (* r c) n)
      (invalid "Facet layout is too small for the panels." {:key :facets :panels n :rows r :columns c}))
    shape))

(defn wrap-positions
  "`[row col]` of panel `i` (0-based) in a `rows` x `cols` wrap layout, `order` `:top-down` or `:bottom-up`."
  [^long i ^long rows ^long cols order]
  (let [r (quot i cols)
        c (rem i cols)]
    (case order
      :top-down [r c]
      :bottom-up [(- rows 1 r) c])))

(defn validate-options!
  "Check plot `:facets` options."
  [facets]
  (when-not (or (nil? facets) (map? facets))
    (invalid "Plot :facets must be a map." {:key :facets}))
  (when-not (contains? #{nil :top-down :bottom-up} (:order facets))
    (invalid "Facet :order must be :top-down or :bottom-up." {:key :facets :value (:order facets)})))
