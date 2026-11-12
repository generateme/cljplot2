(ns cljplot2.facet
  "Facets: splitting rows into panels by facet values.

  Mapping keys: `:facet` (wrap: one dimension laid out row by row) or `:facet-col` / `:facet-row` (grid: one
  column per `:facet-col` value, one row per `:facet-row` value; either alone gives one row or one column).

  Levels (facet values in panel order) come from an explicit `:scales {k {:domain [...]}}` or from the data,
  sorted. `nil` values go to an NA level placed last (`:facets {:na-panel false}` drops them instead). Values
  outside an explicit domain are dropped. Strip text is `(str value)` (`\"NA\"` for the NA level) unless the
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

(defn- sorted-levels
  [k values]
  (try (vec (sort (distinct values)))
       (catch ClassCastException _
         (invalid "Facet values must be mutually comparable to be sorted (or give an explicit :domain)."
                  {:key :facet :scale k}))))

(defn levels
  "Panel-ordered levels of facet dimension `k` from the facet values `values` (`nil` allowed) and the facet scale
  `spec` (`:domain`), with NA handling per `na-panel?`."
  [k values spec na-panel?]
  (let [explicit (:domain spec)
        base (if explicit
               (do (when-not (and (sequential? explicit) (seq explicit) (apply distinct? explicit))
                     (invalid "Facet :domain must be a non-empty sequence of distinct values." {:key :domain :scale k}))
                   (vec explicit))
               (sorted-levels k (remove nil? values)))]
    (if (and na-panel? (some nil? values)) (conj base na-level) base)))

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
  "`[rows cols]` with `rows * cols >= n` closest to a square, `rows <= cols` (old cljplot rule)."
  [^long n]
  (if (<= n 1)
    [1 1]
    (let [s (m/sqrt n)
          [_ ^long x ^long y] (first (sort (for [x (range 1 (inc n)) y (range 1 (inc n))
                                                 :let [x (long x) y (long y) d1 (- (* x y) n)]
                                                 :when (>= d1 0)]
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

(defn valid-facet-value?
  "Facet values may be anything comparable or `nil` (NA); this only rejects invalid numbers."
  [v]
  (or (not (number? v)) (data/valid-value? v)))
