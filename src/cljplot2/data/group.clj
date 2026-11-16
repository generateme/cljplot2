(ns cljplot2.data.group
  "Groups within a panel for charts drawing one mark per group (histogram, density, line, ECDF).

  Grouping dimensions: mapped aesthetics (see [[cljplot2.scale.aesthetic]]) whose scale is ordinal - categorical or
  temporal data, or `:kind :ordinal` / an ordinal explicit domain in the plot's `:scales` - and `:group` (splits
  without styling; no scale, no legend). A mapped aesthetic whose scale would be linear (numeric data) can't group:
  `ex-info {:key :mapping}`.

  A group is `{:key [v ...] :aesthetics {dim v} :rows [row ...]}`: `:key` - values of the grouping dimensions in
  [[group-dims]] order, `:aesthetics` - the aesthetic values (`nil` = NA). `nil` values form the NA group (dropped
  and counted when the plot's `:groups {:na false}`); other invalid values (NaN, infinite, ...) and aesthetic values
  outside an explicit plot scale domain ([[cljplot2.scale.aesthetic/in-domain?]]) are dropped and counted. Groups are
  ordered by [[ordered-keys]]."
  (:require [cljplot2.schema :as schema]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.data.rows :as data]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn group-dims
  "Grouping dimensions of `mapping`: mapped aesthetics, then `:group`."
  [mapping]
  (cond-> (aesthetic/mapped-dims mapping) (some? (:group mapping)) (conj :group)))

(defn- check-groupable!
  "Throw when aesthetic `k` would get a linear or binned scale (numeric values without an ordinal spec)."
  [k values plot-scales]
  (when (aesthetic/dim? k)
    (let [present (remove nil? values)
          numeric? (and (seq present) (every? number? present))]
      (when (#{:linear :binned} (aesthetic/inferred-kind k (get plot-scales k {}) numeric?))
        (schema/fail! "This chart groups by aesthetics; numeric values must be declared categorical ({:scales {k {:type :categorical}}} or {:mapping {k (levels/categorical acc)}})."
                        {:key :mapping :value k})))))

(defn- kept-value?
  "Is grouping value `v` of dimension `k` kept: valid (aesthetics: kept by the plot scale, see
  [[cljplot2.scale.aesthetic/kept-value?]]), or NA."
  [k na-level? plot-scales v]
  (or (if (aesthetic/dim? k) (aesthetic/kept-value? k (get plot-scales k {}) v) (data/valid-value? v))
      (and na-level? (nil? v))))

(defn- level-rank
  "Sort rank of `v` in dimension `k`: its index in an explicit ordinal domain (else `v`), NA last."
  [domain-index v]
  (cond
    (nil? v) [2 nil]
    domain-index [0 (get domain-index (if (number? v) (double v) v) Long/MAX_VALUE)]
    :else [1 v]))

(defn ordered-keys
  "Distinct group `keys` (vectors over `dims`, in first-appearance order) ordered dimension by dimension: explicit
  ordinal `:domain` order from `plot-scales`, identity scales in first-appearance order, otherwise natural order;
  NA (`nil`) last. Incomparable values keep first-appearance order."
  [keys dims plot-scales]
  (let [ks (vec (distinct keys))
        indices (vec (map-indexed (fn [i k]
                                    (let [spec (get plot-scales k)
                                          domain (:domain spec)
                                          level-key #(if (number? %) (double %) %)]
                                      (cond
                                        (and (aesthetic/dim? k) (aesthetic/identity-scale? spec))
                                        (zipmap (distinct (keep #(some-> (nth % i) level-key) ks)) (range))
                                        (and (sequential? domain) (aesthetic/dim? k))
                                        (zipmap (map level-key domain) (range)))))
                                  dims))]
    (try (vec (sort-by (fn [key] (mapv level-rank indices key)) ks))
         (catch ClassCastException _ ks))))

(defn split
  "Rows of every panel (`groups` - `{panel rows}`) split into groups by the grouping dimensions of `mapping`.
  `env` - `:na-level?` (default true) and `:plot-scales` (the plot's `:scales`).
  Returns `{:panels {panel [group ...]} :dims [dim ...] :dropped {dim n}}`, groups of a panel in [[ordered-keys]]
  order. Throws `ex-info {:key :mapping}` for an aesthetic that can't group or has no valid value in non-empty data."
  [groups mapping {:keys [na-level? plot-scales] :or {na-level? true}}]
  (let [dims (group-dims mapping)
        accessors (mapv #(data/row-accessor % (mapping %)) dims)
        all-rows (apply concat (vals groups))
        row-count (count all-rows)
        columns (mapv (fn [acc] (mapv acc all-rows)) accessors)]
    (doseq [[k vs] (map vector dims columns)]
      (when (and (pos? row-count) (aesthetic/dim? k) (not-any? #(aesthetic/valid-value? k (get plot-scales k) %) vs))
        (schema/fail! "No valid values for a mapped dimension - check the mapping." {:key :mapping :value k :rows row-count}))
      (check-groupable! k (filter data/valid-value? vs) plot-scales))
    (let [key-of (fn [row] (mapv #(% row) accessors))
          kept? (fn [key] (every? true? (map #(boolean (kept-value? %1 na-level? plot-scales %2)) dims key)))
          panels (update-vals groups
                              (fn [rows]
                                (let [keyed (keep (fn [row] (let [key (key-of row)] (when (kept? key) [key row]))) rows)
                                      by-key (reduce (fn [acc [key row]] (update acc key (fnil conj []) row)) {} keyed)]
                                  (mapv (fn [key] {:key key
                                                   :aesthetics (into {} (keep (fn [[k v]] (when (aesthetic/dim? k) [k v])))
                                                                     (map vector dims key))
                                                   :rows (by-key key)})
                                        (ordered-keys (distinct (map first keyed)) dims plot-scales)))))]
      {:panels panels
       :dims dims
       :dropped (into {} (map (fn [k vs] [k (count (remove #(kept-value? k na-level? plot-scales %) vs))]) dims columns))})))

(defn continuous
  "A linear or binned aesthetic `k` (`:color`, `:size`) of chart `mapping` - styled per row, not grouped - resolved as
  the plot scale will resolve it (from the plot's scale spec in `env` `:plot-scales` and the values in `groups`,
  `{panel rows}`); `nil` when `k` is unmapped or groups. Returns `{:groups :mapping :value-of :dropped}`: `groups`
  without the rows outside the scale's explicit domain (and NA rows when `env` `:na-level?` is false), `mapping`
  without `k` (to split by the other dimensions), the row accessor of the value and the count of rows dropped."
  [k groups mapping {:keys [plot-scales na-level?] :or {na-level? true}}]
  (when-let [acc (get mapping k)]
    (let [value-of (data/row-accessor k acc)
          spec (get plot-scales k {})
          present (keep value-of (mapcat val groups))
          numeric? (and (seq present) (every? number? present))]
      (when (#{:linear :binned} (aesthetic/inferred-kind k spec numeric?))
        (let [kept? (fn [row] (let [v (value-of row)] (if (nil? v) na-level? (aesthetic/kept-value? k spec v))))]
          {:groups (update-vals groups #(filterv kept? %))
           :mapping (dissoc mapping k)
           :value-of value-of
           :dropped (count (remove kept? (mapcat val groups)))})))))

(defn continuous-color
  "[[continuous]] of `:color`, the value accessor also as `:color-of`."
  [groups mapping env]
  (when-let [c (continuous :color groups mapping env)]
    (assoc c :color-of (:value-of c))))

(defn continuous-stats
  "Stats of a chart's per-row values of aesthetic `k` (see [[continuous]]) from `panel-values` - `{panel [value
  ...]}` (NA as `nil`) of what the chart draws: `:extents` `{panel {k extent}}` of the non-NA values under the plot's
  scale spec in `plot-scales` (panels without one left out), `:dropped` - the rows `continuous` dropped, `:na?` -
  some value is NA."
  [k continuous panel-values plot-scales]
  {:extents (into {} (keep (fn [[panel vs]]
                             (when-let [e (aesthetic/value-extent k (get plot-scales k) (remove nil? vs))]
                               [panel {k e}])))
                  panel-values)
   :dropped (:dropped continuous)
   :na? (boolean (some nil? (mapcat val panel-values)))})

(defn continuous-color-stats
  "[[continuous-stats]] of `:color`."
  [continuous panel-values plot-scales]
  (continuous-stats :color continuous panel-values plot-scales))

(defn aesthetic-extents
  "Per panel of split `panels`: value extents of the non-NA aesthetic values of its groups, under the plot's
  `plot-scales` (see [[cljplot2.scale.aesthetic/value-extent]])."
  [panels plot-scales]
  (update-vals panels
               (fn [gs]
                 (into {} (keep (fn [k]
                                  (when-let [e (aesthetic/value-extent k (get plot-scales k)
                                                                       (keep #(get-in % [:aesthetics k]) gs))]
                                    [k e])))
                       aesthetic/dims))))

(defn na-dims
  "Aesthetics with an NA group in split `panels`."
  [panels]
  (set (for [gs (vals panels) g gs [k v] (:aesthetics g) :when (nil? v)] k)))

(defn style
  "`base` style of a chart with the group's mapped `aesthetics` applied through finalized `scales` (`:color` - NA
  color from theme `[:palette :na]`, `:shape`, `:size`, `:dash` - the base value when NA); `nil` when a value is outside
  its scale's domain (the group is skipped)."
  [base aesthetics scales theme]
  (reduce (fn [st [k v]]
            (if-let [scale (get scales k)]
              (let [na (if (= k :color) (get-in theme [:palette :na]) (get base k))
                    a ((aesthetic/attribute-fn k scale na) v)]
                (if (some? a) (assoc st k a) (reduced nil)))
              st))
          base aesthetics))
