(ns cljplot2.data.group
  "Groups of rows within a panel, for charts drawing one mark per group (histogram, density, line, ECDF), and per-row continuous aesthetics.

  Grouping dimensions: mapped aesthetics (see [[cljplot2.scale.aesthetic]]) whose scale is ordinal - categorical or temporal data, or `:kind :ordinal` / an ordinal explicit domain in the plot's `:scales` - and `:group` (splits without styling; no scale, no legend). A mapped aesthetic whose scale would be linear (numeric data) can't group: `ex-info {:key :mapping}`.

  A group is `{:key [v ...] :aesthetics {dim v} :rows [row ...]}`: `:key` - values of the grouping dimensions in [[group-dims]] order, `:aesthetics` - the aesthetic values (`nil` = NA). `nil` values form the NA group (dropped and counted when the plot's `:groups {:na false}`); other invalid values (NaN, infinite, ...) and aesthetic values outside an explicit plot scale domain ([[cljplot2.scale.aesthetic/in-domain?]]) are dropped and counted. Groups are ordered by [[ordered-keys]].

  Functions:

  - grouping: [[group-dims]], [[ordered-keys]], [[split]]
  - per-row aesthetics (linear or binned scales): [[continuous]], [[continuous-color]], [[continuous-stats]], [[continuous-color-stats]]
  - aesthetics of split groups: [[aesthetic-extents]], [[na-dims]], [[style]]"
  (:require [fastmath.core :as m]
            [cljplot2.schema :as schema]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.data.rows :as data]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn group-dims
  "Gives the grouping dimensions of a mapping.

  Parameters:

  - `mapping` (map): a chart's merged mapping.

  Returns a vector: the mapped aesthetics (see [[cljplot2.scale.aesthetic/mapped-dims]]), then `:group` when mapped."
  [mapping]
  (cond-> (aesthetic/mapped-dims mapping) (some? (:group mapping)) (conj :group)))

(defn- check-groupable!
  "Checks that a mapped dimension can group.

  Parameters:

  - `k` (keyword): the grouping dimension; only aesthetics are checked.
  - `values` (collection): its valid values.
  - `plot-scales` (map): the plot's `:scales`.

  Returns `nil`.

  Throws `ex-info {:key :mapping :value k}` when aesthetic `k` would get a linear or binned scale (numeric or date values without an ordinal spec)."
  [k values plot-scales]
  (when (aesthetic/dim? k)
    (when (#{:linear :binned} (aesthetic/inferred-kind k (get plot-scales k {}) (aesthetic/continuous-values? k values)))
      (schema/fail! "This chart groups by aesthetics; numeric (or date) values must be declared categorical ({:scales {k {:type :categorical}}} or {:mapping {k (levels/categorical acc)}})."
                    {:key :mapping :value k}))))

(defn- kept-value?
  "Is a grouping value kept?

  Parameters:

  - `k` (keyword): the grouping dimension.
  - `na-level?` (boolean): do NA (`nil`) values form a group?
  - `plot-scales` (map): the plot's `:scales`.
  - `v`: the value.

  Returns truthy for a valid value (an aesthetic's: kept by the plot scale, see [[cljplot2.scale.aesthetic/kept-value?]]) or, with `na-level?`, for `nil`."
  [k na-level? plot-scales v]
  (or (if (aesthetic/dim? k) (aesthetic/kept-value? k (get plot-scales k {}) v) (data/valid-value? v))
      (and na-level? (nil? v))))

(defn- level-key
  "Key of a level in a domain index: a number as a double (so `1` and `1.0` match), other values as they are."
  [v]
  (if (number? v) (double v) v))

(defn- level-rank
  "Gives the sort rank of a grouping value.

  Parameters:

  - `domain-index` (map): level key (see [[level-key]]) -> its index in an explicit order; `nil` - natural order.
  - `v`: the value.

  Returns `[0 index]` for a value of the order (values outside it after the indexed ones), `[1 v]` without an order, `[2 nil]` for NA - last."
  [domain-index v]
  (cond
    (nil? v) [2 nil]
    domain-index [0 (get domain-index (level-key v) Long/MAX_VALUE)]
    :else [1 v]))

(defn ordered-keys
  "Orders group keys dimension by dimension.

  Parameters:

  - `keys` (collection): group keys, vectors of values over `dims`, in first-appearance order (repeats allowed).
  - `dims` (vector): the grouping dimensions.
  - `plot-scales` (map): the plot's `:scales`.

  Returns a vector of the distinct keys, per dimension in the explicit ordinal `:domain` order of its plot scale, in first-appearance order for an identity scale, otherwise in natural order; NA (`nil`) last. Keys with incomparable values keep first-appearance order."
  [keys dims plot-scales]
  (let [ks (vec (distinct keys))
        indices (vec (map-indexed (fn [i k]
                                    (let [spec (get plot-scales k)
                                          domain (:domain spec)]
                                      (cond
                                        (and (aesthetic/dim? k) (aesthetic/identity-scale? spec))
                                        (zipmap (distinct (keep #(some-> (nth % i) level-key) ks)) (range))
                                        (and (sequential? domain) (aesthetic/dim? k))
                                        (zipmap (map level-key domain) (range)))))
                                  dims))]
    (try (vec (sort-by (fn [key] (mapv level-rank indices key)) ks))
         (catch ClassCastException _ ks))))

(defn split
  "Splits the rows of every panel into groups by the grouping dimensions of a mapping (see the namespace doc).

  Parameters:

  - `groups` (map): `{panel rows}`.
  - `mapping` (map): the chart's merged mapping.
  - `env` (map): `:na-level?` - do NA values form a group (default true); `:plot-scales` - the plot's `:scales`.

  Returns `{:panels {panel [group ...]} :dims [dim ...] :dropped {dim n}}`: groups of a panel in [[ordered-keys]] order, the grouping dimensions ([[group-dims]]), per dimension the count of rows dropped (invalid values, values outside an explicit domain, NA without `:na-level?`).

  Throws `ex-info {:key :mapping}` for an aesthetic that can't group, or has no valid value in non-empty data."
  [groups mapping {:keys [na-level? plot-scales] :or {na-level? true}}]
  (let [dims (group-dims mapping)
        accessors (mapv #(data/row-accessor % (mapping %)) dims)
        all-rows (apply concat (vals groups))
        row-count (count all-rows)
        columns (mapv (fn [acc] (mapv acc all-rows)) accessors)]
    (doseq [[k vs] (map vector dims columns)]
      (when (and (m/pos? row-count) (aesthetic/dim? k) (not-any? #(aesthetic/valid-value? k (get plot-scales k) %) vs))
        (schema/fail! "No valid values for a mapped dimension - check the mapping." {:key :mapping :value k :rows row-count}))
      (check-groupable! k (filter data/valid-value? vs) plot-scales))
    (let [key-of (fn [row] (mapv #(% row) accessors))
          kept? (fn [key] (every? identity (map #(kept-value? %1 na-level? plot-scales %2) dims key)))
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
  "Resolves a per-row aesthetic: one on a linear or binned scale, styled per row instead of grouping.

  The scale kind is inferred as the plot scale will infer it, from the plot's scale spec and the values.

  Parameters:

  - `k` (keyword): the aesthetic, e.g. `:color`, `:size`.
  - `groups` (map): `{panel rows}`.
  - `mapping` (map): the chart's merged mapping.
  - `env` (map): `:plot-scales` - the plot's `:scales`; `:na-level?` - are NA rows kept (default true).

  Returns `{:groups :mapping :value-of :dropped}`: `groups` without the rows outside the scale's explicit domain (and NA rows without `:na-level?`), `mapping` without `k` (to split by the other dimensions), the row accessor of the value, the count of rows dropped. `nil` when `k` is unmapped or groups (an ordinal scale).

  See also [[continuous-stats]]."
  [k groups mapping {:keys [plot-scales na-level?] :or {na-level? true}}]
  (when-let [acc (get mapping k)]
    (let [value-of (data/row-accessor k acc)
          spec (get plot-scales k {})
          present (keep value-of (mapcat val groups))]
      (when (#{:linear :binned} (aesthetic/inferred-kind k spec (aesthetic/continuous-values? k present)))
        (let [kept? (fn [row] (let [v (value-of row)] (if (nil? v) na-level? (aesthetic/kept-value? k spec v))))]
          {:groups (update-vals groups #(filterv kept? %))
           :mapping (dissoc mapping k)
           :value-of value-of
           :dropped (count (remove kept? (mapcat val groups)))})))))

(defn continuous-color
  "Resolves a per-row `:color` - [[continuous]] of `:color`.

  Parameters:

  - `groups`, `mapping`, `env`: as in [[continuous]].

  Returns the result of [[continuous]] with the value accessor also as `:color-of`; `nil` when `:color` is unmapped or groups."
  [groups mapping env]
  (when-let [c (continuous :color groups mapping env)]
    (assoc c :color-of (:value-of c))))

(defn continuous-stats
  "Gives the stats of a chart's per-row values of an aesthetic (see [[continuous]]).

  Parameters:

  - `k` (keyword): the aesthetic.
  - `continuous` (map): the result of [[continuous]].
  - `panel-values` (map): `{panel [value ...]}` of what the chart draws, NA as `nil`.
  - `plot-scales` (map): the plot's `:scales`.

  Returns `{:extents :dropped :na?}`: `{panel {k extent}}` of the non-NA values under the plot's scale spec (panels without values left out), the rows `continuous` dropped, and whether some value is NA (truthy)."
  [k continuous panel-values plot-scales]
  {:extents (into {} (keep (fn [[panel vs]]
                             (when-let [e (aesthetic/value-extent k (get plot-scales k) (remove nil? vs))]
                               [panel {k e}])))
                  panel-values)
   :dropped (:dropped continuous)
   :na? (some nil? (mapcat val panel-values))})

(defn continuous-color-stats
  "Gives the stats of a chart's per-row colours - [[continuous-stats]] of `:color`.

  Parameters:

  - `continuous`, `panel-values`, `plot-scales`: as in [[continuous-stats]]."
  [continuous panel-values plot-scales]
  (continuous-stats :color continuous panel-values plot-scales))

(defn aesthetic-extents
  "Gives the aesthetic extents of split groups.

  Parameters:

  - `panels` (map): `{panel [group ...]}` (see [[split]]).
  - `plot-scales` (map): the plot's `:scales`.

  Returns `{panel {aesthetic extent}}`: the value extents of the non-NA aesthetic values of a panel's groups under the plot's scale specs (see [[cljplot2.scale.aesthetic/value-extent]]); aesthetics without values left out."
  [panels plot-scales]
  (update-vals panels
               (fn [gs]
                 (into {} (keep (fn [k]
                                  (when-let [e (aesthetic/value-extent k (get plot-scales k)
                                                                       (keep #(get-in % [:aesthetics k]) gs))]
                                    [k e])))
                       aesthetic/dims))))

(defn na-dims
  "Gives the aesthetics with an NA group.

  Parameters:

  - `panels` (map): `{panel [group ...]}` (see [[split]]).

  Returns a set of aesthetic dimensions."
  [panels]
  (set (for [gs (vals panels) g gs [k v] (:aesthetics g) :when (nil? v)] k)))

(defn style
  "Applies a group's aesthetics to a chart's style.

  Parameters:

  - `base` (map): the chart's style.
  - `aesthetics` (map): the group's `{dim value}`; dimensions without a scale are ignored.
  - `scales` (map): finalized aesthetic scales.
  - `theme` (map): the plot's theme - an NA `:color` is its `[:palette :na]`; an NA `:shape`, `:size`, `:dash` keeps the base value.

  Returns the style with each aesthetic's attribute, or `nil` when a value is outside its scale's domain (the group is skipped)."
  [base aesthetics scales theme]
  (reduce (fn [st [k v]]
            (if-let [scale (get scales k)]
              (let [na (if (= k :color) (get-in theme [:palette :na]) (get base k))
                    a ((aesthetic/attribute-fn k scale na) v)]
                (if (some? a) (assoc st k a) (reduced nil)))
              st))
          base aesthetics))
