(ns cljplot2.data.group
  "Groups of rows within a panel, for charts drawing one mark per group (histogram, density, line, ECDF), and per-row continuous aesthetics.

  Grouping dimensions: mapped aesthetics (see [[cljplot2.scale.aesthetic]]) whose scale is ordinal - categorical or temporal data, or `:kind :ordinal` / an ordinal explicit domain in the plot's `:scales` - and `:group` (splits without styling; no scale, no legend). A mapped aesthetic whose scale would be linear (numeric data) can't group: `ex-info {:key :mapping}`.

  A group is `{:key [v ...] :aesthetics {dim v} :rows [row ...]}`: `:key` - values of the grouping dimensions in [[group-dims]] order, `:aesthetics` - the aesthetic values (`nil` = NA). `nil` values form the NA group (dropped and counted when the plot's `:groups {:na false}`); other invalid values (NaN, infinite, ...) and aesthetic values outside an explicit plot scale domain ([[cljplot2.scale.aesthetic/in-domain?]]) are dropped and counted. Groups are ordered by [[ordered-keys]].

  Functions:

  - grouping: [[group-dims]], [[kept-value?]], [[ordered-keys]], [[panel-group-keys]], [[split]], [[row-count]], [[finite-values]]
  - per-row aesthetics (linear or binned scales): [[continuous]], [[continuous-color]], [[continuous-stats]], [[continuous-color-stats]]; marks of several rows judged by one value: [[kept-marks]]
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

  Throws `ex-info {:key :mapping :in [:mapping k]}` when aesthetic `k` would get a linear or binned scale (numeric or date values without an ordinal spec)."
  [k values plot-scales]
  (when (aesthetic/dim? k)
    (when (#{:linear :binned} (aesthetic/inferred-kind k (get plot-scales k {}) (aesthetic/continuous-values? k values)))
      (schema/fail! "This chart groups by aesthetics; numeric (or date) values must be declared categorical ({:scales {k {:type :categorical}}} or {:mapping {k (levels/categorical acc)}})."
                    {:key :mapping :in [:mapping k]}))))

(defn kept-value?
  "Tests whether a row value of a dimension is kept by a chart.

  Parameters:

  - `k` (keyword): the dimension - a position (`:x`, `:y`, `:z` ...), an aesthetic or `:group`.
  - `na-level` (boolean): do NA (`nil`) values of grouping dimensions (aesthetics and `:group`) form a group?
  - `plot-scales` (map): the plot's `:scales`.
  - `v`: the value.

  Returns truthy for a valid value - an aesthetic's when its plot scale keeps it (see [[cljplot2.scale.aesthetic/kept-value?]]), else [[cljplot2.data.rows/valid-value?]] - or, with `na-level`, for a `nil` value of a grouping dimension; positions never keep `nil`.

  Throws as [[cljplot2.scale.aesthetic/kept-value?]] for a value an aesthetic scale can't take."
  [k na-level plot-scales v]
  (let [aesthetic? (aesthetic/dim? k)]
    (or (if aesthetic? (aesthetic/kept-value? k (get plot-scales k {}) v) (data/valid-value? v))
        (and na-level (nil? v) (or aesthetic? (= :group k))))))

(defn- level-rank
  "Gives the sort rank of a grouping value.

  Parameters:

  - `domain-index` (map): level key (see [[cljplot2.scale.aesthetic/level-key]]) -> its index in an explicit order; `nil` - natural order.
  - `v`: the value.

  Returns `[0 index]` for a value of the order (values outside it after the indexed ones), `[1 v]` without an order, `[2 nil]` for NA - last."
  [domain-index v]
  (cond
    (nil? v) [2 nil]
    domain-index [0 (get domain-index (aesthetic/level-key v) Long/MAX_VALUE)]
    :else [1 v]))

(defn ordered-keys
  "Orders group keys dimension by dimension.

  Parameters:

  - `group-keys` (collection): group keys, vectors of values over `dims`, in first-appearance order (repeats allowed).
  - `dims` (vector): the grouping dimensions.
  - `plot-scales` (map): the plot's `:scales`.

  Returns a vector of the distinct keys, per dimension in the explicit ordinal `:domain` order of its plot scale, in first-appearance order for an identity scale, otherwise in natural order; NA (`nil`) last. Keys with incomparable values keep first-appearance order."
  [group-keys dims plot-scales]
  (let [ks (vec (distinct group-keys))
        indices (vec (map-indexed (fn [i k]
                                    (let [spec (get plot-scales k)
                                          domain (data/given-domain (:domain spec))]
                                      (cond
                                        (and (aesthetic/dim? k) (aesthetic/identity-scale? spec))
                                        (zipmap (distinct (keep #(some-> (nth % i) aesthetic/level-key) ks)) (range))
                                        (and (sequential? domain) (aesthetic/dim? k))
                                        (zipmap (map aesthetic/level-key domain) (range)))))
                                  dims))]
    (try (vec (sort-by (fn [group-key] (mapv level-rank indices group-key)) ks))
         (catch ClassCastException _ ks))))

(defn panel-group-keys
  "Gives the keys of the groups of every panel, ordered as one set.

  Parameters:

  - `panels` (map): `{panel [group ...]}` (see [[split]]).
  - `dims` (sequence): the grouping dimensions.
  - `plot-scales` (map): the plot's `:scales`.

  Returns the distinct group keys over all panels in [[ordered-keys]] order."
  [panels dims plot-scales]
  (ordered-keys (mapcat #(map :key %) (vals panels)) dims plot-scales))

(defn split
  "Splits the rows of every panel into groups by the grouping dimensions of a mapping (see the namespace doc).

  Parameters:

  - `groups` (map): `{panel rows}`.
  - `mapping` (map): the chart's merged mapping.
  - `env` (map): `:na-level` - do NA values form a group (default true); `:plot-scales` - the plot's `:scales`.

  Returns `{:panels {panel [group ...]} :dims [dim ...] :dropped {dim n}}`: groups of a panel in [[ordered-keys]] order, the grouping dimensions ([[group-dims]]), per dimension the count of rows dropped (invalid values, values outside an explicit domain, NA without `:na-level`).

  Throws `ex-info {:key :mapping}` for an aesthetic that can't group, or has no valid value in non-empty data."
  [groups mapping {:keys [na-level plot-scales] :or {na-level true}}]
  (let [dims (group-dims mapping)
        accessors (mapv #(data/row-accessor % (mapping %)) dims)
        all-rows (apply concat (vals groups))
        columns (mapv (fn [acc] (mapv acc all-rows)) accessors)]
    (data/check-mapped! all-rows (filter (comp aesthetic/dim? first) (map vector dims accessors))
                        (fn [k v] (aesthetic/valid-value? k (get plot-scales k) v)))
    (doseq [[k vs] (map vector dims columns)]
      (check-groupable! k (filter data/valid-value? vs) plot-scales))
    (let [key-of (fn [row] (mapv #(% row) accessors))
          kept? (fn [group-key] (every? identity (map #(kept-value? %1 na-level plot-scales %2) dims group-key)))
          panels (update-vals groups
                              (fn [rows]
                                (let [keyed (keep (fn [row] (let [group-key (key-of row)] (when (kept? group-key) [group-key row]))) rows)
                                      by-key (reduce (fn [acc [group-key row]] (update acc group-key (fnil conj []) row)) {} keyed)]
                                  (mapv (fn [group-key] {:key group-key
                                                         :aesthetics (into {} (keep (fn [[k v]] (when (aesthetic/dim? k) [k v])))
                                                                           (map vector dims group-key))
                                                         :rows (by-key group-key)})
                                        (ordered-keys (distinct (map first keyed)) dims plot-scales)))))]
      {:panels panels
       :dims dims
       :dropped (into {} (map (fn [k vs] [k (count (remove #(kept-value? k na-level plot-scales %) vs))]) dims columns))})))

(defn row-count
  "Counts the rows of every group of every panel.

  Parameters:

  - `panels` (map): `{panel [group ...]}` (see [[split]]).

  Returns the total number of grouped rows."
  ^long [panels]
  (long (reduce m/long-add 0 (map (comp count :rows) (mapcat val panels)))))

(defn finite-values
  "Gives the finite values of every group of every panel.

  Parameters:

  - `panels` (map): `{panel [group ...]}` (see [[split]]).
  - `value-of` (function): row -> value.

  Returns `{panel [{:values [..] :dropped n} ...]}`, one entry per group in order (see [[cljplot2.data.rows/finite-numbers]])."
  [panels value-of]
  (update-vals panels (fn [groups] (mapv #(data/finite-numbers (map value-of (:rows %))) groups))))

(defn continuous
  "Resolves a per-row aesthetic: one on a linear or binned scale, styled per row instead of grouping.

  The scale kind is inferred as the plot scale will infer it, from the plot's scale spec and the values.

  Parameters:

  - `k` (keyword): the aesthetic, e.g. `:color`, `:size`.
  - `groups` (map): `{panel rows}`.
  - `mapping` (map): the chart's merged mapping.
  - `env` (map): `:plot-scales` - the plot's `:scales`; `:na-level` - are NA rows kept (default true); `:row-filter` - which rows are dropped: `:kept` (default) - rows the scale doesn't keep (an invalid value, or one outside the explicit domain under `:out-of-domain` `nil` / `:drop`, see [[cljplot2.scale.aesthetic/kept-value?]]); `:valid` - rows with an invalid value only (a chart that aggregates several rows into one mark judges the aggregate, e.g. bar); under both, NA rows go by `:na-level`. `:none` - no rows, NA rows included; the caller judges marks by one value ([[kept-marks]]) or breaks lines at points, and applies `:na-level` itself.

  Returns `{:groups :mapping :value-of :dropped}`: `groups` without the dropped rows (NA rows also without `:na-level`, except under `:none`), `mapping` without `k` (to split by the other dimensions), the row accessor of the value, the count of rows dropped. `nil` when `k` is unmapped or groups (an ordinal scale).

  Throws as [[cljplot2.scale.aesthetic/kept-value?]] (a value of the wrong type for the scale kind) under every `:row-filter`.

  See also [[continuous-stats]]."
  [k groups mapping {:keys [plot-scales na-level row-filter] :or {na-level true row-filter :kept}}]
  (when-let [acc (get mapping k)]
    (let [value-of (data/row-accessor k acc)
          spec (get plot-scales k {})
          present (keep value-of (mapcat val groups))]
      (when (#{:linear :binned} (aesthetic/inferred-kind k spec (aesthetic/continuous-values? k present)))
        (let [kept? (fn [row] (let [v (value-of row)]
                                (if (nil? v)
                                  (or (= :none row-filter) na-level)
                                  ;; kept-value? runs in every mode: it rejects a value of the wrong type
                                  (let [kept (aesthetic/kept-value? k spec v)]
                                    (case (or row-filter :kept)
                                      :kept kept
                                      :valid (or kept (data/valid-value? v))
                                      :none true)))))]
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

(defn kept-marks
  "Drops the marks whose one value of an aesthetic the plot's scale doesn't keep; a mark of several rows is judged by that value, never by its rows.

  Parameters:

  - `k` (keyword): the aesthetic.
  - `env` (map): `:plot-scales` - the plot's `:scales`; `:na-level` - is a `nil` value (NA) kept (default true).
  - `value-of` (function): mark -> its value (`nil` - NA).
  - `counted-rows` (function): mark -> the number of rows counted when it's dropped (rows counted elsewhere left out).
  - `panels` (map): `{panel [mark ...]}`.

  Returns `[panels dropped]`: the panels without the marks not kept (see [[kept-value?]] - an invalid value, a value outside the explicit domain under `:out-of-domain` `nil` / `:drop`, NA without `:na-level`), and the sum of their counted rows. A kept value outside the domain is drawn by the scale's `:out-of-domain` (`:as-na` - NA, `:clamp` - the end's attribute)."
  [k {:keys [plot-scales na-level] :or {na-level true}} value-of counted-rows panels]
  (let [judged (update-vals panels #(group-by (fn [mark] (if (kept-value? k na-level plot-scales (value-of mark)) :kept :dropped)) %))]
    [(update-vals judged #(get % :kept []))
     (reduce m/long-add 0 (map (comp long counted-rows) (mapcat #(get (val %) :dropped) judged)))]))

(defn continuous-stats
  "Gives the stats of a chart's per-row values of an aesthetic (see [[continuous]]).

  Parameters:

  - `k` (keyword): the aesthetic.
  - `continuous-split` (map): the result of [[continuous]].
  - `panel-values` (map): `{panel [value ...]}` of what the chart draws, NA as `nil`.
  - `plot-scales` (map): the plot's `:scales`.

  Returns `{:extents :dropped :na}`: `{panel {k extent}}` of the non-NA values under the plot's scale spec (panels without values left out), the rows `continuous-split` dropped, and whether some value is NA (truthy)."
  [k continuous-split panel-values plot-scales]
  {:extents (into {} (keep (fn [[panel vs]]
                             (when-let [e (aesthetic/value-extent k (get plot-scales k) (remove nil? vs))]
                               [panel {k e}])))
                  panel-values)
   :dropped (:dropped continuous-split)
   :na (some nil? (mapcat val panel-values))})

(defn continuous-color-stats
  "Gives the stats of a chart's per-row colours - [[continuous-stats]] of `:color`.

  Parameters:

  - `continuous-split`, `panel-values`, `plot-scales`: as in [[continuous-stats]]."
  [continuous-split panel-values plot-scales]
  (continuous-stats :color continuous-split panel-values plot-scales))

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

(defn- na-attribute
  "Gives the attribute a group of an NA value draws with.

  Parameters:

  - `k` (keyword): the aesthetic dimension.
  - `base` (map): the chart's style.
  - `theme` (map): the plot's theme.

  Returns: `:color` - the theme's `[:palette :na]`; `:shape` - the base `:shape`, else the palette's first shape; `:size` - the base `:size`, else its `:line-width` (line marks); `:dash` - the base `:dash`, else `:solid`; `:pattern` - the base `:pattern` (`nil` without one)."
  [k base theme]
  (case k
    :color (get-in theme [:palette :na])
    :shape (or (:shape base) (first (get-in theme [:palette :shape])))
    :size (or (:size base) (:line-width base))
    :dash (or (:dash base) :solid)
    (get base k)))

(defn style
  "Applies a group's aesthetics to a chart's style.

  Parameters:

  - `base` (map): the chart's style.
  - `aesthetics` (map): the group's `{dim value}`; dimensions without a scale are ignored.
  - `scales` (map): finalized aesthetic scales.
  - `theme` (map): the plot's theme - read for the NA attributes ([[na-attribute]]).

  Returns the style with each aesthetic's attribute (NA - an NA value, or one outside the domain drawn as NA, `:out-of-domain :as-na`), or `nil` when a value is outside its scale's domain otherwise (the group is skipped)."
  [base aesthetics scales theme]
  (reduce (fn [st [k v]]
            (if-let [scale (get scales k)]
              (let [a ((aesthetic/attribute-fn k scale (na-attribute k base theme)) v)]
                (if (some? a) (assoc st k a) (reduced nil)))
              st))
          base aesthetics))
