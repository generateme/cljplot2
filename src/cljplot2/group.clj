(ns cljplot2.group
  "Groups within a panel for charts drawing one mark per group (histogram, density, line, ECDF).

  Grouping dimensions: mapped aesthetics (see [[cljplot2.aesthetic]]) whose scale is ordinal - categorical or
  temporal data, or `:kind :ordinal` / an ordinal explicit domain in the plot's `:scales` - and `:group` (splits
  without styling; no scale, no legend). A mapped aesthetic whose scale would be linear (numeric data) can't group:
  `ex-info {:key :mapping}`.

  A group is `{:key [v ...] :aesthetics {dim v} :rows [row ...]}`: `:key` - values of the grouping dimensions in
  [[group-dims]] order, `:aesthetics` - the aesthetic values (`nil` = NA). `nil` values form the NA group (dropped
  and counted when the plot's `:groups {:na false}`); other invalid values (NaN, infinite, ...) are dropped and
  counted. Groups are ordered by [[ordered-keys]]."
  (:require [cljplot2.aesthetic :as aesthetic]
            [cljplot2.data :as data]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn group-dims
  "Grouping dimensions of `mapping`: mapped aesthetics, then `:group`."
  [mapping]
  (cond-> (aesthetic/mapped-dims mapping) (some? (:group mapping)) (conj :group)))

(defn- check-groupable!
  "Throw when aesthetic `k` would get a linear scale (numeric values without an ordinal spec)."
  [k values plot-scales]
  (when (aesthetic/dim? k)
    (let [present (remove nil? values)
          numeric? (and (seq present) (every? number? present))]
      (when (= :linear (aesthetic/inferred-kind k (get plot-scales k {}) numeric?))
        (throw (ex-info "This chart groups by aesthetics; numeric values need an ordinal scale ({:scales {k {:kind :ordinal}}})."
                        {:key :mapping :value k}))))))

(defn- kept-value?
  [na-level? v]
  (or (data/valid-value? v) (and na-level? (nil? v))))

(defn- level-rank
  "Sort rank of `v` in dimension `k`: its index in an explicit ordinal domain (else `v`), NA last."
  [domain-index v]
  (cond
    (nil? v) [2 nil]
    domain-index [0 (get domain-index (if (number? v) (double v) v) Long/MAX_VALUE)]
    :else [1 v]))

(defn ordered-keys
  "Distinct group `keys` (vectors over `dims`) ordered dimension by dimension: explicit ordinal `:domain` order from
  `plot-scales`, otherwise natural order; NA (`nil`) last. Incomparable values keep first-appearance order."
  [keys dims plot-scales]
  (let [indices (mapv (fn [k]
                        (let [domain (get-in plot-scales [k :domain])]
                          (when (and (sequential? domain) (aesthetic/dim? k))
                            (zipmap (map #(if (number? %) (double %) %) domain) (range)))))
                      dims)
        ks (vec (distinct keys))]
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
      (when (and (pos? row-count) (not-any? data/valid-value? vs) (aesthetic/dim? k))
        (throw (ex-info "No valid values for a mapped dimension - check the mapping." {:key :mapping :value k :rows row-count})))
      (check-groupable! k (filter data/valid-value? vs) plot-scales))
    (let [key-of (fn [row] (mapv #(% row) accessors))
          kept? (fn [key] (every? #(kept-value? na-level? %) key))
          panels (update-vals groups
                              (fn [rows]
                                (let [by-key (group-by key-of (filter (comp kept? key-of) rows))]
                                  (mapv (fn [key] {:key key
                                                   :aesthetics (into {} (keep (fn [[k v]] (when (aesthetic/dim? k) [k v])))
                                                                     (map vector dims key))
                                                   :rows (by-key key)})
                                        (ordered-keys (keys by-key) dims plot-scales)))))]
      {:panels panels
       :dims dims
       :dropped (into {} (map (fn [k vs] [k (count (remove #(kept-value? na-level? %) vs))]) dims columns))})))

(defn aesthetic-extents
  "Per panel of split `panels`: value extents of the non-NA aesthetic values of its groups."
  [panels]
  (update-vals panels
               (fn [gs]
                 (into {} (keep (fn [k]
                                  (when-let [e (data/value-extent k (keep #(get-in % [:aesthetics k]) gs))]
                                    [k e])))
                       aesthetic/dims))))

(defn na-dims
  "Aesthetics with an NA group in split `panels`."
  [panels]
  (set (for [gs (vals panels) g gs [k v] (:aesthetics g) :when (nil? v)] k)))

(defn style
  "`base` style of a chart with the group's mapped `aesthetics` applied through finalized `scales` (`:color` - NA
  color from theme `[:palette :na]`, `:shape`, `:size` - the base value when NA); `nil` when a value is outside
  its scale's domain (the group is skipped)."
  [base aesthetics scales theme]
  (reduce (fn [st [k v]]
            (if-let [scale (get scales k)]
              (let [na (if (= k :color) (get-in theme [:palette :na]) (get base k))
                    a ((aesthetic/attribute-fn k scale na) v)]
                (if (some? a) (assoc st k a) (reduced nil)))
              st))
          base aesthetics))
