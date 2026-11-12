(ns cljplot2.data.rows
  "Plot data: normalization to rows, mapping accessors, valid values and extents.

  `:data` is a finite sequence of maps (rows) or a map of equal-length finite sequences (columns); both become a
  vector of rows.
  A mapping accessor is a keyword or a function of a row.

  An extent describes the values of one mapped dimension:

  * `{:kind :numeric :min a :max b}` (doubles)
  * `{:kind :temporal :min a :max b}` (`java.time` values)
  * `{:kind :categorical :values [v ...]}` (strings/keywords, distinct, in order of first appearance)

  [[value-extent]] also keeps the distinct values (`:values`) of numeric and temporal data, for scales that
  need the levels (ordinal colour, shape, size) - at most [[max-levels]] of them."
  (:require [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- invalid [message data] (throw (ex-info message data)))

(defn rows
  "Vector of rows of plot `data` (`nil` -> `[]`).
  Throws `ex-info` with `{:key :data}` for anything but a sequence of maps or a map of equal-length sequences."
  [data]
  (cond
    (nil? data) []
    (map? data) (let [ks (keys data)
                      cols (map #(get data %) ks)]
                  (when-not (every? sequential? cols)
                    (invalid "Columnar :data values must be sequences." {:key :data}))
                  (let [counts (set (map count cols))]
                    (when (> (count counts) 1)
                      (invalid "Columnar :data sequences must have equal length." {:key :data :lengths counts})))
                  (if (empty? ks) [] (apply mapv (fn [& vs] (zipmap ks vs)) cols)))
    (sequential? data) (let [rs (vec data)]
                         (when-let [bad (first (remove map? rs))]
                           (invalid "Row :data must contain only maps." {:key :data :value (type bad)}))
                         rs)
    :else (invalid ":data must be a sequence of maps or a map of sequences." {:key :data :value (type data)})))

(defn row-accessor
  "Function of a row for mapping accessor `acc` (keyword or function) of dimension `k`."
  [k acc]
  (cond
    (keyword? acc) acc
    (fn? acc) acc
    :else (invalid "Mapping accessor must be a keyword or a function." {:key :mapping :value k})))

(defn temporal? [v] (instance? java.time.temporal.Temporal v))

(defn valid-value?
  "Can `v` be plotted: a finite number, a string, a keyword or a `java.time` value."
  [v]
  (cond
    (number? v) (m/valid-double? (double v))
    (or (string? v) (keyword? v) (temporal? v)) true
    :else false))

(defn finite-numbers
  "Finite numbers among `vs` (as doubles) and the count of other values: `{:values [..] :dropped n}`."
  [vs]
  (let [ok (into [] (comp (filter #(and (number? %) (m/valid-double? (double %)))) (map double)) vs)]
    {:values ok :dropped (- (count vs) (count ok))}))

(defn- value-kind
  [v]
  (cond (number? v) :numeric (temporal? v) :temporal :else :categorical))

(defn extent
  "Extent of valid values `vs` of dimension `k` (`nil` when empty). Mixed kinds throw `ex-info {:key :extents}`."
  [k vs]
  (when (seq vs)
    (let [kinds (set (map value-kind vs))]
      (when (> (count kinds) 1)
        (invalid "Mapped values must be all numbers, all strings/keywords or all java.time values."
                 {:key :extents :scale k :kinds kinds}))
      (case (first kinds)
        :numeric {:kind :numeric :min (double (reduce min vs)) :max (double (reduce max vs))}
        :temporal {:kind :temporal
                   :min (reduce #(if (neg? (compare %2 %1)) %2 %1) vs)
                   :max (reduce #(if (pos? (compare %2 %1)) %2 %1) vs)}
        :categorical {:kind :categorical :values (vec (distinct vs))}))))

(def ^:const max-levels
  "Most distinct numeric / temporal values [[value-extent]] keeps; with more, an ordinal scale needs an explicit domain."
  100)

(defn- capped-values
  "Extent `e` with distinct `vs` as `:values`; numeric / temporal extents lose `:values` beyond [[max-levels]]."
  [e vs]
  (if (= :categorical (:kind e))
    e
    (let [ds (into [] (comp (distinct) (take (inc max-levels))) vs)]
      (if (> (count ds) max-levels) (dissoc e :values) (assoc e :values ds)))))

(defn value-extent
  "[[extent]] of `vs` that also keeps the distinct values (`:values`, in order of first appearance) of numeric and
  temporal data, when there are at most [[max-levels]] of them."
  [k vs]
  (when-let [e (extent k vs)]
    (capped-values e vs)))

(defn sorted-levels
  "Distinct `values` sorted (numbers ascending, strings lexicographic, ...).
  Throws `ex-info {:key error-key :scale k}` when they aren't mutually comparable."
  [k values error-key]
  (try (vec (sort (distinct values)))
       (catch ClassCastException _
         (invalid "Values must be mutually comparable to be sorted (or give an explicit :domain)."
                  {:key error-key :scale k}))))

(defn merge-extents
  "Merge extents of dimension `k` (`nil`s skipped); `:values` are kept (merged, distinct) when every extent has them
  (numeric / temporal: at most [[max-levels]]). Mixed kinds throw `ex-info {:key :extents}`."
  [k extents]
  (let [es (remove nil? extents)]
    (when (seq es)
      (let [kinds (set (map :kind es))]
        (when (> (count kinds) 1)
          (invalid "Charts map different kinds of values to one scale." {:key :extents :scale k :kinds kinds}))
        (cond-> (case (first kinds)
          :categorical {:kind :categorical :values (vec (distinct (mapcat :values es)))}
          :numeric {:kind :numeric :min (reduce min (map :min es)) :max (reduce max (map :max es))}
          :temporal {:kind :temporal
                     :min (reduce #(if (neg? (compare %2 %1)) %2 %1) (map :min es))
                     :max (reduce #(if (pos? (compare %2 %1)) %2 %1) (map :max es))})
          (every? :values es) (capped-values (mapcat :values es)))))))
