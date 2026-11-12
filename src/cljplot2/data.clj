(ns cljplot2.data
  "Plot data: normalization to rows, mapping accessors, valid values and extents.

  `:data` is a sequence of maps (rows) or a map of equal-length sequences (columns); both become a vector of rows.
  A mapping accessor is a keyword or a function of a row.

  An extent describes the values of one mapped dimension:

  * `{:kind :numeric :min a :max b}`
  * `{:kind :temporal :min a :max b}` (`java.time` values)
  * `{:kind :categorical :values [v ...]}` (strings/keywords, distinct, in order of first appearance)"
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
        :numeric {:kind :numeric :min (reduce min vs) :max (reduce max vs)}
        :temporal {:kind :temporal
                   :min (reduce #(if (neg? (compare %2 %1)) %2 %1) vs)
                   :max (reduce #(if (pos? (compare %2 %1)) %2 %1) vs)}
        :categorical {:kind :categorical :values (vec (distinct vs))}))))

(defn merge-extents
  "Merge extents of dimension `k` (`nil`s skipped). Mixed kinds throw `ex-info {:key :extents}`."
  [k extents]
  (let [es (remove nil? extents)]
    (when (seq es)
      (let [kinds (set (map :kind es))]
        (when (> (count kinds) 1)
          (invalid "Charts map different kinds of values to one scale." {:key :extents :scale k :kinds kinds}))
        (case (first kinds)
          :categorical {:kind :categorical :values (vec (distinct (mapcat :values es)))}
          :numeric {:kind :numeric :min (reduce min (map :min es)) :max (reduce max (map :max es))}
          :temporal {:kind :temporal
                     :min (reduce #(if (neg? (compare %2 %1)) %2 %1) (map :min es))
                     :max (reduce #(if (pos? (compare %2 %1)) %2 %1) (map :max es))})))))
