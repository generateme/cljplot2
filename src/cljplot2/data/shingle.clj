(ns cljplot2.data.shingle
  "Shingles: overlapping intervals of a numeric column, as a data transformation (lattice `equal.count`).

  [[shingle]] repeats every row once per interval containing its value and stores that [[Interval]] in a new column;
  faceting by that column gives one panel per interval (a row may appear in several panels). Interval values sort by
  lower then upper bound and print as `[lo, hi]`; a facet strip of Interval levels shows a shaded bar of the interval
  within the range of all levels (see [[cljplot2.content.strip]]).

  Equal-count intervals come from `fastmath.core/co-intervals` (R `co.intervals`)."
  (:require [fastmath.core :as m]
            [cljplot2.data.rows :as rows]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- bound-str
  [v]
  (let [d (double v)]
    (if (and (m/valid-double? d) (== d (m/floor d)) (< (m/abs d) 1.0e15)) (str (long d)) (str v))))

(defrecord Interval [lo hi label]
  Comparable
  (compareTo [_ other]
    (let [^Interval o other
          c (compare lo (.-lo o))]
      (if (zero? c) (compare hi (.-hi o)) c)))
  Object
  (toString [_] (str (when label (str label ": ")) "[" (bound-str lo) ", " (bound-str hi) "]")))

(defn interval
  "Interval `[lo hi]` (closed), `lo <= hi`, finite numbers, with an optional `label` (the data series name, printed
  before the range: `\"mag: [4, 5]\"`)."
  ([lo hi] (interval lo hi nil))
  ([lo hi label]
   (when-not (and (number? lo) (number? hi) (m/valid-double? (double lo)) (m/valid-double? (double hi))
                  (<= (double lo) (double hi)))
     (throw (ex-info "Interval bounds must be finite numbers with lo <= hi." {:key :shingle :value [lo hi]})))
   (when-not (or (nil? label) (string? label))
     (throw (ex-info "Interval label must be a string." {:key :shingle :value label})))
   (->Interval lo hi label)))

(defn interval?
  "Is `v` an [[Interval]]?"
  [v]
  (instance? Interval v))

(defn contains-value?
  "Does closed interval `iv` contain number `v`?"
  [^Interval iv v]
  (<= (double (.-lo iv)) (double v) (double (.-hi iv))))

(defn- finite-number? [v] (and (number? v) (m/valid-double? (double v))))

(defn equal-count
  "Equal-count overlapping intervals of `values` (`nil` and NaN ignored): `{:count n :overlap f}` (defaults 6 and 0.5,
  `0 <= f < 1`), as [[Interval]]s. Fewer intervals when the data can't support `n` distinct ones; none for no values.
  Throws `ex-info {:key :shingle}` for bad options or non-numeric values."
  [values {n :count overlap :overlap :or {n 6 overlap 0.5}}]
  (when-not (and (integer? n) (pos? (long n)))
    (throw (ex-info "Shingle :count must be a positive integer." {:key :shingle :value n})))
  (when-not (and (number? overlap) (<= 0.0 (double overlap)) (< (double overlap) 1.0))
    (throw (ex-info "Shingle :overlap must be a number in [0 1)." {:key :shingle :value overlap})))
  (let [present (remove nil? values)]
    (when-let [bad (first (remove number? present))]
      (throw (ex-info "Shingle values must be numbers." {:key :shingle :value bad})))
    (let [xs (filterv finite-number? present)]
      (if (empty? xs)
        []
        (mapv (fn [[lo hi]] (->Interval lo hi nil)) (m/co-intervals xs (long n) (double overlap)))))))

(defn shingle
  "Rows of `data` (see [[cljplot2.data.rows/rows]]) repeated once per interval containing the value of `accessor`
  (keyword or function), the interval stored under `(:as opts)`. Intervals: `(:intervals opts)` - `[[lo hi] ...]`,
  or equal-count ones from `:count` / `:overlap` (see [[equal-count]]). `:label` names the data series in every
  interval (and so in facet strips): a string, or `true` for the accessor's name (keyword accessors only); default
  none. Rows with a `nil`, NaN or uncovered value are left out; the result's metadata holds `{:dropped n}`.
  Throws `ex-info {:key :shingle}` for bad options."
  [data accessor {:keys [intervals as label] :as opts}]
  (when-not (some? as)
    (throw (ex-info "Shingle needs :as - the key of the interval column." {:key :shingle :value :as})))
  (when-not (or (nil? label) (false? label) (string? label) (and (true? label) (keyword? accessor)))
    (throw (ex-info "Shingle :label must be a string, or true with a keyword accessor." {:key :shingle :value label})))
  (let [rs (rows/rows data)
        acc (rows/row-accessor :shingle accessor)
        _ (when-let [bad (first (remove #(or (nil? %) (number? %)) (map acc rs)))]
            (throw (ex-info "Shingle values must be numbers." {:key :shingle :value bad})))
        series (cond (string? label) label (true? label) (name accessor))
        ivs (if (some? intervals)
              (do (when-not (and (sequential? intervals) (seq intervals)
                                 (every? #(and (sequential? %) (= 2 (clojure.core/count %))) intervals))
                    (throw (ex-info "Shingle :intervals must be a non-empty sequence of [lo hi]." {:key :shingle :value intervals})))
                  (mapv (fn [[lo hi]] (interval lo hi)) intervals))
              (equal-count (map acc rs) opts))
        ivs (mapv #(assoc % :label series) ivs)
        out (into [] (mapcat (fn [row]
                               (let [v (acc row)]
                                 (when (finite-number? v)
                                   (keep (fn [iv] (when (contains-value? iv v) (assoc row as iv))) ivs)))))
                  rs)
        covered (count (filter (fn [row] (let [v (acc row)] (and (finite-number? v) (some #(contains-value? % v) ivs)))) rs))]
    (with-meta out {:dropped (- (count rs) covered)})))
