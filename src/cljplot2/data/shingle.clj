(ns cljplot2.data.shingle
  "Shingles: overlapping intervals of a numeric column, as a data transformation (lattice `equal.count`).

  [[shingle]] repeats every row once per interval containing its value and stores that [[Interval]] in a new column;
  faceting by that column gives one panel per interval (a row may appear in several panels). Interval values sort by
  lower then upper bound and print as `[lo, hi]`; a facet strip of Interval levels shows a shaded bar of the interval
  within the range of all levels (see [[cljplot2.content.strip]]).

  Equal-count intervals come from `fastmath.core/co-intervals` (R `co.intervals`)."
  (:require [fastmath.core :as m]
            [cljplot2.data.rows :as rows]
            [cljplot2.schema :as schema]))

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

(def ^:private bounds
  "`[lo hi]`, finite numbers, `lo <= hi`."
  (schema/with-rules [:tuple schema/finite schema/finite]
    [(fn [[lo hi]] (when (> (double lo) (double hi)) "should be [lo hi] with lo <= hi"))]))

(def IntervalArgs
  "Arguments of [[interval]]: `{:bounds [lo hi] :label label}`."
  [:map [:bounds bounds] [:label [:maybe string?]]])

(defn interval
  "Interval `[lo hi]` (closed), `lo <= hi`, finite numbers, with an optional `label` (the data series name, printed
  before the range: `\"mag: [4, 5]\"`). Throws `ex-info` (see [[cljplot2.schema]]) for bad arguments."
  ([lo hi] (interval lo hi nil))
  ([lo hi label]
   (schema/explain! [[IntervalArgs {:bounds [lo hi] :label label} [:interval]]])
   (->Interval lo hi label)))

(defn interval?
  "Is `v` an [[Interval]]?"
  [v]
  (instance? Interval v))

(defn contains-value?
  "Does closed interval `iv` contain number `v`?"
  [^Interval iv v]
  (<= (double (.-lo iv)) (double v) (double (.-hi iv))))

(def ^:private finite-number? schema/finite?)

(def ^:private overlap
  (schema/named-type ::overlap #(and (number? %) (<= 0.0 (double %)) (< (double %) 1.0)) "should be a number in [0 1)"))

(def EqualCountOptions
  "Options of [[equal-count]]: `:count` (positive integer, default 6), `:overlap` (in `[0 1)`, default 0.5)."
  [:map {:closed true}
   (schema/optional :count schema/pos-int)
   (schema/optional :overlap overlap)])

(def Options
  "Options of [[shingle]]: `:as` (required), `:intervals` or `:count` / `:overlap`, `:label`."
  [:map {:closed true}
   [:as [:fn {:error/message "should be the key of the interval column"} some?]]
   (schema/optional :intervals [:sequential {:min 1} bounds])
   (schema/optional :count schema/pos-int)
   (schema/optional :overlap overlap)
   (schema/nilable :label [:or string? boolean?])])

(def ^:private ShingleArgs
  (schema/with-rules
   [:map [:accessor schema/accessor] [:options Options]]
   [(fn [{:keys [accessor options]}]
      (when (and (true? (:label options)) (not (keyword? accessor)))
        "label true needs a keyword accessor (its name is the label)"))
    [:options :label]]))

(defn- non-numeric-value!
  [values]
  (when-let [bad (first (remove #(or (nil? %) (number? %)) values))]
    (schema/fail! "Shingle values must be numbers." {:key :shingle :value bad})))

(defn equal-count
  "Equal-count overlapping intervals of `values` (`nil` and NaN ignored): `{:count n :overlap f}` (defaults 6 and 0.5,
  `0 <= f < 1`), as [[Interval]]s. Fewer intervals when the data can't support `n` distinct ones; none for no values.
  Throws `ex-info` for bad options ([[EqualCountOptions]], see [[cljplot2.schema]]) or non-numeric values
  (`{:key :shingle}`)."
  [values {n :count overlap :overlap :or {n 6 overlap 0.5} :as opts}]
  (schema/explain! [[EqualCountOptions opts [:options]]])
  (non-numeric-value! values)
  (let [xs (filterv finite-number? values)]
    (if (empty? xs)
      []
      (mapv (fn [[lo hi]] (->Interval lo hi nil)) (m/co-intervals xs (long n) (double overlap))))))

(defn shingle
  "Rows of `data` (see [[cljplot2.data.rows/rows]]) repeated once per interval containing the value of `accessor`
  (keyword or function), the interval stored under `(:as opts)`. Intervals: `(:intervals opts)` - `[[lo hi] ...]`,
  or equal-count ones from `:count` / `:overlap` (see [[equal-count]]). `:label` names the data series in every
  interval (and so in facet strips): a string, or `true` for the accessor's name (keyword accessors only); default
  none. Rows with a `nil`, NaN or uncovered value are left out; the result's metadata holds `{:dropped n}`.
  Throws `ex-info` for bad options ([[Options]], see [[cljplot2.schema]]) or non-numeric values (`{:key :shingle}`)."
  [data accessor {:keys [intervals as label] :as opts}]
  (schema/explain! [[ShingleArgs {:accessor accessor :options opts} []]])
  (let [rs (rows/rows data)
        acc (rows/row-accessor :shingle accessor)
        _ (non-numeric-value! (map acc rs))
        series (cond (string? label) label (true? label) (name accessor))
        ivs (if (some? intervals)
              (mapv (fn [[lo hi]] (->Interval lo hi nil)) intervals)
              (equal-count (map acc rs) (select-keys opts [:count :overlap])))
        ivs (mapv #(assoc % :label series) ivs)
        out (into [] (mapcat (fn [row]
                               (let [v (acc row)]
                                 (when (finite-number? v)
                                   (keep (fn [iv] (when (contains-value? iv v) (assoc row as iv))) ivs)))))
                  rs)
        covered (count (filter (fn [row] (let [v (acc row)] (and (finite-number? v) (some #(contains-value? % v) ivs)))) rs))]
    (with-meta out {:dropped (- (count rs) covered)})))
