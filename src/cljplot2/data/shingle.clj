(ns cljplot2.data.shingle
  "Shingles: overlapping intervals of a numeric column, as a data transformation (lattice `equal.count`).

  [[shingle]] repeats every row once per interval containing its value and stores that `Interval` in a new column;
  faceting by that column gives one panel per interval (a row may appear in several panels). Interval values sort by
  lower then upper bound and print as `[lo, hi]` (bins of [[cljplot2.data.levels/bin]], `:closed :left`, as
  `[lo, hi)`); a facet strip of Interval levels shows a shaded bar of the interval
  within the range of all levels (see [[cljplot2.content.strip]]).

  Equal-count intervals come from `fastmath.core/co-intervals` (R `co.intervals`).

  Functions: [[shingle]] - the repeated rows; [[equal-count]] - equal-count intervals; [[interval]], [[interval?]], [[contains-value?]] - intervals; [[rounded-str]] - a bound's text; [[check-accessor!]] - the accessor and `:label` check shared with [[cljplot2.data.levels/bin]].

  Option schemas: [[shingle-options]], [[equal-count-options]]."
  (:require [fastmath.core :as m]
            [cljplot2.data.rows :as data]
            [cljplot2.schema :as schema])
  (:import [cljplot2.data.rows OrderedLevel]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(declare rounded-str)

(def ^{:private true :const true :tag 'long} bound-digits
  "Significant digits of an exact (shingle) bound's text: enough for any data value, few enough to drop the noise of
  floating-point arithmetic (`4.6499999999999995` prints `4.65`)."
  10)

(defn- bound-str
  "Gives the text of an exact interval bound (shingles).

  Parameters:

  - `v` (number): the bound.

  Returns whole numbers below 1e15 in magnitude without decimals, other finite numbers at [[bound-digits]] significant digits with trailing zeros dropped - plain notation from 1e-4 to 1e15 in magnitude, scientific beyond; non-finite values as `str`."
  [v]
  (let [d (double v)]
    (cond
      (not (m/valid-double? d)) (str v)
      (and (m/== d (m/floor d)) (m/< (m/abs d) 1.0e15)) (str (long d))
      ;; plain notation within [1e-4 1e15), scientific beyond (a 1e300 bound prints 1E+300, not 301 digits)
      (m/<= 1.0e-4 (m/abs d) 1.0e15) (rounded-str d bound-digits)
      :else (-> (java.math.BigDecimal. d) (.round (java.math.MathContext. (int bound-digits))) (.stripTrailingZeros) (.toString)))))

(defn rounded-str
  "Gives the text of a number rounded to significant digits (bin bounds).

  Parameters:

  - `v` (number): finite.
  - `digits` (long): significant digits, positive.

  Returns the text in plain notation, trailing zeros dropped.

  Throws `NumberFormatException` for a non-finite `v`."
  [v ^long digits]
  (-> (java.math.BigDecimal. (double v))
      (.round (java.math.MathContext. (int digits)))
      (.stripTrailingZeros)
      (.toPlainString)))

(defrecord Interval [lo hi label closed digits]
  ;; closed: nil / :both - [lo, hi] (shingles); :left - [lo, hi) (bins, see cljplot2.data.levels/bin)
  ;; digits: nil - exact bounds (shingles); n - bounds printed with n significant digits (bins)
  OrderedLevel
  Comparable
  (compareTo [_ other]
    (let [^Interval o other
          c (compare lo (.-lo o))]
      (if (m/zero? c) (compare hi (.-hi o)) c)))
  Object
  (toString [_] (let [b (if digits #(rounded-str % digits) bound-str)]
                  (str (when label (str label ": ")) "[" (b lo) ", " (b hi) (if (= :left closed) ")" "]")))))

(def ^:private bounds
  "`[lo hi]`, finite numbers, `lo <= hi`."
  (schema/with-rules [:tuple schema/finite schema/finite]
    [(fn [[^double lo ^double hi]] (when (m/> lo hi) "should be [lo hi] with lo <= hi"))]))

(def ^:private interval-args
  "Arguments of [[interval]]: `{:bounds [lo hi] :label label}`."
  [:map [:bounds bounds] [:label [:maybe string?]]])

(defn interval
  "Makes a closed interval level.

  Parameters:

  - `lo`, `hi` (finite numbers): the bounds, `lo <= hi`.
  - `label` (string, optional): the data series name, printed before the range (`mag: [4, 5]`).

  Returns an `Interval`.

  Throws `ex-info` (see [[cljplot2.schema]]) for bad arguments."
  ([lo hi] (interval lo hi nil))
  ([lo hi label]
   (schema/explain! [[interval-args {:bounds [lo hi] :label label} [:interval]]])
   (->Interval lo hi label nil nil)))

(defn interval?
  "Tests whether a value is an `Interval`.

  Parameters:

  - `v`: any value.

  Returns `true` or `false`."
  [v]
  (instance? Interval v))

(defn contains-value?
  "Tests whether a closed interval contains a number.

  Parameters:

  - `iv` (`Interval`): the interval, bounds included.
  - `v` (number).

  Returns `true` when `lo <= v <= hi`."
  [^Interval iv v]
  (m/<= (double (.-lo iv)) (double v) (double (.-hi iv))))

(def ^:private finite-number? schema/finite?)

(def overlap-schema
  "Schema of an overlap: a number from 0 up to, but not including, 1 - the share of points neighbouring equal-count intervals have in common."
  (schema/named-type ::overlap #(and (number? %) (m/<= 0.0 (double %)) (m/< (double %) 1.0)) "should be a number in [0 1)"
                     {:cljplot2/type-name "overlap fraction"
                      :cljplot2/type-meaning "A number from 0 up to, but not including, 1, e.g. `0.5`: the share of points two neighbouring intervals have in common."}))

(def equal-count-options
  "Options of [[equal-count]]: `:n` (positive integer, default 6), `:overlap` (in `[0 1)`, default 0.5)."
  [:map {:closed true}
   (schema/optional :n schema/pos-int)
   (schema/optional :overlap overlap-schema)])

(def shingle-options
  "Options of [[shingle]]: `:as` (required), `:intervals` or `:n` / `:overlap`, `:label`."
  [:map {:closed true}
   [:as {:description "The key of the interval column the function adds to every repeated row." :cljplot2/example :interval}
    [:fn {:error/message "should be the key of the interval column"} some?]]
   (schema/optional :intervals {:description "The intervals, each `[lo hi]` closed with `lo` at most `hi`; a row goes into every interval holding its value."
                                :cljplot2/default-doc "Equal-count intervals from `:n` and `:overlap`."
                                :cljplot2/example [[-1 0] [0 1]] :cljplot2/example-context {:as :interval}}
                    [:sequential {:min 1} bounds])
   (schema/optional :n {:description "The number of equal-count intervals; the function makes fewer when the data can't support them. `:intervals` overrides it."
                        :default 6 :cljplot2/example 3 :cljplot2/example-context {:as :interval}}
                    schema/pos-int)
   (schema/optional :overlap {:description "The share of points neighbouring equal-count intervals have in common. `:intervals` overrides it."
                              :default 0.5 :cljplot2/example 0.1 :cljplot2/example-context {:as :interval}}
                    overlap-schema)
   (schema/nilable :label {:description "The series name printed in every interval, and so in facet strips."
                           :cljplot2/default-doc "No series name."
                           :cljplot2/example true :cljplot2/example-context {:as :interval}
                           :cljplot2/value-meanings {true "The accessor's name, e.g. `x` for `:x`; only for a keyword accessor."
                                                     false "No series name."}
                           :cljplot2/form-meanings {'string? "This name."}}
                   [:or string? boolean?])])

(defn check-accessor!
  "Checks the accessor of a data function with a `:label` option ([[shingle]], [[cljplot2.data.levels/bin]]).

  Parameters:

  - `value`: the accessor (see [[cljplot2.schema/accessor]]).
  - `label`: the `:label` option; `true` needs a keyword accessor (its name is the label).

  Returns `nil`.

  Throws `ex-info {:key :accessor}` for a bad accessor, `{:key :label}` for `:label true` with a function accessor."
  [value label]
  (schema/explain! [[schema/accessor value [:accessor]]])
  (when (and (true? label) (not (keyword? value)))
    (schema/fail! "label true needs a keyword accessor (its name is the label)" {:key :label :in [:label] :value label})))

(defn- non-numeric-value!
  "Checks that values to shingle are numbers.

  Parameters:

  - `values` (sequence): the values; `nil` allowed.

  Returns `nil`.

  Throws `ex-info {:key :shingle}` for a value neither `nil` nor a number."
  [values]
  (when-let [bad (first (remove #(or (nil? %) (number? %)) values))]
    (schema/fail! "Shingle values must be numbers." {:key :shingle :value bad})))

(defn equal-count
  "Computes equal-count overlapping intervals of values (R `co.intervals`).

  Parameters:

  - `values` (sequence of numbers): `nil` and NaN ignored.
  - `options` (map, see [[equal-count-options]]): `:n` - the number of intervals (default 6); `:overlap` - the fraction of points shared by neighbouring intervals, in `[0 1)` (default 0.5).

  Returns a vector of `Interval`s; fewer when the data can't support `:n` distinct ones, none for no values.

  Throws `ex-info` for bad options (see [[cljplot2.schema]]), `{:key :shingle}` for non-numeric values."
  [values {:keys [n] overlap-share :overlap :or {n 6 overlap-share 0.5} :as options}]
  (schema/explain! [[equal-count-options options [:options]]])
  (non-numeric-value! values)
  (let [xs (filterv finite-number? values)]
    (if (empty? xs)
      []
      (mapv (fn [[lo hi]] (->Interval lo hi nil nil nil)) (m/co-intervals xs (long n) (double overlap-share))))))

(defn shingle
  "Repeats rows once per interval containing their value (lattice `equal.count`).

  Parameters:

  - `rows`: rows or columns (see [[cljplot2.data.rows/rows]]).
  - `value` (keyword or function): the numeric value of a row.
  - `options` (map, see [[shingle-options]]):
    - `:as` - the key the interval is stored under (required).
    - `:intervals` - `[[lo hi] ...]`, closed; default equal-count ones from `:n` / `:overlap` (see [[equal-count]]).
    - `:label` - the data series name printed in every interval (and so in facet strips): a string, or `true` for the accessor's name (keyword accessors only); default none.

  Returns a vector of rows, each with its `Interval` under `:as`; rows with a `nil`, NaN or uncovered value are left out, the result's metadata holds `{:dropped n}`.

  Throws `ex-info` for bad options (see [[cljplot2.schema]]), a bad accessor (see [[check-accessor!]]) or non-numeric values (`{:key :shingle}`)."
  {:cljplot2/api true
   :cljplot2/arguments [{:name 'rows :description "The data: rows (maps) or columns (a map of equal-length sequences)."}
                         {:name 'value :description "The accessor of a row's numeric value, e.g. `:wt`."}]
   :cljplot2/returns "The rows repeated once per interval holding their value, the interval under `:as`; the function leaves out rows whose value is missing, not finite or in no interval."
   :cljplot2/options shingle-options}
  [rows value {:keys [intervals as label] :as options}]
  (schema/check-options! shingle-options options)
  (check-accessor! value label)
  (let [rs (data/rows rows)
        acc (data/row-accessor :shingle value)
        _ (non-numeric-value! (map acc rs))
        series (cond (string? label) label (true? label) (name value))
        ivs (mapv #(assoc % :label series)
                  (if (some? intervals)
                    (map (fn [[lo hi]] (->Interval lo hi nil nil nil)) intervals)
                    (equal-count (map acc rs) (select-keys options [:n :overlap]))))
        out (into [] (mapcat (fn [row]
                               (let [v (acc row)]
                                 (when (finite-number? v)
                                   (keep (fn [iv] (when (contains-value? iv v) (assoc row as iv))) ivs)))))
                  rs)
        covered (count (filter (fn [row] (let [v (acc row)] (and (finite-number? v) (some #(contains-value? % v) ivs)))) rs))]
    (with-meta out {:dropped (m/- (count rs) covered)})))
