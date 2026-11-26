(ns cljplot2.data.probability
  "Probability plot statistics for [[cljplot2.chart.probability]] (Q-Q and P-P plots).

  A side of a plot is data - finite values (doubles) - or a distribution: a fastmath distribution object, `[:name]`
  or `[:name params]` (made by `fastmath.random/distribution`, which validates the name and parameters and throws).

  * Q-Q ([[quantile-pairs]]): quantiles of the two sides at common probabilities - the plotting positions
    `(i - 0.5) / n` (R `ppoints`) of a data side, the sorted values as its quantiles, `icdf` of a distribution side;
    two data sides of different sizes pair the smaller sample's sorted values with the larger one linearly
    interpolated at as many evenly spaced positions (R `qqplot`)
  * P-P ([[probability-pairs]]): the two sides' CDFs at common values - the empirical CDF of a data side (the share
    of values `<=` v), `cdf` of a distribution side

  Functions: [[distribution-schema]] - the option type of a distribution side; [[distribution]], [[distribution-name]] - a distribution side; [[quantile-pairs]], [[probability-pairs]] - the points; [[quartile-line]] - the reference line; [[plotting-positions]], [[interpolated]], [[pooled-values]], [[grid]] - helpers."
  (:require [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.stats :as stats]
            [malli.core :as malli]
            [cljplot2.schema :as schema])
  (:import [fastmath.java Array]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private distribution-spec?
  "Tests whether a value is a distribution spec `[:name]` or `[:name params]` (a keyword, then an optional map)."
  (malli/validator [:and vector? [:cat :keyword [:? :map]]]))

(def distribution-schema
  "Schema of a distribution: a fastmath distribution object, `[:name]` or `[:name params]` (names and parameters are checked by `fastmath.random/distribution` when the distribution is made)."
  (schema/named-type ::distribution #(or (r/distribution? %) (distribution-spec? %)) "should be a fastmath distribution, [:name] or [:name params]"
                     {:cljplot2/type-name "distribution"
                      :cljplot2/type-meaning "A fastmath distribution: a distribution object, or its name with optional parameters, e.g. `[:normal {:mu 0 :sd 1}]` or `[:exponential]`."
                      :cljplot2/type-doc "a fastmath distribution, [:name] or [:name params]"}))

(defn distribution
  "Gives the fastmath distribution of a distribution spec.

  Parameters:

  - `spec`: a validated spec (see [[distribution-schema]]) - a fastmath distribution object, `[:name]` or `[:name params]`.

  Returns the distribution object (an object spec itself).

  Fastmath's exceptions for an unknown name or bad parameters propagate."
  [spec]
  (if (r/distribution? spec)
    spec
    (let [[k params] spec] (r/distribution k (or params {})))))

(defn distribution-name
  "Gives the name of a distribution spec.

  Parameters:

  - `spec`: a distribution spec (see [[distribution-schema]]).

  Returns the name of the keyword of `[:name ..]` as a string, `nil` for a distribution object."
  [spec]
  (when (vector? spec) (name (first spec))))

(defn plotting-positions
  "Gives the plotting positions of a data side (R `ppoints`).

  Parameters:

  - `n` (long): the count of values.

  Returns the `n` probabilities `(i - 0.5) / n`, `i` = 1..n, as a vector."
  [^long n]
  (mapv (fn [^long i] (m// (m/- i 0.5) n)) (range 1 (m/long-inc n))))

(defn- interpolate-at
  "Gives the value of sorted numbers at a fractional index, linear between neighbours.

  Parameters:

  - `values` (double array): the sorted values, not empty.
  - `t` (double): the index, from 0.

  Returns the value as a double; an index at or past the last one gives the last value."
  ^double [^doubles values ^double t]
  (let [i (long (m/floor t))
        last-i (m/long-dec (alength values))]
    (if (m/>= i last-i)
      (Array/aget values last-i)
      (let [a (Array/aget values i)]
        (m/+ a (m/* (m/- t i) (m/- (Array/aget values (m/long-inc i)) a)))))))

(defn interpolated
  "Resamples sorted values at evenly spaced positions (R `approx(seq_along(x), x, n = m)`).

  Parameters:

  - `sorted` (sequence of numbers): ascending, not empty.
  - `m` (long): the count of values wanted, positive.

  Returns a vector of `m` doubles at evenly spaced positions from the first to the last value, linear between neighbours; `m` 1 gives the first value."
  [sorted ^long m]
  (let [values (double-array sorted)]
    (if (m/one? m)
      [(Array/aget values 0)]
      (mapv (fn [^double t] (interpolate-at values t)) (m/slice-range 0.0 (m/long-dec (alength values)) m)))))

(defn- sorted-values
  "Gives the values of a data side sorted ascending.

  Parameters:

  - `side` (map): `{:values [..]}`.

  Returns a vector."
  [side]
  (vec (sort (:values side))))

(defn- quantiles-at
  "Gives quantiles of a distribution side.

  Parameters:

  - `side` (map): `{:distribution d}`.
  - `ps` (sequence of numbers): probabilities.

  Returns a vector of doubles, the inverse CDF at each of `ps`."
  [side ps]
  (let [d (:distribution side)] (mapv #(double (r/icdf d %)) ps)))

(defn quantile-pairs
  "Computes the points of a Q-Q plot (see the namespace doc).

  Parameters:

  - `x-side`, `y-side` (maps): `{:values [..]}` (not empty) or `{:distribution d}`.
  - `points` (long): the count of probabilities when both sides are distributions.

  Returns `{:x [..] :y [..]}` - the two sides' quantiles at common probabilities.

  See also [[probability-pairs]]."
  [x-side y-side ^long points]
  (let [data? (comp some? :values)]
    (cond
      (and (data? x-side) (data? y-side))
      (let [n (m/min (count (:values x-side)) (count (:values y-side)))
            side-quantiles (fn [side] (let [s (sorted-values side)] (if (m/== n (count s)) s (interpolated s n))))]
        {:x (side-quantiles x-side) :y (side-quantiles y-side)})
      (data? x-side) (let [s (sorted-values x-side)]
                       {:x s :y (quantiles-at y-side (plotting-positions (count s)))})
      (data? y-side) (let [s (sorted-values y-side)]
                       {:x (quantiles-at x-side (plotting-positions (count s))) :y s})
      :else (let [ps (plotting-positions points)]
              {:x (quantiles-at x-side ps) :y (quantiles-at y-side ps)}))))

(defn- count-at-most
  "Counts the sorted values at most a value (binary search).

  Parameters:

  - `values` (double array): sorted ascending.
  - `v` (double): the bound.

  Returns the count of values `<= v` as a long."
  ^long [^doubles values ^double v]
  (loop [lo 0
         hi (alength values)]
    (if (m/< lo hi)
      (let [mid (unsigned-bit-shift-right (m/+ lo hi) 1)]
        (if (m/<= (Array/aget values mid) v) (recur (m/long-inc mid) hi) (recur lo mid)))
      lo)))

(defn- cdf-fn
  "Makes the CDF of a side.

  Parameters:

  - `side` (map): `{:values [..]}` or `{:distribution d}`.

  Returns a function of a number giving a double: the empirical CDF of data values (the share of values `<=` it), `cdf` of a distribution."
  [side]
  (if-let [vs (:values side)]
    (let [values (double-array vs)
          n (double (alength values))]
      (java.util.Arrays/sort values)
      (fn [v] (m// (count-at-most values (double v)) n)))
    (let [d (:distribution side)] (fn [v] (double (r/cdf d v))))))

(defn pooled-values
  "Pools the values of data sides.

  Parameters:

  - `sides` (sequence of maps): sides, `{:values [..]}` or `{:distribution d}` (no values).

  Returns the sorted distinct values of the data sides as a vector."
  [sides]
  (vec (sort (distinct (mapcat :values sides)))))

(defn probability-pairs
  "Computes the points of a P-P plot (see the namespace doc).

  Parameters:

  - `x-side`, `y-side` (maps): as in [[quantile-pairs]].
  - `at` (sequence of numbers): the common values.

  Returns `{:x [..] :y [..]}` - the two sides' CDFs at `at`.

  See also [[pooled-values]], [[grid]]."
  [x-side y-side at]
  (let [fx (cdf-fn x-side)
        fy (cdf-fn y-side)]
    {:x (mapv fx at) :y (mapv fy at)}))

(defn grid
  "Gives evenly spaced values over an interval.

  Parameters:

  - `interval` (`[lo hi]`, numbers).
  - `n` (long): the count of values, at least 2.

  Returns a vector of `n` doubles from `lo` to `hi`, both included."
  [[^double lo ^double hi] ^long n]
  (vec (m/slice-range lo hi n)))

(defn- quartiles
  "Gives the first and third quartiles of a side.

  Parameters:

  - `side` (map): `{:values [..]}` or `{:distribution d}`.

  Returns `[q1 q3]` (doubles): R type 7 sample quantiles of data, the inverse CDF of a distribution."
  [side]
  (if-let [vs (:values side)]
    (mapv double (stats/quantiles vs [0.25 0.75] :r7))
    (quantiles-at side [0.25 0.75])))

(defn quartile-line
  "Computes the reference line of a Q-Q plot (R `qqline`).

  Parameters:

  - `x-side`, `y-side` (maps): as in [[quantile-pairs]].

  Returns `[slope intercept]` of the line through the sides' first and third quartile pairs, `nil` when the x quartiles coincide or the line isn't finite.

  See also [[identity-line]]."
  [x-side y-side]
  (let [[^double x1 ^double x3] (quartiles x-side)
        [^double y1 ^double y3] (quartiles y-side)
        dx (m/- x3 x1)]
    (when-not (m/zero? dx)
      (let [slope (m// (m/- y3 y1) dx)
            intercept (m/- y1 (m/* slope x1))]
        (when (and (m/valid-double? slope) (m/valid-double? intercept))
          [slope intercept])))))

(def identity-line
  "`[slope intercept]` of `y = x`."
  [1.0 0.0])
