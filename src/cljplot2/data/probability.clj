(ns cljplot2.data.probability
  "Probability plot statistics for [[cljplot2.chart.probability]] (Q-Q and P-P plots).

  A side of a plot is data - finite values (doubles) - or a distribution: a fastmath distribution object, `[:name]`
  or `[:name params]` (made by `fastmath.random/distribution`, which validates the name and parameters and throws).

  * Q-Q ([[quantile-pairs]]): quantiles of the two sides at common probabilities - the plotting positions
    `(i - 0.5) / n` (R `ppoints`) of a data side, the sorted values as its quantiles, `icdf` of a distribution side;
    two data sides of different sizes pair the smaller sample's sorted values with the larger one linearly
    interpolated at as many evenly spaced positions (R `qqplot`)
  * P-P ([[probability-pairs]]): the two sides' CDFs at common values - the empirical CDF of a data side (the share
    of values `<=` v), `cdf` of a distribution side"
  (:require [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.stats :as stats]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def distribution-schema
  "A distribution: a fastmath distribution object, `[:name]` or `[:name params]`."
  [:or
   (schema/named-type ::distribution r/distribution? "should be a fastmath distribution, [:name] or [:name params]")
   [:and vector? [:cat :keyword [:? :map]]]])

(defn distribution
  "The fastmath distribution of a validated `spec` (see [[distribution-schema]]); fastmath's exceptions propagate."
  [spec]
  (if (r/distribution? spec)
    spec
    (let [[k params] spec] (r/distribution k (or params {})))))

(defn distribution-name
  "The name of a distribution `spec`: the keyword's name of `[:name ..]`, `nil` for an object."
  [spec]
  (when (vector? spec) (name (first spec))))

(defn plotting-positions
  "The `n` probabilities `(i - 0.5) / n`, `i` = 1..n."
  [^long n]
  (mapv (fn [^long i] (/ (- i 0.5) n)) (range 1 (inc n))))

(defn- interpolate-at
  "Value of sorted `values` at fractional index `t` (linear between neighbours)."
  ^double [^doubles values ^double t]
  (let [i (long (m/floor t))
        last-i (dec (alength values))]
    (if (>= i last-i)
      (aget values last-i)
      (let [a (aget values i)]
        (+ a (* (- t i) (- (aget values (inc i)) a)))))))

(defn interpolated
  "`m` values of `sorted` (ascending, not empty) at evenly spaced positions from the first to the last value, linear
  between neighbours (R `approx(seq_along(x), x, n = m)`); `m` 1 - the first value."
  [sorted ^long m]
  (let [values (double-array sorted)
        span (double (dec (alength values)))]
    (if (= 1 m)
      [(aget values 0)]
      (mapv (fn [^long j] (interpolate-at values (/ (* span j) (dec m)))) (range m)))))

(defn- sorted-values [side] (vec (sort (:values side))))

(defn- quantiles-at
  "Quantiles of a distribution side at probabilities `ps`."
  [side ps]
  (let [d (:distribution side)] (mapv #(double (r/icdf d %)) ps)))

(defn quantile-pairs
  "`{:x [..] :y [..]}` Q-Q points of sides `x-side` / `y-side` (`{:values [..]}` - not empty - or `{:distribution
  d}`), see the namespace doc; `points` - the count of probabilities when both sides are distributions."
  [x-side y-side ^long points]
  (let [data? (comp some? :values)]
    (cond
      (and (data? x-side) (data? y-side))
      (let [n (min (count (:values x-side)) (count (:values y-side)))
            side-quantiles (fn [side] (let [s (sorted-values side)] (if (= n (count s)) s (interpolated s n))))]
        {:x (side-quantiles x-side) :y (side-quantiles y-side)})
      (data? x-side) (let [s (sorted-values x-side)]
                       {:x s :y (quantiles-at y-side (plotting-positions (count s)))})
      (data? y-side) (let [s (sorted-values y-side)]
                       {:x (quantiles-at x-side (plotting-positions (count s))) :y s})
      :else (let [ps (plotting-positions points)]
              {:x (quantiles-at x-side ps) :y (quantiles-at y-side ps)}))))

(defn- count-at-most
  "The count of values of sorted `values` that are `<= v`."
  ^long [^doubles values ^double v]
  (loop [lo 0 hi (alength values)]
    (if (< lo hi)
      (let [mid (unsigned-bit-shift-right (+ lo hi) 1)]
        (if (<= (aget values mid) v) (recur (inc mid) hi) (recur lo mid)))
      lo)))

(defn- cdf-fn
  "CDF of a side: the empirical CDF of data values, `cdf` of a distribution."
  [side]
  (if-let [vs (:values side)]
    (let [values (double-array vs)
          n (double (alength values))]
      (java.util.Arrays/sort values)
      (fn [v] (/ (count-at-most values (double v)) n)))
    (let [d (:distribution side)] (fn [v] (double (r/cdf d v))))))

(defn pooled-values
  "Sorted distinct values of the data sides among `sides`."
  [sides]
  (vec (sort (distinct (mapcat :values sides)))))

(defn probability-pairs
  "`{:x [..] :y [..]}` P-P points of sides `x-side` / `y-side` (as [[quantile-pairs]]) at values `at`."
  [x-side y-side at]
  (let [fx (cdf-fn x-side) fy (cdf-fn y-side)]
    {:x (mapv fx at) :y (mapv fy at)}))

(defn grid
  "`n` (>= 2) evenly spaced values from `lo` to `hi`."
  [[lo hi] ^long n]
  (let [lo (double lo) step (/ (- (double hi) lo) (dec n))]
    (mapv (fn [^long i] (+ lo (* step i))) (range n))))

(defn- quartiles
  "First and third quartiles of a side: R type 7 sample quantiles of data, `icdf` of a distribution."
  [side]
  (if-let [vs (:values side)]
    (mapv double (stats/quantiles vs [0.25 0.75] :r7))
    (quantiles-at side [0.25 0.75])))

(defn quartile-line
  "`[slope intercept]` of the line through the sides' first and third quartile pairs (R `qqline`), `nil` when the x
  quartiles coincide or the line isn't finite."
  [x-side y-side]
  (let [[x1 x3] (quartiles x-side)
        [y1 y3] (quartiles y-side)
        dx (- (double x3) (double x1))]
    (when-not (zero? dx)
      (let [slope (/ (- (double y3) (double y1)) dx)
            intercept (- (double y1) (* slope (double x1)))]
        (when (and (m/valid-double? slope) (m/valid-double? intercept))
          [slope intercept])))))

(def identity-line
  "`[slope intercept]` of `y = x`."
  [1.0 0.0])
