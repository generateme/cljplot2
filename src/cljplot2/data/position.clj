(ns cljplot2.data.position
  "Group positions: where the marks of several groups go when they share positions (bins of a histogram, evaluation
  points of a density). Pure geometry in data units, shared by charts (see [[cljplot2.data.group]] for grouping).

  Positions:

  * `:overlay` - every group from 0 (marks drawn over each other)
  * `:dodge` - side by side: the shared interval split among the groups ([[dodge-interval]]), heights from 0
  * `:stack` - groups on top of each other, in group order
  * `:normalize` - stacked and normalized: per index the groups sum to 1 (an index summing to 0 stays at 0)
  * `:mirror` - exactly two groups, the second drawn downward (`[-h 0]`)"
  (:require [fastmath.core :as m]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def positions
  "Every group position."
  #{:overlay :dodge :stack :normalize :mirror})

(defn- cumulative
  "Per group (in order) the running sums of `heights` before it: `[[0 ...] [h0 ...] [h0+h1 ...] ...]`."
  [heights ^long n]
  (vec (reductions (fn [acc hs] (mapv + acc hs)) (vec (repeat n 0.0)) (butlast heights))))

(defn stack-values
  "Per group per index `[y0 y1]` of the non-negative `heights` (one vector per group, in group order, all of the
  same length - one value per shared index) for `position` (see the namespace doc). No groups give `[]`.
  Throws `ex-info` `{:key :position}` for `:mirror` with other than two groups."
  [position heights]
  (let [heights (mapv #(mapv double %) heights)
        n (count (first heights))]
    (when-not (apply = n (map count heights))
      (throw (ex-info "Group heights should have the same length." {:key :heights :value (mapv count heights)})))
    (cond
      (empty? heights) []
      (#{:overlay :dodge} position) (mapv (fn [hs] (mapv (fn [h] [0.0 h]) hs)) heights)
      (= :mirror position) (if (= 2 (count heights))
                             [(mapv (fn [h] [0.0 h]) (heights 0)) (mapv (fn [^double h] [(- h) 0.0]) (heights 1))]
                             (schema/fail! "Position :mirror needs exactly two groups." {:key :position :groups (count heights)}))
      (= :stack position) (mapv (fn [hs below] (mapv (fn [^double h ^double b] [b (+ b h)]) hs below))
                                heights (cumulative heights n))
      (= :normalize position) (let [totals (apply mapv + heights)
                                    scale (mapv (fn [^double t] (if (pos? t) (/ 1.0 t) 0.0)) totals)
                                    scaled (mapv #(mapv * % scale) heights)]
                                (mapv (fn [hs below] (mapv (fn [^double h ^double b] [b (+ b h)]) hs below))
                                      scaled (cumulative scaled n)))
      :else (throw (ex-info (str "Unknown position " (pr-str position) ".") {:key :position :value position})))))

(defn dodge-interval
  "The `i`-th of `n` equal sub-intervals of `[a b]` (`n` positive)."
  [[a b] ^long i ^long n]
  (let [a (double a) w (/ (- (double b) a) n)]
    [(+ a (* i w)) (+ a (* (inc i) w))]))

(defn symmetric-extent
  "`[-M M]` of numbers `values`, `M` the largest magnitude (`[0.0 0.0]` for none)."
  [values]
  (let [mx (double (reduce (fn [^double acc v] (m/max acc (m/abs (double v)))) 0.0 values))]
    [(- mx) mx]))
