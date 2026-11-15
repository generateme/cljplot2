(ns cljplot2.data.position
  "Group positions: where the marks of several groups go when they share positions (bins of a histogram, evaluation
  points of a density). Pure geometry in data units, shared by charts (see [[cljplot2.data.group]] for grouping).

  Positions:

  * `:overlay` - every group from 0 (marks drawn over each other)
  * `:dodge` - side by side: the shared interval split among the groups ([[dodge-interval]]), heights from 0
  * `:stack` - groups on top of each other, in group order; negative heights stack downward from 0, separately
  * `:normalize` - stacked and normalized: per index the groups sum to 1 (an index summing to 0 stays at 0)
  * `:mirror` - exactly two groups, the second drawn downward (`[-h 0]`)

  Stream positions ([[stream-positions]], streamgraphs): the stack shifted by a baseline per index -
  `:silhouette` centred on 0 (d3 `stackOffsetSilhouette`), `:wiggle` minimising the slopes of the layers (d3
  `stackOffsetWiggle`, Byron & Wattenberg; the slope per index, not per x distance). `:normalize` and the stream
  positions need non-negative heights."
  (:require [fastmath.core :as m]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def positions
  "Group positions of every grouping chart."
  #{:overlay :dodge :stack :normalize :mirror})

(def stream-positions
  "Stacked positions shifted by a baseline (streamgraphs): line and density only."
  #{:silhouette :wiggle})

(defn- cumulative
  "Per group (in order) the running sums of `heights` before it: `[[0 ...] [h0 ...] [h0+h1 ...] ...]`."
  [heights ^long n]
  (vec (reductions (fn [acc hs] (mapv + acc hs)) (vec (repeat n 0.0)) (butlast heights))))

(defn- stacked
  "Per group per index `[y0 y1]`: positive heights stacked upward from 0, negative ones downward, each in group
  order (`n` - the shared length)."
  [heights ^long n]
  (let [above (cumulative (mapv #(mapv (fn [^double h] (m/max 0.0 h)) %) heights) n)
        below (cumulative (mapv #(mapv (fn [^double h] (m/min 0.0 h)) %) heights) n)]
    (mapv (fn [hs ups downs]
            (mapv (fn [^double h ^double up ^double down] (if (neg? h) [(+ down h) down] [up (+ up h)])) hs ups downs))
          heights above below)))

(defn- silhouette-baseline
  "Per index: minus half the total height (the stack centred on 0)."
  [heights]
  (mapv #(* -0.5 (double %)) (apply mapv + heights)))

(defn- wiggle-baseline
  "Per index the baseline of d3 `stackOffsetWiggle`: 0 at the first index, then moved by `-s2 / s1` - `s1` the total
  height, `s2` the heights weighted by the slope of each layer's middle (half its own change plus the changes of the
  layers below)."
  [heights ^long n]
  (vec (reductions (fn [^double y ^long j]
                     (let [[^double s1 ^double s2]
                           (reduce (fn [[^double s1 ^double s2 ^double below] hs]
                                     (let [h (double (hs j)) dh (- h (double (hs (dec j))))]
                                       [(+ s1 h) (+ s2 (* h (+ below (* 0.5 dh)))) (+ below dh)]))
                                   [0.0 0.0 0.0] heights)]
                       (if (zero? s1) y (- y (/ s2 s1)))))
                   0.0 (range 1 n))))

(defn- shifted
  "Stacked `[y0 y1]` per group per index moved by `baseline` (one value per index)."
  [stack baseline]
  (mapv (fn [ys] (mapv (fn [[^double y0 ^double y1] ^double b] [(+ y0 b) (+ y1 b)]) ys baseline)) stack))

(defn stack-values
  "Per group per index `[y0 y1]` of `heights` (one vector per group, in group order, all of the same length - one
  value per shared index) for `position` (see the namespace doc). No groups give `[]`.
  Throws `ex-info` `{:key :position}` for `:mirror` with other than two groups, and for negative heights with
  `:normalize` or a stream position."
  [position heights]
  (let [heights (mapv #(mapv double %) heights)
        n (long (count (first heights)))]
    (when-not (apply = n (map count heights))
      (throw (ex-info "Group heights should have the same length." {:key :heights :value (mapv count heights)})))
    (when (and (contains? (conj stream-positions :normalize) position) (some neg? (apply concat heights)))
      (schema/fail! (str "Position " position " needs non-negative values.") {:key :position :value position}))
    (cond
      (empty? heights) []
      (#{:overlay :dodge} position) (mapv (fn [hs] (mapv (fn [h] [0.0 h]) hs)) heights)
      (= :mirror position) (if (= 2 (count heights))
                             [(mapv (fn [h] [0.0 h]) (heights 0)) (mapv (fn [^double h] [(- h) 0.0]) (heights 1))]
                             (schema/fail! "Position :mirror needs exactly two groups." {:key :position :groups (count heights)}))
      (= :stack position) (stacked heights n)
      (= :normalize position) (let [totals (apply mapv + heights)
                                    scale (mapv (fn [^double t] (if (pos? t) (/ 1.0 t) 0.0)) totals)]
                                (stacked (mapv #(mapv * % scale) heights) n))
      (= :silhouette position) (shifted (stacked heights n) (silhouette-baseline heights))
      (= :wiggle position) (shifted (stacked heights n) (wiggle-baseline heights n))
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
