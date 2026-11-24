(ns cljplot2.data.position
  "Group positions: where the marks of several groups go when they share positions (bins of a histogram, evaluation
  points of a density). Pure geometry in data units, shared by charts (see [[cljplot2.data.group]] for grouping).

  Positions:

  * `:overlay` - every group from 0 (marks drawn over each other)
  * `:dodge` - side by side: the shared interval split among the groups ([[dodge-interval]]), heights from 0
  * `:stack` - groups on top of each other, in group order; negative heights stack downward from 0, separately - in
    group order too, or (`:negative-order :reverse`) in reverse group order: the last group nearest 0, so groups ordered
    from one end to the other diverge from the middle (Likert scales)
  * `:normalize` - stacked and normalized: per index the groups sum to 1 (an index summing to 0 stays at 0)
  * `:mirror` - exactly two groups, the second drawn downward (`[-h 0]`)

  Stream positions ([[stream-positions]], streamgraphs): the stack shifted by a baseline per index -
  `:silhouette` centred on 0 (d3 `stackOffsetSilhouette`), `:wiggle` minimising the slopes of the layers (d3
  `stackOffsetWiggle`, Byron & Wattenberg; the slope per index, not per x distance). `:normalize` and the stream
  positions need non-negative heights.

  Functions: [[golden-angle]] - the angle spacing spiral and radial placements evenly; [[stack-values]] - per group per index `[y0 y1]` of a position; [[dodge-interval]] - a group's share of a dodged interval; [[symmetric-extent]] - an extent symmetric about 0."
  (:require [fastmath.core :as m]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^{:const true :tag 'double} golden-angle
  "The golden angle in radians, `π (3 - √5)`: successive turns by it spread points around a centre without lining them up (Vogel's spiral)."
  (m/* m/PI (m/- 3.0 m/SQRT5)))

(def positions
  "Group positions of every grouping chart."
  #{:overlay :dodge :stack :normalize :mirror})

(def stream-positions
  "Stacked positions shifted by a baseline (streamgraphs): line and density only."
  #{:silhouette :wiggle})

(defn- cumulative
  "Computes per group the running sums of the heights of the groups before it.

  Parameters:

  - `heights` (sequence of sequences of doubles): one sequence per group, in group order.
  - `n` (long): the shared length of the groups.

  Returns `[[0 ...] [h0 ...] [h0+h1 ...] ...]` - one vector per group, the first all zeros."
  [heights ^long n]
  (vec (reductions (fn [acc hs] (mapv m/+ acc hs)) (vec (repeat n 0.0)) (butlast heights))))

(defn- stacked
  "Stacks groups of heights: positive ones upward from 0, negative ones downward.

  Parameters:

  - `heights` (vector of vectors of doubles): one vector per group, in group order.
  - `n` (long): the shared length of the groups.
  - `reverse-negative?` (optional, default `false`): stack negative heights in reverse group order - the last group nearest 0.

  Returns per group per index `[y0 y1]`."
  ([heights n] (stacked heights n false))
  ([heights ^long n reverse-negative?]
   (let [above (cumulative (map #(map (fn [^double h] (m/max 0.0 h)) %) heights) n)
         negative (map #(map (fn [^double h] (m/min 0.0 h)) %) heights)
         below (if reverse-negative?
                 (rseq (cumulative (reverse negative) n))
                 (cumulative negative n))]
     (mapv (fn [hs ups downs]
             (mapv (fn [^double h ^double up ^double down] (if (m/neg? h) [(m/+ down h) down] [up (m/+ up h)])) hs ups downs))
           heights above below))))

(defn- silhouette-baseline
  "Computes the silhouette baseline of stacked groups: minus half the total height per index, centring the stack on 0.

  Parameters:

  - `heights` (vector of vectors of doubles): one vector per group, all of one length.

  Returns a vector of doubles, one per index."
  [heights]
  (mapv (fn [^double total] (m/* -0.5 total)) (apply mapv m/+ heights)))

(defn- wiggle-baseline
  "Computes the baseline of d3 `stackOffsetWiggle` (Byron and Wattenberg), minimising the slopes of the layers.

  The baseline is 0 at the first index, then moves by `-s2 / s1` per index: `s1` the total height, `s2` the heights weighted by the slope of each layer's middle (half its own change plus the changes of the layers below); an index of total height 0 keeps the previous baseline.

  Parameters:

  - `heights` (vector of vectors of doubles): one vector per group, in group order.
  - `n` (long): the shared length of the groups.

  Returns a vector of `n` doubles."
  [heights ^long n]
  (vec (reductions (fn [^double y ^long j]
                     (let [[^double s1 ^double s2]
                           (reduce (fn [[^double total ^double weighted ^double below] hs]
                                     (let [h (double (hs j))
                                           dh (m/- h (double (hs (m/long-dec j))))]
                                       [(m/+ total h) (m/+ weighted (m/* h (m/+ below (m/* 0.5 dh)))) (m/+ below dh)]))
                                   [0.0 0.0 0.0] heights)]
                       (if (m/zero? s1) y (m/- y (m// s2 s1)))))
                   0.0 (range 1 n))))

(defn- shifted
  "Moves stacked groups by a baseline.

  Parameters:

  - `stack` (vector): per group per index `[y0 y1]`.
  - `baseline` (sequence of doubles): one shift per index.

  Returns `stack` with both ends of every `[y0 y1]` moved by the index's shift."
  [stack baseline]
  (mapv (fn [ys] (mapv (fn [[^double y0 ^double y1] ^double b] [(m/+ y0 b) (m/+ y1 b)]) ys baseline)) stack))

(defn stack-values
  "Computes where the marks of several groups sharing indices go for a group position.

  Parameters:

  - `position` (keyword): one of [[positions]] or [[stream-positions]] (see the namespace doc).
  - `heights` (sequence of sequences of numbers): one sequence per group, in group order, all of one length - one value per shared index.
  - `options` (map, optional): `:negative-order` - `:forward` (default) or `:reverse`, the order negative heights stack in with `:stack`.

  Returns per group per index `[y0 y1]` (doubles); no groups give `[]`.

  Throws `ex-info {:key :heights}` for groups of different lengths, `{:key :position}` for `:mirror` with other than two groups, for negative heights with `:normalize` or a stream position, and for an unknown position."
  ([position heights] (stack-values position heights nil))
  ([position heights {:keys [negative-order]}]
   (let [values (mapv #(mapv double %) heights)
         n (long (count (first values)))]
     (when-not (apply = n (map count values))
       (schema/fail! "Group heights should have the same length." {:key :heights :value (mapv count values)}))
     (when (and (contains? (conj stream-positions :normalize) position) (some m/neg? (apply concat values)))
       (schema/fail! (str "Position " position " needs non-negative values.") {:key :position :value position}))
     (cond
       (empty? values) []
       (#{:overlay :dodge} position) (mapv (fn [hs] (mapv (fn [h] [0.0 h]) hs)) values)
       (= :mirror position) (if (m/== 2 (count values))
                              [(mapv (fn [h] [0.0 h]) (values 0)) (mapv (fn [^double h] [(m/- h) 0.0]) (values 1))]
                              (schema/fail! "Position :mirror needs exactly two groups." {:key :position :groups (count values)}))
       (= :stack position) (stacked values n (= :reverse negative-order))
       (= :normalize position) (let [totals (apply mapv m/+ values)
                                     scale (mapv (fn [^double t] (if (m/pos? t) (m// t) 0.0)) totals)]
                                 (stacked (mapv #(mapv m/* % scale) values) n))
       (= :silhouette position) (shifted (stacked values n) (silhouette-baseline values))
       (= :wiggle position) (shifted (stacked values n) (wiggle-baseline values n))
       :else (schema/fail! (str "Unknown position " (pr-str position) ".") {:key :position :value position})))))

(defn dodge-interval
  "Gives one of `n` equal sub-intervals of an interval - a group's share of a dodged position.

  Parameters:

  - `interval` (`[a b]`, numbers): the shared interval.
  - `i` (long): the sub-interval's index, from 0.
  - `n` (long): the count of sub-intervals, positive.

  Returns `[lo hi]` (doubles), the `i`-th sub-interval from `a`."
  [[^double a ^double b] ^long i ^long n]
  (let [w (m// (m/- b a) n)]
    [(m/+ a (m/* i w)) (m/+ a (m/* (m/long-inc i) w))]))

(defn symmetric-extent
  "Gives an extent symmetric about 0 covering numbers.

  Parameters:

  - `values` (sequence of numbers).

  Returns `[-M M]` (doubles), `M` the largest magnitude of `values`; `M` is 0 for none."
  [values]
  (let [magnitude (double (reduce (fn [^double acc v] (m/max acc (m/abs (double v)))) 0.0 values))]
    [(m/- magnitude) magnitude]))
