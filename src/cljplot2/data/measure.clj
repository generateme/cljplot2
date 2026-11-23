(ns cljplot2.data.measure
  "Count measures: bar heights from counts.

  Shared by the histogram (one bar per bin) and the bar chart (one bar per level). Measures:

  * `:count` - the count
  * `:percent` - percent of the counts' total
  * `:pmf` - share of the total (the heights sum to 1)
  * `:density` - count / (total * width), the bars' area 1 (histogram bins)

  Functions: [[heights]] gives the bar heights of a measure; [[titles]] holds each measure's axis title."
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def titles
  "Axis title of each count measure."
  {:percent "Percent of Total" :count "Count" :pmf "Probability" :density "Density"})

(defn heights
  "Computes bar heights of one group's counts for a count measure.

  Parameters:

  - `measure` (keyword): `:count`, `:percent`, `:pmf` or `:density` (see the namespace doc).
  - `counts` (sequence of longs): one count per bar.
  - `widths` (sequence of doubles): one bar width per bar, used by `:density` only.

  Returns a vector of doubles, one height per bar; all heights are 0 when the counts' total is 0.

  See also [[titles]]."
  [measure counts widths]
  (let [total (v/sum counts)]
    (mapv (fn [^long cnt ^double width]
            (if (m/zero? total)
              0.0
              (case measure
                :count (double cnt)
                :percent (m// (m/* 100.0 cnt) total)
                :pmf (m// cnt total)
                :density (m// cnt (m/* total width)))))
          counts widths)))
