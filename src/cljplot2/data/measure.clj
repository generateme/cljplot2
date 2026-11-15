(ns cljplot2.data.measure
  "Count measures: bar heights from counts, shared by the histogram (one bar per bin) and the bar chart (one bar per
  level).

  * `:count` - the count
  * `:percent` - percent of the counts' total
  * `:pmf` - share of the total (the heights sum to 1)
  * `:density` - count / (total * width), the bars' area 1 (histogram bins)")

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def titles
  "Axis title of each count measure."
  {:percent "Percent of Total" :count "Count" :pmf "Probability" :density "Density"})

(defn heights
  "Heights of `counts` (one per bar, all bars of one group) for count `measure` (see the namespace doc); `widths` -
  one per bar, used by `:density` only. All heights are 0 when the counts' total is 0."
  [measure counts widths]
  (let [total (double (reduce + 0 counts))]
    (mapv (fn [^long cnt ^double width]
            (if (zero? total)
              0.0
              (case measure
                :count (double cnt)
                :percent (/ (* 100.0 cnt) total)
                :pmf (/ cnt total)
                :density (/ cnt (* total width)))))
          counts widths)))
