(ns cljplot2.data.dots
  "Dot plot bins: values grouped into bins of one width, each bin a stack of dots (see [[cljplot2.chart.dotplot]]).

  * [[dot-density-bins]] - Wilkinson's dot density (ggplot `geom_dotplot` `method \"dotdensity\"`, Vega `dotbin`): a
    bin starts at the smallest value not yet binned and holds the values below `start + binwidth`; its centre is the
    midpoint of its smallest and largest value - so neighbouring centres can be closer than one bin width (and dots
    one bin wide overlap a little), as in ggplot
  * [[fixed-bins]] - histogram bins of one width from an origin (ggplot `method \"histodot\"`, Vega `bin` + `stack`)

  A bin is `{:center c :values [v ...]}` (values increasing)."
  (:require [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn default-binwidth
  "Bin width of the finite `values` without `:binwidth`: their range / 30 (ggplot's default), 1 for a single
  distinct value."
  ^double [values]
  (let [lo (double (reduce min values)) hi (double (reduce max values))]
    (if (> hi lo) (/ (- hi lo) 30.0) 1.0)))

(defn dot-density-bins
  "Wilkinson dot density bins of the increasing `values` (see the namespace doc) of `binwidth` (> 0)."
  [values ^double binwidth]
  (loop [vs (seq values) bins []]
    (if-not vs
      bins
      (let [start (double (first vs))
            [in more] (split-with #(< (double %) (+ start binwidth)) vs)
            in (vec in)]
        (recur (seq more) (conj bins {:center (* 0.5 (+ start (double (peek in)))) :values in}))))))

(defn fixed-bins
  "Bins of the increasing `values` of `binwidth` (> 0) from `origin`: bin `k` holds `[origin + k w, origin + (k+1) w)`,
  centred in it; empty bins left out."
  [values ^double binwidth ^double origin]
  (->> values
       (partition-by #(long (m/floor (/ (- (double %) origin) binwidth))))
       (mapv (fn [vs] (let [k (long (m/floor (/ (- (double (first vs)) origin) binwidth)))]
                        {:center (+ origin (* binwidth (+ (double k) 0.5))) :values (vec vs)})))))
