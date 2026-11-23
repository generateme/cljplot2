(ns cljplot2.data.dots
  "Dot plot bins: values grouped into bins of one width, each bin a stack of dots (see [[cljplot2.chart.dotplot]]).

  * [[dot-density-bins]] - Wilkinson's dot density (ggplot `geom_dotplot` `method \"dotdensity\"`, Vega `dotbin`): a
    bin starts at the smallest value not yet binned and holds the values below `start + binwidth`; its centre is the
    midpoint of its smallest and largest value - so neighbouring centres can be closer than one bin width (and dots
    one bin wide overlap a little), as in ggplot
  * [[fixed-bins]] - histogram bins of one width from an origin (ggplot `method \"histodot\"`, Vega `bin` + `stack`)

  A bin is `{:center c :values [v ...]}` (values increasing).

  Functions: [[default-binwidth]] - the bin width when none is given, [[dot-density-bins]], [[fixed-bins]]."
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn default-binwidth
  "Default bin width of a dot plot: the range of the values over 30 (ggplot's default).

  Parameters:

  - `values` (sequence of finite numbers): the binned values, not empty.

  Returns the bin width as a double; 1 for a single distinct value.

  See also [[dot-density-bins]], [[fixed-bins]]."
  ^double [values]
  (let [lo (v/mn values)
        hi (v/mx values)]
    (if (m/> hi lo) (m// (m/- hi lo) 30.0) 1.0)))

(defn dot-density-bins
  "Wilkinson dot density bins of sorted values (see the namespace doc).

  Parameters:

  - `values` (sequence of numbers): increasing values.
  - `binwidth` (double): the bin width, positive.

  Returns a vector of bins `{:center c :values [v ...]}` in increasing order: each starts at the smallest value not yet binned, holds the values below its start plus `binwidth`, centred at the midpoint of its smallest and largest value. No values - an empty vector.

  See also [[fixed-bins]], [[default-binwidth]]."
  [values ^double binwidth]
  (loop [vs (seq values) bins []]
    (if-not vs
      bins
      (let [start (double (first vs))
            [in-bin more] (split-with #(m/< (double %) (m/+ start binwidth)) vs)
            bin-values (vec in-bin)]
        (recur (seq more) (conj bins {:center (m/* 0.5 (m/+ start (double (peek bin-values)))) :values bin-values}))))))

(defn fixed-bins
  "Histogram bins of one width from an origin, as stacks of dots.

  Parameters:

  - `values` (sequence of numbers): increasing values.
  - `binwidth` (double): the bin width `w`, positive.
  - `origin` (double): the start of bin 0.

  Returns a vector of the non-empty bins `{:center c :values [v ...]}` in increasing order: bin `k` holds the values in `[origin + k w, origin + (k + 1) w)` and is centred in that interval.

  See also [[dot-density-bins]]."
  [values ^double binwidth ^double origin]
  (->> values
       (partition-by (fn [^double v] (long (m/floor (m// (m/- v origin) binwidth)))))
       (mapv (fn [vs] (let [k (m/floor (m// (m/- (double (first vs)) origin) binwidth))]
                       {:center (m/+ origin (m/* binwidth (m/+ k 0.5))) :values (vec vs)})))))
