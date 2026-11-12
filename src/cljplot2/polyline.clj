(ns cljplot2.polyline
  "Polylines in pixel coordinates: stroked runs of points, broken where a coordinate is NaN, and areas under them.

  Line style: `{:color c :width w :dash d}` - `:color` a clojure2d color or `:none`, `:width` pixels (0 draws
  nothing), `:dash` `:none` / absent or a sequence of positive dash lengths."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.theme :as theme]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn validate-style!
  "Check line `style` of chart `kind`; throws `ex-info` naming the offending key."
  [kind {:keys [color width dash]}]
  (when (nil? color)
    (throw (ex-info "Line :color must be a color or :none." {:key :color :kind kind})))
  (when-not (and (number? width) (>= (double width) 0.0))
    (throw (ex-info "Line :width must be a non-negative number." {:key :width :kind kind :value width})))
  (when-not (or (nil? dash) (theme/none? dash) (and (sequential? dash) (seq dash) (every? #(and (number? %) (pos? (double %))) dash)))
    (throw (ex-info "Line :dash must be :none or a sequence of positive numbers." {:key :dash :kind kind :value dash}))))

(defn runs
  "Runs of consecutive points `[x y]` (from `xs`, `ys`) with finite coordinates; a non-finite coordinate ends a run."
  [xs ys]
  (->> (map vector xs ys)
       (partition-by (fn [[x y]] (and (m/valid-double? (double x)) (m/valid-double? (double y)))))
       (filter (fn [[[x y]]] (and (m/valid-double? (double x)) (m/valid-double? (double y)))))
       (mapv vec)))

(defn set-style!
  "Set the canvas color and stroke of line `style`; false when the style draws nothing."
  [canvas {:keys [color width dash]}]
  (when (and (not (theme/none? color)) (pos? (double width)))
    (c2d/set-color canvas color)
    (c2d/set-stroke-custom canvas (cond-> {:size (double width) :cap :butt :join :round}
                                    (and dash (not (theme/none? dash))) (assoc :dash (vec dash))))
    true))

(defn draw!
  "Stroke the runs of `xs` / `ys` (pixels) with line `style`; a single-point run draws nothing."
  [canvas xs ys style]
  (when (set-style! canvas style)
    (doseq [run (runs xs ys) :when (> (count run) 1)]
      (c2d/path canvas run false true))))

(defn fill-under!
  "Fill the area between each run of `xs` / `ys` (pixels) and the horizontal line at pixel `baseline` with `color`
  (`:none` fills nothing)."
  [canvas xs ys baseline color]
  (when-not (theme/none? color)
    (c2d/set-color canvas color)
    (doseq [run (runs xs ys) :when (> (count run) 1)
            :let [[x0] (first run) [x1] (peek run)]]
      (c2d/path canvas (-> [[x0 baseline]] (into run) (conj [x1 baseline])) true false))))
