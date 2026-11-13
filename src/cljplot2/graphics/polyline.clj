(ns cljplot2.graphics.polyline
  "Polylines in pixel coordinates: stroked runs of points, broken where a coordinate is NaN, and areas between them
  and a baseline.

  Line style: `{:color c :width w :dash d}` - `:color` a clojure2d color or `:none`, `:width` pixels (0 draws
  nothing), `:dash` `:none` / absent or a sequence of positive dash lengths."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def style-entries
  "Map entries of a line style (all optional; defaults from the theme)."
  [(schema/optional :color schema/color)
   (schema/optional :width schema/non-neg)
   (schema/nilable :dash schema/dash)])

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

(defn- finite-runs
  "Runs of consecutive indices of `xs` / `ys` whose coordinates are finite; a non-finite coordinate ends a run."
  [xs ys]
  (let [finite? (fn [i] (and (m/valid-double? (double (nth xs i))) (m/valid-double? (double (nth ys i)))))]
    (->> (range (count xs))
         (partition-by finite?)
         (filter (comp finite? first))
         (mapv vec))))

(defn fill-under!
  "Fill the area between each run of `xs` / `ys` (pixels) and its baseline: the polygon from the base point of the
  run's first point through the run to the base point of its last one (`bxs` / `bys` - the base point of every
  point, e.g. the projection of `(x, 0)`; a horizontal baseline for unflipped charts, vertical for flipped).
  `:none` fills nothing."
  [canvas xs ys bxs bys color]
  (when-not (theme/none? color)
    (c2d/set-color canvas color)
    (doseq [run (finite-runs xs ys) :when (> (count run) 1)
            :let [i0 (first run) i1 (peek run)]]
      (c2d/path canvas (-> [[(nth bxs i0) (nth bys i0)]]
                           (into (map (fn [i] [(nth xs i) (nth ys i)])) run)
                           (conj [(nth bxs i1) (nth bys i1)]))
                true false))))

(defn fill-between!
  "Fill the area between the upper curve `xs` / `ys` and the lower curve `bxs` / `bys` (pixels, point `i` of one
  paired with point `i` of the other): one polygon per run of indices where both points are finite. `:none` fills
  nothing."
  [canvas xs ys bxs bys color]
  (when-not (theme/none? color)
    (c2d/set-color canvas color)
    (doseq [run (finite-runs (mapv (fn [a b] (if (m/valid-double? (double b)) a ##NaN)) xs bxs)
                             (mapv (fn [a b] (if (m/valid-double? (double b)) a ##NaN)) ys bys))
            :when (> (count run) 1)]
      (c2d/path canvas (-> []
                           (into (map (fn [i] [(nth xs i) (nth ys i)])) run)
                           (into (map (fn [i] [(nth bxs i) (nth bys i)])) (rseq run)))
                true false))))
