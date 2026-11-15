(ns cljplot2.graphics.polyline
  "Polylines in pixel coordinates: stroked runs of points, broken where a coordinate is NaN, and areas between them
  and a baseline.

  Line style: `{:color c :width w :dash d}` - `:color` a clojure2d color or `:none`, `:width` pixels (0 draws
  nothing), `:dash` `:none` / absent, a name of [[cljplot2.schema/dash-styles]] or a sequence of positive dash
  lengths (resolved by [[dash-pattern]] - every stroke with a dash goes through it)."
  (:require [clojure2d.color :as c]
            [clojure2d.core :as c2d]
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

(defn finite-runs
  "Runs of consecutive indices of `xs` / `ys` whose coordinates are finite; a non-finite coordinate ends a run."
  [xs ys]
  (let [finite? (fn [i] (and (m/valid-double? (double (nth xs i))) (m/valid-double? (double (nth ys i)))))]
    (->> (range (count xs))
         (partition-by finite?)
         (filter (comp finite? first))
         (mapv vec))))

(defn runs
  "Runs of consecutive points `[x y]` (from `xs`, `ys`) with finite coordinates; a non-finite coordinate ends a run."
  [xs ys]
  (mapv (fn [run] (mapv (fn [i] [(nth xs i) (nth ys i)]) run)) (finite-runs xs ys)))

(defn dash-pattern
  "Dash lengths (a vector of doubles) of a validated `dash`: `nil` for a solid line (`nil`, `:none`, `:solid`), the
  lengths of a named style (see [[cljplot2.schema/dash-styles]]) or the given lengths."
  [dash]
  (let [lengths (get schema/dash-styles dash dash)]
    (when (and (some? lengths) (not (theme/none? lengths)))
      (mapv double lengths))))

(defn set-style!
  "Set the canvas color and stroke of line `style`; false when the style draws nothing."
  [canvas {:keys [color width dash]}]
  (when (and (not (theme/none? color)) (pos? (double width)))
    (c2d/set-color canvas color)
    (c2d/set-stroke-custom canvas (cond-> {:size (double width) :cap :butt :join :round}
                                    (dash-pattern dash) (assoc :dash (dash-pattern dash))))
    true))

(defn stroke-runs!
  "Stroke every run (a vector of `[x y]` pixel points) of `point-runs` with line `style`; a run of fewer than 2 points
  draws nothing."
  [canvas point-runs style]
  (when (set-style! canvas style)
    (doseq [run point-runs :when (> (count run) 1)]
      (c2d/path canvas run false true))))

(defn draw!
  "Stroke the runs of `xs` / `ys` (pixels) with line `style`; a single-point run draws nothing."
  [canvas xs ys style]
  (stroke-runs! canvas (runs xs ys) style))

(defn bridge-style
  "Line style of a bridge over a gap of a line with `style`: its color with the alpha multiplied by `bridge` `:alpha`
  (a fraction), `bridge` `:width` / `:dash` over the line's when given."
  [style {:keys [alpha] :as bridge}]
  (let [color (:color style)]
    (cond-> (merge style (select-keys bridge [:width :dash]))
      (not (theme/none? color)) (assoc :color (let [c (c/to-color color)]
                                                (c/set-alpha c (* (double alpha) (double (c/alpha c)))))))))

(def bridge-schema
  "Theme `:bridge` of a line style: `:alpha` (fraction of the line alpha), optional `:width`, `:dash`."
  [:map {:closed true}
   [:alpha schema/fraction]
   (schema/optional :width schema/non-neg)
   (schema/nilable :dash schema/dash)])

(defn draw-segments!
  "Stroke every segment of `xs` / `ys` (pixels) between consecutive finite points - segment `i` from point `i` to
  `i + 1` - in color `(segment-colors i)` (`nil` or `:none` skips it), with the `:width` and `:dash` of line `style`
  (each segment restarts the dash) and round caps, so corners show no gaps."
  [canvas xs ys segment-colors {:keys [width dash]}]
  (when (pos? (double width))
    (c2d/set-stroke-custom canvas (cond-> {:size (double width) :cap :round :join :round}
                                    (dash-pattern dash) (assoc :dash (dash-pattern dash))))
    (doseq [run (finite-runs xs ys)
            [i j] (partition 2 1 run)
            :let [color (segment-colors i)]
            :when (and (some? color) (not (theme/none? color)))]
      (c2d/set-color canvas color)
      (c2d/line canvas (double (nth xs i)) (double (nth ys i)) (double (nth xs j)) (double (nth ys j))))))

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

(defn fill-runs-to-baseline!
  "Fill the area between each run of `point-runs` (pixels) and the baseline at pixel `baseline`: horizontal
  (`horizontal?`, a y position - unflipped charts) or vertical (an x position); the polygon runs from the base point
  of the run's first point through the run to the base point of its last. `:none` fills nothing."
  [canvas point-runs baseline horizontal? color]
  (when-not (theme/none? color)
    (c2d/set-color canvas color)
    (let [baseline (double baseline)
          base (if horizontal? (fn [[x _]] [x baseline]) (fn [[_ y]] [baseline y]))]
      (doseq [run point-runs :when (> (count run) 1)]
        (c2d/path canvas (-> [(base (first run))] (into run) (conj (base (peek run)))) true false)))))

(defn fill-closed-runs!
  "Fill every run of `point-runs` (pixels) as the polygon it closes. `:none` fills nothing."
  [canvas point-runs color]
  (when-not (theme/none? color)
    (c2d/set-color canvas color)
    (doseq [run point-runs :when (> (count run) 2)]
      (c2d/path canvas run true false))))

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
