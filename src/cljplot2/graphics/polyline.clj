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
            [cljplot2.theme :as theme])
  (:import [java.awt Color Graphics2D LinearGradientPaint]))

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

(defn- path-length
  "Length of the polyline through `points` (pixels)."
  ^double [points]
  (reduce + 0.0 (map (fn [[x0 y0] [x1 y1]] (m/hypot (- (double x1) (double x0)) (- (double y1) (double y0))))
                     points (rest points))))

(defn draw-segments!
  "Stroke every segment of `xs` / `ys` (pixels) between consecutive finite points - segment `i` from point `i` to
  `i + 1` - in color `(segment-colors i)` (`nil` or `:none` skips it), with the `:width` and `:dash` of line `style`:
  consecutive segments of one color form one path (round joins, round caps at its ends, so color changes show no
  gaps); the dash runs on along each run of finite points, across color changes and skipped segments."
  [canvas xs ys segment-colors {:keys [width dash]}]
  (when (pos? (double width))
    (let [pattern (dash-pattern dash)
          stroke (cond-> {:size (double width) :cap :round :join :round} pattern (assoc :dash pattern))]
      (doseq [run (finite-runs xs ys)]
        (reduce (fn [^double phase segments]
                  (let [color (segment-colors (ffirst segments))
                        points (mapv (fn [i] [(double (nth xs i)) (double (nth ys i))])
                                     (cons (ffirst segments) (map second segments)))]
                    (when (and (some? color) (not (theme/none? color)))
                      (c2d/set-stroke-custom canvas (cond-> stroke pattern (assoc :dash-phase phase)))
                      (c2d/set-color canvas color)
                      (c2d/path canvas points false true))
                    (+ phase (path-length points))))
                0.0
                (partition-by (fn [[i _]] (segment-colors i)) (partition 2 1 run)))))))

(defn key-glyph!
  "Legend key glyph of a line in `box` (`{:x :y :w :h}`): a horizontal line through the middle in line `style`; with
  an `area` color (not `:none`) a filled square (0.7 of the smaller side) outlined in the line style."
  [canvas {:keys [x y w h]} style area]
  (let [cx (+ (double x) (* 0.5 (double w)))
        cy (+ (double y) (* 0.5 (double h)))]
    (if (theme/none? area)
      (draw! canvas [(double x) (+ (double x) (double w))] [cy cy] style)
      (let [side (* 0.7 (m/min (double w) (double h)))]
        (c2d/set-color canvas area)
        (c2d/crect canvas cx cy side side)
        (when (set-style! canvas style)
          (c2d/crect canvas cx cy side side true))))))

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

;; areas

(defn area-color
  "Fill of a group's area with line style `st` (group attributes merged): the group colour with opacity `:alpha`
  (0..1) for `:fill :color` or for stacked areas (`stacked?`) without a fill, else `:fill` (`:none` - no area)."
  [st stacked?]
  (if (or (= :color (:fill st)) (and stacked? (theme/none? (:fill st :none))))
    (c/set-alpha (:color st) (* 255.0 (double (:alpha st))))
    (:fill st :none)))

(def fill-gradient-schema
  "Schema of `:fill-gradient`: a clojure2d gradient - a preset keyword, a colour list or a function of `t`."
  (schema/named-type ::fill-gradient #(or (keyword? %) (sequential? %) (fn? %))
                     "should be a clojure2d gradient: a preset keyword, a colour list or a function"))

(defn gradient-fn
  "clojure2d gradient of the `:fill-gradient` option `g` (a function is taken as is); throws `ex-info
  {:key :fill-gradient}` for an unknown one."
  [g]
  (if (fn? g)
    g
    (try (c/gradient g)
         (catch Exception e
           (schema/fail! "Unknown :fill-gradient." {:key :fill-gradient :value g :cause (ex-message e)})))))

(defn fill-gradient-quads!
  "Fill between the paired point runs `upper` and `lower` (`[x y]` pixels) quad by quad, each coloured by `gradient`
  at its middle position (`ps`, one per point) relative to `[p0 p1]`; quads with a non-finite corner are skipped."
  [canvas upper lower ps gradient [p0 p1]]
  (let [p0 (double p0) span (- (double p1) p0)
        ok? (fn [[x y]] (and (m/valid-double? (double x)) (m/valid-double? (double y))))]
    (c2d/set-stroke-custom canvas {:size 1.0 :join :miter})
    (doseq [^long i (range (dec (count upper)))
            :let [a (upper i) b (upper (inc i)) c (lower (inc i)) d (lower i)]
            :when (and (ok? a) (ok? b) (ok? c) (ok? d))]
      (let [t (if (pos? span) (/ (- (* 0.5 (+ (double (ps i)) (double (ps (inc i))))) p0) span) 0.5)
            quad [a b c d]]
        (c2d/set-color canvas (gradient (m/constrain t 0.0 1.0)))
        (c2d/path canvas quad true false)
        (c2d/path canvas quad true true)))))

(def ^:private ^:const gradient-stops
  "Stops of a linear gradient paint sampled from a gradient function."
  32)

(defn fill-gradient-paint!
  "Fill every polygon of `polygons` (`[x y]` pixel points) with a linear gradient paint from pixel point `from`
  (`gradient` at 0) to `to` (at 1), [[gradient-stops]] stops sampled; the paint is reset to the gradient's last
  colour afterwards (callers set their colour again)."
  [canvas polygons gradient [fx fy] [tx ty]]
  (let [fractions (float-array (map #(/ (double %) (dec gradient-stops)) (range gradient-stops)))
        colors (into-array Color (map #(c/awt-color (gradient (aget fractions %))) (range gradient-stops)))
        ^Graphics2D g (:graphics canvas)]
    (when-not (and (== (double fx) (double tx)) (== (double fy) (double ty)))
      (.setPaint g (LinearGradientPaint. (float fx) (float fy) (float tx) (float ty) fractions colors))
      (doseq [polygon polygons :when (> (count polygon) 2)]
        (c2d/path canvas polygon true false))
      (c2d/set-color canvas (gradient 1.0)))))

