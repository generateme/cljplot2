(ns cljplot2.graphics.curve
  "Curves in pixel coordinates: the runs of a line (consecutive finite points, see [[cljplot2.graphics.polyline]])
  turned into the points that are drawn, plus what lines and areas share around them (bridges over gaps, the area
  baseline, the drawn point - clamped and rounded, see [[drawn-point]]).

  A run is a vector of `[x y]` pixel points. Curve options (`opts`):

  * `:interpolator` - a fastmath 1D interpolation method ([[cljplot2.schema/interpolation-methods]]) or `(fn [xs ys & params] f)`
    returning a function of x; `:interpolator-params` - a vector passed after `xs ys` (as wadogo `:interpolated`).
    Without `:curve`: functional interpolation - each run is fitted along the horizontal pixel axis of the line
    (`:along` `:x` - the default - or `:y` for a flipped chart) and evaluated on a [[sample-grid]]; the run's points
    must be strictly monotone along it (repeated or reversed positions throw `ex-info {:key :interpolator}`).
  * `:curve :parametric` - x(t) and y(t) interpolated (default `:cubic`) over the cumulative chord length t (pixels),
    so non-functional paths (loops, connected scatter) pass through their points; `:curve :bezier` - clojure2d
    `path-bezier` through the points (with `:interpolator` - `ex-info {:key :curve}`).
  * `:sample-step` - the distance between evaluated positions: pixels (default 1.0) or `{:count n}` (n >= 2 per run).

  A run with fewer points than the method needs (fastmath `:too-few-points`) is drawn straight; one point draws
  nothing.

  Functions: options [[options]], [[sample-step-schema]]; sampling [[sample-grid]], [[sample-columns]]; drawn points [[curve-points]] ([[functional-points]], [[parametric-points]], [[bezier-points]]), [[curved?]], [[drawn-point]]; around lines [[bridges]], [[area-baseline]], [[draw-runs!]]."
  (:require [clojure2d.core :as c2d]
            [clojure2d.core.shape :as sh]
            [fastmath.core :as m]
            [cljplot2.data.interpolation :as fit]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^{:const true :tag 'double} default-sample-step
  "Pixels between evaluated positions."
  1.0)

(def sample-step-schema
  "Schema of `:sample-step`: positive pixels or `{:count n}`, n >= 2."
  (schema/named-type ::sample-step
                     #(or (and (schema/finite? %) (m/pos? (double %)))
                          (and (map? %) (= [:count] (keys %)) (integer? (:count %)) (m/>= (long (:count %)) 2)))
                     "should be a positive number of pixels or {:count n}, n >= 2"))

(def options
  "Map entries of the curve options of a line chart."
  (schema/in-fragment
   :curve
   [(schema/optional :interpolator {:description "interpolation of the line between data points (none - straight segments)"}
                     schema/interpolator)
    (schema/optional :interpolator-params {:description "extra parameters passed to the interpolation method"} [:vector any?])
    (schema/optional :curve {:description "path curve instead of a function of x: :parametric (x(t), y(t) over the chord length) or :bezier"}
                     [:enum :parametric :bezier])
    (schema/optional :sample-step {:description "distance between evaluated positions: pixels or {:count n} per run"
                                   :default default-sample-step}
                     sample-step-schema)]))

;; sampling

(defn sample-grid
  "Gives evenly spaced positions between two ends, both included.

  Parameters:

  - `p0`, `p1` (doubles): the first and the last position.
  - `step`: pixels - the span split into the fewest equal intervals no longer than the step; or `{:count n}` - n positions (see [[sample-step-schema]]).

  Returns a vector of doubles, the last exactly `p1` (positions are `p0 + span i / n`, no accumulated rounding); `[p0]` when the ends are equal."
  [^double p0 ^double p1 step]
  (let [span (m/- p1 p0)
        n (long (if (map? step)
                  (m/long-dec (long (:count step)))
                  (m/max 1.0 (m/ceil (m// (m/abs span) (double step))))))]
    (if (m/zero? span)
      [p0]
      (mapv (fn [^long i] (if (m/== i n) p1 (m/+ p0 (m/* span (m// (double i) (double n)))))) (range (m/long-inc n))))))

;; fitting

(defn- strictly-monotone
  "Gives the direction of values that strictly increase or decrease.

  Parameters:

  - `vs` (sequence of numbers): the values.

  Returns `:increasing` or `:decreasing` (either for fewer than 2 values: `:increasing`), `nil` otherwise."
  [vs]
  (let [pairs (partition 2 1 vs)]
    (cond
      (every? (fn [[a b]] (m/< (double a) (double b))) pairs) :increasing
      (every? (fn [[a b]] (m/> (double a) (double b))) pairs) :decreasing)))

(defn- axis-index
  "Gives the index of a pixel axis in an `[x y]` point.

  Parameters:

  - `along` (keyword): the axis, `:x` or `:y`.

  Returns 1 for `:y`, else 0."
  ^long [along]
  (if (= :y along) 1 0))

(defn functional-points
  "Gives the points of a run interpolated along one pixel axis.

  Parameters:

  - `fit-of`: the fitting function (see [[cljplot2.data.interpolation/fit-fn]]).
  - `run` (vector of `[x y]`): the run's pixel points.
  - `along` (keyword): the axis the run is a function of, `:x` or `:y`.
  - `step`: the sample step (see [[sample-grid]]).

  Returns the evaluated points (a vector) in the run's direction; the run as is with fewer than 2 points or too few for the method. A run not strictly monotone along the axis throws `ex-info {:key :interpolator}`."
  [fit-of run along step]
  (if (m/< (count run) 2)
    run
    (let [a (axis-index along)
          b (m/long-sub 1 a)
          direction (strictly-monotone (map #(nth % a) run))
          _ (when-not direction
              (schema/fail! (str "Interpolation needs strictly monotone positions along " (name along)
                                 " in every run (no repeated x; with :sort false, x in order).")
                            {:key :interpolator :value (mapv #(nth % a) run)}))
          ;; reversed: rseq of the run vector, indexed by peek below
          ordered (if (= direction :decreasing) (vec (rseq run)) run)
          ;; the fitted knots go to fastmath interpolation as vectors
          f (fit/fitted fit-of (mapv #(double (nth % a)) ordered) (mapv #(double (nth % b)) ordered))]
      (if (nil? f)
        run
        (let [ts (sample-grid (double (nth (first run) a)) (double (nth (peek run) a)) step)]
          ;; drawn runs are vectors (callers take their last point with peek)
          (mapv (fn [^double t] (if (m/zero? a) [t (double (f t))] [(double (f t)) t])) ts))))))

(defn- distinct-consecutive
  "Gives a run without consecutive repeated points.

  Parameters:

  - `run` (sequence): `[x y]` points.

  Returns a vector of the points, each run of equal neighbours kept once (repeats apart stay); empty for an empty run."
  [run]
  (mapv first (partition-by identity run)))

(defn parametric-points
  "Gives the points of a run interpolated as x(t), y(t) over the cumulative chord length t.

  Consecutive repeated points are removed first.

  Parameters:

  - `fit-of`: the fitting function (see [[cljplot2.data.interpolation/fit-fn]]).
  - `run` (vector of `[x y]`): the run's pixel points.
  - `step`: the sample step of t (see [[sample-grid]]).

  Returns the evaluated points (a vector); the run as is with fewer than 2 distinct points or too few for the method."
  [fit-of run step]
  (let [distinct-run (distinct-consecutive run)]
    (if (m/< (count distinct-run) 2)
      distinct-run
      (let [ts (polyline/cumulative-lengths distinct-run)
            fx (fit/fitted fit-of ts (mapv #(double (first %)) distinct-run))
            fy (fit/fitted fit-of ts (mapv #(double (second %)) distinct-run))]
        (if (or (nil? fx) (nil? fy))
          distinct-run
          (mapv (fn [t] [(double (fx t)) (double (fy t))]) (sample-grid 0.0 (double (peek ts)) step)))))))

(def ^{:private true :const true :tag 'double} bezier-flatness
  "Largest distance (pixels) of the flattened bezier polyline from the curve."
  0.1)

(defn bezier-points
  "Gives the points of clojure2d `path-bezier` through a run, flattened to a polyline.

  Parameters:

  - `run` (vector of `[x y]`): the run's pixel points.

  Returns a vector of points within [[bezier-flatness]] px of the curve; the run as is with fewer than 2 points."
  [run]
  (if (m/< (count run) 2)
    run
    (vec (c2d/shape->points (sh/path-bezier run false) bezier-flatness))))

(defn curve-points
  "Gives the drawn points of a run under the curve options (see the namespace doc).

  Parameters:

  - `run` (vector of `[x y]`): the run's pixel points.
  - `opts` (map): `:interpolator`, `:interpolator-params`, `:curve`, `:sample-step` (default [[default-sample-step]]), `:along` (default `:x`).

  Returns the points; the run as is without `:interpolator` and `:curve`. `:curve :bezier` with `:interpolator` throws `ex-info {:key :curve}`."
  [run {:keys [interpolator interpolator-params curve sample-step along] :or {along :x}}]
  (let [step (or sample-step default-sample-step)]
    (cond
      (and (= :bezier curve) interpolator)
      (schema/fail! ":curve :bezier draws its own path; it can't be combined with :interpolator."
                    {:key :curve :value curve})
      (= :bezier curve) (bezier-points run)
      (= :parametric curve) (parametric-points (fit/fit-fn (or interpolator :cubic) interpolator-params) run step)
      interpolator (functional-points (fit/fit-fn interpolator interpolator-params) run along step)
      :else run)))

(defn- segment-index
  "Gives the index of the segment of increasing positions holding a position.

  Parameters:

  - `ps` (vector of doubles): increasing positions, at least 2.
  - `t` (double): the position.

  Returns `i` of the segment `[ps(i) ps(i+1)]`; beyond the ends the first or the last segment."
  ^long [ps ^double t]
  (loop [lo 0 hi (m/long-sub (count ps) 2)]
    (if (m/>= lo hi)
      lo
      (let [mid (m/long-quot (m/long-add lo hi 1) 2)]
        (if (m/< t (double (ps mid))) (recur lo (m/long-dec mid)) (recur mid hi))))))

(defn- keep-sign
  "Gives a sample kept to the sign of the two knot values around it when they share it.

  Parameters:

  - `ps` (vector of doubles): increasing knot positions.
  - `vs` (vector of doubles): the knot values.
  - `t` (double): the sample position.
  - `v` (double): the sample value.

  Returns `v`, clamped to 0 when both knots around `t` are on its other side."
  ^double [ps vs ^double t ^double v]
  (let [i (segment-index ps t)
        a (double (vs i)) b (double (vs (m/long-inc i)))]
    (cond
      (and (m/>= a 0.0) (m/>= b 0.0)) (m/max 0.0 v)
      (and (m/<= a 0.0) (m/<= b 0.0)) (m/min 0.0 v)
      :else v)))

(defn sample-columns
  "Gives columns of values sampled on one grid.

  Parameters:

  - `positions` (vector of numbers): strictly monotone pixel positions shared by the columns.
  - `columns` (vector of vectors): one vector of values per column, one value per position.
  - `opts` (map): `:interpolator`, `:interpolator-params`, `:sample-step` (see [[sample-grid]]).
  - `keep-sign?`: truthy - a sample between two knots of one sign keeps it (heights of stacked bands never cross 0 where the data doesn't).

  Returns `[grid sampled-columns]`: the [[sample-grid]] from the first position to the last and each column fitted and evaluated on it; a column too short for the method is fitted linearly (all columns share the grid); fewer than 2 positions - `[positions columns]` as given. Not strictly monotone positions throw `ex-info {:key :interpolator}`."
  [positions columns {:keys [interpolator interpolator-params sample-step]} keep-sign?]
  (if (m/< (count positions) 2)
    [positions columns]
    (let [direction (strictly-monotone positions)
          _ (when-not direction
              (schema/fail! "Interpolation needs strictly monotone x positions." {:key :interpolator :value positions}))
          rev? (= direction :decreasing)
          ;; knots: indexed by keep-sign and passed to fastmath interpolation
          ps (mapv double (if rev? (rseq positions) positions))
          grid (sample-grid (double (first positions)) (double (peek positions)) (or sample-step default-sample-step))
          fit-of (fit/fit-fn interpolator interpolator-params)
          linear (fit/fit-fn :linear nil)]
      [grid (mapv (fn [column]
                    (let [vs (mapv double (if rev? (rseq column) column))
                          f (or (fit/fitted fit-of ps vs) (fit/fitted linear ps vs))]
                      (mapv (fn [^double t] (let [v (double (f t))] (if keep-sign? (keep-sign ps vs t v) v))) grid)))
                  columns)])))

(defn curved?
  "Tests whether curve options change the drawn points.

  Parameters:

  - `opts` (map): the curve options.

  Returns truthy for an `:interpolator` or a `:curve`, else `nil`."
  [opts]
  (or (:interpolator opts) (:curve opts)))

;; gaps

(defn bridges
  "Gives the straight segments over the gaps between consecutive runs.

  Parameters:

  - `runs` (sequence of vectors of `[x y]`): the runs; empty ones skipped.

  Returns a sequence of `[[x0 y0] [x1 y1]]` - the last point of one run and the first of the next; empty for fewer than 2 runs."
  [runs]
  ;; consumed twice (the runs and their rest)
  (let [non-empty (filterv seq runs)]
    (map (fn [a b] [(peek a) (first b)]) non-empty (rest non-empty))))

;; pixels

(def ^{:const true :tag 'double} pixel-limit
  "Largest pixel coordinate magnitude drawn: farther points are clamped to it (Java2D draws float coordinates, values
  as 1e300 overflow); the direction of a clamped segment inside the panel changes by under limit / panel size."
  1.0e6)

(def ^{:private true :const true :tag 'double} pixel-resolution
  "Pixel coordinates are rounded to multiples of 1 / pixel-resolution: rounding noise of sampled curves (a flat run at
  `155.0` sampled as `154.99999999999997`) would otherwise cross pixel boundaries under Java2D's stroke normalization
  and draw dots at the joins of thick lines."
  1.0e6)

(def ^{:private true :const true :tag 'double} -pixel-limit
  "The negated [[pixel-limit]]."
  (m/- pixel-limit))

(defn- drawn-coordinate
  "Gives a pixel coordinate as drawn.

  Parameters:

  - `v` (double): a pixel coordinate.

  Returns `v` clamped to plus or minus [[pixel-limit]] and rounded to 1 / [[pixel-resolution]] pixel; `NaN` stays `NaN`."
  ^double [^double v]
  (m// (m/rint (m/* (m/constrain v -pixel-limit pixel-limit) pixel-resolution)) pixel-resolution))

(defn drawn-point
  "Gives a pixel point as drawn.

  Parameters:

  - `[x y]`: the point (numbers).

  Returns `[x y]`, each coordinate clamped to ±[[pixel-limit]] and rounded to 1e-6 pixel (see [[pixel-resolution]])."
  [[^double x ^double y]]
  [(drawn-coordinate x) (drawn-coordinate y)])

(defn area-baseline
  "Gives the pixel position of the area baseline along a finalized y scale.

  Parameters:

  - `y-scale` (map): the finalized y scale.
  - `w`, `h` (doubles): the panel size in pixels.

  Returns y = 0 clamped into the panel in the scale's direction; the scale's range start when it can't place 0."
  ^double [y-scale ^double w ^double h]
  (let [zero (scale/forward y-scale 0.0)]
    (if (m/nan? zero)
      (double (first (:range y-scale)))
      (m/constrain zero 0.0 (if (scale/horizontal? :y y-scale) w h)))))

;; drawing

(defn draw-runs!
  "Draws the drawn runs of one line: the area, the stroke, the bridges.

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `point-runs` (sequence of vectors of `[x y]`): the drawn runs in pixels (see [[curve-points]]).
  - `style` (map): the line style with `:fill` (default `:none`; its `:pattern` too, see [[cljplot2.graphics.draw/pattern-fill]]) and `:bridge`.
  - `area`: `{:baseline px :horizontal? h}` - filled to the baseline (see [[cljplot2.graphics.polyline/fill-runs-to-baseline!]]), `:closed` - each run's own polygon, `nil` - none.
  - `gaps`: `:bridge` strokes the [[bridges]] between the runs in the bridge style; else none.

  Returns `nil`."
  [canvas point-runs style area gaps]
  (let [fill (:fill style :none)
        pattern-fill (draw/pattern-fill style fill)]
    (cond
      (= :closed area) (polyline/fill-closed-runs! canvas point-runs fill pattern-fill)
      (map? area) (polyline/fill-runs-to-baseline! canvas point-runs (:baseline area) (:horizontal? area) fill pattern-fill))
    (polyline/stroke-runs! canvas point-runs style)
    (when (= :bridge gaps)
      (polyline/stroke-runs! canvas (bridges point-runs) (polyline/bridge-style style (:bridge style))))))
