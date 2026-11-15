(ns cljplot2.graphics.curve
  "Curves in pixel coordinates: the runs of a line (consecutive finite points, see [[cljplot2.graphics.polyline]])
  turned into the points that are drawn, plus what lines and areas share around them (bridges over gaps, the area
  baseline, the pixel clamp).

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
  nothing."
  (:require [clojure2d.core.shape :as sh]
            [fastmath.core :as m]
            [fastmath.interpolation :as interpolation]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema])
  (:import [java.awt.geom Path2D PathIterator]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const default-sample-step
  "Pixels between evaluated positions."
  1.0)


(def sample-step
  "Schema of `:sample-step`: positive pixels or `{:count n}`, n >= 2."
  (schema/named-type ::sample-step
                     #(or (and (schema/finite? %) (pos? (double %)))
                          (and (map? %) (= [:count] (keys %)) (integer? (:count %)) (>= (long (:count %)) 2)))
                     "should be a positive number of pixels or {:count n}, n >= 2"))

(def options
  "Map entries of the curve options of a line chart."
  [(schema/optional :interpolator schema/interpolator)
   (schema/optional :interpolator-params [:vector any?])
   (schema/optional :curve [:enum :parametric :bezier])
   (schema/optional :sample-step sample-step)])

;; sampling

(defn sample-grid
  "Evenly spaced positions from `p0` to `p1`, both included: `sample-step` pixels - the span split into the fewest
  equal intervals no longer than the step; `{:count n}` - n positions. `[p0]` when the ends are equal."
  [^double p0 ^double p1 sample-step]
  (let [span (- p1 p0)
        n (long (if (map? sample-step)
                  (dec (long (:count sample-step)))
                  (m/max 1.0 (m/ceil (/ (m/abs span) (double sample-step))))))]
    (if (zero? span)
      [p0]
      (mapv (fn [^long i] (if (== i n) p1 (+ p0 (* span (/ (double i) (double n)))))) (range (inc n))))))

;; fitting

(defn fit-fn
  "`(fn [xs ys] f)` of `interpolator` (a method keyword or a function) with the extra arguments `params`."
  [interpolator params]
  (if (keyword? interpolator)
    (fn [xs ys] (apply interpolation/interpolation interpolator xs ys params))
    (fn [xs ys] (apply interpolator xs ys params))))

(defn- fit
  "`(fit xs ys)` - the fitted function, or `nil` when there are fewer points than the method needs. Other fastmath
  input errors throw `ex-info {:key :interpolator}`."
  [fit xs ys]
  (try (fit xs ys)
       (catch clojure.lang.ExceptionInfo e
         (let [{:keys [type property]} (ex-data e)]
           (cond
             (= :too-few-points property) nil
             (= :invalid-input type) (schema/fail! (str "Interpolation failed: " (ex-message e) ".")
                                                   {:key :interpolator :value (:method (ex-data e))})
             :else (throw e))))))

(defn- strictly-monotone
  "`:increasing` / `:decreasing` when `vs` strictly increase / decrease, else `nil`."
  [vs]
  (let [pairs (partition 2 1 vs)]
    (cond
      (every? (fn [[a b]] (< (double a) (double b))) pairs) :increasing
      (every? (fn [[a b]] (> (double a) (double b))) pairs) :decreasing)))

(defn- axis-index ^long [along] (if (= :y along) 1 0))

(defn functional-points
  "Points of `run` interpolated by `fit` (see [[fit-fn]]) along pixel axis `along` (`:x` / `:y`), evaluated every
  `sample-step`; the run in its direction. The run as is with fewer than 2 points or too few for the method; not
  strictly monotone along the axis - `ex-info {:key :interpolator}`."
  [fit-of run along sample-step]
  (if (< (count run) 2)
    run
    (let [a (axis-index along)
          b (- 1 a)
          direction (strictly-monotone (map #(nth % a) run))
          _ (when-not direction
              (schema/fail! (str "Interpolation needs strictly monotone positions along " (name along)
                                 " in every run (no repeated x; with :sort false, x in order).")
                            {:key :interpolator :value (mapv #(nth % a) run)}))
          ordered (if (= direction :decreasing) (vec (rseq run)) run)
          f (fit fit-of (mapv #(double (nth % a)) ordered) (mapv #(double (nth % b)) ordered))]
      (if (nil? f)
        run
        (let [ts (sample-grid (double (nth (first run) a)) (double (nth (peek run) a)) sample-step)]
          (mapv (fn [t] (if (zero? a) [t (double (f t))] [(double (f t)) t])) ts))))))

(defn- distinct-consecutive [run] (mapv first (partition-by identity run)))

(defn- chord-lengths
  "Cumulative distances along `run` from its first point."
  [run]
  (vec (reductions + 0.0 (map (fn [[x0 y0] [x1 y1]] (m/hypot (- (double x1) (double x0)) (- (double y1) (double y0))))
                              run (rest run)))))

(defn parametric-points
  "Points of `run` interpolated by `fit` as x(t), y(t) over the cumulative chord length t, evaluated every
  `sample-step` of t; consecutive repeated points removed first. The run as is with fewer than 2 distinct points or
  too few for the method."
  [fit-of run sample-step]
  (let [run (distinct-consecutive run)]
    (if (< (count run) 2)
      run
      (let [ts (chord-lengths run)
            fx (fit fit-of ts (mapv #(double (first %)) run))
            fy (fit fit-of ts (mapv #(double (second %)) run))]
        (if (or (nil? fx) (nil? fy))
          run
          (mapv (fn [t] [(double (fx t)) (double (fy t))]) (sample-grid 0.0 (double (peek ts)) sample-step)))))))

(def ^:private ^:const bezier-flatness
  "Largest distance (pixels) of the flattened bezier polyline from the curve."
  0.1)

(defn bezier-points
  "Points of clojure2d `path-bezier` through `run`, flattened to a polyline (within [[bezier-flatness]] px)."
  [run]
  (if (< (count run) 2)
    run
    (let [^Path2D path (sh/path-bezier run false)
          it (.getPathIterator path nil bezier-flatness)
          buf (double-array 6)]
      (loop [acc []]
        (if (.isDone it)
          acc
          (let [t (.currentSegment it buf)]
            (.next it)
            (recur (if (== t PathIterator/SEG_CLOSE) acc (conj acc [(aget buf 0) (aget buf 1)])))))))))

(defn curve-points
  "The drawn points of `run` under the curve options `opts` (see the namespace doc); the run as is without
  `:interpolator` and `:curve`."
  [run {:keys [interpolator interpolator-params curve sample-step along] :or {along :x}}]
  (let [step (or sample-step default-sample-step)]
    (cond
      (and (= :bezier curve) interpolator)
      (schema/fail! ":curve :bezier draws its own path; it can't be combined with :interpolator."
                    {:key :curve :value curve})
      (= :bezier curve) (bezier-points run)
      (= :parametric curve) (parametric-points (fit-fn (or interpolator :cubic) interpolator-params) run step)
      interpolator (functional-points (fit-fn interpolator interpolator-params) run along step)
      :else run)))

(defn- segment-index
  "Index `i` of the segment `[ps(i) ps(i+1)]` of the increasing positions `ps` (at least 2) holding `t` (the first or
  the last segment beyond the ends)."
  ^long [ps ^double t]
  (loop [lo 0 hi (- (count ps) 2)]
    (if (>= lo hi)
      lo
      (let [mid (quot (+ lo hi 1) 2)]
        (if (< t (double (ps mid))) (recur lo (dec mid)) (recur mid hi))))))

(defn- keep-sign
  "Sample `v` at `t` kept to the sign of the two knot values around it (`ps` increasing, `vs`) when they share it."
  ^double [ps vs ^double t ^double v]
  (let [i (segment-index ps t)
        a (double (vs i)) b (double (vs (inc i)))]
    (cond
      (and (>= a 0.0) (>= b 0.0)) (m/max 0.0 v)
      (and (<= a 0.0) (<= b 0.0)) (m/min 0.0 v)
      :else v)))

(defn sample-columns
  "Columns of values sampled on one grid: `positions` - strictly monotone pixel positions shared by the columns,
  `columns` - one vector of values per column (one per position), fitted with `opts` `:interpolator` /
  `:interpolator-params` and evaluated on the [[sample-grid]] (`:sample-step`) from the first position to the last.
  `keep-sign?` - a sample between two knots of one sign keeps it (heights of stacked bands never cross 0 where the
  data doesn't). A column too short for the method is fitted linearly (all columns share the grid); fewer than 2
  positions - as given. Returns `[grid sampled-columns]`. Not strictly monotone positions throw `ex-info {:key
  :interpolator}`."
  [positions columns {:keys [interpolator interpolator-params sample-step]} keep-sign?]
  (if (< (count positions) 2)
    [positions columns]
    (let [direction (strictly-monotone positions)
          _ (when-not direction
              (schema/fail! "Interpolation needs strictly monotone x positions." {:key :interpolator :value positions}))
          rev? (= direction :decreasing)
          ps (mapv double (if rev? (rseq positions) positions))
          grid (sample-grid (double (first positions)) (double (peek positions)) (or sample-step default-sample-step))
          fit-of (fit-fn interpolator interpolator-params)
          linear (fit-fn :linear nil)]
      [grid (mapv (fn [column]
                    (let [vs (mapv double (if rev? (rseq column) column))
                          f (or (fit fit-of ps vs) (fit linear ps vs))]
                      (mapv (fn [t] (let [v (double (f t))] (if keep-sign? (keep-sign ps vs t v) v))) grid)))
                  columns)])))

(defn curved?
  "Do the curve options `opts` change the drawn points (an `:interpolator` or a `:curve`)?"
  [opts]
  (boolean (or (:interpolator opts) (:curve opts))))

;; gaps

(defn bridges
  "Straight segments `[[x0 y0] [x1 y1]]` over the gaps between consecutive runs (the last point of one, the first of
  the next); empty runs skipped."
  [runs]
  (let [runs (filterv seq runs)]
    (mapv (fn [a b] [(peek a) (first b)]) runs (rest runs))))

;; pixels

(def ^:const pixel-limit
  "Largest pixel coordinate magnitude drawn: farther points are clamped to it (Java2D draws float coordinates, values
  as 1e300 overflow); the direction of a clamped segment inside the panel changes by under limit / panel size."
  1.0e6)

(defn clamp-point
  "`[x y]` with each coordinate clamped to ±[[pixel-limit]]."
  [[x y]]
  [(m/constrain (double x) (- pixel-limit) pixel-limit) (m/constrain (double y) (- pixel-limit) pixel-limit)])

(defn area-baseline
  "Pixel position of the area baseline along the finalized `y-scale`: y = 0 clamped into the panel (`w` x `h`) in the
  scale's direction, the scale's range start when it can't place 0."
  ^double [y-scale ^double w ^double h]
  (let [zero (scale/forward y-scale 0.0)]
    (if (m/nan? zero)
      (double (first (:range y-scale)))
      (m/constrain zero 0.0 (if (scale/horizontal? :y y-scale) w h)))))

;; drawing

(defn draw-runs!
  "Draw the drawn runs `point-runs` (pixels, see [[curve-points]]) of one line in line `style` (plus `:fill`,
  `:bridge`): the area first - `area` `{:baseline px :horizontal? h}` (to the baseline, see
  [[cljplot2.graphics.polyline/fill-runs-to-baseline!]]), `:closed` (each run's own polygon) or `nil` (none) - then
  the stroke, then with `gaps` `:bridge` the bridges between the runs (see [[bridges]]) in the bridge style."
  [canvas point-runs style area gaps]
  (let [fill (:fill style :none)]
    (cond
      (= :closed area) (polyline/fill-closed-runs! canvas point-runs fill)
      (map? area) (polyline/fill-runs-to-baseline! canvas point-runs (:baseline area) (:horizontal? area) fill))
    (polyline/stroke-runs! canvas point-runs style)
    (when (= :bridge gaps)
      (polyline/stroke-runs! canvas (bridges point-runs) (polyline/bridge-style style (:bridge style))))))
