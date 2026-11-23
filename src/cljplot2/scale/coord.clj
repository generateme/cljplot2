(ns cljplot2.scale.coord
  "Coordinate systems: how range positions of the finalized position scales become pixels.

  Range space: a finalized position scale maps data to its `:range` - pixels in cartesian plots; in polar plots the
  horizontal scale (`:x`, `:y` with `:flip`) maps to angles (radians, compass style: 0 up, growing clockwise) and the
  vertical one to radii (pixels). A point is `(h, v)` - the horizontal and vertical scale's range positions; [[to-pixel]]
  turns it into a panel pixel (the identity in cartesian plots).

  Plot `:coord`: `nil` / `{:kind :cartesian}` (default) or `{:kind :polar ...}`:

  * `:start` - degrees, the angle of the horizontal scale's range start (0 - 12 o'clock, default)
  * `:direction` - `:clockwise` (default) or `:counter-clockwise`
  * `:span` - degrees covered (default 360; e.g. 180 for a half circle)
  * `:inner` - the inner radius as a fraction of the outer one, `[0 1)` (default 0 - a hole for donuts, circular
    barplots)
  * `:arcs` - data lines (line, segment) between points follow the circle (`true`) or are chords (default `false`);
    circles (rules, grid) and bar edges always follow it

  [[geometry]] is the polar layout of a panel (centre, radii, angles); finalized position scales carry it as `:coord`.

  Functions: schemas [[coord-schema]], [[polar-schema]] and [[polar?]]; layout [[theta-range]], [[sector-box]], [[geometry]]; points and paths [[to-pixel]], [[line-path]], [[rect-path]], [[circle-path]], [[full-turn-circles]] with [[full-circle?]], [[full-turn?]]; labels [[radial-rotation]], [[outward-anchor]]."
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def polar-schema
  "Schema of a polar plot `:coord` (see the namespace doc)."
  [:map {:closed true :cljplot2/type-doc "a polar coord {:kind :polar :start :direction :span :inner :arcs}"}
   [:kind {:description "polar coordinates: the horizontal scale maps to angles, the vertical one to radii"} [:= :polar]]
   (schema/optional :start {:description "angle of the horizontal scale's range start, degrees (0 - 12 o'clock)" :default 0.0}
                    schema/finite)
   (schema/optional :direction {:description "direction of growing angles" :default :clockwise}
                    [:enum :clockwise :counter-clockwise])
   (schema/optional :span {:description "degrees covered (e.g. 180 for a half circle)" :default 360.0}
                    [:and schema/finite [:> 0] [:<= 360]])
   (schema/optional :inner {:description "inner radius as a fraction of the outer one (a hole for donuts)" :default 0.0}
                    [:and schema/finite [:>= 0] [:< 1]])
   (schema/nilable :arcs {:description "data lines between points follow the circle instead of chords" :default false}
                   schema/bool)])

(def coord-schema
  "Schema of the plot `:coord`: cartesian (the default) or [[polar-schema]]."
  [:multi {:dispatch :kind :cljplot2/type-doc "{:kind :cartesian} or a polar coord {:kind :polar ...}"}
   [:cartesian [:map {:closed true} [:kind {:description "cartesian coordinates (the default)"} [:= :cartesian]]]]
   [:polar polar-schema]])

(defn polar?
  "Tests whether a plot coord is polar.

  Parameters:

  - `coord` (map or `nil`): a plot `:coord` value.

  Returns `true` for `{:kind :polar ...}`, else `false` (`nil` and cartesian coords)."
  [coord]
  (= :polar (:kind coord)))

(defn theta-range
  "Gives the range of the angular scale of a polar coord.

  Parameters:

  - `coord` (map): a polar `:coord` - `:start` (degrees, default 0), `:span` (degrees, default 360), `:direction` (`:clockwise` default, or `:counter-clockwise`).

  Returns `[start end]` in radians (compass style: 0 up, growing clockwise): from `:start` over `:span` in `:direction`; `end` is below `start` counter-clockwise.

  See also [[geometry]]."
  [{:keys [^double start ^double span direction] :or {start 0.0 span 360.0}}]
  (let [start-angle (m/radians start)
        span-angle (m/radians span)]
    [start-angle (if (= :counter-clockwise direction) (m/- start-angle span-angle) (m/+ start-angle span-angle))]))

(def ^{:const true :private true :tag 'double} quarter-turn
  "A quarter of a turn, radians."
  (m/* 0.5 m/PI))

(defn- unit-point
  "Gives the panel-oriented point (x right, y down) at angle `theta` (radians, 0 up, clockwise) and radius `r` around the origin, as `[x y]`."
  [^double theta ^double r]
  [(m/* r (m/sin theta)) (m/- (m/* r (m/cos theta)))])

(defn sector-box
  "Gives the bounding box of the sector of a polar coord on the unit circle around the origin.

  The box is in panel orientation (x right, y down) and holds the outer arc (its ends and the 12, 3, 6 and 9 o'clock points within it) and the centre - or, with an `:inner` radius, the inner arc's ends.

  Parameters:

  - `coord` (map): a polar `:coord` (`:start`, `:span`, `:direction`, `:inner` - default 0), see [[theta-range]].

  Returns `[x0 y0 x1 y1]`; a full circle gives `[-1 -1 1 1]`.

  See also [[geometry]]."
  [coord]
  (let [[^double t0 ^double t1] (theta-range coord)
        a (m/min t0 t1)
        b (m/max t0 t1)
        inner (double (get coord :inner 0.0))
        quarters (map #(m/* quarter-turn (double %)) (range (m/ceil (m// a quarter-turn)) (m/inc (m/floor (m// b quarter-turn)))))
        points (concat [(unit-point a 1.0) (unit-point b 1.0)]
                       (map #(unit-point % 1.0) quarters)
                       (if (m/pos? inner) [(unit-point a inner) (unit-point b inner)] [[0.0 0.0]]))
        xs (map first points)
        ys (map second points)]
    [(v/mn xs) (v/mn ys) (v/mx xs) (v/mx ys)]))

(defn geometry
  "Gives the polar layout of a panel for a polar coord.

  The centre places the sector's bounding box (see [[sector-box]]) in the panel's middle - a full circle's centre is the panel's middle, a half circle's on the panel's edge.

  Parameters:

  - `coord` (map): a polar `:coord` (`:start`, `:span`, `:direction`, `:inner`, `:arcs`).
  - `w`, `h` (double): panel width and height, pixels.
  - `radius` (double): the outer radius, pixels.

  Returns `{:kind :polar :cx :cy :radius :inner-radius :theta [start end] :arcs}` - centre pixel, radii (pixels), the angular range (radians, see [[theta-range]]) and the coord's `:arcs`.

  See also [[to-pixel]]."
  [coord ^double w ^double h ^double radius]
  (let [[^double x0 ^double y0 ^double x1 ^double y1] (sector-box coord)]
    {:kind :polar
     :cx (m/- (m/* 0.5 w) (m/* radius 0.5 (m/+ x0 x1)))
     :cy (m/- (m/* 0.5 h) (m/* radius 0.5 (m/+ y0 y1)))
     :radius radius
     :inner-radius (m/* radius (double (get coord :inner 0.0)))
     :theta (theta-range coord)
     :arcs (:arcs coord)}))

(defn to-pixel
  "Gives the panel pixel of a range point.

  Parameters:

  - `coord-geometry` (map or `nil`): a polar [[geometry]] (`:cx`, `:cy`), or `nil` - cartesian.
  - `h` (double): the horizontal scale's range position - in polar the angle (radians, 0 up, clockwise).
  - `v` (double): the vertical scale's range position - in polar the radius (pixels).

  Returns a `Vec2`: the point itself in cartesian, else the point at angle `h` and radius `v` around the centre."
  ^Vec2 [coord-geometry ^double h ^double v]
  (if (= :polar (:kind coord-geometry))
    (let [{:keys [^double cx ^double cy]} coord-geometry]
      (Vec2. (m/+ cx (m/* v (m/sin h))) (m/- cy (m/* v (m/cos h)))))
    (Vec2. h v)))

(defn radial-rotation
  "Gives the rotation of a label laid along a radius.

  Parameters:

  - `theta` (double): the angle, radians (0 up, clockwise).

  Returns clockwise degrees turning the label along `theta`, kept upright on the left half (in `(-90, 90]` or `[270, 360)`)."
  ^double [^double theta]
  (let [a (double (mod (m/- (m/degrees theta) 90.0) 360.0))]
    (if (m/< 90.0 a 270.0) (m/- a 180.0) a)))

(defn outward-anchor
  "Gives the anchor of a label placed at an angle and extending away from the centre.

  Parameters:

  - `theta` (double): the angle, radians (0 up, clockwise).

  Returns `[h v]`: `:left` on the right side, `:right` on the left, `:top` at the bottom, `:bottom` at the top; `:center` / `:middle` near the vertical / horizontal directions."
  [^double theta]
  (let [s (m/sin theta)
        c (m/cos theta)]
    [(cond (m/> s 0.3) :left (m/< s -0.3) :right :else :center)
     (cond (m/> c 0.3) :bottom (m/< c -0.3) :top :else :middle)]))

(def ^{:private true :const true :tag 'double} max-step-angle
  "Largest angle (radians) between points of a densified arc."
  (m/radians 2.0))

(def ^{:private true :const true :tag 'double} max-step-pixels
  "Largest arc length (pixels) between points of a densified arc."
  2.0)

(defn- steps
  "Gives the number of segments a smooth arc needs.

  Parameters:

  - `a`, `b` (double): the arc's end angles, radians.
  - `r` (double): the largest radius along it, pixels.

  Returns at least 1: segments no longer than [[max-step-angle]] nor [[max-step-pixels]]."
  ^long [^double a ^double b ^double r]
  (let [d (m/abs (m/- b a))
        step (m/min max-step-angle (if (m/pos? r) (m// max-step-pixels r) max-step-angle))]
    (m/max 1 (long (m/ceil (m// d step))))))

(defn- segment-points
  "Gives the pixel points of a range segment, its start excluded.

  Parameters:

  - `coord-geometry` (map or `nil`): see [[to-pixel]].
  - `[h0 v0]`, `[h1 v1]`: the segment's start and end range points.
  - `arcs?`: densify along the angle (polar only).

  Returns a vector of `Vec2`: the end alone, or - `arcs?` in polar - the segment densified linearly in `h` and `v` (see [[steps]])."
  [coord-geometry [^double h0 ^double v0] [^double h1 ^double v1] arcs?]
  (if (and arcs? (= :polar (:kind coord-geometry)))
    (let [n (steps h0 h1 (m/max (m/abs v0) (m/abs v1)))]
      (mapv (fn [^long i] (let [t (m// (double i) n)] (to-pixel coord-geometry (m/+ h0 (m/* t (m/- h1 h0))) (m/+ v0 (m/* t (m/- v1 v0))))))
            (range 1 (m/long-inc n))))
    [(to-pixel coord-geometry h1 v1)]))

(defn line-path
  "Gives the pixel points of range points joined in a coordinate system.

  Parameters:

  - `coord-geometry` (map or `nil`): see [[to-pixel]].
  - `pts` (sequence): range points `[h v]`.
  - `arcs?`: in polar, segments follow the circle instead of chords.

  Returns a vector of `[x y]`: chords between the transformed points, or segments densified along the angle; `[]` for no points.

  See also [[rect-path]], [[circle-path]]."
  [coord-geometry pts arcs?]
  (if (empty? pts)
    []
    (let [[[^double h ^double v]] pts
          ^Vec2 start (to-pixel coord-geometry h v)]
      (into [[(.x start) (.y start)]]
            (comp (mapcat (fn [[p q]] (segment-points coord-geometry p q arcs?)))
                  (map (fn [^Vec2 p] [(.x p) (.y p)])))
            (partition 2 1 pts)))))

(defn rect-path
  "Gives the closed polygon of a range rectangle in polar geometry - an annular sector.

  Parameters:

  - `polar-geometry` (map): a polar [[geometry]].
  - `h0`, `h1` (numbers): the angular range.
  - `v0`, `v1` (numbers): the radial range, either order.

  Returns pixel `[x y]` points: the outer arc from `h0` to `h1`, the inner one back (a point at the centre for a zero inner radius); arcs densified.

  See also [[full-turn-circles]]."
  [polar-geometry h0 v0 h1 v1]
  (let [ha (double h0)
        hb (double h1)
        lo (m/min (double v0) (double v1))
        hi (m/max (double v0) (double v1))
        outer (line-path polar-geometry [[ha hi] [hb hi]] true)
        inner (if (m/pos? lo) (line-path polar-geometry [[hb lo] [ha lo]] true) (line-path polar-geometry [[hb 0.0]] false))]
    (into outer inner)))

(def ^{:private true :const true :tag 'double} turn-tolerance
  "Largest difference of angles (radians) taken as equal by [[full-circle?]] and [[full-turn?]]."
  1.0e-9)

(defn full-circle?
  "Tests whether a polar geometry covers the whole circle.

  Parameters:

  - `geometry` (map): a polar [[geometry]].

  Returns `true` when its angular range spans 2π (within 1e-9)."
  [{[^double t0 ^double t1] :theta}]
  (m/< (m/abs (m/- (m/abs (m/- t1 t0)) m/TWO_PI)) turn-tolerance))

(defn full-turn?
  "Tests whether an angular range goes once around a full-circle polar geometry - a sector without radial edges.

  Parameters:

  - `polar-geometry` (map): a polar [[geometry]].
  - `h0`, `h1` (numbers): the angular range, radians.

  Returns `true` when `polar-geometry` is a [[full-circle?]] and `h0..h1` spans its whole range (within 1e-9)."
  [polar-geometry h0 h1]
  (let [[^double t0 ^double t1] (:theta polar-geometry)]
    (and (full-circle? polar-geometry)
         (m/< (m/abs (m/- (m/abs (m/- (double h1) (double h0))) (m/abs (m/- t1 t0)))) turn-tolerance))))

(defn full-turn-circles
  "Gives the open pixel paths outlining a [[full-turn?]] range rectangle in polar geometry.

  Parameters:

  - `polar-geometry` (map): a polar [[geometry]].
  - `h0`, `h1` (numbers): the angular range (a full turn).
  - `v0`, `v1` (numbers): the radial range, either order.

  Returns a vector of paths (see [[line-path]]): the outer circle and, for a positive inner radius, the inner one - the ring's outline without a seam along the start spoke."
  [polar-geometry h0 v0 h1 v1]
  (let [lo (m/min (double v0) (double v1))
        hi (m/max (double v0) (double v1))]
    (cond-> [(line-path polar-geometry [[h0 hi] [h1 hi]] true)]
      (m/pos? lo) (conj (line-path polar-geometry [[h0 lo] [h1 lo]] true)))))

(defn circle-path
  "Gives the pixel points of a whole circle in polar geometry.

  Parameters:

  - `polar-geometry` (map): a polar [[geometry]].
  - `v` (number): the radius as a range position, pixels.

  Returns the [[line-path]] over the geometry's angular range at radius `v`."
  [polar-geometry v]
  (let [[t0 t1] (:theta polar-geometry)]
    (line-path polar-geometry [[t0 v] [t1 v]] true)))
