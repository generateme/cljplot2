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

  [[geometry]] is the polar layout of a panel (centre, radii, angles); finalized position scales carry it as `:coord`."
  (:require [fastmath.core :as m]
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
  "Is plot coord `coord` (a plot `:coord` value, may be `nil`) polar?"
  [coord]
  (= :polar (:kind coord)))

(defn theta-range
  "Range `[start end]` (radians) of the angular scale of polar `coord`: from `:start` over `:span` in `:direction`."
  [coord]
  (let [start (m/radians (double (get coord :start 0.0)))
        span (m/radians (double (get coord :span 360.0)))]
    [start (if (= :counter-clockwise (:direction coord)) (- start span) (+ start span))]))

(defn sector-box
  "Bounding box `[x0 y0 x1 y1]` of the sector of polar `coord` on the unit circle around the origin, in panel
  orientation (x right, y down): the outer arc (its ends and the 12, 3, 6 and 9 o'clock points within it) and the
  centre - or, with an `:inner` radius, the inner arc's ends. A full circle gives `[-1 -1 1 1]`."
  [coord]
  (let [[t0 t1] (theta-range coord)
        a (m/min (double t0) (double t1))
        b (m/max (double t0) (double t1))
        quarter (* 0.5 m/PI)
        inner (double (get coord :inner 0.0))
        point (fn [^double theta ^double r] [(* r (m/sin theta)) (- (* r (m/cos theta)))])
        quarters (map #(* quarter (double %)) (range (long (m/ceil (/ a quarter))) (inc (long (m/floor (/ b quarter))))))
        points (concat [(point a 1.0) (point b 1.0)]
                       (map #(point % 1.0) quarters)
                       (if (pos? inner) [(point a inner) (point b inner)] [[0.0 0.0]]))
        xs (map first points) ys (map second points)]
    [(reduce min xs) (reduce min ys) (reduce max xs) (reduce max ys)]))

(defn geometry
  "Polar layout of a `w` x `h` panel for `coord` with `radius` - the outer radius (pixels): `{:kind :polar :cx :cy
  :radius :inner-radius :theta [start end] :arcs}`; the centre places the sector's bounding box (see [[sector-box]])
  in the panel's middle - a full circle's centre is the panel's middle, a half circle's on the panel's edge."
  [coord ^double w ^double h ^double radius]
  (let [[x0 y0 x1 y1] (sector-box coord)]
    {:kind :polar
     :cx (- (* 0.5 w) (* radius 0.5 (+ (double x0) (double x1))))
     :cy (- (* 0.5 h) (* radius 0.5 (+ (double y0) (double y1))))
     :radius radius
     :inner-radius (* radius (double (get coord :inner 0.0)))
     :theta (theta-range coord)
     :arcs (boolean (:arcs coord))}))

(defn to-pixel
  "Panel pixel of range point `(h, v)` in `geometry` (`nil` - cartesian, the point itself): polar - the angle `h`
  (radians, 0 up, clockwise) at radius `v`."
  ^Vec2 [geometry ^double h ^double v]
  (if (= :polar (:kind geometry))
    (let [{:keys [^double cx ^double cy]} geometry]
      (Vec2. (+ cx (* v (m/sin h))) (- cy (* v (m/cos h)))))
    (Vec2. h v)))

(defn radial-rotation
  "Clockwise degrees turning a label along angle `theta` (radians, 0 up, clockwise), upright on the left half."
  ^double [^double theta]
  (let [a (double (mod (- (m/degrees theta) 90.0) 360.0))]
    (if (< 90.0 a 270.0) (- a 180.0) a)))

(defn outward-anchor
  "Label anchor `[h v]` of text placed at angle `theta` (radians, 0 up, clockwise) that should extend away from the
  centre: left-anchored on the right side, right-anchored on the left, top-anchored at the bottom, bottom-anchored at
  the top; centred near the vertical / horizontal directions."
  [^double theta]
  (let [s (m/sin theta) c (m/cos theta)]
    [(cond (> s 0.3) :left (< s -0.3) :right :else :center)
     (cond (> c 0.3) :bottom (< c -0.3) :top :else :middle)]))

(def ^:private ^:const max-step-angle
  "Largest angle (radians) between points of a densified arc."
  (m/radians 2.0))

(def ^:private ^:const max-step-pixels
  "Largest arc length (pixels) between points of a densified arc."
  2.0)

(defn- steps
  "Points needed between angles `a` and `b` at radii up to `r` for a smooth arc (at least 1)."
  ^long [^double a ^double b ^double r]
  (let [d (m/abs (- b a))
        step (m/min max-step-angle (if (pos? r) (/ max-step-pixels r) max-step-angle))]
    (m/max 1 (long (m/ceil (/ d step))))))

(defn- segment-points
  "Pixel points of the range segment from `p` to `q` (`[h v]` each), without `p`: its end, or - `arcs?` in polar - the
  segment densified along the angle (linear in `h` and `v`)."
  [geometry [^double h0 ^double v0] [^double h1 ^double v1] arcs?]
  (if (and arcs? (= :polar (:kind geometry)))
    (let [n (steps h0 h1 (m/max (m/abs v0) (m/abs v1)))]
      (mapv (fn [^long i] (let [t (/ (double i) n)] (to-pixel geometry (+ h0 (* t (- h1 h0))) (+ v0 (* t (- v1 v0))))))
            (range 1 (inc n))))
    [(to-pixel geometry h1 v1)]))

(defn line-path
  "Pixel points (`[x y]`) of the range points `pts` (`[h v]` each) joined in `geometry`: chords between the
  transformed points, or (`arcs?`, polar) segments densified along the angle; `[]` for no points."
  [geometry pts arcs?]
  (if (empty? pts)
    []
    (let [[h v] (first pts)
          ^Vec2 start (to-pixel geometry (double h) (double v))]
      (into [[(.x start) (.y start)]]
            (comp (mapcat (fn [[p q]] (segment-points geometry p q arcs?)))
                  (map (fn [^Vec2 p] [(.x p) (.y p)])))
            (partition 2 1 pts)))))

(defn rect-path
  "Closed polygon (pixel `[x y]` points) of the range rectangle `h0..h1` x `v0..v1` in polar `geometry`: an annular
  sector - the outer arc from `h0` to `h1`, the inner one back (a point at the centre for a zero inner radius); arcs
  densified."
  [geometry h0 v0 h1 v1]
  (let [h0 (double h0) h1 (double h1)
        lo (m/min (double v0) (double v1)) hi (m/max (double v0) (double v1))
        outer (line-path geometry [[h0 hi] [h1 hi]] true)
        inner (if (pos? lo) (line-path geometry [[h1 lo] [h0 lo]] true) (line-path geometry [[h1 0.0]] false))]
    (into outer inner)))

(defn circle-path
  "Pixel points of the whole circle (over the angular range) of radius `v` in polar `geometry`."
  [geometry v]
  (let [[t0 t1] (:theta geometry)]
    (line-path geometry [[t0 v] [t1 v]] true)))
