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

(def coord-schema
  "Schema of the plot `:coord`."
  [:multi {:dispatch :kind}
   [:cartesian [:map {:closed true} [:kind [:= :cartesian]]]]
   [:polar [:map {:closed true}
            [:kind [:= :polar]]
            (schema/optional :start schema/finite)
            (schema/optional :direction [:enum :clockwise :counter-clockwise])
            (schema/optional :span [:and schema/finite [:fn {:error/message "should be in (0 360]"} #(< 0.0 (double %) 360.000001)]])
            (schema/optional :inner [:and schema/finite [:fn {:error/message "should be in [0 1)"} #(<= 0.0 (double %) 0.999999)]])
            (schema/nilable :arcs schema/bool)]]])

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

(defn geometry
  "Polar layout of a `w` x `h` panel for `coord` with `radius` - the outer radius (pixels): `{:kind :polar :cx :cy
  :radius :inner-radius :theta [start end] :arcs}`; centre in the panel's middle."
  [coord ^double w ^double h ^double radius]
  {:kind :polar
   :cx (* 0.5 w) :cy (* 0.5 h)
   :radius radius
   :inner-radius (* radius (double (get coord :inner 0.0)))
   :theta (theta-range coord)
   :arcs (boolean (:arcs coord))})

(defn to-pixel
  "Panel pixel of range point `(h, v)` in `geometry` (`nil` - cartesian, the point itself): polar - the angle `h`
  (radians, 0 up, clockwise) at radius `v`."
  ^Vec2 [geometry ^double h ^double v]
  (if (= :polar (:kind geometry))
    (let [{:keys [^double cx ^double cy]} geometry]
      (Vec2. (+ cx (* v (m/sin h))) (- cy (* v (m/cos h)))))
    (Vec2. h v)))

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
