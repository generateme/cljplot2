(ns cljplot2.graphics.pattern
  "Fill patterns as seamless texture tiles (see [[cljplot2.schema/pattern-styles]]): a tile holds one period of the
  pattern - marks in the ink colour on a transparent background - and a texture paint repeats it over a fill (see
  [[cljplot2.graphics.draw/fill-with-pattern!]]).

  Periods are whole pixels: `spacing` for lines at 0° / 90° and dots, `spacing · √2` for lines at 45° / 135°
  (rounded, at least 2), so tiles repeat without seams.

  Functions: [[spec-of]], [[period]], [[tile]]."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.schema :as schema])
  (:import [java.awt.image BufferedImage]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn spec-of
  "Gives the pattern spec of a valid pattern.

  Parameters:

  - `pattern`: a name of [[cljplot2.schema/pattern-styles]] or a spec map.

  Returns the spec map; `nil` for `:solid` or `nil` - no pattern."
  [pattern]
  (let [spec (get schema/pattern-styles pattern pattern)]
    (when (map? spec) spec)))

(defn period
  "Gives the whole-pixel period of a pattern's tile (see the namespace doc).

  Parameters:

  - `spec` (map): the pattern spec - `:kind`, `:angle`, `:spacing`.

  Returns the period in pixels, at least 2."
  ^long [{:keys [kind angle ^double spacing]}]
  (m/max 2 (m/round (if (and (not= :dots kind) (contains? #{45 135} (long angle))) (m/* spacing m/SQRT2) spacing))))

(defn- line-angles
  "Gives the angles of the line families of a line or cross spec.

  Parameters:

  - `spec` (map): `:kind`, `:angle`.

  Returns a vector of angles in degrees: one, or two at right angles for `:cross`."
  [{:keys [kind angle]}]
  (if (= :cross kind) [(long angle) (m/long-add 90 (long angle))] [(long angle)]))

(defn- draw-lines!
  "Draws one period of parallel lines across a tile, with copies one period away on both sides so marks crossing the edges wrap.

  Parameters:

  - `canvas`: the tile's canvas in graphics context (stroke and colour set).
  - `angle` (long): 0, 45, 90, 135 or 180 (0 again) degrees.
  - `p` (long): the tile side in pixels.

  Returns `nil`."
  [canvas ^long angle ^long p]
  (let [side (double p)
        mid (m/+ (m/floor (m/* 0.5 side)) 0.5)
        shifts [(m/- side) 0.0 side]
        segments (case (m/long-rem angle 180)
                   0 [[0.0 mid side mid]]
                   90 [[mid 0.0 mid side]]
                   45 (map (fn [^double d] [d side (m/+ d side) 0.0]) shifts)
                   135 (map (fn [^double d] [d 0.0 (m/+ d side) side]) shifts))]
    (doseq [[x0 y0 x1 y1] segments]
      (c2d/line canvas x0 y0 x1 y1))))

(def ^:private tile-hints
  "Rendering hints of a tile canvas (see `clojure2d.core/set-rendering-hint`): the Java2D defaults - interpolation nearest-neighbor, Java's default - with antialiasing on and pure stroke control."
  {:antialiasing :on
   :interpolation :nearest-neighbor
   :alpha-interpolation :default
   :color-rendering :default
   :rendering :default
   :fractional-metrics :default
   :text-antialiasing :default
   :stroke-control :pure})

(defn tile
  "Gives the tile image of a pattern: one period square, transparent background, antialiased marks.

  The tile canvas uses [[tile-hints]]: the Java2D default rendering hints (interpolation nearest-neighbor, Java's default) except antialiasing on and pure stroke control, so marks land on the same pixels at every scale.

  Parameters:

  - `spec` (map): the pattern spec (see [[spec-of]]) - `:kind` (`:dots` - a centred dot of `:size` pixels, else lines of `:width` pixels, square caps, mitred).
  - `ink`: the marks' colour (any clojure2d representation).

  Returns a `BufferedImage` [[period]] pixels square."
  ^BufferedImage [{:keys [kind width size] :as spec} ink]
  (let [p (period spec)
        canvas (c2d/canvas p p :low)]
    (c2d/with-canvas [c canvas]
      (-> c
          (c2d/set-rendering-hint tile-hints)
          (c2d/set-color ink))
      (if (= :dots kind)
        (let [d (double size)]
          (c2d/ellipse c (m/* 0.5 (double p)) (m/* 0.5 (double p)) d d))
        (do (c2d/set-stroke c (double width) :square :miter 10.0)
            (doseq [a (line-angles spec)]
              (draw-lines! c (long a) p)))))
    (c2d/get-image canvas)))
