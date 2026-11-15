(ns cljplot2.graphics.pattern
  "Fill patterns as seamless texture tiles (see [[cljplot2.schema/pattern-styles]]): a tile holds one period of the
  pattern - marks in the ink colour on a transparent background - and a `TexturePaint` repeats it over a fill (see
  [[cljplot2.graphics.draw/fill-with-pattern!]]).

  Periods are whole pixels: `spacing` for lines at 0° / 90° and dots, `spacing · √2` for lines at 45° / 135°
  (rounded, at least 2), so tiles repeat without seams."
  (:require [clojure2d.color :as c]
            [fastmath.core :as m]
            [cljplot2.schema :as schema])
  (:import [java.awt BasicStroke Graphics2D RenderingHints]
           [java.awt.geom Ellipse2D$Double Line2D$Double]
           [java.awt.image BufferedImage]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn spec-of
  "Pattern spec of a valid `pattern` (a name of [[cljplot2.schema/pattern-styles]] or a spec map); `nil` for `:solid`
  or `nil` - no pattern."
  [pattern]
  (let [spec (get schema/pattern-styles pattern pattern)]
    (when (map? spec) spec)))

(defn period
  "Whole-pixel period of the tile of pattern `spec` (see the namespace doc)."
  ^long [{:keys [kind angle spacing]}]
  (let [s (double spacing)]
    (m/max 2 (m/round (if (and (not= :dots kind) (contains? #{45 135} angle)) (* s m/SQRT2) s)))))

(defn- line-angles
  "Angles of the line families of a line or cross `spec`."
  [{:keys [kind angle]}]
  (if (= :cross kind) [angle (+ 90 (long angle))] [angle]))

(defn- draw-lines!
  "One period of parallel lines at `angle` (0, 45, 90, 135 or 180 - 0 again) across the `p`-pixel tile, copies one
  period away on both sides so marks crossing the edges wrap."
  [^Graphics2D g ^long angle ^long p]
  (let [p (double p)
        mid (+ (m/floor (* 0.5 p)) 0.5)
        shifts [(- p) 0.0 p]
        segments (case (rem angle 180)
                   0 [[0.0 mid p mid]]
                   90 [[mid 0.0 mid p]]
                   45 (map (fn [^double d] [d p (+ d p) 0.0]) shifts)
                   135 (map (fn [^double d] [d 0.0 (+ d p) p]) shifts))]
    (doseq [[x0 y0 x1 y1] segments]
      (.draw g (Line2D$Double. x0 y0 x1 y1)))))

(defn tile
  "Tile image of pattern `spec` (see [[spec-of]]) in `ink` (a clojure2d colour): `period` pixels square, transparent
  background, antialiased marks."
  ^BufferedImage [{:keys [kind width size] :as spec} ink]
  (let [p (period spec)
        img (BufferedImage. p p BufferedImage/TYPE_INT_ARGB)
        g (.createGraphics img)]
    (try
      (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
      (.setRenderingHint g RenderingHints/KEY_STROKE_CONTROL RenderingHints/VALUE_STROKE_PURE)
      (.setColor g (c/awt-color ink))
      (if (= :dots kind)
        (let [d (double size)
              o (- (* 0.5 (double p)) (* 0.5 d))]
          (.fill g (Ellipse2D$Double. o o d d)))
        (do (.setStroke g (BasicStroke. (float width) BasicStroke/CAP_SQUARE BasicStroke/JOIN_MITER))
            (doseq [a (line-angles spec)]
              (draw-lines! g (long a) p))))
      img
      (finally (.dispose g)))))
