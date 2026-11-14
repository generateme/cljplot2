(ns cljplot2.graphics.draw
  "Drawing grid item content - the counterpart of [[cljplot2.graphics.measure/natural-size]]."
  (:require [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def stroke-color
  "Schema of a filled mark's `:stroke-color`: a color, `:none` (no outline) or `:darker` (the darkened fill)."
  [:or [:= :darker] schema/color])

(defn outline-color
  "Outline color of a mark filled with `fill` for `:stroke-color` `stroke` (see [[stroke-color]]); `:none` stays `:none`."
  [stroke fill]
  (if (= :darker stroke) (c/darken fill) stroke))

(declare pixel-center)

(defn zero-line!
  "The y = 0 line across the panel `box` (`{:w :h}`; along the x scale's direction, flip-aware) in style
  `:zero-line-color` / `:zero-line-width` (mirror densities and histograms); nothing when 0 isn't in the y scale or
  the line draws nothing."
  [canvas {:keys [w h]} scales {:keys [zero-line-color zero-line-width]}]
  (let [p (if (:y scales) (scale/forward (:y scales) 0.0) ##NaN)
        width (double zero-line-width)]
    (when (and (not (m/nan? p)) (not (theme/none? zero-line-color)) (pos? width))
      (let [p (pixel-center p)]
        (c2d/set-color canvas zero-line-color)
        (c2d/set-stroke-custom canvas {:size width :cap :butt})
        (if (scale/horizontal? :y (:y scales))
          (c2d/line canvas p 0 p h)
          (c2d/line canvas 0 p w p))))))

(defn pixel-center
  "Center of the pixel containing coordinate `p`; thin lines drawn there cover whole pixels (no antialiasing smear)."
  ^double [^double p]
  (+ (m/floor p) 0.5))

(defmulti draw-content
  "Draw item `content` onto `canvas` (in graphics context) inside `box` (`{:x :y :w :h}`),
  dispatched like [[cljplot2.graphics.measure/natural-size]].

  `env` keys: `:theme` - the plot theme, `:scales` - finalized scales (see [[cljplot2.scale.position/finalize]]).

  A method may change any graphics state (clip, transform, font, color, ...) without restoring it;
  callers isolate each call (see [[cljplot2.graphics.canvas/call-with-isolated-graphics]]). To restrict drawing
  to `box`, intersect the clip (`Graphics2D.clip`) rather than replacing it, so an outer clip still applies.

  Content kinds without a method draw nothing."
  (fn [content _canvas _box _env] (measure/content-kind content)))

(defmethod draw-content :default [_ _ _ _] nil)

(defn inner-border!
  "Draw a border of `width` (rounded, at most half the box) and `color` inside box `{:x :y :w :h}` as four filled
  bands - exact pixels, nothing outside the box. `:none` color or width 0 draws nothing."
  [canvas {:keys [x y w h]} color width]
  (let [x (long x) y (long y) w (long w) h (long h)
        bw (m/min (m/round (double width)) (quot w 2) (quot h 2))]
    (when (and (not= :none color) (pos? bw))
      (c2d/set-color canvas color)
      (c2d/rect canvas x y w bw)
      (c2d/rect canvas x (- (+ y h) bw) w bw)
      (c2d/rect canvas x y bw h)
      (c2d/rect canvas (- (+ x w) bw) y bw h))))

(defn rect-of
  "Pixel rectangle `{:x :y :w :h}` (whole pixels) spanned by the data corners `[x0 y0]` and `[x1 y1]` placed with
  `projection` (see [[cljplot2.scale.position/projection]]): both corners rounded, then normalized (non-negative
  width and height), so it works for flipped scales too."
  [projection x0 y0 x1 y1]
  (let [^Vec2 a (projection x0 y0)
        ^Vec2 b (projection x1 y1)
        ax (m/round (.x a)) ay (m/round (.y a))
        bx (m/round (.x b)) by (m/round (.y b))]
    {:x (m/min ax bx) :y (m/min ay by) :w (m/abs (- bx ax)) :h (m/abs (- by ay))}))

(defn cvd-filter-fn
  "Color function simulating the color vision deficiency `deficiency` - a clojure2d `cvd-filter` keyword
  (`:protanopia`, `:deuteranopia`, `:tritanopia`, `:achromatopsia`) or `[keyword severity]` (`:protanomaly`,
  `:deuteranomaly`, `:tritanomaly`, `:achromatomaly`; severity in `[0 1]`). Throws `ex-info` (clojure2d's) for an
  unknown deficiency or severity."
  [deficiency]
  (if (vector? deficiency) (apply c/cvd-filter deficiency) (c/cvd-filter deficiency)))
