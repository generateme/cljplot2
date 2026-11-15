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
  "The line at 0 of value scale `k` (default `:y`) across the panel `box` (`{:w :h}`; flip-aware) in style
  `:zero-line-color` / `:zero-line-width` (mirror densities, histograms and bars); nothing when 0 isn't in the scale
  or the line draws nothing."
  ([canvas box scales style] (zero-line! canvas box scales style :y))
  ([canvas {:keys [w h]} scales {:keys [zero-line-color zero-line-width]} k]
   (let [p (if (k scales) (scale/forward (k scales) 0.0) ##NaN)
         width (double zero-line-width)]
     (when (and (not (m/nan? p)) (not (theme/none? zero-line-color)) (pos? width))
       (let [p (pixel-center p)]
         (c2d/set-color canvas zero-line-color)
         (c2d/set-stroke-custom canvas {:size width :cap :butt})
         (if (scale/horizontal? k (k scales))
           (c2d/line canvas p 0 p h)
           (c2d/line canvas 0 p w p)))))))

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

(def ^:private ^:const min-bar-pixels
  "Bars thinner than this (pixels, along either side) aren't drawn: sub-pixel slivers only smear the baseline."
  0.5)

(defn visible-bar?
  "Is a bar `w` x `h` pixels drawn (both sides at least half a pixel)?"
  [^double w ^double h]
  (and (>= w min-bar-pixels) (>= h min-bar-pixels)))

(defn bar!
  "Fill the rectangle with pixel corners `a` and `b` (`Vec2`, any order) with `fill`, outline it with `stroke` (a
  color or `:none`) when `stroke-width` is positive; nothing for a bar that isn't [[visible-bar?]]."
  [canvas ^Vec2 a ^Vec2 b fill stroke stroke-width]
  (let [stroke-width (double stroke-width)
        x (m/min (.x a) (.x b)) y (m/min (.y a) (.y b))
        w (m/abs (- (.x b) (.x a))) h (m/abs (- (.y b) (.y a)))]
    (when (visible-bar? w h)
      (c2d/set-color canvas fill)
      (c2d/rect canvas x y w h)
      (when (and (pos? stroke-width) (not (theme/none? stroke)))
        (c2d/set-color canvas stroke)
        (c2d/rect canvas x y w h true)))))

(defn bar-key!
  "Legend key glyph of a bar chart (histogram, bar): a filled square inset in `box`, outlined by `:stroke-color` (see
  [[outline-color]]) when `:stroke-width` is positive; `translucent?` - the fill gets `:alpha` (grouped overlay bars).
  `st` - `{:color :stroke-color :stroke-width :alpha}`."
  [canvas {:keys [x y w h]} st translucent?]
  (let [side (* 0.7 (m/min (double w) (double h)))
        cx (+ (double x) (* 0.5 (double w)))
        cy (+ (double y) (* 0.5 (double h)))]
    (c2d/set-color canvas (if translucent? (c/set-alpha (:color st) (* 255.0 (double (:alpha st)))) (:color st)))
    (c2d/crect canvas cx cy side side)
    (let [stroke (outline-color (:stroke-color st) (:color st))]
      (when (and (pos? (double (:stroke-width st))) (not (theme/none? stroke)))
        (c2d/set-stroke-custom canvas {:size (double (:stroke-width st))})
        (c2d/set-color canvas stroke)
        (c2d/crect canvas cx cy side side true)))))

(defn cvd-filter-fn
  "Color function simulating the color vision deficiency `deficiency` - a clojure2d `cvd-filter` keyword
  (`:protanopia`, `:deuteranopia`, `:tritanopia`, `:achromatopsia`) or `[keyword severity]` (`:protanomaly`,
  `:deuteranomaly`, `:tritanomaly`, `:achromatomaly`; severity in `[0 1]`). Throws `ex-info` (clojure2d's) for an
  unknown deficiency or severity."
  [deficiency]
  (if (vector? deficiency) (apply c/cvd-filter deficiency) (c/cvd-filter deficiency)))
