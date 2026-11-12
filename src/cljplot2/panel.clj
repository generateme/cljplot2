(ns cljplot2.panel
  "Panel content `{:kind :panel}`: the box charts are drawn in. Natural size 0x0 (it takes what its tracks give).
  Drawn from the theme's `:panel` section: fill, then a border inside the box."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.draw :as draw]
            [cljplot2.measure :as measure]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defmethod measure/natural-size :panel [_ _] (v/vec2 0.0 0.0))

(defmethod draw/draw-content :panel
  [_ canvas {:keys [x y w h]} {:keys [theme]}]
  (let [{:keys [fill border-color border-width]} (:panel theme)
        x (long x) y (long y) w (long w) h (long h)]
    (when-not (and (number? border-width) (>= (double border-width) 0.0))
      (throw (ex-info "Panel :border-width must be a non-negative number." {:key :border-width :value border-width})))
    (when fill
      (c2d/set-color canvas fill)
      (c2d/rect canvas x y w h))
    (when border-color
      ;; four filled bands inside the box: exact pixels, nothing outside the box
      (let [bw (m/min (long border-width) (quot w 2) (quot h 2))]
        (when (pos? bw)
          (c2d/set-color canvas border-color)
          (c2d/rect canvas x y w bw)
          (c2d/rect canvas x (- (+ y h) bw) w bw)
          (c2d/rect canvas x y bw h)
          (c2d/rect canvas (- (+ x w) bw) y bw h))))))
