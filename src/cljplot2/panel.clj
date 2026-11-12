(ns cljplot2.panel
  "Panel content `{:kind :panel}`: the box charts are drawn in. Natural size 0x0 (it takes what its tracks give).
  Drawn from the theme's `:panel` section (validated by [[cljplot2.theme/plot-theme]]): fill, then a border
  inside the box; `:none` colors draw nothing, `:border-width` is rounded to whole pixels.
  Grid lines (theme `[:panel :grid]`) are drawn at the tick positions of the finalized scales in `env` `:scales`,
  between the fill and the border."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.draw :as draw]
            [cljplot2.measure :as measure]
            [cljplot2.scale :as scale]
            [cljplot2.theme :as theme]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defmethod measure/natural-size :panel [_ _] (v/vec2 0.0 0.0))

(defmethod draw/draw-content :panel
  [_ canvas {:keys [x y w h]} env]
  (let [{:keys [fill border-color border-width]} (:panel (:theme env))
        x (long x) y (long y) w (long w) h (long h)]
    (when-not (theme/none? fill)
      (c2d/set-color canvas fill)
      (c2d/rect canvas x y w h))
    (doseq [k [:x :y]
            :let [finalized (get-in env [:scales k])
                  {:keys [color width dash] :as line} (get-in env [:theme :panel :grid k])]
            :when (and finalized (not (theme/none? line)) (not (theme/none? color)) (pos? (double width)))]
      (c2d/set-color canvas color)
      (c2d/set-stroke-custom canvas (cond-> {:size (double width) :cap :butt} (not (theme/none? dash)) (assoc :dash (vec dash))))
      (let [forward (scale/forward-fn finalized)]
        (doseq [tick (:ticks finalized)
                :let [p (draw/pixel-center (forward tick))]]
          (if (= k :x)
            (c2d/line canvas p y p (+ y h))
            (c2d/line canvas x p (+ x w) p)))))
    (when-not (theme/none? border-color)
      ;; four filled bands inside the box: exact pixels, nothing outside the box
      (let [bw (m/min (m/round (double border-width)) (quot w 2) (quot h 2))]
        (when (pos? bw)
          (c2d/set-color canvas border-color)
          (c2d/rect canvas x y w bw)
          (c2d/rect canvas x (- (+ y h) bw) w bw)
          (c2d/rect canvas x y bw h)
          (c2d/rect canvas (- (+ x w) bw) y bw h))))))
