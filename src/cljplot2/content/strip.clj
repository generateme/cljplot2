(ns cljplot2.content.strip
  "Strip content `{:kind :strip :text t}` (optional `:rotation`): a facet label - a `:strip` role label over theme
  `[:strip {:fill :border-color :border-width}]` (`:none` colors draw nothing). Natural size of the label.
  With `:interval {:lo :hi :min :max}` (a shingle level) a bar in theme `[:strip :shingle-fill]` shades the part of
  the strip that `[lo hi]` covers within `[min max]` - along the text direction (top-down for `:rotation 90`)."
  (:require [clojure2d.core :as c2d]
            [cljplot2.graphics.canvas :as canvas]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.theme :as theme]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- label [{:keys [text rotation]}]
  (cond-> {:kind :label :role :strip :text text} rotation (assoc :rotation rotation)))

(defmethod measure/validate-content :strip
  [content]
  (measure/validate-content (label content)))

(defmethod measure/natural-size :strip
  [content env]
  (measure/natural-size (label content) env))

(defmethod draw/draw-content :strip
  [content c {:keys [x y w h] :as box} env]
  (let [{:keys [fill border-color border-width]} (get-in env [:theme :strip])
        x (long x) y (long y) w (long w) h (long h)]
    (when-not (theme/none? fill)
      (c2d/set-color c fill)
      (c2d/rect c x y w h))
    (when-let [{:keys [lo hi min max]} (:interval content)]
      (let [shade (get-in env [:theme :strip :shingle-fill])
            span (- (double max) (double min))
            [a b] (if (pos? span)
                    [(/ (- (double lo) (double min)) span) (/ (- (double hi) (double min)) span)]
                    [0.0 1.0])]
        (when-not (theme/none? shade)
          (c2d/set-color c shade)
          (if (= 90 (:rotation content))
            (let [y0 (+ y (Math/round (* (double a) h))) y1 (+ y (Math/round (* (double b) h)))]
              (c2d/rect c x y0 w (- y1 y0)))
            (let [x0 (+ x (Math/round (* (double a) w))) x1 (+ x (Math/round (* (double b) w)))]
              (c2d/rect c x0 y (- x1 x0) h))))))
    (draw/inner-border! c box border-color border-width)
    (canvas/call-with-isolated-graphics c #(draw/draw-content (label content) % box env))))
