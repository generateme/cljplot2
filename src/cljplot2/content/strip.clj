(ns cljplot2.content.strip
  "Strip content `{:kind :strip :text t}` (optional `:rotation`): a facet label - a `:strip` role label over theme
  `[:strip {:fill :border-color :border-width}]` (`:none` colors draw nothing). Natural size of the label."
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
    (draw/inner-border! c box border-color border-width)
    (canvas/call-with-isolated-graphics c #(draw/draw-content (label content) % box env))))
