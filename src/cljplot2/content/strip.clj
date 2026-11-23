(ns cljplot2.content.strip
  "Strip content: a facet label over a strip box.

  Content `{:kind :strip :text t}` (optional `:rotation`) is a `:strip` role label over theme
  `[:strip {:fill :border-color :border-width}]` (`:none` colors draw nothing); its natural size is the label's.
  With `:interval {:lo :hi :min :max}` (a shingle level) a bar in theme `[:strip :shingle-fill]` shades the part of
  the strip that `[lo hi]` covers within `[min max]` - along the text direction (top-down for `:rotation 90`).

  Functions: methods of [[cljplot2.graphics.measure/validate-content]], [[cljplot2.graphics.measure/natural-size]]
  and [[cljplot2.graphics.draw/draw-content]] for `:strip`."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.theme :as theme]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- strip-label
  "Gives the `:strip` role label content of strip `content`.

  Parameters:

  - `content` (map): the strip content - `:text`, optional `:rotation`.

  Returns `{:kind :label :role :strip :text t}`, with the rotation when given."
  [{:keys [text rotation]}]
  (cond-> {:kind :label :role :strip :text text} rotation (assoc :rotation rotation)))

(defn- interval-fractions
  "Gives the fractions of a strip a shingle interval covers.

  Parameters:

  - `interval` (map): `:lo`, `:hi` - the level's range; `:min`, `:max` - the range of all levels.

  Returns `[a b]`, the fractions of `lo` and `hi` within `[min max]`; `[0.0 1.0]` when `min` equals `max` (or exceeds it)."
  [{:keys [^double lo ^double hi] ^double start :min ^double end :max}]
  (if (m/> end start)
    [(m/norm lo start end) (m/norm hi start end)]
    [0.0 1.0]))

(defmethod measure/validate-content :strip
  [content]
  (measure/validate-content (strip-label content)))

(defmethod measure/natural-size :strip
  [content env]
  (measure/natural-size (strip-label content) env))

(defmethod draw/draw-content :strip
  [content c {:keys [^long x ^long y ^long w ^long h] :as box} env]
  (let [{:keys [fill border-color border-width shingle-fill]} (get-in env [:theme :strip])]
    (when-not (theme/none? fill)
      (c2d/set-color c fill)
      (c2d/rect c x y w h))
    (when-let [interval (:interval content)]
      (when-not (theme/none? shingle-fill)
        (let [[^double a ^double b] (interval-fractions interval)]
          (c2d/set-color c shingle-fill)
          (if (= 90 (:rotation content))
            (let [y0 (m/long-add y (m/round (m/* a h))) y1 (m/long-add y (m/round (m/* b h)))]
              (c2d/rect c x y0 w (m/long-sub y1 y0)))
            (let [x0 (m/long-add x (m/round (m/* a w))) x1 (m/long-add x (m/round (m/* b w)))]
              (c2d/rect c x0 y (m/long-sub x1 x0) h))))))
    (draw/inner-border! c box border-color border-width)
    (c2d/with-isolated-canvas [isolated c]
      (draw/draw-content (strip-label content) isolated box env))))
