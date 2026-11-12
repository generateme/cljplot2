(ns cljplot2.render
  "Rendering a built plot onto a clojure2d canvas.

  Item content is drawn by [[cljplot2.graphics.draw/draw-content]]; plot items draw their built sub-plot directly on the
  same canvas (translated, clipped - no intermediate image); option `:debug` first paints every item box with a
  distinct color, its outline and its id."
  (:require [clojure2d.core :as c2d]
            [clojure2d.color :as c]
            [cljplot2.graphics.canvas :as canvas]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.layout.grid :as grid]
            [cljplot2.pipeline :as pipeline]
            [cljplot2.content.subplot :as subplot]
            [cljplot2.theme :as theme])
  (:import [java.awt Graphics2D Rectangle]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private debug-palette (c/palette :RColorBrewer/Set3))

(defn- draw-debug-items!
  "Draw `items` (and nested layouts, depth-first) starting at color number `color-idx`; return the next color number."
  ^long [canvas items ^long color-idx]
  (reduce (fn [^long idx {:keys [id box layout]}]
            (let [{:keys [x y w h]} box
                  x (long x) y (long y) w (long w) h (long h)]
              (c2d/set-color canvas (debug-palette (mod idx (count debug-palette))))
              (c2d/rect canvas x y w h)
              (c2d/set-color canvas :black 120)
              (c2d/rect canvas x y w h true)
              (c2d/set-color canvas :black)
              (c2d/text canvas (pr-str id) (+ x (quot w 2)) (+ y (quot h 2) 4) :center)
              (if layout
                (draw-debug-items! canvas (:items layout) (inc idx))
                (inc idx))))
          color-idx items))

(declare draw-built!)

(defn- draw-subplot!
  "Draw built sub-plot `built` in `box`: translated to its corner, clipped to it, its background filling it."
  [canvas built {:keys [x y w h]} debug?]
  (canvas/call-with-isolated-graphics
   canvas
   (fn [c]
     (let [w (long w) h (long h)]
       (c2d/translate c (long x) (long y))
       (.clip ^Graphics2D (:graphics c) (Rectangle. 0 0 (int w) (int h)))
       (let [background (get-in built [:theme :plot :background])]
         (when-not (theme/none? background)
           (c2d/set-color c background)
           (c2d/rect c 0 0 w h)))
       (draw-built! c built debug?)))))

(defn- draw-contents!
  "Draw the content of every item of `grid` at its box in `layout`, recursing into nested grids; plot items draw
  their built sub-plot from `env` `:subplots` (skipped when absent, e.g. a cell under 1x1 px).
  `layout` items are in `grid` `:items` order (see [[cljplot2.layout.grid/solve]])."
  [canvas grid layout env]
  (doseq [[item {:keys [box] :as item-layout}] (map vector (:items grid) (:items layout))
          :let [content (:content item)]]
    (cond
      (grid/grid? content) (draw-contents! canvas content (:layout item-layout) env)
      (subplot/plot-content? content) (when-let [built (get-in env [:subplots (:id item)])]
                                        (draw-subplot! canvas built box (:debug? env)))
      :else (canvas/call-with-isolated-graphics canvas #(draw/draw-content content % box env)))))

(defn- draw-built!
  "Draw a built plot's items (debug boxes first when `debug?`) onto `canvas` at the origin."
  [canvas built debug?]
  (when debug?
    (draw-debug-items! canvas (get-in built [:layout :items]) 0))
  (draw-contents! canvas (:grid built) (:layout built) {:theme (:theme built) :scales (:finalized-scales built)
                                                      :charts (:charts built) :stats (:stats built)
                                                      :subplots (:subplots built) :debug? debug?}))

(defn render
  "Build `plot` (see [[cljplot2.pipeline/build]], `:until` is ignored) and draw it.
  Option `:debug` (truthy) paints item boxes and ids under the content, also inside sub-plots.
  Returns a clojure2d canvas of exactly `:width` x `:height`."
  ([plot] (render plot {}))
  ([plot options]
   (let [built (pipeline/build plot (dissoc options :until :debug))
         {:keys [width height hint]} (:options built)]
     (canvas/call-with-graphics
      (c2d/canvas width height hint)
      (fn [target]
        (let [background (get-in built [:theme :plot :background])]
          (when-not (theme/none? background) ;; canvas is transparent until painted
            (c2d/set-background target background)))
        (draw-built! target built (boolean (:debug options)))
        target)))))
