(ns cljplot2.render
  "Rendering a built plot onto a clojure2d canvas.

  Step 1: debug drawing of the solved grid - every item box filled with a distinct color and labelled with its id."
  (:require [clojure2d.core :as c2d]
            [clojure2d.color :as c]
            [cljplot2.canvas :as canvas]
            [cljplot2.pipeline :as pipeline]))

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

(defn render
  "Build `plot` (see [[cljplot2.pipeline/build]], `:until` is ignored) and draw it.
  Returns a clojure2d canvas of exactly `:width` x `:height`."
  ([plot] (render plot {}))
  ([plot options]
   (let [built (pipeline/build plot (dissoc options :until))
         {:keys [width height hint]} (:options built)]
     (canvas/call-with-graphics
      (c2d/canvas width height hint)
      (fn [target]
        (c2d/set-background target :white)
        (draw-debug-items! target (get-in built [:layout :items]) 0)
        target)))))
