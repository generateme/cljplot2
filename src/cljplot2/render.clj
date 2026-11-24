(ns cljplot2.render
  "Rendering a built plot onto a clojure2d canvas.

  A plot is built by [[cljplot2.pipeline/build]] and each item of its layout grid is drawn at its box: item content by [[cljplot2.graphics.draw/draw-content]], plot items (arranged sub-plots) directly on the same canvas, translated and clipped - no intermediate image. Render option `:debug` first paints every item box with a distinct colour, its outline and its id; `:cvd` passes the finished drawing through a colour vision deficiency simulation.

  Functions: [[render]] draws onto a canvas, [[render-image]] returns a `java.awt.image.BufferedImage`, [[cvd-image]] simulates a colour vision deficiency on an image."
  (:require [fastmath.core :as m]
            [clojure2d.core :as c2d]
            [clojure2d.color :as c]
            [clojure2d.pixels :as p]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.shape :as shape]
            [cljplot2.layout.grid :as grid]
            [cljplot2.pipeline :as pipeline]
            [cljplot2.content.legend :as legend]
            [cljplot2.content.subplot :as subplot]
            [cljplot2.theme :as theme])
  (:import [java.awt.image BufferedImage]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private debug-palette
  "Fill colours of the debug item boxes, cycled."
  (c/palette :RColorBrewer/Set3))

(def ^{:private true :const true :tag 'long} debug-colors
  "Number of colours in [[debug-palette]]."
  (count debug-palette))

(defn- draw-debug-items!
  "Draws the debug boxes of layout items: each box filled with the next colour of [[debug-palette]], outlined, its id in the middle.

  Parameters:

  - `canvas` (clojure2d canvas): the target.
  - `items` (sequence of maps): layout items `{:id :box :layout}`; `:box` is `{:x :y :w :h}` in whole pixels, a nested `:layout` is drawn depth-first after its item.
  - `color-idx` (long): the colour number of the first item.

  Returns the colour number after the last item drawn."
  ^long [canvas items ^long color-idx]
  (reduce (fn [^long idx {:keys [id box layout]}]
            (let [{:keys [^long x ^long y ^long w ^long h]} box]
              (c2d/set-color canvas (debug-palette (m/mod idx debug-colors)))
              (c2d/rect canvas x y w h)
              (c2d/set-color canvas :black 120)
              (c2d/rect canvas x y w h true)
              (c2d/set-color canvas :black)
              (c2d/text canvas (pr-str id) (m/+ x (m// w 2)) (m/+ y (m// h 2) 4) :center)
              (if layout
                (draw-debug-items! canvas (:items layout) (m/inc idx))
                (m/inc idx))))
          color-idx items))

(declare draw-built!)

(defn- draw-subplot!
  "Draws a built sub-plot in a box of the outer plot: translated to the box corner, clipped to it, the sub-plot's background filling it.

  Parameters:

  - `canvas` (clojure2d canvas): the outer plot's canvas (its graphics state is restored afterwards).
  - `built` (map): the built sub-plot.
  - `box` (map): `{:x :y :w :h}` in whole pixels.
  - `debug?` (boolean): debug boxes inside the sub-plot too, see [[draw-built!]]."
  [canvas built {:keys [^long x ^long y ^long w ^long h]} debug?]
  (c2d/with-isolated-canvas [clipped canvas]
    (c2d/translate clipped x y)
    (c2d/add-clip clipped 0 0 w h)
    (let [background (get-in built [:theme :plot :background])]
      (when-not (theme/none? background)
        (c2d/set-color clipped background)
        (c2d/rect clipped 0 0 w h)))
    (draw-built! clipped built debug?)))

(defn- draw-contents!
  "Draws the content of every item of a grid at its box, recursing into nested grids.

  Parameters:

  - `canvas` (clojure2d canvas): the target.
  - `grid` (map): a layout grid; its `:items` hold the content.
  - `layout` (map): the solved layout of `grid`, its `:items` in the same order (see [[cljplot2.layout.grid/solve]]).
  - `env` (map): the drawing environment - theme, scales, charts and stats, `:subplots` (built sub-plots by item id), `:debug?`.

  Plot items draw their built sub-plot from `env` `:subplots`; one without a built sub-plot (e.g. a cell under 1x1 px) is skipped. Every other item is drawn with isolated graphics state."
  [canvas grid layout env]
  (doseq [[item {:keys [box] :as item-layout}] (map vector (:items grid) (:items layout))
          :let [content (:content item)]]
    (cond
      (grid/grid? content) (draw-contents! canvas content (:layout item-layout) env)
      (subplot/plot-content? content) (when-let [built (get-in env [:subplots (:id item)])]
                                        (draw-subplot! canvas built box (:debug? env)))
      :else (c2d/with-isolated-canvas [isolated canvas] (draw/draw-content content isolated box env)))))

(defn- draw-built!
  "Draws a built plot's items onto a canvas at the origin.

  Parameters:

  - `canvas` (clojure2d canvas): the target.
  - `built` (map): a plot built by [[cljplot2.pipeline/build]].
  - `debug?` (boolean): paint the debug item boxes first (see [[draw-debug-items!]]).

  Text shapes without a `:font` use the plot theme's annotation font (see [[cljplot2.graphics.shape/*text-font*]])."
  [canvas built debug?]
  (when debug?
    (draw-debug-items! canvas (get-in built [:layout :items]) 0))
  (binding [shape/*text-font* (get-in built [:theme :text :annotation :font] shape/*text-font*)]
    (draw-contents! canvas (:grid built) (:layout built) {:theme (:theme built) :scales (:finalized-scales built)
                                                        :panel-scales (:panel-scales built)
                                                        :margins (:margin-env built)
                                                        :legend-charts (:charts (legend/legend-charts built))
                                                        :legend-stats (:stats (legend/legend-charts built))
                                                        :charts (:charts built) :stats (:stats built)
                                                        :subplots (:subplots built) :debug? debug?})))

(defn- cvd-pixels
  "Pixels as seen with a colour vision deficiency.

  Parameters:

  - `pixels` (clojure2d pixels): the source, unchanged.
  - `deficiency` (keyword or `[keyword severity]`): see [[cljplot2.graphics.draw/cvd-filter-fn]].

  Returns new pixels with every colour passed through the simulation."
  [pixels deficiency]
  (p/filter-colors (draw/cvd-filter-fn deficiency) pixels))

(defn cvd-image
  "Gives an image as seen with a colour vision deficiency.

  Parameters:

  - `image` (`java.awt.image.BufferedImage`): the source, unchanged.
  - `deficiency` (keyword or `[keyword severity]`): `:protanopia`, `:deuteranopia`, `:tritanopia`, `:achromatopsia`, or `[:protanomaly s]` etc. with a severity in `[0 1]`, see [[cljplot2.graphics.draw/cvd-filter-fn]].

  Returns a new `java.awt.image.BufferedImage`; alpha is kept.

  See also [[render]] (option `:cvd`)."
  ^BufferedImage [image deficiency]
  (c2d/get-image (cvd-pixels (p/to-pixels image) deficiency)))

(defn render
  "Builds a plot and draws it onto a new clojure2d canvas.

  Parameters:

  - `plot` (map): a plot map.
  - `options` (map, optional): render options merged over the plot's `:options` (see [[cljplot2.pipeline/build]]; `:until` is ignored):
    - `:width`, `:height` - pixels (default 600 each), `:hint` - rendering quality (default `:high`);
    - `:debug` - paint item boxes and ids under the content, sub-plots included (default `false`);
    - `:cvd` - a colour vision deficiency the finished drawing, sub-plots included, passes through (as [[cvd-image]]).

  Returns a clojure2d canvas of exactly `:width` x `:height`, filled with the theme's plot background unless it is `:none`. Throws the `ex-info` of [[cljplot2.pipeline/build]] for invalid input.

  See also [[render-image]]."
  ([plot] (render plot {}))
  ([plot options]
   (let [built (pipeline/build plot (dissoc options :until))
         {:keys [width height hint]} (:options built)
         drawn (c2d/with-canvas [target (c2d/canvas width height hint)]
                 (let [background (get-in built [:theme :plot :background])]
                   (when-not (theme/none? background) ;; canvas is transparent until painted
                     (c2d/set-background target background)))
                 (draw-built! target built (get-in built [:options :debug]))
                 target)]
     (if-let [deficiency (get-in built [:options :cvd])]
       (p/set-canvas-pixels! drawn (cvd-pixels (p/to-pixels drawn) deficiency))
       drawn))))

(defn render-image
  "Builds and draws a plot as [[render]] does and returns the drawing as an image.

  Parameters:

  - `plot` (map): a plot map.
  - `options` (map, optional): render options, as for [[render]].

  Returns a `java.awt.image.BufferedImage` of exactly `:width` x `:height` (e.g. for notebooks or Swing); errors as [[render]]."
  (^BufferedImage [plot] (render-image plot {}))
  (^BufferedImage [plot options] (c2d/get-image (render plot options))))
