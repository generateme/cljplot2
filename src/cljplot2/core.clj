(ns cljplot2.core
  "Public API."
  (:require [cljplot2.pipeline :as pipeline]
            [cljplot2.multiplot :as multiplot]
            [cljplot2.render :as render]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn build
  "Run `plot` through the pipeline, see [[cljplot2.pipeline/build]]."
  ([plot] (pipeline/build plot))
  ([plot options] (pipeline/build plot options)))

(defn arrange
  "Plot map arranging `plots` in a grid, see [[cljplot2.multiplot/arrange]]."
  ([plots] (multiplot/arrange plots))
  ([plots options] (multiplot/arrange plots options)))

(defn pairs
  "Scatter-plot matrix (and correlogram) of numeric variables, see [[cljplot2.multiplot/pairs]]."
  [data options]
  (multiplot/pairs data options))

(defn render
  "Build and draw `plot`, returning a clojure2d canvas, see [[cljplot2.render/render]]."
  ([plot] (render/render plot))
  ([plot options] (render/render plot options)))

(defn render-image
  "Build and draw `plot`, returning a `java.awt.image.BufferedImage`, see [[cljplot2.render/render-image]]."
  ([plot] (render/render-image plot))
  ([plot options] (render/render-image plot options)))

(defn cvd-image
  "`image` (a `java.awt.image.BufferedImage`) as seen with a color vision deficiency, a new image, see
  [[cljplot2.render/cvd-image]]."
  ^java.awt.image.BufferedImage [image deficiency]
  (render/cvd-image image deficiency))
