(ns cljplot2.core
  "Public API."
  (:require [cljplot2.pipeline :as pipeline]
            [cljplot2.render :as render]))

(defn build
  "Run `plot` through the pipeline, see [[cljplot2.pipeline/build]]."
  ([plot] (pipeline/build plot))
  ([plot options] (pipeline/build plot options)))

(defn render
  "Build and draw `plot`, returning a clojure2d canvas, see [[cljplot2.render/render]]."
  ([plot] (render/render plot))
  ([plot options] (render/render plot options)))
