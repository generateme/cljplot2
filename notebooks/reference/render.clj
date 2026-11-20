;; # Render

;; Building and drawing a plot: `plot/build` (the built plot, data only), `plot/render` (a clojure2d canvas),
;; `plot/render-image` (a `java.awt.image.BufferedImage`), `plot/cvd-image` (an image as seen with a colour vision
;; deficiency) and `plot/save` (an image or canvas to a file). The first three take the same render options, as
;; their second argument or under the plot's `:options` (merged under the argument).

(ns reference.render
  (:require [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [cljplot2.pipeline :as pipeline]
            [notebook-utils :as nu]))

(nu/source-note {:dataset :iris})

(def iris (nu/dataset :iris))

(def iris-plot
  {:data iris
   :mapping {:x :sepal-length :y :petal-length :color :species}
   :charts [(chart/scatter)]})

;; ## Size and quality

;; The image size in pixels - the whole image, legends and titles included - and the rendering quality (clojure2d
;; hints): `:low` draws without antialiasing (faster, jagged edges), `:mid` antialiased for speed, `:high`
;; antialiased for quality, `:highest` adds exact stroke geometry (lines can look softer).

(nu/options-table (describe/options pipeline/options-schema) :image)

(plot/render-image iris-plot {:width 500 :height 300 :hint :low})

;; ## Inspection

;; Two aids for finding out why a plot looks as it does. `:debug true` paints every layout item's box under its
;; content - panel, axes, axis titles, legend:

(nu/options-table (describe/options pipeline/options-schema) :inspection)

(plot/render-image iris-plot {:width 500 :height 300 :debug true})

;; `:until` stops the build after a stage; the built plot is data. Here the build stops after `:extents` - the
;; extent of each mapped dimension:

(:extents (plot/build iris-plot {:until :extents}))

;; ## Colour vision

;; `:cvd` renders the plot as seen with a colour vision deficiency - a keyword, or `[keyword severity]` for the
;; partial ones; `plot/cvd-image` converts any image. Red and green species, as rendered:

(nu/options-table (describe/options pipeline/options-schema) :color-vision)

(def red-green-plot (assoc iris-plot :scales {:color {:range [[215 48 39] [26 152 80] [69 117 180]]}}))

(plot/render-image red-green-plot {:width 500 :height 300})

;; ... and as seen with deuteranopia - setosa and versicolor turn into similar olive tones, virginica stays blue:

(plot/render-image red-green-plot {:width 500 :height 300 :cvd :deuteranopia})

;; ## Saving

;; `plot/save` writes an image (or the canvas of `plot/render`) to a file - the format from the extension (png,
;; jpg, bmp, tiff ...) - and returns it, so it ends a pipeline:
;;
;; ```clojure
;; (-> iris-plot
;;     (plot/render-image {:width 800 :height 500})
;;     (plot/save "iris.png"))
;; ```
