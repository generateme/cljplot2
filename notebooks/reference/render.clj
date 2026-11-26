;; # Render

;; Five functions build and draw a plot: `plot/build` returns the built plot (data only), `plot/render` a clojure2d
;; canvas, `plot/render-image` a `java.awt.image.BufferedImage`, `plot/cvd-image` an image as seen with a colour vision
;; deficiency, and `plot/save` writes an image or canvas to a file. The first three take the same render options as
;; their second argument; a plot may also carry them under its `:options` key, merged under the argument. The card
;; examples set them under that key.

(ns reference.render
  (:require [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [cljplot2.pipeline :as pipeline]
            [notebook-utils :as nu]))

;; The table below holds the iris flowers; every section draws them (`iris-plot`):

(nu/source-note {:dataset :iris})

(def iris (nu/dataset :iris))

(nu/data-table iris)

(def iris-plot
  {:data iris
   :mapping {:x :sepal-length :y :petal-length :color :species}
   :charts [(chart/scatter)]})

;; ## Size and quality

;; These options set the image size in pixels - the whole image, legends and titles included - and the rendering
;; quality, a clojure2d hint. The plot below draws without antialiasing:

(nu/option-cards [:options] (describe/options pipeline/options-schema) :image)

(plot/render-image iris-plot {:width 500 :height 300 :hint :low})

;; The plot below draws at `:highest`, with smooth edges and text:

(plot/render-image iris-plot {:width 500 :height 300 :hint :highest})

;; ## Inspection

;; This option helps find out why a plot looks as it does. The plot below paints every layout item's box under its
;; content - panel, axes, axis titles, legend (axis titles added here):

(nu/option-cards [:options] (describe/options pipeline/options-schema) :inspection)

(plot/render-image (assoc iris-plot :labels {:x "sepal length" :y "petal length"})
                   {:width 500 :height 300 :debug true})

;; `plot/build` takes one more option, which a plot's `:options` ignores:

(nu/option-cards '(plot/build plot) (describe/options pipeline/options-schema) :build)

;; The build below stops after `:extents`; that key holds the extent of each mapped dimension:

(:extents (plot/build iris-plot {:until :extents}))

;; ## Colour vision

;; With `:cvd` you see how the plot reads with a colour vision deficiency; `plot/cvd-image` converts any image. The plot
;; below draws red and green species, as rendered:

(nu/option-cards [:options] (describe/options pipeline/options-schema) :color-vision)

(def red-green-plot (assoc iris-plot :scales {:color {:range [[215 48 39] [26 152 80] [0 114 178]]}}))

(plot/render-image red-green-plot {:width 500 :height 300})

;; The plot below draws them as seen with deuteranopia: setosa and versicolor turn into similar olive tones, virginica
;; stays blue:

(plot/render-image red-green-plot {:width 500 :height 300 :cvd :deuteranopia})

;; A partial deficiency takes a severity in `[0 1]`; the plot below draws deuteranomaly at half strength:

(plot/render-image red-green-plot {:width 500 :height 300 :cvd [:deuteranomaly 0.5]})

;; ## Saving

;; `plot/save` writes an image (or the canvas of `plot/render`) to a file - the format from the extension (png,
;; jpg, bmp, tiff ...) - and returns it, so it ends a pipeline:
;;
;; ```clojure
;; (-> iris-plot
;;     (plot/render-image {:width 800 :height 500})
;;     (plot/save "iris.png"))
;; ```
