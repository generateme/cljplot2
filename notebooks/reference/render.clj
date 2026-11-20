;; # Render

;; Building and drawing a plot: `plot/build` (the built plot, data only), `plot/render` (a clojure2d canvas),
;; `plot/render-image` (a `java.awt.image.BufferedImage`) and `plot/cvd-image` (an image as seen with a colour vision
;; deficiency). All take the same render options.

(ns reference.render
  (:require [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [cljplot2.pipeline :as pipeline]
            [notebook-utils :as nu]))

;; ## Render options

;; Given as the second argument of `build` / `render` / `render-image`, or under the plot's `:options` (merged under
;; the argument). The size is the whole image, legends and titles included.

(nu/options-table (describe/options pipeline/options-schema))

(nu/source-note {:dataset :iris})

(def iris (nu/dataset :iris))

(def iris-plot
  {:data iris
   :mapping {:x :sepal-length :y :petal-length :color :species}
   :charts [(chart/scatter)]})

;; ## Size and quality

;; `:width` / `:height` in pixels; `:hint` trades antialiasing and text metrics for speed (`:low` draws without
;; antialiasing).

(plot/render-image iris-plot {:width 500 :height 350 :hint :low})

;; ## Layout debugging

;; `:debug true` paints every layout item's box under its content - panel, axes, axis titles, legend.

(plot/render-image iris-plot {:width 500 :height 350 :debug true})

;; ## Colour vision deficiency

;; `:cvd` renders the plot as seen with a deficiency (a keyword, or `[keyword severity]` for the partial ones);
;; `plot/cvd-image` converts any image.

(plot/render-image iris-plot {:width 500 :height 350 :cvd :deuteranopia})

;; ## Intermediate data

;; `:until` stops the build after a stage; the built plot is data. Here the build stops after `:extents` - the
;; extent of each mapped dimension:

(:extents (plot/build iris-plot {:until :extents}))

;; ## Saving a file

;; cljplot2 has no save function: write the image (or the canvas of `plot/render`) with `clojure2d.core/save`,
;; e.g. `(c2d/save (plot/render-image p) "plot.png")` - the file extension picks the format.
