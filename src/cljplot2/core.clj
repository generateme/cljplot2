(ns cljplot2.core
  "Public API."
  (:require [clojure2d.core :as c2d]
            [cljplot2.graphs :as graphs]
            [cljplot2.pipeline :as pipeline]
            [cljplot2.multiplot :as multiplot]
            [cljplot2.render :as render]
            [cljplot2.series-plots :as series-plots]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn build
  "Run `plot` through the pipeline, see [[cljplot2.pipeline/build]]."
  ([plot] (pipeline/build plot))
  ([plot options] (pipeline/build plot options)))

(defn arrange
  "Plot map arranging `plots` in a grid, see [[cljplot2.multiplot/arrange]]."
  {:cljplot2/options multiplot/arrange-options}
  ([plots] (multiplot/arrange plots))
  ([plots options] (multiplot/arrange plots options)))

(defn pairs
  "Scatter-plot matrix (and correlogram) of numeric variables, see [[cljplot2.multiplot/pairs]]."
  {:cljplot2/options multiplot/pairs-options}
  [data options]
  (multiplot/pairs data options))

(defn network
  "Network plot of edge rows (graph layout, segments, nodes, labels), see [[cljplot2.graphs/network]]."
  {:cljplot2/options graphs/network-options}
  [edges options]
  (graphs/network edges options))

(defn dendrogram
  "Dendrogram of an `hclust` result or a tree, see [[cljplot2.graphs/dendrogram]]."
  {:cljplot2/options graphs/dendrogram-options}
  [source options]
  (graphs/dendrogram source options))

(defn tree
  "Node-link tree plot (tidy or cluster layout, diagonal or straight links, cartesian or radial), see
  [[cljplot2.graphs/tree]]."
  {:cljplot2/options graphs/tree-options}
  ([source] (graphs/tree source {}))
  ([source options] (graphs/tree source options)))

(defn clustered-heatmap
  "Heatmap with rows and columns ordered by hierarchical clustering and their dendrograms, see
  [[cljplot2.graphs/clustered-heatmap]]."
  {:cljplot2/options graphs/clustered-heatmap-options}
  [rows options]
  (graphs/clustered-heatmap rows options))

(defn bundle
  "Hierarchical edge bundling of edges between a tree's leaves, see [[cljplot2.graphs/bundle]]."
  {:cljplot2/options graphs/bundle-options}
  [edges tree options]
  (graphs/bundle edges tree options))

(defn chord
  "Chord diagram of edge rows (ring sectors and ribbons), see [[cljplot2.graphs/chord]]."
  {:cljplot2/options graphs/chord-options}
  [edges options]
  (graphs/chord edges options))

(defn arc
  "Arc diagram of edge rows (nodes on a line, edges as semicircles), see [[cljplot2.graphs/arc]]."
  {:cljplot2/options graphs/arc-options}
  [edges options]
  (graphs/arc edges options))

(defn sankey
  "Sankey diagram of flow rows (nodes in columns, bands between them), see [[cljplot2.graphs/sankey]]."
  {:cljplot2/options graphs/sankey-options}
  [edges options]
  (graphs/sankey edges options))

(defn alluvial
  "Alluvial diagram of rows over categorical axes (strata and strands), see [[cljplot2.graphs/alluvial]]."
  {:cljplot2/options graphs/alluvial-options}
  [rows options]
  (graphs/alluvial rows options))

(defn sequences
  "Flow sunburst of sequences (steps as rings coloured by name), see [[cljplot2.graphs/sequences]]."
  {:cljplot2/options graphs/sequences-options}
  [rows options]
  (graphs/sequences rows options))

(defn acf
  "Correlogram of a series (ACF and PACF stems with significance bands), see [[cljplot2.series-plots/acf]]."
  {:cljplot2/options series-plots/acf-options}
  ([values] (series-plots/acf values {}))
  ([values options] (series-plots/acf values options)))

(defn horizon
  "Horizon chart of rows (bands of one height, negatives mirrored, a panel per series), see
  [[cljplot2.series-plots/horizon]]."
  {:cljplot2/options series-plots/horizon-options}
  ([rows] (series-plots/horizon rows {}))
  ([rows options] (series-plots/horizon rows options)))

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

(defn save
  "Save `image` - a `java.awt.image.BufferedImage` ([[render-image]], [[cvd-image]]) or a clojure2d canvas ([[render]])
  - to the file `file-name` and return `image` (`clojure2d.core/save`), so saving composes:
  `(-> plot render-image (save \"plot.png\"))`. The format follows the extension (png, jpg, bmp, tiff ...); missing
  parent directories are created; with an extension no image writer knows nothing is written (clojure2d prints
  \"can't save an image\")."
  [image file-name]
  (c2d/save image file-name))
