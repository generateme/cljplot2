(ns cljplot2.core
  "Public API of cljplot2: build and render plot maps, builders turning data into plot maps, and colour helpers.

  A plot is a plain map of `:data`, `:mapping`, `:charts` (see [[cljplot2.chart]]), `:scales`, `:theme` ... - [[build]] runs it through the pipeline, [[render]] / [[render-image]] draw it, [[save]] writes an image.

  Builders return ordinary plot maps: [[arrange]], [[pairs]] (multiplots); [[network]], [[chord]], [[arc]], [[sankey]], [[alluvial]] (graphs and flows); [[dendrogram]], [[tree]], [[clustered-heatmap]], [[bundle]], [[sequences]] (trees and hierarchies); [[acf]], [[horizon]] (series). Each validates its options first (`:cljplot2/options` var metadata).

  Colours: [[find-palette]], [[find-gradient]], [[palette]], [[gradient]] call clojure2d; [[cvd-image]] simulates a colour vision deficiency.

  Text: [[math]] - a LaTeX formula in any label text, [[runs]] - pieces of text and formulas on one baseline (`$...$` pairs in strings are formulas too, see [[cljplot2.data.text]])."
  (:require [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [cljplot2.data.text :as text]
            [cljplot2.graphs :as graphs]
            [cljplot2.pipeline :as pipeline]
            [cljplot2.multiplot :as multiplot]
            [cljplot2.render :as render]
            [cljplot2.series-plots :as series-plots])
  (:import [java.awt.image BufferedImage]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn math
  "Gives a LaTeX formula value for any label text: titles, axis titles, tick labels, legends, strips, text charts.

  Parameters:

  - `tex` (any value): the formula in LaTeX math mode (converted with `str`), e.g. `x_i^2`.

  Returns a formula value, plain data: the whole text, or a piece of a runs value (see [[runs]]). A formula that doesn't parse throws `ex-info` when the plot is built (plot labels, literal text) or rendered (texts from data or formatters), keyed `:text` or by its option. See [[cljplot2.data.text/math]]."
  [tex]
  (text/math tex))

(defn runs
  "Gives a runs value for any label text: pieces drawn one after another on one baseline.

  Parameters:

  - `pieces` (values): strings (their dollar pairs are formulas with dollar math on), formula values of [[math]], other values (converted with `str`).

  Returns a runs value, plain data - e.g. a word followed by a formula. See [[cljplot2.data.text/runs]]."
  [& pieces]
  (apply text/runs pieces))

(defn build
  "Runs a plot map through the pipeline stages and returns it with every stage's key added.

  Parameters:

  - `plot` (map): a plot map.
  - `options` (map, optional): render options `:width`, `:height`, `:hint`, `:debug`, `:cvd` and `:until` (the last stage to run).

  Throws one `ex-info` listing every invalid input. See [[cljplot2.pipeline/build]]."
  ([plot] (pipeline/build plot))
  ([plot options] (pipeline/build plot options)))

(defn arrange
  "Builds a plot map arranging plots in a grid of cells.

  Parameters:

  - `plots` (sequence of plot maps): the cells, row by row.
  - `options` (map, optional): grid shape, track weights, panel alignment, `:title`, `:theme`.

  See [[cljplot2.multiplot/arrange]]."
  {:cljplot2/options multiplot/arrange-options}
  ([plots] (multiplot/arrange plots))
  ([plots options] (multiplot/arrange plots options)))

(defn pairs
  "Builds a scatter-plot matrix (or correlogram) of numeric variables as a plot map.

  Parameters:

  - `rows` (rows or columns): the data.
  - `options` (map): `:variables` (required), the charts of the `:lower`, `:upper` and `:diagonal` cells (`:none` - empty cells), `:mapping`, `:variable-names`, `:title`, `:theme`.

  Returns a plot map. See [[cljplot2.multiplot/pairs]]."
  {:cljplot2/options multiplot/pairs-options}
  [rows options]
  (multiplot/pairs rows options))

(defn network
  "Builds a network plot of edge rows as a plot map: a graph layout, edges as segments, nodes as points, optional labels.

  Parameters:

  - `edges` (rows): the edges.
  - `options` (map, optional): graph, layout, aesthetics and style options.

  See [[cljplot2.graphs/network]]."
  {:cljplot2/options graphs/network-options}
  ([edges] (graphs/network edges {}))
  ([edges options] (graphs/network edges options)))

(defn dendrogram
  "Builds a dendrogram of an `hclust` result or a tree as a plot map.

  Parameters:

  - `source` (map): a fastmath `hclust` result or a tree of [[cljplot2.data.tree]].
  - `options` (map, optional): labels, clusters, orientation and style options.

  See [[cljplot2.graphs/dendrogram]]."
  {:cljplot2/options graphs/dendrogram-options}
  ([source] (graphs/dendrogram source {}))
  ([source options] (graphs/dendrogram source options)))

(defn tree
  "Builds a node-link tree plot as a plot map: tidy or cluster layout, diagonal or straight links, cartesian or radial.

  Parameters:

  - `source` (map): a tree of [[cljplot2.data.tree]] or an `hclust` result.
  - `options` (map, optional): layout, labels, aesthetics and style options.

  See [[cljplot2.graphs/tree]]."
  {:cljplot2/options graphs/tree-options}
  ([source] (graphs/tree source {}))
  ([source options] (graphs/tree source options)))

(defn clustered-heatmap
  "Builds a heatmap with rows and columns ordered by hierarchical clustering, their dendrograms beside it, as a plot map.

  Parameters:

  - `rows` (rows): one row per cell.
  - `options` (map): row, column and value accessors, clustering method and distance, tree sizes.

  See [[cljplot2.graphs/clustered-heatmap]]."
  {:cljplot2/options graphs/clustered-heatmap-options}
  [rows options]
  (graphs/clustered-heatmap rows options))

(defn bundle
  "Builds a hierarchical edge bundling of edges between a tree's leaves as a plot map.

  Parameters:

  - `edges` (rows): edges between leaves.
  - `leaf-tree` (map): a tree of [[cljplot2.data.tree]].
  - `options` (map, optional): bundling strength, colours, labels.

  See [[cljplot2.graphs/bundle]]."
  {:cljplot2/options graphs/bundle-options}
  ([edges leaf-tree] (graphs/bundle edges leaf-tree {}))
  ([edges leaf-tree options] (graphs/bundle edges leaf-tree options)))

(defn chord
  "Builds a chord diagram of edge rows as a plot map: ring sectors per node, ribbons per flow.

  Parameters:

  - `edges` (rows): the flows.
  - `options` (map, optional): graph and layout options, ribbon opacity, labels.

  See [[cljplot2.graphs/chord]]."
  {:cljplot2/options graphs/chord-options}
  ([edges] (graphs/chord edges {}))
  ([edges options] (graphs/chord edges options)))

(defn arc
  "Builds an arc diagram of edge rows as a plot map: nodes on a line, edges as arcs above it.

  Parameters:

  - `edges` (rows): the edges.
  - `options` (map, optional): node order, aesthetics, style options, labels.

  See [[cljplot2.graphs/arc]]."
  {:cljplot2/options graphs/arc-options}
  ([edges] (graphs/arc edges {}))
  ([edges options] (graphs/arc edges options)))

(defn sankey
  "Builds a sankey diagram of flow rows as a plot map: nodes in columns, bands as wide as their flows between them.

  Parameters:

  - `edges` (rows): the flows.
  - `options` (map, optional): graph and layout options, band colour and opacity, labels.

  See [[cljplot2.graphs/sankey]]."
  {:cljplot2/options graphs/sankey-options}
  ([edges] (graphs/sankey edges {}))
  ([edges options] (graphs/sankey edges options)))

(defn alluvial
  "Builds an alluvial diagram of wide rows over categorical columns as a plot map: strata per level, strands between neighbouring columns.

  Parameters:

  - `rows` (rows): one row per combination of levels.
  - `options` (map): `:variables` (required), `:value` (weights), `:strand-color` (the level colouring the strands), `:variable-names`, layout and opacity.

  Returns a plot map.

  See [[cljplot2.graphs/alluvial]]."
  {:cljplot2/options graphs/alluvial-options}
  [rows options]
  (graphs/alluvial rows options))

(defn sequences
  "Builds a flow sunburst of sequences as a plot map: steps as rings from the centre, coloured by name.

  Parameters:

  - `rows` (rows): one sequence per row.
  - `options` (map, optional): sequence and value accessors, separator, end marker, centre hole, labels.

  See [[cljplot2.graphs/sequences]]."
  {:cljplot2/options graphs/sequences-options}
  ([rows] (graphs/sequences rows {}))
  ([rows options] (graphs/sequences rows options)))

(defn acf
  "Builds a correlogram of a series as a plot map: ACF and PACF stems with significance bands.

  Parameters:

  - `values` (sequence of numbers): the series.
  - `options` (map, optional): lags, significance level, kinds drawn, band shape, colours.

  See [[cljplot2.series-plots/acf]]."
  {:cljplot2/options series-plots/acf-options}
  ([values] (series-plots/acf values {}))
  ([values options] (series-plots/acf values options)))

(defn horizon
  "Builds a horizon chart of rows as a plot map: bands of one height, negatives mirrored, a panel per series.

  Parameters:

  - `rows` (rows): the series points.
  - `options` (map, optional): row keys, band count and height, colours, opacity.

  See [[cljplot2.series-plots/horizon]]."
  {:cljplot2/options series-plots/horizon-options}
  ([rows] (series-plots/horizon rows {}))
  ([rows options] (series-plots/horizon rows options)))

(defn render
  "Builds a plot and draws it onto a new clojure2d canvas.

  Parameters:

  - `plot` (map): a plot map.
  - `options` (map, optional): render options, see [[build]].

  Returns a canvas of exactly `:width` x `:height`. See [[cljplot2.render/render]], [[render-image]]."
  ([plot] (render/render plot))
  ([plot options] (render/render plot options)))

(defn render-image
  "Builds a plot and draws it as an image.

  Parameters:

  - `plot` (map): a plot map.
  - `options` (map, optional): render options, see [[build]].

  Returns a `java.awt.image.BufferedImage` of exactly `:width` x `:height`. See [[cljplot2.render/render-image]], [[save]]."
  (^BufferedImage [plot] (render/render-image plot))
  (^BufferedImage [plot options] (render/render-image plot options)))

(defn cvd-image
  "Gives an image as seen with a colour vision deficiency.

  Parameters:

  - `image` (`java.awt.image.BufferedImage`): the source, unchanged.
  - `deficiency` (keyword or `[keyword severity]`): e.g. `:deuteranopia`, `[:protanomaly 0.5]`.

  Returns a new image. See [[cljplot2.render/cvd-image]]."
  ^BufferedImage [image deficiency]
  (render/cvd-image image deficiency))

(defn find-palette
  "Names of the clojure2d palettes, for a colour scale's `:range`.

  Parameters:

  - `regex` (regex, optional): matched against the name with the colon and namespace.
  - `group` (string, optional): only palettes of this namespace.

  Returns sorted keywords: all of them, or those matching. Calls `clojure2d.color/find-palette`; see also [[palette]]."
  ([] (c/find-palette))
  ([regex] (c/find-palette regex))
  ([group regex] (c/find-palette group regex)))

(defn find-gradient
  "Names of the clojure2d gradients, for a colour scale's `:range`.

  Parameters:

  - `regex` (regex, optional): matched against the name with the colon and namespace.
  - `group` (string, optional): only gradients of this namespace.

  Returns sorted keywords: all of them, or those matching. Calls `clojure2d.color/find-gradient`; see also [[gradient]]."
  ([] (c/find-gradient))
  ([regex] (c/find-gradient regex))
  ([group regex] (c/find-gradient group regex)))

(defn palette
  "Gives a palette - a vector of colours.

  Parameters:

  - `p` (keyword, number, gradient or colours, optional): a palette name, a colourlovers number, a gradient or colours; without it a random palette.
  - `number-of-colors` (long, optional): resample to this many colours.
  - `gradient-params` (map, optional): interpolation of the resampling.

  Calls `clojure2d.color/palette`; see also [[find-palette]]."
  ([] (c/palette))
  ([p] (c/palette p))
  ([p number-of-colors] (c/palette p number-of-colors))
  ([p number-of-colors gradient-params] (c/palette p number-of-colors gradient-params)))

(defn gradient
  "Gives a gradient - a function of `t` in `[0 1]` returning a colour.

  Parameters:

  - `palette-or-gradient-name` (keyword or colours, optional): a gradient name or a palette; without it a random gradient.
  - `options` (map, optional): interpolation options for a palette.

  Calls `clojure2d.color/gradient`; see also [[find-gradient]]."
  ([] (c/gradient))
  ([palette-or-gradient-name] (c/gradient palette-or-gradient-name))
  ([palette-or-gradient-name options] (c/gradient palette-or-gradient-name options)))

(defn save
  "Saves an image to a file and returns the image, so saving composes: `(-> plot render-image (save path))`.

  Parameters:

  - `image` (`java.awt.image.BufferedImage` or clojure2d canvas): from [[render-image]], [[cvd-image]] or [[render]].
  - `file-name` (string): the path; its extension picks the format (png, jpg, bmp, tiff ...).

  Missing parent directories are created; with an extension no image writer knows nothing is written. clojure2d prints its progress to `*out*`. Calls `clojure2d.core/save`."
  [image file-name]
  (c2d/save image file-name))
