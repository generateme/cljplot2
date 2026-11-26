;; # Overview

;; The reference lists every option of cljplot2: the plot map, render options, scales, facets, labels, the theme,
;; every chart, builder and user-facing data function. The option cards are generated from the library's validating
;; schemas, so they name exactly what `plot/build` accepts. Use cases and galleries show the options at work; these
;; four parts say what exists.

(ns reference.intro
  (:require [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [cljplot2.pipeline :as pipeline]
            [notebook-utils :as nu]))

;; The table below holds four made-up points in two groups; most plots of A plot is a map draw them:

(def points [{:x 1 :y 2 :g "a"} {:x 2 :y 3 :g "a"} {:x 3 :y 1 :g "b"} {:x 4 :y 4 :g "b"}])

(nu/data-table points)

;; ## How the reference is organised

;; * **Concepts** (this chapter and the next twelve) - rendering, data and mapping, types, aesthetics and legends, scales
;;   and coordinates, layering, labels and text, facets and multiplots, the theme, the shared option groups, errors
;;   and what the library doesn't do.
;; * **Charts** - the options of each chart kind, grouped by what they show:
;;   [points, lines and areas](reference.charts_points_lines.html), [annotations](reference.charts_annotations.html),
;;   [bars](reference.charts_bars.html), [distribution per level](reference.charts_per_level.html),
;;   [distribution of one variable](reference.charts_one_variable.html),
;;   [fitted curves](reference.charts_fitted_curves.html), [2D bins and fields](reference.charts_bins_fields.html),
;;   [pairwise statistics](reference.charts_pairwise.html), [shares and sets](reference.charts_shares_sets.html),
;;   [multivariate and 3D](reference.charts_multivariate_3d.html).
;; * **Builders** (from [Graphs](reference.builders_graphs.html)) - the functions turning rows into whole plots:
;;   networks, trees, series.
;; * **Data functions** (from [Levels](reference.data_levels.html)) - the user-facing functions preparing rows, and
;;   internal namespaces shown by example.

;; ## How to read a card

;; Each group of cards holds related options and comes before an example that uses them - a plot or a value.

;; * **Header** - the option's key and its default. A nested key shows its path inside the group's map
;;   (`:scales :x`), `(theme)` marks a default the theme holds, and a computed default is described in words. Keys
;;   without a default (required keys, and maps whose own keys follow) show none.
;; * **Example** - for most options, a call that sets the option, or a plot map for plot keys
;;   (`{:scales {:x {:kind :log}}}`).
;; * **Text** - what the option does, then a point per value or form with its meaning. An option of one form shows
;;   only its [type](reference.types.html), and a long list ends with its other values on one line.

;; A chart's, builder's or data function's section lists the shared groups of options it takes in a box linking to
;; the Shared options chapter, its mapping keys in a Mapping box and its positional arguments in an Arguments box.

;; ## A plot is a map

;; `plot/render-image` takes a plot map and render options and returns a `java.awt.image.BufferedImage`. The plot
;; keys come in groups; each has its chapter.

;; ### Content

;; These keys hold the rows, how their values map to dimensions, and the charts drawing them
;; ([Data and mapping](reference.data_mapping.html)):

(nu/option-cards [] (describe/top-level-rows (describe/options pipeline/plot-schema)) :content)

;; The plot below draws the points as a line with points:

(plot/render-image {:data points
                    :mapping {:x :x :y :y}
                    :charts [(chart/line) (chart/scatter)]}
                   {:width 500 :height 300})

;; The same rows under another mapping - `:x` and `:y` swapped - give another plot:

(plot/render-image {:data points
                    :mapping {:x :y :y :x}
                    :charts [(chart/line) (chart/scatter)]}
                   {:width 500 :height 300})

;; ### Scales

;; This key turns data values into positions, colours, shapes ...:

(nu/option-cards [] (describe/top-level-rows (describe/options pipeline/plot-schema)) :scales)

;; The plot below fixes and reverses the y domain and names the colours and their legend:

(plot/render-image {:data points
                    :mapping {:x :x :y :y :color :g}
                    :charts [(chart/scatter {:size 12})]
                    :scales {:y {:domain [0 5] :reverse true}
                             :color {:range [:darkorange :teal] :title "group"}}}
                   {:width 500 :height 300})

;; The `:scales` map takes a spec per dimension
;; ([Scales, axes and coordinates](reference.scales_axes_coords.html), [Facets, multiplots and margins](reference.facets_arrange.html),
;; [Aesthetics and legends](reference.aesthetics_legends.html)):

(nu/option-cards [] (describe/rows-under (describe/options pipeline/plot-schema) [:scales]))

;; The plot below spreads the small values with a logarithmic x scale:

(plot/render-image {:data (for [i (range 1 30)] {:x (* i i i) :y i})
                    :mapping {:x :x :y :y}
                    :charts [(chart/scatter)]
                    :scales {:x {:kind :log :formatter :power}}}
                   {:width 500 :height 300})

;; ### Coordinates

;; These keys set the coordinates: cartesian, flipped or with equal units, or polar
;; ([Scales, axes and coordinates](reference.scales_axes_coords.html)):

(nu/option-cards [] (describe/top-level-rows (describe/options pipeline/plot-schema)) :coordinates)

;; The plot below flips the bars horizontal, their order reversed:

(plot/render-image {:data [{:fruit "apples" :n 3} {:fruit "pears" :n 5} {:fruit "plums" :n 2}]
                    :mapping {:x :fruit :y :n}
                    :charts [(chart/bar)]
                    :scales {:x {:reverse true}}
                    :flip true}
                   {:width 500 :height 300})

;; The same bars in polar coordinates - a rose (bars filling their band, `:width 1.0`,
;; [Strip options](reference.shared_options.html#strip-options)):

(plot/render-image {:data [{:fruit "apples" :n 3} {:fruit "pears" :n 5} {:fruit "plums" :n 2}]
                    :mapping {:x :fruit :y :n :color :fruit}
                    :charts [(chart/bar {:width 1.0})]
                    :coord {:kind :polar}
                    :legend false}
                   {:width 350 :height 350})

;; ### Layout

;; These keys split the panel by data, put several plots in a grid or charts beside the panel
;; ([Facets, multiplots and margins](reference.facets_arrange.html)):

(nu/option-cards [] (describe/top-level-rows (describe/options pipeline/plot-schema)) :layout)

;; The plot below splits the points into a panel per group:

(plot/render-image {:data points
                    :mapping {:x :x :y :y :facet :g}
                    :charts [(chart/line) (chart/scatter)]}
                   {:width 500 :height 300})

;; The plot below puts two plots in one image with `plot/arrange`, in two columns weighted 3 : 2:

(plot/render-image (plot/arrange [{:data points :mapping {:x :x :y :y} :charts [(chart/line) (chart/scatter)]}
                                  {:data points :mapping {:x :x :y :y :color :g} :charts [(chart/scatter)]}]
                                 {:columns 2 :widths [3 2]})
                   {:width 800 :height 300})

;; ### Titles

;; This key holds the titles ([Labels and text](reference.labels_text.html)):

(nu/option-cards [] (describe/top-level-rows (describe/options pipeline/plot-schema)) :titles)

;; The plot below names the plot and both axes:

(plot/render-image {:data points
                    :mapping {:x :x :y :y}
                    :charts [(chart/line)]
                    :labels {:title "Four points" :x "time [s]" :y "height [m]"}}
                   {:width 500 :height 300})

;; ### Legend

;; Mapped aesthetics draw legends; these keys hide them or draw only them
;; ([Aesthetics and legends](reference.aesthetics_legends.html)):

(nu/option-cards [] (describe/top-level-rows (describe/options pipeline/plot-schema)) :legend)

;; The plot below hides the legend of its colours:

(plot/render-image {:data points
                    :mapping {:x :x :y :y :color :g}
                    :charts [(chart/scatter {:size 12})]
                    :legend false}
                   {:width 500 :height 300})

;; ### Style

;; The theme holds every style default; these keys override parts of it for one plot ([Theme](reference.theme.html)):

(nu/option-cards [] (describe/top-level-rows (describe/options pipeline/plot-schema)) :style)

;; The plot below gives the plot a white background and the panel a light fill and a border:

(plot/render-image {:data points
                    :mapping {:x :x :y :y}
                    :charts [(chart/line) (chart/scatter)]
                    :theme {:plot {:background :white}
                            :panel {:fill [245 245 250] :border-color [180 180 180]}}}
                   {:width 500 :height 300})

;; ### Render options

;; This key lets a plot carry its render options ([Render](reference.render.html)):

(nu/option-cards [] (describe/top-level-rows (describe/options pipeline/plot-schema)) :render)

;; The plot below carries its own size, so `render-image` takes no options:

(plot/render-image {:data points
                    :mapping {:x :x :y :y}
                    :charts [(chart/line)]
                    :options {:width 300 :height 200}})

;; ## The pipeline

;; `plot/build` validates the plot first and throws an error listing the problems ([Errors](reference.errors.html)). Then the plot
;; passes through pure stages, each adding one key: `:split` (rows split per facet panel and group) → `:stats`
;; (each chart's computed data) → `:extents` → `:scale-specs` → `:grid` → `:layout` → `:finalized-scales` →
;; `:subplots`. `plot/render` draws the result.

;; ## The cards as data

;; The cards come from `cljplot2.describe`, which you can use at the REPL too:
;;
;; * `describe/options` returns the rows of a chart value, a builder or data function var, or a schema;
;; * `describe/mapping` returns the mapping keys a chart reads, `describe/theme-rows` the theme's rows and
;;   `describe/fragment-rows` a shared group's;
;; * `describe/api-vars` lists the user-facing data functions, `describe/chart-constructors` the charts and
;;   `describe/attribute-values` the built-in shapes, dashes, patterns and cells.
;;
;; A row is a plain map; `:section` names its slice of the options:

(first (describe/options pipeline/plot-schema))
