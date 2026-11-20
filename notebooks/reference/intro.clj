;; # Reference

;; The reference lists every option of cljplot2: the plot map, render options, scales, facets, labels, the theme,
;; every chart, builder and user-facing data function. The tables are generated from the library's validating
;; schemas, so they name exactly what `plot/build` accepts. Use cases and galleries show the options at work; this
;; part says what exists.

(ns reference.intro
  (:require [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [cljplot2.pipeline :as pipeline]
            [notebook-utils :as nu]))

;; ## How the part is organised

;; * **Concepts** (this chapter and the next eleven) - rendering, data and mapping, aesthetics and legends, scales
;;   and coordinates, layering, labels and text, facets and multiplots, the theme, the shared option groups, errors
;;   and what the library doesn't do.
;; * **Charts**, **builders** and **data functions** - the options of each chart kind, builder and user-facing
;;   function, with examples (to come; until then the use cases and galleries show them).

;; ## How to read a table

;; Each table is one group of related options, followed by an example using them - a chart or a value.

;; * **Option** - the key; a nested key is listed after its map, with its full path (`:scales :x`). `(required)`
;;   marks a key that must be given.
;; * **Type** - the values the schema accepts.
;; * **Default** - the value used when the key is absent (style defaults come from the theme), and / or how a
;;   computed default is found. Empty: absent means not set - an optional feature off, or a map whose own keys
;;   have the defaults.
;; * **Description** - what the key does.
;; * **Group** - shown when a table mixes keys of a shared group (data and mapping, curves, strips ...) with others;
;;   a group is described once, in the Shared options chapter.

;; ## A plot is a map

;; `plot/render-image` takes a plot map and render options and returns a `java.awt.image.BufferedImage`. The plot
;; keys come in groups; each has its chapter.

;; ### Content

;; The rows, how their values map to dimensions, and the charts drawing them (Data and mapping chapter).

(nu/options-table (describe/top-level-rows (describe/options pipeline/plot-schema)) :content)

(def points [{:x 1 :y 2 :g "a"} {:x 2 :y 3 :g "a"} {:x 3 :y 1 :g "b"} {:x 4 :y 4 :g "b"}])

(nu/data-table points)

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

;; How data values become positions, colours, shapes ... (Scales and Aesthetics chapters).

(nu/options-table (describe/top-level-rows (describe/options pipeline/plot-schema)) :scales)

(plot/render-image {:data points
                    :mapping {:x :x :y :y :color :g}
                    :charts [(chart/scatter {:size 12})]
                    :scales {:y {:domain [0 5] :reverse true}
                             :color {:range [:darkorange :teal] :title "group"}}}
                   {:width 500 :height 300})

;; A logarithmic x scale spreads the small values:

(plot/render-image {:data (for [i (range 1 30)] {:x (* i i i) :y i})
                    :mapping {:x :x :y :y}
                    :charts [(chart/scatter)]
                    :scales {:x {:kind :log :formatter :power}}}
                   {:width 500 :height 300})

;; ### Coordinates

;; Cartesian (optionally flipped or with equal units) or polar (Scales chapter).

(nu/options-table (describe/top-level-rows (describe/options pipeline/plot-schema)) :coordinates)

(plot/render-image {:data [{:fruit "apples" :n 3} {:fruit "pears" :n 5} {:fruit "plums" :n 2}]
                    :mapping {:x :fruit :y :n}
                    :charts [(chart/bar)]
                    :scales {:x {:reverse true}}
                    :flip true}
                   {:width 500 :height 300})

;; The same bars in polar coordinates - a rose:

(plot/render-image {:data [{:fruit "apples" :n 3} {:fruit "pears" :n 5} {:fruit "plums" :n 2}]
                    :mapping {:x :fruit :y :n :color :fruit}
                    :charts [(chart/bar {:width 1.0})]
                    :coord {:kind :polar}
                    :legend false}
                   {:width 350 :height 350})

;; ### Layout

;; Panels split by data, several plots in a grid, charts beside the panel (Facets chapter).

(nu/options-table (describe/top-level-rows (describe/options pipeline/plot-schema)) :layout)

(plot/render-image {:data points
                    :mapping {:x :x :y :y :facet :g}
                    :charts [(chart/line) (chart/scatter)]}
                   {:width 500 :height 300})

;; A hand-written grid of plots and a note (`:custom-grid`, Facets chapter):

(plot/render-image {:custom-grid {:columns [{:size [:flexible 2]} {:size [:flexible 1]}]
                                  :rows [{:size [:flexible 1]} {:size [:measured]}]
                                  :items [{:id :lines :column 0 :row 0
                                           :content {:kind :plot :plot {:data points :mapping {:x :x :y :y}
                                                                        :charts [(chart/line)]}}}
                                          {:id :bars :column 1 :row 0
                                           :content {:kind :plot :plot {:data points :mapping {:x :g :y :y}
                                                                        :charts [(chart/bar {:stat :sum})]}}}
                                          {:id :note :column {:from 0 :to 1} :row 1
                                           :content {:kind :label :role :axis-title :text "two plots, 2 : 1"}}]}}
                   {:width 600 :height 320})

;; ### Titles

;; The plot title and the axis titles (Labels chapter).

(nu/options-table (describe/top-level-rows (describe/options pipeline/plot-schema)) :titles)

(plot/render-image {:data points
                    :mapping {:x :x :y :y}
                    :charts [(chart/line)]
                    :labels {:title "Four points" :x "time [s]" :y "height [m]"}}
                   {:width 500 :height 300})

;; ### Legend

;; Legends are drawn for mapped aesthetics; these keys hide them or draw only them (Aesthetics chapter).

(nu/options-table (describe/top-level-rows (describe/options pipeline/plot-schema)) :legend)

(plot/render-image {:data points
                    :mapping {:x :x :y :y :color :g}
                    :charts [(chart/scatter {:size 12})]
                    :legend false}
                   {:width 500 :height 300})

;; ### Style

;; The theme holds every style default; a plot overrides parts of it (Theme chapter).

(nu/options-table (describe/top-level-rows (describe/options pipeline/plot-schema)) :style)

(plot/render-image {:data points
                    :mapping {:x :x :y :y}
                    :charts [(chart/line) (chart/scatter)]
                    :theme {:plot {:background :white}
                            :panel {:fill [245 245 250] :border-color [180 180 180]}}}
                   {:width 500 :height 300})

;; ### Render options

;; Render options travel with the plot under `:options`; those given to `render-image` win (Render chapter).

(nu/options-table (describe/top-level-rows (describe/options pipeline/plot-schema)) :render)

(plot/render-image {:data points
                    :mapping {:x :x :y :y}
                    :charts [(chart/line)]
                    :options {:width 300 :height 200}})

;; ## The pipeline

;; `plot/build` validates the plot first and throws an error listing the problems (Errors chapter). Then the plot
;; passes through pure stages, each adding one key: `:split` (rows split per facet panel and group) → `:stats`
;; (each chart's computed data) → `:extents` → `:scale-specs` → `:grid` → `:layout` → `:finalized-scales` →
;; `:subplots`. `plot/render` draws the result.

;; ## The tables as data

;; The tables come from `cljplot2.describe`, usable at the REPL too: `describe/options` takes a chart value, a
;; builder or data function var or a schema; `describe/mapping` the mapping keys a chart reads; `describe/theme-rows`
;; the theme; `describe/fragment-rows` a shared group; `describe/api-vars` lists the user-facing data functions,
;; `describe/chart-constructors` the charts, `describe/attribute-values` the built-in shapes, dashes, patterns and
;; cells. A row is a plain map; `:section` names its table slice:

(first (describe/options pipeline/plot-schema))
