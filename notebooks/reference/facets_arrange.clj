;; # Facets, multiplots and margins

;; One plot can split into panels by data (facets), several plots can share one image (`plot/arrange`,
;; `plot/pairs`, `:custom-grid`), and a panel can carry charts beside it (`:margins`).

(ns reference.facets-arrange
  (:require [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.facet :as facet]
            [cljplot2.describe :as describe]
            [cljplot2.layout.plot-grid :as plot-grid]
            [cljplot2.pipeline :as pipeline]
            [notebook-utils :as nu]))

(nu/source-note {:dataset :mtcars})

(def mtcars (nu/dataset :mtcars))

(nu/data-table mtcars)

;; ## Facet shape

;; Mapping `:facet` wraps one panel per level into rows and columns; `:facet-col` / `:facet-row` make a grid of
;; them. The plot's `:facets` map shapes the wrap and weights the panel columns and rows.

(nu/options-table (describe/options facet/facet-options) :shape)

(plot/render-image {:data mtcars
                    :mapping {:x :wt :y :mpg :facet :carb}
                    :charts [(chart/scatter)]
                    :facets {:columns 4 :order :bottom-up}}
                   {:width 800 :height 400})

;; ## Facet panels

;; Panels share their position scales unless freed per axis (`:free` - each panel its data; `:sliced` - each
;; panel its centre, the widest span); `nil` facet values get a panel of their own; strips can go.

(nu/options-table (describe/options facet/facet-options) :panels)

(plot/render-image {:data mtcars
                    :mapping {:x :wt :y :mpg :facet-col :cyl :facet-row :am}
                    :charts [(chart/scatter)]
                    :facets {:scales {:x :free} :widths [1 1 2]}}
                   {:width 800 :height 450})

;; `:sliced`: every panel spans as much as the widest one, around its own data - the panels' y ranges are
;; comparable in size although they sit at different heights:

(plot/render-image {:data mtcars
                    :mapping {:x :wt :y :mpg :facet :cyl}
                    :charts [(chart/scatter)]
                    :facets {:scales {:y :sliced} :columns 3}}
                   {:width 800 :height 300})

;; ## Facet levels

;; The facet levels have scales: `:scales {:facet spec}` (and `:facet-col` / `:facet-row`) order the panels and
;; write the strip texts.

(nu/options-table (describe/options facet/scale-spec-schema))

(plot/render-image {:data mtcars
                    :mapping {:x :wt :y :mpg :facet :gear}
                    :charts [(chart/scatter)]
                    :scales {:facet {:domain [5 4 3] :formatter #(str % " gears")}}}
                   {:width 800 :height 300})

;; ## Arrange

;; `plot/arrange` puts plots in a grid; the panels align across rows and columns. Its options:

(nu/options-table (describe/options #'plot/arrange))

(plot/render-image (plot/arrange [{:data mtcars :mapping {:x :hp :y :mpg} :charts [(chart/scatter)]}
                                  {:data mtcars :mapping {:x :mpg} :charts [(chart/histogram {:bins 10})]}
                                  {:data mtcars :mapping {:x #(str (:cyl %) " cyl") :y :mpg} :charts [(chart/box)]}]
                                 {:columns 3 :widths [2 1 1] :title "mtcars"})
                   {:width 900 :height 350})

;; A cell too small for its sub-plot's axes, labels, legends and margins is an error naming the cell (`[:plot i]`,
;; row by row; a nested cell by its whole path, outer cells first), its title - else its mapped `:x` / `:y` - its
;; size and the pixels needed, keyed `:width` or `:height`. Eight plots side by side in 400 pixels:

(try (plot/build (plot/arrange (repeat 8 {:data mtcars :mapping {:x :hp :y :mpg} :charts [(chart/scatter)]})
                               {:columns 8})
                 {:width 400 :height 200})
     (catch clojure.lang.ExceptionInfo e
       {:message (ex-message e) :data (ex-data e)}))

;; It returns a plot whose `:arrange` key holds the plots and the layout - also written by hand. Here a legend
;; shown once, in its own cell: the two plots hide their legends (`:legend false`), a third plot draws only its
;; legend (`:legend-only true` - no panel or axes, plot keys in the intro's Legend slice) in a column sized to fit
;; (`:measured`):

(nu/options-table (describe/options plot-grid/arrange-schema))

(plot/render-image {:arrange {:plots [{:data mtcars :mapping {:x :hp :y :mpg :color :gear} :charts [(chart/scatter)]
                                       :scales {:color {:type :categorical}} :legend false}
                                      {:data mtcars :mapping {:x :wt :y :mpg :color :gear} :charts [(chart/scatter)]
                                       :scales {:color {:type :categorical}} :legend false}
                                      {:data mtcars :mapping {:x :wt :y :mpg :color :gear} :charts [(chart/scatter)]
                                       :scales {:color {:type :categorical}} :legend-only true}]
                              :columns 3 :widths [1 1 :measured]}}
                   {:width 900 :height 320})

;; `:row-spans` lets a plot cover several rows from its own down (`{plot-index rows}` - keyed by the plot's index,
;; not its cell), and later plots skip the covered cells; without `:rows` a span reaching past the last row adds
;; rows. Here one legend beside two rows of plots, its colour bar stretched over both:

(plot/render-image (plot/arrange [{:data mtcars :mapping {:x :hp :y :mpg :color :wt} :charts [(chart/scatter)] :legend false}
                                  {:data mtcars :mapping {:x :hp :y :mpg :color :wt} :charts [(chart/scatter)] :legend-only true}
                                  {:data mtcars :mapping {:x :disp :y :mpg :color :wt} :charts [(chart/scatter)] :legend false}]
                                 {:columns 2 :widths [1 :measured] :row-spans {1 2}})
                   {:width 600 :height 500})

;; ## Scatter-plot matrix

;; `plot/pairs` draws a cell for every pair of `:variables` (2 to 20 numeric columns); the cells below, above and on
;; the diagonal take any charts. Its variables:

(nu/options-table (describe/options #'plot/pairs) :variables)

(plot/render-image (plot/pairs mtcars {:variables [:mpg :hp :wt :qsec]
                                       :upper [(chart/correlation)]})
                   {:width 700 :height 700})

;; `:row-variables` and `:column-variables` (each defaulting to `:variables`) put other variables, or another order,
;; on the rows than on the columns - here 3 rows x 2 columns. Such a matrix has no triangles: every cell of two
;; different variables draws `:lower`, a cell whose row and column variable are the same draws `:diagonal`
;; (wherever it is), and `:upper` is an error.

(plot/render-image (plot/pairs mtcars {:row-variables [:hp :wt :mpg]
                                       :column-variables [:mpg :disp]
                                       :lower [(chart/scatter) (chart/smooth {:method :lm})]
                                       :mapping {:color #(str (:cyl %) " cyl")}})
                   {:width 600 :height 700})

;; Its cells:

(nu/options-table (describe/options #'plot/pairs) :cells)

(plot/render-image (plot/pairs mtcars {:variables [:mpg :hp :wt]
                                       :lower (chart/scatter {:alpha 0.6})
                                       :upper (chart/correlation {:glyph :shade})
                                       :diagonal :names
                                       :scales {:color {:domain [-1 1]}}})
                   {:width 500 :height 450})

;; The matrix's title, legend column and theme:

(nu/options-table (describe/options #'plot/pairs) :layout)

;; Only the outer cells have axes (x under the bottom row, y left of the left column), and every panel gets one size
;; (the room between rows follows each row's end tick labels), so a matrix of many variables fits a page - up to 20,
;; though at that count tick labels get dense: give such a matrix a large image. Its rows are low, so a legend taller
;; than a row goes beside the whole matrix with `:legend-span :matrix` (the default `:row` keeps it beside the first
;; row) - here every numeric column of mtcars, with the r colour bar of the shaded correlations as tall as the matrix.

(plot/render-image (plot/pairs mtcars {:variables [:mpg :cyl :disp :hp :drat :wt :qsec :vs :am :gear :carb]
                                       :upper (chart/correlation {:glyph :shade})
                                       :diagonal :names
                                       :legend-span :matrix
                                       :title "mtcars"})
                   {:width 1000 :height 800})

;; ## Margins

;; `:margins` puts charts above (`:top`), right of (`:right`), below (`:bottom`) and left of (`:left`) a single panel,
;; sharing its horizontal / vertical scale; margin charts read the plot's data with `:x` mapped to the shared
;; dimension. A margin is a chart or a vector of charts ...

(nu/options-table (describe/options pipeline/margins-schema))

(plot/render-image {:data mtcars
                    :mapping {:x :wt :y :mpg}
                    :charts [(chart/scatter)]
                    :margins {:top [(chart/density) (chart/rug)]}}
                   {:width 500 :height 400})

;; ... or a map giving the margin's thickness and the magnitude axis of histogram and density margins:

(nu/options-table (describe/options pipeline/margin-spec-schema))

(plot/render-image {:data mtcars
                    :mapping {:x :wt :y :mpg}
                    :charts [(chart/scatter)]
                    :margins {:top (chart/density) :right {:charts (chart/histogram {:bins 10}) :size 60 :axis true}}}
                   {:width 600 :height 500})

;; Bottom and left margins sit between the panel and its axes - the panel keeps its axes on the outer side - and
;; their magnitudes grow away from the panel (bars hang down, densities grow to the left). A margin's magnitude axis
;; stays next to its own margin panel - with a left margin too, the bottom margin's axis sits between the left margin
;; and the panel:

(plot/render-image {:data mtcars
                    :mapping {:x :wt :y :mpg}
                    :charts [(chart/scatter)]
                    :margins {:bottom {:charts (chart/histogram {:bins 10}) :size 60 :axis true}
                              :left [(chart/density) (chart/rug)]}}
                   {:width 600 :height 500})

;; ## Custom grid

;; `:custom-grid` replaces the generated layout with a hand-written track grid (see the `cljplot2.layout.grid`
;; namespace doc): columns and rows of `[:fixed px]`, `[:fraction f]`, `[:measured]` or `[:flexible w]` tracks and
;; items placed on them - labels (`{:kind :label :text t :role r}`, the role required), or whole plots
;; (`{:kind :plot :plot p}`). Only `:labels {:title t}` may accompany it; plots in a regular grid are simpler with
;; `plot/arrange`.

(plot/render-image {:custom-grid {:columns [{:size [:flexible 2]} {:size [:flexible 1]}]
                                  :rows [{:size [:flexible 1]} {:size [:measured]}]
                                  :items [{:id :main :column 0 :row 0
                                           :content {:kind :plot
                                                     :plot {:data mtcars :mapping {:x :wt :y :mpg}
                                                            :charts [(chart/scatter)]}}}
                                          {:id :side :column 1 :row 0
                                           :content {:kind :plot
                                                     :plot {:data mtcars :mapping {:x :mpg}
                                                            :charts [(chart/density)]}}}
                                          {:id :note :column {:from 0 :to 1} :row 1
                                           :content {:kind :label :role :axis-title
                                                     :text "a hand-written grid: 2 : 1 columns, a note row"}}]}
                    :labels {:title "Custom grid"}}
                   {:width 800 :height 400})
