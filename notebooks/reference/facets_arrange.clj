;; # Facets, multiplots and margins

;; One plot can split into panels by data (facets), several plots can share one image (`:arrange`, `plot/arrange`,
;; `plot/pairs`, `:custom-grid`), and a panel can carry charts beside it (`:margins`).

(ns reference.facets-arrange
  (:require [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.facet :as facet]
            [cljplot2.describe :as describe]
            [cljplot2.layout.plot-grid :as plot-grid]
            [cljplot2.pipeline :as pipeline]
            [notebook-utils :as nu]))

;; ## Facets

;; Mapping `:facet` wraps one panel per level into rows and columns; `:facet-col` / `:facet-row` make a grid. The
;; plot's `:facets` map sets the layout; panels share scales unless `:scales` frees them. The level order and strip
;; text come from the facet scales (`:scales {:facet spec}`, Scales chapter).

(nu/options-table (describe/options facet/facet-options))

(nu/source-note {:dataset :mtcars})

(def mtcars (nu/dataset :mtcars))

(plot/render-image {:data mtcars
                    :mapping {:x :wt :y :mpg :facet-col :cyl :facet-row :am}
                    :charts [(chart/scatter)]
                    :facets {:scales {:x :free}}
                    :scales {:facet-row {:formatter #(if (= 1 %) "manual" "automatic")}}}
                   {:width 800 :height 500})

;; ## Multiplots

;; `plot/arrange` puts plots in a grid; the panels align across rows and columns. Its options:

(nu/options-table (describe/options #'plot/arrange))

;; It returns a plot with `:arrange`, which can also be written by hand:

(nu/options-table (describe/options plot-grid/arrange-schema))

(plot/render-image (plot/arrange [{:data mtcars :mapping {:x :hp :y :mpg} :charts [(chart/scatter)]}
                                  {:data mtcars :mapping {:x :mpg} :charts [(chart/histogram {:bins 10})]}
                                  {:data mtcars :mapping {:x #(str (:cyl %) " cyl") :y :mpg} :charts [(chart/box)]}]
                                 {:columns 3 :widths [2 1 1] :title "mtcars"})
                   {:width 900 :height 350})

;; ## Scatter-plot matrix

;; `plot/pairs` draws a cell for every pair of `:variables` (2 to 20 numeric columns); the cells below, above and on
;; the diagonal take any charts (`:lower`, `:upper`, `:diagonal`).

(nu/options-table (describe/options #'plot/pairs))

(plot/render-image (plot/pairs mtcars {:variables [:mpg :hp :wt :qsec]})
                   {:width 700 :height 700})

;; ## Margins

;; `:margins` puts charts above (`:top`) and right of (`:right`) a single panel, sharing its horizontal / vertical
;; scale; margin charts read the plot's data with `:x` mapped to the shared dimension.

(nu/options-table (describe/options pipeline/margins-schema))

;; A margin given as a map:

(nu/options-table (describe/options pipeline/margin-spec-schema))

(plot/render-image {:data mtcars
                    :mapping {:x :wt :y :mpg}
                    :charts [(chart/scatter)]
                    :margins {:top (chart/density) :right {:charts (chart/histogram {:bins 10}) :size 60 :axis true}}}
                   {:width 600 :height 500})

;; ## Custom grid

;; `:custom-grid` replaces the generated layout with a hand-written track grid (see the `cljplot2.layout.grid`
;; namespace doc): columns and rows of `[:fixed px]`, `[:fraction f]`, `[:measured]` or `[:flexible w]` tracks and
;; items placed on them; an item's content `{:kind :plot :plot p}` holds a whole plot. Only `:labels {:title t}` may
;; accompany it; for plots in a regular grid prefer `plot/arrange`.
