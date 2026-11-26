;; # Pairwise statistics

;; These charts draw one statistic of two variables as a single glyph or cell filling the panel: the correlation
;; (`correlation`) or any other pairwise statistic (`pairwise`). They're the cells of a scatter-plot matrix
;; ([`plot/pairs`](reference.facets_arrange.html#scatter-plot-matrix)).

(ns reference.charts-pairwise
  (:require [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [notebook-utils :as nu]))

(nu/source-note {:dataset :iris})

(def iris (nu/dataset :iris))

(nu/data-table iris)

;; ## Correlation cells

;; `chart/correlation` draws the correlation coefficient r of two variables as one glyph filling the panel. Use it as
;; the cell of a correlogram (`plot/pairs`); by default it writes r, a line per group.

(nu/shared-groups (describe/options (chart/correlation)))

(nu/mapping-list (describe/mapping (chart/correlation)))

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :petal-length :color :species}
                    :charts [(chart/correlation)]}
                   {:width 450 :height 300})

;; Its options pick the coefficient and how the chart draws it:

(nu/option-cards 'chart/correlation (describe/own-rows (describe/options (chart/correlation))))

;; Each of the six glyphs shows the same pair of variables (r about 0.82):

(plot/render-image (plot/arrange [{:data iris :mapping {:x :sepal-length :y :petal-width}
                                   :charts [(chart/correlation {:glyph :number})] :labels {:title ":glyph :number"}}
                                  {:data iris :mapping {:x :sepal-length :y :petal-width}
                                   :charts [(chart/correlation {:glyph :shade})] :labels {:title ":glyph :shade"}}
                                  {:data iris :mapping {:x :sepal-length :y :petal-width}
                                   :charts [(chart/correlation {:glyph :square})] :labels {:title ":glyph :square"}}
                                  {:data iris :mapping {:x :sepal-length :y :petal-width}
                                   :charts [(chart/correlation {:glyph :circle})] :labels {:title ":glyph :circle"}}
                                  {:data iris :mapping {:x :sepal-length :y :petal-width}
                                   :charts [(chart/correlation {:glyph :ellipse})] :labels {:title ":glyph :ellipse"}}
                                  {:data iris :mapping {:x :sepal-length :y :petal-width}
                                   :charts [(chart/correlation {:glyph :pie})] :labels {:title ":glyph :pie"}}]
                                 {:columns 3})
                   {:width 900 :height 560})

;; The three coefficients differ for the same pair (Kendall's tau, which compares concordant with discordant pairs,
;; is the lowest). On the right a smaller glyph has one fixed colour and a thick outline:

(plot/render-image (plot/arrange [{:data iris :mapping {:x :sepal-length :y :petal-width}
                                   :charts [(chart/correlation {:method :pearson})] :labels {:title ":method :pearson"}}
                                  {:data iris :mapping {:x :sepal-length :y :petal-width}
                                   :charts [(chart/correlation {:method :spearman})] :labels {:title ":method :spearman"}}
                                  {:data iris :mapping {:x :sepal-length :y :petal-width}
                                   :charts [(chart/correlation {:method :kendall})] :labels {:title ":method :kendall"}}
                                  {:data iris :mapping {:x :sepal-length :y :petal-width}
                                   :charts [(chart/correlation {:glyph :circle :glyph-size 0.6 :color [120 160 90]
                                                                :stroke-color :black :stroke-width 3})]
                                   :labels {:title ":glyph-size 0.6"}}]
                                 {:columns 4})
                   {:width 1000 :height 280})

;; A correlogram is a [`plot/pairs`](reference.facets_arrange.html#scatter-plot-matrix) matrix of these cells: here
;; ellipses below the diagonal (`:lower`), Spearman squares above it (`:upper`) and the variables' names on it
;; (`:diagonal :names`):

(plot/render-image (plot/pairs iris {:variables [:sepal-length :sepal-width :petal-length :petal-width]
                                     :lower (chart/correlation {:glyph :ellipse :stroke-color :none})
                                     :upper (chart/correlation {:glyph :square :method :spearman})
                                     :diagonal :names
                                     :title "ellipses (Pearson) below, squares (Spearman) above"})
                   {:width 700 :height 620})

;; ## Statistic cells

;; `chart/pairwise` draws a statistic of two variables as one cell filling the panel: an error, an association, an
;; effect size, a distance or a function of the two samples, always of the x sample against the y sample. Use it as
;; the cell of a `plot/pairs` matrix; by default it writes the value, a line per group. Unlike r the values have no fixed
;; range. The [`pairwise/matrix-rows`](reference.data_sets_series.html#pairwisematrix-rows) section lists the built-in
;; statistics.

(nu/shared-groups (describe/options (chart/pairwise)))

(nu/mapping-list (describe/mapping (chart/pairwise)))

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :petal-length :color :species}
                    :charts [(chart/pairwise {:method :cohens-d})]}
                   {:width 450 :height 300})

;; Its options pick the statistic and how the chart draws and writes it. The chart colours a shade by its value on a
;; colour scale over the panels' values; a signed statistic takes a diverging palette centred on 0:

(nu/option-cards 'chart/pairwise (describe/own-rows (describe/options (chart/pairwise))))

;; For one pair (sepal against petal length) the first three panels show the mean absolute error as text, as a
;; shade and as both. The last three show the signed mean error on the diverging palette, a user function with its
;; label and format, and one fixed colour:

(plot/render-image (plot/arrange [{:data iris :mapping {:x :sepal-length :y :petal-length}
                                   :charts [(chart/pairwise {:method :mae :glyph :number})] :labels {:title ":number"}}
                                  {:data iris :mapping {:x :sepal-length :y :petal-length}
                                   :charts [(chart/pairwise {:method :mae :glyph :shade})] :labels {:title ":shade"}}
                                  {:data iris :mapping {:x :sepal-length :y :petal-length}
                                   :charts [(chart/pairwise {:method :mae :glyph :shade-number})] :labels {:title ":shade-number"}}
                                  {:data iris :mapping {:x :sepal-length :y :petal-length}
                                   :charts [(chart/pairwise {:method :me :glyph :shade-number})] :labels {:title "signed :me"}}
                                  {:data iris :mapping {:x :sepal-length :y :petal-length}
                                   :charts [(chart/pairwise {:method (fn [xs ys] (- (apply max xs) (apply max ys)))
                                                             :label "max diff" :value-format #(format "%+.1f cm" %)})]
                                   :labels {:title "function, :value-format"}}
                                  {:data iris :mapping {:x :sepal-length :y :petal-length}
                                   :charts [(chart/pairwise {:method :rmse :glyph :shade-number :color [120 160 90]})]
                                   :labels {:title ":color"}}]
                                 {:columns 3})
                   {:width 1000 :height 520})

;; In a [`plot/pairs`](reference.facets_arrange.html#scatter-plot-matrix) matrix every cell shows the statistic of
;; its column variable against its row variable, so the mirrored cells of the antisymmetric mean error have opposite
;; signs - shaded below, as text above, the names on the diagonal (`:diagonal :names`). `plot/pairs` computes the
;; statistic once for the matrix, so every shaded cell and the single colour bar use its range; the cells share one
;; text size:

(plot/render-image (plot/pairs iris {:variables [:sepal-length :sepal-width :petal-length :petal-width]
                                     :lower (chart/pairwise {:method :me :glyph :shade-number})
                                     :upper (chart/pairwise {:method :me})
                                     :diagonal :names
                                     :title "mean error: shaded below, text above"})
                   {:width 700 :height 620})
