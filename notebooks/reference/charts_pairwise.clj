;; # Pairwise statistics

;; Charts that draw one statistic of two variables as a single glyph or cell filling the panel: the correlation
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

;; ## correlation

;; The correlation coefficient r of two variables drawn as one glyph filling the panel - a cell of a correlogram
;; (`plot/pairs`). Alone it writes r, one line per group.

(nu/options-table (describe/mapping (chart/correlation)))

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :petal-length :color :species}
                    :charts [(chart/correlation)]}
                   {:width 450 :height 300})

;; Its options: the coefficient, the glyph and its colours (by default r on a diverging scale):

(nu/options-table (describe/own-rows (describe/options (chart/correlation))))

;; Every glyph, for one pair of variables (r about 0.82):

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

;; The three coefficients of the same pair (Kendall's tau, counting concordant pairs, is the lowest), and a smaller
;; glyph in one fixed colour with a thick outline:

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

;; In a correlogram - a [`plot/pairs`](reference.facets_arrange.html#scatter-plot-matrix) matrix with correlation
;; glyphs below (`:lower`) and above (`:upper`) the diagonal, and the variables' names on it (`:diagonal :names`):

(plot/render-image (plot/pairs iris {:variables [:sepal-length :sepal-width :petal-length :petal-width]
                                     :lower (chart/correlation {:glyph :ellipse :stroke-color :none})
                                     :upper (chart/correlation {:glyph :square :method :spearman})
                                     :diagonal :names
                                     :title "ellipses (Pearson) below, squares (Spearman) above"})
                   {:width 700 :height 620})

(nu/shared-groups (describe/options (chart/correlation)))

;; ## pairwise

;; Any statistic of two variables drawn as one cell filling the panel: an error, an association, an effect size,
;; a distance (the keys of `cljplot2.data.pairwise/statistics`, listed with
;; [`pairwise/matrix-rows`](reference.data_sets_series.html#pairwisematrix-rows) in the Data functions part) or a function of the two samples - always of the x sample against the y sample. Unlike r the
;; values have no fixed range. Alone it writes the value, one line per group.

(nu/options-table (describe/mapping (chart/pairwise)))

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :petal-length :color :species}
                    :charts [(chart/pairwise {:method :cohens-d})]}
                   {:width 450 :height 300})

;; Its options: the statistic and its label, the glyph (by default `:number` - the text alone; a shade colours the
;; value on the colour scale over the panels' range, symmetric around 0 on a diverging palette for a signed
;; statistic), and the number format:

(nu/options-table (describe/own-rows (describe/options (chart/pairwise))))

;; The glyphs for one pair (sepal against petal length): the mean absolute error as text, shaded and both, the signed
;; mean error shaded on the diverging palette, a user function with its label and format, and one fixed colour (the
;; text over a shade is black or white by the fill):

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
;; its column variable against its row variable, so the mirrored cells of the asymmetric mean error have opposite
;; signs - shaded below, as text above, the names on the diagonal (`:diagonal :names`). The statistic is computed
;; once for the matrix: every shaded cell is coloured on its range (symmetric around 0 for the signed mean error) and
;; the one colour bar agrees; the cells share one text size:

(plot/render-image (plot/pairs iris {:variables [:sepal-length :sepal-width :petal-length :petal-width]
                                     :lower (chart/pairwise {:method :me :glyph :shade-number})
                                     :upper (chart/pairwise {:method :me})
                                     :diagonal :names
                                     :title "mean error: shaded below, text above"})
                   {:width 700 :height 620})

(nu/shared-groups (describe/options (chart/pairwise)))
