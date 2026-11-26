;; # Sets and series

;; These functions turn set memberships, time series and variable pairs into rows: the bars and dot matrix of an
;; UpSet plot, lag pairs, autocorrelations, the folded bands of a horizon chart and the matrix of a pairwise
;; statistic. Use them to draw these plots with ordinary charts. The [series builders](reference.builders_series.html)
;; `plot/acf` and `plot/horizon` draw autocorrelations and horizons directly; `chart/pairwise` draws one statistic per
;; panel.

(ns reference.data-sets-series
  (:require [fastmath.core :as m]
            [fastmath.random :as r]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.pairwise :as pairwise]
            [cljplot2.data.series :as series]
            [cljplot2.data.sets :as sets]
            [cljplot2.describe :as describe]
            [notebook-utils :as nu]))

;; The table below holds made-up set memberships, a row per item and set it belongs to (an item may be in several);
;; the UpSet sections use it:

(def members (concat (for [i (range 40)] {:set "coffee" :item i})
                     (for [i (range 25 60)] {:set "tea" :item i})
                     (for [i (range 30 45)] {:set "juice" :item i})
                     (for [i (range 55 70)] {:set "juice" :item i})))

(nu/data-table members)

;; The values below form an autoregressive series: fastmath `r/ar` draws 200 values with coefficient 0.6 over a weekly
;; sine plus seeded white noise. The lag and autocorrelation sections use them:

(def weekly (vec (take 200 (r/ar 0.6 (map (fn [t e] (+ (* 2.0 (m/sin (* m/TWO_PI (/ (double t) 7.0)))) (double e)))
                                           (range) (r/white-noise (r/rng :mersenne 8) 1.0))))))

(nu/data-table weekly)

;; The table below holds made-up temperature anomalies of two cities; the horizon sections use it:

(def temperatures (for [city ["Oslo" "Rome"] day (range 60)]
                    {:city city :day day
                     :anomaly (* (if (= city "Oslo") 4.0 2.5) (m/sin (+ (* 0.12 day) (if (= city "Oslo") 0.0 1.5))))}))

(nu/data-table temperatures)

;; The table below holds the iris flowers; the statistic matrix section uses it:

(nu/source-note {:dataset :iris})

(def iris (nu/dataset :iris))

(nu/data-table iris)

;; ## UpSet bars

;; `sets/upset-rows` returns the bars of an UpSet plot: every combination of sets that holds items of its own, with its
;; sets, size and degree, the largest first. Use it with `chart/bar`, a bar per combination.

(nu/argument-list #'sets/upset-rows)

;; The table below holds the bars:

(nu/result-table (sets/upset-rows members {:set :set :item :item}))

;; ## UpSet membership

;; `sets/membership-rows` returns the dot matrix under the bars: a row per combination and set, `:member` true where the
;; set is in the combination. Use it with `chart/scatter` under the bars.

(nu/argument-list #'sets/membership-rows)

;; The table below holds the matrix:

(nu/result-table (sets/membership-rows members {:set :set :item :item}))

;; The plot below stacks both parts into an UpSet plot: the bars, then the members' dots over grey dots in every cell.
;; It chooses the sets and puts tea first:

(plot/render-image (plot/arrange [{:data (sets/upset-rows members {:set :set :item :item} ["tea" "coffee" "juice"])
                                   :mapping {:x :combination :y :size}
                                   :charts [(chart/bar {:value-label true})]
                                   :labels {:x nil :y "items"}}
                                  {:data (sets/membership-rows members {:set :set :item :item} ["tea" "coffee" "juice"])
                                   :mapping {:x :combination :y :set}
                                   :charts [(chart/scatter {:size 12 :color [210 210 210]})
                                            (chart/scatter {:data (filter :member (sets/membership-rows members {:set :set :item :item}
                                                                                                         ["tea" "coffee" "juice"]))
                                                            :size 12 :color :black})]
                                   :scales {:y {:reverse true}}
                                   :labels {:x nil :y nil}}]
                                 {:columns 1 :heights [2 1]})
                   {:width 900 :height 450})

;; ## Lag pairs

;; `series/lag-rows` pairs every value `y(t)` with `y(t + k)`. Use it for a lag plot, which shows the correlation at lag
;; k.

(nu/argument-list #'series/lag-rows)

;; The table below pairs the values at lag 7; the plots below show the weekly cycle, strong at lag 7 and negative at lag
;; 3:

(nu/result-table (series/lag-rows weekly 7))

(plot/render-image (plot/arrange [{:data (series/lag-rows weekly 3)
                                   :mapping {:x :x :y :y}
                                   :charts [(chart/scatter {:size 5})]
                                   :labels {:title "lag 3" :x "y(t)" :y "y(t+3)"}}
                                  {:data (series/lag-rows weekly 7)
                                   :mapping {:x :x :y :y}
                                   :charts [(chart/scatter {:size 5})]
                                   :labels {:title "lag 7" :x "y(t)" :y "y(t+7)"}}]
                                 {:columns 2})
                   {:width 800 :height 380})

;; ## Autocorrelation

;; `series/acf-rows` returns the autocorrelation function of a series: a row per lag with its value, the
;; significance band and whether the value lies outside it. Use it to draw a correlogram of your own.

(nu/argument-list #'series/acf-rows)

;; These options set the lags, the band's confidence level and the partial autocorrelation. The table below holds
;; lags 0 to 10:

(nu/option-cards '(series/acf-rows values) (describe/options #'series/acf-rows))

(nu/result-table (series/acf-rows weekly {:lags 10}))

;; The plots below draw the rows by hand: segments to 0, the band as a ribbon, the significant lags in red. The left
;; panel draws the autocorrelation up to lag 21 with the default 95% band, the right one the partial autocorrelation
;; with a 99% band:

(plot/render-image (plot/arrange [{:data (series/acf-rows weekly {:lags 21 :level 0.95})
                                   :mapping {:x :lag :y :value :ymin :lo :ymax :hi}
                                   :charts [(chart/ribbon {:color [0 114 178] :alpha 0.15})
                                            (chart/segment {:yend 0 :line-width 2})
                                            (chart/scatter {:mapping {:color #(if (:significant %) "yes" "no")}})]
                                   :scales {:color {:domain ["no" "yes"] :range [[90 90 90] [200 40 40]]
                                                    :title "significant"}}
                                   :labels {:title "ACF, 95% band"}
                                   :legend false}
                                  {:data (series/acf-rows weekly {:lags 21 :level 0.99 :partial true})
                                   :mapping {:x :lag :y :value :ymin :lo :ymax :hi}
                                   :charts [(chart/ribbon {:color [0 114 178] :alpha 0.15})
                                            (chart/segment {:yend 0 :line-width 2})
                                            (chart/scatter {:mapping {:color #(if (:significant %) "yes" "no")}})]
                                   :scales {:color {:domain ["no" "yes"] :range [[90 90 90] [200 40 40]]
                                                    :title "significant"}}
                                   :labels {:title "PACF, :level 0.99"}}]
                                 {:columns 2 :widths [1 1.3]})
                   {:width 1000 :height 340})

;; ## Horizon bands

;; `series/horizon-rows` returns the bands of a horizon chart: it folds the values into `:bands` bands of one height and
;; mirrors negative values upward as negative band numbers. Use it to draw a horizon chart of your own.

(nu/argument-list #'series/horizon-rows)

;; These options name the row keys, set the bands and resample the curve. The table below holds two bands per sign for
;; each city; the function adds a point where the curve crosses a band edge:

(nu/option-cards '(series/horizon-rows rows) (describe/options #'series/horizon-rows))

(nu/result-table (:rows (series/horizon-rows temperatures {:x :day :y :anomaly :series :city :bands 2})))

;; The plots below draw the bands as filled lines. The left panel takes the default band height; the right one draws
;; three bands of height 1 on a monotone curve, resampled 4 points per gap. `horizon-bands` orders the colour domain:

(plot/render-image (plot/arrange [{:data (:rows (series/horizon-rows temperatures {:x :day :y :anomaly :series :city}))
                                   :mapping {:x :x :y :y :color :band :facet :series}
                                   :charts [(chart/line {:fill :color :alpha 1.0 :line-width 0})]
                                   :scales {:color {:kind :ordinal :domain (series/horizon-bands 2)
                                                    :range [[158 202 225] [8 69 148] [252 174 145] [165 15 21]]}}
                                   :facets {:columns 1}
                                   :labels {:title "2 bands" :y nil}
                                   :legend false}
                                  {:data (:rows (series/horizon-rows temperatures {:x :day :y :anomaly :series :city :bands 3
                                                                                   :band-height 1.0 :interpolator :monotone
                                                                                   :steps-per-gap 4}))
                                   :mapping {:x :x :y :y :color :band :facet :series}
                                   :charts [(chart/line {:fill :color :alpha 1.0 :line-width 0})]
                                   :scales {:color {:kind :ordinal :domain (series/horizon-bands 3)
                                                    :range [[198 219 239] [107 174 214] [8 81 156]
                                                            [252 187 161] [251 106 74] [165 15 21]]}}
                                   :facets {:columns 1}
                                   :labels {:title "3 bands of height 1, :monotone" :y nil}}]
                                 {:columns 2 :widths [1 1.2]})
                   {:width 1000 :height 360})

;; ## Horizon band labels

;; `series/horizon-bands` returns the band labels of a horizon of n bands in drawing order: the positive bands
;; first, then the mirrored negative ones. Use it as the colour domain, as above.

(nu/argument-list #'series/horizon-bands)

;; The call below returns the labels of 3 bands per sign:

(series/horizon-bands 3)

;; ## Statistic matrices

;; `pairwise/matrix-rows` computes a statistic for every pair of variables, a row per cell: the statistic of the x
;; variable's sample against the y variable's. Use it for a matrix of correlations, distances or effect sizes drawn as
;; tiles.

(nu/argument-list #'pairwise/matrix-rows)

;; The table below lists the built-in statistics by family. `:symmetry` tells how a value changes when the samples
;; swap: `:symmetric` keeps it, `:antisymmetric` negates it, `:reciprocal` inverts it, `:complementary` turns it into
;; `1 - v`, and `:asymmetric` changes it otherwise. The `:samples` column says how a statistic reads the data: paired
;; statistics take the rows where both values are present; independent-sample ones, which compare two groups, take each
;; variable's own values. For a `:signed` statistic zero is the neutral value, and a shaded `pairwise` chart centres its
;; colours on it:

(nu/data-table (map (fn [[k {:keys [label family symmetry samples signed description]}]]
                      {:family family :method k :label label :symmetry symmetry :samples samples
                       :signed (true? signed) :description description})
                    (sort-by (fn [[k {:keys [family]}]] [({:error 0 :association 1 :effect-size 2 :distance 3} family) (name k)])
                             pairwise/statistics)))

;; These options choose the variables, the statistic and the cells. The table below holds the lower triangle of a
;; symmetric statistic with the diagonal, as `plot/pairs` lays it out:

(nu/option-cards '(pairwise/matrix-rows rows) (describe/options #'pairwise/matrix-rows))

(nu/result-table (pairwise/matrix-rows iris {:variables [:sepal-length :sepal-width :petal-length :petal-width]
                                             :method :spearman :triangle :lower :self-pairs true}))

;; The table below holds the upper triangle of the mean squared error of two variables, a single row:

(nu/result-table (pairwise/matrix-rows iris {:variables [:sepal-length :petal-length] :method :mse :triangle :upper}))

;; [`chart/tile`](reference.charts_bins_fields.html#heatmaps) draws a standalone matrix, a tile per row, and its
;; `:value-label` writes the values; [`plot/arrange`](reference.facets_arrange.html#arrange) puts two side by side
;; below. Tiles order the
;; categories as they first appear and y grows upward, so the scales give both axes the variable order, the first
;; variable on top as in `plot/pairs`. The left panel holds Cohen's d of every ordered pair with the diagonal, a signed
;; statistic on a diverging palette centred on 0. The right one holds a rectangular matrix of sepal against petal
;; measurements with a function of your own and its label:

(plot/render-image (plot/arrange [{:data (pairwise/matrix-rows iris {:variables [:sepal-length :sepal-width :petal-length :petal-width]
                                                                     :method :cohens-d :self-pairs true})
                                   :mapping {:x (comp name :x) :y (comp name :y) :color :value}
                                   :charts [(chart/tile {:value-label true})]
                                   :scales {:x {:domain ["sepal-length" "sepal-width" "petal-length" "petal-width"]}
                                            :y {:domain ["petal-width" "petal-length" "sepal-width" "sepal-length"]}
                                            :color {:range :RColorBrewer/RdBu :domain [-6 6] :title "Cohen's d"}}}
                                  {:data (pairwise/matrix-rows iris {:x-variables [:sepal-length :sepal-width]
                                                                     :y-variables [:petal-length :petal-width]
                                                                     :method (fn [xs ys] (/ (reduce + xs) (reduce + ys)))
                                                                     :label "sum ratio"})
                                   :mapping {:x (comp name :x) :y (comp name :y) :color :value}
                                   :charts [(chart/tile {:value-label true})]
                                   :scales {:color {:title "sum ratio"}}}]
                                 {:columns 2})
                   {:width 1000 :height 400})

