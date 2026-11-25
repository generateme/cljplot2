;; # Sets and series

;; Functions turning set memberships and time series into rows for ordinary plots: the bars and dot matrix of an
;; UpSet plot, lag pairs, autocorrelation rows, the folded bands of a horizon chart and the matrix of a pairwise
;; statistic. The [series builders](reference.builders_series.html) `plot/acf` and `plot/horizon` draw
;; autocorrelations and horizons directly; `chart/pairwise` draws one statistic per panel.

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

;; Set memberships: one row per item and set it belongs to (an item may be in several).

(def members (concat (for [i (range 40)] {:set "coffee" :item i})
                     (for [i (range 25 60)] {:set "tea" :item i})
                     (for [i (range 30 45)] {:set "juice" :item i})
                     (for [i (range 55 70)] {:set "juice" :item i})))

(nu/data-table members)

;; An autoregressive series (fastmath `r/ar`, coefficient 0.6) over a weekly sine plus seeded white noise:

(def weekly (vec (take 200 (r/ar 0.6 (map (fn [t e] (+ (* 2.0 (m/sin (* m/TWO_PI (/ (double t) 7.0)))) (double e)))
                                           (range) (r/white-noise (r/rng :mersenne 8) 1.0))))))

(nu/data-table weekly)

;; ## sets/upset-rows

;; The bars of an UpSet plot: every combination of sets that holds items exclusively - its sets, size and degree -
;; by size descending. The mapping names the set and item keys; a third argument chooses (and orders) the sets.

(nu/data-table (sets/upset-rows members {:set :set :item :item}))

;; ## sets/membership-rows

;; The dot matrix under the bars: a row per combination and set, `:member` true where the set is in the combination.

(nu/data-table (sets/membership-rows members {:set :set :item :item}))

;; Together, an UpSet plot of two stacked plots - the bars, then the dots of the members over grey dots of every
;; cell - here with the sets chosen and ordered (tea first):

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

;; ## series/lag-rows

;; Pairs every value `y(t)` with `y(t + k)` - a lag plot shows the correlation at lag k (here the weekly cycle:
;; strong at 7, negative at 3):

(nu/data-table (series/lag-rows weekly 7))

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

;; ## series/acf-rows

;; The autocorrelation function: a row per lag with its value, the significance band and whether the value is
;; outside it; `:partial true` - the partial autocorrelation.

(nu/options-table (describe/options #'series/acf-rows))

(nu/data-table (series/acf-rows weekly {:lags 10}))

;; Drawn by hand from the rows - segments to 0, the band as a ribbon, significant lags in red: the autocorrelation
;; up to lag 21 at the default 95% band (left) and the partial one with a 99% band (right):

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

;; ## series/horizon-rows

;; The bands of a horizon chart: values folded into `:bands` bands of one height, negatives mirrored upward as
;; negative band numbers; returns `{:rows [...] :band-height h}`.

(nu/options-table (describe/options #'series/horizon-rows))

(def temperatures (for [city ["Oslo" "Rome"] day (range 60)]
                    {:city city :day day
                     :anomaly (* (if (= city "Oslo") 4.0 2.5) (m/sin (+ (* 0.12 day) (if (= city "Oslo") 0.0 1.5))))}))

(nu/data-table temperatures)

;; Two bands per sign for each city (the rows of the bands, where the curve crosses a band edge points are added):

(nu/data-table (:rows (series/horizon-rows temperatures {:x :day :y :anomaly :series :city :bands 2})))

;; The bands drawn as filled lines - default band height (left), and three bands of a fixed height 1 on a
;; monotone curve resampled 4 points per gap (right); `horizon-bands` orders the colour domain:

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

;; ## series/horizon-bands

;; The band labels of a horizon of n bands in drawing order - positive bands first, then the mirrored negative
;; ones - the colour domain above:

(series/horizon-bands 3)

;; ## pairwise/matrix-rows

;; A statistic for every pair of variables, a row per cell: the statistic of the x variable's sample against the y
;; variable's (`:value`, `nil` when it can't be computed) with the sample size (`:n`). The built-in statistics by
;; family: `:symmetry` tells how a value changes when the samples swap (`:symmetric` - unchanged, `:antisymmetric` -
;; negated, `:reciprocal` - inverted, `:complementary` - `1 - v`, `:asymmetric` - otherwise); `:samples` - paired
;; statistics take the rows where both values are present, independent-sample ones (two groups compared) each
;; variable's own values; `:signed` - zero is the neutral value (a shaded `pairwise` chart centres its colours on
;; it):

(nu/data-table (map (fn [[k {:keys [label family symmetry samples signed description]}]]
                      {:family family :method k :label label :symmetry symmetry :samples samples
                       :signed (true? signed) :description description})
                    (sort-by (fn [[k {:keys [family]}]] [({:error 0 :association 1 :effect-size 2 :distance 3} family) (name k)])
                             pairwise/statistics)))

(nu/options-table (describe/options #'pairwise/matrix-rows))

(nu/source-note {:dataset :iris})

(def iris (nu/dataset :iris))

(nu/data-table iris)

;; The lower triangle of a symmetric statistic, the diagonal included (lower - the y variable after the x variable
;; in variable order, as the lower cells of `plot/pairs`):

(nu/data-table (pairwise/matrix-rows iris {:variables [:sepal-length :sepal-width :petal-length :petal-width]
                                           :method :spearman :triangle :lower :self-pairs true}))

;; The upper triangle of the mean squared error of two variables (one row - the pair once):

(nu/data-table (pairwise/matrix-rows iris {:variables [:sepal-length :petal-length] :method :mse :triangle :upper}))

;; A standalone matrix is a [tile](reference.charts_bins_fields.html#tile) per row (`:value-label` writes the
;; values), two side by side through [`plot/arrange`](reference.facets_arrange.html#arrange). Tiles order the
;; categories as they first appear and y grows upward, so the scales give both axes the variable order - the first
;; variable on top, as in `plot/pairs`: Cohen's d of every ordered pair, the diagonal included (left; a signed
;; statistic on a diverging palette centred on 0), and a rectangular matrix of sepal against petal measurements with
;; a user function and its label (right):

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

