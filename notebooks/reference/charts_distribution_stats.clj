;; # Charts: distribution statistics

;; Charts computing a statistic of the values on one axis: binned counts (`histogram`), kernel density curves
;; (`density`), the empirical distribution function (`ecdf`) and the comparison of two distributions by quantiles
;; (`qq`) or probabilities (`pp`).

(ns reference.charts-distribution-stats
  (:require [fastmath.random :as r]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [notebook-utils :as nu]))

(nu/source-note {:dataset :iris})

(def iris (nu/dataset :iris))

(nu/data-table iris)

(nu/source-note {:dataset :faithful})

(def faithful (nu/dataset :faithful))

(nu/data-table faithful)

;; Two species only - for the charts drawing two groups against each other:

(def two-species (filter #(#{"setosa" "versicolor"} (:species %)) iris))

(nu/data-table two-species)

;; ## histogram

;; Bars counting the values falling into each bin; groups overlay, dodge, stack or mirror, and a categorical `:y`
;; gives one histogram per level (ridgelines).

(nu/options-table (describe/mapping (chart/histogram)))

(plot/render-image {:data iris
                    :mapping {:x :petal-length :color :species}
                    :charts [(chart/histogram)]
                    :labels {:x "petal length [cm]"}}
                   {:width 600 :height 350})

;; Its options: the bins (a method, a count or breaks), what a bar's height measures, how groups combine, the bar
;; style and the zero line of mirrored groups:

(nu/options-table (describe/own-rows (describe/options (chart/histogram))))

(plot/render-image (plot/arrange [{:data faithful
                                   :mapping {:x :eruptions}
                                   :charts [(chart/histogram {:bins 30 :y :count :stroke-color :none})]
                                   :labels {:title ":bins 30 :y :count"}}
                                  {:data faithful
                                   :mapping {:x :eruptions}
                                   :charts [(chart/histogram {:bins [1.5 2 2.5 3 3.5 4 4.5 5 5.5] :y :density
                                                              :color [120 160 90] :stroke-color :white})]
                                   :labels {:title "breaks, :y :density"}}
                                  {:data two-species
                                   :mapping {:x :sepal-length :color :species}
                                   :charts [(chart/histogram {:position :mirror :bins 12 :zero-line-color :red
                                                              :zero-line-width 2})]
                                   :labels {:title ":position :mirror"}
                                   :legend false}]
                                 {:columns 3})
                   {:width 1000 :height 320})

(nu/shared-groups (describe/options (chart/histogram)))

;; ## density

;; A kernel density estimate per group drawn as a curve, optionally filled; groups overlay, stack or mirror, and a
;; categorical `:y` gives ridgelines.

(nu/options-table (describe/mapping (chart/density)))

(plot/render-image {:data iris
                    :mapping {:x :petal-length :color :species}
                    :charts [(chart/density)]
                    :labels {:x "petal length [cm]"}}
                   {:width 600 :height 350})

;; ### Style

;; The area under a curve, its opacity, a gradient fill of ridgelines and the zero line of mirrored curves:

(nu/options-table (describe/own-rows (describe/options (chart/density))) :style)

(plot/render-image (plot/arrange [{:data iris
                                   :mapping {:x :petal-length :color :species}
                                   :charts [(chart/density {:fill :color :alpha 0.3})]
                                   :labels {:title ":fill :color :alpha 0.3"}
                                   :legend false}
                                  {:data iris
                                   :mapping {:x :sepal-length :y :species}
                                   :charts [(chart/density {:fill-gradient :mpl/viridis})]
                                   :scales {:y {:reverse true}}
                                   :labels {:title "ridgelines, :fill-gradient"}}
                                  {:data two-species
                                   :mapping {:x :sepal-length :color :species}
                                   :charts [(chart/density {:position :mirror :fill :color
                                                            :zero-line-color :red :zero-line-width 2})]
                                   :labels {:title ":zero-line-color :red"}
                                   :legend false}]
                                 {:columns 3})
                   {:width 1000 :height 320})

;; ### Estimate

;; The kernel, its bandwidth (in data units or a selector), the number of evaluation points and how far a curve
;; reaches past the data:

(nu/options-table (describe/own-rows (describe/options (chart/density))) :estimate)

(plot/render-image (plot/arrange (for [[title opts] [["default (:gaussian, :nrd0)" {}]
                                                     [":bandwidth 0.1" {:bandwidth 0.1}]
                                                     [":kernel :uniform :cut 0" {:kernel :uniform :cut 0}]]]
                                   {:data faithful
                                    :mapping {:x :eruptions}
                                    :charts [(chart/density opts)]
                                    :labels {:title title}})
                                 {:columns 3})
                   {:width 1000 :height 300})

;; ### Position

;; How groups combine (the Layering and position chapter shows the streamgraphs), a common y range of the mirrored
;; halves and the padding:

(nu/options-table (describe/own-rows (describe/options (chart/density))) :position)

(plot/render-image (plot/arrange [{:data iris
                                   :mapping {:x :petal-length :color :species}
                                   :charts [(chart/density {:position :stack})]
                                   :labels {:title ":position :stack"}
                                   :legend false}
                                  {:data two-species
                                   :mapping {:x :petal-length :color :species}
                                   :charts [(chart/density {:position :mirror :fill :color})]
                                   :labels {:title ":position :mirror"}
                                   :legend false}
                                  {:data two-species
                                   :mapping {:x :petal-length :color :species}
                                   :charts [(chart/density {:position :mirror :fill :color :symmetric true})]
                                   :labels {:title ":symmetric true"}}]
                                 {:columns 3 :widths [1 1 1.4]})
                   {:width 1000 :height 320})

(nu/shared-groups (describe/options (chart/density)))

;; ## ecdf

;; A step line per group: the share of values at or below each x.

(nu/options-table (describe/mapping (chart/ecdf)))

(plot/render-image {:data iris
                    :mapping {:x :petal-length :color :species}
                    :charts [(chart/ecdf)]
                    :labels {:x "petal length [cm]" :y "share at or below"}}
                   {:width 600 :height 350})

;; Its only own option is the padding - here none on y, so the steps start and end at the panel's edges:

(nu/options-table (describe/own-rows (describe/options (chart/ecdf))))

(plot/render-image {:data faithful
                    :mapping {:x :eruptions}
                    :charts [(chart/ecdf {:padding {:y [0 0]}})]
                    :labels {:title ":padding {:y [0 0]}"}}
                   {:width 500 :height 300})

(nu/shared-groups (describe/options (chart/ecdf)))

;; ## qq

;; Quantiles of one side against the other's: two data columns, or data against a theoretical distribution. Points
;; on the reference line mean the same shape of distribution.

(nu/options-table (describe/mapping (chart/qq)))

(def samples (let [g (r/rng :mersenne 11)
                   t3 (r/distribution :t {:degrees-of-freedom 3 :rng g})]
               (vec (concat (for [_ (range 200)] {:v (r/grandom g) :sample "normal"})
                            (for [_ (range 200)] {:v (r/sample t3) :sample "t(3)"})))))

(nu/data-table samples)

(plot/render-image {:data samples
                    :mapping {:y :v :color :sample}
                    :charts [(chart/qq {:x-distribution [:normal]})]
                    :labels {:x "normal quantiles" :y "sample quantiles"}}
                   {:width 600 :height 400})

;; ### Style

;; The point style and the reference line's width:

(nu/options-table (describe/own-rows (describe/options (chart/qq))) :style)

(plot/render-image {:data (filter #(= "t(3)" (:sample %)) samples)
                    :mapping {:y :v}
                    :charts [(chart/qq {:x-distribution [:normal] :size 9 :shape \s :color [200 80 40 120]
                                        :stroke-color :black :stroke-width 0.5 :line-width 3})]
                    :labels {:title "squares, :line-width 3"}}
                   {:width 500 :height 350})

;; ### Distributions

;; Either side may be a distribution instead of data; with both sides distributions the chart draws `:points`
;; quantile pairs labelled by `:label`. The reference line through the quartiles may be turned off:

(nu/options-table (describe/own-rows (describe/options (chart/qq))) :distributions)

(plot/render-image (plot/arrange [{:charts [(chart/qq {:x-distribution [:normal] :y-distribution [:t {:degrees-of-freedom 3}]
                                                       :label "t(3)" :mapping {:color :label}})
                                            (chart/qq {:x-distribution [:normal] :y-distribution [:t {:degrees-of-freedom 30}]
                                                       :label "t(30)" :mapping {:color :label} :line false :points 30})]
                                   :labels {:title "both sides distributions"}}
                                  {:data faithful
                                   :mapping {:y :eruptions}
                                   :charts [(chart/qq {:x-distribution [:uniform-real {:lower 0 :upper 1}] :line false})]
                                   :labels {:title "eruptions against uniform, :line false"}}]
                                 {:columns 2 :widths [1.25 1]})
                   {:width 1000 :height 380})

(nu/shared-groups (describe/options (chart/qq)))

;; ## pp

;; Probabilities against probabilities: each value's empirical (or theoretical) CDF on one side against the other
;; side's at the same value; points on y = x mean the same distribution.

(nu/options-table (describe/mapping (chart/pp)))

(plot/render-image {:data samples
                    :mapping {:y :v :color :sample}
                    :charts [(chart/pp {:x-distribution [:normal] :size 4})]
                    :labels {:x "normal probabilities" :y "sample probabilities"}}
                   {:width 600 :height 400})

;; ### Style

;; The point style and the reference line's width:

(nu/options-table (describe/own-rows (describe/options (chart/pp))) :style)

(plot/render-image {:data (filter #(= "t(3)" (:sample %)) samples)
                    :mapping {:y :v}
                    :charts [(chart/pp {:x-distribution [:normal] :size 5 :shape \x :color [200 80 40]
                                        :line-width 3})]
                    :labels {:title "crosses, :line-width 3"}}
                   {:width 500 :height 350})

;; ### Distributions

;; Either side may be a distribution; with both sides distributions the chart evaluates both CDFs at `:points`
;; values over `:domain` (required then):

(nu/options-table (describe/own-rows (describe/options (chart/pp))) :distributions)

(plot/render-image (plot/arrange [{:charts [(chart/pp {:x-distribution [:normal] :y-distribution [:t {:degrees-of-freedom 2}]
                                                       :domain [-5 5] :points 40 :label "t(2)" :mapping {:color :label}})
                                            (chart/pp {:x-distribution [:normal] :y-distribution [:logistic]
                                                       :domain [-5 5] :points 40 :label "logistic" :mapping {:color :label}})]
                                   :labels {:title "both sides distributions, :domain [-5 5]" :y "probabilities"}}
                                  {:data faithful
                                   :mapping {:y :waiting}
                                   :charts [(chart/pp {:x-distribution [:normal {:mu 71 :sd 13.6}] :line false :size 3})]
                                   :labels {:title "waiting against normal(71, 13.6), :line false"}}]
                                 {:columns 2 :widths [1.25 1]})
                   {:width 1000 :height 380})

(nu/shared-groups (describe/options (chart/pp)))
