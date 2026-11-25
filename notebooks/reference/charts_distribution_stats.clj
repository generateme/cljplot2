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

;; Random log-normal values (seeded) - skewed in data units, even on a log axis:

(def skewed (mapv (fn [v] {:value v})
                  (r/->seq (r/distribution :log-normal {:scale 3.0 :shape 0.8 :rng (r/rng :mersenne 7)}) 400)))

(nu/data-table skewed)

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

;; ### Bins

;; The bins (a method, a count or breaks), what a bar's height measures and the space the bins are made in:

(nu/options-table (describe/own-rows (describe/options (chart/histogram))) :bins)

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

;; What a bar's height measures - the same bins as shares summing to 1 (`:pmf`, the default `:percent` scaled to
;; 100), and the running count (`:cumulative`):

(plot/render-image (plot/arrange [{:data faithful
                                   :mapping {:x :eruptions}
                                   :charts [(chart/histogram {:bins 20 :y :pmf})]
                                   :labels {:title ":y :pmf"}}
                                  {:data faithful
                                   :mapping {:x :eruptions}
                                   :charts [(chart/histogram {:bins 20 :y :cumulative})]
                                   :labels {:title ":y :cumulative"}}]
                                 {:columns 2})
                   {:width 900 :height 300})

;; On a log axis bins of equal width in data units crowd into the last decades; `:stat-space :scale` makes them in
;; the axis's log10 coordinates - equal-width on screen, `:density` per log10 unit (values a log axis can't place, 0
;; and negatives, are dropped and counted). In either space the values are numbers: histogram and density take no
;; dates.

(plot/render-image (plot/arrange [{:data skewed
                                   :mapping {:x :value}
                                   :scales {:x {:kind :log}}
                                   :charts [(chart/histogram {:bins 20 :y :count :stat-space :data})]
                                   :labels {:title ":stat-space :data"}}
                                  {:data skewed
                                   :mapping {:x :value}
                                   :scales {:x {:kind :log}}
                                   :charts [(chart/histogram {:bins 20 :y :count :stat-space :scale})]
                                   :labels {:title ":stat-space :scale"}}]
                                 {:columns 2})
                   {:width 900 :height 300})

;; Bins are shared by every facet panel, so panels compare bar by bar. Under a free (or sliced) facet x scale each
;; panel gets bins over its own range instead - a count of bins (here 8) or a method applies per panel, so the bars
;; fill each panel's own axis; explicit breaks stay shared. Margin histograms follow their panel's scale the same way.

(plot/render-image {:data iris
                    :mapping {:x :petal-length :facet :species}
                    :facets {:scales {:x :free}}
                    :charts [(chart/histogram {:bins 8 :y :count})]
                    :labels {:x "petal length [cm]"}}
                   {:width 900 :height 300})

;; ### Invalid values

;; Rows whose x is missing, NaN or not a number (or a value the stat space can't place) are dropped and counted;
;; `:invalid :show` draws their count as a bar in a band beside the x axis's start, outside the axis line, on the
;; same y scale, in the chart's own fill (or the mapped one), labelled by `:invalid-label` (which needs `:invalid
;; :show`). Shares (`:percent`, `:pmf`) are then of all rows, the invalid ones included; groups stack or dodge in the
;; band as in the bins, every facet panel has its own band and a flipped plot puts it under the panel. Only the main
;; charts of a generated grid draw a band: `:invalid :show` in a margin chart or a `:custom-grid` plot is an error.

(nu/options-table (describe/own-rows (describe/options (chart/histogram))) :invalid)

(def gappy (mapv (fn [i {:keys [value]}] {:value (when-not (zero? (mod i 5)) value) :group (if (even? i) "a" "b")})
                 (range) skewed))

(nu/data-table gappy)

(plot/render-image (plot/arrange [{:data gappy
                                   :mapping {:x :value}
                                   :charts [(chart/histogram {:bins 20 :y :count :invalid :show})]
                                   :labels {:title ":invalid :show"}}
                                  {:data gappy
                                   :mapping {:x :value :color :group}
                                   :flip true
                                   :charts [(chart/histogram {:bins 12 :position :stack :invalid :show :invalid-label "missing"})]
                                   :labels {:title "flipped, stacked groups, :invalid-label"}}]
                                 {:columns 2})
                   {:width 900 :height 300})

;; ### Position

;; How groups combine and the zero line and y range of mirrored groups:

(nu/options-table (describe/own-rows (describe/options (chart/histogram))) :position)

;; How groups share the bins: translucent over each other (`:alpha` of `:overlay`), side by side, stacked, or
;; stacked to 1 - the share of each species in every bin:

(plot/render-image (plot/arrange [{:data iris
                                   :mapping {:x :petal-length :color :species}
                                   :charts [(chart/histogram {:position :overlay :alpha 0.4 :bins 20})]
                                   :labels {:title ":position :overlay :alpha 0.4"}
                                   :legend false}
                                  {:data iris
                                   :mapping {:x :petal-length :color :species}
                                   :charts [(chart/histogram {:position :dodge :bins 12})]
                                   :labels {:title ":position :dodge"}
                                   :legend false}
                                  {:data iris
                                   :mapping {:x :petal-length :color :species}
                                   :charts [(chart/histogram {:position :stack :bins 20})]
                                   :labels {:title ":position :stack"}
                                   :legend false}
                                  {:data iris
                                   :mapping {:x :petal-length :color :species}
                                   :charts [(chart/histogram {:position :normalize :bins 20})]
                                   :labels {:title ":position :normalize"}
                                   :legend false}]
                                 {:columns 2})
                   {:width 900 :height 560})

;; Mirrored groups: halves tied to one y range [-M M] (`:symmetric`) or each fitted to its own tallest bar, the zero
;; line off; thick outlines and no padding above the bars:

(plot/render-image (plot/arrange [{:data two-species
                                   :mapping {:x :sepal-length :color :species}
                                   :charts [(chart/histogram {:position :mirror :bins 12 :symmetric true})]
                                   :labels {:title ":symmetric true"}
                                   :legend false}
                                  {:data two-species
                                   :mapping {:x :sepal-length :color :species}
                                   :charts [(chart/histogram {:position :mirror :bins 12 :symmetric false :zero-line false})]
                                   :labels {:title ":symmetric false :zero-line false"}
                                   :legend false}
                                  {:data faithful
                                   :mapping {:x :eruptions}
                                   :charts [(chart/histogram {:bins 15 :stroke-color :black :stroke-width 2
                                                              :padding {:y [0 0]}})]
                                   :labels {:title ":stroke-width 2 :padding {:y [0 0]}"}}]
                                 {:columns 3})
                   {:width 1000 :height 320})

;; ### Style

;; The bar fill, outline and translucency, and the zero line of mirrored groups:

(nu/options-table (describe/own-rows (describe/options (chart/histogram))) :style)

(plot/render-image (plot/arrange [{:data faithful
                                   :mapping {:x :eruptions}
                                   :charts [(chart/histogram {:bins 20 :color [200 120 60] :stroke-color :none})]
                                   :labels {:title ":color, :stroke-color :none"}}
                                  {:data two-species
                                   :mapping {:x :sepal-length :color :species}
                                   :charts [(chart/histogram {:bins 12 :alpha 0.3 :stroke-color :darker :stroke-width 1.5})]
                                   :labels {:title ":alpha 0.3 :stroke-width 1.5"}
                                   :legend false}]
                                 {:columns 2})
                   {:width 900 :height 300})

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

;; The ridgelines' gradient along x (over the x domain) or along y (from each ridge's baseline up):

(plot/render-image (plot/arrange [{:data iris
                                   :mapping {:x :sepal-length :y :species}
                                   :charts [(chart/density {:fill-gradient :mpl/plasma :fill-gradient-axis :x})]
                                   :scales {:y {:reverse true}}
                                   :labels {:title ":fill-gradient-axis :x"}}
                                  {:data iris
                                   :mapping {:x :sepal-length :y :species}
                                   :charts [(chart/density {:fill-gradient :mpl/plasma :fill-gradient-axis :y})]
                                   :scales {:y {:reverse true}}
                                   :labels {:title ":fill-gradient-axis :y"}}]
                                 {:columns 2})
                   {:width 900 :height 320})

;; ### Estimate

;; The kernel, its bandwidth (in data units or a selector), the number of evaluation points and how far a curve
;; reaches past the data:

(nu/options-table (describe/own-rows (describe/options (chart/density))) :estimate)

(plot/render-image (plot/arrange [{:data faithful
                                   :mapping {:x :eruptions}
                                   :charts [(chart/density)]
                                   :labels {:title "default (:gaussian, :nrd0)"}}
                                  {:data faithful
                                   :mapping {:x :eruptions}
                                   :charts [(chart/density {:bandwidth 0.1})]
                                   :labels {:title ":bandwidth 0.1"}}
                                  {:data faithful
                                   :mapping {:x :eruptions}
                                   :charts [(chart/density {:kernel :uniform :cut 0})]
                                   :labels {:title ":kernel :uniform :cut 0"}}]
                                 {:columns 3})
                   {:width 1000 :height 300})

;; A bandwidth selector instead of a number, few evaluation points (the curve drawn through 8 points), and a curve
;; reaching 1 bandwidth past the data (3 by default):

(plot/render-image (plot/arrange [{:data faithful
                                   :mapping {:x :eruptions}
                                   :charts [(chart/density {:bandwidth :sj})]
                                   :labels {:title ":bandwidth :sj"}}
                                  {:data faithful
                                   :mapping {:x :eruptions}
                                   :charts [(chart/density {:n 8})]
                                   :labels {:title ":n 8"}}
                                  {:data faithful
                                   :mapping {:x :eruptions}
                                   :charts [(chart/density {:cut 1})]
                                   :labels {:title ":cut 1"}}]
                                 {:columns 3})
                   {:width 1000 :height 300})

;; Skewed values: estimated in data units (the default) the curve is lopsided and its tail passes 0 - a log axis
;; can't show it; `:stat-space :scale` estimates the density of the log10 values on a log axis (per log10 unit, a
;; numeric `:bandwidth` in log10 units) and draws it back on the axis:

(plot/render-image (plot/arrange [{:data skewed
                                   :mapping {:x :value}
                                   :charts [(chart/density {:stat-space :data :fill :color})]
                                   :labels {:title ":stat-space :data"}}
                                  {:data skewed
                                   :mapping {:x :value}
                                   :scales {:x {:kind :log}}
                                   :charts [(chart/density {:stat-space :scale :fill :color})]
                                   :labels {:title ":stat-space :scale, a log x axis"}}]
                                 {:columns 2})
                   {:width 900 :height 300})

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

;; Stacked to 1 (each species' share of the density at every x), the streamgraphs, and a mirrored pair without the
;; zero line and with no padding:

(plot/render-image (plot/arrange [{:data iris
                                   :mapping {:x :petal-length :color :species}
                                   :charts [(chart/density {:position :normalize})]
                                   :labels {:title ":position :normalize"}
                                   :legend false}
                                  {:data iris
                                   :mapping {:x :petal-length :color :species}
                                   :charts [(chart/density {:position :silhouette})]
                                   :labels {:title ":position :silhouette"}
                                   :legend false}
                                  {:data iris
                                   :mapping {:x :petal-length :color :species}
                                   :charts [(chart/density {:position :wiggle})]
                                   :labels {:title ":position :wiggle"}
                                   :legend false}
                                  {:data two-species
                                   :mapping {:x :petal-length :color :species}
                                   :charts [(chart/density {:position :mirror :fill :color :zero-line false
                                                            :padding {:x [0 0] :y [0 0]}})]
                                   :labels {:title ":zero-line false, no padding"}
                                   :legend false}]
                                 {:columns 2})
                   {:width 900 :height 560})

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

;; The point style (`:alpha` - the points' opacity, not the line's) and the reference line's width:

(nu/options-table (describe/own-rows (describe/options (chart/qq))) :style)

(plot/render-image {:data (filter #(= "t(3)" (:sample %)) samples)
                    :mapping {:y :v}
                    :charts [(chart/qq {:x-distribution [:normal] :size 9 :shape \S :color [200 80 40] :alpha 0.47
                                        :stroke-color :black :stroke-width 0.5 :line-width 3})]
                    :labels {:title "squares, :line-width 3"}}
                   {:width 500 :height 350})

;; ### Distributions

;; Either side may be a distribution instead of data; with both sides distributions the chart draws `:n`
;; quantile pairs labelled by `:label`. The reference line through the quartiles may be turned off, and the padding
;; set (none on the right):

(nu/options-table (describe/own-rows (describe/options (chart/qq))) :distributions)

(plot/render-image (plot/arrange [{:charts [(chart/qq {:x-distribution [:normal] :y-distribution [:t {:degrees-of-freedom 3}]
                                                       :label "t(3)" :mapping {:color :label}})
                                            (chart/qq {:x-distribution [:normal] :y-distribution [:t {:degrees-of-freedom 30}]
                                                       :label "t(30)" :mapping {:color :label} :line false :n 30})]
                                   :labels {:title "both sides distributions"}}
                                  {:data faithful
                                   :mapping {:y :eruptions}
                                   :charts [(chart/qq {:x-distribution [:uniform-real {:lower 0 :upper 1}] :line false
                                                       :padding {:x [0 0] :y [0 0]}})]
                                   :labels {:title "uniform, no line, no padding"}}]
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
                    :charts [(chart/pp {:x-distribution [:normal] :size 8 :shape \O :color [200 80 40] :alpha 0.55
                                        :stroke-color :black :stroke-width 1 :line-width 3})]
                    :labels {:title "outlined points, :line-width 3"}}
                   {:width 500 :height 350})

;; ### Distributions

;; Either side may be a distribution; with both sides distributions the chart evaluates both CDFs at `:n`
;; values over `:domain` (required then); the right panel has no line and wide padding:

(nu/options-table (describe/own-rows (describe/options (chart/pp))) :distributions)

(plot/render-image (plot/arrange [{:charts [(chart/pp {:x-distribution [:normal] :y-distribution [:t {:degrees-of-freedom 2}]
                                                       :domain [-5 5] :n 40 :label "t(2)" :mapping {:color :label}})
                                            (chart/pp {:x-distribution [:normal] :y-distribution [:logistic]
                                                       :domain [-5 5] :n 40 :label "logistic" :mapping {:color :label}})]
                                   :labels {:title "both sides distributions, :domain [-5 5]" :y "probabilities"}}
                                  {:data faithful
                                   :mapping {:y :waiting}
                                   :charts [(chart/pp {:x-distribution [:normal {:mu 71 :sd 13.6}] :line false :size 3
                                                       :padding {:x [0.2 0.2] :y [0.2 0.2]}})]
                                   :labels {:title "normal(71, 13.6), no line, padding 0.2"}}]
                                 {:columns 2 :widths [1.25 1]})
                   {:width 1000 :height 380})

(nu/shared-groups (describe/options (chart/pp)))
