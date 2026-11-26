;; # Distribution of one variable

;; These charts draw how the values of one variable are spread: binned counts (`histogram`), a kernel density
;; (`density`) and the empirical distribution function (`ecdf`). Use them to see the shape of a distribution; `qq` and
;; `pp` compare two distributions by quantiles or by probabilities.

(ns reference.charts-one-variable
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

;; Seeded samples, normal and t with 3 degrees of freedom - for the Q-Q and P-P plots:

(def samples (let [g (r/rng :mersenne 11)
                   t3 (r/distribution :t {:degrees-of-freedom 3 :rng g})]
               (vec (concat (for [_ (range 200)] {:v (r/grandom g) :sample "normal"})
                            (for [_ (range 200)] {:v (r/sample t3) :sample "t(3)"})))))

(nu/data-table samples)

;; Two species only - for the charts drawing two groups against each other:

(def two-species (filter #(#{"setosa" "versicolor"} (:species %)) iris))

(nu/data-table two-species)

;; ## Histograms

;; `chart/histogram` cuts the x values into bins and draws a bar per bin, its height the bin's count or share. Use it
;; to see the shape of a distribution at a glance. Groups overlay, dodge, stack or mirror, and a categorical `:y`
;; draws one histogram per level (ridgelines).

(nu/shared-groups (describe/options (chart/histogram)))

(nu/mapping-list (describe/mapping (chart/histogram)))

(plot/render-image {:data iris
                    :mapping {:x :petal-length :color :species}
                    :charts [(chart/histogram)]
                    :labels {:x "petal length [cm]"}}
                   {:width 600 :height 350})

;; ### Bins

;; These options set the bins, what a bar's height measures and the space the bins are made in. The first panels
;; below use 30 bins of counts, explicit breaks measured as `:density` and two mirrored groups:

(nu/option-cards 'chart/histogram (describe/own-rows (describe/options (chart/histogram))) :bins)

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

;; The next panels measure the same bins as shares summing to 1 (`:pmf`, the default `:percent` divided by 100) and
;; as a running count (`:cumulative`):

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

;; On a log axis, bins of equal width in data units crowd into the last decades. `:stat-space :scale` makes the bins
;; in the axis's log10 coordinates instead, so they are equally wide on screen. In either space the values must be
;; numbers: the chart takes no dates.

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

;; Facet panels share the bins, so they compare bar by bar. Under a free or sliced facet x scale, the chart bins the
;; panels sharing an x scale over their own range instead (each panel of a wrap layout, a column or row of a grid): a
;; bin count (here 8) or a rule applies per panel, while explicit breaks stay shared. Margin histograms follow their panel's scale the same way.

(plot/render-image {:data iris
                    :mapping {:x :petal-length :facet :species}
                    :facets {:scales {:x :free}}
                    :charts [(chart/histogram {:bins 8 :y :count})]
                    :labels {:x "petal length [cm]"}}
                   {:width 900 :height 300})

;; ### Invalid values

;; These options draw the rows whose x is missing or not a number, which the chart drops by default, in a band beside
;; the x axis. With `:invalid :show`, shares (`:percent`, `:pmf`) count all rows, the invalid ones included. Groups
;; stack or dodge in the band as in the bins, and every facet panel has its own band. A margin chart or a
;; `:custom-grid` plot has no band, so `:invalid :show` there is an error. The panels below show every fifth value
;; missing, then stack two groups in a flipped plot with their own label:

(nu/option-cards 'chart/histogram (describe/own-rows (describe/options (chart/histogram))) :invalid)

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

;; These options set how groups share the bins, the 0 line and y range of mirrored groups, and the padding. The
;; first panels below overlay the species translucently, dodge them, stack them and stack them to 1 - the share of
;; each species in every bin:

(nu/option-cards 'chart/histogram (describe/own-rows (describe/options (chart/histogram))) :position)

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

;; The next panels mirror two species on one y range `[-M M]` (`:symmetric`) and on their own ranges without the 0
;; line; the last one draws thick outlines and no padding above the bars:

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

;; These options set the bar fill, outline and opacity, and the 0 line of mirrored groups. The panels below set a fill
;; without outlines, then translucent groups with wider outlines:

(nu/option-cards 'chart/histogram (describe/own-rows (describe/options (chart/histogram))) :style)

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

;; ## Density curves

;; `chart/density` estimates the density of the x values with a kernel and draws it as a curve, optionally filled.
;; Use it for a smooth picture of a distribution that doesn't hang on bin edges. Groups overlay, stack or mirror, and
;; a categorical `:y` draws ridgelines.

(nu/shared-groups (describe/options (chart/density)))

(nu/mapping-list (describe/mapping (chart/density)))

(plot/render-image {:data iris
                    :mapping {:x :petal-length :color :species}
                    :charts [(chart/density)]
                    :labels {:x "petal length [cm]"}}
                   {:width 600 :height 350})

;; ### Style

;; These options fill the area under each curve, set its opacity, colour ridgelines with a gradient and style the 0
;; line of mirrored curves. The first panels below fill the species' curves, colour ridgelines along x and draw a red
;; 0 line:

(nu/option-cards 'chart/density (describe/own-rows (describe/options (chart/density))) :style)

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

;; The next panels run the ridgelines' gradient along x over the x domain, then along y from each ridge's baseline
;; up:

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

;; These options set the estimate: the kernel, its bandwidth, the number of evaluation points, how far a curve reaches
;; past the data and the space of the estimate. The first panels below compare the default with a narrow bandwidth
;; and a uniform kernel cut at the data:

(nu/option-cards 'chart/density (describe/own-rows (describe/options (chart/density))) :estimate)

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

;; The next panels pick the bandwidth with a selector, evaluate the curve at only 8 points and stop it 1 bandwidth
;; past the data (3 by default):

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

;; For skewed values, the curve estimated in data units (the default) is lopsided and its tail passes 0, which a log
;; axis can't show. `:stat-space :scale` estimates the density of the log10 values on a log axis and draws it back
;; on the axis:

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

;; These options set how groups combine, the 0 line and y range of mirrored halves, and the padding. The first panels
;; below stack the species, mirror two of them and tie the mirrored halves to one range:

(nu/option-cards 'chart/density (describe/own-rows (describe/options (chart/density))) :position)

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

;; The next panels stack the curves to 1 (each species' share of the density at every x), draw the two streamgraphs
;; and mirror a pair without the 0 line or padding:

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

;; ## Empirical CDFs

;; `chart/ecdf` draws a step line per group: the share of values at or below each x. Use it to read medians and
;; percentiles off a distribution, or to compare groups without choosing bins or a bandwidth.

(nu/shared-groups (describe/options (chart/ecdf)))

(nu/mapping-list (describe/mapping (chart/ecdf)))

(plot/render-image {:data iris
                    :mapping {:x :petal-length :color :species}
                    :charts [(chart/ecdf)]
                    :labels {:x "petal length [cm]" :y "share at or below"}}
                   {:width 600 :height 350})

;; This option sets the padding. The example below has none on y, so the steps start and end at the panel's edges:

(nu/option-cards 'chart/ecdf (describe/own-rows (describe/options (chart/ecdf))))

(plot/render-image {:data faithful
                    :mapping {:x :eruptions}
                    :charts [(chart/ecdf {:padding {:y [0 0]}})]
                    :labels {:title ":padding {:y [0 0]}"}}
                   {:width 500 :height 300})

;; ## Q-Q plots

;; `chart/qq` draws the quantiles of one side against the other's: two data columns, or data against a theoretical
;; distribution. Use it to check whether a sample follows a distribution: points on the reference line mean the same
;; shape.

(nu/shared-groups (describe/options (chart/qq)))

(nu/mapping-list (describe/mapping (chart/qq)))

(plot/render-image {:data samples
                    :mapping {:y :v :color :sample}
                    :charts [(chart/qq {:x-distribution [:normal]})]
                    :labels {:x "normal quantiles" :y "sample quantiles"}}
                   {:width 600 :height 400})

;; ### Style

;; These options style the points and set the reference line's width. The example below draws translucent outlined
;; squares and a thick line:

(nu/option-cards 'chart/qq (describe/own-rows (describe/options (chart/qq))) :style)

(plot/render-image {:data (filter #(= "t(3)" (:sample %)) samples)
                    :mapping {:y :v}
                    :charts [(chart/qq {:x-distribution [:normal] :size 9 :shape \S :color [200 80 40] :alpha 0.47
                                        :stroke-color :black :stroke-width 0.5 :line-width 3})]
                    :labels {:title "squares, :line-width 3"}}
                   {:width 500 :height 350})

;; ### Distributions

;; These options turn either side into a distribution and set the reference line and the padding; with two
;; distributions they also set the number of points and the label. The left panel below draws two pairs of distributions, the right one a sample against
;; a uniform distribution without the line or padding:

(nu/option-cards 'chart/qq (describe/own-rows (describe/options (chart/qq))) :distributions)

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

;; ## P-P plots

;; `chart/pp` draws probabilities against probabilities: one side's empirical or theoretical CDF against the other
;; side's at the same values. Use it to compare distributions near their centres, where a Q-Q plot crowds its points;
;; points on y = x mean the same distribution.

(nu/shared-groups (describe/options (chart/pp)))

(nu/mapping-list (describe/mapping (chart/pp)))

(plot/render-image {:data samples
                    :mapping {:y :v :color :sample}
                    :charts [(chart/pp {:x-distribution [:normal] :size 4})]
                    :labels {:x "normal probabilities" :y "sample probabilities"}}
                   {:width 600 :height 400})

;; ### Style

;; These options style the points and set the reference line's width. The example below draws translucent outlined
;; circles and a thick line:

(nu/option-cards 'chart/pp (describe/own-rows (describe/options (chart/pp))) :style)

(plot/render-image {:data (filter #(= "t(3)" (:sample %)) samples)
                    :mapping {:y :v}
                    :charts [(chart/pp {:x-distribution [:normal] :size 8 :shape \O :color [200 80 40] :alpha 0.55
                                        :stroke-color :black :stroke-width 1 :line-width 3})]
                    :labels {:title "outlined points, :line-width 3"}}
                   {:width 500 :height 350})

;; ### Distributions

;; These options turn either side into a distribution and set the reference line and the padding; with two
;; distributions they also set the number of points, the domain and the label. The left panel below compares two pairs of distributions over `[-5 5]`, the
;; right one a sample against a fitted normal without the line and with wide padding:

(nu/option-cards 'chart/pp (describe/own-rows (describe/options (chart/pp))) :distributions)

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
