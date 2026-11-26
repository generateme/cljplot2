;; # Distribution per level

;; These charts draw how the values of each level are spread: summaries (`box`, `violin`, `extent`) and the values
;; themselves (`stripplot`, `beeswarm`, `dotplot`, `rug`). Use them to compare spreads across levels; they all take
;; the [Strip options](reference.shared_options.html#strip-options) for placement, width and grouping.

(ns reference.charts-per-level
  (:require [fastmath.random :as r]
            [fastmath.stats :as stats]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [notebook-utils :as nu]))

(nu/source-note {:dataset :iris})

(def iris (nu/dataset :iris))

(nu/data-table iris)

(nu/source-note {:dataset :mtcars})

(def mtcars (nu/dataset :mtcars))

(nu/data-table mtcars)

;; ## Box plots

;; `chart/box` draws a box per level and group: the quartiles, the median, whiskers and outliers. Use it to compare
;; the spread of many levels at a glance. By default the chart computes the statistics from the values:

(nu/shared-groups (describe/options (chart/box)))

(nu/mapping-list (describe/mapping (chart/box)) :values)

(plot/render-image {:data iris
                    :mapping {:x :sepal-width :y :species :color :species}
                    :charts [(chart/box)]
                    :scales {:y {:reverse true}}
                    :legend false}
                   {:width 600 :height 300})

;; With `:stat :identity` the chart reads the statistics from one row per level and group instead:

(nu/mapping-list (describe/mapping (chart/box)) :summary)

(def summaries [{:site "north" :lower 2.1 :q1 3.4 :median 4.0 :q3 4.9 :upper 6.3 :outliers [7.4 8.0] :mean 4.3 :n 40}
                {:site "south" :lower 1.0 :q1 2.2 :median 2.9 :q3 3.3 :upper 4.4 :outliers [0.2] :mean 2.8 :n 15}
                {:site "east" :lower 3.0 :q1 4.1 :median 5.2 :q3 5.8 :upper 7.0 :outliers [] :mean 5.0 :n 90}])

(nu/data-table summaries)

(plot/render-image {:data summaries
                    :mapping {:y :site :lower :lower :q1 :q1 :median :median :q3 :q3 :upper :upper
                              :outliers :outliers :mean :mean :n :n}
                    :charts [(chart/box {:stat :identity :mean true :varwidth true})]
                    :scales {:y {:reverse true}}
                    :labels {:x "value"}}
                   {:width 600 :height 280})

;; These options style the boxes and outliers and pick what the chart draws. The plots below show rear axle ratios,
;; where the 4- and 8-cylinder groups have outliers: a custom style with the mean marker, no outliers with widths by
;; count, and the Tufte variant:

(nu/option-cards 'chart/box (describe/own-rows (describe/options (chart/box))))

(plot/render-image (plot/arrange [{:data mtcars
                                   :mapping {:x :drat :y :cyl}
                                   :charts [(chart/box {:color [230 160 60] :stroke-color :black :stroke-width 2 :shape \x :size 9
                                                        :mean true})]
                                   :scales {:y {:type :categorical :reverse true}}
                                   :labels {:title "style, :mean true"}}
                                  {:data mtcars
                                   :mapping {:x :drat :y :cyl}
                                   :charts [(chart/box {:outliers false :varwidth true})]
                                   :scales {:y {:type :categorical :reverse true}}
                                   :labels {:title ":outliers false, :varwidth"}}
                                  {:data mtcars
                                   :mapping {:x :drat :y :cyl}
                                   :charts [(chart/box {:variant :tufte})]
                                   :scales {:y {:type :categorical :reverse true}}
                                   :labels {:title ":variant :tufte"}}]
                                 {:columns 3})
                   {:width 1000 :height 300})

;; On the right `:padding` widens the value axis by 40% of the values' span on each side; the left panel keeps the
;; default:

(plot/render-image (plot/arrange [{:data mtcars
                                   :mapping {:x :drat :y :cyl}
                                   :charts [(chart/box)]
                                   :scales {:y {:type :categorical :reverse true}}
                                   :labels {:title "default padding"}}
                                  {:data mtcars
                                   :mapping {:x :drat :y :cyl}
                                   :charts [(chart/box {:padding {:x [0.4 0.4]}})]
                                   :scales {:y {:type :categorical :reverse true}}
                                   :labels {:title ":padding {:x [0.4 0.4]}"}}]
                                 {:columns 2})
                   {:width 900 :height 280})

;; ## Violin plots

;; `chart/violin` draws a kernel density estimate of each level's values, mirrored around its centre line, with inner
;; marks, a small box by default. Use it to see the shape of a distribution, e.g. two peaks a box plot hides.

(nu/shared-groups (describe/options (chart/violin)))

(nu/mapping-list (describe/mapping (chart/violin)))

(plot/render-image {:data iris
                    :mapping {:x :species :y :petal-length :color :species}
                    :charts [(chart/violin)]
                    :legend false}
                   {:width 600 :height 320})

;; These options style the violins and set the density estimate, the widths and the inner marks. The panels below
;; pair a bandwidth, a reach past the data and a width scale with three inner marks:

(nu/option-cards 'chart/violin (describe/own-rows (describe/options (chart/violin))))

(plot/render-image (plot/arrange [{:data iris
                                   :mapping {:x :species :y :petal-length}
                                   :charts [(chart/violin {:bandwidth 0.1 :inner :quartiles})]
                                   :labels {:title ":bandwidth 0.1, :inner :quartiles"}}
                                  {:data iris
                                   :mapping {:x :species :y :petal-length}
                                   :charts [(chart/violin {:cut 2 :inner :rug :inner-color [200 40 40]})]
                                   :labels {:title ":cut 2, :inner :rug"}}
                                  {:data mtcars
                                   :mapping {:x :cyl :y :mpg}
                                   :charts [(chart/violin {:scale :count :inner :points})]
                                   :scales {:x {:type :categorical}}
                                   :labels {:title ":scale :count, :inner :points"}}]
                                 {:columns 3})
                   {:width 1000 :height 320})

;; A theme sets the inner marks' sizes and the quartile dash; below, a wider inner box with a bigger median and
;; dotted quartiles:

(nu/theme-key-list :violin (describe/style-rows :violin [:inner-box-width :inner-median-size :inner-quartile-dash
                                                         :inner-rug-width :inner-point-size]))

(plot/render-image (plot/arrange [{:data iris
                                   :mapping {:x :species :y :petal-length}
                                   :charts [(chart/violin)]
                                   :theme {:chart {:violin {:inner-box-width 10 :inner-median-size 8}}}
                                   :labels {:title "wider inner box, bigger median"}}
                                  {:data iris
                                   :mapping {:x :species :y :petal-length}
                                   :charts [(chart/violin {:inner :quartiles})]
                                   :theme {:chart {:violin {:inner-quartile-dash [1.0 2.0]}}}
                                   :labels {:title "dotted quartiles"}}]
                                 {:columns 2})
                   {:width 800 :height 320})

;; The left panel sets a fill and outline. The middle one evaluates the Epanechnikov kernel at only 8 points, so its
;; outline is coarse. The right one, on mtcars by cylinders, sets `:varwidth` and `:padding`:

(plot/render-image (plot/arrange [{:data iris
                                   :mapping {:x :species :y :petal-length}
                                   :charts [(chart/violin {:color [230 160 60] :stroke-color :black :stroke-width 2})]
                                   :labels {:title "fill and outline"}}
                                  {:data iris
                                   :mapping {:x :species :y :petal-length}
                                   :charts [(chart/violin {:kernel :epanechnikov :n 8})]
                                   :labels {:title ":kernel :epanechnikov, :n 8"}}
                                  {:data mtcars
                                   :mapping {:x :cyl :y :mpg}
                                   :charts [(chart/violin {:varwidth true :padding {:y [0.3 0.3]}})]
                                   :scales {:x {:type :categorical}}
                                   :labels {:title ":varwidth, :padding"}}]
                                 {:columns 3})
                   {:width 1000 :height 320})

;; The panels show each `:scale` on mtcars by gears (15, 12 and 5 cars). With areas by count the five-gear violin
;; shrinks:

(plot/render-image (plot/arrange [{:data mtcars
                                   :mapping {:x :gear :y :mpg}
                                   :charts [(chart/violin {:scale :width})]
                                   :scales {:x {:type :categorical}}
                                   :labels {:title ":scale :width"}}
                                  {:data mtcars
                                   :mapping {:x :gear :y :mpg}
                                   :charts [(chart/violin {:scale :area})]
                                   :scales {:x {:type :categorical}}
                                   :labels {:title ":scale :area"}}
                                  {:data mtcars
                                   :mapping {:x :gear :y :mpg}
                                   :charts [(chart/violin {:scale :count})]
                                   :scales {:x {:type :categorical}}
                                   :labels {:title ":scale :count"}}]
                                 {:columns 3})
                   {:width 1000 :height 300})

;; The panels show each `:inner` mark in turn:

(plot/render-image (plot/arrange [{:data iris :mapping {:x :species :y :petal-length}
                                   :charts [(chart/violin {:inner :box})] :labels {:title ":inner :box"}}
                                  {:data iris :mapping {:x :species :y :petal-length}
                                   :charts [(chart/violin {:inner :quartiles})] :labels {:title ":quartiles"}}
                                  {:data iris :mapping {:x :species :y :petal-length}
                                   :charts [(chart/violin {:inner :rug})] :labels {:title ":rug"}}
                                  {:data iris :mapping {:x :species :y :petal-length}
                                   :charts [(chart/violin {:inner :points})] :labels {:title ":points"}}
                                  {:data iris :mapping {:x :species :y :petal-length}
                                   :charts [(chart/violin {:inner :none})] :labels {:title ":none"}}]
                                 {:columns 5})
                   {:width 1000 :height 300})

;; ## Error bars

;; `chart/extent` draws an interval per level and group, computed from the values - their range, a confidence
;; interval, quantiles or a bootstrap estimate - with a mark at its centre. Use it for error bars, alone or over bars and
;; points.

(nu/shared-groups (describe/options (chart/extent)))

(nu/mapping-list (describe/mapping (chart/extent)))

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :species}
                    :charts [(chart/extent)]
                    :scales {:y {:reverse true}}}
                   {:width 600 :height 280})

;; These options style the intervals and pick the method that computes them. The panels show `:stddev` in a custom
;; style, the 10th to 90th percentiles without a centre, and a seeded bootstrap of the mean:

(nu/option-cards 'chart/extent (describe/own-rows (describe/options (chart/extent))))

(plot/render-image (plot/arrange [{:data iris
                                   :mapping {:x :sepal-length :y :species}
                                   :charts [(chart/extent {:method :stddev :color [200 80 40] :line-width 4 :shape \s})]
                                   :scales {:y {:reverse true}}
                                   :labels {:title ":method :stddev"}}
                                  {:data iris
                                   :mapping {:x :sepal-length :y :species}
                                   :charts [(chart/extent {:method :percentile :method-args [10.0 90.0] :center :none})]
                                   :scales {:y {:reverse true}}
                                   :labels {:title "10-90 percentiles, no centre"}}
                                  {:data iris
                                   :mapping {:x :sepal-length :y :species}
                                   :charts [(chart/extent {:method :bootstrap-percentile :rng (r/rng :mersenne 3)})]
                                   :scales {:y {:reverse true}}
                                   :labels {:title "bootstrap mean"}}]
                                 {:columns 3})
                   {:width 1000 :height 300})

;; The plot below draws every method on setosa sepal lengths, a level each: the range, spreads around the mean
;; (`:mad` around the median), quantile intervals, Tukey's whisker ends and fences, the mean's confidence interval
;; and seeded bootstrap intervals:

(def setosa (filter #(= "setosa" (:species %)) iris))

(nu/data-table setosa)

(plot/render-image {:data setosa
                    :mapping {:x :sepal-length}
                    :charts (vec (for [m [:min-max :stddev :sem :mad :percentile :quantile :pi :hpdi :percentile-bc
                                     :percentile-bca :adjacent :inner-fence :outer-fence :ci :bootstrap-normal
                                     :bootstrap-percentile :bootstrap-basic :bootstrap-bc :bootstrap-bca
                                     :bootstrap-studentized :bootstrap-t :bootstrap-mad-median]]
                              (chart/extent {:method m :rng (r/rng :mersenne 5)
                                             :mapping {:y (constantly (name m))}})))
                    :scales {:y {:reverse true}}
                    :labels {:x "sepal length [cm]"}}
                   {:width 700 :height 600})

;; On the left the bootstrap resamples the median instead of the mean (compare the "bootstrap mean" panel above), and
;; `:size` enlarges its centre mark. On the right `:padding` widens the value axis:

(plot/render-image (plot/arrange [{:data iris
                                   :mapping {:x :sepal-length :y :species}
                                   :charts [(chart/extent {:method :bootstrap-percentile :rng (r/rng :mersenne 3)
                                                           :statistic stats/median :size 12})]
                                   :scales {:y {:reverse true}}
                                   :labels {:title ":statistic stats/median"}}
                                  {:data iris
                                   :mapping {:x :sepal-length :y :species}
                                   :charts [(chart/extent {:padding {:x [0.5 0.5]}})]
                                   :scales {:y {:reverse true}}
                                   :labels {:title ":padding {:x [0.5 0.5]}"}}]
                                 {:columns 2})
                   {:width 800 :height 300})

;; ## Strip plots

;; `chart/stripplot` draws every value as a point across its level's strip, jittered so equal values don't hide each
;; other. Use it for small samples, where each value counts.

(nu/shared-groups (describe/options (chart/stripplot)))

(nu/mapping-list (describe/mapping (chart/stripplot)))

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :species :color :species}
                    :charts [(chart/stripplot {:rng (r/rng :mersenne 1)})]
                    :scales {:y {:reverse true}}
                    :legend false}
                   {:width 600 :height 280})

;; These options style the points and set the jitter, seeded here so every build draws the same image. The panels
;; show no jitter, a wide jitter of outlined squares, and `:padding`:

(nu/option-cards 'chart/stripplot (describe/own-rows (describe/options (chart/stripplot))))

(plot/render-image (plot/arrange [{:data iris
                                   :mapping {:x :sepal-length :y :species}
                                   :charts [(chart/stripplot {:jitter 0})]
                                   :scales {:y {:reverse true}}
                                   :labels {:title ":jitter 0"}}
                                  {:data iris
                                   :mapping {:x :sepal-length :y :species}
                                   :charts [(chart/stripplot {:jitter 0.8 :size 8 :shape \S :color [200 80 40 90]
                                                              :stroke-color [120 40 20] :stroke-width 1.5
                                                              :rng (r/rng :mersenne 1)})]
                                   :scales {:y {:reverse true}}
                                   :labels {:title ":jitter 0.8, outlined squares"}}
                                  {:data iris
                                   :mapping {:x :sepal-length :y :species}
                                   :charts [(chart/stripplot {:padding {:x [0.3 0.3]} :rng (r/rng :mersenne 1)})]
                                   :scales {:y {:reverse true}}
                                   :labels {:title ":padding {:x [0.3 0.3]}"}}]
                                 {:columns 3})
                   {:width 1000 :height 280})

;; ## Beeswarm plots

;; `chart/beeswarm` draws every value as a point, packed across the strip so points don't overlap where the slot has
;; room. Use it when each value counts and the swarm's width should show the density.

(nu/shared-groups (describe/options (chart/beeswarm)))

(nu/mapping-list (describe/mapping (chart/beeswarm)))

(plot/render-image {:data iris
                    :mapping {:x :species :y :petal-length :color :species}
                    :charts [(chart/beeswarm)]
                    :legend false}
                   {:width 600 :height 320})

;; These options style the points and set how the chart packs them. The panels show large points left to overflow
;; their slot and squeezed into it, and the packing around their values:

(nu/option-cards 'chart/beeswarm (describe/own-rows (describe/options (chart/beeswarm))))

(plot/render-image (plot/arrange [{:data iris
                                   :mapping {:x :species :y :sepal-width}
                                   :charts [(chart/beeswarm {:size 14 :fit false})]
                                   :labels {:title ":size 14, :fit false"}}
                                  {:data iris
                                   :mapping {:x :species :y :sepal-width}
                                   :charts [(chart/beeswarm {:size 14})]
                                   :labels {:title ":size 14, :fit true"}}
                                  {:data iris
                                   :mapping {:x :species :y :sepal-width}
                                   :charts [(chart/beeswarm {:pack :around :stroke-color :white})]
                                   :labels {:title ":pack :around"}}]
                                 {:columns 3})
                   {:width 1000 :height 320})

;; The left panel draws outlined triangles in the default packing, where points move only across the value axis; the
;; right one sets `:padding`:

(plot/render-image (plot/arrange [{:data iris
                                   :mapping {:x :species :y :sepal-width}
                                   :charts [(chart/beeswarm {:color [200 80 40 160] :shape \^ :stroke-color :black
                                                             :stroke-width 0.5 :pack :across})]
                                   :labels {:title "triangles, outlined, :pack :across"}}
                                  {:data iris
                                   :mapping {:x :species :y :sepal-width}
                                   :charts [(chart/beeswarm {:padding {:y [0.4 0.4]}})]
                                   :labels {:title ":padding {:y [0.4 0.4]}"}}]
                                 {:columns 2})
                   {:width 900 :height 320})

;; ## Dot plots

;; `chart/dotplot` bins the values along the value axis and stacks a dot per value in its bin (Wilkinson's dot plot).
;; Use it to show a distribution's shape while every value stays countable.

(nu/shared-groups (describe/options (chart/dotplot)))

(nu/mapping-list (describe/mapping (chart/dotplot)))

(plot/render-image {:data mtcars
                    :mapping {:x :mpg}
                    :charts [(chart/dotplot {:binwidth 1.5})]}
                   {:width 600 :height 280})

;; These options style the dots and set how the chart bins and stacks them. The panels show fixed bins, stacks
;; centred on their slots, and each dot at its own value:

(nu/option-cards 'chart/dotplot (describe/own-rows (describe/options (chart/dotplot))))

(plot/render-image (plot/arrange [{:data mtcars
                                   :mapping {:x :mpg}
                                   :charts [(chart/dotplot {:method :histogram :binwidth 2 :origin 10})]
                                   :labels {:title ":method :histogram"}}
                                  {:data mtcars
                                   :mapping {:x :mpg}
                                   :charts [(chart/dotplot {:binwidth 1.5 :stack-dir :center :color [200 80 40]})]
                                   :labels {:title ":stack-dir :center"}}
                                  {:data mtcars
                                   :mapping {:x :mpg}
                                   :charts [(chart/dotplot {:binwidth 1.5 :at :value})]
                                   :labels {:title ":at :value"}}]
                                 {:columns 3})
                   {:width 1000 :height 280})

;; The left panel sets a fixed dot size and outlined squares; the right one sets `:padding`:

(plot/render-image (plot/arrange [{:data mtcars
                                   :mapping {:x :mpg}
                                   :charts [(chart/dotplot {:binwidth 1.5 :size 7 :shape \S :color [230 160 60]
                                                            :stroke-color :black :stroke-width 1})]
                                   :labels {:title ":size 7, outlined squares"}}
                                  {:data mtcars
                                   :mapping {:x :mpg}
                                   :charts [(chart/dotplot {:binwidth 1.5 :padding {:x [0.3 0.3]}})]
                                   :labels {:title ":padding {:x [0.3 0.3]}"}}]
                                 {:columns 2})
                   {:width 800 :height 280})

;; ## Rug plots

;; `chart/rug` draws a short line per value, along the panel's edge or in one strip per level. Use it beside another
;; chart to show where the values lie.

(nu/shared-groups (describe/options (chart/rug)))

(nu/mapping-list (describe/mapping (chart/rug)))

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :sepal-width}
                    :charts [(chart/scatter) (chart/rug {:mapping {:y nil}})]}
                   {:width 600 :height 320})

;; These options set the lines' colour and width. The panels show the defaults with a rug per level, wider lines,
;; and `:padding`:

(nu/option-cards 'chart/rug (describe/own-rows (describe/options (chart/rug))))

(plot/render-image (plot/arrange [{:data iris
                                   :mapping {:x :petal-length :y :species}
                                   :charts [(chart/rug)]
                                   :scales {:y {:reverse true}}
                                   :labels {:title "defaults, a rug per level"}}
                                  {:data iris
                                   :mapping {:x :petal-length :y :species}
                                   :charts [(chart/rug {:color [200 40 40 60] :line-width 3})]
                                   :scales {:y {:reverse true}}
                                   :labels {:title ":line-width 3"}}
                                  {:data iris
                                   :mapping {:x :petal-length :y :species}
                                   :charts [(chart/rug {:padding {:x [0.3 0.3]}})]
                                   :scales {:y {:reverse true}}
                                   :labels {:title ":padding {:x [0.3 0.3]}"}}]
                                 {:columns 3})
                   {:width 1000 :height 260})
