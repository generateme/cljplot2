;; # Distribution per level

;; Charts that show how the values of each level are spread: summaries (`box`, `violin`, `extent`) and the values
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

;; ## box

;; A box per level (and group): the quartiles, the median, whiskers to the most extreme values within 1.5 IQR of
;; the box and the outliers past them - computed from the values ...

(nu/options-table (describe/mapping (chart/box)) :values)

(plot/render-image {:data iris
                    :mapping {:x :sepal-width :y :species :color :species}
                    :charts [(chart/box)]
                    :scales {:y {:reverse true}}
                    :legend false}
                   {:width 600 :height 300})

;; ... or read from rows of precomputed statistics, one row per level and group, with `:stat :identity`:

(nu/options-table (describe/mapping (chart/box)) :summary)

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

;; Its options: the box and outlier style, the statistics, outliers on or off, the mean marker, widths by count and
;; the Tufte variant (whisker lines and a median point) - here on rear axle ratios, with outliers in the 4- and
;; 8-cylinder groups:

(nu/options-table (describe/own-rows (describe/options (chart/box))))

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

;; `:padding` - space past the values along the value axis, fractions of their span (default 5% each side):

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

(nu/shared-groups (describe/options (chart/box)))

;; ## violin

;; A mirrored kernel density per level (and group), with inner marks - a small box by default.

(nu/options-table (describe/mapping (chart/violin)))

(plot/render-image {:data iris
                    :mapping {:x :species :y :petal-length :color :species}
                    :charts [(chart/violin)]
                    :legend false}
                   {:width 600 :height 320})

;; Its options: the fill, outline and inner marks' colour, the density estimate (kernel, bandwidth, points, how far
;; past the data), how widths compare between violins and the inner marks:

(nu/options-table (describe/own-rows (describe/options (chart/violin))))

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

;; The inner marks' sizes and the quartile dash are theme keys of `[:chart :violin]`, not options:

(nu/options-table (describe/style-rows :violin [:inner-box-width :inner-median-size :inner-quartile-dash
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

;; The fill and outline, and the density estimate: another kernel, few evaluation points (a coarse outline),
;; widths by count (`:varwidth`) and the padding:

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

;; Every `:scale` - violins as wide as their slots, of equal areas, or of areas by count (mtcars by gears: 15, 12
;; and 5 cars - the five-gear violin shrinks):

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

;; Every `:inner` mark:

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

(nu/shared-groups (describe/options (chart/violin)))

;; ## extent

;; An interval per level (and group) from the item's values - their range, a confidence interval, quantiles or a
;; bootstrap estimate - with a mark at its centre.

(nu/options-table (describe/mapping (chart/extent)))

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :species}
                    :charts [(chart/extent)]
                    :scales {:y {:reverse true}}}
                   {:width 600 :height 280})

;; Its options: the interval's and centre's style, the method giving the interval (and its arguments), the
;; bootstrap's statistic and random generator (seeded here), the centre mark and the padding:

(nu/options-table (describe/own-rows (describe/options (chart/extent))))

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

;; Every method on one sample (setosa sepal lengths), a level each: the range (`:min-max`), spreads around the mean
;; (`:stddev`, `:sem`, `:mad` - around the median), quantile intervals (`:percentile`, `:quantile`, `:pi`, `:hpdi`,
;; the bias-corrected `:percentile-bc` / `:percentile-bca`), Tukey's whisker ends and fences (`:adjacent`,
;; `:inner-fence`, `:outer-fence`), the mean's confidence interval (`:ci`) and the bootstrap confidence intervals of
;; the mean (`:bootstrap-*`, 500 resamples, seeded):

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

;; The bootstrap resamples the mean unless `:statistic` names another function - here the median (the bootstrap
;; of the mean is the "bootstrap mean" panel above); `:center :method` (the default) marks the method's centre, here
;; sized up with `:size`:

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

(nu/shared-groups (describe/options (chart/extent)))

;; ## stripplot

;; Every value as a point across its level's strip, jittered so equal values don't hide each other.

(nu/options-table (describe/mapping (chart/stripplot)))

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :species :color :species}
                    :charts [(chart/stripplot {:rng (r/rng :mersenne 1)})]
                    :scales {:y {:reverse true}}
                    :legend false}
                   {:width 600 :height 280})

;; Its options: the point style, the jitter spread (seeded for the same image on every build) and the padding:

(nu/options-table (describe/own-rows (describe/options (chart/stripplot))))

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

(nu/shared-groups (describe/options (chart/stripplot)))

;; ## beeswarm

;; Every value as a point, packed across the strip without overlaps - the swarm's width shows the density.

(nu/options-table (describe/mapping (chart/beeswarm)))

(plot/render-image {:data iris
                    :mapping {:x :species :y :petal-length :color :species}
                    :charts [(chart/beeswarm)]
                    :legend false}
                   {:width 600 :height 320})

;; Its options: the point style (the size is also the packing diameter), squeezing a wide swarm into its slot,
;; the packing direction and the padding:

(nu/options-table (describe/own-rows (describe/options (chart/beeswarm))))

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

;; The point style, the default packing (`:across` - points move only across the value axis) and the padding:

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

(nu/shared-groups (describe/options (chart/beeswarm)))

;; ## dotplot

;; Values binned along the value axis, each a dot stacked in its bin (Wilkinson's dot plot).

(nu/options-table (describe/mapping (chart/dotplot)))

(plot/render-image {:data mtcars
                    :mapping {:x :mpg}
                    :charts [(chart/dotplot {:binwidth 1.5})]}
                   {:width 600 :height 280})

;; Its options: the dot style, the binning (density or fixed bins from an origin), where dots sit, how they
;; stack and the padding:

(nu/options-table (describe/own-rows (describe/options (chart/dotplot))))

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

;; A style - a fixed dot size, outlined squares - and the padding:

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

(nu/shared-groups (describe/options (chart/dotplot)))

;; ## rug

;; A short line per value - along the panel's edge, or one strip per level; often beside another chart.

(nu/options-table (describe/mapping (chart/rug)))

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :sepal-width}
                    :charts [(chart/scatter) (chart/rug {:mapping {:y nil}})]}
                   {:width 600 :height 320})

;; Its options: the line colour and width, and the padding:

(nu/options-table (describe/own-rows (describe/options (chart/rug))))

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

(nu/shared-groups (describe/options (chart/rug)))
