;; # Levels

;; These functions turn values into levels before plotting: they bin numbers into intervals, read numbers or dates as
;; levels, order levels by a statistic, rank rows within each x and cut overlapping intervals (shingles). Use them to
;; prepare categorical data for an ordinary plot. Most return rows; `order-levels` returns a vector of levels and
;; `categorical` a mapping marker.

(ns reference.data-levels
  (:require [fastmath.stats :as stats]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.levels :as levels]
            [cljplot2.data.shingle :as shingle]
            [cljplot2.describe :as describe]
            [notebook-utils :as nu]))

;; The tables below hold the mtcars cars and the iris flowers; the sections use them:

(nu/source-note {:dataset :mtcars})

(def mtcars (nu/dataset :mtcars))

(nu/data-table mtcars)

(nu/source-note {:dataset :iris})

(def iris (nu/dataset :iris))

(nu/data-table iris)

;; The table below holds made-up yearly sales of three products; the ranking section uses it:

(def sales (for [[year amounts] [[2020 [5 9 7]] [2021 [8 9 4]] [2022 [9 6 6]] [2023 [7 10 5]]]
                 [product amount] (map vector ["apples" "pears" "plums"] amounts)]
             {:year year :product product :amount amount}))

(nu/data-table sales)

;; ## Binning

;; `levels/bin` adds the bin of a numeric value to every row: an interval `[lo, hi)`, the last one closed, printed as
;; R's `cut` prints it. A missing or non-numeric value gets no bin. Use it to count or colour by ranges of a number.

(nu/argument-list #'levels/bin)

;; These options name the bin column, choose the bins and set how their bounds print. The table below adds five
;; equal-width bins of the cars' horsepower, their bounds printed with 2 significant digits:

(nu/option-cards '(levels/bin rows :x) (describe/options #'levels/bin))

(nu/result-table (levels/bin mtcars :hp {:as :hp-bin :bins 5 :digits 2}))

;; Bins are levels, so a [bar](reference.charts_bars.html#bar-charts) chart with `:stat :count` counts them in
;; interval order. The left panel below cuts at explicit breaks, the middle one into quantile bins of about equal counts; the
;; right one prints the series name in every bin:

(plot/render-image (plot/arrange [{:data (levels/bin mtcars :hp {:as :bin :bins [50 100 150 200 350]})
                                   :mapping {:x :bin}
                                   :charts [(chart/bar {:stat :count})]
                                   :labels {:title ":bins [50 100 150 200 350]"}}
                                  {:data (levels/bin mtcars :hp {:as :bin :bins {:quantiles 4}})
                                   :mapping {:x :bin}
                                   :charts [(chart/bar {:stat :count})]
                                   :labels {:title ":bins {:quantiles 4}"}}
                                  {:data (levels/bin mtcars :hp {:as :bin :bins :sturges :label true})
                                   :mapping {:y :bin}
                                   :charts [(chart/bar {:stat :count})]
                                   :scales {:y {:reverse true}}
                                   :labels {:title ":bins :sturges, :label true"}}]
                                 {:columns 3 :widths [1 1 1.3]})
                   {:width 1000 :height 320})

;; ## Categorical mapping

;; `levels/categorical` wraps an accessor as a mapping marker: the accessor reads its values as usual, and the mapping
;; treats its dimension as categorical. Use it to show numbers or dates as levels: bands on a position axis, an ordinal
;; legend for an aesthetic.

(nu/argument-list #'levels/categorical)

;; The left panel below maps the cylinder counts as numbers, the right one as levels:

(plot/render-image (plot/arrange [{:data mtcars
                                   :mapping {:x :cyl :y :mpg}
                                   :charts [(chart/scatter)]
                                   :labels {:title ":x :cyl - numbers"}}
                                  {:data mtcars
                                   :mapping {:x (levels/categorical :cyl) :y :mpg :color (levels/categorical :gear)}
                                   :charts [(chart/box)]
                                   :labels {:title ":x (levels/categorical :cyl), colour by gear"}}]
                                 {:columns 2 :widths [1 1.25]})
                   {:width 900 :height 320})

;; ## Level order

;; `levels/order-levels` returns the levels of a column ordered by a statistic of a value in each level. Use it as a
;; scale `:domain` to sort a level axis or a legend.

(nu/argument-list #'levels/order-levels)

;; This option sets the direction. The calls below order the iris species by their median petal length, ascending,
;; then descending:

(nu/option-cards '(levels/order-levels rows :level :value stats/mean) (describe/options #'levels/order-levels))

(levels/order-levels iris :species :petal-length stats/median)

(levels/order-levels iris :species :petal-length stats/median {:descending true})

;; The plot below takes the descending order as the domain of its level axis, so the carburettor count with the
;; highest median mpg comes first:

(plot/render-image {:data mtcars
                    :mapping {:x (levels/categorical :carb) :y :mpg}
                    :charts [(chart/box)]
                    :scales {:x {:domain (levels/order-levels mtcars :carb :mpg stats/median {:descending true})}}
                    :labels {:title "carburettors by median mpg"}}
                   {:width 600 :height 320})

;; ## Ranks

;; `levels/rank-rows` ranks the rows sharing an x by a value; by default the highest value ranks 1. Use it for a bump
;; chart: a line through the ranks on a reversed y axis.

(nu/argument-list #'levels/rank-rows)

;; These options name the rank column and set the direction and the ties. The table below ranks the sales per year
;; under `:place`; a tie takes the lowest rank, so pears and plums share rank 2 in 2022:

(nu/option-cards '(levels/rank-rows rows :x :value) (describe/options #'levels/rank-rows))

(nu/result-table (levels/rank-rows sales :year :amount {:as :place :ties :min}))

;; The left panel below draws a bump chart of the ranks, rank 1 on top through the y scale's `:reverse true` (see
;; [Order and direction](reference.scales_axes_coords.html#order-and-direction)). The right one counts the ranks
;; from the lowest amount and averages the ties:

(plot/render-image (plot/arrange [{:data (levels/rank-rows sales :year :amount)
                                   :mapping {:x :year :y :rank :color :product}
                                   :charts [(chart/line {:line-width 3}) (chart/scatter {:size 12})]
                                   :scales {:y {:domain [0.7 3.3] :reverse true :ticks [1 2 3]}}
                                   :labels {:title "ranks, highest first" :y "rank"}
                                   :legend false}
                                  {:data (levels/rank-rows sales :year :amount {:descending false :ties :average})
                                   :mapping {:x :year :y :rank :color :product}
                                   :charts [(chart/line {:line-width 3}) (chart/scatter {:size 12})]
                                   :scales {:y {:domain [0.7 3.3] :reverse true :ticks [1 1.5 2 2.5 3]}}
                                   :labels {:title ":descending false, :ties :average" :y "rank"}}]
                                 {:columns 2 :widths [1 1.25]})
                   {:width 900 :height 320})

;; ## Shingles

;; `shingle/shingle` cuts a numeric column into overlapping intervals, a lattice shingle, and repeats every row once per
;; interval holding its value. Use it to condition a plot on a continuous variable, usually as a facet.

(nu/argument-list #'shingle/shingle)

;; These options name the interval column, choose the intervals and set the series name. The plot below cuts the
;; cars' weight into 3 equal-count intervals, neighbours sharing a quarter of their points, and facets mpg against
;; horsepower by them ([facets](reference.data_mapping.html#facets)); every strip prints the series name:

(nu/option-cards '(shingle/shingle rows :x) (describe/options #'shingle/shingle))

(plot/render-image {:data (shingle/shingle mtcars :wt {:as :weight :n 3 :overlap 0.25 :label true})
                    :mapping {:x :hp :y :mpg :facet-col :weight}
                    :charts [(chart/scatter)]}
                   {:width 900 :height 300})

;; The table below takes explicit intervals instead; neighbours overlap by 0.5:

(nu/result-table (shingle/shingle mtcars :wt {:as :weight :intervals [[1.5 3.0] [2.5 4.0] [3.5 5.5]]}))
