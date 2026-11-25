;; # Levels

;; Functions turning values into levels before plotting: numbers binned into intervals, numbers or dates read as
;; levels, levels ordered by a statistic, rows ranked within each x, and overlapping intervals (shingles) for
;; conditioning. Each returns rows, a vector of levels (`order-levels`) or a mapping marker (`categorical`) for an
;; ordinary plot; options are checked against the table shown.

(ns reference.data-levels
  (:require [fastmath.stats :as stats]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.levels :as levels]
            [cljplot2.data.shingle :as shingle]
            [cljplot2.describe :as describe]
            [notebook-utils :as nu]))

(nu/source-note {:dataset :mtcars})

(def mtcars (nu/dataset :mtcars))

(nu/data-table mtcars)

(nu/source-note {:dataset :iris})

(def iris (nu/dataset :iris))

(nu/data-table iris)

;; ## levels/bin

;; Adds the bin of a numeric value to every row - an interval `[lo, hi)` (the last one closed), printed like R's
;; `cut`; a missing or non-numeric value gets no bin.

(nu/options-table (describe/options #'levels/bin))

;; The bins of the cars' horsepower - five equal-width bins, bounds printed with 2 significant digits - as a new
;; column:

(nu/data-table (levels/bin mtcars :hp {:as :hp-bin :bins 5 :digits 2}))

;; Bins are levels: counted by a [bar](reference.charts_bars.html#bar) chart (`:stat :count`), in interval
;; order. Explicit breaks (left), quantile bins of equal counts (middle), and the series name printed in every bin
;; (right):

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

;; ## levels/categorical

;; A mapping marker: the accessor reads its values as usual, and its dimension is declared categorical - numbers or
;; dates become levels (bands on a position axis, an ordinal legend for an aesthetic). It takes no options.

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

;; ## levels/order-levels

;; The levels of a column ordered by a statistic of a value in each level - for a scale `:domain`.

(nu/options-table (describe/options #'levels/order-levels))

;; Iris species by their median petal length, ascending and descending:

(levels/order-levels iris :species :petal-length stats/median)

(levels/order-levels iris :species :petal-length stats/median {:descending true})

;; The descending order as the domain of the level axis - the cars' carburettor counts, highest median mpg first:

(plot/render-image {:data mtcars
                    :mapping {:x (levels/categorical :carb) :y :mpg}
                    :charts [(chart/box)]
                    :scales {:x {:domain (levels/order-levels mtcars :carb :mpg stats/median {:descending true})}}
                    :labels {:title "carburettors by median mpg"}}
                   {:width 600 :height 320})

;; ## levels/rank-rows

;; Ranks the rows sharing an x by a value - rank 1 the highest by default - e.g. for a bump chart (a line through
;; the ranks on a reversed y domain).

(nu/options-table (describe/options #'levels/rank-rows))

(def sales (for [[year amounts] [[2020 [5 9 7]] [2021 [8 9 4]] [2022 [9 6 6]] [2023 [7 10 5]]]
                 [product amount] (map vector ["apples" "pears" "plums"] amounts)]
             {:year year :product product :amount amount}))

(nu/data-table sales)

;; The ranks under `:place`, ties taking the lowest rank (2022: pears and plums share 2):

(nu/data-table (levels/rank-rows sales :year :amount {:as :place :ties :min}))

;; A bump chart of the ranks (left; rank 1 on top with the y scale's `:reverse true`, see
;; [Order and direction](reference.scales_axes_coords.html#order-and-direction)), and ranks counted from the lowest
;; amount with average ties (right):

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

;; ## shingle/shingle

;; Repeats every row once per interval containing its value - overlapping intervals (a lattice shingle) to
;; condition on a continuous variable, typically as a facet.

(nu/options-table (describe/options #'shingle/shingle))

;; Equal-count intervals of the cars' weight - 3 of them, neighbours sharing a quarter of their points - as a facet
;; of mpg against horsepower ([facets](reference.data_mapping.html#facets)); the series name is printed in every
;; strip:

(plot/render-image {:data (shingle/shingle mtcars :wt {:as :weight :n 3 :overlap 0.25 :label true})
                    :mapping {:x :hp :y :mpg :facet-col :weight}
                    :charts [(chart/scatter)]}
                   {:width 900 :height 300})

;; Explicit intervals instead, here overlapping by hand:

(nu/data-table (shingle/shingle mtcars :wt {:as :weight :intervals [[1.5 3.0] [2.5 4.0] [3.5 5.5]]}))
