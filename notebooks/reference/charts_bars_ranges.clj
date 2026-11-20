;; # Charts: bars and ranges

;; Strip charts drawing one item per level of a categorical axis: a bar of a summary (`bar`), an interval with its
;; centre (`extent`) and a bullet - a measure over qualitative ranges with markers (`bullet`). Each chart lists the
;; mapping keys it reads, its own options and the shared option groups it takes.

(ns reference.charts-bars-ranges
  (:require [fastmath.random :as r]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [notebook-utils :as nu]))

(nu/source-note {:dataset :iris})

(def iris (nu/dataset :iris))

(nu/data-table iris)

(def sales (for [[qi q] (map-indexed vector ["Q1" "Q2" "Q3" "Q4"]) [i product] (map-indexed vector ["apples" "pears" "plums"])]
             {:quarter q :product product :amount (+ 10 (* 6 i) (* 8 qi) (mod (* 7 (+ qi (* 2 i))) 11))}))

(nu/data-table sales)

;; ## bar

;; A bar per level (and group) whose length summarises the item's rows - their sum by default, or a count of
;; them; vertical bars take the levels on x, horizontal ones on y.

(nu/options-table (describe/mapping (chart/bar)))

(plot/render-image {:data sales
                    :mapping {:x :quarter :y :amount :color :product}
                    :charts [(chart/bar)]
                    :labels {:y "amount"}}
                   {:width 600 :height 350})

;; ### Style

;; The fill and outline used when nothing is mapped, the opacity of overlaid groups, and the 0 line drawn over
;; mirrored bars (`:mirror` - Mirrored strips):

(nu/options-table (describe/own-rows (describe/options (chart/bar))) :style)

(def pyramid (for [[i age] (map-indexed vector ["0-19" "20-39" "40-59" "60-79" "80+"])
                   sex ["female" "male"]]
               {:age age :sex sex :people (- 40 (* 7 i) (if (= sex "male") (* 2 i) 0))}))

(nu/data-table pyramid)

(plot/render-image (plot/arrange [{:data sales
                                   :mapping {:x :quarter :y :amount}
                                   :charts [(chart/bar {:color [230 160 60] :stroke-color :black :stroke-width 2})]
                                   :labels {:title "fill and outline"}}
                                  {:data sales
                                   :mapping {:x :quarter :y :amount :color :product}
                                   :charts [(chart/bar {:position :overlay :alpha 0.35})]
                                   :labels {:title ":overlay, :alpha 0.35"}
                                   :legend false}
                                  {:data pyramid
                                   :mapping {:x :people :y :age :color :sex}
                                   :charts [(chart/bar {:mirror :color :zero-line-color :red :zero-line-width 3})]
                                   :scales {:y {:reverse true}}
                                   :labels {:title "mirrored, red 0 line"}}]
                                 {:columns 3 :widths [1 1 1.3]})
                   {:width 1000 :height 320})

;; ### Bars

;; What a bar's length is (`:stat`), how the groups of one level share its band, widths by the item's size, the
;; order of negative stacked parts, and the padding:

(nu/options-table (describe/own-rows (describe/options (chart/bar))) :bars)

(plot/render-image (plot/arrange [{:data iris
                                   :mapping {:x :species :y :sepal-length}
                                   :charts [(chart/bar {:stat :mean})]
                                   :labels {:title ":stat :mean"}}
                                  {:data sales
                                   :mapping {:x :quarter :y :amount :color :product}
                                   :charts [(chart/bar {:position :stack})]
                                   :labels {:title ":position :stack"}
                                   :legend false}
                                  {:data sales
                                   :mapping {:x :quarter :y :amount}
                                   :charts [(chart/bar {:varwidth :amount})]
                                   :labels {:title ":varwidth :amount"}}]
                                 {:columns 3})
                   {:width 1000 :height 320})

;; ### Labels

;; Each bar's length written past its end or in its middle, formatted or as the text a function returns:

(nu/options-table (describe/own-rows (describe/options (chart/bar))) :labels)

(plot/render-image (plot/arrange [{:data sales
                                   :mapping {:x :quarter :y :amount :color :product}
                                   :charts [(chart/bar {:value-label true})]
                                   :labels {:title ":value-label true"}
                                   :legend false}
                                  {:data sales
                                   :mapping {:x :quarter :y :amount :color :product}
                                   :charts [(chart/bar {:position :normalize :value-label #(format "%.0f%%" (* 100 (:share %)))})]
                                   :labels {:title ":normalize, shares in %"}
                                   :legend false}
                                  {:data iris
                                   :mapping {:x :species :y :petal-length}
                                   :charts [(chart/bar {:stat :mean :value-label true :value-format "%.2f cm"})]
                                   :labels {:title ":value-format"}}]
                                 {:columns 3})
                   {:width 1000 :height 320})

(nu/shared-groups (describe/options (chart/bar)))

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
;; bootstrap's statistic and random generator (seeded here) and the centre mark:

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

(nu/shared-groups (describe/options (chart/extent)))

;; ## bullet

;; A measure bar over shaded qualitative ranges, with marker lines (targets) - a row per level holding vectors of
;; ranges, measures and markers (here percents of a plan, so the levels share one axis).

(nu/options-table (describe/mapping (chart/bullet)))

(def kpis [{:kpi "revenue" :ranges [60 80 100] :measures [92 75] :markers [95]}
           {:kpi "profit" :ranges [50 75 100] :measures [68 60] :markers [80]}
           {:kpi "customers" :ranges [70 85 100] :measures [88] :markers [90 96]}])

(nu/data-table kpis)

(plot/render-image {:data kpis
                    :mapping {:y :kpi :ranges :ranges :measures :measures :markers :markers}
                    :charts [(chart/bullet)]
                    :scales {:y {:reverse true}}
                    :labels {:x "% of plan"}}
                   {:width 600 :height 260})

;; Its options: the shades of ranges and measures (from the first to the last), the measure bar's width and the
;; markers' colour, width and length:

(nu/options-table (describe/own-rows (describe/options (chart/bullet))))

(plot/render-image (plot/arrange [{:data kpis
                                   :mapping {:y :kpi :ranges :ranges :measures :measures :markers :markers}
                                   :charts [(chart/bullet {:measure-width 0.2 :marker-width 0})]
                                   :scales {:y {:reverse true}}
                                   :labels {:title "thin measures, :marker-width 0"}}
                                  {:data kpis
                                   :mapping {:y :kpi :ranges :ranges :measures :measures :markers :markers}
                                   :charts [(chart/bullet {:range-colors [[255 230 200] [255 250 240]]
                                                           :measure-colors [[180 60 30] [230 150 120]]
                                                           :measure-width 0.6 :marker-color [30 30 160]
                                                           :marker-width 4 :marker-length 0.9})]
                                   :scales {:y {:reverse true}}
                                   :labels {:title "custom colours and sizes"}}]
                                 {:columns 2})
                   {:width 900 :height 260})

(nu/shared-groups (describe/options (chart/bullet)))
