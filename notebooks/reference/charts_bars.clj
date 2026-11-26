;; # Bars

;; These charts draw bars along a categorical axis: a summary of each level's values (`bar`) or a measure against
;; qualitative ranges and targets (`bullet`). Use them to compare amounts across levels.

(ns reference.charts-bars
  (:require [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.levels :as levels]
            [cljplot2.describe :as describe]
            [notebook-utils :as nu]))

(nu/source-note {:dataset :iris})

(def iris (nu/dataset :iris))

(nu/data-table iris)

(def sales (for [[qi q] (map-indexed vector ["Q1" "Q2" "Q3" "Q4"]) [i product] (map-indexed vector ["apples" "pears" "plums"])]
             {:quarter q :product product :amount (+ 10 (* 6 i) (* 8 qi) (mod (* 7 (+ qi (* 2 i))) 11))}))

(nu/data-table sales)

(nu/source-note {:dataset :mtcars})

(def mtcars-cyl (map #(select-keys % [:model :cyl]) (nu/dataset :mtcars)))

(nu/data-table mtcars-cyl)

;; ## Bar charts

;; `chart/bar` draws a bar per level and group; by default its length is the sum of its rows' values, and it can also
;; summarise or count the rows. Use it to compare amounts across categories. Levels on x give vertical bars, levels
;; on y horizontal ones.

(nu/shared-groups (describe/options (chart/bar)))

(nu/mapping-list (describe/mapping (chart/bar)))

(plot/render-image {:data sales
                    :mapping {:x :quarter :y :amount :color :product}
                    :charts [(chart/bar)]
                    :labels {:y "amount"}}
                   {:width 600 :height 350})

;; ### Style

;; These options set the fill and outline of unmapped bars, the opacity of overlaid groups and the 0 line over
;; mirrored bars (`:mirror`, see [Mirrored strips](reference.shared_options.html#mirrored-strips)):

(nu/option-cards 'chart/bar (describe/own-rows (describe/options (chart/bar))) :style)

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

;; These options set the bars' length, colour value and width, how the groups of a level share its band, and the
;; padding:

(nu/option-cards 'chart/bar (describe/own-rows (describe/options (chart/bar))) :bars)

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

;; Each `:stat` in turn: on the left, summaries of the iris sepal lengths; on the right, counts of the cars per
;; number of cylinders:

(plot/render-image (plot/arrange [{:data iris
                                   :mapping {:x :species :y :sepal-length}
                                   :charts [(chart/bar)]
                                   :labels {:title ":sum (default)"}}
                                  {:data iris
                                   :mapping {:x :species :y :sepal-length}
                                   :charts [(chart/bar {:stat :median})]
                                   :labels {:title ":stat :median"}}
                                  {:data mtcars-cyl
                                   :mapping {:x (levels/categorical :cyl)}
                                   :charts [(chart/bar {:stat :count})]
                                   :labels {:title ":stat :count"}}
                                  {:data mtcars-cyl
                                   :mapping {:x (levels/categorical :cyl)}
                                   :charts [(chart/bar {:stat :percent})]
                                   :labels {:title ":stat :percent"}}
                                  {:data mtcars-cyl
                                   :mapping {:x (levels/categorical :cyl)}
                                   :charts [(chart/bar {:stat :pmf})]
                                   :labels {:title ":stat :pmf"}}]
                                 {:columns 5})
                   {:width 1000 :height 280})

;; The first image draws a balance side by side, then stacked: with negative values a stack grows both ways from 0,
;; and `:negative-order :reverse` puts the last group nearest 0. The second image draws mirrored bars without their 0
;; line (left) and leaves 30% past the longest bar with `:padding` (right):

(def balance [{:month "Jan" :item "sales" :eur 40} {:month "Jan" :item "costs" :eur -25} {:month "Jan" :item "tax" :eur -8}
              {:month "Feb" :item "sales" :eur 55} {:month "Feb" :item "costs" :eur -30} {:month "Feb" :item "tax" :eur -12}
              {:month "Mar" :item "sales" :eur 35} {:month "Mar" :item "costs" :eur -20} {:month "Mar" :item "tax" :eur -6}])

(nu/data-table balance)

(plot/render-image (plot/arrange [{:data balance
                                   :mapping {:x :month :y :eur :color :item}
                                   :charts [(chart/bar {:position :dodge})]
                                   :labels {:title ":position :dodge"}
                                   :legend false}
                                  {:data balance
                                   :mapping {:x :month :y :eur :color :item}
                                   :charts [(chart/bar {:position :stack :negative-order :forward})]
                                   :labels {:title ":stack, :negative-order :forward"}
                                   :legend false}
                                  {:data balance
                                   :mapping {:x :month :y :eur :color :item}
                                   :charts [(chart/bar {:position :stack :negative-order :reverse})]
                                   :labels {:title ":negative-order :reverse"}}]
                                 {:columns 3 :widths [1 1 1.3]})
                   {:width 1000 :height 320})

(plot/render-image (plot/arrange [{:data pyramid
                                   :mapping {:x :people :y :age :color :sex}
                                   :charts [(chart/bar {:mirror :color :zero-line false})]
                                   :scales {:y {:reverse true}}
                                   :labels {:title ":zero-line false"}
                                   :legend false}
                                  {:data sales
                                   :mapping {:x :quarter :y :amount}
                                   :charts [(chart/bar {:padding {:y [0 0.3]}})]
                                   :labels {:title ":padding {:y [0 0.3]}"}}]
                                 {:columns 2})
                   {:width 900 :height 300})

;; A numeric `:color` doesn't group the bars: each bar takes its own colour value, shown on a colour bar. On the left
;; `:color-stat :sum` colours each bar by its length. In the middle each stacked segment takes the mean of its own
;; rows. On the right the bars count rows and take their colour from a summary of another column:

(plot/render-image (plot/arrange [{:data sales
                                   :mapping {:x :quarter :y :amount :color :amount}
                                   :charts [(chart/bar {:color-stat :sum})]
                                   :labels {:title ":color-stat :sum"}}
                                  {:data sales
                                   :mapping {:x :quarter :y :amount :group :product :color :amount}
                                   :charts [(chart/bar {:position :stack :color-stat :mean})]
                                   :labels {:title "stacked, :mean"}}
                                  {:data iris
                                   :mapping {:x :species :color :petal-length}
                                   :charts [(chart/bar {:stat :count :color-stat :median})]
                                   :labels {:title ":count, :median"}}]
                                 {:columns 3})
                   {:width 1000 :height 320})

;; An explicit colour `:domain` never drops rows: every bar keeps its length, and a bar whose colour value lies
;; outside the domain takes the NA colour (Q4's mean, 45.7, is above 40). A `:value-label` function reads the
;; bar's colour value as `:color-value`:

(plot/render-image {:data sales
                    :mapping {:x :quarter :y :amount :color :amount}
                    :charts [(chart/bar {:color-stat :mean :value-label #(format "mean %.1f" (:color-value %))})]
                    :scales {:color {:domain [15 40]}}
                    :labels {:title "colour :domain [15 40]"}}
                   {:width 500 :height 320})

;; ### Labels

;; These options turn on a label at each bar, set its text and place it:

(nu/option-cards 'chart/bar (describe/own-rows (describe/options (chart/bar))) :labels)

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

;; The first two panels place labels past the bar's end and in its middle; the last two join a value and a row count
;; (`:n-label`) side by side and one above the other:

(plot/render-image (plot/arrange [{:data iris
                                   :mapping {:x :species :y :sepal-length}
                                   :charts [(chart/bar {:stat :mean :value-label true :label-position :end})]
                                   :labels {:title ":label-position :end"}}
                                  {:data iris
                                   :mapping {:x :species :y :sepal-length}
                                   :charts [(chart/bar {:stat :mean :value-label true :label-position :center})]
                                   :labels {:title ":label-position :center"}}
                                  {:data iris
                                   :mapping {:x :species :y :sepal-length}
                                   :charts [(chart/bar {:stat :mean :value-label true :n-label true
                                                        :label-stack :horizontal})]
                                   :labels {:title ":label-stack :horizontal"}}
                                  {:data iris
                                   :mapping {:x :species :y :sepal-length}
                                   :charts [(chart/bar {:stat :mean :value-label true :n-label true
                                                        :label-stack :vertical})]
                                   :labels {:title ":label-stack :vertical"}}]
                                 {:columns 2})
                   {:width 900 :height 560})

;; A `:base` label starts at the end of the bar nearer 0, each stacked segment's at its own base. With 0 outside the value domain (e.g.
;; `:scales {:x {:domain [4 nil]}}`) the label sits at the visible end nearest 0. In a polar plot with the values on
;; the radius the bases meet at the centre, where labels rarely fit their wedge, so the chart leaves them out:

(plot/render-image (plot/arrange [{:data iris
                                   :mapping {:x :sepal-length :y :species}
                                   :charts [(chart/bar {:stat :mean :value-label true :label-position :base})]
                                   :labels {:title ":label-position :base"}}
                                  {:data sales
                                   :mapping {:x :quarter :y :amount :color :product}
                                   :charts [(chart/bar {:position :stack :value-label true :label-position :base})]
                                   :labels {:title ":stack, :label-position :base"}}]
                                 {:columns 2 :widths [1 1.3]})
                   {:width 900 :height 320})

;; In a polar plot `:outer` places the labels past the outer radius; on the right `:label-leader` adds a line from
;; each bar:

(plot/render-image (plot/arrange [{:data sales
                                   :mapping {:x :quarter :y :amount}
                                   :charts [(chart/bar {:value-label true :label-position :outer})]
                                   :coord {:kind :polar}
                                   :labels {:title ":label-position :outer"}}
                                  {:data sales
                                   :mapping {:x :quarter :y :amount}
                                   :charts [(chart/bar {:value-label true :label-position :outer :label-leader true})]
                                   :coord {:kind :polar}
                                   :labels {:title ":label-leader true"}}]
                                 {:columns 2})
                   {:width 800 :height 400})

;; ## Bullet charts

;; `chart/bullet` draws a measure bar over shaded qualitative ranges, with marker lines across them. Use it to show a
;; measure against its targets. Each row holds a level's ranges, measures and markers (here percents of a plan, so
;; the levels share one axis).

(nu/shared-groups (describe/options (chart/bullet)))

(nu/mapping-list (describe/mapping (chart/bullet)))

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

;; These options set the colours of the ranges and measures and the sizes of the measure bars and markers. On the
;; left the bars are thin, the markers hidden and `:padding` leaves 20% past the largest value; on the right every
;; colour and size is set:

(nu/option-cards 'chart/bullet (describe/own-rows (describe/options (chart/bullet))))

(plot/render-image (plot/arrange [{:data kpis
                                   :mapping {:y :kpi :ranges :ranges :measures :measures :markers :markers}
                                   :charts [(chart/bullet {:measure-fraction 0.2 :marker-width 0 :padding {:x [0 0.2]}})]
                                   :scales {:y {:reverse true}}
                                   :labels {:title "thin, no markers, :padding {:x [0 0.2]}"}}
                                  {:data kpis
                                   :mapping {:y :kpi :ranges :ranges :measures :measures :markers :markers}
                                   :charts [(chart/bullet {:range-colors [[255 230 200] [255 250 240]]
                                                           :measure-colors [[180 60 30] [230 150 120]]
                                                           :measure-fraction 0.6 :marker-color [30 30 160]
                                                           :marker-width 4 :marker-length 0.9})]
                                   :scales {:y {:reverse true}}
                                   :labels {:title "custom colours and sizes"}}]
                                 {:columns 2})
                   {:width 900 :height 260})

