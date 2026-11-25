;; # Charts: bars and ranges

;; Strip charts drawing one item per level of a categorical axis: a bar of a summary (`bar`), an interval with its
;; centre (`extent`) and a bullet - a measure over qualitative ranges with markers (`bullet`). Each chart lists the
;; mapping keys it reads, its own options and the shared option groups it takes.

(ns reference.charts-bars-ranges
  (:require [fastmath.random :as r]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.levels :as levels]
            [cljplot2.describe :as describe]
            [fastmath.stats :as stats]
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
;; mirrored bars (`:mirror` - [Mirrored strips](reference.shared_options.html#mirrored-strips)):

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

;; Every `:stat`: summaries of the mapped values (`:sum` - the default, `:mean`, `:median`) and counts of the rows,
;; needing no values (`:count`, `:percent` of the panel's rows, `:pmf` - shares summing to 1):

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

;; Groups side by side (`:dodge`, the default for several groups of a level) or stacked; with negative values a
;; stack grows both ways from 0 - `:negative-order :reverse` puts the last group nearest 0 below it. Mirrored bars
;; drop their 0 line with `:zero-line false`, and `:padding` widens the value axis (here 30% past the longest bar):

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

;; A numeric `:color` doesn't group the bars: each bar is filled by its own colour value - `:color-stat` over its
;; rows' colour values (default the bar's `:stat`, `:mean` for counts) - with a colour bar. Mapped to the values with
;; `:sum` it colours each bar by its length; stacked segments (groups from `:group` or a categorical aesthetic) take
;; their own colours; a count can be coloured by a summary of another column:

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
;; outside the domain is drawn in the NA colour (Q4's mean, 45.7, is above 40). A `:value-label` function reads the
;; bar's colour value as `:color-value`:

(plot/render-image {:data sales
                    :mapping {:x :quarter :y :amount :color :amount}
                    :charts [(chart/bar {:color-stat :mean :value-label #(format "mean %.1f" (:color-value %))})]
                    :scales {:color {:domain [15 40]}}
                    :labels {:title "colour :domain [15 40]"}}
                   {:width 500 :height 320})

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

;; Where labels sit: past the bar's end (`:end`, the default unstacked) or inside it (`:center`, the default
;; stacked; `:base` - at its base, shown further down); with the value and the row count (`:n-label`) the two
;; texts join side by side or one above the other (`:label-stack`):

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

;; `:base` writes the label inside the bar at its base (the end nearer 0), reading from the axis - each segment's
;; own base when stacked. With 0 outside the value domain (e.g. `:scales {:x {:domain [4 nil]}}`) the label sits at
;; the visible end nearest 0; in a polar plot with the values on the radius the bases meet at the centre, where
;; labels rarely fit their wedge and are skipped:

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

;; In a polar plot `:outer` puts the labels past the outer radius, with a leader line from each bar
;; (`:label-leader`):

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

;; Its options: the shades of ranges and measures (from the first to the last), the measure bar's width, the
;; markers' colour, width and length, and the padding (left: 20% past the largest value):

(nu/options-table (describe/own-rows (describe/options (chart/bullet))))

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

(nu/shared-groups (describe/options (chart/bullet)))
