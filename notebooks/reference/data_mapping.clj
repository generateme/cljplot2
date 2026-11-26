;; # Data and mapping

;; Plot data is a sequence of maps (rows) or a map of equal-length sequences (columns). A mapping says which value
;; of a row each dimension reads. The plot's mapping is merged under each chart's own, and a chart reads only the keys it uses - its page lists them (`(describe/mapping (chart/bar))`).
;; The mapping is open: a chart of your own may read other keys.

(ns reference.data-mapping
  (:require [fastmath.core :as m]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.levels :as levels]
            [cljplot2.data.rows :as rows]
            [cljplot2.describe :as describe]
            [cljplot2.pipeline :as pipeline]
            [notebook-utils :as nu]))

;; The table below holds a made-up wave as two columns; the first section draws it:

(def columns {:t (range 20) :v (map #(m/sin (m/* 0.4 %)) (range 20))})

(nu/data-table columns)

;; The table below holds the iris flowers; the position, aesthetic and facet sections draw them:

(nu/source-note {:dataset :iris})

(def iris (nu/dataset :iris))

(nu/data-table iris)

;; The table below holds made-up yearly visitor counts, the years as `java.time.Year` values; the position section
;; draws them as levels:

(def yearly (for [y (range 2015 2023)] {:year (java.time.Year/of y) :visitors (+ 40 (* 7 (mod (* y 5) 9)))}))

(nu/data-table yearly)

;; The table below holds a made-up curve with a band around it; the interval section draws it:

(def band (for [t (range 0 21)] {:t t :v (* 0.1 t t) :lo (- (* 0.1 t t) 3) :hi (+ (* 0.1 t t) 3)}))

(nu/data-table band)

;; The table below holds made-up daily prices; the price section draws them:

(def prices (reductions (fn [{:keys [close]} d]
                          (let [open close
                                close (+ open (- (mod (* d 37) 11) 5.0))]
                            {:day d :open open :close close
                             :high (+ (max open close) (mod d 3)) :low (- (min open close) (mod d 4))}))
                        {:day 0 :open 100.0 :close 100.0 :high 101.0 :low 99.0}
                        (range 1 25)))

(nu/data-table prices)

;; The tables below hold four made-up walks, one without a number, and overlapping points of four sizes; the group
;; and order section draws them:

(def walks (for [w (range 4) t (range 12)] {:walk (when (pos? w) w) :t t :v (+ w (* t 0.3 (inc w)) (mod (* t w) 3))}))

(nu/data-table walks)

(def overlapping (for [i (range 24)] {:x (* 0.2 i) :y (* 0.2 i) :s (- 3 (mod i 4))}))

(nu/data-table overlapping)

;; The tables below hold a made-up budget of departments and teams and made-up set memberships; the last section
;; draws them:

(def budget [{:dept "science" :team "physics" :cost 40} {:dept "science" :team "biology" :cost 25}
             {:dept "arts" :team "music" :cost 15} {:dept "arts" :team "theatre" :cost 10}
             {:dept "sport" :team "football" :cost 20}])

(nu/data-table budget)

(def members (for [[set items] {"reads" ["ann" "bob" "cid" "dan"] "writes" ["bob" "dan" "eve"]}
                   item items]
               {:set set :item item}))

(nu/data-table members)

;; ## Data: rows or columns

;; The plot's `:data` holds the rows the charts read. Columns become rows, so both forms draw the same plot. A chart
;; may bring its own `:data` and `:mapping`, the [Common options](reference.shared_options.html#common-options) every
;; chart takes:

(nu/option-cards [] (describe/rows-at (describe/options pipeline/plot-schema) #{[:data]}))

;; The plot below draws the wave's columns and two points from the scatter chart's own rows:

(plot/render-image {:data columns
                    :mapping {:x :t :y :v}
                    :charts [(chart/line)
                             (chart/scatter {:data [{:t 4 :v 1.0} {:t 12 :v -1.0}] :size 14 :color [200 50 50 120]})]}
                   {:width 500 :height 280})

;; ## Position

;; These keys place a mark. Every mapping key takes an accessor: a keyword (the row's value under it), a function of
;; the row that computes the value, or either wrapped in `(levels/categorical acc)`, which reads numbers or dates as
;; levels. The plot below computes y as a ratio of two columns:

(nu/plot-mapping-list (describe/options rows/mapping-schema) :position)

(plot/render-image {:data iris
                    :mapping {:x :petal-length
                              :y #(/ (double (:petal-width %)) (double (:petal-length %)))}
                    :charts [(chart/scatter)]
                    :labels {:y "petal width / length"}}
                   {:width 500 :height 300})

;; The plots below round petal lengths to whole numbers and read them as numbers (left, a continuous axis) and as
;; levels (right, one band each):

(plot/render-image (plot/arrange [{:data iris
                                   :mapping {:x #(m/round (:petal-length %)) :y :sepal-length}
                                   :charts [(chart/scatter)]}
                                  {:data iris
                                   :mapping {:x (levels/categorical #(m/round (:petal-length %)))
                                             :y :sepal-length}
                                   :charts [(chart/box)]}]
                                 {:columns 2})
                   {:width 800 :height 300})

;; The plot below reads dates as levels: one bar per year, labelled by the year. Without `levels/categorical` the
;; `Year` values would sit on a time axis:

(plot/render-image {:data yearly
                    :mapping {:x (levels/categorical :year) :y :visitors}
                    :charts [(chart/bar)]}
                   {:width 600 :height 300})

;; ## Intervals and segments

;; These keys give the other end of a mark spanning two values: a band from `:ymin` to `:ymax`, a segment to `:xend`
;; and `:yend`. The plot below draws a band from `:lo` to `:hi` around the curve and, from every fifth point, a
;; segment down to 0 - a constant end set by the [segment](reference.charts_points_lines.html#line-segments) option
;; `:yend 0`, not a mapping:

(nu/plot-mapping-list (describe/options rows/mapping-schema) :interval)

(plot/render-image {:data band
                    :mapping {:x :t :y :v :ymin :lo :ymax :hi}
                    :charts [(chart/ribbon)
                             (chart/line)
                             (chart/segment {:data (filter #(zero? (mod (long (:t %)) 5)) band)
                                             :mapping {:xend :t :ymin nil :ymax nil}
                                             :yend 0})]}
                   {:width 500 :height 280})

;; ## Prices

;; These keys give the four prices of a period, which the candlestick chart draws:

(nu/plot-mapping-list (describe/options rows/mapping-schema) :ohlc)

(plot/render-image {:data prices
                    :mapping {:x :day :open :open :high :high :low :low :close :close}
                    :charts [(chart/candlestick)]}
                   {:width 500 :height 280})

;; ## Aesthetics

;; These keys style marks by their values, each through its scale and legend
;; ([Aesthetics and legends](reference.aesthetics_legends.html)). The plot below maps colour and shape to the species
;; and size to the petal width:

(nu/plot-mapping-list (describe/options rows/mapping-schema) :aesthetic)

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :sepal-width :color :species :shape :species :size :petal-width}
                    :charts [(chart/scatter)]}
                   {:width 600 :height 350})

;; ## Groups and order

;; These keys group rows and order points. Charts that draw one mark per group (lines, areas, densities ...) draw
;; each group separately; an aesthetic mapped to levels groups the rows too. The plot below draws a line per walk:

(nu/plot-mapping-list (describe/options rows/mapping-schema) :groups-and-order)

(plot/render-image {:data walks
                    :mapping {:x :t :y :v :group :walk}
                    :charts [(chart/line {:color [70 70 70]})]}
                   {:width 450 :height 280})

;; The points below repeat a run from largest to smallest. In data order each run's small ones are drawn last, on top
;; (left); ordered by size (ascending, the scatter option `:descending false`, the default) the large ones come last
;; and cover them (right):

(plot/render-image (plot/arrange [{:data overlapping
                                   :mapping {:x :x :y :y :size :s :color :s}
                                   :charts [(chart/scatter)]
                                   :scales {:size {:range [6 40]} :color {:type :categorical}}
                                   :legend false
                                   :labels {:title "data order"}}
                                  {:data overlapping
                                   :mapping {:x :x :y :y :size :s :color :s :order :s}
                                   :charts [(chart/scatter)]
                                   :scales {:size {:range [6 40]} :color {:type :categorical}}
                                   :legend false
                                   :labels {:title ":order :s - larger on top"}}]
                                 {:columns 2})
                   {:width 900 :height 350})

;; The plot's `:groups` key sets what happens to rows whose group value is `nil`; the bottom line of the walks above is
;; the walk without a number. The plot below drops such rows:

(nu/option-cards [] (describe/rows-under (describe/options pipeline/plot-schema) [:groups]))

(plot/render-image {:data walks
                    :mapping {:x :t :y :v :group :walk}
                    :charts [(chart/line {:color [70 70 70]})]
                    :groups {:na false}}
                   {:width 450 :height 280})

;; ## Facets

;; These keys split the plot into panels: a wrap of panels or a grid of columns and rows
;; ([Facets, multiplots and margins](reference.facets_arrange.html)). The plot below draws a panel per species:

(nu/plot-mapping-list (describe/options rows/mapping-schema) :facet)

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :sepal-width :facet :species}
                    :charts [(chart/scatter)]}
                   {:width 700 :height 280})

;; The plot below draws a grid: species as columns, wide and narrow sepals as rows:

(plot/render-image {:data iris
                    :mapping {:x :petal-length :y :petal-width :facet-col :species
                              :facet-row #(if (> (double (:sepal-width %)) 3.0) "wide sepal" "narrow sepal")}
                    :charts [(chart/scatter)]}
                   {:width 700 :height 400})

;; ## Values, sets and hierarchies

;; These keys belong to particular charts. The plot below draws the budget as a treemap and the memberships as a Venn
;; diagram:

(nu/plot-mapping-list (describe/options rows/mapping-schema) :value)

(plot/render-image (plot/arrange [{:data budget
                                   :mapping {:hierarchy [:dept :team] :value :cost}
                                   :charts [(chart/treemap)]}
                                  {:data members
                                   :mapping {:set :set :item :item}
                                   :charts [(chart/venn)]}]
                                 {:columns 2})
                   {:width 800 :height 350})
