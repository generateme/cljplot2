;; # Data and mapping

;; Plot data is a sequence of maps (rows) or a map of equal-length sequences (columns). A mapping says which value
;; of a row each dimension reads: a keyword or a function of the row. The plot's mapping is merged under each
;; chart's own, and a chart reads only the keys it uses - its page lists them (`(describe/mapping (chart/bar))`).
;; The mapping is open: a chart of your own may read other keys.

(ns reference.data-mapping
  (:require [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.levels :as levels]
            [cljplot2.data.rows :as rows]
            [cljplot2.describe :as describe]
            [cljplot2.pipeline :as pipeline]
            [notebook-utils :as nu]))

;; ## Data: rows or columns

;; Columns become rows; both forms give the same plot. A chart may bring its own `:data` (and `:mapping`) - the
;; Data and mapping group every chart takes:

(nu/options-table (describe/fragment-rows :common))

(def columns {:t (range 20) :v (map #(Math/sin (* 0.4 (double %))) (range 20))})

(nu/data-table columns)

(plot/render-image {:data columns
                    :mapping {:x :t :y :v}
                    :charts [(chart/line)
                             (chart/scatter {:data [{:t 4 :v 1.0} {:t 12 :v -1.0}] :size 14 :color [200 50 50 120]})]}
                   {:width 500 :height 280})

;; ## Position

;; The coordinates of a mark. Every mapping key takes an accessor: a keyword (the row's value under it), a
;; function of the row (computing the value - here y is a ratio of two columns), or a categorical marker of either,
;; `(levels/categorical acc)`, which reads numbers as levels (below).

(nu/options-table (describe/options rows/mapping-schema) :position)

(nu/source-note {:dataset :iris})

(def iris (nu/dataset :iris))

(nu/data-table iris)

(plot/render-image {:data iris
                    :mapping {:x :petal-length
                              :y #(/ (double (:petal-width %)) (double (:petal-length %)))}
                    :charts [(chart/scatter)]
                    :labels {:y "petal width / length"}}
                   {:width 500 :height 300})

;; The categorical marker: petal widths rounded to whole numbers, read as numbers (left - a continuous axis) and as
;; levels (right - one band each):

(plot/render-image (plot/arrange [{:data iris
                                   :mapping {:x #(Math/round (double (:petal-length %))) :y :sepal-length}
                                   :charts [(chart/scatter)]}
                                  {:data iris
                                   :mapping {:x (levels/categorical #(Math/round (double (:petal-length %))))
                                             :y :sepal-length}
                                   :charts [(chart/box)]}]
                                 {:columns 2})
                   {:width 800 :height 300})

;; ## Intervals and segments

;; The other end of a mark spanning two values: a band from `:ymin` to `:ymax` (ribbons, error bars), a segment
;; to `:xend` / `:yend`.

(nu/options-table (describe/options rows/mapping-schema) :interval)

(def band (for [t (range 0 21)] {:t t :v (* 0.1 t t) :lo (- (* 0.1 t t) 3) :hi (+ (* 0.1 t t) 3)}))

(nu/data-table band)

(plot/render-image {:data band
                    :mapping {:x :t :y :v :ymin :lo :ymax :hi}
                    :charts [(chart/ribbon)
                             (chart/line)
                             (chart/segment {:data (filter #(zero? (mod (long (:t %)) 5)) band)
                                             :mapping {:xend :t :ymin nil :ymax nil}
                                             :yend 0})]}
                   {:width 500 :height 280})

;; ## Prices

;; Opening, highest, lowest and closing values of a period - the candlestick chart.

(nu/options-table (describe/options rows/mapping-schema) :ohlc)

(def prices (reductions (fn [{:keys [close]} d]
                          (let [open close
                                close (+ open (- (mod (* d 37) 11) 5.0))]
                            {:day d :open open :close close
                             :high (+ (max open close) (mod d 3)) :low (- (min open close) (mod d 4))}))
                        {:day 0 :open 100.0 :close 100.0 :high 101.0 :low 99.0}
                        (range 1 25)))

(nu/data-table prices)

(plot/render-image {:data prices
                    :mapping {:x :day :open :open :high :high :low :low :close :close}
                    :charts [(chart/candlestick)]}
                   {:width 500 :height 280})

;; ## Aesthetics

;; Values styling marks: colour, shape, size, dash, fill pattern - each through its scale and legend (Aesthetics
;; and legends chapter).

(nu/options-table (describe/options rows/mapping-schema) :aesthetic)

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :sepal-width :color :species :shape :species :size :petal-width}
                    :charts [(chart/scatter)]}
                   {:width 600 :height 350})

;; ## Groups and order

;; `:group` splits rows into groups without styling them - charts drawing one mark per group (lines, areas,
;; densities ...) draw each separately; any aesthetic groups too. `:order` sets the drawing order of points.

(nu/options-table (describe/options rows/mapping-schema) :groups-and-order)

(def walks (for [w (range 4) t (range 12)] {:walk (when (pos? w) w) :t t :v (+ w (* t 0.3 (inc w)) (mod (* t w) 3))}))

(nu/data-table walks)

(plot/render-image {:data walks
                    :mapping {:x :t :y :v :group :walk}
                    :charts [(chart/line {:color [70 70 70]})]}
                   {:width 450 :height 280})

;; `:order`: overlapping points listed largest first - in data order the small ones are drawn last, on top (left);
;; ordered by size (ascending, the default `:order-direction`) the large ones come last and cover them (right):

(def overlapping (for [i (range 24)] {:x (* 0.2 i) :y (* 0.2 i) :s (- 3 (mod i 4))}))

(nu/data-table overlapping)

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

;; A `nil` group value forms its own NA level - the bottom line above is the walk without a number. The plot's
;; `:groups {:na false}` drops such rows instead:

(nu/options-table (describe/rows-under (describe/options pipeline/plot-schema) [:groups]))

(plot/render-image {:data walks
                    :mapping {:x :t :y :v :group :walk}
                    :charts [(chart/line {:color [70 70 70]})]
                    :groups {:na false}}
                   {:width 450 :height 280})

;; ## Facets

;; Values splitting the plot into panels: a wrap of panels (`:facet`) or a grid of columns and rows (Facets
;; chapter).

(nu/options-table (describe/options rows/mapping-schema) :facet)

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :sepal-width :facet :species}
                    :charts [(chart/scatter)]}
                   {:width 700 :height 280})

;; A grid: species as columns, wide and narrow sepals as rows:

(plot/render-image {:data iris
                    :mapping {:x :petal-length :y :petal-width :facet-col :species
                              :facet-row #(if (> (double (:sepal-width %)) 3.0) "wide sepal" "narrow sepal")}
                    :charts [(chart/scatter)]}
                   {:width 700 :height 400})

;; ## Values, sets and hierarchies

;; Keys of particular charts: `:value` (cell values, weights, leaf sizes), `:hierarchy` (the levels of a tree,
;; outer first) and `:set` / `:item` (set membership of Venn diagrams).

(nu/options-table (describe/options rows/mapping-schema) :value)

(def budget [{:dept "science" :team "physics" :cost 40} {:dept "science" :team "biology" :cost 25}
             {:dept "arts" :team "music" :cost 15} {:dept "arts" :team "theatre" :cost 10}
             {:dept "sport" :team "football" :cost 20}])

(nu/data-table budget)

(def members (for [[set items] {"reads" ["ann" "bob" "cid" "dan"] "writes" ["bob" "dan" "eve"]}
                   item items]
               {:set set :item item}))

(nu/data-table members)

(plot/render-image (plot/arrange [{:data budget
                                   :mapping {:hierarchy [:dept :team] :value :cost}
                                   :charts [(chart/treemap)]}
                                  {:data members
                                   :mapping {:set :set :item :item}
                                   :charts [(chart/venn)]}]
                                 {:columns 2})
                   {:width 800 :height 350})
