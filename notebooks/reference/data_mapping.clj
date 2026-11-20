;; # Data and mapping

;; Plot data is a sequence of maps (rows) or a map of equal-length sequences (columns). A mapping says which value
;; of a row each dimension reads: a keyword or a function of the row. The plot's mapping is merged under each
;; chart's own, and a chart reads only the keys it uses - its page lists them (`(describe/mapping (chart/bar))`).
;; The mapping is open: a chart of your own may read other keys.

(ns reference.data-mapping
  (:require [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.rows :as rows]
            [cljplot2.describe :as describe]
            [cljplot2.pipeline :as pipeline]
            [notebook-utils :as nu]))

;; ## Data: rows or columns

;; Columns become rows; both forms give the same plot. A chart may bring its own `:data` (and `:mapping`) - the
;; Data and mapping group every chart takes:

(nu/options-table (describe/fragment-rows :common))

(def columns {:t (range 20) :v (map #(Math/sin (* 0.4 (double %))) (range 20))})

(plot/render-image {:data columns
                    :mapping {:x :t :y :v}
                    :charts [(chart/line)
                             (chart/scatter {:data [{:t 4 :v 1.0} {:t 12 :v -1.0}] :size 14 :color [200 50 50 120]})]}
                   {:width 500 :height 280})

;; ## Position

;; The coordinates of a mark. A function accessor computes the value from the row - here y is a ratio of two
;; columns.

(nu/options-table (describe/options rows/mapping-schema) :position)

(nu/source-note {:dataset :iris})

(def iris (nu/dataset :iris))

(plot/render-image {:data iris
                    :mapping {:x :petal-length
                              :y #(/ (double (:petal-width %)) (double (:petal-length %)))}
                    :charts [(chart/scatter)]
                    :labels {:y "petal width / length"}}
                   {:width 500 :height 300})

;; ## Intervals and segments

;; The other end of a mark spanning two values: a band from `:ymin` to `:ymax` (ribbons, error bars), a segment
;; to `:xend` / `:yend`.

(nu/options-table (describe/options rows/mapping-schema) :interval)

(def band (for [t (range 0 21)] {:t t :v (* 0.1 t t) :lo (- (* 0.1 t t) 3) :hi (+ (* 0.1 t t) 3)}))

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
;; densities ...) draw each separately; any aesthetic groups too. `:order` sets the drawing order of points (here
;; the large points are drawn last, on top).

(nu/options-table (describe/options rows/mapping-schema) :groups-and-order)

(def walks (for [w (range 4) t (range 12)] {:walk (when (pos? w) w) :t t :v (+ w (* t 0.3 (inc w)) (mod (* t w) 3))}))

(plot/render-image (plot/arrange [{:data walks
                                   :mapping {:x :t :y :v :group :walk}
                                   :charts [(chart/line {:color [70 70 70]})]
                                   :labels {:title ":group - a line per walk"}}
                                  {:data (for [i (range 40)] {:x (mod (* i 7) 13) :y (mod (* i 5) 11) :s (mod i 4)})
                                   :mapping {:x :x :y :y :size :s :order :s}
                                   :charts [(chart/scatter)]
                                   :scales {:size {:range [4 30]}}
                                   :legend false
                                   :labels {:title ":order - larger on top"}}]
                                 {:columns 2})
                   {:width 900 :height 300})

;; A `nil` group value forms its own NA level - the bottom line above is the walk without a number. The plot's
;; `:groups {:na false}` drops such rows instead:

(nu/options-table (filter #(= :groups (first (:path %))) (describe/options pipeline/plot-schema)))

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

;; ## Values, sets and hierarchies

;; Keys of particular charts: `:value` (cell values, weights, leaf sizes), `:hierarchy` (the levels of a tree,
;; outer first) and `:set` / `:item` (set membership of Venn diagrams).

(nu/options-table (describe/options rows/mapping-schema) :value)

(def budget [{:dept "science" :team "physics" :cost 40} {:dept "science" :team "biology" :cost 25}
             {:dept "arts" :team "music" :cost 15} {:dept "arts" :team "theatre" :cost 10}
             {:dept "sport" :team "football" :cost 20}])

(def members (for [[set items] {"reads" ["ann" "bob" "cid" "dan"] "writes" ["bob" "dan" "eve"]}
                   item items]
               {:set set :item item}))

(plot/render-image (plot/arrange [{:data budget
                                   :mapping {:hierarchy [:dept :team] :value :cost}
                                   :charts [(chart/treemap)]}
                                  {:data members
                                   :mapping {:set :set :item :item}
                                   :charts [(chart/venn)]}]
                                 {:columns 2})
                   {:width 800 :height 350})
