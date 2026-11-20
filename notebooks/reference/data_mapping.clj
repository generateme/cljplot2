;; # Data and mapping

;; Plot data is a sequence of maps (rows) or a map of equal-length sequences (columns). A mapping says which value
;; of a row each dimension reads: a keyword or a function of the row.

(ns reference.data-mapping
  (:require [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.rows :as rows]
            [cljplot2.describe :as describe]
            [cljplot2.pipeline :as pipeline]
            [notebook-utils :as nu]))

;; ## Mapping keys

;; The built-in dimensions; a chart reads only some of them (each chart's page lists its keys, e.g.
;; `(describe/mapping (chart/bar))`). The mapping is open: a chart of your own may read other keys.

(nu/options-table (describe/options rows/mapping-schema))

;; ## Rows or columns

;; Columns become rows; both plots below are the same.

(def columns {:t (range 20) :v (map #(Math/sin (* 0.4 (double %))) (range 20))})

(plot/render-image {:data columns
                    :mapping {:x :t :y :v}
                    :charts [(chart/line) (chart/scatter)]}
                   {:width 500 :height 300})

;; ## Accessors

;; A function accessor computes the value from the row - here the y value is a ratio of two columns and the colour
;; a derived level.

(nu/source-note {:dataset :iris})

(def iris (nu/dataset :iris))

(plot/render-image {:data iris
                    :mapping {:x :petal-length
                              :y #(/ (double (:petal-width %)) (double (:petal-length %)))
                              :color #(if (> (double (:sepal-width %)) 3.0) "wide sepal" "narrow sepal")}
                    :charts [(chart/scatter)]
                    :labels {:y "petal width / length"}}
                   {:width 600 :height 400})

;; ## Plot and chart mappings

;; Each chart merges its own `:mapping` over the plot's and may bring its own `:data` (the Data and mapping group,
;; taken by every chart). Here the line reads a second data set. A key mapped to `nil` is unmapped for that chart.

(def means (for [[species rs] (group-by :species iris)]
             {:species species
              :x (/ (reduce + (map :petal-length rs)) (count rs))
              :y (/ (reduce + (map :petal-width rs)) (count rs))}))

(plot/render-image {:data iris
                    :mapping {:x :petal-length :y :petal-width :color :species}
                    :charts [(chart/scatter)
                             (chart/line {:data (sort-by :x means) :mapping {:x :x :y :y :color nil}})
                             (chart/text {:data means :mapping {:x :x :y :y :label :species :color nil}
                                          :offset [0 -14]})]}
                   {:width 600 :height 400})

;; ## Groups and missing values

;; Aesthetic keys (`:color`, `:shape`, `:size`, `:dash`, `:pattern`) and `:group` split rows into groups; charts
;; drawing one mark per group (lines, areas, densities ...) draw each group separately. A `nil` aesthetic value
;; forms its own NA level unless the plot says `:groups {:na false}` (such rows are dropped).

(nu/options-table (filter #(= :groups (first (:path %))) (describe/options pipeline/plot-schema)))
