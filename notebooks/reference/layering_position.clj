;; # Layering and position

;; Charts are drawn in the order of `:charts`, each over the previous ones, and all of them share the plot's scales.
;; Grouped marks of one chart are placed by its `:position` option: side by side, stacked, normalized or overlaid.

(ns reference.layering-position
  (:require [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [notebook-utils :as nu]))

;; ## Layers

;; A layer is any chart; annotation charts (`rule`, `rect`, `text`, `function`, `image`) take literal positions
;; instead of data and inherit only the plot's facet keys. Their extent counts for the scales unless
;; `:extent false` (the Annotation options group):

(nu/options-table (describe/fragment-rows :annotation))

(nu/source-note {:dataset :faithful})

(def faithful (nu/dataset :faithful))

(plot/render-image {:data faithful
                    :mapping {:x :eruptions :y :waiting}
                    :charts [(chart/rect {:x [3.5 5.5] :fill [69 117 180 30]})
                             (chart/scatter)
                             (chart/smooth)
                             (chart/rule {:y 70 :dash :dashed})
                             (chart/text {:x 4.5 :y 97 :text "long eruptions"})]
                    :labels {:x "eruptions [min]" :y "waiting [min]"}}
                   {:width 600 :height 400})

;; ## Position of groups

;; Bars, histograms, densities and lines combine groups through `:position` (each chart's page lists the values it
;; takes): `:dodge`, `:stack`, `:normalize`, `:overlay`, and for densities and lines the streamgraph `:silhouette` /
;; `:wiggle`; histograms and densities also `:mirror`.

(def sales (for [q ["Q1" "Q2" "Q3" "Q4"] [i product] (map-indexed vector ["apples" "pears" "plums"])]
             {:quarter q :product product :amount (+ 10 (* 7 i) (* 3 (count (str q product))))}))

(plot/render-image (plot/arrange (for [position [:dodge :stack :normalize]]
                                   {:data sales
                                    :mapping {:x :quarter :y :amount :color :product}
                                    :charts [(chart/bar {:position position})]
                                    :labels {:title (str ":position " position)}
                                    :legend (= position :normalize)})
                                 {:columns 3 :widths [1 1 1.4]})
                   {:width 1000 :height 350})
