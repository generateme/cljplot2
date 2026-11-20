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

(nu/data-table faithful)

(plot/render-image {:data faithful
                    :mapping {:x :eruptions :y :waiting}
                    :charts [(chart/rect {:x [3.5 5.5] :fill [69 117 180 30]})
                             (chart/scatter)
                             (chart/smooth)
                             (chart/rule {:y 70 :dash :dashed})
                             (chart/text {:x 4.5 :y 97 :text "long eruptions"})]
                    :labels {:x "eruptions [min]" :y "waiting [min]"}}
                   {:width 600 :height 400})

;; `:extent` decides whether an annotation's positions count for the scales: a rule at 110 minutes stretches the
;; y axis to reach it (left); with `:extent false` the axis keeps the data's range and the rule is off the panel
;; (right):

(plot/render-image (plot/arrange (for [extent [true false]]
                                   {:data faithful
                                    :mapping {:x :eruptions :y :waiting}
                                    :charts [(chart/scatter) (chart/rule {:y 110 :extent extent :dash :dashed})]
                                    :labels {:title (str ":extent " extent)}})
                                 {:columns 2})
                   {:width 800 :height 300})

;; ## Position of groups

;; Bars, histograms, densities and lines combine groups through `:position` (each chart's page lists the values it
;; takes): `:dodge`, `:stack`, `:normalize`, `:overlay`, and for densities and lines the streamgraph `:silhouette` /
;; `:wiggle`; histograms and densities also `:mirror`.

(def sales (for [q ["Q1" "Q2" "Q3" "Q4"] [i product] (map-indexed vector ["apples" "pears" "plums"])]
             {:quarter q :product product :amount (+ 10 (* 7 i) (* 3 (count (str q product))))}))

(nu/data-table sales)

(plot/render-image (plot/arrange (for [position [:dodge :stack :normalize]]
                                   {:data sales
                                    :mapping {:x :quarter :y :amount :color :product}
                                    :charts [(chart/bar {:position position})]
                                    :labels {:title (str ":position " position)}
                                    :legend (= position :normalize)})
                                 {:columns 3 :widths [1 1 1.4]})
                   {:width 1000 :height 350})

;; `:overlay` draws the groups over each other, here translucent histograms; `:mirror` draws the second of two
;; groups downward:

(def two-groups (for [i (range 400)]
                  {:g (if (even? i) "early" "late") :v (+ (if (even? i) 2.0 3.5) (* 0.25 (- (mod (* i 37) 17) 8)))}))

(nu/data-table two-groups)

(plot/render-image (plot/arrange (for [position [:overlay :mirror]]
                                   {:data two-groups
                                    :mapping {:x :v :color :g}
                                    :charts [(chart/histogram {:position position :bins 20})]
                                    :labels {:title (str ":position " position)}
                                    :legend (= position :mirror)})
                                 {:columns 2 :widths [1 1.3]})
                   {:width 900 :height 320})

;; Streamgraphs: the stack shifted around a centre line - `:silhouette` symmetric, `:wiggle` with the least
;; movement:

(def streams (for [series ["a" "b" "c" "d"] t (range 30)]
               {:series series :t t :v (+ 2 (Math/sin (+ (* 0.3 t) (count series) (* 1.7 (int (first series))))))}))

(nu/data-table streams)

(plot/render-image (plot/arrange (for [position [:stack :silhouette :wiggle]]
                                   {:data streams
                                    :mapping {:x :t :y :v :color :series}
                                    :charts [(chart/line {:position position :fill :color})]
                                    :labels {:title (str ":position " position)}
                                    :legend (= position :wiggle)})
                                 {:columns 3 :widths [1 1 1.3]})
                   {:width 1000 :height 320})
