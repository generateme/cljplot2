;; # Layering and position

;; Charts are drawn in the order of `:charts`, each over the previous ones, and all of them share the plot's scales.
;; Grouped marks of one chart are placed by its `:position` option: side by side, stacked, normalized or overlaid.

(ns reference.layering-position
  (:require [fastmath.core :as m]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [notebook-utils :as nu]))

;; ## Layers

;; A layer is any chart; annotation charts (`rule`, `rect`, `text`, `function`, `image`) take literal positions or
;; a function instead of data and inherit only the plot's facet keys. The positions of `rule`, `rect` and `text`
;; count for the scales unless `:extent false` (the Annotation options group, shown at work in
;; [Shared options](reference.shared_options.html#annotation-options)):

(nu/options-table (describe/fragment-rows :annotation))

(nu/source-note {:dataset :faithful})

(def faithful (nu/dataset :faithful))

(nu/data-table faithful)

;; The layers bottom to top: a shaded x range ([rect](reference.charts_annotations.html#rect) `:x [lo hi]`,
;; `:fill`), the points, a smooth curve, a dashed [rule](reference.charts_annotations.html#rule) at 70 minutes and
;; a [text](reference.charts_annotations.html#text) note at a literal position:

(plot/render-image {:data faithful
                    :mapping {:x :eruptions :y :waiting}
                    :charts [(chart/rect {:x [3.5 5.5] :fill [0 114 178 30]})
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

(def sales (for [[qi q] (map-indexed vector ["Q1" "Q2" "Q3" "Q4"]) [i product] (map-indexed vector ["apples" "pears" "plums"])]
             {:quarter q :product product :amount (+ 10 (* 7 i) (* 5 qi) (* 4 (mod (+ qi (* 2 i)) 3)))}))

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
;; movement (lines filled in their colour, [line](reference.charts_points_lines.html#line-charts) `:fill :color`):

(def streams (for [series ["a" "b" "c" "d"] t (range 30)]
               {:series series :t t :v (+ 2 (m/sin (+ (* 0.3 t) (count series) (* 1.7 (int (first series))))))}))

(nu/data-table streams)

(plot/render-image (plot/arrange (for [position [:stack :silhouette :wiggle]]
                                   {:data streams
                                    :mapping {:x :t :y :v :color :series}
                                    :charts [(chart/line {:position position :fill :color})]
                                    :labels {:title (str ":position " position)}
                                    :legend (= position :wiggle)})
                                 {:columns 3 :widths [1 1 1.3]})
                   {:width 1000 :height 320})
