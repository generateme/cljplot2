;; # Shared options

;; Option groups taken by several charts, builders or data functions, documented once here; other tables mark their
;; rows with the group's title. A chart may take only part of a group - its own table lists what it accepts.

(ns reference.shared-options
  (:require [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [notebook-utils :as nu]))

;; ## Charts

;; ### Data and mapping

;; Every chart's own data and mapping (merged over the plot's, Data and mapping chapter).

(nu/options-table (describe/fragment-rows :common))

;; ### Annotation options

(nu/options-table (describe/fragment-rows :annotation))

;; ### Line style

(nu/options-table (describe/fragment-rows :line-style))

;; ### Curves

;; The line between data points: straight, a fastmath interpolation or a step, sampled for drawing.

(nu/options-table (describe/fragment-rows :curve))

(def points (for [x (range 8)] {:x x :y (nth [1 3 2 5 4 4.5 2 3] x)}))

(plot/render-image (plot/arrange (for [interpolator [:linear :monotone :cubic :step]]
                                   {:data points
                                    :mapping {:x :x :y :y}
                                    :charts [(chart/line {:interpolator interpolator}) (chart/scatter)]
                                    :labels {:title (str ":interpolator " interpolator)}})
                                 {:columns 4})
                   {:width 1000 :height 300})

;; ### Fill patterns

(nu/options-table (describe/fragment-rows :pattern))

(plot/render-image {:data (for [[i p] (map-indexed vector [:hatch :back-hatch :cross :dots])] {:p (name p) :v (+ 3 i)})
                    :mapping {:x :p :y :v :pattern :p}
                    :charts [(chart/bar {:color :white :stroke-color :black :pattern-color :black})]
                    :legend false}
                   {:width 500 :height 300})

;; ### Rounded corners

(nu/options-table (describe/fragment-rows :corner-radius))

;; ### Strip options

;; Strip charts (box, violin, stripplot, beeswarm, dotplot, bar, error bars ...) draw one item per level of a
;; categorical axis.

(nu/options-table (describe/fragment-rows :strip))

;; ### Mirrored strips

(nu/options-table (describe/fragment-rows :mirror))

;; ### Ridgelines

(nu/options-table (describe/fragment-rows :ridge))

;; ### Cell style

(nu/options-table (describe/fragment-rows :cell))

;; ### Fields

(nu/options-table (describe/fragment-rows :field))

;; ### Hierarchy labels

(nu/options-table (describe/fragment-rows :hierarchy-labels))

;; ### 3D frame

(nu/options-table (describe/fragment-rows :frame-3d))

;; ### Text style

;; Also the style of every theme text role (Theme chapter).

(nu/options-table (describe/fragment-rows :text-style))

;; ## Builders

;; ### Builder options

(nu/options-table (describe/fragment-rows :builder))

;; ### Multiplot layout

(nu/options-table (describe/fragment-rows :arrange))

;; ## Builders and data functions

;; The options a builder forwards to the data function computing its layout; the function takes them directly too.

;; ### Graph

(nu/options-table (describe/fragment-rows :graph/graph))

;; ### Graph layout

(nu/options-table (describe/fragment-rows :graph/layout))

;; ### Chord layout

(nu/options-table (describe/fragment-rows :graph/chord-layout))

;; ### Arc layout

(nu/options-table (describe/fragment-rows :graph/arc-layout))

;; ### Sankey layout

(nu/options-table (describe/fragment-rows :graph/sankey-layout))

;; ### Alluvial graph

(nu/options-table (describe/fragment-rows :graph/alluvial-graph))

;; ### Dendrogram layout

(nu/options-table (describe/fragment-rows :tree/layout))

;; ### Node-link tree layout

(nu/options-table (describe/fragment-rows :tree/node-link-layout))

;; ### Edge bundling

(nu/options-table (describe/fragment-rows :tree/bundle-paths))

;; ### Sequences

(nu/options-table (describe/fragment-rows :hierarchy/sequence-levels))

;; ### Autocorrelation rows

(nu/options-table (describe/fragment-rows :series/acf-rows))

;; ### Horizon rows

(nu/options-table (describe/fragment-rows :series/horizon-rows))
