;; # Theme

;; The theme holds every style default: backgrounds, grid lines, axes, legends, palettes, text roles and each chart
;; kind's style. A plot's `:theme` is deep-merged over the default theme, so it names only what changes.

(ns reference.theme
  (:require [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [notebook-utils :as nu]))

(nu/source-note {:dataset :iris})

(def iris (nu/dataset :iris))

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :petal-length :color :species :facet :species}
                    :charts [(chart/scatter)]
                    :facets {:columns 3}
                    :labels {:title "A darker theme"}
                    :theme {:plot {:background [40 44 52]}
                            :panel {:fill [55 60 70] :grid {:x {:color [90 96 110] :width 0.5 :dash :none}
                                                            :y {:color [90 96 110] :width 0.5 :dash :none}}}
                            :strip {:fill [70 76 88] :border-color :none}
                            :axis {:x {:line-color :none :tick-color [150 150 150]}
                                   :y {:line-color :none :tick-color [150 150 150]}}
                            :palette {:color :tableau-10}
                            :text {:title {:color :white}
                                   :axis-title {:color [220 220 220]}
                                   :tick-label {:color [200 200 200]}
                                   :strip {:color :white}
                                   :legend-title {:color :white}
                                   :legend-label {:color [220 220 220]}}}}
                   {:width 900 :height 400})

;; ## Plot

(nu/options-table (describe/theme-rows :plot))

;; ## Panel

(nu/options-table (describe/theme-rows :panel))

;; ## Axis

;; Per position scale key (`:x`, `:y`), also when `:flip` draws `:x` vertically.

(nu/options-table (describe/theme-rows :axis))

;; ## Strip

(nu/options-table (describe/theme-rows :strip))

;; ## Facet

(nu/options-table (describe/theme-rows :facet))

;; ## Multiplot

(nu/options-table (describe/theme-rows :multiplot))

;; ## Legend

(nu/options-table (describe/theme-rows :legend))

;; ## Palette

;; The default ranges of the aesthetic scales (Aesthetics and legends chapter).

(nu/options-table (describe/theme-rows :palette))

;; ## Text

;; Each text the library draws has a role; a label's `:role` may also name a role of your own added here. Every
;; role is a text style - the keys of the Text style group:

(nu/options-table (describe/theme-rows :text))

(nu/options-table (describe/fragment-rows :text-style))

;; ## Chart

;; The style of each chart kind, `[:chart kind]` - its style keys and defaults are in the chart's table (the rows
;; with a theme default). Chart options given to the chart override the theme.

(nu/options-table (describe/theme-rows :chart))

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :petal-length}
                    :charts [(chart/scatter)]
                    :theme {:chart {:scatter {:shape \s :size 8 :color [178 34 34 120]}}}}
                   {:width 500 :height 350})
