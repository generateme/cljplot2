;; # Theme

;; The theme holds every style default: backgrounds, grid lines, axes, legends, palettes, text roles and each chart
;; kind's style. A plot's `:theme` is deep-merged over the default theme, so it names only what changes. Each
;; section below lists its keys with their default values and changes some of them.

(ns reference.theme
  (:require [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [notebook-utils :as nu]))

(nu/source-note {:dataset :iris})

(def iris (nu/dataset :iris))

(def iris-plot
  {:data iris
   :mapping {:x :sepal-length :y :petal-length :color :species}
   :charts [(chart/scatter)]})

;; ## Plot

;; The canvas behind everything and the margin around the plot.

(nu/options-table (describe/theme-rows :plot))

(plot/render-image (assoc iris-plot :theme {:plot {:background [255 250 235] :margin 30}})
                   {:width 500 :height 300})

;; ## Panel box

;; The data panel's fill and border; `:alternate-fill` shades every other facet panel.

(nu/options-table (describe/theme-rows :panel) :panel-box)

(plot/render-image (assoc iris-plot
                          :theme {:panel {:fill :white :border-color [60 60 60] :border-width 2}})
                   {:width 500 :height 300})

;; ## Grid lines

;; Grid lines at the ticks of each position scale, of bands scales (with the alternate band fill) and at minor
;; ticks; `:none` turns a kind of line off.

(nu/options-table (describe/theme-rows :panel) :grid)

(plot/render-image (assoc iris-plot
                          :scales {:x {:minor-ticks 4}}
                          :theme {:panel {:grid {:x {:color [150 150 200] :width 1 :dash :none}
                                                 :y :none
                                                 :minor {:color [200 200 220] :width 0.5 :dash :none}}}})
                   {:width 500 :height 300})

;; ## Axes

;; The axis line, ticks and tick labels per position scale - kept by scale (`:x` stays `:x` when `:flip` draws it
;; vertically). `:tick-spacing` sets how many ticks fit (pixels per tick); `:label-rotation` turns long tick labels.

(nu/options-table (describe/theme-rows :axis) :x-axis)

(def long-levels [{:k "a long level" :v 3} {:k "another long level" :v 5} {:k "the last level" :v 4}])

(plot/render-image {:data long-levels
                    :mapping {:x :k :y :v}
                    :charts [(chart/bar)]
                    :theme {:axis {:x {:label-rotation 45 :tick-length 0 :line-color :none}}}}
                   {:width 500 :height 320})

;; The y axis has the same keys:

(nu/options-table (describe/theme-rows :axis) :y-axis)

(plot/render-image (assoc iris-plot :theme {:axis {:y {:tick-spacing 25 :tick-color [200 0 0] :tick-length 10}}})
                   {:width 500 :height 300})

;; ## Strips

;; Facet strip boxes; the shingle fill marks a shingle level's range.

(nu/options-table (describe/theme-rows :strip))

(def faceted-iris (assoc iris-plot :mapping {:x :sepal-length :y :petal-length :facet :species}))

(plot/render-image (assoc faceted-iris :theme {:strip {:fill [70 90 140] :border-color :none}
                                               :text {:strip {:color :white}}})
                   {:width 700 :height 280})

;; ## Facets

;; The gap between facet panels.

(nu/options-table (describe/theme-rows :facet))

(plot/render-image (assoc faceted-iris :theme {:facet {:gap 25}}) {:width 700 :height 280})

;; ## Multiplots

;; The gap between arranged plots.

(nu/options-table (describe/theme-rows :multiplot))

(plot/render-image (plot/arrange [iris-plot (assoc iris-plot :mapping {:x :sepal-width :y :petal-width :color :species})]
                                 {:columns 2 :theme {:multiplot {:gap 40}}})
                   {:width 800 :height 300})

;; ## Legend

;; Legend key sizes, spacing, the legend box and the colour bar.

(nu/options-table (describe/theme-rows :legend))

(plot/render-image (assoc iris-plot :theme {:legend {:key-size 24 :gap 10 :fill :white :border-color [150 150 150]}})
                   {:width 500 :height 300})

;; ## Palette

;; The default ranges of the aesthetic scales (Aesthetics and legends chapter): colours of levels, the gradient of
;; numbers, shapes, sizes, dashes and patterns.

(nu/options-table (describe/theme-rows :palette))

(plot/render-image (assoc iris-plot
                          :mapping {:x :sepal-length :y :petal-length :color :species :shape :species}
                          :theme {:palette {:color :tableau-10 :shape [\s \A \V]}})
                   {:width 500 :height 300})

;; ## Text

;; Every text the library draws has a role; each role is a text style (the keys of the Text style group, last in this
;; section). A label's `:role` may also name a role of your own added under `:text`. A role's `:color` may be
;; `:contrast` - black or white, whichever reads on what's behind: the fill under a chart's label, else the plot
;; background. Labels drawn over fills (hierarchies, Venn counts, tile and bar values) default to it.

;; Titles, strips and axis texts:

(nu/options-table (describe/theme-rows :text) :plot-text)

(plot/render-image (assoc iris-plot
                          :labels {:title "Iris" :x "sepal length" :y "petal length"}
                          :theme {:text {:title {:size 20 :font-style :italic :align [:left :middle]}
                                         :axis-title {:color [120 30 30]}
                                         :tick-label {:size 8}}})
                   {:width 500 :height 320})

;; Legend texts:

(nu/options-table (describe/theme-rows :text) :legend-text)

(plot/render-image (assoc iris-plot :theme {:text {:legend-title {:size 9 :font-style :plain :color [100 100 100]}
                                                   :legend-label {:font-style :italic}}})
                   {:width 500 :height 300})

;; Labels of treemaps, partitions and circle packings - by default in the contrast colour of their cell; a colour
;; given wins, on every cell alike (here the leaves - hard to read on the light ones, which is why `:contrast` is
;; the default):

(nu/options-table (describe/theme-rows :text) :hierarchy-text)

(def budget [{:dept "science" :team "physics" :cost 40} {:dept "science" :team "biology" :cost 25}
             {:dept "arts" :team "music" :cost 15} {:dept "arts" :team "theatre" :cost 10}])

(plot/render-image {:data budget
                    :mapping {:hierarchy [:dept :team] :value :cost}
                    :charts [(chart/treemap)]
                    :theme {:text {:treemap-group {:size 13 :font-style :bold-italic}
                                   :treemap-leaf {:font-style :italic :color [255 220 120]}}}}
                   {:width 500 :height 300})

;; Texts of other charts - text annotations, bar values, Venn diagrams, tiles, correlations, contours (bar values,
;; Venn counts and tile values in the contrast colour of what's behind them by default):

(nu/options-table (describe/theme-rows :text) :chart-text)

(plot/render-image (plot/arrange [(assoc iris-plot
                                          :charts [(chart/scatter) (chart/text {:x 6.5 :y 2 :text "an annotation"})]
                                          :theme {:text {:annotation {:size 14 :color [178 34 34]
                                                                      :background [255 255 255 200]}}})
                                  {:data [{:k "a" :v 3} {:k "b" :v 5} {:k "c" :v 4}]
                                   :mapping {:x :k :y :v}
                                   :charts [(chart/bar {:value-label true :label-position :center})]
                                   :theme {:text {:bar-value {:size 12 :font-style :bold}}}}]
                                 {:columns 2 :widths [3 2]})
                   {:width 800 :height 300})

;; The keys of a text style:

(nu/options-table (describe/fragment-rows :text-style))

(plot/render-image (assoc iris-plot
                          :labels {:title "Iris"}
                          :theme {:text {:title {:font "Serif" :size 16 :padding 10 :background [230 230 245]}}})
                   {:width 500 :height 300})

;; ## Chart

;; The style of each chart kind, `[:chart kind]` - its keys and defaults are the rows with a theme default in the
;; chart's table. Options given to a chart override its theme style.

(nu/options-table (describe/theme-rows :chart))

;; The kinds with a theme style:

(keys (describe/chart-constructors))

(plot/render-image (assoc iris-plot :theme {:chart {:scatter {:shape \s :size 8}}})
                   {:width 500 :height 300})
