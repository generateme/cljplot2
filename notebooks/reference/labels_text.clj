;; # Labels and text

;; A plot has a title and two axis titles (`:labels`), each a string or a label map with its own text style. Text
;; drawn by charts (the `text` chart, value labels, strip and legend texts) takes its style from a theme text role.

(ns reference.labels-text
  (:require [cljplot2.chart :as chart]
            [cljplot2.content.label :as label]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [cljplot2.graphics.font :as font]
            [cljplot2.layout.plot-grid :as plot-grid]
            [notebook-utils :as nu]))

;; ## Plot labels

;; The plot title and the axis titles. Axis titles are absent unless given, except that statistical charts
;; (histogram, density, ecdf, qq ...) suggest one for their computed axis.

(nu/options-table (describe/options plot-grid/labels-schema))

(nu/source-note {:dataset :iris})

(def iris (nu/dataset :iris))

(nu/data-table iris)

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :sepal-width :color :species}
                    :charts [(chart/scatter)]
                    :labels {:title "Iris sepals" :x "sepal length [cm]" :y "sepal width [cm]"}}
                   {:width 600 :height 350})

;; ## Label maps

;; A label is a string or a label map: the text, the theme text role whose style is the base (`:title` for the
;; title, `:axis-title` for axis titles, or a role of your theme) and a rotation. Plot labels get their role from
;; their place; a label in a custom grid (Facets chapter) must name one. Here the y title is horizontal and
;; styled as a tick label.

(nu/options-table (describe/options label/label-options) :label)

(plot/render-image {:data iris
                    :mapping {:x :petal-length :y :petal-width}
                    :charts [(chart/scatter)]
                    :labels {:x {:text "petal length\n[cm]"}
                             :y {:text "petal\nwidth" :rotation 0 :role :tick-label}}}
                   {:width 600 :height 350})

;; ## Label style

;; Text style keys in a label map override its role's style for this label only:

(nu/options-table (describe/options label/label-options) :text-style)

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :sepal-width :color :species}
                    :charts [(chart/scatter)]
                    :labels {:title {:text "Iris sepals" :size 18 :align [:left :middle] :color [40 40 120]
                                     :background [230 230 245] :padding 6}
                             :y {:text "sepal width [cm]" :font-style :italic}}}
                   {:width 600 :height 350})

;; ## Text in the panel

;; The `text` chart writes literal text or a mapped `:label`; `:anchor`, `:offset` and `:rotation` place it,
;; `:select :last` labels the end of each group (e.g. line ends). Its full table is on the chart's page.

(def series (for [[i g] (map-indexed vector ["alpha" "beta" "gamma"]) t (range 10)]
              {:g g :t t :v (+ (* t (inc i) 0.5) (* 3 i))}))

(nu/data-table series)

(plot/render-image {:data series
                    :mapping {:x :t :y :v :color :g}
                    :charts [(chart/line)
                             (chart/text {:mapping {:x :t :y :v :label :g} :select :last :anchor [:left :middle]})
                             (chart/text {:x [:panel 0.02] :y [:panel 0.95] :text "literal text in panel units"
                                          :anchor [:left :middle] :font-style :italic})]
                    :legend false}
                   {:width 600 :height 350})

;; ## font/resolve-font

;; A text style's `:font` is a family name or a `java.awt.Font`; `resolve-font` turns a name, a style (`:plain`,
;; `:bold`, `:italic`, `:bold-italic`) and a size in points into the `java.awt.Font` drawn. Bundled families
;; (`"DejaVu Sans Mono"` - the default theme's font, `"Noto Emoji"`) win over installed ones, so plots render alike on
;; every platform; installed and logical families (`"Serif"`, `"SansSerif"` ...) work too, an unknown name throws
;; `{:key :font}`. It takes no options map.

(font/resolve-font "DejaVu Sans Mono" :bold 14)

(font/resolve-font "Serif" :italic 12)

;; A resolved font (here a bundled bold face) in a label, and a logical family by name for the axis titles:

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :sepal-width :color :species}
                    :charts [(chart/scatter)]
                    :labels {:title {:text "Iris sepals" :font (font/resolve-font "DejaVu Sans Mono" :bold 18)}
                             :x {:text "sepal length [cm]" :font "Serif"}
                             :y {:text "sepal width [cm]" :font "Serif" :font-style :italic}}}
                   {:width 600 :height 350})
