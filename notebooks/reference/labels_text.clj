;; # Labels and text

;; A plot has a title and two axis titles (`:labels`), each a string or a label map with its own text style. Text
;; drawn by charts (the `text` chart, value labels, strip and legend texts) takes its style from a theme text role.

(ns reference.labels-text
  (:require [cljplot2.chart :as chart]
            [cljplot2.content.label :as label]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [cljplot2.layout.plot-grid :as plot-grid]
            [notebook-utils :as nu]))

;; ## Plot labels

(nu/options-table (describe/options plot-grid/labels-schema))

;; A label map: the text, a theme text role (`:title` for the title, `:axis-title` for axis titles - its style is the
;; base), a rotation and text style keys overriding the role's.

(nu/options-table (describe/options label/label-options))

(nu/source-note {:dataset :iris})

(def iris (nu/dataset :iris))

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :sepal-width :color :species}
                    :charts [(chart/scatter)]
                    :labels {:title {:text "Iris sepals" :size 18 :align [:left :middle] :color [40 40 120]}
                             :x "sepal length [cm]"
                             :y {:text "sepal width [cm]" :font-style :italic}}}
                   {:width 600 :height 400})

;; ## Text style

;; The style keys of a label, a text role and the `text` chart (the Text style group):

(nu/options-table (describe/fragment-rows :text-style))

;; ## Text in the panel

;; The `text` chart writes literal text or a mapped `:label`; `:anchor`, `:offset` and `:rotation` place it,
;; `:select :last` labels the end of each group (e.g. line ends). Its full table is on the chart's page.

(def series (for [[i g] (map-indexed vector ["alpha" "beta" "gamma"]) t (range 10)]
              {:g g :t t :v (+ (* t (inc i) 0.5) (* 3 i))}))

(plot/render-image {:data series
                    :mapping {:x :t :y :v :color :g}
                    :charts [(chart/line)
                             (chart/text {:mapping {:x :t :y :v :label :g} :select :last :anchor [:left :middle]})
                             (chart/text {:x [:panel 0.02] :y [:panel 0.95] :text "literal text in panel units"
                                          :anchor [:left :middle] :font-style :italic})]
                    :legend false}
                   {:width 600 :height 350})
