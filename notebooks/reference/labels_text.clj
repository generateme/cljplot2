;; # Labels and text

;; A plot has a title and two axis titles (`:labels`), each a string or a label map with its own text style. Text
;; drawn by charts (the `text` chart, value labels, strip and legend texts) takes its style from a theme text role.

(ns reference.labels-text
  (:require [fastmath.core :as m]
            [cljplot2.chart :as chart]
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
                                     :background [230 230 245] :inset 6}
                             :y {:text "sepal width [cm]" :font-style :italic}}}
                   {:width 600 :height 350})

;; ## Long axis titles

;; An axis title longer than its panels - the width under them, the height beside them; one title spans all facet
;; panels - is fitted to them: wrapped at word boundaries (a line into at most three, the title's row or column
;; grows), then drawn smaller down to the `:axis-title` text role's `:min-size` (8 points by default, theme
;; `[:text :axis-title :min-size]` - every text role takes one, see the Theme chapter), then cut with an ellipsis.
;; Line breaks you give are kept, and a title that fits is drawn as given. Fitting is always on: earlier versions
;; clipped such titles. A title rotated across its own track (a y title with `:rotation 0`) isn't fitted.

;; Wrapped titles beside a small panel; on the right a word too long for the panel shrinks to the least size and is
;; cut (`:min-size 10`):

(plot/render-image (plot/arrange [{:data iris
                                   :mapping {:x :sepal-length :y :sepal-width}
                                   :charts [(chart/scatter)]
                                   :labels {:title "wrapped"
                                            :x "sepal length of the iris flowers, measured in centimetres"
                                            :y "sepal width of the flowers in centimetres"}}
                                  {:data iris
                                   :mapping {:x :sepal-length :y :sepal-width}
                                   :charts [(chart/scatter)]
                                   :labels {:title "shrunk and cut"
                                            :x "sepal_length_in_centimetres_of_the_flowers_of_three_iris_species"
                                            :y "sepal width"}
                                   :theme {:text {:axis-title {:min-size 10}}}}]
                                 {:columns 2})
                   {:width 700 :height 300})

;; ## Text in the panel

;; The `text` chart writes literal text or a mapped `:label`; `:anchor`, `:offset` and `:rotation` place it,
;; `:select :last` labels the end of each group (e.g. line ends). Its full table is on the chart's page
;; ([text](reference.charts_annotations.html#text-labels)).

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

;; ## Formulas

;; Any text can hold LaTeX formulas, typeset at the text's size and colour (bold in a bold role): `plot/math` is one
;; formula, `plot/runs` puts text and formulas on one baseline, and in strings `$...$` is a formula. The scale
;; formatter `:power-math` writes log ticks as typeset powers.

;; Formulas in the title, the axis titles, the tick labels, the legend and a text chart:

(def decay
  (vec (for [rate [0.5 1.0] t (range 0.0 10.5 0.5)]
         {:t t :n (* 1000.0 (m/exp (- (* rate t)))) :rate (str "$\\lambda = " rate "$")})))

(nu/data-table decay)

(plot/render-image {:data decay
                    :mapping {:x :t :y :n :color :rate}
                    :charts [(chart/line)
                             (chart/text {:x 5.5 :y 200.0 :anchor [:left :middle]
                                          :text (plot/runs "half-life " (plot/math "t_{1/2} = \\frac{\\ln 2}{\\lambda}"))})]
                    :scales {:y {:kind :log :formatter :power-math}}
                    :labels {:title (plot/runs "Exponential decay " (plot/math "N(t) = N_0 e^{-\\lambda t}"))
                             :x (plot/math "t")
                             :y (plot/math "N(t)")}}
                   {:width 600 :height 380})

;; A dollar pair has no space inside and its closing dollar isn't followed by a digit, so `cost $5 to $10` stays
;; text; `\$` is always a literal dollar. `:dollar-math false` (a text role, a label map or a text chart) keeps every
;; dollar literal, as on the right - strips, value labels, a rotated two-line title and turned tick labels:

(def masses
  [{:part "$m_1$" :group "$\\alpha$ set" :kg 1.25} {:part "$m_2$" :group "$\\alpha$ set" :kg 2.5}
   {:part "$m_1$" :group "$\\beta$ set" :kg 0.75} {:part "$m_2$" :group "$\\beta$ set" :kg 1.75}])

(nu/data-table masses)

(let [masses-plot (fn [title theme]
                    {:data masses
                     :mapping {:x :part :y :kg :facet-col :group}
                     :charts [(chart/bar {:value-label true :value-format "$%.1f$"})]
                     :labels {:title title :y (plot/runs "mass\n" (plot/math "m_i\\,[\\mathrm{kg}]"))}
                     :theme (merge {:axis {:x {:label-rotation 45}}} theme)})]
  (plot/render-image (plot/arrange [(masses-plot "typeset" nil)
                                    (masses-plot {:text "literal $m_i$" :dollar-math false}
                                                 {:text {:strip {:dollar-math false} :tick-label {:dollar-math false}
                                                         :bar-value {:dollar-math false}}})]
                                   {:columns 2})
                     {:width 760 :height 360}))

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
