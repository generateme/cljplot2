;; # Labels and text

;; A plot has a title, axis titles and legend titles (`:labels`), each a string or a label map with its own text
;; style. Text drawn by charts (the `text` chart, value labels, strip and legend texts) takes its style from a theme
;; text role.

(ns reference.labels-text
  (:require [fastmath.core :as m]
            [cljplot2.chart :as chart]
            [cljplot2.content.label :as label]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [cljplot2.graphics.font :as font]
            [cljplot2.layout.plot-grid :as plot-grid]
            [notebook-utils :as nu]))

;; The table below holds the iris flowers; most sections draw them:

(nu/source-note {:dataset :iris})

(def iris (nu/dataset :iris))

(nu/data-table iris)

;; The table below holds three made-up series; the section on text in the panel draws them:

(def series (for [[i g] (map-indexed vector ["alpha" "beta" "gamma"]) t (range 10)]
              {:g g :t t :v (+ (* t (inc i) 0.5) (* 3 i))}))

(nu/data-table series)

;; The table below holds made-up decays at two rates, the rates named by formulas; the formula section draws them:

(def decay
  (vec (for [rate [0.5 1.0] t (range 0.0 10.5 0.5)]
         {:t t :n (* 1000.0 (m/exp (- (* rate t)))) :rate (str "$\\lambda = " rate "$")})))

(nu/data-table decay)

;; The table below holds made-up masses whose names hold formulas; the formula section draws them:

(def masses
  [{:part "$m_1$" :group "$\\alpha$ set" :kg 1.25} {:part "$m_2$" :group "$\\alpha$ set" :kg 2.5}
   {:part "$m_1$" :group "$\\beta$ set" :kg 0.75} {:part "$m_2$" :group "$\\beta$ set" :kg 1.75}])

(nu/data-table masses)

;; ## Plot labels

;; These keys of the plot's `:labels` map hold the plot title, the axis titles and the legend titles. A 3D plot draws
;; `:x`, `:y` and `:z` on its frame. An axis without a label takes the title a chart suggests, else the name of the
;; column its mapping names by a keyword, left out when the plot is too small for it; `:none` hides it. The plot below names the title and both axes:

(nu/option-cards [:labels] (describe/options plot-grid/labels-schema))

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :sepal-width :color :species}
                    :charts [(chart/scatter)]
                    :labels {:title "Iris sepals" :x "sepal length [cm]" :y "sepal width [cm]"}}
                   {:width 600 :height 350})

;; The plot below titles the colour legend over its scale's `:title` and gives the size legend a label map with its
;; own role:

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :sepal-width :color :species :size :petal-width}
                    :charts [(chart/scatter)]
                    :scales {:color {:title "ignored"}}
                    :labels {:color "Species" :size {:text "petal width [cm]" :role :axis-title}}}
                   {:width 600 :height 350})

;; ## Label maps

;; A label is a string or a label map. These keys of a label map give its text, its rotation and the theme text role it
;; takes its style from; a label in a custom grid
;; ([Facets, multiplots and margins](reference.facets_arrange.html#custom-grid)) must name its role. In the plot below the y title is horizontal and styled as a tick label:

(nu/option-cards [:labels :title] (describe/options label/label-options) :label)

(plot/render-image {:data iris
                    :mapping {:x :petal-length :y :petal-width}
                    :charts [(chart/scatter)]
                    :labels {:x {:text "petal length\n[cm]"}
                             :y {:text "petal\nwidth" :rotation 0 :role :tick-label}}}
                   {:width 600 :height 350})

;; ## Label style

;; The keys of the [Text style](reference.shared_options.html#text-style) group in a label map override its role's
;; style for this label only:

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
;; ([text](reference.charts_annotations.html#text-labels)). The plot below labels each line's end and writes a note
;; in panel units:

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

;; The plot below puts formulas in the title, the axis titles, the tick labels, the legend and a text chart:

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

;; ## Fonts

;; `font/resolve-font` turns a family name, a style (`:plain`, `:bold`, `:italic`, `:bold-italic`) and a size in points
;; into the `java.awt.Font` drawn; use it to see which font a name gives or to hand a text style a font object, since
;; a style's `:font` takes a family name or a `java.awt.Font`. Bundled families
;; (`"DejaVu Sans Mono"` - the default theme's font, `"Noto Emoji"`) win over installed ones, so plots render alike on
;; every platform; installed and logical families (`"Serif"`, `"SansSerif"` ...) work too, an unknown name throws
;; `{:key :font}`.

(nu/argument-list #'font/resolve-font)

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
