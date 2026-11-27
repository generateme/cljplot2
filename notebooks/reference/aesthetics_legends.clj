;; # Aesthetics and legends

;; Five aesthetics style marks from data: `:color`, `:shape`, `:size`, `:dash` and `:pattern`. Each mapped
;; aesthetic has a scale (`:scales {:color spec}`) mapping its domain (levels or a numeric range) onto attributes
;; (colours, shapes ...); each scale draws a legend unless its `:legend` is false. All five take one spec: most
;; options act on colour and size scales, and shape, dash and pattern scales differ mainly in their `:range`.

(ns reference.aesthetics-legends
  (:require [fastmath.core :as m]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [cljplot2.scale.aesthetic :as aesthetic]
            [notebook-utils :as nu])
  (:import [java.time LocalDate]))

;; The table below holds the Gapminder countries of 2007; most sections draw them:

(nu/source-note {:dataset :gapminder-2007})

(def gapminder (nu/dataset :gapminder-2007))

(nu/data-table gapminder)

;; The table below holds made-up readings on dates; the first section colours them by date:

(def readings (for [i (range 40)]
                {:day (.plusDays (LocalDate/of 2024 1 1) (* 9 i)) :x (mod (* i 7) 13) :y (mod (* i 5) 11)}))

(nu/data-table readings)

;; The table below holds the values 1 to 20 and their tenths; the sections on placing values and on sizes draw them:

(def ramp (for [i (range 1 21)] {:i i :tenth (* 0.1 i) :row "value"}))

(nu/data-table ramp)

;; The table below holds the mtcars cars; the legend section draws them:

(nu/source-note {:dataset :mtcars})

(def mtcars (nu/dataset :mtcars))

(nu/data-table mtcars)

;; The table below holds three made-up groups of points; the shape and dash sections draw them:

(def groups (for [[gi g] (map-indexed vector ["a" "b" "c"]) i (range 8)]
              {:g g :x i :y (+ (* 3 gi) (mod (* i 7) 3))}))

(nu/data-table groups)

;; ## Levels or numbers

;; These options set the scale's kind and domain. An aesthetic scale has kinds of its own (`:ordinal`, `:linear`,
;; `:binned`, `:identity`); the `:log` kind in the plots below belongs to the x axis
;; ([Scales, axes and coordinates](reference.scales_axes_coords.html)). The left plot colours the continents in a
;; given order, the right one life expectancy on a gradient:

(nu/option-cards [:scales :color] (describe/options (aesthetic/scale-spec-schema :color)) :levels)

(plot/render-image (plot/arrange [{:data gapminder
                                   :mapping {:x :gdp-per-cap :y :life-exp :color :continent}
                                   :charts [(chart/scatter)]
                                   :scales {:x {:kind :log :formatter :power}
                                            :color {:domain ["Europe" "Asia" "Africa" "Americas" "Oceania"]}}}
                                  {:data gapminder
                                   :mapping {:x :gdp-per-cap :y :life-exp :color :life-exp}
                                   :charts [(chart/scatter)]
                                   :scales {:x {:kind :log :formatter :power}}}]
                                 {:columns 2})
                   {:width 900 :height 350})

;; A colour scale maps dates (`java.time` values) linearly onto its gradient and ticks its colour bar at dates. A
;; `Year` or `YearMonth` reads as the date its period starts, on every scale:

(plot/render-image {:data readings
                    :mapping {:x :x :y :y :color :day}
                    :charts [(chart/scatter {:size 12})]}
                   {:width 600 :height 320})

;; Its `:ticks` take dates, and `:formatter` gets ticks in the data's type (here `LocalDate`s):

(plot/render-image {:data readings
                    :mapping {:x :x :y :y :color :day}
                    :charts [(chart/scatter {:size 12})]
                    :scales {:color {:ticks [(LocalDate/of 2024 3 1) (LocalDate/of 2024 6 1) (LocalDate/of 2024 9 1)]
                                     :formatter #(str "Q" (inc (quot (dec (.getMonthValue ^LocalDate %)) 3)) " " (.getYear ^LocalDate %))}}}
                   {:width 600 :height 320})

;; ### Values outside the domain

;; An explicit `:domain` narrower than the data leaves values outside it, and `:out-of-domain` decides what happens
;; to them. The plots below give the colour scale a domain of 60 to 75 years of life expectancy, on a gradient whose
;; ends both show. The left plot drops the countries outside, so its y axis shrinks with them. The middle one draws
;; them in the NA colour and adds an `outside` swatch under the colour bar. The right one draws them in the end
;; colours and marks each end with a triangle. Without the option, scatter drops them too; bar draws them in the NA
;; colour:

(plot/render-image (plot/arrange (for [option [:drop :as-na :clamp]]
                                   {:data gapminder
                                    :mapping {:x :gdp-per-cap :y :life-exp :color :life-exp}
                                    :charts [(chart/scatter)]
                                    :scales {:x {:kind :log :formatter :power}
                                             :color {:domain [60 75] :range :viridis/viridis :out-of-domain option}}
                                    :labels {:title (str ":out-of-domain " option)}})
                                 {:columns 3})
                   {:width 1000 :height 350})

;; ## Colour range

;; This option sets the colours. clojure2d names 7339 palettes and 5009 gradients:

(nu/option-cards [:scales :color] (describe/options (aesthetic/scale-spec-schema :color)) :range)

;; Search palettes and gradients by name with `plot/find-palette` and `plot/find-gradient` (a regular expression,
;; or a namespace and one); `plot/palette` returns a palette's colours, `plot/gradient` a gradient function:

(plot/find-palette #"tableau")

;; The plot below draws a few palettes, a [tile](reference.charts_bins_fields.html#heatmaps) per colour; an `:identity`
;; colour scale reads the colours from the data:

(def palettes (plot/find-palette #"^:(rcartocolor/(Bold|Pastel|Vivid|Prism)|tableau-10)$"))

(def palette-swatches (for [p palettes [i col] (map-indexed vector (plot/palette p))]
                        {:palette (str p) :i i :color col}))

(nu/data-table palette-swatches)

(plot/render-image {:data palette-swatches
                    :mapping {:x :i :y :palette :color :color}
                    :charts [(chart/tile)]
                    :scales {:color {:kind :identity :legend false}
                             :y {:reverse true}
                             :x {:ticks 12}}}
                   {:width 700 :height 280})

;; The plot below samples the viridis gradients at 32 points and draws tiles without outlines (`:stroke-color :none` -
;; [Cell style](reference.shared_options.html#cell-style)):

(def gradients (plot/find-gradient "viridis" #".*"))

(def gradient-swatches (for [g gradients
                             :let [gradient (plot/gradient g)]
                             i (range 32)]
                         {:gradient (str g) :i i :color (gradient (/ i 31.0))}))

(nu/data-table gradient-swatches)

(plot/render-image {:data gradient-swatches
                    :mapping {:x :i :y :gradient :color :color}
                    :charts [(chart/tile {:stroke-color :none})]
                    :scales {:color {:kind :identity :legend false}
                             :y {:reverse true}
                             :x {:axis false}}}
                   {:width 700 :height 300})

;; ## Placing values on the range

;; These options set how values spread over the attributes. The plots below centre a diverging gradient on 65
;; (left) and place the population by its logarithm on a reversed gradient (right):

(nu/option-cards [:scales :color] (describe/options (aesthetic/scale-spec-schema :color)) :range-mapping)

(plot/render-image (plot/arrange [{:data gapminder
                                   :mapping {:x :gdp-per-cap :y :life-exp :color :life-exp}
                                   :charts [(chart/scatter)]
                                   :scales {:x {:kind :log :formatter :power}
                                            :color {:range [:steelblue :white :firebrick] :midpoint 65
                                                    :title "life-exp, midpoint 65"}}}
                                  {:data gapminder
                                   :mapping {:x :gdp-per-cap :y :life-exp :color :pop}
                                   :charts [(chart/scatter)]
                                   :scales {:x {:kind :log :formatter :power}
                                            :color {:transform :log :range :viridis/viridis :reverse true
                                                    :title "pop, log, reversed"}}}]
                                 {:columns 2})
                   {:width 900 :height 350})

;; The colour bar inverts a `:transform` function numerically. The plots below colour the values 1 to 20 under each
;; transform (`:exp` the values 0.1 to 2, since the exponential of 20 dwarfs everything):

(plot/render-image (plot/arrange (for [[title transform] [["none" nil] [":sqrt" :sqrt] [":log" :log]
                                                         [":sq" :sq] [":exp" :exp] ["cube root (fn)" #(m/cbrt %)]]]
                                   {:data ramp
                                    :mapping {:x :i :y :row :color (if (= :exp transform) :tenth :i)}
                                    :charts [(chart/tile)]
                                    :scales (cond-> {:color {:title nil}} transform (assoc-in [:color :transform] transform))
                                    :labels {:title title :y nil}})
                                 {:columns 3})
                   {:width 900 :height 330})

;; ## Bins

;; This option sets the classes of a `:binned` scale, which draws a colour-bar block or a legend key per class. The
;; plots below bin life expectancy four ways:

(nu/option-cards [:scales :color] (describe/options (aesthetic/scale-spec-schema :color)) :bins)

(plot/render-image (plot/arrange (for [[title bins] [["3 equal bins" 3] ["breaks [39 50 60 70 83]" [39 50 60 70 83]]
                                                    ["method :sturges" :sturges] ["{:quantiles 4}" {:quantiles 4}]]]
                                   {:data gapminder
                                    :mapping {:x :gdp-per-cap :y :life-exp :color :life-exp}
                                    :charts [(chart/scatter)]
                                    :scales {:x {:kind :log :formatter :power}
                                             :color {:kind :binned :bins bins :range :viridis/viridis :title nil}}
                                    :labels {:title title}})
                                 {:columns 2})
                   {:width 900 :height 600})

;; A `:binned` size scale takes the same `:bins` and draws a legend entry per class, labelled by its breaks. Class `i`
;; of `n` gets `(i + 1) / n` of the `:range` (of its area with `:area`), so the largest class gets the range's maximum
;; and the first stays visible with `:range [0 m]`:

(plot/render-image {:data gapminder
                    :mapping {:x :gdp-per-cap :y :life-exp :size :pop}
                    :charts [(chart/scatter {:alpha 0.6})]
                    :scales {:x {:kind :log :formatter :power}
                             :size {:kind :binned :bins [0 1.0e7 5.0e7 2.0e8 1.4e9] :range [0 24] :area true
                                    :formatter #(str (m/round (m// % 1.0e6)) "M")}}}
                   {:width 700 :height 380})

;; ## Legend

;; These options set the scale's legend. Aesthetics mapped to the same column with equal levels share one legend; the
;; plot's `:legend false` hides all of them. Legends sit right of the panels; the [theme](reference.theme.html#legend)
;; sets their sizes and box. The plot below renames the size legend and picks its entries:

(nu/option-cards [:scales :color] (describe/options (aesthetic/scale-spec-schema :color)) :legend)

(plot/render-image {:data gapminder
                    :mapping {:x :gdp-per-cap :y :life-exp :color :continent :shape :continent :size :pop}
                    :charts [(chart/scatter)]
                    :scales {:x {:kind :log :formatter :power}
                             :color {:title "continent"}
                             :size {:title "population" :ticks [1e7 1e8 1e9]
                                    :formatter #(format "%.0f M" (/ (double %) 1e6))}}}
                   {:width 700 :height 380})

;; A colour bar takes `:ticks` and `:formatter` too:

(plot/render-image {:data gapminder
                    :mapping {:x :gdp-per-cap :y :life-exp :color :life-exp}
                    :charts [(chart/scatter)]
                    :scales {:x {:kind :log :formatter :power}
                             :color {:ticks [45 55 65 75] :formatter #(str % " years") :title "life expectancy"}}}
                   {:width 600 :height 350})

;; Counted entries of whole-number data over a short range fall on fractions (left): here 3 to 5 gears (size) and the
;; 0 / 1 engine shape `vs` (colour). `:integer-ticks true` keeps only whole ones, on a size legend and a colour bar alike
;; (right):

(plot/render-image (plot/arrange [{:data mtcars
                                   :mapping {:x :wt :y :mpg :size :gear :color :vs}
                                   :charts [(chart/scatter)]
                                   :scales {:color {:range [[160 200 235] [8 48 107]]}}
                                   :labels {:title "counted ticks"}}
                                  {:data mtcars
                                   :mapping {:x :wt :y :mpg :size :gear :color :vs}
                                   :charts [(chart/scatter)]
                                   :scales {:size {:integer-ticks true} :color {:integer-ticks true :range [[160 200 235] [8 48 107]]}}
                                   :labels {:title ":integer-ticks true"}}]
                                 {:columns 2})
                   {:width 1000 :height 360})

;; ## Shape

;; This option sets the point shapes: characters - filled and outlined circles, squares, triangles, strokes, plus and
;; cross - or shape maps (an SVG path, a text, an SVG file, an image).

(nu/option-cards [:scales :shape] (describe/options (aesthetic/scale-spec-schema :shape)) :range)

;; The plot below draws every built-in shape (`describe/attribute-values` lists them with the built-in dashes,
;; patterns and cells); an `:identity` scale reads the shapes from the data:

(def shapes (map-indexed (fn [i s] {:i i :shape s :name (str s)}) (:shape describe/attribute-values)))

(nu/data-table shapes)

(plot/render-image {:data shapes
                    :mapping {:x :i :y (constantly 1) :shape :shape}
                    :charts [(chart/scatter {:size 18 :color [0 114 178]})
                             (chart/text {:mapping {:x :i :y (constantly 0.4) :label :name}})]
                    :scales {:shape {:kind :identity :legend false} :y {:domain [0 1.5] :axis false}}}
                   {:width 800 :height 200})

;; The plot below takes a shape range of characters and shape maps:

(plot/render-image {:data groups
                    :mapping {:x :x :y :y :shape :g}
                    :charts [(chart/scatter {:size 14})]
                    :scales {:shape {:range [\s {:path "M0 0 L10 0 L5 9 Z"} {:text "★"}]}}}
                   {:width 500 :height 280})

;; ## Size

;; This option sets the sizes. The plot below grows population bubbles by area from 4 to 50 pixels:

(nu/option-cards [:scales :size] (describe/options (aesthetic/scale-spec-schema :size)) :range)

(plot/render-image {:data gapminder
                    :mapping {:x :gdp-per-cap :y :life-exp :size :pop}
                    :charts [(chart/scatter)]
                    :scales {:x {:kind :log :formatter :power}
                             :size {:range [4 50] :area true :legend false}}}
                   {:width 600 :height 350})

;; This option sets how a size scale's marks grow:

(nu/option-cards [:scales :size] (describe/options (aesthetic/scale-spec-schema :size)) :size)

;; The left plot grows sizes by diameter, the right one by area; only the right one reads as proportional:

(plot/render-image (plot/arrange (for [area [false true]]
                                   {:data ramp
                                    :mapping {:x :i :y :row :size :i}
                                    :charts [(chart/scatter)]
                                    :scales {:size {:range [2 26] :area area :legend false}}
                                    :labels {:title (str ":area " area) :y nil}})
                                 {:columns 2})
                   {:width 900 :height 200})

;; ## Dash

;; This option sets the line dashes: named styles or dash arrays, the lengths in pixels of a dash, a gap, a dash ...
;; repeated along the line.

(nu/option-cards [:scales :dash] (describe/options (aesthetic/scale-spec-schema :dash)) :range)

;; The plot below draws every named style and two dash arrays (an `:identity` scale draws the data's dashes; the names are a
;; [text](reference.charts_annotations.html#text-labels) chart anchored right of its point and moved left by `:offset`):

(def dashes (concat (for [[i d] (map-indexed vector (:dash describe/attribute-values)) x [0 10]]
                      {:i i :x x :dash d :name (str d)})
                    (for [[i d] [[8 [12 4]] [9 [2 2 8 2]]] x [0 10]]
                      {:i i :x x :dash d :name (pr-str d)})))

(nu/data-table dashes)

(plot/render-image {:data dashes
                    :mapping {:x :x :y :i :dash :dash :group :i}
                    :charts [(chart/line {:line-width 2 :color :black})
                             (chart/text {:data (filter #(zero? (long (:x %))) dashes)
                                          :mapping {:x :x :y :i :label :name} :anchor [:right :middle] :offset [-8 0]})]
                    :scales {:dash {:kind :identity :legend false}
                             :x {:domain [-6 10] :axis false} :y {:axis false :reverse true}}}
                   {:width 600 :height 380})

;; The plot below takes a dash range by group:

(plot/render-image {:data groups
                    :mapping {:x :x :y :y :dash :g}
                    :charts [(chart/line {:line-width 2})]
                    :scales {:dash {:range [:solid :dotted [8 3 2 3]]}}}
                   {:width 500 :height 280})

;; ## Pattern

;; This option sets the fill patterns of areas and bars: named patterns or pattern maps - lines or crosses
;; `{:kind :lines :angle a :spacing s :width w}` (`a` 0, 45, 90 or 135) or dots `{:kind :dots :spacing s :size d}`.

(nu/option-cards [:scales :pattern] (describe/options (aesthetic/scale-spec-schema :pattern)) :range)

;; The plot below fills bars with every named pattern and two pattern maps (white bars outlined in black, the pattern in black -
;; [Fill patterns](reference.shared_options.html#fill-patterns)):

(def patterns (concat (for [p (:pattern describe/attribute-values)] {:name (name p) :pattern p :v 1})
                      [{:name "lines 135, spacing 8" :pattern {:kind :lines :angle 135 :spacing 8 :width 2} :v 1}
                       {:name "dots, spacing 5" :pattern {:kind :dots :spacing 5 :size 3} :v 1}]))

(nu/data-table patterns)

(plot/render-image {:data patterns
                    :mapping {:x :name :y :v :pattern :pattern}
                    :charts [(chart/bar {:color :white :stroke-color :black :pattern-color :black :width 0.9})]
                    :scales {:pattern {:kind :identity :legend false} :y {:axis false}}
                    :theme {:axis {:x {:label-rotation 45}}}}
                   {:width 800 :height 320})
