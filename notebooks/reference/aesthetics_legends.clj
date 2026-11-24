;; # Aesthetics and legends

;; Five aesthetics style marks from data: `:color`, `:shape`, `:size`, `:dash` and `:pattern`. Each mapped
;; aesthetic has a scale (`:scales {:color spec}`) mapping its domain (levels or a numeric range) onto attributes
;; (colours, shapes ...), and every scale draws a legend unless told not to. All five share one spec; the colour
;; scale uses every key, the others differ only in their `:range`.

(ns reference.aesthetics-legends
  (:require [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [cljplot2.scale.aesthetic :as aesthetic]
            [notebook-utils :as nu])
  (:import [java.time LocalDate]))

(nu/source-note {:dataset :gapminder-2007})

(def gapminder (nu/dataset :gapminder-2007))

(nu/data-table gapminder)

;; ## Levels or numbers

;; An aesthetic scale has its own kinds - not those of the position scales (`:log`, `:datetime` ... in the
;; examples below are the x axis's, Scales chapter). The kind follows the data: levels (strings, keywords) get an
;; `:ordinal` scale - one attribute per level, in `:domain` order - numbers a `:linear` one - a gradient or a size
;; range; `:binned` cuts numbers into classes; `:type :categorical` treats numbers or dates as levels; `:identity`
;; reads the attributes from the data.

(nu/options-table (describe/options (aesthetic/scale-spec-schema :color)) :levels)

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

;; Dates (`java.time` values) mapped to `:color` get a linear gradient over time, the colour bar ticked at dates (a
;; `Year` / `YearMonth` reads as the date its period starts, on every scale):

(def readings (for [i (range 40)]
                {:day (.plusDays (LocalDate/of 2024 1 1) (* 9 i)) :x (mod (* i 7) 13) :y (mod (* i 5) 11)}))

(nu/data-table readings)

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

;; ## Colour range

;; The attributes the domain maps to: a sequence of colours, a level → colour map, or a clojure2d palette or
;; gradient name - 7339 palettes and 5009 gradients. Without `:range` the theme `:palette` supplies it.

(nu/options-table (describe/options (aesthetic/scale-spec-schema :color)) :range)

;; Search palettes and gradients by name with `plot/find-palette` and `plot/find-gradient` (a regular expression,
;; or a namespace and one); `plot/palette` gives a palette's colours, `plot/gradient` a gradient function:

(plot/find-palette #"tableau")

;; Swatches of a few palettes (an `:identity` colour scale reads the colours from the data):

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

;; Gradients, each sampled at 32 points:

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

;; How values spread over the attributes: a diverging gradient around `:midpoint`, values placed by a transform,
;; the range reversed, size by area.

(nu/options-table (describe/options (aesthetic/scale-spec-schema :color)) :range-mapping)

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

;; `:transform` places values along the gradient by `:sqrt`, `:log`, `:sq`, `:exp` or a function of the value; the
;; function must be monotonic over the domain (the colour bar inverts it numerically) and finite at its ends. The
;; values 1 to 20 under each (`:exp` - the values 0.1 to 2, the exponential of 20 dwarfs everything):

(def ramp (for [i (range 1 21)] {:i i :tenth (* 0.1 i) :row "value"}))

(nu/data-table ramp)

(plot/render-image (plot/arrange (for [[title transform] [["none" nil] [":sqrt" :sqrt] [":log" :log]
                                                         [":sq" :sq] [":exp" :exp] ["cube root (fn)" #(Math/cbrt %)]]]
                                   {:data ramp
                                    :mapping {:x :i :y :row :color (if (= :exp transform) :tenth :i)}
                                    :charts [(chart/tile)]
                                    :scales (cond-> {:color {:title nil}} transform (assoc-in [:color :transform] transform))
                                    :labels {:title title :y nil}})
                                 {:columns 3})
                   {:width 900 :height 330})

;; `:area true` grows a size scale's marks by area, not diameter - the right one reads as proportional:

(plot/render-image (plot/arrange (for [area [false true]]
                                   {:data ramp
                                    :mapping {:x :i :y :row :size :i}
                                    :charts [(chart/scatter)]
                                    :scales {:size {:range [2 26] :area area :legend false}}
                                    :labels {:title (str ":area " area) :y nil}})
                                 {:columns 2})
                   {:width 900 :height 200})

;; ## Bins

;; A `:binned` colour scale cuts numbers into classes and draws a key per class. `:bins` takes a count of equal
;; bins, the breaks themselves, a binning method (`:sturges`, `:sqrt`, `:rice`, `:doane`, `:scott` ...) or quantile
;; bins:

(nu/options-table (describe/options (aesthetic/scale-spec-schema :color)) :bins)

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

;; ## Legend

;; One legend per mapped aesthetic, titled by the mapped column's name: `:title` renames it, `:formatter` writes its
;; labels, `:ticks` picks the entries of a numeric scale (`:integer-ticks` keeps counted ones at whole numbers),
;; `:legend false` hides it (the plot's `:legend false` hides
;; them all). Aesthetics mapped to the same column with equal levels share one legend. Legends sit right of the
;; panels; their sizes and frame are in the theme `:legend` section (Theme chapter).

(nu/options-table (describe/options (aesthetic/scale-spec-schema :color)) :legend)

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

(nu/source-note {:dataset :mtcars})

(def mtcars (nu/dataset :mtcars))

(nu/data-table mtcars)

;; Whole-number data over a short range - here 3 to 5 gears (size) and the 0 / 1 engine shape `vs` (colour) - gets
;; fractional counted entries (left); `:integer-ticks true` keeps only whole ones, on a size legend and a colour bar
;; alike (right):

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

;; Point shapes: characters - filled and outlined circles, squares, triangles, strokes, plus and cross - or shape
;; maps (an SVG path, a text, an SVG file, an image).

(nu/options-table (describe/options (aesthetic/scale-spec-schema :shape)) :range)

;; Every built-in shape (`describe/attribute-values` lists the built-in shapes, dashes, patterns and cells; an
;; `:identity` scale draws the data's shapes):

(def shapes (map-indexed (fn [i s] {:i i :shape s :name (str s)}) (:shape describe/attribute-values)))

(nu/data-table shapes)

(plot/render-image {:data shapes
                    :mapping {:x :i :y (constantly 1) :shape :shape}
                    :charts [(chart/scatter {:size 18 :color [69 117 180]})
                             (chart/text {:mapping {:x :i :y (constantly 0.4) :label :name}})]
                    :scales {:shape {:kind :identity :legend false} :y {:domain [0 1.5] :axis false}}}
                   {:width 800 :height 200})

;; A shape range of characters and shape maps:

(def groups (for [[gi g] (map-indexed vector ["a" "b" "c"]) i (range 8)]
              {:g g :x i :y (+ (* 3 gi) (mod (* i 7) 3))}))

(nu/data-table groups)

(plot/render-image {:data groups
                    :mapping {:x :x :y :y :shape :g}
                    :charts [(chart/scatter {:size 14})]
                    :scales {:shape {:range [\s {:path "M0 0 L10 0 L5 9 Z"} {:text "★"}]}}}
                   {:width 500 :height 280})

;; ## Size

;; Sizes `[min max]` in pixels for numbers (a level map or a list for levels).

(nu/options-table (describe/options (aesthetic/scale-spec-schema :size)) :range)

(plot/render-image {:data gapminder
                    :mapping {:x :gdp-per-cap :y :life-exp :size :pop}
                    :charts [(chart/scatter)]
                    :scales {:x {:kind :log :formatter :power}
                             :size {:range [4 50] :area true :legend false}}}
                   {:width 600 :height 350})

;; ## Dash

;; Line dashes: a named style or a dash array - lengths in pixels of a dash, a gap, a dash, ... repeated along the
;; line.

(nu/options-table (describe/options (aesthetic/scale-spec-schema :dash)) :range)

;; Every named style, and two dash arrays (an `:identity` scale draws the data's dashes):

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

;; A dash range by group:

(plot/render-image {:data groups
                    :mapping {:x :x :y :y :dash :g}
                    :charts [(chart/line {:line-width 2})]
                    :scales {:dash {:range [:solid :dotted [8 3 2 3]]}}}
                   {:width 500 :height 280})

;; ## Pattern

;; Fill patterns of areas and bars: a named pattern or your own as a pattern map - lines or crosses
;; `{:kind :lines :angle a :spacing s :width w}` (`a` 0, 45, 90 or 135) or dots `{:kind :dots :spacing s :size d}`.

(nu/options-table (describe/options (aesthetic/scale-spec-schema :pattern)) :range)

;; Every named pattern and two pattern maps:

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
