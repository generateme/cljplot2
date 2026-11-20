;; # Aesthetics and legends

;; Five aesthetics style marks from data: `:color`, `:shape`, `:size`, `:dash` and `:pattern`. Each mapped
;; aesthetic has a scale (`:scales {:color spec}`) mapping its domain (levels or a numeric range) onto attributes
;; (colours, shapes ...), and every scale draws a legend unless told not to. All five share one spec; the colour
;; scale uses every key, the others differ only in their `:range`.

(ns reference.aesthetics-legends
  (:require [clojure2d.color :as c]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [cljplot2.scale.aesthetic :as aesthetic]
            [notebook-utils :as nu]))

(nu/source-note {:dataset :gapminder-2007})

(def gapminder (nu/dataset :gapminder-2007))

;; ## Levels or numbers

;; The scale kind follows the data: levels (strings, keywords) get an `:ordinal` scale - one attribute per level, in
;; `:domain` order - numbers a `:linear` one - a gradient or a size range. `:type :categorical` treats numbers as
;; levels; `:identity` reads the attributes from the data.

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

;; ## Colour range

;; The attributes the domain maps to: a sequence of colours, a level → colour map, or a clojure2d palette or
;; gradient name - 7339 palettes and 5009 gradients. Without `:range` the theme `:palette` supplies it.

(nu/options-table (describe/options (aesthetic/scale-spec-schema :color)) :range)

;; Search palettes and gradients by name with `clojure2d.color/find-palette` and `find-gradient`:

(c/find-palette #"tableau")

;; Swatches of a few palettes (an `:identity` colour scale reads the colours from the data):

(def palettes (c/find-palette #"^:(rcartocolor/(Bold|Pastel|Vivid|Prism)|tableau-10)$"))

(plot/render-image {:data (for [p palettes [i col] (map-indexed vector (c/palette p))]
                            {:palette (str p) :i i :color col})
                    :mapping {:x :i :y :palette :color :color}
                    :charts [(chart/tile)]
                    :scales {:color {:kind :identity :legend false}
                             :y {:reverse true}
                             :x {:ticks 12}}}
                   {:width 700 :height 280})

;; Gradients, sampled the same way:

(def gradients (c/find-gradient #"^:viridis/"))

(plot/render-image {:data (for [g gradients i (range 32)]
                            {:gradient (str g) :i i :color ((c/gradient g) (/ i 31.0))})
                    :mapping {:x :i :y :gradient :color :color}
                    :charts [(chart/tile {:stroke-color :none})]
                    :scales {:color {:kind :identity :legend false}
                             :y {:reverse true}
                             :x {:axis false}}}
                   {:width 700 :height 300})

;; ## Placing values on the range

;; How values spread over the attributes: a diverging gradient around `:midpoint`, values placed by their square
;; root or logarithm (`:transform`), the range reversed, size by area.

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

;; ## Bins

;; A `:binned` colour scale cuts numbers into classes - a count of equal bins, breaks, a binning method or
;; quantile bins - and draws a key per class.

(nu/options-table (describe/options (aesthetic/scale-spec-schema :color)) :bins)

(plot/render-image {:data gapminder
                    :mapping {:x :gdp-per-cap :y :life-exp :color :life-exp}
                    :charts [(chart/scatter)]
                    :scales {:x {:kind :log :formatter :power}
                             :color {:kind :binned :bins {:quantiles 5} :range :viridis/viridis}}}
                   {:width 600 :height 350})

;; ## Legend

;; One legend per mapped aesthetic, titled by the mapped column's name: `:title` renames it, `:formatter` writes its
;; labels, `:ticks` picks the entries of a numeric scale, `:legend false` hides it (the plot's `:legend false` hides
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

;; ## Shape

;; Point shapes: characters (`\O` circle, `\s` square, `\A` triangle ...), keywords or shape maps.

(nu/options-table (describe/options (aesthetic/scale-spec-schema :shape)) :range)

(def groups (for [g ["a" "b" "c"] i (range 8)] {:g g :x i :y (+ (* 2 (count (str g i))) (mod (* i 7) 5))}))

(plot/render-image {:data groups
                    :mapping {:x :x :y :y :shape :g}
                    :charts [(chart/scatter {:size 10})]
                    :scales {:shape {:range [\s \A \x]}}}
                   {:width 500 :height 280})

;; ## Size

;; Sizes `[min max]` for numbers (a level map or a list for levels); `:area true` grows the area, not the
;; diameter, with the value.

(nu/options-table (describe/options (aesthetic/scale-spec-schema :size)) :range)

(plot/render-image {:data gapminder
                    :mapping {:x :gdp-per-cap :y :life-exp :size :pop}
                    :charts [(chart/scatter)]
                    :scales {:x {:kind :log :formatter :power}
                             :size {:range [2 30] :area true :legend false}}}
                   {:width 500 :height 300})

;; ## Dash

;; Line dashes: named styles or dash arrays.

(nu/options-table (describe/options (aesthetic/scale-spec-schema :dash)) :range)

(plot/render-image {:data groups
                    :mapping {:x :x :y :y :dash :g}
                    :charts [(chart/line {:width 2})]
                    :scales {:dash {:range [:solid :dotted [8 3 2 3]]}}}
                   {:width 500 :height 280})

;; ## Pattern

;; Fill patterns of areas and bars: names or pattern maps.

(nu/options-table (describe/options (aesthetic/scale-spec-schema :pattern)) :range)

(plot/render-image {:data [{:k "a" :v 3} {:k "b" :v 5} {:k "c" :v 4}]
                    :mapping {:x :k :y :v :pattern :k}
                    :charts [(chart/bar {:color :white :stroke-color :black :pattern-color :black})]
                    :scales {:pattern {:range [:dots :cross {:kind :lines :angle 45 :spacing 4 :width 1}]}}}
                   {:width 500 :height 280})
