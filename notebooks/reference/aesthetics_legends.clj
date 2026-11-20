;; # Aesthetics and legends

;; Five aesthetics style marks from data: `:color`, `:shape`, `:size`, `:dash` and `:pattern`. Each mapped
;; aesthetic has a scale (`:scales {:color spec}`), and every scale draws a legend unless told not to. The ranges
;; default to the theme `:palette`.

(ns reference.aesthetics-legends
  (:require [clojure2d.color :as c]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [cljplot2.scale.aesthetic :as aesthetic]
            [notebook-utils :as nu]))

;; ## Colour scale

;; Categorical data takes an `:ordinal` scale (palette colours in level order), numbers a `:linear` gradient;
;; `:binned` cuts numbers into classes, `:identity` reads colours from the data.

(nu/options-table (describe/options (aesthetic/scale-spec-schema :color)))

(nu/source-note {:dataset :gapminder-2007})

(def gapminder (nu/dataset :gapminder-2007))

(plot/render-image {:data gapminder
                    :mapping {:x :gdp-per-cap :y :life-exp :color :continent :size :pop}
                    :charts [(chart/scatter)]
                    :scales {:x {:kind :log :formatter :power}
                             :size {:range [2 24] :area true :title "population"}}}
                   {:width 800 :height 500})

;; A numeric colour: a gradient with a colour bar (left), the same values in five quantile classes (right).

(plot/render-image (plot/arrange [{:data gapminder
                                   :mapping {:x :gdp-per-cap :y :life-exp :color :life-exp}
                                   :charts [(chart/scatter)]
                                   :scales {:x {:kind :log :formatter :power}
                                            :color {:range :viridis/viridis}}}
                                  {:data gapminder
                                   :mapping {:x :gdp-per-cap :y :life-exp :color :life-exp}
                                   :charts [(chart/scatter)]
                                   :scales {:x {:kind :log :formatter :power}
                                            :color {:kind :binned :bins {:quantiles 5} :range :viridis/viridis}}}]
                                 {:columns 2})
                   {:width 900 :height 400})

;; ## Shape, size, dash and pattern scales

;; The other aesthetics share the colour scale's schema; each dimension accepts the keys its marks use (shapes,
;; sizes `[min max]`, dash styles, fill patterns).

(nu/options-table (describe/options (aesthetic/scale-spec-schema :shape)))

(nu/options-table (describe/options (aesthetic/scale-spec-schema :size)))

(nu/options-table (describe/options (aesthetic/scale-spec-schema :dash)))

(nu/options-table (describe/options (aesthetic/scale-spec-schema :pattern)))

;; ## Legends

;; One legend per mapped aesthetic scale, titled by the mapped column's name: `:title` renames it, `:legend false`
;; hides it, the plot's `:legend false` hides them all. Aesthetics mapped to the same column with equal levels share
;; one legend (`:color :g :shape :g` - one legend of coloured shapes). Legends sit right of the panels; their sizes
;; and frame are in the theme `:legend` section (Theme chapter).

;; ## Palettes and gradients

;; A colour `:range` takes a sequence of colours, a map level → colour, or a clojure2d palette or gradient name -
;; 7339 palettes and 5009 gradients. Search them by name with `clojure2d.color/find-palette` and `find-gradient`:

(c/find-palette #"tableau")

;; Swatches of a few found palettes (an `:identity` colour scale reads the colours from the data):

(def palettes (c/find-palette #"^:(rcartocolor/(Bold|Pastel|Vivid|Prism)|tableau-10)$"))

(plot/render-image {:data (for [p palettes [i col] (map-indexed vector (c/palette p))]
                            {:palette (str p) :i i :color col})
                    :mapping {:x :i :y :palette :color :color}
                    :charts [(chart/tile)]
                    :scales {:color {:kind :identity :legend false}
                             :y {:reverse true}
                             :x {:ticks 12}}}
                   {:width 700 :height 300})

;; Gradients are sampled the same way:

(def gradients (c/find-gradient #"^:viridis/"))

(plot/render-image {:data (for [g gradients i (range 32)]
                            {:gradient (str g) :i i :color ((c/gradient g) (/ i 31.0))})
                    :mapping {:x :i :y :gradient :color :color}
                    :charts [(chart/tile {:stroke-color :none})]
                    :scales {:color {:kind :identity :legend false}
                             :y {:reverse true}
                             :x {:axis false}}}
                   {:width 700 :height 300})
