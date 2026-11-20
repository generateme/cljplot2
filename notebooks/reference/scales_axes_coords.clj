;; # Scales, axes and coordinates

;; Position scales map `:x` and `:y` data to the panel; their kind follows the data (numbers, levels, dates) unless
;; set. Each position scale draws an axis and grid lines. A plot is cartesian unless `:coord` is polar; `:flip`
;; swaps the directions and `:aspect :iso` keeps data units square.

(ns reference.scales-axes-coords
  (:require [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [cljplot2.scale.coord :as coord]
            [cljplot2.scale.position :as position]
            [cljplot2.schema :as schema]
            [notebook-utils :as nu])
  (:import [java.time LocalDate]))

(nu/source-note {:dataset :gapminder-2007})

(def gapminder (nu/dataset :gapminder-2007))

(def counts (for [[continent rs] (group-by :continent gapminder)] {:continent continent :n (count rs)}))

;; ## Kind and domain

;; The scale kind (see Scale kinds below) and the data range it covers: the charts' data extent unless `:domain`
;; is given - `[lo hi]` for continuous kinds, the levels in order for `:bands`. `:type :categorical` makes numbers
;; levels (left: cylinder counts as three bands).

(nu/options-table (describe/options position/scale-spec-schema) :kind)

(nu/source-note {:dataset :mtcars})

(def mtcars (nu/dataset :mtcars))

(plot/render-image (plot/arrange [{:data mtcars
                                   :mapping {:x :cyl :y :mpg}
                                   :charts [(chart/box)]
                                   :scales {:x {:type :categorical}}}
                                  {:data mtcars
                                   :mapping {:x :wt :y :mpg}
                                   :charts [(chart/scatter)]
                                   :scales {:x {:domain [0 6]} :y {:domain [0 40]}}}]
                                 {:columns 2})
                   {:width 800 :height 320})

;; ## Ticks and labels

;; Where ticks go and how they read: a tick count hint or the tick values, nice domain ends, a formatter (a
;; function, a date pattern, `:power` for powers of ten), minor ticks and the tick algorithm.

(nu/options-table (describe/options position/scale-spec-schema) :ticks)

(plot/render-image {:data gapminder
                    :mapping {:x :gdp-per-cap :y :life-exp}
                    :charts [(chart/scatter)]
                    :scales {:x {:kind :log :formatter :power :minor-ticks 9}
                             :y {:ticks [40 50 60 70 80] :formatter #(str % " y") :nice false}}}
                   {:width 600 :height 350})

;; ## Order and direction

;; The order of bands - by a function of each level's rows, optionally descending - and the axis direction.

(nu/options-table (describe/options position/scale-spec-schema) :order)

(plot/render-image (plot/arrange [{:data counts
                                   :mapping {:x :continent :y :n}
                                   :charts [(chart/bar)]
                                   :scales {:x {:order-by #(reduce + (map :n %)) :descending true}}
                                   :labels {:title "ordered by count"}}
                                  {:data counts
                                   :mapping {:x :continent :y :n}
                                   :charts [(chart/bar)]
                                   :scales {:y {:reverse true}}
                                   :labels {:title "y reversed"}}]
                                 {:columns 2})
                   {:width 900 :height 320})

;; ## Axis

;; What the axis draws (`:grid` - grid lines only, `:no-grid` - the axis only, `false` - nothing), a display-only
;; domain for its ticks, and the padding around the data.

(nu/options-table (describe/options position/scale-spec-schema) :axis)

(plot/render-image (plot/arrange [{:data mtcars
                                   :mapping {:x :wt :y :mpg}
                                   :charts [(chart/scatter)]
                                   :scales {:x {:axis :no-grid :expand [0 0]} :y {:axis :grid}}
                                   :labels {:title "x :no-grid, no padding; y :grid"}}
                                  {:data mtcars
                                   :mapping {:x :wt :y :mpg}
                                   :charts [(chart/scatter)]
                                   :scales {:y {:axis-domain [0 1]}}
                                   :labels {:title "y ticks of [0 1] over the panel"}}]
                                 {:columns 2})
                   {:width 900 :height 320})

;; ## wadogo attributes

;; Options passed to the [wadogo](https://github.com/scicloj/wadogo) scale of the kind; each kind below lists the
;; ones it reads.

(nu/options-table (describe/options position/scale-spec-schema) :wadogo)

(plot/render-image {:data counts
                    :mapping {:x :continent :y :n}
                    :charts [(chart/bar {:width 1.0})]
                    :scales {:x {:padding-in 0.5 :padding-out 1.0}}}
                   {:width 500 :height 280})

;; ## Scale kinds

;; Each kind below is described by wadogo (`wadogo.scale/kind-info`). cljplot2 sets a kind's `:domain` (the data
;; extent or the spec's `:domain`), its `:range` (the panel's pixels), ticks and tick format itself - through the
;; options above; the remaining attributes of a kind are the "wadogo:" options.

;; ### Linear

(nu/scale-kind-doc :linear)

(plot/render-image {:data mtcars
                    :mapping {:x :hp :y :mpg}
                    :charts [(chart/scatter)]}
                   {:width 500 :height 250})

;; ### Log

;; Positive values only; `:base` changes the tick values (here powers of two).

(nu/scale-kind-doc :log)

(plot/render-image {:data (for [i (range 1 11)] {:x (Math/pow 2 i) :y i})
                    :mapping {:x :x :y :y}
                    :charts [(chart/line) (chart/scatter)]
                    :scales {:x {:kind :log :base 2}}}
                   {:width 500 :height 250})

;; ### Pow

(nu/scale-kind-doc :pow)

(plot/render-image {:data mtcars
                    :mapping {:x :hp :y :mpg}
                    :charts [(chart/scatter)]
                    :scales {:x {:kind :pow :exponent 0.5}}}
                   {:width 500 :height 250})

;; ### Symlog

;; For data of both signs over many magnitudes. The constant `:C` isn't passed through (wadogo's default 1.0).

(nu/scale-kind-doc :symlog)

(plot/render-image {:data (for [i (range -40 41)] {:x (* i i i) :y i})
                    :mapping {:x :x :y :y}
                    :charts [(chart/scatter)]
                    :scales {:x {:kind :symlog}}}
                   {:width 500 :height 250})

;; ### Bands

;; Levels; the default kind for strings and keywords.

(nu/scale-kind-doc :bands)

(plot/render-image {:data counts
                    :mapping {:x :continent :y :n}
                    :charts [(chart/bar)]
                    :scales {:x {:align-outer 0.0 :padding-out 2.0}}}
                   {:width 500 :height 250})

;; ### Datetime

;; `java.time` values; `:formatter` takes a date pattern.

(nu/scale-kind-doc :datetime)

(plot/render-image {:data (for [m (range 24)]
                            {:date (.plusMonths (LocalDate/of 2020 1 1) m) :v (+ 10 (mod (* m 7) 9))})
                    :mapping {:x :date :y :v}
                    :charts [(chart/line) (chart/scatter)]
                    :scales {:x {:formatter "MMM yyyy"}}}
                   {:width 600 :height 250})

;; ### Interpolated

;; A piecewise mapping of the domain values onto evenly spaced (or given) `:stops` of the panel, through an
;; `:interpolator`:

(nu/options-table (describe/options position/scale-spec-schema) :interpolated)

(nu/scale-kind-doc :interpolated)

;; The interpolators - fastmath 1D interpolation methods (or a function):

(sort schema/interpolation-methods)

;; Here the middle of the domain, 40 to 60, gets half of the axis:

(plot/render-image {:data (for [i (range 1 101)] {:x i :y (Math/log i)})
                    :mapping {:x :x :y :y}
                    :charts [(chart/line)]
                    :scales {:x {:kind :interpolated :domain [1 40 60 100] :ticks [1 20 40 50 60 80 100]}}}
                   {:width 600 :height 250})

;; ### Radial

;; Radii of areas proportional to the values - for polar charts whose bar areas should read as values.

(nu/scale-kind-doc :radial)

(plot/render-image {:data counts
                    :mapping {:x :continent :y :n :color :continent}
                    :charts [(chart/bar {:width 1.0})]
                    :scales {:y {:kind :radial}}
                    :coord {:kind :polar}
                    :legend false}
                   {:width 400 :height 400})

;; ## Coordinates

;; `:flip true` draws `:x` vertically; scales, labels and theme keys keep their names. `:aspect :iso` gives both
;; linear axes the same pixels per data unit (the panel shrinks to fit). Polar coordinates map the horizontal scale
;; to angles and the vertical one to radii.

(nu/options-table (describe/options coord/polar-schema))

;; Polar coordinates work with these charts:

(sort (for [[kind constructor] (describe/chart-constructors)
            :when (contains? (:coords (constructor)) :polar)]
        kind))

(plot/render-image (plot/arrange [{:data counts
                                   :mapping {:x :continent :y :n}
                                   :charts [(chart/bar)]
                                   :flip true
                                   :scales {:x {:reverse true}}
                                   :labels {:title ":flip true"}}
                                  {:data counts
                                   :mapping {:x :continent :y :n :color :continent}
                                   :charts [(chart/bar {:width 1.0})]
                                   :coord {:kind :polar :inner 0.2 :span 270 :start -135}
                                   :labels {:title "polar, 270 degrees"}
                                   :legend false}]
                                 {:columns 2})
                   {:width 900 :height 400})

(plot/render-image {:data (for [t (range 0 63)] {:x (Math/cos (* 0.1 t)) :y (* 0.5 (Math/sin (* 0.1 t)))})
                    :mapping {:x :x :y :y}
                    :charts [(chart/line)]
                    :aspect :iso
                    :labels {:title ":aspect :iso - an ellipse twice as wide as high"}}
                   {:width 500 :height 400})
