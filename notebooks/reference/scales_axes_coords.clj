;; # Scales, axes and coordinates

;; Position scales map `:x` and `:y` data to the panel; their kind follows the data (numbers, levels, dates) unless
;; set. Each position scale draws an axis and grid lines. A plot is cartesian unless `:coord` is polar; `:flip`
;; swaps the directions and `:aspect :iso` keeps data units square.

(ns reference.scales-axes-coords
  (:require [fastmath.core :as m]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [cljplot2.scale.coord :as coord]
            [cljplot2.scale.position :as position]
            [cljplot2.schema :as schema]
            [notebook-utils :as nu])
  (:import [java.time LocalDate]))

(nu/source-note {:dataset :gapminder-2007})

(def gapminder (nu/dataset :gapminder-2007))

(nu/data-table gapminder)

(def counts (for [[continent rs] (group-by :continent gapminder)] {:continent continent :n (count rs)}))

(nu/data-table counts)

;; ## Kind and domain

;; The scale kind (see Scale kinds below) and the data range it covers: the charts' data extent unless `:domain`
;; is given - `[lo hi]` for continuous kinds, the levels in order for `:bands` - padded (`:expand`) and extended to
;; nice tick values (`:nice`). `:type :categorical` makes numbers levels (left: cylinder counts as three bands);
;; right: fixed domains, x without padding.

(nu/options-table (describe/options position/scale-spec-schema) :kind-domain)

(nu/source-note {:dataset :mtcars})

(def mtcars (nu/dataset :mtcars))

(nu/data-table mtcars)

(plot/render-image (plot/arrange [{:data mtcars
                                   :mapping {:x :cyl :y :mpg}
                                   :charts [(chart/box)]
                                   :scales {:x {:type :categorical}}}
                                  {:data mtcars
                                   :mapping {:x :wt :y :mpg}
                                   :charts [(chart/scatter)]
                                   :scales {:x {:domain [1.5 5.5] :expand [0 0] :nice false} :y {:domain [0 40]}}}]
                                 {:columns 2})
                   {:width 800 :height 320})

;; A partial domain fixes one end and takes the other from the data, as if there were no domain: `[0 nil]` starts
;; the axis at zero (padded and made nice as any explicit domain), `[nil hi]` fixes the top; `[nil nil]` is no domain.
;; Continuous kinds only (not `:interpolated`); the data must reach past the fixed end. Left: the data's range;
;; right: life expectancy from zero.

(plot/render-image (plot/arrange (for [[title y-scale] [["the data's range" {}] [":domain [0 nil]" {:domain [0 nil]}]]]
                                   {:data gapminder
                                    :mapping {:x :gdp-per-cap :y :life-exp}
                                    :charts [(chart/scatter)]
                                    :scales {:x {:kind :log} :y y-scale}
                                    :labels {:title title}})
                                 {:columns 2})
                   {:width 800 :height 320})

;; Which kind a plot's scales get - from the data and what the charts compute - is in the built plot's
;; `:scale-specs`: here a bar chart's levels on x, a histogram's bins and counts:

(into {} (for [[title p] {"bar" {:data mtcars :mapping {:x #(str (:cyl %) " cyl") :y :mpg} :charts [(chart/bar)]}
                          "histogram" {:data mtcars :mapping {:x :mpg} :charts [(chart/histogram)]}}]
           [title (update-vals (select-keys (:scale-specs (plot/build p)) [:x :y]) :kind)]))

;; ## Ticks and labels

;; Where ticks go and how they read: a tick count hint or the tick values, a formatter (a function, a date
;; pattern, `:power` for powers of ten), minor ticks and the tick algorithm. Minor ticks get grid lines only when
;; the theme draws them (`[:panel :grid :minor]`, `:none` by default) - turned on here:

(nu/options-table (describe/options position/scale-spec-schema) :ticks)

(plot/render-image {:data gapminder
                    :mapping {:x :gdp-per-cap :y :life-exp}
                    :charts [(chart/scatter)]
                    :scales {:x {:kind :log :formatter :power :minor-ticks 9}
                             :y {:ticks [40 50 60 70 80] :formatter #(str % " y")}}
                    :theme {:panel {:grid {:minor {:color [200 200 215] :line-width 0.5 :dash :none}}}}}
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
;; domain for its ticks, and whether tick labels overlapping a drawn one are dropped (`:drop`, the default for
;; computed ticks) or all drawn (`:keep`, the default for an explicit `:ticks` vector; builders naming data items on
;; an axis, such as dendrogram leaves, set it).

(nu/options-table (describe/options position/scale-spec-schema) :axis)

(plot/render-image (plot/arrange [{:data mtcars
                                   :mapping {:x :wt :y :mpg}
                                   :charts [(chart/scatter)]
                                   :scales {:x {:axis :no-grid} :y {:axis :grid}}
                                   :labels {:title "x :no-grid; y :grid"}}
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

(plot/render-image {:data (for [i (range 1 11)] {:x (m/pow 2 i) :y i})
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

;; Here the domain values 1, 40, 60 and 100 sit at evenly spaced stops, so each of the three segments gets a third
;; of the axis - 40 to 60 is stretched, 1 to 40 and 60 to 100 are squeezed:

(plot/render-image {:data (for [i (range 1 101)] {:x i :y (m/log i)})
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

;; In polar coordinates `:flip true` swaps the roles - the vertical scale on angles, the horizontal on radii
;; (left); `:arcs true` makes data lines between points follow the circle instead of chords (right):

(def spiral (for [t (range 0 13)] {:t t :r (+ 1 t)}))

(nu/data-table spiral)

(plot/render-image (plot/arrange [{:data counts
                                   :mapping {:x :continent :y :n :color :continent}
                                   :charts [(chart/bar {:width 0.8})]
                                   :coord {:kind :polar}
                                   :flip true
                                   :labels {:title "polar, :flip true"}
                                   :legend false}
                                  {:data spiral
                                   :mapping {:x :t :y :r}
                                   :charts [(chart/line) (chart/scatter)]
                                   :coord {:kind :polar :arcs true}
                                   :labels {:title "polar, :arcs true"}}]
                                 {:columns 2})
                   {:width 900 :height 420})

;; `:aspect :iso` - the same pixels per data unit on both axes, so an ellipse twice as wide as high looks it (the
;; points joined in data order, [line](reference.charts_points_lines.html#line-charts) `:order :data`):

(def ellipse (for [t (m/slice-range 0 m/TWO_PI 64)] {:x (m/cos t) :y (m/* 0.5 (m/sin t))}))

(nu/data-table ellipse)

(plot/render-image {:data ellipse
                    :mapping {:x :x :y :y}
                    :charts [(chart/line {:order :data})]
                    :aspect :iso
                    :labels {:title ":aspect :iso"}}
                   {:width 500 :height 400})
