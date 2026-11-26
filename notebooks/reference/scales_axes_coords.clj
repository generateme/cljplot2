;; # Scales, axes and coordinates

;; Every mapped dimension goes through a scale: the position scales `:x` and `:y` place marks on the panel, the
;; aesthetic scales style them ([Aesthetics and legends](reference.aesthetics_legends.html)). This chapter covers the
;; position scales, set under the plot's `:scales` key (`{:scales {:x {...} :y {...}}}`), and the coordinates, set by
;; the plot keys `:coord`, `:flip` and `:aspect`. A position scale's kind follows the data (numbers, levels, dates)
;; unless set, and each one draws an axis and grid lines.

(ns reference.scales-axes-coords
  (:require [fastmath.core :as m]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [cljplot2.pipeline :as pipeline]
            [cljplot2.scale.coord :as coord]
            [cljplot2.scale.position :as position]
            [cljplot2.schema :as schema]
            [notebook-utils :as nu])
  (:import [java.time LocalDate]))

;; The table below holds the Gapminder countries of 2007; the partial domain and the ticks plots draw them:

(nu/source-note {:dataset :gapminder-2007})

(def gapminder (nu/dataset :gapminder-2007))

(nu/data-table gapminder)

;; The table below counts the countries per continent; the bar plots draw it:

(def counts (for [[continent rs] (group-by :continent gapminder)] {:continent continent :n (count rs)}))

(nu/data-table counts)

;; The table below holds the mtcars cars; the domain and axis sections and two scale kinds draw them:

(nu/source-note {:dataset :mtcars})

(def mtcars (nu/dataset :mtcars))

(nu/data-table mtcars)

;; The tables below hold a made-up spiral and points on an ellipse; the coordinates section draws them:

(def spiral (for [t (range 0 13)] {:t t :r (+ 1 t)}))

(nu/data-table spiral)

(def ellipse (for [t (m/slice-range 0 m/TWO_PI 64)] {:x (m/cos t) :y (m/* 0.5 (m/sin t))}))

(nu/data-table ellipse)

;; ## Kind and domain

;; These options set the scale kind (Scale kinds below) and the data range it covers. The scale extends the domain to
;; nice tick values (`:nice`), then pads it (the charts' padding, or `:expand`). The plots below read cylinder counts
;; as three bands (left) and fix both domains, x without padding (right):

(nu/option-cards [:scales :x] (describe/options position/scale-spec-schema) :kind-domain)

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

;; A partial domain such as `[0 nil]` fixes one end; the charts' padding and nice rounding still apply at the other.
;; Only continuous kinds other than `:interpolated` take one, and the data must reach past the fixed end. The left
;; plot covers the data's range; the right one starts life expectancy at zero:

(plot/render-image (plot/arrange (for [[title y-scale] [["the data's range" {}] [":domain [0 nil]" {:domain [0 nil]}]]]
                                   {:data gapminder
                                    :mapping {:x :gdp-per-cap :y :life-exp}
                                    :charts [(chart/scatter)]
                                    :scales {:x {:kind :log} :y y-scale}
                                    :labels {:title title}})
                                 {:columns 2})
                   {:width 800 :height 320})

;; The built plot's `:scale-specs` hold the kind each scale gets from the data and the charts' statistics. Below, a
;; bar chart puts levels on x and a histogram puts bins and counts on its axes:

(into {} (for [[title p] {"bar" {:data mtcars :mapping {:x #(str (:cyl %) " cyl") :y :mpg} :charts [(chart/bar)]}
                          "histogram" {:data mtcars :mapping {:x :mpg} :charts [(chart/histogram)]}}]
           [title (update-vals (select-keys (:scale-specs (plot/build p)) [:x :y]) :kind)]))

;; ## Ticks and labels

;; These options set where ticks go and how their labels read. Minor ticks get grid lines only when the theme draws
;; them (`[:panel :grid :minor]`, `:none` by default). The plot below writes powers of ten on a log x axis with minor
;; ticks and their grid lines, and gives y its tick values and a formatter:

(nu/option-cards [:scales :x] (describe/options position/scale-spec-schema) :ticks)

(plot/render-image {:data gapminder
                    :mapping {:x :gdp-per-cap :y :life-exp}
                    :charts [(chart/scatter)]
                    :scales {:x {:kind :log :formatter :power :minor-ticks 9}
                             :y {:ticks [40 50 60 70 80] :formatter #(str % " y")}}
                    :theme {:panel {:grid {:minor {:color [200 200 215] :line-width 0.5 :dash :none}}}}}
                   {:width 600 :height 350})

;; ## Order and direction

;; These options order the bands and set the axis direction. The plots below order the continents by their count,
;; largest first (left), and reverse the y axis (right):

(nu/option-cards [:scales :x] (describe/options position/scale-spec-schema) :order)

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

;; These options set what the scale's axis draws. Builders naming data items on an axis, such as dendrogram leaves,
;; set `:overlapping-labels :keep`. The plots below draw the x axis without grid lines and y grid lines without an
;; axis (left), and y ticks of a display domain (right):

(nu/option-cards [:scales :x] (describe/options position/scale-spec-schema) :axis)

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

;; cljplot2 passes these options to the [wadogo](https://github.com/scicloj/wadogo) scale of the kind; each kind below
;; lists the ones it reads. The plot below widens the space between and around the bands:

(nu/option-cards [:scales :x] (describe/options position/scale-spec-schema) :wadogo)

(plot/render-image {:data counts
                    :mapping {:x :continent :y :n}
                    :charts [(chart/bar {:width 1.0})]
                    :scales {:x {:padding-in 0.5 :padding-out 1.0}}}
                   {:width 500 :height 280})

;; ## Scale kinds

;; Each kind below is described by wadogo (`wadogo.scale/kind-info`). cljplot2 sets a kind's `:domain` (the data
;; extent or the spec's `:domain`), its `:range` (the panel's pixels), ticks and tick format itself - through the
;; options above; the remaining attributes of a kind are the options under wadogo attributes.

;; ### Linear

(nu/scale-kind-doc :linear)

(plot/render-image {:data mtcars
                    :mapping {:x :hp :y :mpg}
                    :charts [(chart/scatter)]}
                   {:width 500 :height 250})

;; ### Log

;; The scale takes positive values only; `:base` sets the tick values (here powers of two).

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

;; For data of both signs over many magnitudes. cljplot2 doesn't pass the constant `:C`, so wadogo uses its default
;; 1.0.

(nu/scale-kind-doc :symlog)

(plot/render-image {:data (for [i (range -40 41)] {:x (* i i i) :y i})
                    :mapping {:x :x :y :y}
                    :charts [(chart/scatter)]
                    :scales {:x {:kind :symlog}}}
                   {:width 500 :height 250})

;; ### Bands

;; Bands is the default kind for strings and keywords.

(nu/scale-kind-doc :bands)

(plot/render-image {:data counts
                    :mapping {:x :continent :y :n}
                    :charts [(chart/bar)]
                    :scales {:x {:align-outer 0.0 :padding-out 2.0}}}
                   {:width 500 :height 250})

;; ### Datetime

;; The plot below formats dates with a pattern:

(nu/scale-kind-doc :datetime)

(plot/render-image {:data (for [m (range 24)]
                            {:date (.plusMonths (LocalDate/of 2020 1 1) m) :v (+ 10 (mod (* m 7) 9))})
                    :mapping {:x :date :y :v}
                    :charts [(chart/line) (chart/scatter)]
                    :scales {:x {:formatter "MMM yyyy"}}}
                   {:width 600 :height 250})

;; ### Interpolated

;; The scale maps the domain values piecewise onto evenly spaced or given `:stops` of the panel through an
;; `:interpolator`; these options set both:

(nu/option-cards [:scales :x] (describe/options position/scale-spec-schema) :interpolated)

(nu/scale-kind-doc :interpolated)

;; The `:interpolator` takes a fastmath 1D interpolation method or a function:

(sort schema/interpolation-methods)

;; Here the domain values 1, 40, 60 and 100 sit at evenly spaced stops, so each of the three segments gets a third
;; of the axis - 40 to 60 is stretched, 1 to 40 and 60 to 100 are squeezed:

(plot/render-image {:data (for [i (range 1 101)] {:x i :y (m/log i)})
                    :mapping {:x :x :y :y}
                    :charts [(chart/line)]
                    :scales {:x {:kind :interpolated :domain [1 40 60 100] :ticks [1 20 40 50 60 80 100]}}}
                   {:width 600 :height 250})

;; ### Radial

;; The plot below draws polar bars whose areas read as values:

(nu/scale-kind-doc :radial)

(plot/render-image {:data counts
                    :mapping {:x :continent :y :n :color :continent}
                    :charts [(chart/bar {:width 1.0})]
                    :scales {:y {:kind :radial}}
                    :coord {:kind :polar}
                    :legend false}
                   {:width 400 :height 400})

;; ## Coordinates

;; These plot keys set the coordinate system. They sit at the top of the plot map, beside `:scales`:

(nu/option-cards [] (describe/rows-at (describe/options pipeline/plot-schema) #{[:flip] [:coord]}))

;; The plots below flip a bar chart (left) and lay bars over 270 degrees of a ring with a hole (right):

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

;; A polar `:coord` takes these keys beside its `:kind :polar`:

(nu/option-cards [:coord] (describe/options coord/polar-schema))

;; Polar coordinates work with these charts:

(sort (for [[kind constructor] (describe/chart-constructors)
            :when (contains? (:coords (constructor)) :polar)]
        kind))

;; In polar coordinates `:flip true` swaps the roles - the `:y` scale on angles, the `:x` scale on radii (left);
;; `:arcs true` makes data lines between points follow the circle instead of chords (right):

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

;; ## Aspect

;; This plot key fixes the ratio of the panel's axes in data units:

(nu/option-cards [] (describe/rows-at (describe/options pipeline/plot-schema) #{[:aspect]}))

;; In the plot below `:aspect :iso` keeps an ellipse twice as wide as high (the line joins the points in data order,
;; [line](reference.charts_points_lines.html#line-charts) `:order :data`):

(plot/render-image {:data ellipse
                    :mapping {:x :x :y :y}
                    :charts [(chart/line {:order :data})]
                    :aspect :iso
                    :labels {:title ":aspect :iso"}}
                   {:width 500 :height 400})
