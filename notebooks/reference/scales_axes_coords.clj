;; # Scales, axes and coordinates

;; Position scales map `:x` and `:y` to the panel; their kind follows the data (numbers, levels, dates) unless set.
;; Each position scale draws an axis and grid lines. A plot is cartesian unless `:coord` is polar; `:flip` swaps the
;; directions and `:aspect :iso` keeps data units square.

(ns reference.scales-axes-coords
  (:require [scicloj.kindly.v4.kind :as kind]
            [wadogo.scale :as ws]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.facet :as facet]
            [cljplot2.describe :as describe]
            [cljplot2.scale.coord :as coord]
            [cljplot2.scale.position :as position]
            [cljplot2.schema :as schema]
            [notebook-utils :as nu]))

;; ## Position scale

;; Keys marked "wadogo:" pass through to the [wadogo](https://github.com/scicloj/wadogo) scale of the kind (see the
;; kinds below).

(nu/options-table (describe/options position/scale-spec-schema))

(nu/source-note {:dataset :gapminder-2007})

(def gapminder (nu/dataset :gapminder-2007))

;; A log scale with powers of ten as tick labels and minor ticks; a reversed linear scale with a fixed domain:

(plot/render-image {:data gapminder
                    :mapping {:x :gdp-per-cap :y :life-exp}
                    :charts [(chart/scatter)]
                    :scales {:x {:kind :log :formatter :power :minor-ticks 9}
                             :y {:domain [30 90] :reverse true :ticks 7}}}
                   {:width 600 :height 400})

;; ## Scale kinds

;; The position scale kinds and their wadogo attributes (from `wadogo.scale/kind-info`):

(kind/md (ws/kind-info :linear))

(kind/md (ws/kind-info :log))

(kind/md (ws/kind-info :pow))

(kind/md (ws/kind-info :symlog))

(kind/md (ws/kind-info :bands))

(kind/md (ws/kind-info :datetime))

(kind/md (ws/kind-info :interpolated))

(kind/md (ws/kind-info :radial))

;; ## Interpolated scales

;; An `:interpolated` scale maps the domain piecewise onto evenly spaced (or given) `:stops`; `:interpolator` is one
;; of the fastmath 1D interpolation methods (or a function):

(sort schema/interpolation-methods)

;; Here the middle of the domain gets half of the axis:

(plot/render-image {:data (for [i (range 1 101)] {:x i :y (Math/log i)})
                    :mapping {:x :x :y :y}
                    :charts [(chart/line)]
                    :scales {:x {:kind :interpolated :domain [1 40 60 100] :ticks [1 20 40 50 60 80 100]}}}
                   {:width 600 :height 300})

;; ## Coordinates, flip and aspect

;; `:flip true` draws `:x` vertically; scales, labels and theme keys keep their names. Polar coordinates map the
;; horizontal scale to angles and the vertical one to radii - only some charts draw in them:

(sort (for [v (vals (ns-publics 'cljplot2.chart))
            :let [c (try (v) (catch Exception _ nil))]
            :when (contains? (:coords c) :polar)]
        (:kind c)))

(nu/options-table (describe/options coord/polar-schema))

(def counts (for [[continent rs] (group-by :continent gapminder)] {:continent continent :n (count rs)}))

(plot/render-image (plot/arrange [{:data counts
                                   :mapping {:x :continent :y :n}
                                   :charts [(chart/bar)]
                                   :flip true
                                   :scales {:x {:reverse true}}}
                                  {:data counts
                                   :mapping {:x :continent :y :n :color :continent}
                                   :charts [(chart/bar {:width 1.0})]
                                   :coord {:kind :polar :inner 0.2}
                                   :legend false}]
                                 {:columns 2})
                   {:width 900 :height 400})

;; `:aspect :iso` gives both linear axes the same pixels per data unit (the panel shrinks to fit).

;; ## Facet scales

;; The facet levels have scales too: `:scales {:facet spec}` (and `:facet-col` / `:facet-row`) order the panels and
;; format the strip text (Facets chapter).

(nu/options-table (describe/options facet/scale-spec-schema))
