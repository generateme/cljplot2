;; # Fitted curves

;; A curve fitted to scattered points shows their trend; `smooth` fits one per group and draws its uncertainty
;; band.

(ns reference.charts-fitted-curves
  (:require [fastmath.core :as m]
            [fastmath.random :as r]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [notebook-utils :as nu]))

(nu/source-note {:dataset :faithful})

(def faithful (nu/dataset :faithful))

(nu/data-table faithful)

;; ## Smoothing

;; `chart/smooth` fits a curve to each group's points, by default with a confidence band. Use it to show the trend
;; of scattered data.

(nu/mapping-list (describe/mapping (chart/smooth)))

(plot/render-image {:data faithful
                    :mapping {:x :eruptions :y :waiting}
                    :charts [(chart/scatter) (chart/smooth)]
                    :labels {:x "eruptions [min]" :y "waiting [min]"}}
                   {:width 600 :height 360})

;; ### Fit

;; These options choose how the curve is fitted. Change them when the default loess curve is too wiggly or too stiff,
;; when you want a straight line or a polynomial, or when groups should share one x range.

(nu/option-cards 'chart/smooth (describe/own-rows (describe/options (chart/smooth))) :fit)

(plot/render-image (plot/arrange [{:data faithful :mapping {:x :eruptions :y :waiting}
                                   :charts [(chart/scatter) (chart/smooth {:method :lm})]
                                   :labels {:title ":method :lm"}}
                                  {:data faithful :mapping {:x :eruptions :y :waiting}
                                   :charts [(chart/scatter) (chart/smooth {:method :lm :method-params {:degree 3}})]
                                   :labels {:title ":lm, :method-params {:degree 3}"}}
                                  {:data faithful :mapping {:x :eruptions :y :waiting}
                                   :charts [(chart/scatter) (chart/smooth {:method-params {:bandwidth 0.2}})]
                                   :labels {:title ":loess, :bandwidth 0.2"}}]
                                 {:columns 3})
                   {:width 1000 :height 320})

;; With `:n 6` the curve is a polyline through six points. Short and long eruptions each fit over their own x range
;; unless `:full-range` is `true` (the right panel also drops the x padding):

(plot/render-image (plot/arrange [{:data faithful :mapping {:x :eruptions :y :waiting}
                                   :charts [(chart/scatter) (chart/smooth {:n 6})]
                                   :labels {:title ":n 6"}}
                                  {:data faithful
                                   :mapping {:x :eruptions :y :waiting :color #(if (> (:eruptions %) 3) "long" "short")}
                                   :charts [(chart/scatter) (chart/smooth {:method :lm})]
                                   :labels {:title ":full-range false (default)"}
                                   :legend false}
                                  {:data faithful
                                   :mapping {:x :eruptions :y :waiting :color #(if (> (:eruptions %) 3) "long" "short")}
                                   :charts [(chart/scatter) (chart/smooth {:method :lm :full-range true :padding {:x [0 0]}})]
                                   :labels {:title ":full-range true, :padding x 0"}
                                   :legend false}]
                                 {:columns 3})
                   {:width 1000 :height 320})

;; These seeded random values grow with log x; x spreads evenly over three decades:

(def log-growth (mapv (fn [^double u ^double e] {:x (m/pow 10.0 u) :y (+ (* 3.0 u) e)})
                      (m/slice-range 0.0 3.0 60)
                      (r/->seq (r/distribution :normal {:sd 0.5 :rng (r/rng :mersenne 11)}) 60)))

(nu/data-table log-growth)

;; On a log axis the default `:stat-space :scale` gives a straight line; `:stat-space :data` fits data units, so the
;; axis bends it:

(plot/render-image (plot/arrange [{:data log-growth :mapping {:x :x :y :y} :scales {:x {:kind :log}}
                                   :charts [(chart/scatter) (chart/smooth {:method :lm :stat-space :scale})]
                                   :labels {:title ":stat-space :scale (default)"}}
                                  {:data log-growth :mapping {:x :x :y :y} :scales {:x {:kind :log}}
                                   :charts [(chart/scatter) (chart/smooth {:method :lm :stat-space :data})]
                                   :labels {:title ":stat-space :data"}}]
                                 {:columns 2})
                   {:width 900 :height 320})

;; ### Band

;; These options shape the uncertainty band around the curve. Use them to show where the fitted curve may lie or
;; where new points may fall, to widen or nest the band, or to leave it out.

(nu/option-cards 'chart/smooth (describe/own-rows (describe/options (chart/smooth))) :band)

(plot/render-image (plot/arrange [{:data faithful :mapping {:x :eruptions :y :waiting}
                                   :charts [(chart/scatter) (chart/smooth {:method :lm :band :prediction})]
                                   :labels {:title ":band :prediction"}}
                                  {:data faithful :mapping {:x :eruptions :y :waiting}
                                   :charts [(chart/scatter) (chart/smooth {:level [0.5 0.95 0.999] :alpha 0.2})]
                                   :labels {:title ":level [0.5 0.95 0.999]"}}
                                  {:data faithful :mapping {:x :eruptions :y :waiting}
                                   :charts [(chart/scatter)
                                            (chart/smooth {:band :bootstrap :fill [200 80 40 90]
                                                           :bootstrap {:samples 100 :rng (r/rng :mersenne 3)}})]
                                   :labels {:title ":band :bootstrap"}}]
                                 {:columns 3})
                   {:width 1000 :height 320})

;; A higher `:level` widens the confidence band, here in its own fill; `:band :none` leaves it out:

(plot/render-image (plot/arrange [{:data faithful :mapping {:x :eruptions :y :waiting}
                                   :charts [(chart/scatter) (chart/smooth {:band :confidence :level 0.999 :fill [200 80 40 90]})]
                                   :labels {:title ":band :confidence, :level 0.999"}}
                                  {:data faithful :mapping {:x :eruptions :y :waiting}
                                   :charts [(chart/scatter) (chart/smooth {:band :none})]
                                   :labels {:title ":band :none"}}]
                                 {:columns 2})
                   {:width 800 :height 300})

(nu/shared-groups (describe/options (chart/smooth)))
