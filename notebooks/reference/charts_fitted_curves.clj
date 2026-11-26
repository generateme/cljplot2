;; # Fitted and function curves

;; These charts draw curves: a trend fitted to scattered points (`smooth`, one per group with its uncertainty band)
;; and curves of given functions, y = f(x) or parametric (`function`).

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

(nu/shared-groups (describe/options (chart/smooth)))

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

;; ## Function curves

;; `chart/function` draws curves of functions, y = f(x) or parametric [x y] = f(t), sampled over `:domain` (by
;; default the panel's x range) or, for parametric curves, over `:t`. It takes no data rows and, like an annotation,
;; draws in every panel. Its own mapping can colour or dash several functions by their labels. Use it for a known
;; curve, a model or a reference shape.

(nu/shared-groups (describe/options (chart/function)))

(nu/mapping-list (describe/mapping (chart/function)))

(plot/render-image {:charts [(chart/function {:f {"sin" #(m/sin %) "cos" #(m/cos %)}
                                              :domain [0 m/TWO_PI]
                                              :mapping {:color :function :dash :function}})]}
                   {:width 600 :height 320})

;; ### Curve

;; These options give the functions, the range they are sampled over and how densely, the fill and the chart's own
;; mapping:

(nu/option-cards 'chart/function (describe/own-rows (describe/options (chart/function))) :curve)

(plot/render-image (plot/arrange [{:charts [(chart/function {:f #(* % % 0.5) :domain [-2 2] :fill [0 114 178 60]})]
                                   :labels {:title ":f, :fill"}}
                                  {:charts [(chart/function {:parametric (fn [t] [(m/sin (* 3 t)) (m/sin (* 4 t))])
                                                             :t [0 m/TWO_PI] :fill [200 80 40 60]})]
                                   :aspect :iso
                                   :labels {:title ":parametric (Lissajous)"}}
                                  {:charts [(chart/function {:f #(m/sin %) :domain [0 10] :sample-step 25})]
                                   :labels {:title ":sample-step 25 px"}}]
                                 {:columns 3})
                   {:width 1000 :height 320})

;; ### Gaps

;; These options handle a `nil` or non-finite result of a function:

(nu/option-cards 'chart/function (describe/own-rows (describe/options (chart/function))) :gaps)

(plot/render-image (plot/arrange [{:charts [(chart/function {:f #(m/sqrt (- 1 (* % %))) :domain [-2 2]})]
                                   :labels {:title ":gaps :break (default)"}}
                                  {:charts [(chart/function {:f #(when-not (< 1 % 2) (m/sin %)) :domain [0 6]
                                                             :gaps :bridge :bridge {:alpha-factor 0.6 :dash :dotted}})]
                                   :labels {:title ":gaps :bridge"}}]
                                 {:columns 2})
                   {:width 800 :height 300})

;; ### Extent

;; These options pick the samples that set the y axis and the room around the curve. Here `tan` has poles: the default
;; quantiles keep the axis readable, all samples stretch it, `:y-limits` sets it:

(nu/option-cards 'chart/function (describe/own-rows (describe/options (chart/function))) :extent)

(plot/render-image (plot/arrange [{:charts [(chart/function {:f #(m/tan %) :domain [0 3]})]
                                   :labels {:title ":y-extent :quantiles (default)"}}
                                  {:charts [(chart/function {:f #(m/tan %) :domain [0 3] :y-extent :min-max})]
                                   :labels {:title ":y-extent :min-max"}}
                                  {:charts [(chart/function {:f #(m/tan %) :domain [0 3] :y-limits [-5 5]})]
                                   :labels {:title ":y-limits [-5 5]"}}]
                                 {:columns 3})
                   {:width 1000 :height 300})


;; With no padding on x the curve meets the panel's sides; a quarter of the span on y leaves room above and below:

(plot/render-image {:charts [(chart/function {:f #(m/sin %) :domain [0 6.3] :padding {:x [0 0] :y [0.25 0.25]}})]
                    :labels {:title ":padding {:x [0 0] :y [0.25 0.25]}"}}
                   {:width 500 :height 280})

