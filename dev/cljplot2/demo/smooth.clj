(ns cljplot2.demo.smooth
  "Smooth and ribbon demo (Evolution cycle 3b): ribbon with a gradient, grouped loess, lm with nested levels, a
  Gaussian process, a bootstrap band of a smoothing spline."
  (:require [fastmath.kernel :as kernel]
            [fastmath.random :as r]
            [cljplot2.chart :as chart]
            [cljplot2.demo.data :as data]))

(def faithful (data/json-rows "faithful.json"))
(def quakes (data/json-rows "quakes.json"))

(def noisy-sine
  (let [rng (r/rng :mersenne 42)]
    (vec (for [x (range 0 40)] {:x (* 0.25 x) :y (+ (Math/sin (* 0.25 x)) (* 0.25 (r/grandom rng)))}))))

(def ribbon-gradient
  {:data (mapv (fn [x] {:x x :lo (- (Math/sin x) 0.3 (* 0.05 x)) :hi (+ (Math/sin x) 0.3 (* 0.05 x))}) (range 0 10.1 0.5))
   :mapping {:x :x :ymin :lo :ymax :hi}
   :charts [(chart/ribbon {:fill-gradient [:yellow :purple] :fill-gradient-axis :y :interpolator :cubic
                           :stroke-color [60 60 60]})]
   :labels {:title "Ribbon: cubic bounds, gradient fill along y, edge" :y "y"}})

(def loess-groups
  {:data quakes
   :mapping {:x :depth :y :mag :color #(if (> (:stations %) 50) "> 50 stations" "<= 50 stations")}
   :charts [(chart/scatter {:size 3}) (chart/smooth {:level [0.5 0.95]})]
   :labels {:title "Loess per group, nested 50% / 95% confidence bands"}})

(def lm-nested
  {:data faithful
   :mapping {:x :eruptions :y :waiting}
   :charts [(chart/smooth {:method :lm :band :prediction :level 0.95 :fill [215 48 39 40] :color [215 48 39]})
            (chart/smooth {:method :lm :method-params {:degree 2} :level [0.8 0.99]})
            (chart/scatter {:size 3})]
   :labels {:title "lm: 95% prediction (red); quadratic, 80% / 99% confidence"}})

(def gp-band
  {:data noisy-sine
   :mapping {:x :x :y :y}
   :charts [(chart/smooth {:method :gp :method-params {:kernel (kernel/kernel :gaussian {:sigma 1.5}) :noise 0.05
                                                       :normalize? true}
                           :level [0.5 0.95] :full-range true})
            (chart/scatter)]
   :scales {:x {:domain [-2 12]}}
   :labels {:title "Gaussian process: mean ± z·σ (50% / 95%), full range"}})

(def bootstrap-spline
  {:data noisy-sine
   :mapping {:x :x :y :y}
   :charts [(chart/smooth {:method :cubic-smoothing :method-params {:rho 0.5} :band :bootstrap
                           :bootstrap {:samples 200 :rng (r/rng :mersenne 1)}})
            (chart/scatter)]
   :labels {:title "Smoothing spline with a percentile bootstrap band"}})

(def images
  "Images of this demo: `[file-name plot options]`, saved to `results/smooth/` by [[cljplot2.demo/render!]]."
  [["ribbon-gradient-600x400.png" ribbon-gradient {:width 600 :height 400}]
   ["loess-groups-700x450.png" loess-groups {:width 700 :height 450}]
   ["lm-nested-600x450.png" lm-nested {:width 600 :height 450}]
   ["gp-band-600x400.png" gp-band {:width 600 :height 400}]
   ["bootstrap-spline-600x400.png" bootstrap-spline {:width 600 :height 400}]])
