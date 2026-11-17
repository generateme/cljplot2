(ns cljplot2.demo.extras
  "cljplot extras cycle 1 demo: Q-Q plots (data against a distribution; groups and facets; two distributions), a P-P
  plot, correlograms (ACF + PACF; ACF only), a lag plot, a field mapping, arrows over a scalar field, traces, complex
  domain colouring (field `:pixel-color`), a point cloud drawn by `chart/image`. Every random value is seeded."
  (:require [clojure2d.core :as c2d]
            [fastmath.complex :as cx]
            [fastmath.core :as m]
            [fastmath.fields :as f]
            [fastmath.random :as r]
            [fastmath.vector :as v]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.field :as field]
            [cljplot2.data.series :as series]
            [cljplot2.data.vector-field :as vf]
            [cljplot2.demo.parallel :refer [iris]]))

(def ^:private skewed
  "Seeded log-normal sample: a right tail a normal Q-Q plot shows."
  (let [g (r/rng :mersenne 31)]
    (mapv (fn [_] {:v (m/exp (r/grandom g 0 0.5))}) (range 200))))

(def qq-normal
  {:data skewed :mapping {:y :v}
   :charts [(chart/qq {:x-distribution [:normal]})]
   :labels {:title "Q-Q: a log-normal sample against the normal" :y "sample"}})

(def ^:private iris-long
  (vec (for [row iris k [:sepal_length :petal_length]]
         {:species (:species row) :variable (name k) :value (row k)})))

(def qq-groups
  {:data iris-long :mapping {:y :value :color :species :facet :variable}
   :charts [(chart/qq {:x-distribution [:normal]})]
   :labels {:title "Q-Q per species and variable (a line per group)" :y "value"}})

(def qq-distributions
  {:charts [(chart/qq {:x-distribution [:normal] :y-distribution [:t {:degrees-of-freedom 3}] :points 60
                       :label "t(3)" :mapping {:color :label}})
            (chart/qq {:x-distribution [:normal] :y-distribution [:t {:degrees-of-freedom 30}] :points 60
                       :label "t(30)" :mapping {:color :label} :line false})]
   :scales {:color {:title nil}}
   :labels {:title "Q-Q of t distributions against the normal"}})

(def ^:private t-sample
  (let [d (r/distribution :t {:degrees-of-freedom 3 :rng (r/rng :mersenne 5)})]
    (mapv (fn [_] {:v (r/sample d)}) (range 300))))

(def pp-t
  {:data t-sample :mapping {:y :v}
   :charts [(chart/pp {:x-distribution [:normal]})]
   :labels {:title "P-P: a t(3) sample against the normal" :y "sample probabilities"}})

(def ^:private ma2
  "MA(2) series: e_t + 0.8 e_(t-1) + 0.5 e_(t-2)."
  (let [g (r/rng :mersenne 3)
        e (vec (repeatedly 502 #(r/grandom g)))]
    (mapv (fn [i] (+ (e (+ i 2)) (* 0.8 (e (+ i 1))) (* 0.5 (e i)))) (range 500))))

(def acf-both (plot/acf ma2 {:title "MA(2): ACF cuts off after lag 2, PACF decays"}))

(def ^:private seasonal
  "AR(1) series with a weekly bump and occasional jumps (old cljplot's lag example)."
  (let [g (r/rng :mersenne 8)]
    (vec (reductions (fn [y t] (+ (* 0.6 (double y)) (* 2.0 (m/sin (* m/TWO_PI (/ (double t) 7.0))))
                                  (r/grandom g) (if (< (r/drandom g) 0.03) 6.0 0.0)))
                     0.0 (range 1 400)))))

(def acf-only (plot/acf seasonal {:partial? :acf :lags 30 :title "ACF of a weekly series"}))

(def acf-stepped (plot/acf seasonal {:partial? :pacf :band :stepped :lags 30 :title "PACF, a stepped band"}))

(def lag-7
  {:data (series/lag-rows seasonal 7) :mapping {:x :x :y :y}
   :charts [(chart/scatter {:size 5})]
   :aspect :iso
   :labels {:title "Lag plot: y(t) against y(t+7)" :x "y(t)" :y "y(t+7)"}})

(def ^:private combined-field
  (f/combine {:type :operation :name :comp
              :var1 {:type :operation :name :comp
                     :var1 {:type :variation :name :secant :amount 1.0 :config {}}
                     :var2 {:type :variation :name :diamond :amount 1.0 :config {}} :amount 1.0}
              :var2 {:type :variation :name :power :amount 1.0 :config {}} :amount 1.0}))

(def field-mapping
  (let [g (r/rng :mersenne 9)]
    {:data (vf/mapped-points combined-field (repeatedly 150000 #(vector (r/drandom g -4 4) (r/drandom g -4 4))))
     :mapping {:x :x :y :y}
     :charts [(chart/scatter {:size 1 :color [20 40 90 25]})]
     :labels {:title "Field mapping: points through secant . diamond . power"}}))

(def vector-field
  (let [hyperbolic (f/field :hyperbolic)
        heading (f/heading hyperbolic)]
    {:data (vf/arrows hyperbolic {:domain {:x [-3 3] :y [-3 3]} :n 24 :grid :shifted :scale 1.5})
     :mapping {:x :x :y :y :xend :xend :yend :yend}
     :charts [(chart/field {:f (fn [x y] (m/sin (heading (v/vec2 x y)))) :domain {:x [-3 3] :y [-3 3]} :n 300
})
              (chart/segment {:color [90 160 235] :width 1.5 :arrow {:end true :size 5}})]
     :scales {:color {:range [:black :white] :title "sin heading"}
              :x {:domain [-3 3] :expand [0 0]} :y {:domain [-3 3] :expand [0 0]}}
     :aspect :iso
     :labels {:title "Hyperbolic field: arrows over sin of its heading"}}))

(def traces
  (let [g (r/rng :mersenne 12)
        seeds (repeatedly 3000 #(vector (r/drandom g -7 7) (r/drandom g -7 7)))]
    {:data (vf/traces (f/field :exp) seeds {:steps 150 :step 0.01})
     :mapping {:x :x :y :y}
     :charts [(chart/scatter {:size 1 :color [20 70 140 20]})]
     :scales {:x {:domain [-7 7] :nice false} :y {:domain [-7 7] :nice false}}
     :labels {:title "Traces of the exponential field"}}))

(def complex-function
  {:charts [(chart/field {:pixel-color (field/domain-color (fn [z] (cx/div (cx/log z) (cx/sin z))))
                          :domain {:x [-6 6] :y [-6 6]}})]
   :aspect :iso
   :labels {:title "Domain colouring of log(z) / sin(z)" :x "re" :y "im"}})

(defn- point-cloud
  "`chart/image` callback: 200k seeded Gaussian points drawn translucent through the panel's scales."
  [{:keys [x-scale y-scale size]}]
  (let [[w h] size
        g (r/rng :mersenne 21)
        canvas (c2d/canvas w h)]
    (c2d/with-canvas [c canvas]
      (c2d/set-color c 230 240 255 18)
      (dotimes [_ 200000]
        (let [x (r/grandom g) y (+ (* 0.6 x) (* 0.8 (r/grandom g)))]
          (c2d/rect c (x-scale x) (y-scale y) 1 1))))
    canvas))

(def image-cloud
  {:charts [(chart/image {:f point-cloud :domain {:x [-4 4] :y [-4 4]}})]
   :theme {:panel {:fill [10 15 40]}}
   :aspect :iso
   :labels {:title "Point cloud drawn by a chart/image callback"}})

(def images
  "Images of this demo: `[file-name plot options]`, saved to `results/extras/` by [[cljplot2.demo/render!]]."
  [["qq-normal-500x450.png" qq-normal {:width 500 :height 450}]
   ["qq-groups-900x450.png" qq-groups {:width 900 :height 450}]
   ["qq-distributions-500x450.png" qq-distributions {:width 500 :height 450}]
   ["pp-t-500x450.png" pp-t {:width 500 :height 450}]
   ["acf-both-700x500.png" acf-both {:width 700 :height 500}]
   ["acf-only-700x350.png" acf-only {:width 700 :height 350}]
   ["acf-stepped-700x350.png" acf-stepped {:width 700 :height 350}]
   ["lag-7-500x500.png" lag-7 {:width 500 :height 500}]
   ["field-mapping-600x600.png" field-mapping {:width 600 :height 600}]
   ["vector-field-600x600.png" vector-field {:width 600 :height 600}]
   ["traces-600x600.png" traces {:width 600 :height 600}]
   ["complex-600x600.png" complex-function {:width 600 :height 600}]
   ["image-cloud-600x600.png" image-cloud {:width 600 :height 600}]])
