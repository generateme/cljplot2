(ns cljplot2.demo.evolution
  "Evolution cycle 4 demo: greyed context in facet panels, end labels, lollipops, candlesticks / OHLC, a bump chart."
  (:require [fastmath.random :as r]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.levels :as levels])
  (:import [java.time LocalDate]))

(def ^:private start (LocalDate/of 2024 1 1))

(def walks
  "Random walks of six series over 60 days."
  (let [rng (r/rng :mersenne 11)]
    (vec (for [s ["alpha" "beta" "gamma" "delta" "epsilon" "zeta"]
               [i v] (map-indexed vector (reductions + (repeatedly 60 #(r/grandom rng))))]
           {:day (.plusDays start i) :value v :series s}))))

(def greyed-panels
  {:data walks
   :mapping {:x :day :y :value :color :series :facet :series}
   :charts [(chart/line {:mapping {:facet nil :color nil :group :series} :color [200 200 200]})
            (chart/line {:line-width 2})]
   :labels {:title "Every series in grey behind each panel's own ({:facet nil})"}})

(def end-labels
  {:data walks
   :mapping {:x :day :y :value :color :series}
   :charts [(chart/line)
            (chart/rule {:x (.plusDays start 30) :dash :dashed :color [120 120 120]})
            (chart/text {:mapping {:x :day :y :value :label :series :color :series} :select :last})]
   :labels {:title "Labels at the line ends (text :select :last), a rule at a date"}})

(def lollipop
  (let [rng (r/rng :mersenne 5)
        data (vec (for [c ["Amber" "Basalt" "Coral" "Dune" "Ember" "Fjord" "Garnet"]] {:c c :v (+ 2 (* 8 (r/drandom rng)))}))]
    {:data data
     :mapping {:x :c :y :v}
     :flip true
     :charts [(chart/segment {:yend 0 :color [120 120 120]}) (chart/scatter {:size 8 :color [215 48 39]})]
     :labels {:title "Lollipop: segment {:yend 0} + scatter, flipped" :x "" :y "value"}}))

(def prices
  (let [rng (r/rng :mersenne 3)]
    (vec (map-indexed (fn [i close]
                        (let [open (+ close (* 1.5 (r/grandom rng)))
                              spread (Math/abs (* 1.2 (double (r/grandom rng))))]
                          {:day (.plusDays start i) :open open :close close
                           :high (+ (max open close) spread) :low (- (min open close) spread)}))
                      (take 30 (reductions + 100.0 (repeatedly #(r/grandom rng))))))))

(def candles
  (plot/arrange [{:data prices :mapping {:x :day :open :open :high :high :low :low :close :close}
                  :charts [(chart/candlestick)] :labels {:title "Candlestick"}}
                 {:data prices :mapping {:x :day :open :open :high :high :low :low :close :close}
                  :charts [(chart/candlestick {:style :ohlc})] :labels {:title "OHLC"}}]
                {:columns 2}))

(def bump
  (let [rng (r/rng :mersenne 8)
        teams ["Owls" "Foxes" "Bears" "Hawks" "Wolves"]
        scores (vec (for [round (range 1 9) t teams] {:round round :team t :score (r/drandom rng)}))
        ranked (levels/rank-rows scores :round :score)]
    {:data ranked
     :mapping {:x :round :y :rank :color :team}
     :scales {:y {:domain [5.5 0.5] :nice false :ticks [1 2 3 4 5]}}
     :charts [(chart/line {:interpolator :monotone :line-width 3})
              (chart/scatter {:size 9})
              (chart/text {:mapping {:x :round :y :rank :label :team :color :team} :select :last})]
     :labels {:title "Bump chart: levels/rank-rows, monotone lines, end labels" :y "rank"}}))

(def images
  "Images of this demo: `[file-name plot options]`, saved to `results/evolution/` by [[cljplot2.demo/render!]]."
  [["greyed-panels-900x600.png" greyed-panels {:width 900 :height 600}]
   ["end-labels-700x450.png" end-labels {:width 700 :height 450}]
   ["lollipop-600x400.png" lollipop {:width 600 :height 400}]
   ["candles-1000x400.png" candles {:width 1000 :height 400}]
   ["bump-700x450.png" bump {:width 700 :height 450}]])
