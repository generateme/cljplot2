(ns cljplot2.demo.patterns
  "Ranking cycle 2 demo: fill patterns - the named set, black-and-white textured bars, patterns on histograms,
  densities, boxes, violins and candlesticks."
  (:require [fastmath.random :as r]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.schema :as schema])
  (:import [java.time LocalDate]))

(def pattern-names (vec (remove #{:solid} (sort (keys schema/pattern-styles)))))

(def swatches
  {:data (map-indexed (fn [i p] {:x0 (- i 0.4) :x1 (+ i 0.4) :y0 0 :y1 1 :pattern p :label (name p) :x i :y -0.15})
                      pattern-names)
   :scales {:pattern {:kind :identity} :x {:axis false} :y {:axis false}}
   :theme {:panel {:grid {:x :none :y :none}}}
   :charts [(chart/rect {:mapping {:x0 :x0 :x1 :x1 :y0 :y0 :y1 :y1 :pattern :pattern}
                         :fill [235 235 235] :pattern-color :black :stroke-color :black})
            (chart/text {:mapping {:x :x :y :y :label :label}})]
   :labels {:title "Named patterns (identity :pattern scale on rects)"}})

(def textured
  (let [data [{:name "north" :value 3} {:name "south" :value 12} {:name "east" :value 5}
              {:name "west" :value 18} {:name "centre" :value 7}]]
    {:data data
     :mapping {:x :value :y :name :pattern :name}
     :flip true
     :charts [(chart/bar {:color :white :pattern-color :black :stroke-color :black :position :stack})]
     :labels {:title "Textured bars, black and white (RGG 368)"}}))

(def samples
  (let [rng (r/rng :mersenne 4)]
    (vec (for [[g mu] [["a" 0.0] ["b" 1.5] ["c" 3.0]] _ (range 200)]
           {:g g :v (r/grandom rng mu 1.0)}))))

(def areas
  (plot/arrange [{:data samples :mapping {:x :v :color :g :pattern :g}
                  :charts [(chart/histogram {:position :stack :bins 20})]
                  :labels {:title "Stacked histogram, :pattern per group"}}
                 {:data samples :mapping {:x :v :color :g :pattern :g}
                  :charts [(chart/density {:position :stack})]
                  :labels {:title "Stacked densities"}}]
                {:columns 2}))

(def strips
  (plot/arrange [{:data samples :mapping {:x :v :y :g :pattern :g}
                  :charts [(chart/box {:color [250 250 250] :pattern-color [60 60 60]})]
                  :labels {:title "Box plots, patterns in grey"}}
                 {:data samples :mapping {:x :v :y :g :color :g :pattern :g}
                  :charts [(chart/violin)]
                  :labels {:title "Violins, :pattern-color :darker"}}]
                {:columns 2}))

(def prices
  (let [rng (r/rng :mersenne 7)
        start (LocalDate/of 2024 3 1)]
    (vec (map-indexed (fn [i [o c]] {:day (.plusDays start i) :open o :close c
                                     :high (+ (max o c) (r/drandom rng 0.5 2.0)) :low (- (min o c) (r/drandom rng 0.5 2.0))})
                      (partition 2 1 (take 31 (reductions + 100.0 (repeatedly #(r/grandom rng 0.0 2.0)))))))))

(def candles
  {:data prices
   :mapping {:x :day :open :open :high :high :low :low :close :close}
   :charts [(chart/candlestick {:up-color :white :down-color :white :stroke-color :black :pattern-color :black
                                :down-pattern :diagonal-cross})]
   :labels {:title "Candlesticks for print: up plain, down cross-hatched"}})

(def images
  "Images of this demo: `[file-name plot options]`, saved to `results/patterns/` by [[cljplot2.demo/render!]]."
  [["swatches-800x220.png" swatches {:width 800 :height 220}]
   ["textured-600x400.png" textured {:width 600 :height 400}]
   ["areas-900x380.png" areas {:width 900 :height 380}]
   ["strips-900x380.png" strips {:width 900 :height 380}]
   ["candles-700x380.png" candles {:width 700 :height 380}]])
