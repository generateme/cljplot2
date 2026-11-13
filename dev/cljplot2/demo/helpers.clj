(ns cljplot2.demo.helpers
  "Helpers and options demo (cycle 5): `order-levels` (B4), `bin` (B9), `:stroke-color` (H3)."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [fastmath.stats :as stats]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.levels :as levels]))

(defn- read-json [file] (json/read-str (slurp (io/resource file)) :key-fn keyword))

(def barley (read-json "barley.json"))
(def faithful (read-json "faithful.json"))

(def ordered
  (plot/arrange [{:data barley :mapping {:x :yield :y :variety}
                  :scales {:y {:domain (levels/order-levels barley :variety :yield stats/median)}}
                  :charts [(chart/box)]
                  :labels {:title "Varieties by median yield" :x "yield"}}
                 {:data barley :mapping {:x :yield :y :site}
                  :scales {:y {:domain (levels/order-levels barley :site :yield stats/mean {:descending true})}}
                  :charts [(chart/violin {:inner :quartiles})]
                  :labels {:title "Sites by mean yield, descending" :x "yield"}}]
                {:columns 2}))

(def binned-faithful (levels/bin faithful :waiting {:as :waiting-bin :bins 6}))

(def binned
  (plot/arrange [{:data binned-faithful :mapping {:x :eruptions :y :waiting-bin}
                  :charts [(chart/box {:n-label true})]
                  :labels {:title "Eruptions by waiting bins" :x "eruptions" :y "waiting"}}
                 {:data binned-faithful :mapping {:x :eruptions :y :waiting-bin} :flip true
                  :charts [(chart/box {:varwidth true})]
                  :labels {:title "Flipped, :varwidth" :x "eruptions" :y "waiting"}}]
                {:columns 2}))

(defn- histogram [title options]
  {:data faithful :mapping {:x :eruptions}
   :charts [(chart/histogram (merge {:bins 20} options))]
   :labels {:title title :x "eruptions"}})

(def histogram-strokes
  (plot/arrange [(histogram "default (:darker)" {})
                 (histogram ":stroke-color :black" {:stroke-color :black})
                 (histogram ":stroke-color :white, width 2" {:stroke-color :white :stroke-width 2})
                 (histogram ":stroke-color :none" {:stroke-color :none :stroke-width 3})]
                {:columns 2}))

(def strip-strokes
  {:data barley :mapping {:x :yield :y :site :color :year}
   :scales {:color {:kind :ordinal}}
   :charts [(chart/violin {:stroke-color :black :inner :none :position :mirror})
            (chart/box {:stroke-color :none :width 0.15 :outliers false})]
   :labels {:title "Split violins outlined black, boxes without outline" :x "yield"}})

(def images
  "Images of this demo: `[file-name plot options]`, saved to `results/helpers/` by [[cljplot2.demo/render!]]."
  [["ordered-1000x500.png" ordered {:width 1000 :height 500}]
   ["binned-1000x500.png" binned {:width 1000 :height 500}]
   ["histogram-strokes-900x600.png" histogram-strokes {:width 900 :height 600}]
   ["strip-strokes-700x500.png" strip-strokes {:width 700 :height 500}]])
