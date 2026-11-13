(ns cljplot2.demo.step11
  "Step 11 demo: LMDVR Fig 5.6 (quakes: shingles, iso aspect, colour bar, draw order)."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [cljplot2.chart :as chart]
            [cljplot2.data.shingle :as shingle]))

(def quakes (json/read-str (slurp (io/resource "quakes.json")) :key-fn keyword))

(def fig-5-6
  {:data (shingle/shingle quakes :mag {:count 4 :as :magnitude :label "Magnitude"})
   :mapping {:x :long :y :lat :facet :magnitude :color :depth :order :depth}
   :scales {:color {:range [[230 230 230] [60 60 60]]}}
   :aspect :iso
   :charts [(chart/scatter {:size 8 :shape \O :stroke-color :black :order-direction :descending})]
   :labels {:x "Longitude" :y "Latitude"}})

(def fig-5-6-free (dissoc fig-5-6 :aspect))

(def images
  "Images of this step: `[file-name plot options]`, saved to `results/step11/` by [[cljplot2.demo/run!]]."
  [["fig-5-6-800x700.png" fig-5-6 {:width 800 :height 700}]
   ["fig-5-6-800x700-debug.png" fig-5-6 {:width 800 :height 700 :debug true}]
   ["fig-5-6-no-iso-800x700.png" fig-5-6-free {:width 800 :height 700}]])
