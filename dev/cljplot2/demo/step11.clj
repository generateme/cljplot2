(ns cljplot2.demo.step11
  "Step 11 demo: LMDVR Fig 5.6 (quakes: shingles, iso aspect, colour bar, draw order)."
  (:require [cljplot2.chart :as chart]
            [cljplot2.demo.data :as data]
            [cljplot2.data.shingle :as shingle]))

(def quakes (data/json-rows "quakes.json"))

(def fig-5-6
  {:data (shingle/shingle quakes :mag {:n 4 :as :magnitude :label "Magnitude"})
   :mapping {:x :long :y :lat :facet :magnitude :color :depth :order :depth}
   :scales {:color {:range [[230 230 230] [60 60 60]]}}
   :aspect :iso
   :charts [(chart/scatter {:size 8 :shape \O :stroke-color :black :descending true})]
   :labels {:x "Longitude" :y "Latitude"}})

(def fig-5-6-free (dissoc fig-5-6 :aspect))

(def images
  "Images of this step: `[file-name plot options]`, saved to `results/step11/` by [[cljplot2.demo/render!]]."
  [["fig-5-6-800x700.png" fig-5-6 {:width 800 :height 700}]
   ["fig-5-6-800x700-debug.png" fig-5-6 {:width 800 :height 700 :debug true}]
   ["fig-5-6-no-iso-800x700.png" fig-5-6-free {:width 800 :height 700}]])
