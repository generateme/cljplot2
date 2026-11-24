(ns cljplot2.demo.step5
  "Step 5 demo: scatter of `faithful` (eruptions vs waiting) (plain and debug)."
  (:require [cljplot2.chart :as chart]
            [cljplot2.demo.data :as data]))

(def faithful (data/json-rows "faithful.json"))

(def plot
  {:data faithful
   :mapping {:x :eruptions :y :waiting}
   :charts [(chart/scatter)]
   :labels {:title "Old Faithful" :x "eruptions [min]" :y "waiting [min]"}})

(def images
  "Images of this step: `[file-name plot options]`, saved to `results/step5/` by [[cljplot2.demo/render!]]."
  [["faithful-600x400.png" plot {:width 600 :height 400}]
   ["faithful-600x400-debug.png" plot {:width 600 :height 400 :debug true}]])
