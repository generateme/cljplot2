(ns cljplot2.demo.step6
  "Step 6 demo: facets, saved to `results/step6/`.
  Fig 1.1 layout (Chem97, faceted by `score`, 3x2) and grid facets with an NA row."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure2d.core :as c2d]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]))

(def chem97 (json/read-str (slurp (io/resource "chem97.json")) :key-fn keyword))

(def fig-1-1
  {:data chem97
   :mapping {:x :gcsescore :y :age :facet :score}
   :charts [(chart/scatter {:size 2})]
   :labels {:title "Chem97: gcsescore by score" :x "gcsescore" :y "age"}})

(def grid-plot
  {:data (for [i (range 120)]
           {:x (mod (* i 37) 100) :y (mod (* i 53) 100)
            :g (["a" "b" "c"] (mod i 3)) :h (get [:lo :hi nil] (mod i 3))})
   :mapping {:x :x :y :y :facet-col :g :facet-row :h}
   :charts [(chart/scatter)]
   :labels {:title "Grid facets with an NA row" :x "x" :y "y"}})

(defn -main [& _]
  (doseq [[file-name p options] [["fig-1-1-800x600.png" fig-1-1 {:width 800 :height 600}]
                                 ["fig-1-1-800x600-debug.png" fig-1-1 {:width 800 :height 600 :debug true}]
                                 ["grid-700x500.png" grid-plot {:width 700 :height 500}]]
          :let [path (str "results/step6/" file-name)]]
    (c2d/save (plot/render p options) path)
    (println "saved" path)))
