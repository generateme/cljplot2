(ns cljplot2.demo.step9
  "Step 9 demo: multiplots, saved to `results/step9/`.
  LMDVR Fig 1.4 (Fig 1.1 histogram over the Fig 1.3 grouped density), a 2x2 arrangement with weights and a title,
  and a nested multiplot."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure2d.core :as c2d]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]))

(def chem97 (json/read-str (slurp (io/resource "chem97.json")) :key-fn keyword))
(def faithful (json/read-str (slurp (io/resource "faithful.json")) :key-fn keyword))

(def histogram-by-score
  {:data chem97
   :mapping {:x :gcsescore :facet :score}
   :charts [(chart/histogram)]
   :labels {:x "gcsescore"}})

(def density-by-score
  {:data chem97
   :mapping {:x :gcsescore :color :score}
   :scales {:color {:kind :ordinal}}
   :charts [(chart/density)]
   :labels {:x "gcsescore"}})

(def fig-1-4 (plot/arrange [histogram-by-score density-by-score]))

(def quad
  (plot/arrange [{:data faithful :mapping {:x :eruptions :y :waiting} :charts [(chart/scatter)] :labels {:title "scatter"}}
                 {:data faithful :mapping {:x :eruptions} :charts [(chart/histogram {:y :count})] :labels {:title "histogram"}}
                 {:data faithful :mapping {:x :waiting} :charts [(chart/density {:fill [49 54 149 60]})] :labels {:title "density"}}
                 {:data faithful :mapping {:x :waiting} :charts [(chart/ecdf)] :labels {:title "ECDF"}}]
                {:columns 2 :widths [2 1] :title "Old Faithful"}))

(def nested
  (plot/arrange [quad density-by-score] {:columns 2 :title "Nested multiplot"
                                         :theme {:plot {:background [255 255 255]}}}))

(defn -main [& _]
  (doseq [[file-name p options] [["fig-1-4-800x900.png" fig-1-4 {:width 800 :height 900}]
                                 ["fig-1-4-800x900-debug.png" fig-1-4 {:width 800 :height 900 :debug true}]
                                 ["quad-900x600.png" quad {:width 900 :height 600}]
                                 ["nested-1200x600.png" nested {:width 1200 :height 600}]]
          :let [path (str "results/step9/" file-name)]]
    (c2d/save (plot/render p options) path)
    (println "saved" path)))
