(ns cljplot2.demo.step1
  "Step 1 demo: debug images of the solved grids, saved to `results/step1/`."
  (:require [clojure2d.core :as c2d]
            [cljplot2.core :as plot]
            [cljplot2.examples :as examples]))

(defn -main [& _]
  (doseq [[file-name grid width height] [["fig-1-1-800x600.png" examples/fig-1-1-grid 800 600]
                                         ["fig-1-1-400x300.png" examples/fig-1-1-grid 400 300]
                                         ["nested-1000x600.png" examples/nested-grid 1000 600]]
          :let [path (str "results/step1/" file-name)]]
    (c2d/save (plot/render {:custom-grid grid} {:width width :height height :debug true}) path)
    (println "saved" path)))
