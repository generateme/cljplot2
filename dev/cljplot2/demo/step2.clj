(ns cljplot2.demo.step2
  "Step 2 demo: Fig 1.1 layout with real text, saved to `results/step2/` (plain and debug)."
  (:require [clojure2d.core :as c2d]
            [cljplot2.core :as plot]
            [cljplot2.examples :as examples]))

(defn -main [& _]
  (doseq [[file-name options] [["fig-1-1-800x600.png" {:width 800 :height 600}]
                               ["fig-1-1-800x600-debug.png" {:width 800 :height 600 :debug true}]
                               ["fig-1-1-400x300.png" {:width 400 :height 300}]]
          :let [path (str "results/step2/" file-name)]]
    (c2d/save (plot/render {:custom-grid examples/fig-1-1-labelled-grid} options) path)
    (println "saved" path)))
