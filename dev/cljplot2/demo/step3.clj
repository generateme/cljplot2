(ns cljplot2.demo.step3
  "Step 3 demo: empty panel with titles, saved to `results/step3/` (plain and debug)."
  (:require [clojure2d.core :as c2d]
            [cljplot2.core :as plot]))

(def plot
  {:labels {:title "Empty panel" :x "x title" :y "y title"}})

(defn -main [& _]
  (doseq [[file-name options] [["panel-600x400.png" {:width 600 :height 400}]
                               ["panel-600x400-debug.png" {:width 600 :height 400 :debug true}]]
          :let [path (str "results/step3/" file-name)]]
    (c2d/save (plot/render plot options) path)
    (println "saved" path)))
