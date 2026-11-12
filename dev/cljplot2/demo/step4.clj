(ns cljplot2.demo.step4
  "Step 4 demo: scales, axes and grid lines, saved to `results/step4/` (plain and debug)."
  (:require [clojure2d.core :as c2d]
            [cljplot2.core :as plot]))

(def plots
  {"linear-bands" {:labels {:title "Scales and axes" :x "x (linear)" :y "y (bands)"}
                   :scales {:x {:domain [0.13 7.31]} :y {:domain ["a" "b" "c"]}}}
   "wide-labels" {:labels {:x "x" :y "y"}
                  :scales {:x {:domain [-5 5]} :y {:domain [0 1e6]}}}})

(defn -main [& _]
  (doseq [[nm plot] plots
          [suffix options] [["" {:width 600 :height 400}] ["-debug" {:width 600 :height 400 :debug true}]]
          :let [path (str "results/step4/" nm "-600x400" suffix ".png")]]
    (c2d/save (plot/render plot options) path)
    (println "saved" path)))
