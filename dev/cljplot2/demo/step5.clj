(ns cljplot2.demo.step5
  "Step 5 demo: scatter of `faithful` (eruptions vs waiting), saved to `results/step5/` (plain and debug)."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure2d.core :as c2d]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]))

(def faithful (json/read-str (slurp (io/resource "faithful.json")) :key-fn keyword))

(def plot
  {:data faithful
   :mapping {:x :eruptions :y :waiting}
   :charts [(chart/scatter)]
   :labels {:title "Old Faithful" :x "eruptions [min]" :y "waiting [min]"}})

(defn -main [& _]
  (doseq [[file-name options] [["faithful-600x400.png" {:width 600 :height 400}]
                               ["faithful-600x400-debug.png" {:width 600 :height 400 :debug true}]]
          :let [path (str "results/step5/" file-name)]]
    (c2d/save (plot/render plot options) path)
    (println "saved" path)))
