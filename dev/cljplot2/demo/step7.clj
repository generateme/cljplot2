(ns cljplot2.demo.step7
  "Step 7 demo: groups and legends, saved to `results/step7/`.
  Fig 2.6 (barley: variety by yield, faceted by site, colour by year), merged colour + shape legend (Chem97),
  numeric size and continuous colour (faithful)."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure2d.core :as c2d]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]))

(defn- read-json [file] (json/read-str (slurp (io/resource file)) :key-fn keyword))

(def barley (read-json "barley.json"))
(def chem97 (read-json "chem97.json"))
(def faithful (read-json "faithful.json"))

(def fig-2-6
  {:data barley
   :mapping {:x :yield :y :variety :facet :site :color :year}
   :scales {:color {:kind :ordinal}}
   :facets {:columns 1}
   :charts [(chart/scatter {:size 6})]
   :labels {:x "Barley Yield (bushels/acre)"}})

(def merged
  {:data (take 3000 chem97)
   :mapping {:x :gcsescore :y :age :color :gender :shape :gender}
   :charts [(chart/scatter {:size 5})]
   :labels {:title "Chem97: colour and shape by gender" :x "gcsescore" :y "age"}})

(def size-and-gradient
  {:data faithful
   :mapping {:x :eruptions :y :waiting :size :waiting :color :eruptions}
   :charts [(chart/scatter)]
   :labels {:title "Faithful: size by waiting, gradient by eruptions" :x "eruptions" :y "waiting"}})

(def two-charts
  {:data (for [i (range 60)] {:x i :y (mod (* i 7) 23) :g (["a" "b" "c" nil] (mod i 4))})
   :mapping {:x :x :y :y :color :g}
   :charts [(chart/scatter {:size 9 :shape \s}) (chart/scatter {:size 4 :shape \x :color :black :mapping {:color nil}})
            (chart/scatter {:size 3 :shape \O})]
   :labels {:title "Overlay: squares + dots mapped, crosses not; NA level" :x "x" :y "y"}})

(defn -main [& _]
  (doseq [[file-name p options] [["fig-2-6-400x900.png" fig-2-6 {:width 400 :height 900}]
                                 ["fig-2-6-400x900-debug.png" fig-2-6 {:width 400 :height 900 :debug true}]
                                 ["merged-700x500.png" merged {:width 700 :height 500}]
                                 ["size-gradient-700x500.png" size-and-gradient {:width 700 :height 500}]
                                 ["two-charts-700x500.png" two-charts {:width 700 :height 500}]]
          :let [path (str "results/step7/" file-name)]]
    (c2d/save (plot/render p options) path)
    (println "saved" path)))
