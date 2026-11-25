(ns cljplot2.demo.step9
  "Step 9 demo: multiplots.
  LMDVR Fig 1.4 (Fig 1.1 histogram over the Fig 1.3 grouped density), a 2x2 arrangement with weights and a title,
  a nested multiplot, and the same mixed plots without and with panel alignment."
  (:require [cljplot2.chart :as chart]
            [cljplot2.demo.data :as data]
            [cljplot2.core :as plot]))

(def chem97 (data/chem97-rows))
(def faithful (data/json-rows "faithful.json"))

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
                 {:data faithful :mapping {:x :waiting} :charts [(chart/density {:fill [0 114 178 60]})] :labels {:title "density"}}
                 {:data faithful :mapping {:x :waiting} :charts [(chart/ecdf)] :labels {:title "ECDF"}}]
                {:columns 2 :widths [2 1] :title "Old Faithful"}))

(def nested
  (plot/arrange [quad density-by-score] {:columns 2 :title "Nested multiplot"
                                         :theme {:plot {:background [255 255 255]}}}))

(def mixed
  [{:data faithful :mapping {:x :eruptions :y #(* 1000 (:waiting %))} :charts [(chart/scatter)]
    :labels {:title "big y labels" :y "waiting (ms)"}}
   {:data faithful :mapping {:x :eruptions} :charts [(chart/histogram {:y :count})]}
   {:data (map #(assoc % :long (if (> (:waiting %) 70) "long" "short")) faithful)
    :mapping {:x :eruptions :y :waiting :color :long} :charts [(chart/scatter)]}
   {:data (map #(assoc % :long (if (> (:waiting %) 70) "long" "short")) faithful)
    :mapping {:x :eruptions :facet :long} :charts [(chart/density)] :labels {:x "eruptions"}}])

(def unaligned (plot/arrange mixed {:columns 2 :title "Not aligned (:align false)" :align false}))
(def aligned (plot/arrange mixed {:columns 2 :title "Aligned panels (default)"}))

(def images
  "Images of this step: `[file-name plot options]`, saved to `results/step9/` by [[cljplot2.demo/render!]]."
  [["fig-1-4-800x900.png" fig-1-4 {:width 800 :height 900}]
   ["fig-1-4-800x900-debug.png" fig-1-4 {:width 800 :height 900 :debug true}]
   ["quad-900x600.png" quad {:width 900 :height 600}]
   ["nested-1200x600.png" nested {:width 1200 :height 600}]
   ["unaligned-900x700.png" unaligned {:width 900 :height 700}]
   ["aligned-900x700.png" aligned {:width 900 :height 700}]])
