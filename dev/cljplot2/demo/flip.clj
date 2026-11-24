(ns cljplot2.demo.flip
  "Flip demo (`:flip true`): the `:x` scale drawn vertically, `:y` horizontally - bands scatter (Fig 2.6), histogram,
  grouped density, facets, line area, ECDF, iso aspect and theme by scale key; rotated bands tick labels."
  (:require [cljplot2.chart :as chart]
            [cljplot2.demo.data :as data]))

(def barley (data/json-rows "barley.json"))
(def chem97 (data/chem97-rows))
(def faithful (data/json-rows "faithful.json"))

(def fig-2-6
  {:data barley
   :mapping {:x :yield :y :variety :facet :site :color :year}
   :scales {:color {:kind :ordinal}}
   :facets {:rows 1}
   :flip true
   :charts [(chart/scatter {:size 6})]
   :labels {:x "Barley Yield (bushels/acre)" :title "Fig 2.6 flipped"}})

(def histogram
  {:data faithful
   :mapping {:x :eruptions}
   :flip true
   :charts [(chart/histogram {:bins 20})]
   :labels {:title "Histogram, flipped" :x "eruptions [min]"}})

(def density
  {:data chem97
   :mapping {:x :gcsescore :color :score}
   :scales {:color {:kind :ordinal}}
   :flip true
   :charts [(chart/density {:fill [0 0 0 25]})]
   :labels {:title "Grouped density, flipped" :x "gcsescore"}})

(def facets
  {:data chem97
   :mapping {:x :gcsescore :y :age :facet :score}
   :flip true
   :charts [(chart/scatter {:size 2})]
   :labels {:title "Facets, flipped" :x "gcsescore" :y "age"}})

(def area
  {:data (for [x (range 40)] {:x x :y (* 10 (Math/sin (/ x 6.0)))})
   :mapping {:x :x :y :y}
   :flip true
   :charts [(chart/line {:fill [69 117 180 90] :line-width 1.5})]
   :labels {:title "Area to y = 0, flipped" :x "x" :y "y"}})

(def ecdf
  {:data faithful
   :mapping {:x :eruptions :color #(if (> (:waiting %) 70) "long wait" "short wait")}
   :flip true
   :charts [(chart/ecdf {:line-width 1.5})]
   :labels {:title "ECDF, flipped" :x "eruptions"}})

(def iso
  "An ellipse with `:x` span 4 and `:y` span 2: flipped, the longer `:x` span is vertical."
  {:data (for [i (range 60) :let [t (* 2.0 Math/PI (/ i 60.0))]] {:x (* 2.0 (Math/cos t)) :y (Math/sin t)})
   :mapping {:x :x :y :y}
   :aspect :iso
   :flip true
   :charts [(chart/scatter)]
   :labels {:title "Iso aspect, flipped" :x "x (span 4)" :y "y (span 2)"}})

(def x-theme
  (assoc histogram
         :theme {:axis {:x {:tick-spacing 200}}}
         :labels {:title "Theme [:axis :x] styles the vertical :x axis" :x "eruptions [min]"}))

(def images
  "Images of this demo: `[file-name plot options]`, saved to `results/flip/` by [[cljplot2.demo/render!]]."
  [["fig-2-6-900x450.png" fig-2-6 {:width 900 :height 450}]
   ["fig-2-6-rotated-90-900x500.png" (assoc fig-2-6 :theme {:axis {:y {:label-rotation -90}}}) {:width 900 :height 500}]
   ["fig-2-6-rotated-45-900x500.png" (assoc fig-2-6 :theme {:axis {:y {:label-rotation -45}}}) {:width 900 :height 500}]
   ["fig-2-6-rotated-plus-45-900x500.png" (assoc fig-2-6 :theme {:axis {:y {:label-rotation 45}}}) {:width 900 :height 500}]
   ["histogram-500x500.png" histogram {:width 500 :height 500}]
   ["histogram-500x500-debug.png" histogram {:width 500 :height 500 :debug true}]
   ["density-600x500.png" density {:width 600 :height 500}]
   ["facets-800x600.png" facets {:width 800 :height 600}]
   ["area-500x500.png" area {:width 500 :height 500}]
   ["ecdf-600x400.png" ecdf {:width 600 :height 400}]
   ["iso-600x500.png" iso {:width 600 :height 500}]
   ["x-theme-500x500.png" x-theme {:width 500 :height 500}]])
