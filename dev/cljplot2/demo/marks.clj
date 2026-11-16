(ns cljplot2.demo.marks
  "Correlation cycle 4 demo: area-scaled bubbles beside linear sizes, scatter labels on semi-transparent boxes,
  scatter jitter of overplotted integer data."
  (:require [fastmath.random :as r]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.demo.parallel :refer [mtcars]]))

(defn- bubbles
  [title area?]
  {:data mtcars :mapping {:x :wt :y :mpg :size :hp}
   :charts [(chart/scatter {:color [70 110 180 150] :stroke-color [30 60 120]})]
   :scales {:size (cond-> {:range [2 24] :title "hp"} area? (assoc :area true))}
   :labels {:title title :x "weight (1000 lbs)" :y "miles per gallon"}})

(def area-sizes
  (plot/arrange [(bubbles "size linear in hp" false) (bubbles "area linear in hp (:area true)" true)] {:columns 2}))

(def ^:private label-box {:size 9 :background [255 255 255 190] :padding 2})

(defn- car-labels
  [keep? anchor dx]
  (chart/text (merge label-box {:data (filterv keep? mtcars) :mapping {:x :wt :y :mpg :label :name}
                                :anchor anchor :offset [dx 0]})))

(def labelled
  {:data mtcars :mapping {:x :wt :y :mpg}
   :charts [(chart/scatter {:size 9 :color [200 60 40]})
            (car-labels #(> (double (:mpg %)) 31.0) [:left :middle] 6)
            (car-labels #(> (double (:wt %)) 5.3) [:right :middle] -6)
            (chart/text (merge label-box {:x 3.5 :y 17.0 :text "heavy V8 cluster" :size 11}))]
   :labels {:title "Labels on semi-transparent boxes" :x "weight (1000 lbs)" :y "miles per gallon"}})

(defn- gears
  [title jitter]
  {:data mtcars :mapping {:x :cyl :y :gear}
   :charts [(chart/scatter (cond-> {:size 7 :color [40 40 40 120]} jitter (assoc :jitter jitter :rng (r/rng :mersenne 7))))]
   :labels {:title title :x "cylinders" :y "gears"}})

(def jitter
  (plot/arrange [(gears "plain: 32 cars, 8 positions" nil) (gears "jitter [0.2 0.2]" [0.2 0.2])] {:columns 2}))

(def images
  "Images of this demo: `[file-name plot options]`, saved to `results/marks/` by [[cljplot2.demo/render!]]."
  [["area-sizes-900x420.png" area-sizes {:width 900 :height 420}]
   ["labelled-600x450.png" labelled {:width 600 :height 450}]
   ["jitter-800x380.png" jitter {:width 800 :height 380}]])
