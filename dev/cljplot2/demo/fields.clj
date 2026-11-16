(ns cljplot2.demo.fields
  "Correlation cycle 2 demo: 2D fields - faithful density raster with contour lines, filled density contours under
  the points, iris contours by species, a smoothed heatmap (bicubic gridded field beside its tile), a function field
  with contours, a faceted density, contours on a log x axis."
  (:require [fastmath.random :as r]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.demo.parallel :refer [iris]]
            [cljplot2.demo.smooth :refer [faithful]]))

(def faithful-density
  {:data faithful :mapping {:x :eruptions :y :waiting}
   :charts [(chart/field) (chart/contour {:color :white :width 0.8})]
   :labels {:title "Old Faithful: 2D density and its contours" :x "eruption (min)" :y "waiting (min)"}})

(def faithful-filled
  {:data faithful :mapping {:x :eruptions :y :waiting}
   :charts [(chart/contour {:fill true :levels 8}) (chart/scatter {:size 3 :color [0 0 0 160]})]
   :labels {:title "Filled density contours under the points" :x "eruption (min)" :y "waiting (min)"}})

(def iris-contours
  {:data iris :mapping {:x :sepal_length :y :sepal_width :color :species}
   :charts [(chart/scatter {:size 4}) (chart/contour {:levels 6})]
   :labels {:title "Iris: a density per species" :x "sepal length" :y "sepal width"}})

(def ^:private surface
  "Seeded synthetic gridded measurements: two bumps and noise on a coarse 12 x 9 lattice."
  (let [rng (r/rng :mersenne 21)]
    (vec (for [x (range 12) y (range 9)]
           {:x x :y y :z (+ (* 8 (Math/exp (- (/ (+ (Math/pow (- x 3) 2) (Math/pow (- y 6) 2)) 6.0))))
                            (* 5 (Math/exp (- (/ (+ (Math/pow (- x 8) 2) (Math/pow (- y 2) 2)) 8.0))))
                            (* 0.6 (r/drandom rng)))}))))

(def smoothed-heatmap
  (plot/arrange [{:data surface :mapping {:x :x :y :y :color :z} :charts [(chart/tile)]
                  :labels {:title "measured lattice (tile)"}}
                 {:data surface :mapping {:x :x :y :y :value :z}
                  :charts [(chart/field {:interpolation :bicubic}) (chart/contour {:interpolation :bicubic :color [0 0 0 120]})]
                  :labels {:title "smoothed: bicubic field and contours"}}]
                {:columns 2}))

(def ^:private wave (fn [x y] (* (Math/sin x) (Math/cos y))))

(def function-field
  {:charts [(chart/field {:f wave :domain {:x [0 6.3] :y [0 4]}})
            (chart/contour {:f wave :domain {:x [0 6.3] :y [0 4]} :color :black :levels [-0.75 -0.5 -0.25 0 0.25 0.5 0.75]})]
   :scales {:color {:range :RColorBrewer/RdBu :midpoint 0 :title "sin x cos y"}}
   :labels {:title "A function field with contours"}})

(def faceted
  {:data iris :mapping {:x :petal_length :y :petal_width :facet :species}
   :charts [(chart/field) (chart/scatter {:size 2 :color :black})]
   :labels {:title "Iris petals: one density per panel, one color scale"}})

(def ^:private lognormal-points
  (let [rng (r/rng :mersenne 5)]
    (vec (repeatedly 800 (fn [] (let [a (r/grandom rng)] {:income (Math/exp (+ 3 (* 0.8 a))) :score (+ (* 10 a) (* 8 (r/grandom rng)))}))))))

(def log-contour
  {:data lognormal-points :mapping {:x :income :y :score} :scales {:x {:kind :log}}
   :charts [(chart/scatter {:size 2 :color [0 0 0 60]}) (chart/contour {:levels 6})]
   :labels {:title "Contours on a log x axis (estimated in data units)"}})

(def images
  "Images of this demo: `[file-name plot options]`, saved to `results/fields/` by [[cljplot2.demo/render!]]."
  [["faithful-density-600x500.png" faithful-density {:width 600 :height 500}]
   ["faithful-filled-600x500.png" faithful-filled {:width 600 :height 500}]
   ["iris-contours-600x500.png" iris-contours {:width 600 :height 500}]
   ["smoothed-heatmap-1000x450.png" smoothed-heatmap {:width 1000 :height 450}]
   ["function-600x450.png" function-field {:width 600 :height 450}]
   ["faceted-900x400.png" faceted {:width 900 :height 400}]
   ["log-contour-600x450.png" log-contour {:width 600 :height 450}]])
