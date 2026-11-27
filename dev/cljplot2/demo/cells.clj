(ns cljplot2.demo.cells
  "Correlation cycle 1 demo: grid cells - categorical heatmaps (square and hexagon cells), a numeric tile, the mtcars
  correlation matrix with value labels, hexbin and square 2D histograms, an iso-aspect hexbin, faceted hexbins, a log
  y axis, and the column colour-bar recipe (a one-row tile above the heatmap, sharing its x levels)."
  (:require [fastmath.random :as r]
            [fastmath.stats :as stats]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.demo.parallel :refer [mtcars]]
            [cljplot2.demo.smooth :refer [faithful quakes]]))

(def ^:private weekdays ["Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"])

(def ^:private visits
  "Seeded synthetic visits per weekday and hour band."
  (let [rng (r/rng :mersenne 7)]
    (vec (for [d (range 7) h (range 6)]
           {:day (weekdays d) :hours (str (* 4 h) "-" (+ 4 (* 4 h)))
            :visits (+ (* 10 (Math/sin (/ (* h Math/PI) 5.0))) (if (>= d 5) 6 0) (r/irandom rng 0 5))}))))

(defn- heatmap
  [cell title]
  {:data visits :mapping {:x :hours :y :day :color :visits}
   :scales {:y {:domain (vec (reverse weekdays))}}
   :charts [(chart/tile {:cell cell :stroke-color :white})]
   :labels {:title title :x "hours" :y :none}})

(def categorical
  (plot/arrange [(heatmap :square "square cells") (heatmap :pointy-hex "pointy-hex cells (honeycomb)")] {:columns 2}))

(def numeric
  {:data (vec (for [x (range 0 6.01 0.25) y (range 0 4.01 0.25)] {:x x :y y :z (* (Math/sin x) (Math/cos y))}))
   :mapping {:x :x :y :y :color :z}
   :scales {:color {:range :RColorBrewer/RdBu :midpoint 0}}
   :charts [(chart/tile)]
   :labels {:title "Numeric tile: sin(x) cos(y)"}})

(def ^:private mtcars-variables [:mpg :cyl :disp :hp :drat :wt :qsec])

(def correlation
  (let [vs (mapv name mtcars-variables)
        m (mapv vec (stats/correlation-matrix (mapv (fn [k] (mapv k mtcars)) mtcars-variables)))]
    {:data (vec (for [i (range (count vs)) j (range (count vs))] {:a (vs i) :b (vs j) :r (get-in m [i j])}))
     :mapping {:x :a :y :b :color :r}
     :scales {:y {:domain (vec (reverse vs))} :color {:range :RColorBrewer/RdBu :domain [-1 1] :title "r"}}
     :charts [(chart/tile {:stroke-color :white :value-label (fn [{:keys [value]}] (format "%.2f" (double value)))})]
     :labels {:title "mtcars correlation matrix" :x :none :y :none}}))

(def faithful-bins
  (plot/arrange [{:data faithful :mapping {:x :eruptions :y :waiting} :charts [(chart/bin2d {:bins 20})]
                  :labels {:title "hexbin, 20 cells across"}}
                 {:data faithful :mapping {:x :eruptions :y :waiting} :charts [(chart/bin2d {:cell :square :bins 20})]
                  :labels {:title "square 2D histogram"}}]
                {:columns 2}))

(def ^:private normal-pairs
  "Seeded correlated normal pairs."
  (let [rng (r/rng :mersenne 11)]
    (vec (repeatedly 3000 (fn [] (let [a (r/grandom rng) b (r/grandom rng)] {:u a :v (+ (* 0.6 a) (* 0.8 b))}))))))

(def iso
  {:data normal-pairs :mapping {:x :u :y :v} :aspect :iso
   :charts [(chart/bin2d {:bins 25 :stroke-color :white :stroke-width 0.5})]
   :labels {:title "Iso aspect: regular hexagons" :x "u" :y "v"}})

(def faceted
  {:data quakes :mapping {:x :long :y :lat :facet #(if (> (:depth %) 300) "deep (> 300 km)" "shallow")}
   :charts [(chart/bin2d {:bins 15 :measure :percent})]
   :labels {:title "Quakes by depth: one grid over both panels"}})

(def log-scale
  {:data quakes :mapping {:x :mag :y :stations} :scales {:y {:kind :log}}
   :charts [(chart/bin2d {:cell :square :bins 12})]
   :labels {:title "Log y: binned in log10 y, cells even on screen"}})

(def ^:private cars
  (vec (sort-by (juxt :cyl :mpg) (map #(assoc % :car (:name %)) mtcars))))

(def ^:private car-order (mapv :car cars))

(def ^:private standardized
  (let [stat (into {} (for [k mtcars-variables] (let [vs (map k mtcars)] [k [(stats/mean vs) (stats/stddev vs)]])))]
    (vec (for [c cars k mtcars-variables :let [[mu sd] (stat k)]]
           {:car (:car c) :variable (name k) :z (/ (- (double (k c)) mu) sd)}))))

(def colour-bar
  (plot/arrange [{:data cars :mapping {:x :car :y (constantly "cyl") :color #(str (long (:cyl %)))}
                  :scales {:x {:domain car-order :axis false} :color {:title "cyl"}}
                  :charts [(chart/tile)]
                  :labels {:title "Column colour bar recipe: cylinders above the heatmap" :x :none :y :none}}
                 {:data standardized :mapping {:x :car :y :variable :color :z}
                  :scales {:x {:domain car-order} :color {:range :RColorBrewer/RdBu :midpoint 0 :title "z-score"}}
                  :theme {:axis {:x {:label-rotation 90}}}
                  :charts [(chart/tile)]
                  :labels {:x :none :y :none}}]
                {:columns 1 :heights [1 6]}))

(def images
  "Images of this demo: `[file-name plot options]`, saved to `results/cells/` by [[cljplot2.demo/render!]]."
  [["categorical-900x400.png" categorical {:width 900 :height 400}]
   ["numeric-600x450.png" numeric {:width 600 :height 450}]
   ["correlation-600x550.png" correlation {:width 600 :height 550}]
   ["faithful-bins-1000x450.png" faithful-bins {:width 1000 :height 450}]
   ["iso-600x600.png" iso {:width 600 :height 600}]
   ["faceted-900x450.png" faceted {:width 900 :height 450}]
   ["log-scale-600x450.png" log-scale {:width 600 :height 450}]
   ["colour-bar-900x650.png" colour-bar {:width 900 :height 650}]])
