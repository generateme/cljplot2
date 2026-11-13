(ns cljplot2.demo.positions
  "Group positions demo (`cljplot2.data.position`): histogram stack / fill / mirror, density stack / fill / mirror,
  flipped variants."
  (:require [fastmath.random :as r]
            [cljplot2.chart :as chart]))

(def two-groups
  (let [rng (r/rng :mersenne 42)]
    (vec (concat (for [_ (range 400)] {:v (r/grandom rng 10 2) :g "a"})
                 (for [_ (range 300)] {:v (r/grandom rng 14 3) :g "b"})))))

(def three-groups
  (let [rng (r/rng :mersenne 7)]
    (vec (concat (for [_ (range 300)] {:v (r/grandom rng 8 2) :g "a"})
                 (for [_ (range 300)] {:v (r/grandom rng 12 2) :g "b"})
                 (for [_ (range 200)] {:v (r/grandom rng 15 1.5) :g "c"})))))

(defn- histogram [data position extra]
  {:data data
   :mapping {:x :v :color :g}
   :charts [(chart/histogram (merge {:position position :y :count :bins 30} extra))]
   :labels {:title (str "Histogram :position " position) :x "v"}})

(defn- density [data position extra]
  {:data data
   :mapping {:x :v :color :g}
   :charts [(chart/density (merge {:position position :width 1.5} extra))]
   :labels {:title (str "Density :position " position) :x "v"}})

(def images
  "Images of this demo: `[file-name plot options]`, saved to `results/positions/` by [[cljplot2.demo/render!]]."
  [["hist-stack-600x400.png" (histogram three-groups :stack {}) {:width 600 :height 400}]
   ["hist-fill-600x400.png" (histogram three-groups :fill {}) {:width 600 :height 400}]
   ["hist-stack-flip-500x500.png" (assoc (histogram three-groups :stack {}) :flip true) {:width 500 :height 500}]
   ["density-stack-600x400.png" (density three-groups :stack {}) {:width 600 :height 400}]
   ["density-fill-600x400.png" (density three-groups :fill {}) {:width 600 :height 400}]
   ["density-mirror-600x400.png" (density two-groups :mirror {:fill [0 0 0 30]}) {:width 600 :height 400}]
   ["density-mirror-symmetric-600x400.png" (density two-groups :mirror {:symmetric true}) {:width 600 :height 400}]
   ["density-mirror-flip-500x500.png" (assoc (density two-groups :mirror {}) :flip true) {:width 500 :height 500}]])
