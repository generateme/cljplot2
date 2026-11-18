(ns cljplot2.demo.trellis
  "Lattice cycle 1 demo (trellis layout): free and sliced facet scales, panels without strips, panel weights, level
  order by a statistic (facets and categorical axes), power labels and minor ticks - after LMDVR Figs 2.9, 4.7, 8.2,
  8.4, 8.5, 10.20, 10.21, 10.22 on the datasets of `notebooks/data` and seeded synthetic data."
  (:require [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.stats :as stats]
            [cljplot2.chart :as chart]
            [cljplot2.data.shingle :as shingle]
            [cljplot2.demo.data :as data]))

(def ^:private titanic
  (mapv #(update % :n parse-long) (data/csv-rows "titanic.csv")))

(def ^:private mtcars
  (mapv #(-> % (update :mpg parse-double) (update :cyl parse-long)) (data/csv-rows "mtcars.csv")))

(def ^:private barley (data/json-rows "barley.json"))

(def fig-2-9
  "Fig 2.9: stacked bars of Titanic passengers by class, a panel per sex and age, each with its own count axis."
  {:data titanic
   :mapping {:x :n :y :class :color :survived :facet #(str (:sex %) " " (:age %))}
   :charts [(chart/bar {:position :stack})]
   :facets {:columns 4 :scales {:x :free}}
   :labels {:x "Passengers" :y "Class"}})

(def fig-10-20
  "Fig 10.20 (cars instead of states): a dot per car, a panel per cylinder count; each panel lists only its cars."
  {:data mtcars
   :mapping {:x :mpg :y :model :facet :cyl}
   :charts [(chart/scatter)]
   :facets {:columns 1 :scales {:y :free}}
   :scales {:y {:order-by #(stats/mean (map :mpg %))}}
   :labels {:x "Miles per gallon"}})

(def fig-10-21
  "Fig 10.21: as Fig 10.20 with panel heights proportional to the number of cars."
  (let [counts (frequencies (map :cyl mtcars))]
    (assoc-in fig-10-20 [:facets :heights] (mapv counts (sort (keys counts))))))

(def fig-10-22
  "Fig 10.22 (barley instead of cancer rates): a panel per site, ordered by the site's median yield."
  {:data barley
   :mapping {:x :yield :y :variety :color #(str (:year %)) :facet :site}
   :charts [(chart/scatter)]
   :scales {:facet {:order-by #(stats/median (map :yield %))}}
   :facets {:columns 3}
   :labels {:x "Yield" :y nil}})

(def fig-4-7
  "Fig 4.7 (barley 1932 instead of postdoc reasons): varieties ordered within each site panel by yield."
  {:data (filterv #(= 1932 (:year %)) barley)
   :mapping {:x :yield :y :variety :facet :site}
   :charts [(chart/scatter)]
   :scales {:y {:order-by #(stats/mean (map :yield %))}}
   :facets {:columns 1 :scales {:y :free}}
   :labels {:x "Yield (1932)" :y nil}})

(def ^:private walk
  "Seeded random walk of 600 steps (a stand-in for daily access counts)."
  (let [rng (r/rng :mersenne 81)]
    (mapv (fn [t v] {:t t :v v}) (range 600) (reductions + 100.0 (repeatedly 599 #(r/grandom rng 0 3))))))

(def fig-8-2
  "Fig 8.2: a long series cut into overlapping time slices (a shingle), every slice the same time span, no strips."
  {:data (shingle/shingle walk :t {:count 6 :overlap 0.1 :as :slice})
   :mapping {:x :t :y :v :facet :slice}
   :charts [(chart/line)]
   :facets {:columns 1 :strips false :scales {:x :sliced :y :free}}
   :labels {:x nil :y "Value"}})

(def ^:private attenuation
  "Seeded synthetic attenuation: acceleration falling with distance (log-log linear), as the Earthquake data."
  (let [rng (r/rng :mersenne 84)]
    (vec (for [_ (range 150)
               :let [d (m/pow 2.0 (r/drandom rng 0 8.5))]]
           {:distance d :accel (* 0.9 (m/pow d -0.75) (m/exp (r/grandom rng 0 0.4)))}))))

(def fig-8-4
  "Fig 8.4: log2 axes labelled as powers of two."
  {:data attenuation
   :mapping {:x :distance :y :accel}
   :charts [(chart/scatter)]
   :scales {:x {:kind :log :base 2 :formatter :power} :y {:kind :log :base 2 :formatter :power}}
   :labels {:x "Distance from epicenter (km)" :y "Maximum horizontal acceleration (g)"}})

(def fig-8-5
  "Fig 8.5: log10 axes with minor ticks at 2 .. 9 of every decade (and their grid lines)."
  {:data attenuation
   :mapping {:x :distance :y :accel}
   :charts [(chart/scatter)]
   :scales {:x {:kind :log :minor-ticks 1} :y {:kind :log :minor-ticks 1}}
   :theme {:panel {:grid {:minor {:color [180 180 180] :width 0.5 :dash [1.0 2.0]}}}}
   :labels {:x "Distance from epicenter (km)" :y "Maximum horizontal acceleration (g)"}})

(def images
  "Images of this demo: `[file-name plot options]`, saved to `results/trellis/` by [[cljplot2.demo/render!]]."
  [["fig-2-9-free-x-900x350.png" fig-2-9 {:width 900 :height 350}]
   ["fig-10-20-free-y-600x700.png" fig-10-20 {:width 600 :height 700}]
   ["fig-10-21-heights-600x700.png" fig-10-21 {:width 600 :height 700}]
   ["fig-10-22-order-by-800x600.png" fig-10-22 {:width 800 :height 600}]
   ["fig-4-7-panel-order-500x900.png" fig-4-7 {:width 500 :height 900}]
   ["fig-8-2-sliced-800x700.png" fig-8-2 {:width 800 :height 700}]
   ["fig-8-4-power-600x450.png" fig-8-4 {:width 600 :height 450}]
   ["fig-8-5-minor-ticks-600x450.png" fig-8-5 {:width 600 :height 450}]])
