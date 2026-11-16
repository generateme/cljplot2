(ns cljplot2.demo.venn
  "Part of a whole cycle 3 demo: Venn diagrams of synthetic membership rows - two sets (area-proportional), a subset,
  three sets (classic layout) - and the UpSet recipe of `cljplot2.data.sets`."
  (:require [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.sets :as sets]))

(def members
  "Items 0-59 in A, 40-89 in B, 50-74 and 85-119 in C."
  (concat (for [i (range 0 60)] {:set "A" :item i})
          (for [i (range 40 90)] {:set "B" :item i})
          (for [i (range 50 75)] {:set "C" :item i})
          (for [i (range 85 120)] {:set "C" :item i})))

(def mapping {:set :set :item :item})

(def two-sets
  {:data members :mapping mapping :charts [(chart/venn {:sets ["A" "B"]})] :labels {:title "Two sets (areas proportional)"}})

(def subset
  {:data (concat (for [i (range 100)] {:set "All" :item i}) (for [i (range 30)] {:set "Some" :item i}))
   :mapping mapping :charts [(chart/venn)] :labels {:title "A subset"}})

(def three-sets
  {:data members :mapping mapping :charts [(chart/venn {:region-labels #(format "%d\n%.0f%%" (:size %) (* 100 (:share %)))})]
   :labels {:title "Three sets: counts and shares"}})

(def upset
  (let [dots (sets/membership-rows members mapping)]
    (plot/arrange [{:data (sets/upset-rows members mapping) :mapping {:x :combination :y :size}
                    :charts [(chart/bar {:value-label true})] :labels {:title "UpSet recipe" :x nil :y "Items"}}
                   {:data dots :mapping {:x :combination :y :set}
                    :charts [(chart/scatter {:size 12 :color [210 210 210]})
                             (chart/scatter {:data (filter :member? dots) :size 12 :color :black})]
                    :labels {:x nil :y nil}}]
                  {:columns 1})))

(def images
  "Images of this demo: `[file-name plot options]`, saved to `results/venn/` by [[cljplot2.demo/render!]]."
  [["two-sets-500x400.png" two-sets {:width 500 :height 400}]
   ["subset-500x400.png" subset {:width 500 :height 400}]
   ["three-sets-500x500.png" three-sets {:width 500 :height 500}]
   ["upset-600x500.png" upset {:width 600 :height 500}]])
