(ns cljplot2.demo.sankeys
  "Flow cycle 4 demo: sankey diagrams of UK energy flows (justified, and left-aligned with target-coloured flows), a
  Titanic alluvial diagram and a sunburst of website visit sequences.

  Data (`notebooks/data`, sources and licences in its `datasets.edn`): `energy.csv` - networkD3
  `JSONdata/energy.json` (the d3 sankey example; UK Department of Energy & Climate Change 2050 Calculator data; the
  package is GPL-3, the data's licence isn't stated); `titanic.csv` - R `datasets::Titanic` passenger counts (Dawson
  1995); visit sequences - synthetic, a seeded random walk over six pages."
  (:require [clojure.string :as str]
            [fastmath.random :as r]
            [cljplot2.core :as plot]
            [cljplot2.demo.data :as data]))

(def ^:private energy
  (mapv #(update % :value parse-double) (data/csv-rows "energy.csv")))

(def energy-flows
  (plot/sankey energy {:title "UK energy flows in 2050 (TWh, DECC scenario)"}))

(def energy-left
  (plot/sankey energy {:align :left :link-color :target :title "Left-aligned, flows in their target's colour"}))

(def ^:private titanic
  "R `Titanic` counts without the empty cells."
  (vec (for [row (data/csv-rows "titanic.csv") :let [n (parse-long (:n row))] :when (pos? n)] (assoc row :n n))))

(def titanic-alluvial
  (plot/alluvial titanic {:axes [:class :sex :age :survived] :value :n :fill :class
                          :theme {:palette {:color :set1}}
                          :title "Titanic: 2201 people by class, sex, age and survival"}))

(def ^:private pages ["home" "search" "product" "account" "cart" "help"])

(def ^:private visits
  "2000 synthetic sessions: from the home page, each step to another page or the end (more likely the longer the
  session), at most 6 pages."
  (let [g (r/rng :mersenne 7)
        weights {"home" [0 5 3 2 0 1] "search" [1 2 6 0 1 1] "product" [1 2 2 1 4 1]
                 "account" [2 0 1 1 1 2] "cart" [0 0 2 2 0 1] "help" [2 2 0 2 0 1]}
        pick (fn [ws] (let [total (double (reduce + ws)) u (* total (r/drandom g))]
                        (loop [i 0 acc 0.0] (let [acc (+ acc (double (ws i)))] (if (or (< u acc) (= i (dec (count ws)))) i (recur (inc i) acc))))))]
    (vec (for [_ (range 2000)]
           {:sequence (str/join "-" (loop [path ["home"]]
                                      (if (or (= 6 (count path)) (< (r/drandom g) (* 0.12 (count path))))
                                        (conj path "end")
                                        (recur (conj path (pages (pick (weights (peek path)))))))))}))))

(def visit-sequences
  (plot/sequences visits {:title "2000 website visits: pages in order from the centre"
                          :scales {:color {:domain (conj pages "end")}}}))

(def images
  "Images of this demo: `[file-name plot options]`, saved to `results/sankeys/` by [[cljplot2.demo/render!]]."
  [["energy-1000x650.png" energy-flows {:width 1000 :height 650}]
   ["energy-left-1000x650.png" energy-left {:width 1000 :height 650}]
   ["titanic-alluvial-900x600.png" titanic-alluvial {:width 900 :height 600}]
   ["visit-sequences-700x650.png" visit-sequences {:width 700 :height 650}]])
