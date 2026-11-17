(ns cljplot2.demo.sankeys
  "Flow cycle 4 demo: sankey diagrams of UK energy flows (justified, and left-aligned with target-coloured flows), a
  Titanic alluvial diagram and a sunburst of website visit sequences.

  Data: `energy.json` - networkD3 `JSONdata/energy.json` (the d3 sankey example; UK Department of Energy & Climate
  Change 2050 Calculator data; the package is GPL-3, the data's licence isn't stated); Titanic - R `datasets::Titanic`
  passenger counts (Dawson 1995), inline; visit sequences - synthetic, a seeded random walk over six pages."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [fastmath.random :as r]
            [cljplot2.core :as plot]))

(def ^:private energy
  (let [{:keys [nodes links]} (json/read-str (slurp (io/resource "energy.json")) :key-fn keyword)
        names (mapv :name nodes)]
    (mapv (fn [{:keys [source target value]}] {:from (names source) :to (names target) :value value}) links)))

(def energy-flows
  (plot/sankey energy {:title "UK energy flows in 2050 (TWh, DECC scenario)"}))

(def energy-left
  (plot/sankey energy {:align :left :link-color :target :title "Left-aligned, flows in their target's colour"}))

(def ^:private titanic
  "R `Titanic`: `[class sex age survived n]`."
  (for [[class sex age survived n] [["1st" "Male" "Child" "No" 0] ["2nd" "Male" "Child" "No" 0] ["3rd" "Male" "Child" "No" 35] ["Crew" "Male" "Child" "No" 0]
                                    ["1st" "Female" "Child" "No" 0] ["2nd" "Female" "Child" "No" 0] ["3rd" "Female" "Child" "No" 17] ["Crew" "Female" "Child" "No" 0]
                                    ["1st" "Male" "Adult" "No" 118] ["2nd" "Male" "Adult" "No" 154] ["3rd" "Male" "Adult" "No" 387] ["Crew" "Male" "Adult" "No" 670]
                                    ["1st" "Female" "Adult" "No" 4] ["2nd" "Female" "Adult" "No" 13] ["3rd" "Female" "Adult" "No" 89] ["Crew" "Female" "Adult" "No" 3]
                                    ["1st" "Male" "Child" "Yes" 5] ["2nd" "Male" "Child" "Yes" 11] ["3rd" "Male" "Child" "Yes" 13] ["Crew" "Male" "Child" "Yes" 0]
                                    ["1st" "Female" "Child" "Yes" 1] ["2nd" "Female" "Child" "Yes" 13] ["3rd" "Female" "Child" "Yes" 14] ["Crew" "Female" "Child" "Yes" 0]
                                    ["1st" "Male" "Adult" "Yes" 57] ["2nd" "Male" "Adult" "Yes" 14] ["3rd" "Male" "Adult" "Yes" 75] ["Crew" "Male" "Adult" "Yes" 192]
                                    ["1st" "Female" "Adult" "Yes" 140] ["2nd" "Female" "Adult" "Yes" 80] ["3rd" "Female" "Adult" "Yes" 76] ["Crew" "Female" "Adult" "Yes" 20]]
        :when (pos? (long n))]
    {:class class :sex sex :age age :survived survived :n n}))

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
                          :scales {:color {:domain pages}}}))

(def images
  "Images of this demo: `[file-name plot options]`, saved to `results/sankeys/` by [[cljplot2.demo/render!]]."
  [["energy-1000x650.png" energy-flows {:width 1000 :height 650}]
   ["energy-left-1000x650.png" energy-left {:width 1000 :height 650}]
   ["titanic-alluvial-900x600.png" titanic-alluvial {:width 900 :height 600}]
   ["visit-sequences-700x650.png" visit-sequences {:width 700 :height 650}]])
