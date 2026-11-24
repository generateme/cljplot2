(ns cljplot2.demo.lattice-marks
  "Lattice cycle 2 demo (marks): tile cell sizes from `:size` (LMDVR Fig 6.14), contour level labels (Fig 6.10), Tufte
  quartile plots (Fig 13.4) and sploms arranged in a grid (Fig 5.16) - on the datasets of `notebooks/data` and a
  function field."
  (:require [fastmath.core :as m]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.demo.data :as data]))

(def ^:private chem97 (data/chem97-rows))

(def fig-6-14
  "Fig 6.14: Chem97 counts of A-level score by rounded GCSE score per gender - colour and cell size by the square root
  of the count (cells from 70 % to 100 %), colour bar labelled in counts."
  (let [counts (frequencies (map (juxt :score #(m/round (double (:gcsescore %))) :gender) chem97))]
    {:data (for [[[score gcd gender] n] counts] {:score score :gcd gcd :gender gender :root (m/sqrt n)})
     :mapping {:x :score :y :gcd :color :root :size :root :facet :gender}
     :charts [(chart/tile)]
     :scales {:size {:range [7 10] :legend false}
              :color {:formatter #(str (m/round (* (double %) (double %)))) :title "count"}}
     :facets {:columns 2}
     :labels {:x "Average A-level score" :y "Rounded GCSE score"}}))

(defn- ozone
  "Synthetic smooth surface over wind and temperature (a stand-in for the fitted ozone of the environmental data)."
  [[wind temperature]]
  (let [wind (double wind) temperature (double temperature)]
    (* 4.0 (m/exp (- (* 0.045 (- temperature 60.0)) (* 0.08 wind))))))

(def fig-6-10
  "Fig 6.10: contour lines of a smooth surface, each labelled with its level."
  {:charts [(chart/contour {:f ozone :domain {:x [2 21] :y [57 97]} :levels 15 :labels true :color [40 40 40]})]
   :labels {:x "Wind speed (mph)" :y "Temperature (F)"}})

(def fig-13-4
  "Fig 13.4: Tufte quartile plots of transformed GCSE scores by gender, a panel per A-level score."
  {:data (mapv #(assoc % :transformed (m/pow (double (:gcsescore %)) 2.34)) chem97)
   :mapping {:x :gender :y :transformed :facet :score}
   :charts [(chart/box {:variant :tufte :outliers false})]
   :facets {:columns 6}
   :labels {:x nil :y "Transformed GCSE score"}})

(def ^:private iris
  (mapv #(-> % (update :sepal-length parse-double) (update :sepal-width parse-double) (update :petal-length parse-double))
        (data/csv-rows "iris.csv")))

(def fig-5-16
  "Fig 5.16 (iris species instead of US regions): a scatter-plot matrix per group, arranged in a grid."
  (let [variables [:sepal-length :sepal-width :petal-length]]
    (plot/arrange (conj (mapv (fn [species] (plot/pairs (filterv #(= species (:species %)) iris)
                                                        {:variables variables :title species}))
                              ["setosa" "versicolor" "virginica"])
                        (plot/pairs iris {:variables variables :title "all"}))
                  {:columns 2})))

(def images
  "Images of this demo: `[file-name plot options]`, saved to `results/lattice-marks/` by [[cljplot2.demo/render!]]."
  [["fig-6-14-tile-size-900x450.png" fig-6-14 {:width 900 :height 450}]
   ["fig-6-10-contour-labels-600x500.png" fig-6-10 {:width 600 :height 500}]
   ["fig-13-4-tufte-900x400.png" fig-13-4 {:width 900 :height 400}]
   ["fig-5-16-nested-arrange-900x800.png" fig-5-16 {:width 900 :height 800}]])
