(ns cljplot2.demo.parallel
  "Ranking cycle 4 demo: parallel coordinates (iris by species, a highlighted species, scalings side by side, a
  vertical layout, smooth connections) and radar charts (mtcars rows with fills and limits, groups, smooth, variables
  as rings). Data: `iris.csv` and `mtcars.csv` bundled with fastmath."
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]))

(defn- read-csv
  "Rows of a CSV classpath resource: maps of keyword columns, numeric cells as doubles."
  [resource]
  (let [[header & rows] (csv/read-csv (slurp (io/resource resource)))
        ks (map #(if (= "" %) :name (keyword %)) header)]
    (mapv (fn [row] (zipmap ks (map #(or (parse-double %) %) row))) rows)))

(def iris (read-csv "iris.csv"))
(def mtcars (read-csv "mtcars.csv"))

(def iris-variables [:sepal_length :sepal_width :petal_length :petal_width])

(def by-species
  {:data iris :mapping {:color :species}
   :charts [(chart/parallel {:variables iris-variables})]
   :labels {:title "Iris - parallel coordinates (:uniminmax)" :x nil}})

(def highlight
  {:data iris :mapping {:color :species}
   :scales {:color {:range {"setosa" [180 180 180 90] "versicolor" [180 180 180 90] "virginica" [215 48 39 200]}}}
   :charts [(chart/parallel {:variables [["Sepal length" :sepal_length] ["Sepal width" :sepal_width]
                                         ["Petal length" :petal_length] ["Petal width" :petal_width]]})]
   :labels {:title "Highlight virginica (level-map colours)" :x nil}})

(def scalings
  (plot/arrange (vec (for [scaling [:std :robust :uniminmax :globalminmax]]
                       {:data iris :mapping {:color :species}
                        :charts [(chart/parallel {:variables iris-variables :scaling scaling :width 0.5})]
                        :legend false
                        :labels {:title (str ":scaling " scaling) :x nil}}))
                {:columns 2}))

(def vertical
  {:data iris :mapping {:color :species} :flip true
   :charts [(chart/parallel {:variables iris-variables :dash :none})]
   :labels {:title "Vertical layout (:flip)" :x nil}})

(def car-variables [:mpg :hp :wt :qsec :drat :disp])

(def cars (filterv #(#{"Mazda RX4" "Merc 240D" "Lotus Europa" "Cadillac Fleetwood"} (:name %)) mtcars))

(def radars
  (plot/arrange [{:data cars :mapping {:color :name} :coord {:kind :polar}
                  :charts [(chart/parallel {:variables car-variables :fill :color :width 2})]
                  :labels {:title "Radar - four cars, data ranges" :x nil}}
                 {:data cars :mapping {:color :name} :coord {:kind :polar}
                  :charts [(chart/parallel {:variables car-variables :fill :color :width 2
                                            :limits {:mpg [10 35] :hp [50 350] :wt [1 6] :qsec [14 23]
                                                     :drat [2.5 5] :disp [50 500]}})]
                  :labels {:title "Fixed :limits" :x nil}}]
                {:columns 2}))

(def radar-groups
  (plot/arrange [{:data mtcars :mapping {:color :cyl} :coord {:kind :polar}
                  :scales {:color {:type :categorical}}
                  :charts [(chart/parallel {:variables car-variables :scaling :std})]
                  :labels {:title "All cars by cylinders (:std)" :x nil}}
                 {:data cars :mapping {:color :name} :coord {:kind :polar :arcs true}
                  :charts [(chart/parallel {:variables car-variables :fill :color :pattern :hatch :alpha 0.15})]
                  :labels {:title ":arcs, hatched fills" :x nil}}]
                {:columns 2}))

(def smooth
  (plot/arrange (vec (for [[title opts] [["Straight (default)" {}]
                                         [":curve :smooth" {:curve :smooth}]
                                         [":smoothness 0.4" {:curve :smooth :smoothness 0.4}]]]
                       {:data (take-nth 3 iris) :mapping {:color :species} :legend false
                        :charts [(chart/parallel (merge {:variables iris-variables} opts))]
                        :labels {:title title :x nil}}))
                {:columns 3}))

(def radar-variants
  (plot/arrange [{:data cars :mapping {:color :name} :coord {:kind :polar} :legend false
                  :charts [(chart/parallel {:variables car-variables :fill :color :curve :smooth :width 2})]
                  :labels {:title "Smooth radar" :x nil}}
                 {:data cars :mapping {:color :name} :coord {:kind :polar :inner 0.3} :flip true :legend false
                  :charts [(chart/parallel {:variables [:mpg :hp :wt] :width 2})]
                  :labels {:title "Variables as rings (:flip)" :x nil}}]
                {:columns 2}))

(def images
  "Images of this demo: `[file-name plot options]`, saved to `results/parallel/` by [[cljplot2.demo/render!]]."
  [["iris-700x400.png" by-species {:width 700 :height 400}]
   ["highlight-700x400.png" highlight {:width 700 :height 400}]
   ["scalings-900x600.png" scalings {:width 900 :height 600}]
   ["vertical-600x500.png" vertical {:width 600 :height 500}]
   ["radars-900x450.png" radars {:width 900 :height 450}]
   ["radar-groups-900x450.png" radar-groups {:width 900 :height 450}]
   ["smooth-1000x380.png" smooth {:width 1000 :height 380}]
   ["radar-variants-900x450.png" radar-variants {:width 900 :height 450}]])
