(ns cljplot2.demo.trees
  "Flow cycle 2 demo: trees - the mtcars hierarchical clustering as a dendrogram (3 clusters) and as a circular one,
  the world population hierarchy (regions → subregions) as a horizontal dendrogram, the clustered heatmap of
  standardized mtcars, flare's class imports bundled along its package tree.

  Data: `flare.json`, `flare-dependencies.json` - vega-datasets (the flare ActionScript library's classes and imports;
  licence not documented); world population - data-to-viz `11_SevCatOneNumNestedOneObsPerGroup` (MIT, read from
  `notebooks/data`)."
  (:require [clojure.data.csv :as csv]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [fastmath.ml.clustering :as cl]
            [fastmath.stats :as stats]
            [cljplot2.core :as plot]
            [cljplot2.data.tree :as tree]
            [cljplot2.demo.parallel :refer [mtcars]]))

(def ^:private variables [:mpg :cyl :disp :hp :drat :wt :qsec :vs :am :gear :carb])

(def ^:private standardized
  "mtcars columns as z-scores, one row vector per car."
  (let [moments (into {} (for [k variables :let [vs (map k mtcars)]] [k [(stats/mean vs) (stats/stddev vs)]]))]
    (mapv (fn [car] (mapv (fn [k] (let [[mu sd] (moments k)] (/ (- (double (k car)) mu) sd))) variables)) mtcars)))

(def ^:private cars (mapv :name mtcars))

(def ^:private clustering (cl/hclust standardized {:method :complete}))

(def mtcars-dendrogram
  (plot/dendrogram clustering {:labels cars :k 3 :theme {:palette {:color :set1}}
                                :title "mtcars: complete-linkage clustering, 3 clusters"}))

(def mtcars-circular
  (plot/dendrogram clustering {:labels cars :k 3 :circular true :theme {:palette {:color :set1}}
                              :title "The same clustering, circular"}))

(def ^:private world
  (let [[header & rows] (csv/read-csv (slurp (io/file "notebooks/data/world-population.csv")))
        ks (mapv keyword header)]
    (mapv #(zipmap ks %) rows)))

(def world-hierarchy
  (plot/dendrogram (tree/from-levels world {:hierarchy [:region :subregion]})
                   {:horizontal true :title "World regions and subregions"}))

(def mtcars-heatmap
  (plot/clustered-heatmap (vec (for [[car row] (map vector cars standardized) [k v] (map vector variables row)]
                                 {:car car :variable (name k) :z v}))
                          {:row :car :col :variable :value :z
                           :scales {:color {:range :RColorBrewer/RdBu :midpoint 0 :reverse true :title "z-score"}}
                           :title "mtcars: cars and variables ordered by clustering"}))

(def ^:private read-json #(json/read-str (slurp (io/resource %)) :key-fn keyword))

(def flare-bundle
  (let [nodes (read-json "flare.json")
        imports (read-json "flare-dependencies.json")]
    (plot/bundle imports (tree/from-parents nodes {:name :name})
                 {:from :source :to :target :title "flare: class imports bundled along the package tree"})))

(def images
  "Images of this demo: `[file-name plot options]`, saved to `results/trees/` by [[cljplot2.demo/render!]]."
  [["mtcars-dendrogram-1000x500.png" mtcars-dendrogram {:width 1000 :height 500}]
   ["mtcars-circular-700x700.png" mtcars-circular {:width 700 :height 700}]
   ["world-hierarchy-600x600.png" world-hierarchy {:width 600 :height 600}]
   ["mtcars-heatmap-900x800.png" mtcars-heatmap {:width 900 :height 800}]
   ["flare-bundle-800x800.png" flare-bundle {:width 800 :height 800}]])
