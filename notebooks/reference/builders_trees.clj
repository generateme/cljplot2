;; # Builders: trees

;; Builders drawing a tree or using one: a dendrogram of a clustering or a hierarchy, a node-link tree, a heatmap
;; ordered by clustering, edges bundled along a tree, and sequences as rings (a flow sunburst). Each takes its data
;; and an options map and returns an ordinary plot map; the tree functions it forwards to are shared groups (Shared
;; options), the trees themselves come from the tree data functions.

(ns reference.builders-trees
  (:require [fastmath.ml.clustering :as cl]
            [fastmath.stats :as stats]
            [cljplot2.core :as plot]
            [cljplot2.data.tree :as tree]
            [cljplot2.describe :as describe]
            [notebook-utils :as nu]))

(nu/source-note {:dataset :mtcars})

(def mtcars (take 16 (nu/dataset :mtcars)))

(nu/data-table mtcars)

(def variables [:mpg :disp :hp :drat :wt :qsec])

;; The cars' variables as z-scores, a row vector per car, clustered with complete linkage (fastmath `hclust`):

(def standardized
  (let [moments (into {} (for [k variables :let [vs (map k mtcars)]] [k [(stats/mean vs) (stats/stddev vs)]]))]
    (mapv (fn [car] (mapv (fn [k] (let [[mu sd] (moments k)] (/ (- (double (k car)) (double mu)) (double sd))))
                          variables))
          mtcars)))

(def car-clustering (cl/hclust standardized {:method :complete}))

(def car-names (mapv :model mtcars))

;; A small hierarchy as rows of levels:

(def offices [{:region "Europe" :country "Poland" :city "Warsaw"} {:region "Europe" :country "Poland" :city "Krakow"}
              {:region "Europe" :country "France" :city "Paris"} {:region "Europe" :country "France" :city "Lyon"}
              {:region "Asia" :country "Japan" :city "Tokyo"} {:region "Asia" :country "Japan" :city "Osaka"}
              {:region "Asia" :country "India" :city "Delhi"}])

(nu/data-table offices)

(def office-tree (tree/from-levels offices {:hierarchy [:region :country :city]}))

;; ## dendrogram

;; A dendrogram of a clustering (an `hclust` result, its observations named by `:labels`) or of a tree (levels or
;; parents) - branches at their merge heights, leaves along one axis.

(nu/options-table (describe/own-rows (describe/options #'plot/dendrogram)))

(plot/render-image (plot/dendrogram car-clustering {:labels car-names :k 3 :title "complete linkage, 3 clusters"})
                   {:width 900 :height 420})

;; Horizontal, with thicker red branches (`:line-style`, merged over the default), and without leaf names (left); a
;; hierarchy with its group names at their nodes (right):

(plot/render-image (plot/arrange [(plot/dendrogram car-clustering {:labels car-names :horizontal true :labels? false
                                                                   :line-style {:color [200 60 40] :width 2}
                                                                   :title ":horizontal, :labels? false, :line-style"})
                                  (plot/dendrogram office-tree {:horizontal false :group-labels? true
                                                                :title "a hierarchy, :group-labels? true"})]
                                 {:columns 2})
                   {:width 1000 :height 400})

(nu/shared-groups (describe/options #'plot/dendrogram))

;; ## tree

;; A node-link tree: a point per node, links from parents to children, names beside the nodes; tidy (by depth) or
;; cluster (leaves aligned), cartesian or circular (the Node-link tree layout group).

(nu/options-table (describe/own-rows (describe/options #'plot/tree)))

(plot/render-image (plot/tree office-tree) {:width 600 :height 360})

;; Nodes coloured by depth through `:mapping`, larger nodes (`:node-style`) and dashed links (`:link-style`) (left);
;; circular, neither names nor points - only the links (middle); a clustering's tree named by `:labels` (right):

(plot/render-image (plot/arrange [(plot/tree office-tree {:mapping {:color #(str (:depth %))}
                                                          :node-style {:size 12}
                                                          :link-style {:dash :dashed :color [90 90 90]}
                                                          :title ":mapping, :node-style, :link-style"
                                                          :scales {:color {:legend false}}})
                                  (plot/tree office-tree {:labels? false :nodes? false :circular true
                                                          :title ":circular true, no names or points"})
                                  (plot/tree car-clustering {:labels car-names :labels? true :nodes? true
                                                             :title "a clustering, :labels"})]
                                 {:columns 3})
                   {:width 1000 :height 420})

(nu/shared-groups (describe/options #'plot/tree))

;; ## clustered-heatmap

;; Tiles of a value per row and column, rows and columns ordered by hierarchical clustering, their dendrograms beside
;; the tiles; the data are long rows (`:row`, `:col`, `:value` accessors).

(nu/options-table (describe/own-rows (describe/options #'plot/clustered-heatmap)))

(def car-scores (vec (for [[car row] (map vector car-names standardized) [k v] (map vector variables row)]
                       {:car car :variable (name k) :z v})))

(nu/data-table car-scores)

(plot/render-image (plot/clustered-heatmap car-scores {:row :car :col :variable :value :z
                                                       :scales {:color {:range :RColorBrewer/RdBu :midpoint 0
                                                                        :reverse true :title "z-score"}}})
                   {:width 700 :height 500})

;; The distances between rows and columns (`:distance`, here with average linkage) give different orders and trees:

(plot/render-image (plot/arrange [(plot/clustered-heatmap car-scores {:row :car :col :variable :value :z :method :average
                                                                      :distance :manhattan :title ":distance :manhattan"
                                                                      :scales {:color {:legend false}}})
                                  (plot/clustered-heatmap car-scores {:row :car :col :variable :value :z :method :average
                                                                      :distance :chebyshev :title ":distance :chebyshev"
                                                                      :scales {:color {:legend false}}})
                                  (plot/clustered-heatmap car-scores {:row :car :col :variable :value :z :method :average
                                                                      :distance :canberra :title ":distance :canberra"
                                                                      :scales {:color {:legend false}}})
                                  (plot/clustered-heatmap car-scores {:row :car :col :variable :value :z :method :average
                                                                      :distance :earth-movers
                                                                      :title ":distance :earth-movers"
                                                                      :scales {:color {:legend false}}})]
                                 {:columns 2})
                   {:width 1000 :height 800})

;; Only the rows' tree, wider (`:tree-size`), columns ordered without their tree (left); Ward linkage with the
;; default euclidean distance, no trees - only the order (right):

(plot/render-image (plot/arrange [(plot/clustered-heatmap car-scores {:row :car :col :variable :value :z
                                                                      :col-tree? false :tree-size [0.35 0.2]
                                                                      :title ":col-tree? false, :tree-size [0.35 0.2]"
                                                                      :scales {:color {:legend false}}})
                                  (plot/clustered-heatmap car-scores {:row :car :col :variable :value :z :method :ward
                                                                      :row-tree? false :col-tree? false
                                                                      :title ":method :ward, no trees"})]
                                 {:columns 2 :widths [1 1.15]})
                   {:width 1000 :height 450})

(nu/shared-groups (describe/options #'plot/clustered-heatmap))

;; ## bundle

;; Hierarchical edge bundling: edges between the leaves of a tree, each bent along the tree path between its ends,
;; leaves around a circle. Edges name their leaves by key - a `tree/from-parents` node's id, else the leaf's name
;; (`tree/from-levels`, `tree/from-hclust`): here the office tree from levels, edges between cities.

(nu/options-table (describe/own-rows (describe/options #'plot/bundle)))

(def calls [{:from "Warsaw" :to "Paris"} {:from "Warsaw" :to "Tokyo"} {:from "Krakow" :to "Lyon"}
            {:from "Krakow" :to "Delhi"} {:from "Paris" :to "Osaka"} {:from "Lyon" :to "Tokyo"}
            {:from "Tokyo" :to "Delhi"} {:from "Osaka" :to "Warsaw"} {:from "Delhi" :to "Paris"}])

(nu/data-table calls)

(plot/render-image (plot/bundle calls office-tree {}) {:width 500 :height 500})

;; One edge colour, more opaque, without leaf names (left); coloured by group with the default translucency and
;; names (right):

(plot/render-image (plot/arrange [(plot/bundle calls office-tree {:color-by-group? false :edge-color [120 60 160]
                                                                  :alpha 0.8 :labels? false
                                                                  :title ":color-by-group? false, :alpha 0.8"})
                                  (plot/bundle calls office-tree {:color-by-group? true :alpha 0.4 :labels? true
                                                                  :title ":color-by-group? true"})]
                                 {:columns 2 :widths [1 1.3]})
                   {:width 1000 :height 480})

(nu/shared-groups (describe/options #'plot/bundle))

;; ## sequences

;; A flow sunburst: sequences of steps (a string of steps with a separator) as rings from the centre, every step
;; coloured by its name wherever it occurs, the end of a sequence a step of its own.

(nu/options-table (describe/own-rows (describe/options #'plot/sequences)))

(def visits [{:sequence "home-search-product-end" :n 30} {:sequence "home-product-cart-end" :n 25}
             {:sequence "home-search-end" :n 20} {:sequence "home-end" :n 15}
             {:sequence "home-help-search-product-cart-end" :n 10} {:sequence "home-product-end" :n 18}])

(nu/data-table visits)

(plot/render-image (plot/sequences visits {:value :n}) {:width 500 :height 450})

;; A smaller hole - more room for the rings - and the step names in the cells where they fit (a larger image):

(plot/render-image (plot/sequences visits {:value :n :inner 0.1 :labels? true :title ":inner 0.1, :labels? true"})
                   {:width 700 :height 600})

;; Ending sequences left as their group's grey remainder, the end not a step of its own:

(plot/render-image (plot/sequences visits {:value :n :keep-end? false :title ":keep-end? false"})
                   {:width 550 :height 450})

(nu/shared-groups (describe/options #'plot/sequences))
