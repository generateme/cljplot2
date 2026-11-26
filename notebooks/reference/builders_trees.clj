;; # Trees

;; These builders draw a tree or use one: a dendrogram of a clustering or a hierarchy, a node-link tree, a heatmap
;; ordered by clustering, edges bundled along a tree, and sequences as rings. Use them for clusterings and hierarchies.
;; Each returns an ordinary plot map; the Shared options chapter documents the tree options they pass on.

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

;; The table below holds the cars' variables as z-scores, a row vector per car; `car-clustering` clusters them with
;; complete linkage (fastmath `hclust`):

(def standardized (apply mapv vector (map (fn [k] (stats/standardize (map k mtcars))) variables)))

(nu/data-table standardized)

(def car-clustering (cl/hclust standardized {:method :complete}))

(def car-names (mapv :model mtcars))

;; The table below holds a small hierarchy of offices as rows of levels; the dendrograms, trees and edge bundles
;; use it:

(def offices [{:region "Europe" :country "Poland" :city "Warsaw"} {:region "Europe" :country "Poland" :city "Krakow"}
              {:region "Europe" :country "France" :city "Paris"} {:region "Europe" :country "France" :city "Lyon"}
              {:region "Asia" :country "Japan" :city "Tokyo"} {:region "Asia" :country "Japan" :city "Osaka"}
              {:region "Asia" :country "India" :city "Delhi"}])

(nu/data-table offices)

(def office-tree (tree/from-levels offices {:hierarchy [:region :country :city]}))

;; The table below holds the cars' z-scores as long rows, a row per car and variable; the clustered heatmaps use it:

(def car-scores (vec (for [[car row] (map vector car-names standardized) [k v] (map vector variables row)]
                       {:car car :variable (name k) :z v})))

(nu/data-table car-scores)

;; The table below adds each car's cylinders, transmission and horsepower and each variable's kind; the annotated
;; heatmaps use it:

(def annotated-scores (let [car-info (into {} (map (fn [r] [(:model r) r])) mtcars)]
                        (mapv (fn [{:keys [car variable] :as r}]
                                (assoc r :cylinders (str (:cyl (car-info car)))
                                       :transmission (if (zero? (:am (car-info car))) "automatic" "manual")
                                       :horsepower (:hp (car-info car))
                                       :kind (if (#{"mpg" "qsec"} variable) "performance" "engine")))
                              car-scores)))

(nu/data-table annotated-scores)

;; The table below holds calls between the offices' cities; the edge bundles use it:

(def calls [{:from "Warsaw" :to "Paris"} {:from "Warsaw" :to "Tokyo"} {:from "Krakow" :to "Lyon"}
            {:from "Krakow" :to "Delhi"} {:from "Paris" :to "Osaka"} {:from "Lyon" :to "Tokyo"}
            {:from "Tokyo" :to "Delhi"} {:from "Osaka" :to "Warsaw"} {:from "Delhi" :to "Paris"}])

(nu/data-table calls)

;; The table below holds visits to a website as sequences of pages; the sequence sunbursts use it:

(def visits [{:sequence "home-search-product-end" :n 30} {:sequence "home-product-cart-end" :n 25}
             {:sequence "home-search-end" :n 20} {:sequence "home-end" :n 15}
             {:sequence "home-help-search-product-cart-end" :n 10} {:sequence "home-product-end" :n 18}])

(nu/data-table visits)

;; ## Dendrograms

;; `plot/dendrogram` draws a clustering (an `hclust` result) or a tree of levels or parents. It draws the branches at
;; their merge heights and the leaves along one axis. Use it to show how observations join into clusters.

(nu/shared-groups (describe/options #'plot/dendrogram))

;; These options name an `hclust` result's leaves, turn the dendrogram, write leaf and group names and style the
;; branches. The plot below cuts the cars' clustering into 3 clusters:

(nu/option-cards '(plot/dendrogram tree) (describe/own-rows (describe/options #'plot/dendrogram)))

(plot/render-image (plot/dendrogram car-clustering {:leaf-names car-names :k 3 :title "complete linkage, 3 clusters"})
                   {:width 900 :height 420})

;; The left panel below turns the dendrogram horizontal with thicker red branches and no leaf names; the right one
;; draws a hierarchy with its group names at their nodes:

(plot/render-image (plot/arrange [(plot/dendrogram car-clustering {:leaf-names car-names :horizontal true :labels false
                                                                   :line-style {:color [200 60 40] :line-width 2}
                                                                   :title ":horizontal, :labels false, :line-style"})
                                  (plot/dendrogram office-tree {:horizontal false :group-labels true
                                                                :title "a hierarchy, :group-labels true"})]
                                 {:columns 2})
                   {:width 1000 :height 400})

;; The branches draw in the theme style `[:chart :dendrogram-branch]` (a line style the clustered heatmap's trees use too)
;; and the group names in the text role `:dendrogram-group-label`:

(nu/theme-key-list :dendrogram-branch (describe/style-rows :dendrogram-branch [:color :line-width :dash]))

;; The plot below draws thin blue branches through the theme:

(plot/render-image (plot/dendrogram car-clustering {:leaf-names car-names
                                                    :theme {:chart {:dendrogram-branch {:color [40 90 160] :line-width 0.6}}}})
                   {:width 700 :height 380})

;; ## Node-link trees

;; `plot/tree` draws a point per node of a tree, links from parents to children and names beside the nodes. The layout
;; options (Node-link tree layout) make it a tidy tree or a cluster, cartesian or circular. Use it to show a
;; hierarchy's structure.

(nu/shared-groups (describe/options #'plot/tree))

;; These options write node names, draw node points, map node aesthetics and style the nodes and links. The plot below
;; draws the office tree:

(nu/option-cards '(plot/tree tree) (describe/own-rows (describe/options #'plot/tree)))

(plot/render-image (plot/tree office-tree) {:width 600 :height 360})

;; The left panel below colours nodes by depth, enlarges them and dashes the links; the middle one draws only the
;; links of a circular tree; the right one draws a clustering's tree with names:

(plot/render-image (plot/arrange [(plot/tree office-tree {:mapping {:color #(str (:depth %))}
                                                          :node-style {:size 12}
                                                          :link-style {:dash :dashed :color [90 90 90]}
                                                          :title ":mapping, :node-style, :link-style"
                                                          :scales {:color {:legend false}}})
                                  (plot/tree office-tree {:labels false :node-points false :circular true
                                                          :title ":circular true, no names or points"})
                                  (plot/tree car-clustering {:leaf-names car-names :labels true :node-points true
                                                             :title "a clustering, :labels"})]
                                 {:columns 3})
                   {:width 1000 :height 420})

;; A node-link tree draws in theme styles: the nodes in `[:chart :tree-node]` (a scatter style), the links in
;; `[:chart :tree-link]` (a line style) and the names in the text role `:tree-label`:

(nu/theme-key-list :tree-node (describe/style-rows :tree-node))

;; The plot below draws small dark nodes through the theme:

(plot/render-image (plot/tree office-tree {:theme {:chart {:tree-node {:size 5 :color [60 60 60] :stroke-color :none}}}})
                   {:width 600 :height 360})

;; The links' theme style has these keys; the plot below draws darker, thicker links and bold names through the theme:

(nu/theme-key-list :tree-link (describe/style-rows :tree-link [:color :line-width :dash]))

(plot/render-image (plot/tree office-tree {:theme {:chart {:tree-link {:color [90 90 90] :line-width 2}}
                                                   :text {:tree-label {:font-style :bold}}}})
                   {:width 600 :height 360})

;; ## Clustered heatmaps

;; `plot/clustered-heatmap` draws a tile per row and column of long rows, coloured by a value. Hierarchical clustering
;; orders the rows and the columns, and their dendrograms sit beside the tiles. Use it to find groups of similar rows
;; and columns.

(nu/shared-groups (describe/options #'plot/clustered-heatmap))

;; These options name the tile row, column and value, set the clustering, the trees and their size, and add
;; annotation strips. The plot below draws the cars' z-scores:

(nu/option-cards '(plot/clustered-heatmap rows) (describe/own-rows (describe/options #'plot/clustered-heatmap)))

(plot/render-image (plot/clustered-heatmap car-scores {:row :car :col :variable :value :z
                                                       :scales {:color {:range :RColorBrewer/RdBu :midpoint 0
                                                                        :reverse true :title "z-score"}}})
                   {:width 700 :height 500})

;; The next panels use other distances between rows and columns, with average linkage; they give different orders
;; and trees:

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

;; The left panel below draws only the rows' tree, wider, and orders the columns without their tree; the right one
;; uses Ward linkage and draws no trees, only the order:

(plot/render-image (plot/arrange [(plot/clustered-heatmap car-scores {:row :car :col :variable :value :z
                                                                      :col-tree false :tree-size [0.35 0.2]
                                                                      :title ":col-tree false, :tree-size [0.35 0.2]"
                                                                      :scales {:color {:legend false}}})
                                  (plot/clustered-heatmap car-scores {:row :car :col :variable :value :z :method :ward
                                                                      :row-tree false :col-tree false
                                                                      :title ":method :ward, no trees"})]
                                 {:columns 2 :widths [1 1.15]})
                   {:width 1000 :height 450})

;; Annotations, as pheatmap's `annotation_row` and `annotation_col`, draw a strip per annotation along the tile rows
;; or columns, named beside it, and add its legend to the legend column. The rows of one tile row or column must agree
;; on the value. With annotations the legend column spans every row by default, so the value legend stretches over
;; the room the annotations' legends leave. The plot below annotates the cars by cylinders and transmission and the
;; variables by kind:

(plot/render-image (plot/clustered-heatmap annotated-scores
                                           {:row :car :col :variable :value :z
                                            :row-annotations [[:cylinders {:range :set2}] [:transmission {:range ["#999999" "#e08214"]}]]
                                            :col-annotations [[:kind {:range :set1 :title "variable kind"}]]
                                            :annotation-size 14 :legend-span :matrix
                                            :scales {:color {:range :RColorBrewer/RdBu :midpoint 0 :reverse true :title "z-score"}}})
                   {:width 800 :height 600})

;; An annotation is coloured as any colour mapping: categorical values get a palette, numbers a gradient. The plot
;; below annotates horsepower with a colour bar and keeps the legend cell beside the tiles' row only:

(plot/render-image (plot/clustered-heatmap annotated-scores
                                           {:row :car :col :variable :value :z
                                            :row-annotations [[:horsepower {:range :viridis/magma}] [:cylinders nil]]
                                            :legend-span :row
                                            :scales {:color {:range :RColorBrewer/RdBu :midpoint 0 :reverse true :title "z-score"}}})
                   {:width 800 :height 500})

;; ## Edge bundles

;; `plot/bundle` draws edges between the leaves of a tree around a circle, each edge bent along the tree path between
;; its ends. An edge names its leaves by node id in a `tree/from-parents` tree, else by leaf name. Use it for many links
;; between the members of a hierarchy.

(nu/shared-groups (describe/options #'plot/bundle))

;; These options set the edges' opacity and colours and the leaf names. The plot below bundles calls between the
;; office tree's cities:

(nu/option-cards '(plot/bundle edges tree) (describe/own-rows (describe/options #'plot/bundle)))

(plot/render-image (plot/bundle calls office-tree {}) {:width 500 :height 500})

;; The left panel below draws one edge colour, more opaque, without leaf names; the right one colours by group (the
;; default) and fades the edges:

(plot/render-image (plot/arrange [(plot/bundle calls office-tree {:color-by :none :edge-color [120 60 160]
                                                                  :alpha 0.8 :labels false
                                                                  :title ":color-by :none, :alpha 0.8"})
                                  (plot/bundle calls office-tree {:alpha 0.15 :title ":alpha 0.15"})]
                                 {:columns 2 :widths [1 1.3]})
                   {:width 1000 :height 480})

;; An edge bundle draws in theme styles: the edges in `[:chart :bundle-edge]` (a line style) and the leaves in
;; `[:chart :bundle-leaf]` (a scatter style):

(nu/theme-key-list :bundle-edge (describe/style-rows :bundle-edge [:color :line-width :dash]))

;; The plot below draws thicker edges through the theme:

(plot/render-image (plot/bundle calls office-tree {:theme {:chart {:bundle-edge {:line-width 2.5}}}})
                   {:width 500 :height 500})

;; The leaves' theme style has these keys; the plot below draws large square leaves through the theme:

(nu/theme-key-list :bundle-leaf (describe/style-rows :bundle-leaf))

(plot/render-image (plot/bundle calls office-tree {:theme {:chart {:bundle-leaf {:size 8 :shape \s}}}})
                   {:width 500 :height 500})

;; ## Sequence sunbursts

;; `plot/sequences` draws sequences of steps as rings from the centre. A step keeps its name's colour wherever it
;; occurs, and the end of a sequence is a step of its own. Use it to show the paths through a process, e.g. visits to
;; a website.

(nu/shared-groups (describe/options #'plot/sequences))

;; These options set the centre hole, the step names and the end marker. The plot below draws the website visits:

(nu/option-cards '(plot/sequences rows) (describe/own-rows (describe/options #'plot/sequences)))

(plot/render-image (plot/sequences visits {:value :n}) {:width 500 :height 450})

;; The next plot draws a smaller hole, which leaves more room for the rings, and the step names in the cells where
;; they fit; the larger image gives them room:

(plot/render-image (plot/sequences visits {:value :n :inner-radius 0.1 :labels true :title ":inner-radius 0.1, :labels true"})
                   {:width 700 :height 600})

;; With `:keep-end false` the end is not a step of its own, so sequences that end leave their group's grey remainder:

(plot/render-image (plot/sequences visits {:value :n :keep-end false :title ":keep-end false"})
                   {:width 550 :height 450})

;; The cells draw in the theme style `[:chart :sequences-cell]` (a partition style):

(nu/theme-key-list :sequences-cell (describe/style-rows :sequences-cell))

;; The plot below draws thick white cell outlines through the theme:

(plot/render-image (plot/sequences visits {:value :n :theme {:chart {:sequences-cell {:stroke-width 2}}}})
                   {:width 500 :height 450})

