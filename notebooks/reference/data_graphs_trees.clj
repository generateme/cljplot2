;; # Data functions: graphs and trees

;; Functions turning edges, adjacency matrices, parent links, level columns or clusterings into graphs and trees,
;; and laying them out as rows - node positions, edge paths, ring sectors, bands - that ordinary charts draw (the
;; graph and tree builders are made of them). Options are checked against the table shown.

(ns reference.data-graphs-trees
  (:require [fastmath.ml.clustering :as cl]
            [fastmath.random :as r]
            [fastmath.stats :as stats]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.graph :as graph]
            [cljplot2.data.hierarchy :as hierarchy]
            [cljplot2.data.tree :as tree]
            [cljplot2.describe :as describe]
            [notebook-utils :as nu]))

;; Flights between airports (counts), the airports as node rows, and the same flows as a matrix:

(def flights [{:src "WAW" :dst "BER" :n 12} {:src "WAW" :dst "PRG" :n 5} {:src "BER" :dst "PRG" :n 8}
              {:src "PRG" :dst "VIE" :n 6} {:src "VIE" :dst "WAW" :n 4} {:src "BER" :dst "VIE" :n 3}
              {:src "WAW" :dst "VIE" :n 2} {:src "BUD" :dst "VIE" :n 7}])

(nu/data-table flights)

(def airports [{:code "WAW" :city "Warsaw"} {:code "BER" :city "Berlin"} {:code "PRG" :city "Prague"}
               {:code "VIE" :city "Vienna"} {:code "BUD" :city "Budapest"} {:code "KRK" :city "Krakow"}])

(nu/data-table airports)

;; ## graph/graph

;; The graph of edge rows: nodes (the given node rows first, then ids seen only in edges) with their degrees, and
;; edges with numeric weights. Undirected by default; directed graphs split degrees into in and out.

(nu/options-table (describe/options #'graph/graph))

;; The flights as a directed graph with the airports as nodes - Krakow has no flights (degree 0):

(nu/data-table (:nodes (graph/graph flights {:from :src :to :dst :value :n :nodes airports :id :code :directed? true})))

;; Its edges, weights as numbers (an edge without a weight counts 1):

(nu/data-table (:edges (graph/graph flights {:from :src :to :dst :value :n :directed? false})))

;; ## graph/layout

;; Node positions in the unit square: a force-directed layout (each connected component on its own, packed), a
;; circle or random places. Edges get the positions of both ends - segments to draw.

(nu/options-table (describe/options #'graph/layout))

;; The laid-out nodes (`:x`, `:y` added):

(nu/data-table (:nodes (graph/layout (graph/graph flights {:from :src :to :dst :value :n})
                                     {:rng (r/rng :mersenne 3)})))

;; The edges as segments, the nodes as points: the force layout (seeded, the same every build), a weaker pull to
;; the centre after few iterations, a circle and random places:

(plot/render-image (plot/arrange (for [[title laid]
                                       [[":layout :force (default)"
                                         (graph/layout (graph/graph flights {:from :src :to :dst :value :n})
                                                       {:rng (r/rng :mersenne 3)})]
                                        [":iterations 20 :gravity 0.2"
                                         (graph/layout (graph/graph flights {:from :src :to :dst :value :n})
                                                       {:layout :force :iterations 20 :gravity 0.2 :rng (r/rng :mersenne 3)})]
                                        [":layout :circle"
                                         (graph/layout (graph/graph flights {:from :src :to :dst :value :n}) {:layout :circle})]
                                        [":layout :random"
                                         (graph/layout (graph/graph flights {:from :src :to :dst :value :n})
                                                       {:layout :random :rng (r/rng :mersenne 3)})]]]
                                   {:charts [(chart/segment {:data (:edges laid) :mapping {:x :x :y :y :xend :xend :yend :yend}})
                                             (chart/scatter {:data (:nodes laid) :mapping {:x :x :y :y} :size 14})
                                             (chart/text {:data (:nodes laid) :mapping {:x :x :y :y :label :id} :size 8})]
                                    :aspect :iso
                                    :scales {:x {:axis false} :y {:axis false}}
                                    :labels {:title title}})
                                 {:columns 2})
                   {:width 900 :height 640})

;; ## graph/adjacency->edges

;; Edge rows of a square matrix of weights between named nodes; a zero or missing cell is no edge. Undirected, only
;; the upper triangle (and the diagonal) is read.

(nu/options-table (describe/options #'graph/adjacency->edges))

;; A symmetric matrix of shared projects between four teams: read whole (each pair twice, directed) and as an
;; undirected graph (each pair once):

(def shared-projects [[0 3 1 0] [3 0 2 4] [1 2 0 0] [0 4 0 0]])

(nu/data-table (graph/adjacency->edges shared-projects ["design" "backend" "data" "ops"]))

(nu/data-table (graph/adjacency->edges shared-projects ["design" "backend" "data" "ops"] {:directed? false}))

;; ## graph/arc-layout

;; An arc diagram as rows: nodes on a line at 0, 1, ... in node order, each edge a semicircle between its ends
;; (rows sampled along it).

(nu/options-table (describe/options #'graph/arc-layout))

;; The arcs (rows of each edge's semicircle, `:edge` its index):

(nu/data-table (:arcs (graph/arc-layout (graph/graph flights {:from :src :to :dst :value :n}))))

;; Drawn as lines through the arc rows over the nodes: the input order, an order given by ids (those first) and
;; the clustered order (nodes linked to the same others side by side):

(plot/render-image (plot/arrange (for [[title laid]
                                       [["input order" (graph/arc-layout (graph/graph flights {:from :src :to :dst :value :n}))]
                                        [":order [\"VIE\" \"BUD\"]"
                                         (graph/arc-layout (graph/graph flights {:from :src :to :dst :value :n}) {:order ["VIE" "BUD"]})]
                                        [":order :cluster"
                                         (graph/arc-layout (graph/graph flights {:from :src :to :dst :value :n}) {:order :cluster})]]]
                                   {:charts [(chart/line {:data (:arcs laid) :mapping {:x :x :y :y :group :edge :size :value}
                                                          :sort false})
                                             (chart/scatter {:data (:nodes laid) :mapping {:x :x :y :y} :size 10})]
                                    :scales {:x {:ticks (vec (range 5))
                                                 :formatter (let [ids (mapv :id (:nodes laid))] #(get ids (long %) ""))}
                                             :y {:axis false}}
                                    :labels {:title title}
                                    :legend false})
                                 {:columns 3})
                   {:width 1000 :height 260})

;; ## graph/chord-layout

;; A chord diagram as polygon rows in the unit circle: a ring sector per node (its angle by the node's flow) and a
;; ribbon per linked pair (undirected) or edge (directed) through the centre.

(nu/options-table (describe/options #'graph/chord-layout))

;; The ring sectors (`:start` / `:end` in degrees, clockwise from the top):

(nu/data-table (:groups (graph/chord-layout (graph/graph flights {:from :src :to :dst :value :n}))))

;; Ribbons coloured by their source, sectors by node: the default ring, wide gaps and a thin ring, and the directed
;; graph (a ribbon per edge) in clustered order without flows under 4 (their edges are dropped):

(plot/render-image (plot/arrange (for [[title {:keys [ribbons arcs]}]
                                       [["defaults" (graph/chord-layout (graph/graph flights {:from :src :to :dst :value :n}))]
                                        [":pad 12, :inner 0.75"
                                         (graph/chord-layout (graph/graph flights {:from :src :to :dst :value :n})
                                                             {:pad 12 :inner 0.75})]
                                        [":order :cluster :min-value 4"
                                         (graph/chord-layout (graph/graph flights {:from :src :to :dst :value :n :directed? true})
                                                             {:order :cluster :min-value 4})]]]
                                   {:charts [(chart/polygon {:data ribbons :alpha 0.6 :mapping {:x :x :y :y :group :ribbon :color :from}})
                                             (chart/polygon {:data arcs :mapping {:x :x :y :y :color :group}})]
                                    :aspect :iso
                                    :scales {:x {:axis false} :y {:axis false}}
                                    :labels {:title title}
                                    :legend (= title "defaults")})
                                 {:columns 3 :widths [1.3 1 1]})
                   {:width 1000 :height 330})

;; ## graph/sankey-layout

;; A sankey diagram as rows in the unit square: nodes in columns as rectangles, every edge a band as thick as its
;; flow from its source's right side to its target's left side (edges are flows, whatever the graph's direction).

(nu/options-table (describe/options #'graph/sankey-layout))

;; The nodes with their columns (`:layer`) and rectangles (`:x0 :x1 :y0 :y1`):

(nu/data-table (:nodes (graph/sankey-layout (graph/graph [{:from "WAW" :to "BER" :value 12} {:from "WAW" :to "PRG" :value 5}
                                                         {:from "BER" :to "PRG" :value 8} {:from "PRG" :to "VIE" :value 6}
                                                         {:from "BER" :to "VIE" :value 3} {:from "BUD" :to "VIE" :value 7}]
                                                        {:directed? true}))))

;; Bands (polygons grouped by `:link`) under the node rectangles (grouped by `:rect`), for each column alignment -
;; Krakow, a dead end after Warsaw, sits in the last column (`:justify`, `:right`) or right after Warsaw (`:left`,
;; `:center`); Budapest starts in the first column only with `:left` and `:justify`:

(plot/render-image (plot/arrange (for [[title {:keys [rects bands]}]
                                       [[":align :justify (default)"
                                         (graph/sankey-layout (graph/graph [{:from "WAW" :to "BER" :value 12} {:from "WAW" :to "PRG" :value 5}
                                                                            {:from "BER" :to "PRG" :value 8} {:from "PRG" :to "VIE" :value 6}
                                                                            {:from "BUD" :to "PRG" :value 7} {:from "WAW" :to "KRK" :value 4}]))]
                                        [":align :left"
                                         (graph/sankey-layout (graph/graph [{:from "WAW" :to "BER" :value 12} {:from "WAW" :to "PRG" :value 5}
                                                                            {:from "BER" :to "PRG" :value 8} {:from "PRG" :to "VIE" :value 6}
                                                                            {:from "BUD" :to "PRG" :value 7} {:from "WAW" :to "KRK" :value 4}])
                                                              {:align :left})]
                                        [":align :right"
                                         (graph/sankey-layout (graph/graph [{:from "WAW" :to "BER" :value 12} {:from "WAW" :to "PRG" :value 5}
                                                                            {:from "BER" :to "PRG" :value 8} {:from "PRG" :to "VIE" :value 6}
                                                                            {:from "BUD" :to "PRG" :value 7} {:from "WAW" :to "KRK" :value 4}])
                                                              {:align :right})]
                                        [":align :center"
                                         (graph/sankey-layout (graph/graph [{:from "WAW" :to "BER" :value 12} {:from "WAW" :to "PRG" :value 5}
                                                                            {:from "BER" :to "PRG" :value 8} {:from "PRG" :to "VIE" :value 6}
                                                                            {:from "BUD" :to "PRG" :value 7} {:from "WAW" :to "KRK" :value 4}])
                                                              {:align :center})]]]
                                   {:charts [(chart/polygon {:data bands :alpha 0.5 :mapping {:x :x :y :y :group :link :color :from}})
                                             (chart/polygon {:data rects :mapping {:x :x :y :y :group :rect :color :node}})]
                                    :scales {:x {:axis false} :y {:axis false}}
                                    :labels {:title title}
                                    :legend false})
                                 {:columns 4})
                   {:width 1000 :height 280})

;; Columns given by a node column (`:layer :stage`, winning over `:align`), wide nodes (`:node-width 0.08`) far apart
;; (`:pad 0.1`), no relaxation, nodes in input order (left), against the defaults - columns from `:align`, six
;; relaxation sweeps moving the nodes towards their links' ends (right):

(def stages [{:id "visit" :stage 0} {:id "search" :stage 1} {:id "browse" :stage 1} {:id "cart" :stage 2} {:id "buy" :stage 3}])

(nu/data-table stages)

(plot/render-image (plot/arrange (for [[title {:keys [rects bands]}]
                                       [[":layer, :iterations 0, :node-sort :input"
                                         (graph/sankey-layout (graph/graph [{:from "visit" :to "search" :value 6} {:from "visit" :to "browse" :value 9}
                                                                            {:from "search" :to "cart" :value 4} {:from "browse" :to "cart" :value 3}
                                                                            {:from "cart" :to "buy" :value 5} {:from "browse" :to "buy" :value 1}]
                                                                           {:nodes stages})
                                                              {:layer :stage :node-width 0.08 :pad 0.1 :iterations 0 :node-sort :input})]
                                        ["defaults"
                                         (graph/sankey-layout (graph/graph [{:from "visit" :to "search" :value 6} {:from "visit" :to "browse" :value 9}
                                                                            {:from "search" :to "cart" :value 4} {:from "browse" :to "cart" :value 3}
                                                                            {:from "cart" :to "buy" :value 5} {:from "browse" :to "buy" :value 1}]
                                                                           {:nodes stages}))]]]
                                   {:charts [(chart/polygon {:data bands :alpha 0.5 :mapping {:x :x :y :y :group :link :color :from}})
                                             (chart/polygon {:data rects :mapping {:x :x :y :y :group :rect :color :node}})]
                                    :scales {:x {:axis false} :y {:axis false}}
                                    :labels {:title title}
                                    :legend (= title "defaults")})
                                 {:columns 2 :widths [1 1.2]})
                   {:width 1000 :height 320})

;; ## graph/alluvial-graph

;; The directed graph of an alluvial diagram: a node per level of every axis (a column each), an edge per pair of
;; levels of consecutive axes and fill level, with the summed weight - for `sankey-layout` with fixed columns.

(nu/options-table (describe/options #'graph/alluvial-graph))

(def passengers [{:class "1st" :sex "f" :survived "yes" :n 140} {:class "1st" :sex "m" :survived "no" :n 118}
                 {:class "2nd" :sex "f" :survived "yes" :n 80} {:class "2nd" :sex "m" :survived "no" :n 154}
                 {:class "3rd" :sex "f" :survived "no" :n 106} {:class "3rd" :sex "m" :survived "no" :n 422}
                 {:class "3rd" :sex "f" :survived "yes" :n 90} {:class "1st" :sex "m" :survived "yes" :n 62}])

(nu/data-table passengers)

;; The edges, weighted by `:n` and grouped by the survival level:

(nu/data-table (:edges (graph/alluvial-graph passengers {:axes [:class :sex :survived] :value :n :fill :survived})))

;; Laid out by `sankey-layout` with the axes as columns and the input order kept, strands coloured by their fill
;; level - by survival (left) and by class, the default first axis (right):

(plot/render-image (plot/arrange (for [[title {:keys [rects bands]}]
                                       [[":fill :survived"
                                         (graph/sankey-layout (graph/alluvial-graph passengers {:axes [:class :sex :survived] :value :n :fill :survived})
                                                              {:layer :layer :node-sort :input :iterations 0})]
                                        ["default :fill (the first axis)"
                                         (graph/sankey-layout (graph/alluvial-graph passengers {:axes [:class :sex :survived] :value :n})
                                                              {:layer :layer :node-sort :input :iterations 0})]]]
                                   {:charts [(chart/polygon {:data bands :alpha 0.6 :mapping {:x :x :y :y :group :link :color :fill}})
                                             (chart/polygon {:data rects :color [90 90 90] :stroke-color :white
                                                             :mapping {:x :x :y :y :group :rect}})]
                                    :scales {:x {:axis false} :y {:axis false}}
                                    :labels {:title title}})
                                 {:columns 2})
                   {:width 1000 :height 320})

;; ## tree/from-hclust

;; A tree of a fastmath hierarchical clustering (`fastmath.ml.clustering/hclust`): groups with their merge
;; heights, leaves named by the given labels. It takes no options.

(nu/source-note {:dataset :mtcars})

(def mtcars (take 12 (nu/dataset :mtcars)))

(nu/data-table mtcars)

;; The cars standardized on four variables, clustered with complete linkage; the tree's leaves in order:

(def car-features (let [cols (for [k [:mpg :hp :wt :qsec]] (stats/standardize (map k mtcars)))]
                    (apply mapv vector cols)))

(mapv :name (tree/leaves (tree/from-hclust (cl/hclust car-features {:method :complete}) (mapv :model mtcars))))

;; The linkage methods of `hclust` (`:method`): `:single`, `:complete` (default), `:average`, `:weighted`,
;; `:centroid`, `:median`, `:ward`, `:ward-d` and the divisive `:diana`. Their trees of the same cars:

(plot/render-image (plot/arrange (for [method [:single :complete :average :weighted :centroid :median :ward :ward-d :diana]]
                                   (plot/dendrogram (tree/from-hclust (cl/hclust car-features {:method method}) (mapv :model mtcars))
                                                    {:title (str ":method " method)}))
                                 {:columns 3})
                   {:width 1000 :height 800})

;; ## tree/from-levels

;; A tree of rows grouped by level columns, outer level first: a group per level path, a leaf per row.

(nu/options-table (describe/options #'tree/from-levels))

(def offices [{:region "Europe" :country "Poland" :city "Warsaw"} {:region "Europe" :country "Poland" :city "Krakow"}
              {:region "Europe" :country "Czechia" :city "Prague"} {:region "Asia" :country "Japan" :city "Tokyo"}
              {:region "Asia" :country "Japan" :city "Osaka"} {:region "Asia" :country "India" :city "Pune"}])

(nu/data-table offices)

;; Its leaves (rows) in tree order, and the tree drawn by `tree/node-link-layout` below:

(mapv :city (tree/leaves (tree/from-levels offices {:hierarchy [:region :country]})))

;; ## tree/from-parents

;; A tree of rows with ids and parent ids (as d3 `stratify`): the row without a parent is the root, children in row
;; order, names from a column (the id by default).

(nu/options-table (describe/options #'tree/from-parents))

(def org [{:emp 1 :boss nil :who "CEO"} {:emp 2 :boss 1 :who "CTO"} {:emp 3 :boss 1 :who "CFO"}
          {:emp 4 :boss 2 :who "dev lead"} {:emp 5 :boss 2 :who "ops lead"} {:emp 6 :boss 4 :who "developer"}])

(nu/data-table org)

(mapv :name (tree/leaves (tree/from-parents org {:id :emp :parent :boss :name :who})))

;; ## tree/leaves

;; The leaves under a node, in order (it takes no options) - above the leaves of a clustering, of level groups and
;; of parent links; here of one subtree, the CTO's:

(mapv :name (tree/leaves (first (:children (tree/from-parents org {:id :emp :parent :boss :name :who})))))

;; ## tree/layout

;; A dendrogram layout: nodes at their heights, leaves one step apart, an elbow path per branch; optionally cut into
;; coloured clusters or bent around a circle.

(nu/options-table (describe/options #'tree/layout))

;; The elbow paths (rows grouped by `:id`, the branch's node; `:cluster` from the cut):

(nu/data-table (:paths (tree/layout (tree/from-hclust (cl/hclust car-features) (mapv :model mtcars)) {:k 3})))

;; Drawn as lines: plain, cut into 3 clusters, and (also cut) circular - around the whole circle and over
;; `:span 180` degrees:

(plot/render-image (plot/arrange (for [[title {:keys [paths]} iso?]
                                       [["defaults" (tree/layout (tree/from-hclust (cl/hclust car-features) (mapv :model mtcars)) {}) false]
                                        [":k 3" (tree/layout (tree/from-hclust (cl/hclust car-features) (mapv :model mtcars)) {:k 3}) false]
                                        [":circular true" (tree/layout (tree/from-hclust (cl/hclust car-features) (mapv :model mtcars))
                                                                       {:circular true :k 3}) true]
                                        [":span 180"
                                         (tree/layout (tree/from-hclust (cl/hclust car-features) (mapv :model mtcars))
                                                      {:circular true :span 180 :k 3}) true]]]
                                   (cond-> {:charts [(chart/line {:data paths :sort false
                                                                  :mapping (cond-> {:x :x :y :y :group :id}
                                                                             (some :cluster paths) (assoc :color #(some-> (:cluster %) str)))})]
                                            :scales {:x {:axis false} :y {:axis false}}
                                            :labels {:title title}
                                            :legend false}
                                     iso? (assoc :aspect :iso)))
                                 {:columns 2})
                   {:width 900 :height 640})

;; ## tree/node-link-layout

;; A node-link layout (d3 `tree` / `cluster`): nodes at their depth, one link path per child; tidy or with the
;; leaves aligned, curved or straight links, optionally around a circle.

(nu/options-table (describe/options #'tree/node-link-layout))

;; The nodes (`:depth`, `:breadth`, `:x`, `:y`):

(nu/data-table (:nodes (tree/node-link-layout (tree/from-levels offices {:hierarchy [:region :country :city]}) {})))

;; Links as lines under the nodes - tidy with curved links, the leaves aligned with straight links, and circular
;; (tidy, curved) over a full turn and `:span 180` degrees:

(plot/render-image (plot/arrange (for [[title {:keys [nodes links]} iso?]
                                       [[":layout :tidy, :links :diagonal"
                                         (tree/node-link-layout (tree/from-levels offices {:hierarchy [:region :country :city]})
                                                                {:layout :tidy :links :diagonal}) false]
                                        [":layout :cluster, :links :line"
                                         (tree/node-link-layout (tree/from-levels offices {:hierarchy [:region :country :city]})
                                                                {:layout :cluster :links :line}) false]
                                        [":circular true"
                                         (tree/node-link-layout (tree/from-levels offices {:hierarchy [:region :country :city]})
                                                                {:circular true}) true]
                                        [":span 180"
                                         (tree/node-link-layout (tree/from-levels offices {:hierarchy [:region :country :city]})
                                                                {:circular true :span 180}) true]]]
                                   (cond-> {:charts [(chart/line {:data links :sort false :color [150 150 150] :mapping {:x :x :y :y :group :id}})
                                                     (chart/scatter {:data nodes :size 8 :mapping {:x :x :y :y} :padding {:x [0.1 0.25] :y [0.1 0.1]}})
                                                     (chart/text {:data (filter :leaf? nodes) :size 7 :anchor [:left :middle] :offset [5 0]
                                                                  :mapping {:x :x :y :y :label #(str (:name %))}})]
                                            :scales {:x {:axis false} :y {:axis false}}
                                            :labels {:title title}}
                                     iso? (assoc :aspect :iso)))
                                 {:columns 2})
                   {:width 900 :height 640})

;; ## tree/bundle-paths

;; Hierarchical edge bundling: edges between leaves of a circular tree layout, each a B-spline through the tree
;; path between its ends, straightened by `:beta`.

(nu/options-table (describe/options #'tree/bundle-paths))

(def modules [{:id "app"} {:id "ui" :parent "app"} {:id "core" :parent "app"} {:id "io" :parent "app"}
              {:id "button" :parent "ui"} {:id "form" :parent "ui"} {:id "menu" :parent "ui"}
              {:id "model" :parent "core"} {:id "rules" :parent "core"} {:id "cache" :parent "core"}
              {:id "file" :parent "io"} {:id "net" :parent "io"} {:id "db" :parent "io"}])

(nu/data-table modules)

(def imports [{:module "button" :uses "model"} {:module "form" :uses "rules"} {:module "form" :uses "model"}
              {:module "menu" :uses "cache"} {:module "model" :uses "db"} {:module "rules" :uses "file"}
              {:module "cache" :uses "net"} {:module "button" :uses "net"} {:module "menu" :uses "db"}])

(nu/data-table imports)

;; The paths (rows grouped by `:id`, the edge's index):

(nu/data-table (:paths (tree/bundle-paths imports (tree/layout (tree/from-parents modules {}) {:circular true})
                                          {:from :module :to :uses})))

;; Bundled along the tree (`:beta 1`), the default 0.85, and straight lines (`:beta 0`):

(plot/render-image (plot/arrange (for [[title beta] [[":beta 1" 1.0] [":beta 0.85 (default)" 0.85] [":beta 0" 0.0]]]
                                   {:charts [(chart/line {:data (:paths (tree/bundle-paths imports
                                                                                           (tree/layout (tree/from-parents modules {}) {:circular true})
                                                                                           {:from :module :to :uses :beta beta}))
                                                          :sort false :mapping {:x :x :y :y :group :id :color :module}})
                                             (chart/text {:data (filter :leaf? (:nodes (tree/layout (tree/from-parents modules {}) {:circular true})))
                                                          :size 8 :mapping {:x :x :y :y :label :id}})]
                                    :aspect :iso
                                    :scales {:x {:axis false} :y {:axis false}}
                                    :labels {:title title}
                                    :legend false})
                                 {:columns 3})
                   {:width 1000 :height 340})

;; ## hierarchy/sequence-levels

;; Hierarchy rows of sequences (e.g. visited pages) for a flow sunburst: a step column per position, identical
;; sequences summed; a final end marker is removed (or kept as a step).

(nu/options-table (describe/options #'hierarchy/sequence-levels))

(def visits [{:path "home>search>product>end" :n 30} {:path "home>search>end" :n 12}
             {:path "home>product>cart>end" :n 18} {:path "home>search>product>cart" :n 9}
             {:path "search>product>end" :n 7}])

(nu/data-table visits)

;; The step rows - the end marker dropped (left), kept as a step (right):

(nu/data-table (:rows (hierarchy/sequence-levels visits {:sequence :path :value :n :separator ">" :end "end"})))

(nu/data-table (:rows (hierarchy/sequence-levels visits {:sequence :path :value :n :separator ">" :end "end" :keep-end? true})))

;; The rows as a sunburst - `chart/partition` under polar, the steps as its hierarchy:

(plot/render-image (let [{:keys [rows hierarchy]} (hierarchy/sequence-levels visits {:sequence :path :value :n :separator ">"})]
                     {:data rows
                      :mapping {:hierarchy hierarchy :value :value}
                      :charts [(chart/partition {:color-by :name})]
                      :coord {:kind :polar}})
                   {:width 500 :height 500})
