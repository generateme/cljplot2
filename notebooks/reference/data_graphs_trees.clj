;; # Graphs and trees

;; These functions build graphs and trees from edges, adjacency matrices, parent links, level columns or clusterings,
;; and lay them out as rows: node positions, edge paths, ring sectors and bands. Use them to draw graphs and trees
;; with ordinary charts; the graph and tree builders call them.

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

;; The tables below hold made-up flight counts between airports, and the airports as node rows; the graph sections
;; use them:

(def flights [{:src "WAW" :dst "BER" :n 12} {:src "WAW" :dst "PRG" :n 5} {:src "BER" :dst "PRG" :n 8}
              {:src "PRG" :dst "VIE" :n 6} {:src "VIE" :dst "WAW" :n 4} {:src "BER" :dst "VIE" :n 3}
              {:src "WAW" :dst "VIE" :n 2} {:src "BUD" :dst "VIE" :n 7}])

(nu/data-table flights)

(def airports [{:code "WAW" :city "Warsaw"} {:code "BER" :city "Berlin"} {:code "PRG" :city "Prague"}
               {:code "VIE" :city "Vienna"} {:code "BUD" :city "Budapest"} {:code "KRK" :city "Krakow"}])

(nu/data-table airports)

;; The matrix below holds made-up counts of projects four teams share; the adjacency section uses it:

(def shared-projects [[0 3 1 0] [3 0 2 4] [1 2 0 0] [0 4 0 0]])

;; The table below holds made-up stages of a web shop with their columns; the sankey section uses it:

(def stages [{:id "visit" :stage 0} {:id "search" :stage 1} {:id "browse" :stage 1} {:id "cart" :stage 2} {:id "buy" :stage 3}])

(nu/data-table stages)

;; The table below holds made-up passenger counts by class, sex and survival; the alluvial section uses it:

(def passengers [{:class "1st" :sex "f" :survived "yes" :n 140} {:class "1st" :sex "m" :survived "no" :n 118}
                 {:class "2nd" :sex "f" :survived "yes" :n 80} {:class "2nd" :sex "m" :survived "no" :n 154}
                 {:class "3rd" :sex "f" :survived "no" :n 106} {:class "3rd" :sex "m" :survived "no" :n 422}
                 {:class "3rd" :sex "f" :survived "yes" :n 90} {:class "1st" :sex "m" :survived "yes" :n 62}])

(nu/data-table passengers)

;; The table below holds twelve mtcars cars; the clustering sections use it:

(nu/source-note {:dataset :mtcars})

(def mtcars (take 12 (nu/dataset :mtcars)))

(nu/data-table mtcars)

;; The vectors defined below hold the twelve cars standardized on four variables; the clustering sections use them:

(def car-features (let [cols (for [k [:mpg :hp :wt :qsec]] (stats/standardize (map k mtcars)))]
                    (apply mapv vector cols)))

;; The table below holds made-up offices by region and country; the level and node-link sections use it:

(def offices [{:region "Europe" :country "Poland" :city "Warsaw"} {:region "Europe" :country "Poland" :city "Krakow"}
              {:region "Europe" :country "Czechia" :city "Prague"} {:region "Asia" :country "Japan" :city "Tokyo"}
              {:region "Asia" :country "Japan" :city "Osaka"} {:region "Asia" :country "India" :city "Pune"}])

(nu/data-table offices)

;; The table below holds a made-up company's employees with their bosses; the parent sections use it:

(def org [{:emp 1 :boss nil :who "CEO"} {:emp 2 :boss 1 :who "CTO"} {:emp 3 :boss 1 :who "CFO"}
          {:emp 4 :boss 2 :who "dev lead"} {:emp 5 :boss 2 :who "ops lead"} {:emp 6 :boss 4 :who "developer"}])

(nu/data-table org)

;; The tables below hold a made-up program's modules with their parents, and the modules' imports; the bundling
;; section uses them:

(def modules [{:id "app"} {:id "ui" :parent "app"} {:id "core" :parent "app"} {:id "io" :parent "app"}
              {:id "button" :parent "ui"} {:id "form" :parent "ui"} {:id "menu" :parent "ui"}
              {:id "model" :parent "core"} {:id "rules" :parent "core"} {:id "cache" :parent "core"}
              {:id "file" :parent "io"} {:id "net" :parent "io"} {:id "db" :parent "io"}])

(nu/data-table modules)

(def imports [{:module "button" :uses "model"} {:module "form" :uses "rules"} {:module "form" :uses "model"}
              {:module "menu" :uses "cache"} {:module "model" :uses "db"} {:module "rules" :uses "file"}
              {:module "cache" :uses "net"} {:module "button" :uses "net"} {:module "menu" :uses "db"}])

(nu/data-table imports)

;; The table below holds made-up visits to a web shop, as page sequences with counts; the sequence section uses it:

(def visits [{:path "home>search>product>end" :n 30} {:path "home>search>end" :n 12}
             {:path "home>product>cart>end" :n 18} {:path "home>search>product>cart" :n 9}
             {:path "search>product>end" :n 7}])

(nu/data-table visits)

;; ## Graphs

;; `graph/graph` builds the graph of edge rows: the nodes with their degrees, the given node rows first, and the edges
;; with numeric weights. Use it as the input of the graph layouts.

(nu/argument-list #'graph/graph)

;; These options name the edge and node columns and set the direction. The table below holds the nodes of the flights
;; as a directed graph with the airports as node rows; Krakow has no flights, so its degree is 0:

(nu/option-cards '(graph/graph edges) (describe/options #'graph/graph))

(nu/result-table (:nodes (graph/graph flights {:from :src :to :dst :value :n :nodes airports :id :code :directed true})))

;; The next table holds the edges of the flights as an undirected graph, the weights from `:n` under `:value`:

(nu/result-table (:edges (graph/graph flights {:from :src :to :dst :value :n :directed false})))

;; Without a `:value` column every edge weighs 1:

(nu/result-table (:edges (graph/graph flights {:from :src :to :dst})))

;; ## Network layouts

;; `graph/layout` places the nodes of a graph in the unit square and gives every edge the positions of both ends. Use
;; it to draw a network: segments for the edges, points for the nodes.

(nu/argument-list #'graph/layout)

;; These options choose the placement and tune the force layout. The table below holds the placed nodes, `:x` and `:y`
;; added:

(nu/option-cards '(graph/layout g) (describe/options #'graph/layout))

(nu/result-table (:nodes (graph/layout (graph/graph flights {:from :src :to :dst :value :n})
                                       {:rng (r/rng :mersenne 3)})))

;; The panels below draw the edges as segments and the nodes as points. The first one takes the seeded force layout,
;; the same on every build; the second one stops after 20 iterations with a weaker pull to the centre; the last two
;; place the nodes on a circle and at random:

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

;; ## Adjacency matrices

;; `graph/adjacency->edges` turns a square matrix of weights between named nodes into edge rows; a zero or missing
;; cell is no edge. Use it for graphs stored as matrices.

(nu/argument-list #'graph/adjacency->edges)

;; This option chooses which cells the function reads. The first table below reads the shared projects whole, each pair twice; the
;; second one reads only the upper triangle, each pair once:

(nu/option-cards '(graph/adjacency->edges matrix names) (describe/options #'graph/adjacency->edges))

(nu/result-table (graph/adjacency->edges shared-projects ["design" "backend" "data" "ops"]))

(nu/result-table (graph/adjacency->edges shared-projects ["design" "backend" "data" "ops"] {:whole-matrix false}))

;; ## Arc layouts

;; `graph/arc-layout` lays out an arc diagram: the nodes on a line at 0, 1, ... in node order, and every edge as rows
;; along a semicircle between its ends. Use it to draw an arc diagram of your own.

(nu/argument-list #'graph/arc-layout)

;; This option sets the node order. The table below holds the arcs: rows along each edge's semicircle, with the edge's
;; index under `:edge`:

(nu/option-cards '(graph/arc-layout g) (describe/options #'graph/arc-layout))

(nu/result-table (:arcs (graph/arc-layout (graph/graph flights {:from :src :to :dst :value :n}))))

;; The panels below draw lines through the arc rows over the nodes. The left one keeps the input order, the middle one
;; puts the given ids first, and the right one puts nodes linked to the same others side by side:

(plot/render-image (plot/arrange (for [[title laid]
                                       [["input order" (graph/arc-layout (graph/graph flights {:from :src :to :dst :value :n}))]
                                        [":order [\"VIE\" \"BUD\"]"
                                         (graph/arc-layout (graph/graph flights {:from :src :to :dst :value :n}) {:order ["VIE" "BUD"]})]
                                        [":order :cluster"
                                         (graph/arc-layout (graph/graph flights {:from :src :to :dst :value :n}) {:order :cluster})]]]
                                   {:charts [(chart/line {:data (:arcs laid) :mapping {:x :x :y :y :group :edge :size :value}
                                                          :order :data})
                                             (chart/scatter {:data (:nodes laid) :mapping {:x :x :y :y} :size 10})]
                                    :scales {:x {:ticks (vec (range 5))
                                                 :formatter (let [ids (mapv :id (:nodes laid))] #(get ids (long %) ""))}
                                             :y {:axis false}}
                                    :labels {:title title}
                                    :legend false})
                                 {:columns 3})
                   {:width 1000 :height 260})

;; ## Chord layouts

;; `graph/chord-layout` lays out a chord diagram as polygon rows in the unit circle: a ring sector per node, its angle
;; by the node's flow, and a ribbon through the centre per linked pair of an undirected graph or per edge of a directed
;; one. Use it to draw a chord diagram of your own.

(nu/argument-list #'graph/chord-layout)

;; These options set the node order, the ring and the smallest flow. The table below holds the ring sectors, `:start`
;; and `:end` in degrees clockwise from the top:

(nu/option-cards '(graph/chord-layout g) (describe/options #'graph/chord-layout))

(nu/result-table (:groups (graph/chord-layout (graph/graph flights {:from :src :to :dst :value :n}))))

;; The panels below colour the ribbons by their `:from` end and the sectors by node; an undirected ribbon's `:from` is
;; its end earlier in the ring. The left one draws the default ring, the middle one wide gaps and a thin ring. The
;; right one draws the directed graph, a ribbon per edge coloured by its source, in clustered order and leaves out the
;; flows under 4:

(plot/render-image (plot/arrange (for [[title {:keys [ribbons arcs]}]
                                       [["defaults" (graph/chord-layout (graph/graph flights {:from :src :to :dst :value :n}))]
                                        [":gap-angle 12, :inner-radius 0.75"
                                         (graph/chord-layout (graph/graph flights {:from :src :to :dst :value :n})
                                                             {:gap-angle 12 :inner-radius 0.75})]
                                        [":order :cluster :min-value 4"
                                         (graph/chord-layout (graph/graph flights {:from :src :to :dst :value :n :directed true})
                                                             {:order :cluster :min-value 4})]]]
                                   {:charts [(chart/polygon {:data ribbons :alpha 0.6 :mapping {:x :x :y :y :group :ribbon :color :from}})
                                             (chart/polygon {:data arcs :mapping {:x :x :y :y :color :group}})]
                                    :aspect :iso
                                    :scales {:x {:axis false} :y {:axis false}}
                                    :labels {:title title}
                                    :legend (= title "defaults")})
                                 {:columns 3 :widths [1.3 1 1]})
                   {:width 1000 :height 330})

;; ## Sankey layouts

;; `graph/sankey-layout` lays out a sankey diagram as rows in the unit square: the nodes as rectangles in columns, every
;; edge a band as thick as its flow from its source's right side to its target's left side. Use it to draw a sankey
;; diagram of your own.

(nu/argument-list #'graph/sankey-layout)

;; These options set the columns, the node size and gaps, and the relaxation. The table below holds the nodes with
;; their columns (`:layer`) and rectangles (`:x0 :x1 :y0 :y1`):

(nu/option-cards '(graph/sankey-layout g) (describe/options #'graph/sankey-layout))

(nu/result-table (:nodes (graph/sankey-layout (graph/graph [{:from "WAW" :to "BER" :value 12} {:from "WAW" :to "PRG" :value 5}
                                                           {:from "BER" :to "PRG" :value 8} {:from "PRG" :to "VIE" :value 6}
                                                           {:from "BER" :to "VIE" :value 3} {:from "BUD" :to "VIE" :value 7}]
                                                          {:directed true}))))

;; For each column alignment, the panels below draw the bands as polygons grouped by `:link`, and over them the node
;; rectangles grouped by `:rect`. Krakow, a dead end after Warsaw, sits in the last column with `:justify` and `:right`, and right
;; after Warsaw with `:left` and `:center`. Budapest starts in the first column only with `:left` and `:justify`:

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

;; The left panel below takes the columns from the stages' node column (`:layer :stage`) and draws wide nodes far
;; apart, without relaxation, in input order. The right one keeps the defaults: columns from
;; `:align` and six relaxation sweeps:

(plot/render-image (plot/arrange (for [[title {:keys [rects bands]}]
                                       [[":layer, :iterations 0, :order :input"
                                         (graph/sankey-layout (graph/graph [{:from "visit" :to "search" :value 6} {:from "visit" :to "browse" :value 9}
                                                                            {:from "search" :to "cart" :value 4} {:from "browse" :to "cart" :value 3}
                                                                            {:from "cart" :to "buy" :value 5} {:from "browse" :to "buy" :value 1}]
                                                                           {:nodes stages})
                                                              {:layer :stage :node-width 0.08 :gap 0.1 :iterations 0 :order :input})]
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

;; ## Alluvial graphs

;; `graph/alluvial-graph` builds the directed graph of an alluvial diagram: a node per level of every variable, a
;; column per variable, and, for each strand level, an edge per pair of levels of neighbouring variables, weighted by
;; the summed weight. Use it with `sankey-layout` and fixed columns.

(nu/argument-list #'graph/alluvial-graph)

;; These options name the variables, the weight and the strand level. The table below holds the edges of the
;; passengers, weighted by `:n` and grouped by survival:

(nu/option-cards '(graph/alluvial-graph rows) (describe/options #'graph/alluvial-graph))

(nu/result-table (:edges (graph/alluvial-graph passengers {:variables [:class :sex :survived] :value :n :strand-color :survived})))

;; The panels below lay the graphs out with `sankey-layout`, the variables as columns in input order, and colour the
;; strands by their strand level: by survival on the left and by class (the first variable) on the right:

(plot/render-image (plot/arrange (for [[title {:keys [rects bands]}]
                                       [[":strand-color :survived"
                                         (graph/sankey-layout (graph/alluvial-graph passengers {:variables [:class :sex :survived] :value :n :strand-color :survived})
                                                              {:layer :layer :order :input :iterations 0})]
                                        ["default :strand-color (the first column)"
                                         (graph/sankey-layout (graph/alluvial-graph passengers {:variables [:class :sex :survived] :value :n})
                                                              {:layer :layer :order :input :iterations 0})]]]
                                   {:charts [(chart/polygon {:data bands :alpha 0.6 :mapping {:x :x :y :y :group :link :color :strand-color}})
                                             (chart/polygon {:data rects :color [90 90 90] :stroke-color :white
                                                             :mapping {:x :x :y :y :group :rect}})]
                                    :scales {:x {:axis false} :y {:axis false}}
                                    :labels {:title title}})
                                 {:columns 2})
                   {:width 1000 :height 320})

;; ## Trees from clusterings

;; `tree/from-hclust` builds the tree of a fastmath hierarchical clustering (`fastmath.ml.clustering/hclust`): groups
;; with their merge heights, leaves named by the given labels. Use it to draw a dendrogram of a clustering.

(nu/argument-list #'tree/from-hclust)

;; The call below clusters the cars with complete linkage and returns the tree's leaves in order:

(mapv :name (tree/leaves (tree/from-hclust (cl/hclust car-features {:method :complete}) (mapv :model mtcars))))

;; The panels below draw the trees of every linkage method of `hclust` (`:method`) on the same cars: `:single`,
;; `:complete` (the default), `:average`, `:weighted`, `:centroid`, `:median`, `:ward`, `:ward-d` and the divisive
;; `:diana`:

(plot/render-image (plot/arrange (for [method [:single :complete :average :weighted :centroid :median :ward :ward-d :diana]]
                                   (plot/dendrogram (tree/from-hclust (cl/hclust car-features {:method method}) (mapv :model mtcars))
                                                    {:title (str ":method " method)}))
                                 {:columns 3})
                   {:width 1000 :height 800})

;; ## Trees from level columns

;; `tree/from-levels` builds the tree of rows grouped by level columns: a group per level path, a leaf per innermost
;; path with its number of rows as `:value`. Use it for hierarchies stored as columns.

(nu/argument-list #'tree/from-levels)

;; This option names the level columns. The call below returns the leaves of the offices, the countries with their
;; numbers of offices, in tree order; the node-link section draws the tree with the cities as a third level:

(nu/option-cards '(tree/from-levels rows) (describe/options #'tree/from-levels))

(mapv (juxt :name :value) (tree/leaves (tree/from-levels offices {:hierarchy [:region :country]})))

;; ## Trees from parent links

;; `tree/from-parents` builds the tree of rows with ids and parent ids, as d3 `stratify`: the row without a parent is
;; the root, and children keep the row order. Use it for hierarchies stored as parent links.

(nu/argument-list #'tree/from-parents)

;; These options name the id, parent and name columns. The call below returns the leaves of the company:

(nu/option-cards '(tree/from-parents rows) (describe/options #'tree/from-parents))

(mapv :name (tree/leaves (tree/from-parents org {:id :emp :parent :boss :name :who})))

;; ## Leaves

;; `tree/leaves` returns the leaves under a node in order; the sections above call it on whole trees. Use it to list a
;; subtree's leaves.

(nu/argument-list #'tree/leaves)

;; The call below returns the leaves under the CTO:

(mapv :name (tree/leaves (first (:children (tree/from-parents org {:id :emp :parent :boss :name :who})))))

;; ## Dendrogram layouts

;; `tree/layout` lays out a tree as a dendrogram: the nodes at their heights, the leaves one step apart, an elbow path
;; per branch. Use it to draw a dendrogram of your own, cut into clusters or bent around a circle.

(nu/argument-list #'tree/layout)

;; These options cut the clusters and bend the layout. The table below holds the elbow paths, rows grouped by the
;; branch's node `:id`, with `:cluster` from a cut into 3:

(nu/option-cards '(tree/layout root) (describe/options #'tree/layout))

(nu/result-table (:paths (tree/layout (tree/from-hclust (cl/hclust car-features) (mapv :model mtcars)) {:k 3})))

;; The panels below draw the paths as lines: plain, cut into 3 clusters, and circular with the cut, around the whole
;; circle and over 180 degrees:

(plot/render-image (plot/arrange (for [[title {:keys [paths]} iso?]
                                       [["defaults" (tree/layout (tree/from-hclust (cl/hclust car-features) (mapv :model mtcars)) {}) false]
                                        [":k 3" (tree/layout (tree/from-hclust (cl/hclust car-features) (mapv :model mtcars)) {:k 3}) false]
                                        [":circular true" (tree/layout (tree/from-hclust (cl/hclust car-features) (mapv :model mtcars))
                                                                       {:circular true :k 3}) true]
                                        [":span 180"
                                         (tree/layout (tree/from-hclust (cl/hclust car-features) (mapv :model mtcars))
                                                      {:circular true :span 180 :k 3}) true]]]
                                   (cond-> {:charts [(chart/line {:data paths :order :data
                                                                  :mapping (cond-> {:x :x :y :y :group :id}
                                                                             (some :cluster paths) (assoc :color #(some-> (:cluster %) str)))})]
                                            :scales {:x {:axis false} :y {:axis false}}
                                            :labels {:title title}
                                            :legend false}
                                     iso? (assoc :aspect :iso)))
                                 {:columns 2})
                   {:width 900 :height 640})

;; ## Node-link layouts

;; `tree/node-link-layout` lays out a tree as nodes and links, like d3 `tree` and `cluster`: the nodes by depth, a link
;; path per child. Use it to draw a node-link tree of your own.

(nu/argument-list #'tree/node-link-layout)

;; These options choose the placement and the link shape, and bend the layout around a circle. The table below holds the nodes with
;; `:depth`, `:breadth`, `:x` and `:y`:

(nu/option-cards '(tree/node-link-layout root) (describe/options #'tree/node-link-layout))

(nu/result-table (:nodes (tree/node-link-layout (tree/from-levels offices {:hierarchy [:region :country :city]}) {})))

;; The panels below draw the links as lines under the nodes: tidy with curved links, then the leaves aligned with
;; straight links; the last two bend a tidy, curved layout around a circle, over a full turn and over 180 degrees:

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
                                   (cond-> {:charts [(chart/line {:data links :order :data :color [150 150 150] :mapping {:x :x :y :y :group :id}})
                                                     (chart/scatter {:data nodes :size 8 :mapping {:x :x :y :y} :padding {:x [0.1 0.25] :y [0.1 0.1]}})
                                                     (chart/text {:data (filter :leaf nodes) :size 7 :anchor [:left :middle] :offset [5 0]
                                                                  :mapping {:x :x :y :y :label #(str (:name %))}})]
                                            :scales {:x {:axis false} :y {:axis false}}
                                            :labels {:title title}}
                                     iso? (assoc :aspect :iso)))
                                 {:columns 2})
                   {:width 900 :height 640})

;; ## Edge bundling

;; `tree/bundle-paths` routes edges between the leaves of a laid-out tree by hierarchical edge bundling: each edge is a
;; B-spline through the tree path between its ends. Use it to draw an edge bundle of your own.

(nu/argument-list #'tree/bundle-paths)

;; These options name the edge ends and set the bundling strength. The table below holds the paths of the imports,
;; rows grouped by the edge's index `:id`:

(nu/option-cards '(tree/bundle-paths edges laid) (describe/options #'tree/bundle-paths))

(nu/result-table (:paths (tree/bundle-paths imports (tree/layout (tree/from-parents modules {}) {:circular true})
                                            {:from :module :to :uses})))

;; The panels below bundle the imports fully along the tree, at the default strength 0.85, and not at all:

(plot/render-image (plot/arrange (for [[title beta] [[":beta 1" 1.0] [":beta 0.85 (default)" 0.85] [":beta 0" 0.0]]]
                                   {:charts [(chart/line {:data (:paths (tree/bundle-paths imports
                                                                                           (tree/layout (tree/from-parents modules {}) {:circular true})
                                                                                           {:from :module :to :uses :beta beta}))
                                                          :order :data :mapping {:x :x :y :y :group :id :color :module}})
                                             (chart/text {:data (filter :leaf (:nodes (tree/layout (tree/from-parents modules {}) {:circular true})))
                                                          :size 8 :mapping {:x :x :y :y :label :id}})]
                                    :aspect :iso
                                    :scales {:x {:axis false} :y {:axis false}}
                                    :labels {:title title}
                                    :legend false})
                                 {:columns 3})
                   {:width 1000 :height 340})

;; ## Sequence levels

;; `hierarchy/sequence-levels` turns sequences, such as visited pages, into hierarchy rows: a step column per position,
;; identical sequences summed. Use it for a flow sunburst.

(nu/argument-list #'hierarchy/sequence-levels)

;; These options read the sequences and their weights and handle the end marker. The first table below drops the end
;; marker with `:keep-end false`, the second one keeps it as a step, the default:

(nu/option-cards '(hierarchy/sequence-levels rows) (describe/options #'hierarchy/sequence-levels))

(nu/result-table (:rows (hierarchy/sequence-levels visits {:sequence :path :value :n :separator ">" :end "end" :keep-end false})))

(nu/result-table (:rows (hierarchy/sequence-levels visits {:sequence :path :value :n :separator ">" :end "end"})))

;; The plot below draws the rows as a sunburst: `chart/partition` under polar coordinates, the steps as its
;; hierarchy:

(plot/render-image (let [{:keys [rows hierarchy]} (hierarchy/sequence-levels visits {:sequence :path :value :n :separator ">" :keep-end false})]
                     {:data rows
                      :mapping {:hierarchy hierarchy :value :value}
                      :charts [(chart/partition {:color-by :name})]
                      :coord {:kind :polar}})
                   {:width 500 :height 500})
