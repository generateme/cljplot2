;; # Flow

;; Connections and flows between entities: chord diagrams, networks, sankey and alluvial diagrams, bump charts, arc
;; diagrams, hierarchical edge bundling, dendrograms, clustered heatmaps, circular plots and flow sunbursts (the
;; [R Graph Gallery](https://r-graph-gallery.com/) flow pages and [data-to-viz](https://www.data-to-viz.com/)). Every
;; layout is a function returning rows, drawn by ordinary charts; the `plot/...` builders return plain plot maps.
;;
;; Not here: interactivity, multi-track circular plots, gradient ribbons and links, cyclic sankeys, crossing
;; minimization beyond clustering, arcs below the line and label repel.

(ns use-cases.flow
  (:require [clojure.string :as str]
            [fastmath.core :as m]
            [fastmath.ml.clustering :as cl]
            [fastmath.random :as r]
            [fastmath.stats :as stats]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.graph :as graph]
            [cljplot2.data.levels :as levels]
            [cljplot2.data.tree :as tree]
            [notebook-utils :as nu]))

(def migration
  "Flows between regions without the zeros and the diagonal; the file spells some names with dots."
  (let [region #(str/replace % "." " ")]
    (vec (for [{:keys [from to value]} (nu/dataset :migration)
               :when (and (pos? (double value)) (not= (region from) (region to)))]
           {:from (region from) :to (region to) :value value}))))

(nu/data-table migration)

(def characters
  (mapv (fn [{:keys [name group]}] {:id name :group (str "group " group)}) (nu/dataset :miserables-characters)))

(nu/data-table characters)

(def co-occurrences (nu/dataset :miserables-links))

(nu/data-table co-occurrences)

(def mtcars (nu/dataset :mtcars))

(nu/data-table mtcars)

(def mtcars-variables [:mpg :cyl :disp :hp :drat :wt :qsec :vs :am :gear :carb])

(def standardized
  "mtcars columns as z-scores, one row vector per car."
  (apply mapv vector (map (fn [k] (stats/standardize (map k mtcars))) mtcars-variables)))

(nu/data-table standardized)

(def air-passengers (nu/dataset :air-passengers))

(nu/data-table air-passengers)

;; ## Chord diagram

;; Flows between groups around a circle: a ring sector per group, sized by its flow, and a ribbon per link.

;; ### Chord diagram from an edge list

;; Migration between ten world regions: directed - a ribbon per flow, as wide as the flow at both ends, the sectors
;; holding the flows out and in - and undirected, a ribbon per pair of regions, as wide as their exchange.

(nu/source-note {:source "R Graph Gallery - Chord diagram; data-to-viz - Chord diagram"
                 :url ["https://r-graph-gallery.com/chord-diagram.html" "https://www.data-to-viz.com/graph/chord.html"]
                 :dataset :migration})

(plot/render-image
 (plot/arrange [(plot/chord migration {:directed true :min-value 0.1 :title "directed flows"})
                (plot/chord migration {:min-value 0.1 :title "exchange between regions"})]
               {:columns 2})
 {:width 1000 :height 520})

;; ### Colours, gaps, order and weak links

;; `:min-value` drops the weak flows that clutter the middle, `:gap-angle` widens the gaps, `:order :cluster` puts regions
;; with similar partners side by side, and the palette is a plot scale like any other.

(nu/source-note {:source "R Graph Gallery - Advanced chord diagram with circlize; data-to-viz - Chord diagram"
                 :url ["https://r-graph-gallery.com/123-circular-plot-circlize-package-2.html"
                       "https://www.data-to-viz.com/graph/chord.html"]
                 :dataset :migration})

(plot/render-image
 (plot/chord migration {:directed true :min-value 0.5 :gap-angle 4 :order :cluster
                        :scales {:color {:range :category10}}
                        :title "Flows of at least 0.5 million, clustered order"})
 {:width 700 :height 700})

;; ## Network

;; Nodes and the edges between them, placed by a layout; colour, size and width carry node and edge variables.

;; ### From an adjacency table

;; Co-authorships between researchers come as a table - a row per author, a column per author, 1 for a joint paper.
;; The table isn't square, so its cells become edge rows directly (`graph/adjacency->edges` reads square matrices,
;; see the correlation network below); the force layout places them, seeded for a stable picture. Groups of authors
;; without joint papers between them are laid out apart and packed in rows, the largest first.

(nu/source-note {:source "R Graph Gallery - Input formats for network charts; data-to-viz - Network"
                 :url ["https://r-graph-gallery.com/257-input-formats-for-network-charts.html"
                       "https://www.data-to-viz.com/graph/network.html"]
                 :dataset :coauthors})

(def coauthor-edges
  "One edge per co-author pair (the table lists some pairs both ways)."
  (->> (nu/dataset :coauthors)
       (mapcat (fn [{:keys [from] :as row}]
                 (for [[k v] (dissoc row :from) :when (= 1 v)] (vec (sort [from (name k)])))))
       (remove (fn [[a b]] (= a b)))
       (distinct)
       (mapv (fn [[a b]] {:from a :to b}))))

(nu/data-table coauthor-edges)

(plot/render-image
 (plot/network coauthor-edges {:rng (r/rng :mersenne 11) :node-style {:size 8}
                               :title (str (count coauthor-edges) " co-authorships")})
 {:width 750 :height 700})

;; ### Layouts

;; The same graph on a circle and with the Fruchterman-Reingold force layout: groups of characters that appear
;; together only come out of the force layout.

(nu/source-note {:source "R Graph Gallery - Network layouts; data-to-viz - Network"
                 :url ["https://r-graph-gallery.com/247-network-chart-layouts.html"
                       "https://www.data-to-viz.com/graph/network.html"]
                 :dataset [:miserables-links :miserables-characters]})

(plot/render-image
 (plot/arrange (vec (for [[layout title] [[:circle "circle layout"] [:force "force layout"]]]
                      (plot/network co-occurrences {:nodes characters :layout layout :rng (r/rng :mersenne 3)
                                                    :mapping {:color :group} :scales {:color {:legend false}}
                                                    :title title})))
               {:columns 2})
 {:width 1000 :height 500})

;; ### Node colour, size by degree

;; Nodes coloured by their group and sized by their number of links (`:degree`, computed with the graph); labels for
;; the main characters. One size scale serves one variable, so edge widths (below, for migration flows) and node
;; sizes don't go together.

(nu/source-note {:source "R Graph Gallery - igraph plotting parameters, map a colour, node size by edges number"
                 :url ["https://r-graph-gallery.com/248-igraph-plotting-parameters.html"
                       "https://r-graph-gallery.com/249-igraph-network-map-a-color.html"
                       "https://r-graph-gallery.com/251-network-with-node-size-based-on-edges-number.html"]
                 :dataset [:miserables-links :miserables-characters]})

(plot/render-image
 (plot/network co-occurrences {:nodes characters :rng (r/rng :mersenne 3)
                               :mapping {:color :group :size :degree}
                               :scales {:size {:title "links"}}
                               :labels #(when (>= (long (:degree %)) 10) (:id %))
                               :title "Les Misérables: characters appearing together"})
 {:width 900 :height 750})

;; ### Correlation network

;; Variables linked when their correlation is strong (|r| > 0.7): the matrix becomes edges with
;; `graph/adjacency->edges`, the weight is |r| and the colour its sign.

(nu/source-note {:source "R Graph Gallery - Correlation network with igraph"
                 :url "https://r-graph-gallery.com/250-correlation-network-with-igraph.html"
                 :dataset :mtcars})

(def correlation-edges
  (let [names (mapv name mtcars-variables)
        r (stats/correlation-matrix (mapv (fn [k] (mapv k mtcars)) mtcars-variables))
        strong (mapv (fn [row] (mapv #(if (> (m/abs (double %)) 0.7) % 0) row)) r)]
    (mapv #(assoc % :r (:value %) :value (m/abs (double (:value %))))
          (graph/adjacency->edges strong names {:whole-matrix false}))))

(nu/data-table correlation-edges)

(plot/render-image
 (plot/network correlation-edges {:nodes (mapv (fn [k] {:id (name k)}) mtcars-variables) :rng (r/rng :mersenne 5)
                                  :labels true
                                  :edge-mapping {:color #(if (pos? (double (:r %))) "positive" "negative")}
                                  :scales {:color {:range {"positive" [33 102 172] "negative" [178 24 43]} :title "r"}}
                                  :title "mtcars variables with |r| > 0.7"})
 {:width 700 :height 600})

;; ### Directed, weighted edges

;; Migration flows as arrows ending at the target's edge, edge widths from the flows' size scale.

(nu/source-note {:source "data-to-viz - Network"
                 :url "https://www.data-to-viz.com/graph/network.html"
                 :dataset :migration})

(plot/render-image
 (plot/network migration {:directed true :layout :circle :labels true
                          :edge-mapping {:size :value}
                          :scales {:size {:range [0.5 6] :title "migrants, M"}}
                          :title "Migration between world regions"})
 {:width 750 :height 650})

;; ## Sankey

;; Flows through stages: nodes in columns, as tall as their flow, bands between them as wide as the flow.

;; ### Sankey diagram, node and link colours

;; The UK energy flows of a 2050 scenario: sources on the left, uses on the right (`:align :justify`, the default),
;; bands in their source's colour; then every node as far left as it can go, bands in their target's colour.

(nu/source-note {:source "R Graph Gallery - Sankey diagram with networkD3, custom colours; data-to-viz - Sankey"
                 :url ["https://r-graph-gallery.com/323-sankey-diagram-with-the-networkd3-library.html"
                       "https://r-graph-gallery.com/322-custom-colours-in-sankey-diagram.html"
                       "https://www.data-to-viz.com/graph/sankey.html"]
                 :dataset :energy})

(def energy-flows (nu/dataset :energy))

(nu/data-table energy-flows)

(plot/render-image
 (plot/sankey energy-flows {:title "UK energy flows in 2050 (TWh)"})
 {:width 1000 :height 650})

(plot/render-image
 (plot/sankey energy-flows {:align :left :link-color :target :title "Left-aligned, bands in the target's colour"})
 {:width 1000 :height 650})

;; ### Alluvial diagram

;; A column per variable, a stratum per level and strands between neighbouring columns, coloured by the class all
;; the way through.

(nu/source-note {:source "R Graph Gallery - Sankey diagram (alluvial section)"
                 :url "https://r-graph-gallery.com/sankey-diagram.html"
                 :dataset :titanic})

(nu/data-table (nu/dataset :titanic))

(plot/render-image
 (plot/alluvial (nu/dataset :titanic) {:variables [:class :sex :age :survived] :value :n :strand-color :class
                                       :theme {:palette {:color :set1}}
                                       :title "Titanic: 2201 people by class, sex, age and survival"})
 {:width 900 :height 600})

;; ### Bump chart

;; Ranks over time: the months ranked by passengers within each year
;; ([`levels/rank-rows`](reference.data_levels.html#levelsrank-rows), 1 - the busiest; ties by data order with
;; `:ties :first`). July and August trade the top two places every year; June overtakes September for third and May
;; climbs, while March falls; November stays near the bottom.

(nu/source-note {:source "R Graph Gallery - Sankey diagram (bump chart section)"
                 :url "https://r-graph-gallery.com/sankey-diagram.html"
                 :dataset :air-passengers})

(def month-ranks (levels/rank-rows air-passengers :year :passengers {:ties :first}))

(nu/data-table month-ranks)

(plot/render-image
 {:data month-ranks :mapping {:x :year :y :rank :color :month}
  :charts [(chart/line {:line-width 2})
           (chart/scatter {:size 7})
           (chart/text {:data (filter #(= 1949 (:year %)) month-ranks) :mapping {:x :year :y :rank :label :month}
                        :anchor [:right :middle] :offset [-8 0]})
           (chart/text {:data (filter #(= 1960 (:year %)) month-ranks) :mapping {:x :year :y :rank :label :month}
                        :anchor [:left :middle] :offset [8 0]})]
  :scales {:y {:domain [12.5 0.5] :nice false :ticks (vec (range 1 13))}
           :x {:domain [1948 1961] :nice false :ticks (vec (range 1949 1961))}
           :color {:domain ["Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"]
                   :range :category20 :legend false}}
  :labels {:title "Months ranked by air passengers, 1949-1960" :x nil :y "rank"}}
 {:width 900 :height 550})

;; ## Arc diagram

;; Nodes on a line, every link a semicircle above it: readable when the order puts linked nodes close together.

(nu/source-note {:source "data-to-viz - Arc diagram"
                 :url "https://www.data-to-viz.com/graph/arc.html"
                 :dataset [:miserables-links :miserables-characters]})

(plot/render-image
 (plot/arrange [(plot/arc co-occurrences {:nodes characters :mapping {:color :group}
                                          :title "Les Misérables: input order"})
                (plot/arc co-occurrences {:nodes characters :mapping {:color :group} :order :cluster
                                          :title "clustered order: characters seen with the same others together"})]
               {:columns 1})
 {:width 1000 :height 800})

;; ## Edge bundling

;; Links between the leaves of a hierarchy routed along it: the bundles show which packages depend on which.

;; ### Hierarchical edge bundling

;; The classes of the flare library around a circle, grouped by package, each import a curve through the package
;; tree, coloured by the importing class's top-level package.

(nu/source-note {:source "R Graph Gallery - Hierarchical edge bundling, custom, labels; data-to-viz - Edge bundling"
                 :url ["https://r-graph-gallery.com/309-intro-to-hierarchical-edge-bundling.html"
                       "https://r-graph-gallery.com/310-custom-hierarchical-edge-bundling.html"
                       "https://r-graph-gallery.com/311-add-labels-to-hierarchical-edge-bundling.html"
                       "https://www.data-to-viz.com/graph/edge_bundling.html"]
                 :dataset [:flare :flare-dependencies]})

(nu/data-table (nu/dataset :flare))

(def flare-tree (tree/from-parents (nu/dataset :flare) {:name :name}))

(def flare-imports (nu/dataset :flare-dependencies))

(nu/data-table flare-imports)

(plot/render-image
 (plot/bundle flare-imports flare-tree {:from :source :to :target
                                        :title "flare: class imports bundled along the package tree"})
 {:width 800 :height 800})

;; ### Bundling strength

;; `:beta` straightens the curves: 0 draws straight lines, 1 follows the tree exactly.

(nu/source-note {:source "R Graph Gallery - Custom hierarchical edge bundling"
                 :url "https://r-graph-gallery.com/310-custom-hierarchical-edge-bundling.html"
                 :dataset [:flare :flare-dependencies]})

(plot/render-image
 (plot/arrange (vec (for [beta [0.0 0.5 1.0]]
                      (plot/bundle flare-imports flare-tree {:from :source :to :target :beta beta :labels false
                                                             :scales {:color {:legend false}}
                                                             :title (str ":beta " beta)})))
               {:columns 3})
 {:width 1000 :height 380})

;; ## Dendrogram

;; A hierarchy as a tree of branches: from data with levels, or from hierarchical clustering, where the height of a
;; merge is the distance between the groups.

;; ### From hierarchical data

;; World regions and their subregions, the tree drawn from the levels of the rows.

(nu/source-note {:source "R Graph Gallery - Dendrogram with ggraph; data-to-viz - Dendrogram"
                 :url ["https://r-graph-gallery.com/334-basic-dendrogram-with-ggraph.html"
                       "https://www.data-to-viz.com/graph/dendrogram.html"]
                 :dataset :world-population})

(nu/data-table (nu/dataset :world-population))

(plot/render-image
 (plot/dendrogram (tree/from-levels (nu/dataset :world-population) {:hierarchy [:region :subregion]})
                  {:horizontal true :group-labels true :title "World regions and subregions"})
 {:width 650 :height 650})

;; ### From clustering, coloured clusters, circular

;; Complete-linkage clustering of the standardized cars (fastmath `hclust`), cut into 3 clusters; the same tree
;; around a circle.

(nu/source-note {:source "R Graph Gallery - Basic dendrogram, custom colours, circular dendrogram, dendextend"
                 :url ["https://r-graph-gallery.com/29-basic-dendrogram.html"
                       "https://r-graph-gallery.com/31-custom-colors-in-dendrogram.html"
                       "https://r-graph-gallery.com/339-circular-dendrogram-with-ggraph.html"
                       "https://r-graph-gallery.com/340-custom-your-dendrogram-with-dendextend.html"]
                 :dataset :mtcars})

(def car-clustering (cl/hclust standardized {:method :complete}))

(def cars (mapv :model mtcars))

(plot/render-image
 (plot/dendrogram car-clustering {:leaf-names cars :k 3 :theme {:palette {:color :set1}}
                                  :title "mtcars: complete-linkage clustering, 3 clusters"})
 {:width 1000 :height 500})

(plot/render-image
 (plot/dendrogram car-clustering {:leaf-names cars :k 3 :circular true :theme {:palette {:color :set1}}
                                  :title "The same clustering, circular"})
 {:width 700 :height 700})

;; ## Clustered heatmap

;; Rows and columns of a heatmap ordered by clustering, their dendrograms beside it: similar cars and similar
;; variables come together.

(nu/source-note {:source "R Graph Gallery - The heatmap function"
                 :url "https://r-graph-gallery.com/215-the-heatmap-function.html"
                 :dataset :mtcars})

(plot/render-image
 (plot/clustered-heatmap (vec (for [[car row] (map vector cars standardized) [k v] (map vector mtcars-variables row)]
                                {:car car :variable (name k) :z v}))
                         {:row :car :col :variable :value :z
                          :scales {:color {:range :RColorBrewer/RdBu :midpoint 0 :reverse true :title "z-score"}}
                          :title "mtcars: cars and variables ordered by clustering"})
 {:width 900 :height 800})

;; ## Circular plots

;; Ordinary charts in polar coordinates: the angle runs along a variable, the radius along another; `:start`,
;; `:direction`, `:inner` and `:span` shape the circle.

;; ### Basic, customised, part of the circle

;; Air passengers by month around the circle, a ring per year: the default circle, then a hole, a start at 3 o'clock
;; counter-clockwise, and half a circle.

(nu/source-note {:source "R Graph Gallery - Basic circular plot, custom a track, part of the circular plot"
                 :url ["https://r-graph-gallery.com/224-basic-circular-plot.html"
                       "https://r-graph-gallery.com/225-circular-plot-custom-a-track.html"
                       "https://r-graph-gallery.com/230-draw-part-of-the-circular-plot-only.html"]
                 :dataset :air-passengers})

(def months ["Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"])

(plot/render-image
 (plot/arrange (vec (for [[coord title] [[{:kind :polar} "default"]
                                         [{:kind :polar :inner 0.3 :start 90 :direction :counter-clockwise} "hole, start, direction"]
                                         [{:kind :polar :span 180 :start -90} "half a circle"]]]
                      {:data air-passengers :mapping {:x :month :y :passengers :color :year}
                       :charts [(chart/scatter {:size 5})]
                       :coord coord
                       :scales {:x {:domain months}}
                       :legend false
                       :labels {:title title :x nil :y nil}}))
               {:columns 3})
 {:width 1000 :height 400})

;; ### Chart types, several plots

;; Points, a line per year and bars of the monthly totals in the same circle, and one circular plot per period. A
;; year's line runs from January to December and stays open between them - the next January belongs to the next
;; year's line.

(nu/source-note {:source "R Graph Gallery - Plot types for circular plot, several circular plots in a figure"
                 :url ["https://r-graph-gallery.com/226-plot-types-for-circular-plot.html"
                       "https://r-graph-gallery.com/229-several-circular-plots-in-a-figure.html"]
                 :dataset :air-passengers})

(def periods
  (mapv #(assoc % :period (cond (<= (long (:year %)) 1952) "1949-1952" (<= (long (:year %)) 1956) "1953-1956" :else "1957-1960"))
        air-passengers))

(nu/data-table periods)

(plot/render-image
 (plot/arrange
  [{:data air-passengers :mapping {:x :month :y :passengers}
    :charts [(chart/scatter {:size 4})] :coord {:kind :polar :inner 0.2} :scales {:x {:domain months}}
    :labels {:title "points" :x nil :y nil}}
   {:data air-passengers :mapping {:x :month :y :passengers :group :year}
    :charts [(chart/line {:order :data :color [70 130 180 150]})] :coord {:kind :polar :inner 0.2} :scales {:x {:domain months}}
    :labels {:title "a line per year" :x nil :y nil}}
   {:data air-passengers :mapping {:x :month :y :passengers}
    :charts [(chart/bar {:stat :sum})] :coord {:kind :polar :inner 0.2} :scales {:x {:domain months}}
    :labels {:title "bars: monthly totals" :x nil :y nil}}]
  {:columns 3})
 {:width 1000 :height 400})

(plot/render-image
 {:data periods :mapping {:x :month :y :passengers :group :year :facet :period}
  :charts [(chart/line {:order :data})]
  :coord {:kind :polar :inner 0.2}
  :scales {:x {:domain months}}
  :labels {:title "One circular plot per period" :x nil :y nil}}
 {:width 1000 :height 420})

;; ## Flow sunburst

;; Sequences of steps (website visits, here 2000 synthetic sessions) as rings from the centre: every step coloured by
;; its page wherever it occurs, `end` where a session stops.

(nu/source-note {:source "data-to-viz - Sunburst"
                 :url "https://www.data-to-viz.com/graph/sunburst.html"})

(def pages ["home" "search" "product" "account" "cart" "help"])

(def visits
  "2000 sessions starting at home: each step to another page or the end (more likely the longer the session), at
  most 6 pages; a seeded random walk."
  (let [g (r/rng :mersenne 7)
        weights {"home" [0 5 3 2 0 1] "search" [1 2 6 0 1 1] "product" [1 2 2 1 4 1]
                 "account" [2 0 1 1 1 2] "cart" [0 0 2 2 0 1] "help" [2 2 0 2 0 1]}
        pick (fn [ws] (let [total (double (reduce + ws)) u (* total (r/drandom g))]
                        (loop [i 0 acc 0.0]
                          (let [acc (+ acc (double (ws i)))]
                            (if (or (< u acc) (= i (dec (count ws)))) i (recur (inc i) acc))))))]
    (vec (for [_ (range 2000)]
           {:sequence (str/join "-" (loop [path ["home"]]
                                      (if (or (= 6 (count path)) (< (r/drandom g) (* 0.12 (count path))))
                                        (conj path "end")
                                        (recur (conj path (pages (pick (weights (peek path)))))))))}))))

(nu/data-table visits)

(plot/render-image
 (plot/sequences visits {:scales {:color {:domain (conj pages "end")}}
                         :title "2000 website visits: pages in order from the centre"})
 {:width 700 :height 650})

;; ## Caveats

;; - A network of many nodes and links becomes a hairball: filter weak links, or show the graph as an arc diagram or
;;   an adjacency heatmap instead.
;; - Order changes the reading of chord and arc diagrams: `:order :cluster` puts nodes with the same partners
;;   together; input order may scatter them.
;; - Weak links clutter chord and sankey diagrams; `:min-value` leaves them out (and the layout says how many).
;; - Sankey diagrams show flows that go one way: a flow back to an earlier node is a cycle the layout refuses.
;; - Widths and areas encode values: a band twice as wide is twice the flow, a sector twice as long twice the total.

(nu/source-note {:source "data-to-viz - Caveats: spaghetti plot, order your data"
                 :url ["https://www.data-to-viz.com/caveat/spaghetti.html"
                       "https://www.data-to-viz.com/caveat/order_data.html"]})
