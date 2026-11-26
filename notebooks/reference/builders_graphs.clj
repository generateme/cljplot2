;; # Graphs

;; These builders turn edge rows, or wide rows, into an ordinary plot map: they lay the graph out with the data
;; functions of the Graphs and trees chapter and add the charts. Use them for networks and flows. The Shared options
;; chapter documents the layout options they pass on and the builder options (title, scales, theme).

(ns reference.builders-graphs
  (:require [clojure.string :as str]
            [fastmath.random :as r]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [notebook-utils :as nu]))

;; The tables below hold made-up friendships with their weights, and the people's teams and ages; the networks and arc
;; diagrams use them:

(def friends [{:from "Ann" :to "Bob" :value 3} {:from "Ann" :to "Cid" :value 1} {:from "Bob" :to "Cid" :value 2}
              {:from "Cid" :to "Dan" :value 4} {:from "Dan" :to "Eve" :value 1} {:from "Eve" :to "Ann" :value 2}
              {:from "Dan" :to "Fay" :value 2} {:from "Fay" :to "Eve" :value 1}])

(nu/data-table friends)

(def people [{:id "Ann" :team "red" :age 31} {:id "Bob" :team "red" :age 45} {:id "Cid" :team "blue" :age 27}
             {:id "Dan" :team "blue" :age 52} {:id "Eve" :team "green" :age 38} {:id "Fay" :team "green" :age 23}])

(nu/data-table people)

;; The tables below hold made-up shop visits through stages, and the stages' kinds; the small sankeys use them:

(def stages [{:id "web" :kind "channel"} {:id "store" :kind "channel"} {:id "basket" :kind "step"}
             {:id "paid" :kind "outcome"} {:id "left" :kind "outcome"}])

(nu/data-table stages)

(def visits [{:from "web" :to "basket" :value 60} {:from "store" :to "basket" :value 30}
             {:from "basket" :to "paid" :value 55} {:from "basket" :to "left" :value 35}])

(nu/data-table visits)

;; The table below holds migration flows between world regions; the chord diagrams use it:

(nu/source-note {:dataset :migration})

(def migration
  "Flows between regions without the zeros and the diagonal; the file spells some names with dots."
  (let [region #(str/replace % "." " ")]
    (vec (for [{:keys [from to value]} (nu/dataset :migration)
               :when (and (pos? (double value)) (not= (region from) (region to)))]
           {:from (region from) :to (region to) :value value}))))

(nu/data-table migration)

;; The table below holds the UK energy flows of a 2050 scenario; the large sankey uses it:

(nu/source-note {:dataset :energy})

(def energy (nu/dataset :energy))

(nu/data-table energy)

;; The table below counts the Titanic's passengers and crew by class, sex, age and survival; the alluvial plots use it:

(nu/source-note {:dataset :titanic})

(def titanic (nu/dataset :titanic))

(nu/data-table titanic)

;; ## Networks

;; `plot/network` draws a node-link diagram of edge rows. A force-directed, circle or random layout places the nodes,
;; and segments join them, with arrows in a directed graph. Use it to show who links to whom.

(nu/argument-list #'plot/network)

(nu/shared-groups (describe/options #'plot/network))

;; These options map node and edge aesthetics, style the nodes and edges, and write labels. The plot below takes node
;; aesthetics from the node rows and the computed degree (whole numbers through the builder's `:scales`) and writes
;; labels beside the nodes; a label overlapping that of a larger node is left out:

(nu/option-cards '(plot/network edges) (describe/own-rows (describe/options #'plot/network)))

(plot/render-image (plot/network friends {:nodes people :rng (r/rng :mersenne 1)
                                          :mapping {:color :team :size :degree}
                                          :scales {:size {:integer-ticks true}
                                                   :color {:range {"red" [213 94 0] "blue" [0 114 178] "green" [0 158 115]}}}
                                          :labels true})
                   {:width 600 :height 400})

;; The left panel below merges a node and edge style over the defaults, with a directed graph's arrows in
;; `:edge-style`; the right one takes edge widths from the weights and labels from a node column (`:labels :age`). An
;; aesthetic maps either nodes or edges, not both:

(plot/render-image (plot/arrange [(plot/network friends {:directed true :rng (r/rng :mersenne 1)
                                                         :node-style {:size 16 :color [230 120 40] :shape \S}
                                                         :edge-style {:color [60 60 60] :arrow {:size 9}}
                                                         :labels false
                                                         :title "styles, directed"})
                                  (plot/network friends {:nodes people :layout :circle
                                                         :edge-mapping {:size :value}
                                                         :labels :age
                                                         :title ":labels :age, circle layout"})]
                                 {:columns 2})
                   {:width 900 :height 380})

;; A network draws in theme styles: the nodes in `[:chart :network-node]` (a scatter style), the edges in
;; `[:chart :network-edge]` (a segment style) and the labels in the text role `:network-label`. `:node-style` and
;; `:edge-style` win over them per call:

(nu/theme-key-list :network-node (describe/style-rows :network-node))

;; A theme restyles every network of a plot; the plot below draws square, dark nodes:

(plot/render-image (plot/network friends {:rng (r/rng :mersenne 1) :labels true
                                          :theme {:chart {:network-node {:shape \S :color [60 60 60] :size 10}}}})
                   {:width 500 :height 360})

;; The edges' theme style has these keys; the plot below draws dashed blue edges and larger labels through the theme:

(nu/theme-key-list :network-edge (describe/style-rows :network-edge))

(plot/render-image (plot/network friends {:rng (r/rng :mersenne 1) :labels true
                                          :theme {:chart {:network-edge {:color [40 90 200] :dash :dashed :line-width 2}}
                                                  :text {:network-label {:size 11}}}})
                   {:width 500 :height 360})

;; ## Chord diagrams

;; `plot/chord` draws flows between groups around a circle: a sector per node sized by its flow, and a ribbon per
;; flow of a directed graph, or per pair of an undirected one. Use it for flows in both directions between a few
;; groups.

(nu/argument-list #'plot/chord)

(nu/shared-groups (describe/options #'plot/chord))

;; These options set the ribbons' opacity and the node names. The left panel below draws directed migration between
;; world regions without flows under 0.1; the right one draws opaque ribbons without names:

(nu/option-cards '(plot/chord edges) (describe/own-rows (describe/options #'plot/chord)))

(plot/render-image (plot/arrange [(plot/chord migration {:directed true :min-value 0.1 :title "directed, :alpha 0.6"})
                                  (plot/chord migration {:min-value 0.1 :alpha 1.0 :labels false
                                                         :title ":alpha 1.0, :labels false"})]
                                 {:columns 2})
                   {:width 1000 :height 500})

;; ## Arc diagrams

;; `plot/arc` draws the nodes on a line and the links as arcs above it. Use it for a compact view of a graph whose node
;; order matters.

(nu/argument-list #'plot/arc)

(nu/shared-groups (describe/options #'plot/arc))

;; These options map node and edge aesthetics, set the arcs' opacity, style the nodes and arcs, and write labels. The
;; plot below colours the nodes by team and takes arc widths from the weights, as arc does by default when the weights
;; differ:

(nu/option-cards '(plot/arc edges) (describe/own-rows (describe/options #'plot/arc)))

(plot/render-image (plot/arc friends {:nodes people :mapping {:color :team}
                                     :scales {:color {:range {"red" [213 94 0] "blue" [0 114 178] "green" [0 158 115]}}}})
                   {:width 600 :height 320})

;; The left panel below colours the arcs, sets their opacity and the node style and takes labels from a node column;
;; the right one draws no labels:

(plot/render-image (plot/arrange [(plot/arc friends {:nodes people :edge-mapping {:color :value} :alpha 0.9
                                                     :node-style {:size 12 :color [40 40 40]}
                                                     :edge-style {:line-width 2}
                                                     :labels :team
                                                     :title "arc colours, :labels :team"})
                                  (plot/arc friends {:nodes people :mapping {:size :age} :labels false
                                                     :title ":labels false, node size by age"})]
                                 {:columns 2})
                   {:width 900 :height 320})

;; An arc diagram draws in theme styles: the nodes in `[:chart :arc-node]` (a scatter style) and the arcs in
;; `[:chart :arc-edge]` (a line style: the box shows its colour, width and dash; the other line style keys apply too):

(nu/theme-key-list :arc-node (describe/style-rows :arc-node))

;; The plot below draws hollow nodes through the theme:

(plot/render-image (plot/arc friends {:theme {:chart {:arc-node {:shape \o :color [200 60 40] :size 10}}}})
                   {:width 500 :height 300})

(nu/theme-key-list :arc-edge (describe/style-rows :arc-edge [:color :line-width :dash]))

;; The plot below draws thick dark arcs through the theme:

(plot/render-image (plot/arc friends {:edge-mapping {} :alpha 0.8
                                      :theme {:chart {:arc-edge {:color [40 40 40] :line-width 3}}}})
                   {:width 500 :height 300})

;; ## Sankey diagrams

;; `plot/sankey` draws flows through layered nodes: the nodes as bars in columns, the links as bands as wide as their
;; flows. Use it to follow quantities through stages.

(nu/argument-list #'plot/sankey)

(nu/shared-groups (describe/options #'plot/sankey))

;; These options colour the nodes and the flow bands, set the bands' opacity and write the node names. The plot below
;; draws the UK energy flows of a 2050 scenario, bands in their source's colour (the default). A node name too long for
;; its room shrinks down to the `:min-size` of the text role `:sankey-label` (here the farm wastes before
;; Bio-conversion); a name overlapping that of a larger node is left out:

(nu/option-cards '(plot/sankey edges) (describe/own-rows (describe/options #'plot/sankey)))

(plot/render-image (plot/sankey energy {:link-color :source})
                   {:width 900 :height 550})

;; The left panel below draws bands in the target's colour at a higher opacity; the middle one draws them in the
;; theme's grey; the right one colours the nodes by a node column, with a legend and no names:

(plot/render-image (plot/arrange [(plot/sankey visits {:link-color :target :alpha 0.8 :title ":link-color :target"})
                                  (plot/sankey visits {:link-color :theme :title ":link-color :theme"})
                                  (plot/sankey visits {:nodes stages :mapping {:color :kind} :labels false
                                                       :title ":mapping {:color :kind}, no labels"})]
                                 {:columns 3 :widths [1 1 1.3]})
                   {:width 1000 :height 330})

;; The flow bands draw in the theme style `[:chart :sankey-link]` (a polygon style; its colour shows with
;; `:link-color :theme`) and the node names in the text role `:sankey-label`:

(nu/theme-key-list :sankey-link (describe/style-rows :sankey-link))

;; The plot below draws grey bands outlined in white and larger names:

(plot/render-image (plot/sankey visits {:link-color :theme
                                        :theme {:chart {:sankey-link {:color [120 140 170] :stroke-color :white
                                                                      :stroke-width 1}}
                                                :text {:sankey-label {:size 11}}}})
                   {:width 600 :height 330})

;; ## Alluvial diagrams

;; `plot/alluvial` draws a column per categorical variable of wide rows, with a stratum per level. Strands join
;; neighbouring columns; a value weights them and one variable colours them all the way through. Use it to follow how
;; groups split across several categories.

(nu/argument-list #'plot/alluvial)

(nu/shared-groups (describe/options #'plot/alluvial))

;; These options set the strands' opacity, the strata's width and gaps, and the names under the columns. The plot
;; below draws Titanic passengers by class, sex, age and survival, coloured by class:

(nu/option-cards '(plot/alluvial rows) (describe/own-rows (describe/options #'plot/alluvial)))

(plot/render-image (plot/alluvial titanic {:variables [:class :sex :age :survived] :value :n :strand-color :class})
                   {:width 900 :height 500})

;; The next plot draws nearly opaque strands, wide strata with large gaps and names of your own under the columns:

(plot/render-image (plot/alluvial titanic {:variables [:class :survived] :value :n :strand-color :survived
                                           :alpha 0.9 :node-width 0.25 :gap 0.1
                                           :variable-names ["passenger class" "survived?"]})
                   {:width 600 :height 400})

;; The strata draw in the theme style `[:chart :alluvial-stratum]` (a polygon style) and their names in the text
;; role `:alluvial-label`:

(nu/theme-key-list :alluvial-stratum (describe/style-rows :alluvial-stratum))

;; The plot below draws dark strata with light names:

(plot/render-image (plot/alluvial titanic {:variables [:class :survived] :value :n :strand-color :survived
                                           :theme {:chart {:alluvial-stratum {:color [50 50 50]}}
                                                   :text {:alluvial-label {:color :white :background :none}}}})
                   {:width 600 :height 400})
