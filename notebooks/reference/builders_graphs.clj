;; # Builders: graphs

;; Builders turn edge rows (or wide rows) into an ordinary plot map: they lay the graph out with the data functions
;; of the Graphs and trees chapter and add the charts. Each builder lists its own options; the options it forwards
;; to the layout functions and the builder options (title, scales, theme) are shared groups.

(ns reference.builders-graphs
  (:require [clojure.string :as str]
            [fastmath.random :as r]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [notebook-utils :as nu]))

(def friends [{:from "Ann" :to "Bob" :value 3} {:from "Ann" :to "Cid" :value 1} {:from "Bob" :to "Cid" :value 2}
              {:from "Cid" :to "Dan" :value 4} {:from "Dan" :to "Eve" :value 1} {:from "Eve" :to "Ann" :value 2}
              {:from "Dan" :to "Fay" :value 2} {:from "Fay" :to "Eve" :value 1}])

(nu/data-table friends)

(def people [{:id "Ann" :team "red" :age 31} {:id "Bob" :team "red" :age 45} {:id "Cid" :team "blue" :age 27}
             {:id "Dan" :team "blue" :age 52} {:id "Eve" :team "green" :age 38} {:id "Fay" :team "green" :age 23}])

(nu/data-table people)

(nu/source-note {:dataset :migration})

(def migration
  "Flows between regions without the zeros and the diagonal; the file spells some names with dots."
  (let [region #(str/replace % "." " ")]
    (vec (for [{:keys [from to value]} (nu/dataset :migration)
               :when (and (pos? (double value)) (not= (region from) (region to)))]
           {:from (region from) :to (region to) :value value}))))

(nu/data-table migration)

(nu/source-note {:dataset :energy})

(def energy (nu/dataset :energy))

(nu/data-table energy)

(nu/source-note {:dataset :titanic})

(def titanic (nu/dataset :titanic))

(nu/data-table titanic)

;; ## network

;; A node-link diagram of edge rows (`:from`, `:to`, optional `:value`): nodes placed by a force-directed, circle or
;; random layout (seeded here), edges as segments - with arrows for a directed graph.

(nu/options-table (describe/own-rows (describe/options #'plot/network)))

;; Node aesthetics from the node rows or the computed degree, labels beside the nodes:

(plot/render-image (plot/network friends {:nodes people :rng (r/rng :mersenne 1)
                                          :mapping {:color :team :size :degree}
                                          :labels true})
                   {:width 600 :height 400})

;; The node and edge style merged over the defaults, a directed graph's arrows in `:edge-style` (left); edge widths
;; from the weights and labels from a node column (`:labels :age`, right) - an aesthetic maps either nodes or edges,
;; not both:

(plot/render-image (plot/arrange [(plot/network friends {:directed? true :rng (r/rng :mersenne 1)
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

(nu/shared-groups (describe/options #'plot/network))

;; ## chord

;; Flows between groups around a circle: a sector per node sized by its flow, a ribbon per link - directed (a
;; ribbon per flow) or not (a ribbon per pair).

(nu/options-table (describe/own-rows (describe/options #'plot/chord)))

;; Migration between world regions (flows under 0.1 dropped), opaque ribbons and no labels (right):

(plot/render-image (plot/arrange [(plot/chord migration {:directed? true :min-value 0.1 :title "directed, :alpha 0.6"})
                                  (plot/chord migration {:min-value 0.1 :alpha 1.0 :labels? false
                                                         :title ":alpha 1.0, :labels? false"})]
                                 {:columns 2})
                   {:width 1000 :height 500})

(nu/shared-groups (describe/options #'plot/chord))

;; ## arc

;; Nodes on a line, links as arcs above it - a compact view of a graph whose node order matters.

(nu/options-table (describe/own-rows (describe/options #'plot/arc)))

;; Node colours from their rows, arc widths from the weights (the default when weights differ), names under the
;; line:

(plot/render-image (plot/arc friends {:nodes people :mapping {:color :team}})
                   {:width 600 :height 320})

;; Arc colours and opacity, node style, labels from a node column (left); no labels (right):

(plot/render-image (plot/arrange [(plot/arc friends {:nodes people :edge-mapping {:color :value} :alpha 0.9
                                                     :node-style {:size 12 :color [40 40 40]}
                                                     :edge-style {:width 2}
                                                     :labels :team
                                                     :title "arc colours, :labels :team"})
                                  (plot/arc friends {:nodes people :mapping {:size :age} :labels? false
                                                     :title ":labels? false, node size by age"})]
                                 {:columns 2})
                   {:width 900 :height 320})

(nu/shared-groups (describe/options #'plot/arc))

;; ## sankey

;; Flows through layered nodes: nodes as bars in columns, links as bands as wide as their flow.

(nu/options-table (describe/own-rows (describe/options #'plot/sankey)))

;; The UK energy flows of a 2050 scenario, bands in their source's colour (the default):

(plot/render-image (plot/sankey energy {:link-color :source})
                   {:width 900 :height 550})

;; Bands in the target's colour or grey with stronger opacity, nodes coloured by a node column with a legend, and
;; no labels:

(def stages [{:id "web" :kind "channel"} {:id "store" :kind "channel"} {:id "basket" :kind "step"}
             {:id "paid" :kind "outcome"} {:id "left" :kind "outcome"}])

(nu/data-table stages)

(def visits [{:from "web" :to "basket" :value 60} {:from "store" :to "basket" :value 30}
             {:from "basket" :to "paid" :value 55} {:from "basket" :to "left" :value 35}])

(nu/data-table visits)

(plot/render-image (plot/arrange [(plot/sankey visits {:link-color :target :alpha 0.8 :title ":link-color :target"})
                                  (plot/sankey visits {:link-color :none :title ":link-color :none"})
                                  (plot/sankey visits {:nodes stages :mapping {:color :kind} :labels? false
                                                       :title ":mapping {:color :kind}, no labels"})]
                                 {:columns 3 :widths [1 1 1.3]})
                   {:width 1000 :height 330})

(nu/shared-groups (describe/options #'plot/sankey))

;; ## alluvial

;; A column per categorical variable of wide rows, a stratum per level and strands between neighbouring columns,
;; weighted by a value and coloured by one variable all the way through.

(nu/options-table (describe/own-rows (describe/options #'plot/alluvial)))

;; Titanic passengers by class, sex, age and survival, coloured by class:

(plot/render-image (plot/alluvial titanic {:axes [:class :sex :age :survived] :value :n :fill :class})
                   {:width 900 :height 500})

;; Opaque strands, wide strata with large gaps and names of your own under the columns:

(plot/render-image (plot/alluvial titanic {:axes [:class :survived] :value :n :fill :survived
                                           :alpha 0.9 :node-width 0.25 :pad 0.1
                                           :axis-names ["passenger class" "survived?"]})
                   {:width 600 :height 400})

(nu/shared-groups (describe/options #'plot/alluvial))
