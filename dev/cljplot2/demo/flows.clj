(ns cljplot2.demo.flows
  "Flow cycle 3 demo: chord diagrams of migration flows between world regions (directed - `chordDirected`, and
  undirected - the exchange of a pair, `chord`), Les Misérables arc diagrams in input and clustered node order, and
  polygons from rows.

  Data: as [[cljplot2.demo.network]] - `migration.csv` (data-to-viz, Yan Holtz, MIT) and the Les Misérables
  characters and co-occurrences (vega-datasets, D. E. Knuth; licence not specified), from `notebooks/data`."
  (:require [fastmath.core :as m]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.demo.network :as network]))

(def ^:private flows @#'network/flows)
(def ^:private co-occurrences @#'network/co-occurrences)
(def ^:private characters @#'network/characters)

(def ^:private regions-theme {:palette {:color :category10}})

(def migration-directed
  (plot/chord flows {:directed true :min-value 0.1 :theme regions-theme
                     :title "Migration between world regions (flows < 0.1 M left out)"}))

(def migration-undirected
  (plot/chord flows {:min-value 0.1 :theme regions-theme
                     :title "Migration exchange between world regions"}))

(def miserables-arcs
  (plot/arrange [(plot/arc co-occurrences {:nodes characters :mapping {:color :group}
                                           :title "Les Misérables: input order"})
                 (plot/arc co-occurrences {:nodes characters :mapping {:color :group} :order :cluster
                                           :title "clustered order (hclust of the co-occurrence rows)"})]
                {:columns 1}))

(def ^:private shapes
  "Regular polygons with 3 to 8 sides on a row, and a five-pointed star."
  (into (vec (for [sides (range 3 9) k (range sides)
                   :let [a (cond-> (/ (* m/TWO_PI k) sides) (even? sides) (+ (/ m/PI sides)))]]
               {:shape (str sides " sides") :sides sides :x (+ (* 2.5 sides) (m/sin a)) :y (m/cos a)}))
        (for [k (range 10) :let [a (/ (* m/TWO_PI k) 10) r (if (even? k) 1.2 0.5)]]
          {:shape "star" :sides 10 :x (+ 12.5 (* r (m/sin a))) :y (+ -2.8 (* r (m/cos a)))})))

(def polygons
  {:data shapes :mapping {:x :x :y :y :group :shape :color :sides}
   :charts [(chart/polygon {:alpha 0.8 :stroke-color :black})]
   :aspect :iso
   :scales {:color {:title "points" :range :viridis/viridis}}
   :labels {:title "Polygons from rows (coloured by their first row)" :x nil :y nil}})

(def images
  "Images of this demo: `[file-name plot options]`, saved to `results/flows/` by [[cljplot2.demo/render!]]."
  [["migration-directed-750x750.png" migration-directed {:width 750 :height 750}]
   ["migration-undirected-750x750.png" migration-undirected {:width 750 :height 750}]
   ["miserables-arcs-1000x800.png" miserables-arcs {:width 1000 :height 800}]
   ["polygons-700x450.png" polygons {:width 700 :height 450}]])
