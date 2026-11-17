(ns cljplot2.demo.network
  "Flow cycle 1 demo: network plots - Les Misérables co-occurrences (force layout, colour by group, size by degree),
  circle vs force layout, directed migration flows with arrows and widths (opposite flows of a pair share one line -
  curved edges come later), the mtcars correlation network.

  Data (`notebooks/data`, sources and licences in its `datasets.edn`): `miserables-characters.csv`,
  `miserables-links.csv` - vega-datasets (D. E. Knuth, The Stanford GraphBase, 1993; licence not specified);
  `migration.csv` - data-to-viz `13_AdjacencyDirectedWeighted_long.csv` (Yan Holtz, MIT), migration flows between
  regions (from, to, value)."
  (:require [fastmath.random :as r]
            [fastmath.stats :as stats]
            [cljplot2.core :as plot]
            [cljplot2.data.graph :as graph]
            [cljplot2.demo.data :as data]
            [cljplot2.demo.parallel :refer [mtcars]]))

(def ^:private characters
  (mapv (fn [{:keys [name group]}] {:id name :group (str "group " group)}) (data/csv-rows "miserables-characters.csv")))

(def ^:private co-occurrences
  (mapv #(update % :value parse-long) (data/csv-rows "miserables-links.csv")))

(def les-miserables
  (plot/network co-occurrences {:nodes characters :rng (r/rng :mersenne 3)
                                :mapping {:color :group :size :degree}
                                :labels #(when (>= (long (:degree %)) 10) (:id %))
                                :title "Les Misérables: characters appearing together"}))

(def layouts
  (plot/arrange (vec (for [[l title] [[:circle "circle layout"] [:force "force layout (Fruchterman-Reingold)"]]]
                       (plot/network co-occurrences {:nodes characters :layout l :rng (r/rng :mersenne 3)
                                                     :mapping {:color :group} :scales {:color {:legend false}}
                                                     :title title})))
                {:columns 2}))

(def ^:private flows
  (let [rows (data/csv-rows "migration.csv")]
    ;; the file spells some names with dots ("South.Asia") - one spelling
    (vec (for [{from :from to :to v :value} rows :let [v (parse-double v) name #(clojure.string/replace % "." " ")]
               :when (and (pos? v) (not= (name from) (name to)))]
           {:from (name from) :to (name to) :value v}))))

(def migration
  (plot/network flows {:directed? true :layout :circle :labels true
                       :edge-mapping {:size :value}
                       :scales {:size {:range [0.5 6] :title "migrants, M"}}
                       :title "Migration between world regions"}))

(def ^:private mtcars-variables [:mpg :cyl :disp :hp :drat :wt :qsec :vs :am :gear :carb])

(def correlation-network
  "Weights are |r| (graph weights are non-negative), the sign in `:r`."
  (let [names (mapv name mtcars-variables)
        r (stats/correlation-matrix (mapv (fn [k] (mapv k mtcars)) mtcars-variables))
        strong (mapv (fn [row] (mapv #(if (> (Math/abs (double %)) 0.7) % 0) row)) r)]
    (plot/network (mapv #(assoc % :r (:value %) :value (Math/abs (double (:value %))))
                        (graph/adjacency->edges strong names {:directed? false}))
                  {:nodes (mapv (fn [n] {:id n}) names) :rng (r/rng :mersenne 5) :labels true
                   :edge-mapping {:color #(if (pos? (double (:r %))) "positive" "negative")}
                   :scales {:color {:range {"positive" [33 102 172] "negative" [178 24 43]} :title "r"}}
                   :title "mtcars: |r| > 0.7"})))

(def images
  "Images of this demo: `[file-name plot options]`, saved to `results/network/` by [[cljplot2.demo/render!]]."
  [["les-miserables-850x750.png" les-miserables {:width 850 :height 750}]
   ["layouts-1000x500.png" layouts {:width 1000 :height 500}]
   ["migration-700x650.png" migration {:width 700 :height 650}]
   ["correlation-network-650x600.png" correlation-network {:width 650 :height 600}]])
