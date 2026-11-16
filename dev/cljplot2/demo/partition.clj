(ns cljplot2.demo.partition
  "Part of a whole cycle 1 demo: the portfolio (`portfolio.csv`, R data.tree) as an icicle, a sunburst (partition under
  a polar coord), a sunburst with the root ring and a hole, coloured by currency (legend), and a treemap with
  name + share labels (a label function)."
  (:require [cljplot2.chart :as chart]
            [cljplot2.demo.step10 :refer [portfolio mapping]]))

(def icicle
  {:data portfolio :mapping mapping :charts [(chart/partition)] :labels {:title "Portfolio icicle"}})

(def sunburst
  {:data portfolio :mapping mapping :coord {:kind :polar} :charts [(chart/partition)] :labels {:title "Portfolio sunburst"}})

(def sunburst-root
  {:data portfolio :mapping mapping :coord {:kind :polar :inner 0.3} :charts [(chart/partition {:root true})]
   :labels {:title "Root ring, :inner 0.3"}})

(def by-currency
  {:data portfolio :mapping (assoc mapping :color :Ccy) :coord {:kind :polar} :charts [(chart/partition)]
   :labels {:title "Leaves by currency"}})

(def shares
  {:data portfolio :mapping mapping
   :charts [(chart/treemap {:leaf-labels #(format "%s\n%.1f%%" (:name %) (* 100 (:share %)))})]
   :labels {:title "Treemap: name and share of the total"}})

(def images
  "Images of this demo: `[file-name plot options]`, saved to `results/partition/` by [[cljplot2.demo/render!]]."
  [["icicle-800x500.png" icicle {:width 800 :height 500}]
   ["sunburst-700x700.png" sunburst {:width 700 :height 700}]
   ["sunburst-root-700x700.png" sunburst-root {:width 700 :height 700}]
   ["by-currency-800x700.png" by-currency {:width 800 :height 700}]
   ["shares-900x600.png" shares {:width 900 :height 600}]])
