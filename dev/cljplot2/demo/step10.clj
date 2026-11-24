(ns cljplot2.demo.step10
  "Step 10 demo: rectangular treemaps of `portfolio.csv` (R data.tree).
  Squarified (default top-level colours), slice-and-dice, and coloured by currency (legend)."
  (:require [cljplot2.chart :as chart]
            [cljplot2.demo.data :as data]))

(def portfolio
  (mapv (fn [r] (update r :Weight #(when (seq %) (parse-double %)))) (data/resource-csv-rows "portfolio.csv")))

(def mapping {:value :Weight :hierarchy [:AssetCategory :AssetClass :SubAssetClass]})

(def squarified
  {:data portfolio :mapping mapping :charts [(chart/treemap)] :labels {:title "Portfolio (squarified)"}})

(def slice-dice
  {:data portfolio :mapping mapping :charts [(chart/treemap {:layout :slice-dice})] :labels {:title "Portfolio (slice-and-dice)"}})

(def by-currency
  {:data portfolio :mapping (assoc mapping :color :Ccy) :charts [(chart/treemap)] :labels {:title "Portfolio by currency"}})

(def by-type
  {:data portfolio :mapping (assoc mapping :facet :Type) :charts [(chart/treemap {:leaf-labels false})]
   :labels {:title "Portfolio faceted by type"}})

(def images
  "Images of this step: `[file-name plot options]`, saved to `results/step10/` by [[cljplot2.demo/render!]]."
  [["squarified-900x600.png" squarified {:width 900 :height 600}]
   ["slice-dice-900x600.png" slice-dice {:width 900 :height 600}]
   ["by-currency-900x600.png" by-currency {:width 900 :height 600}]
   ["by-type-900x450.png" by-type {:width 900 :height 450}]])
