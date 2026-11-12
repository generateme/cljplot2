(ns cljplot2.demo.step10
  "Step 10 demo: rectangular treemaps of `portfolio.csv` (R data.tree), saved to `results/step10/`.
  Squarified (default top-level colours), slice-and-dice, and coloured by currency (legend)."
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure2d.core :as c2d]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]))

(def portfolio
  (let [[header & rows] (csv/read-csv (slurp (io/resource "portfolio.csv")))
        ks (map keyword header)]
    (mapv (fn [r] (update (zipmap ks r) :Weight #(when (seq %) (parse-double %)))) rows)))

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

(defn -main [& _]
  (doseq [[file-name p options] [["squarified-900x600.png" squarified {:width 900 :height 600}]
                                 ["slice-dice-900x600.png" slice-dice {:width 900 :height 600}]
                                 ["by-currency-900x600.png" by-currency {:width 900 :height 600}]
                                 ["by-type-900x450.png" by-type {:width 900 :height 450}]]
          :let [path (str "results/step10/" file-name)]]
    (c2d/save (plot/render p options) path)
    (println "saved" path)))
