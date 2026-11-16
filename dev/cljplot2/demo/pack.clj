(ns cljplot2.demo.pack
  "Part of a whole cycle 2 demo: circle packing of the portfolio (`portfolio.csv`, R data.tree) - one level (weights by
  currency), three levels, wider gaps (`:padding 12`), leaves coloured by currency (legend), the top level hidden."
  (:require [cljplot2.chart :as chart]
            [cljplot2.demo.step10 :refer [portfolio mapping]]))

(def one-level
  {:data portfolio :mapping {:value :Weight :hierarchy [:Ccy]} :charts [(chart/pack)]
   :labels {:title "One level: weights by currency"}})

(def three-levels
  {:data portfolio :mapping mapping :charts [(chart/pack)] :labels {:title "Portfolio packing"}})

(def padded
  {:data portfolio :mapping mapping :charts [(chart/pack {:padding 12})] :labels {:title ":padding 12 (pixel gaps)"}})

(def by-currency
  {:data portfolio :mapping (assoc mapping :color :Ccy) :charts [(chart/pack)] :labels {:title "Leaves by currency"}})

(def hidden-top
  {:data portfolio :mapping mapping :charts [(chart/pack {:hidden-levels #{1}})]
   :labels {:title "Top level hidden (:hidden-levels #{1})"}})

(def images
  "Images of this demo: `[file-name plot options]`, saved to `results/pack/` by [[cljplot2.demo/render!]]."
  [["one-level-600x500.png" one-level {:width 600 :height 500}]
   ["three-levels-700x700.png" three-levels {:width 700 :height 700}]
   ["padded-700x700.png" padded {:width 700 :height 700}]
   ["by-currency-800x700.png" by-currency {:width 800 :height 700}]
   ["hidden-top-700x700.png" hidden-top {:width 700 :height 700}]])
