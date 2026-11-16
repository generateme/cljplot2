(ns cljplot2.demo.waffle
  "Part of a whole cycle 3 demo: waffles - portfolio weights by asset category (a 10 x 10 percent waffle), mtcars
  counts by cylinders (`:unit 1`, circle cells), and the portfolio by category faceted by security type."
  (:require [cljplot2.chart :as chart]
            [cljplot2.data.levels :as levels]
            [cljplot2.demo.parallel :refer [mtcars]]
            [cljplot2.demo.step10 :refer [portfolio]]))

(def percent
  {:data portfolio :mapping {:color :AssetCategory :value :Weight} :charts [(chart/waffle)]
   :labels {:title "Portfolio weights (one cell = 1%)"}})

(def counts
  {:data mtcars :mapping {:color (levels/categorical #(long (:cyl %)))} :charts [(chart/waffle {:unit 1 :columns 8 :shape :circle})]
   :labels {:title "mtcars by cylinders (one cell = one car)"}})

(def by-type
  {:data portfolio :mapping {:color :AssetCategory :value :Weight :facet :Type} :charts [(chart/waffle)]
   :labels {:title "Weights by category within each security type"}})

(def images
  "Images of this demo: `[file-name plot options]`, saved to `results/waffle/` by [[cljplot2.demo/render!]]."
  [["percent-600x450.png" percent {:width 600 :height 450}]
   ["counts-600x320.png" counts {:width 600 :height 320}]
   ["by-type-900x380.png" by-type {:width 900 :height 380}]])
