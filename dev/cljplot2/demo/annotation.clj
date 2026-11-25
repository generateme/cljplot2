(ns cljplot2.demo.annotation
  "Annotation demo (cycle 4): rule, rect, text charts (literal and data mode), strip n labels."
  (:require [fastmath.stats :as stats]
            [cljplot2.chart :as chart]
            [cljplot2.demo.data :as data]
            [cljplot2.core :as plot]))

(def barley (data/json-rows "barley.json"))
(def faithful (data/json-rows "faithful.json"))

(def eruptions (map :eruptions faithful))

(def density-mean
  (let [mean (stats/mean eruptions)]
    {:data faithful
     :mapping {:x :eruptions}
     :charts [(chart/density {:fill [0 114 178 60]})
              (chart/rule {:x mean :dash [5.0 3.0] :color :red})
              (chart/text {:x mean :y [:panel 0.95] :text (format "mean = %.2f" mean) :anchor [:left :top]
                           :offset [4 0] :color :red})]
     :labels {:title "Density with a mean rule and a label (D4)" :x "eruptions"}}))

(def histogram-tail
  (let [q95 (stats/quantile eruptions 0.95)]
    {:data faithful
     :mapping {:x :eruptions}
     :charts [(chart/rect {:x [q95 nil] :fill [215 48 39 70]})
              (chart/histogram {:bins 30})
              (chart/rule {:x q95 :color [215 48 39]})
              (chart/text {:x q95 :y [:panel 0.9] :text "95%" :anchor [:right :middle] :offset [-4 0]})]
     :labels {:title "Histogram with a shaded tail (rect behind the bars, H8)" :x "eruptions"}}))

(def box-n
  (plot/arrange [{:data barley :mapping {:x :yield :y :site} :charts [(chart/box {:n-label true})]
                  :labels {:title "Box with n labels" :x "yield"}}
                 {:data barley :mapping {:x :yield :y :site :color :year} :scales {:color {:kind :ordinal}}
                  :flip true :charts [(chart/box {:n-label true :n-format "%d"})]
                  :labels {:title "Flipped, dodged, :n-format" :x "yield"}}]
                {:columns 2}))

(def violin-n
  {:data barley
   :mapping {:x :yield :y :variety}
   :charts [(chart/violin {:n-label true :inner :quartiles}) (chart/rule {:x 30 :dash [4.0]})]
   :labels {:title "Violin with n labels and a reference rule (V4)" :x "yield"}})

(def site-year-means
  (for [[[site year] rows] (group-by (juxt :site :year) barley)]
    {:site site :year year :mean (stats/mean (map :yield rows))}))

(def faceted
  {:data barley
   :mapping {:x :yield :facet :site :color :year}
   :scales {:color {:kind :ordinal}}
   :charts [(chart/rect {:x [20 30]})
            (chart/density)
            (chart/rule {:data site-year-means :mapping {:x :mean :color :year} :dash [4.0 2.0]})
            (chart/text {:x 25 :y [:panel 0.92] :text "20-30"})]
   :labels {:title "Facets: literal rect and text in every panel, per-panel mean rules by year" :x "yield"}})

(def scatter-abline
  (let [xs (map :waiting faithful) ys eruptions
        slope (/ (stats/covariance xs ys) (stats/variance xs))
        intercept (- (stats/mean ys) (* slope (stats/mean xs)))]
    {:data faithful
     :mapping {:x :waiting :y :eruptions}
     :charts [(chart/scatter)
              (chart/rule {:slope slope :intercept intercept :color :red :line-width 2})
              (chart/rule {:y 3 :dash [2.0 2.0]})
              (chart/text {:x [:panel 0.03] :y [:panel 0.97] :anchor [:left :top]
                           :text (format "y = %.3f + %.4f x" intercept slope)})]
     :labels {:title "Scatter with an ab line, a y rule and a corner label"}}))

(def images
  "Images of this demo: `[file-name plot options]`, saved to `results/annotation/` by [[cljplot2.demo/render!]]."
  [["density-mean-600x400.png" density-mean {:width 600 :height 400}]
   ["histogram-tail-600x400.png" histogram-tail {:width 600 :height 400}]
   ["box-n-1000x500.png" box-n {:width 1000 :height 500}]
   ["violin-n-700x600.png" violin-n {:width 700 :height 600}]
   ["faceted-900x600.png" faceted {:width 900 :height 600}]
   ["scatter-abline-600x450.png" scatter-abline {:width 600 :height 450}]])
