(ns cljplot2.demo.vega-distributions
  "Vega cycle 2 demo (distributions and composites): Wilkinson, dot-density, wheat and quantile dot plots, a
  pre-aggregated box plot and horizon charts - on the inline data of the Vega / Vega-Lite examples (BSD-3) and seeded
  generated data."
  (:require [fastmath.core :as m]
            [fastmath.random :as r]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]))

(def wilkinson
  "Vega-Lite \"Wilkinson Dot Plot\": a stack of dots per value."
  {:data (map (fn [v] {:v v}) [1 1 1 1 1 1 1 1 1 1 2 2 2 3 3 4 4 4 4 4 4])
   :mapping {:x :v}
   :charts [(chart/dotplot {:binwidth 1})]
   :labels {:title "Wilkinson dot plot" :x "data"}})

(def ^:private dot-plot-points
  "The `points` values of the Vega \"Dot Plot\" example."
  [6.3 2.1 9.1 15.8 5.2 10.9 8.3 11.0 3.2 7.6 6.3 8.6 6.6 9.5 4.8 12.0 3.3 11.0 4.7 10.4 7.4 2.1 7.7 17.9 6.1 8.2 8.4
   11.9 10.8 13.8 14.3 15.2 10.0 11.9 6.5 7.5 10.6 7.4 8.4 5.7 4.9 3.2 8.1 11.0 4.9 13.2 9.7 12.8])

(def dot-density
  "Vega \"Dot Plot\": Wilkinson dot density bins of width 0.65, up from the baseline (left) and centred (right)."
  (plot/arrange
   (for [dir [:up :center]]
     {:data (map (fn [v] {:v v}) dot-plot-points)
      :mapping {:x :v}
      :charts [(chart/dotplot {:binwidth 0.65 :stack-dir dir})]
      :labels {:title (str "dot density, :stack-dir " dir) :x "value"}})
   {:columns 2}))

(def wheat
  "Vega \"Wheat Plot\": fixed bins of 0.075 over [-1 1], every dot at its own value, stacked within its bin (seeded
  normal values in place of `normal-2d.json`)."
  (let [rng (r/rng :mersenne 21)
        us (filterv #(< -1.0 (double %) 1.0) (repeatedly 500 #(r/grandom rng 0.0 0.35)))]
    {:data (map (fn [u] {:u u}) us)
     :mapping {:x :u}
     :charts [(chart/dotplot {:method :histogram :binwidth 0.075 :origin -1.0 :at :value
                              :shape \o :color [70 130 180] :stroke-width 0.7})]
     :labels {:title "Wheat plot" :x "u"}}))

(def quantile-dots
  "Vega \"Quantile Dot Plot\": 20 quantiles of a log-normal arrival time as dots; the dots below the 9 minute
  threshold coloured, the region under it marked by a rect (the user's recipe for the selection)."
  (let [d (r/distribution :log-normal {:scale (m/log 11.4) :shape 0.2})
        threshold 9.0
        qs (mapv #(r/icdf d (/ (+ 0.5 (double %)) 20.0)) (range 20))]
    {:data (map (fn [q] {:q q :side (if (< (double q) threshold) "before" "after")}) qs)
     :mapping {:x :q :color :side}
     :charts [(chart/rect {:x [6.0 threshold] :fill [200 30 30 30]})
              (chart/dotplot {:binwidth 0.6})]
     :scales {:color {:domain ["before" "after"] :range [[178 34 34] [70 130 180]]}}
     :labels {:title "Quantile dot plot: arrival in minutes" :x "minutes"}}))

(def ^:private penguin-boxes
  "Statistics of the Vega-Lite \"Box Plot with Pre-Calculated Summaries\" example (penguin body mass, g)."
  [{:species "Adelie" :lower 2850 :q1 3350 :median 3700 :q3 4000 :upper 4775 :outliers []}
   {:species "Chinstrap" :lower 2700 :q1 3487.5 :median 3700 :q3 3950 :upper 4800 :outliers [2700 4800]}
   {:species "Gentoo" :lower 3950 :q1 4700 :median 5000 :q3 5500 :upper 6300 :outliers []}])

(def preaggregated-box
  "Vega-Lite \"Box Plot with Pre-Calculated Summaries\": boxes from given statistics (`:stat :identity`)."
  {:data penguin-boxes
   :mapping {:y :species :color :species :lower :lower :q1 :q1 :median :median :q3 :q3 :upper :upper
             :outliers :outliers}
   :charts [(chart/box {:stat :identity})]
   :scales {:color {:legend false}}
   :labels {:title "Body mass of penguin species (g)" :x :none :y :none}})

(def ^:private horizon-table
  "The `table` values of the Vega \"Horizon Graph\" example."
  (map (fn [x y] {:x x :y y}) (range 1 21) [28 55 43 91 81 53 19 87 52 48 24 49 87 66 17 27 68 16 49 15]))

(def horizon
  "Vega \"Horizon Graph\": two bands of the table."
  (plot/horizon horizon-table {:title "Horizon graph, 2 bands"}))

(def horizon-series
  "A horizon chart of three seeded random walks, negatives mirrored in red, 3 bands, a panel per series."
  (let [rng (r/rng :mersenne 5)
        walk (fn [] (reductions + (repeatedly 150 #(r/grandom rng))))]
    (plot/horizon (for [s ["alpha" "beta" "gamma"] [t v] (map-indexed vector (walk))] {:t t :v v :s s})
                  {:x :t :y :v :series :s :bands 3 :title "Horizon chart of random walks, 3 bands"})))

(def images
  "Images of this demo: `[file-name plot options]`, saved to `results/vega-distributions/` by
  [[cljplot2.demo/render!]]."
  [["wilkinson-500x220.png" wilkinson {:width 500 :height 220}]
   ["dot-density-900x260.png" dot-density {:width 900 :height 260}]
   ["wheat-700x320.png" wheat {:width 700 :height 320}]
   ["quantile-dots-600x260.png" quantile-dots {:width 600 :height 260}]
   ["preaggregated-box-600x300.png" preaggregated-box {:width 600 :height 300}]
   ["horizon-600x160.png" horizon {:width 600 :height 160}]
   ["horizon-series-800x360.png" horizon-series {:width 800 :height 360}]])
