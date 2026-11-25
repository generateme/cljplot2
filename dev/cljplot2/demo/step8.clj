(ns cljplot2.demo.step8
  "Step 8 demo: statistical charts.
  Fig 1.1 (histogram), Fig 1.2 (density), Fig 1.3 (density grouped by score), densities by score and gender, grouped histograms (overlay, dodge, mirror),
  line and ECDF."
  (:require [fastmath.random :as r]
            [cljplot2.chart :as chart]
            [cljplot2.demo.data :as data]))

(def chem97 (data/chem97-rows))
(def faithful (data/json-rows "faithful.json"))

(def fig-1-1
  {:data chem97
   :mapping {:x :gcsescore :facet :score}
   :charts [(chart/histogram)]
   :labels {:x "gcsescore"}})

(def fig-1-2
  {:data chem97
   :mapping {:x :gcsescore :facet :score}
   :charts [(chart/density)]
   :labels {:x "gcsescore"}})

(def fig-1-3
  "LMDVR Fig 1.3: one panel, densities grouped by score."
  {:data chem97
   :mapping {:x :gcsescore :color :score}
   :scales {:color {:kind :ordinal}}
   :charts [(chart/density)]
   :labels {:x "gcsescore"}})

(def density-score-gender
  {:data chem97
   :mapping {:x :gcsescore :facet :score :color :gender}
   :charts [(chart/density)]
   :labels {:x "gcsescore"}})

(def two-groups
  (let [rng (r/rng :mersenne 42)]
    (vec (concat (for [_ (range 400)] {:v (r/grandom rng 10 2) :g "a"})
                 (for [_ (range 400)] {:v (r/grandom rng 14 3) :g "b"})))))

(defn- grouped
  ([position] (grouped position {}))
  ([position extra]
   {:data (if (:uneven extra) (concat two-groups (take 400 two-groups)) two-groups)
    :mapping {:x :v :color :g}
    :charts [(chart/histogram (merge {:position position :y (get extra :y :density) :bins 30} (dissoc extra :uneven)))]
    :labels {:title (str "Histogram :position " position (when (:symmetric extra) ", :symmetric")) :x "v"}}))

(def lines
  {:data (for [[slope g] (map vector [0.5 1.0 1.5] ["a" "b" "c"]) x (shuffle (range 20))]
           {:x x :y (+ (* slope x) (mod (* x 7) 5)) :g g})
   :mapping {:x :x :y :y :color :g}
   :charts [(chart/line {:line-width 2}) (chart/scatter {:size 5})]
   :labels {:title "Lines (sorted by x) with points" :x "x" :y "y"}})

(def ecdf
  {:data faithful
   :mapping {:x :eruptions :color #(if (> (:waiting %) 70) "long wait" "short wait")}
   :charts [(chart/ecdf {:line-width 1.5})]
   :labels {:title "ECDF of eruptions by waiting" :x "eruptions"}})

(def group-vs-color
  {:data (for [id (range 6) x (range 15)]
           {:x x :y (+ (* x (if (even? id) 1.0 -0.5)) (* 3 id) (mod (* x (inc id)) 4)) :id id :kind (if (even? id) "rising" "falling")})
   :mapping {:x :x :y :y :group :id :color :kind}
   :charts [(chart/line {:line-width 1.5})]
   :labels {:title ":group splits series, :color styles them" :x "x" :y "y"}})

(def area
  {:data (for [x (range 40)] {:x x :y (* 10 (Math/sin (/ x 6.0)))})
   :mapping {:x :x :y :y}
   :charts [(chart/line {:fill [0 114 178 90] :line-width 1.5})]
   :labels {:title "Area: line with :fill (down to y = 0)" :x "x" :y "y"}})

(def images
  "Images of this step: `[file-name plot options]`, saved to `results/step8/` by [[cljplot2.demo/render!]]."
  [["fig-1-1-800x600.png" fig-1-1 {:width 800 :height 600}]
   ["fig-1-2-800x600.png" fig-1-2 {:width 800 :height 600}]
   ["fig-1-3-800x400.png" fig-1-3 {:width 800 :height 400}]
   ["density-score-gender-800x600.png" density-score-gender {:width 800 :height 600}]
   ["hist-overlay-600x400.png" (grouped :overlay) {:width 600 :height 400}]
   ["hist-dodge-600x400.png" (grouped :dodge) {:width 600 :height 400}]
   ["hist-mirror-600x400.png" (grouped :mirror) {:width 600 :height 400}]
   ["hist-mirror-count-600x400.png" (grouped :mirror {:y :count :uneven true}) {:width 600 :height 400}]
   ["hist-mirror-symmetric-600x400.png" (grouped :mirror {:y :count :uneven true :symmetric true}) {:width 600 :height 400}]
   ["lines-600x400.png" lines {:width 600 :height 400}]
   ["ecdf-600x400.png" ecdf {:width 600 :height 400}]
   ["group-vs-color-600x400.png" group-vs-color {:width 600 :height 400}]
   ["area-600x400.png" area {:width 600 :height 400}]])
