(ns cljplot2.demo.step8
  "Step 8 demo: statistical charts, saved to `results/step8/`.
  Fig 1.1 (histogram), Fig 1.2 (density), Fig 1.3 (density by gender), grouped histograms (overlay, dodge, mirror),
  line and ECDF."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure2d.core :as c2d]
            [fastmath.random :as r]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]))

(defn- read-json [file] (json/read-str (slurp (io/resource file)) :key-fn keyword))

(def chem97 (read-json "chem97.json"))
(def faithful (read-json "faithful.json"))

(def fig-1-1
  {:data chem97
   :mapping {:x :gcsescore :facet :score}
   :charts [(chart/histogram)]
   :labels {:x "gcsescore"}})

(def fig-1-2
  {:data chem97
   :mapping {:x :gcsescore :facet :score}
   :charts [(chart/density {:ref true})]
   :labels {:x "gcsescore"}})

(def fig-1-3
  {:data chem97
   :mapping {:x :gcsescore :facet :score :color :gender}
   :charts [(chart/density {:ref true})]
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
   :charts [(chart/line {:width 2}) (chart/scatter {:size 5})]
   :labels {:title "Lines (sorted by x) with points" :x "x" :y "y"}})

(def ecdf
  {:data faithful
   :mapping {:x :eruptions :group #(if (> (:waiting %) 70) "long wait" "short wait") :color #(if (> (:waiting %) 70) "long wait" "short wait")}
   :charts [(chart/ecdf {:width 1.5})]
   :labels {:title "ECDF of eruptions by waiting" :x "eruptions"}})

(defn -main [& _]
  (doseq [[file-name p options] [["fig-1-1-800x600.png" fig-1-1 {:width 800 :height 600}]
                                 ["fig-1-2-800x600.png" fig-1-2 {:width 800 :height 600}]
                                 ["fig-1-3-800x600.png" fig-1-3 {:width 800 :height 600}]
                                 ["hist-overlay-600x400.png" (grouped :overlay) {:width 600 :height 400}]
                                 ["hist-dodge-600x400.png" (grouped :dodge) {:width 600 :height 400}]
                                 ["hist-mirror-600x400.png" (grouped :mirror) {:width 600 :height 400}]
                                 ["hist-mirror-count-600x400.png" (grouped :mirror {:y :count :uneven true}) {:width 600 :height 400}]
                                 ["hist-mirror-symmetric-600x400.png" (grouped :mirror {:y :count :uneven true :symmetric true}) {:width 600 :height 400}]
                                 ["lines-600x400.png" lines {:width 600 :height 400}]
                                 ["ecdf-600x400.png" ecdf {:width 600 :height 400}]]
          :let [path (str "results/step8/" file-name)]]
    (c2d/save (plot/render p options) path)
    (println "saved" path)))
