(ns cljplot2.demo.vega-marks
  "Vega cycle 1 demo (marks): rounded bar corners (stacked, end-only, polar), isotype rows of path and emoji shapes,
  an image scatter and trails (Vega-Lite trail and comet charts) - on the datasets of `notebooks/data` and generated
  data. The icons are this demo's own path data."
  (:require [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.demo.data :as data]))

(def ^:private month-names ["Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"])

(def ^:private seattle
  (mapv (fn [{:keys [date weather]}] {:month (month-names (dec (parse-long (subs date 5 7)))) :weather weather})
        (data/csv-rows "seattle-weather.csv")))

(def rounded-stacked
  "Vega-Lite \"Stacked Bar Chart with Rounded Corners\": days of each weather type per month (Seattle 2012-2015),
  every segment rounded."
  {:data seattle :mapping {:x :month :color :weather}
   :charts [(chart/bar {:stat :count :position :stack :corner-radius 3 :stroke-color :white :stroke-width 1})]
   :scales {:x {:domain month-names}}
   :labels {:title "Weather in Seattle - rounded stacked bars" :x "month" :y "days"}})

(def ^:private radial-values [12 23 47 6 52 19])

(def rounded-radial
  "Vega-Lite \"Radial Plot\" with rounded corners: values as radial bars of a polar plot, only the value end
  rounded (left), every corner of a dodged donut (right)."
  (plot/arrange
   [{:data (map-indexed (fn [i v] {:k (str "v" i) :v v}) radial-values) :mapping {:x :k :y :v :color :k}
     :coord {:kind :polar :inner 0.2} :charts [(chart/bar {:corner-radius-end 8})]
     :labels {:title ":corner-radius-end 8"}}
    {:data (for [[g vs] [["a" [3 5 2]] ["b" [2 4 6]]] [i v] (map-indexed vector vs)] {:k (str "k" i) :g g :v v})
     :mapping {:x :k :y :v :color :g} :coord {:kind :polar :inner 0.35}
     :charts [(chart/bar {:position :dodge :corner-radius 6})]
     :labels {:title ":corner-radius 6, dodged"}}]
   {:columns 2}))

(def ^:private icons
  "Own simple icons as SVG path data: a house, a tree and a drop."
  {"houses" {:path "M0 9 L10 0 L20 9 L17 9 L17 20 L12 20 L12 14 L8 14 L8 20 L3 20 L3 9 Z"}
   "trees" {:path "M10 0 L16 7 L13 7 L18 14 L11.5 14 L11.5 20 L8.5 20 L8.5 14 L2 14 L7 7 L4 7 Z"}
   "drops" {:path "M10 0 C14 7 18 10 18 14 A8 8 0 0 1 2 14 C2 10 6 7 10 0 Z"}})

(def ^:private emoji
  "Emoji text shapes (drawn from the bundled monochrome Noto Emoji font)."
  {"houses" {:text "🏠"} "trees" {:text "🌳"} "drops" {:text "💧"}})

(def ^:private village-counts
  "Illustrative counts per village and item."
  {["North" "houses"] 7 ["North" "trees"] 10 ["North" "drops"] 3
   ["South" "houses"] 4 ["South" "trees"] 3 ["South" "drops"] 9})

(def ^:private isotype-rows
  (for [[[village item] n] (sort village-counts) i (range 1 (inc (long n)))]
    {:i i :row (str village " - " item) :item item}))

(defn- isotype
  "Isotype rows: one icon per unit along x, a row per village and item, icon `shapes` by item."
  [title shapes]
  {:data isotype-rows :mapping {:x :i :y :row :shape :item :color :item}
   :charts [(chart/scatter {:size 22 :stroke-width 0})]
   :scales {:shape {:range (mapv shapes ["drops" "houses" "trees"])} :x {:axis false}}
   :labels {:title title :x nil :y nil}})

(def isotype-icons
  "Vega-Lite \"Isotype Dot Plot\" with this demo's path icons (left) and emoji (right)."
  (plot/arrange [(isotype "path icons" icons) (isotype "emoji" emoji)] {:columns 2}))

(defn- disc-image
  "A `side` x `side` image: a disc shaded from `color` to white (light from the top left), transparent outside."
  [color side]
  (let [cv (c2d/canvas side side)
        side (double side)]
    (c2d/with-canvas [g cv]
      (doseq [i (range 12)
              :let [t (/ (double i) 12.0)
                    d (* side (- 1.0 t))
                    off (* 0.15 side t)]]
        (c2d/set-color g (c/lerp color :white (* 0.8 t)))
        (c2d/ellipse g (- (* 0.5 side) off) (- (* 0.5 side) off) d d)))
    (c2d/get-image cv)))

(def image-scatter
  "Vega-Lite \"Image-based Scatter Plot\": points drawn as images (shaded discs made here), one image per group."
  (let [rng (r/rng :mersenne 11)
        groups (update-vals {"red" [200 50 40] "green" [40 150 70] "blue" [50 90 200]} #(hash-map :image (disc-image % 48)))]
    {:data (for [[g cx cy] [["red" 2 3] ["green" 5 6] ["blue" 7 2]] _ (range 8)]
             {:x (+ (double cx) (r/grandom rng)) :y (+ (double cy) (r/grandom rng)) :g g})
     :mapping {:x :x :y :y :shape :g}
     :charts [(chart/scatter {:size 26})]
     :scales {:shape {:range (mapv groups ["blue" "green" "red"])}}
     :labels {:title "Image shapes" :x "x" :y "y"}}))

(def trail
  "Vega-Lite \"Line Chart with Varying Size (trail)\": a Lissajous path whose width follows the speed."
  (let [ts (range 0.0 6.3 0.05)]
    {:data (for [t ts :let [t (double t)]]
             {:x (m/sin (* 3.0 t)) :y (m/sin (* 2.0 t))
              :speed (m/hypot (* 3.0 (m/cos (* 3.0 t))) (* 2.0 (m/cos (* 2.0 t))))})
     :mapping {:x :x :y :y :size :speed}
     :charts [(chart/line {:trail true :sort false :color [40 90 160]})]
     :scales {:size {:range [1 14]}}
     :aspect :iso
     :labels {:title "Trail: width by speed" :x "x" :y "y"}}))

(def ^:private barley (data/json-rows "barley.json"))

(def comet
  "Vega-Lite \"Comet Chart\": barley yield per variety from 1931 (thin) to 1932 (wide), a panel per site, coloured
  by the direction of the change."
  (let [change (into {} (for [[k rows] (group-by (juxt :site :variety) barley)
                              :let [by-year (into {} (map (juxt :year :yield) rows))]]
                          [k (if (> (double (by-year 1932)) (double (by-year 1931))) "increase" "decrease")]))]
    {:data (map #(assoc % :change (change [(:site %) (:variety %)])) (sort-by :year barley))
     :mapping {:x :yield :y :variety :size :year :color :change :group :variety :facet :site}
     :charts [(chart/line {:trail true :sort false})]
     :scales {:size {:range [1 10] :ticks [1931 1932] :title "year"}}
     :facets {:columns 3}
     :labels {:title "Comet chart: barley yield 1931 to 1932" :x "yield" :y nil}}))

(def images
  "Images of this demo: `[file-name plot options]`, saved to `results/vega-marks/` by [[cljplot2.demo/render!]]."
  [["rounded-stacked-700x420.png" rounded-stacked {:width 700 :height 420}]
   ["rounded-radial-900x420.png" rounded-radial {:width 900 :height 420}]
   ["isotype-900x360.png" isotype-icons {:width 900 :height 360}]
   ["image-scatter-600x450.png" image-scatter {:width 600 :height 450}]
   ["trail-500x450.png" trail {:width 500 :height 450}]
   ["comet-900x600.png" comet {:width 900 :height 600}]])
