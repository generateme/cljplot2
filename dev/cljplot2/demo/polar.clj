(ns cljplot2.demo.polar
  "Ranking cycle 3 demo: polar coordinates - circular barplots (labels, gaps, stacked, dodged), a circular lollipop,
  rings, chords vs arcs, pie / donut / half donut, pie labels (outer with leaders, centred shares, collisions)."
  (:require [clojure.string :as str]
            [fastmath.random :as r]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]))

(def sales
  (sort-by :value [{:country "United States" :value 12394} {:country "Russia" :value 6148} {:country "Germany" :value 1653}
                   {:country "France" :value 2162} {:country "United Kingdom" :value 1214} {:country "China" :value 1131}
                   {:country "Spain" :value 814} {:country "Netherlands" :value 1167} {:country "Italy" :value 660}
                   {:country "Israel" :value 1263}]))

(def circular
  (plot/arrange [{:data sales :mapping {:x :value :y :country}
                  :flip true :coord {:kind :polar}
                  :scales {:y {:domain (mapv :country sales)}}
                  :charts [(chart/bar {:color [0 114 178]})]
                  :labels {:title "Circular barplot (:flip, ordered)" :x :none :y :none}}
                 {:data sales :mapping {:x :value :y :country}
                  :flip true :coord {:kind :polar :inner 0.35}
                  :scales {:y {:domain (mapv :country sales) :axis false} :x {:axis false :domain [0 40000]}}
                  :charts [(chart/bar {:color [215 48 39] :stroke-color :none})
                           (chart/text {:mapping {:x :value :y :country :label :country} :rotation :radial})]
                  :labels {:title "Inner hole, radial labels" :x :none :y :none}}]
                {:columns 2}))

(def groups-data
  (let [rng (r/rng :mersenne 12)]
    (vec (for [[g n] [["A" 6] ["B" 5] ["C" 7]] i (range n)]
           {:group g :id (str g i) :value (+ 10 (r/irandom rng 90))}))))

(def gaps
  (let [levels (vec (mapcat (fn [g] (concat (map :id (filter #(= g (:group %)) groups-data)) [(str "_" g)])) ["A" "B" "C"]))]
    {:data groups-data :mapping {:x :value :y :id :color :group}
     :flip true :coord {:kind :polar :inner 0.2}
     :scales {:y {:domain levels :formatter (fn [v] (if (str/starts-with? (str v) "_") "" (str v)))}}
     :charts [(chart/bar)]
     :labels {:title "Gaps between groups (blank levels)" :x :none :y :none}}))

(def survey
  (let [rng (r/rng :mersenne 21)
        answers ["no" "maybe" "yes"]]
    (vec (for [g ["North" "South" "East" "West" "Centre" "Coast"] i (range 30)]
           {:region g :answer (answers (mod (+ i (r/irandom rng 3) (count g)) 3))}))))

(def positions
  (plot/arrange [{:data survey :mapping {:y :region :color :answer} :flip true :coord {:kind :polar :inner 0.15}
                  :charts [(chart/bar {:stat :count :position :stack})]
                  :labels {:title "Stacked circular bars" :x :none :y :none}}
                 {:data survey :mapping {:y :region :color :answer} :flip true :coord {:kind :polar :inner 0.15}
                  :charts [(chart/bar {:stat :count})]
                  :labels {:title "Dodged circular bars" :x :none :y :none}}]
                {:columns 2}))

(def lollipop-rings
  (plot/arrange [{:data sales :mapping {:x :country :y :value} :coord {:kind :polar}
                  :scales {:x {:domain (mapv :country sales)}}
                  :charts [(chart/segment {:yend 0 :color [150 150 150]}) (chart/scatter {:size 8 :color [215 48 39]})]
                  :labels {:title "Circular lollipop" :x :none :y :none}}
                 {:data (take 5 (reverse sales)) :mapping {:x :value :y :country} :coord {:kind :polar :inner 0.3 :span 270}
                  :charts [(chart/bar {:width 0.7})]
                  :labels {:title "Rings (bars without :flip, 270°)" :x :none :y :none}}]
                {:columns 2}))

(def months
  (vec (for [m (range 12)] {:month m :value (+ 10 (* 6 (Math/sin (/ (* m Math/PI) 6))))})))

(def chords-arcs
  (plot/arrange (vec (for [arcs [false true]]
                       {:data months :mapping {:x :month :y :value} :coord {:kind :polar :arcs arcs}
                        :scales {:x {:domain [0 12] :ticks (vec (range 12))}}
                        :charts [(chart/line {:line-width 2}) (chart/scatter {:size 7})]
                        :labels {:title (if arcs "Line with :arcs true" "Line with chords (default)") :x :none :y :none}}))
                {:columns 2}))

(def shares [{:part "a" :n 45} {:part "b" :n 25} {:part "c" :n 18} {:part "d" :n 12}])

(def pies
  (plot/arrange [{:data shares :mapping {:x :n :color :part} :coord {:kind :polar}
                  :scales {:x {:axis false}}
                  :charts [(chart/bar {:position :stack :width 1.0})]
                  :labels {:title "Pie" :x :none :y :none}}
                 {:data shares :mapping {:x :n :color :part} :coord {:kind :polar :inner 0.5}
                  :scales {:x {:axis false}}
                  :charts [(chart/bar {:position :stack :width 1.0 :value-label true})]
                  :labels {:title "Donut" :x :none :y :none}}
                 {:data shares :mapping {:x :n :color :part} :coord {:kind :polar :inner 0.5 :span 180 :start -90}
                  :scales {:x {:axis false}}
                  :charts [(chart/bar {:position :stack :width 1.0})]
                  :labels {:title "Half donut (:span 180)" :x :none :y :none}}]
                {:columns 3}))

(def pie-label
  "Pie label: the part's name and its share of the whole."
  #(format "%s\n%.0f%%" (get-in % [:group :color]) (* 100 (:share %))))

(def labelled-pies
  (plot/arrange [{:data shares :mapping {:x :n :color :part} :coord {:kind :polar} :scales {:x {:axis false}} :legend nil
                  :charts [(chart/bar {:position :stack :width 1.0 :value-label pie-label :label-position :outer
                                       :label-leader true})]
                  :labels {:title "Outer labels with leaders" :x :none :y :none}}
                 {:data shares :mapping {:x :n :color :part} :coord {:kind :polar :inner 0.5} :scales {:x {:axis false}}
                  :charts [(chart/bar {:position :stack :width 1.0 :value-label #(format "%.0f%%" (* 100 (:share %)))})]
                  :labels {:title "Donut: centred shares" :x :none :y :none}}]
                {:columns 2}))

(def many-slices
  {:data (map (fn [i] {:part (str "p" i) :n (if (< i 3) (* 30 (- 3 i)) (inc (mod i 3)))}) (range 20))
   :mapping {:x :n :color :part} :coord {:kind :polar} :scales {:x {:axis false}} :legend nil
   :charts [(chart/bar {:position :stack :width 1.0 :value-label #(get-in % [:group :color]) :label-position :outer})]
   :labels {:title "20 slices: colliding labels skipped" :x :none :y :none}})

(def images
  "Images of this demo: `[file-name plot options]`, saved to `results/polar/` by [[cljplot2.demo/render!]]."
  [["circular-900x450.png" circular {:width 900 :height 450}]
   ["gaps-500x500.png" gaps {:width 500 :height 500}]
   ["positions-900x450.png" positions {:width 900 :height 450}]
   ["lollipop-rings-900x450.png" lollipop-rings {:width 900 :height 450}]
   ["chords-arcs-800x400.png" chords-arcs {:width 800 :height 400}]
   ["pies-900x320.png" pies {:width 900 :height 320}]
   ["labelled-pies-900x420.png" labelled-pies {:width 900 :height 420}]
   ["many-slices-500x500.png" many-slices {:width 500 :height 500}]])
