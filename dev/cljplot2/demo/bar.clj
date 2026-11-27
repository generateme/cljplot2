(ns cljplot2.demo.bar
  "Ranking cycle 1 demo: bar charts - stats, orientation, order, width, error bars, labels, positions, mirror."
  (:require [fastmath.random :as r]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]))

(def sales
  "Weapon sales per country (illustrative values)."
  [{:country "United States" :value 12394} {:country "Russia" :value 6148} {:country "Germany" :value 1653}
   {:country "France" :value 2162} {:country "United Kingdom" :value 1214} {:country "China" :value 1131}
   {:country "Spain" :value 814} {:country "Netherlands" :value 1167} {:country "Italy" :value 660}
   {:country "Israel" :value 1263}])

(def ordered (sort-by :value sales))

(def basic
  (plot/arrange [{:data sales :mapping {:x :value :y :country}
                  :charts [(chart/bar)] :labels {:title "Horizontal (default)"}}
                 {:data ordered :mapping {:x :value :y :country}
                  :scales {:y {:domain (mapv :country ordered)}}
                  :flip true
                  :charts [(chart/bar {:color [215 48 39] :stroke-color :none :width 0.5})]
                  :labels {:title "Vertical (:flip), ordered, :width 0.5"}}]
                {:columns 2}))

(def samples
  "Measurements of five treatments, eight replicates each, two sexes."
  (let [rng (r/rng :mersenne 3)]
    (vec (for [[t mu] [["A" 4.0] ["B" 6.5] ["C" 5.0] ["D" 8.0] ["E" 3.0]]
               sex ["female" "male"]
               _ (range 8)]
           {:treatment t :sex sex :value (+ mu (if (= sex "male") 0.8 0.0) (r/grandom rng 0.0 1.0))}))))

(def error-bars
  (plot/arrange [{:data samples :mapping {:x :value :y :treatment}
                  :flip true
                  :charts [(chart/bar {:stat :mean})
                           (chart/extent {:method :stddev :center :none :color [40 40 40] :line-width 1})]
                  :labels {:title "Mean bars, sd error bars" :x "mean"}}
                 {:data samples :mapping {:x :value :y :treatment :color :sex}
                  :flip true
                  :charts [(chart/bar {:stat :mean})
                           (chart/extent {:method :stddev :center :none :color [40 40 40] :line-width 1})]
                  :labels {:title "Dodged groups share their error bars' slots" :x "mean"}}]
                {:columns 2}))

(def survey
  "Answers of 120 people: their group and answer."
  (let [rng (r/rng :mersenne 21)
        answers ["no" "maybe" "yes"]]
    (vec (for [g ["North" "South" "East" "West"] i (range 30)]
           {:region g :answer (answers (mod (+ i (r/irandom rng 3) (count g)) 3)) :id i}))))

(def positions
  (plot/arrange (vec (for [p [:dodge :stack :normalize :overlay]]
                       {:data survey :mapping {:y :region :color :answer}
                        :flip true
                        :charts [(chart/bar {:stat :count :position p})]
                        :labels {:title (str ":stat :count, :position " p)}}))
                {:columns 2}))

(def pyramid-data
  "Population per age group, sex and region (illustrative)."
  (let [rng (r/rng :mersenne 9)]
    (vec (for [[i age] (map-indexed vector ["0-14" "15-29" "30-44" "45-59" "60-74" "75+"])
               sex ["female" "male"]
               region ["urban" "rural"]]
           {:age age :sex sex :region region
            :people (+ (* (- 7 i) (if (= region "urban") 90 60)) (r/irandom rng 40))}))))

(def pyramid
  (plot/arrange [{:data pyramid-data :mapping {:x :people :y :age :color :sex}
                  :charts [(chart/bar {:mirror :color :value-label true})]
                  :labels {:title ":mirror :color, value labels"}}
                 {:data pyramid-data :mapping {:x :people :y :age :color :sex :group :region}
                  :charts [(chart/bar {:mirror :color :position :stack})]
                  :labels {:title "regions stacked on each side"}}
                 {:data pyramid-data :mapping {:x :people :y :age :color :region :group :sex}
                  :charts [(chart/bar {:mirror :group :position :normalize})]
                  :labels {:title ":mirror :group, :normalize" :x "male | share | female"}}]
                {:columns 3}))

(def widths
  (let [data [{:item "small" :value 3 :n 10} {:item "medium" :value 5 :n 40} {:item "large" :value 4 :n 90}]]
    (plot/arrange [{:data data :mapping {:x :value :y :item}
                    :charts [(chart/bar {:varwidth :n :n-label true :stat :sum})]
                    :labels {:title ":varwidth :n (widths by a column)"}}
                   {:data samples :mapping {:x :value :y :treatment}
                    :charts [(chart/bar {:stat :median :varwidth true :n-label true})]
                    :labels {:title ":stat :median, :varwidth true, :n-label"}}]
                  {:columns 2})))

(def economist
  {:data ordered
   :mapping {:x :value :y :country}
   :scales {:y {:domain (mapv :country ordered)}}
   :theme {:panel {:grid {:y :none}}}
   :charts [(chart/bar {:color [0 104 139] :stroke-color :none :width 0.6 :value-label true})]
   :labels {:title "Labelled horizontal bars (Economist style)" :x "sales, $m" :y :none}})

(def images
  "Images of this demo: `[file-name plot options]`, saved to `results/bar/` by [[cljplot2.demo/render!]]."
  [["basic-900x400.png" basic {:width 900 :height 400}]
   ["error-bars-900x400.png" error-bars {:width 900 :height 400}]
   ["positions-900x700.png" positions {:width 900 :height 700}]
   ["pyramid-1000x400.png" pyramid {:width 1000 :height 400}]
   ["widths-900x350.png" widths {:width 900 :height 350}]
   ["economist-700x400.png" economist {:width 700 :height 400}]])
