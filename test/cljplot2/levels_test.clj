(ns cljplot2.levels-test
  "Cycle 5: level helpers (`order-levels`, `bin`, `breaks`), interval levels, `:stroke-color`."
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure2d.color :as c]
            [fastmath.stats :as stats]
            [cljplot2.test-util :refer [error error-ins]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.facet :as facet]
            [cljplot2.data.levels :as levels]
            [cljplot2.data.rows :as rows]
            [cljplot2.data.shingle :as shingle]
            [cljplot2.layout.grid :as grid])
  (:import [java.awt.image BufferedImage]))

(def ^:private options {:width 400 :height 300})

;; 1.1 order-levels

(deftest order-levels-test
  (let [rs [{:l "a" :v 5} {:l "b" :v 1} {:l "a" :v 7} {:l "c" :v 3} {:l "b" :v 2}]]
    (testing "ascending by the statistic, descending reversed"
      (is (= ["b" "c" "a"] (levels/order-levels rs :l :v stats/median)))
      (is (= ["a" "c" "b"] (levels/order-levels rs :l :v stats/median {:descending true})))
      (is (= ["c" "a" "b"] (levels/order-levels rs :l :v count)) "any function: count")
      (is (= ["b" "c" "a"] (levels/order-levels rs #(:l %) #(:v %) stats/mean)) "function accessors"))
    (testing "ties keep first appearance"
      (is (= ["x" "y" "z"] (levels/order-levels [{:l "x" :v 1} {:l "y" :v 1} {:l "z" :v 1}] :l :v stats/mean)))
      (is (= ["x" "y" "z"] (levels/order-levels [{:l "x" :v 1} {:l "y" :v 1} {:l "z" :v 1}] :l :v stats/mean
                                                {:descending true}))))
    (testing "levels without finite values after the ordered ones (natural order), nil last"
      (is (= ["b" "a" "q" "z" nil]
             (levels/order-levels [{:l "z" :v nil} {:l "a" :v 2} {:l nil :v 0} {:l "b" :v 1} {:l "q" :v ##NaN}]
                                  :l :v stats/mean))))
    (testing "a statistic without a finite value leaves the level unordered"
      (is (= ["b" "a"] (levels/order-levels [{:l "a" :v 1} {:l "b" :v 2}] :l :v #(if (= [1] (vec %)) ##NaN (first %))))))
    (testing "empty data"
      (is (= [] (levels/order-levels [] :l :v stats/mean))))
    (testing "barley varieties by median yield equal a manual sort; usable as a domain"
      (let [barley (for [i (range 60)] {:variety (str "v" (mod i 6)) :yield (double (mod (* i 37) 41))})
            manual (->> (group-by :variety barley)
                        (sort-by (fn [[_ rs]] (stats/median (map :yield rs))))
                        (map first))
            ordered (levels/order-levels barley :variety :yield stats/median)]
        (is (= manual ordered))
        (is (= ordered (get-in (plot/build {:data barley :mapping {:x :yield :y :variety} :scales {:y {:domain ordered}}
                                            :charts [(chart/box)]} options)
                               [:finalized-scales :y :domain])))))))

;; 1.2 breaks and bin

(deftest bin-index-test
  (let [arr (double-array [0 5 10])]
    (are [v i] (= i (levels/bin-index arr v))
      0.0 0, 4.999 0, 5.0 1, 9.99 1, 10.0 1, -0.001 -1, 10.001 -1)))

(deftest bin-test
  (testing "breaks [0 5 10] over 1..10: [0, 5) x5, [5, 10] x5"
    (let [bs (map :b (levels/bin (map (fn [v] {:v v}) (range 1 11)) :v {:as :b :bins [0 5 10]}))]
      (is (= {"[0, 5)" 4 "[5, 10]" 6} (frequencies (map str bs))) "5 is in the upper bin")
      (is (every? shingle/interval? bs))))
  (testing "values exactly on breaks and the last edge"
    (is (= ["[0, 5)" "[5, 10]" "[5, 10]"]
           (mapv (comp str :b) (levels/bin [{:v 0} {:v 5} {:v 10}] :v {:as :b :bins [0 5 10]})))))
  (testing "nil, NaN, non-numbers and outside values get nil; every row kept with its columns"
    (let [out (levels/bin [{:v nil :k 1} {:v ##NaN :k 2} {:v "x" :k 3} {:v 11 :k 4} {:v 2 :k 5}] :v {:as :b :bins [0 5 10]})]
      (is (= [nil nil nil nil "[0, 5)"] (mapv #(some-> (:b %) str) out)))
      (is (= [1 2 3 4 5] (mapv :k out)))))
  (testing "all nil / empty data"
    (is (= [nil nil] (mapv :b (levels/bin [{:v nil} {:v nil}] :v {:as :b}))))
    (is (= [] (levels/bin [] :v {:as :b}))))
  (testing "a single distinct value: one closed bin of width 1"
    (is (= ["[2.5, 3.5]" "[2.5, 3.5]"] (mapv (comp str :b) (levels/bin [{:v 3} {:v 3}] :v {:as :b})))))
  (testing "count and method bins match the histogram breaks"
    (let [vs (map double (range 1 101))
          bs (distinct (map :b (levels/bin (map (fn [v] {:v v}) vs) :v {:as :b :bins 4})))]
      (is (= 4 (count bs)))
      (is (= (levels/breaks vs :sturges)
             (let [ivs (sort (distinct (map :b (levels/bin (map (fn [v] {:v v}) vs) :v {:as :b}))))]
               (conj (mapv :lo ivs) (:hi (last ivs))))))))
  (testing ":digits and :label"
    (is (= "[0.333, 0.667)" (str (:b (first (levels/bin [{:v 0.4} {:v 1.0}] :v {:as :b :bins [0 (/ 1.0 3) (/ 2.0 3) 1]}))))))
    (is (= "[0.33333, 0.66667)" (str (:b (first (levels/bin [{:v 0.4}] :v {:as :b :bins [0 (/ 1.0 3) (/ 2.0 3) 1] :digits 5}))))))
    (is (= "v: [0, 5)" (str (:b (first (levels/bin [{:v 1}] :v {:as :b :bins [0 5 10] :label true}))))))
    (is (= "w: [0, 5)" (str (:b (first (levels/bin [{:v 1}] :v {:as :b :bins [0 5 10] :label "w"})))))))
  (testing "errors"
    (is (seq (error-ins #(levels/bin [{:v 1}] :v {:bins 3}))) ":as required")
    (is (seq (error-ins #(levels/bin [{:v 1}] :v {:as :b :bins [3 1]}))) "decreasing breaks")
    (is (seq (error-ins #(levels/bin [{:v 1}] :v {:as :b :digits 0}))))
    (is (seq (error-ins #(levels/bin [{:v 1}] (fn [r] (:v r)) {:as :b :label true}))) "label true needs a keyword")
    (is (= :bins (:key (error #(levels/bin [{:v 1} {:v 2}] :v {:as :b :bins :nope})))))))

(deftest shingle-printing-unchanged-test
  (is (= "[1, 2.5]" (str (shingle/interval 1 2.5))))
  (is (= "m: [1, 2]" (str (shingle/interval 1 2 "m")))))

;; 1.3 interval levels

(defn- panel-box [b] (grid/find-content-box (:grid b) (:layout b) #(= :panel (:kind %))))

(deftest interval-levels-test
  (let [rs (levels/bin (for [i (range 80)] {:w (mod (* i 13) 40) :x (mod (* i 7) 11) :g (["p" "q"] (mod i 2))})
                       :w {:as :bin :bins [0 10 20 30 40]})
        reversed (reverse rs)]
    (testing "valid values; categorical extents of intervals sorted ascending, strings in first-appearance order"
      (is (rows/valid-value? (:bin (first rs))))
      (is (= ["[0, 10)" "[10, 20)" "[20, 30)" "[30, 40]"] (mapv str (:values (rows/extent :y (map :bin reversed))))))
      (is (= ["b" "a"] (:values (rows/extent :y ["b" "a" "b"])))))
    (testing "a box over bins: bands domain ascending, tick labels printed intervals, flipped too"
      (doseq [flip [false true]
              :let [b (plot/build {:data reversed :mapping {:x :x :y :bin} :flip flip :charts [(chart/box)]} options)
                    y (get-in b [:finalized-scales :y])]]
        (is (= :bands (:kind y)))
        (is (= ["[0, 10)" "[10, 20)" "[20, 30)" "[30, 40]"] (mapv str (:domain y))))
        (is (= ["[0, 10)" "[10, 20)" "[20, 30)" "[30, 40]"] (:tick-labels y)))))
    (testing "colour levels and ridges over intervals"
      (is (= ["[0, 10)" "[10, 20)" "[20, 30)" "[30, 40]"]
             (mapv str (get-in (plot/build {:data reversed :mapping {:x :x :y :w :color :bin} :charts [(chart/scatter)]} options)
                               [:scale-specs :color :domain]))))
      (is (= 4 (count (get-in (plot/build {:data rs :mapping {:x :x :y :bin} :charts [(chart/density)]} options)
                              [:stats 0 :data facet/single-panel :ridges])))))
    (testing "all :y values invalid (nil bins) throw {:key :mapping}"
      (is (= :mapping (:key (error #(plot/build {:data (map (fn [r] (assoc r :bin nil)) rs) :mapping {:x :x :y :bin}
                                                 :charts [(chart/box)]} options))))))
    (testing "renders"
      (is (instance? BufferedImage (plot/render-image {:data rs :mapping {:x :x :y :bin :color :g} :charts [(chart/violin)]}
                                                      options))))))

;; 2. stroke-color

(defn- colors [p]
  (let [^BufferedImage img (plot/render-image p options)]
    (frequencies (for [i (range (.getWidth img)) j (range (.getHeight img))] (bit-and (.getRGB img (int i) (int j)) 0xffffff)))))

(def ^:private fill [200 200 0])

(def ^:private dark-red
  "RGB int of the darkened pure red (the `:darker` outline of a red fill)."
  (let [[r g b] (map long (c/to-color (c/darken [255 0 0])))]
    (bit-or (bit-shift-left r 16) (bit-shift-left g 8) b)))
(def ^:private plain {:panel {:grid {:x :none :y :none :bands-fill :none}}})

(deftest stroke-color-test
  (let [rs (for [i (range 50)] {:v (mod (* i 7) 13) :l (["a" "b"] (mod i 2))})
        red 0xff0000]
    (testing "a color outlines; :none draws no outline even with a stroke width; default unchanged (darker fill)"
      (doseq [c [(chart/histogram {:color fill :stroke-color :red :stroke-width 2})
                 (chart/box {:color fill :stroke-color :red :outliers false})
                 (chart/violin {:color fill :stroke-color :red :inner :none})]]
        (is (pos? (get (colors {:data rs :mapping {:x :v :y :l} :theme plain :charts [c]}) red 0)) (str (:kind c))))
      (doseq [c [(chart/histogram {:color [255 0 0] :stroke-color :none :stroke-width 3})
                 (chart/violin {:color [255 0 0] :stroke-color :none :inner :none})]
              :let [cs (colors {:data rs :mapping {:x :v} :theme plain :charts [c]})]]
        (is (pos? (get cs red 0)))
        (is (zero? (get cs dark-red 0)) (str (:kind c) " no darkened outline")))
      (is (pos? (get (colors {:data rs :mapping {:x :v} :theme plain :charts [(chart/histogram {:color [255 0 0]})]}) dark-red 0))
          "the default outline is the darkened fill")
      (is (= (colors {:data rs :mapping {:x :v} :charts [(chart/histogram)]})
             (colors {:data rs :mapping {:x :v} :charts [(chart/histogram {:stroke-color :darker})]}))))
    (testing "box :none keeps its whiskers (darkened fill)"
      (let [cs (colors {:data rs :mapping {:x :v :y :l} :theme plain
                        :charts [(chart/box {:color [255 0 0] :stroke-color :none :outliers false})]})]
        (is (pos? (get cs dark-red 0)))))
    (testing "legend keys use the stroke"
      (is (pos? (get (colors {:data rs :mapping {:x :v :color :l} :theme plain
                              :charts [(chart/histogram {:stroke-color :red :position :dodge})]})
                     red 0))))
    (testing "schema"
      (is (= #{[:charts 0 :options :stroke-color]}
             (error-ins #(plot/build {:data rs :mapping {:x :v} :charts [(chart/histogram {:stroke-color :darkest})]}))))
      (is (= #{[:theme :chart :box :stroke-color]}
             (error-ins #(plot/build {:theme {:chart {:box {:stroke-color 12.5}}}})))))))
