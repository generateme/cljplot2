(ns cljplot2.batch-c2-bc-test
  "Batch C2 (bc): bars with a linear or binned (numeric) `:color` - one colour value per bar, no grouping."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.color :as c]
            [cljplot2.test-util :refer [error]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot])
  (:import [java.awt.image BufferedImage]))

(def ^:private options {:width 400 :height 300})

(defn- built [p] (plot/build p options))

(defn- bars
  "The bars of every panel of the first chart of plot `p`, in order."
  [p]
  (vec (mapcat (comp :bars val) (get-in (built p) [:stats 0 :data]))))

(defn- color-values
  "`{level colour-value}` of the bars of plot `p` (one bar per level)."
  [p]
  (into {} (map (juxt :level :color-value) (bars p))))

(def ^:private fruit
  [{:name "a" :n 21} {:name "b" :n 13} {:name "c" :n 8}])

(def ^:private rows
  "Two groups per level, three rows each; colour `:c` - level index * 10 + row index, `nil` for level r group B."
  (vec (for [[li l] (map-indexed vector ["p" "q" "r"]) g ["A" "B"] i (range 3)]
         {:l l :g g :v (inc i) :c (when-not (= [l g] ["r" "B"]) (+ (* 10 li) i))})))

(deftest continuous-color-test
  (testing "zero rows: nothing to colour"
    (is (instance? BufferedImage (plot/render-image {:data [] :mapping {:x :name :y :n :color :n} :charts [(chart/bar)]} options))))
  (testing "one row per bar: its own value, no grouping (one slot, :overlay)"
    (is (= {"a" 21.0 "b" 13.0 "c" 8.0} (color-values {:data fruit :mapping {:x :name :y :n :color :n} :charts [(chart/bar)]})))
    (is (= 1 (count (distinct (map :slot (bars {:data fruit :mapping {:x :name :y :n :color :n} :charts [(chart/bar)]}))))))
    (is (= [8.0 21.0]
           ((juxt :min :max) (:color (val (first (get-in (built {:data fruit :mapping {:x :name :y :n :color :n} :charts [(chart/bar)]})
                                                         [:stats 0 :extents]))))))))
  (testing "one bar"
    (is (= {"a" 21.0} (color-values {:data (take 1 fruit) :mapping {:x :name :y :n :color :n} :charts [(chart/bar)]}))))
  (testing "many rows: default :color-stat - the bar's value stat; :mean for count stats"
    (let [p {:data (remove #(= "B" (:g %)) rows) :mapping {:x :l :y :v :color :c}}]
      (is (= {"p" 3.0 "q" 33.0 "r" 63.0} (color-values (assoc p :charts [(chart/bar)]))))
      (is (= {"p" 1.0 "q" 11.0 "r" 21.0} (color-values (assoc p :charts [(chart/bar {:stat :mean})]))))
      (is (= {"p" 1.0 "q" 11.0 "r" 21.0} (color-values (assoc p :charts [(chart/bar {:stat :count})]))))
      (is (= {"p" 3.0 "q" 33.0 "r" 63.0} (color-values (assoc p :charts [(chart/bar {:stat :count :color-stat :sum})]))))
      (is (= {"p" 1.0 "q" 11.0 "r" 21.0} (color-values (assoc p :charts [(chart/bar {:color-stat :median})]))))
      (is (= {"p" 2.0 "q" 12.0 "r" 22.0} (color-values (assoc p :charts [(chart/bar {:color-stat #(reduce max %)})]))))))
  (testing "rows without a finite value don't count; count stats count every row"
    (let [p {:data [{:l "p" :v 1 :c 1} {:l "p" :v nil :c 100}] :mapping {:x :l :y :v :color :c}}]
      (is (= {"p" 1.0} (color-values (assoc p :charts [(chart/bar)]))))
      (is (= {"p" 50.5} (color-values (assoc-in (assoc p :charts [(chart/bar {:stat :count})]) [:mapping :y] nil))))))
  (testing "groups from :group / categorical aesthetics only: stacked segments, each its own colour"
    (let [bs (bars {:data rows :mapping {:x :l :y :v :color :c :group :g} :charts [(chart/bar {:position :stack})]})]
      (is (= 6 (count bs)))
      (is (= 2 (count (distinct (map :key bs)))))
      (is (= [63.0 nil] (map :color-value (filter #(= "r" (:level %)) bs)))
          "NA rows only - NA colour value")
      (is (= [3.0 3.0 33.0 33.0 63.0 nil] (map :color-value (sort-by (juxt :level :key) bs)))
          "p-A 0+1+2, p-B the same, q-A 10+11+12 ... per segment"))
    (is (= 2 (count (distinct (map :slot (bars {:data rows :mapping {:x :l :y :v :color :c :pattern :g} :charts [(chart/bar)]})))))
        "a pattern dodges, the colour doesn't"))
  (testing "NA: kept (:na :color) or dropped with :groups {:na false}"
    (is (= {"p" 6.0 "q" 66.0 "r" 63.0} (color-values {:data rows :mapping {:x :l :y :v :color :c} :charts [(chart/bar)]}))
        "one bar of a level's rows: NA values left out")
    (let [s (get-in (built {:data rows :mapping {:x :l :y :v :color :c :group :g} :charts [(chart/bar)]}) [:stats 0])]
      (is (contains? (set (:na s)) :color))
      (is (= 0 (get-in s [:dropped :color]))))
    (let [s (get-in (built {:data rows :mapping {:x :l :y :v :color :c} :groups {:na false} :charts [(chart/bar)]}) [:stats 0])]
      (is (not (contains? (set (:na s)) :color)))
      (is (= 3 (get-in s [:dropped :color])))))
  (testing "an explicit colour domain drops no rows: every row builds its bar (see batch-c2-fix-y2-test)"
    (is (= 0 (get-in (built {:data (remove #(= "B" (:g %)) rows) :mapping {:x :l :y :v :color :c}
                             :scales {:color {:domain [0 15]}} :charts [(chart/bar)]})
                     [:stats 0 :dropped :color]))))
  (testing "dates: one row per bar"
    (is (instance? BufferedImage
                   (plot/render-image {:data [{:l "a" :v 1 :d (java.time.LocalDate/of 2020 1 1)} {:l "b" :v 2 :d (java.time.LocalDate/of 2021 1 1)}]
                                       :mapping {:x :l :y :v :color :d} :charts [(chart/bar)]} options))))
  (testing "binned scale, flip, polar and facets render"
    (doseq [extra [{:scales {:color {:kind :binned}}} {:flip true} {:coord {:kind :polar}} {:mapping {:x :l :y :v :color :c :facet :g}}]]
      (is (instance? BufferedImage (plot/render-image (merge {:data rows :mapping {:x :l :y :v :color :c} :charts [(chart/bar)]} extra) options))))))

(deftest continuous-color-drawn-test
  (testing "the bar is filled with its colour under the scale"
    (let [img (plot/render-image {:data [{:l "a" :v 1 :c 0} {:l "b" :v 1 :c 1}] :mapping {:x :l :y :v :color :c}
                                  :scales {:color {:range [[255 0 0] [0 0 255]]}}
                                  :charts [(chart/bar {:stroke-color :none :width 1.0})] :legend false}
                                 {:width 400 :height 300})
          ^BufferedImage img img
          rgb (fn [x y] (c/to-color (.getRGB img x y)))]
      (is (= (c/to-color [255 0 0]) (c/set-alpha (rgb 120 150) 255)))
      (is (= (c/to-color [0 0 255]) (c/set-alpha (rgb 300 150) 255))))))

(deftest color-stat-errors-test
  (testing ":color-stat needs a linear or binned :color"
    (is (= :color-stat (:key (error #(built {:data rows :mapping {:x :l :y :v :color :g} :charts [(chart/bar {:color-stat :sum})]})))))
    (is (= :color-stat (:key (error #(built {:data rows :mapping {:x :l :y :v} :charts [(chart/bar {:color-stat :mean})]}))))))
  (testing "wrong type -> schema error at :color-stat"
    (is (= [:charts 0 :options :color-stat] (:in (error #(built {:data rows :mapping {:x :l :y :v :color :c} :charts [(chart/bar {:color-stat :max})]}))))))
  (testing "nil - the default"
    (is (= (color-values {:data rows :mapping {:x :l :y :v :color :c} :charts [(chart/bar)]})
           (color-values {:data rows :mapping {:x :l :y :v :color :c} :charts [(chart/bar {:color-stat nil})]}))))
  (testing "a function returning no finite number"
    (is (= :color-stat (:key (error #(built {:data rows :mapping {:x :l :y :v :color :c} :charts [(chart/bar {:color-stat (constantly ##NaN)})]}))))))
  (testing "several dates in one bar"
    (is (= :color-stat (:key (error #(built {:data [{:l "a" :v 1 :d (java.time.LocalDate/of 2020 1 1)} {:l "a" :v 2 :d (java.time.LocalDate/of 2021 1 1)}]
                                              :mapping {:x :l :y :v :color :d} :charts [(chart/bar)]})))))))
