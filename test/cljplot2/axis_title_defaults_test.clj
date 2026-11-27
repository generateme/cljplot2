(ns cljplot2.axis-title-defaults-test
  "2D axis title defaults (spec Axis Title Defaults, vault notes 07): without a `:labels` entry or a chart's suggestion a
  drawn axis takes the name of the first keyword accessor a data chart maps to it - none for a function accessor, an
  annotation chart, a chart without that mapping key, a polar or a 3D plot, or an axis not drawn; `:none` hides; a label
  map without `:text` restyles the title; the panel size estimate counts the default."
  (:require [clojure.test :refer [deftest testing is]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.levels :as levels]
            [cljplot2.pipeline :as pipeline]))

(def ^:private rows (vec (for [i (range 20)] {:a (mod i 5) :b (* 0.5 i) :g (if (even? i) "p" "q")})))

(defn- built [p] (plot/build p {:width 400 :height 300}))

(defn- title [b id] (some #(when (= id (:id %)) (get-in % [:content :text])) (get-in b [:grid :items])))

(defn- titles [p] (let [b (built p)] [(title b :x-title) (title b :y-title)]))

(def ^:private defaults #'pipeline/axis-title-defaults)

(def ^:private scatter {:data rows :mapping {:x :a :y :b} :charts [(chart/scatter)]})

(deftest axis-title-defaults-helper-test
  (let [s (chart/scatter) r (chart/rule)]
    (testing "the first data chart mapping the axis by a keyword"
      (is (= {:x "a" :y "b"} (defaults [s] [{:x :a :y :b}] nil {})))
      (is (= {:x "c" :y "b"} (defaults [s s] [{:x #(:a %) :y :b} {:x :c}] nil {})) "a function passes to the next chart")
      (is (= {:x "a"} (defaults [s] [{:x :a :y #(:b %)}] nil {})) "a function gives none")
      (is (= {:x "a"} (defaults [s s] [{:x :a} {:x :c}] nil {})) "the first of two keywords"))
    (testing "a chart whose mapping has no such key never gives one"
      (is (= {} (defaults [(chart/parallel {:variables [:a :b]})] [{:x :a :y :b}] nil {})))
      (is (= {:x "c"} (defaults [(chart/parallel {:variables [:a :b]}) s] [{:x :a} {:x :c}] nil {}))))
    (testing "annotation charts never give one"
      (is (= {} (defaults [r] [{:x :a :y :b}] nil {})))
      (is (= {:x "c"} (defaults [r s] [{:x :a} {:x :c}] nil {}))))
    (testing "a categorical marker is unwrapped"
      (is (= {:x "a"} (defaults [s] [{:x (levels/categorical :a)}] nil {}))))
    (testing "polar and 3D plots get none"
      (is (= {} (defaults [s] [{:x :a :y :b}] {:kind :polar} {})))
      (is (= {} (defaults [(chart/cloud)] [{:x :a :y :b :z :b}] nil {}))))
    (testing "an axis not drawn gets none"
      (is (= {:y "b"} (defaults [s] [{:x :a :y :b}] nil {:x {:axis false}})))
      (is (= {:x "a"} (defaults [s] [{:x :a :y :b}] nil {:y {:axis :grid}}))))
    (testing "no charts, no mappings"
      (is (= {} (defaults [] [] nil {}))))))

(deftest grid-titles-test
  (testing "a plain scatter is titled by its columns"
    (is (= ["a" "b"] (titles scatter))))
  (testing "a suggestion and :labels win; :none hides; nil keeps the default"
    (is (= ["a" "Count"] (titles {:data rows :mapping {:x :a} :charts [(chart/histogram {:y :count})]})))
    (is (= ["a" "v2"] (titles (assoc scatter :labels {:y "v2"}))))
    (is (= ["a" nil] (titles (assoc scatter :labels {:y :none}))))
    (is (= ["a" "b"] (titles (assoc scatter :labels {:y nil})))))
  (testing "a chart's own mapping wins over the plot's"
    (is (= ["a" "g"] (titles (assoc scatter :charts [(chart/scatter {:mapping {:y :g}})])))))
  (testing ":flip - each title follows its scale"
    (let [b (built (assoc scatter :flip true))]
      (is (= ["b" "a"] [(title b :x-title) (title b :y-title)]))))
  (testing "facets: one title per axis"
    (let [b (built (assoc scatter :mapping {:x :a :y :b :facet :g}))]
      (is (= 1 (count (filter #(= :x-title (:id %)) (get-in b [:grid :items])))))
      (is (= "a" (title b :x-title)))))
  (testing "polar, 3D, function and annotation-only plots get none"
    (is (= [nil nil] (titles (assoc scatter :coord {:kind :polar}))))
    (is (= [nil nil] (titles {:data rows :mapping {:x :a :y :b :z :b} :charts [(chart/cloud)]})))
    (is (= [nil nil] (titles (assoc scatter :mapping {:x #(:a %) :y #(:b %)}))))
    (is (= [nil nil] (titles {:charts [(chart/rule {:data [{:a 1}] :mapping {:x :a}})] :scales {:x {:domain [0 2]} :y {:domain [0 2]}}}))))
  (testing "an axis switched off gets none"
    (is (= [nil "b"] (titles (assoc scatter :scales {:x {:axis false}})))))
  (testing "two charts: the first keyword names the axis"
    (is (= ["a" "b"] (titles (assoc scatter :charts [(chart/scatter) (chart/line {:mapping {:x :b}})])))))
  (testing "a categorical marker in the grid"
    (is (= ["a" "b"] (titles (assoc scatter :mapping {:x (levels/categorical :a) :y :b} :charts [(chart/box)])))))
  (testing "a label map without :text restyles the default and the suggestion"
    (let [b (built (assoc scatter :labels {:y {:rotation 0 :color [255 0 0]}}))
          item (some #(when (= :y-title (:id %)) (:content %)) (get-in b [:grid :items]))]
      (is (= ["b" 0] [(:text item) (:rotation item)])))
    (is (= "Count" (title (built {:data rows :mapping {:x :a} :charts [(chart/histogram {:y :count})] :labels {:y {:size 20}}}) :y-title))))
  (testing "parallel under a plot :x mapping: no default (its axis shows the variables)"
    (is (= [nil nil] (titles {:data rows :mapping {:x :a :y :b :color :g} :charts [(chart/parallel {:variables [:a :b]})]}))))
  (testing "pairs: explicit names only; inner cells none"
    (let [b (plot/build (plot/pairs rows {:variables [:a :b]}) {:width 400 :height 400})
          cell-titles (fn [k] (let [c (get-in b [:subplots [:plot k]])] [(title c :x-title) (title c :y-title)]))]
      (is (= [nil "a"] (cell-titles 0)))
      (is (= [nil nil] (cell-titles 1)))))
  (testing "margins, an invalid-value band and wrap facets with free scales keep one title per axis"
    (is (= ["a" "b"] (titles (assoc scatter :margins {:top {:charts [(chart/histogram)]} :right {:charts [(chart/histogram)]}}))))
    (is (= ["a" "Percent of Total"] (titles {:data (conj rows {:a nil :b 1}) :mapping {:x :a} :charts [(chart/histogram {:invalid :show})]})))
    (let [b (built (assoc scatter :mapping {:x :a :y :b :facet :g} :facets {:scales {:x :free}}))]
      (is (= 1 (count (filter #(= :x-title (:id %)) (get-in b [:grid :items])))))))
  (testing "a legend-only plot has no axis titles"
    (is (= #{:legends} (set (map :id (get-in (built (assoc scatter :mapping {:x :a :y :b :color :g} :legend-only true)) [:grid :items])))))))

(deftest estimate-test
  (testing "the panel size estimate counts the default titles (bin2d's cell shape reads it)"
    (let [points (vec (for [i (range 200)] {:a (mod (* i 37) 101) :b (mod (* i 53) 97)}))
          cells (fn [mapping labels] (get-in (built (cond-> {:data points :mapping mapping :charts [(chart/bin2d {:cell :pointy-hex})]}
                                                       labels (assoc :labels labels)))
                                             [:stats 0 :data]))]
      (is (not= (cells {:x :a :y :b} nil) (cells {:x #(:a %) :y #(:b %)} nil)) "a keyword mapping titles the axes")
      (is (= (cells {:x :a :y :b} nil) (cells {:x #(:a %) :y #(:b %)} {:x "a" :y "b"})) "as explicit titles do")
      (is (= (cells {:x :a :y :b} nil) (cells {:x :a :y :b} {:y nil})) "a nil label keeps the default"))))
