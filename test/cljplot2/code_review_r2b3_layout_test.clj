(ns cljplot2.code-review-r2b3-layout-test
  "Code review round 2 B3 - layout/ boundaries after the style pass: packing, treemaps, Venn regions, arrange grids."
  (:require [clojure.test :refer [deftest testing is]]
            [cljplot2.layout.pack :as pack]
            [cljplot2.layout.plot-grid :as plot-grid]
            [cljplot2.layout.treemap :as treemap]
            [cljplot2.layout.venn :as venn]))

(deftest pack-boundaries-test
  (testing "0, 1 and 2 siblings"
    (is (= [] (pack/pack-siblings [])))
    (is (= [[0.0 0.0]] (pack/pack-siblings [3])))
    (is (= [[-1.0 0.0] [2.0 0.0]] (pack/pack-siblings [2 1]))))
  (testing "3 siblings touch pairwise; equal radii - an equilateral triangle"
    (let [[[ax ay] [bx by] [cx cy]] (pack/pack-siblings [1 1 1])
          d (fn [x0 y0 x1 y1] (Math/hypot (- x1 x0) (- y1 y0)))]
      (is (every? #(< (Math/abs (- 2.0 %)) 1e-9) [(d ax ay bx by) (d ax ay cx cy) (d bx by cx cy)]))))
  (testing "a lazy seq of radii"
    (is (= (pack/pack-siblings [1 2 3 4]) (pack/pack-siblings (map inc (range 4))))))
  (testing "enclose: one circle as given, a circle inside another, two apart"
    (is (= {:x 1 :y 2 :r 3} (pack/enclose [{:x 1 :y 2 :r 3}])))
    (is (= {:x 0.0 :y 0.0 :r 5.0} (pack/enclose [{:x 0.0 :y 0.0 :r 5.0} {:x 1.0 :y 0.0 :r 1.0}])))
    (let [{:keys [x y r]} (pack/enclose [{:x -2.0 :y 0.0 :r 1.0} {:x 2.0 :y 0.0 :r 1.0}])]
      (is (< (Math/abs (double x)) 1e-12))
      (is (zero? (double y)))
      (is (< (Math/abs (- 3.0 (double r))) 1e-12)))
    (testing "zero radii (points)"
      (let [{:keys [r]} (pack/enclose [{:x 0.0 :y 0.0 :r 0.0} {:x 2.0 :y 0.0 :r 0.0} {:x 1.0 :y 1.0 :r 0.0}])]
        (is (< (Math/abs (- 1.0 (double r))) 1e-9)))))
  (testing "pack: one leaf fills the box's shorter side; zero box and zero padding skip the gap search"
    (let [one (pack/pack {:value 4} {:x 0 :y 0 :w 100 :h 60} 0)]
      (is (= [50.0 30.0 30.0] ((juxt :x :y :r) one))))
    (let [tree {:value 3 :children [{:value 1} {:value 2}]}
          zero (pack/pack tree {:x 0 :y 0 :w 0 :h 0} 5)]
      (is (every? #(zero? (double (:r %))) (cons zero (:children zero)))))
    (testing "padding: the gap between two siblings is the asked one (within the search tolerance)"
      (let [{[a b] :children} (pack/pack {:value 2 :children [{:value 1} {:value 1}]} {:x 0 :y 0 :w 200 :h 200} 10)
            gap (- (Math/hypot (- (double (:x b)) (double (:x a))) (- (double (:y b)) (double (:y a))))
                   (double (:r a)) (double (:r b)))]
        (is (< (Math/abs (- 10.0 gap)) 0.011))))))

(defn- area [{:keys [w h]}] (* (double w) (double h)))

(deftest treemap-boundaries-test
  (let [rect {:x 0.0 :y 0.0 :w 100.0 :h 50.0}]
    (testing "empty values"
      (is (= [] (treemap/squarify [] rect)))
      (is (= [] (treemap/slice-dice [] rect 0))))
    (testing "one value fills the rectangle"
      (is (= [rect] (treemap/squarify [7] rect)))
      (is (= [rect] (treemap/slice-dice [7] rect 1))))
    (testing "areas proportional, in value order"
      (let [rs (treemap/squarify [6 3 1] rect)]
        (is (= 3 (count rs)))
        (is (every? true? (map (fn [r v] (< (Math/abs (- (area r) (* 500.0 v))) 1e-9)) rs [6 3 1])))))
    (testing "zero total (slice-dice) and zero-size rectangle give zero-size cells"
      (is (every? #(zero? (area %)) (treemap/slice-dice [0 0] rect 0)))
      (is (every? #(zero? (area %)) (treemap/squarify [2 1] {:x 0.0 :y 0.0 :w 0.0 :h 50.0}))))
    (testing "slice-dice cuts along x at even depths, along y at odd ones"
      (is (= [50.0 50.0] (map :w (treemap/slice-dice [1 1] rect 0))))
      (is (= [25.0 25.0] (map :h (treemap/slice-dice [1 1] rect 1)))))))

(deftest venn-boundaries-test
  (testing "lens area: apart, contained, partial"
    (is (zero? (venn/lens-area 3.0 1.0 1.0)))
    (is (= (* Math/PI 1.0) (venn/lens-area 0.5 2.0 1.0)))
    (is (< 0.0 (venn/lens-area 1.0 1.0 1.0) Math/PI)))
  (testing "two sets: disjoint, subset, empty set"
    (let [[a b] (venn/two-sets 4 4 0 true)]
      (is (> (double (:x b)) (+ (double (:r a)) (double (:r b))))))
    (let [[a b] (venn/two-sets 4 1 1 true)
          regions (venn/region-points [a b])]
      (is (contains? regions #{0}))
      (is (not (contains? regions #{1})) "the subset has no region of its own"))
    (let [regions (venn/region-points (venn/two-sets 4 0 0 true))]
      (is (= #{#{0}} (set (keys regions))))))
  (testing "three sets: every region"
    (is (= 7 (count (venn/region-points (venn/three-sets)))))))

(deftest arrange-grid-test
  (let [problem @#'plot-grid/arrange-problem
        shape @#'plot-grid/arrange-shape]
    (testing "shapes: one column by default, the other side rounded up"
      (is (= [3 1] (shape 3 nil nil)))
      (is (= [2 2] (shape 3 2 nil)))
      (is (= [2 2] (shape 4 nil 2)))
      (is (= [1 5] (shape 1 5 1))))
    (testing "plots given lazily (a seq) are indexed by cell"
      (let [realized (atom 0)
            plots (map (fn [i] (swap! realized inc) (if (odd? i) {:legend-only true} {})) (range 4))]
        (is (nil? (problem {:plots plots :columns 2 :widths [1 :measured]})))
        (is (= 4 @realized))))
    (testing "too few cells; :measured over a column with a full plot; weights per column / row"
      (is (some? (problem {:plots [{} {} {}] :columns 1 :rows 2})))
      (is (some? (problem {:plots [{} {}] :columns 2 :widths [:measured 1]})))
      (is (some? (problem {:plots [{} {}] :columns 2 :widths [1]})))
      (is (some? (problem {:plots [{} {}] :rows 2 :heights [1 1 1]}))))
    (testing "the grid: equal weights by default, gaps between cells"
      (let [g (plot-grid/arrange-grid {:plots [{} {} {}] :columns 2} nil {:multiplot {:gap 7} :plot {:margin 3}})]
        (is (= [[:fixed 3] [:flexible 1] [:fixed 7] [:flexible 1] [:fixed 3]] (map :size (:columns g))))
        (is (= [[:plot 0] [:plot 1] [:plot 2]] (map :id (:items g))))
        (is (= [[:column 0] [:column 1] [:column 0]] (map :column (:items g))))))))
