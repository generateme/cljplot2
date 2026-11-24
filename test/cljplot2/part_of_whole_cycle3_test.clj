(ns cljplot2.part-of-whole-cycle3-test
  "Part of a whole cycle 3: waffle, set intersections, Venn diagrams."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is are]]
            [fastmath.core :as m]
            [cljplot2.test-util :refer [error]]
            [cljplot2.chart :as chart]
            [cljplot2.chart.waffle :as waffle]
            [cljplot2.core :as plot]
            [cljplot2.data.sets :as sets]
            [cljplot2.layout.grid :as grid]
            [cljplot2.layout.venn :as venn])
  (:import [java.awt.image BufferedImage]))

(defn- rgb [^BufferedImage img x y]
  (let [v (.getRGB img (int x) (int y))]
    [(bit-and (bit-shift-right v 16) 0xff) (bit-and (bit-shift-right v 8) 0xff) (bit-and v 0xff)]))

(def ^:private plain {:plot {:background :white} :palette {:color [[255 0 0] [0 0 255] [0 160 0]] :na [128 128 128]}})

(defn- render [p size]
  (let [p (update p :theme #(merge plain %))
        built (plot/build p size)]
    [built (plot/render-image p size) (grid/item-box (:layout built) :panel)]))

;; 1. waffle

(deftest largest-remainder-test
  (are [weights n counts] (= counts (waffle/largest-remainder weights n))
    [1 1 1] 100 [34 33 33]
    [0.4 99.6] 100 [0 100]
    [0.6 99.4] 100 [1 99]
    [5] 100 [100]
    [0 0] 100 [0 0]
    [3 1] 0 [0 0]
    [] 100 []
    [2.5 2.5] 5 [3 2])
  (is (every? #(= 100 (reduce + (waffle/largest-remainder % 100))) [[1 2 3 4 5 6 7] [0.001 1000] [1e-9 1e-9 1e-9]])))

(deftest waffle-cells-test
  (testing "1.3 cell positions: rows from the bottom-left, or columns"
    (are [k order pos] (= pos (waffle/cell-position k 10 4 order))
      0 :rows [0 0]
      9 :rows [9 0]
      10 :rows [0 1]
      0 :columns [0 0]
      1 :columns [0 1]
      4 :columns [1 0]))
  (let [rows [{:c "a" :v 1} {:c "b" :v 1} {:c "c" :v 1}]
        stats (fn [p] (first (:stats (plot/build p {:width 400 :height 400}))))]
    (testing "1.2.1 percent grid: shares by largest remainder"
      (let [d (get-in (stats {:data rows :mapping {:color :c :value :v} :charts [(chart/waffle)]}) [:data :panel])]
        (is (= [34 33 33] (map :cells (:categories d))))
        (is (= [10 10] [(:columns d) (:rows d)]))))
    (testing "1.2.2 :unit - cells per unit, rows as needed"
      (let [d (get-in (stats {:data [{:c "a" :v 2.5} {:c "b" :v 2.5} {:c "c" :v 17}] :mapping {:color :c :value :v}
                              :charts [(chart/waffle {:unit 1 :columns 10})]}) [:data :panel])]
        (is (= [3 2 17] (map :cells (:categories d))))
        (is (= [10 3] [(:columns d) (:rows d)]))))
    (testing "rows counted without :value; invalid weights dropped and counted"
      (is (= [50 50] (map :cells (get-in (stats {:data [{:c "a"} {:c "b"}] :mapping {:color :c} :charts [(chart/waffle)]})
                                         [:data :panel :categories]))))
      (let [s (stats {:data (into rows [{:c "a" :v -1} {:c "a" :v ##NaN} {:c "a" :v "x"}]) :mapping {:color :c :value :v}
                      :charts [(chart/waffle)]})]
        (is (= 3 (get-in s [:dropped :value])))
        (is (nil? (get-in s [:extents :panel :x])))))
    (testing "a zero total: no cells"
      (is (= [0 0] (map :cells (get-in (stats {:data [{:c "a" :v 0} {:c "b" :v 0}] :mapping {:color :c :value :v} :charts [(chart/waffle)]})
                                       [:data :panel :categories])))))
    (testing "1.1 mapping and options"
      (is (= [:charts 0 :options :mapping :color] (:in (error #(stats {:data rows :mapping {:value :v} :charts [(chart/waffle)]})))))
      (are [opts k] (= k (:key (error #(stats {:data rows :mapping {:color :c} :charts [(chart/waffle opts)]}))))
        {:rows 0} :rows
        {:unit 0} :unit
        {:gap 1} :gap
        {:shape :star} :shape
        {:fill-order :diagonal} :fill-order))
    (testing "X2 at most 10000 cells"
      (is (= :unit (:key (error #(stats {:data [{:c "a" :v 2e6} {:c "b" :v 1e6}] :mapping {:color :c :value :v} :charts [(chart/waffle {:unit 1})]})))))
      (is (= 10000 (reduce + (map :cells (get-in (stats {:data [{:c "a" :v 1e4}] :mapping {:color :c :value :v} :charts [(chart/waffle {:unit 1 :columns 100})]})
                                                 [:data :panel :categories]))))
          "exactly the limit")
      (is (= :rows (:key (error #(stats {:data rows :mapping {:color :c} :charts [(chart/waffle {:rows 200 :columns 100})]})))))
      (is (= :columns (:key (error #(stats {:data rows :mapping {:color :c} :charts [(chart/waffle {:columns 10001})]}))))))
    (testing "facets and empty data render"
      (is (= 2 (count (:data (stats {:data (map #(assoc % :f (if (= "a" (:c %)) "p" "q")) rows) :mapping {:color :c :facet :f}
                                     :charts [(chart/waffle)]})))))
      (is (some? (plot/render {:data [] :mapping {:color :c} :charts [(chart/waffle)]} {:width 200 :height 200}))))))

(deftest waffle-drawing-test
  (let [p (fn [opts] {:data [{:c "a" :v 1} {:c "b" :v 3}] :mapping {:color :c :value :v} :charts [(chart/waffle opts)]})]
    (testing "1.3 / 1.4 the first category in the bottom-left cell; square cells, the grid centred in a wide panel"
      (let [[_ img {:keys [x y w h]}] (render (p {:gap 0}) {:width 800 :height 300})
            side (/ (double h) 10.0)
            left (+ (double x) (* 0.5 (- (double w) (* 10.0 side))))]
        (is (= [255 0 0] (rgb img (+ left (* 0.5 side)) (- (+ (double y) (double h)) (* 0.5 side)))))
        (is (= [0 0 255] (rgb img (+ left (* 0.5 side)) (+ (double y) (* 0.5 side)))) "the last category at the top")
        (is (= [255 255 255] (rgb img (- left 2) (+ (double y) (* 0.5 (double h))))) "left of the grid: background")
        (is (= [255 0 0] (rgb img (+ left side) (- (+ (double y) (double h)) (* 0.5 side)))) ":gap 0 - cells touch")))
    (testing "1.4 :gap leaves space between cells; circles leave the corners"
      (let [[_ img {:keys [x y w h]}] (render (p {:gap 0.4}) {:width 300 :height 300})
            side (/ (m/min (double w) (double h)) 10.0)
            left (+ (double x) (* 0.5 (- (double w) (* 10.0 side))))
            top (+ (double y) (* 0.5 (- (double h) (* 10.0 side))))]
        (is (= [255 255 255] (rgb img (+ left (* 0.05 side)) (- (+ top (* 10.0 side)) (* 0.5 side))))))
      (let [[_ img {:keys [x y w h]}] (render (p {:gap 0 :cell-shape :circle}) {:width 300 :height 300})
            side (/ (m/min (double w) (double h)) 10.0)
            left (+ (double x) (* 0.5 (- (double w) (* 10.0 side))))
            bottom (+ (double y) (* 0.5 (+ (double h) (* 10.0 side))))]
        (is (= [255 255 255] (rgb img (+ left 1) (- bottom 2))) "a circle cell's corner")
        (is (= [255 0 0] (rgb img (+ left (* 0.5 side)) (- bottom (* 0.5 side)))))))
    (testing "1.3 :fill-order :columns fills a column first"
      (let [[_ img {:keys [x y w h]}] (render (assoc (p {:gap 0 :fill-order :columns}) :data [{:c "a" :v 15} {:c "b" :v 85}]) {:width 300 :height 300})
            side (/ (m/min (double w) (double h)) 10.0)
            left (+ (double x) (* 0.5 (- (double w) (* 10.0 side))))
            bottom (+ (double y) (* 0.5 (+ (double h) (* 10.0 side))))]
        (is (= [255 0 0] (rgb img (+ left (* 0.5 side)) (- bottom (* 9.5 side)))) "column 0 full of a")
        (is (= [255 0 0] (rgb img (+ left (* 1.5 side)) (- bottom (* 4.5 side)))) "a's last cell: column 1, row 4")
        (is (= [0 0 255] (rgb img (+ left (* 1.5 side)) (- bottom (* 5.5 side)))))))
    (testing "1.5 legend keys follow the shape; theme validated"
      (let [[built img] (render (p {:cell-shape :circle}) {:width 400 :height 300})
            {:keys [x y]} (grid/item-box (:layout built) :legends)]
        (is (some? x))
        (is (some #(= [255 0 0] %) (for [i (range (long x) (+ (long x) 30)) j (range (long y) (+ (long y) 40))] (rgb img i j)))))
      (is (some? (error #(plot/build (assoc (p {}) :theme {:chart {:waffle {:gap "x"}}}) {:width 200 :height 200})))))))

;; 2. sets

(def ^:private members
  (concat (for [i (range 0 60)] {:set "A" :item i})
          (for [i (range 40 90)] {:set "B" :item i})
          (for [i (range 50 75)] {:set "C" :item i})
          (for [i (range 85 120)] {:set "C" :item i})))

(def ^:private mapping {:set :set :item :item})

(defn- as-map [xs] (into {} (map (juxt :sets :size)) xs))

(deftest sets-test
  (testing "2.1 mapping keys :set / :item are accessors"
    (is (= :set (:key (error #(plot/build {:data members :mapping {:set 3 :item :item} :charts [(chart/venn)]} {:width 200 :height 200}))))))
  (testing "X1 combinations in first-appearance order of their items"
    (let [rows (concat (for [i (range 20)] {:set "A" :item (str "a" i)}) (for [i (range 10 30)] {:set "B" :item (str "a" i)})
                       (for [i (range 25 40)] {:set "C" :item (str "a" i)}))]
      (is (= [["A"] ["A" "B"] ["B"] ["B" "C"] ["C"]] (map :sets (sets/intersections rows mapping))))
      (is (= ["a0" "a1"] (take 2 (:items (sets/memberships rows mapping)))))))
  (testing "2.2 exact combinations"
    (is (= {["A"] 40 ["A" "B"] 10 ["A" "B" "C"] 10 ["B" "C"] 20 ["B"] 10 ["C"] 30} (as-map (sets/intersections members mapping)))))
  (testing "2.2 repeated rows count once; disjoint; subset; empty"
    (is (= {["A"] 1 ["A" "B"] 1} (as-map (sets/intersections [{:set "A" :item 1} {:set "A" :item 1} {:set "A" :item 2} {:set "B" :item 2}] mapping))))
    (is (= {["A"] 2 ["B"] 1} (as-map (sets/intersections [{:set "A" :item 1} {:set "A" :item 2} {:set "B" :item 3}] mapping))))
    (is (= {["A"] 2 ["A" "B"] 1} (as-map (sets/intersections [{:set "A" :item 1} {:set "A" :item 2} {:set "A" :item 3} {:set "B" :item 3}] mapping))))
    (is (= [] (sets/intersections [] mapping))))
  (testing "2.2 :sets chooses and orders; nil sets / items dropped"
    (is (= {["B" "A"] 20 ["A"] 40 ["B"] 30} (as-map (sets/intersections members mapping ["B" "A"]))))
    (is (= 2 (:dropped (sets/memberships (conj members {:set nil :item 1} {:set "A" :item nil}) mapping)))))
  (testing "2.3 set sizes, UpSet rows"
    (is (= {"A" 60 "B" 50 "C" 60} (sets/set-sizes members mapping)))
    (is (= {"A" 0 "B" 1} (sets/set-sizes [{:set "B" :item 1}] mapping ["A" "B"])) "a chosen set without items")
    (let [up (sets/upset-rows members mapping)]
      (is (= ["A" "C" "B & C" "B" "A & B" "A & B & C"] (map :combination up)) "size, then degree, then set order")
      (is (= [40 30 20 10 10 10] (map :size up)))
      (is (= [1 1 2 1 2 3] (map :degree up))))
    (let [dots (sets/membership-rows members mapping)]
      (is (= 18 (count dots)))
      (is (= #{"A" "B"} (set (map :set (filter #(and (= "A & B" (:combination %)) (:member? %)) dots))))))))

;; 3. venn

(defn- inside? [{:keys [x y r]} [px py]] (< (m/hypot (- (double px) (double x)) (- (double py) (double y))) (double r)))

(deftest venn-layout-test
  (testing "lens areas"
    (is (zero? (venn/lens-area 3.0 1.0 1.0)))
    (is (m/delta-eq (* m/PI 4.0) (venn/lens-area 0.5 2.0 3.0) 1e-12) "contained: the smaller circle")
    (is (m/delta-eq (venn/lens-area 1.2 1.0 0.7) (venn/lens-area 1.2 0.7 1.0) 1e-12)))
  (testing "3.2.1 two sets: areas and the lens proportional"
    (doseq [[a b o] [[60 50 20] [100 10 5] [40 40 39] [5 500 1]]
            :let [[c1 c2] (venn/two-sets a b o true)
                  lens (venn/lens-area (- (double (:x c2)) (double (:x c1))) (:r c1) (:r c2))]]
      (is (m/delta-eq (* m/PI (m/sq (:r c1))) (double a) 1e-9))
      (is (< (m/abs (- lens (double o))) (* 0.005 (double o))) (str [a b o]))))
  (testing "3.2.1 subset, disjoint, equal sets, an empty set"
    (let [[c1 c2] (venn/two-sets 100 30 30 true)]
      (is (m/delta-eq (- (:r c1) (:r c2)) (- (:x c2) (:x c1)) 1e-12) "touching from inside"))
    (let [[c1 c2] (venn/two-sets 10 10 0 true)]
      (is (> (- (:x c2) (:x c1)) (+ (:r c1) (:r c2))) "a gap"))
    (let [[c1 c2] (venn/two-sets 10 10 10 true)]
      (is (= (:x c1) (:x c2)) "coincident")
      (is (= [#{0 1}] (keys (venn/region-points [c1 c2])))))
    (let [[_ c2] (venn/two-sets 10 0 0 true)]
      (is (zero? (:r c2)))))
  (testing "3.2.3 / 3.2.2 equal circles"
    (is (apply = (map :r (venn/two-sets 100 10 5 false))))
    (is (= [1.0 1.0 1.0] (map :r (venn/three-sets)))))
  (testing "3.4 region points lie in exactly their region"
    (doseq [circles [(venn/three-sets) (venn/two-sets 60 50 20 true) (venn/two-sets 100 30 30 true) (venn/two-sets 60 50 0 true)]
            [region point] (venn/region-points circles)]
      (is (= region (set (keep-indexed (fn [i c] (when (inside? c point) i)) circles))) (str region))))
  (testing "region counts per layout"
    (is (= 7 (count (venn/region-points (venn/three-sets)))))
    (is (= 3 (count (venn/region-points (venn/two-sets 60 50 20 true)))))
    (is (= #{#{0} #{0 1}} (set (keys (venn/region-points (venn/two-sets 100 30 30 true))))) "subset: no region of the smaller set alone")
    (is (= #{#{0} #{1}} (set (keys (venn/region-points (venn/two-sets 60 50 0 true))))) "disjoint: no lens")))

(deftest venn-chart-test
  (let [p (fn [extra] (merge {:data members :mapping mapping :charts [(chart/venn)]} extra))
        build #(plot/build % {:width 400 :height 400})]
    (testing "3.1 set counts: 1 or more than 3 throw; :sets chooses"
      (is (= :sets (:key (error #(build (p {:data [{:set "A" :item 1}]}))))))
      (is (= :sets (:key (error #(build (p {:data (conj members {:set "D" :item 1})}))))))
      (is (str/includes? (ex-message (try (build (p {:data [{:set "A" :item 1}]})) (catch Exception e e))) "UpSet"))
      (is (= ["A" "B"] (get-in (build (p {:charts [(chart/venn {:sets ["A" "B"]})]})) [:stats 0 :data :panel :sets]))))
    (testing "stats: regions by set indices, total, dropped; no legend"
      (let [b (build (p {:data (conj members {:set nil :item 3})}))
            d (get-in b [:stats 0 :data :panel])]
        (is (= {#{0} 40 #{0 1} 10 #{0 1 2} 10 #{1 2} 20 #{1} 10 #{2} 30} (:regions d)))
        (is (= 120 (:total d)))
        (is (= [60 50 60] (:sizes d)))
        (is (= 1 (get-in b [:stats 0 :dropped :set])))
        (is (not-any? #(= :legends (:id %)) (get-in b [:grid :items])))))
    (testing "3.4 region labels: the function gets sets, size, share; one per region of the layout"
      (let [seen (atom [])]
        (plot/render-image (p {:charts [(chart/venn {:region-labels (fn [r] (swap! seen conj r) nil)})]}) {:width 400 :height 400})
        (is (= 7 (count @seen)))
        (is (some #(= {:sets ["A" "B" "C"] :size 10 :share (/ 10.0 120.0)} %) @seen))
        (is (some #(= {:sets ["A" "C"] :size 0 :share 0.0} %) @seen) "an empty region of the classic layout"))
      (let [seen (atom [])]
        (plot/render-image (p {:data (concat (for [i (range 100)] {:set "All" :item i}) (for [i (range 30)] {:set "Some" :item i}))
                               :charts [(chart/venn {:region-labels (fn [r] (swap! seen conj (:sets r)) nil)})]})
                           {:width 400 :height 400})
        (is (= #{["All"] ["All" "Some"]} (set @seen)))))
    (testing "3.3 overlaps mix the translucent fills"
      (let [[_ img {:keys [x y w h]}] (render (p {:charts [(chart/venn {:sets ["A" "B"] :region-labels false :set-labels false})]})
                                              {:width 400 :height 400})
            row (+ (long y) (quot (long h) 2))
            colors (set (for [i (range (long x) (+ (long x) (long w)) 5)] (rgb img i row)))]
        (is (<= 4 (count colors)) "background, A, B and their overlap along the centre line")))
    (testing "X7 every circle inside the panel, margins kept; empty data renders"
      (let [fit #'cljplot2.chart.venn/fit]
        (doseq [circles [(venn/three-sets) (venn/two-sets 60 50 20 true) (venn/two-sets 100 30 30 true) (venn/two-sets 60 50 0 true)]
                [w h mx my] [[300.0 500.0 0.0 0.0] [500.0 300.0 20.0 12.0] [400.0 400.0 30.0 30.0]]
                :let [to-pixel (fit circles w h mx my)
                      scale (double (:scale (meta to-pixel)))]
                {:keys [x y r]} circles
                :let [[px py] (to-pixel [x y]) pr (* scale (double r))]]
          (is (<= (- mx 1e-9) (- (double px) pr)))
          (is (<= (+ (double px) pr) (+ (- w mx) 1e-9)))
          (is (<= (- my 1e-9) (- (double py) pr)))
          (is (<= (+ (double py) pr) (+ (- h my) 1e-9)))))
      (is (some? (plot/render (p {:data []}) {:width 200 :height 200}))))
    (testing "X4 set names too wide for the panel: left out, the diagram still drawn"
      (let [long-name (apply str (repeat 40 "W"))
            [_ img {:keys [x y w h]}] (render (p {:data (map #(update % :set (fn [s] (if (= "A" s) long-name s))) members)
                                                  :charts [(chart/venn {:region-labels false})]})
                                              {:width 300 :height 300})]
        (is (not= [255 255 255] (rgb img (+ (long x) (quot (long w) 2)) (+ (long y) (quot (long h) 2)))))))
    (testing "X5 region labels in black or white by the painted color"
      (let [[_ img {:keys [x y w h]}] (render (p {:data (take 110 members) :charts [(chart/venn {:sets ["A" "B"] :alpha 1.0 :set-labels false})]
                                                  :theme {:palette {:color [[0 0 90] [0 0 90]]}}})
                                              {:width 400 :height 400})
            row (+ (long y) (quot (long h) 2))]
        (is (some (fn [[r g b]] (and (> (long r) 200) (> (long g) 200) (> (long b) 200) (< (long r) 255)))
                  (for [i (range (long x) (+ (long x) (long w)))] (rgb img i row)))
            "light text (antialiased white) on the dark fills")))
    (testing "3.5 docs"
      (is (str/includes? (:doc (meta (find-ns 'cljplot2.chart.venn))) "UpSet"))
      (is (str/includes? (:doc (meta (find-ns 'cljplot2.chart.waffle))) "largest remainder"))
      (is (str/includes? (:doc (meta (find-ns 'cljplot2.chart.venn))) "spills")))))

(deftest painted-color-test
  (testing "X5 the color painted under a user-space point, through the canvas transform"
    (let [canvas (clojure2d.core/canvas 50 40)]
      (clojure2d.core/with-canvas-> canvas
        (clojure2d.core/set-color :white) (clojure2d.core/rect 0 0 50 40)
        (clojure2d.core/translate 10 5)
        (clojure2d.core/set-color [0 0 200]) (clojure2d.core/rect 0 0 5 5))
      (let [g (clojure2d.core/with-canvas [c canvas] (clojure2d.core/translate c 10 5) (cljplot2.graphics.draw/painted-color c 2.0 2.0))]
        (is (= [0.0 0.0 200.0] (vec (take 3 g))))))
    (is (nil? (clojure2d.core/with-canvas [c (clojure2d.core/canvas 10 10)] (cljplot2.graphics.draw/painted-color c 50.0 5.0))))))
