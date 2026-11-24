(ns cljplot2.part-of-whole-cycle2-test
  "Part of a whole cycle 2: the circle packing layout and the pack chart."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is are]]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [cljplot2.test-util :refer [error]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.hierarchy :as hierarchy]
            [cljplot2.layout.grid :as grid]
            [cljplot2.layout.pack :as pack])
  (:import [java.awt.image BufferedImage]
           [fastmath.vector Vec2]))

;; 1. layout

(defn- dist ^double [[x1 y1] [x2 y2]] (m/hypot (- (double x1) (double x2)) (- (double y1) (double y2))))

(defn- overlapping [rs cs]
  (for [i (range (count rs)) j (range (inc i) (count rs))
        :when (< (dist (cs i) (cs j)) (* (+ (double (rs i)) (double (rs j))) (- 1.0 1e-9)))]
    [i j]))

(defn- random-radii [n seed] (let [g (r/rng :mersenne seed)] (vec (repeatedly n #(* 1e-3 (m/pow 1e6 (r/drandom g)))))))

(deftest pack-siblings-test
  (testing "1.1 none, one, two"
    (is (= [] (pack/pack-siblings [])))
    (is (= [[0.0 0.0]] (pack/pack-siblings [5])))
    (is (= [[-2.0 0.0] [1.0 0.0]] (pack/pack-siblings [1 2])) "touching on the x axis, contact at the origin"))
  (testing "1.1 no overlap; from the third on each circle touches two earlier ones"
    (doseq [[n seed] [[3 1] [5 2] [50 3] [1000 4]]
            :let [rs (random-radii n seed) cs (pack/pack-siblings rs)]]
      (is (= n (count cs)))
      (is (empty? (overlapping rs cs)) (str n " circles"))
      (is (every? (fn [i] (<= 2 (count (filter #(< (m/abs (- (dist (cs i) (cs %)) (+ (double (rs i)) (double (rs %)))))
                                                 (* 1e-7 (+ (double (rs i)) (double (rs %)))))
                                               (range i)))))
                  (range 2 n))
          (str n " circles: tangent placements"))))
  (testing "equal radii, repeated"
    (let [rs (vec (repeat 40 1.0))]
      (is (empty? (overlapping rs (pack/pack-siblings rs))))))
  (testing "1.3 deterministic"
    (is (= (pack/pack-siblings (random-radii 300 9)) (pack/pack-siblings (random-radii 300 9))))))

(defn- contains-circle? [{:keys [x y r]} c]
  (<= (+ (dist [x y] [(:x c) (:y c)]) (double (:r c))) (+ (double r) (* 1e-9 (m/max 1.0 (double r))))))

(defn- brute-enclose
  "Smallest of the candidate circles (single circles, pairs, Apollonius triples) containing all."
  [cs]
  (let [e2 #'pack/enclosing-2 e3 #'pack/enclosing-3
        n (count cs)
        candidates (concat cs
                           (for [i (range n) j (range (inc i) n)] (e2 (cs i) (cs j)))
                           (for [i (range n) j (range (inc i) n) k (range (inc j) n)
                                 :let [e (e3 (cs i) (cs j) (cs k))]
                                 :when (m/valid-double? (double (:r e)))]
                             e))]
    (apply min-key :r (filter (fn [e] (every? #(contains-circle? e %) cs)) candidates))))

(defn- enclosing-radius
  "Radius of the smallest circle centred at `[x y]` containing circles `cs`."
  ^double [cs x y]
  (reduce max (map #(+ (dist [x y] [(:x %) (:y %)]) (double (:r %))) cs)))

(defn- searched-radius
  "Smallest enclosing radius by compass search over centres (a convex function), independent of the layout's formulas."
  ^double [cs]
  (loop [x (/ (reduce + (map :x cs)) (count cs)) y (/ (reduce + (map :y cs)) (count cs)) step 10.0]
    (if (< step 1e-12)
      (enclosing-radius cs x y)
      (let [here (enclosing-radius cs x y)
            [bx by br] (apply min-key #(nth % 2) (for [[dx dy] [[1 0] [-1 0] [0 1] [0 -1] [0.7 0.7] [-0.7 0.7] [0.7 -0.7] [-0.7 -0.7]]
                                                       :let [nx (+ x (* step dx)) ny (+ y (* step dy))]]
                                                   [nx ny (enclosing-radius cs nx ny)]))]
        (if (< (double br) here) (recur bx by step) (recur x y (* 0.5 step)))))))

(deftest enclose-test
  (testing "1.2 one, two"
    (is (= {:x 1 :y 2 :r 3} (pack/enclose [{:x 1 :y 2 :r 3}])))
    (is (= {:x 0.0 :y 0.0 :r 2.0} (pack/enclose [{:x -1 :y 0 :r 1} {:x 1 :y 0 :r 1}])))
    (is (= 3.0 (double (:r (pack/enclose [{:x 0 :y 0 :r 3} {:x 1 :y 0 :r 1}])))) "a circle inside another")
    (is (= 2.0 (double (:r (pack/enclose [{:x 0 :y 0 :r 2} {:x 0 :y 0 :r 2}])))) "repeated"))
  (testing "1.2 random sets: contains all, equals the brute force"
    (doseq [seed (range 30)
            :let [g (r/rng :mersenne seed)
                  n (+ 2 (mod seed 7))
                  cs (vec (repeatedly n (fn [] {:x (r/drandom g -10 10) :y (r/drandom g -10 10) :r (r/drandom g 0.1 4)})))
                  e (pack/enclose cs)
                  b (brute-enclose cs)]]
      (is (every? #(contains-circle? e %) cs) (str "seed " seed))
      (is (< (m/abs (- (double (:r e)) (double (:r b)))) (* 1e-9 (double (:r b)))) (str "seed " seed))
      (is (<= (double (:r e)) (+ (searched-radius cs) 1e-7)) (str "seed " seed ": no smaller circle found by search")))))

(defn- tree-of [rows hierarchy]
  (hierarchy/ordered-tree (get-in (hierarchy/panel-trees {:p rows} {:value :v :hierarchy hierarchy} {}) [:trees :p]) :value))

(def ^:private rows
  [{:a "A" :b "a1" :v 4} {:a "A" :b "a2" :v 1} {:a "A" :b "a3" :v 2} {:a "B" :b "b1" :v 6} {:a "B" :b "b2" :v 3} {:a "C" :v 2}])

(defn- nodes [tree] (tree-seq :children :children tree))

(deftest pack-tree-test
  (let [box {:x 10.0 :y 20.0 :w 400.0 :h 300.0}]
    (testing "1.4.3 the root fits the shorter side, centred"
      (let [p (pack/pack (tree-of rows [:a :b]) box 0)]
        (is (m/delta-eq 150.0 (:r p) 1e-9))
        (is (= [210.0 170.0] [(:x p) (:y p)]))))
    (testing "1.4.1 leaf areas proportional to values (no padding)"
      (let [p (pack/pack (tree-of rows [:a :b]) box 0)
            leaves (remove :children (nodes p))
            k (/ (m/sq (:r (first leaves))) (:value (first leaves)))]
        (is (every? #(< (m/abs (- (/ (m/sq (:r %)) (:value %)) k)) (* 1e-6 k)) leaves))))
    (testing "1.4.2 / 1.4.4 children inside their parents, gaps of the padding (pixels, as d3's)"
      (doseq [padding [0 1.5 5 20]
              :let [p (pack/pack (tree-of rows [:a :b]) box padding)
                    gap (- (double padding) 0.05)]]
        (doseq [g (filter :children (nodes p))
                :let [cs (:children g)]]
          (is (every? #(<= (+ (dist [(:x g) (:y g)] [(:x %) (:y %)]) (:r %)) (- (:r g) gap)) cs)
              (str "padding " padding ": child to parent edge"))
          (is (every? (fn [[a b]] (>= (- (dist [(:x a) (:y a)] [(:x b) (:y b)]) (:r a) (:r b)) gap))
                      (for [i (range (count cs)) j (range (inc i) (count cs))] [(cs i) (cs j)]))
              (str "padding " padding ": siblings")))
        (is (<= (- 10.0 1e-6) (- (double (:x p)) (double (:r p)))) "root inside the box")
        (is (<= (+ (double (:y p)) (double (:r p))) (+ 320.0 1e-6)))))
    (testing "V1 the fitted gap: exact within 0.01 px where it fits; the widest a small box holds"
      (let [min-gap (fn [t] (reduce min (for [g (filter :children (nodes t)) :let [cs (:children g)]
                                              i (range (count cs)) j (range (inc i) (count cs))
                                              :let [a (cs i) b (cs j)]]
                                          (- (dist [(:x a) (:y a)] [(:x b) (:y b)]) (:r a) (:r b)))))
            t (tree-of rows [:a :b])]
        (doseq [[padding side] [[6 200] [12 200] [2 60]]]
          (is (< (m/abs (- (double (min-gap (pack/pack t {:x 0 :y 0 :w side :h side} padding))) (double padding))) 0.011)
              (str "padding " padding " in " side " px")))
        (let [small (pack/pack t {:x 0 :y 0 :w 40 :h 40} 30)
              wider (pack/pack t {:x 0 :y 0 :w 40 :h 40} 60)]
          (is (< (double (min-gap small)) 30.0) "too small for 30 px gaps")
          (is (every? #(m/valid-double? (double (:r %))) (nodes small)))
          (is (< (m/abs (- (double (min-gap small)) (double (min-gap wider)))) 0.5) "saturated: the widest gaps it holds"))))
    (testing "one leaf: centred, as large as the root"
      (let [p (pack/pack (tree-of [{:a "A" :v 3}] [:a]) box 0)
            leaf (first (:children p))]
        (is (= [(:x p) (:y p)] [(:x leaf) (:y leaf)]))
        (is (m/delta-eq (:r p) (:r leaf) 1e-9))))
    (testing "an empty box, a padding larger than the box: finite"
      (is (every? #(zero? (double (:r %))) (nodes (pack/pack (tree-of rows [:a :b]) (assoc box :w 0.0) 3))))
      (is (every? #(m/valid-double? (double (:r %))) (nodes (pack/pack (tree-of rows [:a :b]) {:x 0 :y 0 :w 20 :h 20} 50)))))
    (testing "1.5 2000 leaves in three levels"
      (let [big (tree-of (for [a (range 10) b (range 10) c (range 20)] {:a (str a) :b (str a "-" b) :c (str a b c) :v (inc (mod (* 7 (+ a b c)) 13))})
                         [:a :b :c])
            _ (pack/pack big box 1.5)
            t0 (System/nanoTime)
            _ (pack/pack big box 1.5)
            ms (/ (- (System/nanoTime) t0) 1e6)]
        (is (< ms 1000.0) (str ms " ms (500 ms nominal, slack for load)"))))))

;; 2. chart

(def ^:private mapping {:value :v :hierarchy [:a :b]})
(def ^:private size {:width 400 :height 400})
(defn- build [p] (plot/build p size))
(defn- pk [extra] (merge {:data rows :mapping mapping :charts [(chart/pack)]} extra))

(deftest schema-and-stats-test
  (testing "2.1 mapping, options, theme"
    (is (= [:charts 0 :options :mapping :hierarchy] (:in (error #(build (pk {:mapping {:value :v}}))))))
    (are [opts k] (= k (:key (error #(build (pk {:charts [(chart/pack opts)]})))))
      {:order :size} :order
      {:root "x"} :root
      {:hidden-levels #{-1}} :hidden-levels
      {:hidden-levels [1]} :hidden-levels
      {:gap -1} :gap
      {:leaf-labels 1} :leaf-labels
      {:pattern :nope} :pattern)
    (is (some? (error #(build (pk {:theme {:chart {:pack {:padding "1"}}}}))))))
  (testing "2.1 / 2.2 a panel without axes or position scales"
    (let [built (build (pk {}))]
      (is (not-any? #{:x :y} (keys (:scale-specs built))))
      (is (not-any? #(#{:axis :axis-overhang} (get-in % [:content :kind])) (get-in built [:grid :items])))
      (is (nil? (get-in built [:stats 0 :extents :panel :x])))))
  (testing "facets: a tree per panel; empty data and tiny canvases render"
    (is (= 2 (count (get-in (build (pk {:data (map #(assoc % :f (if (= "A" (:a %)) "p" "q")) rows) :mapping (assoc mapping :facet :f)}))
                            [:stats 0 :data]))))
    (is (= {} (get-in (build (pk {:data []})) [:stats 0 :data])))
    (is (some? (plot/render (pk {:data []}) size)))
    (is (some? (plot/render (pk {}) {:width 20 :height 20})))))

(def ^:private plain {:plot {:background :white} :palette {:color [[255 230 150] [200 230 255] [190 255 190]] :na [128 128 128]}
                      :chart {:pack {:gap 0 :stroke-color :none}}})

(defn- rgb [^BufferedImage img x y]
  (let [v (.getRGB img (int x) (int y))]
    [(bit-and (bit-shift-right v 16) 0xff) (bit-and (bit-shift-right v 8) 0xff) (bit-and v 0xff)]))

(defn- render
  "`[built img packed]` - `packed` the panel's layout in image pixels."
  [p]
  (let [p (update p :theme #(merge plain %))
        built (build p)
        img (plot/render-image p size)
        {:keys [x y w h]} (grid/item-box (:layout built) :panel)
        {:keys [tree style]} (get-in built [:stats 0 :data :panel])]
    [built img (when tree (pack/pack tree {:x (double x) :y (double y) :w (double w) :h (double h)} (:gap style)))]))

(defn- named [packed n] (some #(when (= n (:name %)) %) (nodes packed)))
(defn- at [[_ img] {:keys [x y]}] (rgb img (m/round (double x)) (m/round (double y))))
(defn- above-children
  "A point of group `g` above its children (inside its ring)."
  [g]
  (let [top (reduce min (map #(- (double (:y %)) (double (:r %))) (:children g)))]
    {:x (:x g) :y (* 0.5 (+ top (- (double (:y g)) (double (:r g)))))}))

(defn- close? [a b] (every? true? (map #(<= (m/abs (- (long %1) (long %2))) 1) a b)))
(defn- color-rgb [col] (let [col (clojure2d.color/to-color col)] (mapv #(long (Math/round (double %))) [(clojure2d.color/red col) (clojure2d.color/green col) (clojure2d.color/blue col)])))

(def ^:private no-labels [(chart/pack {:leaf-labels false :group-labels false})])

(deftest colors-test
  (testing "2.4 / 2.5 top-level colours, deeper levels shaded; children over their groups"
    (let [[_ _ packed :as r] (render (pk {:charts no-labels}))
          [c-a c-b] (get-in plain [:palette :color])]
      (is (close? (color-rgb (hierarchy/depth-shade (first (get-in plain [:palette :color])) 2)) (at r (named packed "a1"))))
      (is (close? (color-rgb (hierarchy/depth-shade c-b 2)) (at r (named packed "b1"))))
      (is (= c-a (at r (above-children (named packed "A")))) "group A's ring")
      (is (= [190 255 190] (at r (named packed "C"))) "a top-level leaf")))
  (testing "2.4 mapped :color: leaves mapped, groups NA, legend"
    (let [[built _ packed :as r] (render (pk {:data (map #(assoc % :k (if (= "A" (:a %)) [0 150 0] [150 0 150])) rows)
                                              :mapping (assoc mapping :color :k) :charts no-labels :scales {:color {:kind :identity}}}))]
      (is (= [0 150 0] (at r (named packed "a1"))))
      (is (= [128 128 128] (at r (above-children (named packed "B")))))))
  (testing "2.4 :root true - the root in NA"
    (let [[_ _ packed :as r] (render (pk {:data [{:a "L" :v 1} {:a "R" :v 1}] :charts [(chart/pack {:root true :leaf-labels false})]}))]
      (is (= [128 128 128] (at r {:x (:x packed) :y (- (double (:y packed)) (* 0.8 (double (:r packed))))})))))
  (testing "2.7 :hidden-levels - no rings at that depth, leaves in place"
    (let [[_ _ packed :as r] (render (pk {:charts [(chart/pack {:hidden-levels #{1} :leaf-labels false :group-labels false})]}))]
      (is (not= (first (get-in plain [:palette :color])) (at r (above-children (named packed "A")))))
      (is (close? (color-rgb (hierarchy/depth-shade (first (get-in plain [:palette :color])) 2)) (at r (named packed "a1"))))))
  (testing "patterns and borders draw"
    (let [[_ ^BufferedImage img packed] (render (pk {:charts [(chart/pack {:pattern :hatch :leaf-labels false :group-labels false})]}))
          {:keys [x y]} (named packed "b1")
          colors (set (for [i (range -6 6) j (range -6 6)] (rgb img (+ (long (m/round (double x))) i) (+ (long (m/round (double y))) j))))]
      (is (< 1 (count colors)))))
  (testing "1.3 the same plot renders the same image"
    (let [px (fn [] (let [[_ ^BufferedImage img] (render (pk {}))] (vec (.getRGB img 0 0 400 400 nil 0 400))))]
      (is (= (px) (px))))))

(defn- dark? [[r g b]] (and (< (long r) 80) (< (long g) 80) (< (long b) 80)))

(defn- dark-in [[_ ^BufferedImage img] {:keys [x y r]} below-top?]
  (count (filter dark? (for [i (range (- (long (m/round (double x))) (long (double r))) (+ (long (m/round (double x))) (long (double r))))
                             j (if below-top?
                                 (range (- (long (m/round (double y))) (long (double r))) (long (m/round (double y))))
                                 (range (- (long (m/round (double y))) 6) (+ (long (m/round (double y))) 6)))
                             :when (and (< -1 i 400) (< -1 j 400))]
                         (rgb img i j)))))

(deftest labels-test
  (testing "2.6.1 leaf labels: centred when they fit"
    (let [[_ _ packed :as r] (render (pk {:charts [(chart/pack {:group-labels false})]}))]
      (is (pos? (dark-in r (named packed "b1") false))))
    (let [long-name (apply str (repeat 40 "W"))
          [_ _ packed :as r] (render (pk {:data (conj rows {:a "C" :b long-name :v 1}) :charts [(chart/pack {:group-labels false})]}))]
      (is (zero? (dark-in r (named packed long-name) false)))))
  (testing "2.6.2 group labels under the circle's top"
    (let [[_ _ packed :as r] (render (pk {:charts [(chart/pack {:leaf-labels false})]}))]
      (is (pos? (dark-in r (named packed "B") true))))
    (let [long-name (apply str (repeat 60 "W"))
          [_ _ packed :as r] (render (pk {:data (map #(cond-> % (= "B" (:a %)) (assoc :a long-name)) rows)
                                          :charts [(chart/pack {:leaf-labels false})]}))]
      (is (zero? (dark-in r (named packed long-name) true)))))
  (testing "labels in white on dark fills"
    (let [[_ ^BufferedImage img packed] (render (pk {:data [{:a "Group" :v 1}] :theme {:palette {:color [[0 0 110]]}}}))
          {:keys [x y]} (named packed "Group")]
      (is (some (fn [[r g b]] (and (> (long r) 200) (> (long g) 200) (> (long b) 200))) (for [i (range -30 30) j (range -6 6)] (rgb img (+ (long (m/round (double x))) i) (+ (long (m/round (double y))) j))))))))

(deftest legend-and-docs-test
  (testing "2.8 the legend key is a circle"
    (let [p (update (pk {:mapping (assoc mapping :color :a) :charts no-labels}) :theme #(merge plain %))
          built (build p)
          ^BufferedImage img (plot/render-image p size)
          legend (some #(when (= :legends (:id %)) %) (get-in built [:grid :items]))
          {:keys [x y]} (grid/item-box (:layout built) :legends)]
      (is (some? legend))
      (is (some? x))
      (is (some #(= [255 230 150] %) (for [i (range (long x) (min 400 (+ (long x) 40))) j (range (long y) (min 400 (+ (long y) 60)))] (rgb img i j))))))
  (testing "2.9 docs"
    (let [doc (:doc (meta (find-ns 'cljplot2.chart.pack)))]
      (is (str/includes? doc ":hierarchy [:name]"))
      (is (str/includes? doc "not proportional"))
      (is (str/includes? doc "the gap between"))
      (is (str/includes? doc "not across panels")))
    (is (str/includes? (:doc (meta #'chart/pack)) "cljplot2.chart.pack"))))

(deftest label-placements-test
  (let [placements #'cljplot2.chart.pack/label-placements
        size-of (fn [{:keys [text]}] (Vec2. (* 6.0 (count text)) 12.0))
        fill-of (fn [{:keys [depth]}] (if (= 1 depth) [0 0 120] [250 250 200]))
        line (constantly 9)
        parent {:x 100.0 :y 100.0 :r 80.0 :depth 1 :leaf? false :label "Parent"}
        child {:x 100.0 :y 104.0 :r 75.0 :depth 2 :leaf? false :label "Child"}]
    (testing "V3 a label overlapping an ancestor's isn't drawn"
      (is (= ["Parent"] (map (comp :text :content) (placements [parent child] size-of fill-of line)))))
    (testing "V3 separate labels are all drawn"
      (is (= ["Parent" "leaf"] (map (comp :text :content)
                                    (placements [parent {:x 100.0 :y 120.0 :r 30.0 :depth 2 :leaf? true :label "leaf"}] size-of fill-of line)))))
    (testing "V5 backdrop (the role's :contrast colour is resolved by it): the circle painted under the label - the
              child's light fill, not the parent's dark one"
      (let [[{:keys [backdrop]}] (placements [parent (assoc child :y 100.0 :r 78.0 :label nil)] size-of fill-of line)]
        (is (= [250 250 200] backdrop))))
    (testing "the parent's dark fill when no child lies under the label"
      (let [[{:keys [backdrop]}] (placements [parent {:x 100.0 :y 150.0 :r 20.0 :depth 2 :leaf? true :label nil}] size-of fill-of line)]
        (is (= [0 0 120] backdrop))))
    (testing "circles narrower than a line aren't measured"
      (is (empty? (placements [{:x 10.0 :y 10.0 :r 4.0 :depth 1 :leaf? true :label "x"}]
                              (fn [_] (throw (ex-info "measured" {}))) fill-of line))))))
