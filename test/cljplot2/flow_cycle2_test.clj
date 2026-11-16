(ns cljplot2.flow-cycle2-test
  "Flow cycle 2: trees (hclust, levels, parents), tree layout, dendrograms, clustered heatmap, edge bundling."
  (:require [clojure.test :refer [deftest testing is are]]
            [fastmath.core :as m]
            [fastmath.ml.clustering :as cl]
            [cljplot2.test-util :refer [error]]
            [cljplot2.core :as plot]
            [cljplot2.data.tree :as tree]
            [cljplot2.layout.grid :as grid])
  (:import [java.awt.image BufferedImage]))

(def ^:private points [[1.0 1.0] [1.2 1.1] [5.0 5.0] [5.1 5.3] [9.0 0.0]])
(def ^:private result (cl/hclust points {:method :complete}))
(def ^:private names ["a" "b" "c" "d" "e"])

(defn- groups [t] (filter :children (tree-seq :children :children t)))

;; 1. trees

(deftest from-hclust-test
  (testing "1.1 merges, heights, leaf order"
    (let [t (tree/from-hclust result names)]
      (is (= (mapv names (:order (:tree result))) (mapv :name (tree/leaves t))) "leaves in :order")
      (is (= (sort (:height (:tree result))) (sort (map :height (groups t)))) "one group per merge, its height")
      (is (= (apply max (:height (:tree result))) (:height t)) "the root - the last merge")
      (is (every? #(zero? (double (:height %))) (tree/leaves t)))
      (is (= #{0 1 2 3 4} (set (map :index (tree/leaves t)))))))
  (testing "1.1 labels: indices without, a wrong count throws; one observation"
    (is (= #{"0" "1" "2" "3" "4"} (set (map :name (tree/leaves (tree/from-hclust result))))))
    (is (= :labels (:key (error #(tree/from-hclust result ["a" "b"])))))
    (is (= {:name "x" :index 0 :height 0.0} (tree/from-hclust (cl/hclust [[1.0 2.0]] {}) ["x"])))))

(deftest from-levels-test
  (testing "1.2 heights from the deepest leaf; missing levels end the path"
    (let [rows [{:r "A" :s "a1" :c "x"} {:r "A" :s "a1" :c "y"} {:r "A" :s "a2" :c "z"} {:r "B" :s "b1" :c "w"} {:r "B" :s nil}]
          t (tree/from-levels rows {:hierarchy [:r :s :c]})]
      (is (== 3.0 (double (:height t))))
      (is (= [2.0 2.0] (mapv :height (:children t))) "regions")
      (is (= ["x" "y" "z" "w" nil] (mapv :name (tree/leaves t))) "B's row without a subregion - an unnamed leaf of B")
      (is (every? #(zero? (double (:height %))) (tree/leaves t)))))
  (testing "1.2 no rows / no levels - an empty root"
    (is (= [] (:children (tree/from-levels [] {:hierarchy [:r]}))))
    (is (= [] (:children (tree/from-levels [{:r nil}] {:hierarchy [:r]}))))))

(deftest from-parents-test
  (let [rows [{:id 1 :name "root"} {:id 2 :parent 1 :name "p"} {:id 3 :parent 2 :name "x"} {:id 4 :parent 2 :name "y"} {:id 5 :parent 1 :name "z"}]]
    (testing "1.3 one root, children in row order, names, heights"
      (let [t (tree/from-parents rows {:name :name})]
        (is (= ["x" "y" "z"] (mapv :name (tree/leaves t))))
        (is (= [3 4 5] (mapv :id (tree/leaves t))))
        (is (= 2.0 (:height t)))
        (is (= [1.0 0.0] (mapv :height (:children t))) "a leaf one level up is still height 0")
        (is (= [1 2 3 4 5] (sort (map :id (tree-seq :children :children t)))))))
    (testing "1.3 names default to ids"
      (is (= [3 4 5] (mapv :name (tree/leaves (tree/from-parents rows {}))))))
    (testing "1.3 no root, several roots, unknown parent, repeated id, cycle"
      (are [rs] (= :parent (:key (error #(tree/from-parents rs {}))))
        [{:id 1 :parent 2} {:id 2 :parent 1}]
        [{:id 1} {:id 2}]
        [{:id 1} {:id 2 :parent 9}]
        [{:id 1} {:id 1 :parent 1}]
        [{:id 1} {:id 2 :parent 3} {:id 3 :parent 2}]
        []))))

;; 1.4-1.6 layout

(def ^:private three {:height 2.0 :children [{:height 1.0 :children [{:name "a" :height 0.0} {:name "b" :height 0.0}]}
                                             {:name "c" :height 0.0}]})

(deftest layout-test
  (testing "1.4 leaves 0..n-1, a group at its children's mean x, y = height; elbow paths"
    (let [{:keys [nodes paths]} (tree/layout three {})
          at (into {} (map (juxt (juxt :name :y) :x) nodes))]
      (is (= {[nil 2.0] 1.25 [nil 1.0] 0.5 ["a" 0.0] 0.0 ["b" 0.0] 1.0 ["c" 0.0] 2.0} at))
      (is (= 4 (count (distinct (map :id paths)))) "a path per non-root node")
      (is (= [[0.0 0.0] [0.0 1.0] [0.5 1.0]] (mapv (juxt :x :y) (filter #(= 2 (:id %)) paths))) "a: up to its parent, across")
      (is (every? nil? (map :cluster paths)) "no :k - no clusters")))
  (testing "1.5 circular: leaves at radius 1 from the top clockwise, the root in the centre, arcs at the parent radius"
    (let [{:keys [nodes paths]} (tree/layout three {:circular true})
          r (fn [{:keys [x y]}] (m/hypot (double x) (double y)))]
      (is (every? #(< (m/abs (- 1.0 (r %))) 1.0e-9) (filter :leaf? nodes)))
      (is (< (r (first nodes)) 1.0e-9))
      (is (= [0.0 120.0 240.0] (mapv #(m/approx (double (:angle %)) 6) (filter :leaf? nodes))))
      (let [p (nodes 1) arc (rest (map (juxt :x :y) (filter #(= 2 (:id %)) paths)))]
        (is (every? (fn [[x y]] (< (m/abs (- (double (:radius p)) (m/hypot (double x) (double y)))) 1.0e-9)) arc)))))
  (testing "1.5 partial span: the leaves spread over it"
    (is (= [0.0 90.0 180.0] (mapv #(m/approx (double (:angle %)) 6) (filter :leaf? (:nodes (tree/layout three {:circular true :span 180})))))))
  (testing "1.6 :k - the same partition as fastmath cut; above the cut nil"
    (let [t (tree/from-hclust result names)]
      (doseq [k [1 2 3 5]]
        (let [{:keys [nodes]} (tree/layout t {:k k})
              ours (set (vals (group-by :cluster (filter :leaf? nodes))))
              theirs (set (map (comp set :indices) (:clusters (cl/cut result points {:k k}))))]
          (is (= theirs (set (map #(set (map :index %)) ours))) (str "k = " k))))
      (is (= #{1 2} (set (keep :cluster (:nodes (tree/layout t {:k 2}))))))
      (is (nil? (:cluster (first (:nodes (tree/layout t {:k 2}))))) "the root above the cut")
      (is (= 1 (:cluster (first (:nodes (tree/layout t {:k 1}))))) "k 1 - everything one cluster")
      (are [k] (= :k (:key (error #(tree/layout t {:k k}))))
        0 6))))

;; 2. dendrogram

(deftest dendrogram-test
  (testing "2.1 an hclust result: paths, leaf ticks and names, height axis"
    (let [p (plot/dendrogram result {:labels names :k 2})]
      (is (instance? BufferedImage (plot/render-image p {:width 400 :height 300})))
      (is (= [0 1 2 3 4] (get-in p [:scales :x :ticks])))
      (is (= (names (first (:order (:tree result)))) ((get-in p [:scales :x :formatter]) 0.0)))
      (is (not= false (get-in p [:scales :y :axis])) "height axis shown")
      (is (= -90 (get-in p [:theme :axis :x :label-rotation])))))
  (testing "2.1 trees: levels / parents hide the height axis; horizontal flips; circular is iso with radial labels"
    (let [lt (tree/from-levels [{:r "A" :c "x"} {:r "A" :c "y"} {:r "B" :c "z"}] {:hierarchy [:r :c]})]
      (is (false? (get-in (plot/dendrogram lt {}) [:scales :y :axis])))
      (is (true? (:flip (plot/dendrogram lt {:horizontal true}))))
      (let [c (plot/dendrogram lt {:circular true})]
        (is (= :iso (:aspect c)))
        (is (= 3 (count (:charts c))) "branches and two label halves")
        (is (instance? BufferedImage (plot/render-image c {:width 300 :height 300}))))
      (is (= 1 (count (:charts (plot/dendrogram lt {:circular true :labels? false})))))
      (is (false? (get-in (plot/dendrogram lt {:labels? false}) [:scales :x :axis])))))
  (testing "2.2 the height axis spans 0 to the root height"
    (is (= [0.0 (apply max (:height (:tree result)))] (get-in (plot/dendrogram result {}) [:scales :y :domain]))))
  (testing "2.1 one leaf"
    (is (instance? BufferedImage (plot/render-image (plot/dendrogram {:name "x" :height 0.0} {}) {:width 200 :height 200})))))

;; 3. clustered heatmap

(def ^:private cells
  (vec (for [[r row] (map vector ["r1" "r2" "r3" "r4"] [[1 2 3] [1.1 2.1 2.9] [9 8 7] [9.2 8.1 7.3]])
             [c v] (map vector ["c1" "c2" "c3"] row)]
         {:r r :c c :v v})))

(deftest clustered-heatmap-test
  (testing "3.1 tile order = the clustering's leaf order; leaves at their tiles' centres"
    (let [p (plot/clustered-heatmap cells {:row :r :col :c :value :v})
          [_ col-plot row-plot heat] (get-in p [:arrange :plots])]
      (is (= 4 (count (get-in p [:arrange :plots]))))
      (is (= #{"r1" "r2"} (set (take 2 (get-in heat [:scales :y :domain])))) "similar rows together")
      (is (= [-0.5 3.5] (get-in row-plot [:scales :x :domain])) "row dendrogram: a step per row")
      (is (= [-0.5 2.5] (get-in col-plot [:scales :x :domain])))
      (is (instance? BufferedImage (plot/render-image p {:width 500 :height 500})))))
  (testing "3.1 dendrogram panels aligned with the tiles (same x / width over columns, y / height over rows): a leaf
            at x = i in [-0.5, n - 0.5] sits at its tile band's centre"
    (let [p (plot/clustered-heatmap cells {:row :r :col :c :value :v})
          built (plot/build p {:width 600 :height 600})
          box (fn [i] (grid/item-box (get-in built [:subplots [:plot i] :layout]) :panel))
          col-tree (box 1) row-tree (box 2) heat (box 3)]
      (is (every? some? [col-tree row-tree heat]))
      (is (<= (m/abs (- (double (:x heat)) (double (:x col-tree)))) 1.0) "column dendrogram over the tiles")
      (is (<= (m/abs (- (double (:w heat)) (double (:w col-tree)))) 1.0))
      (is (<= (m/abs (- (double (:y heat)) (double (:y row-tree)))) 1.0) "row dendrogram beside the tiles")
      (is (<= (m/abs (- (double (:h heat)) (double (:h row-tree)))) 1.0))))
  (testing "3.2 missing / non-finite cell, too few rows or columns"
    (is (= :value (:key (error #(plot/clustered-heatmap (pop cells) {:row :r :col :c :value :v})))))
    (is (= :value (:key (error #(plot/clustered-heatmap (assoc-in cells [0 :v] ##NaN) {:row :r :col :c :value :v})))))
    (is (= :rows (:key (error #(plot/clustered-heatmap (filterv (fn [c] (= "r1" (:r c))) cells) {:row :r :col :c :value :v})))))
    (is (= :rows (:key (error #(plot/clustered-heatmap (filterv (fn [c] (= "c1" (:c c))) cells) {:row :r :col :c :value :v}))))))
  (testing "3.3 order only"
    (is (= 1 (count (get-in (plot/clustered-heatmap cells {:row :r :col :c :value :v :row-tree? false :col-tree? false}) [:arrange :plots]))))
    (is (= 2 (count (get-in (plot/clustered-heatmap cells {:row :r :col :c :value :v :row-tree? false}) [:arrange :plots]))))))

;; 4. bundling

(def ^:private package-tree
  (tree/from-parents [{:id :root} {:id :p :parent :root} {:id :q :parent :root}
                      {:id :a :parent :p} {:id :b :parent :p} {:id :c :parent :q} {:id :d :parent :q}] {}))

(deftest bundle-test
  (let [laid (tree/layout package-tree {:circular true})
        pos (into {} (map (juxt :id (juxt :x :y)) (:nodes laid)))
        path (fn [edges beta] (mapv (juxt :x :y) (:paths (tree/bundle-paths edges laid {:beta beta}))))]
    (testing "4.1 beta 0 - a straight line; ends at the leaves"
      (let [ps (path [{:from :a :to :c}] 0.0)
            [x0 y0] (pos :a) [x1 y1] (pos :c)]
        (is (= [(m/approx x0 9) (m/approx y0 9)] (mapv #(m/approx % 9) (first ps))))
        (is (= [(m/approx x1 9) (m/approx y1 9)] (mapv #(m/approx % 9) (peek ps))))
        (is (every? (fn [[x y]] (< (m/abs (- (* (- (double x1) (double x0)) (- (double y) (double y0)))
                                               (* (- (double y1) (double y0)) (- (double x) (double x0)))))
                                   1.0e-9))
                    ps))))
    (testing "4.1 beta 1 - drawn towards the tree path (through the centre): closer to the root than the straight line"
      (let [closest (fn [ps] (apply min (map (fn [[x y]] (m/hypot (double x) (double y))) ps)))]
        (is (< (closest (path [{:from :a :to :c}] 1.0)) (closest (path [{:from :a :to :c}] 0.5)) (closest (path [{:from :a :to :c}] 0.0))))))
    (testing "4.1 siblings: a short path through their parent"
      (is (< (count (path [{:from :a :to :b}] 0.85)) (count (path [{:from :a :to :c}] 0.85)))))
    (testing "4.2 non-leaf, unknown or self edges dropped and counted; beta validated"
      (is (= 3 (:dropped (tree/bundle-paths [{:from :a :to :p} {:from :a :to :zz} {:from :a :to :a} {:from :a :to :d}] laid {}))))
      (are [b] (= :beta (:key (error #(tree/bundle-paths [] laid {:beta b}))))
        -0.1 1.1 "x"))
    (testing "4.2 the builder: lines by group, leaf points, radial labels; renders"
      (let [p (plot/bundle [{:from :a :to :c} {:from :b :to :d} {:from :c :to :b}] package-tree {})]
        (is (= :iso (:aspect p)))
        (is (= 4 (count (:charts p))))
        (is (instance? BufferedImage (plot/render-image p {:width 400 :height 400})))
        (is (= 2 (count (:charts (plot/bundle [{:from :a :to :c}] package-tree {:labels? false})))))
        (is (some? (get-in (plot/bundle [{:from :a :to :c}] package-tree {:color-by-group? false}) [:charts 0 :options :color])))))))
