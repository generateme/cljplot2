(ns cljplot2.vega-cycle3c-test
  "Vega cycle 3c review: legend key glyphs clipped to whole pixels around their box (no cropped size keys), arc
  diagram `:labels`, the node-link tree (`data.tree/node-link-layout`, `tidy-breadths`, `plot/tree`)."
  (:require [clojure.test :refer [deftest testing is]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.tree :as tree]
            [cljplot2.test-util :refer [error]])
  (:import [java.awt.image BufferedImage]))

;; legend

(defn- red-box
  "`[w h]` of the bounding box of the exact red pixels of `img`."
  [^BufferedImage img]
  (let [red (unchecked-int 0xffff0000)
        pts (for [x (range (.getWidth img)) y (range (.getHeight img)) :when (== red (.getRGB img (int x) (int y)))] [x y])
        xs (map first pts) ys (map second pts)]
    [(inc (- (long (reduce max xs)) (long (reduce min xs)))) (inc (- (long (reduce max ys)) (long (reduce min ys))))]))

(deftest legend-size-key-test
  (testing "the largest size key's hollow circle is drawn whole (as tall as wide)"
    (doseq [size [14 20 23.5]]
      (let [img (plot/render-image {:data [{:x 5 :y 5 :g "a"}] :mapping {:x :x :y :y :size :g}
                                    :charts [(chart/scatter {:shape \o :color :red :stroke-width 1.5})]
                                    :scales {:x {:domain [0 1]} :y {:domain [0 1]} :size {:range [size size]}}}
                                   {:width 300 :height 150})
            [w h] (red-box img)]
        (is (<= (Math/abs (- (long w) (long h))) 1) [size w h])))))

;; arc labels

(def ^:private nodes [{:index 0 :name "Alice"} {:index 1 :name "Bob"} {:index 2 :name nil}])
(def ^:private links [{:source 0 :target 1} {:source 1 :target 2}])

(defn- arc-labels
  [options]
  (let [p (plot/arc links (merge {:from :source :to :target :nodes nodes :id :index} options))
        f (get-in p [:scales :x :formatter])]
    (mapv f [0 1 2])))

(deftest arc-labels-test
  (testing "default - the node ids; an accessor - its values (nil - empty)"
    (is (= ["0" "1" "2"] (arc-labels {})))
    (is (= ["Alice" "Bob" ""] (arc-labels {:labels :name})))
    (is (= ["ALICE" "BOB" ""] (arc-labels {:labels #(some-> (:name %) clojure.string/upper-case)}))))
  (testing ":labels? false - no axis; the theme turns the labels"
    (is (false? (get-in (plot/arc links {:from :source :to :target :nodes nodes :id :index :labels? false}) [:scales :x :axis])))
    (is (= -90 (get-in (plot/arc links {:from :source :to :target :nodes nodes :id :index
                                       :theme {:axis {:x {:label-rotation -90}}}})
                       [:theme :axis :x :label-rotation])))))

;; node-link tree

(defn- parents->tree [rows] (tree/from-parents rows {}))

(def ^:private small (parents->tree [{:id 1} {:id 2 :parent 1} {:id 3 :parent 1} {:id 4 :parent 2} {:id 5 :parent 2}
                                     {:id 6 :parent 2} {:id 7 :parent 3}]))

(def ^:private lopsided
  "A tree whose deep left subtree and wide right one must not overlap at depth 3."
  (parents->tree (concat [{:id 1} {:id 2 :parent 1} {:id 3 :parent 1} {:id 4 :parent 2} {:id 5 :parent 4}
                          {:id 6 :parent 5} {:id 7 :parent 5} {:id 8 :parent 3} {:id 9 :parent 8}]
                         (map (fn [i] {:id i :parent 9}) (range 10 16)))))

(defn- min-gap
  "The smallest breadth gap between nodes of one depth (nil - no depth has two nodes)."
  [nodes]
  (let [gaps (for [[_ ns] (group-by :depth nodes) :let [bs (sort (map :breadth ns))] [a b] (partition 2 1 bs)] (- (double b) (double a)))]
    (when (seq gaps) (reduce min gaps))))

(defn- centred?
  "Is every group at the middle of its first and last child?"
  [nodes]
  (every? (fn [[p ks]] (or (nil? p) (< (Math/abs (- (double (:breadth (nth nodes p)))
                                                 (* 0.5 (+ (double (:breadth (first ks))) (double (:breadth (last ks)))))))
                                     1e-9)))
          (group-by :parent nodes)))

(deftest tidy-layout-test
  (testing "tidy: nodes of a depth at least 1 apart, groups centred over their children, depth by level"
    (doseq [t [small lopsided]
            :let [{:keys [nodes]} (tree/node-link-layout t {})]]
      (is (>= (double (min-gap nodes)) 1.0))
      (is (centred? nodes))
      (is (every? #(== (double (:x %)) (double (:depth %))) nodes))
      (is (== 0.0 (double (reduce min (map :breadth nodes)))) "breadths from 0")))
  (testing "boundary: one node, a chain"
    (is (= [[0.0 0.0]] (mapv (juxt :x :y) (:nodes (tree/node-link-layout (parents->tree [{:id 1}]) {})))))
    (is (empty? (:links (tree/node-link-layout (parents->tree [{:id 1}]) {}))))
    (let [{:keys [nodes]} (tree/node-link-layout (parents->tree [{:id 1} {:id 2 :parent 1} {:id 3 :parent 2}]) {})]
      (is (= [0.0 0.0 0.0] (mapv :breadth nodes)))
      (is (= [0 1 2] (mapv :depth nodes)))))
  (testing "cluster: leaves at the deepest depth, one step apart"
    (let [{:keys [nodes]} (tree/node-link-layout lopsided {:layout :cluster})
          leaves (filter :leaf? nodes)]
      (is (apply = (map :x leaves)))
      (is (= (map double (range (count leaves))) (sort (map :breadth leaves))))))
  (testing "links: a path per non-root node from its parent; :line - two points, :diagonal - 17"
    (let [{:keys [nodes links]} (tree/node-link-layout small {:links :line})]
      (is (= (dec (count nodes)) (count (distinct (map :id links)))))
      (is (every? #(= 2 (count %)) (vals (group-by :id links))))
      (is (= [(:x (nth nodes (:parent (nth nodes 1)))) (:y (nth nodes (:parent (nth nodes 1))))]
             ((juxt :x :y) (first (filter #(= 1 (:id %)) links))))))
    (is (every? #(= 17 (count %)) (vals (group-by :id (:links (tree/node-link-layout small {})))))))
  (testing "circular: inside the unit circle, the root at the centre, the deepest nodes on it"
    (let [{:keys [nodes links]} (tree/node-link-layout lopsided {:circular true})]
      (is (every? #(<= (Math/hypot (double (:x %)) (double (:y %))) (+ 1.0 1e-9)) (concat nodes links)))
      (is (= [0.0 0.0] ((juxt :x :y) (first nodes))))
      (is (== 1.0 (double (reduce max (map :radius nodes))))))))

(deftest tree-builder-test
  (testing "renders: tidy / cluster, diagonal / line, cartesian / radial, a single node"
    (doseq [opts [{} {:layout :cluster} {:links :line} {:circular true} {:circular true :layout :cluster}
                  {:labels? false :nodes? false} {:mapping {:color #(str (:depth %))}}]]
      (is (some? (plot/render-image (plot/tree small opts) {:width 400 :height 300})) opts))
    (is (some? (plot/render-image (plot/tree (parents->tree [{:id 1 :name "root"}])) {:width 200 :height 150}))))
  (testing "labels: leaves right of their node, groups left; unnamed nodes none"
    (let [texts (filter #(= :text (:kind %)) (:charts (plot/tree small {})))]
      (is (= [[:left :middle] [:right :middle]] (mapv #(get-in % [:options :anchor]) texts))))
    (is (empty? (filter #(= :text (:kind %))
                        (:charts (plot/tree (tree/from-levels [{:a "x" :b "y"}] {:hierarchy [:a :b]}) {:labels? false}))))))
  (testing "bad :layout / :links"
    (is (= :layout (:key (error #(plot/tree small {:layout :radial})))))
    (is (= :links (:key (error #(plot/tree small {:links :curve})))))))
