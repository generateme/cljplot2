(ns cljplot2.flow-notebook-test
  "Flow notebook review: theme colours, `plot/bundle` `:scales`, text `:reserve-room` for circular labels, scale
  `:axis :no-grid`, force layouts packing connected components."
  (:require [clojure.test :refer [deftest testing is]]
            [fastmath.random :as r]
            [clojure2d.color :as c]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.graph :as graph]
            [cljplot2.data.tree :as tree]
            [cljplot2.scale.position :as scale]
            [cljplot2.test-util :refer [error-ins]]
            [cljplot2.theme :as theme])
  (:import [java.awt.image BufferedImage]))

(defn- stats [p] (first (:stats (plot/build p {:width 400 :height 400}))))

(deftest theme-colours-test
  (testing "U1 lighter background, alternate panel fill and polar bands"
    (is (= (c/to-color 0xececf3) (c/to-color (get-in theme/default-theme [:plot :background]))))
    (is (= (c/to-color [0 0 0 12]) (c/to-color (get-in theme/default-theme [:panel :alternate-fill]))))
    (is (= (c/to-color [0 0 0 8]) (c/to-color (get-in theme/default-theme [:panel :grid :bands-fill]))))))

(def ^:private small-tree (tree/from-parents [{:id 1} {:id 2 :parent 1} {:id 3 :parent 1} {:id 4 :parent 2}] {}))

(deftest bundle-scales-test
  (testing "R1 :scales merged key by key over the builder's"
    (let [p (plot/bundle [{:from 3 :to 4}] small-tree {:scales {:color {:legend false}}})]
      (is (= {:title nil :legend false} (get-in p [:scales :color])))
      (is (= [-1.05 1.05] (get-in p [:scales :x :domain])) "the circle axis kept")))
  (testing "R1 without :scales - the builder's"
    (is (= {:title nil} (get-in (plot/bundle [{:from 3 :to 4}] small-tree {}) [:scales :color])))))

(deftest reserve-room-test
  (let [rows [{:x 0 :y 0 :t "a long label"} {:x 1 :y 1 :t "b"}]
        room (fn [opts] (:padding-px (stats {:data rows :charts [(chart/text (merge {:mapping {:x :x :y :y :label :t}} opts))]})))]
    (testing "R2 :reserve-room - the longest label's diagonal on every side of x and y"
      (let [{:keys [x y]} (room {:reserve-room true})]
        (is (= x y))
        (is (apply = x))
        (is (< 50.0 (double (first x)) 150.0) "12 characters of the annotation font")))
    (testing "R2 the offset adds; a larger font adds"
      (let [base (first (:x (room {:reserve-room true})))]
        (is (< (double base) (double (first (:x (room {:reserve-room true :offset [10 0]}))))))
        (is (< (double base) (double (first (:x (room {:reserve-room true :size 16}))))))))
    (testing "R2 off by default; literal labels too"
      (is (nil? (room {})))
      (is (some? (:padding-px (stats {:charts [(chart/text {:x 0 :y 0 :text "label" :reserve-room true})]})))))
    (testing "R2 with :select the end room and the reserved room merge per side (the larger)"
      (let [{:keys [x]} (room {:reserve-room true :select :last})
            reserved (first (:x (room {:reserve-room true})))]
        (is (<= (double reserved) (double (first x))))
        (is (<= (double reserved) (double (second x))))))
    (testing "R2 radial labels of the circular builders keep their room: labels inside the image"
      (let [p (plot/chord [{:from "South East Asia" :to "North America" :value 1} {:from "North America" :to "Oceania" :value 2}] {})]
        (is (every? #(get-in % [:options :reserve-room]) (drop 2 (:charts p))))
        (is (instance? BufferedImage (plot/render-image p {:width 300 :height 300})))))))

(deftest axis-no-grid-test
  (testing "R3 :axis :no-grid - the axis drawn, no grid lines"
    (is (scale/axis-drawn? {:axis :no-grid}))
    (is (not (scale/grid-drawn? {:axis :no-grid})))
    (is (scale/grid-drawn? {:axis :grid}))
    (is (not (scale/axis-drawn? {:axis :grid})))
    (is (and (scale/axis-drawn? {}) (scale/grid-drawn? {}))))
  (testing "R3 the finalized scale keeps it; the schema takes it, rejects others"
    (let [built (plot/build {:data [{:x 1 :y 1}] :mapping {:x :x :y :y} :charts [(chart/scatter)]
                             :scales {:x {:axis :no-grid}}}
                            {:width 300 :height 300})]
      (is (= :no-grid (get-in built [:scales :x :axis]))))
    (is (seq (error-ins #(plot/build {:data [{:x 1 :y 1}] :mapping {:x :x :y :y} :charts [(chart/scatter)]
                                      :scales {:x {:axis :ticks}}})))))
  (testing "R3 leaf axes of dendrograms and arc diagrams without grid lines"
    (is (= :no-grid (get-in (plot/arc [{:from "a" :to "b"}] {}) [:scales :x :axis])))
    (is (= false (get-in (plot/arc [{:from "a" :to "b"}] {:labels? false}) [:scales :x :axis])))))

(defn- boxes
  "Bounding boxes `[x0 x1 y0 y1]` of the laid-out nodes of each id group in `groups`."
  [nodes groups]
  (let [by-id (into {} (map (juxt :id identity)) nodes)]
    (mapv (fn [ids] (let [ps (map by-id ids)]
                      [(reduce min (map :x ps)) (reduce max (map :x ps)) (reduce min (map :y ps)) (reduce max (map :y ps))]))
          groups)))

(defn- overlap? [[ax0 ax1 ay0 ay1] [bx0 bx1 by0 by1]]
  (and (< (double bx0) (double ax1)) (< (double ax0) (double bx1)) (< (double by0) (double ay1)) (< (double ay0) (double by1))))

(def ^:private two-triangles
  (for [[a b] [["a" "b"] ["b" "c"] ["a" "c"] ["x" "y"] ["y" "z"] ["x" "z"]]] {:from a :to b}))

(deftest packed-components-test
  (testing "R4 components laid out apart, boxes not overlapping, inside the unit square"
    (let [nodes (:nodes (graph/layout (graph/graph two-triangles {:nodes [{:id "q"}]}) {:rng (r/rng :mersenne 1)}))
          [abc xyz q] (boxes nodes [["a" "b" "c"] ["x" "y" "z"] ["q"]])]
      (is (not (overlap? abc xyz)))
      (is (not (overlap? abc q)))
      (is (not (overlap? xyz q)))
      (is (every? #(<= -1e-9 (double (:x %)) (+ 1.0 1e-9)) nodes))
      (is (every? #(<= -1e-9 (double (:y %)) (+ 1.0 1e-9)) nodes))))
  (testing "R4 the largest component first, the next one in the row below (it doesn't fit beside)"
    (let [nodes (:nodes (graph/layout (graph/graph (concat [{:from "p" :to "q"}] two-triangles [{:from "c" :to "x"}]))
                                      {:rng (r/rng :mersenne 1)}))
          [big small] (boxes nodes [["a" "b" "c" "x" "y" "z"] ["p" "q"]])]
      (is (< (double (nth small 3)) (double (nth big 2))) "the small one's top under the big one's bottom")))
  (testing "R4 a connected graph - one force layout, seeded: the same positions every time"
    (let [run #(mapv (juxt :x :y) (:nodes (graph/layout (graph/graph two-triangles) {:rng (r/rng :mersenne 2)})))]
      (is (= (run) (run)))))
  (testing "R4 only isolated nodes, a single node, no nodes"
    (let [nodes (:nodes (graph/layout (graph/graph [] {:nodes (mapv (fn [i] {:id i}) (range 5))}) {:rng (r/rng :mersenne 1)}))]
      (is (= 5 (count (distinct (map (juxt :x :y) nodes))))))
    (is (= [[0.5 0.5]] (mapv (juxt :x :y) (:nodes (graph/layout (graph/graph [] {:nodes [{:id 1}]}) {:rng (r/rng :mersenne 1)})))))
    (is (= [] (:nodes (graph/layout (graph/graph []) {}))))))
