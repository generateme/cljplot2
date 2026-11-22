(ns cljplot2.flow-cycle4-test
  "Flow cycle 4: sankey layout and plot, alluvial graph and plot, partition `:color-by :name`, sequence levels and the
  sequences sunburst."
  (:require [clojure.test :refer [deftest testing is are]]
            [fastmath.core :as m]
            [clojure2d.color :as c]
            [cljplot2.test-util :refer [error drawn-style]]
            [cljplot2.chart :as chart]
            [cljplot2.content.legend :as legend]
            [cljplot2.core :as plot]
            [cljplot2.data.graph :as graph]
            [cljplot2.data.hierarchy :as hierarchy]
            [cljplot2.pipeline :as pipeline])
  (:import [java.awt.image BufferedImage]))

(defn- rgb [^BufferedImage img x y]
  (let [v (.getRGB img (int x) (int y))]
    [(bit-and (bit-shift-right v 16) 0xff) (bit-and (bit-shift-right v 8) 0xff) (bit-and v 0xff)]))

(defn- close? [a b] (< (m/abs (- (double a) (double b))) 1e-9))

(defn- layout
  ([edges] (layout edges {}))
  ([edges opts] (graph/sankey-layout (graph/graph edges {:directed? true}) opts)))

(defn- by-id [nodes] (into {} (map (juxt :id identity)) nodes))

(def ^:private flows
  "Two sources into a hub, the hub to two sinks, one source straight to a sink."
  [{:from "a" :to "h" :value 3} {:from "b" :to "h" :value 1} {:from "h" :to "x" :value 2} {:from "h" :to "y" :value 2}
   {:from "b" :to "y" :value 2}])

;; 1. sankey layout

(deftest sankey-scale-test
  (let [{:keys [nodes links]} (layout flows)
        ky (/ (- (double (:y1 (first nodes))) (double (:y0 (first nodes)))) (double (:value (first nodes))))]
    (testing "1.1 node values - the larger of in and out; one scale for every node"
      (is (= {"a" 3.0 "b" 3.0 "h" 4.0 "x" 2.0 "y" 4.0} (into {} (map (juxt :id :value)) nodes)))
      (is (every? #(close? (* ky (double (:value %))) (- (double (:y1 %)) (double (:y0 %)))) nodes)))
    (testing "1.1 bands as thick as their value; stacked inside both their nodes"
      (is (every? #(close? (* ky (double (:value %))) (:width %)) links))
      (let [nodes (by-id nodes)]
        (doseq [[end k] [[:from :sy] [:to :ty]]
                [id ls] (group-by end links)
                :let [{:keys [y0 y1 value]} (nodes id)
                      tops (sort > (map k ls))
                      height (- (double y1) (double y0))
                      side (double (reduce + (map :width ls)))]]
          (is (close? y1 (first tops)) (str id " stack starts at the node's top"))
          (is (<= (- (double y0) 1e-9) (- (double (last tops)) (double (:width (last (sort-by k > ls)))))) id)
          (is (<= side (+ height 1e-9)) (str id " " end " bands within the node"))
          (when (close? value (reduce + (map :value ls)))
            (is (close? height side) (str id " " end " - the node's flow fills it"))))))
    (testing "1.1 a node with more flow out than in: its incoming bands fill only part of it"
      (let [{:keys [nodes links]} (layout [{:from "a" :to "b" :value 1} {:from "b" :to "c" :value 3}])
            b (first (filter #(= "b" (:id %)) nodes))
            in (first (filter #(= "b" (:to %)) links))]
        (is (close? (* 3.0 (double (:width in))) (- (double (:y1 b)) (double (:y0 b)))))))
    (testing "1.1 columns: nodes inside [0, 1], at least :pad apart"
      (doseq [[_ column] (group-by :layer nodes)
              [upper lower] (partition 2 1 (sort-by :y1 > column))]
        (is (>= (- (double (:y0 upper)) (double (:y1 lower))) (- 0.02 1e-9))))
      (is (every? #(<= -1e-9 (double (:y0 %)) (double (:y1 %)) (+ 1.0 1e-9)) nodes)))
    (testing "1.1 x - columns spread over [0, 1], :node-width wide"
      (is (= {"a" 0 "b" 0 "h" 1 "x" 2 "y" 2} (into {} (map (juxt :id :layer)) nodes)))
      (is (close? 0.0 (:x0 (first nodes))))
      (is (close? 1.0 (reduce max (map :x1 nodes))))
      (is (every? #(close? 0.03 (- (double (:x1 %)) (double (:x0 %)))) nodes)))
    (testing "1.1 polygon rows: a rectangle per node, a band per link (top curve, bottom back)"
      (let [{:keys [rects bands]} (layout flows)]
        (is (= (repeat 5 4) (vals (frequencies (map :rect rects)))))
        (is (= #{0 1 2 3 4} (set (map :link bands))))
        (is (every? #(= 50 %) (vals (frequencies (map :link bands)))))))))

(deftest sankey-columns-test
  (let [chain [{:from "a" :to "b" :value 1} {:from "b" :to "c" :value 1} {:from "a" :to "e" :value 1}]
        layers (fn [opts] (into {} (map (juxt :id :layer)) (:nodes (layout chain opts))))]
    (testing "1.1 :align - justify (sinks last), left, right, center"
      (is (= {"a" 0 "b" 1 "c" 2 "e" 2} (layers {})))
      (is (= {"a" 0 "b" 1 "c" 2 "e" 1} (layers {:align :left})))
      (is (= {"a" 0 "b" 1 "c" 2 "e" 2} (layers {:align :right})))
      (is (= {"s" 1 "t" 2 "u" 0 "v" 1}
             (into {} (map (juxt :id :layer))
                   (:nodes (layout [{:from "u" :to "v" :value 1} {:from "v" :to "t" :value 1} {:from "s" :to "t" :value 1}]
                                   {:align :center}))))
          "a source joins one column before its target"))
    (testing "1.1 :layer fixes the columns; backward or non-integer layers throw"
      (let [g (graph/graph chain {:directed? true :nodes [{:id "a" :c 0} {:id "b" :c 1} {:id "c" :c 3} {:id "e" :c 3}]})]
        (is (= {"a" 0 "b" 1 "c" 3 "e" 3} (into {} (map (juxt :id :layer)) (:nodes (graph/sankey-layout g {:layer :c}))))))
      (is (= :layer (:key (error #(graph/sankey-layout (graph/graph chain {:nodes [{:id "a" :c 1} {:id "b" :c 0} {:id "c" :c 2} {:id "e" :c 2}]})
                                                       {:layer :c})))))
      (is (= :layer (:key (error #(graph/sankey-layout (graph/graph chain {:nodes [{:id "a" :c 0.5}]}) {:layer :c})))))))
  (testing "1.1 relaxation uncrosses: a -> y, b -> x end on the same sides (crossed in the start order)"
    (let [crossing? (fn [opts]
                      (let [nodes (by-id (:nodes (graph/sankey-layout (graph/graph [{:from "a" :to "y" :value 1} {:from "b" :to "x" :value 1}]
                                                                                   {:nodes [{:id "a"} {:id "b"} {:id "x"} {:id "y"}]})
                                                                      opts)))]
                        (not= (> (double (:y0 (nodes "a"))) (double (:y0 (nodes "b"))))
                              (> (double (:y0 (nodes "y"))) (double (:y0 (nodes "x")))))))]
      (is (crossing? {:iterations 0}))
      (is (not (crossing? {})))))
  (testing "1.1 :iterations 0 and :node-sort :input keep the input order, top to bottom"
    (let [edges [{:from "a" :to "z" :value 1} {:from "b" :to "z" :value 5} {:from "c" :to "z" :value 1}]]
      (is (= ["a" "b" "c" "z"] (mapv :id (:nodes (layout edges {:iterations 0 :node-sort :input})))))
      (is (= ["a" "b" "c" "z"] (mapv :id (:nodes (layout edges {:node-sort :input}))))))))

(deftest sankey-boundary-test
  (testing "1.2 a cycle throws"
    (is (= :edges (:key (error #(layout [{:from "a" :to "b" :value 1} {:from "b" :to "a" :value 1}]))))))
  (testing "1.2 loops, zero flows and nodes without flow left out, counted"
    (let [{:keys [nodes dropped]} (graph/sankey-layout (graph/graph [{:from "a" :to "b" :value 1} {:from "a" :to "a" :value 2}
                                                                     {:from "a" :to "c" :value 0}]
                                                                    {:nodes [{:id "q"}]})
                                                       {})]
      (is (= #{"a" "b"} (set (map :id nodes))))
      (is (= {:edges 2 :nodes 2} dropped) "c and q"))
    (is (= {:edges 1 :nodes 0} (:dropped (layout [{:from "a" :to nil :value 1} {:from "a" :to "b" :value 1}])))
        "the graph's own drop - an edge with a nil end"))
  (testing "1.2 no flows - an empty layout; one link - two nodes edge to edge"
    (is (= [] (:nodes (layout []))))
    (let [[a b] (:nodes (layout [{:from "a" :to "b" :value 2}]))]
      (is (= [0.0 0.0 1.0 1.0] (mapv #(m/approx (double %) 9) [(:x0 a) (:y0 a) (:x1 b) (:y1 b)])))))
  (testing "1.2 bad options and a :pad leaving no room throw with their keys"
    (are [opts k] (= k (:key (error #(layout flows opts))))
      {:align :top} :align
      {:node-width 0} :node-width
      {:node-width 1} :node-width
      {:pad -0.1} :pad
      {:pad 1.0} :pad
      {:iterations -1} :iterations
      {:iterations 1.5} :iterations
      {:node-sort :value} :node-sort)
    (is (some? (layout flows {:pad 0.99})) "just enough room: 2 nodes in the fullest column")))

;; 2. sankey plot

(deftest sankey-plot-test
  (testing "2.1 bands in the source's / target's colour, grey for :none"
    (let [band-colors (fn [opts] (let [[bands] (:charts (plot/sankey flows opts))]
                                   (set (map (juxt :from :to #(get % :cljplot2.graphs/color)) (get-in bands [:options :data])))))]
      (is (contains? (band-colors {}) ["b" "y" "b"]))
      (is (contains? (band-colors {:link-color :target}) ["b" "y" "y"]))
      (is (= (c/to-color [150 150 150]) (c/to-color (:color (drawn-style (first (:charts (plot/sankey flows {:link-color :none}))))))))
      (is (= :link-color (:key (error #(plot/sankey flows {:link-color :from})))))))
  (testing "2.1 labels beside the nodes, inside the frame; legend only for a mapped colour"
    (let [p (plot/sankey flows {})
          [left right] (drop 2 (:charts p))]
      (is (= [:left :middle] (get-in left [:options :anchor])))
      (is (= #{"a" "b" "h"} (set (map :label (get-in left [:options :data])))))
      (is (= [:right :middle] (get-in right [:options :anchor])))
      (is (= false (get-in p [:scales :color :legend])))
      (is (= true (get-in (plot/sankey flows {:nodes [{:id "a" :kind "in"}] :mapping {:color #(or (:kind %) "other")}})
                          [:scales :color :legend])))
      (is (= :sankey-label (get-in left [:options :role])) "labels in the sankey role")
      (is (= (c/to-color [255 255 255 170]) (c/to-color (get-in cljplot2.theme/default-theme [:text :sankey-label :background]))) "on translucent boxes")
      (is (= 2 (count (:charts (plot/sankey flows {:labels? false})))))
      (is (instance? BufferedImage (plot/render-image p {:width 400 :height 300}))))))

;; 3. alluvial

(def ^:private people
  [{:class "1st" :sex "F" :age "adult" :n 4} {:class "1st" :sex "M" :age "adult" :n 6}
   {:class "2nd" :sex "F" :age "child" :n 1} {:class "2nd" :sex "M" :age "adult" :n 9}])

(deftest alluvial-graph-test
  (testing "3.1 a node per axis level in natural order, edges per consecutive pair and fill level"
    (let [g (graph/alluvial-graph people {:axes [:class :sex :age] :value :n})
          layout (graph/sankey-layout g {:layer :layer :node-sort :input :iterations 0})]
      (is (= [[0 "1st"] [0 "2nd"] [1 "F"] [1 "M"] [2 "adult"] [2 "child"]] (mapv :id (:nodes g))))
      (is (every? #(= 20.0 %) (vals (reduce (fn [acc {:keys [layer value]}] (update acc layer (fnil + 0.0) value)) {} (:nodes layout))))
          "every column sums to the total")
      (is (= ["1st" "1st" "1st" "1st" "2nd" "2nd" "2nd" "2nd"] (mapv :fill (:edges g))) "edges grouped by fill level")
      (is (= 8 (count (:edges g))))
      (is (= ["1st" "2nd" "F" "M" "adult" "child"] (mapv :name (:nodes layout))) "strata in level order, top to bottom")))
  (testing "3.1 default value - a row each; levels that don't compare - first appearance"
    (let [g (graph/alluvial-graph [{:a 2 :b "x"} {:a "q" :b "y"} {:a 2 :b "y"}] {:axes [:a :b]})]
      (is (= [[0 2] [0 "q"] [1 "x"] [1 "y"]] (mapv :id (:nodes g))))
      (is (= 3.0 (reduce + (map :value (:edges g)))))))
  (testing "3.1 values: nil counts 1, a string or a negative number throws"
    (is (= 2.0 (reduce + (map :value (:edges (graph/alluvial-graph [{:a 1 :b 2 :n nil} {:a 1 :b 2 :n 1}] {:axes [:a :b] :value :n}))))))
    (are [v] (= :value (:key (error #(graph/alluvial-graph [{:a 1 :b 2 :n v}] {:axes [:a :b] :value :n}))))
      "x" -1 ##NaN))
  (testing "3.1 fewer than 2 axes or a missing value throw"
    (is (= :axes (:key (error #(graph/alluvial-graph people {:axes [:class]})))))
    (is (= :axes (:key (error #(graph/alluvial-graph people {:axes nil})))))
    (is (= :axes (:key (error #(graph/alluvial-graph (conj people {:class "3rd"}) {:axes [:class :sex]})))))))

(deftest alluvial-plot-test
  (testing "3.2 strands by fill, grey strata, axis names as ticks"
    (let [p (plot/alluvial people {:axes [:class :sex :age] :value :n :fill :sex})
          [bands strata labels] (:charts p)]
      (is (= :fill (get-in bands [:options :mapping :color])))
      (is (= #{"F" "M"} (set (map :fill (get-in bands [:options :data])))))
      (is (= (c/to-color [150 150 150]) (c/to-color (:color (drawn-style strata)))))
      (is (= :text (:kind labels)))
      (is (= "sex" (get-in p [:scales :color :title])))
      (is (= ["class" "sex" "age"] (mapv (get-in p [:scales :x :formatter]) (get-in p [:scales :x :ticks]))))
      (is (instance? BufferedImage (plot/render-image p {:width 500 :height 300})))))
  (testing "3.2 :axis-names"
    (let [p (plot/alluvial people {:axes [:class :sex] :axis-names ["Class" "Sex"]})]
      (is (= ["Class" "Sex"] (mapv (get-in p [:scales :x :formatter]) (get-in p [:scales :x :ticks])))))))

;; 4. flow sunburst

(def ^:private named-rows [{:l1 "a" :l2 "a" :v 1} {:l1 "b" :l2 "c" :v 1}])

(defn- icicle [opts]
  {:data named-rows :mapping {:value :v :hierarchy [:l1 :l2]} :charts [(chart/partition opts)]
   :legend false :theme {:plot {:margin 0}}})

(deftest color-by-name-test
  (testing "4.1 equal names alike at every depth, other names apart"
    (let [img (plot/render-image (icicle {:color-by :name :group-labels false :leaf-labels false}) {:width 200 :height 200})
          a1 (rgb img 50 150) a2 (rgb img 50 50) b (rgb img 150 150)]
      (is (= a1 a2))
      (is (not= a1 b))))
  (testing "4.1 default :top unchanged: the child of a is lighter than a"
    (let [img (plot/render-image (icicle {:group-labels false :leaf-labels false}) {:width 200 :height 200})]
      (is (not= (rgb img 50 150) (rgb img 50 50)))))
  (testing "4.1 a legend of the names with the partition's glyphs"
    (let [built (pipeline/build (dissoc (icicle {:color-by :name}) :legend) {:width 300 :height 200})
          [content] (legend/legends built)]
      (is (= ["a" "b" "c"] (mapv :label (:entries content))))
      (is (= [{:chart 0 :dims [:color]}] (:charts content)))))
  (testing "4.1 :color-by :name with a mapped :color throws"
    (is (= :color-by (:key (error #(plot/build (assoc-in (icicle {:color-by :name}) [:mapping :color] :l1))))))))

(deftest sequence-levels-test
  (testing "4.2 steps split, the end marker removed, identical sequences summed, padded with nil"
    (let [{:keys [rows hierarchy dropped]} (hierarchy/sequence-levels [{:sequence "a-b-end"} {:sequence "a-b-end"} {:sequence "a-end"}
                                                                       {:sequence "a-b-c"} {:sequence ""} {:sequence nil}]
                                                                      {})]
      (is (= [:step-1 :step-2 :step-3] hierarchy))
      (is (= [{:step-1 "a" :step-2 "b" :step-3 nil :value 2.0} {:step-1 "a" :step-2 nil :step-3 nil :value 1.0}
              {:step-1 "a" :step-2 "b" :step-3 "c" :value 1.0}]
             rows))
      (is (= 2 dropped) "an empty string and nil")))
  (testing "4.2 sequences of steps, separators, values"
    (is (= [{:step-1 :x :step-2 :y :value 5.0}]
           (:rows (hierarchy/sequence-levels [{:s [:x :y :stop] :n 5}] {:sequence :s :value :n :end :stop}))))
    (is (= [{:step-1 "a" :step-2 "b" :value 1.0}]
           (:rows (hierarchy/sequence-levels [{:sequence "a.b"}] {:separator "."}))) "a literal separator, not a regex"))
  (testing "4.2 only end markers - dropped"
    (is (= 1 (:dropped (hierarchy/sequence-levels [{:sequence "end"}] {})))))
  (testing "4.2 :keep-end? - the end marker a step of its own (only at the end)"
    (is (= [{:step-1 "a" :step-2 "end" :value 1.0}] (:rows (hierarchy/sequence-levels [{:sequence "a-end"}] {:keep-end? true}))))
    (is (= [{:step-1 "end" :step-2 "a" :value 1.0}] (:rows (hierarchy/sequence-levels [{:sequence "end-a"}] {})))
        "an end marker inside a sequence is an ordinary step"))
  (testing "4.2 values: nil counts 1, a string or a negative number throws"
    (is (= [{:step-1 "a" :value 1.0}] (:rows (hierarchy/sequence-levels [{:sequence "a" :n nil}] {:value :n}))))
    (are [v] (= :value (:key (error #(hierarchy/sequence-levels [{:sequence "a" :n v}] {:value :n}))))
      "x" -1 ##Inf)))

(deftest sequences-plot-test
  (testing "4.3 partition by name under polar with a hole"
    (let [p (plot/sequences [{:sequence "home-search-end"} {:sequence "home-end"}] {:inner 0.4})]
      (is (= :name (get-in (first (:charts p)) [:options :color-by])))
      (is (= {:kind :polar :inner 0.4} (:coord p)))
      (is (instance? BufferedImage (plot/render-image p {:width 300 :height 300})))))
  (testing "4.3 the end marker kept by default, with a legend entry; :keep-end? false drops it"
    (let [rows [{:sequence "home-search-end"} {:sequence "home-end"}]
          names (fn [p] (set (keep identity (mapcat vals (map #(dissoc % :value) (:data p))))))]
      (is (contains? (names (plot/sequences rows {})) "end"))
      (is (not (contains? (names (plot/sequences rows {:keep-end? false})) "end")))
      (is (= ["end" "home" "search"]
             (mapv :label (:entries (first (legend/legends (pipeline/build (plot/sequences rows {}) {:width 300 :height 300}))))))))))
