(ns cljplot2.reference-r5-test
  "Reference part R5 - leaf keys of edge bundling (`tree/leaf-key`: a from-parents id, else the leaf name),
  `plot/bundle` failing when no edge joins two leaves; aesthetic scale `:integer-ticks` (whole legend entries and colour
  bar ticks)."
  (:require [clojure.test :refer [deftest testing is]]
            [fastmath.ml.clustering :as cl]
            [cljplot2.chart :as chart]
            [cljplot2.content.legend :as legend]
            [cljplot2.core :as plot]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.theme :as theme]
            [cljplot2.data.tree :as tree]
            [cljplot2.test-util :refer [error-data]]))

(def ^:private offices
  [{:region "Europe" :city "Warsaw"} {:region "Europe" :city "Paris"} {:region "Asia" :city "Tokyo"}])

(def ^:private level-tree (tree/from-levels offices {:hierarchy [:region :city]}))

(deftest leaf-key-test
  (testing "a from-parents node by its id, a from-levels / from-hclust leaf by its name"
    (is (= "w" (tree/leaf-key {:id "w" :name "Warsaw"})))
    (is (= "Warsaw" (tree/leaf-key {:name "Warsaw"})))
    (is (= #{"a" "b" "c"} (set (map tree/leaf-key (tree/leaves (tree/from-hclust (cl/hclust [[0.0] [1.0] [5.0]] {:method :complete})
                                                                               ["a" "b" "c"])))))))
  (testing "boundary: an id of nil is still the id; no id and no name - nil"
    (is (nil? (tree/leaf-key {:id nil :name "x"})))
    (is (nil? (tree/leaf-key {})))))

(deftest bundle-paths-leaf-keys-test
  (let [laid (tree/layout level-tree {:circular true})
        paths (fn [edges] (tree/bundle-paths edges laid {}))]
    (testing "edges between leaves of a tree from levels, named by the leaf names"
      (let [{:keys [paths dropped]} (paths [{:from "Warsaw" :to "Tokyo"} {:from "Paris" :to "Warsaw"}])]
        (is (= 0 dropped))
        (is (= #{0 1} (set (map :id paths))))))
    (testing "boundary: an end that isn't a leaf (a group, an unknown name), a loop - dropped and counted; no edges"
      (is (= 3 (:dropped (paths [{:from "Europe" :to "Tokyo"} {:from "Oslo" :to "Paris"} {:from "Paris" :to "Paris"}]))))
      (is (= {:paths [] :dropped 0} (paths []))))
    (testing "repeated leaf names: the first leaf"
      (let [twice (tree/layout (tree/from-levels [{:a "x" :b "same"} {:a "y" :b "same"} {:a "y" :b "other"}]
                                                 {:hierarchy [:a :b]})
                               {:circular true})
            {:keys [paths dropped]} (tree/bundle-paths [{:from "same" :to "other"}] twice {})]
        (is (= 0 dropped))
        (is (seq paths))))))

(deftest bundle-nothing-drawn-test
  (testing "plot/bundle throws {:key :edges} when every edge is dropped (nothing would be drawn)"
    (is (= :edges (:key (error-data #(plot/bundle [{:from "Oslo" :to "Rome"}] level-tree {}))))))
  (testing "boundary: some edges kept - a plot; no edges at all - a plot of the leaves"
    (is (map? (plot/bundle [{:from "Oslo" :to "Rome"} {:from "Warsaw" :to "Tokyo"}] level-tree {})))
    (is (map? (plot/bundle [] level-tree {})))))

(deftest integer-ticks-test
  (testing "the whole ticks; fewer than two - the whole numbers of the domain (at most n)"
    (is (= [0.0 2.0 4.0] (aesthetic/integer-ticks [0.0 1.5 2.0 3.5 4.0] 0 4 5)))
    (is (= [2.0 3.0] (aesthetic/integer-ticks [2.0 2.2 2.4 2.6 2.8 3.0] 2 3 5)) "two whole ticks kept")
    (is (= [3.0 4.0 5.0] (aesthetic/integer-ticks [3.0 3.5 4.0 4.5 5.0] 3 5 5))))
  (testing "boundary: a domain within one gap - no whole number, ticks as they are; one whole number; negatives; more
            whole numbers than n - the whole ticks found"
    (is (= [0.2 0.4 0.6] (aesthetic/integer-ticks [0.2 0.4 0.6] 0.1 0.7 5)))
    (is (= [1.0] (aesthetic/integer-ticks [0.8 1.2] 0.7 1.3 5)))
    (is (= [-2.0 -1.0 0.0] (aesthetic/integer-ticks [-2.0 -1.5 -1.0 -0.5 0.0] -2 0 5)))
    (is (= [10.0] (aesthetic/integer-ticks [9.5 10.0 10.5] 2.5 17.5 5)))
    (is (= [] (aesthetic/integer-ticks [] 0.1 0.2 5)))))

(defn- size-spec [extra]
  (aesthetic/scale-spec :size (merge {} extra) {:kind :numeric :min 2.0 :max 3.0} false (:palette theme/default-theme)))

(deftest integer-ticks-legends-test
  (testing "a size legend: counted entries fractional, whole with :integer-ticks; an explicit :ticks vector as given"
    (is (< 2 (count (aesthetic/legend-entries :size (size-spec {})))))
    (is (= [2.0 3.0] (mapv :value (aesthetic/legend-entries :size (size-spec {:integer-ticks true})))))
    (is (= ["2" "3"] (mapv :label (aesthetic/legend-entries :size (size-spec {:integer-ticks true})))))
    (is (= [2.5] (mapv :value (aesthetic/legend-entries :size (size-spec {:integer-ticks true :ticks [2.5]}))))))
  (testing "a colour bar: whole ticks with :integer-ticks"
    (let [spec (aesthetic/scale-spec :color {:integer-ticks true} {:kind :numeric :min 0.0 :max 1.0} false (:palette theme/default-theme))]
      (is (= [0.0 1.0] (:ticks (#'legend/colorbar-content spec [:vs]))))))
  (testing "boundary: other kinds and dimensions, dates - errors keyed :integer-ticks; false is fine everywhere"
    (is (= :integer-ticks (:key (error-data #(plot/build {:data [{:x 1 :y 1 :s "a"}] :mapping {:x :x :y :y :shape :s}
                                                           :charts [(chart/scatter)] :scales {:shape {:integer-ticks true}}})))))
    (is (= :integer-ticks (:key (error-data #(plot/build {:data [{:x 1 :y 1 :c (java.time.LocalDate/of 2024 1 1)}
                                                                  {:x 2 :y 2 :c (java.time.LocalDate/of 2024 2 1)}]
                                                           :mapping {:x :x :y :y :color :c}
                                                           :charts [(chart/scatter)] :scales {:color {:integer-ticks true}}})))))
    (is (map? (plot/build {:data [{:x 1 :y 1 :c (java.time.LocalDate/of 2024 1 1)}] :mapping {:x :x :y :y :color :c}
                           :charts [(chart/scatter)] :scales {:color {:integer-ticks false}}})))))

(deftest integer-ticks-colorbar-boundary-test
  (let [bar (fn [spec extent] (:ticks (#'legend/colorbar-content
                                       (aesthetic/scale-spec :color spec extent false (:palette theme/default-theme)) [:v])))]
    (testing "an explicit :midpoint tick is drawn as given, among the whole ticks"
      (is (= [2.0 2.5 3.0] (bar {:integer-ticks true :midpoint 2.5} {:kind :numeric :min 2.0 :max 3.0}))))
    (testing "a log colour bar keeps its whole powers of ten - one on [0.01 1]; several on [1 1000]"
      (is (= [1.0] (bar {:integer-ticks true :transform :log} {:kind :numeric :min 0.01 :max 1.0})))
      (is (every? #(== % (Math/floor %)) (bar {:integer-ticks true :transform :log} {:kind :numeric :min 1.0 :max 1000.0}))))))
