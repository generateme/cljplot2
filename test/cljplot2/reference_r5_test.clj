(ns cljplot2.reference-r5-test
  "Reference part R5 - leaf keys of edge bundling (`tree/leaf-key`: a from-parents id, else the leaf name) and
  `plot/bundle` failing when no edge joins two leaves."
  (:require [clojure.test :refer [deftest testing is]]
            [fastmath.ml.clustering :as cl]
            [cljplot2.core :as plot]
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
