(ns cljplot2.code-review-r3a-f4-test
  "Code review round 3, batch A, top-level namespaces: builders without options, radial builder labels in the
  `:radial-label` text role, error keys naming the option or mapping (never a stage), one sliced-scale error."
  (:require [clojure.test :refer [deftest testing is]]
            [fastmath.ml.clustering :as cl]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.tree :as tree]
            [cljplot2.test-util :refer [error]]))

(def ^:private size {:width 300 :height 300})

(def ^:private flows
  [{:from "a" :to "h" :value 3} {:from "b" :to "h" :value 1} {:from "h" :to "x" :value 2} {:from "h" :to "y" :value 2}
   {:from "b" :to "y" :value 2}])

(def ^:private hc (cl/hclust [[1.0 1.0] [1.2 1.1] [5.0 5.0] [5.1 5.3] [9.0 0.0]] {:method :complete}))

(def ^:private package-tree
  (tree/from-parents [{:id :root} {:id :p :parent :root} {:id :q :parent :root}
                      {:id :a :parent :p} {:id :b :parent :p} {:id :c :parent :q} {:id :d :parent :q}]
                     {}))

(def ^:private leaf-edges [{:from :a :to :c} {:from :b :to :d}])

(deftest builders-without-options-test
  (testing "every builder without a required option takes its data alone"
    (doseq [p [(plot/network flows) (plot/chord flows) (plot/arc flows) (plot/sankey flows)
               (plot/sequences [{:sequence "a-b-end"} {:sequence "a-end"} {:sequence "b-a-c"}]) (plot/dendrogram hc)
               (plot/bundle leaf-edges package-tree) (plot/tree package-tree)]]
      (is (map? (plot/build p size))))))

(defn- radial-texts
  "Gives the text charts of a built plot drawing in the `:radial-label` role."
  [p]
  (filter #(= :radial-label (get-in % [:options :role])) (:charts p)))

(deftest radial-labels-role-test
  (testing "circular builders label leaves in the :radial-label role"
    (doseq [p [(plot/dendrogram hc {:circular true}) (plot/tree package-tree {:circular true})
               (plot/bundle leaf-edges package-tree) (plot/chord flows)]]
      (is (seq (radial-texts p)))))
  (testing "the size is capped at the role's size"
    (is (every? #(= 9.0 (double (get-in % [:options :size]))) (radial-texts (plot/chord flows))))
    (is (every? #(= 6.0 (double (get-in % [:options :size])))
                (radial-texts (plot/chord flows {:theme {:text {:radial-label {:size 6}}}})))))
  (testing "a theme restyles them"
    (is (map? (plot/build (plot/chord flows {:theme {:text {:radial-label {:color :red}}}}) size)))))

(deftest error-keys-test
  (testing "mixed value kinds on a mapped dimension name the mapping"
    (let [data (error #(plot/build {:data [{:x 1 :y 1} {:x "a" :y 2}] :mapping {:x :x :y :y} :charts [(chart/scatter)]} size))]
      (is (= :mapping (:key data)))
      (is (= [:mapping :x] (:in data)))))
  (testing "a sliced facet scale over levels is one error at the facet scale"
    (let [data (error #(plot/build {:data [{:x "a" :y 1 :f 1} {:x "b" :y 2 :f 2}] :mapping {:x :x :y :y :facet :f}
                                    :facets {:scales {:x :sliced}} :charts [(chart/scatter)]}
                                   size))]
      (is (= :facets (:key data))))))
