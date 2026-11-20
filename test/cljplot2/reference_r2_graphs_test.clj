(ns cljplot2.reference-r2-graphs-test
  "Reference part R2 for the graph and tree builders ([[cljplot2.graphs]]) and their data functions
  ([[cljplot2.data.graph]], [[cljplot2.data.tree]], [[cljplot2.data.hierarchy/sequence-levels]]): every schema
  `:default` is the value used when the option is absent, options are checked first (bad options fail with any data),
  data errors fail as before."
  (:require [clojure.test :refer [deftest testing is are]]
            [fastmath.ml.clustering :as cl]
            [fastmath.random :as r]
            [cljplot2.describe :as describe]
            [cljplot2.graphs :as graphs]
            [cljplot2.data.graph :as graph]
            [cljplot2.data.hierarchy :as hierarchy]
            [cljplot2.data.tree :as tree]
            [cljplot2.test-util :as tu :refer [error]]))

(def ^:private size {:width 400 :height 400})

(def ^:private flows
  "A small acyclic weighted graph."
  [{:from "a" :to "h" :value 3} {:from "b" :to "h" :value 1} {:from "h" :to "x" :value 2} {:from "h" :to "y" :value 2}
   {:from "b" :to "y" :value 2}])

(def ^:private hc (cl/hclust [[1.0 1.0] [1.2 1.1] [5.0 5.0] [5.1 5.3] [9.0 0.0]] {:method :complete}))
(def ^:private names ["a" "b" "c" "d" "e"])

(def ^:private parent-rows
  [{:id :root} {:id :p :parent :root} {:id :q :parent :root}
   {:id :a :parent :p} {:id :b :parent :p} {:id :c :parent :q} {:id :d :parent :q}])

(def ^:private package-tree (tree/from-parents parent-rows {}))

(def ^:private cells
  (vec (for [[r row] (map vector ["r1" "r2" "r3" "r4"] [[1 2 3] [1.1 2.1 2.9] [9 8 7] [9.2 8.1 7.3]])
             [c v] (map vector ["c1" "c2" "c3"] row)]
         {:r r :c c :v v})))

(def ^:private people
  [{:class "1st" :sex "F" :age "adult" :n 4} {:class "1st" :sex "M" :age "adult" :n 6}
   {:class "2nd" :sex "F" :age "child" :n 1} {:class "2nd" :sex "M" :age "adult" :n 9}])

(def ^:private visits [{:sequence "a-b-end"} {:sequence "a-b-end"} {:sequence "a-end"} {:sequence "b-a-c"}])

(defn- rng [] (r/rng :mersenne 1))

;; builders: defaults hold

(deftest builder-defaults-test
  (are [v args options] (tu/builder-defaults-hold? v args options size)
    #'graphs/network [flows] {:rng (rng)}
    #'graphs/network [flows] {:rng (rng) :directed? true :labels true :edge-style {:width 2}}
    #'graphs/dendrogram [hc] {:labels names}
    #'graphs/dendrogram [hc] {:labels names :k 2 :circular true}
    #'graphs/tree [package-tree] {}
    #'graphs/tree [package-tree] {:circular true :node-style {:size 5}}
    #'graphs/clustered-heatmap [cells] {:row :r :col :c :value :v}
    #'graphs/bundle [[{:from :a :to :c} {:from :b :to :d}] package-tree] {}
    #'graphs/chord [flows] {}
    #'graphs/arc [flows] {}
    #'graphs/sankey [flows] {}
    #'graphs/sankey [flows] {:mapping {:color :id}}
    #'graphs/alluvial [people] {:axes [:class :sex :age] :value :n}
    #'graphs/sequences [visits] {}))

;; API functions: defaults hold

(defn- explicit
  "`options` with every constant default of `v`'s options set."
  [v options]
  (tu/explicit-defaults (describe/options v) options))

(deftest api-defaults-test
  (let [g (graph/graph flows)
        laid (tree/layout package-tree {:circular true})
        bundle-edges [{:from :a :to :c} {:from :b :to :d}]]
    (are [v args options] (= (apply @v (concat args [options])) (apply @v (concat args [(explicit v options)])))
      #'graph/graph [flows] {}
      #'graph/adjacency->edges [[[0 1] [2 0]] ["a" "b"]] {}
      #'graph/layout [g] {:rng (rng)}
      #'graph/chord-layout [g] {}
      #'graph/arc-layout [g] {}
      #'graph/sankey-layout [g] {}
      #'graph/alluvial-graph [people] {:axes [:class :sex]}
      #'tree/from-levels [people] {:hierarchy [:class :sex]}
      #'tree/from-parents [parent-rows] {}
      #'tree/layout [package-tree] {}
      #'tree/node-link-layout [package-tree] {}
      #'tree/bundle-paths [bundle-edges laid] {}
      #'hierarchy/sequence-levels [visits] {})))

;; boundaries

(def ^:private builders
  [#'graphs/network #'graphs/dendrogram #'graphs/tree #'graphs/clustered-heatmap #'graphs/bundle #'graphs/chord
   #'graphs/arc #'graphs/sankey #'graphs/alluvial #'graphs/sequences])

(def ^:private api-fns
  [#'graph/graph #'graph/adjacency->edges #'graph/layout #'graph/chord-layout #'graph/arc-layout #'graph/sankey-layout
   #'graph/alluvial-graph #'tree/from-levels #'tree/from-parents #'tree/layout #'tree/node-link-layout
   #'tree/bundle-paths #'hierarchy/sequence-levels])

(defn- call-nil-data
  "`v` called with `nil` positional arguments and `options`."
  [v options]
  (let [n (dec (count (last (sort-by count (:arglists (meta v))))))]
    (apply @v (concat (repeat n nil) [options]))))

(deftest options-first-test
  (testing "an unknown key fails first (nil data), keyed by it"
    (doseq [v (concat builders api-fns)]
      (is (= :bogus (:key (error #(call-nil-data v {:bogus 1})))) (str v))))
  (testing "a wrong type fails first, keyed by the option"
    (are [v options k] (= k (:key (error #(call-nil-data v options))))
      #'graphs/network {:layout :tree} :layout
      #'graphs/network {:labels 3} :labels
      #'graphs/dendrogram {:k 0} :k
      #'graphs/dendrogram {:labels? nil} :labels?
      #'graphs/tree {:layout :radial} :layout
      #'graphs/tree {:links :curve} :links
      #'graphs/clustered-heatmap {:tree-size [0 0.2]} :tree-size
      #'graphs/clustered-heatmap {:method :ward-x} :method
      #'graphs/bundle {:beta 2} :beta
      #'graphs/bundle {:alpha "x"} :alpha
      #'graphs/chord {:inner 1} :inner
      #'graphs/arc {:labels false} :labels
      #'graphs/sankey {:link-color :from} :link-color
      #'graphs/sankey {:directed? true} :directed?
      #'graphs/sankey {:mapping {:size :value}} :size
      #'graphs/alluvial {:node-width 1} :node-width
      #'graphs/sequences {:inner 1} :inner
      #'graph/graph {:directed? "yes"} :directed?
      #'graph/adjacency->edges {:directed? nil} :directed?
      #'graph/layout {:gravity -1} :gravity
      #'graph/chord-layout {:min-value ##NaN} :min-value
      #'graph/arc-layout {:order :degree} :order
      #'graph/sankey-layout {:iterations 1.5} :iterations
      #'graph/alluvial-graph {:axes [:a]} :axes
      #'tree/from-levels {:hierarchy :a} :hierarchy
      #'tree/from-parents {:id nil} :id
      #'tree/layout {:span 0} :span
      #'tree/node-link-layout {:links :curve} :links
      #'tree/bundle-paths {:beta -0.1} :beta
      #'hierarchy/sequence-levels {:separator 1} :separator))
  (testing "nil and {} options are accepted"
    (is (= (graph/graph flows nil) (graph/graph flows {})))
    (is (= (tree/node-link-layout package-tree nil) (tree/node-link-layout package-tree {})))
    (is (= (hierarchy/sequence-levels visits nil) (hierarchy/sequence-levels visits {})))
    (is (some? (graphs/sankey flows nil)))
    (is (= (graphs/chord flows nil) (graphs/chord flows {})))))

(deftest data-errors-test
  (testing "data errors still fail with their keys"
    (is (= :edges (:key (error #(graphs/sankey [{:from "a" :to "b"} {:from "b" :to "a"}] {})))))
    (is (= :edges (:key (error #(graph/graph [[1 2]] {})))))
    (is (= :parent (:key (error #(tree/from-parents [{:id 1} {:id 2}] {})))))
    (is (= :k (:key (error #(tree/layout package-tree {:k 9})))))
    (is (= :pad (:key (error #(graph/chord-layout (graph/graph flows) {:pad 120})))))
    (is (= :pad (:key (error #(graph/sankey-layout (graph/graph flows {:directed? true}) {:pad 1.0})))))
    (is (= :axes (:key (error #(graphs/alluvial people {})))))
    (is (= :row (:key (error #(graphs/clustered-heatmap cells {:col :c :value :v})))))
    (is (= :rows (:key (error #(graphs/clustered-heatmap (filterv (fn [c] (= "r1" (:r c))) cells) {:row :r :col :c :value :v})))))
    (is (= :edge-mapping (:key (error #(graphs/arc flows {:mapping {:size :degree} :edge-mapping {:size :value}})))))))

(deftest schemas-test
  (testing "every builder var carries its schema; no duplicate keys"
    (doseq [v builders]
      (let [rows (describe/options v)]
        (is (some? (:cljplot2/options (meta v))) (str v))
        (is (= (count rows) (count (distinct (map :path rows)))) (str v)))))
  (testing "sankey: own rows, then graph/sankey-layout's"
    (is (some #(= :graph/sankey-layout (:fragment %)) (describe/options #'graphs/sankey)))
    (is (not-any? #(= [:directed?] (:path %)) (describe/options #'graphs/sankey))))
  (testing "sequences: the builder's :keep-end? (default true) wins over sequence-levels' (default false)"
    (is (= [true] (keep #(when (= [:keep-end?] (:path %)) (:default %)) (describe/options #'graphs/sequences))))))
