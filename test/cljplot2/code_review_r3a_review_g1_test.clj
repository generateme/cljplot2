(ns cljplot2.code-review-r3a-review-g1-test
  "Code review round 3, batch A review fixes, group G1 (builders and API data functions): `:leaf-names` errors,
  alluvial `:variables` / `:strand-color` / `:variable-names`, bundle `:color-by`, `pairs` slots (`:none` empty, `nil`
  default), sankey `:link-color :theme`, chord / sequences `:inner-radius`, optional options arities, the sankey layout's
  plain `:dropped` count and the renamed `rows` parameters."
  (:require [clojure.test :refer [deftest testing is]]
            [fastmath.ml.clustering :as cl]
            [cljplot2.test-util :refer [error]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.graph :as graph]
            [cljplot2.data.levels :as levels]
            [cljplot2.data.series :as series]
            [cljplot2.data.shingle :as shingle]
            [cljplot2.data.tree :as tree]
            [cljplot2.data.vector-field :as vector-field]
            [cljplot2.graphs :as graphs]))

(def ^:private clustering
  "A clustering of four observations."
  (cl/hclust [[0.0 0.0] [0.0 1.0] [5.0 5.0] [5.0 6.0]]))

(deftest leaf-names-test
  (testing "one name per observation: the leaves take them"
    (is (= #{"a" "b" "c" "d"} (set (map :name (tree/leaves (tree/from-hclust clustering ["a" "b" "c" "d"])))))))
  (testing "nil: indices as text"
    (is (= #{"0" "1" "2" "3"} (set (map :name (tree/leaves (tree/from-hclust clustering nil)))))))
  (testing "a wrong count names the option :leaf-names (not the boolean :labels)"
    (is (= :leaf-names (:key (error #(tree/from-hclust clustering ["a" "b"])))))
    (is (= :leaf-names (:key (error #(tree/from-hclust clustering [])))))
    (is (= :leaf-names (:key (error #(plot/tree clustering {:leaf-names ["a" "b"]})))))
    (is (= :leaf-names (:key (error #(plot/dendrogram clustering {:leaf-names ["a"]})))))))

(def ^:private people
  [{:class "1st" :sex "F" :n 2} {:class "1st" :sex "M" :n 1} {:class "2nd" :sex "F" :n 3}])

(deftest alluvial-columns-test
  (testing ":strand-color defaults to the first column and is the edges' key"
    (is (= #{"1st" "2nd"} (set (map :strand-color (:edges (graph/alluvial-graph people {:variables [:class :sex]})))))))
  (testing ":strand-color picks the colouring level"
    (is (= #{"F" "M"} (set (map :strand-color (:edges (graph/alluvial-graph people {:variables [:class :sex] :strand-color :sex})))))))
  (testing "column errors: fewer than 2, none, a row without a value"
    (is (= :variables (:key (error #(graph/alluvial-graph people {:variables [:class]})))))
    (is (= :variables (:key (error #(graph/alluvial-graph people {:variables nil})))))
    (is (= :variables (:key (error #(graph/alluvial-graph (conj people {:class "3rd"}) {:variables [:class :sex]})))))
    (is (= :variables (:key (error #(plot/alluvial people {}))))))
  (testing "the old keys are rejected"
    (is (some? (error #(plot/alluvial people {:axes [:class :sex]}))))
    (is (some? (error #(plot/alluvial people {:variables [:class :sex] :color-by :sex}))))
    (is (some? (error #(plot/alluvial people {:variables [:class :sex] :axis-names ["a" "b"]})))))
  (testing ":variable-names label the columns"
    (let [p (plot/alluvial people {:variables [:class :sex] :variable-names ["Class" "Sex"]})
          fmt (get-in p [:scales :x :formatter])]
      (is (= ["Class" "Sex"] (mapv fmt (get-in p [:scales :x :ticks])))))))

(def ^:private package-tree
  (tree/from-parents [{:id "root"} {:id "g1" :parent "root"} {:id "g2" :parent "root"}
                      {:id :a :parent "g1"} {:id :b :parent "g1"} {:id :c :parent "g2"}]))

(deftest bundle-color-by-test
  (let [edges [{:from :a :to :c} {:from :b :to :c}]
        edge-color (fn [p] (get-in p [:charts 0 :options :color]))
        group-mapped? (fn [p] (some? (get-in p [:charts 0 :options :mapping :color])))]
    (testing "default and nil: coloured by group"
      (is (group-mapped? (plot/bundle edges package-tree)))
      (is (group-mapped? (plot/bundle edges package-tree {:color-by nil})))
      (is (group-mapped? (plot/bundle edges package-tree {:color-by :group}))))
    (testing ":single - one edge colour, no group mapping"
      (let [p (plot/bundle edges package-tree {:color-by :single})]
        (is (not (group-mapped? p)))
        (is (some? (edge-color p)))))
    (testing "other values, the old :none (which would draw nothing by the glossary) and the old key are rejected"
      (is (= :color-by (:key (error #(plot/bundle edges package-tree {:color-by :top})))))
      (is (= :color-by (:key (error #(plot/bundle edges package-tree {:color-by :none})))))
      (is (some? (error #(plot/bundle edges package-tree {:color-by-group false})))))))

(def ^:private cloud
  (for [i (range 12)] {:a (double i) :b (double (* i i)) :c (double (mod i 5))}))

(deftest pairs-slots-test
  (let [cells (fn [p] (remove :legend-only (get-in p [:arrange :plots])))
        chart-kinds (fn [p] (mapv #(mapv :kind (:charts %)) (cells p)))]
    (testing "nil and absent slots give the defaults"
      (is (= (chart-kinds (plot/pairs cloud {:variables [:a :b]}))
             (chart-kinds (plot/pairs cloud {:variables [:a :b] :lower nil :upper nil :diagonal nil})))))
    (testing ":none empties a slot"
      (is (= [[] [:correlation] [:scatter] []]
             (chart-kinds (plot/pairs cloud {:variables [:a :b] :diagonal :none})))
          "diagonal empty")
      (is (= [[:density] [] [:scatter] [:density]] (chart-kinds (plot/pairs cloud {:variables [:a :b] :upper :none}))))
      (is (= [[:density] [:correlation] [] [:density]] (chart-kinds (plot/pairs cloud {:variables [:a :b] :lower :none})))))
    (testing "an empty matrix of :none slots still builds"
      (is (some? (plot/render-image (plot/pairs cloud {:variables [:a :b] :lower :none :upper :none :diagonal :none})
                                    {:width 300 :height 300}))))
    (testing "other keywords are rejected"
      (is (some? (error #(plot/pairs cloud {:variables [:a :b] :lower :empty}))))
      (is (some? (error #(plot/pairs cloud {:variables [:a :b] :diagonal :blank})))))))

(def ^:private flows
  [{:from "a" :to "h" :value 3} {:from "b" :to "h" :value 1} {:from "h" :to "x" :value 2}])

(deftest sankey-link-color-test
  (let [mapped (fn [options] (get-in (plot/sankey flows options) [:charts 0 :options :mapping :color]))]
    (testing "default and nil: the source node's colour"
      (is (some? (mapped {})))
      (is (= (mapped {}) (mapped {:link-color nil})) "nil is the default (:source)")
      (is (= (map ::graphs/color (get-in (plot/sankey flows {}) [:charts 0 :options :data]))
             (map ::graphs/color (get-in (plot/sankey flows {:link-color :source}) [:charts 0 :options :data])))))
    (testing ":theme - the theme style, no colour mapping"
      (is (nil? (mapped {:link-color :theme}))))
    (testing "other values are rejected"
      (is (= :link-color (:key (error #(plot/sankey flows {:link-color :none}))))))))

(deftest inner-radius-test
  (testing "chord and sequences take :inner-radius in [0, 1)"
    (is (some? (graph/chord-layout (graph/graph flows {:directed true}) {:inner-radius 0.5})))
    (is (= :inner-radius (:key (error #(plot/chord flows {:inner-radius 1})))))
    (is (= {:kind :polar :inner 0.0} (:coord (plot/sequences [{:sequence "a-b"}] {:inner-radius 0.0}))))
    (is (= :inner-radius (:key (error #(plot/sequences [{:sequence "a-b"}] {:inner-radius 1}))))))
  (testing "the old key is rejected"
    (is (some? (error #(plot/chord flows {:inner 0.8}))))
    (is (some? (error #(plot/sequences [{:sequence "a-b"}] {:inner 0.2}))))))

(deftest optional-options-test
  (testing "API functions whose options have no required key take one argument less"
    (is (= (tree/from-levels [{:a "x"}] {}) (tree/from-levels [{:a "x"}])))
    (is (= (tree/from-parents [{:id 1} {:id 2 :parent 1}] {}) (tree/from-parents [{:id 1} {:id 2 :parent 1}])))
    (is (= (tree/layout package-tree {}) (tree/layout package-tree)))
    (is (= (tree/node-link-layout package-tree {}) (tree/node-link-layout package-tree)))
    (let [laid (tree/layout package-tree)]
      (is (= (tree/bundle-paths [{:from :a :to :c}] laid {}) (tree/bundle-paths [{:from :a :to :c}] laid))))
    (is (= (series/acf-rows [1 2 3 2 1 2 3] {}) (series/acf-rows [1 2 3 2 1 2 3])))
    (is (= (series/horizon-rows [{:x 0 :y 1} {:x 1 :y -1}] {}) (series/horizon-rows [{:x 0 :y 1} {:x 1 :y -1}])))
    (is (= (vector-field/traces (fn [[x y]] [y (- x)]) [[1.0 0.0]] {})
           (vector-field/traces (fn [[x y]] [y (- x)]) [[1.0 0.0]])))
    (is (= (mapv (juxt :kind (comp :data :options)) (:charts (graphs/tree package-tree {})))
           (mapv (juxt :kind (comp :data :options)) (:charts (graphs/tree package-tree))))))
  (testing "empty input with the default options"
    (is (= {:name nil :height 0.0 :children []} (tree/from-levels [])))
    (is (= [] (:rows (series/horizon-rows []))))))

(deftest sankey-layout-dropped-test
  (testing "a plain count of the dropped edge rows"
    (is (= 0 (:dropped (graph/sankey-layout (graph/graph flows {:directed true})))))
    (is (= 1 (:dropped (graph/sankey-layout (graph/graph (conj flows {:from "a" :to nil :value 1}) {:directed true})))))
    (is (= 1 (:dropped (graph/sankey-layout (graph/graph (conj flows {:from "a" :to "x" :value 0}) {:directed true})))))
    (is (= 0 (:dropped (graph/sankey-layout (graph/graph [] {:directed true})))) "no edges")))

(deftest rows-parameters-test
  (testing "level and shingle functions read rows or columns"
    (is (= 2 (count (levels/bin [{:v 1} {:v 2}] :v {:as :bin}))))
    (is (= 2 (count (levels/bin {:v [1 2]} :v {:as :bin}))))
    (is (= 2 (count (shingle/shingle [{:v 1} {:v 2}] :v {:as :s :n 1}))))
    (is (= [] (levels/bin [] :v {:as :bin})) "no rows")))
