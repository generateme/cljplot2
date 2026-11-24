(ns cljplot2.code-review-r2b3-data-test
  "Code review round 2 B3 - data/ and scale/ paths made lazy (lazy-collection retro): results unchanged, boundaries
  (empty, one, reversed) included; schema predicates."
  (:require [clojure.test :refer [deftest testing is]]
            [fastmath.core :as m]
            [cljplot2.data.field :as field]
            [cljplot2.data.levels :as levels]
            [cljplot2.data.series :as series]
            [cljplot2.data.shingle :as shingle]
            [cljplot2.data.smooth :as smooth]
            [cljplot2.schema :as schema]
            [cljplot2.test-util :refer [error-data]]
            [malli.core]))

(deftest stacked-test
  (let [stacked #'cljplot2.data.position/stacked]
    (testing "positive up, negative down; reversed negatives - the last group nearest 0"
      (is (= [[[0.0 1.0] [-2.0 0.0]] [[1.0 3.0] [-3.0 -2.0]]] (stacked [[1.0 -2.0] [2.0 -1.0]] 2)))
      (is (= [[[0.0 1.0] [-3.0 -1.0]] [[1.0 3.0] [-1.0 0.0]]] (stacked [[1.0 -2.0] [2.0 -1.0]] 2 true))))
    (testing "boundary: one group, empty groups, lazy input"
      (is (= [[[0.0 1.0] [-1.0 0.0]]] (stacked [[1.0 -1.0]] 2 true)))
      (is (= [] (stacked [] 0)))
      (is (= [[]] (stacked [[]] 0 true)))
      (is (= (stacked [[1.0 2.0] [3.0 -4.0]] 2 true) (stacked (map seq [[1.0 2.0] [3.0 -4.0]]) 2 true))))))

(deftest quantile-breaks-test
  (testing "a count and inner probabilities give the same breaks; ties merged"
    (let [vs (map double (range 11))]
      (is (= [0.0 5.0 10.0] (levels/breaks vs {:quantiles 2})))
      (is (= [0.0 5.0 10.0] (levels/breaks vs {:quantiles [0.5]})))
      (is (= [0.5 1.5] (levels/breaks [1.0 1.0 1.0] {:quantiles 4})) "boundary: one distinct value - one bin of width 1")
      (is (vector? (levels/breaks vs {:quantiles 3}))))))

(deftest rank-rows-test
  (let [rows [{:x 1 :v 3.0} {:x 1 :v 5.0} {:x 1 :v 3.0} {:x 2 :v 1.0} {:x 2 :v nil}]]
    (testing "per x, descending by default, ties :min"
      (is (= [2 1 2 1 nil] (map :rank (levels/rank-rows rows :x :v))))
      (is (= [1 3 1 1 nil] (map :rank (levels/rank-rows rows :x :v {:descending false}))))
      (is (= [2.5 1.0 2.5] (take 3 (map :rank (levels/rank-rows rows :x :v {:ties :average}))))))
    (testing "boundary: no rows, no rankable row"
      (is (= [] (levels/rank-rows [] :x :v)))
      (is (= [nil] (map :rank (levels/rank-rows [{:x 1 :v nil}] :x :v)))))))

(deftest tree-route-test
  (let [route #'cljplot2.data.tree/tree-route
        ;; 0 root; 1, 2 children of 0; 3 child of 1; 4 child of 2
        nodes [{:parent nil} {:parent 0} {:parent 0} {:parent 1} {:parent 2}]]
    (testing "up to the lowest common ancestor, then down"
      (is (= [3 1 0 2 4] (route nodes 3 4)))
      (is (vector? (route nodes 3 4))))
    (testing "boundary: an ancestor and itself"
      (is (= [3 1] (route nodes 3 1)))
      (is (= [1 3] (route nodes 1 3)))
      (is (= [3] (route nodes 3 3))))))

(deftest fit-curve-test
  (let [xs [1.0 2.0 3.0 4.0 5.0 6.0] ys [1.1 1.9 3.2 3.9 5.1 6.0] grid [1.0 3.5 6.0]]
    (testing "lm and glm on seqs; bands widest first, distinct levels"
      (let [{:keys [fit bands]} (smooth/fit-curve {:method :lm :band :confidence :levels [0.5 0.95 0.5]} xs ys grid)]
        (is (= 3 (count fit)))
        (is (= [0.95 0.5] (map :level bands)))
        (is (vector? bands)))
      (is (= 3 (count (:fit (smooth/fit-curve {:method :lm :params {:degree 2} :band :none :levels []} xs ys grid)))))
      (is (= 3 (count (:fit (smooth/fit-curve {:method :glm :band :none :levels []} xs ys grid))))))
    (testing "boundary: no levels - no bands"
      (is (= [] (:bands (smooth/fit-curve {:method :lm :band :confidence :levels []} xs ys grid)))))))

(deftest horizon-series-test
  (testing "series in first-appearance order"
    (let [rows [{:t 1 :y 1.0 :s "b"} {:t 2 :y 2.0 :s "b"} {:t 1 :y -1.0 :s "a"} {:t 2 :y 0.5 :s "a"}]]
      (is (= ["b" "a"] (distinct (map :series (:rows (series/horizon-rows rows {:x :t :y :y :series :s :bands 2})))))))))

(deftest function-domain-test
  (testing "explicit scale domains fill missing axes, as doubles"
    (is (= [[0.0 1.0] [2.0 3.0]] (field/function-domain {:x [0 1]} {:y {:domain [2 3]}})))
    (is (= [[0.0 1.0] [2.0 3.0]] (field/function-domain nil {:x {:domain [0 1]} :y {:domain [2 3]}}))))
  (testing "boundary: a non-numeric or short scale domain doesn't count"
    (is (= :domain (:key (error-data #(field/function-domain {:x [0 1]} {:y {:domain [2]}})))))
    (is (= :domain (:key (error-data #(field/function-domain {:x [0 1]} {:y {:domain [:a :b]}})))))))

(deftest shingle-intervals-test
  (testing "explicit intervals, every row in each interval holding it"
    (let [rows [{:v 1.0} {:v 2.5} {:v 4.0}]
          out (shingle/shingle rows :v {:as :iv :intervals [[0 2] [2 5]]})]
      (is (= 3 (count out)))))
  (testing "boundary: no intervals given - equal count"
    (is (seq (shingle/shingle (map (fn [i] {:v (double i)}) (range 10)) :v {:as :iv :n 2})))))

(deftest half-pi-test
  (is (== (* 0.5 Math/PI) m/HALF_PI) "the polar quarter turn"))

(deftest schema-predicates-test
  (testing "statements, truthy / falsey"
    (is (schema/finite? 1))
    (is (not (schema/finite? ##NaN)))
    (is (not (schema/finite? "1")))
    (is (schema/dash? [2 1]))
    (is (not (schema/dash? [])))
    (is (schema/pattern? :hatch))
    (is (not (schema/pattern? {:kind :dots :spacing 2.0})))
    (is (schema/forbidden? (schema/forbidden "no")))
    (is (not (schema/forbidden? int?))))
  (testing "named-type: the type name in the form"
    (is (= ::t (malli.core/type (schema/named-type ::t int? "should be an int"))))))
