(ns cljplot2.part-of-whole-notebook-test
  "Part of a whole notebook review: `:negative-order` - the negative side of a stack in reverse group order."
  (:require [clojure.test :refer [deftest testing is are]]
            [cljplot2.test-util :refer [error]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.position :as position]))

(deftest stack-values-negative-order-test
  (let [heights [[-1 2] [-2 3] [-3 1]]]
    (testing "forward (default): negative groups from 0 in group order"
      (is (= [[[-1.0 0.0] [0.0 2.0]] [[-3.0 -1.0] [2.0 5.0]] [[-6.0 -3.0] [5.0 6.0]]]
             (position/stack-values :stack heights)
             (position/stack-values :stack heights {:negative-order :forward}))))
    (testing ":reverse - the last group nearest 0 below; the positive side unchanged"
      (is (= [[[-6.0 -5.0] [0.0 2.0]] [[-5.0 -3.0] [2.0 5.0]] [[-3.0 0.0] [5.0 6.0]]]
             (position/stack-values :stack heights {:negative-order :reverse})))))
  (testing "edges: no groups, one group, zeros, all positive, other positions"
    (are [heights] (= (position/stack-values :stack heights) (position/stack-values :stack heights {:negative-order :reverse}))
      []
      [[-1 -2]]
      [[0 0] [0 0]]
      [[1 2] [3 4]])
    (is (= (position/stack-values :dodge [[-1] [-2]]) (position/stack-values :dodge [[-1] [-2]] {:negative-order :reverse})))))

(deftest bar-negative-order-test
  (let [rows [{:l "q" :a "SD" :v -3} {:l "q" :a "D" :v -2} {:l "q" :a "A" :v 4}]
        bars (fn [opts] (into {} (map (juxt (comp :color :aesthetics) (juxt :y0 :y1)))
                              (get-in (plot/build {:data rows :mapping {:x :v :y :l :color :a}
                                                   :scales {:color {:domain ["SD" "D" "A"]}}
                                                   :charts [(chart/bar (merge {:position :stack} opts))]}
                                                  {:width 300 :height 200})
                                      [:stats 0 :data :panel :bars])))]
    (testing "default: SD nearest 0"
      (is (= {"SD" [-3.0 0.0] "D" [-5.0 -3.0] "A" [0.0 4.0]} (bars {}))))
    (testing ":reverse - D (the last negative group) nearest 0: the scale diverges from the middle"
      (is (= {"SD" [-5.0 -2.0] "D" [-2.0 0.0] "A" [0.0 4.0]} (bars {:negative-order :reverse}))))
    (testing "schema"
      (is (= :negative-order (:key (error #(plot/build {:data rows :mapping {:x :v :y :l :color :a}
                                                         :charts [(chart/bar {:negative-order :sideways})]}
                                                        {:width 300 :height 200}))))))))
