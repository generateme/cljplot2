(ns cljplot2.data.position-test
  (:require [clojure.test :refer [deftest testing is are]]
            [cljplot2.test-util :refer [error]]
            [cljplot2.data.position :as position]))

(deftest stack-values-test
  (testing "no groups"
    (doseq [p position/positions] (is (= [] (position/stack-values p [])) (str p))))
  (testing "one group"
    (are [p expected] (= expected (position/stack-values p [[1 2 0]]))
      :overlay [[[0.0 1.0] [0.0 2.0] [0.0 0.0]]]
      :dodge [[[0.0 1.0] [0.0 2.0] [0.0 0.0]]]
      :stack [[[0.0 1.0] [0.0 2.0] [0.0 0.0]]]
      :fill [[[0.0 1.0] [0.0 1.0] [0.0 0.0]]]))
  (testing "three groups: stack is cumulative in group order, fill sums to 1, a zero total stays 0"
    (let [hs [[1 0 2] [2 0 2] [1 0 4]]]
      (is (= [[[0.0 1.0] [0.0 0.0] [0.0 2.0]]
              [[1.0 3.0] [0.0 0.0] [2.0 4.0]]
              [[3.0 4.0] [0.0 0.0] [4.0 8.0]]]
             (position/stack-values :stack hs)))
      (is (= [[[0.0 0.25] [0.0 0.0] [0.0 0.25]]
              [[0.25 0.75] [0.0 0.0] [0.25 0.5]]
              [[0.75 1.0] [0.0 0.0] [0.5 1.0]]]
             (position/stack-values :fill hs)))))
  (testing "mirror: exactly two groups, the second downward"
    (is (= [[[0.0 1.0] [0.0 3.0]] [[-2.0 0.0] [-0.0 0.0]]] (position/stack-values :mirror [[1 3] [2 0]])))
    (is (= :position (:key (error #(position/stack-values :mirror [[1]])))))
    (is (= :position (:key (error #(position/stack-values :mirror [[1] [1] [1]]))))))
  (testing "unequal lengths and unknown positions throw"
    (is (= :heights (:key (error #(position/stack-values :stack [[1 2] [1]])))))
    (is (= :position (:key (error #(position/stack-values :spiral [[1]])))))))

(deftest dodge-interval-test
  (is (= [0.0 10.0] (position/dodge-interval [0 10] 0 1)))
  (is (= [[0.0 2.5] [2.5 5.0] [5.0 7.5] [7.5 10.0]] (mapv #(position/dodge-interval [0 10] % 4) (range 4))))
  (is (= [-1.0 0.0] (position/dodge-interval [-2 0] 1 2))))

(deftest symmetric-extent-test
  (is (= [0.0 0.0] (position/symmetric-extent [])))
  (is (= [-3.0 3.0] (position/symmetric-extent [1 -3 2])))
  (is (= [-5.0 5.0] (position/symmetric-extent [5]))))
