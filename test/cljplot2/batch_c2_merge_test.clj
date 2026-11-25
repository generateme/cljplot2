(ns cljplot2.batch-c2-merge-test
  "Batch C2 merge fixes: `:equal-panels` corrects tracks towards their mean panel (the arrange keeps its size) and a
  round that doesn't fit ends the rounds; `pairs` keeps integer weights up to 9 variables."
  (:require [clojure.test :refer [deftest testing is]]
            [fastmath.random :as r]
            [cljplot2.core :as plot]
            [cljplot2.multiplot :as multiplot]))

(defn- rows
  "Seeded rows of `n` numeric variables `:v0` ... with different magnitudes (tick labels of different widths)."
  [^long n]
  (let [g (r/rng :mersenne 3)]
    (vec (for [_ (range 60)]
           (into {} (for [i (range n)] [(keyword (str "v" i)) (* (Math/pow 10.0 (mod i 4)) (r/drandom g))]))))))

(deftest equal-panels-keep-size-test
  (testing "many variables of different magnitudes build at 1000x800"
    (doseq [n [12 16 18]]
      (is (= 1000 (.getWidth ^java.awt.image.BufferedImage
                   (plot/render-image (plot/pairs (rows n) {:variables (vec (for [i (range n)] (keyword (str "v" i))))})
                                      {:width 1000 :height 800})))
          (str n " variables"))))
  (testing "boundary: two and three variables build; one is an error of :variables"
    (doseq [n [2 3]]
      (is (plot/build (plot/pairs (rows n) {:variables (vec (for [i (range n)] (keyword (str "v" i))))}) {:width 400 :height 400})))
    (is (= :variables (:key (ex-data (try (plot/pairs (rows 1) {:variables [:v0]}) (catch clojure.lang.ExceptionInfo e e))))))))

(deftest pairs-weights-test
  (testing "equal integer weights up to 9 tracks, a larger double outer weight from 10"
    (let [weights #'multiplot/outer-weights]
      (is (= [1 1 1] (weights 3 false)))
      (is (= [1 1 1 1 1 1 1 1 1] (weights 9 true)))
      (is (= [1.125 1 1 1 1 1 1 1 1 1] (weights 10 false)))
      (is (= [1 1 1 1 1 1 1 1 1 1.125] (weights 10 true)))
      (is (= [1] (weights 1 false))))))
