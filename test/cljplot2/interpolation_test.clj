(ns cljplot2.interpolation-test
  "Fitting functions of interpolators: random fits (kriging, microsphere projection) are deterministic unless given their own generator."
  (:require [clojure.test :refer [deftest testing is]]
            [fastmath.kernel.variogram :as vgram]
            [fastmath.random :as r]
            [cljplot2.data.interpolation :as fit]))

(def ^:private xs [0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0])
(def ^:private ys [1.0 3.0 2.0 5.0 4.0 4.5 2.0 3.0])
(def ^:private probes [0.5 2.5 3.7 6.2])

(defn- values
  "Values of a fit of `xs` / `ys` at the probe points."
  [interpolator params]
  (mapv (fit/fitted (fit/fit-fn interpolator params) xs ys) probes))

(deftest kriging-deterministic-test
  (testing "no params, nil options, options without :rng: every fit gives the same function"
    (is (= (values :kriging nil) (values :kriging nil)))
    (is (= (values :kriging [nil nil]) (values :kriging nil)))
    (is (= (values :kriging [nil {:N 50}]) (values :kriging [nil {:N 50}]))))
  (testing "a given generator is used: two generators of one seed give equal fits"
    (is (= (values :kriging [nil {:rng (r/rng :mersenne 5)}]) (values :kriging [nil {:rng (r/rng :mersenne 5)}]))))
  (testing "boundary: an explicit variogram needs no generator; it fits without one"
    (let [v (vgram/gaussian {:nugget 0.0 :psill 2.0 :range 3.0})]
      (is (= (values :kriging [v]) (values :kriging [v])))
      (is (every? #(Double/isFinite %) (values :kriging [v]))))))

(deftest microsphere-deterministic-test
  (testing "no params, nil options, options without :rng: every fit gives the same function"
    (is (= (values :microsphere-projection nil) (values :microsphere-projection nil)))
    (is (= (values :microsphere-projection [nil]) (values :microsphere-projection nil)))
    (is (= (values :microsphere-projection [{:elements 50}]) (values :microsphere-projection [{:elements 50}]))))
  (testing "a given generator is used: equal seeds equal fits"
    (is (= (values :microsphere-projection [{:rng (r/rng :mersenne 5)}])
           (values :microsphere-projection [{:rng (r/rng :mersenne 5)}])))))

(deftest other-interpolators-unchanged-test
  (testing "params pass through untouched for other methods and for functions"
    (is (= (values :linear nil) (mapv (fn [x] (let [i (long x) t (- x i)] (+ (* (- 1.0 t) (ys i)) (* t (ys (inc i)))))) probes)))
    (is (= [7.0 7.0 7.0 7.0] (values (fn [_ _ c] (constantly c)) [7.0])))))
