(ns cljplot2.reference-r2-multiplot-test
  "Reference part R2, multiplot builders: [[cljplot2.multiplot/arrange]] and [[cljplot2.multiplot/pairs]] validate
  their documented options first, and their constant defaults are the real ones."
  (:require [clojure.test :refer [deftest testing is are]]
            [fastmath.random :as r]
            [cljplot2.chart :as chart]
            [cljplot2.describe :as describe]
            [cljplot2.multiplot :as multiplot]
            [cljplot2.test-util :refer [builder-defaults-hold? error]]))

(def ^:private rows
  (let [rng (r/rng :mersenne 7)]
    (vec (for [i (range 40)]
           (let [a (r/drandom rng) b (+ a (r/drandom rng))]
             {:a a :b b :c (- (r/drandom rng) a) :g (if (even? i) "u" "v") :s "text"})))))

(def ^:private p1 {:data rows :mapping {:x :a :y :b} :charts [(chart/scatter)]})
(def ^:private p2 {:data rows :mapping {:x :a :y :c :color :g} :charts [(chart/line)]})

(deftest documented-test
  (testing "the schemas are attached to the vars"
    (is (= multiplot/arrange-options (:cljplot2/options (meta #'multiplot/arrange))))
    (is (= multiplot/pairs-options (:cljplot2/options (meta #'multiplot/pairs)))))
  (testing "arrange: the :arrange layout group but :plots, the builder's :title and :theme (no :scales)"
    (let [ks (map :key (describe/options #'multiplot/arrange))]
      (is (= [:columns :rows :widths :heights :align :equal-panels :title :theme] ks))
      (is (every? #(= :arrange (:fragment %)) (take 6 (describe/options #'multiplot/arrange))))))
  (testing "pairs"
    (is (= [:variables :mapping :lower :upper :diagonal :variable-names :scales :color :theme :title]
           (map :key (describe/options #'multiplot/pairs))))))

(deftest defaults-test
  (testing "every constant :default is the real one"
    (is (builder-defaults-hold? #'multiplot/arrange [[p1 p2]] {:columns 2} {:width 500 :height 300}))
    (is (builder-defaults-hold? #'multiplot/pairs [rows] {:variables [:a :b :c]} {:width 450 :height 400}))))

(deftest arrange-boundaries-test
  (testing "no options, nil and {} accepted"
    (is (= {:arrange {:plots [p1]}} (multiplot/arrange [p1])))
    (is (= {:arrange {:plots [p1]}} (multiplot/arrange [p1] nil)))
    (is (= {:arrange {:plots [p1]}} (multiplot/arrange [p1] {}))))
  (testing "every option accepted"
    (is (map? (multiplot/arrange [p1 p2] {:columns 2 :rows 1 :widths [1 [:fixed 100]] :heights [1] :align false
                                          :equal-panels true :title "t" :theme {:multiplot {:gap 4}}}))))
  (testing "nil-able layout options"
    (is (map? (multiplot/arrange [p1] {:columns nil :rows nil :widths nil :heights nil :align nil :equal-panels nil}))))
  (testing "wrong types fail keyed by the option, before the plots are read"
    (are [opts k] (= k (:key (error #(multiplot/arrange :not-plots opts))))
      {:columns 0} :columns
      {:columns 1.5} :columns
      {:rows -1} :rows
      {:widths [1 :wide]} :widths
      {:widths 2} :widths
      {:heights [1 :measured]} :heights
      {:align 1} :align
      {:equal-panels "yes"} :equal-panels))
  (testing "unknown keys - :plots and :scales too - fail keyed by the key"
    (are [opts k] (= k (:key (error #(multiplot/arrange [p1] opts))))
      {:plots [p1]} :plots
      {:scales {:x {:domain [0 1]}}} :scales
      {:column 2} :column))
  (testing "cross-key layout errors still fail at build"
    (is (map? (multiplot/arrange [p1 p2] {:columns 2 :widths [1]})))))

(deftest pairs-boundaries-test
  (testing "options accepted"
    (is (map? (multiplot/pairs rows {:variables [:a :b] :mapping {:color :g} :lower [(chart/scatter) (chart/smooth)]
                                     :upper :none :diagonal :names :variable-names {:a "A"} :theme {:facet {:gap 2}} :title "t"})))
    (is (map? (multiplot/pairs rows {:variables [:a :b] :lower (chart/scatter) :upper (list (chart/scatter))
                                     :diagonal nil :mapping nil :variable-names nil}))))
  (testing "wrong types and unknown keys fail at the option, before the data is read (`:key` - the innermost key)"
    (are [opts k] (= k (first (:in (error #(multiplot/pairs nil opts)))))
      {:variables :a} :variables
      {:variables ["a" "b"]} :variables
      {:variables [:a :b] :lower 1} :lower
      {:variables [:a :b] :upper [{:kind :scatter}]} :upper
      {:variables [:a :b] :diagonal :name} :diagonal
      {:variables [:a :b] :variable-names {:a 1}} :variable-names
      {:variables [:a :b] :mapping {:x 1}} :mapping
      {:variables [:a :b] :columns 2} :columns
      {:variables [:a :b] :scales {:size {}}} :scales)
    (is (= :compute (:key (error #(multiplot/pairs nil {:variables [:a :b] :upper [{:kind :scatter}]})))) "a chart without :compute")
    (is (= :a (:key (error #(multiplot/pairs nil {:variables [:a :b] :variable-names {:a 1}}))))))
  (testing "nil and {} options pass validation, then fail on the missing variables"
    (is (= :variables (:key (error #(multiplot/pairs rows nil)))))
    (is (= :variables (:key (error #(multiplot/pairs rows {}))))))
  (testing "data errors as before"
    (is (= :variables (:key (error #(multiplot/pairs rows {:variables [:a]})))))
    (is (= :variables (:key (error #(multiplot/pairs rows {:variables (vec (repeat 21 :a))})))))
    (is (= :variables (:key (error #(multiplot/pairs rows {:variables [:a :s]})))))
    (is (= :variables (:key (error #(multiplot/pairs rows {:variables [:a :missing]})))))))
