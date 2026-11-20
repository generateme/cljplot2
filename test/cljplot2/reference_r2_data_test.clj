(ns cljplot2.reference-r2-data-test
  "Reference part R2, data functions: the options schemas of the user-facing data functions (levels, shingle, field,
  vector field) - every constant `:default` is the real one; options are validated first, keyed by the option; data
  checks fail as before."
  (:require [clojure.test :refer [deftest testing is]]
            [fastmath.complex :as cx]
            [cljplot2.data.field :as field]
            [cljplot2.data.levels :as levels]
            [cljplot2.data.shingle :as shingle]
            [cljplot2.data.vector-field :as vf]
            [cljplot2.describe :as describe]
            [cljplot2.test-util :refer [error explicit-defaults]]))

(def ^:private rows
  [{:g "a" :x 1 :v 1.0} {:g "b" :x 1 :v 3.0} {:g "a" :x 2 :v 2.5} {:g "b" :x 2 :v 2.5} {:g "c" :x 2 :v 7.5}
   {:g "c" :x 3 :v 4.0} {:g "a" :x 3 :v 0.5}])

(def ^:private rotation (fn [[x y]] [(- (double y)) x]))

(def ^:private domain {:x [-1 1] :y [-2 2]})

(defn- explicit
  "`options` with every constant default of `v`'s options set."
  [v options]
  (explicit-defaults (describe/options v) options))

(deftest defaults-hold-test
  (testing "bin"
    (is (= (levels/bin rows :v {:as :b}) (levels/bin rows :v (explicit #'levels/bin {:as :b}))))
    (is (= {:as :b :bins :sturges :digits 3} (explicit #'levels/bin {:as :b}))))
  (testing "order-levels"
    (is (= (levels/order-levels rows :g :v count {}) (levels/order-levels rows :g :v count (explicit #'levels/order-levels {}))))
    (is (= (levels/order-levels rows :g :v count) (levels/order-levels rows :g :v count (explicit #'levels/order-levels {})))))
  (testing "rank-rows"
    (is (= (levels/rank-rows rows :x :v {}) (levels/rank-rows rows :x :v (explicit #'levels/rank-rows {}))))
    (is (= {:key :rank :descending true :ties :min} (explicit #'levels/rank-rows {}))))
  (testing "shingle (equal-count intervals)"
    (is (= (shingle/shingle rows :v {:as :s}) (shingle/shingle rows :v (explicit #'shingle/shingle {:as :s}))))
    (is (= {:as :s :count 6 :overlap 0.5} (explicit #'shingle/shingle {:as :s}))))
  (testing "domain-color (compared on sample points)"
    (let [f (field/domain-color cx/log {})
          g (field/domain-color cx/log (explicit #'field/domain-color {}))]
      (is (= {:wrap :log2 :colorspace :HSI} (explicit #'field/domain-color {})))
      (doseq [p [[0.0 0.0] [0.5 -0.3] [-2.0 1.5] [3.0 3.0]]]
        (is (= (f p) (g p)) p))))
  (testing "arrows"
    (is (= (vf/arrows rotation {:domain domain}) (vf/arrows rotation (explicit #'vf/arrows {:domain domain}))))
    (is (= {:domain domain :n 20 :grid :square :scale 1.0} (explicit #'vf/arrows {:domain domain}))))
  (testing "traces"
    (is (= (vf/traces rotation [[0.5 0.0] [0.0 1.0]] {}) (vf/traces rotation [[0.5 0.0] [0.0 1.0]] (explicit #'vf/traces {}))))
    (is (= {:steps 100 :step 0.01} (explicit #'vf/traces {})))))

(deftest nil-and-empty-options-test
  (testing "accepted where nothing is required"
    (is (= (levels/order-levels rows :g :v count) (levels/order-levels rows :g :v count nil)))
    (is (= (levels/rank-rows rows :x :v) (levels/rank-rows rows :x :v nil)))
    (is (= ((field/domain-color cx/sin) [0.5 0.5]) ((field/domain-color cx/sin nil) [0.5 0.5])))
    (is (= (vf/traces rotation [[0.5 0.0]] {}) (vf/traces rotation [[0.5 0.0]] nil))))
  (testing "a required option is missing"
    (is (= :as (:key (error #(levels/bin rows :v nil)))))
    (is (= :as (:key (error #(levels/bin rows :v {:bins 3})))))
    (is (= :as (:key (error #(shingle/shingle rows :v {})))))
    (is (= :domain (:key (error #(vf/arrows rotation nil)))))
    (is (= :domain (:key (error #(vf/arrows rotation {:n 3})))))))

(deftest bad-options-test
  (testing "options are checked first: nil data, keyed by the option"
    (is (= :bins (:key (error #(levels/bin nil nil {:as :b :bins 0})))))
    (is (= :descending (:key (error #(levels/order-levels nil nil nil nil {:descending 1})))))
    (is (= :ties (:key (error #(levels/rank-rows nil nil nil {:ties :nope})))))
    (is (= :count (:key (error #(shingle/shingle nil nil {:as :s :count 0})))))
    (is (= :wrap (:key (error #(field/domain-color nil {:wrap :log3})))))
    (is (= :n (:key (error #(vf/arrows nil {:domain domain :n 0})))))
    (is (= :steps (:key (error #(vf/traces nil nil {:steps 1.5}))))))
  (testing "wrong types"
    (is (= :digits (:key (error #(levels/bin rows :v {:as :b :digits 0})))))
    (is (= :label (:key (error #(levels/bin rows :v {:as :b :label 3})))))
    (is (= :key (:key (error #(levels/rank-rows rows :x :v {:key nil})))))
    (is (= :descending (:key (error #(levels/rank-rows rows :x :v {:descending nil})))))
    (is (= :overlap (:key (error #(shingle/shingle rows :v {:as :s :overlap 1.0})))))
    (is (= :intervals (:key (error #(shingle/shingle rows :v {:as :s :intervals []})))))
    (is (= :colorspace (:key (error #(field/domain-color cx/sin {:colorspace :LAB})))))
    (is (= :x (:key (error #(vf/arrows rotation {:domain {:x [1 1] :y [0 1]}})))))
    (is (= :grid (:key (error #(vf/arrows rotation {:domain domain :grid :hex})))))
    (is (= :scale (:key (error #(vf/arrows rotation {:domain domain :scale ##Inf})))))
    (is (= :step (:key (error #(vf/traces rotation [[0 0]] {:step 0}))))))
  (testing "unknown keys"
    (doseq [[k f] [[:bin #(levels/bin rows :v {:as :b :bin 3})]
                   [:desc #(levels/order-levels rows :g :v count {:desc true})]
                   [:tie #(levels/rank-rows rows :x :v {:tie :min})]
                   [:counts #(shingle/shingle rows :v {:as :s :counts 3})]
                   [:wrapping #(field/domain-color cx/sin {:wrapping :sin})]
                   [:size #(vf/arrows rotation {:domain domain :size 3})]
                   [:steps-n #(vf/traces rotation [[0 0]] {:steps-n 3})]]]
      (is (= k (:key (error f))) k))))

(deftest data-errors-test
  (testing "data checks fail as before"
    (is (= :accessor (:key (error #(levels/bin rows "v" {:as :b})))))
    (is (= :label (:key (error #(levels/bin rows (fn [r] (:v r)) {:as :b :label true})))))
    (is (= :bins (:key (error #(levels/bin [{:v 1} {:v 2}] :v {:as :b :bins :nope})))) "unknown method (data-time check)")
    (is (= :accessor (:key (error #(shingle/shingle rows "v" {:as :s})))))
    (is (= :label (:key (error #(shingle/shingle rows (fn [r] (:v r)) {:as :s :label true})))))
    (is (= :shingle (:key (error #(shingle/shingle [{:v "x"}] :v {:as :s :intervals [[0 1]]})))))
    (is (= :f (:key (error #(vf/arrows (constantly 1.0) {:domain domain})))))
    (is (= :seeds (:key (error #(vf/traces rotation 5 {})))))
    (is (= :seeds (:key (error #(vf/mapped-points rotation [[1 2 3]])))))))
