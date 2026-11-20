(ns cljplot2.describe-test
  "Reference part R1a: `cljplot2.describe` rows, type texts, fragments, API vars."
  (:require [clojure.test :refer [deftest testing is]]
            [cljplot2.chart :as chart]
            [cljplot2.describe :as describe]
            [cljplot2.schema :as schema]))

(defn- row [rows k] (first (filter #(= [k] (:path %)) rows)))

(deftest chart-rows-test
  (let [rows (describe/options (chart/bullet))]
    (testing "style rows take theme defaults"
      (is (= 0.35 (:default (row rows :measure-width))))
      (is (= "a number in [0 1]" (:type (row rows :measure-width)))))
    (testing "fragment rows carry their fragment"
      (is (= :common (:fragment (row rows :data))))
      (is (= :strip (:fragment (row rows :place))))
      (is (nil? (:fragment (row rows :measure-width)))))
    (testing "every row has the documented keys"
      (is (every? #(= #{:key :path :type :required? :default :default-doc :description :fragment} (set (keys %))) rows)))))

(deftest schema-rows-test
  (let [s [:map {:closed true}
           [:a {:description "a" :default 1} :int]
           (schema/optional :b {:cljplot2/default-doc "computed"} [:maybe [:map {:closed true} (schema/optional :c {:description "c"} :string)]])
           (schema/optional :d [:and :int [:> 0]])
           (schema/optional :internal (schema/forbidden "is internal"))]
        rows (describe/schema-rows s)]
    (testing "required and optional, defaults and default texts"
      (is (= [true 1 nil] ((juxt :required? :default :default-doc) (row rows :a))))
      (is (= [false nil "computed"] ((juxt :required? :default :default-doc) (row rows :b)))))
    (testing "a nested closed map: its own row (type map), then its entries' rows"
      (is (= [[:a] [:b] [:b :c] [:d]] (map :path rows)))
      (is (= "map" (:type (row rows :b)))))
    (testing "forbidden entries have no row"
      (is (nil? (row rows :internal))))
    (testing "boundary: no entries, nil, a non-map schema"
      (is (= [] (describe/schema-rows [:map])))
      (is (= [] (describe/schema-rows nil)))
      (is (= [] (describe/schema-rows :int))))
    (testing "with-rules schemas (an :and) still give rows"
      (is (= [[:x]] (map :path (describe/schema-rows (schema/with-rules [:map [:x :int]] [(constantly nil) nil]))))))))

(deftest type-text-test
  (is (= "a non-negative number" (describe/type-text schema/non-neg)))
  (is (= "one of :a :b" (describe/type-text [:enum :a :b])))
  (is (= "integer (2..2000)" (describe/type-text [:int {:min 2 :max 2000}])))
  (is (= "[a number in [0 1], keyword]" (describe/type-text [:tuple schema/fraction keyword?])))
  (is (= "true or false or nil" (describe/type-text [:maybe schema/bool])))
  (is (= "a finite number, >= 0" (describe/type-text [:and schema/finite [:>= 0]])))
  (is (= "{:at a finite number}" (describe/type-text [:map {:closed true} [:at schema/finite]])))
  (is (= "vector of keyword (at least 1)" (describe/type-text [:vector {:min 1} keyword?])))
  (is (= "map of one of :x :y to string" (describe/type-text [:map-of [:enum :x :y] :string])))
  (is (= ":none or 1" (describe/type-text [:or [:= :none] [:= 1]])))
  (is (= "vector, [keyword, optional map]" (describe/type-text [:and vector? [:cat :keyword [:? :map]]])))
  (is (= "sequence" (describe/type-text sequential?)))
  (testing "named types: type doc over message"
    (is (re-find #"^a fastmath 1D" (describe/type-text schema/interpolator)))
    (is (re-find #"^:none, a dash style" (describe/type-text schema/dash)))))

(deftest fragments-test
  (testing "in-fragment tags both entry forms"
    (is (= [[:a {:cljplot2/fragment :f} :int] [:b {:optional true :description "b" :cljplot2/fragment :f} :int]]
           (schema/in-fragment :f [[:a :int] (schema/optional :b {:description "b"} :int)])))
    (is (= [] (schema/in-fragment :f []))))
  (testing "every fragment has a title and doc"
    (is (every? (fn [[_ v]] (and (string? (:title v)) (string? (:doc v)))) describe/fragments))))

(deftest api-vars-test
  (let [inside (create-ns 'cljplot2.describe-probe)
        outside (create-ns 'describe-probe-outside)]
    (try
      (let [api (intern inside (with-meta 'f {:cljplot2/api true
                                              :cljplot2/options [:map (schema/optional :k {:description "k"} :int)]})
                        (fn [] nil))
            _ (intern inside 'g (fn [] nil))
            _ (intern outside (with-meta 'h {:cljplot2/api true}) (fn [] nil))]
        (testing "only :cljplot2/api vars of cljplot2.* namespaces"
          (is (some #(= api %) (describe/api-vars)))
          (is (not-any? #(#{"g" "h"} (str (:name (meta %)))) (describe/api-vars))))
        (testing "sorted by namespace and name"
          (is (= (describe/api-vars) (sort-by (fn [v] [(str (:ns (meta v))) (str (:name (meta v)))]) (describe/api-vars)))))
        (testing "a var's :cljplot2/options rows; none without them"
          (is (= [[:k]] (map :path (describe/options api))))
          (is (= [] (describe/options (ns-resolve inside 'g))))))
      (finally
        (remove-ns 'cljplot2.describe-probe)
        (remove-ns 'describe-probe-outside)))))
