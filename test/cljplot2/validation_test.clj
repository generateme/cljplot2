(ns cljplot2.validation-test
  "The validation boundary of `build` (see cljplot2.schema): one error per problem, all errors at once, before any
  stage runs."
  (:require [clojure.test :refer [deftest testing is are]]
            [cljplot2.test-util :refer [error-data error error-ins]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.schema :as schema]))

(def ^:private rows [{:x 1 :y 2} {:x 2 :y 3}])
(def ^:private scatter-plot {:data rows :mapping {:x :x :y :y} :charts [(chart/scatter)]})

(defn- message [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-message e))))

(deftest valid-inputs-test
  (testing "nothing given, nil meaning not set"
    (is (map? (plot/build {})))
    (is (map? (plot/build {:theme nil :options nil :legend nil :aspect nil :groups nil :labels nil :facets nil})))
    (is (map? (plot/build scatter-plot {:width 1 :height 1 :until :scale-specs})) "smallest valid size")
    (is (map? (plot/build (assoc scatter-plot :theme {:legend {:gap nil} :plot {:background :none}}))))))

(deftest error-shape-test
  (testing "one error"
    (let [data (error-data #(plot/build (assoc scatter-plot :legend :yes)))]
      (is (= {:key :legend :in [:legend] :value :yes
              :errors [{:in [:legend] :value :yes :message "should be true or false"}]}
             data))
      (is (= "Invalid input: [:legend] should be true or false, got :yes"
             (message #(plot/build (assoc scatter-plot :legend :yes)))))))
  (testing "not a map at all"
    (is (= [] (:in (error #(plot/build 5)))))
    (is (= :input (:key (error #(plot/build 5)))))
    (is (= [:theme] (:in (error #(plot/build {:theme 5})))))
    (is (= [:options] (:in (error #(plot/build {:options 5})))))
    (is (= [:charts 0] (:in (error #(plot/build {:charts [5]}))))))
  (testing "long values are cut in the message"
    (is (< (count (message #(plot/build {:legend (vec (range 10000))}))) 150))))

(deftest all-errors-test
  (let [computed (atom 0)
        counting (assoc (chart/scatter) :compute (fn [& _] (swap! computed inc) {}))
        bad {:data rows :mapping {:x :x :y :y} :charts [counting]
             :theme {:legend {:gap -1}}
             :scales {:x {:domain [5 5]}}}]
    (testing "errors of several owners in one ex-info"
      (is (= #{[:options :width] [:theme :legend :gap] [:scales :x :domain]}
             (error-ins #(plot/build bad {:width 0})))))
    (testing "no stage runs"
      (error #(plot/build bad {:width 0}))
      (is (zero? @computed))
      (plot/build (dissoc bad :theme :scales))
      (is (= 1 @computed)))
    (testing "the message lists every error"
      (is (re-find #"\(3 errors\)" (message #(plot/build bad {:width 0})))))))

(deftest unknown-keys-test
  (testing "closed maps, with a suggestion for close keys"
    (are [p in suggestion] (and (= in (:in (error #(plot/build p))))
                                (re-find (re-pattern suggestion) (message #(plot/build p))))
      {:lables {:title "t"}} [:lables] "should be spelled :labels"
      {:theme {:legend {:gapp 1}}} [:theme :legend :gapp] "should be spelled :gap"
      (assoc scatter-plot :charts [(chart/scatter {:sise 3})]) [:charts 0 :options :sise] "should be spelled :size"
      {:zzz 1} [:zzz] "disallowed key"))
  (testing "mappings stay open (user charts read their own keys)"
    (is (map? (plot/build (assoc-in scatter-plot [:mapping :weight] :x))))))

(deftest colors-resolved-test
  (are [p in] (= in (:in (error #(plot/build p))))
    {:theme {:plot {:background :no-such-color}}} [:theme :plot :background]
    {:theme {:strip {:fill "zzz"}}} [:theme :strip :fill]
    (assoc scatter-plot :charts [(chart/scatter {:color :no-such-color})]) [:charts 0 :options :color])
  (is (map? (plot/build (assoc scatter-plot :charts [(chart/scatter {:color [10 20 30 40] :stroke-color :none})])))))

(deftest chart-theme-test
  (testing "every built-in chart kind's theme is checked, also when not plotted"
    (is (= {:key :alpha :in [:theme :chart :histogram :alpha] :value 2}
           (error #(plot/build (assoc scatter-plot :theme {:chart {:histogram {:alpha 2}}}))))))
  (testing "theme style keys only"
    (is (= [:theme :chart :histogram :position]
           (:in (error #(plot/build (assoc scatter-plot :theme {:chart {:histogram {:position :dodge}}})))))))
  (testing "a non-map chart theme"
    (is (= [:theme :chart :scatter] (:in (error #(plot/build (assoc scatter-plot :theme {:chart {:scatter 5}}))))))))

(deftest rules-test
  (testing "rules across keys run once the shapes hold"
    (is (= #{[:legend]}
           (error-ins #(plot/build (assoc scatter-plot :legend 1 :scales {:color {:domain ["a"]}}))))))
  (is (= [:scales :color] (:in (error #(plot/build (assoc scatter-plot :scales {:color {:domain ["a"]}})))))))

(deftest subplot-test
  (testing "a sub-plot validates itself; provenance wraps its error data"
    (let [data (error-data #(plot/build (plot/arrange [scatter-plot (assoc scatter-plot :legend 1)])))]
      (is (= {:key :legend :in [:legend] :value 1 :subplot [:plot 1] :subplot-path [[:plot 1]]}
             (dissoc data :errors)))
      (is (= [{:in [:legend] :value 1 :message "should be true or false"}] (:errors data))))))

(deftest schema-helpers-test
  (testing "fail! gives the same shape"
    (is (= {:key :k :in [:a :k] :value 1 :errors [{:in [:a :k] :value 1 :message "bad"}]}
           (error-data #(schema/fail! "bad" {:key :k :in [:a :k] :value 1})))))
  (testing "explain! is nil for valid values, rules see bad values safely"
    (is (nil? (schema/explain! [[[:map [:a schema/pos]] {:a 1} []]])))
    (is (= [[:a]] (map :in (schema/errors [:and [:map [:a [:vector int?]]] (schema/rule #(when (apply < (:a %)) "x"))]
                                          {:a "zz"} [])))))
  (testing "many distinct schemas (made per call) still work"
    (is (every? nil? (for [i (range 300)] (schema/explain! [[[:map [:a [:= i]]] {:a i} []]]))))))
