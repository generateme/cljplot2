(ns cljplot2.code-review-r3a-f2-test
  "Code review round 3, batch A, xy charts: option conflicts failing the options schema at build validation, `:dropped`
  keys per mapped dimension (rule, text, rect, candlestick), error keys naming the mapping, the padding pair of
  parallel."
  (:require [clojure.test :refer [deftest testing is]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.test-util :refer [error error-ins]]))

(def ^:private size {:width 300 :height 300})

(defn- build [p] (plot/build p size))

(def ^:private rows [{:x 1 :y 1} {:x 2 :y 3} {:x 3 :y 2}])

(defn- option-error-in
  "The `:in` paths of the build errors of one chart over `rows`."
  [c]
  (error-ins #(build {:data rows :mapping {:x :x :y :y} :charts [c]})))

(defmacro ^:private are-in
  "Asserts every `[chart key]` pair's build fails at `[:charts 0 :options key]`."
  [pairs]
  `(doseq [[c# k#] ~pairs]
     (is (contains? (option-error-in c#) [:charts 0 :options k#]) (str (:kind c#) " " k#))))

(deftest option-rules-test
  (testing "conflicts fail the chart's options schema (paths under :options)"
    (are-in [[(chart/line {:fill-gradient-axis :y}) :fill-gradient-axis]
             [(chart/line {:position :stack :curve :step}) :curve]
             [(chart/line {:position :stack :gaps :bridge}) :gaps]
             [(chart/line {:trail true :dash [2 2]}) :trail]
             [(chart/ribbon {:fill-gradient-axis :y}) :fill-gradient-axis]
             [(chart/smooth {:method :lm :band :bootstrap :method-params {:degree 0}}) :method-params]
             [(chart/smooth {:method :linear :band :confidence}) :band]
             [(chart/smooth {:method :loess :band :prediction}) :band]
             [(chart/rule {:slope 1}) :intercept]
             [(chart/rule {:intercept 1}) :slope]])
    (is (contains? (error-ins #(build {:charts [(chart/function {})]})) [:charts 0 :options :f]))
    (is (contains? (error-ins #(build {:charts [(chart/function {:parametric (fn [t] [t t])})]})) [:charts 0 :options :t]))
    (is (contains? (error-ins #(build {:charts [(chart/function {:f identity :t [0 1]})]})) [:charts 0 :options :t]))
    (is (contains? (error-ins #(build {:charts [(chart/function {:parametric (fn [t] [t t]) :t [0 1] :domain [0 1]})]}))
                   [:charts 0 :options :domain])))
  (testing "valid combinations still build"
    (is (map? (build {:data rows :mapping {:x :x :y :y} :charts [(chart/line {:fill-gradient :viridis :fill-gradient-axis :y})]})))
    (is (map? (build {:charts [(chart/rule {:slope 1 :intercept 0})] :scales {:x {:domain [0 1]} :y {:domain [0 1]}}})))))

(deftest dropped-test
  (testing "rule counts invalid mapped positions"
    (let [b (build {:data [{:v 1} {:v ##NaN} {:v nil}] :charts [(chart/rule {:mapping {:x :v}})]
                    :scales {:y {:domain [0 1]}}})]
      (is (= 2 (get-in b [:stats 0 :dropped :x])))))
  (testing "text counts rows without a position"
    (let [b (build {:data [{:x 1 :y 1 :t "a"} {:x ##NaN :y 1 :t "b"}]
                    :charts [(chart/text {:mapping {:x :x :y :y :label :t}})]})]
      (is (= {:x 1 :y 0} (select-keys (get-in b [:stats 0 :dropped]) [:x :y])))))
  (testing "rect counts per mapped end"
    (is (= {:x0 1 :x1 0} (select-keys (get-in (build {:data [{:a 1 :b 2} {:a ##NaN :b 3}]
                                                       :charts [(chart/rect {:mapping {:x0 :a :x1 :b}})]
                                                       :scales {:y {:domain [0 1]}}})
                                              [:stats 0 :dropped])
                                      [:x0 :x1]))))
  (testing "candlestick: an inconsistent complete row counts only under :ohlc"
    (let [b (build {:data [{:d 1 :o 1 :h 2 :l 0 :c 1.5} {:d 2 :o 1 :h 0.5 :l 0 :c 1.5}]
                    :mapping {:x :d :open :o :high :h :low :l :close :c}
                    :charts [(chart/candlestick)]})]
      (is (= {:x 0 :open 0 :high 0 :low 0 :close 0 :ohlc 1} (get-in b [:stats 0 :dropped]))))))

(deftest error-keys-test
  (testing "smooth: a non-numeric x names the mapping"
    (is (= [:mapping [:mapping :x]]
           ((juxt :key :in) (error #(build {:data [{:x "a" :y 1} {:x "b" :y 2}] :mapping {:x :x :y :y}
                                            :charts [(chart/smooth {:method :lm})]}))))))
  (testing "annotations: numbers mixed with levels name the mapping"
    (is (= :mapping (:key (error #(build {:data [{:v 1} {:v "a"}] :charts [(chart/rule {:mapping {:x :v}})]
                                          :scales {:y {:domain [0 1]}}})))))))

(deftest parallel-padding-test
  (is (contains? (get-in (build {:data [{:a 1 :b 2} {:a 2 :b 1}] :charts [(chart/parallel {:variables [:a :b]})]})
                         [:stats 0])
                 :padding-explicit)))
