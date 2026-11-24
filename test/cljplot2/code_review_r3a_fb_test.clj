(ns cljplot2.code-review-r3a-fb-test
  "Code review round 3, A3 renames (fork FB): outline keys of part-of-whole charts, contour `:filled`, `:binned`,
  bin2d `:aspect`, violin `:inner-color :contrast` / `:none`, format options taking a format string or a function."
  (:require [clojure.test :refer [deftest testing is]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.schema :as schema]))

(def ^:private size {:width 300 :height 240})

(def ^:private rows
  (for [i (range 60)] {:v (double (mod (* i 7) 23)) :g (if (even? i) "a" "b")}))

(defn- error-key [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (:key (ex-data e)))))

(deftest text-formatter-test
  (testing "a format string, a function, nil"
    (is (= "1.50" ((schema/text-formatter "%.2f") 1.5)))
    (is (= "x1" ((schema/text-formatter #(str "x" %)) 1)))
    (is (nil? (schema/text-formatter nil)))))

(deftest format-options-test
  (testing "strip :n-format and bar :value-format take a function"
    (is (some? (plot/render {:data rows :mapping {:x :v :y :g} :charts [(chart/box {:n-label true :n-format #(str % " rows")})]} size)))
    (is (some? (plot/render {:data rows :mapping {:x :g :y :v}
                             :charts [(chart/bar {:stat :mean :value-label true :value-format #(format "%.0f!" %)})]} size))))
  (testing "a format string that can't format is still an error; a non-string non-function is rejected"
    (is (= :n-format (error-key #(plot/build {:data rows :mapping {:x :v :y :g} :charts [(chart/box {:n-label true :n-format "%s %s"})]} size))))
    (is (some? (error-key #(plot/build {:data rows :mapping {:x :v :y :g} :charts [(chart/box {:n-format 3})]} size))))))

(deftest violin-inner-color-test
  (doseq [c [:contrast :none [200 0 0]]]
    (is (some? (plot/render {:data rows :mapping {:x :v :y :g} :charts [(chart/violin {:inner :box :inner-color c})]} size)) (str c)))
  (is (some? (error-key #(plot/build {:data rows :mapping {:x :v :y :g} :charts [(chart/violin {:inner-color :darker})]} size)))))

(deftest renamed-keys-test
  (testing "old keys are rejected, new ones taken"
    (is (some? (error-key #(plot/build {:data rows :mapping {:x :v :y :v} :charts [(chart/bin2d {:shape 0.5})]} size))))
    (is (some? (plot/build {:data rows :mapping {:x :v :y :v} :charts [(chart/bin2d {:aspect 0.5})]} size)))
    (is (some? (error-key #(plot/build {:charts [(chart/contour {:f (fn [[x y]] (+ x y)) :domain {:x [0 1] :y [0 1]} :fill true})]} size))))
    (is (some? (plot/build {:charts [(chart/contour {:f (fn [[x y]] (+ x y)) :domain {:x [0 1] :y [0 1]} :filled true})]} size)))
    (doseq [k [:border-color :border-width]]
      (is (some? (error-key #(plot/build {:data [{:v 1 :n "a"}] :mapping {:value :v :hierarchy [:n]} :charts [(chart/treemap {k 1})]} size))) (str k)))))
