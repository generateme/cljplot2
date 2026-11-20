(ns cljplot2.reference-r2-series-test
  "Reference part R2, time series: `acf` / `horizon` and the `cljplot2.data.series` functions document and validate
  their options - constant defaults are the real ones, options are checked first, data errors stay."
  (:require [clojure.test :refer [deftest testing is]]
            [cljplot2.data.series :as series]
            [cljplot2.describe :as describe]
            [cljplot2.series-plots :as sp]
            [cljplot2.test-util :as tu]))

(def ^:private values
  "A small deterministic series."
  (mapv (fn [i] (+ (Math/sin (* 0.7 (double i))) (* 0.3 (Math/cos (* 2.3 (double i)))))) (range 40)))

(def ^:private wave
  "Horizon rows of two series, both signs."
  (vec (for [s ["a" "b"] i (range 30)]
         {:x i :y (* (if (= s "a") 30.0 12.0) (Math/sin (* 0.4 (double i)))) :s s})))

(def ^:private render-options {:width 400 :height 300})

(deftest builder-defaults-test
  (testing "acf: every constant default is the real one"
    (is (tu/builder-defaults-hold? #'sp/acf [values] {} render-options))
    (is (tu/builder-defaults-hold? #'sp/acf [values] {:colors {:stem :black}} render-options) "partial nested colours")
    (is (tu/builder-defaults-hold? #'sp/acf [values] {:partial? :pacf :lags 8} render-options)))
  (testing "horizon: every constant default is the real one"
    (is (tu/builder-defaults-hold? #'sp/horizon [wave] {} render-options))
    (is (tu/builder-defaults-hold? #'sp/horizon [wave] {:series :s :colors {:positive [:pink :red :darkred]}} render-options)
        "partial nested colours")))

(deftest api-defaults-test
  (testing "acf-rows"
    (is (= (series/acf-rows values {})
           (series/acf-rows values (tu/explicit-defaults (describe/options #'series/acf-rows) {})))))
  (testing "horizon-rows"
    (is (= (series/horizon-rows wave {})
           (series/horizon-rows wave (tu/explicit-defaults (describe/options #'series/horizon-rows) {}))))
    (is (= (series/horizon-rows wave {:series :s})
           (series/horizon-rows wave (tu/explicit-defaults (describe/options #'series/horizon-rows) {:series :s}))))))

(deftest boundaries-test
  (testing "nil options and {} - the defaults"
    (is (= (sp/acf values) (sp/acf values nil) (sp/acf values {})))
    (is (= (sp/horizon wave) (sp/horizon wave nil) (sp/horizon wave {})))
    (is (= (series/acf-rows values {}) (series/acf-rows values nil)))
    (is (= (series/horizon-rows wave {}) (series/horizon-rows wave nil))))
  (testing "a wrong type fails keyed by the option, before the data is read"
    (is (= :band (:key (tu/error #(sp/acf nil {:band :blocky})))))
    (is (= :lags (:key (tu/error #(sp/acf nil {:lags 1.5})))))
    (is (= :alpha (:key (tu/error #(sp/acf nil {:alpha 1.0})))))
    (is (= :stem (:key (tu/error #(sp/acf nil {:colors {:stem :not-a-colour}})))))
    (is (= :bands (:key (tu/error #(sp/horizon nil {:bands 0})))))
    (is (= :alpha (:key (tu/error #(sp/horizon nil {:alpha 2.0})))))
    (is (= :positive (:key (tu/error #(sp/horizon nil {:colors {:positive :red}})))))
    (is (= :partial? (:key (tu/error #(series/acf-rows nil {:partial? :yes})))))
    (is (= :alpha (:key (tu/error #(series/acf-rows nil {:alpha 0})))))
    (is (= :band-height (:key (tu/error #(series/horizon-rows nil {:band-height ##Inf})))))
    (is (= :interpolator (:key (tu/error #(series/horizon-rows nil {:interpolator :nope})))))
    (is (= :steps-per-gap (:key (tu/error #(series/horizon-rows nil {:steps-per-gap 0}))))))
  (testing "an unknown key fails keyed by it"
    (is (= :lag (:key (tu/error #(sp/acf nil {:lag 3})))))
    (is (= :stems (:key (tu/error #(sp/acf nil {:colors {:stems :red}})))))
    (is (= :colour (:key (tu/error #(sp/horizon nil {:colour :red})))))
    (is (= :up (:key (tu/error #(sp/horizon nil {:colors {:up [:red :blue]}})))))
    (is (= :lag (:key (tu/error #(series/acf-rows nil {:lag 3})))))
    (is (= :band (:key (tu/error #(series/horizon-rows nil {:band 3}))))))
  (testing "data errors still fail as before"
    (is (= :values (:key (tu/error #(series/acf-rows [1 2 ##NaN 4] {})))))
    (is (= :values (:key (tu/error #(sp/acf [1 1 1 1] {})))))
    (is (= :lags (:key (tu/error #(series/acf-rows [1 2 3 4] {:lags 4})))))
    (is (= :x (:key (tu/error #(series/horizon-rows [{:x "a" :y 1}] {})))))
    (is (= :y (:key (tu/error #(sp/horizon [{:x 0 :y "a"}] {})))))
    (is (= :interpolator (:key (tu/error #(series/horizon-rows [{:x 0 :y 1} {:x 0 :y 2} {:x 1 :y 3}] {:interpolator :linear})))))
    (is (= :colors (:key (tu/error #(sp/horizon [{:x 0 :y 1}] {:colors {:positive [[0 0 255]]}})))) "fewer than :bands colours")
    (is (= :lag (:key (tu/error #(series/lag-rows [1 2 3] 0)))))))

(deftest documented-test
  (testing "the user-facing functions are marked"
    (is (every? #(:cljplot2/api (meta %)) [#'series/lag-rows #'series/acf-rows #'series/horizon-rows #'series/horizon-bands])))
  (testing "forwarded rows keep their group"
    (is (= #{:lags :alpha} (set (keep #(when (= :series/acf-rows (:fragment %)) (:key %)) (describe/options #'sp/acf)))))
    (is (= (set (map :key (describe/options #'series/horizon-rows)))
           (set (keep #(when (= :series/horizon-rows (:fragment %)) (:key %)) (describe/options #'sp/horizon)))))))
