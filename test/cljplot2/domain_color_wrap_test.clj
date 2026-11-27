(ns cljplot2.domain-color-wrap-test
  "Tests of `field/domain-color`'s `:wrap` methods: `:log2` / `:log10` of the magnitude, `:log2+` / `:log10+` of the magnitude plus 1."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure2d.color :as c]
            [fastmath.complex :as cx]
            [fastmath.core :as m]
            [cljplot2.data.field :as field]
            [cljplot2.test-util :refer [error-data]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- wrapped
  "The magnitude `v` wrapped by method `w` (the private `field/wrap`)."
  ^double [w ^double v]
  (#'field/wrap w v))

(defn- near?
  "Tests whether doubles `a` and `b` differ by less than 1e-12."
  [^double a ^double b]
  (m/< (m/abs (m/- a b)) 1.0E-12))

(defn- brightness
  "The HSB brightness (0-255) of colour `col`."
  ^double [col]
  (nth (c/to-HSB* col) 2))

(def ^:private log-methods
  "The wraps of a log of the magnitude: they wrap a zero to `NaN`."
  #{:log2 :log10})

(deftest log-wraps-test
  (testing ":log2 / :log10 - the fractional part of |log v|, the same for v and 1/v"
    (is (near? 0.5849625007211561 (wrapped :log2 1.5)))
    (is (near? (m/frac (m/log10 1.5)) (wrapped :log10 1.5)))
    (doseq [v [1.5 3.3 0.7 2.01 10.5]]
      (is (near? (wrapped :log2 v) (wrapped :log2 (m// 1.0 (double v)))) ["1/v wraps as v" v])
      (is (near? (wrapped :log10 v) (wrapped :log10 (m// 1.0 (double v)))) ["1/v wraps as v" v])))
  (testing "band ends: powers of the base wrap to 0, the magnitude 1 too"
    (doseq [v [1.0 2.0 4.0 0.5 0.25 1024.0]]
      (is (near? 0.0 (wrapped :log2 v)) v))
    (doseq [v [1.0 10.0 100.0 1000.0 1.0E300 0.1 0.01 0.001]]
      (is (near? 0.0 (wrapped :log10 v)) v)))
  (testing "a zero magnitude: log 0 wraps to NaN (drawn like a pole); extreme magnitudes stay finite"
    (is (m/nan? (wrapped :log2 0.0)))
    (is (m/nan? (wrapped :log10 0.0)))
    (is (near? 0.0 (wrapped :log10 1.0E-300)) "a band end: log10 is -300")
    (is (near? (wrapped :log2 1.0E-300) (wrapped :log2 1.0E300)) "1/v wraps as v at the extremes")
    (is (near? 0.0 (wrapped :log2 Double/MIN_VALUE)) "log2 is -1074")
    (doseq [w log-methods v [1.0E-300 1.0E300 Double/MIN_VALUE]]
      (is (m/valid-double? (wrapped w v)) [w v])))
  (testing ":log2+ / :log10+ - the fractional part of log(v + 1) (the first :log2 / :log10)"
    (doseq [v [0.0 0.3 1.0 1.5 3.0 99.0]]
      (is (near? (m/frac (m/log2 (m/inc (double v)))) (wrapped :log2+ v)) v)
      (is (near? (m/frac (m/log10 (m/inc (double v)))) (wrapped :log10+ v)) v))
    (is (near? 0.0 (wrapped :log2+ 0.0)) "a zero is dark")
    (is (near? 0.0 (wrapped :log2+ 1.0E-300)) "dark near a zero - unlike :log2")
    (is (near? 0.0 (wrapped :log2+ 1.0)) "1 + 1 = 2")
    (is (near? 0.0 (wrapped :log2+ 3.0)) "3 + 1 = 4")
    (is (near? 0.0 (wrapped :log10+ 9.0)) "9 + 1 = 10")
    (is (near? 0.3010299956639812 (wrapped :log10+ 1.0)) "log10 2"))
  (testing "non-finite magnitudes: NaN gives NaN under every method; infinity NaN, but 1 under :exp and :sigmoid"
    (doseq [w field/wrap-methods]
      (is (m/nan? (wrapped w ##NaN)) w))
    (doseq [w (disj field/wrap-methods :exp :sigmoid)]
      (is (m/nan? (wrapped w ##Inf)) w))
    (is (m/one? (wrapped :exp ##Inf)))
    (is (m/one? (wrapped :sigmoid ##Inf)))))

(deftest wrap-values-test
  (testing "every method wraps magnitudes into [0 1]; the others than :log2 / :log10 also 0 and the extremes"
    (doseq [w field/wrap-methods v [0.1 0.7 1.5 3.3 50.0]]
      (is (m/<= 0.0 (wrapped w v) 1.0) [w v]))
    (doseq [w (apply disj field/wrap-methods log-methods) v [0.0 1.0E-300 1.0E300]]
      (is (m/<= 0.0 (wrapped w v) 1.0) [w v])))
  (testing "the eight methods"
    (is (= #{:log2 :log10 :log2+ :log10+ :sin :exp :sigmoid :frac} field/wrap-methods)))
  (testing "the brightness at a zero (gamma 1): full for :log2 / :log10, dark for :log2+ / :log10+ / :exp / :frac, half for :sin / :sigmoid"
    (let [at-zero (fn [w] (brightness ((field/domain-color identity {:wrap w :gamma 1.0}) [0.0 0.0])))]
      (doseq [w log-methods]
        (is (m/> (at-zero w) 254.0) w))
      (doseq [w [:log2+ :log10+ :exp :frac]]
        (is (m/< (at-zero w) 1.0) w))
      (doseq [w [:sin :sigmoid]]
        (is (m/< (m/abs (m/- (double (at-zero w)) 127.5)) 1.0) w))))
  (testing "the brightness at a pole: full under every method"
    (doseq [w field/wrap-methods]
      (is (m/> (brightness ((field/domain-color (fn [z] (cx/div cx/ONE z)) {:wrap w :gamma 1.0}) [0.0 0.0])) 254.0) w)))
  (testing "an unknown or misspelt method is rejected with the enum's message"
    (doseq [bad [:log3 :log2- "log2" :LOG2 nil]]
      (let [{:keys [errors] :as data} (error-data #(field/domain-color identity {:wrap bad}))]
        (is (= :wrap (:key data)) (pr-str bad))
        (is (re-find #"should be either" (str (:message (first errors)))) (pr-str bad))))))
