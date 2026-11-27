(ns cljplot2.domain-color-gamma-test
  "Tests of `field/domain-color`'s `:gamma`: the wrapped magnitude raised to it sets the colour space's third channel."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure2d.color :as c]
            [fastmath.complex :as cx]
            [fastmath.core :as m]
            [cljplot2.data.field :as field]
            [cljplot2.test-util :refer [error error-data]]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- brightness
  "The HSB brightness channel (0-255) of colour `col`."
  ^double [col]
  (nth (c/to-HSB* col) 2))

(defn- third-channel
  "The third channel (0-255) of colour `col` in hue-first colour space `colorspace`."
  ^double [colorspace col]
  (nth ((first (c/colorspaces* colorspace)) col) 2))

(defn- near?
  "Tests whether doubles `a` and `b` differ by less than one channel step."
  [^double a ^double b]
  (m/< (m/abs (m/- a b)) 1.0))

(def ^:private point
  "A point inside a band of every wrap of the identity: |z| 1.5, under the default `:log2` wrapped log2 1.5 (about 0.585)."
  [1.5 0.0])

(def ^:private wrapped
  "The wrapped magnitude at [[point]]."
  (m/frac (m/log2 1.5)))

(deftest gamma-brightness-test
  (testing "the brightness is the wrapped magnitude raised to :gamma (HSB: the third channel)"
    (doseq [gamma [0.2 0.5 1.0 2.0 3.0]]
      (is (near? (brightness ((field/domain-color identity {:gamma gamma}) point)) (m/* 255.0 (m/pow wrapped gamma))) gamma)))
  (testing "the saturation follows the raised value (3/4 to full); the hue ignores :gamma"
    (let [[_ s] (c/to-HSB* ((field/domain-color identity {:gamma 0.5}) point))
          b (m/* 255.0 (m/pow wrapped 0.5))]
      (is (near? s (m/- 255.0 (m// (m/- 255.0 b) 4.0)))))
    (let [hues (map #(first (c/to-HSB* ((field/domain-color identity {:gamma %}) [0.3 0.4]))) [1.0E-9 0.2 1.0 3.0])]
      (is (apply = hues) hues)))
  (testing "the default :wrap is :log2"
    (doseq [p [[0.0 0.0] [0.7 0.0] [1.5 0.0] [3.0 2.0]]]
      (is (= ((field/domain-color identity) p) ((field/domain-color identity {:wrap :log2}) p)) p)))
  (testing "the default is 0.2: the same colours as an explicit 0.2, other than 1"
    (is (= ((field/domain-color identity) point) ((field/domain-color identity {:gamma 0.2}) point)))
    (is (not= ((field/domain-color identity) point) ((field/domain-color identity {:gamma 1.0}) point))))
  (testing "below 1 brightens, above 1 darkens, monotone in :gamma"
    (let [bs (mapv #(brightness ((field/domain-color identity {:gamma %}) point)) [0.1 0.5 1.0 2.0 10.0])]
      (is (apply > bs) bs)))
  (testing "an integer :gamma reads as the double"
    (is (= ((field/domain-color identity {:gamma 1}) point) ((field/domain-color identity {:gamma 1.0}) point)))
    (is (= ((field/domain-color identity {:gamma 2}) point) ((field/domain-color identity {:gamma 2.0}) point))))
  (testing ":HSI with :wrap :log2+ and :gamma 1 gives the colours of the first default (old cljplot's example)"
    (is (= (c/to-color [14.248959277767629 165.65057667832318 327.10268824128354 255.0])
           (c/to-color ((field/domain-color identity {:colorspace :HSI :wrap :log2+ :gamma 1.0}) [0.5 0.3]))))
    (is (= (c/to-color [98.24530004422213 7.107299335094821 561.8599611160525 255.0])
           (c/to-color ((field/domain-color cx/log {:colorspace :HSI :wrap :log2+ :gamma 1.0}) [-2.0 1.5]))))))

(deftest gamma-per-wrap-and-space-test
  (testing "every wrap: the brightness is its wrapped value raised to :gamma (|z| 1.5 lies inside each band)"
    (doseq [w field/wrap-methods]
      (let [v (#'field/wrap w 1.5)]
        (is (m/< 0.0 v 1.0) [w v])
        (is (near? (brightness ((field/domain-color identity {:wrap w :gamma 2.0}) point)) (m/* 255.0 (m/sq v))) w))))
  (testing "every brightness-like space takes the raised value as its third channel"
    (doseq [cs (disj field/hue-colorspaces :HWB)]
      (is (near? (third-channel cs ((field/domain-color identity {:colorspace cs :gamma 0.5}) point))
                 (m/* 255.0 (m/pow wrapped 0.5)))
          cs)))
  (testing ":HWB (third channel blackness) changes with :gamma too"
    (is (not= ((field/domain-color identity {:colorspace :HWB :gamma 0.5}) point)
              ((field/domain-color identity {:colorspace :HWB :gamma 1.0}) point)))))

(deftest gamma-edges-test
  (testing "zeros stay black (:log2+), poles full at every :gamma (0 and 1 are fixed points of a power); under :log2 a zero is full too (log 0)"
    (doseq [gamma [1.0E-9 0.2 1.0 1.0E9]]
      (doseq [w [:log2+ :log10+]]
        (is (m/< (brightness ((field/domain-color identity {:wrap w :gamma gamma}) [0.0 0.0])) 1.0) [w gamma]))
      (doseq [w [:log2 :log10]]
        (is (m/> (brightness ((field/domain-color identity {:wrap w :gamma gamma}) [0.0 0.0])) 254.0) [w gamma]))
      (is (m/> (brightness ((field/domain-color (fn [z] (cx/div cx/ONE z)) {:gamma gamma}) [0.0 0.0])) 254.0) gamma)))
  (testing "a tiny :gamma makes inside a band full, a huge one black; every channel finite (HSI / HCL may leave the RGB gamut at any :gamma - drawing clamps)"
    (is (m/> (brightness ((field/domain-color identity {:gamma 1.0E-9}) point)) 254.0))
    (is (m/< (brightness ((field/domain-color identity {:gamma 1.0E9}) point)) 1.0))
    (doseq [gamma [1.0E-9 0.2 1.0E9] colorspace field/hue-colorspaces p [[0.0 0.0] point [0.99 0.0] [3.0 2.0]]]
      (let [col ((field/domain-color identity {:gamma gamma :colorspace colorspace}) p)]
        (is (every? #(m/valid-double? (double %)) (c/to-RGB* col)) [gamma colorspace p])))))

(deftest gamma-schema-test
  (testing "a positive finite number only: keyed :gamma with the type's message"
    (doseq [bad [0 0.0 -1.0 ##Inf ##-Inf ##NaN nil "1" :x [1.0]]]
      (let [{:keys [errors] :as data} (error-data #(field/domain-color identity {:gamma bad}))]
        (is (= :gamma (:key data)) (pr-str bad))
        (is (= "should be a positive finite number" (:message (first errors))) (pr-str bad)))))
  (testing "accepted at the edges of the range"
    (doseq [ok [Double/MIN_VALUE 1 1.0E300]]
      (is (nil? (error #(field/domain-color identity {:gamma ok}))) ok)
      (is (some? ((field/domain-color identity {:gamma ok}) point)) ok))))
