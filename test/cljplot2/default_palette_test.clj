(ns cljplot2.default-palette-test
  "Batch D - the default categorical palette (`theme/default-palette`): 14 colours, the second readable as text and
  thin lines, every pair apart under normal vision and simulated colour vision deficiencies, the lighter six keeping
  their base hues, the gray apart from the NA colour; the single default colour is the palette's first."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.color :as c]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.theme :as theme]))

(def ^:private palette theme/default-palette)

(def ^:private visions
  "Normal vision and the simulated deficiencies the palette was checked under."
  {:normal identity :deuteranopia c/deuteranopia :protanopia c/protanopia :tritanopia c/tritanopia})

(defn- closest-pair
  "Smallest CIEDE2000 difference between two colours of `colors` seen through `vision`."
  [colors vision]
  (v/mn (for [i (range (count colors)) j (range (inc i) (count colors))]
          (c/delta-E*-2000 (vision (nth colors i)) (vision (nth colors j))))))

(defn- hue
  "Oklch hue of a colour, degrees."
  ^double [color]
  (nth (c/to-Oklch color) 2))

(defn- hue-distance
  "Distance between two hues on the circle, degrees."
  ^double [^double a ^double b]
  (let [d (m/abs (m/- a b))] (m/min d (m/- 360.0 d))))

(deftest default-palette-test
  (testing "the theme's palette, 14 colours, the Okabe-Ito blue first"
    (is (= palette (get-in theme/default-theme [:palette :color])))
    (is (= 14 (count palette)))
    (is (= (c/to-color "#0072b2") (first palette))))
  (testing "the second colour reads as text and thin lines on the plot background (WCAG 3:1)"
    (is (<= 3.0 (c/contrast-ratio (second palette) (get-in theme/default-theme [:plot :background])))))
  (testing "every pair apart under normal vision and each deficiency (CIEDE2000; tritanopia the tightest, 3.15)"
    (is (<= 13.0 (closest-pair palette c/to-color)) "normal vision")
    (doseq [[k vision] visions]
      (is (<= 3.0 (closest-pair palette vision)) (name k))))
  (testing "the first six well apart under every vision (at least 8; tritanopia 8.16)"
    (doseq [[k vision] visions]
      (is (<= 8.0 (closest-pair (take 6 palette) vision)) (name k))))
  (testing "colours 10-14 keep the hues of 2-6 (within 15 degrees); colour 9 is the blue turned 30-40 degrees toward violet"
    (doseq [i (range 1 6)]
      (is (<= (hue-distance (hue (nth palette i)) (hue (nth palette (+ i 8)))) 15.0) (str "colour " (+ i 9))))
    (is (<= 30.0 (m/- (hue (nth palette 8)) (hue (first palette))) 40.0)))
  (testing "the gray stays apart from the NA colour (at least 10)"
    (is (<= 10.0 (c/delta-E*-2000 (nth palette 7) (get-in theme/default-theme [:palette :na])))))
  (testing "boundary: an ordinal scale of 15 levels gives level 15 level 1's colour"
    (let [f (aesthetic/palette-color-fn theme/default-theme 15)]
      (is (= (f 0) (f 14)))
      (is (not= (f 0) (f 13))))))

(deftest default-color-test
  (testing "the single default colour is the palette's first, and every single-colour chart style uses it"
    (is (= (first palette) theme/default-color))
    (doseq [[kind style] (:chart theme/default-theme)
            :let [color (:color style)]
            :when (and (some? color) (not (keyword? color)) (m/> (double (nth (c/to-color color) 2)) 100.0)
                       (m/< (double (nth (c/to-color color) 0)) 100.0))]
      (is (= (vec (take 3 theme/default-color)) (vec (take 3 (c/to-color color)))) (name kind)))))
