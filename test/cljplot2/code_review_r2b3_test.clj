(ns cljplot2.code-review-r2b3-test
  "Code review round 2, batch B3 - shared helpers: finite pixel points, alpha, legend key glyphs, the largest size, the
  mapping check, and strip charts computed and drawn through the strip multimethods."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [fastmath.vector :as v]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.rows :as rows]
            [cljplot2.chart.strip :as strip-chart]
            [cljplot2.data.strip :as strip]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.graphics.shape :as shape])
  (:import [java.awt BasicStroke Graphics2D]
           [java.awt.image BufferedImage]))

(deftest finite-point-test
  (testing "Vec2 and [x y] with finite coordinates"
    (is (rows/finite-point? (v/vec2 1.0 -2.0)))
    (is (rows/finite-point? [0 0.5]))
    (is (rows/finite-point? (v/vec2 0.0 0.0))))
  (testing "boundary: NaN, infinite, non-number, nil, short vector"
    (is (not (rows/finite-point? (v/vec2 ##NaN 1.0))))
    (is (not (rows/finite-point? (v/vec2 1.0 ##Inf))))
    (is (not (rows/finite-point? [##-Inf 0])))
    (is (not (rows/finite-point? [1 nil])))
    (is (not (rows/finite-point? ["a" 1])))
    (is (not (rows/finite-point? [1])))
    (is (not (rows/finite-point? [])))
    (is (not (rows/finite-point? nil)))))

(defn- alpha [col] (c/alpha col))

(deftest with-alpha-test
  (testing "alpha set to a fraction of 255"
    (is (== 255.0 (alpha (draw/with-alpha :red 1.0))))
    (is (== 127.5 (alpha (draw/with-alpha (c/color 10 20 30 40) 0.5))))
    (is (== 0.0 (alpha (draw/with-alpha :red 0)))))
  (testing "boundary: nil alpha and :none unchanged"
    (is (= :red (draw/with-alpha :red nil)))
    (is (= :none (draw/with-alpha :none 0.5)))
    (is (= :none (draw/with-alpha :none nil)))))

(deftest faded-test
  (testing "the colour's own alpha scaled"
    (is (== 50.0 (alpha (draw/faded (c/color 10 20 30 100) 0.5))))
    (is (== 255.0 (alpha (draw/faded :red 1.0))))
    (is (== 0.0 (alpha (draw/faded :red 0.0)))))
  (testing "colour channels kept"
    (is (= (take 3 (c/to-color (c/color 10 20 30 100))) (take 3 (c/to-color (draw/faded (c/color 10 20 30 100) 0.3)))))))

(defn- glyph-pixels
  "ARGB pixels of a 40 x 40 white canvas after `(f canvas)`."
  [f]
  (let [cv (c2d/canvas 40 40)]
    (c2d/with-canvas [cn cv]
      (c2d/set-background cn :white)
      (f cn))
    (let [^BufferedImage img (c2d/get-image cv)]
      (fn [x y] (bit-and 0xffffff (.getRGB img (int x) (int y)))))))

(deftest key-glyph-test
  (is (== 0.7 draw/key-glyph-share))
  (testing "circle: filled at the centre, empty in the corner; empty box draws nothing"
    (let [px (glyph-pixels #(draw/circle-key! % {:x 0 :y 0 :w 40 :h 40} :black nil))]
      (is (zero? (px 20 20)))
      (is (= 0xffffff (px 1 1))))
    (let [px (glyph-pixels #(draw/circle-key! % {:x 0 :y 0 :w 0 :h 40} :black nil))]
      (is (= 0xffffff (px 20 20)))))
  (testing "spike: a vertical line over its span only"
    (let [px (glyph-pixels #(draw/spike-key! % {:x 0 :y 0 :w 40 :h 40} {:color :black :line-width 2.0 :span [0.25 0.75]}))]
      (is (zero? (px 20 20)))
      (is (= 0xffffff (px 20 5)))
      (is (= 0xffffff (px 20 35)))
      (is (= 0xffffff (px 5 20)))))
  (testing "styled shape key: sets the stroke only when asked"
    (c2d/with-canvas [cn (c2d/canvas 40 40)]
      (let [width #(.getLineWidth ^BasicStroke (.getStroke ^Graphics2D (:graphics cn)))
            st {:shape \o :size 10.0 :color :black :stroke-color :black :stroke-width 3.0}]
        (c2d/set-stroke-custom cn {:size 1.5})
        (shape/styled-key! cn {:x 0 :y 0 :w 40 :h 40} st false)
        (is (== 1.5 (width)))
        (shape/styled-key! cn {:x 0 :y 0 :w 40 :h 40} st true)
        (is (== 3.0 (width)))))))

(deftest max-size-test
  (testing "the largest width and height taken separately"
    (is (= (v/vec2 5.0 7.0) (measure/max-size [(v/vec2 5.0 1.0) (v/vec2 2.0 7.0)])))
    (is (= (v/vec2 3.0 4.0) (measure/max-size [(v/vec2 3.0 4.0)]))))
  (testing "boundary: no sizes - zero; a lazy sequence"
    (is (= (v/vec2 0.0 0.0) (measure/max-size [])))
    (is (= (v/vec2 0.0 0.0) (measure/max-size nil)))
    (is (= (v/vec2 9.0 9.0) (measure/max-size (map #(v/vec2 % %) (range 10)))))))

(deftest check-mapped-test
  (let [rows [{:a 1 :b nil} {:a ##NaN :b "x"}]
        acc [[:a :a] [:b :b]]]
    (testing "a valid value per dimension passes; empty rows pass"
      (is (nil? (rows/check-mapped! rows acc)))
      (is (nil? (rows/check-mapped! [] [[:a :a]])))
      (is (nil? (rows/check-mapped! nil [[:a :a]]))))
    (testing "the first dimension without a valid value throws with its key and the row count"
      (let [e (try (rows/check-mapped! rows [[:b :b] [:c :c] [:d :d]]) (catch clojure.lang.ExceptionInfo e e))]
        (is (re-find #"No valid values for a mapped dimension" (ex-message e)))
        (is (= {:key :mapping :in [:mapping :c] :rows 2} (select-keys (ex-data e) [:key :in :rows]))))
      (is (thrown? clojure.lang.ExceptionInfo (rows/check-mapped! [{:a nil}] [[:a :a]]))))
    (testing "a custom predicate of the dimension and the value"
      (is (nil? (rows/check-mapped! rows acc (fn [k x] (if (= k :b) (string? x) (number? x))))))
      (is (thrown? clojure.lang.ExceptionInfo (rows/check-mapped! rows acc (fn [k _] (not= k :b))))))
    (testing "charts report it the same way"
      (let [e (try (plot/build {:data [{:x 1 :y ##NaN} {:x 2 :y nil}] :mapping {:x :x :y :y} :charts [(chart/line)]})
                   (catch clojure.lang.ExceptionInfo e e))]
        (is (= {:key :mapping :in [:mapping :y] :rows 2} (select-keys (ex-data e) [:key :in :rows])))))))

(def ^:private strip-kinds [:bar :beeswarm :box :bullet :dotplot :extent :rug :stripplot :violin])

(deftest strip-multimethods-test
  (testing "every strip chart computes and draws through the strip functions and has both methods"
    (doseq [[k ch] (map vector strip-kinds [(chart/bar) (chart/beeswarm) (chart/box) (chart/bullet) (chart/dotplot)
                                            (chart/extent) (chart/rug) (chart/stripplot) (chart/violin)])]
      (is (= k (:kind ch)))
      (is (= strip-chart/compute (:compute ch)))
      (is (= strip-chart/draw (:draw ch)))
      (is (contains? (methods strip-chart/in-frame-compute) k))
      (is (contains? (methods strip-chart/in-frame-draw) k))))
  (testing "a kind without methods is an error"
    (is (thrown? IllegalArgumentException (strip-chart/in-frame-compute {:kind :nope} {} {}))))
  (testing "both orientations render"
    (let [data (for [l ["a" "b"] i (range 5)] {:l l :v (double i)})]
      (doseq [mapping [{:x :l :y :v} {:x :v :y :l}]
              ch [(chart/box) (chart/violin) (chart/rug)]]
        (is (some? (plot/render-image {:data data :mapping mapping :charts [ch]} {:width 200 :height 150})))))))
