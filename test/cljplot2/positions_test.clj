(ns cljplot2.positions-test
  "Histogram and density group positions (`cljplot2.data.position`)."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.core :as c2d]
            [cljplot2.theme :as theme]
            [cljplot2.layout.grid :as grid]
            [cljplot2.test-util :refer [error error-ins]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.facet :as facet]))

(def ^:private groups3
  (vec (for [i (range 300)] {:v (+ (mod (* i 7) 13) (* 3 (mod i 3))) :g (["a" "b" "c"] (mod i 3))})))

(def ^:private groups2 (vec (filter #(#{"a" "b"} (:g %)) groups3)))

(defn- built [data c] (plot/build {:data data :mapping {:x :v :color :g} :charts [c]} {:width 400 :height 300}))
(defn- panel-data [b] (get-in b [:stats 0 :data facet/single-panel]))

(deftest histogram-positions-test
  (testing ":stack - bars of a bin are contiguous, the top is the bin total"
    (let [b (built groups3 (chart/histogram {:position :stack :y :count :bins 6}))
          bars (:bars (panel-data b))
          by-bin (group-by :x0 bars)
          total (fn [bin-bars] (reduce + (map #(- (double (:y1 %)) (double (:y0 %))) bin-bars)))]
      (doseq [[_ bin-bars] by-bin
              :let [sorted (sort-by :y0 bin-bars)]]
        (is (zero? (double (:y0 (first sorted)))))
        (is (every? (fn [[a z]] (== (double (:y1 a)) (double (:y0 z)))) (partition 2 1 sorted)))
        (is (== (total bin-bars) (double (:y1 (last sorted))))))
      (is (= 300.0 (reduce + (map total (vals by-bin)))) "every value counted once")
      (is (== (reduce max (map :y1 bars)) (double (get-in b [:extents :y :extent :max]))))
      (is (not (:translucent? (panel-data b))))))
  (testing ":normalize - every non-empty bin tops at 1; y title Proportion"
    (let [b (built groups3 (chart/histogram {:position :normalize :bins 6}))
          tops (map (fn [[_ bs]] (reduce max (map :y1 bs))) (group-by :x0 (:bars (panel-data b))))]
      (is (every? #(< (Math/abs (- 1.0 (double %))) 1e-12) tops))
      (is (= "Proportion" (get-in b [:stats 0 :axis-titles :y])))))
  (testing "new positions are accepted by the schema, unknown ones are not"
    (is (= #{[:charts 0 :options :position]} (error-ins #(built groups3 (chart/histogram {:position :spiral})))))))

(deftest density-positions-test
  (testing ":overlay is the default: no :y0, each group on its own range"
    (let [curves (:curves (panel-data (built groups3 (chart/density))))]
      (is (= 3 (count curves)))
      (is (not-any? :y0 curves))
      (is (apply distinct? (map (comp first :x) curves)))))
  (testing ":stack - one common grid; :y0 of a group is the :y of the previous one"
    (let [curves (:curves (panel-data (built groups3 (chart/density {:position :stack :n 64}))))]
      (is (apply = (map :x curves)))
      (is (= 64 (count (:x (first curves)))))
      (is (every? zero? (:y0 (first curves))))
      (doseq [[a z] (partition 2 1 curves)] (is (= (:y a) (:y0 z))))))
  (testing ":normalize - the last curve tops at 1 where any density is positive; y title Proportion"
    (let [b (built groups3 (chart/density {:position :normalize :n 64}))
          last-curve (last (:curves (panel-data b)))]
      (is (every? #(< (Math/abs (- 1.0 (double %))) 1e-9) (:y last-curve)))
      (is (= "Proportion" (get-in b [:stats 0 :axis-titles :y])))))
  (testing ":mirror - the second group is non-positive, magnitude tick labels, :symmetric range"
    (let [b (built groups2 (chart/density {:position :mirror}))
          [up down] (:curves (panel-data b))]
      (is (every? #(>= (double %) 0.0) (:y up)))
      (is (every? #(<= (double %) 0.0) (:y down)))
      (is (= :absolute (get-in b [:stats 0 :tick-format :y]))))
    (let [b (built groups2 (chart/density {:position :mirror :symmetric true}))
          {:keys [min max]} (get-in b [:extents :y :extent])]
      (is (== (- (double min)) (double max)))))
  (testing "mirror needs two groups; :dodge is not a density position"
    (is (= :position (:key (error #(built groups3 (chart/density {:position :mirror}))))))
    (is (= #{[:charts 0 :options :position]} (error-ins #(built groups3 (chart/density {:position :dodge})))))))

(defn- black-pixels-on-zero
  "Count of black pixels on the panel row of y = 0 of a mirrored density of `groups2` with chart `options`."
  [options]
  (let [p {:data groups2 :mapping {:x :v :color :g} :charts [(chart/density (merge {:position :mirror} options))]}
        opts {:width 400 :height 300}
        b (plot/build p opts)
        {:keys [x y w]} (grid/find-content-box (:grid b) (:layout b) #(= :panel (:kind %)))
        row (long (+ (double y) (Math/floor (double ((cljplot2.scale.position/forward-fn (get-in b [:finalized-scales :y])) 0.0)))))
        ^java.awt.image.BufferedImage img (plot/render-image p opts)]
    (count (filter #(= -16777216 (.getRGB img (int %) (int row))) (range (long x) (+ (long x) (long w)))))))

(deftest density-zero-line-test
  (testing "mirror draws the y = 0 line by default, `:zero-line false` turns it off"
    (is (> (black-pixels-on-zero {}) 200))
    (is (< (black-pixels-on-zero {:zero-line false}) 20)))
  (testing "style from the theme or options; a :none colour draws nothing"
    (is (< (black-pixels-on-zero {:zero-line-color :none}) 20)))
  (testing "no zero line without mirror"
    (is (false? (:zero-line? (panel-data (built groups3 (chart/density {:position :stack})))))))
  (testing "schema"
    (is (= #{[:charts 0 :options :zero-line]} (error-ins #(built groups2 (chart/density {:position :mirror :zero-line 1})))))))

(defn- glyph-pixel
  "ARGB of pixel (10, 4) of the legend glyph of `c` (colour red) drawn in a 20x20 box."
  [c]
  (let [canvas (c2d/canvas 20 20)]
    (c2d/with-canvas-> canvas
      ((fn [cv] ((:legend c) c {:color :red} cv {:x 0 :y 0 :w 20 :h 20} {:theme theme/default-theme}) cv)))
    (.getRGB ^java.awt.image.BufferedImage (c2d/get-image canvas) 10 4)))

(defn- alpha ^long [^long argb] (bit-and (bit-shift-right argb 24) 0xff))

(deftest density-legend-glyph-test
  (testing "line glyph without areas: the pixel above the middle line is empty"
    (is (zero? (alpha (glyph-pixel (chart/density))))))
  (testing "filled glyph for stacked and normalized areas and for a :fill colour"
    (doseq [c [(chart/density {:position :stack}) (chart/density {:position :normalize}) (chart/density {:fill :blue})]]
      (is (pos? (alpha (glyph-pixel c))) (pr-str (:options c))))))

(deftest density-alpha-test
  (testing "stacked areas use the style :alpha (theme or option) for the group colour"
    (let [area-alpha (fn [c] (let [canvas (c2d/canvas 20 20)]
                               (c2d/with-canvas-> canvas
                                 ((fn [cv] ((:legend c) c {:color :red} cv {:x 0 :y 0 :w 20 :h 20} {:theme theme/default-theme}) cv)))
                               (alpha (.getRGB ^java.awt.image.BufferedImage (c2d/get-image canvas) 10 10))))]
      (is (= 153 (area-alpha (chart/density {:position :stack}))) "default 0.6")
      (is (= 255 (area-alpha (chart/density {:position :stack :alpha 1.0}))))
      (is (= #{[:charts 0 :options :alpha]} (error-ins #(built groups3 (chart/density {:position :stack :alpha 2})))))))
  (testing ":fill is no longer a position"
    (is (= #{[:charts 0 :options :position]} (error-ins #(built groups3 (chart/density {:position :fill})))))
    (is (= #{[:charts 0 :options :position]} (error-ins #(built groups3 (chart/histogram {:position :fill})))))))
