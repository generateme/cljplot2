(ns cljplot2.correlation-cycle3-test
  "Correlation cycle 3: correlation glyphs, `:axis :grid`, `:legend-only`, equal panels, `plot/pairs`."
  (:require [clojure.test :refer [deftest testing is are]]
            [fastmath.core :as m]
            [fastmath.stats :as stats]
            [cljplot2.test-util :refer [error]]
            [cljplot2.chart :as chart]
            [cljplot2.chart.correlation :as correlation]
            [cljplot2.core :as plot]
            [cljplot2.layout.grid :as grid]
            [cljplot2.scale.aesthetic :as aesthetic])
  (:import [java.awt.image BufferedImage]))

(defn- rgb [^BufferedImage img x y]
  (let [v (.getRGB img (int x) (int y))]
    [(bit-and (bit-shift-right v 16) 0xff) (bit-and (bit-shift-right v 8) 0xff) (bit-and v 0xff)]))

(def ^:private plain {:plot {:background :white} :panel {:fill [250 250 250] :alternate-fill :none :grid {:x :none :y :none}}})

(defn- render [p size]
  (let [p (update p :theme #(merge plain %))
        built (plot/build p size)]
    [built (plot/render-image p size) (grid/item-box (:layout built) :panel)]))

(defn- stats [p] (first (:stats (plot/build p {:width 400 :height 400}))))

(def ^:private cloud
  (let [rng (java.util.Random. 3)]
    (vec (repeatedly 200 (fn [] (let [a (.nextGaussian rng) b (.nextGaussian rng) c (.nextGaussian rng)]
                                  {:a a :b (+ (* 0.6 a) (* 0.8 b)) :c c :d (- a) :g (if (pos? b) "p" "n") :s "x"}))))))

;; 1. chart/correlation

(deftest r-test
  (testing "1.1 r of each method equals fastmath"
    (let [xs (mapv :a cloud) ys (mapv :b cloud)]
      (are [method f] (m/delta-eq (f xs ys) (correlation/r method xs ys))
        :pearson stats/pearson-correlation
        :spearman stats/spearman-correlation
        :kendall stats/kendall-correlation)))
  (testing "1.1 fewer than 2 points or a constant variable: NaN"
    (is (m/nan? (correlation/r :pearson [1.0] [2.0])))
    (is (m/nan? (correlation/r :pearson [] [])))
    (is (m/nan? (correlation/r :spearman [1.0 1.0 1.0] [1.0 2.0 3.0]))))
  (testing "1.1 per panel and per group; invalid rows dropped and counted"
    (let [s (stats {:data (conj cloud {:a nil :b 1} {:a 1 :b ##NaN}) :mapping {:x :a :y :b :color :g} :charts [(chart/correlation)]})
          d (get-in s [:data :panel])]
      (is (m/delta-eq (stats/pearson-correlation (map :a cloud) (map :b cloud)) (:r d)))
      (is (= 2 (count (filter #(some? (get-in % [:aesthetics :color])) (:groups d)))) "two groups and the NA one")
      (is (= {:x 1 :y 1} (:dropped s)))
      (is (= #{:color} (set (keys (get-in s [:extents :panel])))) "group colors only, no x / y extents"))))

(deftest computed-color-test
  (testing "1.3 colored by r unless number, fixed color or grouped"
    (is (= {} (aesthetic/computed-aesthetics (chart/correlation) {:x :a :y :b})))
    (is (= {:color {:title "r" :legend false :range :RColorBrewer/RdBu}} (aesthetic/computed-aesthetics (chart/correlation {:glyph :pie}) {:x :a :y :b})))
    (is (= {} (aesthetic/computed-aesthetics (chart/correlation {:glyph :pie :color :red}) {:x :a :y :b})))
    (is (= {} (aesthetic/computed-aesthetics (chart/correlation {:glyph :pie}) {:x :a :y :b :color :g}))))
  (testing "1.3 the r legend hidden by default, shown on request; domain [-1 1]"
    (let [legends (fn [p] (some #(when (= :legends (:id %)) (get-in % [:content :legends])) (get-in (plot/build p {:width 400 :height 300}) [:grid :items])))
          p {:data cloud :mapping {:x :a :y :b} :charts [(chart/correlation {:glyph :shade})]}]
      (is (empty? (legends p)))
      (is (= [:colorbar] (map :kind (legends (assoc p :scales {:color {:legend true}})))))
      (is (= {:kind :numeric :min -1.0 :max 1.0} (get-in (stats p) [:extents :panel :color]))))))

(defn- glyph-image
  "Image, panel box and r of a correlation glyph of `ys` against x = 0..n-1, filled pure red."
  [glyph ys]
  (let [rows (map-indexed (fn [i y] {:x i :y y}) ys)
        p {:data rows :mapping {:x :x :y :y} :legend false
           :charts [(chart/correlation {:glyph glyph :color [255 0 0] :stroke-color :none})]}
        [built img box] (render p {:width 300 :height 300})]
    [img box (get-in built [:stats 0 :data :panel :r])]))

(defn- at [img {:keys [x y w h]} fx fy]
  (rgb img (+ (long x) (long (* (double fx) (double w)))) (+ (long y) (long (* (double fy) (double h))))))

(def ^:private red [255 0 0])
(def ^:private bg [250 250 250])

(deftest glyph-test
  (let [rising (range 20)
        falling (map - (range 20))
        quarter (concat (range 10) (repeat 10 0) (range 10 0 -1))]
    (testing "1.2.2 shade fills the panel"
      (let [[img box] (glyph-image :shade rising)]
        (is (= red (at img box 0.02 0.02)))
        (is (= red (at img box 0.98 0.98)))))
    (testing "1.2.3 square / 1.2.4 circle: area by |r|; r = 1 nearly fills, r = 0 nothing"
      (let [[img box] (glyph-image :square rising)]
        (is (= red (at img box 0.07 0.5)))
        (is (= bg (at img box 0.03 0.5))))
      (let [[img box] (glyph-image :circle rising)]
        (is (= red (at img box 0.5 0.07)))
        (is (= bg (at img box 0.1 0.1)) "outside the disk, inside its square"))
      (let [[img box r] (glyph-image :square (map #(m/sq (- (double %) 9.5)) (range 20)))]
        (is (< (m/abs (double r)) 0.1))
        (is (= bg (at img box 0.3 0.5)) "|r| small: a small square")))
    (testing "1.2.5 ellipse rises for r > 0, falls for r < 0"
      (let [[img box] (glyph-image :ellipse (map #(+ % (if (even? %) 3 -3)) (range 20)))]
        (is (= red (at img box 0.75 0.25)) "up-right")
        (is (= bg (at img box 0.25 0.25)) "not up-left"))
      (let [[img box] (glyph-image :ellipse (map #(- (+ % (if (even? %) 3 -3))) (range 20)))]
        (is (= red (at img box 0.25 0.25)))
        (is (= bg (at img box 0.75 0.25)))))
    (testing "1.2.6 pie: clockwise from the top for r > 0, counter-clockwise for r < 0"
      (let [[img box r] (glyph-image :pie quarter)]
        (is (< 0.0 (double r) 0.75) (str r))
        (is (= red (at img box 0.6 0.15)) "just right of the top")
        (is (= bg (at img box 0.4 0.15)) "just left of the top"))
      (let [[img box] (glyph-image :pie (map - quarter))]
        (is (= red (at img box 0.4 0.15)))
        (is (= bg (at img box 0.6 0.15)))))
    (testing "1.2.1 number: NA for a constant variable, renders grouped"
      (is (m/nan? (get-in (stats {:data (map #(assoc % :s 1) cloud) :mapping {:x :a :y :s} :charts [(chart/correlation)]}) [:data :panel :r])))
      (is (instance? BufferedImage (plot/render-image {:data cloud :mapping {:x :a :y :b :color :g} :charts [(chart/correlation)]}
                                                      {:width 300 :height 300}))))
    (testing "1.5 theme validated"
      (is (some? (error #(plot/build {:data cloud :mapping {:x :a :y :b} :charts [(chart/correlation)] :theme {:chart {:correlation {:bogus 1}}}}
                                     {:width 200 :height 200}))))
      (is (some? (error #(stats {:data cloud :mapping {:x :a :y :b} :charts [(chart/correlation {:glyph :star})]})))))))

;; 2.1 :axis :grid

(deftest axis-grid-test
  (testing "2.1 grid lines kept, no axis or tick labels"
    (let [p {:data cloud :mapping {:x :a :y :b} :scales {:x {:axis :grid}} :charts [(chart/scatter)] :legend false
             :theme {:panel {:grid {:x {:color [255 0 0] :width 1}}}}}
          b (plot/build p {:width 300 :height 300})
          img (plot/render-image p {:width 300 :height 300})
          {:keys [x y h]} (grid/item-box (:layout b) :panel)
          tick (first (get-in b [:finalized-scales :x :ticks]))
          px (long (Math/round (+ (double x) (double ((cljplot2.scale.position/forward-fn (get-in b [:finalized-scales :x])) tick)))))]
      (is (= :grid (get-in b [:finalized-scales :x :axis])))
      (is (nil? (grid/item-box (:layout b) :x-axis)) "no x axis item")
      (is (some (fn [[r g]] (and (> (long r) 230) (< (long g) 160))) (for [dx [-1 0 1]] (rgb img (+ px dx) (+ (long y) (quot (long h) 2)))))
          "a (red, antialiased) grid line")))
  (testing "2.1 schema"
    (is (some? (error #(stats {:data cloud :mapping {:x :a :y :b} :scales {:x {:axis :bogus}} :charts [(chart/scatter)]}))))))

;; 2.2 :legend-only, :measured, :equal-panels

(deftest legend-only-test
  (let [lo {:data cloud :mapping {:x :a :y :b :color :g} :legend-only true :charts [(chart/scatter)]}]
    (testing "2.2 only the legends: no panel, no axes"
      (let [b (plot/build lo {:width 200 :height 300})]
        (is (grid/item-box (:layout b) :legends))
        (is (nil? (grid/item-box (:layout b) :panel)))
        (is (instance? BufferedImage (plot/render-image lo {:width 200 :height 300})))))
    (testing "2.2 not with :arrange / :custom-grid"
      (is (= :legend-only (:key (error #(plot/build (assoc (plot/arrange [lo]) :legend-only true) {:width 300 :height 300}))))))
    (testing "2.2 :measured sizes a column of legend-only plots to the legend; other plots can't be measured"
      (let [b (plot/build (plot/arrange [{:data cloud :mapping {:x :a :y :b} :charts [(chart/scatter)]} lo] {:columns 2 :widths [1 :measured]})
                          {:width 600 :height 300})
            legend-w (:w (grid/find-item-box (:layout (get-in b [:subplots [:plot 1]])) :legends))
            cell-w (:w (grid/item-box (:layout b) [:plot 1]))]
        (is (<= (double legend-w) (double cell-w) (+ (double legend-w) 30.0))))
      (is (some? (error #(plot/build (plot/arrange [lo {:data cloud :mapping {:x :a :y :b} :charts [(chart/scatter)]}] {:columns 2 :widths [1 :measured]})
                                     {:width 600 :height 300})))))))

(defn- panel-boxes
  "Absolute panel boxes of the sub-plots of a built arrange plot."
  [b]
  (keep (fn [[id s]] (when-let [p (grid/item-box (:layout s) :panel)]
                       (let [cell (grid/item-box (:layout b) id)] (assoc p :x (+ (long (:x cell)) (long (:x p))) :y (+ (long (:y cell)) (long (:y p)))))))
        (:subplots b)))

(deftest equal-panels-test
  (testing "2.x :equal-panels - every panel one size although outer cells carry axes"
    (let [b (plot/build (plot/pairs cloud {:variables [:a :b :c]}) {:width 600 :height 600})
          boxes (panel-boxes b)]
      (is (= 9 (count boxes)))
      (is (<= (- (long (reduce max (map :w boxes))) (long (reduce min (map :w boxes)))) 1))
      (is (<= (- (long (reduce max (map :h boxes))) (long (reduce min (map :h boxes)))) 1)))))

;; 3. plot/pairs

(deftest pairs-structure-test
  (let [{:keys [arrange]} (plot/pairs cloud {:variables [:a :b :c]})
        cell (fn [i j] (get-in arrange [:plots (+ (* 3 i) j)]))]
    (testing "3.2 cells: x - column variable, y - row variable; default charts by slot"
      (is (= 9 (count (:plots arrange))))
      (is (= {:x :b :y :c} (select-keys (:mapping (cell 2 1)) [:x :y])))
      (is (= [:scatter] (map :kind (:charts (cell 2 0)))))
      (is (= [:correlation] (map :kind (:charts (cell 0 2)))))
      (is (= [:density] (map :kind (:charts (cell 1 1)))))
      (is (nil? (:y (:mapping (cell 1 1)))) "the diagonal maps x only"))
    (testing "3.3 / 3.4 shared domains, outer axes, outer titles"
      (is (= (get-in (cell 0 1) [:scales :x :domain]) (get-in (cell 2 1) [:scales :x :domain])))
      (is (= (get-in (cell 2 0) [:scales :y :domain]) (get-in (cell 2 1) [:scales :y :domain])))
      (is (= [true :grid] [(get-in (cell 2 1) [:scales :x :axis]) (get-in (cell 1 1) [:scales :x :axis])]))
      (is (= [true :grid] [(get-in (cell 1 0) [:scales :y :axis]) (get-in (cell 1 2) [:scales :y :axis])]))
      (is (= {:x "b" :y nil} (:labels (cell 2 1))))
      (is (= {:x nil :y "a"} (:labels (cell 0 0)))))
    (testing "3.5 no grouping: no legend column")
    (is (= 3 (:columns arrange))))
  (testing "3.5 grouping: a legend-only column sized to the legend"
    (let [{:keys [arrange]} (plot/pairs cloud {:variables [:a :b] :mapping {:color :g}})]
      (is (= 6 (count (:plots arrange))))
      (is (= [1 1 :measured] (:widths arrange)))
      (is (every? :legend-only [(get-in arrange [:plots 2]) (get-in arrange [:plots 5])]))
      (is (seq (:charts (get-in arrange [:plots 2]))) "the first legend cell carries the charts")))
  (testing "3.2 slots: nil - empty, vectors, :names"
    (let [{:keys [arrange]} (plot/pairs cloud {:variables [:a :b] :upper nil :lower [(chart/scatter) (chart/smooth)] :diagonal :names
                                               :labels {:a "Alpha"}})]
      (is (empty? (:charts (get-in arrange [:plots 1]))))
      (is (= [:scatter :smooth] (map :kind (:charts (get-in arrange [:plots 2])))))
      (is (= "Alpha" (get-in arrange [:plots 0 :charts 0 :options :text])))))
  (testing "3.6 validation"
    (is (= :variables (:key (error #(plot/pairs cloud {:variables [:a]})))))
    (is (= :variables (:key (error #(plot/pairs cloud {:variables (vec (repeat 21 :a))})))))
    (is (= :variables (:key (error #(plot/pairs cloud {:variables [:a :s]})))) "non-numeric")
    (is (= :variables (:key (error #(plot/pairs cloud {:variables [:a :missing]})))) "missing")
    (is (some? (plot/pairs cloud {:variables (vec (take 20 (cycle [:a :b :c :d])))})) "20 variables")))

(deftest pairs-render-test
  (testing "3.3 a variable's finalized domain is the same in every cell"
    (let [b (plot/build (plot/pairs cloud {:variables [:a :b :c]}) {:width 600 :height 600})
          domain (fn [k id] (get-in b [:subplots id :finalized-scales k :domain]))]
      (is (= (domain :x [:plot 1]) (domain :x [:plot 4]) (domain :x [:plot 7])))
      (is (= (domain :y [:plot 3]) (domain :y [:plot 5])))))
  (testing "3.7 6 variables x 150 rows within 3 s (after warm-up)"
    (let [p (plot/pairs (take 150 cloud) {:variables [:a :b :c :d :a :b] :mapping {:color :g}})]
      (plot/render-image p {:width 900 :height 800})
      (let [t (System/nanoTime)]
        (plot/render-image p {:width 900 :height 800})
        (is (< (/ (- (System/nanoTime) t) 1.0e6) 3000.0))))))
