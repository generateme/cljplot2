(ns cljplot2.batch-c2-s7-test
  "Batch C2 (s7): `:stat-space` - histogram, density and smooth statistics in the data's units or in the position
  scale's stat coordinates (log10 on a `:log` axis)."
  (:require [clojure.test :refer [deftest testing is]]
            [fastmath.core :as m]
            [cljplot2.test-util :refer [error error-ins]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.facet :as facet])
  (:import [java.awt.image BufferedImage]))

(def ^:private options {:width 400 :height 300})

(defn- built [p] (plot/build p options))

(def ^:private decades
  "Values 10^(i/4), i 0..15 (four per decade over 1..~5600): even on a log axis."
  (mapv (fn [^long i] {:v (m/pow 10.0 (/ i 4.0)) :g (if (even? i) "a" "b")}) (range 16)))

(def ^:private with-zeros
  "The decades plus a 0 and a negative value (not placeable on a log axis)."
  (into decades [{:v 0.0 :g "a"} {:v -3.0 :g "b"}]))

(defn- stats [p] (get-in (built p) [:stats 0]))

(defn- bars [p] (->> (stats p) :data vals (mapcat :bars)))

(defn- hist-plot [data chart-options & {:as plot-options}]
  (merge {:data data :mapping {:x :v} :scales {:x {:kind :log}} :charts [(chart/histogram chart-options)]} plot-options))

(defn- close? [^double a ^double b] (m/< (m/abs (m/- a b)) 1e-9))

(deftest histogram-stat-space-test
  (testing "default :data and nil: bins in data units (equal width there)"
    (let [default (bars (hist-plot decades {:bins 4}))
          widths (map (fn [{:keys [^double x0 ^double x1]}] (m/- x1 x0)) default)]
      (is (= default (bars (hist-plot decades {:bins 4 :stat-space nil}))))
      (is (= default (bars (hist-plot decades {:bins 4 :stat-space :data}))))
      (is (apply close? (take 2 widths)))))
  (testing ":scale on a log axis: equal-width log10 bins, four values each"
    (let [bs (bars (hist-plot decades {:bins 4 :stat-space :scale :y :count}))
          log-widths (map (fn [{:keys [^double x0 ^double x1]}] (m/- (m/log10 x1) (m/log10 x0))) bs)]
      (is (= 4 (count bs)))
      (is (every? #(close? (first log-widths) %) log-widths))
      (is (= [4.0 4.0 4.0 4.0] (mapv :y1 bs)))
      (is (close? 1.0 (:x0 (first bs))))))
  (testing ":scale :density - area 1 in log10 units"
    (let [bs (bars (hist-plot decades {:bins 4 :stat-space :scale :y :density}))]
      (is (close? 1.0 (reduce + (map (fn [{:keys [^double x0 ^double x1 ^double y1]}] (* y1 (- (m/log10 x1) (m/log10 x0)))) bs))))))
  (testing "zeros and negatives: dropped and counted"
    (let [s (stats (hist-plot with-zeros {:bins 4 :stat-space :scale :y :count}))]
      (is (= 2 (get-in s [:dropped :x])))
      (is (= 16.0 (reduce + (map :y1 (mapcat :bars (vals (:data s)))))))))
  (testing "explicit breaks stay in data units; one not placeable throws {:key :bins}"
    (is (= [1.0 10.0 100.0 10000.0] (vec (distinct (mapcat (juxt :x0 :x1) (bars (hist-plot decades {:bins [1 10 100 10000] :stat-space :scale})))))))
    (is (= :bins (:key (error #(built (hist-plot decades {:bins [0 10 100] :stat-space :scale})))))))
  (testing "one value and every value unplaceable"
    (is (= 1 (count (bars (hist-plot [{:v 100}] {:stat-space :scale})))))
    (is (= :mapping (:key (error #(built (hist-plot [{:v 0} {:v -1}] {:stat-space :scale})))))))
  (testing "a linear axis: no change"
    (is (= (bars (hist-plot decades {:bins 5} :scales {}))
           (bars (hist-plot decades {:bins 5 :stat-space :scale} :scales {})))))
  (testing "dodge, facets, flip, ridgelines"
    (let [dodged (bars (hist-plot decades {:bins 4 :stat-space :scale :position :dodge} :mapping {:x :v :color :g}))]
      (is (= 8 (count dodged))))
    (let [s (stats (hist-plot with-zeros {:bins 4 :stat-space :scale} :mapping {:x :v :facet :g}))]
      (is (= 2 (get-in s [:dropped :x])))
      (is (= 2 (count (remove #{facet/all-panels} (keys (:data s)))))))
    (is (= (bars (hist-plot decades {:bins 4 :stat-space :scale}))
           (bars (hist-plot decades {:bins 4 :stat-space :scale} :flip true))))
    (let [s (stats (hist-plot with-zeros {:bins 4 :stat-space :scale} :mapping {:x :v :y :g}))]
      (is (:ridges s))
      (is (= 2 (get-in s [:dropped :x])))
      (is (every? (fn [[a b _]] (and (pos? a) (> b a))) (mapcat :bars (mapcat :ridges (vals (:data s))))))))
  (testing "wrong value -> error at :stat-space"
    (is (contains? (error-ins #(built (hist-plot decades {:stat-space :log}))) [:charts 0 :options :stat-space])))
  (testing "renders"
    (is (instance? BufferedImage (plot/render-image (hist-plot with-zeros {:stat-space :scale}) options)))))

(defn- curves [p] (->> (stats p) :data vals (mapcat :curves)))

(defn- density-plot [data chart-options & {:as plot-options}]
  (merge {:data data :mapping {:x :v} :scales {:x {:kind :log}} :charts [(chart/density chart-options)]} plot-options))

(defn- peak-x
  "x of the highest density of the first curve."
  [p]
  (let [{:keys [x y]} (first (curves p))] (nth x (first (apply max-key second (map-indexed vector y))))))

(deftest density-stat-space-test
  (testing "default: in data units (nil the same) - its tails past 0 don't fit a log axis"
    (is (= (curves (density-plot decades {} :scales {})) (curves (density-plot decades {:stat-space nil} :scales {}))))
    (is (some #(not (pos? %)) (:x (first (curves (density-plot decades {} :scales {}))))))
    (is (= :domain (:key (error #(built (density-plot decades {})))))))
  (testing ":scale: kde of log10 values; the curve in data units, all positive, peaked in the log middle"
    (let [cs (curves (density-plot decades {:stat-space :scale}))
          {:keys [x y]} (first cs)]
      (is (every? pos? x))
      (is (< 10 (peak-x (density-plot decades {:stat-space :scale})) 1000))
      (is (close? 1.0 (m/round (reduce + (map (fn [[^double a ^double b] ^double d] (* d (- (m/log10 b) (m/log10 a)))) (partition 2 1 x) y)))))))
  (testing "a numeric bandwidth in log10 units"
    (let [{:keys [x]} (first (curves (density-plot decades {:stat-space :scale :bandwidth 0.1 :cut 1})))]
      (is (close? (m/pow 10.0 -0.1) (first x)))))
  (testing "zeros and negatives dropped and counted; all unplaceable -> mapping error"
    (is (= 2 (get-in (stats (density-plot with-zeros {:stat-space :scale})) [:dropped :x])))
    (is (= :mapping (:key (error #(built (density-plot [{:v 0} {:v -1}] {:stat-space :scale})))))))
  (testing "one value: no curve, counted as a group"
    (is (= 1 (get-in (stats (density-plot [{:v 5}] {:stat-space :scale} :scales {:x {:kind :log :domain [1 10]}})) [:dropped :group]))))
  (testing "a linear axis: no change"
    (is (= (curves (density-plot decades {} :scales {})) (curves (density-plot decades {:stat-space :scale} :scales {})))))
  (testing "stack, facets, flip, ridgelines"
    (is (= 2 (count (curves (density-plot decades {:stat-space :scale :position :stack} :mapping {:x :v :color :g})))))
    (is (= 2 (get-in (stats (density-plot with-zeros {:stat-space :scale} :mapping {:x :v :facet :g})) [:dropped :x])))
    (is (= (curves (density-plot decades {:stat-space :scale})) (curves (density-plot decades {:stat-space :scale} :flip true))))
    (let [s (stats (density-plot with-zeros {:stat-space :scale} :mapping {:x :v :y :g}))]
      (is (:ridges s))
      (is (= 2 (get-in s [:dropped :x])))
      (is (every? pos? (mapcat :xs (mapcat :ridges (vals (:data s))))))))
  (testing "wrong value -> error at :stat-space"
    (is (contains? (error-ins #(built (density-plot decades {:stat-space "scale"}))) [:charts 0 :options :stat-space])))
  (testing "renders"
    (is (instance? BufferedImage (plot/render-image (density-plot with-zeros {:stat-space :scale :fill :color}) options)))))

(def ^:private power
  "y = 2^x, x 1..10 (a straight line on a log y axis), and a row with y 0."
  (conj (mapv (fn [^long i] {:x i :y (m/pow 2.0 i)}) (range 1 11)) {:x 11 :y 0.0}))

(defn- smooth-plot [chart-options]
  {:data power :mapping {:x :x :y :y} :scales {:y {:kind :log}} :charts [(chart/smooth (merge {:method :lm :band :none} chart-options))]})

(deftest smooth-stat-space-test
  (testing "default :scale (and nil): the fit in log10 y - exact for 2^x; the 0 left out and counted"
    (let [s (stats (smooth-plot {}))
          {:keys [x y]} (first (curves (smooth-plot {})))]
      (is (= (curves (smooth-plot {})) (curves (smooth-plot {:stat-space nil}))))
      (is (= (curves (smooth-plot {})) (curves (smooth-plot {:stat-space :scale}))))
      (is (every? #(close? 0.0 %) (map (fn [^double xv ^double yv] (- (m/log2 yv) xv)) x y)))
      (is (= 1 (get-in s [:dropped :y])))))
  (testing ":data: the fit in data units - a log x axis shows y = log10 x fitted straight there, but not by :data"
    (let [p {:data (mapv (fn [^long i] {:x (m/pow 10.0 i) :y i}) (range 4)) :mapping {:x :x :y :y} :scales {:x {:kind :log}}}
          fitted (fn [o] (first (curves (assoc p :charts [(chart/smooth (merge {:method :lm :band :none} o))]))))
          on-log (fn [{:keys [x y]}] (every? true? (map (fn [^double xv ^double yv] (close? (m/log10 xv) yv)) x y)))]
      (is (on-log (fitted {})))
      (is (not (on-log (fitted {:stat-space :data}))))
      (is (close? 1.0 (first (:x (fitted {:stat-space :data})))))))
  (testing "linear axes: :data and :scale the same"
    (let [p (dissoc (smooth-plot {}) :scales)]
      (is (= (curves p) (curves (assoc p :charts [(chart/smooth {:method :lm :band :none :stat-space :data})]))))))
  (testing "a log x axis: unplaceable x counted under :x"
    (is (= 1 (get-in (stats {:data [{:x 0 :y 1} {:x 1 :y 2} {:x 10 :y 3} {:x 100 :y 4}] :mapping {:x :x :y :y}
                             :scales {:x {:kind :log}} :charts [(chart/smooth {:method :lm})]})
                     [:dropped :x]))))
  (testing "wrong value -> error"
    (is (contains? (error-ins #(built (smooth-plot {:stat-space true}))) [:charts 0 :options :stat-space]))))
