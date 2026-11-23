(ns cljplot2.code-review-r2b3-theme-test
  "Code review round 2 B3 - one-point densities, colour bar and 3D frame theme keys."
  (:require [clojure.test :refer [deftest testing is]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.field :as field]
            [cljplot2.data.kde :as kde]
            [cljplot2.scale.position :as scale]
            [cljplot2.test-util :refer [error error-data same-pixels?]]))

(def ^:private size {:width 300 :height 240})

(def ^:private one-point [{:x 1.0 :y 2.0}])

(defn- chart-stats [p] (first (:stats (plot/build p))))

(deftest one-point-density-test
  (let [identity-transforms [(scale/stat-transform nil) (scale/stat-transform nil)]]
    (testing "a selector needs 2 points; numeric bandwidths estimate one"
      (is (nil? (field/density [1.0] [2.0] {} identity-transforms)))
      (is (nil? (field/density [1.0] [2.0] {:bandwidth :scott} identity-transforms)))
      (is (nil? (field/density [1.0] [2.0] {:bandwidth [1.0 :nrd]} identity-transforms)) "a selector on one axis")
      (is (fn? (:f (field/density [1.0] [2.0] {:bandwidth 1.0} identity-transforms))))
      (is (fn? (:f (field/density [1.0] [2.0] {:bandwidth [1.0 2.0]} identity-transforms))))
      (is (fn? (:f (field/density [1.0 1.0] [2.0 2.0] {} identity-transforms))) "2 equal points - no variance needed"))
    (testing "boundary: no placeable point is still an error"
      (is (= :x (:key (error-data #(field/density [] [] {} identity-transforms)))))))
  (testing "surface, contour and field render one point and report the panel without a field"
    (doseq [c [(chart/surface {:density true}) (chart/contour) (chart/field)]]
      (let [p {:data one-point :mapping {:x :x :y :y} :charts [c]}]
        (is (some? (plot/render p size)) (str (:kind c)))
        (is (= 1 (get-in (chart-stats p) [:dropped :field])) (str (:kind c))))))
  (testing "a numeric bandwidth draws one point's density; nothing dropped"
    (doseq [c [(chart/contour {:bandwidth 1.0}) (chart/field {:bandwidth 1.0})]]
      (is (zero? (get-in (chart-stats {:data one-point :mapping {:x :x :y :y} :charts [c]}) [:dropped :field])) (str (:kind c)))))
  (testing "boundary: facets - only the one-point panel is dropped"
    (let [p {:data (into [{:x 1.0 :y 2.0 :g "a"}] (map (fn [i] {:x (double i) :y (double (* i i)) :g "b"}) (range 10)))
             :mapping {:x :x :y :y :facet :g} :charts [(chart/contour)]}]
      (is (some? (plot/render p size)))
      (is (= 1 (get-in (chart-stats p) [:dropped :field])))))
  (testing "kde/estimate checks its 2 distinct values"
    (is (= :density (:key (error-data #(kde/estimate [1.0 1.0 1.0] {})))))
    (is (= :density (:key (error-data #(kde/estimate [] {})))))
    (is (fn? (:kde (kde/estimate [1.0 2.0] {}))))))

(def ^:private colorbar-plot
  {:data (map (fn [i] {:x (double i) :y (double (* i i)) :c (double i)}) (range 20))
   :mapping {:x :x :y :y :color :c}
   :charts [(chart/scatter)]})

(deftest colorbar-theme-test
  (testing "defaults are the old constants"
    (is (same-pixels? colorbar-plot (assoc colorbar-plot :theme {:legend {:bar-ticks 5 :bar-tick-length 3 :bar-tick-width 1}}) size)))
  (testing "each key changes the drawing"
    (doseq [legend [{:bar-ticks 10} {:bar-tick-length 8} {:bar-tick-width 3} {:bar-tick-width 0}]]
      (is (not (same-pixels? colorbar-plot (assoc colorbar-plot :theme {:legend legend}) size)) (pr-str legend))))
  (testing "a scale's :ticks wins over :bar-ticks"
    (is (same-pixels? (assoc colorbar-plot :scales {:color {:ticks 3}})
                      (assoc colorbar-plot :scales {:color {:ticks 3}} :theme {:legend {:bar-ticks 12}}) size)))
  (testing "boundary: zero length is allowed; invalid values rejected at the theme path"
    (is (some? (plot/render (assoc colorbar-plot :theme {:legend {:bar-tick-length 0}}) size)))
    (doseq [[k v] [[:bar-ticks 0] [:bar-ticks 2.5] [:bar-tick-length -1] [:bar-tick-width -0.5] [:bar-tick-width :none]]]
      (is (= [:theme :legend k] (:in (error #(plot/build (assoc colorbar-plot :theme {:legend {k v}}))))) (str k " " v)))))

(def ^:private box-plot
  {:charts [(chart/surface {:f (fn [[x y]] (* x y)) :domain {:x [-1 1] :y [-1 1]}})]})

(deftest frame-3d-theme-test
  (testing "defaults are the old constants"
    (is (same-pixels? box-plot (assoc box-plot :theme {:frame-3d {:hidden-edge-dash [3 3]} :axis {:x {:line-width 1}}}) size)))
  (testing "the hidden edge dash and the x axis line width change the box"
    (doseq [t [{:frame-3d {:hidden-edge-dash :none}} {:frame-3d {:hidden-edge-dash [8 2]}}
               {:axis {:x {:line-width 3}}} {:axis {:x {:line-width 0}}}]]
      (is (not (same-pixels? box-plot (assoc box-plot :theme t) size)) (pr-str t))))
  (testing "boundary: invalid dash rejected at the theme path"
    (is (= [:theme :frame-3d :hidden-edge-dash] (:in (error #(plot/build (assoc box-plot :theme {:frame-3d {:hidden-edge-dash [-1 2]}}))))))))
