(ns cljplot2.three-d-title-angle-test
  "Tests of the 3D frame titles' rotation: a title on a steep edge reads bottom to top, also when a perspective view leans the edge."
  (:require [clojure.test :refer [deftest is testing]]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.core :as plot]
            [cljplot2.chart :as chart]
            [cljplot2.content.label :as label]
            [cljplot2.content.space3d :as space-draw]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- near?
  "Tests whether doubles `a` and `b` differ by less than 1e-6 degrees."
  [^double a ^double b]
  (m/< (m/abs (m/- a b)) 1.0E-6))

(defn- upward?
  "Tests whether a clockwise rotation (y down) runs text bottom to top: its baseline points up the screen."
  [^double deg]
  (m/neg? (m/sin (m/radians deg))))

(defn- readable?
  "Tests whether a title rotation is one [[space-draw/edge-angle]] gives: left to right up to 45 degrees, else bottom to top - never top to bottom (225 included: a 45 degree edge off by a rounding error counts as steep)."
  [^double deg]
  (or (m/<= 0.0 deg 45.0) (and (m/<= 225.0 deg) (m/< deg 360.0))))

(deftest edge-angle-test
  (testing "a steep direction reads bottom to top, either way along the line and leaning either way"
    (doseq [[x y] [[-1.6 -131.2] [1.6 131.2] [1.6 -131.2] [-1.6 131.2] [-16.8 -122.1] [16.8 122.1] [-36.6 -50.5] [36.6 50.5]
                   [56.0 112.1] [-56.0 -112.1]]]
      (let [deg (space-draw/edge-angle (v/vec2 x y))]
        (is (upward? deg) [x y deg])
        (is (m/< 225.0 deg 315.0) [x y deg]))))
  (testing "the same line gives the same angle whichever way along it"
    (doseq [[x y] [[-16.8 -122.1] [16.8 -122.1] [121.2 -23.9] [10 10] [10 -10] [0 10]]]
      (is (near? (space-draw/edge-angle (v/vec2 x y)) (space-draw/edge-angle (v/vec2 (m/- (double x)) (m/- (double y))))) [x y])))
  (testing "a vertical direction: 270, also off by a rounding error (an orthographic x / y edge)"
    (is (near? 270.0 (space-draw/edge-angle (v/vec2 0 10))))
    (is (near? 270.0 (space-draw/edge-angle (v/vec2 0 -10))))
    (is (near? 270.0 (space-draw/edge-angle (v/vec2 1.0E-14 1.0))))
    (is (near? 270.0 (space-draw/edge-angle (v/vec2 -1.0E-14 1.0)))))
  (testing "exactly 45 degrees reads left to right in all four sign combinations; just steeper turns"
    (is (near? 45.0 (space-draw/edge-angle (v/vec2 10 10))))
    (is (near? 45.0 (space-draw/edge-angle (v/vec2 -10 -10))))
    (is (near? 315.0 (space-draw/edge-angle (v/vec2 10 -10))))
    (is (near? 315.0 (space-draw/edge-angle (v/vec2 -10 10))))
    (is (near? (m/+ 180.0 (m/degrees (m/atan2 10.01 10.0))) (space-draw/edge-angle (v/vec2 10 10.01))))
    (is (near? (m/+ 360.0 (m/degrees (m/atan2 -10.01 10.0))) (space-draw/edge-angle (v/vec2 10 -10.01)))))
  (testing "shallow directions read left to right"
    (is (near? 30.0 (space-draw/edge-angle (v/vec2 (m/cos (m/radians 30)) (m/sin (m/radians 30))))))
    (is (near? 330.0 (space-draw/edge-angle (v/vec2 (m/cos (m/radians 30)) (m/- (m/sin (m/radians 30)))))))
    (is (near? 0.0 (space-draw/edge-angle (v/vec2 10 0))))
    (is (near? 0.0 (space-draw/edge-angle (v/vec2 -10 0)))))
  (testing "a zero direction gives 0; a direction a rounding error below horizontal gives 0, never 360"
    (is (near? 0.0 (space-draw/edge-angle (v/vec2 0 0))))
    (is (= 0.0 (space-draw/edge-angle (v/vec2 1.0 -1.0E-17))))
    (is (m/< (space-draw/edge-angle (v/vec2 1.0 -1.0E-10)) 360.0))))

(def ^:private titles
  "The frame title texts of the test plots."
  #{"X axis" "Y axis" "Z axis"})

(defn- title-draws
  "`[text rotation]` of every frame title drawn by rendering `plot`, in drawing order."
  [plot]
  (let [drawn (atom [])
        draw-at! label/draw-at!]
    (with-redefs [label/draw-at! (fn [c content anchor x y env]
                                   (when (titles (:text content))
                                     (swap! drawn conj [(:text content) (:rotation content)]))
                                   (draw-at! c content anchor x y env))]
      (plot/render-image plot {:width 400 :height 400}))
    @drawn))

(defn- title-rotations
  "Rotations of the frame titles drawn by rendering `plot`, keyed by title text (see [[title-draws]])."
  [plot]
  (into {} (title-draws plot)))

(defn- surface
  "A surface plot titled on every axis, seen from `view`, with extra z `label` options."
  [view z-label]
  {:charts [(chart/surface {:f (fn [[^double x ^double y]] (m/* x y)) :domain {:x [-1 1] :y [-1 1]} :view view})]
   :labels {:x "X axis" :y "Y axis" :z (merge {:text "Z axis"} z-label)}})

(deftest surface-z-title-test
  (testing "the reported case: the z title reads bottom to top under perspective (orthographic: exactly 270)"
    (doseq [d [0.0 0.05 0.2 0.5] az [-120 -60 -30 0 30 60 90 120 180] el [-30 0 20 60]]
      (let [deg (double (get (title-rotations (surface {:distance d :azimuth az :elevation el} nil)) "Z axis"))]
        (is (upward? deg) [d az el deg])
        (when (m/zero? (double d))
          (is (near? 270.0 deg) [az el deg])))))
  (testing "a z label map's own :rotation wins"
    (is (= 0 (get (title-rotations (surface {:distance 0.2 :azimuth 30 :elevation 20} {:rotation 0})) "Z axis")))
    (is (= 90 (get (title-rotations (surface {:distance 0.2 :azimuth 30 :elevation 20} {:rotation 90})) "Z axis")))))

(deftest every-title-readable-test
  (testing "every view, up to the largest distance: each drawn title once, none read top to bottom"
    (doseq [d [0.0 0.2 0.5 1.1 1.15] az [-90 -30 0 30 90 180 270 360] el [-90 -60 -30 0 20 60 85 90]]
      (let [draws (title-draws (surface {:distance d :azimuth az :elevation el} nil))]
        (doseq [[text n] (frequencies (map first draws))]
          (is (= 1 n) [d az el text]))
        (doseq [[text deg] draws]
          (is (readable? (double deg)) [d az el text deg])))))
  (testing "orthographic x / y edges vertical up to a rounding error read bottom to top (azimuth 90, 180)"
    (doseq [[az el t] [[90 30 "X axis"] [-90 -30 "X axis"] [180 60 "Y axis"] [180 -60 "Y axis"]]]
      (is (near? 270.0 (double (get (title-rotations (surface {:distance 0.0 :azimuth az :elevation el} nil)) t))) [az el t])))
  (testing "orthographic from straight above or below: the z edges collapse, no z title"
    (doseq [el [90 -90]]
      (let [rotations (title-rotations (surface {:distance 0.0 :azimuth 30 :elevation el} nil))]
        (is (nil? (get rotations "Z axis")) el)
        (is (some? (get rotations "X axis")) el))))
  (testing "the azimuth wraps"
    (let [wrapped (title-rotations (surface {:distance 1.1 :azimuth 405 :elevation 30} nil))
          base (title-rotations (surface {:distance 1.1 :azimuth 45 :elevation 30} nil))]
      (is (= (set (keys base)) (set (keys wrapped))))
      (doseq [t (keys base)]
        (is (near? (double (base t)) (double (wrapped t))) t)))))

(deftest cloud-z-title-test
  (testing "cloud bars share the frame: the z title reads bottom to top under perspective"
    (let [rows (for [x (range 3) y (range 3)] {:x x :y y :z (m/long-inc (m/long-add x y))})
          rotations (title-rotations {:data rows :mapping {:x :x :y :y :z :z}
                                      :charts [(chart/cloud {:type :bars :view {:distance 0.5 :azimuth 30 :elevation 20}})]
                                      :labels {:x "X axis" :y "Y axis" :z "Z axis"}})]
      (is (upward? (double (get rotations "Z axis"))) rotations))))
