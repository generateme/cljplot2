(ns cljplot2.code-review-r3a-f5-test
  "Code review round 3, batch A (infrastructure namespaces): the shared helpers of A2 and their boundaries."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.core :as c2d]
            [cljplot2.data.group :as group]
            [fastmath.core :as m]
            [cljplot2.data.rows :as rows]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.font :as font]
            [cljplot2.content.label]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.scale.coord :as coord]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme]))

(deftest finite-double-test
  (is (= 2.0 (schema/finite-double 2)))
  (is (= 0.5 (schema/finite-double 0.5)))
  (doseq [v [nil ##NaN ##Inf ##-Inf "1" :a]]
    (is (nil? (schema/finite-double v)) (pr-str v))))

(deftest rows-helpers-test
  (testing "invalid-count"
    (is (= 3 (rows/invalid-count :x [{:x 1} {:x nil} {:x ##NaN} {} {:x "a"}])))
    (is (= 0 (rows/invalid-count :x []))))
  (testing "widened-range"
    (is (= [1.0 2.0] (rows/widened-range [1.0 2.0])))
    (is (= [0.5 1.5] (rows/widened-range [1.0 1.0]))))
  (testing "padding-stats"
    (is (= {:padding (assoc rows/default-padding :x [0.1 0.1]) :padding-explicit (rows/explicit-padding nil {:padding {:x [0.1 0.1]}})}
           (rows/padding-stats rows/default-padding nil {:padding {:x [0.1 0.1]}})))
    (is (= rows/zero-padding (:padding (rows/padding-stats rows/zero-padding nil {}))))))

(deftest group-helpers-test
  (let [panels {:p [{:rows [{:x 1} {:x ##NaN}]} {:rows [{:x 2} {:x 3} {}]}]}]
    (is (= 5 (group/row-count panels)))
    (is (= 0 (group/row-count {})))
    (is (= [[1.0] [2.0 3.0]] (map :values (:p (group/finite-values panels :x)))))
    (is (= [1 1] (map :dropped (:p (group/finite-values panels :x)))))))

(deftest kept-value-test
  (testing "positions never keep nil; grouping dimensions do with the NA level"
    (is (not (group/kept-value? :x true {} nil)))
    (is (group/kept-value? :x false {} 1.0))
    (is (not (group/kept-value? :x false {} ##NaN)))
    (is (group/kept-value? :color true {} nil))
    (is (not (group/kept-value? :color false {} nil)))
    (is (group/kept-value? :group true {} nil))
    (is (not (group/kept-value? :group false {} nil))))
  (testing "an explicit aesthetic domain"
    (is (group/kept-value? :color false {:color {:domain ["a"]}} "a"))
    (is (not (group/kept-value? :color false {:color {:domain ["a"]}} "b")))))

(deftest stroke-helpers-test
  (is (draw/visible-stroke? :black 1))
  (is (not (draw/visible-stroke? :none 1)))
  (is (not (draw/visible-stroke? :black 0)))
  (testing "outline! strokes only a visible outline"
    (let [calls (atom 0)
          canvas (c2d/canvas 10 10)]
      (c2d/with-canvas [c canvas]
        (draw/outline! c :none 2.0 (fn [_] (swap! calls inc)))
        (draw/outline! c :black 0.0 (fn [_] (swap! calls inc)))
        (is (= 0 @calls))
        (is (nil? (draw/outline! c :black 2.0 :miter (fn [_] (swap! calls inc)))))
        (is (= 1 @calls)))))
  (testing "translucent-fill"
    (is (= :red (draw/translucent-fill :red 0.5 false)))
    (is (= 127 (long (nth (draw/translucent-fill [255 0 0] 0.5 true) 3))))
    (is (= :none (draw/translucent-fill :none 0.5 true)))))

(deftest label-size-test
  (measure/call-with-scratch-canvas
   :high
   (fn [c]
     (let [env {:canvas c :theme theme/default-theme}]
       (is (= (measure/natural-size {:kind :label :role :tick-label :text "abc"} env)
              (measure/label-size :tick-label "abc" env)))))))

(deftest font-errors-test
  (doseq [[args k] [[["DejaVu Sans" :bad 10] :font-style]
                    [["DejaVu Sans" :plain -1] :size]
                    [[42 :plain 10] :font]]]
    (let [data (try (apply font/resolve-font args) nil (catch clojure.lang.ExceptionInfo e (ex-data e)))]
      (is (= k (:key data)) (pr-str args))
      (is (= 1 (count (:errors data)))))))

(deftest upright-label-angle-test
  (testing "label angles stay in the upright half-turns, as before the fastmath mod"
    (doseq [deg [0.0 45.0 90.0 135.0 180.0 225.0 270.0 315.0 -90.0 -450.0 720.0]]
      (let [a (coord/radial-rotation (m/radians deg))]
        (is (or (< -90.0 a 90.0000001) (<= 270.0 a 360.0)) (str deg " -> " a))))))
