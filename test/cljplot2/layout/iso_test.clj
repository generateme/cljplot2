(ns cljplot2.layout.iso-test
  "Step 11: :aspect :iso - equal pixels per data unit on x and y."
  (:require [clojure.test :refer [deftest testing is are]]
            [cljplot2.test-util :refer [error-data error error-ins]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.layout.grid :as grid]))

(defn- px-per-unit
  "`[x y]` pixels per data unit of the built plot's first panel."
  [built]
  (let [{:keys [w h]} (grid/find-content-box (:grid built) (:layout built) #(= :panel (:kind %)))
        span (fn [k] (let [[a b] (get-in built [:finalized-scales k :domain])] (Math/abs (- (double b) (double a)))))]
    [(/ (double w) (span :x)) (/ (double h) (span :y)) (min w h)]))

(defn- iso? [built]
  (let [[px py m] (px-per-unit built)]
    (<= (/ (Math/abs (- px py)) (max px py)) (/ 1.0 (double m)))))

(def ^:private wide {:data (for [i (range 50)] {:x (* 4 i) :y (mod (* i 7) 50) :g (["a" "b" "c"] (mod i 3))})
                     :mapping {:x :x :y :y} :charts [(chart/scatter)] :aspect :iso})

(deftest iso-single-panel-test
  (doseq [[w h] [[600 400] [400 600] [300 300] [900 200]]]
    (is (iso? (plot/build wide {:width w :height h})) (str w "x" h)))
  (testing "panel block centred: spacers on both sides"
    (let [built (plot/build wide {:width 600 :height 600})
          tracks (fn [axis] (into {} (map (juxt :id :size)) (get-in built [:grid axis])))]
      (doseq [axis [:rows :columns]
              :let [t (tracks axis)]]
        (is (<= 0 (- (long (second (t :iso-after))) (long (second (t :iso-before)))) 1)
            (str axis " spacers equal up to the odd pixel")))
      (is (pos? (long (second (get (tracks :rows) :iso-before)))) "wide data in a square canvas: row spacers used")
      (is (zero? (long (second (get (tracks :columns) :iso-before)))) "no column spacers"))))

(deftest iso-facet-test
  (let [p (assoc wide :mapping {:x :x :y :y :facet :g})]
    (doseq [[w h] [[800 400] [500 700]]]
      (let [built (plot/build p {:width w :height h})
            panels (map :box (grid/content-items (:grid built) (:layout built) #(= :panel (:kind %))))]
        (is (iso? built) (str w "x" h))
        (is (<= (- (apply max (map :w panels)) (apply min (map :w panels))) 1) "equal panel widths")))))

(deftest iso-validation-test
  (is (= :aspect (:key (error #(plot/build (assoc wide :aspect :bogus))))))
  (is (= :aspect (:key (error #(plot/build {:data [{:x "a" :y 1}] :mapping {:x :x :y :y} :charts [(chart/scatter)] :aspect :iso})))))
  (is (= :aspect (:key (error #(plot/build {:custom-grid {:columns [{:size [:flexible]}] :rows [{:size [:flexible]}] :items []}
                                                 :aspect :iso})))))
  (testing "without :aspect nothing changes"
    (is (not-any? #(#{:iso-before :iso-after} (:id %)) (get-in (plot/build (dissoc wide :aspect)) [:grid :columns])))))

(deftest iso-in-multiplot-test
  (let [mp (plot/arrange [wide (dissoc wide :aspect)] {:columns 2})
        built (plot/build mp {:width 900 :height 400})]
    (is (iso? (get-in built [:subplots [:plot 0]])))
    (is (nil? (get-in built [:subplots [:plot 0] :alignment-padding])) "iso sub-plot not aligned")
    (is (some? (plot/render mp {:width 900 :height 400})))))
