(ns cljplot2.content.colorbar-test
  "Step 11: color bar legend for linear color scales."
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure2d.core :as c2d]
            [cljplot2.chart :as chart]
            [cljplot2.content.legend :as legend]
            [cljplot2.core :as plot]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.layout.grid :as grid])
  (:import [java.awt.image BufferedImage]))

(defn- error-data [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- pixel [canvas x y]
  (let [v (.getRGB ^BufferedImage (c2d/get-image canvas) (int x) (int y))]
    [(bit-and (bit-shift-right v 16) 0xff) (bit-and (bit-shift-right v 8) 0xff) (bit-and v 0xff)]))

(def ^:private rows (vec (for [i (range 20)] {:x i :y i :d (* 10 i) :g (["a" "b"] (mod i 2))})))
(def ^:private red-blue {:color {:range [[255 0 0] [0 0 255]]}})
(defn- cb-plot [extra] (merge {:data rows :mapping {:x :x :y :y :color :d} :charts [(chart/scatter)] :scales red-blue
                               :theme {:plot {:background :white}}} extra))
(defn- build [p] (plot/build p {:width 600 :height 400}))
(defn- gradient? "A red-to-blue bar pixel (title / label ink is black, background white)." [[r g b]]
  (and (< g 40) (> (+ r b) 200)))
(defn- legends [built] (some #(when (= :legends (:id %)) (get-in % [:content :legends])) (get-in built [:grid :items])))
(defn- boxes [built]
  (let [item (some #(when (= :legends (:id %)) %) (get-in built [:grid :items]))]
    (measure/call-with-scratch-canvas :high #(legend/legend-boxes (:content item) {:canvas % :theme (:theme built)}
                                                                 (grid/item-box (:layout built) :legends)))))

(deftest colorbar-content-test
  (let [[cb] (legends (build (cb-plot {})))]
    (is (= :colorbar (:kind cb)))
    (is (= "d" (:title cb)))
    (is (= [0.0 190.0] (get-in cb [:scale :domain])))
    (is (= (count (:ticks cb)) (count (:labels cb))))
    (is (every? #(<= 0.0 % 190.0) (:ticks cb))))
  (testing "single value domain: one tick"
    (is (= 1 (count (:ticks (first (legends (build (cb-plot {:data [{:x 1 :y 1 :d 5}]})))))))))
  (testing "hidden by :legend false on the plot or the scale"
    (is (nil? (legends (build (cb-plot {:legend false})))))
    (is (nil? (legends (build (cb-plot {:scales (assoc-in red-blue [:color :legend] false)})))))))

(deftest colorbar-drawing-test
  (let [p (cb-plot {})
        built (build p)
        [{:keys [x y w h]}] (boxes built)
        c (plot/render p {:width 600 :height 400})
        key-size 14
        column (for [py (range y (+ y h))] (pixel c (+ x 4) py))
        bar (filter gradient? column)]
    (testing "bar: top = domain max (blue), bottom = min (red)"
      (is (> (nth (first bar) 2) 240) "top blue")
      (is (> (nth (last bar) 0) 240) "bottom red"))
    (testing "stretches over the panel area height minus title and label room"
      (let [{ph :h} (grid/item-box (:layout built) :panel)]
        (is (> (count bar) (* 0.7 (double ph))))
        (is (>= (count bar) (* 3 key-size)))))
    (testing "fixed length from the theme"
      (let [p (cb-plot {:theme {:plot {:background :white} :legend {:bar-length 100}}})
            [{:keys [x y h]}] (boxes (build p))
            c (plot/render p {:width 600 :height 400})]
        (is (<= 99 (count (filter gradient? (for [py (range y (+ y h))] (pixel c (+ x 4) py)))) 101))))))

(deftest colorbar-stack-test
  (testing "with another legend: stacked below it, stretching over the rest"
    (let [built (build (cb-plot {:mapping {:x :x :y :y :color :d :shape :g}}))
          [l0 l1] (boxes built)]
      (is (= [:legend :colorbar] (map :kind (reverse (legends built)))) "shape legend + color bar")
      (is (some? l1))
      (is (or (> (:y l1) (:y l0)) (> (:x l1) (:x l0))))))
  (testing "too little room left: new stack column"
    (let [built (plot/build (cb-plot {:mapping {:x :x :y :y :color :d :shape :g}}) {:width 600 :height 140})
          [l0 l1] (boxes built)]
      (is (> (:x l1) (:x l0)))))
  (testing "theme validation"
    (are [t k] (= k (:key (error-data #(build (cb-plot {:theme t})))))
      {:legend {:bar-length -1}} :bar-length
      {:legend {:bar-width 0}} :bar-width
      {:legend {:bar-length "x"}} :bar-length)))
