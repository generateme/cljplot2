(ns cljplot2.render-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.core :as c2d]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.layout.grid :as grid]
            [cljplot2.examples :as examples]
            [cljplot2.test-util :refer [error-data]])
  (:import [java.awt.image BufferedImage]))

(def ^:private white (unchecked-int 0xFFFFFFFF))

(defn- pixel ^long [canvas x y]
  (.getRGB ^BufferedImage (c2d/get-image canvas) (int x) (int y)))

(deftest canvas-size-test
  (doseq [[w h] [[600 600] [37 23] [1 1] [800 600]]]
    (let [canvas (plot/render {:theme {:plot {:margin 0}}} {:width w :height h})]
      (is (= [w h] [(c2d/width canvas) (c2d/height canvas)]))))
  (testing ":until is ignored"
    (is (= 10 (c2d/width (plot/render {:theme {:plot {:margin 0}}} {:width 10 :height 10 :until :split}))))))

(deftest item-boxes-painted-test
  (doseq [[g w h] [[examples/fig-1-1-grid 800 600] [examples/fig-1-1-grid 400 300] [examples/nested-grid 1000 600]]]
    (let [canvas (plot/render {:custom-grid g :theme {:plot {:background :white}}} {:width w :height h :debug true})
          layout (:layout (plot/build {:custom-grid g} {:width w :height h}))
          leaves (fn leaves [items] (mapcat #(if (:layout %) (leaves (get-in % [:layout :items])) [%]) items))]
      ;; sample near the top-left corner, inside the outline and away from the centred id label
      (doseq [{:keys [id box]} (leaves (:items layout))
              :let [{:keys [x y w h]} box]
              :when (and (>= w 5) (>= h 5))]
        (is (not= white (pixel canvas (+ x 2) (+ y 2))) (pr-str id))))))

(deftest fig-1-1-panels-test
  (doseq [[w h] [[800 600] [400 300]]]
    (let [layout (:layout (plot/build {:custom-grid examples/fig-1-1-grid} {:width w :height h}))
          boxes (for [r (range 2) c (range 3)] (grid/item-box layout [:panel r c]))
          ws (map :w boxes)
          hs (map :h boxes)]
      (is (= 6 (count (remove nil? boxes))))
      (is (<= (- (apply max ws) (apply min ws)) 1) (str w "x" h))
      (is (<= (- (apply max hs) (apply min hs)) 1) (str w "x" h)))))

(deftest too-small-test
  (is (= :columns (:axis (try (plot/render {:custom-grid examples/fig-1-1-grid} {:width 50 :height 300}) nil
                              (catch clojure.lang.ExceptionInfo e (ex-data e)))))))

(defn- pixels [^BufferedImage image]
  (vec (.getRGB image 0 0 (.getWidth image) (.getHeight image) nil 0 (.getWidth image))))

(def ^:private faithful-like
  {:data (for [i (range 50)] {:x (mod (* i 7) 13) :y (mod (* i 11) 17) :g (if (even? i) "a" "b")})
   :mapping {:x :x :y :y :color :g}
   :charts [(chart/scatter)]
   :labels {:title "render-image" :x "x" :y "y"}})

(deftest render-image-test
  (testing "a BufferedImage of exactly :width x :height"
    (doseq [[w h] [[300 200] [1 1] [37 23] [1000 800]]]
      (let [image (plot/render-image {:theme {:plot {:margin 0}}} {:width w :height h})]
        (is (instance? BufferedImage image))
        (is (= [w h] [(.getWidth image) (.getHeight image)])))))
  (testing "default size, as render"
    (let [image (plot/render-image {})]
      (is (= [600 600] [(.getWidth image) (.getHeight image)]))))
  (testing "same pixels as the image of render"
    (doseq [options [{:width 300 :height 200} {:width 300 :height 200 :debug true :hint :low}]]
      (is (= (pixels (c2d/get-image (plot/render faithful-like options)))
             (pixels (plot/render-image faithful-like options))))))
  (testing "same errors as render"
    (doseq [[p options] [[{} {:width 0 :height 10}]
                         [{} {:width 10 :height 10 :hint :retina}]
                         [{:charts [(chart/scatter {:size -1})]} {}]
                         [{:custom-grid examples/fig-1-1-grid} {:width 50 :height 300}]]]
      (let [expected (error-data #(plot/render p options))]
        (is (some? expected))
        (is (= expected (error-data #(plot/render-image p options))))))))
