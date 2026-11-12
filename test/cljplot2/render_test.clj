(ns cljplot2.render-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.core :as c2d]
            [cljplot2.core :as plot]
            [cljplot2.grid :as grid]
            [cljplot2.examples :as examples])
  (:import [java.awt.image BufferedImage]))

(def ^:private white (unchecked-int 0xFFFFFFFF))

(defn- pixel ^long [canvas x y]
  (.getRGB ^BufferedImage (c2d/get-image canvas) (int x) (int y)))

(deftest canvas-size-test
  (doseq [[w h] [[600 600] [37 23] [1 1] [800 600]]]
    (let [canvas (plot/render {} {:width w :height h})]
      (is (= [w h] [(c2d/width canvas) (c2d/height canvas)]))))
  (testing ":until is ignored"
    (is (= 10 (c2d/width (plot/render {} {:width 10 :height 10 :until :split}))))))

(deftest item-boxes-painted-test
  (doseq [[g w h] [[examples/fig-1-1-grid 800 600] [examples/fig-1-1-grid 400 300] [examples/nested-grid 1000 600]]]
    (let [canvas (plot/render {:grid g} {:width w :height h :debug true})
          layout (:layout (plot/build {:grid g} {:width w :height h}))
          leaves (fn leaves [items] (mapcat #(if (:layout %) (leaves (get-in % [:layout :items])) [%]) items))]
      ;; sample near the top-left corner, inside the outline and away from the centred id label
      (doseq [{:keys [id box]} (leaves (:items layout))
              :let [{:keys [x y w h]} box]
              :when (and (>= w 5) (>= h 5))]
        (is (not= white (pixel canvas (+ x 2) (+ y 2))) (pr-str id))))))

(deftest fig-1-1-panels-test
  (doseq [[w h] [[800 600] [400 300]]]
    (let [layout (:layout (plot/build {:grid examples/fig-1-1-grid} {:width w :height h}))
          boxes (for [r (range 2) c (range 3)] (grid/item-box layout [:panel r c]))
          ws (map :w boxes)
          hs (map :h boxes)]
      (is (= 6 (count (remove nil? boxes))))
      (is (<= (- (apply max ws) (apply min ws)) 1) (str w "x" h))
      (is (<= (- (apply max hs) (apply min hs)) 1) (str w "x" h)))))

(deftest too-small-test
  (is (= :columns (:axis (try (plot/render {:grid examples/fig-1-1-grid} {:width 50 :height 300}) nil
                              (catch clojure.lang.ExceptionInfo e (ex-data e)))))))
