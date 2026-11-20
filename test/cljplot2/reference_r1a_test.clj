(ns cljplot2.reference-r1a-test
  "Reference part R1a review: rect `:stroke-color :darker` (the darkened fill, as bars)."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.color :as c]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.test-util :refer [error]]))

(defn- rect-plot
  [options]
  {:charts [(chart/rect (merge {:x [0 10] :y [0 10] :stroke-width 3} options))]
   :scales {:x {:domain [-1 11]} :y {:domain [-1 11]}}})

(defn- colors
  "Distinct ARGB values of `img`."
  [^java.awt.image.BufferedImage img]
  (set (for [x (range (.getWidth img)) y (range (.getHeight img))] (.getRGB img (int x) (int y)))))

(deftest rect-darker-test
  (testing ":darker draws the darkened fill as the outline"
    (let [darker (c/pack (c/darken (c/color :red)))]
      (is (contains? (colors (plot/render-image (rect-plot {:fill :red :stroke-color :darker}) {:width 120 :height 120})) darker))
      (is (not (contains? (colors (plot/render-image (rect-plot {:fill :red :stroke-color :none}) {:width 120 :height 120})) darker)))))
  (testing "boundary: an unfilled rect with :darker has no outline (nothing to darken)"
    (is (= (colors (plot/render-image (rect-plot {:fill :none :stroke-color :none}) {:width 120 :height 120}))
           (colors (plot/render-image (rect-plot {:fill :none :stroke-color :darker}) {:width 120 :height 120})))))
  (testing "the legend key renders with :darker too"
    (is (some? (plot/render-image {:data [{:x0 0 :x1 1 :y0 0 :y1 1 :g "a"}]
                                   :charts [(chart/rect {:mapping {:x0 :x0 :x1 :x1 :y0 :y0 :y1 :y1 :color :g}
                                                         :stroke-color :darker})]}
                                  {:width 200 :height 120}))))
  (testing "other keywords are still schema errors"
    (is (= :stroke-color (:key (error #(plot/build (rect-plot {:stroke-color :lighter}))))))))
