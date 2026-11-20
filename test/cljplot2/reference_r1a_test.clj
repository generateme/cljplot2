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
  (testing "boundary: a translucent fill gets a translucent darker outline (drawn, not the opaque darkened colour)"
    (let [opaque-darker (c/pack (c/darken (c/color :red)))
          img (plot/render-image (rect-plot {:fill :red :alpha 0.5 :stroke-color :darker}) {:width 120 :height 120})]
      (is (not= (colors (plot/render-image (rect-plot {:fill :red :alpha 0.5 :stroke-color :none}) {:width 120 :height 120}))
                (colors img)))
      (is (not (contains? (colors img) opaque-darker)))))
  (testing "the legend key renders with :darker too"
    (is (some? (plot/render-image {:data [{:x0 0 :x1 1 :y0 0 :y1 1 :g "a"}]
                                   :charts [(chart/rect {:mapping {:x0 :x0 :x1 :x1 :y0 :y0 :y1 :y1 :color :g}
                                                         :stroke-color :darker})]}
                                  {:width 200 :height 120}))))
  (testing "other keywords are still schema errors"
    (is (= :stroke-color (:key (error #(plot/build (rect-plot {:stroke-color :lighter}))))))))

(def ^:private theme-blue (c/pack (c/color 69 117 180)))

(defn- points
  "Two seeded clusters, tagged by `:g`."
  []
  (let [rng (java.util.Random. 3)]
    (for [g ["a" "b"] _ (range 60)]
      {:g g :x (+ (if (= g "a") 0.0 3.0) (.nextGaussian rng)) :y (.nextGaussian rng)})))

(deftest contour-color-default-test
  (let [render #(plot/render-image {:data (points) :mapping %1 :charts [(chart/contour %2)]} {:width 300 :height 300})]
    (testing "ungrouped lines are coloured by level, not in the theme colour"
      (is (not (contains? (colors (render {:x :x :y :y} {})) theme-blue))))
    (testing "lines grouped without a mapped colour take the theme colour"
      (is (contains? (colors (render {:x :x :y :y :group :g} {})) theme-blue)))
    (testing "lines grouped by :color take their group's colour"
      (is (not (contains? (colors (render {:x :x :y :y :color :g} {})) theme-blue))))))

(deftest correlation-color-default-test
  (let [render #(plot/render-image {:data (points) :mapping %1 :charts [(chart/correlation {:glyph :square})]}
                                   {:width 200 :height 200})]
    (testing "an ungrouped glyph shows r on the colour scale, not the theme colour"
      (is (not (contains? (colors (render {:x :x :y :y})) theme-blue))))
    (testing "a grouped chart's glyph takes the theme colour"
      (is (contains? (colors (render {:x :x :y :y :group :g})) theme-blue)))))
