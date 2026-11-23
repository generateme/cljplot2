(ns cljplot2.code-review-r2b3-content-test
  "Code review round 2 B3 - content/: intersecting clips, isolated drawing, the default chart stroke, quarter turns,
  colour bar positions and strip interval fractions."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.core :as c2d]
            [cljplot2.content.label :as label]
            [cljplot2.content.legend :as legend]
            [cljplot2.content.panel]
            [cljplot2.content.strip]
            [cljplot2.data.facet :as facet]
            [cljplot2.theme :as theme])
  (:import [java.awt BasicStroke Graphics2D Rectangle]))

(defn- clip-of [c] (.getClipBounds ^Graphics2D (:graphics c)))

(deftest quarter-turn-test
  (testing "multiples of 90 of any sign or size normalize to 0, 90, 180, 270"
    (doseq [[r q] [[0 0] [90 90] [180 180] [270 270] [360 0] [450 90] [720 0] [-90 270] [-180 180] [-360 0] [-450 270]
                   [0.0 0] [-0.0 0] [90.0 90] [1080.0 0]]]
      (is (= q (label/quarter-turn r)) (str r))))
  (testing "boundary: other angles give nil"
    (doseq [r [45 -45 1e-12 -1e-12 89.999999 359.9 0.5]]
      (is (nil? (label/quarter-turn r)) (str r)))))

(deftest strip-interval-fractions-test
  (let [fractions @#'cljplot2.content.strip/interval-fractions]
    (is (= [0.25 0.75] (fractions {:lo 1.0 :hi 3.0 :min 0.0 :max 4.0})))
    (is (= [0.0 1.0] (fractions {:lo 0 :hi 4 :min 0 :max 4})) "longs, the whole range")
    (testing "boundary: an empty or reversed range - the whole strip"
      (is (= [0.0 1.0] (fractions {:lo 2.0 :hi 2.0 :min 2.0 :max 2.0})))
      (is (= [0.0 1.0] (fractions {:lo 1.0 :hi 2.0 :min 3.0 :max 1.0}))))
    (testing "an interval outside the range extrapolates"
      (is (= [-0.5 1.5] (fractions {:lo -1.0 :hi 3.0 :min 0.0 :max 2.0}))))))

(deftest colorbar-position-test
  (let [value-fn @#'legend/bar-value-fn
        position-fn @#'legend/bar-position-fn]
    (testing "linear: proportional, the position the inverse of the value"
      (let [spec {:kind :linear :domain [10.0 20.0]}
            value-at (value-fn spec) position-of (position-fn spec)]
        (is (= [10.0 15.0 20.0] (mapv value-at [0.0 0.5 1.0])))
        (is (= [0.0 0.5 1.0] (mapv position-of [10 15 20.0])))
        (is (= 1.5 (position-of 25.0)) "outside the domain extrapolates")))
    (testing "boundary: a one-value domain places every value in the middle"
      (let [position-of (position-fn {:kind :linear :domain [5.0 5.0]})]
        (is (= [0.5 0.5] (mapv position-of [5.0 7.0])))))
    (testing "binned: equal blocks per bin, proportional within"
      (let [spec {:kind :binned :domain [0.0 100.0] :breaks [0.0 10.0 100.0]}
            value-at (value-fn spec) position-of (position-fn spec)]
        (is (= [0.0 10.0 55.0 100.0] (mapv value-at [0.0 0.5 0.75 1.0])))
        (is (= [0.0 0.5 0.75 1.0] (mapv position-of [0.0 10.0 55.0 100.0])))
        (is (= 5.0 (value-at 0.25)))
        (testing "boundary: positions outside [0 1] stay in the end bins"
          (is (= -10.0 (value-at -0.5)))
          (is (= 145.0 (value-at 1.25))))))))

(deftest legend-key-clip-test
  (testing "a key glyph's clip intersects an outer clip (a legend inside a clipped panel)"
    (let [seen (atom nil)
          draw-key! @#'legend/draw-key!
          chart {:legend (fn [_ attributes c _ env] (reset! seen [attributes (clip-of c) (:chart-stats env)]))}
          env {:legend-charts [chart] :legend-stats [:stats-0]}]
      (c2d/with-canvas [c (c2d/canvas 100 100)]
        (draw-key! c {:color :red :size 3} {:x 5.5 :y 5.5 :w 20.0 :h 10.0} [{:chart 0 :dims [:color]}] env)
        (is (= [{:color :red} (Rectangle. 4 4 23 13) :stats-0] @seen) "whole pixels plus one around, only mapped dims")
        (c2d/add-clip c 0 0 10 10)
        (draw-key! c {} {:x 5.5 :y 5.5 :w 20.0 :h 10.0} [{:chart 0 :dims []}] env)
        (is (= (Rectangle. 4 4 6 6) (second @seen)) "intersected with the outer clip")
        (is (= (Rectangle. 0 0 10 10) (clip-of c)) "the outer clip is restored")))))

(deftest label-isolation-test
  (testing "draw-at! and draw-at-frame! leave the canvas state as it was; the label clip intersects an outer clip"
    (c2d/with-canvas [c (c2d/canvas 200 100)]
      (c2d/add-clip c 0 0 50 50)
      (c2d/translate c 3 4)
      (let [^Graphics2D g (:graphics c)
            before [(.getClip g) (.getTransform g) (.getColor g) (.getFont g)]
            content {:kind :label :role :title :text "a label"}
            env {:theme theme/default-theme}]
        (is (nil? (label/draw-at! c content [:center :middle] 100 50 env)))
        (is (nil? (label/draw-at-frame! c (assoc content :rotation 30) [:left :middle] 10 10 env)))
        (is (= before [(.getClip g) (.getTransform g) (.getColor g) (.getFont g)]))))))

(deftest panel-chart-stroke-test
  (testing "every chart draws with the default stroke, clipped to the panel inside an outer clip"
    (let [seen (atom [])
          chart {:draw (fn [_ _ c box _ _] (swap! seen conj [(.getStroke ^Graphics2D (:graphics c)) (clip-of c) box]))}
          draw-charts! @#'cljplot2.content.panel/draw-charts!
          env {:charts [chart chart] :stats [{:data {facet/single-panel :a}} {:data {facet/all-panels :b}}] :scales {}}]
      (c2d/with-canvas [c (c2d/canvas 100 100)]
        (c2d/set-stroke c 5.0)
        (draw-charts! c facet/single-panel 40 30 env)
        (c2d/add-clip c 0 0 20 20)
        (draw-charts! c facet/single-panel 40 30 env))
      (is (= 4 (count @seen)))
      (is (every? #(= (BasicStroke.) (first %)) @seen))
      (is (= [(Rectangle. 0 0 40 30) (Rectangle. 0 0 40 30) (Rectangle. 0 0 20 20) (Rectangle. 0 0 20 20)] (mapv second @seen)))
      (is (every? #(= {:x 0 :y 0 :w 40 :h 30} (nth % 2)) @seen)))))
