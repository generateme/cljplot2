(ns cljplot2.batch-c1-merge-test
  "Batch C1 merge fixes: a function chart under a partial y domain takes the open end from its samples; area fills of
  every line-like chart keep the group colour's own alpha."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.color :as c]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.graphics.polyline :as polyline]))

(defn- y-domain
  "The finalized y domain of a plot of `charts` with plot `scales`."
  [charts scales]
  (get-in (plot/build {:charts charts :scales scales} {:width 300 :height 200}) [:finalized-scales :y :domain]))

(deftest function-partial-y-domain-test
  (let [f (chart/function {:f (fn [^double x] (* x x)) :domain [1 3]})]
    (testing "an open high end comes from the samples; the fixed low end stays below them"
      (let [[lo hi] (y-domain [f] {:y {:domain [0 nil]}})]
        (is (<= (double lo) 0.0))
        (is (>= (double hi) 9.0))))
    (testing "an open low end; both ends fixed - no extent needed"
      (let [[lo hi] (y-domain [f] {:y {:domain [nil 20]}})]
        (is (<= (double lo) 1.0))
        (is (>= (double hi) 20.0)))
      (let [[lo hi] (y-domain [f] {:y {:domain [0 20]}})]
        (is (<= (double lo) 0.0))
        (is (>= (double hi) 20.0))))
    (testing "boundary: no domain and [nil nil] give the same scale"
      (is (= (y-domain [f] nil) (y-domain [f] {:y {:domain [nil nil]}}))))))

(deftest area-color-keeps-alpha-test
  (testing "every chart filling through polyline/area-color: :alpha times the colour's own alpha"
    (is (= 76.8 (c/alpha (polyline/area-color {:color (c/color 0 0 255 128) :alpha 0.6 :fill :color} false))))
    (is (= 153.0 (c/alpha (polyline/area-color {:color (c/color 0 0 255) :alpha 0.6 :fill :color} false)))))
  (testing "boundary: alpha 0 colour, nil :alpha, an explicit fill"
    (is (= 0.0 (c/alpha (polyline/area-color {:color (c/color 0 0 255 0) :alpha 0.6 :fill :color} false))))
    (is (= [1 2 3] (polyline/area-color {:color (c/color 0 0 255 128) :alpha 0.6 :fill [1 2 3]} false))))
  (testing "a density area (no outline) of a translucent colour differs from the opaque one's - only the fill can"
    (let [rows (for [i (range 50)] {:x (mod (* i 7) 13)})
          pixels (fn [color] (let [^java.awt.image.BufferedImage i
                                   (plot/render-image {:data rows :mapping {:x :x}
                                                       :charts [(chart/density {:color color :fill :color :alpha 0.8 :line-width 0})]}
                                                      {:width 200 :height 150})]
                               (vec (.getRGB i 0 0 200 150 nil 0 200))))]
      (is (not= (pixels (c/color 0 0 255)) (pixels (c/color 0 0 255 60)))))))
