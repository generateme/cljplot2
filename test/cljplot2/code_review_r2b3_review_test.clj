(ns cljplot2.code-review-r2b3-review-test
  "Code review round 2 B3 - review fixes: 3D zero-width ticks, faded :none, contour groups without a field, shared
  helpers (chart values, sizes, polyline lengths, fill with a pattern, the mapping check, line styles, name texts)."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [fastmath.vector :as v]
            [cljplot2.chart :as chart]
            [cljplot2.content.label :as label]
            [cljplot2.core :as plot]
            [cljplot2.data.kde :as kde]
            [cljplot2.data.rows :as rows]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.graphics.shape :as shape]
            [cljplot2.test-util :refer [error-data same-pixels?]])
  (:import [java.awt Graphics2D]
           [java.awt.image BufferedImage]))

(def ^:private size {:width 300 :height 240})

(deftest zero-width-3d-ticks-test
  (let [p {:charts [(chart/surface {:f (fn [[x y]] (* x y)) :domain {:x [-1 1] :y [-1 1]}})]}]
    (testing "a tick of width 0 isn't drawn, as a :none tick colour"
      (is (same-pixels? (assoc p :theme {:axis {:x {:tick-width 0}}}) (assoc p :theme {:axis {:x {:tick-color :none}}}) size)))
    (testing "boundary: a positive width still draws ticks"
      (is (not (same-pixels? (assoc p :theme {:axis {:x {:tick-width 0.5}}}) (assoc p :theme {:axis {:x {:tick-color :none}}}) size))))))

(deftest faded-none-test
  (testing ":none and nil pass through; a colour gets its alpha scaled"
    (is (= :none (draw/faded :none 0.5)))
    (is (nil? (draw/faded nil 0.5)))
    (is (= 100.0 (c/alpha (draw/faded (c/color 1 2 3 200) 0.5))))
    (is (zero? (c/alpha (draw/faded :red 0.0)))))
  (testing "a polygon without a fill renders"
    (is (some? (plot/render {:data [{:x 0 :y 0} {:x 1 :y 0} {:x 1 :y 1}] :mapping {:x :x :y :y}
                             :charts [(chart/polygon {:color :none})]} size)))))

(defn- dropped [p] (:dropped (first (:stats (plot/build p)))))

(deftest contour-groups-without-field-test
  (let [b-rows (map (fn [i] {:x (double i) :y (double (mod (* 7 i) 11)) :g "b"}) (range 12))]
    (testing "a one-point group is counted"
      (is (= 1 (:field (dropped {:data (conj b-rows {:x 1.0 :y 2.0 :g "a"}) :mapping {:x :x :y :y :color :g}
                                 :charts [(chart/contour)]})))))
    (testing "boundary: every group estimable - nothing dropped; two one-point groups - two"
      (is (zero? (:field (dropped {:data b-rows :mapping {:x :x :y :y :color :g} :charts [(chart/contour)]}))))
      (is (= 2 (:field (dropped {:data (conj b-rows {:x 1.0 :y 2.0 :g "a"} {:x 3.0 :y 1.0 :g "c"})
                                 :mapping {:x :x :y :y :color :g} :charts [(chart/contour)]})))))
    (testing "ungrouped: a one-point panel; a function source drops nothing"
      (is (= 1 (:field (dropped {:data [{:x 1.0 :y 2.0}] :mapping {:x :x :y :y} :charts [(chart/contour)]}))))
      (is (zero? (:field (dropped {:charts [(chart/contour {:f (fn [[x y]] (* x y)) :domain {:x [0 1] :y [0 1]}})]})))))))

(deftest chart-value-test
  (is (true? (chart/chart-value? (chart/scatter))))
  (is (false? (chart/chart-value? {:charts []})))
  (is (false? (chart/chart-value? nil)))
  (is (false? (chart/chart-value? [(chart/scatter)]))))

(deftest max-size-test
  (is (= (v/vec2 0.0 0.0) (measure/max-size [])))
  (is (= (v/vec2 0.0 0.0) (measure/max-size nil)))
  (is (= (v/vec2 5.0 7.0) (measure/max-size [(v/vec2 5.0 1.0) (v/vec2 2.0 7.0)])))
  (is (= (v/vec2 3.0 4.0) (measure/max-size (map identity [(v/vec2 3.0 4.0)])))))

(deftest polyline-lengths-test
  (testing "segments and cumulative distances"
    (is (= [5.0 5.0] (vec (polyline/segment-lengths [[0 0] [3 4] [6 8]]))))
    (is (= [0.0 5.0 10.0] (polyline/cumulative-lengths [[0 0] [3 4] [6 8]])))
    (is (= 10.0 (polyline/path-length [[0 0] [3 4] [6 8]]))))
  (testing "boundary: empty and one point; repeated points add 0"
    (is (empty? (polyline/segment-lengths [])))
    (is (= [0.0] (polyline/cumulative-lengths [])))
    (is (= [0.0] (polyline/cumulative-lengths [[1 1]])))
    (is (zero? (polyline/path-length [[1 1]])))
    (is (= [0.0 0.0 5.0] (polyline/cumulative-lengths [[0 0] [0 0] [3 4]])))))

(defn- canvas-pixels [f]
  (let [canvas (c2d/canvas 20 20)]
    (c2d/with-canvas [c canvas] (c2d/set-background c :white) (f c))
    (vec (.getRGB ^BufferedImage (c2d/get-image canvas) 0 0 20 20 nil 0 20))))

(deftest fill-patterned-test
  (testing "without a pattern - the fill alone; the canvas keeps the fill colour"
    (is (= (canvas-pixels #(do (c2d/set-color % :red) (c2d/rect % 2 2 10 10)))
           (canvas-pixels #(draw/fill-patterned! % :red nil (fn [c] (c2d/rect c 2 2 10 10))))))
    (c2d/with-canvas [c (c2d/canvas 5 5)]
      (draw/fill-patterned! c :blue nil (fn [_]))
      (is (= (c/to-color :blue) (c/to-color (.getColor ^Graphics2D (:graphics c)))))))
  (testing "with a pattern - the fill, then the pattern over it (the draw function runs twice)"
    (let [calls (atom 0)
          fp (draw/pattern-fill {:pattern :hatch :pattern-color :black} :red)]
      (is (= (canvas-pixels #(do (c2d/set-color % :red) (c2d/rect % 2 2 10 10)
                                 (draw/fill-with-pattern! % fp (fn [c] (c2d/rect c 2 2 10 10)))))
             (canvas-pixels #(draw/fill-patterned! % :red fp (fn [c] (swap! calls inc) (c2d/rect c 2 2 10 10))))))
      (is (= 2 @calls)))))

(deftest check-mapped-sites-test
  (testing "the mapping check at the shared sites keeps its error"
    (doseq [p [{:data [{:x 1 :y "a" :c nil} {:x 2 :y "b" :c nil}] :mapping {:x :x :y :y :color :c} :charts [(chart/tile)]}
               {:data [{:x nil :y 1}] :mapping {:x :x :y :y} :charts [(chart/scatter)]}
               {:data [{:x 1 :y nil}] :mapping {:x :x :y :y} :charts [(chart/box)]}
               {:data [{:x nil :y "a"}] :mapping {:x :x :y :y} :charts [(chart/box)]}
               {:data [{:x 1 :y 1 :g nil}] :mapping {:x :x :y :y :color :g} :charts [(chart/scatter)]}]]
      (let [d (error-data #(plot/build p))]
        (is (= :mapping (:key d)) (pr-str (:mapping p))))))
  (testing "boundary: empty data passes"
    (is (map? (plot/build {:data [] :mapping {:x :x :y :y} :charts [(chart/scatter)]})))))

(deftest set-style-test
  (c2d/with-canvas [c (c2d/canvas 5 5)]
    (testing "styles that draw nothing - nil, :none, no colour, :none colour, no or non-positive width"
      (doseq [st [nil :none {} {:line-width 1} {:color :none :line-width 1} {:color :red} {:color :red :line-width 0} {:color :red :line-width -1}]]
        (is (nil? (polyline/set-style! c st)) (pr-str st))))
    (testing "a drawing style"
      (is (true? (polyline/set-style! c {:color :red :line-width 2})))
      (is (true? (polyline/set-style! c {:color :red :line-width 1 :dash :dashed}))))))

(deftest name-text-test
  (is (= "a" (label/name-text :a)))
  (is (= "b" (label/name-text :a/b)) "a keyword's name, as before")
  (is (= "1.2345" (label/name-text 1.2345)) "no rounding, unlike value-text")
  (is (= "1.23" (label/value-text 1.2345)))
  (is (= "" (label/name-text nil)))
  (is (= "x" (label/name-text "x"))))

(deftest kde-distinct-test
  (is (= :density (:key (error-data #(kde/estimate [1 1.0] {})))) "1 and 1.0 are one value")
  (is (fn? (:kde (kde/estimate [1 2.0] {})))))

(deftest image-shape-isolation-test
  (let [img (let [b (BufferedImage. 4 4 BufferedImage/TYPE_INT_ARGB)] b)]
    (c2d/with-canvas [c (c2d/canvas 40 40)]
      (let [^Graphics2D g (:graphics c)
            before (.getTransform g)]
        (shape/draw! c {:image img} (v/vec2 20.0 20.0) {:size 10.0 :color :red :stroke-color :none :stroke-width 0.0})
        (is (= before (.getTransform g)) "the transform is unchanged")))))

(deftest finite-point-sites-test
  (testing "infinite values are left out of scatter and probability draws without failing"
    (is (some? (plot/render {:data [{:x 1 :y 1} {:x 2 :y ##Inf} {:x 3 :y 2}] :mapping {:x :x :y :y} :charts [(chart/scatter)]} size))))
  (testing "rows/finite-point? on pixel points"
    (is (rows/finite-point? (v/vec2 0.0 0.0)))
    (is (not (rows/finite-point? (v/vec2 ##Inf 0.0))))))
