(ns cljplot2.vega-cycle1-test
  "Vega cycle 1 (marks): rounded bar corners (cartesian, polar, value end only, legend keys), point shapes from SVG
  path data, texts (emoji from the bundled font), SVG files and images, line trails (width per point)."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.font :as font]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.graphics.shape :as shape]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.coord :as coord]
            [cljplot2.test-util :refer [error]]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]
           [java.awt.image BufferedImage]))

(def ^:private white (unchecked-int 0xffffffff))
(def ^:private red (unchecked-int 0xffff0000))
(def ^:private blue (unchecked-int 0xff0000ff))

(defn- drawn
  "Image of a `w` x `h` white canvas after `(f canvas)`."
  ^BufferedImage [w h f]
  (let [cv (c2d/canvas w h)]
    (c2d/with-canvas [g cv]
      (c2d/set-background g :white)
      (f g))
    (c2d/get-image cv)))

(defn- px [^BufferedImage img x y] (.getRGB img (int x) (int y)))

(defn- count-color
  "Pixels of `img` exactly `color`."
  [^BufferedImage img color]
  (count (for [x (range (.getWidth img)) y (range (.getHeight img)) :when (== (int color) (px img x y))] 1)))

(defn- ink-count
  "Pixels of `img` that aren't white."
  [^BufferedImage img]
  (count (for [x (range (.getWidth img)) y (range (.getHeight img)) :when (not= white (px img x y))] 1)))

(defn- color-bounds
  "`[x0 y0 x1 y1]` of the pixels of `img` that aren't white."
  [^BufferedImage img]
  (let [pts (for [x (range (.getWidth img)) y (range (.getHeight img)) :when (not= white (px img x y))] [x y])]
    [(reduce min (map first pts)) (reduce min (map second pts)) (reduce max (map first pts)) (reduce max (map second pts))]))

(deftest corner-rounding-test
  (testing "absent or 0 - square (nil); :corner-radius-end wins, also at 0"
    (is (nil? (draw/corner-rounding {})))
    (is (nil? (draw/corner-rounding {:corner-radius 0})))
    (is (= {:radius 8.0 :end? false} (draw/corner-rounding {:corner-radius 8})))
    (is (= {:radius 4.0 :end? true} (draw/corner-rounding {:corner-radius 8 :corner-radius-end 4})))
    (is (nil? (draw/corner-rounding {:corner-radius 8 :corner-radius-end 0}))))
  (testing "the value axis: where the baseline and value-end points differ"
    (is (= :v (draw/value-axis [10 50] [10 5])))
    (is (= :h (draw/value-axis [10 50] [80 50]))))
  (testing "a negative radius is a schema error"
    (is (= :corner-radius (:key (error #(plot/build {:data [{:x "a" :y 1}] :mapping {:x :x :y :y}
                                                     :charts [(chart/bar {:corner-radius -1})]})))))))

(deftest cartesian-corners-test
  (testing "r = 8: corner pixels background, the centre filled; the opposite corner too"
    (let [img (drawn 60 50 #(draw/bar! % (Vec2. 10 10) (Vec2. 50 40) :red :none 0 nil {:radius 8.0 :end? false}))]
      (is (= white (px img 10 10)))
      (is (= white (px img 49 39)))
      (is (= red (px img 30 25)))
      (is (= red (px img 10 25)) "the side midpoint")))
  (testing "a radius above the bar size is clamped to half the shorter side (6 px bar - 3)"
    (let [sh (draw/rounded-rect {:x 0 :y 0 :w 6 :h 20} [100 100 100 100])]
      (is (.contains sh 3.0 3.0))
      (is (.contains sh 0.5 10.0))
      (is (not (.contains sh 0.3 0.3)))
      (is (= [0.0 0.0 6.0 20.0] (let [b (.getBounds2D sh)] [(.getX b) (.getY b) (.getWidth b) (.getHeight b)])))))
  (testing "radius 0 corners stay square"
    (let [sh (draw/rounded-rect {:x 0 :y 0 :w 10 :h 10} [0 0 0 0])]
      (is (.contains sh 0.1 0.1))))
  (testing "end rounding: the value-end corners only (upright bar, value at the top)"
    (let [img (drawn 60 50 #(draw/bar! % (Vec2. 10 40) (Vec2. 50 10) :red :none 0 nil {:radius 8.0 :end? true :axis :v}))]
      (is (= white (px img 10 10)))
      (is (= white (px img 49 10)))
      (is (= red (px img 10 39)))
      (is (= red (px img 49 39)))))
  (testing "end rounding of a horizontal bar growing left (negative value)"
    (let [img (drawn 60 50 #(draw/bar! % (Vec2. 50 10) (Vec2. 10 40) :red :none 0 nil {:radius 8.0 :end? true :axis :h}))]
      (is (= white (px img 10 10)))
      (is (= white (px img 10 39)))
      (is (= red (px img 49 10)))
      (is (= red (px img 49 39))))))

(defn- polar-geometry
  "A full-circle polar geometry of a 200 x 200 panel, outer radius 90."
  []
  (coord/geometry {:kind :polar} 200.0 200.0 90.0))

(defn- polar-pixel
  "Pixel of angle `a` (radians) at radius `r` in `geometry`."
  [geometry a r]
  (let [^Vec2 p (coord/to-pixel geometry (double a) (double r))] [(m/round (.x p)) (m/round (.y p))]))

(deftest polar-corners-test
  (let [g (polar-geometry)
        draw-sector (fn [rounding] (drawn 200 200 #(draw/range-bar! % g [0.2 30.0] [1.2 80.0] :red :none 0 nil rounding)))]
    (testing "r = 6: the four corners background, the middle filled"
      (let [img (draw-sector {:radius 6.0 :end? false})]
        (doseq [[a r] [[0.205 79.0] [1.195 79.0] [0.205 31.0] [1.195 31.0]]]
          (is (= white (apply px img (polar-pixel g a r))) (str "corner " a " " r)))
        (is (= red (apply px img (polar-pixel g 0.7 55.0))))))
    (testing "square corners: the same corner pixels filled"
      (let [img (draw-sector nil)]
        (is (= red (apply px img (polar-pixel g 0.21 78.0))))))
    (testing "end rounding along the radius: outer corners round, inner square"
      (let [img (draw-sector {:radius 6.0 :end? true :axis :v})]
        (is (= white (apply px img (polar-pixel g 0.205 79.0))))
        (is (= red (apply px img (polar-pixel g 0.21 32.0))))))
    (testing "end rounding along the angle: the corners at the second angle only"
      (let [img (draw-sector {:radius 6.0 :end? true :axis :h})]
        (is (= white (apply px img (polar-pixel g 1.195 79.0))))
        (is (= red (apply px img (polar-pixel g 0.21 78.0)))))))
  (testing "a full-circle ring has no corners: the plain annulus"
    (let [g (polar-geometry)]
      (is (= (coord/rect-path g 0.0 30.0 m/TWO_PI 80.0)
             (draw/rounded-sector-path g [0.0 30.0] [m/TWO_PI 80.0] {:radius 6.0 :end? false})))))
  (testing "a wedge (inner radius 0): outer corners only, every point finite"
    (let [pts (draw/rounded-sector-path (polar-geometry) [0.0 0.0] [1.0 80.0] {:radius 10.0 :end? false})]
      (is (every? (fn [[x y]] (and (m/valid-double? (double x)) (m/valid-double? (double y)))) pts))))
  (testing "a radius above the thickness is clamped (no NaN, the middle filled)"
    (let [g (polar-geometry)
          img (drawn 200 200 #(draw/range-bar! % g [0.2 70.0] [1.2 80.0] :red :none 0 nil {:radius 100.0 :end? false}))]
      (is (= red (apply px img (polar-pixel g 0.7 75.0)))))))

(defn- stacked-plot
  [rounding]
  {:data [{:x "a" :y 4 :g "p"} {:x "a" :y 3 :g "q"}] :mapping {:x :x :y :y :color :g}
   :scales {:color {:range [[255 0 0] [0 0 255]]}}
   :charts [(chart/bar (merge {:position :stack :stroke-width 0} rounding))]})

(deftest plot-corners-test
  (testing ":corner-radius 0 and absent render the same image"
    (let [a (plot/render-image (stacked-plot {}) {:width 200 :height 200})
          b (plot/render-image (stacked-plot {:corner-radius 0}) {:width 200 :height 200})]
      (is (= (count-color a red) (count-color b red)))
      (is (= (count-color a blue) (count-color b blue)))))
  (testing "every stacked segment is rounded: each loses its corner pixels"
    (let [square (plot/render-image (stacked-plot {}) {:width 200 :height 200})
          rounded (plot/render-image (stacked-plot {:corner-radius 8}) {:width 200 :height 200})]
      (is (< 30 (- (count-color square red) (count-color rounded red))))
      (is (< 30 (- (count-color square blue) (count-color rounded blue))))))
  (testing "histogram and rect take the options; a rect has no value end"
    (is (some? (plot/build {:data [{:x 1} {:x 2}] :mapping {:x :x} :charts [(chart/histogram {:corner-radius-end 3})]})))
    (is (some? (plot/build {:charts [(chart/rect {:x [0 1] :y [0 1] :corner-radius 4})]})))
    (is (= :corner-radius-end (:key (error #(plot/build {:charts [(chart/rect {:x [0 1] :y [0 1] :corner-radius-end 4})]})))))))

(deftest key-corners-test
  (testing "a bar legend key rounded, clamped to its square"
    (let [img (drawn 20 20 #(draw/bar-key! % {:x 0 :y 0 :w 20 :h 20} {:color :red :stroke-color :none :stroke-width 0} false
                                           {:radius 100.0 :end? false}))]
      (is (= red (px img 10 10)))
      (is (= white (px img 3 3)) "the key's corner (square from 3 to 17)")))
  (testing "end rounding: the key's top corners round, the bottom square"
    (let [img (drawn 20 20 #(draw/bar-key! % {:x 0 :y 0 :w 20 :h 20} {:color :red :stroke-color :none :stroke-width 0} false
                                           {:radius 5.0 :end? true}))]
      (is (= white (px img 3 3)))
      (is (= red (px img 3 16))))))

(def ^:private wide-path
  "A 2:1 rectangle as SVG path data."
  {:path "M0 0 L40 0 L40 20 L0 20 Z"})

(defn- blue-image
  "A 10 x 10 opaque blue image."
  ^BufferedImage []
  (let [img (BufferedImage. 10 10 BufferedImage/TYPE_INT_ARGB)]
    (doseq [x (range 10) y (range 10)] (.setRGB img (int x) (int y) blue))
    img))

(defn- svg-file
  "A temporary SVG file of `content`."
  [content]
  (let [f (java.io.File/createTempFile "cljplot2-shape" ".svg")]
    (.deleteOnExit f)
    (spit f content)
    (str f)))

(def ^:private red-square-svg
  "<svg xmlns='http://www.w3.org/2000/svg' width='10' height='10'><rect width='10' height='10' fill='red'/></svg>")

(def ^:private no-size-svg
  "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 4 2'><rect width='4' height='2' fill='red'/></svg>")

(deftest shape-map-test
  (testing "valid shape maps and their problems"
    (is (shape/shape? \O))
    (is (shape/shape? wide-path))
    (is (shape/shape? {:text "🐄"}))
    (is (shape/shape? {:text "A" :font "DejaVu Sans Mono"}))
    (is (shape/shape? {:image (blue-image)}))
    (is (shape/shape? {:svg (svg-file red-square-svg)}))
    (is (not (shape/shape? \Q)))
    (is (re-find #"exactly one" (shape/shape-problem {:path "M0 0 L1 1" :text "a"})))
    (is (re-find #"exactly one" (shape/shape-problem {})))
    (is (re-find #"doesn't take" (shape/shape-problem {:path "M0 0 L1 1" :color :red})))
    (is (re-find #"only a :text" (shape/shape-problem {:path "M0 0 L1 1" :font "Serif"})))
    (is (re-find #"doesn't parse" (shape/shape-problem {:path "M0 0 Q"})))
    (is (re-find #"empty path" (shape/shape-problem {:path ""})))
    (is (re-find #"non-empty string" (shape/shape-problem {:text 3})))
    (is (re-find #"non-empty string" (shape/shape-problem {:text ""})))
    (is (re-find #"isn't an existing" (shape/shape-problem {:svg "no/such/file.svg"})))
    (is (re-find #"image" (shape/shape-problem {:image "x.png"}))))
  (testing "errors carry {:key :shape} (options and the shape scale range)"
    (is (= :shape (:key (error #(plot/build {:data [{:x 1 :y 1}] :mapping {:x :x :y :y}
                                             :charts [(chart/scatter {:shape {:path "M0 0 Q"}})]})))))
    (is (= [:scales :shape :range 0]
           (:in (error #(plot/build {:data [{:x 1 :y 1 :s "a"}] :mapping {:x :x :y :y :shape :s}
                                     :scales {:shape {:range [{:svg "no/such/file.svg"}]}}
                                     :charts [(chart/scatter)]})))))))

(deftest shape-drawing-test
  (testing "a 2:1 path at :size 20 measures 20 x 10, centred, in :color"
    (let [img (drawn 40 40 #(shape/draw! % wide-path (Vec2. 20 20) {:size 20 :color :red :stroke-color :none :stroke-width 0}))
          [x0 y0 x1 y1] (color-bounds img)]
      (is (= [20 10] [(inc (- (long x1) (long x0))) (inc (- (long y1) (long y0)))]))
      (is (= [10 15] [x0 y0]))
      (is (= red (px img 20 20)))))
  (testing "an image keeps its colours (:color ignored), fitted into the size square"
    (let [img (drawn 40 40 #(shape/draw! % {:image (blue-image)} (Vec2. 20 20) {:size 20 :color :red}))]
      (is (= blue (px img 20 20)))
      (is (zero? (count-color img red)))
      (is (= [10 10 29 29] (color-bounds img)))))
  (testing "an SVG keeps its colours; one without a size is fitted by its viewBox (aspect kept)"
    (let [img (drawn 40 40 #(shape/draw! % {:svg (svg-file red-square-svg)} (Vec2. 20 20) {:size 20 :color :blue}))]
      (is (= red (px img 20 20))))
    (let [img (drawn 40 40 #(shape/draw! % {:svg (svg-file no-size-svg)} (Vec2. 20 20) {:size 20 :color :blue}))
          [_ y0 _ y1] (color-bounds img)]
      (is (= red (px img 20 20)))
      (is (<= 9 (inc (- (long y1) (long y0))) 11) "2:1 - about 10 px high")))
  (testing "an SVG is transcoded once per source and size"
    (let [calls (atom 0)
          file (svg-file red-square-svg)
          transcode c2d/transcode-svg]
      (with-redefs [c2d/transcode-svg (fn [input ^long w ^long h] (swap! calls inc) (transcode input w h))]
        (drawn 40 40 (fn [g] (dotimes [_ 1000] (shape/draw! g {:svg file} (Vec2. 20 20) {:size 13})))))
      (is (= 1 @calls))))
  (testing "a text shape: the bundled emoji font fills in characters the font lacks - no tofu"
    (is (some? (font/resolve-font "Noto Emoji" :plain 12)))
    (let [cow {:text "🐄"}
          style {:size 20 :color :red}
          fallback (drawn 40 40 #(shape/draw! % cow (Vec2. 20 20) style))
          direct (drawn 40 40 #(shape/draw! % (assoc cow :font "Noto Emoji") (Vec2. 20 20) style))]
      (is (< 100 (ink-count fallback)))
      (is (= (ink-count fallback) (ink-count direct))))))

(deftest shape-charts-test
  (testing "scatter, stripplot and beeswarm take every kind; legend keys draw them"
    (let [shapes [wide-path {:text "🐄"} {:image (blue-image)} {:svg (svg-file red-square-svg)}]
          rows (for [i (range 4) j (range 3)] {:x j :y i :s (str "s" i)})
          img (plot/render-image {:data rows :mapping {:x :x :y :y :shape :s} :scales {:shape {:range shapes}}
                                  :charts [(chart/scatter {:size 16})]}
                                 {:width 300 :height 200})]
      (is (pos? (count-color img blue)))
      (is (some? (plot/render-image {:data rows :mapping {:x :s :y :y}
                                     :charts [(chart/stripplot {:shape {:text "★"}})
                                              (chart/beeswarm {:shape wide-path})]}
                                    {:width 300 :height 200}))))))

(defn- filled?
  "Does the drawn `shape` (non-zero rule) cover pixel `(x, y)` of a canvas it is filled on in red?"
  [shape x y]
  (= red (px (drawn 120 100 #(do (c2d/set-color % :red) (c2d/shape % shape false))) x y)))

(deftest trail-shape-test
  (testing "widths 2 -> 20: thicker at the end"
    (let [sh (polyline/trail-shape [10 90] [50 50] [2 20])]
      (is (filled? sh 88 58))
      (is (not (filled? sh 12 58)))
      (is (filled? sh 10 50))
      (is (filled? sh 50 50) "the hull between the discs")))
  (testing "one fill: a translucent trail has no darker overlaps at the joins"
    (let [img (drawn 120 100 #(do (c2d/set-color % [255 0 0 128])
                                  (c2d/shape % (polyline/trail-shape [10 50 90] [50 20 50] [10 10 10]) false)))]
      (is (= (px img 50 22) (px img 30 35)) "a joint pixel and a segment pixel alike")))
  (testing "boundary: one point - its disc; size 0 everywhere - nothing"
    (let [b (.getBounds2D (polyline/trail-shape [10] [10] [8]))]
      (is (= [6.0 6.0 8.0 8.0] [(.getX b) (.getY b) (.getWidth b) (.getHeight b)])))
    (is (zero? (ink-count (drawn 30 10 #(do (c2d/set-color % :red)
                                             (c2d/shape % (polyline/trail-shape [0 10 20] [5 5 5] [0 0 0]) false)))))))
  (testing "boundary: one disc inside the other - just the bigger disc"
    (let [b (.getBounds2D (polyline/trail-shape [10 11] [10 10] [20 2]))]
      (is (< (m/abs (- 20.0 (.getWidth b))) 1e-9))))
  (testing "a trail going right to left or up fills alike (one winding for discs and hulls)"
    (is (filled? (polyline/trail-shape [90 10] [50 50] [20 2]) 50 50))
    (is (filled? (polyline/trail-shape [50 50] [90 10] [20 2]) 50 50))))

(def ^:private trail-rows
  [{:x 0 :y 0 :s 1} {:x 1 :y 1 :s 5} {:x 2 :y 0 :s nil} {:x 3 :y 1 :s 9}])

(deftest trail-chart-test
  (testing "a trail renders; rows without a size are dropped and counted"
    (let [built (plot/build {:data trail-rows :mapping {:x :x :y :y :size :s} :charts [(chart/line {:trail true})]})]
      (is (= 1 (get-in built [:stats 0 :dropped :size])))
      (is (= [1 5 9] (get-in built [:stats 0 :data :panel :lines 0 :sizes]))))
    (is (some? (plot/render-image {:data trail-rows :mapping {:x :x :y :y :size :s} :charts [(chart/line {:trail true})]}
                                  {:width 200 :height 150}))))
  (testing "boundary: 1- and 2-point trails, size 0, no size mapping (constant :width)"
    (doseq [rows [[{:x 0 :y 0 :s 3}] [{:x 0 :y 0 :s 0} {:x 1 :y 1 :s 0}]]]
      (is (some? (plot/render-image {:data rows :mapping {:x :x :y :y :size :s} :charts [(chart/line {:trail true})]}
                                    {:width 200 :height 150}))))
    (is (some? (plot/render-image {:data trail-rows :mapping {:x :x :y :y} :charts [(chart/line {:trail true :width 5})]}
                                  {:width 200 :height 150}))))
  (testing "options a trail can't take -> {:key :trail}"
    (doseq [opts [{:fill :color} {:fill-gradient :viridis} {:position :stack} {:arrow {:end true}} {:start-gap 3}
                  {:dash :dashed} {:interpolator :cubic} {:curve :bezier} {:gaps :bridge}]]
      (is (= :trail (:key (error #(plot/build {:data trail-rows :mapping {:x :x :y :y :size :s}
                                               :charts [(chart/line (assoc opts :trail true))]}))))
          (pr-str opts)))
    (is (= :trail (:key (error #(plot/build {:data trail-rows :mapping {:x :x :y :y :color :s}
                                             :charts [(chart/line {:trail true})]}))))
        "colour along the length")))

(deftest polar-trail-test
  (testing "with :arcs a polar trail follows the circle: unlike the chords"
    (let [rows [{:x 0 :y 1 :s 1} {:x 1 :y 1 :s 5} {:x 2 :y 1 :s 9}]
          plot (fn [arcs] {:data rows :mapping {:x :x :y :y :size :s} :coord (cond-> {:kind :polar} arcs (assoc :arcs true))
                           :scales {:x {:domain [0 3]} :y {:domain [0 1]}}
                           :charts [(chart/line {:trail true :color :red})]})
          chords (plot/render-image (plot false) {:width 200 :height 200})
          arcs (plot/render-image (plot true) {:width 200 :height 200})]
      (is (pos? (count-color arcs red)))
      (is (not= (count-color chords red) (count-color arcs red))))))

(deftest key-fit-test
  (testing "a constant :size above the key box shrinks to the box (no crossing into the next row)"
    (let [img (drawn 40 40 #(shape/draw-key! % wide-path {:x 13 :y 13 :w 14 :h 14} {:size 30 :color :red :stroke-width 0}))
          [x0 _ x1 _] (color-bounds img)]
      (is (= 14 (inc (- (long x1) (long x0)))))))
  (testing "a size inside the box is unchanged"
    (let [img (drawn 40 40 #(shape/draw-key! % wide-path {:x 0 :y 0 :w 40 :h 40} {:size 20 :color :red :stroke-width 0}))
          [x0 _ x1 _] (color-bounds img)]
      (is (= 20 (inc (- (long x1) (long x0))))))))

(deftest key-rounding-test
  (testing "the key radius is at most a quarter of its side; end rounding at the top"
    (is (= {:radius 2.5 :end? false :axis :v} (draw/key-rounding {:radius 6.0 :end? false} 10.0)))
    (is (= {:radius 1.0 :end? true :axis :v} (draw/key-rounding {:radius 1.0 :end? true} 10.0)))
    (is (nil? (draw/key-rounding nil 10.0))))
  (testing "a rect legend key is rounded too"
    (let [rect-chart (chart/rect {:corner-radius 40})
          img (drawn 20 20 #((:legend rect-chart) rect-chart {:color :red} % {:x 0 :y 0 :w 20 :h 20} {:theme theme/default-theme}))]
      (is (= red (px img 10 10)))
      (is (not= red (px img 3 3)) "corner of the 14 px key"))))

(deftest polar-stacked-corners-test
  (testing "every stacked segment of a polar bar is rounded"
    (let [plot (fn [r] (assoc (stacked-plot (when r {:corner-radius r})) :coord {:kind :polar :inner 0.3}))
          square (plot/render-image (plot nil) {:width 240 :height 240})
          rounded (plot/render-image (plot 8) {:width 240 :height 240})]
      (is (< 20 (- (count-color square red) (count-color rounded red))))
      (is (< 20 (- (count-color square blue) (count-color rounded blue)))))))

(deftest mirror-end-corners-test
  (testing "a mirror histogram's downward bars round at the bottom (their value end), square at the baseline"
    (let [rows (concat (repeat 5 {:x 1.0 :g "p"}) (repeat 5 {:x 1.0 :g "q"}))
          img (plot/render-image {:data rows :mapping {:x :x :color :g}
                                  :scales {:color {:range [[255 0 0] [0 0 255]] :legend false}}
                                  :charts [(chart/histogram {:position :mirror :corner-radius-end 8 :stroke-width 0
                                                             :zero-line false})]}
                                 {:width 200 :height 200})
          pts (for [x (range 200) y (range 200) :when (= blue (px img x y))] [x y])
          x0 (reduce min (map first pts)) y0 (reduce min (map second pts)) y1 (reduce max (map second pts))]
      (is (not= blue (px img x0 y1)) "bottom corner rounded")
      (is (= blue (px img x0 y0)) "baseline corner square"))))

(deftest text-font-test
  (testing "a text shape without :font follows *text-font*; render binds the theme's annotation font"
    (let [draw-a (fn [] (drawn 40 40 #(shape/draw! % {:text "Ag"} (Vec2. 20 20) {:size 30 :color :red})))
          mono (draw-a)
          serif (binding [shape/*text-font* "Serif"] (draw-a))]
      (is (not= (ink-count mono) (ink-count serif))))
    (let [plot (fn [theme] (cond-> {:data [{:x 1 :y 1}] :mapping {:x :x :y :y}
                                    :charts [(chart/scatter {:shape {:text "Ag"} :size 60 :color :red})]}
                             theme (assoc :theme theme)))]
      (is (not= (count-color (plot/render-image (plot nil) {:width 150 :height 150}) red)
                (count-color (plot/render-image (plot {:text {:annotation {:font "Serif"}}}) {:width 150 :height 150}) red))))))

(deftest svg-draw-error-test
  (testing "an SVG that exists but doesn't transcode fails at drawing as a schema-shaped {:key :shape}"
    (let [file (svg-file "<svg xmlns='http://www.w3.org/2000/svg'><rect width='oops'")
          data (try (drawn 20 20 #(shape/draw! % {:svg file} (Vec2. 10 10) {:size 9})) nil
                    (catch clojure.lang.ExceptionInfo e (assoc (ex-data e) :message (ex-message e))))]
      (is (= :shape (:key data)))
      (is (= 1 (count (:errors data))))
      (is (re-find #"^Invalid input: the :svg shape can't be drawn" (:message data))))))

(deftest aesthetic-ticks-test
  (testing "a linear size legend: :ticks values inside the domain, sorted and distinct, or a count"
    (is (= [1931 1932] (mapv :value (aesthetic/legend-entries :size {:kind :linear :domain [1931 1932] :ticks [1932 1931 1932]}))))
    (is (= [2] (mapv :value (aesthetic/legend-entries :size {:kind :linear :domain [0 5] :ticks [-1 2 9]}))))
    (is (= [] (mapv :value (aesthetic/legend-entries :size {:kind :linear :domain [0 5] :ticks [9]}))) "none inside")
    (is (= [0.0 5.0 10.0] (mapv #(double (:value %)) (aesthetic/legend-entries :size {:kind :linear :domain [0 10] :ticks 2}))))
    (is (= 6 (count (aesthetic/legend-entries :size {:kind :linear :domain [0 10]}))) "default unchanged"))
  (testing "boundary: a one-value domain with :ticks keeps the given values inside it"
    (is (= [3] (mapv :value (aesthetic/legend-entries :size {:kind :linear :domain [3 3] :ticks [3 4]})))))
  (testing ":ticks needs a linear :size or :color scale; values finite, a count positive"
    (doseq [[k spec] [[:size {:kind :ordinal :ticks [1]}] [:color {:kind :binned :bins 3 :ticks 3}] [:shape {:ticks 2}]]]
      (is (= :ticks (:key (error #(plot/build {:data [{:x 1 :y 1 :v 1}] :mapping {:x :x :y :y k :v}
                                               :scales {k spec} :charts [(chart/scatter)]}))))
          (pr-str k spec)))
    (doseq [t [0 -2 [##NaN] ["a"]]]
      (is (= :ticks (:key (error #(plot/build {:data [{:x 1 :y 1 :v 1}] :mapping {:x :x :y :y :size :v}
                                               :scales {:size {:ticks t}} :charts [(chart/scatter)]}))))
          (pr-str t))))
  (testing "plots render with :ticks on a size legend and a color bar (also with none inside)"
    (doseq [scales [{:size {:ticks [1931 1932]}} {:color {:ticks [1931.5]}} {:color {:ticks 3}} {:size {:ticks [0]}}]]
      (is (some? (plot/render-image {:data [{:x 1 :y 1 :v 1931} {:x 2 :y 2 :v 1932}]
                                     :mapping {:x :x :y :y (key (first scales)) :v} :scales scales
                                     :charts [(chart/scatter)]}
                                    {:width 300 :height 200}))
          (pr-str scales)))))

(deftest value-ends-test
  (testing "the value end is the end farther from 0, whatever the order"
    (is (= [0 5] (draw/value-ends 0 5)))
    (is (= [3 5] (draw/value-ends 3 5)) "a stacked segment")
    (is (= [0 -4] (draw/value-ends -4 0)) "a mirror's downward bar")
    (is (= [-2 -6] (draw/value-ends -6 -2)) "a stacked negative segment")
    (is (= [0 -3] (draw/value-ends 0 -3)) "a dodged negative bar"))
  (testing "a stacked negative bar rounds at its far end (bottom), square toward 0"
    (let [img (plot/render-image {:data [{:x "a" :y -4 :g "p"} {:x "a" :y -3 :g "q"}] :mapping {:x :x :y :y :color :g}
                                  :scales {:color {:range [[255 0 0] [0 0 255]] :legend false}}
                                  :charts [(chart/bar {:position :stack :stroke-width 0 :corner-radius-end 8})]}
                                 {:width 200 :height 200})
          pts (for [x (range 200) y (range 200) :when (= blue (px img x y))] [x y])
          x0 (reduce min (map first pts)) y0 (reduce min (map second pts)) y1 (reduce max (map second pts))]
      (is (not= blue (px img x0 y1)) "far corner rounded")
      (is (= blue (px img x0 y0)) "corner toward 0 square"))))
