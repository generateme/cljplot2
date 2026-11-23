(ns cljplot2.code-review-r2b3-graphics-test
  "Code review round 2 B3 - graphics/ on clojure2d: pattern tiles and fills, painted colours, contrast colours, trails,
  rings, bezier flattening, drawn points, image shapes, fonts."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [clojure2d.core.shape :as sh]
            [fastmath.core :as m]
            [cljplot2.core :as plot]
            [cljplot2.graphics.curve :as curve]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.font :as font]
            [cljplot2.graphics.pattern :as pattern]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.graphics.shape :as shape]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec2]
           [java.awt BasicStroke Graphics2D RenderingHints Shape]
           [java.awt.geom AffineTransform Ellipse2D$Double Line2D$Double Path2D PathIterator]
           [java.awt.image BufferedImage]))

(defn- argb [^BufferedImage img]
  (vec (.getRGB img 0 0 (.getWidth img) (.getHeight img) nil 0 (.getWidth img))))

(defn- java2d-tile
  "The tile as drawn before B3: a raw BufferedImage, antialiasing on, pure strokes."
  [{:keys [kind width size] :as spec} ink]
  (let [p (pattern/period spec)
        img (BufferedImage. p p BufferedImage/TYPE_INT_ARGB)
        g (.createGraphics img)]
    (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
    (.setRenderingHint g RenderingHints/KEY_STROKE_CONTROL RenderingHints/VALUE_STROKE_PURE)
    (.setColor g (c/awt-color ink))
    (if (= :dots kind)
      (let [d (double size) o (- (* 0.5 (double p)) (* 0.5 d))]
        (.fill g (Ellipse2D$Double. o o d d)))
      (do (.setStroke g (BasicStroke. (float width) BasicStroke/CAP_SQUARE BasicStroke/JOIN_MITER))
          (doseq [a (if (= :cross kind) [(long (:angle spec)) (+ 90 (long (:angle spec)))] [(long (:angle spec))])
                  :let [pd (double p) mid (+ (Math/floor (* 0.5 pd)) 0.5)]
                  [x0 y0 x1 y1] (case (rem (long a) 180)
                                  0 [[0.0 mid pd mid]]
                                  90 [[mid 0.0 mid pd]]
                                  45 (map (fn [d] [d pd (+ d pd) 0.0]) [(- pd) 0.0 pd])
                                  135 (map (fn [d] [d 0.0 (+ d pd) pd]) [(- pd) 0.0 pd]))]
            (.draw g (Line2D$Double. x0 y0 x1 y1)))))
    (.dispose g)
    img))

(deftest pattern-tile-test
  (testing "every named pattern draws the same pixels as the Java2D tile"
    (doseq [[name spec] schema/pattern-styles
            :when (map? spec)
            ink [:black (c/color 200 30 30 120)]]
      (is (= (argb (java2d-tile spec ink)) (argb (pattern/tile spec ink))) (str name " " ink))))
  (testing "boundary: the smallest period"
    (let [spec {:kind :lines :angle 0 :spacing 1 :width 1}]
      (is (= 2 (.getWidth ^BufferedImage (pattern/tile spec :black)))))))

(deftest pattern-fill-test
  (testing "no pattern, ink or fill - nil"
    (is (nil? (draw/pattern-fill {:pattern :solid} :red)))
    (is (nil? (draw/pattern-fill {} :red)))
    (is (nil? (draw/pattern-fill {:pattern :dots} :none)))
    (is (nil? (draw/pattern-fill {:pattern :dots} nil)))
    (is (nil? (draw/pattern-fill {:pattern :dots :pattern-color :none} :red))))
  (testing "a tile of its period"
    (let [{:keys [tile period]} (draw/pattern-fill {:pattern :dots} :red)]
      (is (instance? BufferedImage tile))
      (is (= period (.getWidth ^BufferedImage tile)))))
  (testing "fill-with-pattern! paints on an isolated canvas, the outer paint stays"
    (c2d/with-canvas [c (c2d/canvas 20 20)]
      (c2d/set-color c :blue)
      (let [g ^Graphics2D (:graphics c)
            paint (.getPaint g)
            drawn (atom nil)]
        (is (nil? (draw/fill-with-pattern! c nil (fn [_] (reset! drawn :called)))))
        (is (nil? @drawn) "no pattern - draw-fn not called")
        (draw/fill-with-pattern! c (draw/pattern-fill {:pattern :dots} :red)
                                 (fn [ic] (reset! drawn (.getPaint ^Graphics2D (:graphics ic))) (c2d/rect ic 0 0 20 20)))
        (is (instance? java.awt.TexturePaint @drawn))
        (is (= paint (.getPaint g)))))))

(deftest painted-color-test
  (c2d/with-canvas [c (c2d/canvas 10 10)]
    (c2d/set-background c :white)
    (c2d/set-color c :red)
    (c2d/rect c 5 5 5 5)
    (testing "through the canvas transform"
      (is (= (c/to-color :white) (c/to-color (draw/painted-color c 1.0 1.0))))
      (c2d/translate c 5 5)
      (is (= (c/to-color :red) (c/to-color (draw/painted-color c 0.5 0.5)))))
    (testing "boundary: outside the image - nil"
      (is (nil? (draw/painted-color c 5.0 5.0)))
      (is (nil? (draw/painted-color c -5.5 -5.5)))
      (is (some? (draw/painted-color c -5.0 -5.0))))))

(deftest contrast-colors-test
  (is (= (c/to-color :white) (c/to-color (draw/contrast-color :black))))
  (is (= (c/to-color :black) (c/to-color (draw/contrast-color :white))))
  (is (= (c/to-color :black) (c/to-color (draw/contrast-color (c/gray 128)))) "luma 128 - black")
  (is (= (c/to-color :white) (c/to-color (draw/plot-backdrop {:plot {:background :none}}))))
  (is (= :red (draw/plot-backdrop {:plot {:background :red}}))))

(defn- segments [^Shape s]
  (let [it (.getPathIterator s nil) buf (double-array 6)]
    (loop [acc []]
      (if (.isDone it) acc
          (let [t (.currentSegment it buf)] (.next it) (recur (conj acc [t (vec buf)])))))))

(deftest trail-shape-test
  (testing "one point - its disc; zero widths - nothing filled"
    (is (= [4.0 5.0 2.0 2.0] (vec (sh/rectangle2d->box (.getBounds2D (polyline/trail-shape [5.0] [6.0] [2.0]))))))
    (is (zero? (.getHeight (.getBounds2D (polyline/trail-shape [0.0 10.0] [0.0 0.0] [0.0 0.0])))) "a zero-area hull: nothing filled"))
  (testing "two points - discs and the hull in one non-zero path"
    (let [s (polyline/trail-shape [0.0 10.0] [0.0 0.0] [2.0 4.0])]
      (is (= Path2D/WIND_NON_ZERO (.getWindingRule ^Path2D s)))
      (is (= [-1.0 -2.0 13.0 4.0] (mapv #(m/approx % 6) (sh/rectangle2d->box (.getBounds2D s)))))))
  (testing "boundary: one disc inside the other - no hull"
    (is (= 2 (count (filter #(= PathIterator/SEG_CLOSE (first %)) (segments (polyline/trail-shape [0.0 0.5] [0.0 0.0] [10.0 1.0])))))))
  (testing "no points - empty"
    (is (empty? (segments (polyline/trail-shape [] [] []))))))

(deftest rings-path-test
  (let [outer [[0 0] [10 0] [10 10] [0 10]]
        hole [[3 3] [6 3] [6 6] [3 6]]
        p (polyline/rings-path [outer hole [[1 1] [2 2]]])]
    (testing "even-odd: the hole is open"
      (is (= Path2D/WIND_EVEN_ODD (.getWindingRule ^Path2D p)))
      (is (.contains ^Path2D p 1.0 1.0))
      (is (not (.contains ^Path2D p 4.5 4.5))))
    (testing "rings of fewer than 3 points left out"
      (is (= 2 (count (filter #(= PathIterator/SEG_CLOSE (first %)) (segments p))))))
    (testing "boundary: no rings - empty"
      (is (empty? (segments (polyline/rings-path [])))))))

(defn- java2d-bezier-points [run]
  (let [it (.getPathIterator ^Path2D (sh/path-bezier run false) nil 0.1)
        buf (double-array 6)]
    (loop [acc []]
      (if (.isDone it) acc
          (let [t (.currentSegment it buf)]
            (.next it)
            (recur (if (== t PathIterator/SEG_CLOSE) acc (conj acc [(aget buf 0) (aget buf 1)]))))))))

(deftest bezier-points-test
  (testing "the same points as the PathIterator flattening"
    (let [run [[0.0 0.0] [10.0 5.0] [20.0 0.0] [30.0 8.0]]]
      (is (= (java2d-bezier-points run) (curve/bezier-points run)))
      (is (vector? (curve/bezier-points run)))))
  (testing "boundary: fewer than 2 points as given"
    (is (= [] (curve/bezier-points [])))
    (is (= [[1.0 2.0]] (curve/bezier-points [[1.0 2.0]])))))

(deftest drawn-point-test
  (testing "rounded to 1e-6 pixel as Math/rint did"
    (doseq [v [154.99999999999997 0.5 -0.5 1.0000005 -2.4999995 123456.7891234]]
      (is (= (/ (Math/rint (* v 1.0e6)) 1.0e6) (first (curve/drawn-point [v 0.0]))) (str v))))
  (testing "boundary: clamped to the pixel limit; NaN stays"
    (is (= [1.0e6 -1.0e6] (curve/drawn-point [##Inf ##-Inf])))
    (is (m/nan? (first (curve/drawn-point [##NaN 0.0]))))))

(deftest sample-grid-test
  (is (= [3.0] (curve/sample-grid 3.0 3.0 1.0)) "equal ends")
  (is (= 11.5 (peek (curve/sample-grid 1.0 11.5 0.3))) "the last exactly p1")
  (is (= 4 (count (curve/sample-grid 0.0 1.0 {:count 4}))))
  (is (= [0.0 -1.0] (curve/sample-grid 0.0 -1.0 5.0)) "a step longer than the span - the ends"))

(deftest image-shape-test
  (testing "an :image shape draws as the AffineTransform drawImage did (sub-pixel position and scale kept)"
    (let [img (let [c (c2d/canvas 7 3)] (c2d/with-canvas-> c (c2d/set-background :red) (c2d/set-color :blue) (c2d/rect 0 0 3 3)) (c2d/get-image c))
          center (Vec2. 10.3 9.7)
          side 11.5
          ours (let [c (c2d/canvas 20 20)]
                 (c2d/with-canvas [cc c] (shape/draw! cc {:image img} center {:size side :color :black :stroke-color :none :stroke-width 0.0}))
                 (c2d/get-image c))
          reference (let [c (c2d/canvas 20 20)]
                      (c2d/with-canvas [cc c]
                        (let [k (/ side 7.0)
                              t (AffineTransform/getTranslateInstance (- 10.3 (* 0.5 k 7)) (- 9.7 (* 0.5 k 3)))]
                          (.scale t k k)
                          (.drawImage ^Graphics2D (:graphics cc) ^BufferedImage img t nil)))
                      (c2d/get-image c))]
      (is (= (argb reference) (argb ours))))))

(deftest text-and-path-shapes-test
  (testing "text outlines and path data still parse and draw"
    (is (nil? (shape/shape-problem {:text "A🐄"})))
    (is (nil? (shape/shape-problem {:path "M0 0 L10 0 L5 8 Z"})))
    (is (string? (shape/shape-problem {:path "M0 0 Q"})) "bad data - a problem, not a throw")
    (is (string? (shape/shape-problem {:path ""})) "empty data - draws nothing")))

(deftest fonts-test
  (is (font/known-font? "SansSerif"))
  (is (font/known-font? "DejaVu Sans Mono"))
  (is (not (font/known-font? 42)))
  (is (= 12.0 (double (.getSize2D (font/resolve-font "SansSerif" :bold 12)))))
  (is (= :size (:key (ex-data (try (font/resolve-font "SansSerif" :plain 0) (catch Exception e e)))))))

(deftest render-releases-and-clips-test
  (testing "render releases the target canvas (c2d/with-canvas)"
    (let [flush-graphics c2d/flush-graphics
          released (atom [])]
      (with-redefs [c2d/flush-graphics (fn [cv] (swap! released conj cv) (flush-graphics cv))]
        (let [result (plot/render {:theme {:plot {:margin 0}}} {:width 5 :height 5})]
          (is (some #(identical? result %) @released)))))))
