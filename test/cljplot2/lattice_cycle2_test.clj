(ns cljplot2.lattice-cycle2-test
  "Lattice cycle 2 (marks): tile cell sizes from `:size`, contour level labels, the Tufte box variant, nested
  arranges of scatter-plot matrices."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.chart :as chart]
            [cljplot2.chart.box :as box]
            [cljplot2.chart.contour :as contour]
            [cljplot2.chart.tile :as tile]
            [cljplot2.content.label :as label]
            [cljplot2.core :as plot]
            [cljplot2.data.cells :as cells]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.graphics.shape :as shape]
            [cljplot2.layout.grid :as grid]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.position :as scale]
            [cljplot2.theme :as theme]
            [cljplot2.test-util :refer [error]])
  (:import [java.awt.image BufferedImage]))

(defn- built [p] (plot/build p {:width 500 :height 400}))
(defn- image ^BufferedImage [p] (c2d/get-image (plot/render p {:width 300 :height 240})))
(defn- painted
  "Count of pixels of `img` that differ from the top-left pixel (the background)."
  ^long [^BufferedImage img]
  (let [bg (.getRGB img 0 0)]
    (count (for [x (range (.getWidth img)) y (range (.getHeight img)) :when (not= bg (.getRGB img x y))] 1))))

(defn- calls
  "Calls of the var `v` (its args) made while running `f`, the original still called."
  [v f]
  (let [log (atom [])
        original @v]
    (with-redefs-fn {v (fn [& args] (swap! log conj (vec args)) (apply original args))} f)
    @log))

;; 1 tile :size

(def ^:private cells-data [{:x 0 :y 0 :c 1.0 :s 1.0} {:x 1 :y 0 :c 2.0 :s 4.0}])

(deftest tile-size-test
  (testing "1.1 factor = size / largest size of the range: 1 and 4 on [2 8] -> 0.25 and 1.0"
    (let [b (built {:data cells-data :mapping {:x :x :y :y :color :c :size :s} :charts [(chart/tile)]
                    :scales {:size {:range [2 8]}}})
          spec (get-in b [:finalized-scales :size])
          size-of (aesthetic/attribute-fn :size spec nil)]
      (is (= 8.0 (tile/largest-size spec)))
      (is (= [0.25 1.0] (mapv #(tile/size-factor (size-of %) (tile/largest-size spec)) [1.0 4.0])))))
  (testing "1.1 largest size: a level map range, identity and no scale"
    (is (= 6.0 (tile/largest-size {:kind :ordinal :range {"a" 2 "b" 6}})))
    (is (= 1.0 (tile/largest-size {:kind :identity})))
    (is (= 1.0 (tile/largest-size nil)))
    (is (= 0.0 (tile/size-factor 3.0 0.0)))
    (is (= 1.0 (tile/size-factor 12.0 8.0)) "clamped"))
  (testing "1.1 nil and out-of-scale sizes dropped and counted"
    (let [rows (conj cells-data {:x 2 :y 0 :c 3.0 :s nil} {:x 3 :y 0 :c 3.0 :s 9.0})
          b (built {:data rows :mapping {:x :x :y :y :color :c :size :s} :charts [(chart/tile)]
                    :scales {:size {:domain [0 5]}}})]
      (is (= 2 (get-in b [:stats 0 :dropped :size])))
      (is (= 0 (get-in b [:stats 0 :dropped :color])))))
  (testing "1.1 a size of 0 draws no cell (identity sizes are the fractions)"
    (let [p (fn [s] {:data [{:x 0 :y 0 :c 1.0 :s s} {:x 1 :y 0 :c 1.0 :s 1.0}] :mapping {:x :x :y :y :color :c :size :s}
                     :charts [(chart/tile)] :scales {:size {:kind :identity}} :legend false})]
      (is (< (painted (image (p 0.0))) (painted (image (p 0.5)))))))
  (testing "1.2 size legend keys grow with size (grey without a color)"
    (let [canvas (c2d/canvas 40 40)
          env {:theme theme/default-theme :scales {:size {:kind :linear :range [2 8] :domain [0 1]}}}
          key-pixels (fn [s] (let [c (c2d/canvas 40 40)]
                               (c2d/with-canvas-> c (c2d/set-background :white))
                               (c2d/with-canvas [cv c] (tile/legend (chart/tile) {:size s} cv {:x 0 :y 0 :w 40 :h 40} env))
                               (painted (c2d/get-image c))))]
      (is (some? canvas))
      (is (< (key-pixels 2.0) (key-pixels 8.0)))
      (is (zero? (key-pixels 0.0)))))
  (testing "1.3 seamless (non-antialiased) rendering only without :size"
    (let [p {:data cells-data :mapping {:x :x :y :y :color :c} :charts [(chart/tile)]}]
      (is (= 1 (count (calls #'cells/call-with-cell-rendering #(image p)))))
      (is (= 0 (count (calls #'cells/call-with-cell-rendering #(image (assoc-in p [:mapping :size] :s))))))))
  (testing "1.4 value labels stay at the cell centres when cells shrink"
    (let [p {:data cells-data :mapping {:x :x :y :y :color :c} :charts [(chart/tile {:value-label true})]
             :legend false}
          at (fn [p] (mapv (fn [[_ _ _ x y]] [x y]) (calls #'label/draw-at! #(image p))))]
      (is (= 2 (count (at p))))
      (is (= (at p) (at (assoc-in p [:mapping :size] :s)))))))

;; 2 contour labels

(defn- circle-plot [opts]
  {:charts [(chart/contour (merge {:f (fn [[x y]] (+ (* x x) (* y y))) :domain {:x [-2 2] :y [-2 2]} :levels [1.0 2.0]} opts))]})

(defn- label-texts [p] (mapv #(get-in % [1 :text]) (calls #'label/draw-at! #(image p))))

(deftest contour-labels-test
  (testing "2.1 true - the level; a function - its text; nil - no label; default none"
    (is (= ["1" "2"] (label-texts (circle-plot {:labels true}))))
    (is (= ["r²=2"] (label-texts (circle-plot {:labels #(when (= 2.0 (:level %)) (str "r²=" (m/round (:level %))))}))))
    (is (empty? (label-texts (circle-plot {})))))
  (testing "2.1 labels need line contours"
    (is (= :labels (:key (error #(built (circle-plot {:labels true :fill true})))))))
  (testing "2.1 value text: numbers to 2 decimals"
    (is (= ["1.5" "1" "0.33" "a" nil] (mapv label/value-text [1.5 1.0 (/ 1.0 3) :a nil]))))
  (testing "2.2 the label point is halfway along the line: on a circle, at its radius"
    (let [ring (vec (for [i (range 65)] (let [a (* 2.0 m/PI (/ i 64.0))] [(+ 50 (* 20 (m/cos a))) (+ 50 (* 20 (m/sin a)))])))
          [[x y] length] (contour/line-midpoint ring)]
      (is (< (m/abs (- 20.0 (m/hypot (- x 50.0) (- y 50.0)))) 0.5))
      (is (< (m/abs (- (* 2.0 m/PI 20.0) length)) 0.5))
      (is (nil? (contour/line-midpoint [[0 0]])))
      (is (nil? (contour/line-midpoint [[0 0] [##NaN 1]])))))
  (testing "2.3 a line too short for its label and overlapping labels get none"
    (is (empty? (label-texts (circle-plot {:labels true :levels [0.001]}))) "a tiny circle")
    (let [tight (circle-plot {:labels true :levels (vec (range 0.5 3.0 0.02))})]
      (is (< 0 (count (label-texts tight)) 125))))
  (testing "2.3 labels leaving the panel are skipped"
    (is (empty? (label-texts (circle-plot {:labels true :levels [7.9]}))) "arcs in the corners only"))
  (testing "2.4 every line is stroked before the first label"
    (let [log (atom [])
          stroke @#'polyline/stroke-runs!
          draw-at @#'label/draw-at!]
      (with-redefs-fn {#'polyline/stroke-runs! (fn [& args] (swap! log conj :line) (apply stroke args))
                       #'label/draw-at! (fn [& args] (swap! log conj :label) (apply draw-at args))}
        #(image (circle-plot {:labels true})))
      (is (= [:line :line :label :label] @log)))))

;; 3 Tufte box

(def ^:private box-rows
  (vec (for [g ["a" "b"] i (range 41)] {:g g :lvl "L" :v (+ (* 0.1 i) (if (= g "a") 0.0 1.0))})))

(deftest tufte-box-test
  (testing "3.1 no box: no rectangles; two lines per item, none inside the inter-quartile range"
    (let [p {:data box-rows :mapping {:x :lvl :y :v} :charts [(chart/box {:variant :tufte})]}
          b (plot/build p {:width 300 :height 240})
          {:keys [q1 q3]} (box/box-stats (map :v box-rows))
          forward (scale/forward-fn (get-in b [:finalized-scales :y]))
          [lo hi] (sort [(forward q1) (forward q3)])
          lines (filter (fn [args] (and (= 3 (count args)) (instance? fastmath.vector.Vec2 (nth args 1))))
                        (calls #'c2d/line #(image p)))
          lines (mapv (fn [[_ a b]] [nil (.x ^fastmath.vector.Vec2 a) (.y ^fastmath.vector.Vec2 a) (.x ^fastmath.vector.Vec2 b) (.y ^fastmath.vector.Vec2 b)]) lines)]
      (is (empty? (calls #'c2d/rect #(image p))))
      (is (= 2 (count lines)))
      (is (every? (fn [[_ _ y1 _ y2]] (let [a (min y1 y2) b (max y1 y2)] (or (<= b (+ lo 1.0)) (>= a (- hi 1.0))))) lines))))
  (testing "3.1 the classic box is the default"
    (is (seq (calls #'c2d/rect #(image {:data box-rows :mapping {:x :lvl :y :v} :charts [(chart/box)]})))))
  (testing "3.1 an unknown variant is a schema error"
    (is (= :variant (:key (error #(built {:data box-rows :mapping {:x :v} :charts [(chart/box {:variant :notched})]}))))))
  (testing "3.2 vertical, grouped (dodged): each median point at its own position"
    (let [p {:data box-rows :mapping {:x :lvl :y :v :color :g} :charts [(chart/box {:variant :tufte :outliers false})]
             :legend false}
          centres (map (fn [args] (.x ^fastmath.vector.Vec2 (nth args 2))) (calls #'shape/draw! #(image p)))]
      (is (= 2 (count centres)))
      (is (apply distinct? centres))))
  (testing "3.2 flipped"
    (is (instance? BufferedImage (image {:data box-rows :mapping {:x :v :y :lvl} :charts [(chart/box {:variant :tufte})] :flip true}))))
  (testing "3.3 legend glyph: a point between two short lines"
    (let [c (c2d/canvas 30 20)
          env {:theme theme/default-theme}
          shapes (atom 0) lines (atom 0)
          draw-shape @#'shape/draw! line @#'c2d/line]
      (with-redefs-fn {#'shape/draw! (fn [& args] (swap! shapes inc) (apply draw-shape args))
                       #'c2d/line (fn [& args] (swap! lines inc) (apply line args))}
        #(c2d/with-canvas [cv c] (box/legend (chart/box {:variant :tufte}) {:color [200 0 0]} cv {:x 0 :y 0 :w 30 :h 20} env)))
      (is (= [1 2] [@shapes @lines])))))

;; 4 nested arrange

(def ^:private iris-like
  (vec (for [i (range 60)] {:sp (["s" "v" "g"] (mod i 3)) :a (m/sin i) :b (m/cos (* 0.7 i)) :c (* 0.01 i i)})))

(defn- panel-sizes
  "Panel `[w h]` of every cell sub-plot of a built splom `b` (cells with a panel)."
  [b]
  (keep (fn [cell] (when-let [{:keys [w h]} (grid/find-content-box (:grid cell) (:layout cell) #(and (= :panel (:kind %)) (nil? (:margin %))))]
                     [w h]))
        (vals (:subplots b))))

(deftest nested-arrange-test
  (let [variables [:a :b :c]
        sploms (mapv (fn [sp] (plot/pairs (filterv #(= sp (:sp %)) iris-like) {:variables variables :title sp})) ["s" "v" "g"])
        nested (plot/arrange (conj sploms (plot/pairs iris-like {:variables variables})) {:columns 2})]
    (testing "4.1 an arrange of scatter-plot matrices builds and renders"
      (is (instance? BufferedImage (c2d/get-image (plot/render nested {:width 700 :height 600})))))
    (testing "4.2 panels stay equal inside every inner matrix (within the 3 px a standalone matrix of this size spreads)"
      (let [b (plot/build nested {:width 700 :height 600})]
        (is (= 4 (count (:subplots b))))
        (doseq [inner (vals (:subplots b))
                :let [sizes (panel-sizes inner)]]
          (is (= 9 (count sizes)))
          (is (<= (- (reduce max (map first sizes)) (reduce min (map first sizes))) 3.0))
          (is (<= (- (reduce max (map second sizes)) (reduce min (map second sizes))) 3.0)))))))

;; review fixes

(deftest review-fixes-test
  (testing "H1 axis-domain: display only - the axis shows the domain's ticks over the same pixels"
    (let [f (scale/finalize (scale/scale-spec :y {:domain [0 1] :axis-domain [10 20]}) [100.0 0.0] 5)
          shown (scale/display-scale (assoc f :direction :vertical))]
      (is (= [0 1] (mapv long (:domain f))) "the chart's scale")
      (is (= [10.0 20.0] (mapv double (:domain shown))))
      (is (= :vertical (:direction shown)))
      (is (every? #(<= 10.0 (double %) 20.0) (:ticks shown)))
      (let [g (scale/finalize (scale/scale-spec :y {:domain [0 1]}) [100.0 0.0] 5)]
        (is (= g (scale/display-scale g)) "unchanged without :axis-domain"))))
  (testing "H1 axis-domain rules: two different numbers, numeric continuous scales"
    (is (= :axis-domain (:key (error #(built {:data [{:x 0 :y 0} {:x 1 :y 1}] :mapping {:x :x :y :y} :charts [(chart/scatter)]
                                               :scales {:y {:axis-domain [3 3]}}})))))
    (is (= :axis-domain (:key (error #(built {:data [{:x "a" :y 0}] :mapping {:x :x :y :y} :charts [(chart/scatter)]
                                               :scales {:x {:axis-domain [0 1]}}}))))))
  (testing "H1 the splom's top-left cell shows the first variable on its y axis; other diagonals its grid"
    (let [b (plot/build (plot/pairs iris-like {:variables [:a :b :c]}) {:width 500 :height 450})
          y-of (fn [k] (get-in b [:subplots [:plot k] :finalized-scales :y]))
          a-domain (get-in (y-of 1) [:domain])]
      (is (= true (get-in b [:subplots [:plot 0] :scale-specs :y :axis])) "an axis, not only grid lines")
      (is (= (mapv double a-domain) (mapv double (:domain (scale/display-scale (y-of 0))))) "the row's domain")
      (is (= :grid (:axis (scale/display-scale (y-of 4)))) "an inner diagonal keeps grid lines only")))
  (testing "H2 equal panels in a splom at any size"
    (doseq [[w h] [[350 300] [301 257] [600 500]]]
      (let [b (plot/build (plot/pairs iris-like {:variables [:a :b :c]}) {:width w :height h})]
        (is (= 1 (count (distinct (panel-sizes b)))) (str w "x" h)))))
  (testing "H2 an equal-panels arrange given by rows"
    (let [cells (vec (for [i (range 4)] {:data [{:x 0 :y (* i 1000)} {:x 1 :y 1}] :mapping {:x :x :y :y} :charts [(chart/scatter)]}))
          b (plot/build (plot/arrange cells {:rows 2 :equal-panels true}) {:width 400 :height 300})]
      (is (= 1 (count (distinct (panel-sizes b)))))))
  (testing "H3 only drawn lines get labels"
    (is (= ["2"] (label-texts (assoc-in (circle-plot {:labels true}) [:scales :color :domain] [1.5 3.0])))))
  (testing "H7 of two overlapping labels the lower level's is kept"
    (is (= ["0"] (label-texts {:charts [(chart/contour {:f (fn [[x _]] x) :domain {:x [-1 1] :y [-1 1]} :levels [0.0 0.02] :labels true})]})))))

