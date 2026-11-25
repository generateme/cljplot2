(ns cljplot2.batch-c3-fix-z1-test
  "Batch C3 review fixes (z1): label repel leaders (no leader crosses a label, leaders on the turned frame, one move
  rule, labels larger than the panel kept, candidates bounded by the panel), one leader helper and colour rule
  (`content.label/draw-leader!`), the label style and background along a path (`content.label/drawn-style`,
  `draw-along-path!`), glyphs by arc length on the baseline line (`text-path/baseline-path`), one
  `polyline/point-at-length`, the text chart's dependent-option rules and nilable options."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.chart :as chart]
            [cljplot2.content.label :as label]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.graphics.text-path :as text-path]
            [cljplot2.theme :as theme]
            [cljplot2.test-util :refer [error]])
  (:import [java.awt.image BufferedImage]))

(def ^:private size {:width 300 :height 200})

(defn- pixels
  "Gives a hash of the pixels of a rendered plot."
  [p]
  (let [^BufferedImage img (plot/render-image p size)
        w (.getWidth img) h (.getHeight img)]
    (hash (vec (.getRGB img 0 0 w h nil 0 w)))))

(defn- text-plot
  "Gives a plot of labels at rows `rows` (`:x :y :label`) with text options `options`."
  [rows options]
  {:data rows
   :charts [(chart/text (merge {:mapping {:x :x :y :y :label :label}} options))]
   :scales {:x {:domain [0 10]} :y {:domain [0 10]}}})

;; point along a polyline (D9, D18)

(deftest point-at-length-test
  (let [run [[0.0 0.0] [10.0 0.0] [10.0 0.0] [10.0 10.0]]
        lengths (polyline/cumulative-lengths run)]
    (testing "ends, inside, past the ends"
      (is (= [[0.0 0.0] 0 0.0] (polyline/point-at-length run lengths 0.0)))
      (is (= [[5.0 0.0] 0 0.0] (polyline/point-at-length run lengths 5.0)))
      (is (= [[10.0 10.0] 2 m/HALF_PI] (polyline/point-at-length run lengths 20.0)))
      (is (nil? (polyline/point-at-length run lengths -0.1)))
      (is (nil? (polyline/point-at-length run lengths 20.1))))
    (testing "a zero-length segment is skipped"
      (is (= [[10.0 5.0] 2 m/HALF_PI] (polyline/point-at-length run lengths 15.0))))
    (testing "no segment of positive length - nil"
      (is (nil? (polyline/point-at-length [[1.0 1.0]] [0.0] 0.0)))
      (is (nil? (polyline/point-at-length [[1.0 1.0] [1.0 1.0]] [0.0 0.0] 0.0)))))
  (testing "text-path has no point function of its own"
    (is (nil? (resolve 'cljplot2.graphics.text-path/point-at)))))

;; baseline line (P6)

(deftest baseline-path-test
  (testing "zero distance - the path; a straight path - shifted along its normal (right as drawn)"
    (is (= [[0.0 0.0] [10.0 0.0]] (text-path/baseline-path [[0.0 0.0] [10.0 0.0]] 0.0)))
    (is (= [[0.0 3.0] [10.0 3.0]] (text-path/baseline-path [[0.0 0.0] [10.0 0.0]] 3.0)))
    (is (= [[0.0 -3.0] [10.0 -3.0]] (text-path/baseline-path [[0.0 0.0] [10.0 0.0]] -3.0))))
  (testing "a right-angle corner is mitred: the offset lines meet"
    (let [[_ [^double x ^double y] _] (text-path/baseline-path [[0.0 0.0] [20.0 0.0] [20.0 20.0]] -2.0)]
      (is (m/delta-eq 22.0 x 1.0e-9))
      (is (m/delta-eq -2.0 y 1.0e-9))))
  (testing "a bend tighter than the distance: the line never steps back"
    (let [arc (mapv (fn [^double t] [(* 5.0 (m/cos t)) (* -5.0 (m/sin t))]) (m/slice-range m/PI 0.0 30))
          line (text-path/baseline-path arc 8.0)]
      (is (every? (fn [[[^double x0] [^double x1]]] (m/>= x1 x0)) (partition 2 1 line))))))

(deftest glyph-spacing-test
  (testing "glyph starts are their advances apart along the baseline line, on the inner side of a bend too"
    (let [wave (mapv (fn [^double t] [(* 40.0 t) (* -60.0 (m/sin t))]) (m/slice-range 0.0 m/PI 40))
          advances (mapv #(* 7.0 (double %)) (range 9))
          placed (text-path/glyph-placements wave advances {:baseline 10.0 :position 0.5})
          gaps (map (fn [[^double x0 ^double y0] [^double x1 ^double y1]] (m/hypot (- x1 x0) (- y1 y0))) placed (rest placed))]
      (is (= 8 (count placed)))
      (is (every? #(m/delta-eq 7.0 (double %) 0.2) gaps) "chords of 7 px arcs: no piling, no spreading"))))

;; repel leaders (decision 3, P1-P5)

(defn- crossings
  "Gives the pairs `[leader-index box-index]` where a leader crosses another label's moved box."
  [boxes placements]
  (let [moved (vec (map (fn [{:keys [x y] :as b} p]
                          (when-let [[dx dy] (:shift p)] (assoc b :x (+ x dx) :y (+ y dy))))
                        boxes placements))]
    (for [i (range (count placements))
          :let [leader (:leader (placements i))]
          :when leader
          j (range (count moved))
          :when (and (not= i j) (moved j) (measure/box-crosses-path? (moved j) leader))]
      [i j])))

(deftest leaders-never-cross-labels-test
  (let [boxes (vec (for [i (range 40)] {:x (+ 100.0 (* 3.0 (mod i 5))) :y (+ 60.0 (* 2.0 (mod i 7))) :w 34.0 :h 12.0}))
        points (mapv (fn [{:keys [x y]}] [(- x 4.0) (+ y 6.0)]) boxes)
        area {:w 300.0 :h 200.0 :distance 80.0 :leaders true}
        placements (measure/repelled-placements boxes points area)]
    (testing "some labels moved with leaders, none crossing another label's box"
      (is (some :leader placements))
      (is (empty? (crossings boxes placements))))
    (testing "deterministic"
      (is (= placements (measure/repelled-placements boxes points area))))
    (testing "without leaders more labels may be placed, never fewer"
      (is (<= (count (filter some? placements))
              (count (filter some? (measure/repelled-placements boxes points (dissoc area :leaders)))))))))

(deftest leader-move-rule-test
  (let [box {:x 100.0 :y 50.0 :w 40.0 :h 10.0}
        area {:w 300.0 :h 200.0 :distance 60.0 :leaders true}]
    (testing "a label moved at most 6 px has no leader, whatever its offset from its point"
      (let [[_ {[_ ^double dy] :shift leader :leader}] (measure/repelled-placements [box (update box :y + 6.0)]
                                                                                    [[60.0 55.0] [60.0 61.0]] area)]
        (is (<= (m/abs dy) 6.0))
        (is (nil? leader))))
    (testing "a label moved farther has one, from its point"
      (let [[_ {:keys [leader]}] (measure/repelled-placements [box box] [[60.0 55.0] [60.0 55.0]] area)]
        (is (= [60.0 55.0] (first leader)))))))

(deftest leader-on-turned-frame-test
  (testing "a rotated label's leader ends on its turned frame, not its bounding box"
    (let [frame [[120.0 40.0] [140.0 60.0] [120.0 80.0] [100.0 60.0]]
          box {:x 100.0 :y 40.0 :w 40.0 :h 40.0}
          [_ {[^double dx ^double dy] :shift [_ [^double ex ^double ey]] :leader}]
          (measure/repelled-placements [box box] [[60.0 60.0] [60.0 60.0]]
                                       {:w 300.0 :h 200.0 :distance 80.0 :leaders true :frames [frame frame]})
          ;; distance of the end from the moved diamond's edge |x - cx| + |y - cy| = 20
          on-edge (+ (m/abs (- ex (+ 120.0 dx))) (m/abs (- ey (+ 60.0 dy))))]
      (is (m/delta-eq 20.0 on-edge 1.0e-6)))))

(deftest repel-bounds-test
  (testing "a box larger than the panel stays unmoved and takes its place"
    (is (= [{:shift [0.0 0.0] :leader nil} nil]
           (measure/repelled-placements [{:x 0.0 :y 0.0 :w 400.0 :h 10.0} {:x 10.0 :y 0.0 :w 10.0 :h 10.0}]
                                        [[0.0 5.0] [10.0 5.0]] {:w 300.0 :h 10.0 :distance 60.0 :leaders true}))))
  (testing "two labels and a huge distance stay fast (candidates bounded by the panel)"
    (let [t0 (System/nanoTime)]
      (plot/render-image (text-plot [{:x 5 :y 5 :label "a"} {:x 5 :y 5 :label "b"}]
                                    {:overlapping-labels :repel :repel-distance 1.0e7 :label-leader true})
                         size)
      (is (< (/ (- (System/nanoTime) t0) 1.0e9) 2.0)))))

;; one leader helper, one colour rule (D3)

(defn- pixel-at
  "Gives the colour at a pixel of a canvas after `f` draws on it."
  [f x y]
  (c2d/with-canvas [c (c2d/canvas 40 40 :low)]
    (c2d/set-background c :white)
    (f c)
    (c/to-color (draw/painted-color c (double x) (double y)))))

(deftest draw-leader-test
  (let [env {:theme theme/default-theme}
        draw #(pixel-at (fn [c] (label/draw-leader! c % [[0 20.5] [40 20.5]] env)) 20 20)]
    (testing "the label's inline colour"
      (is (= (c/to-color [200 0 0]) (draw {:kind :label :role :annotation :color [200 0 0]}))))
    (testing "else its role's colour, :contrast resolved against the plot backdrop"
      (is (= (c/to-color (:color (label/drawn-style theme/default-theme {:role :annotation})))
             (draw {:kind :label :role :annotation})))
      (is (= (c/to-color :black) (draw {:kind :label :role :annotation :color :contrast}))))))

;; label style along a path (D10)

(def ^:private series (vec (for [t (range 11)] {:t t :v (* 0.5 t)})))

(defn- path-plot
  "Gives a line with a text along it, text options `options`, merged with `extra`."
  ([options] (path-plot options nil))
  ([options extra]
   (merge {:data series :mapping {:x :t :y :v}
           :charts [(chart/line) (chart/text (merge {:x (mapv :t series) :y (mapv :v series) :text "along the line"
                                                     :along-path true}
                                                    options))]}
          extra)))

(deftest along-path-style-test
  (testing "drawn-style resolves the font, colour and background of the role and inline keys"
    (let [st (label/drawn-style theme/default-theme {:role :annotation :background [1 2 3] :inset 4})]
      (is (instance? java.awt.Font (:font st)))
      (is (= 4.0 (:inset st)))
      (is (= [1 2 3] (:background st)))))
  (testing "an inline :background fills a band under the glyphs; the role's background too"
    (let [plain (pixels (path-plot {}))]
      (is (not= plain (pixels (path-plot {:background [255 240 160] :inset 2}))))
      (is (= plain (pixels (path-plot {:background :none}))))
      (is (not= plain (pixels (path-plot {} {:theme {:text {:annotation {:background [200 230 255]}}}}))))))
  (testing "an exactly vertical path reads the same either way"
    (let [vertical (fn [ys] {:data [{:x 5 :y 0} {:x 5 :y 10}] :mapping {:x :x :y :y}
                             :charts [(chart/text {:x [5 5] :y ys :text "vertical" :along-path true})]})]
      (is (= (pixels (vertical [0 10])) (pixels (vertical [10 0])))))))

;; rules, nilable options, descriptions (D1, D19)

(deftest text-rules-test
  (let [one [{:x 5 :y 5 :label "label"}]]
    (testing "options needing another"
      (is (= :repel-distance (:key (error #(plot/build (text-plot one {:repel-distance 30}) size)))))
      (is (= :repel-distance (:key (error #(plot/build (text-plot one {:overlapping-labels :drop :repel-distance 30}) size)))))
      (is (= :label-leader (:key (error #(plot/build (text-plot one {:label-leader true}) size)))))
      (is (= :path-position (:key (error #(plot/build (text-plot one {:path-position 0.3}) size)))))
      (is (= :path-position (:key (error #(plot/build (text-plot one {:along-path false :path-position 0.3}) size))))))
    (testing "accepted: with their option, false / nil / default values"
      (is (map? (plot/build (text-plot one {:repel-distance 60 :path-position 0.5}) size)))
      (is (map? (plot/build (text-plot one {:overlapping-labels :repel :repel-distance 30 :label-leader true}) size)))
      (is (map? (plot/build (text-plot one {:label-leader false}) size)))
      (is (map? (plot/build (path-plot {:path-position 0.3}) size)))
      (is (map? (plot/build (path-plot {:background [255 255 255] :inset 2}) size))))
    (testing "every new option takes nil - the default"
      (is (= (pixels (text-plot one {}))
             (pixels (text-plot one {:repel-distance nil :label-leader nil :along-path nil :path-position nil}))))
      (is (= (pixels (path-plot {})) (pixels (path-plot {:path-position nil}))))
      (is (= (pixels (text-plot one {:overlapping-labels :repel}))
             (pixels (text-plot one {:overlapping-labels :repel :repel-distance nil :label-leader nil})))))
    (testing "the :overlapping-labels description names :repel"
      (let [row (first (filter #(= [:overlapping-labels] (:path %)) (describe/options (chart/text))))]
        (is (re-find #":repel" (str (:description row))))))))
