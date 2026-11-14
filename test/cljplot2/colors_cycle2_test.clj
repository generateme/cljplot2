(ns cljplot2.colors-cycle2-test
  "Colors cycle 2: rect colours, line colored along its length, color vision deficiency preview."
  (:require [clojure.test :refer [deftest testing is]]
            [cljplot2.test-util :refer [error]]
            [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [cljplot2.chart :as chart]
            [cljplot2.chart.rect :as rect]
            [cljplot2.core :as plot]
            [cljplot2.data.facet :as facet]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.layout.grid :as grid])
  (:import [java.awt.image BufferedImage]))

(def ^:private size {:width 400 :height 300})
(defn- build [p] (plot/build p size))
(def ^:private white {:plot {:background :white}})

(defn- rgba [^BufferedImage img x y]
  (let [v (.getRGB img (int x) (int y))]
    [(bit-and (bit-shift-right v 16) 0xff) (bit-and (bit-shift-right v 8) 0xff) (bit-and v 0xff)
     (bit-and (bit-shift-right v 24) 0xff)]))
(defn- rgb [img x y] (vec (take 3 (rgba img x y))))
(defn- near [a b] (every? true? (map (fn [x y] (<= (Math/abs (- (long x) (long y))) 3)) a b)))

(defn- panel-box [p] (grid/item-box (:layout (build p)) :panel))

(defn- panel-pixel
  "Color at fraction `fx`, `fy` (from the top-left) of the panel of plot `p`."
  [p fx fy]
  (let [{:keys [x y w h]} (panel-box p)
        img (plot/render-image p size)]
    (rgb img (+ x (* fx w)) (+ y (* fy h)))))

;; 1. rect colours

(def ^:private bands [{:y0 0 :y1 1 :c :red} {:y0 1 :y1 2 :c :blue}])

(defn- band-plot
  ([scales] (band-plot bands scales {}))
  ([data scales options]
   {:data data :theme white :scales scales
    :charts [(chart/rect (merge {:mapping {:y0 :y0 :y1 :y1 :color :c}} options))]}))

(deftest rect-color-test
  (testing "1.1 identity colors fill the rects (bands across the panel: no x pair)"
    (let [p (band-plot {:color {:kind :identity}})]
      (is (near (rgb (plot/render-image p size) 0 0) [255 255 255]) "background")
      (is (near (panel-pixel p 0.5 0.75) [255 0 0]) "lower band red")
      (is (near (panel-pixel p 0.5 0.25) [0 0 255]) "upper band blue")))
  (testing "1.1 ordinal colors with a legend that lists the rect chart"
    (let [b (build (band-plot [{:y0 0 :y1 1 :c "a"} {:y0 1 :y1 2 :c "b"}] {} {}))
          legend (some #(when (= :legends (:id %)) (first (get-in % [:content :legends]))) (get-in b [:grid :items]))]
      (is (= ["a" "b"] (mapv :label (:entries legend))))
      (is (= [{:chart 0 :dims [:color]}] (:charts legend)))))
  (testing "1.1 linear and binned color scales"
    (let [data [{:y0 0 :y1 1 :v 0} {:y0 1 :y1 2 :v 10}]
          p (fn [scale] {:data data :theme white :scales {:color scale}
                         :charts [(chart/rect {:mapping {:y0 :y0 :y1 :y1 :color :v}})]})]
      (is (near (panel-pixel (p {:range [[255 0 0] [0 0 255]]}) 0.5 0.25) [0 0 255]))
      (is (near (panel-pixel (p {:kind :binned :bins 2 :range [[255 0 0] [0 0 255]]}) 0.5 0.75) [255 0 0]))))
  (testing "1.1 NA color value: the NA color; outside an explicit domain: dropped"
    (let [na (vec (take 3 (map long (c/to-color (get-in (build (band-plot {})) [:theme :palette :na])))))]
      (is (near (panel-pixel (band-plot [{:y0 0 :y1 1 :c nil} {:y0 1 :y1 2 :c "b"}] {} {}) 0.5 0.75) na)))
    (let [b (build (band-plot [{:y0 0 :y1 1 :c "a"} {:y0 1 :y1 2 :c "b"}] {:color {:domain ["b"]}} {}))]
      (is (= 1 (get-in b [:stats 0 :dropped :color])))
      (is (= 1 (count (get-in b [:stats 0 :data facet/single-panel :rects]))))))
  (testing "1.2 unmapped: the theme fill (no :aesthetics on the rects)"
    (let [b (build {:data bands :charts [(chart/rect {:mapping {:y0 :y0 :y1 :y1}})]})]
      (is (= [{:y [0 1]} {:y [1 2]}] (get-in b [:stats 0 :data facet/single-panel :rects])))))
  (testing "1.3 literal fill vectors, paired 1 or n"
    (let [p {:theme white :data [{:x 0 :y 0} {:x 3 :y 1}] :mapping {:x :x :y :y}
             :charts [(chart/scatter {:size 0}) (chart/rect {:x [[0 1] [1 2] [2 3]] :fill [:red :lime :blue]})]}]
      (is (near (panel-pixel p 0.1 0.5) [255 0 0]))
      (is (near (panel-pixel p 0.5 0.5) [0 255 0]))
      (is (near (panel-pixel p 0.9 0.5) [0 0 255])))
    (is (= [{:x [0 1] :fill :red} {:x [1 2] :fill :red}]
           (get-in (build {:charts [(chart/rect {:x [[0 1] [1 2]] :fill [:red]})]}) [:stats 0 :data facet/all-panels :rects]))
        "one color for every interval")
    (is (= [{:x [0 1]}] (get-in (build {:charts [(chart/rect {:x [0 1] :fill [255 0 0]})]}) [:stats 0 :data facet/all-panels :rects]))
        "a vector that is a color is one fill (the style)")
    (is (= :fill (:key (error #(build {:charts [(chart/rect {:x [[0 1] [1 2] [2 3]] :fill [:red :blue]})]})))))
    (is (= :fill (:key (error #(build {:charts [(chart/rect {:x [0 1] :fill [:red :no-such-color]})]})))))
    (is (= [{:x [0 1]}] (get-in (build {:charts [(chart/rect {:x [0 1] :fill []})]}) [:stats 0 :data facet/all-panels :rects]))
        "[] is a clojure2d color (black), so one fill"))
  (testing "1.4 :alpha on mapped and literal fills"
    (is (near (panel-pixel (band-plot bands {:color {:kind :identity}} {:alpha 0.25}) 0.5 0.75) [255 191 191]))
    (is (near (panel-pixel (band-plot bands {:color {:kind :identity}} {:alpha 0}) 0.5 0.75) [255 255 255]) "alpha 0")
    (is (near (panel-pixel (band-plot bands {:color {:kind :identity}} {:alpha 1}) 0.5 0.75) [255 0 0]) "alpha 1")
    (is (= :alpha (:key (error #(build (band-plot bands {:color {:kind :identity}} {:alpha 1.5})))))))
  (testing "1.5 legend glyph: a filled square with the stroke"
    (let [canvas (c2d/canvas 20 20)]
      (c2d/with-canvas-> canvas (c2d/set-background :white))
      (c2d/with-canvas [c canvas]
        (rect/legend {:options {:stroke-color :black :stroke-width 1}} {:color :red} c {:x 0 :y 0 :w 20 :h 20}
                     {:theme (:theme (build {:charts [(chart/rect {:x [0 1]})]}))}))
      (let [img (c2d/get-image canvas)]
        (is (near (rgb img 10 10) [255 0 0]) "filled")
        (is (near (rgb img 1 1) [255 255 255]) "inside the box margin")
        (is (near (rgb img 3 10) [0 0 0]) "stroke on the edge"))))
  (testing "1.6 :shape only groups"
    (let [b (build {:data [{:y0 0 :y1 1 :s "a"} {:y0 1 :y1 2 :s "b"}]
                    :charts [(chart/rect {:mapping {:y0 :y0 :y1 :y1 :shape :s}})]})]
      (is (= 2 (count (get-in b [:stats 0 :data facet/single-panel :rects]))))
      (is (some? (plot/render-image {:data [{:y0 0 :y1 1 :s "a"}] :charts [(chart/rect {:mapping {:y0 :y0 :y1 :y1 :shape :s}})]} size)))))
  (testing "invalid ends still counted under :position"
    (is (= 1 (get-in (build {:data [{:y0 0 :y1 1} {:y0 "x" :y1 ##NaN}] :charts [(chart/rect {:mapping {:y0 :y0 :y1 :y1}})]})
                     [:stats 0 :dropped :position])))))

;; 2. line gradient

(def ^:private red-blue {:range [[255 0 0] [0 0 255]]})

(defn- line-plot
  ([data] (line-plot data {:color red-blue} {}))
  ([data scales options]
   {:data data :mapping {:x :x :y :y :color :t} :scales scales :theme white
    :charts [(chart/line (merge {:width 6} options))]}))

(deftest line-gradient-test
  (testing "2.1 a linear color doesn't group: one line with point colors"
    (let [b (build (line-plot [{:x 0 :y 0 :t 0} {:x 1 :y 1 :t 5} {:x 2 :y 0 :t 10}]))
          lines (get-in b [:stats 0 :data facet/single-panel :lines])]
      (is (= 1 (count lines)))
      (is (= [0 5 10] (:colors (first lines))))
      (is (= :colorbar (:kind (first (some #(when (= :legends (:id %)) (get-in % [:content :legends])) (get-in b [:grid :items]))))))))
  (testing "2.1 :group still groups"
    (let [b (build (assoc-in (line-plot [{:x 0 :y 0 :t 0 :id 1} {:x 1 :y 1 :t 5 :id 1} {:x 0 :y 1 :t 3 :id 2} {:x 1 :y 0 :t 9 :id 2}])
                             [:mapping :group] :id))]
      (is (= [[0 5] [3 9]] (mapv :colors (get-in b [:stats 0 :data facet/single-panel :lines]))))))
  (testing "2.2 segment color: the mean of its ends (horizontal line, purple in the middle)"
    (let [p (line-plot [{:x 0 :y 0 :t 0} {:x 10 :y 0 :t 10}])]
      (is (near (panel-pixel p 0.5 0.5) [128 0 127]))))
  (testing "2.2 binned: the bin of the mean"
    (let [p (line-plot [{:x 0 :y 0 :t 0} {:x 10 :y 0 :t 10}] {:color (merge red-blue {:kind :binned :bins [0 6 10]})} {})]
      (is (near (panel-pixel p 0.5 0.5) [255 0 0]))))
  (testing "2.3 round caps: the outer corner of a right angle is covered"
    (let [canvas (c2d/canvas 40 40)]
      (c2d/with-canvas-> canvas (c2d/set-background :white))
      (c2d/with-canvas [c canvas] (polyline/draw-segments! c [5 20 20] [20 20 5] [:red :red] {:width 10}))
      (is (near (rgb (c2d/get-image canvas) 23 23) [255 0 0])))
    (let [canvas (c2d/canvas 40 40)]
      (c2d/with-canvas-> canvas (c2d/set-background :white))
      (c2d/with-canvas [c canvas] (polyline/draw-segments! c [5 20 35] [20 ##NaN 20] [:red :red] {:width 4}))
      (is (near (rgb (c2d/get-image canvas) 20 20) [255 255 255]) "a NaN point breaks the line")
      (c2d/with-canvas [c canvas] (polyline/draw-segments! c [5 35] [30 30] [nil] {:width 4}))
      (is (near (rgb (c2d/get-image canvas) 20 30) [255 255 255]) "a nil color skips the segment")
      (c2d/with-canvas [c canvas] (polyline/draw-segments! c [5 35] [10 10] [:red] {:width 0}))
      (is (near (rgb (c2d/get-image canvas) 20 10) [255 255 255]) "zero width draws nothing")))
  (testing "2.4 an NA end gives the NA color; :groups {:na false} joins the neighbours"
    (let [data [{:x 0 :y 0 :t 0} {:x 5 :y 0 :t nil} {:x 10 :y 0 :t 10}]
          na (vec (take 3 (map long (c/to-color (get-in (build (line-plot data)) [:theme :palette :na])))))]
      (is (near (panel-pixel (line-plot data) 0.25 0.5) na))
      (let [b (build (assoc (line-plot data) :groups {:na false}))]
        (is (= [0 10] (:colors (first (get-in b [:stats 0 :data facet/single-panel :lines]))))))))
  (testing "2.4 out-of-domain rows dropped and counted"
    (let [b (build (line-plot [{:x 0 :y 0 :t 0} {:x 1 :y 1 :t 50} {:x 2 :y 0 :t 10}] {:color (assoc red-blue :domain [0 10])} {}))]
      (is (= [0 10] (:colors (first (get-in b [:stats 0 :data facet/single-panel :lines])))))
      (is (= 1 (get-in b [:stats 0 :dropped :color])))))
  (testing "2.4 a declared linear scale with only NA colors: every segment NA"
    (is (some? (plot/render-image (line-plot [{:x 0 :y 0 :t nil} {:x 1 :y 1 :t nil}] {:color {:kind :linear}} {}) size))))
  (testing "2.4 one point: nothing to stroke, no error"
    (is (some? (plot/render-image (line-plot [{:x 0 :y 0 :t 1}]) size))))
  (testing "2.5 / 2.6 fill and flip"
    (is (some? (plot/render-image (line-plot [{:x 0 :y 0 :t 0} {:x 1 :y 1 :t 5}] {:color red-blue} {:fill [0 0 0 40]}) size)))
    (let [p (assoc (line-plot [{:x 0 :y 0 :t 0} {:x 10 :y 0 :t 10}]) :flip true)]
      (is (near (panel-pixel p 0.5 0.5) [128 0 127]) "flipped: vertical line, purple in the middle")))
  (testing "2.7 ordinal colors still group"
    (let [b (build {:data [{:x 0 :y 0 :g "a"} {:x 1 :y 1 :g "a"} {:x 0 :y 1 :g "b"}] :mapping {:x :x :y :y :color :g}
                    :charts [(chart/line)]})]
      (is (= 2 (count (get-in b [:stats 0 :data facet/single-panel :lines]))))
      (is (not-any? :colors (get-in b [:stats 0 :data facet/single-panel :lines]))))))

;; 3. CVD

(defn- two-pixels []
  (doto (BufferedImage. 2 1 BufferedImage/TYPE_INT_ARGB)
    (.setRGB 0 0 (unchecked-int 0xffff0000))
    (.setRGB 1 0 (unchecked-int 0x00000000))))

(deftest cvd-test
  (testing "3.1 / 3.4 pixels through the clojure2d filter, alpha kept, input unchanged"
    (let [img (two-pixels)
          out (plot/cvd-image img :deuteranopia)
          expected (mapv #(Math/round (double %)) (take 3 ((c/cvd-filter :deuteranopia) :red)))]
      (is (near (rgb out 0 0) expected))
      (is (= 0 (last (rgba out 1 0))) "transparent stays transparent")
      (is (= [255 0 0 255] (rgba img 0 0)) "input unchanged")
      (is (= [2 1] [(.getWidth out) (.getHeight out)]))))
  (testing "3.1 severity form; severity 0 keeps the colors"
    (is (near (rgb (plot/cvd-image (two-pixels) [:deuteranomaly 0.0]) 0 0) [255 0 0]))
    (is (near (rgb (plot/cvd-image (two-pixels) [:protanomaly 1.0]) 0 0)
              (rgb (plot/cvd-image (two-pixels) :protanopia) 0 0))))
  (let [p {:data [{:x 0 :y 0 :g "a"} {:x 1 :y 1 :g "b"}] :mapping {:x :x :y :y :color :g} :charts [(chart/scatter)]}]
    (testing "3.2 render option = cvd-image of the render, pixel for pixel"
      (let [a (plot/render-image p (assoc size :cvd :deuteranopia))
            b (plot/cvd-image (plot/render-image p size) :deuteranopia)]
        (is (every? (fn [[x y]] (= (.getRGB a x y) (.getRGB b x y))) (for [x (range 0 400 7) y (range 0 300 7)] [x y])))))
    (testing "3.2 from the plot's :options; on arrange"
      (is (= (.getRGB (plot/render-image p (assoc size :cvd :tritanopia)) 200 150)
             (.getRGB (plot/render-image (assoc p :options {:cvd :tritanopia}) size) 200 150)))
      (is (some? (plot/render-image (plot/arrange [p p]) (assoc size :cvd :achromatopsia)))))
    (testing "3.3 invalid deficiencies throw :cvd at build"
      (doseq [bad [:nope [:deuteranomaly 2] [:deuteranomaly -0.1] [:deuteranopia 0.5] "deuteranopia" [:deuteranomaly] 3]]
        (is (= :cvd (:key (error #(plot/render-image p (assoc size :cvd bad))))) (pr-str bad))))
    (testing "3.5 :cvd nil: unchanged"
      (is (= (.getRGB (plot/render-image p size) 200 150) (.getRGB (plot/render-image p (assoc size :cvd nil)) 200 150))))))

;; review fixes

(deftest review-fixes-test
  (testing "F1 a :fill vector in data mode throws; a single fill is fine"
    (is (= :fill (:key (error #(build {:data bands :charts [(chart/rect {:mapping {:y0 :y0 :y1 :y1} :fill [:red :blue]})]})))))
    (is (some? (build {:data bands :charts [(chart/rect {:mapping {:y0 :y0 :y1 :y1} :fill [255 0 0]})]})) "a color vector is one fill"))
  (testing "F2 :cvd on a sub-plot throws :cvd (top level still works)"
    (let [p {:data [{:x 1 :y 1}] :mapping {:x :x :y :y} :charts [(chart/scatter)]}
          e (error #(plot/render-image (plot/arrange [p (assoc p :options {:cvd :deuteranopia})]) size))]
      (is (= :cvd (:key e)))
      (is (some? (:subplot e)))
      (is (some? (plot/render-image (assoc (plot/arrange [p p]) :options {:cvd :deuteranopia}) size)))))
  (testing "F3 per-row color stats shared by line and rect (NA, dropped, extents)"
    (let [b (build {:data [{:y0 0 :y1 1 :v 0} {:y0 1 :y1 2 :v nil} {:y0 2 :y1 3 :v 99}] :scales {:color {:domain [0 10]}}
                    :charts [(chart/rect {:mapping {:y0 :y0 :y1 :y1 :color :v}})]})]
      (is (contains? (get-in b [:stats 0 :na]) :color))
      (is (= 1 (get-in b [:stats 0 :dropped :color])))
      (is (= [0.0 10.0] (get-in b [:scale-specs :color :domain])))))
  (testing "F4a rows with an unplaceable end are skipped before grouping: their colors count nowhere"
    (let [b (build {:data [{:y0 0 :y1 1 :v 0} {:y0 1 :y1 2 :v 10} {:y0 "x" :y1 ##NaN :v nil}]
                    :charts [(chart/rect {:mapping {:y0 :y0 :y1 :y1 :color :v}})]})]
      (is (= 1 (get-in b [:stats 0 :dropped :position])))
      (is (not (contains? (get-in b [:stats 0 :na]) :color)))
      (is (= 0 (get-in b [:stats 0 :dropped :color]))))))
