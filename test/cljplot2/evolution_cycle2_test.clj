(ns cljplot2.evolution-cycle2-test
  "Evolution cycle 2: stream positions and negative stacking, line stacking, bands, area colour, gradient fills,
  density streamgraphs."
  (:require [clojure.test :refer [deftest testing is are]]
            [cljplot2.test-util :refer [error]]
            [clojure2d.color :as c]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.facet :as facet]
            [cljplot2.data.position :as position]
            [cljplot2.graphics.curve :as curve]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.layout.grid :as grid])
  (:import [java.awt.image BufferedImage]
           [java.time LocalDate]))

(def ^:private size {:width 400 :height 300})
(defn- build [p] (plot/build p size))
(def ^:private white {:plot {:background :white} :panel {:grid {:x :none :y :none}}})
(defn- rgb [^BufferedImage img x y]
  (let [v (.getRGB img (int x) (int y))] [(bit-and (bit-shift-right v 16) 0xff) (bit-and (bit-shift-right v 8) 0xff) (bit-and v 0xff)]))
(defn- panel-box [p] (grid/item-box (:layout (build p)) :panel))
(defn- lines-of [b] (get-in b [:stats 0 :data facet/single-panel :lines]))

(def ^:private two [{:x 1 :y 1 :g "a"} {:x 2 :y 2 :g "a"} {:x 3 :y 3 :g "a"} {:x 2 :y 4 :g "b"} {:x 4 :y 2 :g "b"}])
(defn- stack-plot [opts & [extra]]
  (merge {:data two :mapping {:x :x :y :y :color :g} :charts [(chart/line opts)] :theme white} extra))

;; 1. positions

(deftest positions-test
  (testing "1.1 stream positions are separate; histogram doesn't take them"
    (is (= #{:silhouette :wiggle} position/stream-positions))
    (is (not-any? position/stream-positions position/positions))
    (is (= :position (:key (error #(build {:data two :mapping {:x :x} :charts [(chart/histogram {:position :wiggle})]}))))))
  (testing "1.2 negatives stack separately, downward"
    (is (= [[[0.0 1.0] [-1.0 0.0]] [[1.0 3.0] [-3.0 -1.0]]] (position/stack-values :stack [[1 -1] [2 -2]])))
    (is (= [[[0.0 0.0]] [[0.0 2.0]]] (position/stack-values :stack [[0] [2]])) "a zero height"))
  (testing "1.3 silhouette centred on 0"
    (is (= [[[-2.0 -1.0]] [[-1.0 2.0]]] (position/stack-values :silhouette [[1] [3]]))))
  (testing "1.4 wiggle: flat for constant groups; d3's formula"
    (is (= [[[0.0 1.0] [0.0 1.0]] [[1.0 3.0] [1.0 3.0]]] (position/stack-values :wiggle [[1 1] [2 2]])))
    ;; d3: j=1 s1=4 s2=2 -> -0.5; j=2 s1=4 s2=2 -> -1.0
    (is (= [[[0.0 1.0] [-0.5 1.5] [-1.0 2.0]] [[1.0 4.0] [1.5 3.5] [2.0 3.0]]] (position/stack-values :wiggle [[1 2 3] [3 2 1]])))
    (is (= [[[0.0 0.0] [0.0 0.0]]] (position/stack-values :wiggle [[0 0]])) "zero totals keep the baseline")
    (is (= [[[0.0 5.0]]] (position/stack-values :wiggle [[5]])) "one index"))
  (testing "1.5 negatives with normalize / stream positions"
    (are [p] (= :position (:key (error #(position/stack-values p [[1 -1] [2 2]]))))
      :normalize :silhouette :wiggle))
  (testing "existing positions unchanged"
    (is (= [[[0.0 0.25]] [[0.25 1.0]]] (position/stack-values :normalize [[1] [3]])))
    (is (= [] (position/stack-values :stack [])))))

;; 2. line stacking

(deftest alignment-test
  (testing "2.2 union of x; linear inside a group's range, 0 outside"
    (let [[a b] (lines-of (build (stack-plot {:position :stack})))]
      (is (= [1 2 3 4] (:x a)))
      (is (= [1.0 2.0 3.0 0.0] (mapv - (:y a) (:y0 a))) "A: its values, 0 at x 4")
      (is (= [0.0 4.0 3.0 2.0] (mapv - (:y b) (:y0 b))) "B: 0 at x 1, interpolated at x 3")
      (is (= (:y a) (:y0 b)) "B sits on A")))
  (testing "2.2 temporal x aligned by time, drawn x stays temporal"
    (let [d (fn [day] (LocalDate/of 2016 1 day))
          data [{:x (d 1) :y 1 :g "a"} {:x (d 3) :y 3 :g "a"} {:x (d 2) :y 1 :g "b"}]
          [a b] (lines-of (build (stack-plot {:position :stack} {:data data})))]
      (is (= [(d 1) (d 2) (d 3)] (:x a)))
      (is (= [1.0 2.0 3.0] (mapv - (:y a) (:y0 a))))
      (is (= [0.0 1.0 0.0] (mapv - (:y b) (:y0 b))))))
  (testing "2.2 errors: repeated x in a group, categorical x"
    (is (= :x (:key (error #(build (stack-plot {:position :stack} {:data (conj two {:x 1 :y 5 :g "a"})}))))))
    (is (= :x (:key (error #(build (stack-plot {:position :stack} {:data [{:x "p" :y 1 :g "a"} {:x "q" :y 2 :g "a"}]})))))))
  (testing "2.2 one group, one point"
    (is (= [[1.0 2.0]] (let [[a] (lines-of (build (stack-plot {:position :stack} {:data [{:x 1 :y 1 :g "a"} {:x 2 :y 2 :g "a"}]})))]
                         [(:y a)]))))
  (testing "2.3 NA y left out, interpolated across, counted; :gaps rejected"
    (let [b (build (stack-plot {:position :stack} {:data [{:x 1 :y 1 :g "a"} {:x 2 :y nil :g "a"} {:x 3 :y 3 :g "a"}]}))]
      (is (= [1.0 3.0] (:y (first (lines-of b)))))
      (is (= 1 (get-in b [:stats 0 :dropped :y]))))
    (is (= :gaps (:key (error #(build (stack-plot {:position :stack :gaps :break})))))))
  (testing "2.4 extents cover both boundaries (stream below 0)"
    (let [e (get-in (build (stack-plot {:position :silhouette})) [:stats 0 :extents facet/single-panel :y])]
      (is (neg? (double (:min e))))))
  (testing "2.5 normalize: Proportion; y padding from 0 for stacks"
    (let [b (build (stack-plot {:position :normalize}))]
      (is (= "Proportion" (get-in b [:stats 0 :axis-titles :y])))
      (is (= [0.0 0.05] (get-in b [:stats 0 :padding :y]))))
    (is (= [0.05 0.05] (get-in (build (stack-plot {:position :wiggle})) [:stats 0 :padding :y])))
    (is (= [0.05 0.05] (get-in (build (stack-plot {})) [:stats 0 :padding :y])) "overlay unchanged"))
  (testing "2.6 / 2.7 a line coloured along its length, :curve"
    (is (= :position (:key (error #(build (stack-plot {:position :stack} {:mapping {:x :x :y :y :color :y}}))))))
    (is (= :curve (:key (error #(build (stack-plot {:position :stack :curve :bezier})))))))
  (testing "negative stacks pad both sides"
    (is (= [0.05 0.05] (get-in (build (stack-plot {:position :stack} {:data (conj two {:x 5 :y -2 :g "b"})})) [:stats 0 :padding :y])))))

;; 3. bands

(deftest bands-test
  (testing "3.1 paired samples: one grid for both boundaries"
    (let [upper [[0.0 10.0] [10.0 0.0] [20.0 5.0] [30.0 2.0]]
          lower [[0.0 20.0] [10.0 15.0] [20.0 18.0] [30.0 16.0]]
          [u l] (curve/band-points upper lower {:interpolator :pchip})]
      (is (= (count u) (count l) 31))
      (is (= (mapv first u) (mapv first l))))
    (is (= [[[0.0 1.0]] [[0.0 2.0]]] (curve/band-points [[0.0 1.0]] [[0.0 2.0]] {})) "no interpolator: as given")
    (let [[u l] (curve/band-points [[0.0 1.0] [1.0 2.0]] [[0.0 3.0] [1.0 4.0]] {:interpolator :akima})]
      (is (= [[0.0 1.0] [1.0 2.0]] u) "too few for akima: straight")
      (is (= 2 (count l)))))
  (testing "3.2 no background between adjacent bands"
    (let [data (for [g ["a" "b"] x (range 6)] {:x x :y (+ 1 (mod (+ x (count g)) 3)) :g g})
          p {:data data :mapping {:x :x :y :y :color :g} :theme white
             :charts [(chart/line {:position :stack :interpolator :pchip :alpha 1.0})]}
          {:keys [x y w h]} (panel-box p)
          img (plot/render-image p size)
          col (+ x (* 0.5 w))
          ;; from the panel bottom up to the top band's upper edge every pixel is coloured
          colored (take-while #(not= [255 255 255] (rgb img col %)) (range (- (+ y h) 2) y -1))]
      (is (> (count colored) (* 0.3 h))))))

;; 4. area colour

(deftest area-color-test
  (testing "4.1 shared area colour"
    (is (= 153.0 (c/alpha (polyline/area-color {:color [10 20 30] :fill :color :alpha 0.6} false))))
    (is (= :none (polyline/area-color {:color :red :fill :none :alpha 0.6} false)))
    (is (= 153.0 (c/alpha (polyline/area-color {:color [10 20 30] :fill :none :alpha 0.6} true))) "stacked default")
    (is (= :blue (polyline/area-color {:color :red :fill :blue :alpha 0.6} true))))
  (testing "4.2 line :fill :color and :alpha"
    (let [p (fn [opts] {:data [{:x 0 :y 1} {:x 1 :y 1}] :mapping {:x :x :y :y} :theme white :scales {:y {:domain [0 2]}}
                        :charts [(chart/line (merge {:color [255 0 0]} opts))]})
          {:keys [x y w h]} (panel-box (p {}))
          probe (fn [q] (rgb (plot/render-image q size) (+ x (* 0.5 w)) (+ y (* 0.75 h))))]
      (is (= [255 255 255] (probe (p {}))))
      (is (= [255 102 102] (probe (p {:fill :color}))) "red at 0.6 over white")
      (is (= [255 0 0] (probe (p {:fill :color :alpha 1.0}))))
      (is (= :alpha (:key (error #(build (p {:alpha 2})))))))))

;; 5. gradient fill

(deftest gradient-fill-test
  (let [p (fn [opts] {:data [{:x 0 :y 2} {:x 10 :y 2}] :mapping {:x :x :y :y} :theme white :scales {:y {:domain [0 2]} :x {:domain [0 10]}}
                      :charts [(chart/line (merge {:color :none :fill-gradient [[255 0 0] [0 0 255]]} opts))]})
        {:keys [x y w h]} (panel-box (p {}))
        at (fn [q fx fy] (rgb (plot/render-image q size) (+ x (* fx w)) (+ y (* fy h))))]
    (testing "5.2 along x: red on the left, blue on the right, constant up the column"
      (let [[r0 _ b0] (at (p {}) 0.05 0.5) [r1 _ b1] (at (p {}) 0.95 0.5)]
        (is (> r0 200)) (is (< b0 60)) (is (> b1 200)) (is (< r1 60)))
      (is (= (at (p {}) 0.3 0.2) (at (p {}) 0.3 0.8))))
    (testing "5.3 along y: changes with height, not with x"
      (let [q (p {:fill-gradient-axis :y})]
        (is (= (at q 0.2 0.5) (at q 0.8 0.5)))
        (is (not= (at q 0.5 0.1) (at q 0.5 0.9)))
        (is (> (first (at q 0.5 0.95)) 200) "the y domain start (bottom) is the first colour")))
    (testing "5.4 stacked bands take the gradient; a gradient line can't"
      (is (some? (plot/render-image (stack-plot {:position :stack :fill-gradient [:red :blue]}) size)))
      (is (= :fill-gradient (:key (error #(build (stack-plot {:fill-gradient [:red :blue]} {:mapping {:x :x :y :y :color :y}})))))))
    (testing "5.5 the gradient wins over :fill; unknown gradients"
      (is (= (at (p {}) 0.05 0.5) (at (p {:fill :green}) 0.05 0.5)))
      (is (= :fill-gradient (:key (error #(plot/render-image (p {:fill-gradient :no-such-gradient}) size))))))))

;; 6. density

(deftest density-stream-test
  (let [data (for [g ["a" "b"] i (range 50)] {:v (+ (* 3 (count g)) (mod (* i 7) 11)) :g g})]
    (testing "6.1 silhouette / wiggle on flat densities"
      (let [b (build {:data data :mapping {:x :v :color :g} :charts [(chart/density {:position :silhouette})]})]
        (is (neg? (double (get-in b [:stats 0 :extents facet/single-panel :y :min]))) "6.2 extents below 0")
        (is (= [0.05 0.05] (get-in b [:stats 0 :padding :y]))))
      (is (some? (plot/render-image {:data data :mapping {:x :v :color :g} :charts [(chart/density {:position :wiggle})]} size))))
    (testing "6.1 ridgelines reject stream positions"
      (is (= :position (:key (error #(build {:data data :mapping {:x :v :y :g} :charts [(chart/density {:position :silhouette})]}))))))
    (testing "5.3 ridge gradient along y"
      (is (some? (plot/render-image {:data data :mapping {:x :v :y :g}
                                     :charts [(chart/density {:fill-gradient [:red :blue] :fill-gradient-axis :y})]} size))))))
