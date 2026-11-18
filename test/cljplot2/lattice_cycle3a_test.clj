(ns cljplot2.lattice-cycle3a-test
  "Lattice cycle 3a (3D): the 3D space (view, fit, depth order, shading, box and axes), `chart/cloud` (points,
  spikes, bars, facets, explicit domains) and the plot rules of 3D charts."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.chart :as chart]
            [cljplot2.chart.cloud :as cloud]
            [cljplot2.content.label :as label]
            [cljplot2.core :as plot]
            [cljplot2.data.space3d :as space]
            [cljplot2.graphics.space3d :as space-draw]
            [cljplot2.test-util :refer [error]]
            [cljplot2.theme]
            [cljplot2.data.shingle])
  (:import [fastmath.vector Vec2 Vec3]
           [java.awt.image BufferedImage]))

(defn- near? [^double a ^double b] (< (m/abs (- a b)) 1e-9))
(defn- built [p] (plot/build p {:width 500 :height 400}))
(defn- image ^BufferedImage [p] (c2d/get-image (plot/render p {:width 400 :height 300})))

(def ^:private cube
  (vec (for [x [0 1] y [0 1] z [0 1]] {:x x :y y :z z :g (if (zero? x) "a" "b") :v (+ x y z)})))

;; 1 space

(deftest view-test
  (testing "1.1 azimuth 0, elevation 0: x right, z up, y away; azimuth 90: y right"
    (let [r (space/rotation {:azimuth 0 :elevation 0})
          r90 (space/rotation {:azimuth 90 :elevation 0})]
      (is (= [1.0 0.0 0.0] (vec (r (Vec3. 1 0 0)))))
      (is (= [0.0 1.0 0.0] (vec (r (Vec3. 0 0 1)))))
      (is (= [0.0 0.0 1.0] (vec (r (Vec3. 0 1 0)))))
      (is (near? 1.0 (.x ^Vec3 (r90 (Vec3. 0 1 0)))))))
  (testing "1.1 rotations keep lengths"
    (let [r (space/rotation {:azimuth 37 :elevation -55})]
      (doseq [p [(Vec3. 1 2 3) (Vec3. -0.5 0 0.25)]]
        (is (near? (v/mag p) (v/mag (r p)))))))
  (testing "1.1 normalization: domain ends at the box sides, reversed domains flip"
    (let [n (space/normalizer {:x [0 10] :y [0 1] :z [690 40]} [1 2 1])]
      (is (= [-0.5 -1.0 -0.5] (vec (n [0 0 690]))))
      (is (= [0.5 1.0 0.5] (vec (n [10 1 40]))))))
  (testing "1.1 a single value widens"
    (is (= [4.5 5.5] (space/widen [5 5])))))

(deftest fit-test
  (testing "1.2 the projected box fits in the panel less the margin, its diagonal spanning the smaller side"
    (doseq [view [space/default-view {:azimuth 0 :elevation 0} {:azimuth 200 :elevation 75} {:azimuth 45 :elevation 35.26}]]
      (let [{:keys [corner scale]} (space/projector {:domains {:x [0 1] :y [0 1] :z [0 1]} :view view} {:w 400 :h 300 :margin 20})
            ps (map corner space/corners)
            xs (map #(.x ^Vec2 %) ps) ys (map #(.y ^Vec2 %) ps)]
        (is (every? #(<= 19.999 (double %) 380.001) xs))
        (is (every? #(<= 19.999 (double %) 280.001) ys))
        (is (near? (/ 260.0 (m/sqrt 3.0)) (double scale))))))
  (testing "U5 one box size whatever the view"
    (is (apply = (map #(:scale (space/projector {:domains {:x [0 1] :y [0 1] :z [0 1]} :view %} {:w 400 :h 300 :margin 20}))
                      [{:azimuth 20 :elevation 20} {:azimuth 70 :elevation 30} {:azimuth 200 :elevation 60}]))))
  (testing "1.2 a panel smaller than its margins gives scale 0"
    (is (zero? (double (:scale (space/projector {:domains {:x [0 1] :y [0 1] :z [0 1]} :view space/default-view}
                                                {:w 30 :h 30 :margin 20})))))))

(deftest depth-and-shading-test
  (testing "1.3 the nearer of two overlapping squares wins, whatever the list order"
    (let [sq (fn [x0 color depth] {:kind :polygon :depth depth :fill color :stroke :none
                                   :points [(v/vec2 x0 10) (v/vec2 (+ x0 30) 10) (v/vec2 (+ x0 30) 40) (v/vec2 x0 40)]})
          render (fn [prims] (let [cv (c2d/canvas 60 50)] (c2d/with-canvas [c cv] (space-draw/draw-primitives! c prims)) (c2d/get-image cv)))
          red-near [(sq 5 [255 0 0] 1.0) (sq 20 [0 0 255] 2.0)]]
      (is (= (c/to-color :red) (c/to-color (.getRGB ^BufferedImage (render red-near) 28 25))))
      (is (= (c/to-color :red) (c/to-color (.getRGB ^BufferedImage (render (reverse red-near)) 28 25))))))
  (testing "1.4 a face turned to the light is brighter than one edge-on"
    (let [lit (space/shade [200 200 200] (v/normalize (Vec3. -0.5 0.6 -1.0)))
          side (space/shade [200 200 200] (v/normalize (Vec3. 1.0 0.0 -0.5)))]
      (is (> (c/luma lit) (c/luma side)))
      (is (= 200.0 (c/red lit)) "full light keeps the color")
      (is (= 255.0 (c/alpha (space/shade [200 200 200] (Vec3. 0 0 0)))) "alpha kept; no normal - ambient only"))))

(deftest frame-test
  (testing "1.5 visible edges: 9 in a general view, 4 face-on; hidden ones the rest"
    (is (= 9 (count (space/visible-edges space/default-view))))
    (is (= 3 (count (space/hidden-edges space/default-view))))
    (is (= 4 (count (space/visible-edges {:azimuth 0 :elevation 0}))))
    (is (= 8 (count (space/hidden-edges {:azimuth 0 :elevation 0})))))
  (testing "1.5 an axis per dimension on an edge along it"
    (let [{:keys [corner]} (space/projector {:domains {:x [0 1] :y [0 1] :z [0 1]} :view space/default-view} {:w 400 :h 300 :margin 20})
          edges (space/axis-edges space/default-view corner)]
      (is (= #{:x :y :z} (set (keys edges))))
      (is (every? (fn [[k e]] (= k (:axis e))) edges))))
  (testing "1.5 tick labels and titles drawn inside the panel; none without axes"
    (let [p {:data cube :mapping {:x :x :y :y :z :z} :charts [(chart/cloud)]}
          at (fn [p] (let [log (atom []) draw-at @#'label/draw-at!]
                       (with-redefs-fn {#'label/draw-at! (fn [c content anchor x y env] (swap! log conj [x y]) (draw-at c content anchor x y env))}
                         #(image p))
                       @log))
          positions (at p)]
      (is (>= (count positions) 9) "ticks of three axes and three titles")
      (is (every? (fn [[x y]] (and (< 0 (double x) 400) (< 0 (double y) 300))) positions))
      (is (empty? (at (assoc p :charts [(chart/cloud {:axes false})]))))))
  (testing "1.5 axis ticks over a domain, at least a few"
    (is (= [0.0 0.5 1.0] (mapv double (:ticks (space-draw/axis-ticks :x [0 1] 3)))))))

;; 2 cloud

(deftest cloud-test
  (testing "2.1 a categorical color gives a legend, a numeric one a color bar; points per row"
    (let [legends (fn [p] (filter #(= :legends (get-in % [:content :kind])) (:items (:grid (built p)))))]
      (is (= 1 (count (legends {:data cube :mapping {:x :x :y :y :z :z :color :g} :charts [(chart/cloud)]}))))
      (is (= 1 (count (legends {:data cube :mapping {:x :x :y :y :z :z :color :v} :charts [(chart/cloud)]}))))
      (is (empty? (legends {:data cube :mapping {:x :x :y :y :z :z} :charts [(chart/cloud)]})))))
  (testing "2.1 no position scales or axes: the chart lays out its panel"
    (let [b (built {:data cube :mapping {:x :x :y :y :z :z} :charts [(chart/cloud)]})]
      (is (not-any? #{:x :y} (keys (:scale-specs b))))))
  (testing "2.1 non-finite coordinates dropped and counted"
    (let [rows (conj cube {:x ##NaN :y 0 :z 0} {:x 0 :y 0 :z nil})
          b (built {:data rows :mapping {:x :x :y :y :z :z} :charts [(chart/cloud)]})]
      (is (= {:x 1 :y 0 :z 1} (get-in b [:stats 0 :dropped])))
      (is (= 8 (count (get-in b [:stats 0 :data :panel :points :x]))))))
  (testing "2.2 primitives per type: a point, a spike line, the front faces of a bar"
    (let [computed (fn [opts] (get-in (built {:data cube :mapping {:x :x :y :y :z :z} :charts [(chart/cloud opts)]})
                                      [:stats 0 :data :panel]))
          prims (fn [opts] (let [cp (computed opts)
                                 proj (space/projector (assoc (:frame cp) :domains (:domains cp)) {:w 400 :h 300 :margin 20})]
                             (cloud/primitives cp (:point proj) (constantly (:style cp)))))]
      (is (= {:point 8} (frequencies (map :kind (prims {})))))
      (is (= {:line 8} (frequencies (map :kind (prims {:type :spikes})))))
      (is (= {:polygon 24} (frequencies (map :kind (prims {:type :bars})))) "3 visible faces per bar")
      (is (= {:polygon 8} (frequencies (map :kind (prims {:type :bars :view {:azimuth 0 :elevation 0}})))) "face-on: one")))
  (testing "2.2 bars widen x / y by half a bar and reach the base"
    (let [cp (get-in (built {:data cube :mapping {:x :x :y :y :z :z} :charts [(chart/cloud {:type :bars :base -1})]})
                     [:stats 0 :data :panel])]
      (is (= [-0.4 1.4] (:x (:domains cp))))
      (is (= [-1.0 1.0] (:z (:domains cp))))))
  (testing "2.3 facets share the domains"
    (let [b (built {:data cube :mapping {:x :x :y :y :z :z :facet :g} :charts [(chart/cloud)]})
          ds (map :domains (vals (get-in b [:stats 0 :data])))]
      (is (= 2 (count ds)))
      (is (apply = ds))))
  (testing "2.4 explicit domains (reversed allowed); points outside dropped and counted"
    (let [b (built {:data cube :mapping {:x :x :y :y :z :z} :charts [(chart/cloud {:domain {:x [0 0.5] :z [1 0]}})]})]
      (is (= [0 0.5] (get-in b [:stats 0 :data :panel :domains :x])))
      (is (= [1 0] (get-in b [:stats 0 :data :panel :domains :z])))
      (is (= 4 (get-in b [:stats 0 :dropped :x])))))
  (testing "2.4 a domain of two equal numbers is a schema error"
    (is (some? (error #(built {:data cube :mapping {:x :x :y :y :z :z} :charts [(chart/cloud {:domain {:x [1 1]}})]})))))
  (testing "renders, also faceted and with every type"
    (doseq [t [:points :spikes :bars]]
      (is (instance? BufferedImage (image {:data cube :mapping {:x :x :y :y :z :z :facet :g} :charts [(chart/cloud {:type t})]}))))))

;; 3 plot rules

(deftest rules-test
  (let [p {:data cube :mapping {:x :x :y :y :z :z}}
        full {:x [0 1] :y [0 1] :z [0 1]}]
    (testing "3.1 no 2D charts beside a 3D one"
      (is (= :charts (:key (error #(built (assoc p :charts [(chart/cloud) (chart/scatter)])))))))
    (testing "3.2 several 3D charts need full explicit domains"
      (is (= :charts (:key (error #(built (assoc p :charts [(chart/cloud) (chart/cloud {:type :spikes})]))))))
      (is (= :charts (:key (error #(built (assoc p :charts [(chart/cloud {:domain full}) (chart/cloud {:domain (dissoc full :z)})]))))))
      (is (instance? BufferedImage (image (assoc p :charts [(chart/cloud {:domain full}) (chart/cloud {:domain full :type :spikes})])))))
    (testing "3.3 rejected plot options"
      (is (= :flip (:key (error #(built (assoc p :charts [(chart/cloud)] :flip true))))))
      (is (= :aspect (:key (error #(built (assoc p :charts [(chart/cloud)] :aspect :iso))))))
      (is (= :margins (:key (error #(built (assoc p :charts [(chart/cloud)] :margins {:top (chart/density)}))))))
      (is (= :facets (:key (error #(built (-> p (assoc :charts [(chart/cloud)] :facets {:scales {:x :free}}) (assoc-in [:mapping :facet] :g)))))))
      (is (= :charts (:key (error #(built (assoc p :charts [(chart/cloud)] :coord {:kind :polar})))))))))

;; review fixes

(defn- lines-in [f]
  (let [log (atom []) line @#'c2d/line]
    (with-redefs-fn {#'c2d/line (fn [& args] (swap! log conj (vec (rest args))) (apply line args))} f)
    @log))

(deftest review-fixes-test
  (let [proj (space/projector {:domains {:x [0 1] :y [0 1] :z [0 1]} :view space/default-view} {:w 400 :h 300 :margin 30})
        frame {:view space/default-view :domains {:x [0 1] :y [0 1] :z [0 1]} :box? true :axes? true :titles {}}
        env {:theme cljplot2.theme/default-theme}
        draw (fn [f] (lines-in #(let [cv (c2d/canvas 400 300)] (c2d/with-canvas [c cv] (f c proj frame env)))))]
    (testing "U1 / U2 behind the data: the floor grid (x and y ticks) and the 3 hidden edges"
      (let [ticks (fn [k] (count (:ticks (space-draw/axis-ticks k [0 1] 3))))
            back (draw space-draw/draw-frame-back!)]
        (is (= (+ 3 (ticks :x) (ticks :y)) (count back)))
        (is (empty? (draw (fn [c p f e] (space-draw/draw-frame-back! c p (assoc f :box? false) e)))))))
    (testing "U1 in front: the 9 visible edges (plus axis lines and ticks)"
      (is (>= (count (draw space-draw/draw-frame-front!)) 9))))
  (testing "U6 the frame behind the data, the data, then the frame in front"
    (let [log (atom [])
          wrap (fn [k v] (let [f @v] (fn [& args] (swap! log conj k) (apply f args))))]
      (with-redefs-fn {#'space-draw/draw-frame-back! (wrap :back #'space-draw/draw-frame-back!)
                       #'space-draw/draw-primitives! (wrap :data #'space-draw/draw-primitives!)
                       #'space-draw/draw-frame-front! (wrap :front #'space-draw/draw-frame-front!)}
        #(image {:data cube :mapping {:x :x :y :y :z :z} :charts [(chart/cloud {:type :bars})]}))
      (is (= [:back :data :front] @log))))
  (testing "U3 titles along their edges, readable: left to right, vertical bottom to top"
    (is (near? 0.0 (space-draw/edge-angle (v/vec2 10 0))))
    (is (near? 0.0 (space-draw/edge-angle (v/vec2 -10 0))))
    (is (near? 30.0 (space-draw/edge-angle (v/vec2 (m/cos (m/radians 30)) (m/sin (m/radians 30))))))
    (is (near? 330.0 (space-draw/edge-angle (v/vec2 (m/cos (m/radians 30)) (- (m/sin (m/radians 30)))))))
    (is (near? 270.0 (space-draw/edge-angle (v/vec2 0 10))))
    (is (near? 270.0 (space-draw/edge-angle (v/vec2 0 -10)))))
  (testing "U4 shingle bounds without floating-point noise"
    (is (= "[4.25, 4.65]" (str (cljplot2.data.shingle/->Interval 4.25 4.6499999999999995 nil nil nil))))
    (is (= "[-3, 2.5]" (str (cljplot2.data.shingle/->Interval -3 2.5 nil nil nil))))
    (is (= "[1E-12, 1E+300]" (str (cljplot2.data.shingle/->Interval 1e-12 1e300 nil nil nil))) "scientific at the extremes")
    (is (= "[0.0001, 0]" (str (cljplot2.data.shingle/->Interval 1e-4 0.0 nil nil nil)))))
  (testing "F7 tick labels and titles stay inside panels of any size"
    (doseq [[w h] [[900 800] [250 220]]]
      (let [log (atom []) draw-at @#'label/draw-at!]
        (with-redefs-fn {#'label/draw-at! (fn [c content anchor x y env] (swap! log conj [x y]) (draw-at c content anchor x y env))}
          #(c2d/get-image (plot/render {:data (for [i (range 50)] {:x (* i 1234.5) :y (- i) :z (* i i 0.001)})
                                        :mapping {:x :x :y :y :z :z} :charts [(chart/cloud)]} {:width w :height h})))
        (is (every? (fn [[x y]] (and (< 0 (double x) w) (< 0 (double y) h))) @log) (str w "x" h)))))
  (testing "F8 a spikes legend key is a vertical line"
    (let [lines (lines-in #(let [cv (c2d/canvas 20 20)]
                             (c2d/with-canvas [c cv]
                               (cloud/legend (chart/cloud {:type :spikes}) {:color [0 0 0]} c {:x 0 :y 0 :w 20 :h 20}
                                             {:theme cljplot2.theme/default-theme}))))]
      (is (= 1 (count lines)))
      (is (= (first (first lines)) (nth (first lines) 2)) "vertical"))))

