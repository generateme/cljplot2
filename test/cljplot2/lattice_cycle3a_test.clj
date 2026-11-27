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
            [cljplot2.data.rows :as rows]
            [cljplot2.data.space3d :as space]
            [cljplot2.content.space3d :as space-draw]
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
    (is (= [4.5 5.5] (rows/widened-range [5 5])))))

(deftest fit-test
  (testing "1.2 the projected box fits in the panel less the margin, at the scale of its largest turned projection"
    (doseq [view [space/default-view {:azimuth 0 :elevation 0} {:azimuth 200 :elevation 75} {:azimuth 45 :elevation 35.26}]]
      (let [{:keys [corner scale]} (space/projector {:domains {:x [0 1] :y [0 1] :z [0 1]} :view view} {:w 400 :h 300 :margin 20})
            ps (map corner space/corners)
            xs (map #(.x ^Vec2 %) ps) ys (map #(.y ^Vec2 %) ps)]
        (is (every? #(<= 19.999 (double %) 380.001) xs))
        (is (every? #(<= 19.999 (double %) 280.001) ys))
        (let [[tw th] (space/turning-extent [1 1 1] (:elevation view))]
          (is (near? (min (/ 360.0 (double tw)) (/ 260.0 (double th))) (double scale)))))))
  (testing "W3 the turned extent: elevation 0 - floor diagonal by height; 90 - floor diagonal both ways"
    (is (= [5.0 2.0] (mapv double (space/turning-extent [3 4 2] 0))))
    (let [[w h] (space/turning-extent [3 4 2] 90)] (is (near? 5.0 (double w))) (is (near? 5.0 (double h))))
    (let [[_ h] (space/turning-extent [3 4 2] -30)] (is (near? (+ (* 2 (m/cos (m/radians 30))) (* 5 0.5)) (double h))) "below as above"))
  (testing "U5 / W3 one box size whatever the azimuth; the elevation may change it"
    (let [scale #(:scale (space/projector {:domains {:x [0 1] :y [0 1] :z [0 1]} :view %} {:w 400 :h 300 :margin 20}))]
      (is (apply = (map scale [{:azimuth 20 :elevation 30} {:azimuth 70 :elevation 30} {:azimuth 200 :elevation 30} {:azimuth -45 :elevation 30}])))
      (is (not= (scale {:azimuth 20 :elevation 10}) (scale {:azimuth 20 :elevation 60})))))
  (testing "W3 every azimuth's projection fits the panel"
    (doseq [a (range 0 360 15)]
      (let [{:keys [corner]} (space/projector {:domains {:x [0 1] :y [0 1] :z [0 1]} :view {:azimuth a :elevation 35} :aspect [1 1.5 0.8]}
                                              {:w 400 :h 300 :margin 20})
            ps (map corner space/corners)]
        (is (every? #(<= 19.999 (.x ^Vec2 %) 380.001) ps) (str a))
        (is (every? #(<= 19.999 (.y ^Vec2 %) 280.001) ps) (str a)))))
  (testing "1.2 a panel smaller than its margins gives scale 0"
    (is (zero? (double (:scale (space/projector {:domains {:x [0 1] :y [0 1] :z [0 1]} :view space/default-view}
                                                {:w 30 :h 30 :margin 20})))))))

(deftest depth-and-shading-test
  (testing "1.3 the nearer of two overlapping squares wins, whatever the list order"
    (let [sq (fn [x0 color depth] {:kind :polygon :depth depth :fill color :stroke-color :none
                                   :points [(v/vec2 x0 10) (v/vec2 (+ x0 30) 10) (v/vec2 (+ x0 30) 40) (v/vec2 x0 40)]})
          render (fn [prims] (let [cv (c2d/canvas 60 50)] (c2d/with-canvas [c cv] (space-draw/draw-primitives! c prims)) (c2d/get-image cv)))
          red-near [(sq 5 [255 0 0] 1.0) (sq 20 [0 0 255] 2.0)]]
      (is (= (c/to-color :red) (c/to-color (.getRGB ^BufferedImage (render red-near) 28 25))))
      (is (= (c/to-color :red) (c/to-color (.getRGB ^BufferedImage (render (reverse red-near)) 28 25))))))
  (testing "1.4 a face turned to the light is brighter than one edge-on"
    (let [shade (space/shader {})
          lit (shade [200 200 200] (v/normalize (Vec3. -0.5 0.6 -1.0)))
          side (shade [200 200 200] (v/normalize (Vec3. 1.0 0.0 -0.5)))]
      (is (> (c/luma lit) (c/luma side)))
      (is (= 200.0 (c/red lit)) "full light keeps the color")
      (is (= 255.0 (c/alpha (shade [200 200 200] (Vec3. 0 0 0)))) "alpha kept; no normal - ambient only"))))

(deftest frame-test
  (testing "1.5 visible edges: 9 in a general view, 4 face-on; hidden ones the rest"
    (is (= 9 (count (space/visible-edges space/default-view [1 1 1]))))
    (is (= 3 (count (space/hidden-edges space/default-view [1 1 1]))))
    (is (= 4 (count (space/visible-edges {:azimuth 0 :elevation 0} [1 1 1]))))
    (is (= 8 (count (space/hidden-edges {:azimuth 0 :elevation 0} [1 1 1])))))
  (testing "1.5 an axis per dimension on an edge along it"
    (let [{:keys [corner]} (space/projector {:domains {:x [0 1] :y [0 1] :z [0 1]} :view space/default-view} {:w 400 :h 300 :margin 20})
          edges (space/axis-edges space/default-view [1 1 1] corner)]
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
          ;; the scene's frame, as draw-scene! passes it: with the box's domains
          prims (fn [opts] (let [cp (let [c (computed opts)] (assoc-in c [:frame :domains] (:domains c)))
                                 proj (space/projector (:frame cp) {:w 400 :h 300 :margin 20})]
                             (cloud/primitives cp (:point proj) (constantly (:style cp)))))]
      (is (= {:point 8} (frequencies (map :kind (prims {})))))
      (is (= {:line 8} (frequencies (map :kind (prims {:type :spikes})))))
      (is (= {:polygon 24} (frequencies (map :kind (prims {:type :bars})))) "3 visible faces per bar")
      (is (= {:polygon 8} (frequencies (map :kind (prims {:type :bars :view {:azimuth 0 :elevation 0}})))) "face-on: one")))
  (testing "2.2 bars widen x / y by half a bar and reach the base"
    (let [cp (get-in (built {:data cube :mapping {:x :x :y :y :z :z} :charts [(chart/cloud {:type :bars :base -1})]})
                     [:stats 0 :data :panel])]
      (is (= [-0.5 1.5] (:x (:domains cp))) "[-0.4 1.4] made nice (cycle 3b)")
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
    ;; the point-pair arity calls the coordinate one: log only that, once per line
    (with-redefs-fn {#'c2d/line (fn [& args] (when (= 5 (count args)) (swap! log conj (vec (rest args)))) (apply line args))} f)
    @log))

(deftest review-fixes-test
  (let [proj (space/projector {:domains {:x [0 1] :y [0 1] :z [0 1]} :view space/default-view} {:w 400 :h 300 :margin 30})
        frame {:view space/default-view :domains {:x [0 1] :y [0 1] :z [0 1]} :box true :axes true :titles {}}
        env {:theme cljplot2.theme/default-theme}
        draw (fn [f] (lines-in #(let [cv (c2d/canvas 400 300)] (c2d/with-canvas [c cv] (f c proj frame env)))))]
    (testing "U1 / U2 / V3 behind the data: grids of the floor and the two back walls, the 3 hidden edges"
      (let [grid (fn [axis side] (count (#'space-draw/face-grid-lines proj axis side frame (assoc env :canvas (c2d/canvas 10 10)))))
            back-x (space/far-side space/default-view [1 1 1] :x)
            back-y (space/far-side space/default-view [1 1 1] :y)
            back (draw space-draw/draw-frame-back!)]
        (is (some? back-x))
        (is (some? back-y))
        (is (= (+ 3 (grid :z 0) (grid :x back-x) (grid :y back-y)) (count back)))
        (is (= #{:x :y} (set (map first (#'space-draw/face-grid-lines proj :z 0 frame (assoc env :canvas (c2d/canvas 10 10)))))))
        (is (= #{:y :z} (set (map first (#'space-draw/face-grid-lines proj :x back-x frame (assoc env :canvas (c2d/canvas 10 10)))))))
        (is (empty? (draw (fn [c p f e] (space-draw/draw-frame-back! c p (assoc f :box false) e)))))))
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

(deftest review2-fixes-test
  (testing "V1 a title's distance from its edge: past ticks and labels, plus half its thickness across the edge"
    (is (= 27.0 (space-draw/title-distance 6 2 10 14)) "6 + 2 gaps of 2 + 10 + 14 / 2"))
  (testing "V2 a short front bar is drawn over a tall bar behind it"
    (let [rows [{:x 0 :y 0 :z 1 :c "front"} {:x 0 :y 1 :z 10 :c "back"}]
          b (built {:data rows :mapping {:x :x :y :y :z :z :color :c} :charts [(chart/cloud {:type :bars :base 0 :view {:azimuth 0 :elevation 20}})]
                    :scales {:color {:domain ["front" "back"] :range [[255 0 0] [0 0 255]]}}})
          cp (let [c (get-in b [:stats 0 :data :panel])] (assoc-in c [:frame :domains] (:domains c)))
          proj (space/projector (:frame cp) {:w 300 :h 300 :margin 20})
          colors {"front" [255 0 0] "back" [0 0 255]}
          prims (cloud/primitives cp (:point proj) (fn [i] (assoc (:style cp) :color (colors (get-in cp [:points :color i])))))
          front-face (first (filter #(> (c/red (:fill %)) 100) prims))
          centre (v/mult (reduce v/add (:points front-face)) (/ 1.0 (count (:points front-face))))
          cv (c2d/canvas 300 300)]
      (c2d/with-canvas [c cv] (space-draw/draw-primitives! c (shuffle prims)))
      (let [px (c/to-color (.getRGB ^BufferedImage (c2d/get-image cv) (int (.x ^Vec2 centre)) (int (.y ^Vec2 centre))))]
        (is (> (c/red px) (c/blue px)) "the front bar's color"))
      (is (apply = (map :depth (filter #(> (c/red (:fill %)) 100) prims))) "a bar's faces share one depth")))
  (testing "V2 a spike sorts by its base"
    (let [cp (get-in (built {:data cube :mapping {:x :x :y :y :z :z} :charts [(chart/cloud {:type :spikes :base 0})]}) [:stats 0 :data :panel])
          proj (space/projector (assoc (:frame cp) :domains (:domains cp)) {:w 300 :h 300 :margin 20})
          prims (cloud/primitives cp (:point proj) (constantly (:style cp)))]
      (doseq [[p i] (map vector prims (range))]
        (is (near? (double (second ((:point proj) [(get-in cp [:points :x i]) (get-in cp [:points :y i]) 0]))) (double (:depth p)))))))
  (testing "V3 an edge-on side has no back wall"
    (is (nil? (space/far-side {:azimuth 0 :elevation 0} [1 1 1] :x)))
    (is (= 1 (space/far-side {:azimuth 0 :elevation 0} [1 1 1] :y))))
  (testing "F4 z grid lines use the y grid style; :none turns them off"
    (let [proj (space/projector {:domains {:x [0 1] :y [0 1] :z [0 1]} :view space/default-view} {:w 400 :h 300 :margin 30})
          frame {:view space/default-view :domains {:x [0 1] :y [0 1] :z [0 1]} :box true}
          count-lines (fn [theme] (count (lines-in #(c2d/with-canvas [c (c2d/canvas 400 300)]
                                                      (space-draw/draw-frame-back! c proj frame {:theme theme})))))
          t cljplot2.theme/default-theme]
      (is (< (count-lines (assoc-in t [:panel :grid :y] :none)) (count-lines t)))
      (is (= 3 (count-lines (-> t (assoc-in [:panel :grid :y] :none) (assoc-in [:panel :grid :x] :none)))) "only hidden edges"))))

(deftest turning-test
  (let [proj (fn [azimuth aspect] (space/projector {:domains {:x [0 1] :y [0 1] :z [0 1]} :view {:azimuth azimuth :elevation 30} :aspect aspect}
                                                   {:w 300 :h 300 :margin 20}))
        px (fn [^Vec2 p] [(m/round (.x p)) (m/round (.y p))])
        outline (fn [p] (set (map (comp px (:corner p)) space/corners)))
        origin (fn [p] (px ((:corner p) [0 0 0])))]
    (testing "Y3 a turn of 90 degrees moves the box's corners; a cube's outline maps onto itself"
      (is (not= (origin (proj 20 [1 1 1])) (origin (proj 110 [1 1 1]))))
      (is (= (outline (proj 20 [1 1 1])) (outline (proj 110 [1 1 1])))))
    (testing "Y3 a box of unequal sides, or a turn other than 90 degrees, changes the outline"
      (is (not= (outline (proj 20 [1 1.3 0.8])) (outline (proj 110 [1 1.3 0.8]))))
      (is (not= (outline (proj 20 [1 1 1])) (outline (proj 60 [1 1 1])))))
    (testing "Y3 a full turn and a turn by -340 are the same view"
      (is (= (outline (proj 20 [1 1.3 0.8])) (outline (proj 380 [1 1.3 0.8])) (outline (proj -340 [1 1.3 0.8])))))))

(deftest review4-fixes-test
  (testing "Z1 text z throws {:key :mapping} (text x / y - categorical axes, cycle 3b); nil and NaN are dropped and counted"
    (is (= :mapping (:key (error #(built {:data [{:x 1 :y 1 :z "a"}] :mapping {:x :x :y :y :z :z} :charts [(chart/cloud)]})))))
    (is (= :mapping (:key (error #(built {:data [{:x 1 :y 1 :z (java.time.LocalDate/of 2020 1 1)}] :mapping {:x :x :y :y :z :z} :charts [(chart/cloud)]})))))
    (is (= {:x 0 :y 1 :z 1} (get-in (built {:data (conj cube {:x 0 :y nil :z 1} {:x 0 :y 0 :z ##Inf}) :mapping {:x :x :y :y :z :z}
                                            :charts [(chart/cloud)]})
                                    [:stats 0 :dropped]))))
  (testing "Z2 several 3D charts: one frame, every chart's primitives in one depth sort"
    (let [log (atom [])
          wrap (fn [k v] (let [f @v] (fn [& args] (swap! log conj (if (= k :data) [k (count (second args))] k)) (apply f args))))
          full {:x [0 1] :y [0 1] :z [0 1]}]
      (with-redefs-fn {#'space-draw/draw-frame-back! (wrap :back #'space-draw/draw-frame-back!)
                       #'space-draw/draw-primitives! (wrap :data #'space-draw/draw-primitives!)
                       #'space-draw/draw-frame-front! (wrap :front #'space-draw/draw-frame-front!)}
        #(image {:data cube :mapping {:x :x :y :y :z :z}
                 :charts [(chart/cloud {:domain full}) (chart/cloud {:type :spikes :domain full})]}))
      (is (= [:back [:data 16] :front] @log))))
  (testing "Z2 a near point of one chart is drawn over a far spike of another"
    (let [full {:x [0 1] :y [0 1] :z [0 1]}
          p {:mapping {:x :x :y :y :z :z}
             :charts [(chart/cloud {:data [{:x 0.5 :y 0.0 :z 0.5}] :domain full :size 30 :color [255 0 0] :axes false :box false})
                      (chart/cloud {:data [{:x 0.5 :y 1.0 :z 1.0}] :type :spikes :line-width 30 :domain full :color [0 0 255]})]
             :scales {} :legend false}
          img (c2d/get-image (plot/render (assoc p :charts (vec (reverse (:charts p)))) {:width 300 :height 300}))
          img2 (c2d/get-image (plot/render p {:width 300 :height 300}))
          reds (fn [^BufferedImage im] (count (for [x (range 300) y (range 300) :let [cc (c/to-color (.getRGB im x y))]
                                                     :when (and (> (c/red cc) 200) (< (c/blue cc) 80))] 1)))]
      (is (= (reds img) (reds img2)) "chart order doesn't matter: depth does")
      (is (pos? (reds img)))))
  (testing "Z2 a 3D chart without :primitives is refused"
    (is (= :primitives (:key (error #(built {:data cube :mapping {:x :x :y :y :z :z} :charts [(dissoc (chart/cloud) :primitives)]}))))))
  (testing "Z3 a base outside an explicit z domain is kept inside it"
    (is (= 0.0 (get-in (built {:data cube :mapping {:x :x :y :y :z :z} :charts [(chart/cloud {:type :bars :base -5 :domain {:z [0 1]}})]})
                       [:stats 0 :data :panel :base])))
    (is (= 1.0 (get-in (built {:data cube :mapping {:x :x :y :y :z :z} :charts [(chart/cloud {:type :spikes :base 9 :domain {:z [1 0]}})]})
                       [:stats 0 :data :panel :base])))
    (is (= -5.0 (get-in (built {:data cube :mapping {:x :x :y :y :z :z} :charts [(chart/cloud {:type :bars :base -5})]})
                        [:stats 0 :data :panel :base])) "without an explicit domain the domain reaches the base"))
  (testing "Z4 seen from straight above the z axis is left out"
    (let [axes (atom [])
          draw-axis @#'space-draw/draw-axis!]
      (with-redefs-fn {#'space-draw/draw-axis! (fn [c k & args] (swap! axes conj k) (apply draw-axis c k args))}
        #(image {:data cube :mapping {:x :x :y :y :z :z} :charts [(chart/cloud {:view {:azimuth 30 :elevation 90}})]}))
      (is (= #{:x :y} (set @axes)))))
  (testing "groups: color by a category and size by a number give varying point styles and two legends"
    (let [p {:data cube :mapping {:x :x :y :y :z :z :color :g :size :v} :charts [(chart/cloud)]}
          b (built p)
          cp (get-in b [:stats 0 :data :panel])
          proj (space/projector (assoc (:frame cp) :domains (:domains cp)) {:w 300 :h 300 :margin 20})
          prims (cloud/chart-primitives (chart/cloud) cp (:point proj) (:finalized-scales b) {:theme (:theme b)})]
      (is (= 2 (count (distinct (map #(get-in % [:style :color]) prims)))))
      (is (= 4 (count (distinct (map #(get-in % [:style :size]) prims)))))
      (is (= 1 (count (filter #(= :legends (get-in % [:content :kind])) (:items (:grid b)))))))))

