(ns cljplot2.space3d-perspective-test
  "3D perspective (`:view {:distance δ}`, lattice's `distance`): the option and its limit, the projection and its
  turning fit, position-aware face visibility (box faces, bars), ticks on projected points, 3D charts rendering."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.space3d :as space]
            [cljplot2.content.space3d :as space-draw]
            [cljplot2.test-util :refer [error-data error-ins]])
  (:import [fastmath.vector Vec2]))

(def ^:private unit {:x [0 1] :y [0 1] :z [0 1]})

(defn- proj
  "Projector of the unit data box in a 400 x 300 panel, margin 20."
  ([view] (proj view [1 1 1]))
  ([view aspect] (space/projector {:domains unit :aspect aspect :view view} {:w 400 :h 300 :margin 20})))

(defn- dist ^double [^Vec2 a ^Vec2 b] (v/mag (v/sub a b)))

(def ^:private cube
  (vec (for [x [0 1 2] y [0 1 2]] {:x x :y y :z (+ x y 1)})))

;; 1.1 - 1.2 the option and its limit

(deftest distance-option-test
  (testing "1.1 :view {:distance δ} validates for δ >= 0 (0 - orthographic)"
    (doseq [d [0 0.2 1]]
      (is (some? (plot/build {:data cube :mapping {:x :x :y :y :z :z} :charts [(chart/cloud {:view {:distance d}})]})))))
  (testing "1.1 boundary: negative or not a number - a schema error at [:view :distance]"
    (doseq [d [-0.1 "0.2" ##Inf]]
      (is (some #(= [:view :distance] (take-last 2 %))
                (error-ins #(plot/build {:data cube :mapping {:x :x :y :y :z :z} :charts [(chart/cloud {:view {:distance d}})]})))
          (pr-str d))))
  (testing "1.2 the eye within the box's sphere (δ / L · |aspect| / 2 >= 1, L - the longest side): an error; just below passes"
    (let [cube-limit (/ 2.0 (m/sqrt 3.0))]
      (is (some? (space/distance-problem {:distance 1.2} [1 1 1])))
      (is (some? (space/distance-problem {:distance cube-limit} [1 1 1])) "at the limit")
      (is (nil? (space/distance-problem {:distance (* 0.999 cube-limit)} [1 1 1])))
      (is (nil? (space/distance-problem {:distance 0.2} [1 1 1])))
      (is (nil? (space/distance-problem {} [1 1 1])) "no distance")
      (is (some? (space/distance-problem {:distance 1.2} nil)) "nil aspect: a cube")
      (is (nil? (space/distance-problem {:distance 1.1} [6 6 6])) "a larger cube: the same limit (proportions only)")
      (is (nil? (space/distance-problem {:distance 1.3} [1 1 0.2])) "a flat box takes more: 1/0.714")
      (is (some? (space/distance-problem {:distance 1.41} [1 1 0.2])))
      (is (= :view (:key (error-data #(space/check-distance! {:distance 1.2} [1 1 1])))))
      (is (nil? (space/check-distance! {:distance 0.2} [1 1 1])))))
  (testing "1.2 / F6 a chart's own view and aspect: a schema error at the chart's [:view :distance] (cloud and surface)"
    (is (contains? (error-ins #(plot/build {:data cube :mapping {:x :x :y :y :z :z}
                                            :charts [(chart/scatter) (chart/cloud {:view {:distance 1.2}})]}))
                   [:charts 1 :options :view :distance]))
    (is (contains? (error-ins #(plot/build {:charts [(chart/surface {:f (fn [[x y]] (* x y)) :domain {:x [0 1] :y [0 1]}
                                                                     :view {:distance 1.3} :aspect [1 1 1]})]}))
                   [:charts 0 :options :view :distance])))
  (testing "1.2 a scene merging frames (a later chart's aspect) is checked when drawn"
    (let [domain {:x [0 2] :y [0 2] :z [0 5]}
          scene (fn [aspect] {:data cube :mapping {:x :x :y :y :z :z}
                              :charts [(chart/cloud {:domain domain :view {:distance 1.3} :aspect [1 1 0.2]})
                                       (chart/cloud {:domain domain :aspect aspect})]})]
      (is (some? (plot/render-image (scene [1 1 0.2]) {:width 300 :height 300})) "each chart alone is fine")
      (is (= :view (:key (error-data #(plot/render-image (scene [1 1 1]) {:width 300 :height 300}))))))))

(deftest box-units-test
  (testing "F1 :distance is measured in a box of longest side 1 (lattice's): proportions alone set the picture"
    (doseq [view [{:azimuth 40 :elevation 30 :distance 0.2} {:azimuth 200 :elevation -20 :distance 0.9}]]
      (let [a (proj view [1 1.5 0.8]) b (proj view [2 3 1.6])]
        (is (every? true? (map (fn [c] (< (dist ((:corner a) c) ((:corner b) c)) 1e-9)) space/corners))))))
  (testing "F1 a box of longest side 1 is unchanged; a longer one gets weaker perspective than the raw δ would give"
    (is (= 0.2 (space/box-distance {:distance 0.2} [1 0.7 0.5])))
    (is (= 0.1 (space/box-distance {:distance 0.2} [1 2 0.5])))
    (is (= 0.0 (space/box-distance {} [1 2 0.5])))))

;; 1.3 - 1.4 projection and fit

(deftest projection-test
  (testing "1.3 δ = 0 gives the orthographic pixels and depths"
    (doseq [view [space/default-view {:azimuth 0 :elevation 0} {:azimuth 200 :elevation -40}]]
      (let [a (proj view) b (proj (assoc view :distance 0))]
        (is (= (map (:corner a) space/corners) (map (:corner b) space/corners)))
        (is (= ((:point a) [0.3 0.7 0.1]) ((:point b) [0.3 0.7 0.1]))))))
  (testing "1.3 δ = 0.2: the nearest corner farther from the box centre than the farthest one (orthographic: equal)"
    (let [view (assoc space/default-view :distance 0.2)
          {:keys [point centre]} (proj view)
          by-depth (sort-by second (map #(point (mapv double %)) space/corners))
          [near] (first by-depth) [far] (last by-depth)]
      (is (> (dist near centre) (* 1.1 (dist far centre))))
      (let [{:keys [point centre]} (proj space/default-view)
            by-depth (sort-by second (map #(point (mapv double %)) space/corners))]
        (is (m/delta-eq (dist (first (first by-depth)) centre) (dist (first (last by-depth)) centre) 1e-9)))))
  (testing "1.3 the box centre projects on :centre at its depth 0; the depth is the view depth"
    (let [{:keys [point centre]} (proj (assoc space/default-view :distance 0.2))
          [px d] (point [0.5 0.5 0.5])]
      (is (< (dist px centre) 1e-9))
      (is (m/delta-eq 0.0 (double d) 1e-12))))
  (testing "1.4 every corner inside the panel less the margin at every azimuth (also between whole degrees)"
    (doseq [d [0.2 1.1] e [-90 -30 0 30 75 90] a (concat (range 0.25 360 3.1) [137.25 359.9]) aspect [[1 1 1] [1 1.5 0.6]]]
      (let [{:keys [corner]} (proj {:azimuth a :elevation e :distance d} aspect)
            ps (map corner space/corners)]
        (is (every? #(<= 20.0 (.x ^Vec2 %) 380.0) ps) (pr-str [d a e aspect]))
        (is (every? #(<= 20.0 (.y ^Vec2 %) 280.0) ps) (pr-str [d a e aspect])))))
  (testing "1.4 one box size and centre whatever the azimuth; it fills the panel at some azimuth"
    (let [at (fn [a] (proj {:azimuth a :elevation 30 :distance 0.2}))]
      (is (= (:scale (at 0)) (:scale (at 137)) (:scale (at 290))))
      (is (= (:centre (at 0)) (:centre (at 137))))
      (is (some (fn [a] (let [ps (map (:corner (at a)) space/corners)]
                          (or (some #(< (.x ^Vec2 %) 21.0) ps) (some #(< (.y ^Vec2 %) 21.0) ps))))
                (range 0 360)))))
  (testing "1.4 boundary: a panel smaller than its margins - scale 0"
    (is (zero? (double (:scale (space/projector {:domains unit :view {:distance 0.2}} {:w 30 :h 30 :margin 20})))))))

;; 1.5 - 1.6 visibility

(deftest visibility-test
  (let [top-back {:axis :x :from [0 1 1] :to [1 1 1]}
        low {:azimuth 0 :elevation 2}]
    (testing "1.5 elevation 2: orthographic shows the top face; with the eye below the top (δ 0.2) it's hidden"
      (is (some #{top-back} (space/visible-edges low [1 1 1])))
      (is (not (some #{top-back} (space/visible-edges (assoc low :distance 0.2) [1 1 1]))))
      (is (some #{top-back} (space/hidden-edges (assoc low :distance 0.2) [1 1 1]))))
    (testing "1.5 a flat box (aspect z 0.2): the eye above the top again - the top shows"
      (is (some #{top-back} (space/visible-edges (assoc low :distance 0.2 :elevation 10) [1 1 0.2])))))
  (testing "1.5 far side: the farther-turned face when the eye is between both (perspective), the orthographic one else"
    (is (= 0 (space/far-side {:azimuth 0 :elevation 2 :distance 0.2} [1 1 1] :z)) "both turn away, the bottom farther")
    (is (= 0 (space/far-side {:azimuth 0 :elevation 2} [1 1 1] :z)))
    (is (= 1 (space/far-side {:azimuth 0 :elevation 0 :distance 0.2} [1 1 1] :x)) "eye centred: both x faces turn away equally - the high one")
    (is (nil? (space/far-side {:azimuth 0 :elevation 0} [1 1 1] :x)) "orthographic: both edge-on")
    (is (= 1 (space/far-side {:azimuth 0 :elevation 0 :distance 0.2} [1 1 1] :y))))
  (testing "1.5 front view (azimuth 0, elevation 0): only the front face, as orthographic - the sides turn away"
    (is (= 4 (count (space/visible-edges {:azimuth 0 :elevation 0 :distance 0.2} [1 1 1])))))
  (testing "1.5 the axes stay on outline edges under perspective"
    (let [view (assoc space/default-view :distance 0.2)
          edges (space/axis-edges view [1 1 1] (:corner (proj view)))]
      (is (= #{:x :y :z} (set (keys edges))))
      (is (every? (fn [[k e]] (= k (:axis e))) edges))))
  (let [frame (fn [view domains] {:domains domains :aspect [1 1 1] :view view})
        front (fn [view domains axis side at] (second ((space/face-orientation (frame view domains)) axis side at)))
        ahead {:azimuth 0 :elevation 0}]
    (testing "1.6 a bar right of the eye shows its left side under perspective; orthographic shows it edge-on (hidden)"
      (is (true? (front (assoc ahead :distance 0.2) unit :x 0 [0.8 0.5 0.5])))
      (is (false? (front ahead unit :x 0 [0.8 0.5 0.5])))
      (is (false? (front (assoc ahead :distance 0.2) unit :x 1 [0.8 0.5 0.5])) "its right side turns away"))
    (testing "1.6 a reversed domain turns the data-low side to the box's high side"
      (is (false? (front (assoc ahead :distance 0.2) {:x [1 0] :y [0 1] :z [0 1]} :x 0 [0.2 0.5 0.5])))
      (is (true? (front (assoc ahead :distance 0.2) {:x [1 0] :y [0 1] :z [0 1]} :x 1 [0.2 0.5 0.5]))))
    (testing "1.6 orthographic as before: the face's normal alone, reversed domain or not"
      (is (true? (front space/default-view unit :z 1 [0.5 0.5 0.5])))
      (is (true? (front space/default-view {:x [0 1] :y [0 1] :z [1 0]} :z 0 [0.5 0.5 0.5]))
          "the data-low side of a reversed z is the box top"))))

;; 2.1 ticks

(deftest edge-point-test
  (testing "2.1 a point along an edge is the projected surface point: the middle of a receding edge isn't the screen middle"
    (let [view (assoc space/default-view :distance 0.2)
          {:keys [corner]} (proj view)
          edge {:axis :y :from [0 0 0] :to [0 1 0]}
          mid (space-draw/edge-point corner edge 0.5)]
      (is (< (dist mid (corner [0 0.5 0])) 1e-9))
      (is (> (dist mid (v/mult (v/add (corner [0 0 0]) (corner [0 1 0])) 0.5)) 0.5))))
  (testing "2.1 orthographic: the screen middle; ends at the corners"
    (let [{:keys [corner]} (proj space/default-view)
          edge {:axis :x :from [0 1 0] :to [1 1 0]}]
      (is (< (dist (space-draw/edge-point corner edge 0.5) (v/mult (v/add (corner [0 1 0]) (corner [1 1 0])) 0.5)) 1e-9))
      (is (< (dist (space-draw/edge-point corner edge 0.0) (corner [0 1 0])) 1e-12))
      (is (< (dist (space-draw/edge-point corner edge 1.0) (corner [1 1 0])) 1e-12)))))

;; 3.1 charts

(deftest perspective-charts-test
  (let [view {:azimuth 30 :elevation 25 :distance 0.2}
        render (fn [p] (c2d/get-image (plot/render p {:width 400 :height 300})))
        cloud (fn [opts] {:data cube :mapping {:x :x :y :y :z :z} :charts [(chart/cloud (merge {:view view} opts))]})]
    (testing "3.1 clouds: points, spikes, bars, projections, a 2D histogram"
      (doseq [opts [{} {:type :spikes} {:type :bars} {:projections {:xy :density :xz :contour :yz :points}}
                    {:bins [3 3] :type :bars}]]
        (is (some? (render (if (:bins opts)
                             {:data cube :mapping {:x :x :y :y} :charts [(chart/cloud (merge {:view view} opts))]}
                             (cloud opts))))
            (pr-str opts))))
    (testing "3.1 surfaces: three styles, a parametric one"
      (doseq [style [:surface :wireframe :both]]
        (is (some? (render {:charts [(chart/surface {:f (fn [[x y]] (* x y)) :domain {:x [-1 1] :y [-1 1]} :n 8
                                                     :style style :view view})]}))))
      (is (some? (render {:charts [(chart/surface {:fu (fn [[u w]] [(m/cos u) (m/sin u) w]) :u [0 m/TWO_PI] :v [0 1]
                                                   :n 12 :view view})]}))))
    (testing "3.1 boundary: from straight above and below, facets"
      (doseq [e [90 -90]]
        (is (some? (render (cloud {:type :bars :view {:azimuth 0 :elevation e :distance 0.2}})))))
      (is (some? (render (assoc (cloud {}) :data (mapv #(assoc % :g (if (even? (:x %)) "a" "b")) cube)
                                :mapping {:x :x :y :y :z :z :facet :g})))))
    (testing "3.1 bar faces turn by the scene's view - a later chart's view override reaches the bars"
      (let [domain {:x [-0.5 2.5] :y [-0.5 2.5] :z [0 6]}
            faces (fn [charts]
                    (let [log (atom [])
                          draw @#'space-draw/draw-primitives!]
                      (with-redefs-fn {#'space-draw/draw-primitives! (fn [c ps] (reset! log (vec ps)) (draw c ps))}
                        #(render {:data cube :mapping {:x :x :y :y :z :z} :charts charts}))
                      (count (filter #(= :polygon (:kind %)) @log))))]
        (is (= (faces [(chart/cloud {:domain domain :type :bars :view {:azimuth 0 :elevation 0}})])
               (faces [(chart/cloud {:domain domain :type :bars})
                       (chart/cloud {:domain domain :type :none :view {:azimuth 0 :elevation 0}})]))
            "front view: one face per bar either way")))))
