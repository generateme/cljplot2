(ns cljplot2.code-review-r2-test
  "Code review round 2, batch B1 - the 3D shading options (`:ambient`, `:light`, [[cljplot2.data.space3d/shader]]) and
  the view rotation as one fastmath matrix ([[cljplot2.data.space3d/view-matrix]])."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.color :as c]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.graph :as graph]
            [cljplot2.data.space3d :as space]
            [cljplot2.describe :as describe]
            [cljplot2.test-util :refer [error]])
  (:import [fastmath.vector Vec3]))

(defn- old-shade
  "The shading before the options: ambient 0.4, light from the viewer's upper left."
  [color normal]
  (let [light (v/normalize (Vec3. -0.5 0.6 -1.0))
        k (+ 0.4 (* (- 1.0 0.4) (m/abs (v/dot normal light))))
        col (c/to-color color)]
    (c/color (* k (c/red col)) (* k (c/green col)) (* k (c/blue col)) (c/alpha col))))

(def ^:private normals
  (map v/normalize [(Vec3. -0.5 0.6 -1.0) (Vec3. 1.0 0.0 -0.5) (Vec3. 0.0 0.0 1.0) (Vec3. 0.3 -0.8 0.2) (Vec3. 0.0 0.0 0.0)]))

(deftest shader-test
  (testing "defaults: today's colours"
    (let [shade (space/shader {})]
      (doseq [n normals col [[200 200 200] [10 120 250 128] :red]]
        (is (= (old-shade col n) (shade col n))))))
  (testing "boundary: ambient 0 - the light alone (a face edge-on to the light is black); 1 - the colour itself"
    (let [dark (space/shader {:ambient 0.0 :light [0.0 0.0 1.0]})
          flat (space/shader {:ambient 1.0})]
      (is (= (c/color 0 0 0) (dark [200 100 50] (Vec3. 1.0 0.0 0.0))))
      (is (= (c/color 200 100 50) (dark [200 100 50] (Vec3. 0.0 0.0 -1.0))) "two-sided: facing away lit as well")
      (doseq [n normals] (is (= (c/to-color [200 100 50 70]) (flat [200 100 50 70] n))))))
  (testing "boundary: the light direction is normalized; a zero vector - an error keyed :light"
    (let [n (v/normalize (Vec3. 0.3 -0.8 0.2))]
      (is (every? #(< (m/abs (double %)) 1e-9)
                  (v/sub ((space/shader {:light [1.0 2.0 -3.0]}) :red n) ((space/shader {:light [10.0 20.0 -30.0]}) :red n)))))
    (is (= :light (:key (error #(space/shader {:light [0.0 0.0 0.0]})))))))

(def ^:private surface-plot {:charts [(chart/surface {:f (fn [[x y]] (* x y)) :domain {:x [-1 1] :y [-1 1]} :n 4 :color [200 100 50]})]})

(def ^:private bars-plot
  {:data [{:x 1 :y 1 :z 2} {:x 2 :y 1 :z 3} {:x 1 :y 2 :z 1}]
   :mapping {:x :x :y :y :z :z}
   :charts [(chart/cloud {:type :bars :color [200 100 50]})]})

(defn- fills
  "Fill colours of the polygons the first chart of `p` (options `options` merged) draws in a 400 x 300 panel (the
  frame with the domains, as a scene's)."
  [p options]
  (let [p (update-in p [:charts 0 :options] merge options)
        b (plot/build p {:width 500 :height 400})
        ch (first (:charts p))
        cp (let [cp (get-in b [:stats 0 :data :panel])] (assoc-in cp [:frame :domains] (:domains cp)))
        proj (space/projector (assoc (:frame cp) :domains (:domains cp)) {:w 400 :h 300 :margin 20})]
    (->> ((:primitives ch) ch cp (:point proj) (:finalized-scales b) {:theme (:theme b)})
         (filter #(= :polygon (:kind %)))
         (map #(c/to-color (:fill %))))))

(deftest shading-options-test
  (testing "surface quads and cloud bars take :ambient and :light"
    (doseq [p [surface-plot bars-plot]]
      (is (every? #(= (c/color 200 100 50) %) (fills p {:ambient 1.0})) "ambient 1 - no shading")
      (is (not-every? #(= (c/color 200 100 50) %) (fills p {})) "shaded by default")
      (is (not= (fills p {}) (fills p {:light [1.0 0.0 0.0]})) "another light - other colours")))
  (testing "defaults: the same colours as without the options"
    (doseq [p [surface-plot bars-plot]]
      (is (= (fills p {}) (fills p {:ambient 0.4 :light [-0.5 0.6 -1.0]})))))
  (testing "boundary: options errors - ambient outside [0 1], a zero or short light"
    (doseq [p [surface-plot bars-plot]]
      (is (= [:charts 0 :options :ambient] (:in (error #(plot/build (assoc-in p [:charts 0 :options :ambient] 1.5))))))
      (is (= [:charts 0 :options :ambient] (:in (error #(plot/build (assoc-in p [:charts 0 :options :ambient] -0.1))))))
      (is (= [:charts 0 :options :light] (:in (error #(plot/build (assoc-in p [:charts 0 :options :light] [0 0 0]))))))
      (is (= [:charts 0 :options :light] (:in (error #(plot/build (assoc-in p [:charts 0 :options :light] [1 0]))))))))
  (testing "boundary: only the shaded 3D charts take the group"
    (is (every? (fn [ch] (some #(= :light (:key %)) (describe/options ch))) [(chart/surface {}) (chart/cloud {})]))
    (is (not-any? #(#{:ambient :light} (:key %)) (describe/options (chart/image))))
    (is (= [:charts 0 :options :ambient] (:in (error #(plot/build {:charts [(chart/text {:text "a" :x 1 :y 1 :ambient 0.5})]})))))
    (is (= [:charts 0 :options :light] (:in (error #(plot/build {:charts [(chart/text {:text "a" :x 1 :y 1 :light [1 0 0]})]})))))))

(deftest graph-directed-test
  (testing "boundary: :directed? missing, nil or false - false; true - true (the documented default holds for nil)"
    (is (= [false false false true] (map #(:directed? (graph/graph [] %)) [{} {:directed? nil} {:directed? false} {:directed? true}])))))

(defn- old-rotation
  "The view rotation before the matrix: the hand-written formula."
  [{:keys [azimuth elevation]} [x y z]]
  (let [a (m/radians azimuth) e (m/radians elevation)
        ca (m/cos a) sa (m/sin a) ce (m/cos e) se (m/sin e)
        u (+ (* x ca) (* y sa))
        d0 (- (* y ca) (* x sa))]
    [u (+ (* z ce) (* d0 se)) (- (* d0 ce) (* z se))]))

(deftest rotation-matrix-test
  (testing "the matrix gives the old formula's view coordinates within 1e-12 (8 views x 8 corners)"
    (doseq [view [{:azimuth 0.0 :elevation 0.0} {:azimuth 40.0 :elevation 30.0} {:azimuth 90.0 :elevation 0.0}
                  {:azimuth 180.0 :elevation 90.0} {:azimuth -135.0 :elevation -90.0} {:azimuth 270.0 :elevation 45.0}
                  {:azimuth 12.5 :elevation -20.0} {:azimuth 721.0 :elevation 60.0}]
            corner space/corners
            :let [p (space/corner-point corner [1.0 2.0 0.5])
                  q ((space/rotation view) p)]]
      (is (every? #(< (m/abs (double %)) 1e-12) (map - (old-rotation view p) q)) (str view " " corner))))
  (testing "boundary: missing angles - the default view; azimuth 0, elevation 0 looks along +y"
    (is (= (space/view-matrix space/default-view) (space/view-matrix {})))
    (let [q ((space/rotation {:azimuth 0.0 :elevation 0.0}) (Vec3. 1.0 2.0 3.0))]
      (is (every? #(< (m/abs (double %)) 1e-15) (map - [1.0 3.0 2.0] q)) "x right, z up, y depth"))))
