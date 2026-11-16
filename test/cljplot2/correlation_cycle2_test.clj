(ns cljplot2.correlation-cycle2-test
  "Correlation cycle 2: 2D fields - sources and sampling, marching squares, the field (raster) and contour charts."
  (:require [clojure.test :refer [deftest testing is are]]
            [fastmath.core :as m]
            [fastmath.kernel :as k]
            [cljplot2.test-util :refer [error]]
            [clojure2d.core :as c2d]
            [cljplot2.chart :as chart]
            [cljplot2.chart.field :as field-chart]
            [cljplot2.core :as plot]
            [cljplot2.data.contour :as contour]
            [cljplot2.data.field :as field]
            [cljplot2.layout.grid :as grid]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.position :as scale])
  (:import [java.awt.image BufferedImage]))

(defn- rgb [^BufferedImage img x y]
  (let [v (.getRGB img (int x) (int y))]
    [(bit-and (bit-shift-right v 16) 0xff) (bit-and (bit-shift-right v 8) 0xff) (bit-and v 0xff)]))

(def ^:private plain {:plot {:background :white} :panel {:fill [250 250 250] :alternate-fill :none}})

(defn- render [p size]
  (let [p (update p :theme #(merge plain %))
        built (plot/build p size)]
    [built (plot/render-image p size) (grid/item-box (:layout built) :panel)]))

(defn- stats [p] (first (:stats (plot/build p {:width 400 :height 400}))))

(def ^:private cloud
  (let [rng (java.util.Random. 5)]
    (vec (repeatedly 300 (fn [] (let [a (.nextGaussian rng)] {:x a :y (+ (* 0.5 a) (.nextGaussian rng))}))))))

(def ^:private grouped-cloud (map #(assoc % :g (if (neg? (double (:x %))) "a" "b")) cloud))

;; 2. sources and sampling

(deftest density-source-test
  (testing "2.1 the fastmath 2D KDE, domain widened by :cut bandwidths"
    (let [xs (map :x cloud) ys (map :y cloud)
          {:keys [f domain]} (field/density xs ys {} [false false])
          {:keys [kde h]} (k/kernel-density-2d :gaussian xs ys :nrd true)
          [hx] h]
      (is (m/delta-eq (kde 0.3 0.1) (f 0.3 0.1)))
      (is (m/delta-eq (- (double (reduce min xs)) (* 3.0 (double hx))) (ffirst domain)))
      (is (m/delta-eq (double (reduce min xs)) (ffirst (:domain (field/density xs ys {:cut 0} [false false])))))))
  (testing "2.1 a log axis keeps its domain positive"
    (let [{:keys [domain]} (field/density [1 2 3 100] [1 2 3 4] {} [true false])]
      (is (== 1.0 (ffirst domain)))))
  (testing "2.5 one point with a selector: {:key :bandwidth}; with a number it estimates"
    (is (= :bandwidth (:key (error #(field/density [1] [1] {} [false false])))))
    (is (some? (field/density [1] [1] {:bandwidth 0.5} [false false])))))

(deftest lattice-source-test
  (let [rows (for [x [0 1 2] y [0 10]] {:x x :y y :v (+ x y)})
        acc (fn [k] #(get % k))]
    (testing "2.2 interpolation through the lattice values"
      (let [{:keys [f domain]} (field/lattice rows (acc :x) (acc :y) (acc :v) nil)]
        (is (m/delta-eq 11.0 (f 1.0 10.0)))
        (is (m/delta-eq 6.5 (f 1.5 5.0)))
        (is (= [[0.0 2.0] [0.0 10.0]] domain))))
    (testing "2.2 lattice errors"
      (is (= :value (:key (error #(field/lattice (rest rows) (acc :x) (acc :y) (acc :v) nil)))) "missing point")
      (is (= :value (:key (error #(field/lattice (conj (vec rows) {:x 0 :y 0 :v 9}) (acc :x) (acc :y) (acc :v) nil)))) "repeated point")
      (is (= :value (:key (error #(field/lattice [{:x 0 :y 0 :v 1} {:x 0 :y 1 :v 1}] (acc :x) (acc :y) (acc :v) nil)))) "one x"))
    (testing "non-finite values are NaN; non-finite positions left out"
      (let [{:keys [f]} (field/lattice (conj (vec (map #(if (= [1 0] [(:x %) (:y %)]) (assoc % :v nil) %) rows)) {:x nil :y 0 :v 1})
                                       (acc :x) (acc :y) (acc :v) :bilinear)]
        (is (m/nan? (f 1.0 0.0)))))))

(deftest function-source-test
  (testing "2.3 domain from the option, then explicit scale domains, else an error"
    (is (= [[0.0 1.0] [2.0 3.0]] (field/function-domain {:x [0 1] :y [2 3]} {})))
    (is (= [[0.0 5.0] [1.0 2.0]] (field/function-domain nil {:x {:domain [0 5]} :y {:domain [1 2]}})))
    (is (= :domain (:key (error #(field/function-domain nil {:x {:domain [0 5]}})))))))

(deftest sample-test
  (testing "2.4 a grid spanning the domain end to end"
    (let [{:keys [xs ys nx ny values]} (field/sample (constantly 2.0) [[0 1] [5 9]] [3 5])]
      (is (= [0.0 0.5 1.0] (vec xs)))
      (is (= [5.0 6.0 7.0 8.0 9.0] (vec ys)))
      (is (= [3 5] [nx ny]))
      (is (every? #(== 2.0 %) values))))
  (testing "2.5 non-finite values are NaN"
    (let [{:keys [values]} (field/sample (fn [x _] (if (pos? (double x)) (/ 1.0 0.0) nil)) [[0 1] [0 1]] [2 2])]
      (is (every? m/nan? values))))
  (testing "grid-value: bilinear inside, NaN outside or next to NaN"
    (let [g (field/sample (fn [x y] (+ (double x) (* 10.0 (double y)))) [[0 1] [0 1]] [3 3])]
      (is (m/delta-eq 5.25 (field/grid-value g 0.25 0.5)))
      (is (m/delta-eq 11.0 (field/grid-value g 1.0 1.0)) "the far corner")
      (is (m/nan? (field/grid-value g 1.01 0.5)))
      (is (m/nan? (field/grid-value g -0.01 0.5)))
      (is (m/nan? (field/grid-value (field/sample (fn [x _] (when (< (double x) 0.6) 1.0)) [[0 1] [0 1]] [3 3]) 0.75 0.5))))))

;; 4.1 / 4.2 marching squares

(def ^:private cone (field/sample (fn [x y] (- 1.0 (m/hypot (double x) (double y)))) [[-1 1] [-1 1]] [41 41]))

(deftest isolines-test
  (testing "4.1 a cone: one closed ring per level, on the circle"
    (doseq [level [0.2 0.5 0.8]]
      (let [[ring & more] (contour/isolines cone level)]
        (is (nil? more))
        (is (:closed? ring))
        (is (every? (fn [[x y]] (< (m/abs (- (m/hypot (double x) (double y)) (- 1.0 level))) 0.01)) (:points ring))))))
  (testing "4.1 a saddle resolved by the cell average"
    (let [saddle (field/sample (fn [x y] (* (double x) (double y))) [[-1 1] [-1 1]] [2 2])]
      (is (= #{[[0.0 -1.0] [1.0 0.0]] [[0.0 1.0] [-1.0 0.0]]} (set (map :points (contour/isolines saddle 0.0)))) "centre on the level counts as above")
      (is (= #{[[-1.0 -0.5] [-0.5 -1.0]] [[0.5 1.0] [1.0 0.5]]}
             (set (map (comp vec sort :points) (contour/isolines saddle 0.5)))) "average below the level: the high corners cut off")))
  (testing "4.1 a constant grid has no lines; a level outside the range none"
    (is (= [] (contour/isolines (field/sample (constantly 1.0) [[0 1] [0 1]] [4 4]) 1.0)))
    (is (= [] (contour/isolines cone 2.0))))
  (testing "4.1 NaN cells break the lines"
    (let [holed (field/sample (fn [x y] (when-not (and (< 0.4 (double x) 0.6)) (- 1.0 (m/hypot (double x) (double y))))) [[-1 1] [-1 1]] [41 41])
          lines (contour/isolines holed 0.5)]
      (is (= 1 (count lines)))
      (is (not (:closed? (first lines)))))))

(defn- shoelace [ring]
  (* 0.5 (reduce + (map (fn [[x1 y1] [x2 y2]] (- (* (double x1) (double y2)) (* (double x2) (double y1))))
                        ring (concat (rest ring) [(first ring)])))))

(deftest level-rings-test
  (testing "4.2 the region at or above a level: the cone's disk"
    (let [[ring] (contour/level-rings cone 0.5)]
      (is (< (m/abs (- 1.0 (/ (m/abs (shoelace ring)) (* m/PI 0.25)))) 0.02))))
  (testing "4.2 a ring field: the outer ring and the hole"
    (let [annulus (field/sample (fn [x y] (- 1.0 (m/abs (- (m/hypot (double x) (double y)) 0.5)))) [[-1 1] [-1 1]] [61 61])
          rings (contour/level-rings annulus 0.8)
          areas (sort (map (comp m/abs shoelace) rings))]
      (is (= 2 (count rings)))
      (is (< (m/abs (- 1.0 (/ (- (double (second areas)) (double (first areas))) (* m/PI (- 0.49 0.09))))) 0.03))))
  (testing "4.2 the whole domain at a level below the range; nothing above it"
    (let [[ring] (contour/level-rings cone -5.0)]
      (is (m/delta-eq 4.0 (m/abs (shoelace ring)) 1e-9)))
    (is (= [] (contour/level-rings cone 5.0)))))

(deftest levels-test
  (testing "4.3 evenly inside the range, or given"
    (is (= [0.25 0.5 0.75] (contour/levels [0 1] 3)))
    (is (= [0.5] (contour/levels [0 1] 1)))
    (is (= [-1 2] (contour/levels [0 1] [2 -1])))
    (is (= [] (contour/levels [1 1] 3)))
    (is (= [] (contour/levels nil 3)))))

;; 3. field chart

(def ^:private ramp {:charts [(chart/field {:f (fn [x _] x) :domain {:x [0 1] :y [0 1]}})]
                     :scales {:color {:range [[0 0 0] [255 255 255]]} :x {:domain [-0.5 1.5]}} :legend false})

(deftest field-chart-test
  (testing "3.2 the scale color of the value under each pixel; transparent outside the domain"
    (let [[built img {:keys [x y w h]}] (render ramp {:width 300 :height 300})
          proj (scale/projection (select-keys (:finalized-scales built) [:x :y]))
          at (fn [dx dy] (let [^fastmath.vector.Vec2 p (proj dx dy)] (rgb img (+ (long x) (long (.x p))) (+ (long y) (long (.y p))))))]
      (is (every? #(< (m/abs (- (double %) 128.0)) 4.0) (at 0.5 0.5)) "the middle value: mid grey")
      (is (every? #(< (double %) 20.0) (at 0.02 0.5)) "near 0: black")
      (is (= [250 250 250] (at 1.3 0.5)) "outside the domain: the panel shows")))
  (testing "3.2 flipped: the value follows x on the vertical axis"
    (let [[built img {:keys [x y]}] (render (assoc ramp :flip true) {:width 300 :height 300})
          proj (scale/projection (select-keys (:finalized-scales built) [:x :y]))
          ^fastmath.vector.Vec2 p (proj 0.9 0.5)]
      (is (every? #(> (double %) 200.0) (rgb img (+ (long x) (long (.x p))) (+ (long y) (long (.y p))))))))
  (testing "3.2 log scale renders"
    (is (instance? BufferedImage (plot/render-image {:charts [(chart/field {:f (fn [x y] (* x y)) :domain {:x [1 100] :y [0 1]}})]
                                                     :scales {:x {:kind :log}}} {:width 300 :height 300}))))
  (testing "3.2 raster time after sampling: the draw of a 600 x 600 plot's panel"
    (let [b (plot/build {:charts [(chart/field {:f (fn [x y] (* x y)) :domain {:x [0 1] :y [0 1]}})]} {:width 600 :height 600})
          box (grid/item-box (:layout b) :panel)
          computed (get-in b [:stats 0 :data :panel])
          draw! #(c2d/with-canvas [c (c2d/canvas 600 600)] (field-chart/draw nil computed c box (:finalized-scales b) {:theme (:theme b)}))]
      (draw!)
      (let [t (System/nanoTime)]
        (draw!)
        (is (< (/ (- (System/nanoTime) t) 1.0e6) 200.0)))))
  (testing "3.1 extents, value range over the panels, legend title by source"
    (let [s (stats {:data cloud :mapping {:x :x :y :y} :charts [(chart/field {:n 20})]})]
      (is (= [20 20] ((juxt :nx :ny) (get-in s [:data :panel :grid]))))
      (is (pos? (double (get-in s [:extents :panel :color :max])))))
    (is (= {:color "Density"} (aesthetic/computed-aesthetics (chart/field) {:x :x :y :y})))
    (is (= {:color "z"} (aesthetic/computed-aesthetics (chart/field) {:x :x :y :y :value :z})))
    (is (= {:color nil} (aesthetic/computed-aesthetics (chart/field {:f +}) {}))))
  (testing "3.1 the color is computed: another chart can't map it; the field's own mapping neither"
    (is (some? (error #(stats {:data cloud :mapping {:x :x :y :y} :charts [(chart/field) (chart/scatter {:mapping {:color :x}})]}))))
    (is (some? (error #(stats {:data cloud :mapping {:x :x :y :y :color :x} :charts [(chart/field)]})))))
  (testing "2.5 empty data: no field; invalid positions dropped and counted"
    (let [s (stats {:data (conj cloud {:x nil :y 1} {:x 1 :y ##NaN}) :mapping {:x :x :y :y} :charts [(chart/field {:n 5})]})]
      (is (= {:x 1 :y 1} (:dropped s))))
    (is (empty? (:data (stats {:data [] :mapping {:x :x :y :y} :charts [(chart/field)]}))))))

(deftest field-facets-test
  (testing "one grid domain over the panels, a density per panel"
    (let [s (stats {:data (map #(assoc % :f (if (neg? (double (:x %))) "l" "r")) cloud) :mapping {:x :x :y :y :facet :f}
                    :charts [(chart/field {:n 10})]})
          gl (get-in s [:data "l" :grid]) gr (get-in s [:data "r" :grid])]
      (is (= (vec (:xs gl)) (vec (:xs gr))))
      (is (not= (vec (:values gl)) (vec (:values gr)))))))

;; 4. contour chart

(deftest contour-chart-test
  (testing "4.3 :levels - a count or given values"
    (let [levels (fn [opts] (distinct (map :level (get-in (stats {:data cloud :mapping {:x :x :y :y} :charts [(chart/contour opts)]})
                                                         [:data :panel :contours]))))]
      (is (= 10 (count (levels {}))))
      (is (= [0.01 0.05] (levels {:levels [0.05 0.01]})))))
  (testing "4.4 by level: a computed color; a fixed :color or grouping: none"
    (is (= {:color "Density"} (aesthetic/computed-aesthetics (chart/contour) {:x :x :y :y})))
    (is (= {} (aesthetic/computed-aesthetics (chart/contour {:color :black}) {:x :x :y :y})))
    (is (= {} (aesthetic/computed-aesthetics (chart/contour) {:x :x :y :y :color :g})))
    (is (nil? (error #(stats {:data grouped-cloud :mapping {:x :x :y :y :color :g}
                              :charts [(chart/scatter) (chart/contour)]})))
        "a scatter colored by group with grouped contours"))
  (testing "4.4 grouped: a field per group, shared levels"
    (let [s (stats {:data grouped-cloud :mapping {:x :x :y :y :color :g}
                    :charts [(chart/contour {:levels 3})]})
          cs (get-in s [:data :panel :contours])]
      (is (= #{{:color "a"} {:color "b"}} (set (map :aesthetics cs))))
      (is (= 6 (count cs)))
      (is (= {:kind :categorical :values ["a" "b"]} (select-keys (get-in s [:extents :panel :color]) [:kind :values])))))
  (testing "4.2 filled regions drawn lowest first; lines otherwise"
    (let [d (get-in (stats {:data cloud :mapping {:x :x :y :y} :charts [(chart/contour {:fill true :levels 3})]}) [:data :panel])]
      (is (:fill? d))
      (is (every? :rings (:contours d))))))

(deftest contour-draw-test
  (testing "4.4 a fixed color line crosses the cone where expected"
    (let [cone-f (fn [x y] (- 1.0 (m/hypot (double x) (double y))))
          [built img {:keys [x y]}] (render {:charts [(chart/contour {:f cone-f :domain {:x [-1 1] :y [-1 1]} :levels [0.5] :color [255 0 0] :width 3})]
                                             :legend false}
                                            {:width 300 :height 300})
          proj (scale/projection (select-keys (:finalized-scales built) [:x :y]))
          ^fastmath.vector.Vec2 on (proj 0.5 0.0)
          ^fastmath.vector.Vec2 off (proj 0.07 0.11)]
      (is (= [255 0 0] (rgb img (+ (long x) (long (m/round (.x on)))) (+ (long y) (long (m/round (.y on)))))))
      (is (= [250 250 250] (rgb img (+ (long x) (long (.x off))) (+ (long y) (long (.y off))))))))
  (testing "4.2 filled: the disk filled, the hole of an annulus open"
    (let [annulus (fn [x y] (- 1.0 (m/abs (- (m/hypot (double x) (double y)) 0.5))))
          [built img {:keys [x y]}] (render {:charts [(chart/contour {:f annulus :domain {:x [-1 1] :y [-1 1]} :levels [0.8] :fill true :n 81})]
                                             :scales {:color {:range [[0 0 200] [0 0 200]]}} :legend false}
                                            {:width 300 :height 300})
          proj (scale/projection (select-keys (:finalized-scales built) [:x :y]))
          at (fn [dx dy] (let [^fastmath.vector.Vec2 p (proj dx dy)] (rgb img (+ (long x) (long (.x p))) (+ (long y) (long (.y p))))))]
      (is (= [0 0 200] (at 0.5 0.0)) "on the ring")
      (is (= [250 250 250] (at 0.07 0.11)) "the hole (off the grid lines)")
      (is (= [250 250 250] (at 0.95 0.95)) "outside")))
  (testing "4.5 log x: renders, legend for groups"
    (is (instance? BufferedImage (plot/render-image {:data (map #(update % :x (fn [v] (m/exp (double v)))) cloud) :mapping {:x :x :y :y}
                                                     :scales {:x {:kind :log}} :charts [(chart/contour)]}
                                                    {:width 300 :height 300})))
    (is (instance? BufferedImage (plot/render-image {:data grouped-cloud
                                                     :mapping {:x :x :y :y :color :g} :charts [(chart/contour {:fill true})]}
                                                    {:width 300 :height 300})))))

(deftest theme-test
  (testing "theme entries validated"
    (is (some? (error #(plot/build {:charts [(chart/field {:f + :domain {:x [0 1] :y [0 1]}})] :theme {:chart {:field {:alpha 2}}}}
                                   {:width 200 :height 200}))))
    (is (some? (error #(plot/build {:charts [(chart/contour {:f + :domain {:x [0 1] :y [0 1]}})] :theme {:chart {:contour {:bogus 1}}}}
                                   {:width 200 :height 200}))))
    (is (some? (error #(stats {:charts [(chart/contour {:levels 0})]}))))
    (is (some? (error #(stats {:charts [(chart/field {:n 1})]}))))
    (is (some? (error #(stats {:charts [(chart/field {:bandwidth [1 :bogus]})]}))))))
