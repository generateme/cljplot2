(ns cljplot2.ranking-cycle3-test
  "Ranking cycle 3: polar coordinates - the `:coord` option, polar scales and geometry, range-space paths, chart
  support, guides."
  (:require [clojure.test :refer [deftest testing is are]]
            [fastmath.core :as m]
            [cljplot2.test-util :refer [error error-data]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.scale.coord :as coord])
  (:import [java.awt.image BufferedImage]
           [fastmath.vector Vec2]))

(def ^:private size {:width 400 :height 300})
(defn- build [p] (plot/build p size))
(defn- rgb [^BufferedImage img x y]
  (let [v (.getRGB img (int x) (int y))] [(bit-and (bit-shift-right v 16) 0xff) (bit-and (bit-shift-right v 8) 0xff) (bit-and v 0xff)]))
(defn- near? [[r g b] [r' g' b']] (every? #(<= (Math/abs (long %)) 40) [(- r r') (- g g') (- b b')]))
(def ^:private red [255 0 0])
(def ^:private plain {:plot {:background :white} :panel {:fill :white :grid {:x :none :y :none :bands-fill :none}}})

(def ^:private sales [{:c "a" :v 10} {:c "b" :v 20} {:c "c" :v 30} {:c "d" :v 40}])
(def ^:private polar {:kind :polar})

(defn- geometry [built] (or (get-in built [:finalized-scales :x :coord]) (get-in built [:finalized-scales :y :coord])))

(defn- panel-origin
  "Top-left pixel of the (first) panel of `built`."
  [built]
  (let [box (cljplot2.layout.grid/find-content-box (:grid built) (:layout built) #(= :panel (:kind %)))]
    [(:x box) (:y box)]))

(defn- pixel-at
  "Image pixel of panel point `^Vec2 p` (panel-local) of `built`."
  [^BufferedImage img built ^Vec2 p]
  (let [[x0 y0] (panel-origin built)]
    (rgb img (+ (double x0) (.x p)) (+ (double y0) (.y p)))))

(defn- red-around?
  "Is a pixel of the 3 x 3 neighbourhood of panel point `p` of `built` red (antialiased lines land between pixels)?"
  [^BufferedImage img built ^Vec2 p]
  (some #(near? red (pixel-at img built (Vec2. (+ (.x p) (double (first %))) (+ (.y p) (double (second %))))))
        (for [dx [-1 0 1] dy [-1 0 1]] [dx dy])))

(deftest coord-option
  (testing "1.1 the schema"
    (are [c ok?] (= ok? (nil? (error #(build {:data sales :mapping {:x :c :y :v} :coord c :charts [(chart/scatter)]}))))
      nil true
      {:kind :cartesian} true
      polar true
      {:kind :polar :start 90 :direction :counter-clockwise :span 180 :inner 0.5 :arcs true} true
      {:kind :polar :inner 1.0} false
      {:kind :polar :inner -0.1} false
      {:kind :polar :span 0} false
      {:kind :polar :span 400} false
      {:kind :polar :direction :up} false
      {:kind :polar :radius 3} false
      {:kind :spherical} false)
    (is (= [:coord :inner] (:in (error-data #(build {:data sales :mapping {:x :c :y :v} :coord {:kind :polar :inner 2}
                                                     :charts [(chart/scatter)]}))))))
  (testing "1.1.2 not with :aspect :iso or :margins"
    (is (= :coord (:key (error #(build {:data sales :mapping {:x :v :y :v} :coord polar :aspect :iso :charts [(chart/scatter)]})))))
    (is (= :coord (:key (error #(build {:data sales :mapping {:x :v :y :v} :coord polar
                                        :margins {:top (chart/histogram)} :charts [(chart/scatter)]})))))))

(deftest polar-scales
  (testing "1.2 the angle over [start, start + span] radians, the radius over [inner R, R]"
    (let [b (build {:data sales :mapping {:x :c :y :v} :coord polar :charts [(chart/scatter)]})
          {:keys [radius inner-radius cx cy]} (geometry b)]
      (is (= [0.0 (* 2.0 Math/PI)] (mapv double (get-in b [:finalized-scales :x :range]))))
      (is (= [(double inner-radius) (double radius)] (mapv double (get-in b [:finalized-scales :y :range]))))
      (is (< 0.0 (double radius) (* 0.5 (min (* 2.0 (double cx)) (* 2.0 (double cy))))) "room for the angle labels")
      (is (seq (get-in b [:finalized-scales :y :ticks])))
      (is (= :horizontal (get-in b [:finalized-scales :x :direction])))))
  (testing "1.2 :flip puts the angle on :y; :inner and :start / :direction shape the ranges"
    (let [b (build {:data sales :mapping {:x :v :y :c} :flip true :coord {:kind :polar :inner 0.5 :start 90 :direction :counter-clockwise}
                    :charts [(chart/bar)]})
          {:keys [radius inner-radius]} (geometry b)]
      (is (= :horizontal (get-in b [:finalized-scales :y :direction])))
      (is (< (Math/abs (- (* 0.5 (double radius)) (double inner-radius))) 1e-9))
      (let [[t0 t1] (get-in b [:finalized-scales :y :range])]
        (is (< (Math/abs (- (double t0) (/ Math/PI 2))) 1e-9))
        (is (< (Math/abs (- (double t1) (- (/ Math/PI 2) (* 2 Math/PI)))) 1e-9)))))
  (testing "1.5 :span 180 - ticks only in the half"
    (let [b (build {:data sales :mapping {:x :v :y :v} :coord {:kind :polar :span 180} :charts [(chart/scatter)]})
          fx (cljplot2.scale.position/forward-fn (get-in b [:finalized-scales :x]))]
      (is (every? #(<= -1e-9 (double (fx %)) (+ Math/PI 1e-9)) (get-in b [:finalized-scales :x :ticks])))))
  (testing "a continuous angle is neither padded nor niced by default (full circle)"
    (let [b (build {:data [{:v 3} {:v 97}] :mapping {:x :v} :coord polar :charts [(chart/bar {:position :stack})]})]
      (is (= [0.0 100.0] (mapv double (get-in b [:finalized-scales :x :domain])))))
    (let [b (build {:data [{:v 3} {:v 97}] :mapping {:x :v} :coord polar :scales {:x {:nice true}} :charts [(chart/bar {:position :stack})]})]
      (is (some? (get-in b [:finalized-scales :x :domain])) "the plot's :nice is kept"))))

(deftest geometry-and-paths
  (let [g {:kind :polar :cx 100.0 :cy 100.0 :radius 50.0 :inner-radius 0.0 :theta [0.0 (* 2 Math/PI)] :arcs false}
        r-of (fn [[x y]] (Math/hypot (- (double x) 100.0) (- (double y) 100.0)))]
    (testing "1.3 to-pixel: 0 at 12 o'clock, clockwise; cartesian identity"
      (is (= [100.0 90.0] (let [^Vec2 p (coord/to-pixel g 0.0 10.0)] [(Math/rint (.x p)) (Math/rint (.y p))])))
      (is (= [110.0 100.0] (let [^Vec2 p (coord/to-pixel g (/ Math/PI 2) 10.0)] [(Math/rint (.x p)) (Math/rint (.y p))])))
      (is (= [90.0 100.0] (let [^Vec2 p (coord/to-pixel g (- (/ Math/PI 2)) 10.0)] [(Math/rint (.x p)) (Math/rint (.y p))]))
          "counter-clockwise ranges run through negative angles")
      (is (= [3.0 4.0] (let [^Vec2 p (coord/to-pixel nil 3.0 4.0)] [(.x p) (.y p)]))))
    (testing "1.4.2 a sector's outline lies on its two radii"
      (let [poly (coord/rect-path g 0.0 10.0 (/ Math/PI 2) 50.0)]
        (is (every? (fn [pt] (let [r (r-of pt)] (or (< (Math/abs (- r 10.0)) 0.5) (< (Math/abs (- r 50.0)) 0.5)))) poly))
        (is (> (count poly) 10) "arcs densified")))
    (testing "1.4.2 a zero inner radius closes at the centre"
      (is (some #(< (r-of %) 1e-9) (coord/rect-path g 0.0 0.0 1.0 50.0))))
    (testing "1.4.1 arcs keep a constant radius, chords don't"
      (let [arc (coord/line-path g [[0.0 40.0] [Math/PI 40.0]] true)
            chord (coord/line-path g [[0.0 40.0] [Math/PI 40.0]] false)]
        (is (every? #(< (Math/abs (- (r-of %) 40.0)) 1e-6) arc))
        (is (= 2 (count chord)))
        (is (= [] (coord/line-path g [] true)))))
    (testing "circle-path spans the angular range"
      (is (every? #(< (Math/abs (- (r-of %) 25.0)) 1e-6) (coord/circle-path g 25.0))))))

(deftest chart-support
  (testing "2.1 charts without :polar in :coords are rejected"
    (is (= :charts (first (:in (error-data #(build {:data sales :mapping {:x :v} :coord polar :charts [(chart/histogram)]}))))))
    (is (some? (error #(build {:data sales :mapping {:x :v :y :c} :coord polar :charts [(chart/box)]}))))
    (is (some? (error #(build {:data sales :mapping {:x :c :y :v} :coord polar
                               :charts [(assoc (chart/scatter) :coords #{:cartesian})]}))) "a chart value's own :coords")
    (is (nil? (error #(build {:data sales :mapping {:x :v :y :c} :coord polar :charts [(chart/bar)]})))))
  (testing "2.4 polar lines: straight and unfilled"
    (are [opts] (= :coord (:key (error #(build {:data sales :mapping {:x :v :y :v} :coord polar :charts [(chart/line opts)]}))))
      {:interpolator :monotone} {:curve :bezier} {:position :stack} {:fill :color} {:fill-gradient :viridis})
    (is (nil? (error #(build {:data sales :mapping {:x :v :y :v} :coord polar :charts [(chart/line)]})))))
  (testing "2.6 ab lines are rejected in polar"
    (is (= :coord (:key (error #(build {:data sales :mapping {:x :v :y :v} :coord polar
                                        :charts [(chart/scatter) (chart/rule {:slope 1 :intercept 0})]})))))))

(deftest drawing
  (testing "2.2 a circular bar covers its band's angle at mid radius, not the opposite side"
    (let [p {:data [{:c "a" :v 10} {:c "b" :v 1}] :mapping {:x :v :y :c} :flip true :coord polar :theme plain
             :charts [(chart/bar {:color red :stroke-color :none :width 1.0})]}
          b (build p) img (plot/render-image p size)
          g (geometry b)
          fy (cljplot2.scale.position/forward-fn (get-in b [:finalized-scales :y]))
          fx (cljplot2.scale.position/forward-fn (get-in b [:finalized-scales :x]))
          theta-a (double (fy "a"))
          r-mid (* 0.5 (double (fx 10)))]
      (is (near? red (pixel-at img b (coord/to-pixel g theta-a r-mid))))
      (is (not (near? red (pixel-at img b (coord/to-pixel g (+ theta-a Math/PI) r-mid)))))))
  (testing "2.6 a rule on the radius is a circle"
    (let [p {:data sales :mapping {:x :c :y :v} :coord polar :theme plain
             :charts [(chart/scatter {:size 0.1}) (chart/rule {:y 25 :color red :width 2})]}
          b (build p) img (plot/render-image p size)
          g (geometry b)
          r (double ((cljplot2.scale.position/forward-fn (get-in b [:finalized-scales :y])) 25))]
      (is (every? #(red-around? img b (coord/to-pixel g % r)) [0.3 1.5 2.9 4.4]))))
  (testing "2.7 a rect band on the radius is a ring"
    (let [p {:data sales :mapping {:x :c :y :v} :coord polar :theme plain
             :charts [(chart/scatter {:size 0.1}) (chart/rect {:y [15 25] :fill red})]}
          b (build p) img (plot/render-image p size)
          g (geometry b)
          fy (cljplot2.scale.position/forward-fn (get-in b [:finalized-scales :y]))
          inside (* 0.5 (+ (double (fy 15)) (double (fy 25))))]
      (is (every? #(near? red (pixel-at img b (coord/to-pixel g % inside))) [0.5 2.0 3.5 5.5]))
      (is (not (near? red (pixel-at img b (coord/to-pixel g 1.0 (+ 3.0 (double (fy 25))))))))))
  (testing "2.3 / 2.4 polar segments and lines (chords and arcs) render"
    (doseq [arcs [false true]]
      (is (instance? BufferedImage (plot/render-image {:data sales :mapping {:x :c :y :v} :coord {:kind :polar :arcs arcs}
                                                       :charts [(chart/segment {:yend 0}) (chart/line) (chart/scatter)]} size)))))
  (testing "4.2 a pie closes the circle"
    (let [p {:data [{:p "a" :v 30} {:p "b" :v 70}] :mapping {:x :v :color :p} :coord polar :theme plain :legend false
             :charts [(chart/bar {:position :stack :width 1.0 :stroke-color :none})]}
          b (build p) img (plot/render-image p size)
          g (geometry b)]
      (is (every? #(not= [255 255 255] (pixel-at img b (coord/to-pixel g (m/radians %) (* 0.5 (double (:radius g)))))) [1 90 180 270 355])))))

(deftest text
  (testing "2.5.1 radial rotation: along the angle, upright"
    (let [rot #(double (#'cljplot2.chart.text/radial-rotation (m/radians %)))]
      (is (< (Math/abs (rot 90)) 1e-9) "3 o'clock - horizontal")
      (is (< (Math/abs (rot 270)) 1e-9) "9 o'clock - horizontal, upright")
      (is (< (Math/abs (- (rot 200) -70.0)) 1e-9) "left half turned upright")
      (is (every? #(let [r (rot %)] (or (<= -90.0 r 90.0) (> r 269.9))) (range 0 360 15)))))
  (testing "2.5 text in polar plots, :radial and panel positions"
    (is (instance? BufferedImage (plot/render-image {:data sales :mapping {:x :v :y :c} :flip true :coord polar
                                                     :charts [(chart/bar) (chart/text {:mapping {:x :v :y :c :label :c} :rotation :radial})
                                                              (chart/text {:x [:panel 0.5] :y [:panel 0.5] :text "centre"})]}
                                                    size)))
    (is (nil? (error #(build {:data sales :mapping {:x :v :y :c} :charts [(chart/bar) (chart/text {:mapping {:x :v :y :c :label :c} :rotation :radial})]})))
        ":radial is 0 in cartesian plots")))

(deftest guides
  (testing "3.1 no outer axes in a polar plot"
    (let [b (build {:data sales :mapping {:x :c :y :v} :coord polar :charts [(chart/scatter)]})]
      (is (not-any? #(#{:axis :axis-overhang} (get-in % [:content :kind])) (get-in b [:grid :items])))))
  (testing "3.2 circles at radial ticks"
    (let [p {:data sales :mapping {:x :c :y :v} :coord polar
             :theme {:plot {:background :white} :panel {:fill :white :grid {:x :none :y {:color red :width 2 :dash :none} :bands-fill :none}}}
             :charts [(chart/scatter {:size 0.1})]}
          b (build p) img (plot/render-image p size)
          g (geometry b)
          fy (cljplot2.scale.position/forward-fn (get-in b [:finalized-scales :y]))
          tick (second (get-in b [:finalized-scales :y :ticks]))]
      (is (every? #(red-around? img b (coord/to-pixel g % (double (fy tick)))) [0.7 2.2 3.9 5.1]))))
  (testing "3.3 the circle and its angle labels fit the panel"
    (let [b (build {:data sales :mapping {:x :c :y :v} :coord polar :charts [(chart/scatter)]})
          {:keys [radius cx cy]} (geometry b)]
      (is (<= (double radius) (min (double cx) (double cy))))))
  (testing "3.6 facets: a circle per panel"
    (is (instance? BufferedImage (plot/render-image {:data (for [f ["p" "q"] r sales] (assoc r :f f)) :mapping {:x :c :y :v :facet :f}
                                                     :coord polar :charts [(chart/bar {:mapping {:x :v :y :c}})]} size)))))
