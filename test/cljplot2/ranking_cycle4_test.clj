(ns cljplot2.ranking-cycle4-test
  "Ranking cycle 4: parallel coordinates and radar charts (`chart/parallel`) - variables, scaling, axes, groups,
  missing values, polar closing, fills, limits, the web."
  (:require [clojure.test :refer [deftest testing is are]]
            [cljplot2.test-util :refer [error]]
            [cljplot2.chart :as chart]
            [cljplot2.chart.parallel :as parallel]
            [cljplot2.core :as plot]
            [cljplot2.scale.coord :as coord]
            [cljplot2.scale.position :as scale])
  (:import [java.awt.image BufferedImage]
           [fastmath.vector Vec2]))

(def ^:private size {:width 400 :height 300})
(defn- build [p] (plot/build p size))
(defn- rgb [^BufferedImage img x y]
  (let [v (.getRGB img (int x) (int y))] [(bit-and (bit-shift-right v 16) 0xff) (bit-and (bit-shift-right v 8) 0xff) (bit-and v 0xff)]))
(defn- near? [[r g b] [r' g' b']] (every? #(<= (Math/abs (long %)) 40) [(- r r') (- g g') (- b b')]))
(def ^:private red [255 0 0])
(def ^:private polar {:kind :polar})

(defn- panel-origin [built]
  (let [box (cljplot2.layout.grid/find-content-box (:grid built) (:layout built) #(= :panel (:kind %)))]
    [(:x box) (:y box)]))

(defn- red-around?
  "Is a pixel of the 3 x 3 neighbourhood of panel point `[x y]` of `built` red?"
  [^BufferedImage img built [x y]]
  (let [[x0 y0] (panel-origin built)]
    (some #(near? red (rgb img (+ (double x0) (double x) (double (first %))) (+ (double y0) (double y) (double (second %)))))
          (for [dx [-1 0 1] dy [-1 0 1]] [dx dy]))))

(defn- red-in?
  "Is a pixel of the panel-local rectangle `[x0 x1] x [y0 y1]` of `built` red?"
  [^BufferedImage img built [x0 x1] [y0 y1]]
  (let [[ox oy] (panel-origin built)]
    (some #(near? red %) (for [x (range (long x0) (long x1)) y (range (long y0) (long y1))]
                           (rgb img (+ (double ox) (double x)) (+ (double oy) (double y)))))))

(def ^:private plain
  {:plot {:background :white} :panel {:fill :white :grid {:x :none :y :none :bands-fill :none}}})

(def ^:private rows3 [{:a 1 :b 10 :g "p"} {:a 2 :b 20 :g "q"} {:a 3 :b 40 :g "r"}])

(defn- stats [p] (first (:stats (build p))))
(defn- panel-data [p] (first (vals (:data (stats p)))))
(defn- scaled-rows
  "Scaled row values of every line of `p` (one chart), groups concatenated."
  [p]
  (vec (mapcat :rows (:lines (panel-data p)))))
(defn- column [rows i] (mapv #(nth % i) rows))

(defn- close? [xs ys] (and (= (count xs) (count ys)) (every? true? (map #(< (Math/abs (- (double %1) (double %2))) 1e-9) xs ys))))

(def ^:private iris-like
  (vec (for [[s base] [["setosa" 1.0] ["versicolor" 2.0] ["virginica" 3.0]] i (range 5)]
         {:species s :sl (+ 4.3 base (* 0.1 i)) :sw (- 4.0 (* 0.3 base) (* 0.05 i)) :pl (* base 1.9) :pw (* base 0.8)})))
(def ^:private iris-vars [:sl :sw :pl :pw])

(deftest variables
  (testing "1.1 iris builds; < 2 variables or none -> {:key :variables}"
    (is (nil? (error #(build {:data iris-like :mapping {:color :species} :charts [(chart/parallel {:variables iris-vars})]}))))
    (are [opts] (= :variables (:key (error #(build {:data rows3 :charts [(chart/parallel opts)]}))))
      {}
      {:variables []}
      {:variables [:a]}
      {:variables [:a "b"]}
      {:variables [["A" 1] :b]}))
  (testing "1.2 labels - names or pair labels, duplicates rejected, x extent ordered"
    (let [p {:data rows3 :charts [(chart/parallel {:variables [:b ["Alpha" :a]]})]}]
      (is (= {:kind :categorical :values ["b" "Alpha"] :ordered? true} (get-in (stats p) [:extents (first (keys (:data (stats p)))) :x])))
      (is (= ["b" "Alpha"] (get-in (build p) [:finalized-scales :x :domain]))))
    (is (= :variables (:key (error #(build {:data rows3 :charts [(chart/parallel {:variables [:a ["a" :b]]})]})))))
    (is (= :variables (:key (error #(build {:data rows3 :charts [(chart/parallel {:variables [:a :a]})]}))))))
  (testing "1.3 / 4.1 facets share the axes; order follows :variables"
    (let [b (build {:data rows3 :mapping {:facet :g} :charts [(chart/parallel {:variables [:a :b]})]})
          axes (map :axes (vals (:data (first (:stats b)))))]
      (is (= 3 (count axes)))
      (is (apply = axes))
      (is (= [[1.0 3.0] [10.0 40.0]] (mapv :domain (first axes)))))
    (is (= ["b" "a"] (get-in (build {:data rows3 :charts [(chart/parallel {:variables [:b :a]})]}) [:finalized-scales :x :domain]))))
  (testing "a variable without numeric values"
    (is (= {:key :variables :value :c} (select-keys (error #(build {:data (map (fn [r] (assoc r :c "x")) rows3) :charts [(chart/parallel {:variables [:a :c]})]}))
                                                    [:key :value])))
    (is (nil? (error #(build {:data (map (fn [r] (assoc r :c "x")) rows3) :charts [(chart/parallel {:variables [:a :c] :limits {:c [0 1]}})]})))
        "limits give it a domain")))

(deftest scaling
  (let [rows (fn [scaling vs] (scaled-rows {:data (map (fn [v] {:a v :b 1}) vs) :charts [(chart/parallel {:variables [:a :b] :scaling scaling})]}))]
    (testing "1.4 every method"
      (are [scaling vs expected] (= expected (column (rows scaling vs) 0))
        :uniminmax [1 2 3] [0.0 0.5 1.0]
        :std [1 2 3] [-1.0 0.0 1.0]
        :center [1 2 3] [-1.0 0.0 1.0]
        :globalminmax [1 2 3] [1.0 2.0 3.0]))
    (testing "R4 :robust - :robust-center and a robust-stddev :robust-spread"
      (let [mad (/ 1.0 0.6744897501960816)
            robust (fn [opts vs] (column (scaled-rows {:data (map (fn [v] {:a v :b 1}) vs)
                                                       :charts [(chart/parallel (merge {:variables [:a :b] :scaling :robust} opts))]}) 0))]
        (is (close? [(/ -1.0 mad) 0.0 (/ 2.0 mad)] (robust {} [1 2 4])) "default: median, :mad (R mad = MAD / 0.6745)")
        (is (close? (mapv #(/ (- (double %) (/ 7.0 3.0)) mad) [1 2 4]) (robust {:robust-center :mean} [1 2 4])))
        (is (close? [-1.0 0.0 1.0] (robust {:robust-spread :stddev} [1 2 3])))
        (is (= (robust {:robust-spread :iqr} [1 2 4 8 20]) (robust {:robust-spread 0.25} [1 2 4 8 20])) "a quantile p")
        (is (= [0.0 0.0 0.0] (robust {:robust-spread :qn} [5 5 5])) "zero spread")))
    (testing "R4 schema: estimators, and only with :scaling :robust"
      (are [opts k] (= k (:key (error #(build {:data rows3 :charts [(chart/parallel (merge {:variables [:a :b]} opts))]}))))
        {:scaling :robust :robust-spread :sd} :robust-spread
        {:scaling :robust :robust-spread 0.5} :robust-spread
        {:scaling :robust :robust-spread 0} :robust-spread
        {:scaling :robust :robust-center :mode} :robust-center
        {:robust-spread :mad} :robust-spread
        {:scaling :std :robust-center :mean} :robust-center
        {:scaling :robust :robust-spread 0.1 :robust-center :mean} nil))
    (testing "1.4 zero spread - 0.5 for :uniminmax, 0 otherwise"
      (is (= [0.5 0.5 0.5] (column (rows :uniminmax [1 2 3]) 1)))
      (is (every? #(= [0.0 0.0 0.0] (column (rows % [1 2 3]) 1)) [:std :robust :center]))
      (is (= [1.0 1.0 1.0] (column (rows :globalminmax [1 2 3]) 1))))
    (testing "one row"
      (is (= [[0.5 0.5]] (rows :uniminmax [7])))))
  (testing "1.5 the y extent covers values and axis ends"
    (let [y (fn [opts] (-> (stats {:data rows3 :charts [(chart/parallel (merge {:variables [:a :b]} opts))]}) :extents vals first :y (select-keys [:min :max])))]
      (is (= {:min 0.0 :max 1.0} (y {})))
      (is (= {:min 0.0 :max 1.0} (y {:limits {:a [0 4]}})) ":uniminmax - limits are the axis ends 0 and 1")
      (is (= [0.25 0.5 0.75] (column (scaled-rows {:data rows3 :charts [(chart/parallel {:variables [:a :b] :limits {:a [0 4]}})]}) 0)))
      (is (= {:min -2.0 :max 2.0} (y {:scaling :std :limits {:a [0 4]}})) ":std - limits [0 4] around mean 2, sd 1: ends -2 and 2"))))

(deftest groups-and-missing
  (testing "1.6 groups by species - three lines groups, three legend levels"
    (let [b (build {:data iris-like :mapping {:color :species} :charts [(chart/parallel {:variables iris-vars})]})]
      (is (= 3 (count (:lines (first (vals (:data (first (:stats b)))))))))
      (is (= ["setosa" "versicolor" "virginica"] (get-in b [:finalized-scales :color :domain])))))
  (testing "1.7 a missing or non-numeric value breaks the row, counted"
    (let [data [{:a 1 :b 1 :c 1 :d 1} {:a 2 :b nil :c 2 :d 2} {:a 3 :b 3 :c "x" :d ##NaN}]
          p {:data data :charts [(chart/parallel {:variables [:a :b :c :d]})]}
          s (stats p)]
      (is (= 2 (get-in s [:dropped :variables])))
      (is (= [[0.0 0.0 0.0 0.0] [0.5 nil 1.0 1.0] [1.0 1.0 nil nil]] (scaled-rows p)))
      (let [row (second (scaled-rows p))
            pts (mapv (fn [i v] (if v [(* 10.0 i) (* 10.0 (double v))] [##NaN ##NaN])) (range) row)]
        (is (= 2 (count (#'parallel/point-runs nil pts nil))) "two runs")
        (is (= [[[0.0 5.0]] [[20.0 10.0] [30.0 10.0]]] (#'parallel/point-runs nil pts nil))))
      (testing "1.7.1 :missing :drop"
        (let [p' {:data data :charts [(chart/parallel {:variables [:a :b :c :d] :missing :drop})]}]
          (is (= 1 (count (scaled-rows p'))))
          (is (= 2 (get-in (stats p') [:dropped :variables]))))))))

(deftest style-and-legend
  (testing "1.8 theme validation"
    (is (= [:theme :chart :parallel :width] (:in (error #(build {:data rows3 :theme {:chart {:parallel {:width -1}}}
                                                                    :charts [(chart/parallel {:variables [:a :b]})]})))))
    (is (some? (error #(build {:data rows3 :charts [(chart/parallel {:variables [:a :b] :size 3})]}))) "closed options"))
  (testing "1.8 legend - a line, an area with a radar fill"
    (let [legend-entries (fn [p] (some #(when (= :legends (:id %)) (get-in % [:content :legends])) (get-in (build p) [:grid :items])))
          p {:data rows3 :mapping {:color :g} :charts [(chart/parallel {:variables [:a :b]})]}]
      (is (= 3 (count (:entries (first (legend-entries p))))))
      (is (instance? BufferedImage (plot/render-image (assoc p :coord polar :charts [(chart/parallel {:variables [:a :b] :fill :color :pattern :hatch})]) size))))))

(defn- axis-pixels
  "Panel pixels `[x y]` of variable `label`'s scaled value `v` in built `b` (cartesian)."
  [b label v]
  (let [^Vec2 p ((scale/projection (:finalized-scales b)) label v)] [(.x p) (.y p)]))

(deftest axes
  (testing "2.1 the plot y axis hidden; the plot can restore it"
    (let [y-axis? (fn [b] (some #(and (= :axis (get-in % [:content :kind])) (= :y (get-in % [:content :scale]))) (get-in b [:grid :items])))]
      (is (not (y-axis? (build {:data rows3 :charts [(chart/parallel {:variables [:a :b]})]}))))
      (is (y-axis? (build {:data rows3 :scales {:y {:axis true}} :charts [(chart/parallel {:variables [:a :b]})]})))))
  (testing "2.2.1 ticks in original units, count from the axis length"
    (is (every? #(<= 4.3 (double %) 7.9) (map first (parallel/axis-ticks [4.3 7.9] 200.0 50.0))))
    (is (< (count (parallel/axis-ticks [0.0 100.0] 60.0 50.0)) (count (parallel/axis-ticks [0.0 100.0] 600.0 50.0))))
    (is (= [[5.0 "5"]] (parallel/axis-ticks [5.0 5.0] 0.0 50.0)) "a zero-length domain: one tick"))
  (let [theme (assoc plain :text {:tick-label {:color red}})
        p (fn [opts] {:data rows3 :theme theme :scales {:x {:axis false}} :legend false
                      :charts [(chart/parallel (merge {:variables [:a :b] :color [0 0 255]} opts))]})]
    (testing "2.2 tick labels right of the axis"
      (let [b (build (p {})) img (plot/render-image (p {}) size)
            [x y] (axis-pixels b "a" 0.5)]
        (is (red-in? img b [(+ (double x) 8) (+ (double x) 30)] [(- (double y) 7) (+ (double y) 7)]))
        (is (not (red-in? img b [(- (double x) 30) (- (double x) 2)] [(- (double y) 7) (+ (double y) 7)])))))
    (testing "2.2 flipped: below the axis"
      (let [b (build (assoc (p {}) :flip true)) img (plot/render-image (assoc (p {}) :flip true) size)
            [x y] (axis-pixels b "a" 0.5)]
        (is (red-in? img b [(- (double x) 12) (+ (double x) 12)] [(+ (double y) 8) (+ (double y) 22)]))))
    (testing "2.3 :axes false"
      (let [b (build (p {:axes false})) img (plot/render-image (p {:axes false}) size)
            [x y] (axis-pixels b "a" 0.5)]
        (is (not (red-in? img b [(+ (double x) 8) (+ (double x) 30)] [(- (double y) 7) (+ (double y) 7)]))))))
  (testing "2.2 the axis line in theme [:axis :y]"
    (let [p {:data rows3 :theme (assoc plain :axis {:y {:line-color red :line-width 2}}) :legend false
             :charts [(chart/parallel {:variables [:a :b] :color [0 0 255] :width 0})]}
          b (build p) img (plot/render-image p size)]
      (is (red-around? img b (axis-pixels b "b" 0.3)))))
  (testing "2.4 variable labels on the x axis"
    (is (= ["a" "Bee"] (get-in (build {:data rows3 :charts [(chart/parallel {:variables [:a ["Bee" :b]]})]}) [:finalized-scales :x :tick-labels])))))

(defn- radar-pixel
  "Panel pixel of variable `label` at scaled value `v` in the polar plot built `b`."
  [b label v]
  (let [[h r] ((scale/range-projection (:finalized-scales b)) label v)
        ^Vec2 p (coord/to-pixel (scale/coord-geometry (:finalized-scales b)) h r)]
    [(.x p) (.y p)]))

(deftest radar
  (let [square [{:a 0 :b 0 :c 0 :d 0} {:a 1 :b 1 :c 1 :d 1}]]
    (testing "3.1 a closed polygon of n vertices"
      (let [b (build {:data square :coord polar :charts [(chart/parallel {:variables [:a :b :c :d]})]})
            g (scale/coord-geometry (:finalized-scales b))
            rp (scale/range-projection (:finalized-scales b))
            pts (mapv #(rp % 1.0) ["a" "b" "c" "d"])
            [run :as runs] (#'parallel/point-runs g (#'parallel/closed g true pts) nil)]
        (is (= 1 (count runs)))
        (is (= 5 (count run)))
        (is (every? true? (map #(< (Math/abs (- (double %1) (double %2))) 1e-6) (first run) (peek run))) "closes on the first vertex")
        (is (= 4 (count (#'parallel/closed nil false pts))) "cartesian: open")))
    (testing "3.1 a row with a gap: the closing segment joins the last run to the first point"
      (let [b (build {:data square :coord polar :charts [(chart/parallel {:variables [:a :b :c :d]})]})
            g (scale/coord-geometry (:finalized-scales b))
            rp (scale/range-projection (:finalized-scales b))
            pts [(rp "a" 1.0) [##NaN ##NaN] (rp "c" 1.0) (rp "d" 1.0)]]
        (is (= [1 3] (mapv count (#'parallel/point-runs g (#'parallel/closed g true pts) nil))))))
    (testing "3.2 spokes in the axis style"
      (let [p {:data square :coord polar :theme (assoc plain :axis {:y {:line-color red :line-width 2}}) :legend false
               :charts [(chart/parallel {:variables [:a :b :c :d] :color [0 0 255] :width 0})]}
            b (build p) img (plot/render-image p size)]
        (is (every? #(red-around? img b (radar-pixel b % 0.6)) ["a" "b" "c" "d"]))))
    (testing "3.3 :fill fills complete rows, polar only"
      (let [p (fn [data] {:data data :coord polar :theme plain :legend false
                          :charts [(chart/parallel {:variables [:a :b :c :d] :color [0 0 255] :fill [255 0 0] :axes false})]})
            ;; between the spokes of a and b, half way out: inside the square of the outer row
            probe (fn [b] (let [[ha r] ((scale/range-projection (:finalized-scales b)) "a" 0.5)
                                [hb _] ((scale/range-projection (:finalized-scales b)) "b" 0.5)
                                ^Vec2 q (coord/to-pixel (scale/coord-geometry (:finalized-scales b)) (* 0.5 (+ (double ha) (double hb))) r)]
                            [(.x q) (.y q)]))
            full (p square)
            gap (p [{:a 0 :b 0 :c 0 :d 0} {:a 1 :b 1 :c nil :d 1}])]
        (is (red-around? (plot/render-image full size) (build full) (probe (build full))))
        (is (not (red-around? (plot/render-image gap size) (build gap) (probe (build gap)))) "a row with a gap isn't filled"))
      (is (= :fill (:key (error #(build {:data rows3 :charts [(chart/parallel {:variables [:a :b] :fill :color})]})))))
      (is (nil? (error #(build {:data rows3 :charts [(chart/parallel {:variables [:a :b] :fill :none})]}))) ":none is fine"))
    (testing "3.4 :limits fix the domain: ticks and scaled positions"
      (let [p {:data [{:a 2 :b 1} {:a 4 :b 2}] :charts [(chart/parallel {:variables [:a :b] :limits {:a [0 10]}})]}]
        (is (= [[0.2 0.0] [0.4 1.0]] (scaled-rows p)))
        (is (= [0.0 10.0] (:domain (first (:axes (panel-data p))))))
        (is (= ["0" "5" "10"] (mapv second (parallel/axis-ticks [0.0 10.0] 100.0 50.0)))))
      (is (= :limits (:key (error #(build {:data rows3 :charts [(chart/parallel {:variables [:a :b] :limits {:z [0 1]}})]})))))
      (is (= [:charts 0 :options :limits :a] (:in (error #(build {:data rows3 :charts [(chart/parallel {:variables [:a :b] :limits {:a [1 0]}})]})))))
      (is (nil? (error #(build {:data rows3 :charts [(chart/parallel {:variables [["A" :a] :b] :limits {"A" [0 5]}})]})))
          "pairs keyed by their label"))
    (testing "3.5 the web joins the axes"
      (let [p {:data square :coord polar :legend false
               :theme (assoc-in plain [:panel :grid :y] {:color red :width 2 :dash :none})
               :charts [(chart/parallel {:variables [:a :b :c :d] :color [0 0 255] :width 0 :axes false})]}
            b (build p) img (plot/render-image p size)
            [ax ay] (radar-pixel b "a" 1.0) [bx by] (radar-pixel b "b" 1.0)]
        (is (red-around? img b [(* 0.5 (+ (double ax) (double bx))) (* 0.5 (+ (double ay) (double by)))]))))))

(deftest flipped-radar
  (let [p (fn [theme opts] {:data rows3 :coord polar :flip true :legend false :theme theme :scales {:x {:axis false}}
                            :charts [(chart/parallel (merge {:variables [:a :b] :color [0 0 255] :width 0} opts))]})]
    (testing "R1 variables on rings: each axis an arc"
      (let [pl (p (assoc plain :axis {:y {:line-color red :line-width 2}}) {})
            b (build pl) img (plot/render-image pl size)]
        (is (every? #(red-around? img b (radar-pixel b "b" %)) [0.25 0.5 0.75]) "on the ring, not a chord")))
    (testing "R1 tick labels outside the ring"
      (let [pl (p (assoc plain :text {:tick-label {:color red}}) {})
            b (build pl) img (plot/render-image pl size)
            g (scale/coord-geometry (:finalized-scales b))
            [h r] ((scale/range-projection (:finalized-scales b)) "b" (/ 1.0 3.0))
            ^Vec2 out (coord/to-pixel g h (+ (double r) 14.0))
            ^Vec2 in (coord/to-pixel g h (- (double r) 14.0))]
        (is (red-in? img b [(- (.x out) 9) (+ (.x out) 9)] [(- (.y out) 9) (+ (.y out) 9)]) "tick 20 of b (domain 10..40)")
        (is (not (red-in? img b [(- (.x in) 4) (+ (.x in) 4)] [(- (.y in) 4) (+ (.y in) 4)])))))
    (testing "R1 the web: lines across the rings"
      (let [pl (p (assoc-in plain [:panel :grid :y] {:color red :width 2 :dash :none}) {:axes false})
            b (build pl) img (plot/render-image pl size)
            [ax ay] (radar-pixel b "a" 1.0) [bx by] (radar-pixel b "b" 1.0)]
        (is (red-around? img b [(* 0.5 (+ (double ax) (double bx))) (* 0.5 (+ (double ay) (double by)))]))))
    (testing "R1 rows don't close; fills render"
      (let [b (build (p plain {}))
            g (scale/coord-geometry (:finalized-scales b))
            rp (scale/range-projection (:finalized-scales b))]
        (is (= [2] (mapv count (#'parallel/point-runs g (#'parallel/closed g false [(rp "a" 0.2) (rp "b" 0.8)]) nil)))))
      (is (instance? BufferedImage (plot/render-image (p plain {:fill :color :width 1}) size))))))

(deftest review-fixes
  (testing "R5 a theme fill is ignored in cartesian plots - drawing and legend share the computed style"
    (let [theme {:chart {:parallel {:fill :color}}}
          s (fn [extra] (:style (stats (merge {:data rows3 :theme theme :charts [(chart/parallel {:variables [:a :b]})]} extra))))]
      (is (= :none (:fill (s {}))))
      (is (= :color (:fill (s {:coord polar}))))))
  (testing "R7 no web while the plot shows its own y axis and grid"
    (let [pl (fn [scales] {:data [{:a 0 :b 0 :c 0 :d 0} {:a 1 :b 1 :c 1 :d 1}] :coord polar :legend false :scales scales
                           :theme (assoc-in plain [:panel :grid :y] {:color red :width 2 :dash :none})
                           :charts [(chart/parallel {:variables [:a :b :c :d] :color [0 0 255] :width 0 :axes false})]})
          probe (fn [b] (let [[ax ay] (radar-pixel b "a" 1.0) [bx by] (radar-pixel b "b" 1.0)]
                          [(* 0.5 (+ (double ax) (double bx))) (* 0.5 (+ (double ay) (double by)))]))
          web? (fn [scales] (let [b (build (pl scales))] (red-around? (plot/render-image (pl scales) size) b (probe b))))]
      (is (web? {}))
      (is (not (web? {:y {:axis true :ticks [0.0]}})) "the plot's circles instead (one, at 0)")))
  (testing "R9 a full circle closes a turn further on"
    (let [g {:kind :polar :theta [0.0 (* 2.0 Math/PI)] :cx 0.0 :cy 0.0}
          [h v] (peek (#'parallel/closed g true [[0.5 5.0] [2.0 6.0]]))]
      (is (close? [(+ 0.5 (* 2.0 Math/PI)) 5.0] [h v])))))

(deftest review2-fixes
  (let [angles (fn [extra opts]
                 (let [b (build (merge {:data rows3 :coord polar :flip true :charts [(chart/parallel (merge {:variables [:a :b]} opts))]} extra))
                       rp (scale/range-projection (:finalized-scales b))
                       [t0 t1] (:theta (scale/coord-geometry (:finalized-scales b)))]
                   {:lo-gap (- (double (first (rp "a" 0.0))) (double t0)) :hi-gap (- (double t1) (double (first (rp "a" 1.0))))}))]
    (testing "S1 a flipped full-circle radar pads the value angle: an axis's ends don't meet"
      (let [{:keys [lo-gap hi-gap]} (angles {} {})]
        (is (< 0.25 (double lo-gap) 0.35) "5% of 1.1 x 2 pi")
        (is (< 0.25 (double hi-gap) 0.35))))
    (testing "S1 :padding :y sets the angle padding; the plot's :scales :y :expand wins"
      (is (> (double (:lo-gap (angles {} {:padding {:y [0.2 0.2]}}))) (double (:lo-gap (angles {} {})))))
      (is (< (Math/abs (double (:lo-gap (angles {:scales {:y {:expand [0 0]}}} {})))) 1e-9))
      (is (< (Math/abs (double (:lo-gap (angles {:scales {:y {:axis true}}} {})))) 1e-9) "plot spec sets :axis - suggestions dropped as a whole"))
    (testing "S1 the suggestion only under flip + polar"
      (is (= {:y {:axis false}} (:scale-defaults (stats {:data rows3 :charts [(chart/parallel {:variables [:a :b]})]}))))
      (is (= {:y {:axis false}} (:scale-defaults (stats {:data rows3 :coord polar :charts [(chart/parallel {:variables [:a :b]})]}))))
      (is (= {:y {:axis false}} (:scale-defaults (stats {:data rows3 :flip true :charts [(chart/parallel {:variables [:a :b]})]}))))
      (is (= {:y {:axis false :expand [0.1 0]}}
             (:scale-defaults (stats {:data rows3 :coord polar :flip true :charts [(chart/parallel {:variables [:a :b] :padding {:y [0.1 0]}})]}))))))
  (testing "S1 other polar charts keep unpadded angles (pies)"
    (let [b (build {:data [{:p "a" :n 1} {:p "b" :n 3}] :mapping {:x :n :color :p} :coord polar :scales {:x {:axis false}}
                    :charts [(chart/bar {:position :stack :width 1.0})]})]
      (is (= [0.0 4.0] (mapv double (get-in b [:finalized-scales :x :domain]))))))
  (testing "S2 only a full circle closes rows"
    (is (coord/full-circle? {:theta [0.0 (* 2.0 Math/PI)]}))
    (is (coord/full-circle? {:theta [1.0 (- 1.0 (* 2.0 Math/PI))]}) "counter-clockwise")
    (is (not (coord/full-circle? {:theta [0.0 Math/PI]})))
    (let [p {:data [{:a 0 :b 0 :c 0 :d 0} {:a 1 :b 1 :c 1 :d 1}] :coord {:kind :polar :span 180 :start -90} :legend false
             :theme plain :charts [(chart/parallel {:variables [:a :b :c :d] :color red :width 2 :axes false :curve :smooth})]}
          b (build p) img (plot/render-image p size)
          [ax ay] (radar-pixel b "a" 1.0) [dx dy] (radar-pixel b "d" 1.0)]
      (is (red-around? img b (radar-pixel b "b" 1.0)) "the row is drawn")
      (is (not (red-around? img b [(* 0.5 (+ (double ax) (double dx))) (* 0.5 (+ (double ay) (double dy)))])) "no closing d -> a")))
  (testing "S4 :padding takes :y only"
    (is (= [:charts 0 :options :padding :x] (:in (error #(build {:data rows3 :charts [(chart/parallel {:variables [:a :b] :padding {:x [0 0]}})]})))))))

(deftest curves-and-edges
  (testing "R2 smooth-segment: ends exact, perpendicular to the axes, between the values"
    (let [seg (parallel/smooth-segment [0.0 10.0] [100.0 50.0] 1 1.0 100)
          vs (mapv second seg) hs (mapv first seg)]
      (is (= 100 (count seg)))
      (is (close? [100.0 50.0] (peek seg)))
      (is (close? [50.0 30.0] (nth seg 49)) "smoothness 1: the middle at the middle")
      (is (every? #(<= 10.0 (double %) 50.0) vs))
      (is (apply <= vs) "monotone values")
      (is (apply <= hs) "monotone along the axes")
      (is (< (/ (- (double (first vs)) 10.0) (double (first hs))) 0.05) "flat at the start axis")
      (is (< (/ (- 50.0 (double (nth vs 98))) (- 100.0 (double (nth hs 98)))) 0.05) "flat at the end axis"))
    (is (every? #(= 10.0 (second %)) (parallel/smooth-segment [0.0 10.0] [100.0 10.0] 1 1.0 10)) "equal values: flat")
    (is (close? [30.0 50.0] (nth (parallel/smooth-segment [10.0 0.0] [50.0 100.0] 0 1.0 2) 0)) "value-index 0 (flipped): value first")
    (is (close? [0.0 16.25] (first (parallel/smooth-segment [0.0 10.0] [0.0 50.0] 1 0.5 4))) "a zero gap between axes")
    (let [deviation (fn [k] (reduce max (map (fn [[a b]] (Math/abs (- (double a) (double b))))
                                             (parallel/smooth-segment [0.0 0.0] [100.0 100.0] 1 k 50))))]
      (is (< (double (deviation 0.1)) (double (deviation 1.0))) "smaller smoothness: straighter between the axes, bent near them")))
  (testing "R2 :curve :smooth in a row: never beyond the values"
    (let [pts [[0.0 10.0] [100.0 90.0] [200.0 20.0] [##NaN ##NaN] [300.0 40.0] [400.0 40.0]]
          runs (#'parallel/point-runs nil pts {:value-index 1 :smoothness 1.0})]
      (is (= 2 (count runs)))
      (is (every? #(<= 10.0 (double (second %)) 90.0) (first runs)))
      (is (= [[0.0 10.0] [200.0 20.0]] [(first (first runs)) (peek (first runs))]))
      (is (every? #(= 40.0 (second %)) (second runs)))))
  (testing "R2 :curve :smooth renders - cartesian, flipped, polar, flipped polar, arcs"
    (doseq [extra [{} {:flip true} {:coord polar} {:coord polar :flip true} {:coord {:kind :polar :arcs true}}]]
      (is (instance? BufferedImage (plot/render-image (merge {:data rows3 :charts [(chart/parallel {:variables [:a :b ["a2" :a]] :curve :smooth :smoothness 0.6})]} extra) size)))))
  (testing "R2 schema"
    (are [opts k] (= k (:key (error #(build {:data rows3 :charts [(chart/parallel (merge {:variables [:a :b]} opts))]}))))
      {:curve :bezier} :curve
      {:smoothness 0.5} :smoothness
      {:curve :smooth :smoothness 0} :smoothness
      {:curve :smooth :smoothness 1.5} :smoothness
      {:curve :smooth :smoothness 1} nil))
  (testing "arcs and a half circle render"
    (is (instance? BufferedImage (plot/render-image {:data rows3 :coord {:kind :polar :arcs true :span 180}
                                                     :charts [(chart/parallel {:variables [:a :b ["a2" :a]] :fill :color})]} size))))
  (testing "R8 :padding"
    (is (= [0.0 1.0] (get-in (build {:data rows3 :charts [(chart/parallel {:variables [:a :b] :padding {:y [0 0]}})]}) [:finalized-scales :y :domain]))))
  (testing "no rows; one row (zero spreads) in polar"
    (is (instance? BufferedImage (plot/render-image {:data [] :charts [(chart/parallel {:variables [:a :b]})]} size)))
    (is (instance? BufferedImage (plot/render-image {:data [{:a 1 :b 2 :c 3}] :coord polar
                                                     :charts [(chart/parallel {:variables [:a :b :c] :fill :color})]} size))))
  (testing "rows of NA groups and facets render"
    (is (instance? BufferedImage (plot/render-image {:data (conj rows3 {:a 2 :b 30 :g nil}) :mapping {:color :g :facet :g}
                                                     :charts [(chart/parallel {:variables [:a :b]})]} size)))))
