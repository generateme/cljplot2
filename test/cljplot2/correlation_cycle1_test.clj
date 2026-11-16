(ns cljplot2.correlation-cycle1-test
  "Correlation cycle 1: grid cells - the normalized grid, cell drawing, tile, bin2d, legend titles."
  (:require [clojure.test :refer [deftest testing is are]]
            [fastmath.core :as m]
            [cljplot2.test-util :refer [error]]
            [cljplot2.chart :as chart]
            [cljplot2.chart.tile :as tile]
            [cljplot2.content.legend :as legend]
            [cljplot2.core :as plot]
            [cljplot2.data.cells :as cells]
            [cljplot2.layout.grid :as grid]
            [cljplot2.layout.plot-grid :as plot-grid]
            [cljplot2.scale.position :as scale])
  (:import [java.awt.image BufferedImage]))

(defn- rgb [^BufferedImage img x y]
  (let [v (.getRGB img (int x) (int y))]
    [(bit-and (bit-shift-right v 16) 0xff) (bit-and (bit-shift-right v 8) 0xff) (bit-and v 0xff)]))

(def ^:private plain {:plot {:background :white} :panel {:fill :white}
                      :palette {:color [[255 0 0] [0 0 255] [0 160 0]] :na [128 128 128]}})

(defn- render [p size]
  (let [p (update p :theme #(merge plain %))
        built (plot/build p size)]
    [built (plot/render-image p size) (grid/item-box (:layout built) :panel)]))

(defn- stats [p] (first (:stats (plot/build p {:width 400 :height 400}))))

(defn- inside?
  "Is point `[px py]` strictly inside the convex polygon `corners` (either winding), `eps` away from its edges?"
  [corners [px py] eps]
  (let [px (double px) py (double py)
        crosses (map (fn [[ax ay] [bx by]]
                       (let [ax (double ax) ay (double ay) bx (double bx) by (double by)
                             len (m/hypot (- bx ax) (- by ay))]
                         (/ (- (* (- bx ax) (- py ay)) (* (- by ay) (- px ax))) len)))
                     corners (concat (rest corners) [(first corners)]))]
    (or (every? #(> (double %) (double eps)) crosses) (every? #(< (double %) (- (double eps))) crosses))))

(defn- near-edge?
  [corners p eps]
  (and (not (inside? corners p eps)) (inside? corners p (- (double eps)))))

;; 1.1 stats env

(def ^:private seen-env (atom nil))

(defn- probe-chart []
  {:kind :probe :options {} :legend nil
   :compute (fn [_ _ env] (reset! seen-env env) {:data {}})
   :draw (fn [& _])})

(deftest stats-env-test
  (testing "1.1 :aspect and a lazy panel-size estimate reach compute"
    (plot/build {:data [{:x 1 :y 1}] :mapping {:x :x :y :y} :charts [(chart/scatter) (probe-chart)]} {:width 640 :height 480})
    (is (nil? (:aspect @seen-env)))
    (is (delay? (:panel-size @seen-env)))
    (is (not (realized? (:panel-size @seen-env))) "not measured unless a chart needs it")
    (let [[w h] @(:panel-size @seen-env)]
      (is (< 560 w 640) "axis room taken, no legend")
      (is (< 400 h 480)))
    (plot/build {:data (for [f ["a" "b" "c"]] {:x 1 :y 1 :f f}) :mapping {:x :x :y :y :facet :f} :aspect :iso
                 :charts [(chart/scatter) (probe-chart)]} {:width 600 :height 400})
    (is (= :iso (:aspect @seen-env)))
    (is (< 150.0 (first @(:panel-size @seen-env)) 200.0) "3 wrapped panels in one row")))

(deftest panel-layout-test
  (testing "panel layout of the split facets, without formatting strips"
    (is (= {:columns 1 :rows 1} (plot-grid/panel-layout {:dims []} {})))
    (is (= {:columns 3 :rows 1} (plot-grid/panel-layout {:dims [:facet] :levels {:facet ["a" "b" "c"]}} {})))
    (is (= {:columns 2 :rows 2} (plot-grid/panel-layout {:dims [:facet] :levels {:facet ["a" "b" "c"]}} {:columns 2})))
    (is (= {:columns 3 :rows 2} (plot-grid/panel-layout {:dims [:facet-col :facet-row] :levels {:facet-col [1 2 3] :facet-row [1 2]}} {})))
    (is (= {:columns 1 :rows 2} (plot-grid/panel-layout {:dims [:facet-row] :levels {:facet-row [1 2]}} {})))))

;; 1.2 shape factor

(deftest shape-factor-test
  (testing "1.2 :shape > :iso > estimated panel shape > 1"
    (is (== 2.5 (cells/shape-factor {:shape 2.5} 10 1 {:aspect :iso :size [600 400]})))
    (is (== 0.1 (cells/shape-factor {} 10 1 {:aspect :iso :size [600 400]})))
    (is (m/delta-eq (/ 2.0 3.0) (cells/shape-factor {} 10 1 {:panel-size (delay [600 400])})))
    (is (== 1.0 (cells/shape-factor {} 10 1 {})))))

;; 1.3 / 1.4 binning

(def ^:private cloud
  (let [rng (java.util.Random. 5)]
    (vec (repeatedly 500 (fn [] [(* 10.0 (.nextDouble rng)) (* 4.0 (.nextDouble rng))])))))

(deftest bin-points-test
  (doseq [cell [:square :pointy-hex :flat-hex :triangle :rhombus :shifted-square]]
    (testing (str "1.3 each point inside its cell, counts sum to n: " cell)
      (let [{:keys [panels]} (cells/bin-points {:p cloud} {:cell cell :bins 7} {:size [600 600]})
            cs (:p panels)]
        (is (= 500 (reduce + (map :count cs))))
        (is (every? (fn [pt] (some #(inside? (:corners %) pt -1e-9) cs)) cloud)))))
  (testing "1.3 panels share one grid: a cell present in both has the same corners"
    (let [{:keys [panels]} (cells/bin-points {:a [[0 0] [1 1] [5 2]] :b [[0.01 0.01] [9 3]]} {:cell :pointy-hex :bins 5} {:size [600 600]})
          corners-a (set (map :corners (:a panels)))]
      (is (some corners-a (map :corners (:b panels))))))
  (testing "1.4.1 :bins n - n cells across x (square)"
    (let [cs (:p (:panels (cells/bin-points {:p cloud} {:cell :square :bins 10} {:size [600 600]})))
          xs (sort (distinct (map #(m/approx (double (ffirst (:corners %))) 6) cs)))]
      (is (<= 9 (count xs) 11))))
  (testing "1.4.2 :binwidth in x data units wins over :bins"
    (let [cs (:p (:panels (cells/bin-points {:p cloud} {:cell :square :bins 3 :binwidth 1.0} {:size [600 600]})))
          [[x0] [x1]] (:corners (first cs))]
      (is (m/delta-eq 1.0 (m/abs (- (double x1) (double x0))) 1e-9))))
  (testing "1.4.1 every estimation method bins; the default is :scott-correlated"
    (doseq [method cells/size-methods
            cell [:square :pointy-hex]]
      (is (seq (:p (:panels (cells/bin-points {:p cloud} {:cell cell :bins method} {:size [600 600]})))) [method cell]))
    (is (= :scott-correlated cells/default-size-method))
    (let [area (fn [opts] (:area (cells/bin-points {:p cloud} (merge {:cell :square} opts) {:size [600 600]})))]
      (is (== (area {:bins :scott-correlated}) (area {})) "no :bins - the default method")
      (is (not= (area {:bins :scott}) (area {:bins :scott-correlated})) "methods differ")))
  (testing "1.4.1 estimation on degenerate data: one point, constant x, two panels share the estimate"
    (is (= 1 (count (:p (:panels (cells/bin-points {:p [[1.0 2.0]]} {:cell :square} {:size [600 600]}))))))
    (is (seq (:p (:panels (cells/bin-points {:p [[1.0 2.0] [1.0 3.0] [1.0 5.0]]} {:cell :pointy-hex :bins :cross-validation}
                                            {:size [600 600]})))))
    (let [{:keys [panels area]} (cells/bin-points {:a (take 100 cloud) :b (drop 100 cloud)} {:cell :square} {:size [600 600]})
          sides (fn [cs] (set (map (fn [{:keys [corners]}] (let [xs (map #(double (first %)) corners)] (m/approx (- (apply max xs) (apply min xs)) 6))) cs)))]
      (is (pos? (double area)))
      (is (= 1 (count (sides (:a panels)))) "equal cells")
      (is (= (sides (:a panels)) (sides (:b panels))) "one grid for both panels"))))

(deftest bin-points-boundary-test
  (testing "no points"
    (is (= {:panels {} :area 0.0} (cells/bin-points {} {:cell :square :bins 5} {})))
    (is (= {:panels {} :area 0.0} (cells/bin-points {:a []} {:cell :square :bins 5} {}))))
  (testing "one point, constant x / y - unit spans, one cell"
    (let [{:keys [panels area]} (cells/bin-points {:a [[3 3]]} {:cell :pointy-hex :bins 4} {})]
      (is (= [1] (map :count (:a panels))))
      (is (pos? (double area))))
    (let [{:keys [panels]} (cells/bin-points {:a [[3 1] [3 2] [3 5]]} {:cell :square :bins 4} {:size [400 400]})]
      (is (= 3 (reduce + (map :count (:a panels)))))))
  (testing "a panel without points next to one with"
    (let [{:keys [panels]} (cells/bin-points {:a [[0 0] [1 1]] :b []} {:cell :square :bins 2} {})]
      (is (= [] (:b panels))))))

;; 1.5 regularity

(defn- hex-pixel-sides
  "Side lengths in pixels of the first drawn cell (of any panel) of a built bin2d plot."
  [built]
  (let [{:keys [x y]} (:finalized-scales built)
        proj (scale/projection {:x x :y y})
        corners (:corners (first (mapcat :cells (vals (:data (first (:stats built)))))))
        pts (mapv (fn [[a b]] (proj a b)) corners)]
    (map (fn [^fastmath.vector.Vec2 p ^fastmath.vector.Vec2 q] (m/hypot (- (.x p) (.x q)) (- (.y p) (.y q))))
         pts (concat (rest pts) [(first pts)]))))

(defn- spread [sides] (/ (- (double (reduce max sides)) (double (reduce min sides))) (double (reduce max sides))))

(deftest regularity-test
  (testing "1.5 :aspect :iso - regular within 1 %"
    (let [b (plot/build {:data (map (fn [[x y]] {:u (* 0.3 (double x)) :v y}) cloud) :mapping {:x :u :y :v} :aspect :iso
                         :charts [(chart/bin2d {:bins 12})]}
                        {:width 600 :height 600})]
      (is (< (spread (hex-pixel-sides b)) 0.01))))
  (testing "1.5 a given :shape matching the panel - regular within 1 %"
    (let [p {:data (map (fn [[x y]] {:x x :y y}) cloud) :mapping {:x :x :y :y} :legend false}
          build (fn [shape] (plot/build (assoc p :charts [(chart/bin2d (cond-> {:bins 8} shape (assoc :shape shape)))])
                                        {:width 600 :height 400}))
          panel-shape (fn [b] (let [{:keys [w h]} (grid/item-box (:layout b) :panel)] (/ (double h) (double w))))
          ;; the shape changes the extents, so the tick labels and the panel: settle it in a few builds
          b (nth (iterate (comp build panel-shape) (build nil)) 3)]
      (is (< (m/abs (- 1.0 (/ (panel-shape b) (double (get-in b [:charts 0 :options :shape]))))) 0.005) "shape matches the panel")
      (is (< (spread (hex-pixel-sides b)) 0.01))))
  (testing "1.5 default 600 x 600 with a legend - within 10 %"
    (let [b (plot/build {:data (map (fn [[x y]] {:x x :y y}) cloud) :mapping {:x :x :y :y} :charts [(chart/bin2d {:bins 8})]}
                        {:width 600 :height 600})]
      (is (< (spread (hex-pixel-sides b)) 0.1)))))

;; 1.6 non-linear scales

(deftest log-scale-test
  (testing "1.6 log y: renders, cells projected corner by corner"
    (is (instance? BufferedImage (plot/render-image {:data (map (fn [[x y]] {:x x :y (+ 1.0 (* 100.0 (double y)))}) cloud)
                                                     :mapping {:x :x :y :y} :scales {:y {:kind :log}}
                                                     :charts [(chart/bin2d {:cell :square :bins 8})]}
                                                    {:width 400 :height 300})))))

;; lattice tessellation (3.2 / 3.3)

(defn- coverage
  "Counts of lattice cells (a 6 x 6 block) covering sample points of the inner region, edge points left out."
  [cell centre-x? centre-y?]
  (let [polys (for [i (range 6) j (range 6)]
                (mapv (fn [[dx dy]] [(+ i (double dx)) (+ j (double dy))]) (cells/lattice-offsets cell i j centre-x? centre-y?)))]
    (for [sx (range 2.0 4.0 0.0731) sy (range 2.0 4.0 0.0797)
          :let [p [sx sy]]
          :when (not-any? #(near-edge? % p 1e-3) polys)]
      (count (filter #(inside? % p 1e-3) polys)))))

(deftest lattice-test
  (doseq [cell [:square :shifted-square :pointy-hex :flat-hex :rhombus :triangle]]
    (testing (str "3.2 categorical lattice tessellates - one cell per point: " cell)
      (is (every? #(= 1 %) (coverage cell false false)))))
  (testing "3.3 numeric squares tile without gaps"
    (is (every? #(= 1 %) (coverage :square true true))))
  (testing "reach: squares half a cell, honeycomb shift reaches a whole column"
    (is (= [0.5 0.5] (cells/lattice-reach :square false false)))
    (is (m/delta-eq 1.0 (first (cells/lattice-reach :pointy-hex false false))))
    (is (m/delta-eq 0.5 (first (cells/lattice-reach :pointy-hex true false))) "numeric x: centred on the value")))

(deftest resolution-test
  (are [vs r] (== r (tile/resolution vs))
    [] 1.0
    [3] 1.0
    [3 3 3] 1.0
    [0 2 4 5] 1.0
    [5 0 0.5] 0.5
    [-1 1] 2.0))

;; 2. cell drawing

(deftest draw-cell-test
  (let [p (fn [opts] {:data [{:x "a" :y "b" :c "k"}] :mapping {:x :x :y :y :color :c} :legend false
                      :charts [(chart/tile opts)]})
        [_ img {:keys [x y w h]}] (render (p {}) {:width 200 :height 200})
        [_ img8 _] (render (p {:cell-scale 0.8}) {:width 200 :height 200})
        cx (+ (long x) (quot (long w) 2)) cy (+ (long y) (quot (long h) 2))
        edge-x (+ (long x) 3)]
    (testing "2.1 fill"
      (is (= [255 0 0] (rgb img cx cy)))
      (is (= [255 0 0] (rgb img edge-x cy))))
    (testing "2.1 :cell-scale 0.8 leaves a gap at the edge"
      (is (= [255 0 0] (rgb img8 cx cy)))
      (is (not= [255 0 0] (rgb img8 edge-x cy)) "the panel shows through"))))

;; 3. tile

(def ^:private grid-rows (for [x ["a" "b" "c"] y ["p" "q"]] {:x x :y y :v (double (+ (int (first x)) (int (first y))))}))

(deftest tile-schema-test
  (testing "3.1 mapping and options checked"
    (is (some? (error #(stats {:data grid-rows :mapping {:x :x :y :y} :charts [(chart/tile)]}))))
    (is (= :cell (last (:in (error #(stats {:data grid-rows :mapping {:x :x :y :y :color :v} :charts [(chart/tile {:cell :circle})]}))))))
    (is (some? (error #(stats {:data grid-rows :mapping {:x :x :y :y :color :v} :charts [(chart/tile {:cell-scale 0})]}))))
    (is (some? (error #(stats {:data [{:x (java.time.LocalDate/of 2020 1 1) :y "a" :v 1}] :mapping {:x :x :y :y :color :v}
                               :charts [(chart/tile)]}))) "dates are neither numbers nor categories")))

(deftest tile-compute-test
  (testing "3.2 categorical axes: band room on the side cells reach past, no extents widening"
    (let [s (stats {:data grid-rows :mapping {:x :x :y :y :color :v} :charts [(chart/tile {:cell :pointy-hex})]})
          {:keys [padding-in padding-out align-outer]} (get-in s [:scale-defaults :x])]
      (is (== 0.0 padding-in))
      (is (m/delta-eq 0.25 padding-out) "half a step in all, twice the per-side padding")
      (is (== 0.0 align-outer) "all of it after the last column (odd rows shifted right)")
      (is (m/delta-eq 0.5 (get-in s [:scale-defaults :y :align-outer])) "hexagon tips: both ends")
      (is (= ["a" "b" "c"] (get-in s [:extents :panel :x :values]))))
    (let [s (stats {:data grid-rows :mapping {:x :x :y :y :color :v} :charts [(chart/tile {:cell :flat-hex})]})]
      (is (== 1.0 (get-in s [:scale-defaults :y :align-outer])) "vertical: the room above the top row is the low-pixel side")
      (is (= {:padding-in 0.0 :padding-out 0.0} (get-in (stats {:data grid-rows :mapping {:x :x :y :y :color :v} :charts [(chart/tile)]})
                                                         [:scale-defaults :x])) "squares: no room"))
    (let [s (stats {:data grid-rows :mapping {:x :x :y :y :color :v} :flip true :charts [(chart/tile {:cell :flat-hex})]})]
      (is (== 0.0 (get-in s [:scale-defaults :y :align-outer])) "flipped: y runs horizontally, room after its last band")))
  (testing "3.3 numeric axes: extents widened by half the resolution, no nice domain"
    (let [s (stats {:data [{:x 0 :y 0 :v 1} {:x 2 :y 0 :v 2} {:x 4 :y 1 :v 3}] :mapping {:x :x :y :y :color :v} :charts [(chart/tile)]})]
      (is (= {:kind :numeric :min -1.0 :max 5.0} (get-in s [:extents :panel :x])))
      (is (= {:kind :numeric :min -0.5 :max 1.5} (get-in s [:extents :panel :y])))
      (is (= {:nice false} (get-in s [:scale-defaults :x])))))
  (testing "3.5 duplicates: the last row wins, counted; NA colour; invalid positions dropped"
    (let [s (stats {:data [{:x "a" :y "p" :v 1} {:x "a" :y "p" :v 2} {:x "b" :y "p" :v nil} {:x nil :y "p" :v 3}
                           {:x "c" :y ##NaN :v 3}]
                    :mapping {:x :x :y :y :color :v} :charts [(chart/tile)]})]
      (is (= [["a" "p" 2] ["b" "p" nil]] (get-in s [:data :panel :cells])))
      (is (= {:x 1 :y 1 :color 0 :duplicate 1} (:dropped s)))
      (is (= #{:color} (:na s)))))
  (testing "empty data: no cells, no extents"
    (let [s (stats {:data [] :mapping {:x :x :y :y :color :v} :charts [(chart/tile)]})]
      (is (empty? (get-in s [:data :panel :cells] [])))))
  (testing "3.4 mixed axes and every cell type render"
    (doseq [cell [:square :shifted-square :pointy-hex :flat-hex :rhombus :triangle]]
      (is (instance? BufferedImage (plot/render-image {:data [{:x "a" :y 1 :v 1} {:x "b" :y 2 :v 2} {:x "c" :y 3 :v 3}]
                                                       :mapping {:x :x :y :y :color :v} :charts [(chart/tile {:cell cell})]}
                                                      {:width 300 :height 300}))))))

(defn- legends [built] (some #(when (= :legends (:id %)) (get-in % [:content :legends])) (get-in built [:grid :items])))

(deftest tile-legend-test
  (testing "3.5 numeric colour - a color bar; categorical - legend keys"
    (let [b (plot/build {:data grid-rows :mapping {:x :x :y :y :color :v} :charts [(chart/tile)]} {:width 400 :height 300})]
      (is (= [:colorbar] (map :kind (legends b)))))
    (let [b (plot/build {:data grid-rows :mapping {:x :x :y :y :color :x} :charts [(chart/tile)]} {:width 400 :height 300})]
      (is (= [:legend] (map :kind (legends b))))
      (is (= 3 (count (:entries (first (legends b)))))))))

(deftest tile-value-label-test
  (testing "3.6 label text"
    (is (= "0.12" (#'tile/label-text true 0.123456 "a" "b")))
    (is (= "3" (#'tile/label-text true 3 "a" "b")))
    (is (= "a:b" (#'tile/label-text (fn [{:keys [x y]}] (str x ":" y)) 1 "a" "b")))
    (is (nil? (#'tile/label-text (constantly nil) 1 "a" "b")))
    (is (nil? (#'tile/label-text true nil "a" "b"))))
  (testing "3.6 white text on a dark cell, black on a light one"
    (let [p {:data [{:x "a" :y "p" :c "dark"} {:x "b" :y "p" :c "light"}] :mapping {:x :x :y :y :color :c} :legend false
             :theme {:palette {:color [[0 0 60] [250 250 200]]} :text {:tile-value {:size 30}}}
             :charts [(chart/tile {:value-label (constantly "WWW")})]}
          [_ img {:keys [x y w h]}] (render p {:width 300 :height 150})
          half (quot (long w) 2)
          pixels (fn [x0] (for [px (range (+ (long x0) 10) (+ (long x0) half -10)) py (range (+ (long y) 10) (+ (long y) (long h) -10))]
                            (rgb img px py)))]
      (is (some #{[255 255 255]} (pixels x)) "white ink on the dark cell")
      (is (some #{[0 0 0]} (pixels (+ (long x) half))) "black ink on the light cell"))))

(deftest tile-theme-test
  (testing "3.7 theme [:chart :tile] validated"
    (is (some? (error #(plot/build {:data grid-rows :mapping {:x :x :y :y :color :v} :charts [(chart/tile)]
                                    :theme {:chart {:tile {:cell :circle}}}} {:width 200 :height 200}))))
    (is (some? (error #(plot/build {:data grid-rows :mapping {:x :x :y :y :color :v} :charts [(chart/tile)]
                                    :theme {:chart {:tile {:bogus 1}}}} {:width 200 :height 200}))))))

;; 4. bin2d

(def ^:private cloud-rows (mapv (fn [[x y]] {:x x :y y}) cloud))
(def ^:private cloud-rows* cloud-rows)

(deftest bin2d-schema-test
  (testing "4.1 options and mapping"
    (is (some? (error #(stats {:data cloud-rows :mapping {:x :x :y :y :color :x} :charts [(chart/bin2d)]}))) "mapped :color")
    (is (nil? (error #(stats {:data cloud-rows :mapping {:x :x :y :y :color :x} :charts [(chart/bin2d {:mapping {:color nil}})]})))
        ":color set to nil in the chart's mapping")
    (are [opts] (some? (error #(stats {:data cloud-rows :mapping {:x :x :y :y} :charts [(chart/bin2d opts)]})))
      {:bins 0} {:bins 2.5} {:bins :sturges} {:bins :scott-2d} {:binwidth 0} {:shape -1} {:measure :mean} {:cell :hex})))

(deftest bin2d-measure-test
  (let [values (fn [measure] (map :value (get-in (stats {:data cloud-rows :mapping {:x :x :y :y}
                                                         :charts [(chart/bin2d {:bins 6 :measure measure})]})
                                                 [:data :panel :cells])))]
    (testing "4.2 measures"
      (is (= 500.0 (reduce + (values :count))))
      (is (m/delta-eq 100.0 (reduce + (values :percent)) 1e-9))
      (is (m/delta-eq 1.0 (reduce + (values :pmf)) 1e-9)))
    (testing "4.2 density x cell area sums to 1"
      (let [d (stats {:data cloud-rows :mapping {:x :x :y :y} :charts [(chart/bin2d {:bins 6 :measure :density})]})
            area (let [cs (:corners (first (get-in d [:data :panel :cells])))]
                   (* 0.5 (m/abs (reduce + (map (fn [[x1 y1] [x2 y2]] (- (* (double x1) (double y2)) (* (double x2) (double y1))))
                                                cs (concat (rest cs) [(first cs)]))))))]
        (is (m/delta-eq 1.0 (* area (reduce + (map :value (get-in d [:data :panel :cells])))) 1e-9)))))
  (testing "4.2 invalid rows dropped and counted; colour extent over all panels"
    (let [s (stats {:data (concat [{:x nil :y 1} {:x 1 :y ##NaN} {:x "a" :y 2}]
                                  (map #(assoc % :f (if (< (double (:x %)) 5.0) "l" "r")) cloud-rows))
                    :mapping {:x :x :y :y :facet :f} :charts [(chart/bin2d {:bins 5})]})]
      (is (= {:x 2 :y 1} (:dropped s)))
      (is (apply = (keep :color (vals (:extents s)))) "the NA facet panel has no cells")))
  (testing "empty data: no cells"
    (let [s (stats {:data [] :mapping {:x :x :y :y} :charts [(chart/bin2d)]})]
      (is (empty? (:data s))))))

(deftest bin2d-draw-test
  (testing "4.3 only occupied cells: an empty region stays background"
    (let [p {:data [{:x 0 :y 0} {:x 10 :y 10}] :mapping {:x :x :y :y} :legend false
             :charts [(chart/bin2d {:cell :square :bins 10})]}
          [built img {:keys [x y w h]}] (render p {:width 300 :height 300})
          proj (scale/projection (select-keys (:finalized-scales built) [:x :y]))
          ^fastmath.vector.Vec2 occupied (proj 0.2 0.2)]
      (is (= [255 255 255] (rgb img (+ (long x) (quot (long w) 2)) (+ (long y) (quot (long h) 2)))))
      (is (not= [255 255 255] (rgb img (+ (long x) (long (.x occupied))) (+ (long y) (long (.y occupied)))))))))

(deftest legend-title-test
  (testing "4.4 suggested legend title; a plot scale :title wins"
    (let [title (fn [p] (:title (first (legends (plot/build p {:width 400 :height 300})))))
          p {:data cloud-rows :mapping {:x :x :y :y} :charts [(chart/bin2d)]}]
      (is (= "Count" (title p)))
      (is (= "Percent of Total" (title (assoc p :charts [(chart/bin2d {:measure :percent})]))))
      (is (= "n" (title (assoc p :scales {:color {:title "n"}}))))
      (is (nil? (title (assoc p :scales {:color {:title nil}})))))))

(deftest bin2d-theme-test
  (testing "4.5 theme [:chart :bin2d] validated; default hexagons"
    (is (some? (error #(plot/build {:data cloud-rows :mapping {:x :x :y :y} :charts [(chart/bin2d)]
                                    :theme {:chart {:bin2d {:cell-scale 2}}}} {:width 200 :height 200}))))
    (is (= 6 (count (:corners (first (get-in (stats {:data cloud-rows :mapping {:x :x :y :y} :charts [(chart/bin2d)]})
                                             [:data :panel :cells]))))))))

;; 5.1.8 colour bar recipe

(deftest colour-bar-alignment-test
  (testing "5.1.8 the bar's band centres match the heatmap's within 1 px"
    (let [cars (mapv #(str "car " %) (range 12))
          rows (map-indexed (fn [i c] {:car c :cyl (str (* 2 (+ 2 (mod i 3)))) :v (double i)}) cars)
          b (plot/build (plot/arrange [{:data rows :mapping {:x :car :y (constantly "cyl") :color :cyl}
                                        :scales {:x {:domain cars :axis false}} :charts [(chart/tile)]}
                                       {:data (for [r rows k ["a" "b" "c"]] (assoc r :k k))
                                        :mapping {:x :car :y :k :color :v} :scales {:x {:domain cars}} :charts [(chart/tile)]}]
                                      {:columns 1 :heights [1 4]})
                        {:width 900 :height 650})
          centre (fn [id level]
                   (let [s (get-in b [:subplots id])
                         cell (grid/item-box (:layout b) id)
                         panel (grid/item-box (:layout s) :panel)]
                     (+ (double (:x cell)) (double (:x panel)) (scale/forward (get-in s [:finalized-scales :x]) level))))]
      (is (every? (fn [car] (< (m/abs (- (centre [:plot 0] car) (centre [:plot 1] car))) 1.0)) cars)))))

;; review fixes

(deftest regularity-estimate-test
  (testing "R1 hexagons within 10 % of regular: wide legends, tall / wide plots, facets"
    (doseq [[w h opts p] [[600 600 {:measure :percent} {}] [400 900 {} {}] [900 400 {} {}] [600 600 {:measure :density} {}]
                          [900 400 {} {:mapping {:x :x :y :y :facet :f}}] [600 700 {} {:mapping {:x :x :y :y :facet :f} :facets {:columns 1}}]
                          [600 500 {} {:labels {:title "A title" :x "x title" :y "y title"}}]]]
      (let [rows (map (fn [[x y]] {:x x :y y :f (if (< (double x) 5.0) "left" "right")}) cloud)
            b (plot/build (merge {:data rows :mapping {:x :x :y :y} :charts [(chart/bin2d (merge {:bins 8} opts))]} p)
                          {:width w :height h})]
        (is (< (spread (hex-pixel-sides b)) 0.1) (str [w h opts p]))))))

(deftest estimated-panel-size-test
  (testing "R1 the estimate is within 5 % of the laid-out panel"
    (doseq [[p size] [[{:data grid-rows :mapping {:x :x :y :y :color :v} :charts [(chart/tile)]} {:width 600 :height 400}]
                      [{:data grid-rows :mapping {:x :x :y :y :color :x} :charts [(chart/tile)] :labels {:title "t" :x "x"}} {:width 500 :height 500}]
                      [{:data cloud-rows* :mapping {:x :x :y :y} :charts [(chart/bin2d {:measure :percent})]} {:width 700 :height 400}]]]
      (let [b (plot/build (assoc p :charts (conj (:charts p) (probe-chart))) size)
            [ew eh] @(:panel-size @seen-env)
            {:keys [w h]} (grid/item-box (:layout b) :panel)]
        (is (< (m/abs (- 1.0 (/ (double ew) (double w)))) 0.05) (str "width " ew " vs " w))
        (is (< (m/abs (- 1.0 (/ (double eh) (double h)))) 0.05) (str "height " eh " vs " h))))))

(deftest provisional-legends-test
  (let [inputs (fn [mapping rows] [{:mapping mapping :groups {:panel rows}}])]
    (testing "R1 legends expected before stats"
      (is (= [] (legend/provisional-legends {:charts [] :scales {} :legend false} (inputs {:color :c} [{:c "a"}]))))
      (is (= [] (legend/provisional-legends {:charts [] :scales {}} (inputs {:x :x} [{:x 1}]))) "nothing mapped")
      (let [[l] (legend/provisional-legends {:charts [] :scales {}} (inputs {:color :c} [{:c "a"} {:c "b"} {:c "a"} {:c nil}]))]
        (is (= [:legend "c" ["a" "b"]] [(:kind l) (:title l) (mapv :label (:entries l))])))
      (let [[l] (legend/provisional-legends {:charts [] :scales {}} (inputs {:color :v} [{:v 1} {:v 2}]))]
        (is (= [:colorbar "v"] [(:kind l) (:title l)])))
      (let [[l] (legend/provisional-legends {:charts [(chart/bin2d {:measure :pmf})] :scales {}} (inputs {:x :x :y :y} [{:x 1 :y 1}]))]
        (is (= [:colorbar "Probability"] [(:kind l) (:title l)])))
      (is (= [] (legend/provisional-legends {:charts [] :scales {:color {:legend false}}} (inputs {:color :c} [{:c "a"}]))))
      (is (= "own" (:title (first (legend/provisional-legends {:charts [] :scales {:color {:title "own"}}} (inputs {:color :c} [{:c "a"}])))))))))

(deftest computed-aesthetics-test
  (testing "R4 another chart can't map an aesthetic bin2d computes"
    (is (= [:charts 0 :options :mapping :color]
           (:in (error #(stats {:data cloud-rows* :mapping {:x :x :y :y}
                                :charts [(chart/scatter {:mapping {:color :x}}) (chart/bin2d)]})))))
    (is (nil? (error #(stats {:data cloud-rows* :mapping {:x :x :y :y} :charts [(chart/scatter) (chart/bin2d)]})))))
  (testing "the measure titles the computed color"
    (is (= {:color "Density"} (:computed-aesthetics (chart/bin2d {:measure :density}))))
    (is (= {:color "Count"} (:computed-aesthetics (chart/bin2d))))))

(deftest tile-label-fixes-test
  (testing "R6 whole numbers without decimals, keyword names"
    (is (= "321" (#'tile/label-text true 321.0 "a" "b")))
    (is (= "-2" (#'tile/label-text true -2.0004 "a" "b")))
    (is (= "a" (#'tile/label-text true :a "a" "b"))))
  (testing "R6 contrast against the painted color: a translucent dark fill reads light"
    (let [p {:data [{:x "a" :y "p" :c "dark"}] :mapping {:x :x :y :y :color :c} :legend false
             :theme {:palette {:color [[0 0 60]]} :text {:tile-value {:size 30}}}
             :charts [(chart/tile {:alpha 0.15 :value-label (constantly "WWW")})]}
          [_ img {:keys [x y w h]}] (render p {:width 200 :height 150})
          pixels (for [px (range (+ (long x) 10) (+ (long x) (long w) -10)) py (range (+ (long y) 10) (+ (long y) (long h) -10))]
                   (rgb img px py))]
      (is (some #{[0 0 0]} pixels) "black ink")
      (is (not-any? #{[255 255 255]} (remove #(= % (rgb img (+ (long x) 2) (+ (long y) 2))) pixels)) "no white ink"))))

(deftest tile-flip-facets-test
  (testing "R7 flipped tile: the cell of (a, p) sits where the flipped projection puts it"
    (let [p {:data [{:x "a" :y "p" :c "k"} {:x "b" :y "q" :c "m"}] :mapping {:x :x :y :y :color :c} :legend false :flip true
             :charts [(chart/tile)]}
          [built img {:keys [x y]}] (render p {:width 300 :height 300})
          proj (scale/projection (select-keys (:finalized-scales built) [:x :y]))
          ^fastmath.vector.Vec2 a (proj "a" "p")
          ^fastmath.vector.Vec2 b (proj "b" "q")]
      (is (= [255 0 0] (rgb img (+ (long x) (long (.x a))) (+ (long y) (long (.y a))))))
      (is (= [0 0 255] (rgb img (+ (long x) (long (.x b))) (+ (long y) (long (.y b))))))))
  (testing "R7 faceted tile: a heatmap per panel"
    (let [s (stats {:data (concat (map #(assoc % :f "one") grid-rows) (map #(assoc % :f "two") (take 2 grid-rows)))
                    :mapping {:x :x :y :y :color :v :facet :f} :charts [(chart/tile)]})]
      (is (= [6 2] (map (comp count :cells) (map (:data s) ["one" "two"]))))
      (is (= (get-in s [:data "one" :axes]) (get-in s [:data "two" :axes])) "one layout over the panels"))))
