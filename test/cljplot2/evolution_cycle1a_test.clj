(ns cljplot2.evolution-cycle1a-test
  "Evolution cycle 1a: dash styles and the :dash aesthetic, curve geometry (interpolators, sampling, curves, gaps),
  line curve options, the function chart."
  (:require [clojure.test :refer [deftest testing is are]]
            [cljplot2.test-util :refer [error]]
            [cljplot2.data.interpolation :as fit]
            [clojure2d.color :as c]
            [fastmath.core :as m]
            [cljplot2.chart :as chart]
            [cljplot2.chart.function :as function]
            [cljplot2.chart.line :as line]
            [cljplot2.core :as plot]
            [cljplot2.data.facet :as facet]
            [cljplot2.data.group :as group]
            [cljplot2.graphics.curve :as curve]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.layout.grid :as grid]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema])
  (:import [java.awt.image BufferedImage]
           [java.time LocalDate LocalDateTime]))

(def ^:private size {:width 400 :height 300})
(defn- build [p] (plot/build p size))
(def ^:private white {:plot {:background :white} :panel {:grid {:x :none :y :none}}})

(defn- rgba [^BufferedImage img x y]
  (let [v (.getRGB img (int x) (int y))]
    [(bit-and (bit-shift-right v 16) 0xff) (bit-and (bit-shift-right v 8) 0xff) (bit-and v 0xff)
     (bit-and (bit-shift-right v 24) 0xff)]))
(defn- rgb [img x y] (vec (take 3 (rgba img x y))))
(defn- white? [img x y] (= [255 255 255] (rgb img x y)))
(defn- legends [built] (some #(when (= :legends (:id %)) (get-in % [:content :legends])) (get-in built [:grid :items])))
(defn- panel-box [p] (grid/item-box (:layout (build p)) :panel))
(defn- same-image? [p q]
  (let [^BufferedImage a (plot/render-image p size) ^BufferedImage b (plot/render-image q size)]
    (every? true? (for [x (range 0 (.getWidth a) 2) y (range 0 (.getHeight a) 2)] (= (.getRGB a x y) (.getRGB b x y))))))

(def ^:private pts [{:x 0 :y 1} {:x 1 :y 3} {:x 2 :y 2} {:x 3 :y 4} {:x 4 :y 1}])
(defn- line-plot [opts & [extra]]
  (merge {:data pts :mapping {:x :x :y :y} :charts [(chart/line opts)] :theme white} extra))

;; 1. dash styles

(deftest dash-names-test
  (testing "1.1 names resolve to fixed pixel lengths; :solid is solid"
    (is (= [5.0 3.0] (polyline/dash-pattern :dashed)))
    (is (= [1.0 2.0 5.0 2.0] (polyline/dash-pattern :dot-dash)))
    (is (nil? (polyline/dash-pattern :solid)))
    (is (nil? (polyline/dash-pattern :none)))
    (is (nil? (polyline/dash-pattern nil)))
    (is (= [2.0 1.0] (polyline/dash-pattern [2 1])))
    (is (same-image? (line-plot {:dash :dashed :line-width 2}) (line-plot {:dash [5 3] :line-width 2})))
    (is (same-image? (line-plot {:dash :solid}) (line-plot {:dash :none}))))
  (testing "1.1 an unknown name throws {:key :dash} with the closest name"
    (let [e (error #(build (line-plot {:dash :dashd})))]
      (is (= :dash (:key e))))
    (is (re-find #"should be spelled :dashed"
                 (try (build (line-plot {:dash :dashd})) (catch Exception e (ex-message e)))))
    (is (= :dash (:key (error #(build (line-plot {:dash []}))))))
    (is (= :dash (:key (error #(build (line-plot {:dash [1 0]})))))))
  (testing "1.2 theme grid lines take names"
    (let [grid-theme (fn [dash] {:panel {:grid {:x {:color :black :line-width 1 :dash dash} :y :none}}})]
      (is (same-image? (line-plot {} {:theme (grid-theme :dotted)}) (line-plot {} {:theme (grid-theme [1 2])}))))))

(def ^:private grouped (for [s ["a" "b" "c"] i (range 4)] {:x i :y (+ i ({"a" 0 "b" 3 "c" 6} s)) :s s}))

(deftest dash-aesthetic-test
  (testing "1.3 an ordinal :dash scale cycles theme :palette :dash"
    (let [b (build {:data grouped :mapping {:x :x :y :y :dash :s} :charts [(chart/line)]})]
      (is (= [:solid :dashed :dotted] (mapv #(get-in % [:attributes :dash]) (:entries (first (legends b))))))))
  (testing "1.3 level map and vector ranges, reverse"
    (let [b (build {:data grouped :mapping {:x :x :y :y :dash :s} :charts [(chart/line)]
                    :scales {:dash {:range {"a" :dotted "b" :dashed "c" [9 1]}}}})]
      (is (= [:dotted :dashed [9 1]] (mapv #(get-in % [:attributes :dash]) (:entries (first (legends b)))))))
    (let [b (build {:data grouped :mapping {:x :x :y :y :dash :s} :charts [(chart/line)]
                    :scales {:dash {:range [:dashed :dotted] :reverse true}}})]
      (is (= [:dashed :dotted :dashed] (mapv #(get-in % [:attributes :dash]) (:entries (first (legends b))))))))
  (testing "1.3 numeric data is ordinal; :linear / :binned throw {:key :kind}"
    (let [data (for [i (range 6)] {:x (mod i 3) :y i :d (quot i 3)})]
      (is (= 2 (count (:entries (first (legends (build {:data data :mapping {:x :x :y :y :dash :d} :charts [(chart/line)]}))))))))
    (is (= :kind (:key (error #(build {:data grouped :mapping {:x :x :y :y :dash :y} :scales {:dash {:kind :linear}} :charts [(chart/line)]})))))
    (is (= :kind (:key (error #(build {:data grouped :mapping {:x :x :y :y :dash :y} :scales {:dash {:kind :binned}} :charts [(chart/line)]}))))))
  (testing "1.3 identity values are dashes; invalid values throw {:key :mapping}"
    (let [data [{:x 0 :y 0 :d :dotted} {:x 1 :y 1 :d :dotted} {:x 0 :y 1 :d [3 1]} {:x 1 :y 2 :d [3 1]}]]
      (is (some? (plot/render-image {:data data :mapping {:x :x :y :y :dash :d} :scales {:dash {:kind :identity}} :charts [(chart/line)]} size))))
    (is (= :mapping (:key (error #(build {:data [{:x 0 :y 0 :d :wavy}] :mapping {:x :x :y :y :dash :d}
                                          :scales {:dash {:kind :identity}} :charts [(chart/line)]}))))))
  (testing "1.3 NA takes the base dash (no attribute: the glyph and the line draw the chart's dash)"
    (let [data (concat grouped [{:x 0 :y 9 :s nil} {:x 1 :y 9 :s nil}])
          b (build {:data data :mapping {:x :x :y :y :dash :s} :charts [(chart/line {:dash :dashed})]})]
      (is (= "NA" (:label (peek (:entries (first (legends b)))))))
      (is (not (contains? (:attributes (peek (:entries (first (legends b))))) :dash)))
      (is (= :dashed (:dash (group/style {:dash :dashed} {:dash nil} (:scale-specs b) {}))))))
  (testing "1.3 invalid ranges are rejected by the scale schema"
    (is (= :range (:key (error #(build {:data grouped :mapping {:x :x :y :y :dash :s} :scales {:dash {:range [:wavy]}} :charts [(chart/line)]})))))))

(deftest dash-charts-test
  (testing "1.4 color and dash on one variable merge into one legend with both attributes"
    (let [b (build {:data grouped :mapping {:x :x :y :y :color :s :dash :s} :charts [(chart/line)]})
          ls (legends b)]
      (is (= 1 (count ls)))
      (is (every? #(and (:color (:attributes %)) (:dash (:attributes %))) (:entries (first ls))))))
  (testing "1.4 line, density and ecdf draw a mapped dash"
    (doseq [[ch mapping] [[(chart/line {:line-width 3}) {:x :x :y :y}]
                          [(chart/density {:line-width 3}) {:x :y}]
                          [(chart/ecdf {:line-width 3}) {:x :y}]]
            :let [p {:data grouped :theme white :mapping mapping :charts [ch]}]]
      (is (not (same-image? p (assoc-in p [:mapping :dash] :s))) (str (:kind ch) " changes with :dash mapped"))))
  (testing "1.4 a data rule draws a mapped dash through its own mapping"
    (let [p (fn [mapping] {:data grouped :theme white :charts [(chart/rule {:mapping mapping :line-width 3})]})]
      (is (not (same-image? (p {:y :y}) (p {:y :y :dash :s}))))))
  (testing "1.5 a chart without lines ignores a mapped dash, as :shape on line"
    (is (some? (plot/render-image {:data grouped :mapping {:x :x :y :y :dash :s} :charts [(chart/scatter)]} size)))
    (is (some? (plot/render-image {:data grouped :mapping {:x :x :y :y :shape :s} :charts [(chart/line)]} size)))))

;; 2. curve geometry

(deftest sample-grid-test
  (testing "2.1 pixel steps and counts, ends included"
    (is (= 11 (count (curve/sample-grid 0.0 10.0 1.0))))
    (is (= [0.0 5.0 10.0] (curve/sample-grid 0.0 10.0 {:count 3})))
    (is (= [0.0 2.5 5.0 7.5 10.0] (curve/sample-grid 0.0 10.0 3.0)) "the span split into equal steps no longer than the step")
    (is (= [10.0 5.0 0.0] (curve/sample-grid 10.0 0.0 5.0)) "decreasing")
    (is (= [0.0 10.0] (curve/sample-grid 0.0 10.0 100.0)) "a step longer than the span")
    (is (= [3.0] (curve/sample-grid 3.0 3.0 1.0)) "equal ends"))
  (testing "2.1 invalid steps throw {:key :sample-step}"
    (are [v] (= :sample-step (:key (error #(build (line-plot {:interpolator :linear :sample-step v})))))
      0 -1 ##Inf {:count 1} {:count 2.5} {:n 3} {:count 3 :x 1})))

(deftest functional-interpolation-test
  (let [run [[0.0 10.0] [10.0 0.0] [20.0 10.0] [30.0 5.0]]
        fit (fit/fit-fn :pchip nil)]
    (testing "2.2 pchip passes through every point; samples every step"
      (let [drawn (curve/functional-points fit run :x 1.0)]
        (is (= 31 (count drawn)))
        (is (every? (fn [[x y]] (some (fn [[a b]] (and (= x a) (< (Math/abs (- (double y) (double b))) 0.5))) drawn)) run))))
    (testing "2.2 steps are flat"
      (let [drawn (curve/functional-points (fit/fit-fn :step-after nil) run :x 1.0)]
        (is (= #{10.0} (set (map second (filter #(< (first %) 10.0) drawn)))))))
    (testing "2.2 decreasing positions (reversed scale) keep the run's direction"
      (let [drawn (curve/functional-points fit (vec (rseq run)) :x 1.0)]
        (is (= [30.0 5.0] (first drawn)))
        (is (= [0.0 10.0] (peek drawn)))))
    (testing "2.2 along y (flipped)"
      (let [drawn (curve/functional-points fit (mapv (fn [[a b]] [b a]) run) :y 1.0)]
        (is (= [10.0 0.0] (first drawn)))))
    (testing "2.2 one point draws nothing, an empty run stays empty"
      (is (= [[1.0 1.0]] (curve/functional-points fit [[1.0 1.0]] :x 1.0)))
      (is (= [] (curve/functional-points fit [] :x 1.0))))
    (testing "2.3 repeated or unordered positions throw {:key :interpolator}"
      (is (= :interpolator (:key (error #(curve/functional-points fit [[0.0 1.0] [0.0 2.0] [1.0 3.0]] :x 1.0)))))
      (is (= :interpolator (:key (error #(curve/functional-points fit [[0.0 1.0] [2.0 2.0] [1.0 3.0]] :x 1.0))))))
    (testing "2.3 in a chart: repeated x, :order :data out of order; the group is named"
      (is (= :interpolator (:key (error #(plot/render-image (line-plot {:interpolator :pchip} {:data (conj pts {:x 2 :y 7})}) size)))))
      (let [e (error #(plot/render-image {:data (map (fn [r] (assoc r :g "a")) [{:x 1 :y 1} {:x 3 :y 2} {:x 2 :y 3} {:x 4 :y 1}]) :mapping {:x :x :y :y :color :g}
                                          :charts [(chart/line {:interpolator :pchip :order :data})]} size))]
        (is (= :interpolator (:key e)))
        (is (= {:color "a"} (:group e)))))
    (testing "2.4 methods, functions with params; unknown and 2D methods are rejected"
      (is (some? (plot/render-image (line-plot {:interpolator :loess :interpolator-params [{:bandwidth 0.9}]}) size)))
      (let [calls (atom nil)
            f (fn [xs ys k] (reset! calls [(count xs) k]) (fn [x] (* k (double x))))]
        (plot/render-image (line-plot {:interpolator f :interpolator-params [2.0]}) size)
        (is (= [5 2.0] @calls)))
      (is (= :interpolator (:key (error #(build (line-plot {:interpolator :blah}))))))
      (is (= :interpolator (:key (error #(build (line-plot {:interpolator :bilinear}))))))
      (is (= :interpolator (:key (error #(plot/render-image (line-plot {:interpolator :loess}) size))))
          "other fastmath input errors"))
    (testing "2.5 too few points for the method: the run as is"
      (is (= [[0.0 0.0] [1.0 1.0] [2.0 0.0]] (curve/functional-points (fit/fit-fn :akima nil) [[0.0 0.0] [1.0 1.0] [2.0 0.0]] :x 1.0))))))

(deftest paths-test
  (let [square [[0.0 0.0] [10.0 0.0] [10.0 10.0] [0.0 10.0] [0.0 0.0]]
        near? (fn [drawn [x y]] (some (fn [[a b]] (< (m/hypot (- (double a) x) (- (double b) y)) 0.5)) drawn))]
    (testing "2.6 parametric: through every point, curved corners, repeated points removed"
      (let [drawn (curve/curve-points square {:curve :parametric})]
        (is (every? #(near? drawn %) square))
        (is (some (fn [[x y]] (and (< 10.0 (double x)) (< 0.0 (double y) 10.0))) drawn) "overshoots the corner: curved")
        (is (= [[1.0 1.0] [5.0 5.0]] (curve/curve-points [[1.0 1.0] [1.0 1.0] [5.0 5.0]] {:curve :parametric :interpolator :akima}))
            "repeated points removed; too few distinct points for akima: straight")))
    (testing "2.6 the parametric interpolator can be chosen"
      (is (every? #(near? (curve/curve-points square {:curve :parametric :interpolator :linear}) %) square)))
    (testing "2.7 bezier: through every point; can't take an interpolator"
      (let [run [[0.0 0.0] [10.0 5.0] [20.0 0.0] [30.0 5.0]]]
        (is (every? #(near? (curve/curve-points run {:curve :bezier}) %) run)))
      (is (= [[0.0 0.0] [5.0 5.0]] (curve/curve-points [[0.0 0.0] [5.0 5.0]] {:curve :bezier})) "two points: a line")
      (is (= :curve (:key (error #(curve/curve-points square {:curve :bezier :interpolator :cubic}))))))
    (testing "2.6 / 2.7 in a chart (connected scatter)"
      (is (some? (plot/render-image (line-plot {:curve :bezier :order :data}) size)))
      (is (some? (plot/render-image (line-plot {:curve :parametric :order :data}) size))))))

(deftest gaps-test
  (let [data [{:x 0 :y 0} {:x 1 :y 1} {:x 2 :y nil} {:x 3 :y 1} {:x 4 :y 0}]
        p (fn [opts] {:data data :mapping {:x :x :y :y} :theme white
                      :charts [(chart/line (merge {:color [0 0 255] :line-width 3} opts))]})
        mid (fn [q] (let [{:keys [x y w h]} (panel-box q)
                          img (plot/render-image q size)]
                      ;; the gap's middle: x = 2, y = 1 of an x span 0..4 and y span 0..1 padded by 5%
                      (rgb img (+ x (* 0.5 w)) (+ y (* (/ 0.05 1.1) h)))))]
    (testing "2.8 break: background in the gap; bridge: the line color at the bridge alpha"
      (is (= [255 255 255] (mid (p {}))))
      (let [[r g b] (mid (p {:gaps :bridge}))]
        (is (< 170 r 185) "~30 % blue over white")
        (is (= 255 b)))
      (let [[r _ _] (mid (p {:gaps :bridge :bridge {:alpha-factor 1.0}}))]
        (is (< r 10) "alpha 1: full color")))
    (testing "2.8 bridges between consecutive runs"
      (is (= [[[1 1] [3 3]] [[4 4] [6 6]]] (curve/bridges [[[0 0] [1 1]] [[3 3] [4 4]] [] [[6 6]]])))
      (is (= [] (curve/bridges [[[0 0] [1 1]]]))))
    (testing "2.8 bridge style: alpha multiplied, width and dash over the line's"
      (let [st (polyline/bridge-style {:color [0 0 255 200] :line-width 2 :dash :none} {:alpha-factor 0.5 :dash :dotted})]
        (is (= 100.0 (c/alpha (:color st))))
        (is (= :dotted (:dash st)))
        (is (= 2 (:line-width st))))
      (is (= :none (:color (polyline/bridge-style {:color :none :line-width 1} {:alpha 0.3})))))
    (testing "2.8 :gaps is validated"
      (is (= :gaps (:key (error #(build (p {:gaps :join})))))))))

(deftest pixel-clamp-test
  (testing "2.9 coordinates clamped to the pixel limit"
    (is (= [1.0e6 -1.0e6] (curve/drawn-point [1.0e300 ##-Inf])))
    (is (= [3.0 4.0] (curve/drawn-point [3 4]))))
  (testing "2.9 a huge value under an explicit y domain draws clipped"
    (is (some? (plot/render-image (line-plot {} {:data (conj pts {:x 5 :y 1.0e300}) :scales {:y {:domain [0 5]}}}) size)))))

;; 3. line options

(deftest line-options-test
  (testing "3.1 the closed options schema takes the curve options"
    (is (some? (build (line-plot {:interpolator :cubic :interpolator-params [] :sample-step {:count 10} :gaps :break}))))
    (is (= :curve (:key (error #(build (line-plot {:curve :spline})))))))
  (testing "3.2 the area follows the curve"
    (let [data [{:x 0 :y 0} {:x 1 :y 10} {:x 2 :y 0} {:x 3 :y 10}]
          p (fn [opts] {:data data :mapping {:x :x :y :y} :theme white
                        :charts [(chart/line (merge {:fill [255 0 0] :color :none} opts))]})
          {:keys [x y w h]} (panel-box (p {}))
          ;; x = 0.5: the straight line is at y = 5, cubic overshoots above it
          probe (fn [q] (rgb (plot/render-image q size) (+ x (* (/ 0.65 3.3) w)) (+ y (* (/ (- 10.5 5.6) 11.0) h))))]
      (is (= [255 255 255] (probe (p {}))))
      (is (= [255 0 0] (probe (p {:interpolator :cubic}))))))
  (testing "3.3 gradient lines: sampled color values interpolated linearly; :curve rejected"
    (let [lerp @#'line/lerp-values]
      (is (= [0.0 5.0 10.0 20.0] (lerp [0.0 10.0 20.0] [0.0 10.0 20.0] [0.0 5.0 10.0 20.0])))
      (is (= [15.0 5.0] (lerp [20.0 10.0 0.0] [20.0 10.0 0.0] [15.0 5.0])) "decreasing positions")
      (is (= [nil nil 25.0] (lerp [0.0 10.0 20.0 30.0] [0.0 nil 20.0 30.0] [5.0 15.0 25.0])) "an NA end gives NA"))
    (is (some? (plot/render-image (line-plot {:interpolator :pchip :gaps :bridge} {:mapping {:x :x :y :y :color :y}}) size)))
    (is (= :curve (:key (error #(plot/render-image (line-plot {:curve :parametric} {:mapping {:x :x :y :y :color :y}}) size)))))))

;; 4. function chart

(defn- fn-plot [opts & [extra]] (merge {:charts [(chart/function opts)] :theme white} extra))
(defn- fn-stats [opts & [extra]] (get-in (build (fn-plot opts extra)) [:stats 0]))

(deftest function-input-test
  (testing "4.2 exactly one of :f / :parametric"
    (is (= :f (:key (error #(build (fn-plot {}))))))
    (is (= :f (:key (error #(build (fn-plot {:f m/sin :parametric m/cos :domain [0 1]})))))))
  (testing "4.2 labels: map keys, vector indices, 0 for one function; theme style without a mapping"
    (let [b (build (fn-plot {:f [m/sin m/cos] :domain [0 3] :mapping {:color :function}}))]
      (is (= ["0" "1"] (mapv :label (:entries (first (legends b)))))))
    (let [b (build (fn-plot {:f {"s" m/sin "c" m/cos} :domain [0 3] :mapping {:dash :function}}))]
      (is (= #{"s" "c"} (set (map :label (:entries (first (legends b))))))))
    (let [fs (get-in (fn-stats {:f [m/sin m/cos] :domain [0 3]}) [:data facet/all-panels :functions])]
      (is (= 2 (count fs)))
      (is (= [{} {}] (mapv :aesthetics fs)))))
  (testing "4.2 labels are levels: numeric labels get an ordinal scale"
    (let [b (build (fn-plot {:f {1 m/sin 2 m/cos 3 m/tan} :domain [0 1] :mapping {:color :function}}))]
      (is (= :ordinal (get-in b [:scale-specs :color :kind])))
      (is (= ["1" "2" "3"] (mapv :label (:entries (first (legends b))))))))
  (testing "4.2 the plot mapping isn't inherited (only facets)"
    (is (some? (plot/render-image {:data grouped :mapping {:x :x :y :y :color :s}
                                   :charts [(chart/line) (chart/function {:f (fn [x] (* 2 x)) :domain [0 3]})]} size))))
  (testing "4.2 :data is rejected"
    (is (= :data (:key (error #(build (fn-plot {:f m/sin :domain [0 1] :data [{:x 1}]}))))))))

(deftest function-domain-test
  (testing "4.3 :domain gives the x extent"
    (is (= {:kind :numeric :min 0.0 :max 2.0} (get-in (fn-stats {:f m/sin :domain [0 2]}) [:extents facet/all-panels :x]))))
  (testing "4.3 the plot's explicit x domain is sampled"
    (is (= 0.0 (get-in (fn-stats {:f m/sin :y-extent :min-max} {:scales {:x {:domain [0 m/PI]}}}) [:extents facet/all-panels :y :min]))))
  (testing "4.3 without both: no extents, drawn over another chart's x"
    (let [p (fn-plot {:f (fn [x] (* 0.5 x))} {:data pts :mapping {:x :x :y :y} :charts [(chart/scatter) (chart/function {:f (fn [x] (* 0.5 x))})]})]
      (is (nil? (get-in (build p) [:stats 1 :extents facet/all-panels :x])))
      (is (some? (plot/render-image p size)))))
  (testing "4.3 alone without a domain - {:key :domain}; no y anywhere - {:key :y-extent}"
    (is (= :domain (:key (error #(plot/render-image (fn-plot {:f m/sin}) size)))))
    (is (= :y-extent (:key (error #(plot/render-image (fn-plot {:f m/sin :domain [0 1] :y-extent :none}) size))))))
  (testing "4.3 bad spans"
    (are [d] (= :domain (:key (error #(build (fn-plot {:f m/sin :domain d})))))
      [1 0] [0 0] [0] [0 ##Inf] [0 (LocalDate/of 2016 1 1)] [(LocalDate/of 2016 1 1) (LocalDate/of 2016 1 1)])))

(deftest parametric-test
  (testing "4.4 :t required; count sampling only"
    (is (= :t (:key (error #(build (fn-plot {:parametric (fn [t] [t t])}))))))
    (is (= :sample-step (:key (error #(build (fn-plot {:parametric (fn [t] [t t]) :t [0 1] :sample-step 2}))))))
    (is (= :domain (:key (error #(build (fn-plot {:parametric (fn [t] [t t]) :t [0 1] :domain [0 1]}))))))
    (is (= :t (:key (error #(build (fn-plot {:f m/sin :domain [0 1] :t [0 1]})))))))
  (testing "4.4 extents: x by min / max, y by :y-extent"
    (let [e (get-in (fn-stats {:parametric (fn [t] [(m/cos t) (m/sin t)]) :t [0 m/TWO_PI] :y-extent :min-max})
                    [:extents facet/all-panels])]
      (is (< (Math/abs (- -1.0 (double (get-in e [:x :min])))) 1e-3))
      (is (< (Math/abs (- 1.0 (double (get-in e [:y :max])))) 1e-3))))
  (testing "4.4 invalid results are skipped"
    (is (some? (plot/render-image (fn-plot {:parametric (fn [t] (when (pos? t) [t (m/sqrt t)])) :t [-1 1]}) size)))))

(deftest function-temporal-test
  (testing "4.5 a temporal domain: f receives LocalDateTime"
    (let [seen (atom #{})
          p (fn-plot {:f (fn [d] (swap! seen conj (class d)) (.getDayOfYear ^LocalDateTime d))
                      :domain [(LocalDate/of 2016 1 1) (LocalDate/of 2016 12 31)]})]
      (plot/render-image p size)
      (is (= #{LocalDateTime} @seen))
      (is (= :temporal (get-in (build p) [:stats 0 :extents facet/all-panels :x :kind])))))
  (testing "4.5 without a domain on a datetime x scale"
    (let [seen (atom #{})]
      (plot/render-image (fn-plot {:f (fn [d] (swap! seen conj (class d)) 1.0)}
                                  {:scales {:x {:domain [(LocalDate/of 2016 1 1) (LocalDate/of 2016 2 1)]}}})
                         size)
      (is (= #{LocalDateTime} @seen))))
  (testing "4.5 a temporal :t, evenly in elapsed time"
    (let [ts (atom [])]
      (plot/render-image (fn-plot {:parametric (fn [d] (swap! ts conj d) [(.getDayOfMonth ^LocalDateTime d) 1.0])
                                   :t [(LocalDate/of 2016 1 1) (LocalDate/of 2016 1 3)] :sample-step {:count 3}})
                         size)
      (is (= [(LocalDateTime/of 2016 1 1 0 0) (LocalDateTime/of 2016 1 2 0 0) (LocalDateTime/of 2016 1 3 0 0)]
             (vec (take 3 @ts)))))))

(deftest inverse-fn-test
  (testing "4.6 inverse of forward on linear, log and datetime scales"
    (doseq [[spec values] [[{:kind :linear :domain [-3.0 7.0]} [-3.0 0.5 7.0]]
                           [{:kind :log :domain [1.0 1000.0]} [1.0 31.6 1000.0]]]]
      (let [f (scale/finalize (scale/scale-spec :x spec) [0.0 400.0] 5)
            fwd (scale/forward-fn f) inv (scale/inverse-fn f)]
        (doseq [v values] (is (< (Math/abs (- v (double (inv (fwd v))))) 1e-9)))))
    (let [f (scale/finalize (scale/scale-spec :x {:domain [(LocalDateTime/of 2016 1 1 0 0) (LocalDateTime/of 2017 1 1 0 0)]}) [0.0 400.0] 5)
          d (LocalDateTime/of 2016 6 15 12 30 45 123000000)]
      (is (= d ((scale/inverse-fn f) ((scale/forward-fn f) d))))))
  (testing "4.6 bands have no inverse"
    (is (= :kind (:key (error #(scale/inverse-fn (scale/finalize (scale/scale-spec :x {:domain ["a" "b"]}) [0.0 100.0] 5))))))))

(deftest function-values-test
  (testing "4.7 nil and non-finite results break the curve"
    (let [p (fn-plot {:f (fn [x] (when-not (< 1.0 x 2.0) 1.0)) :domain [0 3] :color [0 0 255] :line-width 3} {:scales {:y {:domain [0 2]}}})
          {:keys [x y w h]} (panel-box p)
          img (plot/render-image p size)
          row (+ y (* 0.5 h))]
      (is (not (white? img (+ x (* (/ 0.65 3.3) w)) row)))
      (is (white? img (+ x (* (/ 1.65 3.3) w)) row))))
  (testing "4.7 exceptions from f propagate"
    (is (= "boom" (try (plot/render-image (fn-plot {:f (fn [_] (throw (ex-info "boom" {}))) :domain [0 1]}) size)
                       (catch Exception e (ex-message e)))))))

(deftest y-extent-test
  (let [ey (fn [opts & [extra]] (get-in (fn-stats (merge {:f m/tan :domain [-5 5]} opts) extra) [:extents facet/all-panels :y]))]
    (testing "4.9 default 1 % / 99 % quantiles keep tan readable"
      (let [{:keys [min max]} (ey {})]
        (is (< 20.0 (double max) 200.0))
        (is (< -200.0 (double min) -20.0))))
    (testing "4.9 :min-max, custom quantiles, :none"
      (is (< (double (:max (ey {}))) (double (:max (ey {:y-extent :min-max})))))
      (is (< (double (:max (ey {:y-extent {:quantiles [0.25 0.75]}}))) 5.0))
      (is (nil? (ey {:y-extent :none}))))
    (testing "4.8 :y-limits: min / max of the samples within"
      (let [{:keys [min max]} (ey {:y-limits [-10 10]})]
        (is (<= -10.0 (double min)))
        (is (>= 10.0 (double max)))
        (is (< 9.0 (double max)))))
    (testing "4.9 an explicit y domain: no y extent"
      (is (nil? (ey {} {:scales {:y {:domain [-3 3]}}}))))
    (testing "4.9 :none takes another chart's y"
      (is (some? (plot/render-image {:data pts :mapping {:x :x :y :y}
                                     :charts [(chart/scatter) (chart/function {:f m/tan :y-extent :none})]} size))))
    (testing "4.8 / 4.9 invalid values"
      (is (= :y-limits (:key (error #(build (fn-plot {:f m/sin :domain [0 1] :y-limits [1 1]}))))))
      (is (= :y-extent (:key (error #(build (fn-plot {:f m/sin :domain [0 1] :y-extent {:quantiles [0.5 1.5]}})))))))))

(deftest asymptote-test
  (let [cut @#'function/cut-pieces]
    (testing "4.10 a jump farther than the panel cuts; each piece runs out of the panel"
      (let [pieces (cut [[0.0 50.0] [1.0 60.0] [2.0 90.0] [3.0 -500.0] [4.0 -450.0] [5.0 -440.0]] 100.0 100.0)]
        (is (= 2 (count pieces)))
        (is (= [2.0 90.0] (nth (first pieces) 2)))
        (is (> (double (second (peek (first pieces)))) 100.0) "stub beyond the panel")
        (is (< (double (second (first (second pieces)))) 0.0) "the next piece starts beyond the panel")))
    (testing "4.10 no jump, no cut; NaN points stay"
      (is (= 1 (count (cut [[0.0 1.0] [1.0 ##NaN] [2.0 3.0]] 100.0 100.0))))))
  (testing "4.10 tan: no vertical line at an asymptote"
    (let [p (fn-plot {:f m/tan :domain [0 3] :color [0 0 255]})
          {:keys [x y w h]} (panel-box p)
          img (plot/render-image p size)
          ;; x = pi/2 within [0 3] padded by 5 %
          col (+ x (* (/ (+ 0.15 m/HALF_PI) 3.3) w))]
      (is (every? #(white? img col %) (range (+ y (* 0.4 h)) (+ y (* 0.6 h))))))))

(deftest function-style-test
  (testing "4.11 theme [:chart :function] and options"
    (is (= [:charts 0 :options :bridge :alpha-factor] (:in (error #(build (fn-plot {:f m/sin :domain [0 1] :bridge {:alpha-factor 2}}))))))
    (is (some? (build (fn-plot {:f m/sin :domain [0 1]} {:theme {:chart {:function {:color :red :line-width 2}}}})))))
  (testing "4.12 legend glyph with dash"
    (let [b (build (fn-plot {:f [m/sin m/cos] :domain [0 3] :mapping {:dash :function}}))]
      (is (= [:solid :dashed] (mapv #(get-in % [:attributes :dash]) (:entries (first (legends b))))))
      (is (some? (plot/render-image (fn-plot {:f [m/sin m/cos] :domain [0 3] :mapping {:dash :function} :fill [0 0 255 60]}) size))))))

(deftest function-orientation-test
  (testing "4.13 flipped: x runs vertically"
    (let [p (fn-plot {:f (fn [x] x) :domain [0 1] :color [0 0 255] :line-width 3} {:flip true})
          b (build p)]
      (is (= :vertical (scale/direction :x (get-in b [:scales :x] {:direction :vertical}))))
      (is (some? (plot/render-image p size)))))
  (testing "4.13 log x: samples even in pixels - log on a log scale is straight"
    (let [p (fn-plot {:f m/log :domain [1 1000] :color [0 0 255] :line-width 3 :y-extent :min-max} {:scales {:x {:kind :log}}})
          {:keys [x y w h]} (panel-box p)
          img (plot/render-image p size)
          ;; the straight line from (1, 0) to (1000, ln 1000) passes the panel centre region
          cx (+ x (* 0.5 w)) cy (+ y (* 0.5 h))]
      (is (some #(not (white? img cx %)) (range (- cy 3) (+ cy 4)))))))

(deftest function-fill-test
  (testing "4.14.1 / 4.14.2 :f fills to y = 0"
    (let [p (fn-plot {:f (fn [_] 1.0) :domain [0 1] :fill [255 0 0] :color :none} {:scales {:y {:domain [-1 2]}}})
          {:keys [x y w h]} (panel-box p)
          img (plot/render-image p size)]
      (is (= [255 0 0] (rgb img (+ x (* 0.5 w)) (+ y (* (/ (- 2.15 0.5) 3.3) h)))) "between 0 and 1")
      (is (white? img (+ x (* 0.5 w)) (+ y (* (/ (- 2.15 -0.5) 3.3) h))) "below 0")))
  (testing "4.14.3 cuts and gaps leave no area"
    (let [p (fn-plot {:f m/tan :domain [0 3] :fill [255 0 0] :color :none})
          {:keys [x y w h]} (panel-box p)
          img (plot/render-image p size)
          ;; just right of pi/2 tan is very negative: nothing filled above the baseline there
          col (+ x (* (/ (+ 0.15 m/HALF_PI 0.1) 3.3) w))]
      (is (every? #(white? img col %) (range (+ y 2) (+ y (* 0.3 h)))))))
  (testing "4.14.4 parametric: the closed polygon"
    (let [p (fn-plot {:parametric (fn [t] [(m/cos t) (m/sin t)]) :t [0 m/TWO_PI] :fill [255 0 0] :color :none})
          {:keys [x y w h]} (panel-box p)
          img (plot/render-image p size)]
      (is (= [255 0 0] (rgb img (+ x (* 0.5 w)) (+ y (* 0.5 h)))))))
  (testing "4.14.5 legend glyph shows the fill"
    (let [b (build (fn-plot {:f [m/sin m/cos] :domain [0 3] :mapping {:color :function} :fill [0 255 0]}))]
      (is (= 2 (count (:entries (first (legends b)))))))))

(deftest area-baseline-test
  (testing "4.14.2 baseline: y = 0 clamped into the panel, the range start without 0"
    (let [ys (fn [d] (scale/finalize (scale/scale-spec :y {:domain d :nice false}) [100.0 0.0] 5))]
      (is (= 50.0 (curve/area-baseline (ys [-1.0 1.0]) 200.0 100.0)))
      (is (= 100.0 (curve/area-baseline (ys [1.0 2.0]) 200.0 100.0)))
      (is (= 0.0 (curve/area-baseline (ys [-2.0 -1.0]) 200.0 100.0))))))

(deftest schema-dash-test
  (testing "dash? vocabulary"
    (is (every? schema/dash? [:none :solid :dashed [1] [1.5 2]]))
    (is (not-any? schema/dash? [nil :wavy [] [0] [-1 2] "dashed"]))))

;; review fixes

(deftest dash-set-test
  (testing "F2 every named pattern shows a gap within a 14 px legend key"
    (doseq [[k lengths] schema/dash-styles :when (vector? lengths)]
      (is (< (double (first lengths)) 14.0) (str k " starts with a dash shorter than the key"))
      (is (<= (+ (double (first lengths)) (double (second lengths))) 14.0) (str k " has its first gap inside the key"))))
  (testing "F2 the default palette: the named styles, all different"
    (let [palette (get-in (build {:data grouped :mapping {:x :x :y :y} :charts [(chart/line)]}) [:theme :palette :dash])]
      (is (every? #(contains? schema/dash-styles %) palette))
      (is (apply distinct? palette))
      (is (= :solid (first palette))))))

(deftest gradient-dash-test
  (let [data (for [i (range 8)] {:x i :y (m/sin i)})
        p (fn [opts] {:data data :mapping {:x :x :y :y :color :y} :theme white :charts [(chart/line (merge {:line-width 2} opts))]})]
    (testing "F1 a dash shows on an interpolated gradient line"
      (is (not (same-image? (p {:interpolator :pchip}) (p {:interpolator :pchip :dash :dashed})))))
    (testing "F1 the dash runs on across segments of an uncurved gradient line"
      (is (not (same-image? (p {}) (p {:dash :long-dash})))))
    (testing "F3 sampled segments share colors in chunks of a few pixels"
      (let [chunk-colors @#'line/chunk-colors
            points (mapv (fn [i] [(double i) 0.0]) (range 21))
            color-scale (aesthetic/scale-spec :color {:domain [0 20] :range [:black :white]} nil false {})
            colors (chunk-colors points [0.0 20.0] [0.0 20.0] 0 color-scale {:palette {:na :grey}})]
        (is (= 20 (count colors)))
        (is (= 5 (count (partition-by identity colors))) "20 px in 4 px chunks")))))

(deftest lerp-walk-test
  (let [lerp @#'line/lerp-values]
    (testing "F8 one walk: ascending, descending, outside the ends, repeated ts"
      (is (= [-5.0 0.0 5.0 5.0 15.0 25.0] (lerp [0.0 10.0 20.0] [0.0 10.0 20.0] [-5.0 0.0 5.0 5.0 15.0 25.0])))
      (is (= [25.0 15.0 5.0] (lerp [20.0 10.0 0.0] [20.0 10.0 0.0] [25.0 15.0 5.0])))
      (is (= [] (lerp [0.0 1.0] [0.0 1.0] []))))))

(deftest function-bands-test
  (testing "F7 a categorical x scale: {:key :f}"
    (let [e (error #(plot/render-image {:data [{:x "a" :y 1} {:x "b" :y 2}] :mapping {:x :x :y :y}
                                        :charts [(chart/scatter) (chart/function {:f m/sin})]} size))]
      (is (= :f (:key e)))
      (is (= :bands (:value e))))))

(deftest key-glyph-test
  (testing "F6 line and function legends show the fill; without it a line"
    (let [glyph (fn [opts] (let [b (build {:data grouped :mapping {:x :x :y :y :color :s} :theme white
                                            :charts [(chart/line opts)]})]
                             (some? b)))]
      (is (glyph {:fill [255 0 0]}))
      (is (not (same-image? {:data grouped :mapping {:x :x :y :y :color :s} :theme white :charts [(chart/line {})]}
                            {:data grouped :mapping {:x :x :y :y :color :s} :theme white :charts [(chart/line {:fill [255 0 0 0]})]}))
          "a transparent fill still changes the legend glyph (square outline)"))))
