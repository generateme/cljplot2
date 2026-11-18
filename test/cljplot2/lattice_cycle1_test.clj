(ns cljplot2.lattice-cycle1-test
  "Lattice cycle 1 (trellis layout): panel-local scales, free / sliced facet scales, `:strips false`, panel weights,
  `:order-by` / `:descending` of facet and categorical position scales, `:formatter :power`, `:minor-ticks`,
  `plot/acf` with a free y scale."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.stats :as stats]
            [cljplot2.chart :as chart]
            [cljplot2.content.axis :as axis]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.layout.plot-grid :as plot-grid]
            [cljplot2.theme :as theme]
            [cljplot2.core :as plot]
            [cljplot2.data.levels :as levels]
            [cljplot2.layout.grid :as grid]
            [cljplot2.scale.position :as scale]
            [cljplot2.series-plots :as series-plots]
            [cljplot2.test-util :refer [error]])
  (:import [java.awt.image BufferedImage]))

(defn- built [p] (plot/build p {:width 600 :height 500}))
(defn- y-domain [b k] (get-in b [:panel-scales k :y :domain]))
(defn- x-domain [b k] (get-in b [:panel-scales k :x :domain]))
(defn- span ^double [[a b]] (- (double b) (double a)))
(defn- near? [^double a ^double b ^double eps] (< (m/abs (- a b)) eps))
(defn- items [b] (:items (:grid b)))
(defn- contents [b kind] (filter #(= kind (get-in % [:content :kind])) (items b)))
(defn- panel-boxes
  "Panel key -> box of the built plot `b`."
  [b]
  (into {} (map (fn [{:keys [content box]}] [(:panel content) box]))
        (grid/content-items (:grid b) (:layout b) #(and (= :panel (:kind %)) (nil? (:margin %))))))

(def ^:private groups
  "Four groups: x around 10·i (width i + 1), y up to 10^i."
  (vec (for [[i g] (map-indexed vector ["a" "b" "c" "d"]) j (range 20)]
         {:g g :h (if (< i 2) "top" "bottom") :x (+ (* 10.0 i) (* (inc i) (/ j 19.0))) :y (* (m/pow 10.0 i) (/ (inc j) 20.0))})))

(defn- scatter-plot [extra] (merge {:data groups :mapping {:x :x :y :y :facet :g} :charts [(chart/scatter)]} extra))

;; 1 panel-local scales

(deftest panel-scales-test
  (testing "1.1 free y: panels with their own y domains; none without panel options"
    (let [b (built (scatter-plot {:facets {:scales {:y :free}}}))]
      (is (= #{"a" "b" "c" "d"} (set (keys (:panel-scales b)))))
      (is (not= (y-domain b "a") (y-domain b "d")))
      (is (< (second (y-domain b "a")) 2.0))
      (is (> (second (y-domain b "d")) 900.0)))
    (is (nil? (:panel-scales (built (scatter-plot {}))))))
  (testing "1.2 one resolver: a panel's scales from :panel-scales, else the shared ones"
    (let [env {:scales {:y :shared} :panel-scales {"a" {:y :local}}}]
      (is (= {:y :local} (scale/panel-scales-of env {:panel "a"})))
      (is (= {:y :shared} (scale/panel-scales-of env {:panel "b"})) "a panel without an entry")
      (is (= {:y :shared} (scale/panel-scales-of env {})) "no :panel key")
      (is (= {:y :na} (scale/panel-scales-of (assoc-in env [:panel-scales nil] {:y :na}) {:panel nil})) "the NA panel")
      (is (= {:y :shared} (scale/panel-scales-of (assoc-in env [:panel-scales nil] {:y :na}) {})) "no key is not the NA panel")))
  (testing "1.2 every axis item names its panel; free axes match their panel's ticks"
    (let [b (built (scatter-plot {:facets {:scales {:y :free}}}))
          axes (filter #(= :y (get-in % [:content :scale])) (contents b :axis))]
      (is (= 4 (count axes)))
      (is (= #{"a" "b" "c" "d"} (set (map #(get-in % [:content :panel]) axes))))))
  (testing "1.4 a panel twice as tall gets at least as many ticks"
    (let [b (built (scatter-plot {:facets {:heights [2 1]}}))
          ticks #(count (get-in b [:panel-scales % :y :ticks]))]
      (is (>= (ticks "a") (ticks "c")))
      (is (> (ticks "a") 2)))))

;; 2 free and sliced scales

(deftest free-scales-test
  (testing "2.2 free x: each panel its own x domain (around its data)"
    (let [b (built (scatter-plot {:facets {:scales {:x :free}}}))]
      (doseq [[k lo] [["a" 0.0] ["b" 10.0] ["c" 20.0] ["d" 30.0]]]
        (is (<= (- lo 1.0) (first (x-domain b k)) lo))
        (is (< (second (x-domain b k)) (+ lo 6.0))))))
  (testing "2.2 a free scale with an explicit :domain throws {:key :facets}"
    (is (= :facets (:key (error #(built (scatter-plot {:facets {:scales {:y :free}} :scales {:y {:domain [0 10]}}})))))))
  (testing "2.3 sliced: equal spans, own centres"
    (let [b (built (scatter-plot {:facets {:scales {:x :sliced}}}))
          spans (map #(span (x-domain b %)) ["a" "b" "c" "d"])
          mids (map #(let [[lo hi] (x-domain b %)] (* 0.5 (+ (double lo) (double hi)))) ["a" "b" "c" "d"])]
      (is (every? #(near? (first spans) % 1e-9) spans))
      (is (>= (first spans) 4.0))
      (is (= 4 (count (distinct mids))))))
  (testing "2.3 sliced log scale: equal spans in log space"
    (let [b (built (scatter-plot {:facets {:scales {:y :sliced}} :scales {:y {:kind :log}}}))
          log-span (fn [k] (let [[lo hi] (y-domain b k)] (- (m/log10 hi) (m/log10 lo))))]
      (is (near? (log-span "a") (log-span "d") 1e-9))))
  (testing "2.3 sliced panels of single values get the span of all data"
    (let [rows (for [g ["a" "b"] i (range 3)] {:g g :x (if (= g "a") 1.0 5.0) :y i})
          b (built {:data rows :mapping {:x :x :y :y :facet :g} :charts [(chart/scatter)] :facets {:scales {:x :sliced}}})]
      (is (near? (span (x-domain b "a")) (span (x-domain b "b")) 1e-9))
      (is (pos? (span (x-domain b "a"))))))
  (testing "2.4 free categorical: only the panel's levels, in the scale's order (first appearance)"
    (let [rows [{:f "z" :v 1 :g "p"} {:f "a" :v 2 :g "p"} {:f "m" :v 3 :g "q"} {:f "a" :v 4 :g "q"}]
          b (built {:data rows :mapping {:x :v :y :f :facet :g} :charts [(chart/scatter)] :facets {:scales {:y :free}}})]
      (is (= ["z" "a" "m"] (vec (get-in b [:scale-specs :y :domain]))))
      (is (= ["z" "a"] (vec (y-domain b "p"))))
      (is (= ["a" "m"] (vec (y-domain b "q"))))))
  (testing "2.4.1 sliced categorical and sliced dates throw {:key :facets}"
    (is (= :facets (:key (error #(built {:data [{:f "a" :v 1 :g "p"}] :mapping {:x :v :y :f :facet :g}
                                         :charts [(chart/scatter)] :facets {:scales {:y :sliced}}})))))
    (is (= :facets (:key (error #(built {:data [{:d (java.time.LocalDate/of 2020 1 1) :v 1 :g "p"}
                                                {:d (java.time.LocalDate/of 2020 2 1) :v 2 :g "q"}]
                                         :mapping {:x :d :y :v :facet :g} :charts [(chart/scatter)]
                                         :facets {:scales {:x :sliced}}}))))))
  (testing "2.5 wrap: an axis at every panel in its own track"
    (let [b (built (scatter-plot {:facets {:scales {:x :free :y :free}}}))
          ids (set (map :id (contents b :axis)))]
      (is (every? ids (for [r [0 1] c [0 1]] [:x-axis r c])))
      (is (every? ids (for [r [0 1] c [0 1]] [:y-axis r c])))
      (is (some #{[:y-axis 1]} (map :id (:columns (:grid b)))))))
  (testing "2.6 grid: free x shared down a column, differing across columns; axes compare the next panel's labels"
    (let [b (built {:data groups :mapping {:x :x :y :y :facet-col :g :facet-row :h} :charts [(chart/scatter)]
                    :facets {:scales {:x :free}}})]
      (is (= (x-domain b ["a" "top"]) (x-domain b ["a" "bottom"])))
      (is (not= (x-domain b ["a" "top"]) (x-domain b ["d" "top"])))
      (is (= {:next-panel-gap 5 :next-panel ["c" "top"]}
             (select-keys (:content (first (filter #(= [:x-axis 1] (:id %)) (items b)))) [:next-panel-gap :next-panel])))
      (is (nil? (get-in (first (filter #(= [:x-axis 3] (:id %)) (items b))) [:content :next-panel-gap])) "last column")))
  (testing "2.6 grid: sliced x - equal spans, shared down a column"
    (let [b (built {:data groups :mapping {:x :x :y :y :facet-col :g :facet-row :h} :charts [(chart/scatter)]
                    :facets {:scales {:x :sliced}}})]
      (is (= (x-domain b ["b" "top"]) (x-domain b ["b" "bottom"])))
      (is (near? (span (x-domain b ["a" "top"])) (span (x-domain b ["d" "bottom"])) 1e-9))))
  (testing "2.7 rejected combinations throw {:key :facets}"
    (doseq [p [{:data groups :mapping {:x :x :y :y} :charts [(chart/scatter)] :facets {:scales {:y :free}}}
               {:data groups :mapping {:x :x :y :y} :charts [(chart/scatter)] :facets {:widths [1]}}
               (scatter-plot {:facets {:scales {:y :free}} :aspect :iso})
               (scatter-plot {:facets {:scales {:y :free}} :coord {:kind :polar}})]]
      (is (= :facets (:key (error #(built p)))))))
  (testing "2.1 an unknown relation is a schema error"
    (is (= [:facets :scales :x] (:in (error #(built (scatter-plot {:facets {:scales {:x :loose}}}))))))
    (is (some? (built (scatter-plot {:facets {:scales {:x :same :y :same}}})))))
  (testing "2.7.1 flip: the free :x scale is drawn vertically, per panel"
    (let [b (built (scatter-plot {:facets {:scales {:x :free}} :flip true}))
          x-axes (filter #(= :x (get-in % [:content :scale])) (contents b :axis))]
      (is (= 4 (count x-axes)))
      (is (every? #(= :y-axis (first (:id %))) x-axes))
      (is (= :vertical (get-in b [:panel-scales "a" :x :direction])))))
  (testing "2.8 aesthetic scales and the legend stay shared"
    (let [b (built (scatter-plot {:mapping {:x :x :y :y :facet :g :color :y} :facets {:scales {:y :free}}}))]
      (is (= 1 (count (contents b :legends))))
      (is (= (get-in b [:finalized-scales :color]) (get-in b [:panel-scales "a" :color]))))))

;; 3 facet layout and level order

(deftest facet-layout-test
  (testing "3.1 :strips false - no strips, wrap or grid"
    (is (empty? (contents (built (scatter-plot {:facets {:strips false}})) :strip)))
    (is (empty? (contents (built {:data groups :mapping {:x :x :y :y :facet-col :g :facet-row :h} :charts [(chart/scatter)]
                                  :facets {:strips false}})
                          :strip)))
    (is (seq (contents (built (scatter-plot {})) :strip))))
  (testing "3.2 panel heights in the ratio of :heights (within 1 px)"
    (let [b (built {:data groups :mapping {:x :x :y :y :facet :g} :charts [(chart/scatter)]
                    :facets {:columns 1 :heights [16 13 9 12]}})
          hs (map #(double (:h ((panel-boxes b) %))) ["a" "b" "c" "d"])
          unit (/ (reduce + hs) 50.0)]
      (doseq [[h w] (map vector hs [16 13 9 12])]
        (is (near? h (* unit w) 1.0)))))
  (testing "3.2 a weight count not matching the layout throws"
    (is (= :heights (:key (error #(built (scatter-plot {:facets {:heights [1 2 3]}}))))))
    (is (= :widths (:key (error #(built (scatter-plot {:facets {:widths [1]}})))))))
  (testing "3.3 facet :order-by: levels ascending by a statistic of their rows; :descending"
    (let [levels-of #(get-in (built (scatter-plot {:scales {:facet %}})) [:split :facets :levels :facet])]
      (is (= ["a" "b" "c" "d"] (levels-of {:order-by #(stats/mean (map :y %))})))
      (is (= ["d" "c" "b" "a"] (levels-of {:order-by #(stats/mean (map :y %)) :descending true})))
      (is (= ["d" "c" "b" "a"] (levels-of {:order-by #(- (stats/mean (map :x %)))})))))
  (testing "3.3 the NA level stays last"
    (let [rows (conj groups {:g nil :x 1.0 :y 1.0})
          b (built {:data rows :mapping {:x :x :y :y :facet :g} :charts [(chart/scatter)]
                    :scales {:facet {:order-by #(stats/mean (map :y %)) :descending true}}})]
      (is (= ["d" "c" "b" "a" nil] (get-in b [:split :facets :levels :facet])))))
  (testing "3.3 / 3.5 option rules"
    (is (= :order-by (:key (error #(built (scatter-plot {:scales {:facet {:order-by count :domain ["a"]}}}))))))
    (is (= :descending (:key (error #(built (scatter-plot {:scales {:facet {:descending true}}})))))))
  (testing "3.4 categorical position :order-by - global, and per panel under :free"
    (let [rows (for [[g vs] {"p" {"u" 1 "v" 2 "w" 3} "q" {"u" 3 "v" 2 "w" 1}} [f v] vs] {:g g :f f :v v})
          by-v {:order-by #(stats/mean (map :v %))}
          shared (built {:data rows :mapping {:x :v :y :f :facet :g} :charts [(chart/scatter)]
                         :scales {:y (assoc by-v :order-by #(reduce + (map :v %)))}})
          free (built {:data rows :mapping {:x :v :y :f :facet :g} :charts [(chart/scatter)]
                       :facets {:scales {:y :free}} :scales {:y by-v}})
          desc (built {:data rows :mapping {:x :v :y :f :facet :g} :charts [(chart/scatter)]
                       :facets {:scales {:y :free}} :scales {:y (assoc by-v :descending true)}})]
      (is (= 3 (count (get-in shared [:scale-specs :y :domain]))))
      (is (= ["u" "v" "w"] (vec (y-domain free "p"))))
      (is (= ["w" "v" "u"] (vec (y-domain free "q"))))
      (is (= ["w" "v" "u"] (vec (y-domain desc "p"))))))
  (testing "3.4 position :order-by rules: not with :domain, only on categorical scales"
    (is (= :order-by (:key (error #(built (scatter-plot {:scales {:y {:order-by count :domain [0 1]}}}))))))
    (is (= :order-by (:key (error #(built (scatter-plot {:scales {:y {:order-by count}}}))))))
    (is (= :descending (:key (error #(built (scatter-plot {:scales {:y {:descending true}}}))))))))

(deftest review-fixes-test
  (testing "F2 a label is skipped only where it meets the next panel's drawn label (the next panel keeps its own)"
    (let [own [[0.0 50.0 100.0] [10.0 10.0 10.0]]]
      (is (= #{2} (axis/crossing-labels own [0 1 2] [[0.0 50.0] [10.0 10.0]] [0 1] 105.0)) "100 meets the next 0")
      (is (= #{} (axis/crossing-labels own [0 1 2] [[20.0 50.0] [10.0 10.0]] [0 1] 105.0)) "the next panel starts later")
      (is (= #{} (axis/crossing-labels own [0 1] [[0.0 50.0] [10.0 10.0]] [0 1] 105.0)) "an undrawn label meets nothing")
      (is (= #{2} (axis/crossing-labels own [0 1 2] 105.0)) "identical next axis")))
  (testing "F2 vertical neighbours compared only without strips or axis rows between the panel rows"
    (let [v-next (fn [p] (keep #(get-in % [:content :next-panel]) (filter #(= :y (get-in % [:content :scale])) (contents (built p) :axis))))]
      (is (empty? (v-next (scatter-plot {:facets {:scales {:y :free}}}))))
      (is (seq (v-next (scatter-plot {:facets {:scales {:y :free} :strips false}}))))
      (is (empty? (v-next (scatter-plot {:facets {:scales {:x :free :y :free} :strips false}}))))))
  (testing "F3 per-panel :order-by also scores levels of data drawn in every panel"
    (let [rows [{:g "p" :f "u" :v 1} {:g "p" :f "v" :v 2}]
          b (built {:data rows :mapping {:x :v :y :f :facet :g}
                    :charts [(chart/scatter) (chart/scatter {:data [{:f "w" :v 0}] :mapping {:facet nil}})]
                    :facets {:scales {:y :free}} :scales {:y {:order-by #(stats/mean (map :v %))}}})]
      (is (= ["w" "u" "v"] (vec (y-domain b "p"))))))
  (testing "G6 a minor tick equal to a major tick is left out, also at zero"
    (is (= [-1.0 1.0] (scale/minor-tick-values :linear [-2 2] [-2 0 2] 2 10)))
    (is (not-any? zero? (scale/minor-tick-values :linear [-1 1] [-1 0 1] 2 10))))
  (testing "F4 log minor ticks near a tiny major tick are kept"
    (let [vs (scale/minor-tick-values :log [1e-9 1e3] [1e-9 1e-6 1e-3 1 1e3] 1 10)]
      (is (some #(near? 2e-9 % 1e-18) vs))
      (is (= 96 (count vs)))))
  (testing "F5 the panel-size estimate leaves room for the axes of a free wrap layout"
    (let [plot {:options {:width 600 :height 500 :hint :high} :theme theme/default-theme}
          [w h] (plot-grid/estimated-panel-size plot [] nil {:columns 2 :rows 2} true false)
          [fw fh] (plot-grid/estimated-panel-size (assoc plot :facets {:scales {:x :free :y :free}}) [] nil {:columns 2 :rows 2} true false)
          grid (plot-grid/estimated-panel-size plot [] nil {:columns 2 :rows 2} true true)
          free-grid (plot-grid/estimated-panel-size (assoc plot :facets {:scales {:x :free :y :free}}) [] nil {:columns 2 :rows 2} true true)]
      (is (< fw w))
      (is (< fh h))
      (is (= grid free-grid) "grid layouts keep their outer axes")))
  (testing "F6 the x start overhang shares the first y-axis column when y is free"
    (let [b (built (scatter-plot {:facets {:scales {:x :free :y :free}}}))
          starts (filter #(and (vector? (:id %)) (= :x-axis-start (first (:id %)))) (items b))]
      (is (= 2 (count starts)))
      (is (every? #(= [:y-axis 0] (:column %)) starts))))
  (testing "F7 a longer minor tick widens the axis track"
    (let [p {:data [{:x 0 :y 0} {:x 10 :y 1}] :mapping {:x :x :y :y} :charts [(chart/scatter)] :scales {:x {:minor-ticks 2}}}
          axis-height (fn [p] (let [b (built p)]
                                (measure/call-with-scratch-canvas
                                 :high (fn [canvas] (.y ^fastmath.vector.Vec2
                                                     (measure/natural-size {:kind :axis :scale :x}
                                                                           {:canvas canvas :theme (:theme b) :scales (:finalized-scales b)}))))))]
      (is (< (axis-height p) (axis-height (assoc p :theme {:axis {:x {:minor-tick-length 20}}}))))
      (is (= (axis-height (assoc p :theme {:axis {:x {:minor-tick-length 2}}})) (axis-height (dissoc p :scales)))))))

(deftest na-panel-test
  (testing "G1 the NA facet panel (key nil) keeps its own scales, axis and neighbour checks"
    (let [rows (concat (for [i (range 5)] {:g "a" :x i :y (* 100 i)}) (for [i (range 5)] {:g nil :x (* 100 i) :y i}))
          free (built {:data rows :mapping {:x :x :y :y :facet :g} :charts [(chart/scatter)] :facets {:scales {:x :free :y :free}}})
          axes (contents free :axis)
          na-y (first (filter #(and (= :y (get-in % [:content :scale])) (contains? (:content %) :panel) (nil? (get-in % [:content :panel]))) axes))]
      (is (< (second (y-domain free nil)) 5.0))
      (is (> (second (y-domain free "a")) 300.0))
      (is (some? na-y) "the NA panel's y axis names it")
      (is (= (y-domain free nil) (get-in (scale/panel-scales-of free (:content na-y)) [:y :domain])))
      (is (some #(and (= "a" (get-in % [:content :panel])) (contains? (:content %) :next-panel) (nil? (get-in % [:content :next-panel])))
                (filter #(= :x (get-in % [:content :scale])) axes))
          "the axis before the NA panel compares its labels")))
  (testing "G1 weighted panels with an NA level"
    (let [rows (concat (for [i (range 5)] {:g "a" :x i :y i}) (for [i (range 5)] {:g nil :x i :y i}))
          b (built {:data rows :mapping {:x :x :y :y :facet :g} :charts [(chart/scatter)] :facets {:widths [2 1]}})]
      (is (contains? (:panel-scales b) nil))
      (is (> (double (:w ((panel-boxes b) "a"))) (* 1.5 (double (:w ((panel-boxes b) nil)))))))))

(deftest order-by-score-test
  (testing "ties keep the given order; unscored levels follow, sorted"
    (is (= ["b" "a" "c" "d" "e"] (levels/order-by-score ["e" "b" "a" "c" "d"] {"a" 1 "b" 1 "c" 2 "d" nil "e" ##NaN} false)))
    (is (= ["c" "b" "a"] (levels/order-by-score ["a" "b" "c"] {"a" 1 "b" 2 "c" 3} true)))
    (is (= [] (levels/order-by-score [] {} false))))
  (testing "levels without rows aren't scored"
    (is (= ["b" "a"] (levels/order-by-rows ["a" "b"] {"b" [{:v 1}]} #(count %) false)))))

;; 4 axis labels and ticks

(deftest power-formatter-test
  (testing "4.1 base^k in superscripts; other ticks as numbers"
    (is (= "2⁵" (scale/power-label 2 32)))
    (is (= "10⁻³" (scale/power-label 10 0.001)))
    (is (= "10⁰" (scale/power-label 10 1)))
    (is (= "3" (scale/power-label 2 3)))
    (is (= "0.5" (scale/power-label 10 0.5))))
  (testing "4.1 a log base-2 axis labels its ticks 2^k"
    (let [b (built {:data [{:x 2 :y 1} {:x 256 :y 2}] :mapping {:x :x :y :y} :charts [(chart/scatter)]
                    :scales {:x {:kind :log :base 2 :formatter :power}}})]
      (is (every? #(re-matches #"2[⁰¹²³⁴⁵⁶⁷⁸⁹]+" %) (get-in b [:finalized-scales :x :tick-labels])))))
  (testing "4.1 :power off a log scale throws {:key :formatter}"
    (is (= :formatter (:key (error #(built (scatter-plot {:scales {:x {:formatter :power}}}))))))))

(deftest minor-ticks-test
  (testing "4.2 linear: n subdivisions, outer intervals repeated inside the domain"
    (is (= [1.0 3.0 5.0 7.0 9.0] (scale/minor-tick-values :linear [0 10] [0 2 4 6 8 10] 2 10)))
    (is (= [-1.0 1.0 2.0 3.0 4.0 6.0 7.0 8.0 9.0 11.0] (scale/minor-tick-values :linear [-1 11] [0 5 10] 5 10)))
    (is (= [] (scale/minor-tick-values :linear [0 10] [0 5 10] 1 10)))
    (is (= [] (scale/minor-tick-values :linear [0 1] [0.5] 2 10))))
  (testing "4.2 log: mantissas 2..9 of every decade; none for base 2"
    (is (= 16 (count (scale/minor-tick-values :log [1 100] [1 10 100] 1 10))))
    (is (= [] (scale/minor-tick-values :log [1 64] [1 2 4 8 16 32 64] 1 2))))
  (testing "4.2 finalized scales carry :minor-ticks only when asked"
    (let [b (built {:data [{:x 0 :y 0} {:x 10 :y 1}] :mapping {:x :x :y :y} :charts [(chart/scatter)]
                    :scales {:x {:minor-ticks 2}}})]
      (is (seq (get-in b [:finalized-scales :x :minor-ticks])))
      (is (nil? (get-in b [:finalized-scales :y :minor-ticks])))))
  (testing "4.2 :minor-ticks on bands or dates throws {:key :minor-ticks}"
    (is (= :minor-ticks (:key (error #(built {:data [{:x "a" :y 1}] :mapping {:x :x :y :y} :charts [(chart/scatter)]
                                               :scales {:x {:minor-ticks 2}}})))))
    (is (= :minor-ticks (:key (error #(built {:data [{:x (java.time.LocalDate/of 2020 1 1) :y 1}
                                                     {:x (java.time.LocalDate/of 2021 1 1) :y 2}]
                                              :mapping {:x :x :y :y} :charts [(chart/scatter)]
                                              :scales {:x {:minor-ticks 2}}})))))))

(defn- image ^BufferedImage [p] (c2d/get-image (plot/render p {:width 300 :height 200})))

(defn- same-pixels? [^BufferedImage a ^BufferedImage b]
  (every? true? (for [x (range (.getWidth a)) y (range (.getHeight a))] (= (.getRGB a x y) (.getRGB b x y)))))

(deftest minor-grid-test
  (testing "4.3 minor grid lines only with theme [:panel :grid :minor]"
    (let [p {:data [{:x 0 :y 0} {:x 10 :y 1}] :mapping {:x :x :y :y} :charts [(chart/scatter)] :scales {:x {:minor-ticks 4}}}
          plain (image (dissoc p :scales))]
      (is (not (same-pixels? plain (image p))) "minor axis ticks drawn")
      (is (not (same-pixels? (image p) (image (assoc p :theme {:panel {:grid {:minor {:color :red :width 1 :dash [1.0]}}}}))))))))

;; 5 builders

(deftest acf-free-y-test
  (testing "5.1 ACF and PACF rows with their own y domains"
    (let [values (map #(m/sin (* 0.3 (double %))) (range 120))
          b (built (series-plots/acf values {}))
          domains (map #(get-in % [:y :domain]) (vals (:panel-scales b)))]
      (is (= {:scales {:y :free}} (:facets (series-plots/acf values {}))))
      (is (nil? (:facets (series-plots/acf values {:partial? :acf}))))
      (is (nil? (:facets (series-plots/acf values {:scales {:y {:domain [-1 1]}}}))) "an explicit y domain stays shared")
      (is (= 2 (count (distinct domains)))))))
