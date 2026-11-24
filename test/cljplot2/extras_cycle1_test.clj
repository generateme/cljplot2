(ns cljplot2.extras-cycle1-test
  "cljplot extras cycle 1: Q-Q / P-P charts with distribution sides, ACF / lag rows and `plot/acf`, vector-field rows,
  field `:pixel-color` with `domain-color`, the free `chart/image`."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [clojure2d.pixels :as p]
            [fastmath.complex :as cx]
            [fastmath.core :as m]
            [fastmath.fields :as f]
            [fastmath.random :as r]
            [fastmath.stats :as stats]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.field :as field]
            [cljplot2.data.probability :as probability]
            [cljplot2.data.series :as series]
            [cljplot2.data.vector-field :as vf]
            [cljplot2.test-util :refer [error error-ins]])
  (:import [java.awt.image BufferedImage]))

(defn- built [p] (plot/build p {:width 400 :height 400}))
(defn- stats-of [p i] (nth (:stats (built p)) i))
(defn- groups-of [p] (mapcat :groups (vals (:data (stats-of p 0)))))
(defn- near? [^double a ^double b] (< (m/abs (- a b)) 1e-9))

(def ^:private sample
  (let [g (r/rng :mersenne 11)]
    (vec (for [i (range 60)] {:a (r/grandom g) :b (r/grandom g 1 2) :g (if (even? i) "e" "o")}))))

;; 1.1 distribution sides

(deftest distribution-sides-test
  (testing "1.1.1 [:name params] and a distribution object give the same points"
    (let [pts (fn [d] (first (groups-of {:data sample :mapping {:y :a} :charts [(chart/qq {:x-distribution d})]})))]
      (is (= (:x (pts [:normal {:mu 1}])) (:x (pts (r/distribution :normal {:mu 1})))))
      (is (= (:x (pts [:normal])) (:x (pts [:normal {}]))))))
  (testing "1.1.2 an unknown name - fastmath's exception; a plain function rejected by the schema"
    (is (thrown? IllegalArgumentException (built {:data sample :mapping {:y :a} :charts [(chart/qq {:x-distribution [:nope]})]})))
    (is (contains? (error-ins #(built {:data sample :mapping {:y :a} :charts [(chart/qq {:x-distribution (fn [p] p)})]}))
                   [:charts 0 :options :x-distribution]))
    (is (seq (error-ins #(built {:data sample :mapping {:y :a} :charts [(chart/qq {:x-distribution [:normal 1]})]})))))
  (testing "1.1.3 (R2) a distribution wins over the plot's mapping of its side; the chart's own mapping of it throws"
    (let [{:keys [x y]} (first (groups-of {:data sample :mapping {:x :a :y :b} :charts [(chart/qq {:x-distribution [:normal]})]}))]
      (is (= (vec (sort (map :b sample))) y))
      (is (near? (r/icdf (r/distribution :normal) (/ 0.5 60)) (first x))))
    (is (= :x-distribution (:key (error #(built {:data sample :charts [(chart/qq {:x-distribution [:normal] :mapping {:x :a :y :b}})]})))))
    (is (= :y-distribution (:key (error #(built {:data sample :mapping {:x :a} :charts [(chart/pp {:y-distribution [:normal] :mapping {:y :b}})]})))))
    (is (= {:key :mapping :in [:mapping :x]} (select-keys (error #(built {:data sample :mapping {:y :a} :charts [(chart/qq)]})) [:key :in])))
    (testing "- a scatter and a Q-Q of the same plot mapping"
      (is (= 2 (count (:stats (built {:data sample :mapping {:x :a :y :b} :charts [(chart/scatter) (chart/qq {:y-distribution [:normal]})]}))))))))

;; 1.2 Q-Q

(deftest qq-points-test
  (let [vs (mapv :a sample)
        sorted (vec (sort vs))
        n (count vs)
        d (r/distribution :normal)]
    (testing "1.2.1 data vs [:normal]: sorted values against icdf((i - 0.5) / n)"
      (let [{:keys [x y]} (first (groups-of {:data sample :mapping {:y :a} :charts [(chart/qq {:x-distribution [:normal]})]}))]
        (is (= n (count x) (count y)))
        (is (= sorted y))
        (is (near? (r/icdf d (/ 0.5 n)) (first x)))
        (is (near? (r/icdf d (/ (- n 0.5) n)) (peek x)))))
    (testing "1.2.1 the distribution on y: data on x"
      (let [{:keys [x y]} (first (groups-of {:data sample :mapping {:x :a} :charts [(chart/qq {:y-distribution [:normal]})]}))]
        (is (= sorted x))
        (is (near? (r/icdf d (/ 0.5 n)) (first y))))))
  (testing "1.2.2 unequal columns: n_min points, the larger interpolated (R approx)"
    (let [rows (concat (map (fn [i] {:a i :b (* 10 i)}) (range 5)) (map (fn [i] {:a i}) (range 5 9)))
          {:keys [x y]} (first (groups-of {:data rows :mapping {:x :a :y :b} :charts [(chart/qq)]}))]
      (is (= 5 (count x) (count y)))
      (is (= [0.0 10.0 20.0 30.0 40.0] y))
      (is (= [0.0 2.0 4.0 6.0 8.0] x) "9 values at 5 evenly spaced positions")))
  (testing "1.2.2 interpolated - fractional positions, one value, equal sizes"
    (is (= [0.0 1.5 3.0] (probability/interpolated [0 1 2 3] 3)))
    (is (= [7.0] (probability/interpolated [7 8 9] 1)))
    (is (= [1.0] (probability/interpolated [1] 1))))
  (testing "1.2.3 two distributions: :points positions (default 100)"
    (is (= 100 (count (:x (first (groups-of {:charts [(chart/qq {:x-distribution [:normal] :y-distribution [:normal]})]}))))))
    (is (= 7 (count (:x (first (groups-of {:charts [(chart/qq {:x-distribution [:normal] :y-distribution [:normal] :n 7})]}))))))
    (is (seq (error-ins #(built {:charts [(chart/qq {:x-distribution [:normal] :y-distribution [:normal] :n 1})]}))))))

(deftest qq-line-test
  (testing "1.2.4 the line through the quartile pairs (R type 7)"
    (let [rows (map (fn [i] {:a i :b (+ 3 (* 2 i))}) (range 1 11))
          [slope intercept] (:line (first (groups-of {:data rows :mapping {:x :a :y :b} :charts [(chart/qq)]})))
          [x1 x3] (stats/quantiles (range 1 11) [0.25 0.75] :r7)
          [y1 y3] (stats/quantiles (map #(+ 3 (* 2 %)) (range 1 11)) [0.25 0.75] :r7)]
      (is (near? (double slope) 2.0))
      (is (near? (+ (double intercept) (* (double slope) (double x1))) (double y1)))
      (is (near? (+ (double intercept) (* (double slope) (double x3))) (double y3)))))
  (testing "1.2.4 distribution sides: quartiles by icdf; :line false - none"
    (let [[slope intercept] (:line (first (groups-of {:charts [(chart/qq {:x-distribution [:normal] :y-distribution [:normal]})]})))]
      (is (near? 1.0 (double slope)))
      (is (near? 0.0 (double intercept))))
    (is (nil? (:line (first (groups-of {:data sample :mapping {:y :a} :charts [(chart/qq {:x-distribution [:normal] :line false})]}))))))
  (testing "1.2.4 equal x quartiles - no line"
    (is (nil? (probability/quartile-line {:values [1 1 1 1]} {:values [1 2 3 4]}))))
  (testing "1.2.4 the line adds no extents; a log axis throws {:key :line}"
    (let [rows (map (fn [i] {:a i :b i}) (range 1 11))
          e (get-in (stats-of {:data rows :mapping {:x :a :y :b} :charts [(chart/qq)]} 0) [:extents :panel :x])]
      (is (= [1.0 10.0] [(:min e) (:max e)]))
      (is (= :line (:key (error #(plot/render-image {:data rows :mapping {:x :a :y :b} :scales {:x {:kind :log}} :charts [(chart/qq)]}
                                                    {:width 200 :height 200})))))
      (is (instance? BufferedImage (plot/render-image {:data rows :mapping {:x :a :y :b} :scales {:x {:kind :log}} :charts [(chart/qq {:line false})]}
                                                      {:width 200 :height 200}))))))

(deftest qq-groups-test
  (testing "1.2.5 groups and facets get their own point sets; whole-group statistics"
    (let [gs (groups-of {:data sample :mapping {:x :a :y :b :color :g} :charts [(chart/qq)]})]
      (is (= #{{:color "e"} {:color "o"}} (set (map :aesthetics gs))))
      (is (every? #(= 30 (count (:x %))) gs)))
    (let [data (:data (stats-of {:data sample :mapping {:x :a :y :b :facet :g} :charts [(chart/qq)]} 0))]
      (is (= #{"e" "o"} (set (keys data))))
      (is (= (vec (sort (map :a (filter #(= "e" (:g %)) sample)))) (:x (first (:groups (data "e"))))))))
  (testing "1.2.3 two distributions in every panel of a faceted plot (facet keys only)"
    (let [s (stats-of {:data sample :mapping {:x :a :y :b :facet :g}
                       :charts [(chart/scatter) (chart/qq {:x-distribution [:normal] :y-distribution [:normal]})]} 1)]
      (is (= [:cljplot2.data.facet/all-panels] (keys (:data s))))
      (is (instance? BufferedImage (plot/render-image {:data sample :mapping {:facet :g}
                                                       :charts [(chart/qq {:x-distribution [:normal] :y-distribution [:t {:degrees-of-freedom 3}]})]}
                                                      {:width 400 :height 200}))))))

(deftest qq-titles-dropped-test
  (testing "1.2.6 axis titles"
    (is (= {:x "normal quantiles" :y "a"} (:axis-titles (stats-of {:data sample :mapping {:y :a} :charts [(chart/qq {:x-distribution [:normal]})]} 0))))
    (is (= {:x "Theoretical quantiles" :y "normal quantiles"}
           (:axis-titles (stats-of {:charts [(chart/qq {:x-distribution (r/distribution :normal) :y-distribution [:normal]})]} 0))))
    (is (= {:x "a"} (:axis-titles (stats-of {:data sample :mapping {:x :a :y (fn [row] (:b row))} :charts [(chart/qq)]} 0))) "a function accessor - none"))
  (testing "1.2.7 non-finite values dropped per side and counted; all invalid throws"
    (let [rows (conj sample {:a ##NaN :b 1.0} {:a nil :b nil})
          s (stats-of {:data rows :mapping {:x :a :y :b} :charts [(chart/qq)]} 0)]
      (is (= {:x 2 :y 1} (select-keys (:dropped s) [:x :y])))
      (is (= 60 (count (:x (first (:groups (:panel (:data s)))))))))
    (is (= :mapping (:key (error #(built {:data [{:a "x"} {:a nil}] :mapping {:y :a} :charts [(chart/qq {:x-distribution [:normal]})]})))))
    (testing "- a group with an empty side draws nothing"
      (is (= [{:color "o"}] (map :aesthetics (groups-of {:data [{:a 1.0 :b nil :g "e"} {:a 2.0 :b 3.0 :g "o"}]
                                                        :mapping {:x :a :y :b :color :g} :charts [(chart/qq)]})))
          "e has no y value"))))

;; 1.3 P-P

(deftest pp-test
  (testing "1.3.1 identical columns lie on y = x, at the pooled distinct values"
    (let [rows (map (fn [v] {:a v :b v}) [3 1 2 2 5])
          {:keys [x y]} (first (groups-of {:data rows :mapping {:x :a :y :b} :charts [(chart/pp)]}))]
      (is (= x y))
      (is (= [0.2 0.6 0.8 1.0] x) "ECDF at 1 2 3 5")))
  (testing "1.3.1 data vs a distribution: r/cdf at the data values"
    (let [{:keys [x y]} (first (groups-of {:data sample :mapping {:y :a} :charts [(chart/pp {:x-distribution [:normal]})]}))
          vs (sort (distinct (map :a sample)))]
      (is (= (mapv #(r/cdf (r/distribution :normal) %) vs) x))
      (is (near? 1.0 (peek y)))))
  (testing "1.3.2 two distributions need :domain; :points values over it"
    (is (= :domain (:key (error #(built {:charts [(chart/pp {:x-distribution [:normal] :y-distribution [:normal]})]})))))
    (is (= 11 (count (:x (first (groups-of {:charts [(chart/pp {:x-distribution [:normal] :y-distribution [:normal {:mu 1}] :domain [-3 3] :n 11})]}))))))
    (is (seq (error-ins #(built {:charts [(chart/pp {:x-distribution [:normal] :y-distribution [:normal] :domain [3 -3]})]})))))
  (testing "1.3.3 / 1.3.4 identity line by default, :line false; extents [0 1]"
    (let [s (stats-of {:data sample :mapping {:x :a :y :b} :charts [(chart/pp)]} 0)]
      (is (= [1.0 0.0] (:line (first (:groups (:panel (:data s)))))))
      (is (= {:kind :numeric :min 0.0 :max 1.0} (get-in s [:extents :panel :x]) (get-in s [:extents :panel :y])))
      (is (= {:x "a probabilities" :y "b probabilities"} (:axis-titles s))))
    (is (nil? (:line (first (groups-of {:data sample :mapping {:x :a :y :b} :charts [(chart/pp {:line false})]})))))
    (is (seq (error-ins #(built {:data sample :mapping {:x :a :y :b} :charts [(chart/pp {:line :quartiles})]}))))
    (is (seq (error-ins #(built {:data sample :mapping {:x :a :y :b} :charts [(chart/qq {:line :identity})]}))))))

(deftest distribution-label-test
  (testing "R3 a distribution-only chart: :label grouped by its own mapping - legend keys, shared colour scale"
    (let [p {:charts [(chart/qq {:x-distribution [:normal] :y-distribution [:t {:degrees-of-freedom 3}] :label "t3" :mapping {:color :label}})
                      (chart/qq {:x-distribution [:normal] :y-distribution [:normal] :label "n" :mapping {:color :label}})]}
          b (built p)]
      (is (= [{:color "t3"}] (map :aesthetics (:groups (val (first (:data (first (:stats b)))))))))
      (is (= #{"t3" "n"} (set (get-in b [:scale-specs :color :domain]))))
      (is (instance? BufferedImage (plot/render-image p {:width 400 :height 300})))))
  (testing "R3 the default label: <y name> vs <x name>; unmapped - no aesthetics"
    (let [b (built {:charts [(chart/qq {:x-distribution [:normal] :y-distribution [:t {:degrees-of-freedom 3}] :mapping {:color :label}})]})]
      (is (= #{"t vs normal"} (set (get-in b [:scale-specs :color :domain])))))
    (is (= {} (:aesthetics (first (groups-of {:charts [(chart/qq {:x-distribution (r/distribution :normal) :y-distribution [:normal] :label "x"})]}))))))
  (testing "R3 :label validated"
    (is (seq (error-ins #(built {:charts [(chart/qq {:x-distribution [:normal] :y-distribution [:normal] :label 1})]})))))
  (testing "R5 :n, :domain, :label with a data side throw"
    (is (= :n (:key (error #(built {:data sample :mapping {:y :a} :charts [(chart/qq {:x-distribution [:normal] :n 10})]})))))
    (is (= :label (:key (error #(built {:data sample :mapping {:x :a :y :b} :charts [(chart/qq {:label "l"})]})))))
    (is (= :domain (:key (error #(built {:data sample :mapping {:y :a} :charts [(chart/pp {:x-distribution [:normal] :domain [0 1]})]})))))))

(deftest probability-wiring-test
  (testing "1.4 theme styles validated; a mapped colour legend"
    (is (seq (error-ins #(built {:data sample :mapping {:y :a} :charts [(chart/qq {:x-distribution [:normal]})] :theme {:chart {:qq {:line-width -1}}}}))))
    (let [img (plot/render-image {:data sample :mapping {:x :a :y :b :color :g} :charts [(chart/qq)]} {:width 400 :height 300})]
      (is (instance? BufferedImage img)))
    (is (= 2 (count (:stats (built {:data sample :mapping {:x :a :y :b :color :g} :charts [(chart/qq) (chart/pp {:mapping {:facet nil}})]})))))))

;; 2 time series

(def ^:private noise (let [g (r/rng :mersenne 4)] (vec (repeatedly 200 #(r/grandom g)))))

(deftest acf-rows-test
  (testing "2.1 ACF rows equal fastmath's acf-ci; lag 0 kept, not significant"
    (let [rows (series/acf-rows noise {:lags 20})
          {:keys [acf cis]} (stats/acf-ci noise 20 0.05)]
      (is (= 21 (count rows)))
      (is (= (mapv double acf) (mapv :value rows)))
      (is (= (mapv double cis) (mapv :hi rows)))
      (is (= (mapv #(- (double %)) cis) (mapv :lo rows)))
      (is (= {:lag 0 :value 1.0 :significant false} (select-keys (first rows) [:lag :value :significant])))
      (is (<= (count (filter :significant rows)) 2) "white noise")))
  (testing "2.1 PACF rows: lags 1.., the constant band"
    (let [rows (series/acf-rows noise {:lags 20 :partial true :level 0.99})
          {:keys [ci pacf]} (stats/pacf-ci noise 20 0.01)]
      (is (= (range 1 21) (map :lag rows)))
      (is (= (mapv double (rest pacf)) (mapv :value rows)))
      (is (every? #(= (double ci) (:hi %)) rows))))
  (testing "2.1 an MA(1) series: lag 1 significant"
    (let [ma (mapv (fn [i] (+ (noise (inc i)) (* 0.9 (noise i)))) (range 199))]
      (is (:significant (second (series/acf-rows ma {}))))))
  (testing "2.1 default lags: min(floor(10 log10 n), n - 1)"
    (is (= 23 (series/default-lags 200)))
    (is (= 1 (series/default-lags 2)))
    (is (= 24 (count (series/acf-rows noise {})))))
  (testing "2.1 boundaries"
    (is (= :values (:key (error #(series/acf-rows [1 2 ##NaN 4] {})))))
    (is (= :values (:key (error #(series/acf-rows [1] {})))))
    (is (= :values (:key (error #(series/acf-rows [] {})))))
    (is (= :values (:key (error #(series/acf-rows [1 1 1 1] {})))))
    (is (= :lags (:key (error #(series/acf-rows [1 2 3 4] {:lags 4})))))
    (is (= :lags (:key (error #(series/acf-rows [1 2 3 4] {:lags 0})))))
    (is (= 2 (count (series/acf-rows [1 2 4] {:lags 1}))) "two values and lag 1")))

(deftest lag-rows-test
  (testing "2.2 pairs and counts"
    (is (= [{:t 0 :x 1 :y 3} {:t 1 :x 2 :y 4}] (series/lag-rows [1 2 3 4] 2)))
    (is (= 3 (count (series/lag-rows [1 2 3 4] 1)))))
  (testing "2.2 k >= n gives none; k < 1 throws"
    (is (= [] (series/lag-rows [1 2 3] 3)))
    (is (= [] (series/lag-rows [1 2 3] 10)))
    (is (= [] (series/lag-rows [] 1)))
    (is (= :lag (:key (error #(series/lag-rows [1 2 3] 0)))))
    (is (= :lag (:key (error #(series/lag-rows [1 2 3] 1.5)))))))

(deftest acf-builder-test
  (testing "2.3 ACF and PACF by default: two facet rows"
    (let [p (plot/acf noise)]
      (is (= :kind (get-in p [:mapping :facet-row])))
      (is (= #{"ACF" "PACF"} (set (map :kind (:data p)))))
      (is (= [:ribbon :rule :segment :scatter :scatter] (mapv :kind (:charts p))))
      (is (= ["ACF" "PACF"] (get-in (built p) [:split :facets :levels :facet-row])))))
  (testing "2.3 one kind: no facets, the y title"
    (let [p (plot/acf noise {:correlograms :acf})]
      (is (nil? (get-in p [:mapping :facet-row])))
      (is (= "ACF" (get-in p [:labels :y]))))
    (is (= "PACF" (get-in (plot/acf noise {:correlograms :pacf}) [:labels :y])))
    (is (= "ACF" (get-in (plot/acf noise {:correlograms :acf}) [:labels :y]))))
  (testing "R4 invalid :correlograms, :band, :colors throw; nil options - the defaults"
    (is (= :correlograms (:key (error #(plot/acf noise {:correlograms :acf2})))))
    (is (= :band (:key (error #(plot/acf noise {:band :blocky})))))
    (is (= :stems (:key (error #(plot/acf noise {:colors {:stems :red}})))))
    (is (= :stem (:key (error #(plot/acf noise {:colors {:stem :not-a-colour}})))))
    (is (= :colors (:key (error #(plot/acf noise {:colors [:red]})))))
    (is (= (plot/acf noise) (plot/acf noise {:correlograms nil :band nil :colors nil}))))
  (testing "U1 the band: a ribbon through the lags (default), a block per lag with :band :stepped"
    (let [smooth (first (:charts (plot/acf noise {:correlograms :acf :lags 5})))
          stepped (first (:charts (plot/acf noise {:correlograms :acf :lags 5 :band :stepped})))]
      (is (= {:x :lag :ymin :lo :ymax :hi} (get-in smooth [:options :mapping])))
      (is (= 6 (count (get-in smooth [:options :data]))))
      (is (= :lag (get-in stepped [:options :mapping :group])))
      (is (= [-0.5 0.5 0.5 1.5] (take 4 (map :x (get-in stepped [:options :data])))))))
  (testing "2.3 significant lags highlighted; colours, scales, title, theme"
    (let [ma (mapv (fn [i] (+ (noise (inc i)) (* 0.9 (noise i)))) (range 199))
          p (plot/acf ma {:colors {:significant [0 200 0]} :scales {:y {:domain [-1 1]}} :title "t" :theme {:plot {:background :white}}})
          hi (last (:charts p))]
      (is (= [0 200 0] (get-in hi [:options :color])))
      (is (every? :significant (get-in hi [:options :data])))
      (is (seq (get-in hi [:options :data])))
      (is (= [-1 1] (get-in p [:scales :y :domain])))
      (is (= "t" (get-in p [:labels :title])))
      (is (instance? BufferedImage (plot/render-image p {:width 500 :height 400})))))
  (testing "2.3 no significant lag: an empty highlight renders"
    (is (instance? BufferedImage (plot/render-image (plot/acf [1 2 1 2 1 3 2 1] {:correlograms :acf :lags 2}) {:width 300 :height 200})))))

;; 3 vector fields

(deftest field-input-test
  (testing "3.0 a fastmath field and (fn [[x y]] ..) give the same rows"
    (let [seeds [[0.5 1.0] [-1.0 2.0]]]
      (is (= (vf/mapped-points (f/field :sinusoidal) seeds)
             (vf/mapped-points (fn [[x y]] [(m/sin x) (m/sin y)]) seeds)))))
  (testing "3.0 not a pair throws {:key :f}; a non-finite result skipped"
    (is (= :f (:key (error #(vf/mapped-points (fn [_] 1.0) [[0 0]])))))
    (is (= :f (:key (error #(vf/mapped-points (fn [_] [1 2 3]) [[0 0]])))))
    (is (= [] (vf/mapped-points (fn [[x _]] [(/ 1.0 x) 0.0]) [[0.0 1.0]])))
    (is (= [] (vf/mapped-points (fn [_] [nil 1]) [[0.0 1.0]])))))

(deftest mapped-points-test
  (testing "3.1 the identity field returns the seeds"
    (is (= [{:x 1.0 :y 2.0 :x0 1 :y0 2}] (vf/mapped-points identity [[1 2]])))
    (is (= [] (vf/mapped-points identity []))))
  (testing "3.1 seeds must be pairs"
    (is (= :seeds (:key (error #(vf/mapped-points identity [[1 2 3]])))))
    (is (= :seeds (:key (error #(vf/mapped-points identity 5)))))))

(deftest arrows-test
  (let [domain {:x [0 4] :y [0 2]}]
    (testing "3.2 cell centres, length below a cell, direction along v"
      (let [rows (vf/arrows (fn [[x y]] [(- y 1.0) (+ x 1.0)]) {:domain domain :n [4 2]})]
        (is (= 8 (count rows)))
        (is (= [[0.5 0.5] [1.5 0.5]] (mapv (juxt :x :y) (take 2 rows))))
        (doseq [{:keys [x y xend yend magnitude]} rows
                :let [dx (- (double xend) (double x)) dy (- (double yend) (double y))
                      len (m/hypot dx dy) vx (- (double y) 1.0) vy (+ (double x) 1.0)]]
          (is (< len 1.0) "cell 1")
          (is (near? len (- 1.0 (m/exp (- (double magnitude))))))
          (is (near? 0.0 (- (* dx vy) (* dy vx))) "parallel")
          (is (pos? (+ (* dx vx) (* dy vy))) "same direction"))))
    (testing "3.2 :shifted moves every second row half a cell; :scale"
      (let [rows (vf/arrows (constantly [1.0 0.0]) {:domain domain :n 2 :grid :shifted :length-scale 2})]
        (is (= [[1.0 0.5] [3.0 0.5] [2.0 1.5] [4.0 1.5]] (mapv (juxt :x :y) rows)))
        (is (near? (* 2.0 1.0 (- 1.0 (m/exp -1.0))) (- (double (:xend (first rows))) 1.0)) "cell = min(2, 1)")))
    (testing "3.2 zero and non-finite vectors left out"
      (is (= [] (vf/arrows (constantly [0.0 0.0]) {:domain domain})))
      (is (= [] (vf/arrows (constantly [##Inf 0.0]) {:domain domain})))
      (is (= 400 (count (vf/arrows (constantly [1 1]) {:domain domain}))) "n 20 by default"))
    (testing "3.2 invalid options"
      (is (= :domain (:key (error #(vf/arrows identity {})))))
      (is (= :x (:key (error #(vf/arrows identity {:domain {:x [1 1] :y [0 1]}})))))
      (is (= :n (:key (error #(vf/arrows identity {:domain domain :n 0})))))
      (is (= :grid (:key (error #(vf/arrows identity {:domain domain :grid :hex})))))
      (is (= :length-scale (:key (error #(vf/arrows identity {:domain domain :length-scale 0})))))
      (is (= 1 (count (vf/arrows (constantly [1 0]) {:domain domain :n 1})))))))

(deftest traces-test
  (testing "3.3 a constant field: straight equidistant paths"
    (let [rows (vf/traces (constantly [2.0 0.0]) [[0 0] [0 1]] {:steps 4 :step 0.5})
          step (* 0.5 2.0 (- 1.0 (m/exp -2.0)))]
      (is (= 10 (count rows)))
      (is (= [0 0 0 0 0 1 1 1 1 1] (mapv :id rows)))
      (is (= [0 1 2 3 4] (mapv :i (take 5 rows))))
      (is (every? #(near? (* step (double (:i %))) (double (:x %))) rows))
      (is (= [0.0 0.0 0.0 0.0 0.0 1.0 1.0 1.0 1.0 1.0] (mapv :y rows)))))
  (testing "3.3 a path ends before a non-finite point"
    (is (= 3 (count (vf/traces (fn [[x _]] (if (< (double x) 0.25) [1.0 0.0] [##NaN 0.0])) [[0 0]] {:steps 10 :step 0.2})))))
  (testing "3.3 defaults, boundaries"
    (is (= 101 (count (vf/traces (constantly [0.0 0.0]) [[0 0]] {}))))
    (is (= [] (vf/traces identity [] {})))
    (is (= :steps (:key (error #(vf/traces identity [[0 0]] {:steps 0})))))
    (is (= :step (:key (error #(vf/traces identity [[0 0]] {:step -1})))))))

;; 4 rasters

(defn- pixel ^long [^BufferedImage img x y] (bit-and 0xffffff (.getRGB img (int x) (int y))))

(defn- panel-image
  "Rendered plot `p` without axes; the panel spans the image."
  [p w h]
  (plot/render-image (-> p
                         (update :scales #(merge-with merge {:x {:axis false} :y {:axis false}} %))
                         (assoc :theme {:plot {:margin 0}}))
                     {:width w :height h}))

(deftest pixel-color-test
  (testing "4.1 a constant colour fills the panel; no colour legend"
    (let [p {:charts [(chart/field {:pixel-color (constantly (c/color 255 0 0)) :domain {:x [0 1] :y [0 1]}})]}
          img (panel-image p 100 100)]
      (is (= 0xff0000 (pixel img 50 50)))
      (is (= 0xff0000 (pixel img 5 95)))
      (is (nil? (get-in (built p) [:scales :color])))))
  (testing "4.1 a half-plane splits at x = 0.5"
    (let [img (panel-image {:charts [(chart/field {:pixel-color (fn [[x _]] (if (< (double x) 0.5) :black :white)) :domain {:x [0 1] :y [0 1]}})]} 100 100)]
      (is (= 0x000000 (pixel img 49 50)))
      (is (= 0xffffff (pixel img 50 50)))))
  (testing "4.1 nil - transparent; :alpha; the plot's explicit domains; with :f throws"
    (let [img (panel-image {:charts [(chart/field {:pixel-color (constantly nil)})] :scales {:x {:domain [0 1]} :y {:domain [0 1]}}} 60 60)]
      (is (not= 0 (pixel img 30 30)) "the panel fill shows"))
    (is (= :pixel-color (:key (error #(built {:charts [(chart/field {:pixel-color (constantly :red) :f (fn [_] 1.0) :domain {:x [0 1] :y [0 1]}})]})))))
    (is (= :domain (:key (error #(built {:charts [(chart/field {:pixel-color (constantly :red)})]})))))
    (is (seq (error-ins #(built {:charts [(chart/field {:pixel-color :red :domain {:x [0 1] :y [0 1]}})]}))))))

(deftest domain-color-test
  (let [hue (fn [col] (first (c/to-HSB* col)))
        dc (field/domain-color identity)]
    (testing "4.2 arg 0 and arg pi - opposite hues (|z| 0.5: not on a band edge)"
      (is (< (m/abs (- 127.5 (m/abs (- (double (hue (dc [0.5 0.0]))) (double (hue (dc [-0.5 0.0]))))))) 1.0)))
    (testing "4.2 :log2 bands: the brightness wraps where |f| + 1 doubles"
      (let [bright (fn [col] (nth (c/to-HSB* col) 2))]
        (is (> (double (bright (dc [0.99 0.0]))) 240.0) "just below |f| = 1")
        (is (< (double (bright (dc [1.01 0.0]))) 10.0) "just above")
        (is (< (double (bright (dc [3.0 0.0]))) 1.0) "|f| = 3: log2 4")))
    (testing "4.2 zeros and poles give a colour; unknown wrap throws"
      (is (some? (dc [0.0 0.0])))
      (is (some? ((field/domain-color cx/log) [0.0 0.0])))
      (is (some? ((field/domain-color (fn [z] (cx/div cx/ONE z))) [0.0 0.0])))
      (is (= :wrap (:key (error #(field/domain-color identity {:wrap :log3})))))
      (is (every? #(some? ((field/domain-color identity {:wrap %}) [0.3 0.4])) field/wrap-methods)))
    (testing "U2 :colorspace - hue-first spaces, default HSI (brighter than HSB)"
      (let [luma (fn [col] (c/luma col))]
        (is (= (dc [0.5 0.3]) ((field/domain-color identity {:colorspace :HSI}) [0.5 0.3])))
        (is (> (double (luma (dc [0.5 0.3]))) (double (luma ((field/domain-color identity {:colorspace :HSB}) [0.5 0.3]))))))
      (is (every? #(some? ((field/domain-color identity {:colorspace %}) [0.3 0.4])) field/hue-colorspaces))
      (is (= :colorspace (:key (error #(field/domain-color identity {:colorspace :LAB})))))
      (is (= :colorspace (:key (error #(field/domain-color identity {:colorspace :hsi}))))))
    (testing "4.2 the cx/log raster renders"
      (is (instance? BufferedImage (plot/render-image {:charts [(chart/field {:pixel-color (field/domain-color cx/log) :domain {:x [-2 2] :y [-2 2]}})]}
                                                      {:width 120 :height 120}))))))

(deftest image-chart-test
  (testing "4.3 per facet: the key and the panel size; facets nil without facets"
    (let [calls (atom [])
          cb (fn [{:keys [facet size x-domain]}] (swap! calls conj [facet size x-domain]) nil)]
      (plot/render-image {:data [{:k "a"} {:k "b"}] :mapping {:facet :k} :charts [(chart/image {:f cb :domain {:x [0 1] :y [0 1]}})]}
                         {:width 400 :height 200})
      (is (= #{"a" "b"} (set (map first @calls))))
      (is (every? (fn [[_ [w h]]] (and (pos? (long w)) (< (long w) 200) (pos? (long h)))) @calls))
      (is (every? #(= [0.0 1.0] (mapv double (nth % 2))) @calls) ":domain - no padding, no nice")
      (reset! calls [])
      (plot/render-image {:charts [(chart/image {:f cb :domain {:x [0 1] :y [0 1]}})]} {:width 200 :height 200})
      (is (= [nil] (map first @calls)))))
  (testing "4.3 the pixel the callback marks at data (1, 1) is where scatter draws (1, 1)"
    (let [mark (fn [{:keys [x-scale y-scale size]}]
                 (let [[w h] size
                       img (BufferedImage. (int w) (int h) BufferedImage/TYPE_INT_ARGB)]
                   (.setRGB img (int (x-scale 1.0)) (int (y-scale 1.0)) (unchecked-int 0xffff0000))
                   img))
          p {:data [{:x 1 :y 1}] :mapping {:x :x :y :y} :scales {:x {:domain [0 2]} :y {:domain [0 2]}}}
          marked (panel-image (assoc p :charts [(chart/image {:f mark})]) 101 101)
          dotted (panel-image (assoc p :charts [(chart/scatter {:shape \S :size 3 :color :red})]) 101 101)
          red (fn [img] (set (for [x (range 101) y (range 101)
                                   :let [rgb (pixel img x y)]
                                   :when (and (> (bit-shift-right rgb 16) 200) (< (bit-and 0xff (bit-shift-right rgb 8)) 100))]
                               [x y])))]
      (is (= 1 (count (red marked))))
      (is (contains? (red dotted) (first (red marked))) "inside the 3-pixel square")))
  (testing "4.3 the three return types draw; nil draws nothing; another value throws"
    (doseq [make [(fn [w h] (let [img (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)] (doto (.createGraphics img) (.setColor java.awt.Color/RED) (.fillRect 0 0 w h)) img))
                  (fn [w h] (c2d/with-canvas-> (c2d/canvas w h) (c2d/set-background :red)))
                  (fn [w h] (p/filter-colors (constantly (c/color :red)) (p/pixels w h)))]]
      (let [img (panel-image {:charts [(chart/image {:f (fn [{[w h] :size}] (make w h)) :domain {:x [0 1] :y [0 1]}})]} 80 80)]
        (is (= 0xff0000 (pixel img 40 40)))))
    (is (instance? BufferedImage (plot/render-image {:charts [(chart/image {:f (constantly nil) :domain {:x [0 1] :y [0 1]}})]} {:width 100 :height 100})))
    (is (= :f (:key (error #(plot/render-image {:charts [(chart/image {:f (constantly 42) :domain {:x [0 1] :y [0 1]}})]} {:width 100 :height 100}))))))
  (testing "4.3 polar throws; without :domain the other charts' domains"
    (is (contains? (error-ins #(built {:coord {:kind :polar} :data [{:x 1 :y 1}] :mapping {:x :x :y :y}
                                       :charts [(chart/scatter) (chart/image {:f (constantly nil)})]}))
                   [:charts 1]))
    (let [domains (atom nil)]
      (plot/render-image {:data [{:x 2 :y 3} {:x 4 :y 5}] :mapping {:x :x :y :y}
                          :charts [(chart/scatter) (chart/image {:f (fn [{:keys [x-domain]}] (reset! domains x-domain) nil)})]}
                         {:width 200 :height 200})
      (is (< (double (first @domains)) 2.0))
      (is (> (double (second @domains)) 4.0)))
    (is (seq (error-ins #(built {:charts [(chart/image {:f identity :domain {:x [1 0] :y [0 1]}})]}))))
    (is (seq (error-ins #(built {:charts [(chart/image {})]}))))
    (is (= (chart/image {}) (chart/image)) "R6 the 0-arity constructor")
    (is (seq (error-ins #(built {:charts [(chart/image)]}))))))

;; gallery review (cycle 3)

(deftest seamless-cells-test
  (testing "U3 abutting tiles (37 x 37 cells on fractional pixel edges) leave no seam: one colour across the panel"
    (let [rows (for [x (range 37) y (range 37)] {:x x :y y :v 1.0})
          img (panel-image {:data rows :mapping {:x :x :y :y :color :v} :charts [(chart/tile)] :legend false
                            :scales {:color {:range [[0 20 52] [0 20 52]]}}}
                           301 301)
          colours (set (for [x (range 20 280 3) y (range 20 280 3)] (pixel img x y)))]
      (is (= #{0x001434} colours))))
  (testing "U3 hexbin cells abut too"
    (let [g (r/rng :mersenne 3)
          rows (repeatedly 4000 (fn [] {:x (r/drandom g) :y (r/drandom g)}))
          img (panel-image {:data rows :mapping {:x :x :y :y} :charts [(chart/bin2d {:bins 8})] :legend false
                            :scales {:color {:range [[0 20 52] [0 20 52]]}}}
                           301 301)]
      (is (= #{0x001434} (set (for [x (range 100 200 2) y (range 100 200 2)] (pixel img x y)))))))
  (testing "U3 spaced cells (:cell-scale < 1) keep their gaps"
    (let [rows (for [x (range 10) y (range 10)] {:x x :y y :v 1.0})
          img (panel-image {:data rows :mapping {:x :x :y :y :color :v} :charts [(chart/tile {:cell-scale 0.8})] :legend false
                            :scales {:color {:range [[0 20 52] [0 20 52]]}}}
                           301 301)]
      (is (< 1 (count (set (for [x (range 20 280 2) y (range 20 280 2)] (pixel img x y)))))))))
