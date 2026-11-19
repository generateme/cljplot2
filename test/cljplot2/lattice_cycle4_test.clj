(ns cljplot2.lattice-cycle4-test
  "Lattice cycle 4 (data and notebook): library gaps fixed while writing the lattice notebook - `chart/smooth` fitted
  in the coordinates of log axes (Fig 5.9, 5.10), color scale `:transform` (Fig 5.14), `:type :categorical` domains
  (Fig 2.6), one default point size, at least two ticks on an axis (Fig 14.2), the nearest tick count, reversed
  axes - 2D `:reverse true`, 3D `:reverse #{..}` (Fig 6.18)."
  (:require [clojure.test :refer [deftest testing is]]
            [fastmath.core :as m]
            [wadogo.scale :as s]
            [cljplot2.chart :as chart]
            [cljplot2.chart.parallel :as parallel]
            [cljplot2.content.legend :as legend]
            [cljplot2.data.space3d :as space]
            [cljplot2.core :as plot]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.position :as position]
            [cljplot2.test-util :refer [error-data error-ins]]
            [cljplot2.theme :as theme]))

(defn- curve
  "The first fitted curve of the smooth (chart 0) of plot `p`."
  [p]
  (first (get-in (plot/build p {:width 400 :height 300}) [:stats 0 :data :panel :curves])))

(def ^:private power-rows
  "y = 2^x on x 1..10 (a straight line on a log y axis)."
  (mapv (fn [x] {:x x :y (m/pow 2.0 x)}) (range 1 11)))

(deftest smooth-log-axes-test
  (testing "a log y axis: the fit runs in log10 y, a straight line there is exact"
    (let [{:keys [x y]} (curve {:data power-rows :mapping {:x :x :y :y}
                                :scales {:y {:kind :log}}
                                :charts [(chart/smooth {:method :lm :band :none})]})]
      (is (every? true? (map (fn [xv yv] (< (m/abs (- (m/log2 yv) xv)) 1e-9)) x y)))))
  (testing "log x and y: a power law is a straight line; the curve's x stays in data units over the data's range"
    (let [rows (mapv (fn [d] {:d d :a (* 3.0 (m/pow d -0.5))}) [1 2 4 8 16 32 64])
          {:keys [x y]} (curve {:data rows :mapping {:x :d :y :a}
                                :scales {:x {:kind :log :base 2} :y {:kind :log :base 2}}
                                :charts [(chart/smooth {:method :lm :band :none})]})]
      (is (< (m/abs (- 1.0 (double (first x)))) 1e-9))
      (is (< (m/abs (- 64.0 (double (peek x)))) 1e-9))
      (is (every? true? (map (fn [d a] (< (m/abs (- a (* 3.0 (m/pow d -0.5)))) 1e-9)) x y)))))
  (testing "a loess fit on a log axis stays positive (it went below 0 in data units)"
    (let [rows (mapv (fn [x] {:x x :y (if (< x 5) 1.0 (m/pow 10.0 (- x)))}) (range 0 10))
          {:keys [y]} (curve {:data rows :mapping {:x :x :y :y} :scales {:y {:kind :log}}
                              :charts [(chart/smooth {:band :none})]})]
      (is (every? pos? y))))
  (testing "boundary: a 0 or negative y on a log axis is left out of the fit, the rest fitted"
    (let [{:keys [x]} (curve {:data (conj power-rows {:x 11 :y 0.0} {:x 12 :y -4.0}) :mapping {:x :x :y :y}
                              :scales {:y {:kind :log}}
                              :charts [(chart/smooth {:method :lm :band :none})]})]
      (is (< (m/abs (- 10.0 (double (peek x)))) 1e-9) "x range of the kept points")))
  (testing "bands come back to data units: around the curve on a log axis"
    (let [rows (mapv (fn [x] {:x x :y (* (m/pow 2.0 x) (if (even? x) 1.1 0.9))}) (range 1 21))
          {:keys [y bands]} (curve {:data rows :mapping {:x :x :y :y} :scales {:y {:kind :log}}
                                    :charts [(chart/smooth {:method :lm})]})
          {:keys [lo hi]} (first bands)]
      (is (every? true? (map (fn [l c h] (< l c h)) lo y hi)))))
  (testing "linear axes: unchanged (identity transform)"
    (let [rows (mapv (fn [x] {:x x :y (* 2.0 x)}) (range 5))
          {:keys [y]} (curve {:data rows :mapping {:x :x :y :y} :charts [(chart/smooth {:method :lm :band :none})]})]
      (is (< (m/abs (- 8.0 (double (peek y)))) 1e-9))))
  (testing "renders on log axes"
    (is (some? (plot/render-image {:data power-rows :mapping {:x :x :y :y} :scales {:y {:kind :log}}
                                   :charts [(chart/scatter) (chart/smooth)]}
                                  {:width 300 :height 200})))))

(deftest color-transform-test
  (let [spec (fn [s] (aesthetic/scale-spec :color s {:kind :numeric :min 0.0 :max 100.0} false
                                          (get-in theme/default-theme [:palette])))
        t-of (fn [s v] (let [g (aesthetic/attribute-fn :color (assoc (spec s) :range (fn [t] t)) nil)] (g v)))]
    (testing ":sqrt places values by their square root, :log by their logarithm; ends at 0 and 1"
      (is (< (m/abs (- 0.5 (double (t-of {:transform :sqrt} 25.0)))) 1e-12))
      (is (= 0.0 (double (t-of {:transform :sqrt} 0.0))))
      (is (= 1.0 (double (t-of {:transform :sqrt} 100.0))))
      (is (< (m/abs (- 0.5 (double (t-of {:transform :log :domain [1 100]} 10.0)))) 1e-12))
      (is (< (m/abs (- 0.25 (double (t-of {} 25.0)))) 1e-12) "no transform: linear"))
    (testing "boundary: :sqrt from 0, :log above 0 - else {:key :transform}"
      (is (= :sqrt (:transform (spec {:transform :sqrt}))))
      (is (= :transform (:key (error-data #(spec {:transform :log})))) "data from 0")
      (is (= :transform (:key (error-data #(spec {:transform :sqrt :domain [-1 4]}))))))
    (testing "only a linear :color scale takes :transform"
      (is (seq (error-ins #(plot/build {:data [{:x 1 :y 1 :s 2}] :mapping {:x :x :y :y :size :s}
                                        :scales {:size {:transform :sqrt}} :charts [(chart/scatter)]}))))
      (is (seq (error-ins #(plot/build {:data [{:x 1 :y 1 :g "a"}] :mapping {:x :x :y :y :color :g}
                                        :scales {:color {:transform :sqrt}} :charts [(chart/scatter)]})))))
    (testing "color bar: positions and values follow the transform (sqrt: 25 halfway; log: 10 halfway of [1 100])"
      (let [position (#'legend/bar-position-fn {:kind :linear :domain [0.0 100.0] :transform :sqrt})
            value (#'legend/bar-value-fn {:kind :linear :domain [1.0 100.0] :transform :log})]
        (is (< (m/abs (- 0.5 (double (position 25.0)))) 1e-12))
        (is (< (m/abs (- 10.0 (double (value 0.5)))) 1e-9))))
    (testing "the color bar keeps data-unit ticks, placed by the transform; renders"
      (let [b (plot/build {:data (mapv (fn [i] {:x i :y i :c (* i i)}) (range 11)) :mapping {:x :x :y :y :color :c}
                           :scales {:color {:transform :sqrt}} :charts [(chart/scatter)]}
                          {:width 400 :height 300})]
        (is (= :sqrt (get-in b [:scale-specs :color :transform])))
        (is (some? (plot/render-image (select-keys b [:data :mapping :scales :charts]) {:width 400 :height 300})))))))

(deftest categorical-type-domain-test
  (testing "`:type :categorical` with two numbers is an ordinal domain (it was validated as linear)"
    (let [b (plot/build {:data [{:x 1 :y 1 :year 1932} {:x 2 :y 2 :year 1931}] :mapping {:x :x :y :y :color :year}
                         :scales {:color {:type :categorical :domain [1932 1931]}} :charts [(chart/scatter)]}
                        {:width 300 :height 200})]
      (is (= :ordinal (get-in b [:scale-specs :color :kind])))
      (is (= [1932 1931] (get-in b [:scale-specs :color :domain])))))
  (testing "without :type two numbers stay a linear domain; reversed throws"
    (is (seq (error-ins #(plot/build {:data [{:x 1 :y 1 :c 5}] :mapping {:x :x :y :y :color :c}
                                      :scales {:color {:domain [9 1]}} :charts [(chart/scatter)]}))))))

(deftest default-point-size-test
  (testing "one default point size, 6, for the point charts"
    (is (= #{6} (set (map #(get-in theme/default-theme [:chart % :size]) [:scatter :cloud :qq :pp :beeswarm :stripplot])))))
  (testing "the strip plot stays translucent"
    (is (< (long (nth (get-in theme/default-theme [:chart :stripplot :color]) 3)) 255))))

(defn- finalized-ticks
  "Ticks of a `:y` scale spec of `options` finalized over 70 px with tick hint 2 (a short facet panel)."
  [options]
  (:ticks (position/finalize (position/scale-spec :y (merge {:nice false} options)) [0.0 70.0] 2)))

(deftest two-ticks-test
  (testing "a short axis whose nice step is wider than its domain gets more ticks until two are inside (Fig 14.2)"
    (is (>= (count (finalized-ticks {:domain [-2100 4500]})) 2))
    (is (>= (count (finalized-ticks {:domain [-6650 7650]})) 2))
    (is (>= (count (finalized-ticks {:domain [2330 3870]})) 2))
    (is (>= (count (finalized-ticks {:domain [1.5 1.7] :kind :log})) 2) "log within one decade")
    (is (>= (count (finalized-ticks {:domain [0.0001 0.00012]})) 2) "small numbers"))
  (testing "the domain and the mapping stay; the count used is the scale's"
    (let [spec (position/scale-spec :y {:domain [-2100 4500] :nice false})
          f (position/finalize spec [70.0 0.0] 2)]
      (is (= [-2100.0 4500.0] (mapv double (:domain f))))
      (is (> (long (get-in f [:attributes :ticks])) 2))
      (is (< (m/abs (- 70.0 (double ((position/forward-fn f) -2100.0)))) 1e-9))))
  (testing "boundary: ticks the user set stay as given - a count, a vector"
    (is (= [0.0] (mapv double (finalized-ticks {:domain [-2100 4500] :ticks 2}))))
    (is (= [100.0] (mapv double (finalized-ticks {:domain [-2100 4500] :ticks [100 9000]})))))
  (testing "boundary: a long axis keeps its count (unchanged ticks)"
    (let [f (position/finalize (position/scale-spec :y {:domain [0 10] :nice false}) [0.0 500.0] 10)]
      (is (= 10 (get-in f [:attributes :ticks])))))
  (testing "ticks-inside: the first count giving two inside; the last count's ticks when none does"
    (let [{:keys [count ticks]} (position/ticks-inside #(s/scale :linear {:domain [-2100 4500] :ticks %}) 2
                                                       #(<= -2100.0 (double %) 4500.0))]
      (is (= 3 count))
      (is (= [-2000.0 0.0 2000.0 4000.0] (mapv double ticks))))
    (let [{:keys [count ticks]} (position/ticks-inside #(s/scale :linear {:domain [0 1] :ticks %}) 3 (constantly false))]
      (is (= position/max-tick-count count))
      (is (empty? ticks))))
  (testing "parallel axes and color bars follow the same rule; a zero-length parallel domain - one tick"
    (is (>= (count (parallel/axis-ticks [-2100.0 4500.0] 70.0 50.0)) 2))
    (is (= 1 (count (parallel/axis-ticks [3.0 3.0] 70.0 50.0))))
    (is (>= (count (:ticks (#'legend/colorbar-content {:kind :linear :domain [2330.0 2390.0]} nil))) 2)))
  (testing "a faceted plot of short panels: every y axis at least two ticks"
    (let [rows (for [[g k] [["a" 1.0] ["b" 3000.0] ["c" 0.01]] i (range 20)] {:g g :x i :y (* k (m/sin i))})
          b (plot/build {:data rows :mapping {:x :x :y :y :facet :g} :charts [(chart/line)]
                         :facets {:columns 1 :scales {:y :free}}}
                        {:width 400 :height 200})
          y-ticks (map #(get-in % [:y :ticks]) (vals (:panel-scales b)))]
      (is (= 3 (count y-ticks)))
      (is (every? #(>= (count %) 2) y-ticks)))))

(deftest tick-hint-test
  (testing "the nearest whole count: panels a few pixels apart around a multiple of the spacing ask for the same (1.1 / 1.2)"
    (is (= 3 (position/tick-hint 241.0 80.0) (position/tick-hint 239.0 80.0))))
  (testing "boundary: the half rounds up; at least 2 (a short or empty axis)"
    (is (= 2 (position/tick-hint 199.0 80.0)))
    (is (= 3 (position/tick-hint 200.0 80.0)))
    (is (= 2 (position/tick-hint 10.0 80.0)))
    (is (= 2 (position/tick-hint 0.0 80.0))))
  (testing "a histogram and a density of one column in one layout (panels a few px apart): the same tick count"
    (let [rows (for [g ["a" "b" "c" "d" "e" "f"] i (range 200)] {:g g :v (+ 4.0 (* 2.0 (m/sin (* 0.37 i))))})
          x-ticks (fn [c] (get-in (plot/build {:data rows :mapping {:x :v :facet :g} :charts [c] :facets {:columns 3}}
                                              {:width 800 :height 500})
                                  [:finalized-scales :x :ticks]))]
      (is (= (x-ticks (chart/histogram)) (x-ticks (chart/density)))))))

(defn- finalized
  "`:x` scale of `options` finalized over 400 px (hint 5)."
  [options]
  (position/finalize (position/scale-spec :x options) [0.0 400.0] 5))

(deftest reverse-2d-test
  (testing ":reverse true: the domain the other way - the low end at the far pixel"
    (let [f (finalized {:domain [0 10] :reverse true})
          px (position/forward-fn f)]
      (is (= [10.0 0.0] (mapv double (:domain f))))
      (is (< (m/abs (- 400.0 (double (px 0.0)))) 1e-9))
      (is (< (m/abs (double (px 10.0))) 1e-9))
      (is (= #{0.0 2.0 4.0 6.0 8.0 10.0} (set (map double (:ticks f)))) "the same ticks")))
  (testing "after nice and padding: the extended domain reversed"
    (is (= [10.0 0.0] (mapv double (:domain (finalized {:domain [0.3 9.7] :reverse true}))))))
  (testing "bands: levels in reverse order; log and datetime reverse too"
    (is (= ["c" "b" "a"] (:domain (finalized {:domain ["a" "b" "c"] :reverse true}))))
    (is (= [1000.0 1.0] (mapv double (:domain (finalized {:domain [1 1000] :kind :log :reverse true})))))
    (let [a (java.time.LocalDateTime/of 2020 1 1 0 0) b (java.time.LocalDateTime/of 2021 1 1 0 0)]
      (is (= [b a] (:domain (finalized {:domain [a b] :reverse true}))))))
  (testing "boundary: an explicit reversed domain reversed again runs forward; false / nil - unchanged"
    (is (= [0.0 10.0] (mapv double (:domain (finalized {:domain [10 0] :reverse true})))))
    (is (= [0.0 10.0] (mapv double (:domain (finalized {:domain [0 10] :reverse false})))))
    (is (= [0.0 10.0] (mapv double (:domain (finalized {:domain [0 10] :reverse nil}))))))
  (testing "boundary: not on :interpolated (its :stops reverse it); not a boolean - a schema error"
    (is (= :reverse (:key (error-data #(position/scale-spec :x {:kind :interpolated :domain [0 1 2] :reverse true})))))
    (is (seq (error-ins #(plot/build {:data [{:x 1 :y 1}] :mapping {:x :x :y :y} :scales {:x {:reverse 1}}
                                      :charts [(chart/scatter)]})))))
  (testing "a plot's data scale: the x axis runs high to low"
    (let [b (plot/build {:data (mapv (fn [i] {:x i :y i}) (range 11)) :mapping {:x :x :y :y}
                         :scales {:x {:reverse true}} :charts [(chart/scatter)]}
                        {:width 400 :height 300})
          x (get-in b [:finalized-scales :x])
          px (position/forward-fn x)]
      (is (> (double (px 0)) (double (px 10))))))
  (testing "facets with free scales reverse every panel"
    (let [b (plot/build {:data (for [g ["a" "b"] i (range 5)] {:g g :x (if (= g "a") i (* 10 i)) :y i})
                         :mapping {:x :x :y :y :facet :g} :scales {:x {:reverse true}}
                         :facets {:scales {:x :free}} :charts [(chart/scatter)]}
                        {:width 400 :height 300})]
      (is (every? (fn [sc] (let [[a z] (get-in sc [:x :domain])] (> (double a) (double z)))) (vals (:panel-scales b)))))))

(deftest reverse-3d-test
  (let [rows (vec (for [x [0 1 2] y [0 1 2]] {:x x :y y :z (+ x y)}))
        computed (fn [chart] (get-in (plot/build {:data rows :mapping {:x :x :y :y :z :z} :charts [chart]}
                                                 {:width 400 :height 300})
                                     [:stats 0 :data :panel]))]
    (testing ":reverse #{..} reverses the final (data, nice) domains of the axes named"
      (let [plain (:domains (computed (chart/cloud)))
            reversed (:domains (computed (chart/cloud {:reverse #{:x :z}})))]
        (is (= (vec (reverse (:x plain))) (:x reversed)))
        (is (= (:y plain) (:y reversed)))
        (is (= (vec (reverse (:z plain))) (:z reversed)))))
    (testing "explicit domains, the surface too"
      (is (= [4 0] (get-in (computed (chart/cloud {:domain {:x [0 4]} :reverse #{:x}})) [:domains :x])))
      (is (= [1.0 -1.0] (mapv double (get-in (computed (chart/surface {:f (fn [[x y]] (* x y)) :domain {:x [-1 1] :y [-1 1]}
                                                                     :reverse #{:x}}))
                                             [:domains :x])))))
    (testing "the projection of a reversed x runs right to left (azimuth 0: x to the right)"
      (let [domains (space/reverse-domains {:x [0 1] :y [0 1] :z [0 1]} #{:x})
            {:keys [point]} (space/projector {:domains domains :view {:azimuth 0 :elevation 0}} {:w 400 :h 300 :margin 20})]
        (is (> (.x ^fastmath.vector.Vec2 (first (point [0 0.5 0.5]))) (.x ^fastmath.vector.Vec2 (first (point [1 0.5 0.5])))))))
    (testing "boundary: an empty set / nil - unchanged; not x / y / z - a schema error"
      (is (= {:x [0 1]} (space/reverse-domains {:x [0 1]} #{})))
      (is (= {:x [0 1]} (space/reverse-domains {:x [0 1]} nil)))
      (is (seq (error-ins #(plot/build {:data rows :mapping {:x :x :y :y :z :z} :charts [(chart/cloud {:reverse #{:w}})]})))))
    (testing "renders: bars, projections (densities sampled low to high), a surface"
      (is (some? (plot/render-image {:data rows :mapping {:x :x :y :y :z :z}
                                     :charts [(chart/cloud {:type :bars :reverse #{:x :y}
                                                            :projections {:xy :density :xz :contour}})]}
                                    {:width 400 :height 300})))
      (is (some? (plot/render-image {:charts [(chart/surface {:f (fn [[x y]] (* x y)) :domain {:x [-1 1] :y [-1 1]}
                                                              :reverse #{:z}})]}
                                    {:width 400 :height 300}))))))
