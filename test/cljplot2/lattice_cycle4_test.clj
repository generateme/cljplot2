(ns cljplot2.lattice-cycle4-test
  "Lattice cycle 4 (data and notebook): library gaps fixed while writing the lattice notebook - `chart/smooth` fitted
  in the coordinates of log axes (Fig 5.9, 5.10)."
  (:require [clojure.test :refer [deftest testing is]]
            [fastmath.core :as m]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]))

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
