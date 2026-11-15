(ns cljplot2.evolution-cycle1b-test
  "Evolution cycle 1b: position kinds :radial and :interpolated, tick methods, calendar ticks, date pattern labels."
  (:require [clojure.test :refer [deftest testing is are]]
            [cljplot2.test-util :refer [error]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.scale.position :as scale])
  (:import [java.time LocalDate LocalDateTime]))

(def ^:private size {:width 400 :height 300})
(defn- build [p] (plot/build p size))
(defn- finalized [k spec rng] (scale/finalize (scale/scale-spec k spec) rng 5))
(defn- near? [^double a ^double b] (< (Math/abs (- a b)) 1e-6))
(defn- spec-error [k spec] (error #(scale/scale-spec k spec)))
(def ^:private pts (for [i (range 0 101 5)] {:x i :y i}))
(defn- scatter [scales] {:data pts :mapping {:x :x :y :y} :charts [(chart/scatter)] :scales scales})
(def ^:private d0 (LocalDate/of 2016 1 10))
(def ^:private d1 (LocalDate/of 2016 12 20))

;; 1. kinds

(deftest radial-test
  (testing "1.1 square-root like: 25 % of the domain at half the range; linear ticks"
    (let [f (finalized :y {:kind :radial :domain [0.0 100.0] :nice false} [100.0 0.0])]
      (is (near? 50.0 ((scale/forward-fn f) 25.0)))
      (is (near? 0.0 ((scale/forward-fn f) 100.0)))
      (is (= [0.0 20.0 40.0 60.0 80.0 100.0] (mapv double (:ticks f))))))
  (testing "1.1 any orientation and range start: distances from the start grow as the square root"
    (is (near? 50.0 ((scale/forward-fn (finalized :x {:kind :radial :domain [0.0 100.0] :nice false} [0.0 100.0])) 25.0)))
    (is (near? 70.0 ((scale/forward-fn (finalized :x {:kind :radial :domain [0.0 100.0] :nice false} [20.0 120.0])) 25.0)))
    (let [f (finalized :y {:kind :radial :domain [0.0 100.0] :nice false} [300.0 0.0])]
      (is (near? 25.0 (double ((scale/inverse-fn f) ((scale/forward-fn f) 25.0)))))
      (is (= 6 (count (:ticks f))) "ticks placed with the offset stay inside")))
  (testing "1.1 a padded domain below 0 stays finite (signed squares)"
    (let [fwd (scale/forward-fn (finalized :y {:kind :radial :domain [-10.0 100.0] :nice false} [100.0 0.0]))]
      (is (every? #(Double/isFinite (fwd %)) [-10.0 0.0 50.0 100.0]))
      (is (near? 100.0 (fwd -10.0)))))
  (testing "1.1 a data domain is padded as linear"
    (let [b (build (scatter {:y {:kind :radial}}))]
      (is (= :radial (get-in b [:scale-specs :y :kind])))
      (is (some? (get-in b [:scale-specs :y :padding])))
      (is (some? (plot/render-image (scatter {:y {:kind :radial}}) size))))))

(deftest interpolated-test
  (testing "1.2 domain values at their stops"
    (let [fwd (scale/forward-fn (finalized :x {:kind :interpolated :domain [0 10 100] :stops [0 0.7 1]} [0.0 100.0]))]
      (is (near? 70.0 (fwd 10)))
      (is (near? 85.0 (fwd 55)))
      (is (near? 100.0 (fwd 100)))))
  (testing "1.2 default stops evenly spaced; a reversed (vertical) range"
    (is (near? 50.0 ((scale/forward-fn (finalized :x {:kind :interpolated :domain [0 10 100]} [0.0 100.0])) 10)))
    (is (near? 30.0 ((scale/forward-fn (finalized :y {:kind :interpolated :domain [0 10 100] :stops [0 0.7 1]} [100.0 0.0])) 10))))
  (testing "1.2 two values: linear"
    (is (near? 25.0 ((scale/forward-fn (finalized :x {:kind :interpolated :domain [0 4]} [0.0 100.0])) 1))))
  (testing "1.3 domain errors"
    (is (= :domain (:key (error #(build (scatter {:x {:kind :interpolated}}))))) "no explicit domain")
    (are [d] (= :domain (:key (spec-error :x {:kind :interpolated :domain d})))
      [5] [0 0 1] [0 10 5] [0 ##Inf]))
  (testing "1.3 stops errors"
    (are [spec] (= :stops (:key (spec-error :x spec)))
      {:kind :interpolated :domain [0 10 100] :stops [0 1]}
      {:kind :interpolated :domain [0 10 100] :stops [0 0.8 0.5]}
      {:kind :interpolated :domain [0 10 100] :stops [0 0.5 1.5]}
      {:kind :interpolated :domain [0 10 100] :stops [-0.1 0.5 1]}
      {:kind :linear :domain [0 10] :stops [0 1]}))
  (testing "1.4 interpolator: methods, functions, params; only for :interpolated"
    (is (some? (finalized :x {:kind :interpolated :domain [0 10 100] :interpolator :pchip} [0.0 100.0])))
    (is (some? (finalized :x {:kind :interpolated :domain [0 10 100] :interpolator :loess :interpolator-params [{:bandwidth 1.0}]} [0.0 100.0])))
    (is (= :interpolator (:key (spec-error :x {:domain [0 10] :interpolator :pchip}))))
    (is (= :interpolator-params (:key (spec-error :x {:domain [0 10] :interpolator-params []}))))
    (is (= :interpolator (:key (error #(build (scatter {:x {:kind :interpolated :domain [0 100] :interpolator :wavy}})))))))
  (testing "1.5 no padding, no nice by default; :expand rejected; :nice keeps the middle values"
    (let [b (build (scatter {:x {:kind :interpolated :domain [0 10 100]}}))]
      (is (nil? (get-in b [:scale-specs :x :padding])))
      (is (false? (get-in b [:scale-specs :x :nice]))))
    (is (= [0 10 97] (:domain (finalized :x {:kind :interpolated :domain [0 10 97]} [0.0 100.0]))))
    (is (= 3 (count (:domain (finalized :x {:kind :interpolated :domain [0 10 97] :nice true} [0.0 100.0])))))
    (is (= :expand (:key (spec-error :x {:kind :interpolated :domain [0 10 100] :expand [0.1 0.1]})))))
  (testing "1.6 inverse on radial and interpolated"
    (doseq [f [(finalized :x {:kind :radial :domain [0.0 100.0] :nice false} [0.0 400.0])
               (finalized :x {:kind :interpolated :domain [0 10 100] :stops [0 0.7 1]} [0.0 400.0])]
            v [0.0 3.0 10.0 55.0 100.0]]
      (is (< (Math/abs (- v (double ((scale/inverse-fn f) ((scale/forward-fn f) v))))) 1e-6))))
  (testing "1.7 explicit ticks"
    (is (= [0 5 10 50 100] (:ticks (finalized :x {:kind :interpolated :domain [0 10 100] :ticks [0 5 10 50 100]} [0.0 100.0]))))
    (is (some? (plot/render-image (scatter {:x {:kind :interpolated :domain [0 10 100] :stops [0 0.7 1] :ticks [0 5 10 50 100]}}) size)))))

;; 2. tick methods

(deftest ticks-method-test
  (testing "2.1 :pretty on a linear scale; tick methods rejected on bands"
    (let [p (finalized :x {:domain [0.0 97.0] :ticks-method :pretty :nice false} [0.0 400.0])]
      (is (= [0.0 20.0 40.0 60.0 80.0] (mapv double (:ticks p)))))
    (is (= :ticks-method (:key (spec-error :x {:domain ["a" "b"] :ticks-method :pretty}))))
    (is (= :ticks-params (:key (spec-error :x {:domain ["a" "b"] :ticks-params {:min-n 1}})))))
  (testing "2.2 datetime defaults to :pretty; :default kept"
    (is (= :pretty (:ticks-method (scale/scale-spec :x {:domain [d0 d1]}))))
    (is (= :default (:ticks-method (scale/scale-spec :x {:domain [d0 d1] :ticks-method :default}))))
    (let [ticks (:ticks (finalized :x {:domain [d0 d1] :nice false} [0.0 400.0]))]
      (is (seq ticks))
      (is (every? #(= 1 (.getDayOfMonth ^LocalDateTime %)) ticks) "month starts"))
    (is (some? (plot/render-image {:data [{:d d0 :v 1} {:d d1 :v 2}] :mapping {:x :d :y :v} :charts [(chart/line)]} size))))
  (testing "2.3 ticks params"
    (is (= :ticks-params (:key (spec-error :x {:domain [0 1] :ticks-params {:unit :months}}))) "calendar on a linear scale")
    (is (= :ticks-params (:key (spec-error :x {:domain [d0 d1] :ticks-method :default :ticks-params {:unit :months}}))) "calendar with :default")
    (is (= :ticks-params (:key (spec-error :x {:domain [d0 d1] :ticks-params {:step 2}}))) ":step without :unit")
    (are [tp] (some #{:ticks-params} (:in (error #(build (scatter {:x {:domain [0 100] :ticks-params tp}})))))
      {:min-n -1} {:min-n 1.5} {:unit :quarters} {:step 0} {:foo 1})
    (testing "an oversized :min-n is clamped to the tick count"
      (is (some? (plot/render-image (scatter {:x {:ticks-method :pretty :ticks-params {:min-n 50}}}) size))))))

;; 3. calendar ticks

(deftest calendar-ticks-test
  (testing "3.1 every 3 months from the year start"
    (is (= [(LocalDateTime/of 2016 4 1 0 0) (LocalDateTime/of 2016 7 1 0 0) (LocalDateTime/of 2016 10 1 0 0)]
           (:ticks (finalized :x {:domain [d0 d1] :nice false :ticks-params {:unit :months :step 3}} [0.0 400.0])))))
  (testing "3.2 nice to the outer calendar ticks"
    (is (= [(LocalDateTime/of 2016 1 1 0 0) (LocalDateTime/of 2017 1 1 0 0)]
           (:domain (finalized :x {:domain [d0 d1] :ticks-params {:unit :months :step 3}} [0.0 400.0])))))
  (testing "3.1 weeks start on Monday; step defaults to 1"
    (is (every? #(= java.time.DayOfWeek/MONDAY (.getDayOfWeek ^LocalDateTime %))
                (:ticks (finalized :x {:domain [(LocalDate/of 2016 1 1) (LocalDate/of 2016 2 1)] :nice false
                                       :ticks-params {:unit :weeks}} [0.0 400.0])))))
  (testing "3.3 :unit wins over the tick count and :min-n"
    (is (= 11 (count (:ticks (finalized :x {:domain [d0 d1] :nice false :ticks 3 :ticks-params {:unit :months :min-n 3}} [0.0 400.0])))))))

;; 4. date labels

(deftest date-formatter-test
  (testing "4.1 a pattern string labels datetime ticks"
    (is (= ["Apr 2016" "Jul 2016" "Oct 2016"]
           (:tick-labels (finalized :x {:domain [d0 d1] :nice false :ticks-params {:unit :months :step 3} :formatter "MMM yyyy"} [0.0 400.0])))))
  (testing "4.2 invalid patterns, zones, strings off datetime"
    (is (= :formatter (:key (spec-error :x {:domain [d0 d1] :formatter "yyyy {"}))))
    (is (= :formatter (:key (spec-error :x {:domain [d0 d1] :formatter "HH:mm z"}))))
    (is (= :formatter (:key (spec-error :x {:domain [0 1] :formatter "MMM"}))))
    (is (= :formatter (:key (error #(build (scatter {:x {:formatter "MMM"}})))))))
  (testing "4.1 a function formatter still works; 4.3 the default format without one"
    (is (= ["x" "x" "x"] (:tick-labels (finalized :x {:domain [d0 d1] :nice false :ticks-params {:unit :months :step 3}
                                                      :formatter (constantly "x")} [0.0 400.0]))))
    (is (every? string? (:tick-labels (finalized :x {:domain [d0 d1]} [0.0 400.0]))))))

;; review fixes

(deftest calendar-cap-test
  (testing "G1 too many calendar ticks: {:key :ticks-params} at build"
    (let [e (error #(build {:data [{:d (LocalDate/of 2016 1 1) :v 1} {:d (LocalDate/of 2017 1 1) :v 2}] :mapping {:x :d :y :v}
                            :charts [(chart/line)] :scales {:x {:ticks-params {:unit :minutes}}}}))]
      (is (= :ticks-params (:key e)))
      (is (= {:unit :minutes :step 1} (:value e))))
    (is (= :ticks-params (:key (spec-error :x {:domain [(LocalDateTime/of 2016 1 1 0 0) (LocalDateTime/of 2016 1 2 0 0)]
                                               :ticks-params {:unit :seconds :step 5}})))))
  (testing "G1 under the cap: hours over a day, a step that brings it under"
    (is (= 25 (count (:ticks (finalized :x {:domain [(LocalDateTime/of 2016 1 1 0 0) (LocalDateTime/of 2016 1 2 0 0)] :nice false
                                            :ticks-params {:unit :hours}} [0.0 400.0])))))
    (is (some? (scale/scale-spec :x {:domain [d0 d1] :ticks-params {:unit :minutes :step 60}})))))

(deftest radial-padding-test
  (testing "G2 no padding below the data on a radial scale; above as linear; :expand wins"
    (is (= [0.0 0.05] (get-in (build (scatter {:y {:kind :radial}})) [:scale-specs :y :padding])))
    (is (= [0.1 0.1] (get-in (build (scatter {:y {:kind :radial :expand [0.1 0.1]}})) [:scale-specs :y :padding])))
    (is (= [0.05 0.05] (get-in (build (scatter {:y {:kind :linear}})) [:scale-specs :y :padding])) "linear unchanged")))

(deftest reversed-interpolated-test
  (testing "G6 decreasing stops: a reversed axis"
    (let [f (finalized :x {:kind :interpolated :domain [0 10 100] :stops [1 0.3 0]} [0.0 100.0])
          fwd (scale/forward-fn f)]
      (is (near? 100.0 (fwd 0)))
      (is (near? 30.0 (fwd 10)))
      (is (near? 0.0 (fwd 100)))
      (is (near? 10.0 (double ((scale/inverse-fn f) 30.0))))
      (is (= 5 (count (:ticks (finalized :x {:kind :interpolated :domain [0 10 100] :stops [1 0.3 0] :ticks [0 5 10 50 100]} [0.0 100.0])))))))
  (testing "G6 non-monotone stops still rejected"
    (is (= :stops (:key (spec-error :x {:kind :interpolated :domain [0 10 100] :stops [0 1 0.5]}))))))
