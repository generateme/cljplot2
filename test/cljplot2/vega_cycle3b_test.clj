(ns cljplot2.vega-cycle3b-test
  "Vega cycle 3b review: `chart/bullet`; horizon `:interpolator` / `:steps-per-gap` (resampled before folding) and
  `:alpha`."
  (:require [clojure.test :refer [deftest testing is]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.series :as series]
            [cljplot2.test-util :refer [error error-data]]))

;; bullet

(def ^:private revenue {:title "Revenue" :ranges [150 225 300] :measures [220 270] :markers [250]})

(defn- bullet-plot
  [rows options]
  {:data rows :mapping {:ranges :ranges :measures :measures :markers :markers}
   :charts [(chart/bullet options)]})

(defn- bullet-stats
  [p]
  (get-in (plot/build p {:width 400 :height 200}) [:stats 0]))

(deftest bullet-values-test
  (testing "one bullet: values as given (doubles), the x extent from 0 to the largest value"
    (let [{:keys [data extents]} (bullet-stats (bullet-plot [revenue] {}))
          item (first (get-in data [:panel :items]))]
      (is (= [150.0 225.0 300.0] (:ranges item)))
      (is (= [220.0 270.0] (:measures item)))
      (is (= [250.0] (:markers item)))
      (is (= [0.0 300.0] ((juxt :min :max) (get-in extents [:panel :x]))))))
  (testing "boundary: a number for a collection, nil and an empty collection - no values of that kind"
    (let [item (first (get-in (bullet-stats (bullet-plot [{:ranges 10 :measures nil :markers []}] {}))
                              [:data :panel :items]))]
      (is (= [[10.0] [] []] ((juxt :ranges :measures :markers) item)))))
  (testing "negative values: the extent reaches below 0"
    (is (= -5.0 (get-in (bullet-stats (bullet-plot [{:measures [-5 3]}] {})) [:extents :panel :x :min]))))
  (testing "levels: one bullet per level"
    (is (= ["A" "B"] (map :level (get-in (bullet-stats (assoc-in (bullet-plot [(assoc revenue :t "A") (assoc revenue :t "B")] {})
                                                                 [:mapping :y] :t))
                                        [:data :panel :items]))))))

(deftest bullet-errors-test
  (testing "a value that isn't a finite number (or a collection of them)"
    (is (= :mapping (:key (error #(plot/build (bullet-plot [{:ranges [1 ##NaN]}] {}))))))
    (is (= :mapping (:key (error #(plot/build (bullet-plot [{:ranges "300"}] {})))))))
  (testing "two rows of one level"
    (is (= :mapping (:key (error #(plot/build (bullet-plot [revenue revenue] {})))))))
  (testing "nothing mapped; bad style options"
    (is (some? (error #(plot/build {:data [revenue] :charts [(chart/bullet)]}))))
    (is (some? (error #(plot/build (bullet-plot [revenue] {:measure-width 1.5})))))
    (is (some? (error #(plot/build (bullet-plot [revenue] {:range-colors [:red]})))))))

(deftest bullet-render-test
  (testing "renders horizontal, vertical, faceted with free x"
    (is (some? (plot/render-image (bullet-plot [revenue] {}) {:width 300 :height 100})))
    (is (some? (plot/render-image (assoc-in (bullet-plot [(assoc revenue :t "A")] {}) [:mapping :x] :t) {:width 200 :height 300})))
    (is (some? (plot/render-image (-> (bullet-plot [(assoc revenue :t "A") {:t "B" :ranges [1 2] :measures [1.5] :markers [1.8]}] {})
                                      (assoc-in [:mapping :facet] :t)
                                      (assoc :facets {:columns 1 :scales {:x :free}}))
                                  {:width 300 :height 200}))))
  (testing "the marker is drawn in the marker colour"
    (let [img (plot/render-image (bullet-plot [{:markers [5] :ranges [10]}] {:marker-color :red :marker-width 4})
                                 {:width 300 :height 100})
          reds (for [x (range 300) y (range 100) :when (== (unchecked-int 0xffff0000) (.getRGB ^java.awt.image.BufferedImage img (int x) (int y)))] x)]
      (is (seq reds))
      (is (<= (- (reduce max reds) (reduce min reds)) 5) "one vertical line"))))

(deftest bullet-review-test
  (testing "M1: aesthetics are schema errors; nil-ed plot aesthetics and :group are fine"
    (is (= :mapping (:key (error #(plot/build (assoc-in (bullet-plot [(assoc revenue :g "a")] {}) [:mapping :color] :g))))))
    (is (re-find #"doesn't take aesthetics"
                 (pr-str (:errors (error-data #(plot/build (assoc-in (bullet-plot [(assoc revenue :g "a")] {}) [:mapping :size] :g)))))))
    (is (some? (plot/build {:data [(assoc revenue :g "a")] :mapping {:color :g}
                            :charts [(chart/bullet {:mapping {:color nil :ranges :ranges :measures :measures :markers :markers}})]})))
    (is (= 2 (count (get-in (bullet-stats (-> (bullet-plot [(assoc revenue :g "a") (assoc revenue :g "b")] {})
                                              (assoc-in [:mapping :group] :g)))
                            [:data :panel :items])))))
  (testing "M2: no n labels"
    (is (some? (error #(plot/build (bullet-plot [revenue] {:n-label true})))))
    (is (some? (error #(plot/build (bullet-plot [revenue] {:n-format "%d"}))))))
  (testing "M3: 0.05 on the sides holding values, 0 on the side of 0; :padding wins"
    (let [pad (fn [rows options] (get-in (bullet-stats (bullet-plot rows options)) [:padding :x]))]
      (is (= [0.0 0.05] (pad [revenue] {})))
      (is (= [0.05 0.0] (pad [{:ranges [-30 -10] :measures [-20]}] {})))
      (is (= [0.05 0.05] (pad [{:ranges [-30 10]}] {})))
      (is (= [0.0 0.0] (pad [{:ranges [0] :measures []}] {})))
      (is (= [0.1 0.1] (pad [revenue] {:padding {:x [0.1 0.1]}})))
      (is (some? (plot/render-image (bullet-plot [{:ranges [0]}] {}) {:width 200 :height 80})) "all zero renders")
      (is (= [0.0 0.05] (get-in (bullet-stats (assoc-in (bullet-plot [(assoc revenue :t "A")] {}) [:mapping :x] :t))
                                [:padding :y]))
          "levels on x: the value padding on y")))
  (testing "M4: sequential collections only (a list is fine, a set isn't)"
    (is (= [2.0 1.0] (:measures (first (get-in (bullet-stats (bullet-plot [{:measures (list 2 1)}] {})) [:data :panel :items])))))
    (is (= :mapping (:key (error #(plot/build (bullet-plot [{:measures #{1 2}}] {}))))))))

;; horizon

(def ^:private wave (map (fn [x y] {:x x :y y}) (range 6) [10 60 20 90 40 5]))

(deftest horizon-interpolation-test
  (let [plain (series/horizon-rows wave {:band-height 50})
        smooth (series/horizon-rows wave {:band-height 50 :interpolator :monotone})]
    (testing "default: unchanged (no interpolation)"
      (is (= plain (series/horizon-rows wave {:band-height 50 :steps-per-gap 4}))))
    (testing "monotone: more points, every data point kept in every band"
      (is (> (count (:rows smooth)) (count (:rows plain))))
      (doseq [{:keys [x y]} wave
              band [1 2]
              :let [expected (min 50.0 (max 0.0 (- (double y) (* 50.0 (dec band)))))]]
        (is (some #(and (== x (:x %)) (= band (:band %)) (== expected (:y %))) (:rows smooth)) [x band])))
    (testing "boundary: one step per gap - the data points only, as without interpolation"
      (is (= plain (series/horizon-rows wave {:band-height 50 :interpolator :monotone :steps-per-gap 1}))))
    (testing "every band value stays inside its band"
      (is (every? #(<= 0.0 (double (:y %)) 50.0) (remove (comp nil? :y) (:rows smooth))))))
  (testing "a nil breaks the runs; a run too short for the method stays as is"
    (let [rows (series/horizon-rows [{:x 0 :y 10} {:x 1 :y nil} {:x 2 :y 20} {:x 3 :y 30} {:x 4 :y 10}]
                                    {:bands 1 :band-height 50 :interpolator :monotone :steps-per-gap 5})
          xs (map :x (filter #(= 1 (:band %)) (:rows rows)))]
      (is (some nil? (map :y (:rows rows))))
      (is (= 0.0 (first xs)))
      (is (= 1 (count (filter #(< (double %) 1.0) xs))) "the one-point run isn't resampled")
      (is (= 11 (count (filter #(>= (double %) 2.0) xs))) "2 gaps × 5 steps + the last point")))
  (testing "errors: unknown method, repeated x, bad steps"
    (is (= :interpolator (:key (error #(series/horizon-rows wave {:interpolator :nope})))))
    (is (re-find #":monotone" (pr-str (:errors (error-data #(series/horizon-rows wave {:interpolator "monotone"}))))) "M5: the schema's message")
    (is (= :interpolator (:key (error #(series/horizon-rows [{:x 0 :y 1} {:x 0 :y 2} {:x 1 :y 3}] {:interpolator :linear})))))
    (is (= :steps-per-gap (:key (error #(series/horizon-rows wave {:interpolator :monotone :steps-per-gap 0})))))
    (is (= :steps-per-gap (:key (error #(series/horizon-rows wave {:steps-per-gap 1.5})))))))

(deftest horizon-alpha-test
  (testing "the builder passes :alpha to the band areas (default opaque)"
    (is (= 1.0 (get-in (plot/horizon wave {}) [:charts 0 :options :alpha])))
    (is (= 0.6 (get-in (plot/horizon wave {:alpha 0.6 :interpolator :monotone}) [:charts 0 :options :alpha])))
    (is (some? (error #(plot/build (plot/horizon wave {:alpha 2.0})))))))
