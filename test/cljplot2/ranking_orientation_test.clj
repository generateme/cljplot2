(ns cljplot2.ranking-orientation-test
  "Strip charts oriented by the mapping: the level axis (`:level-axis`), the transpose of input, stats and scales,
  equivalence with flipped plots."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is are]]
            [fastmath.random :as r]
            [cljplot2.test-util :refer [error]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.levels :as levels]
            [cljplot2.data.strip :as strip])
  (:import [java.awt.image BufferedImage]
           [java.time LocalDate]))

(def ^:private size {:width 400 :height 300})
(defn- build [p] (plot/build p size))
(defn- stats [p] (first (:stats (build p))))
(defn- level-axis [p] (:level-axis (stats p)))

(def ^:private rows
  (vec (for [c ["a" "b" "c"] g ["p" "q"] i (range 5)]
         {:c c :g g :n (count c) :v (+ i (count c) (if (= g "q") 2 0) (* 0.3 i))})))

(deftest level-axis-resolution
  (testing "1.1 the option: schema, and it always wins"
    (is (= :level-axis (:key (error #(build {:data rows :mapping {:x :c :y :v} :charts [(chart/bar {:level-axis :z})]})))))
    (is (= :x (level-axis {:data rows :mapping {:x :c :y :g} :charts [(chart/bar {:stat :count :level-axis :x})]})))
    (is (= :y (level-axis {:data rows :mapping {:x :c :y :g} :charts [(chart/bar {:stat :count :level-axis :y})]})))
    (is (= :y (level-axis {:data rows :mapping {:x :v :y :c} :charts [(chart/box {:level-axis :y})]}))))
  (testing "1.2 from the mapping"
    (are [mapping chart axis] (= axis (level-axis {:data rows :mapping mapping :charts [chart]}))
      {:x :c :y :v} (chart/bar) :x                                ; 1.2.1
      {:x :c} (chart/bar {:stat :count}) :x                        ; 1.2.1, y absent
      {:x :v :y :c} (chart/bar) :y                                 ; 1.2.2
      {:y :c} (chart/bar {:stat :count}) :y                        ; 1.2.2, x absent
      {:x :v} (chart/box) :y                                       ; 1.2.3
      {:y :v} (chart/box) :x                                       ; 1.2.4
      {} (chart/bar {:stat :count}) :y                             ; 1.2.5
      {:x (levels/categorical :n) :y :v} (chart/box) :x            ; a categorical marker
      {:x :v :y (levels/categorical :n)} (chart/box) :y))
  (testing "1.2 plot declarations and temporal values"
    (is (= :x (level-axis {:data rows :mapping {:x :n :y :v} :scales {:x {:type :categorical}} :charts [(chart/box)]})))
    (is (= :y (strip/level-axis {} {:x :d :y :c} (map #(assoc % :d (LocalDate/of 2020 1 (inc (long (:n %))))) rows) {}))
        "dates aren't levels")
    (is (= :x (strip/level-axis {} {:x :c :y :d} (map #(assoc % :d (LocalDate/of 2020 1 (inc (long (:n %))))) rows) {}))))
  (testing "1.3 ambiguous mappings"
    (are [mapping chart] (= :level-axis (:key (error #(build {:data rows :mapping mapping :charts [chart]}))))
      {:x :c :y :g} (chart/bar {:stat :count})
      {:x :v :y :v} (chart/box)
      {:x :n :y :v} (chart/bar))
    (let [msg (ex-message (try (build {:data rows :mapping {:x :v :y :v} :charts [(chart/box)]}) (catch Exception e e)))]
      (is (str/includes? msg ":level-axis"))
      (is (str/includes? msg "categorical"))))
  (testing "1.3 an axis without values decides nothing: the strip frame reports it"
    (is (= :mapping (:key (error #(build {:data (map (fn [r] (assoc r :c nil)) rows) :mapping {:x :v :y :c} :charts [(chart/box)]})))))
    (is (empty? (:bars (first (vals (:data (stats {:data rows :mapping {:x :c :y :v} :charts [(chart/bar {:data []})]})))))))
    (is (= :mapping (:key (error #(build {:data rows :mapping {:x :c} :charts [(chart/box)]}))))
        "levels without values")
    (is (= :mapping (:key (error #(build {:data (map (fn [r] (assoc r :w ##NaN)) rows) :mapping {:x :v :y :w} :charts [(chart/box)]}))))
        "T4 only NaN on :y: not numeric - the strip frame reports the missing levels")
    (is (str/includes? (ex-message (try (build {:data (map (fn [r] (assoc r :v "x")) rows) :mapping {:x :v :y :c} :charts [(chart/box)]})
                                        (catch Exception e e)))
                       "values are numbers")
        "T2 both categorical: the message points at non-numeric values too")
    (is (= :mapping (:key (error #(build {:data rows :mapping {:color :g} :charts [(chart/box)]}))))
        "no values axis mapped (schema)"))
  (testing "1.4 one level axis for all facets"
    (let [s (stats {:data rows :mapping {:x :c :y :v :facet :g} :charts [(chart/bar)]})]
      (is (= :x (:level-axis s)))
      (is (= 2 (count (:data s))))
      (is (every? #(= {:kind :categorical :values ["a" "b" "c"]} (select-keys (:x %) [:kind :values])) (vals (:extents s)))))))

(deftest transposed-stats
  (testing "2.1 extents, titles, padding, dropped, tick format on the exchanged dimensions"
    (let [s (stats {:data (conj rows {:c "a" :g "p" :v nil}) :mapping {:x :c :y :v} :charts [(chart/bar {:stat :mean})]})
          e (first (vals (:extents s)))]
      (is (= :categorical (get-in e [:x :kind])))
      (is (= :numeric (get-in e [:y :kind])))
      (is (= {:y "mean of v"} (:axis-titles s)) "3.2")
      (is (contains? (:padding s) :y))
      (is (= 1 (get-in s [:dropped :y]))))
    (is (= {:y :absolute} (:tick-format (stats {:data rows :mapping {:x :c :y :v :color :g} :charts [(chart/bar {:mirror :color})]})))))
  (testing "2.1.2 the :padding option follows the orientation"
    (let [hi (fn [opts] (second (get-in (build {:data rows :mapping {:x :c :y :v} :charts [(chart/bar opts)]}) [:finalized-scales :y :domain])))]
      (is (> (double (hi {:padding {:y [0 0.5]}})) (double (hi {})))))))

(def ^:private plain
  {:axis {:x {:tick-spacing 60} :y {:tick-spacing 60}} :panel {:grid {:x :none :y :none}}})

(defn- pixels [p]
  (let [^BufferedImage img (plot/render-image (assoc p :theme plain) size)]
    (vec (.getRGB img 0 0 400 300 nil 0 400))))

(deftest equivalence-with-flip
  (testing "2.4 levels on :x = levels on :y flipped (byte-identical; grid off - crossing grid lines draw in key order)"
    (doseq [[n charts] [["bar" [(chart/bar)]]
                        ["dodged bars + error bars" [(chart/bar {:mapping {:color :g} :stat :mean})
                                                     (chart/extent {:mapping {:color nil :group :g} :method :stddev :color :black})]]
                        ["stacked, labels" [(chart/bar {:mapping {:color :g} :position :stack :value-label true})]]
                        ["normalized" [(chart/bar {:mapping {:color :g} :position :normalize})]]
                        ["mirror" [(chart/bar {:mapping {:color :g} :mirror :color :value-label true})]]
                        ["varwidth, n labels" [(chart/bar {:stat :mean :varwidth true :n-label true})]]
                        ["box" [(chart/box)]]
                        ["violin" [(chart/violin)]]
                        ["extent" [(chart/extent)]]
                        ["stripplot" [(chart/stripplot {:rng (r/rng :mersenne 3)})]]
                        ["beeswarm" [(chart/beeswarm)]]
                        ["rug" [(chart/rug)]]]]
      (is (= (pixels {:data rows :mapping {:x :c :y :v} :charts charts})
             (pixels {:data rows :mapping {:x :v :y :c} :flip true :charts charts}))
          (str n ": vertical"))
      (is (= (pixels {:data rows :mapping {:x :c :y :v} :flip true :charts charts})
             (pixels {:data rows :mapping {:x :v :y :c} :charts charts}))
          (str n ": horizontal"))))
  (testing "2.4.3 polar bars"
    (is (= (pixels {:data rows :mapping {:x :c :y :v :color :g} :coord {:kind :polar} :charts [(chart/bar {:position :stack})]})
           (pixels {:data rows :mapping {:x :v :y :c :color :g} :flip true :coord {:kind :polar} :charts [(chart/bar {:position :stack})]}))))
  (testing "2.4.4 facets"
    (is (= (pixels {:data rows :mapping {:x :c :y :v :facet :g} :charts [(chart/box)]})
           (pixels {:data rows :mapping {:x :v :y :c :facet :g} :flip true :charts [(chart/box)]})))))

(deftest charts-and-legends
  (testing "2.3 legends of vertical grouped bars"
    (let [b (build {:data rows :mapping {:x :c :y :v :color :g} :charts [(chart/bar)]})
          legend (some #(when (= :legends (:id %)) (get-in % [:content :legends])) (get-in b [:grid :items]))]
      (is (= 2 (count (:entries (first legend)))))))
  (testing "3.3 a count over a categorical :x: a vertical bar per level"
    (let [s (stats {:data rows :mapping {:x :c} :charts [(chart/bar {:stat :count})]})]
      (is (= :x (:level-axis s)))
      (is (= 3 (count (:bars (first (vals (:data s)))))))
      (is (= ["a" "b" "c"] (get-in (first (vals (:extents s))) [:x :values])))))
  (testing "3.4 the strip level message names the level axis"
    (is (str/includes? (ex-message (try (build {:data rows :mapping {:x :v :y :n} :charts [(chart/box {:level-axis :y})]})
                                        (catch Exception e e)))
                       ":level-axis")))
  (testing "every strip chart takes :level-axis"
    (doseq [c [(chart/bar {:level-axis :x}) (chart/box {:level-axis :x}) (chart/violin {:level-axis :x})
               (chart/stripplot {:level-axis :x}) (chart/beeswarm {:level-axis :x}) (chart/extent {:level-axis :x})
               (chart/rug {:level-axis :x})]]
      (is (= :x (level-axis {:data rows :mapping {:x :c :y :v} :charts [c]})) (str (:kind c))))))
