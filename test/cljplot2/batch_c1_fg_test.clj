(ns cljplot2.batch-c1-fg-test
  "Batch C1, fork fg: returned data fields and internal data keys never end in `?` (renamed: `:closed`,
  `:significant`, `:member`, `:signed`, `:companion`, `:leaf`, `:box` / `:axes`, `:value-end`, `:required` ...)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [fastmath.core :as m]
            [cljplot2.data.contour :as contour]
            [cljplot2.data.field :as field]
            [cljplot2.data.pairwise :as pairwise]
            [cljplot2.data.series :as series]
            [cljplot2.data.sets :as sets]
            [cljplot2.data.space3d :as space3d]
            [cljplot2.data.strip :as strip]
            [cljplot2.data.tree :as tree]
            [cljplot2.describe :as describe]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.pipeline :as pipeline]
            [cljplot2.chart :as chart]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- question-keys
  "Gives the keyword keys ending in `?` anywhere in a value (maps nested in maps and sequences)."
  [x]
  (let [found (atom #{})]
    (walk/postwalk (fn [v] (when (map? v) (swap! found into (filter #(and (keyword? %) (.endsWith (name %) "?")) (keys v)))) v) x)
    @found))

(def ^:private cone (field/sample (fn [[x y]] (- 1.0 (m/hypot (double x) (double y)))) [[-1 1] [-1 1]] [21 21]))

(deftest isolines-keys-test
  (testing "no crossing - empty"
    (is (= [] (contour/isolines cone 2.0))))
  (testing "one closed ring: :closed true"
    (let [lines (contour/isolines cone 0.5)]
      (is (= 1 (count lines)))
      (is (true? (:closed (first lines))))
      (is (= #{:points :closed} (set (keys (first lines)))))
      (is (empty? (question-keys lines)))))
  (testing "an open line: :closed false"
    (let [saddle (field/sample (fn [[x y]] (* (double x) (double y))) [[-1 1] [-1 1]] [2 2])
          lines (contour/isolines saddle 0.5)]
      (is (every? #(false? (:closed %)) lines))
      (is (empty? (question-keys lines))))))

(deftest acf-rows-keys-test
  (testing "two values (one lag): rows with :significant, lag 0 never significant"
    (let [rs (series/acf-rows [1.0 2.0] {:lags 1})]
      (is (= [0 1] (map :lag rs)))
      (is (false? (:significant (first rs))))
      (is (every? #(contains? % :significant) rs))
      (is (empty? (question-keys rs)))))
  (testing "partial autocorrelation rows too"
    (let [rs (series/acf-rows [1 3 2 5 4 6 5 8] {:partial true})]
      (is (every? #(boolean? (:significant %)) rs))
      (is (empty? (question-keys rs))))))

(deftest membership-rows-keys-test
  (let [mapping {:set :set :item :item}]
    (testing "empty rows - no dots"
      (is (= [] (sets/membership-rows [] mapping))))
    (testing "one row - one dot, a member"
      (is (= [{:combination "A" :set "A" :member true}] (sets/membership-rows [{:set "A" :item 1}] mapping))))
    (testing "two sets: members and non-members, no ? keys"
      (let [dots (sets/membership-rows [{:set "A" :item 1} {:set "B" :item 2}] mapping)]
        (is (= #{true false} (set (map :member dots))))
        (is (empty? (question-keys dots)))))))

(deftest pairwise-registry-keys-test
  (is (empty? (question-keys pairwise/statistics)))
  (is (true? (get-in pairwise/statistics [:pearson :signed])))
  (is (pairwise/signed-statistic? :pearson))
  (is (not (pairwise/signed-statistic? :mae))))

(deftest companion-extent-test
  (testing "a strip chart at the bottom gives a companion y extent"
    (let [e (strip/extents :bottom {:p [{:level "a"}]} (constantly [0.0 1.0]))]
      (is (true? (get-in e [:p :y :companion])))
      (is (empty? (question-keys e)))))
  (testing "no items - no extents"
    (is (= {} (strip/extents :bottom {} (constantly [0.0 1.0])))))
  (testing "a companion alone gives no scale; beside a data chart the extent joins"
    (let [built (pipeline/build {:data [{:x 1 :y 2} {:x 3 :y 5}] :mapping {:x :x :y :y}
                                 :charts [(chart/scatter)]})]
      (is (empty? (question-keys (:extents built)))))))

(deftest other-internal-keys-test
  (testing "corner rounding: :value-end"
    (is (= {:radius 8.0 :value-end false} (draw/corner-rounding {:corner-radius 8})))
    (is (= {:radius 4.0 :value-end true} (draw/corner-rounding {:corner-radius 8 :corner-radius-end 4})))
    (is (nil? (draw/corner-rounding {}))))
  (testing "3D frame settings: :box and :axes"
    (let [s (space3d/frame-settings {:box false} {:x :x :y :y :z :z})]
      (is (false? (:box s)))
      (is (true? (:axes s)))
      (is (empty? (question-keys s)))))
  (testing "tree layout nodes: :leaf"
    (let [root (tree/from-levels [{:a "x" :b "p"} {:a "x" :b "q"}] {:hierarchy [:a :b]})
          {:keys [nodes]} (tree/layout root)]
      (is (= 2 (count (filter :leaf nodes))))
      (is (empty? (question-keys nodes)))))
  (testing "describe rows: :required"
    (let [rows (describe/schema-rows [:map [:a int?] [:b {:optional true} int?]])]
      (is (= [true false] (map :required rows)))
      (is (empty? (question-keys rows))))))
