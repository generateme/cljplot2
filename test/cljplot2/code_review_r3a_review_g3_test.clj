(ns cljplot2.code-review-r3a-review-g3-test
  "Code review round 3, batch A, review fixes of the line, strip and statistic charts: `:nice nil` as absent, format
  strings checked by the schema, option conflicts as schema rules, one mapping error shape in the user's dimensions,
  the loess degree, band runs upper first, renamed style keys."
  (:require [clojure.test :refer [deftest testing is]]
            [fastmath.vector :as v]
            [cljplot2.chart.legend :as chart-legend]
            [cljplot2.chart :as chart]
            [cljplot2.chart.common :as common]
            [cljplot2.chart.strip :as strip-chart]
            [cljplot2.core :as plot]
            [cljplot2.data.rows :as rows]
            [cljplot2.graphics.curve :as curve]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.test-util :refer [error error-ins]]))

(def ^:private size {:width 300 :height 240})

(defn- build [p] (plot/build p size))

(def ^:private xy [{:x 0.137 :y 1} {:x 4.2 :y 3} {:x 9.776 :y 2}])

(def ^:private levels [{:g "a" :v 1.0} {:g "a" :v 2.0} {:g "b" :v 3.0} {:g "b" :v 5.0}])

(defn- x-domain [built] (:domain (get-in built [:finalized-scales :x])))

(deftest nice-nil-is-absent-test
  (testing "polar and sliced scales: :nice nil as no :nice"
    (doseq [extra [{:coord {:kind :polar}} {:mapping {:x :x :y :y :facet :f} :facets {:scales {:x :sliced}}}]
            :let [p (merge {:data (map #(assoc % :f (if (< (:x %) 5) "l" "r")) xy) :mapping {:x :x :y :y}
                            :charts [(chart/scatter)]}
                           extra)
                  scales-of (fn [spec] (-> (build (assoc p :scales {:x spec})) :subplots))]]
      (is (= (scales-of {}) (scales-of {:nice nil})) (pr-str extra)))))

(deftest format-schemas-test
  (testing "number formats: a format string of a double or a function; checked at build"
    (is (contains? (error-ins #(build {:data levels :mapping {:x :v :y :g} :charts [(chart/bar {:stat :mean :value-label true :value-format "%d"})]}))
                   [:charts 0 :options :value-format]))
    (is (some? (build {:data levels :mapping {:x :v :y :g} :charts [(chart/bar {:stat :mean :value-label true :value-format "%.1f"})]})))
    (is (contains? (error-ins #(build {:data xy :mapping {:x :x :y :y} :charts [(chart/pairwise {:value-format "%d"})]}))
                   [:charts 0 :options :value-format]))
    (is (some? (build {:data xy :mapping {:x :x :y :y} :charts [(chart/pairwise {:value-format str})]}))))
  (testing "count formats: a format string of a long"
    (is (contains? (error-ins #(build {:data levels :mapping {:x :v :y :g} :charts [(chart/box {:n-label true :n-format "%.1f"})]}))
                   [:charts 0 :options :n-format]))
    (is (contains? (error-ins #(build {:data levels :mapping {:x :v :y :g} :charts [(chart/box {:n-format "%d %d"})]}))
                   [:charts 0 :options :n-format]))
    (is (some? (build {:data levels :mapping {:x :v :y :g} :charts [(chart/box {:n-label true :n-format "n=%d"})]})))))

(deftest option-rules-test
  (testing "line arrows and end gaps need a plain stroked line (schema rule at the given key)"
    (doseq [[options k] [[{:arrow {:end true} :fill-gradient :viridis} :arrow]
                         [{:start-gap 3 :position :stack} :start-gap]
                         [{:end-gap 0 :fill [255 0 0]} :end-gap]]]
      (is (contains? (error-ins #(build {:data xy :mapping {:x :x :y :y} :charts [(chart/line options)]})) [:charts 0 :options k])
          (pr-str options)))
    (doseq [options [{:arrow {:end true}} {:arrow {:end true} :fill :none} {:start-gap 2}]]
      (is (some? (build {:data xy :mapping {:x :x :y :y} :charts [(chart/line options)]})) (pr-str options))))
  (testing "dotplot :origin needs :method :histogram"
    (is (contains? (error-ins #(build {:data levels :mapping {:x :v} :charts [(chart/dotplot {:origin 0})]})) [:charts 0 :options :origin]))
    (is (some? (build {:data levels :mapping {:x :v} :charts [(chart/dotplot {:method :histogram :origin 0})]})))
    (is (some? (build {:data levels :mapping {:x :v} :charts [(chart/dotplot {:method :dot-density})]})))))

(deftest mapping-error-shape-test
  (testing "check-mapped!: the dimension in :in, the row count; empty rows and one valid value pass"
    (is (= {:key :mapping :in [:mapping :y] :rows 2}
           (error #(rows/check-mapped! [{:y nil} {:y ##NaN}] [[:y :y]]))))
    (is (nil? (rows/check-mapped! [] [[:y :y]])))
    (is (nil? (rows/check-mapped! [{:y nil} {:y 1}] [[:y :y]]))))
  (testing "fail-mapping! merges the details"
    (is (= {:key :mapping :in [:mapping :x] :value :xx} (error #(rows/fail-mapping! "m" :x {:value :xx})))))
  (testing "strip charts name the user's dimension, levels on either axis"
    (is (= [:mapping :y] (:in (error #(build {:data levels :mapping {:x :g :y :typo} :charts [(chart/box)]})))))
    (is (= [:mapping :x] (:in (error #(build {:data levels :mapping {:x :typo :y :g} :charts [(chart/box)]})))))
    (is (= [:mapping :y] (:in (error #(build {:data levels :mapping {:x :g} :charts [(chart/bar {:stat :sum})]}))))
        "values unmapped, levels on :x - reported on :y"))
  (testing "stacked lines: mixed x kinds or repeated x as mapping errors"
    (is (= [:mapping :x] (:in (error #(build {:data [{:x 1 :y 1 :g "a"} {:x 1 :y 2 :g "a"} {:x 2 :y 1 :g "a"}]
                                               :mapping {:x :x :y :y :color :g} :charts [(chart/line {:position :stack})]})))))))

(deftest loess-degree-test
  (let [p (fn [params] {:data (for [i (range 30)] {:x i :y (* i (Math/sin i))}) :mapping {:x :x :y :y}
                        :charts [(chart/smooth {:method :loess :method-params params})]})]
    (doseq [d [1 2 1.0 2.0]]
      (is (some? (build (p {:degree d}))) (str d)))
    (doseq [d [0 3 1.5 -1]]
      (is (contains? (error-ins #(build (p {:degree d}))) [:charts 0 :options :method-params]) (str d)))))

(deftest band-runs-test
  (let [identity-projection (fn [x y] (v/vec2 x y))]
    (testing "upper bound first, as the result"
      (let [[[upper lower]] (curve/band-runs [0 1] [5 6] [1 2] identity-projection {})]
        (is (= [5.0 6.0] (mapv second upper)))
        (is (= [1.0 2.0] (mapv second lower)))))
    (testing "empty, one pair, a break"
      (is (= [] (curve/band-runs [] [] [] identity-projection {})))
      (is (= [] (curve/band-runs [0] [1] [0] identity-projection {})))
      (is (= 1 (count (curve/band-runs [0 1 2 3] [1 1 nil 1] [0 0 0 0] identity-projection {})))))))

(deftest renamed-keys-test
  (testing "bullet :measure-fraction, bridge :alpha-factor, line :stroke-alpha-factor; old keys rejected"
    (let [bullet-rows [{:ranges [2 4] :measures [3] :markers [3.5]}]
          bullet (fn [o] {:data bullet-rows :mapping {:ranges :ranges :measures :measures :markers :markers} :charts [(chart/bullet o)]})]
      (is (seq (error-ins #(build (bullet {:measure-width 0.3})))))
      (is (some? (build (bullet {:measure-fraction 0.3})))))
    (is (seq (error-ins #(build {:data xy :mapping {:x :x :y :y} :charts [(chart/line {:gaps :bridge :bridge {:alpha 0.5}})]}))))
    (is (some? (build {:data xy :mapping {:x :x :y :y} :charts [(chart/line {:gaps :bridge :bridge {:alpha-factor 0.5}})]})))
    (is (seq (error-ins #(build {:data xy :mapping {:x :x :y :y} :charts [(chart/line {:stroke-alpha 0.5})]}))))
    (is (some? (build {:data xy :mapping {:x :x :y :y} :charts [(chart/line {:stroke-alpha-factor 0.5})]}))))
  (testing "point legends named by their stroke; draw-end-labels! public; width 0 outlines invisible"
    (is (fn? chart-legend/stroked-point-legend))
    (is (fn? chart-legend/point-legend))
    (is (not (:private (meta #'strip-chart/draw-end-labels!))))
    (is (not (draw/visible-stroke? :black 0)))
    (is (draw/visible-stroke? :black 0.5))
    (is (some? (plot/render {:data levels :mapping {:x :v :y :g} :charts [(chart/violin {:stroke-width 0}) (chart/box {:stroke-width 0})]} size)))))
