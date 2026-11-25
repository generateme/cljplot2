(ns cljplot2.batch-c3-nb-test
  "Batch C3 (fork nb): the histogram's `:invalid :show` - invalid x values counted in a band beside the x axis's start
  (`cljplot2.content.invalid-band`, band tracks of `cljplot2.layout.plot-grid`)."
  (:require [clojure.test :refer [deftest testing is]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.content.invalid-band :as invalid-band]
            [cljplot2.data.facet :as facet]
            [cljplot2.test-util :refer [error error-ins same-pixels?]]))

(def ^:private rows
  "30 rows, every fifth x missing (nil, NaN or a string), groups a / b, facets 0 / 1."
  (vec (for [i (range 30)]
         {:x (if (zero? (mod i 5)) ([nil ##NaN "n/a"] (mod i 3)) (double (mod (* i 7) 11)))
          :g (if (even? i) "a" "b") :f (mod i 2)})))

(def ^:private valid-rows (filterv #(number? (:x %)) (remove #(and (number? (:x %)) (Double/isNaN (:x %))) rows)))

(def ^:private opts {:width 400 :height 300})

(defn- built [p] (plot/build p opts))
(defn- stats [p] (first (:stats (built p))))
(defn- items [p kind] (filter #(= kind (get-in % [:content :kind])) (:items (:grid (built p)))))
(defn- track-ids [p axis] (set (map :id (get-in (built p) [:grid axis]))))
(defn- band-bars [p panel] (get-in (stats p) [:invalid-bands :x :data panel :bars]))

(defn- histogram-plot
  "A histogram of `:x` over `data` with chart options `options` and plot keys `extra`."
  [data options & [extra]]
  (merge {:data data :mapping {:x :x} :charts [(chart/histogram (merge {:bins 5 :y :count} options))]} extra))

(deftest default-drops-invalid
  (testing "default :drop - invalid rows counted in :dropped, no band"
    (let [p (histogram-plot rows nil)]
      (is (= 6 (get-in (stats p) [:dropped :x])))
      (is (nil? (:invalid-bands (stats p))))
      (is (empty? (items p :invalid-band)))))
  (testing ":invalid :drop given equals the default image"
    (is (same-pixels? (histogram-plot rows nil) (histogram-plot rows {:invalid :drop}) opts))))

(deftest show-without-invalid-rows
  (testing "no invalid rows: no band and the same image"
    (is (nil? (:invalid-bands (stats (histogram-plot valid-rows {:invalid :show})))))
    (is (same-pixels? (histogram-plot valid-rows nil) (histogram-plot valid-rows {:invalid :show}) opts))))

(deftest show-one-and-many
  (testing "one invalid row - a band bar of 1"
    (let [p (histogram-plot (conj valid-rows {:x nil}) {:invalid :show})]
      (is (= [[0.0 1.0]] (map (juxt :y0 :y1) (band-bars p facet/single-panel))))
      (is (zero? (get-in (stats p) [:dropped :x])))))
  (testing "many - nil, NaN and strings counted, not dropped"
    (let [p (histogram-plot rows {:invalid :show})
          s (stats p)]
      (is (= 0 (get-in s [:dropped :x])))
      (is (= "NA" (get-in s [:invalid-bands :x :label])))
      (is (= [6.0] (map :y1 (band-bars p facet/single-panel))))
      (is (= 1 (count (items p :invalid-band))))
      (is (= 1 (count (items p :invalid-band-axis))))
      (is (every? (track-ids p :columns) [:invalid-band :invalid-band-gap :invalid-band-inner]))
      (is (= :invalid-band-inner (:column (first (filter #(= :x-axis-start (:id %)) (:items (:grid (built p))))))))))
  (testing "values outside explicit breaks stay dropped"
    (is (= 1 (get-in (stats (histogram-plot (conj rows {:x 100.0}) {:invalid :show :bins [0 5 10]})) [:dropped :x]))))
  (testing "the y extent covers the band bar"
    (let [many-missing (into valid-rows (repeat 50 {:x nil}))]
      (is (<= 50.0 (get-in (stats (histogram-plot many-missing {:invalid :show})) [:extents facet/single-panel :y :max]))))))

(deftest show-measures
  (testing ":percent shares of all rows, band included (sum 100)"
    (let [s (stats (histogram-plot rows {:invalid :show :y :percent}))
          bins (get-in s [:data facet/single-panel :bars])
          band (get-in s [:invalid-bands :x :data facet/single-panel :bars])]
      (is (< 99.999 (reduce + (map :y1 (concat bins band))) 100.001))))
  (testing ":density / :cumulative refused by the schema"
    (is (contains? (error-ins #(built (histogram-plot rows {:invalid :show :y :density}))) [:charts 0 :options :invalid]))
    (is (contains? (error-ins #(built (histogram-plot rows {:invalid :show :y :cumulative}))) [:charts 0 :options :invalid])))
  (testing "wrong types"
    (is (contains? (error-ins #(built (histogram-plot rows {:invalid :yes}))) [:charts 0 :options :invalid]))
    (is (contains? (error-ins #(built (histogram-plot rows {:invalid :show :invalid-label 3}))) [:charts 0 :options :invalid-label]))))

(deftest show-groups
  (testing ":stack - band bars stacked"
    (let [bars (band-bars (histogram-plot rows {:invalid :show :position :stack} {:mapping {:x :x :color :g}})
                          facet/single-panel)]
      (is (= [[0.0 3.0] [3.0 6.0]] (map (juxt :y0 :y1) bars)))
      (is (every? #(= [0.0 1.0] [(:x0 %) (:x1 %)]) bars))))
  (testing ":dodge - the band split among the groups"
    (is (= [[0.0 0.5] [0.5 1.0]]
           (map (juxt :x0 :x1) (band-bars (histogram-plot rows {:invalid :show :position :dodge} {:mapping {:x :x :color :g}})
                                          facet/single-panel)))))
  (testing ":mirror - the second group downward"
    (is (= [[0.0 3.0] [-3.0 0.0]]
           (map (juxt :y0 :y1) (band-bars (histogram-plot rows {:invalid :show :position :mirror} {:mapping {:x :x :color :g}})
                                          facet/single-panel))))))

(deftest show-flip-and-facets
  (testing "flip - the band is a row under the panel, its axis in the y-axis column"
    (let [p (histogram-plot rows {:invalid :show} {:flip true})]
      (is (every? (track-ids p :rows) [:invalid-band :invalid-band-gap :invalid-band-inner]))
      (is (not (contains? (track-ids p :columns) :invalid-band)))
      (is (= [:y-axis :invalid-band] ((juxt :column :row) (first (items p :invalid-band-axis)))))))
  (testing "facets - a band per panel, each with its own count"
    (let [p (histogram-plot rows {:invalid :show} {:mapping {:x :x :facet :f}})]
      (is (= 2 (count (items p :invalid-band))))
      (is (= 2 (count (items p :invalid-band-axis))))
      (is (= [3.0 3.0] (map #(:y1 (first (band-bars p %))) [0 1])))))
  (testing "facets flipped - band rows per panel row"
    (let [p (histogram-plot rows {:invalid :show} {:mapping {:x :x :facet :f} :flip true :facets {:columns 1}})]
      (is (every? (track-ids p :rows) [[:invalid-band 0] [:invalid-band 1]]))))
  (testing "renders: single, flipped, facets, free facet scales, stacked, log stat space"
    (doseq [p [(histogram-plot rows {:invalid :show})
               (histogram-plot rows {:invalid :show :invalid-label "missing"} {:flip true})
               (histogram-plot rows {:invalid :show} {:mapping {:x :x :facet :f} :facets {:scales {:y :free}}})
               (histogram-plot rows {:invalid :show :position :stack} {:mapping {:x :x :color :g}})
               (histogram-plot (conj rows {:x 0.0}) {:invalid :show :stat-space :scale}
                               {:scales {:x {:kind :log}}})]]
      (is (some? (plot/render-image p opts))))))

(deftest show-boundaries
  (testing "log x in :stat-space :scale - values the axis can't place go to the band"
    (let [p (histogram-plot (into (filterv #(pos? (:x %)) valid-rows) [{:x 0.0} {:x -1.0} {:x nil}]) {:invalid :show :stat-space :scale}
                            {:scales {:x {:kind :log}}})]
      (is (= [3.0] (map :y1 (band-bars p facet/single-panel))))))
  (testing "all rows invalid still throw {:key :mapping}"
    (is (= :mapping (:key (error #(built (histogram-plot [{:x nil} {:x "a"}] {:invalid :show})))))))
  (testing "ridgelines refuse :invalid :show"
    (is (= :invalid (:key (error #(built (histogram-plot rows {:invalid :show} {:mapping {:x :x :y :g}}))))))))

(deftest band-scale-test
  (testing "the band scale - [0 1] padded, one tick at the centre labelled, no grid"
    (let [s (invalid-band/band-scale {:dim :x :direction :horizontal :label "null"} 30)]
      (is (= ["null"] (:tick-labels s)))
      (is (= [0.5] (:ticks s)))
      (is (= :no-grid (:axis s)))
      (is (= :horizontal (:direction s))))
    (is (= [30.0 0.0] (:range (invalid-band/band-scale {:dim :x :direction :vertical :label "NA"} 30))))))
