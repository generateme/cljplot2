(ns cljplot2.batch-c3-fix-z2-test
  "Batch C3 review fixes (fork z2): histogram bins per panel-local x scale (main and margin histograms), invalid-value
  band rules (`:invalid :show` in margins / custom grids, `:invalid-label` needs `:show`), the band's colour, theme
  `[:panel :invalid-band-size]` and a band beside a left margin."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.color :as c]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.facet :as facet]
            [cljplot2.test-util :refer [error error-ins]]))

(def ^:private opts {:width 600 :height 400})

(def ^:private rows
  "Three facets with very different x ranges: f 0 - [0 1], f 1 - [10 20], f 2 - [100 400]; every seventh x missing."
  (vec (for [f (range 3) i (range 40)]
         {:f f :g (if (even? i) "a" "b") :y (double i)
          :x (when-not (zero? (mod i 7))
               (case (long f) 0 (/ i 39.0) 1 (+ 10.0 (/ i 3.9)) (+ 100.0 (* 300.0 (/ i 39.0)))))})))

(defn- built [p] (plot/build p opts))
(defn- stats [p] (first (:stats (built p))))

(defn- histogram-plot
  "A histogram of `:x` faceted by `:f` with chart options `options` and plot keys `extra`."
  [options & [extra]]
  (merge {:data rows :mapping {:x :x :facet :f} :charts [(chart/histogram (merge {:bins 5 :y :count} options))]} extra))

(defn- span
  "The `[first-x0 last-x1]` of a panel's bars."
  [bars]
  [(:x0 (first bars)) (:x1 (last bars))])

(defn- panel-range
  "The `[min max]` of a facet's valid x values."
  [f]
  (let [xs (keep #(when (= f (:f %)) (:x %)) rows)]
    [(apply min xs) (apply max xs)]))

(deftest free-scale-bins-per-panel
  (testing ":same scales - one set of breaks for every panel"
    (let [s (stats (histogram-plot nil))]
      (is (apply = (map #(span (get-in s [:data % :bars])) [0 1 2])))))
  (testing ":free x - each panel binned over its own range (a count per panel)"
    (let [s (stats (histogram-plot nil {:facets {:scales {:x :free}}}))]
      (doseq [f [0 1 2]]
        (is (= (panel-range f) (span (get-in s [:data f :bars]))))
        (is (= 5 (count (get-in s [:data f :bars]))))
        (is (= [(panel-range f)] (map (juxt :min :max) [(get-in s [:extents f :x])]))))))
  (testing ":sliced x - per panel too"
    (let [s (stats (histogram-plot nil {:facets {:scales {:x :sliced}}}))]
      (is (= (panel-range 2) (span (get-in s [:data 2 :bars]))))))
  (testing "a method is estimated per panel"
    (let [s (stats (histogram-plot {:bins :sturges} {:facets {:scales {:x :free}}}))]
      (is (= (panel-range 1) (span (get-in s [:data 1 :bars]))))))
  (testing "explicit breaks stay shared"
    (let [s (stats (histogram-plot {:bins [0 1 10 20]} {:facets {:scales {:x :free}}}))]
      (is (apply = (map #(span (get-in s [:data % :bars])) [0 1])))
      (is (= 52 (get-in s [:dropped :x])))))
  (testing "free y only - x bins stay shared"
    (let [s (stats (histogram-plot nil {:facets {:scales {:y :free}}}))]
      (is (apply = (map #(span (get-in s [:data % :bars])) [0 1 2])))))
  (testing "flip with free x - still per panel (x drawn vertically)"
    (let [s (stats (histogram-plot nil {:flip true :facets {:scales {:x :free}}}))]
      (is (= (panel-range 2) (span (get-in s [:data 2 :bars]))))))
  (testing "facet grid - panels of one column share their breaks"
    (let [s (stats (histogram-plot nil {:mapping {:x :x :facet-col :f :facet-row :g} :facets {:scales {:x :free}}}))
          spans (group-by first (map (fn [[k d]] [(first k) (span (:bars d))]) (:data s)))]
      (is (every? #(apply = (map second %)) (vals spans)))
      (is (= 3 (count (distinct (map second (apply concat (vals spans)))))))))
  (testing "ridgelines under free x - per panel"
    (let [s (stats (histogram-plot nil {:mapping {:x :x :y :g :facet :f} :facets {:scales {:x :free}}}))
          bars (fn [f] (mapcat :bars (get-in s [:data f :ridges])))]
      (is (= (first (panel-range 2)) (apply min (map first (bars 2)))))
      (is (= (second (panel-range 0)) (apply max (map second (bars 0))))))))

(deftest free-scale-margin-histograms
  (let [margin-plot (fn [extra] (merge {:data rows :mapping {:x :x :y :y :facet :f} :charts [(chart/scatter)]
                                        :margins {:top (chart/histogram {:bins 5}) :right (chart/histogram {:bins 4})}}
                                       extra))
        margin-bars (fn [p side f] (get-in (built p) [:margin-stats side :stats 0 :data f :bars]))]
    (testing ":same scales - shared breaks"
      (let [p (margin-plot nil)]
        (is (= (span (margin-bars p :top 0)) (span (margin-bars p :top 2))))))
    (testing ":free x - the top margin bins over its panel's x range; the right one (y, :same) stays shared"
      (let [p (margin-plot {:facets {:scales {:x :free}}})]
        (doseq [f [0 1 2]] (is (= (panel-range f) (span (margin-bars p :top f)))))
        (is (= (span (margin-bars p :right 0)) (span (margin-bars p :right 2))))))
    (testing "renders"
      (is (some? (plot/render-image (margin-plot {:facets {:scales {:x :free}}}) {:width 800 :height 450}))))))

(deftest invalid-show-rules
  (let [base {:data [{:x 1.0} {:x nil}] :mapping {:x :x :y :x} :charts [(chart/scatter)]}
        h (chart/histogram {:invalid :show})]
    (testing "a margin chart - every margin form, keyed :invalid"
      (is (= :invalid (:key (error #(built (assoc base :margins {:top h}))))))
      (is (contains? (error-ins #(built (assoc base :margins {:top h}))) [:margins :top :options :invalid]))
      (is (contains? (error-ins #(built (assoc base :margins {:left [(chart/rug) h]}))) [:margins :left 1 :options :invalid]))
      (is (contains? (error-ins #(built (assoc base :margins {:right {:charts h}}))) [:margins :right :charts :options :invalid]))
      (is (contains? (error-ins #(built (assoc base :margins {:bottom {:charts [h]}}))) [:margins :bottom :charts 0 :options :invalid])))
    (testing ":drop and the default are fine in margins"
      (is (some? (built (assoc base :margins {:top (chart/histogram {:invalid :drop}) :right (chart/histogram)})))))
    (testing "a custom grid"
      (is (contains? (error-ins #(built {:data [{:x 1.0} {:x nil}] :mapping {:x :x} :charts [h]
                                         :custom-grid {:columns [{:id :a :size [:flexible]}] :rows [{:id :a :size [:flexible]}]
                                                       :items []}}))
                     [:charts 0 :options :invalid])))))

(deftest invalid-label-rule
  (let [p (fn [options] {:data [{:x 1.0} {:x 2.0} {:x nil}] :mapping {:x :x} :charts [(chart/histogram options)]})]
    (testing ":invalid-label without :invalid :show"
      (is (= :invalid-label (:key (error #(built (p {:invalid-label "x"}))))))
      (is (= :invalid-label (:key (error #(built (p {:invalid :drop :invalid-label "x"})))))))
    (testing "nil label, or with :show - fine"
      (is (some? (built (p {:invalid-label nil}))))
      (is (= "x" (get-in (stats (p {:invalid :show :invalid-label "x"})) [:invalid-bands :x :label]))))))

(deftest ridgeline-error-in
  (is (= [:invalid] (:in (error #(built (histogram-plot {:invalid :show} {:mapping {:x :x :y :g}})))))))

(deftest band-colour
  (let [band-style (fn [p] (get-in (stats p) [:invalid-bands :x :data facet/single-panel :style]))
        single (fn [options & [mapping]] {:data rows :mapping (merge {:x :x} mapping) :charts [(chart/histogram (merge {:invalid :show} options))]})]
    (testing "the chart's own fill"
      (is (= (c/to-color [200 0 0]) (c/to-color (:color (band-style (single {:color [200 0 0]})))))))
    (testing "the theme's histogram fill by default, not the NA colour"
      (is (= (c/to-color (:color (get-in (stats (single nil)) [:data facet/single-panel :style])))
             (c/to-color (:color (band-style (single nil)))))))
    (testing "two charts - each band in its own colour, the first one's label"
      (let [s (:stats (built {:data rows :mapping {:x :x}
                              :charts [(chart/histogram {:invalid :show :invalid-label "first"})
                                       (chart/histogram {:invalid :show :invalid-label "second" :color [200 0 0]})]}))
            labels (filter #(= :invalid-band-axis (get-in % [:content :kind]))
                           (:items (:grid (built {:data rows :mapping {:x :x}
                                                  :charts [(chart/histogram {:invalid :show :invalid-label "first"})
                                                           (chart/histogram {:invalid :show :invalid-label "second"})]}))))]
        (is (not= (c/to-color (get-in (first s) [:invalid-bands :x :data facet/single-panel :style :color]))
                  (c/to-color (get-in (second s) [:invalid-bands :x :data facet/single-panel :style :color]))))
        (is (= ["first"] (map #(get-in % [:content :label]) labels)))))))

(deftest band-size-theme-key
  (let [p {:data rows :mapping {:x :x} :charts [(chart/histogram {:invalid :show})]}
        band-width (fn [theme] (let [b (plot/build (assoc p :theme theme) opts)
                                     i (first (keep-indexed #(when (= :invalid-band (:id %2)) %1) (get-in b [:grid :columns])))]
                                 (nth (map :size (get-in b [:layout :columns])) i)))]
    (testing "[:panel :invalid-band-size] sets the band's width (at least the label's)"
      (is (= 60.0 (double (band-width {:panel {:invalid-band-size 60}}))))
      (is (< 0.0 (double (band-width {:panel {:invalid-band-size 0}})) 60.0)))
    (testing "the old [:plot :invalid-band-size] is unknown"
      (is (contains? (error-ins #(plot/build (assoc p :theme {:plot {:invalid-band-size 60}}) opts)) [:theme :plot :invalid-band-size])))))

(deftest band-beside-left-margin
  (let [p {:data rows :mapping {:x :x :y :y} :charts [(chart/histogram {:invalid :show})]
           :margins {:left (chart/rug)}}
        b (built p)
        columns (mapv :id (get-in b [:grid :columns]))
        at (fn [id] (.indexOf ^java.util.List columns id))]
    (testing "column order: y axis, band, gap, inner track, left margin, panel"
      (is (< (at :y-axis) (at :invalid-band) (at :invalid-band-gap) (at :invalid-band-inner) (at :left-margin) (at :panel))))
    (testing "the x axis's start overhang sits in the margin's inner track, the band's inner track holds nothing"
      (is (= :left-margin-inner (:column (first (filter #(= :x-axis-start (:id %)) (:items (:grid b)))))))
      (is (not-any? #(= :invalid-band-inner (:column %)) (:items (:grid b)))))
    (testing "renders"
      (is (some? (plot/render-image p opts))))))
