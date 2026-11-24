(ns cljplot2.vega-cycle3a-test
  "Vega cycle 3a review: every chart drawn with the default stroke (no dashed grid leaking into line shapes), rect
  `:width`, signed bar value labels, histogram `:y :cumulative`."
  (:require [clojure.test :refer [deftest testing is]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.annotation :as annotation]
            [cljplot2.test-util :refer [error]])
  (:import [java.awt.image BufferedImage]))

(def ^:private red (unchecked-int 0xffff0000))

(defn- red-column
  "Rows of the first image column holding exact red pixels, as a string (`#` red, `.` other)."
  [^BufferedImage img]
  (let [red? (fn [x y] (== (int red) (.getRGB img (int x) (int y))))
        x (first (filter (fn [x] (some #(red? x %) (range (.getHeight img)))) (range (.getWidth img))))]
    (apply str (map #(if (red? x %) "#" ".") (range (.getHeight img))))))

(deftest default-stroke-test
  (testing "a stripplot tick is one solid stroke - the panel grid's dash doesn't leak into it"
    (let [img (plot/render-image {:data [{:x 1} {:x 2} {:x 3}] :mapping {:x :x}
                                  :charts [(chart/stripplot {:shape \| :size 30 :jitter 0 :color :red})]}
                                 {:width 300 :height 120})
          col (red-column img)]
      (is (re-find #"#{20,}" col) "a run of at least 20 red pixels")
      (is (= 1 (count (re-seq #"#+" col))) "one run - not dashes")))
  (testing "beeswarm outline shapes too"
    (let [img (plot/render-image {:data [{:x 1}] :mapping {:x :x}
                                  :charts [(chart/beeswarm {:shape \| :size 30 :color :red})]}
                                 {:width 300 :height 120})]
      (is (= 1 (count (re-seq #"#+" (red-column img))))))))

(defn- rect-plot
  [options]
  {:data [{:t "A" :s 1 :e 3} {:t "B" :s 3 :e 5}]
   :charts [(chart/rect (merge {:mapping {:x0 :s :x1 :e :y0 :t :y1 :t}} options))]})

(deftest rect-width-test
  (let [y (get-in (plot/build (rect-plot {}) {:width 300 :height 200}) [:finalized-scales :y])
        [lo hi] ((annotation/interval-fn y) "A" "A")
        [lo-half hi-half] ((annotation/interval-fn y 0.5) "A" "A")]
    (testing "a level covers :width of its step, centred; default the whole step"
      (is (== (* 0.5 (- (double hi) (double lo))) (- (double hi-half) (double lo-half))))
      (is (== (* 0.5 (+ (double lo) (double hi))) (* 0.5 (+ (double lo-half) (double hi-half)))))
      (is (= [lo hi] ((annotation/interval-fn y 1.0) "A" "A"))))
    (testing "two levels: from the first's band to the second's, each end narrowed"
      (let [[a b] ((annotation/interval-fn y 0.5) "A" "B")
            [a1 b1] ((annotation/interval-fn y) "A" "B")]
        (is (< (double a1) (double a) (double b) (double b1))))))
  (testing "numbers and nil ends are unaffected"
    (let [x (get-in (plot/build (rect-plot {}) {:width 300 :height 200}) [:finalized-scales :x])]
      (is (= ((annotation/interval-fn x) 1 3) ((annotation/interval-fn x 0.5) 1 3)))
      (is (= ((annotation/interval-fn x) nil 3) ((annotation/interval-fn x 0.5) nil 3)))))
  (testing "boundary: :width 1 renders as the default; 0 and above 1 are schema errors"
    (is (= (seq (.getRGB ^BufferedImage (plot/render-image (rect-plot {}) {:width 200 :height 150}) 0 0 200 150 nil 0 200))
           (seq (.getRGB ^BufferedImage (plot/render-image (rect-plot {:width 1}) {:width 200 :height 150}) 0 0 200 150 nil 0 200))))
    (is (= :width (:key (error #(plot/build (rect-plot {:width 0}))))))
    (is (= :width (:key (error #(plot/build (rect-plot {:width 1.5}))))))
    (is (some? (plot/render-image (assoc (rect-plot {:width 0.5}) :coord {:kind :polar}) {:width 200 :height 200})))))

(defn- labels
  "End label texts of the bar chart of plot `p`."
  [p]
  (keep #(get-in % [:label :text]) (get-in (plot/build p {:width 400 :height 300}) [:stats 0 :data :panel :bars])))

(deftest signed-value-label-test
  (testing "negative bars get negative labels"
    (is (= #{"-28" "55"} (set (labels {:data [{:a "A" :b -28} {:a "B" :b 55}] :mapping {:x :a :y :b}
                                       :charts [(chart/bar {:value-label true})]})))))
  (testing "stacked negative segments are negative; positive ones positive"
    (is (= #{"-3" "-4" "2"} (set (labels {:data [{:a "A" :b -3 :g "p"} {:a "A" :b -4 :g "q"} {:a "A" :b 2 :g "r"}]
                                          :mapping {:x :a :y :b :color :g}
                                          :charts [(chart/bar {:value-label true :position :stack})]})))))
  (testing "a mirror's second group keeps its positive value"
    (is (= #{"3" "4"} (set (labels {:data [{:a "A" :b 3 :g "p"} {:a "A" :b 4 :g "q"}]
                                    :mapping {:x :a :y :b :color :g}
                                    :charts [(chart/bar {:value-label true :mirror :color})]})))))
  (testing "a :value-format formats the signed value; a function can drop the sign"
    (is (= #{"-28.0"} (set (labels {:data [{:a "A" :b -28}] :mapping {:x :a :y :b}
                                    :charts [(chart/bar {:value-label true :value-format "%.1f"})]}))))
    (is (= #{"28"} (set (labels {:data [{:a "A" :b -28}] :mapping {:x :a :y :b}
                                 :charts [(chart/bar {:value-label #(str (Math/round (Math/abs (double (:value %)))))})]}))))))

(defn- histogram-bars
  [rows mapping options]
  (get-in (plot/build {:data rows :mapping mapping :charts [(chart/histogram options)]} {:width 400 :height 300})
          [:stats 0]))

(deftest cumulative-histogram-test
  (let [rows (map (fn [v] {:v v}) [1 1 2 3 3 3])]
    (testing "every bin - its count plus all before it; the last the total; the axis title"
      (let [stats (histogram-bars rows {:x :v} {:bins [0.5 1.5 2.5 3.5] :y :cumulative})]
        (is (= [2.0 3.0 6.0] (mapv :y1 (get-in stats [:data :panel :bars]))))
        (is (= "Cumulative Count" (get-in stats [:axis-titles :y])))))
    (testing "boundary: an empty first bin stays 0; a bin after the data keeps the total"
      (is (= [0.0 2.0 3.0 6.0 6.0]
             (mapv :y1 (get-in (histogram-bars rows {:x :v} {:bins [-0.5 0.5 1.5 2.5 3.5 4.5] :y :cumulative})
                               [:data :panel :bars])))))
    (testing "per group"
      (let [bars (get-in (histogram-bars (concat (map #(assoc % :g "a") rows) [{:v 1 :g "b"}]) {:x :v :color :g}
                                         {:bins [0.5 1.5 2.5 3.5] :y :cumulative :position :dodge})
                         [:data :panel :bars])]
        (is (= 6.0 (double (reduce max (map :y1 bars)))))))
    (testing "an unknown measure is still a schema error"
      (is (= :y (:key (error #(plot/build {:data rows :mapping {:x :v} :charts [(chart/histogram {:y :cumsum})]}))))))))
