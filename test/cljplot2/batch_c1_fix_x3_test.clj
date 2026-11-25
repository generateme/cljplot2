(ns cljplot2.batch-c1-fix-x3-test
  "Batch C1 review fixes (x3): bar base labels at the visible end nearest 0 and `:inside` for centred labels, one
  tick-label content for every tick label, text `:select-within` rule and `[:panel f]` fallback range, the grid space
  error (marker, plot wording, nested cell paths, pairs cells), `matrix-settings` with one variables shape, partial
  colour domains in pairs / correlation / pairwise, strip rotation compared numerically."
  (:require [clojure.test :refer [deftest testing is]]
            [fastmath.core :as m]
            [cljplot2.test-util :refer [error error-data]]
            [cljplot2.chart :as chart]
            [cljplot2.chart.pairwise :as pairwise]
            [cljplot2.content.axis :as axis]
            [cljplot2.content.strip :as strip]
            [cljplot2.core :as plot]
            [cljplot2.data.annotation :as annotation]
            [cljplot2.layout.grid :as grid]
            [cljplot2.layout.plot-grid :as plot-grid]
            [cljplot2.theme :as theme])
  (:import [java.awt.image BufferedImage]))

(def ^:private options {:width 400 :height 300})

(defn- built [p] (plot/build p options))

(defn- pixels
  "The ARGB pixels of an image."
  [^BufferedImage img]
  (vec (.getRGB img 0 0 (.getWidth img) (.getHeight img) nil 0 (.getWidth img))))

(defn- image
  ([p] (image p options))
  ([p opts] (pixels (plot/render-image p opts))))

(defn- message [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-message e))))

;; P2 / D15 - bar labels

(def ^:private two-bars {:data [{:l "p" :v 13} {:l "q" :v 15}] :mapping {:x :v :y :l}})

(defn- bar-labels [p] (keep :label (get-in (built p) [:stats 0 :data :panel :bars])))

(deftest base-label-domain-test
  (testing "0 outside the domain: the labels sit at the visible end nearest 0 and are drawn"
    (doseq [p [(assoc two-bars :scales {:x {:domain [10 nil]}})
               (assoc two-bars :scales {:x {:domain [10 nil]}} :flip true)
               {:data [{:l "p" :v -13} {:l "q" :v -15}] :mapping {:x :v :y :l} :scales {:x {:domain [nil -10]}}}]]
      (is (not= (image (assoc p :charts [(chart/bar {:value-label true :label-position :base})]))
                (image (assoc p :charts [(chart/bar {})]))))))
  (testing "0 inside the domain: unchanged (the base is 0)"
    (is (= #{0.0} (set (map :at (bar-labels (assoc two-bars :charts [(chart/bar {:value-label true :label-position :base})]))))))
    (is (not= (image (assoc two-bars :charts [(chart/bar {:value-label true :label-position :base})]))
              (image (assoc two-bars :charts [(chart/bar {})])))))
  (testing "a bar wholly outside the domain: its label is skipped"
    (is (= (image (assoc two-bars :scales {:x {:domain [20 30]}} :charts [(chart/bar {:value-label true :label-position :base})]))
           (image (assoc two-bars :scales {:x {:domain [20 30]}} :charts [(chart/bar {})])))))
  (testing "polar, values on the radius: builds and renders (labels that don't fit are skipped)"
    (is (instance? BufferedImage (plot/render-image {:data [{:l "p" :v 3} {:l "q" :v 5}] :mapping {:x :l :y :v} :coord {:kind :polar}
                                                     :charts [(chart/bar {:value-label true :label-position :base})]}
                                                    options)))))

(deftest inside-label-test
  (testing ":center and :base labels are :inside, :end and :outer ones aren't"
    (is (every? :inside (bar-labels (assoc two-bars :charts [(chart/bar {:value-label true :label-position :center})]))))
    (is (every? :inside (bar-labels (assoc two-bars :charts [(chart/bar {:value-label true :label-position :base})]))))
    (is (not-any? :inside (bar-labels (assoc two-bars :charts [(chart/bar {:value-label true :label-position :end})]))))
    (is (not-any? :inside (bar-labels {:data [{:l "p" :v 3} {:l "q" :v 5}] :mapping {:x :l :y :v} :coord {:kind :polar}
                                       :charts [(chart/bar {:value-label true :label-position :outer})]}))))
  (testing "stacked bars default to centred labels, inside"
    (is (every? :inside (bar-labels {:data [{:l "p" :g "a" :v 1} {:l "p" :g "b" :v 2}] :mapping {:x :v :y :l :color :g}
                                     :charts [(chart/bar {:position :stack :value-label true})]})))))

;; D9 - one tick-label content

(def ^:private big-ticks {:axis {:x {:tick-label {:size 22}} :y {:tick-label {:size 22}}}})

(deftest tick-label-test
  (testing "the content: the role, the axis's style keys, the rotation only when given"
    (is (= {:kind :label :role :tick-label :text "a"} (axis/tick-label theme/default-theme :x "a")))
    (is (= {:kind :label :role :tick-label :text "a" :rotation 90} (axis/tick-label theme/default-theme :x "a" 90)))
    (is (= {:kind :label :role :tick-label :text "a" :size 22}
           (axis/tick-label (theme/deep-merge theme/default-theme big-ticks) :y "a")))
    (is (= {:kind :label :role :tick-label :text nil} (axis/tick-label {} :x nil)) "no theme style, nil text"))
  (testing "parallel axes, 3D frames and polar guides take [:axis k :tick-label]"
    (doseq [p [{:data [{:a 1 :b 2 :c 3} {:a 2 :b 1 :c 5}] :charts [(chart/parallel {:variables [:a :b :c]})]}
               {:data [{:x 1 :y 2 :z 3} {:x 2 :y 1 :z 5}] :mapping {:x :x :y :y :z :z} :charts [(chart/cloud)]}
               {:data [{:x 1 :y 2} {:x 2 :y 1} {:x 3 :y 4}] :mapping {:x :x :y :y} :coord {:kind :polar} :charts [(chart/scatter)]}]]
      (is (not= (image p) (image (assoc p :theme big-ticks))))))
  (testing "the estimated panel size uses it (a larger tick label - a smaller panel)"
    (let [size (fn [t] (plot-grid/estimated-panel-size {:options options :theme (theme/plot-theme t)} [] {} {:columns 1 :rows 1} false false))
          [w0 h0] (size nil)
          [w1 h1] (size big-ticks)]
      (is (< (double w1) (double w0)))
      (is (< (double h1) (double h0))))))

;; D11 / R9 - text rule

(deftest select-within-rule-test
  (testing ":select-within :group without :select names the key"
    (let [e (error-data #(built {:data [{:x 1 :y 2 :t "a"}] :mapping {:x :x :y :y}
                                 :charts [(chart/text {:mapping {:x :x :y :y :label :t} :select-within :group})]}))]
      (is (some #(re-find #":select-within :group needs :select" (str (:message %))) (:errors e)))))
  (testing "the default without :select and :group with :select are fine"
    (is (map? (built {:data [{:x 1 :y 2 :t "a"}] :mapping {:x :x :y :y}
                      :charts [(chart/text {:mapping {:x :x :y :y :label :t} :select-within :group-and-label})]})))
    (is (map? (built {:data [{:x 1 :y 2 :t "a"}] :mapping {:x :x :y :y}
                      :charts [(chart/text {:mapping {:x :x :y :y :label :t} :select :last :select-within :group})]})))))

;; D12 - axis-pixel-fn fallback

(deftest axis-pixel-fn-test
  (testing "without a scale: [:panel f] over the fallback range, other positions NaN"
    (let [f (annotation/axis-pixel-fn nil [0.0 200.0])]
      (is (m/delta-eq 50.0 (f [:panel 0.25])))
      (is (m/delta-eq 0.0 (f [:panel 0])))
      (is (m/delta-eq 200.0 (f [:panel 1])))
      (is (m/nan? (f 3)))))
  (testing "a descending fallback (a vertical axis from its low-value end)"
    (is (m/delta-eq 150.0 ((annotation/axis-pixel-fn nil [200.0 0.0]) [:panel 0.25]))))
  (testing "no scale, no fallback: always NaN"
    (is (m/nan? ((annotation/axis-pixel-fn nil) [:panel 0.5])))
    (is (m/nan? ((annotation/axis-pixel-fn nil nil) 1))))
  (testing "with a scale the fallback is ignored"
    (let [b (built {:data [{:x 0 :y 0} {:x 10 :y 10}] :mapping {:x :x :y :y} :charts [(chart/scatter)]})
          xs (get-in b [:finalized-scales :x])
          [r0 r1] (:range xs)]
      (is (m/delta-eq (+ (double r0) (* 0.5 (- (double r1) (double r0))))
                      ((annotation/axis-pixel-fn xs [1000.0 2000.0]) [:panel 0.5])))))
  (testing "the text chart on a dot plot's cross axis still places [:panel f]"
    (let [p {:data [{:v 1} {:v 1.1} {:v 2}] :mapping {:x :v} :charts [(chart/dotplot {:binwidth 0.2})]}]
      (is (not= (image p) (image (update p :charts conj (chart/text {:x [:panel 0.1] :y [:panel 0.9] :text "here"}))))))))

;; D13 / P7 - the grid space error

(def ^:private small {:data [{:a 1 :b 2} {:a 2 :b 3}] :mapping {:x :a :y :b} :charts [(chart/scatter)] :labels {:title "tiny"}})

(deftest space-error-test
  (testing "the marker key"
    (is (grid/space-error? {:cause :track-space}))
    (is (false? (grid/space-error? {:key :width :axis :columns :available 10 :required 20})))
    (is (false? (grid/space-error? nil))))
  (testing "a plot: its message (and :errors) names the plot's fixed parts"
    (let [f #(plot/build (assoc small :labels nil) {:width 30 :height 300})
          d (error-data f)]
      (is (= [:width :track-space] ((juxt :key :cause) d)))
      (is (re-find #"the plot's fixed parts" (message f)))
      (is (re-find #"the plot's fixed parts" (:message (first (:errors d)))))))
  (testing "a nested arrange cell: the whole cell path, outer cells first"
    (let [f #(plot/build (plot/arrange [small (plot/arrange (repeat 6 small) {:columns 6})] {:columns 2}) {:width 300 :height 200})
          e (error f)]
      (is (= [[:plot 1] [[:plot 1] [:plot 0]]] ((juxt :subplot :subplot-path) e)))
      (is (re-find #"^Sub-plot \[:plot 1\] > \[:plot 0\] \"tiny\" is \d+x\d+ px" (message f)))))
  (testing "a cell without a title: its mapped variables (a pairs cell)"
    (is (re-find #"^Sub-plot \[:plot \d+\] \(x :a(, y :b)?\) is"
                 (message #(plot/build (plot/pairs [{:a 1 :b 2} {:a 2 :b 1} {:a 3 :b 5}] {:variables [:a :b]}) {:width 80 :height 80})))))
  (testing "a cell without a title or keyword mapping: no name"
    (is (re-find #"^Sub-plot \[:plot 0\] is"
                 (message #(plot/build (plot/arrange (repeat 8 (assoc small :labels nil :mapping {:x (fn [r] (:a r)) :y (fn [r] (:b r))}))
                                                     {:columns 8})
                                       {:width 200 :height 200}))))))

;; D14 - matrix-settings takes the {:rows :columns} map

(deftest matrix-variables-test
  (let [cells #'pairwise/slot-cells]
    (testing "square: triangles"
      (is (= [[:a :b]] (vec (cells {:rows [:a :b] :columns [:a :b]} :lower))))
      (is (= [[:b :a]] (vec (cells {:rows [:a :b] :columns [:a :b]} :upper)))))
    (testing "rectangular: every off-diagonal cell :lower, no :upper"
      (is (= #{[:b :a] [:c :a] [:a :b] [:c :b]} (set (cells {:rows [:a :b] :columns [:a :b :c]} :lower))))
      (is (empty? (cells {:rows [:a :b] :columns [:a :b :c]} :upper))))
    (testing "one variable: no cells"
      (is (empty? (cells {:rows [:a] :columns [:a]} :lower)))))
  (testing "matrix-settings: a value-coloured statistic's domain"
    (let [rows (for [i (range 20)] {:a i :b (* i i) :c (- i)})
          s (pairwise/matrix-settings {:lower [(chart/pairwise {:glyph :shade})] :upper []} rows {:rows [:a :b :c] :columns [:a :b :c]} {:x :a :y :b})]
      (is (= 2 (count (:domain s))))
      (is (= :pairwise (get-in s [:slots :lower 0 :kind]))))))

;; P3 - partial colour domains

(def ^:private crows (for [i (range 30)] {:x (m/sin i) :y (m/cos (* 2 i)) :v (mod (* i 7) 11)}))

(deftest partial-color-domain-test
  (testing "pairs with a mapped continuous colour and [0 nil]"
    (is (map? (built (plot/pairs crows {:variables [:x :y :v] :mapping {:color :v} :scales {:color {:domain [0 nil]}}})))))
  (testing "correlation and pairwise alone: the statistic's range fills the open end"
    (is (= [0.0 1.0] (get-in (built {:data crows :mapping {:x :x :y :y :color :v} :charts [(chart/correlation)]
                                     :scales {:color {:domain [0 nil]}}})
                             [:scale-specs :color :domain])))
    (is (map? (built {:data crows :mapping {:x :x :y :y :color :v} :charts [(chart/pairwise {:glyph :shade})]
                      :scales {:color {:domain [0 nil]}}}))))
  (testing "beside a chart drawing the colour, the fallback extent is left out"
    (is (= {:kind :numeric :min 0.0 :max 10.0}
           (select-keys (get-in (built {:data crows :mapping {:x :x :y :y :color :v} :charts [(chart/scatter) (chart/correlation)]})
                                [:extents :color :extent])
                        [:kind :min :max]))))
  (testing "no mapped colour: no colour extent (unchanged)"
    (is (nil? (get-in (built {:data crows :mapping {:x :x :y :y} :charts [(chart/correlation)]}) [:extents :color])))))

;; R8 - strip rotation

(deftest strip-rotation-test
  ;; strips test their rotation with content.label/quarter-turn (numeric: a double matches)
  (let [quarter-turn @(requiring-resolve 'cljplot2.content.label/quarter-turn)]
    (is (= 90 (quarter-turn 90)))
    (is (= 90 (quarter-turn 90.0)) "a double matches")
    (is (= 270 (quarter-turn 270)))
    (is (= 0 (quarter-turn 0)))
    (is (nil? (quarter-turn 45)) "not a quarter turn")))
