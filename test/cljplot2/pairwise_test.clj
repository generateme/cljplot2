(ns cljplot2.pairwise-test
  "Pairwise statistics (B4): the paired samples, the statistics registry, one statistic's value, matrix rows, the `pairwise` chart and its shared domain in `pairs`."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.color :as c]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [cljplot2.chart :as chart]
            [cljplot2.content.label :as label]
            [cljplot2.core :as plot]
            [cljplot2.data.pairwise :as pw]
            [cljplot2.test-util :refer [error-data]]))

(def ^:private size {:width 300 :height 240})

(defn- nan? [v] (and (number? v) (m/nan? (double v))))

(def ^:private rng (r/rng :mersenne 7))

(def ^:private rows
  (vec (for [_ (range 40)]
         (let [a (r/drandom rng)]
           {:a a :b (+ a (* 0.5 (r/drandom rng))) :c (* 3.0 (r/drandom rng)) :d (+ 1.0 (r/drandom rng))}))))

(deftest pair-samples-test
  (testing "pairwise-complete rows as doubles"
    (is (= [[1.0 3.0] [2.0 6.0]] (pw/pair-samples [{:x 1 :y 2} {:x 2 :y nil} {:x 3 :y 6} {:x ##NaN :y 1} {:x ##Inf :y 1} {:x 4 :y "a"}] :x :y))))
  (testing "boundary: no rows, all missing"
    (is (= [[] []] (pw/pair-samples [] :x :y)))
    (is (= [[] []] (pw/pair-samples [{:x nil :y 1} {:x 1}] :x :y)))))

(deftest statistics-symmetry-test
  (let [g (r/rng :mersenne 3)
        xs (vec (repeatedly 30 #(+ 1.0 (r/drandom g))))
        ys (vec (repeatedly 30 #(+ 1.5 (r/drandom g))))
        close? (fn [a b] (m/delta-eq (double a) (double b) 1e-9))]
    (doseq [[k {f :fn :keys [symmetry label description samples]}] pw/statistics]
      (testing (str k)
        (is (and (string? label) (string? description) (#{:paired :independent} samples)))
        (let [a (f xs ys) b (f ys xs)]
          (case symmetry
            :symmetric (is (close? a b))
            :antisymmetric (is (close? a (- (double b))))
            :reciprocal (is (close? a (/ 1.0 (double b))))
            :complementary (is (close? a (- 1.0 (double b))))
            :asymmetric (is (not (close? a b)))))))
    (is (= 21 (count pw/statistics)))))

(deftest statistic-value-test
  (testing "a keyword or a function"
    (is (m/delta-eq 1.0 (pw/statistic-value :pearson [1.0 2.0 3.0] [2.0 4.0 6.0])))
    (is (= 6.0 (pw/statistic-value (fn [xs ys] (reduce + (map - ys xs))) [1.0 2.0 3.0] [2.0 4.0 6.0]))))
  (testing "boundary: n 0 and 1 - NaN; n 2 - a value"
    (is (nan? (pw/statistic-value :mse [] [])))
    (is (nan? (pw/statistic-value :mse [1.0] [2.0])))
    (is (= 1.0 (pw/statistic-value :mse [1.0 2.0] [2.0 3.0]))))
  (testing "boundary: constant data, a throwing function, infinite and non-number results - NaN"
    (is (nan? (pw/statistic-value :pearson [1.0 1.0 1.0] [1.0 2.0 3.0])))
    (is (nan? (pw/statistic-value (fn [_ _] (throw (Exception. "no"))) [1.0 2.0] [1.0 2.0])))
    (is (nan? (pw/statistic-value (fn [_ _] ##Inf) [1.0 2.0] [1.0 2.0])))
    (is (nan? (pw/statistic-value (fn [_ _] "x") [1.0 2.0] [1.0 2.0])))
    (is (nan? (pw/statistic-value :glass-delta [1.0 2.0] [3.0 3.0])) "a zero deviation"))
  (testing "an unknown keyword is an error keyed :method"
    (is (= :method (:key (error-data #(pw/statistic-value :nope [1.0 2.0] [1.0 2.0])))))))

(deftest labels-and-symmetry-test
  (is (= "MSE" (pw/statistic-label :mse nil)))
  (is (= "mine" (pw/statistic-label :mse "mine")))
  (is (= "value" (pw/statistic-label (fn [_ _] 1) nil)))
  (is (= :antisymmetric (pw/statistic-symmetry :me)))
  (is (= :asymmetric (pw/statistic-symmetry (fn [_ _] 1)))))

(deftest matrix-rows-test
  (testing "square: every ordered pair without the diagonal, row order (y outer)"
    (let [rs (pw/matrix-rows rows {:variables [:a :b :c] :method :me})]
      (is (= [[:b :a] [:c :a] [:a :b] [:c :b] [:a :c] [:b :c]] (map (juxt :x :y) rs)))
      (is (every? #(= 40 (:n %)) rs))
      (is (every? #(= "ME" (:statistic %)) rs))
      (let [v (fn [x y] (:value (first (filter #(= [x y] [(:x %) (:y %)]) rs))))]
        (is (m/delta-eq (double (v :a :b)) (- (double (v :b :a)))) "an asymmetric statistic in both orders"))))
  (testing "the diagonal and triangles"
    (is (= 9 (count (pw/matrix-rows rows {:variables [:a :b :c] :diagonal? true}))))
    (is (= [[:a :b] [:a :c] [:b :c]] (map (juxt :x :y) (pw/matrix-rows rows {:variables [:a :b :c] :triangle :lower}))))
    (is (= [[:b :a] [:c :a] [:c :b]] (map (juxt :x :y) (pw/matrix-rows rows {:variables [:a :b :c] :triangle :upper}))))
    (is (= [[:a :a] [:a :b] [:b :b]] (map (juxt :x :y) (pw/matrix-rows rows {:variables [:a :b] :triangle :lower :diagonal? true})))))
  (testing "a symmetric statistic once per pair: mirrored values equal"
    (let [calls (atom 0)
          rs (pw/matrix-rows rows {:variables [:a :b :c] :method :mse})]
      (is (= (:value (first rs)) (:value (nth rs 2))))
      (with-redefs [pw/statistics (assoc-in pw/statistics [:mse :fn] (fn [_ _] (swap! calls inc) 1.0))]
        (pw/matrix-rows rows {:variables [:a :b :c] :method :mse})
        (is (= 3 @calls)))))
  (testing "X x Y"
    (let [rs (pw/matrix-rows rows {:x-variables [:a :b] :y-variables [:c :d] :method :r2})]
      (is (= [[:a :c] [:b :c] [:a :d] [:b :d]] (map (juxt :x :y) rs)))))
  (testing "a user function with a label; missing data per pair"
    (let [data [{:a 1 :b 2 :c nil} {:a 2 :b 4 :c 1} {:a 3 :b 6 :c 2} {:a nil :b 1 :c 3}]
          rs (pw/matrix-rows data {:variables [:a :b :c] :method (fn [xs _] (count xs)) :label "n"})]
      (is (= {[:b :a] 3.0 [:c :a] 2.0 [:a :b] 3.0 [:c :b] 3.0 [:a :c] 2.0 [:b :c] 3.0}
             (into {} (map (juxt (juxt :x :y) :value) rs))))
      (is (every? #(= "n" (:statistic %)) rs))))
  (testing "boundary: fewer than 2 complete rows - NaN, kept"
    (let [rs (pw/matrix-rows [{:a 1 :b 2} {:a nil :b 3}] {:variables [:a :b]})]
      (is (every? (comp nan? :value) rs))
      (is (every? #(= 1 (:n %)) rs))))
  (testing "errors: variables, triangle, method, options"
    (is (= :variables (:key (error-data #(pw/matrix-rows rows {})))))
    (is (= :variables (:key (error-data #(pw/matrix-rows rows {:variables [:a :b] :x-variables [:a]})))))
    (is (= :variables (:key (error-data #(pw/matrix-rows rows {:x-variables [:a]})))))
    (is (= :variables (:key (error-data #(pw/matrix-rows rows {:variables [:a]})))))
    (is (= :triangle (:key (error-data #(pw/matrix-rows rows {:variables [:a :b] :method :me :triangle :lower})))))
    (is (= :triangle (:key (error-data #(pw/matrix-rows rows {:variables [:a :b] :method (fn [_ _] 1) :triangle :upper})))))
    (is (= :triangle (:key (error-data #(pw/matrix-rows rows {:x-variables [:a] :y-variables [:b] :triangle :lower})))))
    (is (= :method (:key (error-data #(pw/matrix-rows rows {:variables [:a :b] :method :nope})))))
    (is (= :diagonal (:key (error-data #(pw/matrix-rows rows {:variables [:a :b] :diagonal true}))))))
  (testing "boundary: no rows"
    (is (every? (comp nan? :value) (pw/matrix-rows [] {:variables [:a :b]})))))

(defn- panel-stats [p] (-> (plot/build p) :stats first :data first val))

(defn- cell-plot [options] {:data rows :mapping {:x :a :y :c} :charts [(chart/pairwise options)]})

(deftest chart-compute-test
  (testing "the value, label and format of a panel"
    (let [d (panel-stats (cell-plot {:method :me}))]
      (is (m/delta-eq (double (:value d)) (pw/statistic-value :me (mapv :a rows) (mapv :c rows))))
      (is (= "ME" (:label d)))
      (is (= :number (:glyph d)))))
  (testing "colour extent: :domain, else the panels' range; none for :number"
    (let [stats (fn [p] (-> (plot/build p) :stats first))]
      (is (= {:kind :numeric :min -5.0 :max 5.0} (get-in (stats (cell-plot {:method :me :glyph :shade :domain [-5 5]})) [:extents :panel :color])))
      (is (some? (get-in (stats (cell-plot {:method :me :glyph :shade})) [:extents :panel :color])))
      (is (nil? (get-in (stats (cell-plot {:method :me})) [:extents :panel :color])))))
  (testing "a missing value: NaN, counted"
    (let [p {:data [{:a 1 :c 2}] :mapping {:x :a :y :c} :charts [(chart/pairwise {:glyph :shade})]}]
      (is (nan? (:value (panel-stats p))))
      (is (= 1 (get-in (plot/build p) [:stats 0 :dropped :value])))
      (is (some? (plot/render p size)))))
  (testing "grouping: a value per group, no computed colour"
    (let [p {:data (map-indexed (fn [i row] (assoc row :g (if (even? i) "e" "o"))) rows)
             :mapping {:x :a :y :c :color :g} :charts [(chart/pairwise {:glyph :shade :method :mae})]}
          d (panel-stats p)]
      (is (= 2 (count (:groups d))))
      (is (not (:by-value? d)))
      (is (some? (plot/render p size)))))
  (testing "every glyph renders; a user function with a label"
    (doseq [g [:number :shade :shade-number]]
      (is (some? (plot/render (cell-plot {:glyph g :method :rmse}) size)) (str g)))
    (is (= "diff" (:label (panel-stats (cell-plot {:method (fn [xs ys] (- (reduce + xs) (reduce + ys))) :label "diff"})))))
    (is (= "v=" (subs (str ((:format (panel-stats (cell-plot {:format #(str "v=" %)}))) 1.0)) 0 2))))
  (testing "invalid options rejected"
    (is (= :domain (:key (error-data #(plot/build (cell-plot {:domain [2 1]}))))))
    (is (some? (error-data #(plot/build (cell-plot {:method :nope})))))
    (is (some? (error-data #(plot/build (cell-plot {:glyph :pie})))))))

(defn- first-line-color
  "Colour of the first text line a plot's pairwise cell draws."
  [p]
  (let [seen (atom nil)
        draw label/draw-fitted-lines!]
    (with-redefs [label/draw-fitted-lines! (fn [canvas text box env] (when-not @seen (reset! seen (:color (first (:lines text))))) (draw canvas text box env))]
      (plot/render p size))
    @seen))

(deftest chart-text-color-test
  (testing ":contrast text - white on a dark shade, black on a light one and on the plain panel"
    (let [dark {:data [{:a 0 :c 0} {:a 1 :c 1}] :mapping {:x :a :y :c} :scales {:color {:range [:white :black]}}
                :charts [(chart/pairwise {:glyph :shade-number :method :mae :domain [-1 0.01]})]}
          light (assoc dark :charts [(chart/pairwise {:glyph :shade-number :method :mae :domain [-0.01 1]})])]
      (is (= (c/to-color :white) (c/to-color (first-line-color dark))))
      (is (= (c/to-color :black) (c/to-color (first-line-color light))))
      (is (= (c/to-color :black) (c/to-color (first-line-color (cell-plot {:glyph :number})))))))
  (testing "a :color option wins"
    (is (= (c/to-color :red) (c/to-color (first-line-color (cell-plot {:glyph :number :color :red})))))))

(defn- pairwise-charts [p] (filter #(= :pairwise (:kind %)) (tree-seq coll? seq p)))

(deftest pairs-shared-domain-test
  (testing "every pairwise cell and the legend column share the matrix's domain"
    (let [p (plot/pairs rows {:variables [:a :b :c :d] :lower (chart/pairwise {:method :mse :glyph :shade}) :upper nil})
          domains (set (map #(get-in % [:options :domain]) (pairwise-charts p)))
          values (keep (fn [{:keys [value]}] (when-not (nan? value) value)) (pw/matrix-rows rows {:variables [:a :b :c :d] :method :mse}))]
      (is (= 1 (count domains)))
      (is (= [(reduce min values) (reduce max values)] (first domains)))
      (is (some? (plot/render p {:width 500 :height 400})))))
  (testing "an explicit domain is kept; other kinds unchanged"
    (let [p (plot/pairs rows {:variables [:a :b] :lower (chart/pairwise {:method :mse :glyph :shade :domain [0 10]})})]
      (is (= #{[0 10]} (set (map #(get-in % [:options :domain]) (pairwise-charts p))))))
    (is (empty? (pairwise-charts (plot/pairs rows {:variables [:a :b]})))))
  (testing "boundary: one distinct value - a domain of width 1 around it"
    (let [p (plot/pairs [{:a 1 :b 1} {:a 2 :b 2} {:a 3 :b 3}] {:variables [:a :b] :lower (chart/pairwise {:method :mse :glyph :shade})})]
      (is (= #{[-0.5 0.5]} (set (map #(get-in % [:options :domain]) (pairwise-charts p)))))))
  (testing ":me mirrored cells have opposite signs (lower - column vs row, upper - row vs column)"
    (let [built (plot/build (plot/pairs rows {:variables [:a :c] :lower (chart/pairwise {:method :me}) :upper (chart/pairwise {:method :me})}))
          values (keep (fn [x] (when (and (map? x) (contains? x :value) (contains? x :label) (= "ME" (:label x))) (:value x))) (tree-seq coll? seq built))]
      (is (= 2 (count values)))
      (is (m/delta-eq (double (first values)) (- (double (second values))))))))
