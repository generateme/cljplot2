(ns cljplot2.pairwise-test
  "Pairwise statistics (B4): the paired samples, the statistics registry, one statistic's value, matrix rows, the `pairwise` chart and its shared domain in `pairs`."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.color :as c]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [cljplot2.chart :as chart]
            [cljplot2.chart.pairwise :as chart.pairwise]
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

(defn- stats-of [k xs ys] ((get-in pw/statistics [k :fn]) xs ys))

(deftest statistics-symmetry-test
  (let [g (r/rng :mersenne 3)
        xs (vec (repeatedly 30 #(+ 1.0 (r/drandom g))))
        ys (vec (repeatedly 30 #(+ 1.5 (r/drandom g))))
        close? (fn [a b] (m/delta-eq (double a) (double b) 1e-9))]
    (doseq [[k {f :fn :keys [symmetry label description samples]}] pw/statistics]
      (testing (str k)
        (is (and (string? label) (string? description) (#{:paired :independent} samples)))
        (is (#{:error :association :effect-size :distance} (get-in pw/statistics [k :family])))
        (let [a (f xs ys) b (f ys xs)]
          (case symmetry
            :symmetric (is (close? a b))
            :antisymmetric (is (close? a (- (double b))))
            :reciprocal (is (close? a (/ 1.0 (double b))))
            :complementary (is (close? a (- 1.0 (double b))))
            :asymmetric (is (not (close? a b)))))))
    (is (= 21 (count pw/statistics))))
  (testing "complementary and antisymmetric hold with ties too"
    (let [xs [1.0 1.0 2.0 2.0] ys [1.0 2.0 2.0 2.0]]
      (is (m/delta-eq 1.0 (+ (double (stats-of :ameasure xs ys)) (double (stats-of :ameasure ys xs)))))
      (is (m/delta-eq 0.0 (+ (double (stats-of :cliffs-delta xs ys)) (double (stats-of :cliffs-delta ys xs)))))))
  (testing ":hedges-g is the bias-corrected d, not Cohen's d"
    (let [xs [1.0 2.0 3.0 4.0] ys [2.0 3.0 5.0 7.0]]
      (is (m/delta-eq (double (fastmath.stats/hedges-g-corrected xs ys)) (double (stats-of :hedges-g xs ys))))
      (is (not (m/delta-eq (double (stats-of :cohens-d xs ys)) (double (stats-of :hedges-g xs ys)))))))
  (testing "signed statistics"
    (is (= #{:me :covariance :pearson :spearman :kendall :cohens-d :hedges-g :glass-delta :cliffs-delta :cosine}
           (set (filter pw/signed-statistic? (keys pw/statistics)))))
    (is (nil? (pw/signed-statistic? (fn [_ _] 1))))))

(deftest statistic-samples-test
  (let [data [{:a 1 :b 5} {:a 2 :b 6} {:a 3 :b nil} {:a 4 :b nil} {:a 10 :b nil}]]
    (testing "paired: complete rows; independent: each variable's own finite values"
      (is (= [[1.0 2.0] [5.0 6.0]] (pw/statistic-samples :me data :a :b)))
      (is (= [[1.0 2.0 3.0 4.0 10.0] [5.0 6.0]] (pw/statistic-samples :cohens-d data :a :b)))
      (is (= [[1.0 2.0] [5.0 6.0]] (pw/statistic-samples (fn [_ _] 1) data :a :b)) "a function - paired"))
    (testing "matrix-rows uses them: Cohen's d over every value of a, n - the smaller sample"
      (let [[r] (pw/matrix-rows data {:x-variables [:a] :y-variables [:b] :method :cohens-d})]
        (is (m/delta-eq (double (fastmath.stats/cohens-d [1.0 2.0 3.0 4.0 10.0] [5.0 6.0])) (double (:value r))))
        (is (= 2 (:n r)))))))

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
    (is (nan? (pw/statistic-value (fn [_ _] (assert false)) [1.0 2.0] [1.0 2.0])) "an Error too")
    (is (nan? (pw/statistic-value :cohens-d [1.0 2.0 3.0] [1.0])) "independent samples: each needs 2 values")
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
    (is (= 9 (count (pw/matrix-rows rows {:variables [:a :b :c] :self-pairs true}))))
    (is (= [[:a :b] [:a :c] [:b :c]] (map (juxt :x :y) (pw/matrix-rows rows {:variables [:a :b :c] :triangle :lower}))))
    (is (= [[:b :a] [:c :a] [:c :b]] (map (juxt :x :y) (pw/matrix-rows rows {:variables [:a :b :c] :triangle :upper}))))
    (is (= [[:a :a] [:a :b] [:b :b]] (map (juxt :x :y) (pw/matrix-rows rows {:variables [:a :b] :triangle :lower :self-pairs true})))))
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
  (testing "boundary: fewer than 2 complete rows - a nil value (a tile paints NA), kept"
    (let [rs (pw/matrix-rows [{:a 1 :b 2} {:a nil :b 3}] {:variables [:a :b]})]
      (is (every? (comp nil? :value) rs))
      (is (every? #(= 1 (:n %)) rs))))
  (testing "X x Y: a variable in both lists keeps its cell (every pair)"
    (is (= [[:z :a] [:a :a] [:z :m] [:a :m]]
           (map (juxt :x :y) (pw/matrix-rows (map #(assoc % :z (:a %) :m (:b %)) rows) {:x-variables [:z :a] :y-variables [:a :m]})))))
  (testing "antisymmetric and reciprocal mirrors derived, not computed"
    (let [calls (atom 0)]
      (with-redefs [pw/statistics (assoc-in pw/statistics [:me :fn] (fn [xs ys] (swap! calls inc) (- (reduce + xs) (reduce + ys))))]
        (let [rs (pw/matrix-rows rows {:variables [:a :b :c] :method :me})
              v (fn [x y] (:value (first (filter #(= [x y] [(:x %) (:y %)]) rs))))]
          (is (= 3 @calls))
          (is (= (v :a :b) (- (double (v :b :a)))))))))
  (testing "errors: variables, triangle, method, options"
    (is (= :variables (:key (error-data #(pw/matrix-rows rows {})))))
    (is (= :variables (:key (error-data #(pw/matrix-rows rows {:variables [:a :b] :x-variables [:a]})))))
    (is (= :variables (:key (error-data #(pw/matrix-rows rows {:x-variables [:a]})))))
    (is (= :variables (:key (error-data #(pw/matrix-rows rows {:variables [:a]})))))
    (is (= :triangle (:key (error-data #(pw/matrix-rows rows {:variables [:a :b] :method :me :triangle :lower})))))
    (is (= :triangle (:key (error-data #(pw/matrix-rows rows {:variables [:a :b] :method (fn [_ _] 1) :triangle :upper})))))
    (is (= :triangle (:key (error-data #(pw/matrix-rows rows {:x-variables [:a] :y-variables [:b] :triangle :lower})))))
    (is (= :method (:key (error-data #(pw/matrix-rows rows {:variables [:a :b] :method :nope})))))
    (is (= :diagonal (:key (error-data #(pw/matrix-rows rows {:variables [:a :b] :diagonal true})))))
    (is (= :variables (:key (error-data #(pw/matrix-rows rows {:variables [:a :a :b]})))) "repeated variables")
    (is (= :variables (:key (error-data #(pw/matrix-rows rows {:variables [:a :zz]})))) "an unknown variable")
    (is (= :variables (:key (error-data #(pw/matrix-rows [{:a 1 :s "x"} {:a 2 :s "y"}] {:variables [:a :s]})))) "a non-numeric one"))
  (testing "boundary: no rows - no numeric values"
    (is (= :variables (:key (error-data #(pw/matrix-rows [] {:variables [:a :b]})))))))

(defn- panel-stats [p] (-> (plot/build p) :stats first :data first val))

(defn- cell-plot [options] {:data rows :mapping {:x :a :y :c} :charts [(chart/pairwise options)]})

(declare first-lines)

(deftest chart-compute-test
  (testing "the value, label and format of a panel"
    (let [d (panel-stats (cell-plot {:method :me}))]
      (is (m/delta-eq (double (:value d)) (pw/statistic-value :me (mapv :a rows) (mapv :c rows))))
      (is (= "ME" (:label d)))
      (is (= :number (:glyph d)))))
  (testing "colour extent: the panels' domain (signed - symmetric around 0, one value widened); none for :number"
    (let [stats (fn [p] (-> (plot/build p) :stats first))
          me (pw/statistic-value :me (mapv :a rows) (mapv :c rows))
          mae (pw/statistic-value :mae (mapv :a rows) (mapv :c rows))]
      (is (= [(- (m/abs me)) (m/abs me)] ((juxt :min :max) (get-in (stats (cell-plot {:method :me :glyph :shade})) [:extents :panel :color]))))
      (is (= [(- mae 0.5) (+ mae 0.5)] ((juxt :min :max) (get-in (stats (cell-plot {:method :mae :glyph :shade})) [:extents :panel :color]))))
      (is (nil? (get-in (stats (cell-plot {:method :me})) [:extents :panel :color])))))
  (testing "a plot colour domain wins; signed statistics on the diverging range"
    (let [scale (fn [p] (get-in (plot/build p) [:scale-specs :color]))]
      (is (= [-5.0 5.0] (mapv double (:domain (scale (assoc (cell-plot {:method :me :glyph :shade}) :scales {:color {:domain [-5 5]}}))))))
      (is (= {:title "ME" :range :RColorBrewer/RdBu} (:color (chart.pairwise/computed-aesthetics {:method :me :glyph :shade} {:x :a :y :c}))))
      (is (= {:title "MAE"} (:color (chart.pairwise/computed-aesthetics {:method :mae :glyph :shade} {:x :a :y :c}))))
      (is (= {} (chart.pairwise/computed-aesthetics {:method (fn [_ _] 1) :glyph :number} {:x :a :y :c})))
      (is (= (:range (scale (cell-plot {:method :pearson :glyph :shade}))) (:range (scale {:data rows :mapping {:x :a :y :c} :charts [(chart/correlation {:glyph :shade})]})))
          "the same diverging colours as correlation")))
  (testing "value-domain boundaries"
    (is (nil? (chart.pairwise/value-domain [] false)))
    (is (nil? (chart.pairwise/value-domain [##NaN nil] true)))
    (is (= [-0.5 0.5] (chart.pairwise/value-domain [0.0] true)))
    (is (= [-3.0 3.0] (chart.pairwise/value-domain [1.0 -3.0 ##NaN] true)))
    (is (= [1.5 2.5] (chart.pairwise/value-domain [2.0 2.0] false))))
  (testing "a missing value: NaN, counted"
    (let [p {:data [{:a 1 :c 2}] :mapping {:x :a :y :c} :charts [(chart/pairwise {:glyph :shade})]}]
      (is (nan? (:value (panel-stats p))))
      (is (= 1 (get-in (plot/build p) [:stats 0 :dropped :value])))
      (is (some? (plot/render p size)))))
  (testing "a continuous colour mapping (of other charts) doesn't group; the cell keeps the theme colour (the plot's colour belongs to the mapping)"
    (let [d (panel-stats {:data rows :mapping {:x :a :y :c :color :d} :charts [(chart/pairwise {:glyph :shade :method :mae})]})]
      (is (nil? (:groups d)))
      (is (not (:by-value? d)))))
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
    (is (= "v=" (subs (str ((:format (panel-stats (cell-plot {:value-format #(str "v=" %)}))) 1.0)) 0 2))))
  (testing ":value-format that throws or gives nil shows NA; a non-string is printed"
    (is (= "ME: NA" (:text (first (first-lines (cell-plot {:method :me :value-format (fn [_] (throw (Exception. "boom")))}))))))
    (is (= "ME: NA" (:text (first (first-lines (cell-plot {:method :me :value-format (constantly nil)}))))))
    (is (= "ME: 7" (:text (first (first-lines (cell-plot {:method :me :value-format (constantly 7)})))))))
  (testing "invalid options rejected"
    (is (= :domain (:key (error-data #(plot/build (cell-plot {:domain [0 1]}))))) ":domain is gone - the colour scale's")
    (is (some? (error-data #(plot/build (cell-plot {:method :nope})))))
    (is (some? (error-data #(plot/build (cell-plot {:glyph :pie})))))))

(defn- fitted-texts
  "The texts (`{:lines :template}`) a plot's cells draw, in drawing order."
  [p]
  (let [seen (atom [])
        draw label/draw-fitted-lines!]
    (with-redefs [label/draw-fitted-lines! (fn [canvas text box env] (swap! seen conj text) (draw canvas text box env))]
      (plot/render p size))
    @seen))

(defn- first-lines [p] (:lines (first (fitted-texts p))))

(defn- first-line-color
  "Colour of the first text line a plot's pairwise cell draws."
  [p]
  (:color (first (first-lines p))))

(deftest chart-text-color-test
  (testing ":contrast text - white on a dark shade, black on a light one and on the plain panel"
    (let [dark {:data [{:a 0 :c 0} {:a 1 :c 1}] :mapping {:x :a :y :c} :scales {:color {:range [:white :black] :domain [-1 0.01]}}
                :charts [(chart/pairwise {:glyph :shade-number :method :mae})]}
          light (assoc-in dark [:scales :color :domain] [-0.01 1])]
      (is (= (c/to-color :white) (c/to-color (first-line-color dark))))
      (is (= (c/to-color :black) (c/to-color (first-line-color light))))
      (is (= (c/to-color :black) (c/to-color (first-line-color (cell-plot {:glyph :number})))))))
  (testing "a :color option wins for :number text; over a shade it colours the shade only (text by contrast)"
    (is (= (c/to-color :red) (c/to-color (first-line-color (cell-plot {:glyph :number :color :red})))))
    (is (= (c/to-color :white) (c/to-color (first-line-color (cell-plot {:glyph :shade-number :color [20 30 60]})))))
    (is (= (c/to-color :black) (c/to-color (first-line-color (cell-plot {:glyph :shade-number :color [240 240 200]}))))))
  (testing "group lines over a shade take the contrast colour too"
    (let [p {:data (map-indexed (fn [i row] (assoc row :g (if (even? i) "e" "o"))) rows)
             :mapping {:x :a :y :c :color :g} :charts [(chart/pairwise {:glyph :shade-number :method :mae :color [20 30 60]})]}
          lines (first-lines p)]
      (is (= 3 (count lines)))
      (is (every? #(= (c/to-color :white) (c/to-color (:color %))) lines)))))

(defn- cell-color-domains
  "The colour domains of a pairs plot's cells (and legend cell) that have one."
  [p]
  (set (keep #(get-in % [:scales :color :domain]) (tree-seq coll? seq p))))

(defn- templates [p] (set (keep #(when (= :pairwise (:kind %)) (get-in % [:options :text-template])) (tree-seq coll? seq p))))

(deftest pairs-shared-domain-test
  (testing "every colouring cell and the legend cell share the matrix's domain"
    (let [p (plot/pairs rows {:variables [:a :b :c :d] :lower (chart/pairwise {:method :mse :glyph :shade}) :upper :none})
          values (keep :value (pw/matrix-rows rows {:variables [:a :b :c :d] :method :mse :triangle :lower}))]
      (is (= #{[(reduce min values) (reduce max values)]} (cell-color-domains p)))
      (is (= 7 (count (filter #(= [(reduce min values) (reduce max values)] (get-in % [:scales :color :domain])) (tree-seq coll? seq p))))
          "6 lower cells and the legend cell")
      (is (some? (plot/render p {:width 500 :height 400})))))
  (testing "a signed statistic: symmetric around 0"
    (let [p (plot/pairs rows {:variables [:a :b :c] :lower (chart/pairwise {:method :me :glyph :shade}) :upper (chart/pairwise {:method :me})})
          [[lo hi]] (seq (cell-color-domains p))]
      (is (= lo (- hi)))))
  (testing "the statistic is computed once for the matrix (each ordered cell once), and only when it colours"
    (let [calls (atom 0)]
      (with-redefs [pw/statistics (assoc-in pw/statistics [:mae :fn] (fn [xs ys] (swap! calls inc) 1.0))]
        (plot/pairs rows {:variables [:a :b :c] :lower (chart/pairwise {:method :mae :glyph :shade}) :upper (chart/pairwise {:method :mae})})
        (is (= 3 @calls) "symmetric: 3 unordered pairs for both slots")
        (reset! calls 0)
        (is (empty? (cell-color-domains (plot/pairs rows {:variables [:a :b :c] :lower (chart/pairwise {:method :mae}) :upper :none}))))
        (is (= 3 @calls) ":number - values for the text size only"))))
  (testing ":scales {:color ..} reaches every colouring cell; its :domain wins"
    (let [p (plot/pairs rows {:variables [:a :b] :lower (chart/pairwise {:method :mse :glyph :shade}) :scales {:color {:domain [0 10] :title "t"}}})]
      (is (= #{[0 10]} (cell-color-domains p)))
      (is (some? (plot/render p {:width 400 :height 300}))))
    (is (empty? (cell-color-domains (plot/pairs rows {:variables [:a :b]}))) "nothing colours: no colour scale"))
  (testing "two shaded statistics, or a pairwise shade with a correlation shade - one colour bar: an error keyed :upper"
    (is (= :upper (:key (error-data #(plot/pairs rows {:variables [:a :b] :lower (chart/pairwise {:method :me :glyph :shade})
                                                       :upper (chart/pairwise {:method :mse :glyph :shade})})))))
    (is (= :upper (:key (error-data #(plot/pairs rows {:variables [:a :b] :lower (chart/pairwise {:method :pearson :glyph :shade})
                                                       :upper (chart/correlation {:glyph :shade})})))))
    (is (map? (plot/pairs rows {:variables [:a :b] :lower (chart/pairwise {:method :me :glyph :shade}) :upper (chart/pairwise {:method :mse})}))
        "a :number statistic doesn't colour"))
  (testing "boundary: one distinct value - a domain of width 1 around it"
    (is (= #{[-0.5 0.5]} (cell-color-domains (plot/pairs [{:a 1 :b 1} {:a 2 :b 2} {:a 3 :b 3}] {:variables [:a :b] :lower (chart/pairwise {:method :mse :glyph :shade})})))))
  (testing "one text size: every pairwise cell gets the matrix's widest text as its template"
    (let [p (plot/pairs rows {:variables [:a :b :c] :lower (chart/pairwise {:method :me}) :upper (chart/pairwise {:method :me})})
          ts (templates p)]
      (is (= 1 (count ts)))
      (is (every? #(= (first ts) (:template %)) (filter :template (fitted-texts p))))))
  (testing ":me mirrored cells have opposite signs (every cell - column variable against row variable)"
    (let [built (plot/build (plot/pairs rows {:variables [:a :c] :lower (chart/pairwise {:method :me}) :upper (chart/pairwise {:method :me})}))
          values (keep (fn [x] (when (and (map? x) (contains? x :value) (contains? x :label) (= "ME" (:label x))) (:value x))) (tree-seq coll? seq built))]
      (is (= 2 (count values)))
      (is (m/delta-eq (double (first values)) (- (double (second values))))))
    (let [data [{:a 10.0 :c 0.0} {:a 12.0 :c 1.0} {:a 14.0 :c -1.0}]
          built (plot/build (plot/pairs data {:variables [:a :c] :lower :none :upper (chart/pairwise {:method :me})}))
          [v] (keep (fn [x] (when (and (map? x) (= "ME" (:label x))) (:value x))) (tree-seq coll? seq built))]
      (is (m/delta-eq -12.0 (double v)) "upper cell (row a, column c): mean of c - a"))))

(deftest tile-matrix-test
  (testing "nil values keep their tile (NA colour) and category"
    (let [rs (pw/matrix-rows [{:a 1 :b 2 :c 1} {:a 2 :b 2 :c 3} {:a 3 :b 2 :c 2}] {:variables [:a :b :c] :method :pearson})]
      (is (some (comp nil? :value) rs))
      (is (some? (plot/render {:data rs :mapping {:x (comp name :x) :y (comp name :y) :color :value} :charts [(chart/tile)]} size))))))
