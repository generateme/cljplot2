(ns cljplot2.batch-c1-fd-test
  "Batch C1, fork fd: `plot/pairs` with separate row and column variables (#25), the error of a plot or sub-plot
  too small for its fixed tracks (#6)."
  (:require [clojure.test :refer [deftest testing is]]
            [cljplot2.test-util :refer [error]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.layout.grid :as grid]))

(def ^:private rows (vec (for [i (range 30)] {:a i :b (* i i) :c (mod (* i 7) 11) :g (if (even? i) "x" "y") :s "s"})))

(defn- cells
  "The cell plots of a pairs plot map, without the legend column."
  [p]
  (vec (remove :legend-only (get-in p [:arrange :plots]))))

(defn- kinds [cell] (mapv :kind (:charts cell)))

;; #25

(deftest pairs-rectangular-test
  (testing "3 rows x 2 columns: cell (i, j) - column variable j on x, row variable i on y"
    (let [p (plot/pairs rows {:row-variables [:a :b :c] :column-variables [:b :c]})
          cs (cells p)]
      (is (= 2 (get-in p [:arrange :columns])) "no mapping, scatter only - no legend column")
      (is (= 6 (count cs)))
      (is (= [[:b :a] [:c :a] [:b :b] [:c :b] [:b :c] [:c :c]]
             (mapv (fn [c] [(get-in c [:mapping :x]) (get-in c [:mapping :y] (get-in c [:mapping :x]))]) cs)))
      (testing "a cell of one variable is diagonal wherever it is; every other cell takes :lower"
        (is (= [[:scatter] [:scatter] [:density] [:scatter] [:scatter] [:density]] (mapv kinds cs))))
      (testing "axes only on the outer cells"
        (is (= [:grid :grid :grid :grid true true] (mapv #(get-in % [:scales :x :axis]) cs)))
        (is (= [true :grid true :grid true :grid] (mapv #(get-in % [:scales :y :axis]) cs))))
      (is (plot/render-image p {:width 400 :height 500}))))
  (testing "one row variable, one column variable"
    (let [cs (cells (plot/pairs rows {:row-variables [:a] :column-variables [:b]}))]
      (is (= [[:scatter]] (mapv kinds cs)))))
  (testing "each defaults to :variables: a reversed column order"
    (let [cs (cells (plot/pairs rows {:variables [:a :b] :column-variables [:b :a] :diagonal :none}))]
      (is (= [[:scatter] [] [] [:scatter]] (mapv kinds cs)))))
  (testing "the same row and column variables in the same order - the square matrix of :variables"
    (is (= (plot/pairs rows {:variables [:a :b :c]})
           (plot/pairs rows {:row-variables [:a :b :c] :column-variables [:a :b :c]}))))
  (testing "grouped: one legend column, :lower charts and :diagonal names"
    (let [p (plot/pairs rows {:row-variables [:a :b] :column-variables [:c :a] :mapping {:color :g}
                              :lower [(chart/scatter) (chart/smooth)] :diagonal :names})]
      (is (= 3 (get-in p [:arrange :columns])))
      (is (= [[:scatter :smooth] [:text] [:scatter :smooth] [:scatter :smooth]] (mapv kinds (cells p))))
      (is (plot/render-image p {:width 500 :height 400}))))
  (testing "pairwise statistic cells of a rectangular matrix share one colour domain"
    (let [p (plot/pairs rows {:row-variables [:a :b] :column-variables [:c :a] :lower (chart/pairwise {:glyph :shade})})
          domains (keep #(get-in % [:scales :color :domain]) (cells p))]
      (is (= 3 (count domains)) "three cells of two variables")
      (is (apply = domains))
      (is (plot/render-image p {:width 400 :height 400})))))

(deftest pairs-rectangular-errors-test
  (testing "no variables"
    (is (= :variables (:key (error #(plot/pairs rows {})))))
    (is (= :variables (:key (error #(plot/pairs rows {:row-variables [:a :b]}))))))
  (testing "wrong counts and types"
    (is (= :row-variables (:key (error #(plot/pairs rows {:row-variables [] :column-variables [:a]})))))
    (is (= :column-variables (:key (error #(plot/pairs rows {:row-variables [:a] :column-variables (vec (repeat 21 :a))})))))
    (is (= :column-variables (:key (error #(plot/pairs rows {:row-variables [:a] :column-variables :a})))))
    (is (= :row-variables (:key (error #(plot/pairs rows {:row-variables [:a nil] :column-variables [:a]}))))))
  (testing "a non-numeric variable names its option"
    (is (= {:key :row-variables :value :s} (select-keys (error #(plot/pairs rows {:row-variables [:s] :column-variables [:a]})) [:key :value])))
    (is (= :column-variables (:key (error #(plot/pairs rows {:variables [:a :b] :column-variables [:g]})))))
    (is (= :variables (:key (error #(plot/pairs rows {:variables [:a :s] :row-variables [:a]}))))))
  (testing ":upper needs a square matrix"
    (is (= :upper (:key (error #(plot/pairs rows {:row-variables [:a :b] :column-variables [:b] :upper :none})))))
    (is (= :upper (:key (error #(plot/pairs rows {:variables [:a :b] :column-variables [:b :a] :upper (chart/scatter)})))))
    (is (map? (plot/pairs rows {:variables [:a :b] :row-variables [:a :b] :upper :none})) "the same order is square")))

;; #6

(def ^:private small-plot {:data rows :mapping {:x :a :y :b} :charts [(chart/scatter)] :labels {:title "T"}})

(deftest space-error-test
  (testing "a single plot narrower than its fixed tracks: keyed by the render option, sizes in pixels"
    (let [e (error #(plot/build small-plot {:width 30 :height 300}))]
      (is (= :width (:key e)))
      (is (= 30 (:available e)))
      (is (integer? (:required e)))
      (is (< 30 (long (:required e))))))
  (testing "too low"
    (is (= :height (:key (error #(plot/build small-plot {:width 300 :height 20}))))))
  (testing "an infinite need (a nested grid whose fractions sum to 1) stays infinite"
    (is (= {:key :width :axis :columns :available 60 :required ##Inf :cause :track-space} (#'grid/space-error-data :columns 60 ##Inf))))
  (testing "a wide enough plot builds"
    (is (map? (plot/build small-plot {:width 300 :height 300}))))
  (testing "an arrange cell: the message names the cell, its title and the sizes"
    (let [f #(plot/build (plot/arrange (repeat 8 small-plot) {:columns 8}) {:width 200 :height 200})
          e (error f)
          message (try (f) (catch clojure.lang.ExceptionInfo ex (ex-message ex)))]
      (is (= [:width [:plot 0] [[:plot 0]]] ((juxt :key :subplot :subplot-path) e)))
      (is (re-find #"\[:plot 0\] \"T\" is \d+x\d+ px" message))
      (is (re-find (re-pattern (str (:required e) " px of width")) message))))
  (testing "a pairs cell"
    (let [e (error #(plot/build (plot/pairs rows {:variables [:a :b :c]}) {:width 100 :height 100}))]
      (is (= :width (:key e)))
      (is (= [:plot 0] (:subplot e)))))
  (testing "facet panels and a custom grid"
    (is (= :width (:key (error #(plot/build (assoc small-plot :mapping {:x :a :y :b :facet :c}) {:width 60 :height 400})))))
    (is (= {:key :width :axis :columns :available 60 :required 100 :cause :track-space}
           (error #(plot/build {:custom-grid {:columns [{:size [:fixed 100]}] :rows [{:size [:flexible]}] :items []}}
                               {:width 60 :height 150})))))
  (testing "the error data shape"
    (is (grid/space-error? (#'grid/space-error-data :rows 10 20.5)))
    (is (= {:key :height :axis :rows :available 10 :required 21 :cause :track-space} (#'grid/space-error-data :rows 10 20.5)))
    (is (false? (grid/space-error? {:key :width :axis :columns :available 1 :required 2})) "the marker, not the shape")
    (is (false? (grid/space-error? nil)))))
