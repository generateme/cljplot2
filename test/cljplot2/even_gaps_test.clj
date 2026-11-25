(ns cljplot2.even-gaps-test
  "Even gaps between the panels of an `:equal-panels` arrange (`layout.align`): the space between neighbouring
  columns' and rows' panels is the same at every boundary."
  (:require [clojure.test :refer [deftest testing is]]
            [fastmath.random :as r]
            [cljplot2.chart :as chart]
            [cljplot2.content.subplot :as subplot]
            [cljplot2.core :as plot]
            [cljplot2.layout.align :as align]
            [cljplot2.layout.grid :as grid]))

(defn- panel-boxes
  "Outer pixel boxes `[x0 y0 x1 y1]` of the main panels of a built arrange's cells."
  [built]
  (for [{:keys [id box]} (grid/content-items (:grid built) (:layout built) subplot/plot-content?)
        :let [insets (align/panel-insets (get-in built [:subplots id]))]
        :when insets]
    [(+ (:x box) (:left insets)) (+ (:y box) (:top insets))
     (- (+ (:x box) (:w box)) (:right insets)) (- (+ (:y box) (:h box)) (:bottom insets))]))

(defn- gaps
  "Distinct gaps between consecutive panel columns (`:x`) or rows (`:y`) of panel boxes."
  [boxes axis]
  (let [[s e] (if (= :x axis) [0 2] [1 3])
        spans (sort (distinct (map (fn [b] [(nth b s) (nth b e)]) boxes)))]
    (set (map (fn [[[_ e0] [s1 _]]] (- s1 e0)) (partition 2 1 spans)))))

(def ^:private rows
  (let [rng (r/rng :mersenne 3)]
    (vec (for [_ (range 30)]
           {:a (r/drandom rng 0 1) :b (r/drandom rng 10 400) :c (r/irandom rng 0 2) :d (r/drandom rng -5 5)}))))

(deftest pairs-even-gaps-test
  (testing "a matrix whose variables end on ticks at different panel edges has one gap per direction"
    (let [built (plot/build (plot/pairs rows {:variables [:a :b :c :d]}) {:width 600 :height 600})
          boxes (panel-boxes built)]
      (is (= 16 (count boxes)))
      (is (= 1 (count (gaps boxes :x))) (str (gaps boxes :x)))
      (is (= 1 (count (gaps boxes :y))) (str (gaps boxes :y)))
      (is (= 1 (count (set (map (fn [[x0 _ x1]] (- x1 x0)) boxes)))) "panels stay equal")))
  (testing "boundary: two variables (one boundary each way), and a legend column beside the matrix"
    (let [built (plot/build (plot/pairs rows {:variables [:a :b]}) {:width 400 :height 400})]
      (is (= 1 (count (gaps (panel-boxes built) :x)))))
    (let [built (plot/build (plot/pairs (map #(assoc % :g (if (> (:a %) 0.5) "u" "v")) rows) {:variables [:a :b :c] :mapping {:color :g}})
                            {:width 600 :height 500})]
      (is (= 1 (count (gaps (panel-boxes built) :x)))))))

(defn- cell
  "A cell at a box whose built plot's panel has the given insets."
  [id [x y w h] [l t r b]]
  {:id id :box {:x x :y y :w w :h h}
   :built {:grid {:id :root :columns [] :rows []} :layout {} :options {:width w :height h} ::insets [l t r b]}})

(deftest gap-paddings-test
  (with-redefs [align/panel-insets (fn [built] (when-let [[l t r b] (::insets built)] {:left l :top t :right r :bottom b}))]
    (testing "the smaller boundary is raised to the larger, on the earlier column's right"
      (is (= {:a {:right 12 :bottom 0}}
             (align/gap-paddings [(cell :a [0 0 100 100] [0 0 2 0]) (cell :b [110 0 100 100] [3 0 9 0]) (cell :c [220 0 100 100] [8 0 0 0])]))))
    (testing "rows: the bottom of the upper row"
      (is (= {:a {:right 0 :bottom 4}}
             (align/gap-paddings [(cell :a [0 0 100 100] [0 0 0 1]) (cell :b [0 110 100 100] [0 2 0 5]) (cell :c [0 220 100 100] [0 2 0 0])]))))
    (testing "boundary: no cells, one cell, even gaps, a cell without a panel, overlapping columns"
      (is (= {} (align/gap-paddings [])))
      (is (= {} (align/gap-paddings [(cell :a [0 0 100 100] [5 5 5 5])])))
      (is (= {} (align/gap-paddings [(cell :a [0 0 100 100] [0 0 4 0]) (cell :b [110 0 100 100] [0 0 0 0]) (cell :c [220 0 100 100] [4 0 0 0])])))
      (is (= {} (align/gap-paddings [(cell :a [0 0 100 100] [0 0 4 0]) (assoc-in (cell :b [110 0 100 100] [0 0 0 0]) [:built ::insets] nil)])))
      (is (= {} (align/gap-paddings [(cell :a [0 0 200 100] [0 0 4 0]) (cell :b [100 0 200 100] [0 0 0 0])])) "overlapping: no boundary"))))

(deftest arrange-gaps-test
  (let [p (fn [y] {:data rows :mapping {:x :a :y y} :charts [(chart/scatter)]})]
    (testing "an :equal-panels arrange evens its gaps"
      (let [built (plot/build (plot/arrange [(p :b) (p :c) (p :d) (p :b)] {:columns 2 :equal-panels true}) {:width 600 :height 500})]
        (is (= 1 (count (gaps (panel-boxes built) :x))))
        (is (= 1 (count (gaps (panel-boxes built) :y))))))
    (testing "boundary: one cell has no boundary"
      (is (= 1 (count (panel-boxes (plot/build (plot/arrange [(p :b)] {:equal-panels true}) {:width 300 :height 300}))))))))
