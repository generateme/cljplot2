(ns cljplot2.batch-c2-pm-test
  "Batch C2 (pm): `plot/pairs` with many variables - outer-track weights of the first layout and `:legend-span` (a legend cell spanning rows, arrange `:row-spans`)."
  (:require [clojure.test :refer [deftest is testing]]
            [fastmath.random :as r]
            [cljplot2.core :as plot]
            [cljplot2.chart :as chart]))

(def ^:private options {:width 1000 :height 800})

(def ^:private rows
  "Seeded rows of 20 uniform variables `:v0` ... `:v19` and a group `:g`."
  (let [rng (r/rng :mersenne 7)]
    (vec (for [i (range 120)]
           (assoc (into {} (for [k (range 20)] [(keyword (str "v" k)) (r/drandom rng)])) :g (["a" "b" "c"] (mod i 3)))))))

(defn- variables
  "The first `n` variable keys."
  [n]
  (mapv #(keyword (str "v" %)) (range n)))

(defn- error
  "The ex-data of calling `f`, `nil` when it doesn't throw."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- image-size
  "Width and height of the rendered plot."
  [p]
  (let [^java.awt.image.BufferedImage img (plot/render-image p options)] [(.getWidth img) (.getHeight img)]))

(deftest outer-weights-test
  (testing "up to 9 variables: equal weights"
    (is (every? #(== 1 %) (get-in (plot/pairs rows {:variables (variables 9)}) [:arrange :widths])))
    (is (every? #(== 1 %) (get-in (plot/pairs rows {:variables (variables 2)}) [:arrange :heights]))))
  (testing "many variables: the left column and the bottom row weigh more"
    (let [{:keys [widths heights]} (:arrange (plot/pairs rows {:variables (variables 17)}))]
      (is (== 2.0 (first widths)))
      (is (== 2.0 (peek heights)))
      (is (every? #(== 1 %) (rest widths)))
      (is (every? #(== 1 %) (pop heights)))))
  (testing "the legend column stays measured"
    (is (= :measured (peek (get-in (plot/pairs rows {:variables (variables 12) :mapping {:color :g}}) [:arrange :widths])))))
  (testing "rectangular: each axis its own count"
    (let [{:keys [widths heights]} (:arrange (plot/pairs rows {:row-variables (variables 3) :column-variables (variables 17)}))]
      (is (= 17 (count widths)))
      (is (== 2.0 (first widths)))
      (is (every? #(== 1 %) heights)))))

(deftest many-variables-build-test
  (testing "20 variables at 1000 x 800"
    (is (= [1000 800] (image-size (plot/pairs rows {:variables (variables 20)})))))
  (testing "20 variables, correlation and pairwise cells, names on the diagonal"
    (is (= [1000 800] (image-size (plot/pairs rows {:variables (variables 20) :lower (chart/correlation {:glyph :ellipse})
                                                    :upper (chart/pairwise) :diagonal :names})))))
  (testing "rectangular, many columns / many rows"
    (is (= [1000 800] (image-size (plot/pairs rows {:row-variables (variables 3) :column-variables (variables 15)}))))
    (is (= [1000 800] (image-size (plot/pairs rows {:row-variables (variables 15) :column-variables (variables 2)})))))
  (testing "one cell"
    (is (= [1000 800] (image-size (plot/pairs rows {:row-variables [:v0] :column-variables [:v0]}))))
    (is (= [1000 800] (image-size (plot/pairs rows {:row-variables [:v0] :column-variables [:v1] :diagonal :names}))))))

(deftest legend-span-test
  (testing "default and :row - the legend in an extra column of the matrix"
    (doseq [span [nil :row]]
      (let [p (plot/pairs rows {:variables (variables 3) :mapping {:color :g} :legend-span span})]
        (is (= 4 (get-in p [:arrange :columns])))
        (is (:legend-only (get-in p [:arrange :plots 3]))))))
  (testing ":matrix - one legend cell of the extra column spanning every row"
    (let [p (plot/pairs rows {:variables (variables 3) :mapping {:color :g} :legend-span :matrix :title "m"})
          plots (get-in p [:arrange :plots])]
      (is (= 4 (get-in p [:arrange :columns])))
      (is (= {3 3} (get-in p [:arrange :row-spans])))
      (is (= :measured (peek (get-in p [:arrange :widths]))))
      (is (= 10 (count plots)) "3 x 3 cells and one legend cell, no empty legend cells")
      (is (:legend-only (nth plots 3)))
      (is (not-any? :legend-only (concat (take 3 plots) (drop 4 plots))))
      (is (= "m" (get-in p [:labels :title])))))
  (testing ":matrix without a legend: the plain matrix"
    (let [p (plot/pairs rows {:variables (variables 3) :upper (chart/scatter) :legend-span :matrix})]
      (is (= 3 (get-in p [:arrange :columns])))))
  (testing "a legend taller than a row: 20 variables with keys and a colour bar"
    (is (= [1000 800] (image-size (plot/pairs rows {:variables (variables 20) :mapping {:color :g}
                                                    :upper (chart/correlation {:glyph :shade}) :diagonal :names
                                                    :legend-span :matrix}))))
    (is (= [1000 800] (image-size (plot/pairs rows {:row-variables (variables 3) :column-variables (variables 15)
                                                    :mapping {:color :g} :legend-span :matrix})))))
  (testing "a wrong value"
    (is (= :legend-span (:key (error #(plot/pairs rows {:variables (variables 3) :legend-span :column})))))
    (is (= :legend-span (:key (error #(plot/pairs rows {:variables (variables 3) :legend-span "matrix"})))))))
