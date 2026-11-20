(ns cljplot2.data.shingle-test
  "Step 11: shingles (equal-count intervals, Interval values, row repetition) and shingle strips."
  (:require [clojure.test :refer [deftest testing is are]]
            [cljplot2.test-util :refer [error-data error error-ins]]
            [clojure2d.core :as c2d]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.shingle :as shingle]
            [cljplot2.layout.grid :as grid])
  (:import [java.awt.image BufferedImage]))

(def ^:private quake-mags
  "`mag` column of dev/resources/quakes.json (R datasets::quakes)."
  (mapv parse-double (map second (re-seq #"\"mag\":([0-9.]+)" (slurp "dev/resources/quakes.json")))))

(defn- bounds [ivs] (mapv (juxt :lo :hi) ivs))
(defn- close? [a b] (every? true? (map (fn [[a1 a2] [b1 b2]] (and (< (Math/abs (- a1 b1)) 1e-9) (< (Math/abs (- a2 b2)) 1e-9))) a b)))

(deftest equal-count-test
  (testing "matches R co.intervals / equal.count (references from Rscript)"
    (is (= 1000 (count quake-mags)))
    (is (close? [[3.95 4.55] [4.25 4.75] [4.45 4.95] [4.65 6.45]] (bounds (shingle/equal-count quake-mags {:count 4}))))
    (is (close? [[3.95 4.35] [4.15 4.45] [4.35 4.65] [4.45 4.85] [4.65 5.05] [4.85 6.45]]
                (bounds (shingle/equal-count quake-mags {:count 6 :overlap 0.25}))))
    (are [vs opts expected] (close? expected (bounds (shingle/equal-count vs opts)))
      [1 2 3 4 5] {:count 2} [[0.5 3.5] [2.5 5.5]]
      [5 5 5] {:count 3} [[5.0 5.0]]
      [1 2 2 3 10] {:count 1 :overlap 0} [[0.5 10.5]]))
  (testing "no values: no intervals; nil / NaN ignored"
    (is (= [] (shingle/equal-count [] {:count 3})))
    (is (= [] (shingle/equal-count [nil ##NaN] {:count 3})))
    (is (close? [[0.5 3.5] [2.5 5.5]] (bounds (shingle/equal-count [1 nil 2 ##NaN 3 4 5] {:count 2})))))
  (testing "errors"
    (are [vs opts] (= :options (first (:in (error #(shingle/equal-count vs opts)))))
      [1 2] {:count 0}
      [1 2] {:count 1.5}
      [1 2] {:overlap 1}
      [1 2] {:overlap -0.1}
      [1 2] {:as :b})
    (is (= {:key :shingle :value "x"} (error #(shingle/equal-count [1 "x"] {:count 1}))) "non-numeric value")))

(deftest interval-test
  (let [a (shingle/interval 1 2) b (shingle/interval 1 3) c (shingle/interval 0.5 9)]
    (is (= a (shingle/interval 1 2)))
    (is (= (hash a) (hash (shingle/interval 1 2))))
    (is (= [c a b] (sort [b a c])) "by lo, then hi")
    (is (= "[1, 2]" (str a)))
    (is (= "[0.5, 9]" (str c)))
    (is (= "[4.25, 4.75]" (str (shingle/interval 4.25 4.75))))
    (is (shingle/contains-value? a 1))
    (is (shingle/contains-value? a 2) "closed")
    (is (not (shingle/contains-value? a 2.0001)))
    (are [lo hi] (= [:interval :bounds] (take 2 (:in (error #(shingle/interval lo hi)))))
      2 1 ##NaN 1 1 ##Inf "a" 2)))

(deftest shingle-test
  (let [data [{:v 1} {:v 3} {:v 5} {:v nil} {:v ##NaN} {:v 50}]
        out (shingle/shingle data :v {:intervals [[0 3] [3 6]] :as :band})]
    (testing "rows repeated per containing interval"
      (is (= [[1 "[0, 3]"] [3 "[0, 3]"] [3 "[3, 6]"] [5 "[3, 6]"]] (map (juxt :v (comp str :band)) out)))
      (is (= {:dropped 3} (meta out)) "nil, NaN and the uncovered 50"))
    (testing "equal-count intervals; row count = sum of memberships"
      (let [out (shingle/shingle (map #(hash-map :mag %) quake-mags) :mag {:count 4 :as :m})
            ivs (shingle/equal-count quake-mags {:count 4})]
        (is (= (reduce + (for [iv ivs v quake-mags :when (shingle/contains-value? iv v)] 1)) (count out)))
        (is (= {:dropped 0} (meta out)))))
    (testing "empty data; columnar data; function accessor"
      (is (= [] (shingle/shingle [] :v {:count 2 :as :b})))
      (is (= 2 (count (shingle/shingle {:v [1 2]} #(:v %) {:intervals [[0 5]] :as :b})))))
    (testing "errors"
      (are [opts in] (= in (:in (error #(shingle/shingle data :v opts))))
        {:intervals [[0 3]]} [:as]
        {:intervals [] :as :b} [:intervals]
        {:intervals [[3 0]] :as :b} [:intervals 0]
        {:intervals [[0 1 2]] :as :b} [:intervals 0]
        {:count 0 :as :b} [:count]
        {:as :b :lable "x"} [:lable])
      (is (= [:accessor] (:in (error #(shingle/shingle data "v" {:as :b})))))
      (is (= :shingle (:key (error #(shingle/shingle [{:v "x"}] :v {:intervals [[0 1]] :as :b}))))))))

(defn- pixel [canvas x y]
  (let [v (.getRGB ^BufferedImage (c2d/get-image canvas) (int x) (int y))]
    [(bit-and (bit-shift-right v 16) 0xff) (bit-and (bit-shift-right v 8) 0xff) (bit-and v 0xff)]))

(deftest shingle-facet-test
  (let [data (shingle/shingle (for [i (range 40)] {:x i :y i :v (mod i 10)}) :v {:intervals [[5 9] [0 4]] :as :band})
        p {:data data :mapping {:x :x :y :y :facet :band} :charts [(chart/scatter)]
           :theme {:strip {:fill [255 255 255] :shingle-fill [255 0 0] :border-color :none}}}
        built (plot/build p {:width 600 :height 300})
        strips (filter #(= :strip (get-in % [:content :kind])) (get-in built [:grid :items]))]
    (testing "interval levels sorted by lo; strips carry the interval within the levels' range"
      (is (= ["[0, 4]" "[5, 9]"] (map str (get-in built [:split :facets :levels :facet]))))
      (is (= [{:lo 0 :hi 4 :min 0 :max 9} {:lo 5 :hi 9 :min 0 :max 9}] (map #(get-in % [:content :interval]) strips))))
    (testing "the first interval shades the strip's left end, the last its right end"
      (let [c (plot/render p {:width 600 :height 300})
            box (fn [id] (grid/item-box (:layout built) id))
            {x0 :x y0 :y w0 :w h0 :h} (box [:strip 0 0])
            {x1 :x y1 :y w1 :w h1 :h} (box [:strip 0 1])]
        (is (= [255 0 0] (pixel c (+ x0 2) (+ y0 (quot h0 2)))))
        (is (= [255 255 255] (pixel c (+ x0 w0 -2) (+ y0 (quot h0 2)))))
        (is (= [255 0 0] (pixel c (+ x1 w1 -2) (+ y1 (quot h1 2)))))
        (is (= [255 255 255] (pixel c (+ x1 2) (+ y1 (quot h1 2)))))))
    (testing "ordinary facet levels: no interval"
      (is (not-any? #(get-in % [:content :interval])
                    (get-in (plot/build {:data [{:x 1 :y 1 :g "a"}] :mapping {:x :x :y :y :facet :g} :charts [(chart/scatter)]})
                            [:grid :items]))))))

(deftest shingle-label-test
  (let [data [{:mag 4.0} {:mag 5.0}]]
    (testing "no label by default"
      (is (= ["[3, 6]" "[3, 6]"] (map (comp str :m) (shingle/shingle data :mag {:intervals [[3 6]] :as :m})))))
    (testing ":label true - the column name; a string - that name"
      (is (= "mag: [3, 6]" (str (:m (first (shingle/shingle data :mag {:intervals [[3 6]] :as :m :label true}))))))
      (is (= "Magnitude: [3, 6]" (str (:m (first (shingle/shingle data :mag {:intervals [[3 6]] :as :m :label "Magnitude"})))))))
    (testing "labelled intervals: same order, still contain values, equal within one shingle"
      (let [out (shingle/shingle (map #(hash-map :mag %) quake-mags) :mag {:count 4 :as :m :label true})
            ivs (distinct (map :m out))]
        (is (= 4 (count ivs)))
        (is (= (sort-by :lo ivs) (sort ivs)))
        (is (every? #(.startsWith ^String (str %) "mag: [") ivs))))
    (testing "errors: :label true with a function accessor, non-string label"
      (is (= [:label] (:in (error #(shingle/shingle data (fn [r] (:mag r)) {:intervals [[3 6]] :as :m :label true})))))
      (is (= [:label] (:in (error #(shingle/shingle data :mag {:intervals [[3 6]] :as :m :label 3})))))
      (is (= [:interval :label] (:in (error #(shingle/interval 1 2 :x))))))))
