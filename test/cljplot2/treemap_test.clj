(ns cljplot2.treemap-test
  "Step 10: treemap layouts and the treemap chart."
  (:require [clojure.test :refer [deftest testing is are]]
            [cljplot2.test-util :refer [error-data error error-ins]]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.layout.grid :as grid]
            [cljplot2.layout.treemap :as treemap])
  (:import [java.awt.image BufferedImage]))

(defn- pixel [canvas x y]
  (let [v (.getRGB ^BufferedImage (c2d/get-image canvas) (int x) (int y))]
    [(bit-and (bit-shift-right v 16) 0xff) (bit-and (bit-shift-right v 8) 0xff) (bit-and v 0xff)]))

;; layouts

(def ^:private rect {:x 10.0 :y 20.0 :w 300.0 :h 200.0})

(defn- area [{:keys [w h]}] (* (double w) (double h)))
(defn- inside? [{:keys [x y w h]} r]
  (let [e 1e-6]
    (and (>= (:x r) (- x e)) (>= (:y r) (- y e))
         (<= (+ (:x r) (:w r)) (+ x w e)) (<= (+ (:y r) (:h r)) (+ y h e)))))
(defn- overlap? [a b]
  (let [e 1e-6]
    (and (< (+ (:x a) e) (+ (:x b) (:w b))) (< (+ (:x b) e) (+ (:x a) (:w a)))
         (< (+ (:y a) e) (+ (:y b) (:h b))) (< (+ (:y b) e) (+ (:y a) (:h a))))))
(defn- worst-aspect [rects]
  (reduce max (map (fn [{:keys [w h]}] (let [w (double w) h (double h)] (m/max (/ w h) (/ h w)))) rects)))

(defn- check-layout [values rects]
  (let [total (reduce + values)]
    (is (= (count values) (count rects)))
    (is (every? #(inside? rect %) rects) "inside the rectangle")
    (is (every? (fn [[v r]] (< (m/abs (- (area r) (* (area rect) (/ v total)))) 1e-6)) (map vector values rects))
        "area proportional to value")
    (is (not-any? true? (for [i (range (count rects)) j (range (inc i) (count rects))] (overlap? (rects i) (rects j))))
        "no overlap")))

(deftest squarify-test
  (testing "invariants"
    (doseq [values [[6 6 4 3 2 2 1] [1] [5 5] [100 1 1 1] (vec (range 30 0 -1))]]
      (check-layout values (treemap/squarify values rect))))
  (testing "empty; single value fills the rectangle"
    (is (= [] (treemap/squarify [] rect)))
    (let [[r & more] (treemap/squarify [7] rect)]
      (is (nil? more))
      (is (every? #(< (m/abs (- (double (% r)) (double (% rect)))) 1e-9) [:x :y :w :h]))))
  (testing "zero-size rectangle: zero-size rectangles, no error"
    (is (every? #(zero? (area %)) (treemap/squarify [3 2 1] {:x 0.0 :y 0.0 :w 0.0 :h 50.0}))))
  (testing "squarified aspect ratios are no worse than slice-and-dice"
    (let [rng (java.util.Random. 7)]
      (dotimes [_ 50]
        (let [values (vec (sort > (repeatedly (inc (.nextInt rng 20)) #(inc (.nextInt rng 100)))))]
          (is (<= (worst-aspect (treemap/squarify values rect))
                  (+ 1e-9 (worst-aspect (treemap/slice-dice values rect 0))))))))))

(deftest slice-dice-test
  (let [values [3 1 2]]
    (check-layout values (treemap/slice-dice values rect 0))
    (check-layout values (treemap/slice-dice values rect 1))
    (is (every? #(= 200.0 (:h %)) (treemap/slice-dice values rect 0)) "depth 0: full height")
    (is (every? #(= 300.0 (:w %)) (treemap/slice-dice values rect 1)) "depth 1: full width")
    (is (= [10.0 160.0 210.0] (map :x (treemap/slice-dice values rect 0))) "data order kept")
    (is (= [] (treemap/slice-dice [] rect 0)))))

;; chart

(def ^:private rows
  [{:a "A" :b "a1" :c "x" :v 4}
   {:a "A" :b "a1" :c "y" :v 2}
   {:a "A" :b "a2" :c nil :v 3}
   {:a "B" :b "" :c "z" :v 1}
   {:a "B" :b "b1" :c "w" :v 5}
   {:a "C" :b "c1" :c "q" :v 6}])

(def ^:private mapping {:value :v :hierarchy [:a :b :c]})
(defn- tm [extra] (merge {:data rows :mapping mapping :charts [(chart/treemap)]} extra))
(defn- build [p] (plot/build p {:width 600 :height 400}))
(defn- tree [built] (get-in built [:stats 0 :data :panel :tree]))
(defn- child [node n] (some #(when (= n (:name %)) %) (:children node)))

(deftest tree-test
  (let [t (tree (build (tm {})))]
    (testing "top-level groups in first-appearance order, values summed"
      (is (= ["A" "B" "C"] (map :name (:children t))))
      (is (= [9.0 6.0 6.0] (map :value (:children t))))
      (is (= [0 1 2] (map :top-index (:children t)))))
    (testing "nil / \"\" level: leaf at the parent"
      (is (= {:name "a2" :value 3.0 :color nil} (dissoc (child (child t "A") "a2") :top-index)))
      (is (= ["b1" nil] (map :name (:children (child t "B")))) "\"\" level ends at B: an unnamed leaf of B"))
    (testing "identical paths merge"
      (let [t (tree (build (tm {:data (conj rows {:a "C" :b "c1" :c "q" :v 4})})))]
        (is (= 10.0 (:value (child (child (child t "C") "c1") "q"))))))
    (testing "single row: the root's only child"
      (is (= ["A"] (map :name (:children (tree (build (tm {:data [(first rows)]}))))))))))

(deftest value-and-mapping-test
  (testing "invalid values dropped and counted"
    (let [built (build (tm {:data (into rows [{:a "D" :v 0} {:a "D" :v -1} {:a "D" :v nil} {:a "D" :v "x"} {:a "D" :v ##NaN} {:a "D" :v ##Inf}])}))]
      (is (= 6 (get-in built [:stats 0 :dropped :value])))
      (is (nil? (child (tree built) "D")))))
  (testing "nothing valid / no data: no tree, no error, renders"
    (is (= {} (get-in (build (tm {:data [{:a "A" :v 0}]})) [:stats 0 :data])))
    (is (= {} (get-in (build (tm {:data []})) [:stats 0 :data])))
    (is (some? (plot/render (tm {:data []})))))
  (testing "mapping errors"
    (are [m in] (= in (:in (error #(build (tm {:mapping m})))))
      {:hierarchy [:a]} [:charts 0 :options :mapping :value]
      {:value :v} [:charts 0 :options :mapping :hierarchy]
      {:value :v :hierarchy :a} [:mapping :hierarchy]
      {:value :v :hierarchy []} [:mapping :hierarchy]
      {:value :v :hierarchy [:a 3]} [:mapping :hierarchy 1]))
  (testing "options and theme validated"
    (are [opts k] (= k (:key (error #(build (tm {:charts [(chart/treemap opts)]})))))
      {:layout :pie} :layout
      {:group-labels 1} :group-labels
      {:gap -1} :gap
      {:stroke-width "1"} :stroke-width)))

(deftest no-axes-test
  (let [built (build (tm {}))]
    (is (not-any? #{:x :y} (keys (:scale-specs built))))
    (is (not-any? #(#{:axis :axis-overhang} (get-in % [:content :kind])) (get-in built [:grid :items])))))

;; drawing

(def ^:private plain {:plot {:background :white} :palette {:color [[255 0 0] [0 0 255] [0 255 0]]}
                      :chart {:treemap {:gap 0 :stroke-color :none}}})

(deftest colors-test
  (testing "top-level groups get palette colors; leaves share them"
    (let [p (tm {:data [{:a "A" :v 1} {:a "B" :v 1}] :theme plain :charts [(chart/treemap {:leaf-labels false})]})
          built (build p)
          {:keys [x y w h]} (grid/item-box (:layout built) :panel)
          c (plot/render p {:width 600 :height 400})
          colors (set [(pixel c (+ x 5) (+ y (quot h 2))) (pixel c (+ x w -5) (+ y (quot h 2)))])]
      (is (= #{[255 0 0] [0 0 255]} colors))))
  (testing "top-level colors agree across facet panels"
    (let [built (build (tm {:data (concat (map #(assoc % :f "p") rows) (map #(assoc % :f "q") (drop 3 rows)))
                            :mapping (assoc mapping :facet :f)}))
          idx (fn [panel] (into {} (map (juxt :name :top-index)) (get-in built [:stats 0 :data panel :tree :children])))]
      (is (= (select-keys (idx "p") ["B" "C"]) (idx "q")))))
  (testing "mapped :color: per-leaf colors, legend, NA, explicit domain drops"
    (let [built (build (tm {:mapping (assoc mapping :color :c)}))]
      (is (some #(= :legends (:id %)) (get-in built [:grid :items])))
      (is (contains? (get-in built [:stats 0 :na]) :color) "nil :c rows -> NA"))
    (let [built (build (tm {:mapping (assoc mapping :color :c) :scales {:color {:domain ["x" "y"]}}}))]
      (is (= 3 (get-in built [:stats 0 :dropped :color])) "z w q outside; nil kept as NA"))))

(deftest labels-test
  (let [p (fn [opts] (tm {:data [{:a "G" :b "leaf" :v 1}] :theme plain :charts [(chart/treemap opts)]}))
        top-pixel (fn [opts]
                    (let [built (build (p opts))
                          {:keys [x y w]} (grid/item-box (:layout built) :panel)]
                      (pixel (plot/render (p opts) {:width 600 :height 400}) (+ x (quot w 2)) (+ y 3))))]
    (testing "group header strip on top; off with :group-labels false"
      (is (not= [255 0 0] (top-pixel {:leaf-labels false})) "header strip (lightened group fill)")
      (is (= [255 0 0] (top-pixel {:leaf-labels false :group-labels false})) "leaf reaches the top"))
    (testing "tiny rectangles and tiny canvases don't raise"
      (is (some? (plot/render (tm {:data (for [i (range 200)] {:a (str "g" (mod i 9)) :b (str "l" i) :v (inc (mod i 13))})})
                              {:width 60 :height 40})))
      (is (some? (plot/render (tm {:charts [(chart/treemap {:layout :slice-dice})]}) {:width 20 :height 20}))))))

;; review fixes

(deftest squarify-reference-test
  (testing "Bruls et al. example: 6 6 4 3 2 2 1 in 6x4"
    (is (= [[0.0 0.0 3.0 2.0] [0.0 2.0 3.0 2.0] [3.0 0.0 1.71 2.33] [4.71 0.0 1.29 2.33]
            [3.0 2.33 1.2 1.67] [4.2 2.33 1.2 1.67] [5.4 2.33 0.6 1.67]]
           (mapv (fn [r] (mapv #(/ (Math/round (* 100.0 (double (% r)))) 100.0) [:x :y :w :h]))
                 (treemap/squarify [6 6 4 3 2 2 1] {:x 0.0 :y 0.0 :w 6.0 :h 4.0}))))))

(deftest color-identity-test
  (let [built (build (tm {:data [{:a "A" :b "a1" :v 2 :k "x"} {:a "A" :b "a1" :v 3 :k "y"} {:a "A" :b "a1" :v 1 :k "x"}]
                          :mapping (assoc mapping :color :k)}))]
    (testing "same path, different colors: separate leaves; same color: merged"
      (is (= [["a1" 3.0 "x"] ["a1" 3.0 "y"]] (map (juxt :name :value :color) (:children (child (tree built) "A")))))))
  (testing "without :color: one merged leaf"
    (is (= 6.0 (:value (child (child (tree (build (tm {:data [{:a "A" :b "a1" :v 2} {:a "A" :b "a1" :v 4}]}))) "A") "a1"))))))

(defn- dark? [[r g b]] (and (< r 80) (< g 80) (< b 80)))

(deftest leaf-label-fit-test
  (let [p (tm {:data [{:a "BIG" :v 1000} {:a "tiny" :v 1}] :mapping {:value :v :hierarchy [:a]}
              ;; light fills: black labels (contrast with the fill)
              :theme (assoc plain :palette {:color [[255 230 150] [200 230 255]]})})
        built (build p)
        {px :x py :y pw :w ph :h} (grid/item-box (:layout built) :panel)
        [big tiny] (treemap/squarify [1000 1] {:x 0.0 :y 0.0 :w (double pw) :h (double ph)})
        c (plot/render p {:width 600 :height 400})
        dark-in (fn [{:keys [x y w h]}]
                  (count (filter dark? (for [ix (range (long x) (long (+ x w))) iy (range (long y) (long (+ y h)))]
                                         (pixel c (+ px ix) (+ py iy))))))]
    (is (pos? (dark-in big)) "label drawn in the big leaf")
    (is (zero? (dark-in tiny)) "no label in the tiny leaf")
    (is (zero? (count (filter dark? (for [ix (range pw) iy [0 (dec ph)]] (pixel c (+ px ix) (+ py iy))))))
        "no label ink on the panel's top / bottom edge")))

(deftest header-fit-test
  (let [top-pixel (fn [group-name]
                    (let [p (tm {:data [{:a group-name :b "leaf" :v 1}] :theme plain :charts [(chart/treemap {:leaf-labels false})]})
                          built (build p)
                          {:keys [x y w]} (grid/item-box (:layout built) :panel)]
                      (pixel (plot/render p {:width 600 :height 400}) (+ x (quot w 2)) (+ y 3))))]
    (is (not= [255 0 0] (top-pixel "G")) "header fits: strip reserved")
    (is (= [255 0 0] (top-pixel (apply str (repeat 120 "W")))) "header too wide: no strip, the leaf reaches the top")))
