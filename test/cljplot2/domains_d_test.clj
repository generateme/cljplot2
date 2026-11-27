(ns cljplot2.domains-d-test
  "Batch D (round 08) - domains of marks of several rows: the row filter of per-row aesthetics, marks judged by one
  value (polygon, a line's size, bar), lines coloured along their length breaking at points not kept, invalid apart
  from out-of-domain, one domain source for binned scales."
  (:require [clojure.test :refer [deftest testing is]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.facet :as facet]
            [cljplot2.data.group :as group]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.test-util :refer [error]]))

(def ^:private size {:width 300 :height 200})

(defn- stats
  "The first chart's stats of a built plot."
  [p]
  (get-in (plot/build p size) [:stats 0]))

(defn- panel-data
  "The single panel's computed data of the first chart."
  [p]
  (get-in (stats p) [:data facet/single-panel]))

(def ^:private rows4
  "Four colour values: inside [0 4], outside, NaN, NA."
  {:p [{:c 1} {:c 9} {:c ##NaN} {:c nil}]})

(defn- kept-cs
  "The colour values `group/continuous` keeps, and its dropped count."
  [row-filter env]
  (let [r (group/continuous :color rows4 {:color :c} (merge {:plot-scales {:color {:domain [0 4]}} :row-filter row-filter} env))]
    [(mapv :c (get-in r [:groups :p])) (:dropped r)]))

(deftest row-filter-test
  (testing ":kept drops invalid and out-of-domain rows; :valid only invalid ones; :none none"
    (let [[kept n] (kept-cs :kept {})] (is (= [[1 nil] 2] [kept n])))
    (let [[kept n] (kept-cs :valid {})] (is (= [[1 9 nil] 1] [kept n])))
    (let [[kept n] (kept-cs :none {})] (is (= [1 9 nil] (remove #(and (number? %) (Double/isNaN (double %))) kept))) (is (= 4 (count kept))) (is (zero? n))))
  (testing "NA rows: :na-level decides under :kept and :valid; :none keeps them (the caller judges NA)"
    (is (= [[1] 3] (kept-cs :kept {:na-level false})))
    (is (= [[1 9] 2] (kept-cs :valid {:na-level false})))
    (is (= [4 0] ((juxt (comp count first) second) (kept-cs :none {:na-level false}))) "the nil row stays"))
  (testing "the default is :kept; :as-na keeps outside rows under :kept"
    (is (= [[1 nil] 2] (kept-cs nil {})))
    (is (= [1 9 nil] (mapv :c (get-in (group/continuous :color rows4 {:color :c} {:plot-scales {:color {:domain [0 4] :out-of-domain :as-na}}})
                                      [:groups :p])))))
  (testing "a value of the wrong type throws under every mode"
    (doseq [rf [:kept :valid :none]]
      (is (= :kind (:key (error #(group/continuous :color {:p [{:c 1} {:c "x"}]} {:color :c}
                                                   {:plot-scales {:color {:kind :linear}} :row-filter rf}))))
          (str rf))))
  (testing "empty groups"
    (is (= {:p []} (:groups (group/continuous :color {:p []} {:color :c} {:plot-scales {:color {:kind :linear}} :row-filter :none}))))))

(def ^:private marks
  "Marks with a value and a row count: inside, outside, NA, NaN."
  {:p [{:v 1 :n 2} {:v 9 :n 3} {:v nil :n 1} {:v ##NaN :n 4}] :q []})

(defn- judged
  "The values of the marks `kept-marks` keeps, and the rows of those it drops."
  [spec env]
  (let [[ps n] (group/kept-marks :color (merge {:plot-scales {:color spec}} env) :v :n marks)]
    [(mapv :v (:p ps)) n (:q ps)]))

(deftest kept-marks-test
  (testing "nil / :drop drop an outside mark; :as-na / :clamp keep it; an invalid value is always dropped"
    (is (= [[1 nil] 7 []] (judged {:domain [0 4]} {})))
    (is (= [[1 nil] 7 []] (judged {:domain [0 4] :out-of-domain :drop} {})))
    (is (= [[1 9 nil] 4 []] (judged {:domain [0 4] :out-of-domain :as-na} {})))
    (is (= [[1 9 nil] 4 []] (judged {:domain [0 4] :out-of-domain :clamp} {}))))
  (testing "NA: kept with an NA level, dropped without"
    (is (= [[1] 8 []] (judged {:domain [0 4]} {:na-level false}))))
  (testing "no domain: only the invalid mark goes; partial and reversed domains"
    (is (= [[1 9 nil] 4 []] (judged {:kind :linear} {})))
    (is (= [[1 nil] 7 []] (judged {:domain [nil 4]} {})))
    (is (= [[1 nil] 7 []] (judged {:domain [0 4] :reverse true} {}))))
  (testing "no marks"
    (is (= [{} 0] (group/kept-marks :color {:plot-scales {}} :v :n {})))))

(def ^:private square
  "The corners of a unit square, in drawing order."
  [{:x 0 :y 0} {:x 1 :y 0} {:x 1 :y 1} {:x 0 :y 1}])

(defn- polygon-plot
  "A polygon of the square's corners coloured by `cs`, under the colour scale `spec`."
  [cs spec & [extra]]
  (merge {:data (mapv #(assoc %1 :c %2) square cs) :mapping {:x :x :y :y :color :c}
          :scales {:color spec} :charts [(chart/polygon)]}
         extra))

(defn- polygons [p] (:polygons (panel-data p)))

(deftest polygon-value-test
  (let [d {:domain [0 4]}]
    (testing "an outside value on a later vertex keeps every vertex (no vertex lost)"
      (let [[pg] (polygons (polygon-plot [1 1 9 1] d))]
        (is (= [4 1] [(count (:x pg)) (:color-value pg)]))
        (is (zero? (get-in (stats (polygon-plot [1 1 9 1] d)) [:dropped :color])))))
    (testing "an outside first vertex drops the polygon, its vertices counted"
      (is (empty? (polygons (polygon-plot [9 1 1 1] d))))
      (is (= 4 (get-in (stats (polygon-plot [9 1 1 1] d)) [:dropped :color]))))
    (testing "two outside vertices after the first: one polygon valued 1"
      (is (= [[4 1]] (mapv (juxt (comp count :x) :color-value) (polygons (polygon-plot [1 9 9 1] d))))))
    (testing ":as-na and :clamp keep an outside polygon whole"
      (doseq [ood [:as-na :clamp]]
        (is (= [[4 9]] (mapv (juxt (comp count :x) :color-value) (polygons (polygon-plot [9 1 1 1] (assoc d :out-of-domain ood)))))
            (str ood))))
    (testing "a reversed scale judges the same way (partial domains: kept-marks-test)"
      (is (empty? (polygons (polygon-plot [9 1 1 1] (assoc d :reverse true)))))))
  (testing "invalid values: on a later vertex ignored, on the first the polygon dropped (counted)"
    (is (= [4] (mapv (comp count :x) (polygons (polygon-plot [1 ##NaN 1 1] {:kind :linear})))))
    (doseq [bad [##NaN ##Inf]]
      (is (= 4 (get-in (stats (polygon-plot [bad 1 1 1] {:kind :linear})) [:dropped :color])) (pr-str bad))))
  (testing "the deciding row is the first drawn vertex: a first row without a valid x doesn't decide"
    (let [p {:data (into [{:x nil :y 0 :c 9}] (map #(assoc %1 :c %2) square [1 1 1 1])) :mapping {:x :x :y :y :color :c}
             :scales {:color {:domain [0 4]}} :charts [(chart/polygon)]}]
      (is (= [1] (mapv :color-value (polygons p))))
      (is (= {:x 1 :color 0} (select-keys (:dropped (stats p)) [:x :color])))))
  (testing "NA: a first NA vertex is NA, or drops the polygon without an NA level; a later NA vertex is kept"
    (is (= [nil] (mapv :color-value (polygons (polygon-plot [nil 1 1 1] {:kind :linear})))))
    (is (= 4 (get-in (stats (polygon-plot [nil 1 1 1] {:kind :linear} {:groups {:na false}})) [:dropped :color])))
    (is (= [4] (mapv (comp count :x) (polygons (polygon-plot [1 nil 1 1] {:kind :linear} {:groups {:na false}}))))))
  (testing "fewer than 3 vertices: counted under :group, not judged by colour"
    (let [s (stats {:data (mapv #(assoc %1 :c %2) (take 2 square) [9 9]) :mapping {:x :x :y :y :color :c}
                    :scales {:color {:domain [0 4]}} :charts [(chart/polygon)]})]
      (is (= {:group 1 :color 0} (select-keys (:dropped s) [:group :color])))))
  (testing "a polygon renders with an outside later vertex"
    (is (some? (plot/render-image (polygon-plot [1 1 9 1] {:domain [0 4]}) size)))))

(defn- lines-plot
  "Two lines `a` and `b` of three points each, sized by `wa` / `wb`, under the size scale `spec`."
  [wa wb spec]
  {:data (concat (map (fn [x w] {:x x :y x :g "a" :w w}) [0 1 2] wa) (map (fn [x w] {:x x :y (inc x) :g "b" :w w}) [0 1 2] wb))
   :mapping {:x :x :y :y :group :g :size :w} :scales {:size spec} :charts [(chart/line)]})

(deftest line-size-value-test
  (testing "a line is judged by its first drawn point's size: a later outside size keeps every point"
    (let [p (lines-plot [1 9 1] [9 1 1] {:domain [0 2]})]
      (is (= [[3 1]] (mapv (juxt (comp count :x) :size-value) (:lines (panel-data p)))))
      (is (= 3 (get-in (stats p) [:dropped :size])) "line b's points")))
  (testing ":clamp keeps both lines; no domain keeps both"
    (is (= 2 (count (:lines (panel-data (lines-plot [1 9 1] [9 1 1] {:domain [0 2] :out-of-domain :clamp}))))))
    (is (= 2 (count (:lines (panel-data (lines-plot [1 9 1] [9 1 1] {:kind :linear})))))))
  (testing "a first row without a valid y doesn't decide; its row counted under :y"
    (let [p (update (lines-plot [9 1 1] [1 1 1] {:domain [0 2]}) :data #(cons {:x -1 :y nil :g "a" :w 9} (rest %)))]
      (is (= 2 (count (:lines (panel-data p)))))
      (is (= {:y 1 :size 0} (select-keys (:dropped (stats p)) [:y :size]))))))

(defn- gradient-plot
  "A line of three points coloured along its length by `cs`, under the colour scale `spec`."
  [ys cs spec & [extra]]
  (merge {:data (mapv (fn [x y c] {:x x :y y :c c}) [0 1 2] ys cs) :mapping {:x :x :y :y :color :c}
          :scales {:color spec} :charts [(chart/line)]}
         extra))

(defn- first-line [p] (first (:lines (panel-data p))))

(deftest along-length-break-test
  (testing "an invalid colour breaks the line without a domain, counted"
    (doseq [bad [##NaN ##Inf]]
      (let [p (gradient-plot [0 1 0] [0 bad 10] {:kind :linear})]
        (is (= [0 nil 0] (:y (first-line p))) (pr-str bad))
        (is (= 1 (get-in (stats p) [:dropped :color]))))))
  (testing ":as-na and :clamp keep the point (segment colouring)"
    (doseq [ood [:as-na :clamp]]
      (let [p (gradient-plot [0 1 0] [0 50 10] {:domain [0 10] :out-of-domain ood})]
        (is (= [0 1 0] (:y (first-line p))) (str ood))
        (is (zero? (get-in (stats p) [:dropped :color]))))))
  (testing "a point with an invalid y and an outside colour is counted once, under :y"
    (let [s (stats (gradient-plot [0 nil 0] [0 50 10] {:domain [0 10]}))]
      (is (= {:y 1 :color 0} (select-keys (:dropped s) [:y :color])))))
  (testing "a broken point's colour doesn't reach the stats: no NA entry for a dropped NA point"
    (is (not (contains? (:na (stats (gradient-plot [0 1 0] [0 nil 10] {:kind :linear} {:groups {:na false}}))) :color)))
    (is (contains? (:na (stats (gradient-plot [0 1 0] [0 nil 10] {:kind :linear}))) :color) "kept NA point"))
  (testing "every point outside: the line is left out, its points counted, the plot renders"
    (let [p (gradient-plot [0 1 0] [50 60 70] {:domain [0 10]})]
      (is (empty? (:lines (panel-data p))))
      (is (= 3 (get-in (stats p) [:dropped :color])))
      (is (some? (plot/render-image p size)))))
  (testing "a trail breaks at a point without a kept size (NA included)"
    (doseq [w [nil ##NaN]]
      (let [p {:data (mapv (fn [x w'] {:x x :y x :w w'}) [0 1 2] [1 w 2]) :mapping {:x :x :y :y :size :w}
               :charts [(chart/line {:trail true})]}]
        (is (= [0 nil 2] (:y (first-line p))) (pr-str w))
        (is (= 1 (get-in (stats p) [:dropped :size])))
        (is (some? (plot/render-image p size)))))))

(deftest bar-invalid-test
  (let [p {:data [{:l "a" :v 1 :c 1} {:l "a" :v 2 :c ##Inf} {:l "b" :v 3 :c nil} {:l "c" :v 4 :c ##NaN}]
           :mapping {:x :l :y :v :color :c} :charts [(chart/bar)]}
        bars (fn [q] (vec (mapcat (comp :bars val) (:data (stats q)))))]
    (testing "invalid colour rows are dropped and counted; a bar of only such rows vanishes; NA is nil only"
      (is (= [1.0 3.0] (mapv :length (bars p))))
      (is (= 2 (get-in (stats p) [:dropped :color])))
      (is (contains? (:na (stats p)) :color) "b's nil"))
    (testing "without an NA level the nil row goes too"
      (is (= [1.0] (mapv :length (bars (assoc p :groups {:na false})))))
      (is (= 3 (get-in (stats (assoc p :groups {:na false})) [:dropped :color]))))
    (testing ":out-of-domain :drop judges the bar's value"
      (let [q (assoc p :scales {:color {:domain [2 4] :out-of-domain :drop}})]
        (is (= [3.0] (mapv :length (bars q))) "a (1) dropped, b (NA) kept")
        (is (= 3 (get-in (stats q) [:dropped :color])) "two invalid rows and a's row")))))

(deftest binned-domain-test
  (testing "a breaks vector is the domain at compute, as when drawn"
    (let [spec {:kind :binned :bins [0 10 20] :domain [0 5]}]
      (is (aesthetic/kept-value? :color spec 15) "inside the breaks, beyond a contradicting :domain")
      (is (not (aesthetic/kept-value? :color spec 25)))
      (is (aesthetic/in-domain? :color {:kind :binned :bins [0 10 20]} 20) "the last break")))
  (testing "pairs: a :bins vector replaces the computed matrix domain"
    (let [rows [{:a 1 :b 2 :c 3} {:a 2 :b 1 :c 1} {:a 3 :b 3 :c 2} {:a 4 :b 5 :c 4}]
          p (plot/pairs rows {:variables [:a :b :c] :upper (chart/pairwise {:glyph :shade})
                              :scales {:color {:kind :binned :bins [-1 0 1]}}})]
      (is (= [{:kind :binned :bins [-1 0 1]}]
             (->> (tree-seq coll? seq p) (filter #(and (map? %) (map? (:color %)) (contains? (:color %) :bins))) (map :color) distinct))
          "the cells' colour scale holds no computed :domain")
      (is (some? (plot/render-image p {:width 400 :height 400})))))
  (testing "compute agrees with drawing on a built plot: a value beyond the last break is dropped, the scale's domain is the breaks'"
    (let [p {:data [{:x 0 :y 0 :c 5} {:x 1 :y 1 :c 25} {:x 2 :y 2 :c 15}] :mapping {:x :x :y :y :color :c}
             :scales {:color {:kind :binned :bins [0 10 20] :domain [0 nil]}} :charts [(chart/scatter)]}]
      (is (= 1 (get-in (stats p) [:dropped :color])))
      (is (= [0.0 20.0] (get-in (plot/build p size) [:scale-specs :color :domain]))))))

(deftest review-additions-test
  (testing "a line's size is decided by its first drawn point in drawing order (x order), not its first row"
    (let [p {:data [{:x 2 :y 0 :w 9} {:x 0 :y 0 :w 1} {:x 1 :y 1 :w 1}] :mapping {:x :x :y :y :size :w}
             :scales {:size {:domain [0 2]}} :charts [(chart/line)]}]
      (is (= [[[0 1 2] 1]] (mapv (juxt :x :size-value) (:lines (panel-data p)))))
      (is (zero? (get-in (stats p) [:dropped :size]))))
    (let [p {:data [{:x 0 :y 0 :c 50 :w 9} {:x 1 :y 1 :c 1 :w 1} {:x 2 :y 0 :c 2 :w 1}] :mapping {:x :x :y :y :color :c :size :w}
             :scales {:color {:domain [0 10]} :size {:domain [0 2]}} :charts [(chart/line)]}]
      (is (= [1] (mapv :size-value (:lines (panel-data p)))) "a point broken by its colour doesn't decide")
      (is (= {:color 1 :size 0} (select-keys (:dropped (stats p)) [:color :size])))))
  (testing "a line coloured along its length and sized: each count under its own aesthetic"
    (let [p (fn [ws] {:data (mapv (fn [x c w] {:x x :y x :c c :w w}) [0 1 2] [0 50 10] ws) :mapping {:x :x :y :y :color :c :size :w}
                      :scales {:color {:domain [0 10]} :size {:domain [0 2]}} :charts [(chart/line)]})]
      (is (= {:color 1 :size 0} (select-keys (:dropped (stats (p [1 1 1]))) [:color :size])))
      (is (= [[0 nil 2] 1] ((juxt :y :size-value) (first-line (p [1 1 1])))))
      (is (= {:color 0 :size 3} (select-keys (:dropped (stats (p [9 1 1]))) [:color :size])) "the line dropped by its size")))
  (testing "trail sizes outside the domain break under nil / :drop, not under :clamp / :as-na"
    (doseq [[ood y n] [[nil [0 nil 2] 1] [:drop [0 nil 2] 1] [:clamp [0 1 2] 0] [:as-na [0 1 2] 0]]]
      (let [p {:data [{:x 0 :y 0 :w 1} {:x 1 :y 1 :w 9} {:x 2 :y 2 :w 2}] :mapping {:x :x :y :y :size :w}
               :scales {:size {:domain [0 4] :out-of-domain ood}} :charts [(chart/line {:trail true})]}]
        (is (= y (:y (first-line p))) (str ood))
        (is (= n (get-in (stats p) [:dropped :size])) (str ood)))))
  (testing "an area fill and :gaps :bridge break at the same point"
    (doseq [c [(chart/line {:fill :color}) (chart/line {:gaps :bridge})]]
      (let [p (gradient-plot [0 1 0] [0 50 10] {:domain [0 10]} {:charts [c]})]
        (is (= [0 nil 0] (:y (first-line p))))
        (is (= 1 (get-in (stats p) [:dropped :color])))
        (is (some? (plot/render-image p size))))))
  (testing "line size boundaries: NaN or NA (no NA level) first size drops the line, :as-na keeps it, stacked lines"
    (is (= [3 1] ((juxt #(get-in % [:dropped :size]) #(count (get-in % [:data facet/single-panel :lines])))
                  (stats (lines-plot [##NaN 1 1] [1 1 1] {:kind :linear})))))
    (is (= 3 (get-in (stats (assoc (lines-plot [nil 1 1] [1 1 1] {:kind :linear}) :groups {:na false})) [:dropped :size])))
    (is (= [nil 1] (mapv :size-value (:lines (panel-data (lines-plot [nil 1 1] [1 1 1] {:kind :linear}))))) "NA kept with an NA level")
    (is (= [9 9] (mapv :size-value (:lines (panel-data (lines-plot [9 1 1] [9 1 1] {:domain [0 2] :out-of-domain :as-na}))))))
    (is (= 3 (get-in (stats (assoc-in (lines-plot [1 1 1] [9 1 1] {:domain [0 2]}) [:charts 0] (chart/line {:position :stack}))) [:dropped :size]))))
  (testing "{:na nil} is the default (NA kept), as {:na true}"
    (is (= [nil] (mapv :color-value (polygons (polygon-plot [nil 1 1 1] {:kind :linear} {:groups {:na nil}})))))
    (is (= [0 1 0] (:y (first-line (gradient-plot [0 1 0] [0 nil 10] {:kind :linear} {:groups {:na nil}}))))))
  (testing "facets: each panel's marks judged on their own"
    (let [p {:data (concat (map #(assoc %1 :c %2 :f "A") square [9 1 1 1]) (map #(assoc %1 :c %2 :f "B") square [1 9 1 1]))
             :mapping {:x :x :y :y :color :c :facet :f} :scales {:color {:domain [0 4]}} :charts [(chart/polygon)]}
          s (stats p)]
      (is (= [[] [[4 1]]] (mapv #(mapv (juxt (comp count :x) :color-value) (:polygons %)) (map (:data s) (sort (keys (:data s)))))))
      (is (= 4 (get-in s [:dropped :color])))))
  (testing "binned scales: a polygon and a line coloured along its length"
    (let [b {:kind :binned :bins [0 10 20]}]
      (is (empty? (polygons (polygon-plot [25 5 5 5] b))))
      (is (= [0 nil 0] (:y (first-line (gradient-plot [0 1 0] [5 25 15] b)))))))
  (testing "dates on a linear colour: the line breaks at the outside day; a polygon takes a date value"
    (let [d #(java.time.LocalDate/of 2024 1 (int %))]
      (is (= [0 nil 0] (:y (first-line (gradient-plot [0 1 0] [(d 1) (d 20) (d 3)] {:domain [(d 1) (d 10)]})))))
      (is (= [(d 2)] (mapv :color-value (polygons (polygon-plot [(d 2) (d 30) (d 3) (d 4)] {:domain [(d 1) (d 10)]}))))))))
