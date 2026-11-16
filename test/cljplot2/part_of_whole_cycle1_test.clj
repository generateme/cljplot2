(ns cljplot2.part-of-whole-cycle1-test
  "Part of a whole cycle 1: the hierarchy namespace, the partition chart (icicle, sunburst), label functions."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is are]]
            [clojure2d.color :as c]
            [fastmath.core :as m]
            [cljplot2.test-util :refer [error]]
            [cljplot2.chart :as chart]
            [cljplot2.chart.partition :as partition]
            [cljplot2.core :as plot]
            [cljplot2.data.hierarchy :as hierarchy]
            [cljplot2.layout.grid :as grid]
            [cljplot2.scale.coord :as coord])
  (:import [java.awt.image BufferedImage]))

(def ^:private rows
  [{:a "A" :b "a1" :v 3} {:a "A" :b "a2" :v 1} {:a "B" :b "b1" :v 4} {:a "B" :b nil :v 2}])

(def ^:private mapping {:value :v :hierarchy [:a :b]})
(def ^:private size {:width 400 :height 400})
(defn- build [p] (plot/build p size))
(defn- pt [extra] (merge {:data rows :mapping mapping :charts [(chart/partition)]} extra))
(defn- panel-data [built] (get-in built [:stats 0 :data :panel]))
(defn- span [cell] (mapv #(/ (Math/round (* 1000.0 (double (% cell)))) 1000.0) [:x0 :x1 :y0 :y1]))

(defn- tree-of [rs]
  (get-in (hierarchy/panel-trees {:panel rs} mapping {}) [:trees :panel]))

;; 1. hierarchy

(deftest hierarchy-test
  (testing "1.1 row paths stop at nil / \"\""
    (let [acc (hierarchy/level-accessors [:a :b :c])]
      (are [row path] (= path (hierarchy/path-of acc row))
        {:a "A" :b "x" :c "y"} ["A" "x" "y"]
        {:a "A" :b nil :c "y"} ["A"]
        {:a "A" :b "" :c "y"} ["A"]
        {:a nil :b "x"} [])))
  (testing "1.1 panel trees: values summed, top indices, dropped rows"
    (let [{:keys [trees top-count dropped]} (hierarchy/panel-trees {:p (conj rows {:a "C" :v 0}) :q [{:a "B" :v 1}]} mapping {})]
      (is (= 2 top-count) "C dropped (value 0)")
      (is (= {:value 1} dropped))
      (is (= [["A" 4.0 0] ["B" 6.0 1]] (map (juxt :name :value :top-index) (:children (trees :p)))))
      (is (= [["B" 1 ]] (map (juxt :name :top-index) (:children (trees :q)))) "indices agree across panels")))
  (testing "1.1 nothing kept: no tree"
    (is (= {} (:trees (hierarchy/panel-trees {:p [{:a "A" :v -1}] :q []} mapping {})))))
  (testing "1.2 depth and path: cells and node info of a 3-level tree"
    (let [t (get-in (hierarchy/panel-trees {:p [{:a "A" :b "b" :c "c" :v 1}]} {:value :v :hierarchy [:a :b :c]} {}) [:trees :p])
          paths (atom [])
          cs (partition/cells t {:order :value :group-labels (fn [i] (swap! paths conj (:path i)) nil)
                                 :leaf-labels (fn [i] (swap! paths conj (:path i)) nil)})]
      (is (= [1 2 3] (map :depth cs)))
      (is (= [["A"] ["A" "b"] ["A" "b" "c"]] @paths))))
  (testing "1.3 node info: shares of the total and the parent"
    (let [t (tree-of rows)
          a (first (:children t))
          infos (map #(hierarchy/node-info % 2 ["A" (:name %)] (:value a) (:value t)) (:children a))]
      (is (= [0.75 0.25] (map :parent-share infos)))
      (is (= 1.0 (reduce + (map :parent-share infos))) "siblings sum to 1")
      (is (= [0.3 0.1] (map :share infos)))
      (is (= {:name "a1" :value 3.0 :depth 2 :path ["A" "a1"]} (select-keys (first infos) [:name :value :depth :path])))))
  (testing "1.4 order: value descending (ties in data order), data"
    (let [node {:children [{:name "x" :value 1.0} {:name "y" :value 3.0} {:name "z" :value 1.0} {:name nil :value 2.0}]}]
      (is (= ["y" nil "x" "z"] (map :name (hierarchy/ordered-children node :value))))
      (is (= ["x" "y" "z" nil] (map :name (hierarchy/ordered-children node :data))))))
  (testing "label text: true - name, function, false / nil, empty results, unnamed nodes"
    (let [node {:name "n" :value 2.0} info {:name "n" :value 2.0}]
      (are [labels n t] (= t (hierarchy/label-text labels n info))
        true node "n"
        false node nil
        nil node nil
        (fn [i] (str (:name i) "!")) node "n!"
        (constantly nil) node nil
        (constantly "") node nil
        (constantly 12) node "12"
        true {:name nil :value 1.0} nil
        (constantly "x") {:name nil :value 1.0} nil))))

;; 2. partition

(deftest cells-test
  (testing "2.2 :value order: fractions of the total, nested, depth bands, unnamed leaf last"
    (let [cs (partition/cells (tree-of rows) {:order :value :root false :leaf-labels true :group-labels true})]
      (is (= [[0.0 0.6 0.0 1.0] [0.0 0.4 1.0 2.0] [0.4 0.6 1.0 2.0] [0.6 1.0 0.0 1.0] [0.6 0.9 1.0 2.0] [0.9 1.0 1.0 2.0]]
             (map span cs)))
      (is (= ["B" "b1" nil "A" "a1" "a2"] (map :label cs)))
      (is (= [1 2 2 1 2 2] (map :depth cs)))
      (is (= [false true true false true true] (map :leaf? cs)))
      (is (= [1 1 1 0 0 0] (map :top-index cs)) "descendants inherit the top-level index")))
  (testing "2.2 :data order"
    (is (= [[0.0 0.4 0.0 1.0] [0.0 0.3 1.0 2.0] [0.3 0.4 1.0 2.0] [0.4 1.0 0.0 1.0] [0.4 0.8 1.0 2.0] [0.8 1.0 1.0 2.0]]
           (map span (partition/cells (tree-of rows) {:order :data})))))
  (testing "2.2 per level the spans sum to the parents' (exact for whole leaves)"
    (let [cs (partition/cells (tree-of rows) {:order :value})]
      (is (m/delta-eq 1.0 (reduce + (map #(- (double (:x1 %)) (double (:x0 %))) (filter #(= 1 (:depth %)) cs)))))
      (is (m/delta-eq 1.0 (:x1 (last cs))))))
  (testing "2.2.2 :root true: the root on [0 1], levels shifted"
    (let [cs (partition/cells (tree-of rows) {:order :value :root true})]
      (is (= [0.0 1.0 0.0 1.0] (span (first cs))))
      (is (= 0 (:depth (first cs))))
      (is (= [1.0 2.0] ((juxt :y0 :y1) (second cs))))
      (is (= 3.0 (reduce max (map :y1 cs))))))
  (testing "one row, one level: one cell over everything"
    (is (= [[0.0 1.0 0.0 1.0]] (map span (partition/cells (tree-of [{:a "A" :v 5}]) {:order :value})))))
  (testing "labels: functions get the node info; unnamed nodes none"
    (let [cs (partition/cells (tree-of rows) {:order :value :leaf-labels #(format "%s %.2f" (:name %) (:share %))
                                              :group-labels false})]
      (is (= [nil "b1 0.40" nil nil "a1 0.30" "a2 0.10"] (map :label cs))))))

(deftest stats-test
  (let [built (build (pt {}))
        s (get-in built [:stats 0])]
    (testing "2.3 extents, padding, hidden axes"
      (is (= [0.0 1.0] (get-in built [:finalized-scales :x :domain])))
      (is (= [0.0 2.0] (get-in built [:finalized-scales :y :domain])))
      (is (= {:x [0 0] :y [0 0]} (:padding s)))
      (is (= {:x {:axis false} :y {:axis false}} (:scale-defaults s)))
      (is (not-any? #(#{:axis :axis-overhang} (get-in % [:content :kind])) (get-in built [:grid :items])))))
  (testing "2.3 plot scales override the suggestions"
    (is (some #(= :axis (get-in % [:content :kind]))
              (get-in (build (pt {:scales {:y {:axis true}}})) [:grid :items]))))
  (testing "facets: each panel fills [0 1]; the deepest panel sets the levels"
    (let [built (build (pt {:data (concat (map #(assoc % :f "p") rows) [{:a "C" :v 2 :f "q"} {:a "D" :v 6 :f "q"}])
                            :mapping (assoc mapping :facet :f)}))
          d (get-in built [:stats 0 :data])]
      (is (every? #(m/delta-eq 1.0 (reduce max (map :x1 (:cells %)))) (vals d)))
      (is (= [0.0 2.0] (get-in built [:finalized-scales :y :domain])))))
  (testing "empty data / nothing valid: no cells, renders"
    (is (= {} (get-in (build (pt {:data []})) [:stats 0 :data])))
    (is (= {:value 1} (get-in (build (pt {:data [{:a "A" :v ##NaN}]})) [:stats 0 :dropped])))
    (is (some? (plot/render (pt {:data []}) size)))
    (is (some? (plot/render (pt {:data [] :coord {:kind :polar}}) size)))))

(deftest schema-test
  (testing "2.1 mapping and options"
    (is (= [:charts 0 :options :mapping :hierarchy] (:in (error #(build (pt {:mapping {:value :v}}))))))
    (is (= [:charts 0 :options :mapping :value] (:in (error #(build (pt {:mapping {:hierarchy [:a]}}))))))
    (are [opts k] (= k (:key (error #(build (pt {:charts [(chart/partition opts)]})))))
      {:order :size} :order
      {:root 1} :root
      {:leaf-labels 1} :leaf-labels
      {:group-labels "x"} :group-labels
      {:border-width -1} :border-width
      {:pattern :nope} :pattern))
  (testing "2.6 the theme entry is checked"
    (is (some? (error #(build (pt {:theme {:chart {:partition {:border-width "1"}}}})))))))

;; drawing

(def ^:private plain {:plot {:background :white} :palette {:color [[255 0 0] [0 0 255]] :na [128 128 128]}
                      :chart {:partition {:border-color :none}}})

(defn- rgb [^BufferedImage img x y]
  (let [v (.getRGB img (int x) (int y))]
    [(bit-and (bit-shift-right v 16) 0xff) (bit-and (bit-shift-right v 8) 0xff) (bit-and v 0xff)]))

(defn- color-rgb [col] (let [col (c/to-color col)] (mapv #(long (Math/round (double %))) [(c/red col) (c/green col) (c/blue col)])))
(defn- close? [a b] (every? true? (map #(<= (m/abs (- (long %1) (long %2))) 1) a b)))

(defn- render [p] (let [p (update p :theme #(merge plain %))] [(build p) (plot/render-image p size)]))
(defn- panel-box [built] (grid/item-box (:layout built) :panel))

(defn- cell-pixel
  "Pixel of a cartesian icicle at data fractions `fx` (of x) and `fy` (of the panel height from the bottom)."
  [[built img] fx fy]
  (let [{:keys [x y w h]} (panel-box built)]
    (rgb img (+ (long x) (long (* (double fx) (long w)))) (- (+ (long y) (long h)) 1 (long (* (double fy) (long h)))))))

(defn- polar-pixel
  "Pixel of a polar plot at `deg` (clockwise from 12 o'clock) and `fr` (fraction of the outer radius)."
  [[built img] deg fr]
  (let [{:keys [x y]} (panel-box built)
        {:keys [cx cy radius]} (get-in built [:finalized-scales :x :coord])
        a (m/radians (double deg)) r (* (double fr) (double radius))]
    (rgb img (+ (long x) (m/round (+ (double cx) (* r (m/sin a))))) (+ (long y) (m/round (- (double cy) (* r (m/cos a))))))))

(def ^:private no-labels [(chart/partition {:leaf-labels false :group-labels false})])

(deftest colors-test
  (testing "2.5.1 top-level palette colors, lighter by depth; 2.8 the root at the bottom"
    (let [r (render (pt {:charts no-labels}))
          blue [0 0 255]]
      (is (= blue (cell-pixel r 0.3 0.25)) "B, depth 1, bottom half")
      (is (= [255 0 0] (cell-pixel r 0.8 0.25)) "A")
      (is (close? (color-rgb (hierarchy/depth-shade blue 2)) (cell-pixel r 0.2 0.75)) "b1, depth 2 - lighter, above")
      (is (close? (color-rgb (hierarchy/depth-shade [255 0 0] 2)) (cell-pixel r 0.95 0.75)))))
  (testing ":root true - the root cell in the NA color"
    (is (= [128 128 128] (cell-pixel (render (pt {:charts [(chart/partition {:root true :leaf-labels false :group-labels false})]})) 0.5 0.1))))
  (testing "2.5.2 mapped :color: leaves mapped, groups NA, legend"
    (let [r (render (pt {:data (map #(assoc % :k (if (= "A" (:a %)) [0 200 0] [200 0 200])) rows)
                         :mapping (assoc mapping :color :k) :charts no-labels :scales {:color {:kind :identity}}}))]
      (is (= [128 128 128] (cell-pixel r 0.3 0.25)) "group B")
      (is (= [200 0 200] (cell-pixel r 0.2 0.75)) "leaf b1")
      (is (= [0 200 0] (cell-pixel r 0.95 0.75)) "leaf a2"))
    (is (some #(= :legends (:id %)) (get-in (build (pt {:mapping (assoc mapping :color :a)})) [:grid :items]))))
  (testing "2.6 pattern ink inside a cell"
    (let [[built img] (render (pt {:charts [(chart/partition {:pattern :hatch :leaf-labels false :group-labels false})]}))
          {:keys [x y w h]} (panel-box built)
          colors (set (for [i (range 20) j (range 20)] (rgb img (+ (long x) (quot (long w) 4) i) (+ (long y) (quot (* 3 (long h)) 4) j))))]
      (is (< 1 (count colors)))))
  (testing "2.4 sunburst: sectors from 12 o'clock clockwise, depth rings outward; no gap at 12 o'clock"
    (let [r (render (pt {:coord {:kind :polar} :charts no-labels}))]
      (is (= [0 0 255] (polar-pixel r 90 0.25)) "B over 0..216 degrees, inner ring")
      (is (= [255 0 0] (polar-pixel r 300 0.25)) "A over 216..360")
      (is (close? (color-rgb (hierarchy/depth-shade [0 0 255] 2)) (polar-pixel r 60 0.75)) "b1, outer ring")
      (is (= [0 0 255] (polar-pixel r 1 0.4)) "just right of 12 o'clock")
      (is (= [255 0 0] (polar-pixel r 359 0.4)) "just left of it")))
  (testing ":inner leaves a hole; flipped polar and tiny canvases render"
    (let [r (render (pt {:coord {:kind :polar :inner 0.4} :charts no-labels}))]
      (is (= [255 255 255] (polar-pixel r 90 0.2))))
    (is (some? (plot/render (pt {:coord {:kind :polar} :flip true}) size)))
    (is (some? (plot/render (pt {}) {:width 20 :height 20})))
    (is (some? (plot/render (pt {:coord {:kind :polar}}) {:width 20 :height 20})))))

(defn- dark? [[r g b]] (and (< (long r) 80) (< (long g) 80) (< (long b) 80)))

(defn- dark-count [[_ ^BufferedImage img]]
  (count (filter dark? (for [i (range (.getWidth img)) j (range (.getHeight img))] (rgb img i j)))))

(def ^:private one [{:a "Group" :b "leaf" :v 1}])

(def ^:private light-fills
  "Light fills: labels in black (see [[cljplot2.graphics.draw/contrast-color]])."
  {:palette {:color [[255 230 150] [200 230 255]]}})

(deftest labels-test
  (testing "2.7.1 cartesian: drawn when they fit, else hidden"
    (is (pos? (dark-count (render (pt {:data one :theme light-fills})))))
    (is (zero? (dark-count (render (pt {:data one :charts [(chart/partition {:leaf-labels false :group-labels false})]})))))
    (is (zero? (dark-count (render (pt {:data [{:a (apply str (repeat 80 "W")) :v 1}]})))) "too wide"))
  (testing "2.7.2 polar: along the radius when they fit"
    (is (pos? (dark-count (render (pt {:data one :theme light-fills :coord {:kind :polar}})))))
    (is (zero? (dark-count (render (pt {:data [{:a (apply str (repeat 60 "W")) :v 1}] :coord {:kind :polar}}))))
        "wider than the ring")
    (is (zero? (dark-count (render (pt {:data (cons {:a "W" :v 1} (for [i (range 400)] {:a (str "g" i) :v 100}))
                                        :coord {:kind :polar} :charts [(chart/partition {:order :data})]}))))
        "taller than its arc: thin sectors"))
  (testing "2.7.3 roles: the theme's text role styles the labels"
    (is (zero? (dark-count (render (pt {:data one :theme {:text {:partition-leaf {:size 300} :partition-group {:size 300}}}}))))
        "too big to fit")))

;; 3. label functions

(defn- tm [opts] {:data one :mapping mapping :theme (-> plain (merge light-fills) (assoc-in [:chart :treemap] {:padding 0 :border-color :none})) :charts [(chart/treemap (merge {:group-labels false} opts))]})

(deftest treemap-label-functions-test
  (testing "3.1 the function gets the node info"
    (let [seen (atom nil)]
      (render (tm {:leaf-labels (fn [i] (reset! seen i) "x")}))
      (is (= {:name "leaf" :value 1.0 :depth 2 :path ["Group" "leaf"] :share 1.0 :parent-share 1.0} @seen))))
  (testing "3.1 nil / \"\" - no label; two lines drawn"
    (is (zero? (dark-count (render (tm {:leaf-labels (constantly nil)})))))
    (is (zero? (dark-count (render (tm {:leaf-labels (constantly "")})))))
    (let [one-line (dark-count (render (tm {:leaf-labels (constantly "name")})))
          two-lines (dark-count (render (tm {:leaf-labels (constantly "name\nname")})))]
      (is (pos? one-line))
      (is (< one-line two-lines))))
  (testing "3.2 fit checks use the text: too many lines - hidden"
    (is (zero? (dark-count (render (tm {:leaf-labels (constantly (str/join "\n" (repeat 60 "x")))}))))))
  (testing "3.1 group headers: the function's text, none for nil (no strip)"
    (let [top (fn [opts] (let [[built img] (render (tm (merge {:leaf-labels false} opts)))
                               {:keys [x y w]} (panel-box built)]
                           (rgb img (+ (long x) (quot (long w) 2)) (+ (long y) 3))))]
      (is (not= [255 230 150] (top {:group-labels #(str "G " (:value %))})) "header strip")
      (is (= [255 230 150] (top {:group-labels (constantly nil)})) "no strip"))))

(deftest docs-test
  (testing "2.9 the sunburst is documented"
    (is (str/includes? (:doc (meta #'chart/partition)) "sunburst"))
    (is (str/includes? (:doc (meta (find-ns 'cljplot2.chart.partition))) ":coord {:kind :polar}")))
  (testing "3.3 the share format example"
    (is (str/includes? (:doc (meta (find-ns 'cljplot2.chart.treemap))) ":share"))))

;; review fixes

(defn- light? [[r g b]] (and (> (long r) 200) (> (long g) 200) (> (long b) 200)))

(defn- count-in-panel [pred [built ^BufferedImage img]]
  (let [{:keys [x y w h]} (panel-box built)]
    (count (filter pred (for [i (range (long x) (+ (long x) (long w))) j (range (long y) (+ (long y) (long h)))] (rgb img i j))))))

(deftest review-fixes-test
  (testing "U1 labels in black or white by the fill: partition and treemap"
    (let [dark {:palette {:color [[0 0 110]]}}
          light {:palette {:color [[250 240 120]]}}]
      (is (pos? (count-in-panel light? (render (pt {:data [{:a "Group" :v 1}] :theme dark})))) "white text on a dark cell")
      (is (pos? (count-in-panel dark? (render (pt {:data [{:a "Group" :v 1}] :theme light})))))
      (is (pos? (count-in-panel light? (render (pt {:data [{:a "Group" :v 1}] :theme dark :coord {:kind :polar}})))))
      (is (pos? (count-in-panel light? (render (assoc (tm {:group-labels false}) :data [{:a "G" :b "Leaf" :v 1}]
                                                      :theme (merge plain dark {:chart {:treemap {:padding 0 :border-color :none}}}))))))))
  (testing "U2 depth shade: L* a quarter of the way to 90 per level; never past it; light colors kept"
    (let [l (fn [col] (.x ^fastmath.vector.Vec4 (c/to-LAB col)))
          blue [0 0 255]
          muted [90 110 130]
          l1 (double (l muted))]
      (is (= blue (hierarchy/depth-shade blue 1)) "the top level: the color itself")
      (is (< (m/abs (- (double (l (hierarchy/depth-shade muted 2))) (+ l1 (* 0.25 (- 90.0 l1))))) 0.5))
      (is (< (m/abs (- (double (l (hierarchy/depth-shade muted 3))) (+ l1 (* 0.4375 (- 90.0 l1))))) 0.5))
      (is (< (double (l blue)) (double (l (hierarchy/depth-shade blue 2)))) "saturated: lighter (RGB clamped)")
      (is (< (double (l (hierarchy/depth-shade [174 199 232] 4))) 90.0) "a light palette color stays below 90")
      (is (< (double (l (hierarchy/depth-shade blue 9))) (double (l (hierarchy/depth-shade blue 10))) 90.5) "monotone, bounded")
      (is (= [250 250 250] (hierarchy/depth-shade [250 250 250] 3)) "lighter than the ceiling: kept")
      (is (< (m/abs (- (double (l (hierarchy/depth-shade [0 0 0] 2))) 22.5)) 0.5) "black")))
  (testing "U4 a reversed y domain puts the root at the top"
    (let [r (render (pt {:charts no-labels :scales {:y {:domain [2 0]}}}))]
      (is (= [0 0 255] (cell-pixel r 0.3 0.75)) "B (depth 1) in the top half")
      (is (str/includes? (:doc (meta (find-ns 'cljplot2.chart.partition))) ":domain [n 0]"))))
  (testing "U3 partition listed among the polar charts"
    (is (str/includes? (:doc (meta (find-ns 'cljplot2.chart))) "[[partition]]"))
    (is (str/includes? (:doc (meta (find-ns 'cljplot2.pipeline))) "parallel, partition")))
  (testing "U5 the treemap legend key: a square in the leaf color"
    (let [[built ^BufferedImage img] (render (assoc (tm {}) :mapping (assoc mapping :color :b)))
          legend (some #(when (= :legends (:id %)) %) (get-in built [:grid :items]))]
      (is (some? legend))
      (is (true? (boolean (some #(= [255 230 150] %) (for [i (range (- (.getWidth img) 120) (.getWidth img)) j (range (.getHeight img))]
                                                         (rgb img i j)))))
          "the legend's key in the first palette color"))))

(deftest sector-fit-test
  (testing "U7 sector boxes"
    (let [box (fn [coord] (mapv #(/ (Math/round (* 1000.0 (double %))) 1000.0) (coord/sector-box (assoc coord :kind :polar))))]
      (are [coord b] (= b (box coord))
        {} [-1.0 -1.0 1.0 1.0]
        {:start 45} [-1.0 -1.0 1.0 1.0]
        {:inner 0.5} [-1.0 -1.0 1.0 1.0]
        {:span 180 :start -90} [-1.0 -1.0 1.0 0.0]
        {:span 90} [0.0 -1.0 1.0 0.0]
        {:span 90 :inner 0.5} [0.0 -1.0 1.0 0.0]
        {:span 90 :direction :counter-clockwise} [-1.0 -1.0 0.0 0.0]
        {:start 30 :span 30 :inner 0.5} [0.25 -0.866 0.866 -0.25]
        {:start 90 :span 180} [-1.0 0.0 1.0 1.0])))
  (testing "U7 geometry: a full circle centred, a half circle on the panel's bottom edge"
    (let [g (coord/geometry {:kind :polar} 400.0 300.0 100.0)]
      (is (= [200.0 150.0] [(:cx g) (:cy g)])))
    (let [g (coord/geometry {:kind :polar :span 180 :start -90} 400.0 200.0 200.0)]
      (is (= [200.0 200.0] [(:cx g) (:cy g)]))))
  (testing "U7 a half circle gets the larger radius and stays in the panel"
    (let [p {:data [{:a "A" :v 1} {:a "B" :v 2}] :mapping {:value :v :hierarchy [:a]} :charts [(chart/partition)]
             :coord {:kind :polar :span 180 :start -90}}
          built (plot/build p {:width 700 :height 450})
          {:keys [w h]} (panel-box built)
          {:keys [cy radius]} (get-in built [:finalized-scales :x :coord])
          full (get-in (plot/build (assoc p :coord {:kind :polar}) {:width 700 :height 450}) [:finalized-scales :x :coord])]
      (is (= (* 0.5 (m/min (double w) (double h))) (:radius full)) "a full circle: half the smaller side (no angle labels)")
      (is (> (double radius) (* 1.5 (double (:radius full)))))
      (is (< (m/abs (- (* 2.0 (double radius)) (double w))) 0.5) "width-limited: the diameter spans the panel")
      (is (< (m/abs (- (- (double cy) (double radius)) (- (double h) (double cy)))) 0.5)
          "the half circle's box centred: equal room above the arc and below the centre")
      (is (<= 0.0 (- (double cy) (double radius)))))))
