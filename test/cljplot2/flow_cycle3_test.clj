(ns cljplot2.flow-cycle3-test
  "Flow cycle 3: polygon chart, per-group line width, node order, chord and arc layouts and plots."
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.test-util :refer [error error-ins]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.graph :as graph]
            [cljplot2.theme :as theme])
  (:import [java.awt.image BufferedImage]))

(defn- rgb [^BufferedImage img x y]
  (let [v (.getRGB img (int x) (int y))]
    [(bit-and (bit-shift-right v 16) 0xff) (bit-and (bit-shift-right v 8) 0xff) (bit-and v 0xff)]))

(defn- painted
  "`[[x y] ..]` of the pixels of `color` in `img`."
  [^BufferedImage img color]
  (for [x (range (.getWidth img)) y (range (.getHeight img)) :when (= color (rgb img x y))] [x y]))

(defn- span [pts k] (let [vs (map k pts)] (if (seq vs) (- (long (apply max vs)) (long (apply min vs))) -1)))

(def ^:private plain {:plot {:background :white :margin 0} :panel {:fill :white :alternate-fill :none :grid {:x :none :y :none}}})

(defn- image [p size] (plot/render-image (-> p (update :theme #(merge plain %)) (update :labels #(merge {:x :none :y :none} %))) size))

(defn- stats [p] (first (:stats (plot/build p {:width 400 :height 400}))))

(def ^:private red [255 0 0])
(def ^:private blue [0 0 255])

(defn- key-pixels
  "Pixels of `color` in a 40 x 40 legend key of chart `ch` drawn with `attributes`."
  [ch attributes color]
  (let [canvas (c2d/canvas 40 40)]
    (c2d/with-canvas-> canvas (c2d/set-background :white))
    (c2d/with-canvas [c canvas] ((:legend ch) ch attributes c {:x 0 :y 0 :w 40 :h 40} {:theme theme/default-theme}))
    (painted (c2d/get-image canvas) color)))

;; 1. polygon

(def ^:private triangle [{:x 0 :y 0} {:x 1 :y 0} {:x 0.5 :y 1}])
(def ^:private unit {:x {:domain [0 1] :nice false :expand [0 0] :axis false}
                     :y {:domain [0 1] :nice false :expand [0 0] :axis false}})

(deftest polygon-test
  (testing "1.1 a triangle fills its pixels, nothing outside"
    (let [img (image {:data triangle :mapping {:x :x :y :y} :scales unit
                      :charts [(chart/polygon {:color red :padding {:x [0 0] :y [0 0]}})]}
                     {:width 200 :height 200})
          pts (painted img red)]
      (is (= red (rgb img 100 150)) "inside, near the base")
      (is (not= red (rgb img 10 10)) "outside, top left corner")
      (is (< 0.4 (/ (double (count pts)) (* 0.5 200 200)) 1.1) "about half the panel")))
  (testing "1.1 groups in row order: a square's points in order - not a bow tie"
    (let [img (image {:data [{:x 0 :y 0} {:x 1 :y 0} {:x 1 :y 1} {:x 0 :y 1}] :mapping {:x :x :y :y} :scales unit
                      :charts [(chart/polygon {:color red :padding {:x [0 0] :y [0 0]}})]}
                     {:width 100 :height 100})]
      (is (= red (rgb img 50 5)))
      (is (= red (rgb img 5 50)))))
  (testing "1.1 fewer than 3 points - not drawn, counted; invalid coordinates dropped per key"
    (let [s (stats {:data (into (mapv #(assoc % :g "a") triangle)
                                [{:x 0 :y 0 :g "b"} {:x 1 :y 1 :g "b"} {:x nil :y 1 :g "a"} {:x 2 :y ##NaN :g "c"}])
                    :mapping {:x :x :y :y :group :g} :charts [(chart/polygon)]})]
      (is (= 2 (get-in s [:dropped :group])) "b (2 points) and c (none valid)")
      (is (= 1 (get-in s [:dropped :x])))
      (is (= 1 (get-in s [:dropped :y])))
      (is (= 1 (count (get-in s [:data :panel :polygons]))))))
  (testing "1.1 an undrawn polygon's level doesn't reach the legend"
    (let [s (stats {:data (into (mapv #(assoc % :g "a") triangle) [{:x 0 :y 0 :g "b"}])
                    :mapping {:x :x :y :y :color :g} :charts [(chart/polygon)]})]
      (is (= 1 (get-in s [:dropped :group])))
      (is (= ["a"] (get-in s [:extents :panel :color :values])) (pr-str (get-in s [:extents :panel :color])))))
  (testing "1.1 ordinal colour per group; a linear colour from the group's first row"
    (let [rows (fn [g c dx] (map #(assoc % :g g :c c :x (+ dx (double (:x %)))) triangle))
          data (concat (rows "a" 1 0) (rows "b" 5 2))
          s (stats {:data (concat (rows "a" 1 0) [{:x 0.5 :y 0.5 :g "a" :c 99}] (rows "b" 5 2))
                    :mapping {:x :x :y :y :group :g :color :c} :charts [(chart/polygon)]})
          img (image {:data data :mapping {:x :x :y :y :color :g} :scales {:color {:range {"a" red "b" blue}}}
                      :charts [(chart/polygon)] :legend false}
                     {:width 300 :height 100})]
      (is (= [1 5] (mapv :color-value (get-in s [:data :panel :polygons]))) "the first row's value, not the later 99")
      (is (seq (painted img red)))
      (is (seq (painted img blue)))
      (is (contains? (:na (stats {:data (concat (rows "a" nil 0) (rows "b" 5 2)) :mapping {:x :x :y :y :group :g :color :c}
                                  :charts [(chart/polygon)]}))
                     :color)
          "an NA first row - the NA colour")))
  (testing "1.1 alpha and outline"
    (let [img (image {:data triangle :mapping {:x :x :y :y} :scales unit
                      :charts [(chart/polygon {:color red :alpha 0.5 :stroke-color blue :stroke-width 3 :padding {:x [0 0] :y [0 0]}})]}
                     {:width 200 :height 200})]
      (is (empty? (painted img red)) "half transparent - no pure red")
      (is (seq (painted img blue)) "the outline")))
  (testing "1.1 legend key - a filled square"
    (is (> (count (key-pixels (chart/polygon {:color red}) {} red)) 300)))
  (testing "1.1 polar rejected by the pipeline; no rows build"
    (is (contains? (error-ins #(plot/build {:data triangle :mapping {:x :x :y :y} :coord {:kind :polar}
                                            :charts [(chart/polygon)]}))
                   [:charts 0]))
    (is (some? (plot/build {:data [] :mapping {:x :x :y :y} :charts [(chart/polygon)]})))))

;; 2. line width per group

(defn- lines-data [w1 w2]
  [{:x 0 :y 0 :g "a" :w w1} {:x 1 :y 0 :g "a" :w w1} {:x 0 :y 1 :g "b" :w w2} {:x 1 :y 1 :g "b" :w w2}])

(defn- line-image [data mapping]
  (image {:data data :mapping mapping
          :scales {:x {:domain [0 1]} :y {:domain [-1 2]} :size {:range [1 9]}}
          :charts [(chart/line {:color red})] :legend false}
         {:width 200 :height 300}))

(deftest line-width-test
  (testing "2.1 a linear :size - each group's width from its value"
    (let [pts (painted (line-image (lines-data 1 5) {:x :x :y :y :group :g :size :w}) red)
          lower (filter #(> (long (second %)) 150) pts)
          upper (filter #(< (long (second %)) 150) pts)]
      (is (<= (span lower second) 2) "value 1 - the range's low end")
      (is (>= (span upper second) 7) "value 5 - the high end")))
  (testing "2.1 a linear :size doesn't group: without :group the rows form one line"
    (is (= 1 (count (get-in (stats {:data (lines-data 1 5) :mapping {:x :x :y :y :size :w} :charts [(chart/line)]})
                            [:data :panel :lines])))))
  (testing "2.1 NA size - the style width, counted as NA; outside an explicit domain - dropped"
    (let [s (stats {:data (lines-data nil 5) :mapping {:x :x :y :y :group :g :size :w} :charts [(chart/line)]})]
      (is (contains? (:na s) :size))
      (is (= [nil 5] (mapv :size-value (get-in s [:data :panel :lines])))))
    (is (= 2 (get-in (stats {:data (lines-data 1 5) :mapping {:x :x :y :y :group :g :size :w}
                             :scales {:size {:domain [0 2]}} :charts [(chart/line)]})
                     [:dropped :size]))))
  (testing "2.1 an ordinal :size groups and sets the width"
    (let [pts (painted (line-image (lines-data "thin" "thick") {:x :x :y :y :size :w}) red)]
      (is (not= (span (filter #(> (long (second %)) 150) pts) second)
                (span (filter #(< (long (second %)) 150) pts) second)))))
  (testing "2.1 stacked lines keep their widths"
    (is (= [1 5] (mapv :size-value (get-in (stats {:data (lines-data 1 5) :mapping {:x :x :y :y :group :g :size :w}
                                                   :charts [(chart/line {:position :stack})]})
                                           [:data :panel :lines])))))
  (testing "2.1 legend lines of the widths"
    (let [ch (chart/line {:color red})]
      (is (< (span (key-pixels ch {:size 1} red) second) (span (key-pixels ch {:size 9} red) second))))))

;; 3. node order

(def ^:private blocks
  "Two dense blocks {a b c} and {x y z}, interleaved in input order, one weak link between them."
  (concat (for [[f t] [["a" "b"] ["b" "c"] ["a" "c"] ["x" "y"] ["y" "z"] ["x" "z"]]] {:from f :to t :value 5})
          [{:from "c" :to "x" :value 1}]))

(def ^:private interleaved [{:id "a"} {:id "x"} {:id "b"} {:id "y"} {:id "c"} {:id "z"}])

(deftest node-order-test
  (let [g (graph/graph blocks {:nodes interleaved})]
    (testing "3.1 nil - input order; ids first, the rest in input order, repeats once"
      (is (= ["a" "x" "b" "y" "c" "z"] (graph/node-order g nil)))
      (is (= ["z" "a" "x" "b" "y" "c"] (graph/node-order g ["z"])))
      (is (= ["b" "a" "x" "y" "c" "z"] (graph/node-order g ["b" "a" "b"])))
      (is (= ["a" "x" "b" "y" "c" "z"] (graph/node-order g []))))
    (testing "3.1 unknown ids and other orders throw"
      (is (= :order (:key (error #(graph/node-order g ["a" "q"])))))
      (is (= :order (:key (error #(graph/node-order g :degree))))))
    (testing "3.1 :cluster - the dense blocks contiguous"
      (let [o (graph/node-order g :cluster)
            block (fn [id] (if (#{"a" "b" "c"} id) 0 1))]
        (is (= 6 (count (distinct o))))
        (is (= 2 (count (partition-by block o))) (str o))))
    (testing "3.1 :cluster - a hub with heavy ties stays in its block (cosine, not weight size)"
      (let [heavy (concat blocks (for [t ["a" "b"]] {:from "h" :to t :value 50}) [{:from "h" :to "c" :value 50}])
            o (graph/node-order (graph/graph heavy {:nodes (conj interleaved {:id "h"})}) :cluster)
            block (fn [id] (if (#{"a" "b" "c" "h"} id) 0 1))]
        (is (= 2 (count (partition-by block o))) (str o))
        (is (< 0 (.indexOf ^java.util.List o "h") 6) (str "not at an end (raw Euclidean puts it first): " o)))))
  (testing "3.1 1- and 2-node graphs, no nodes"
    (is (= ["a"] (graph/node-order (graph/graph [] {:nodes [{:id "a"}]}) :cluster)))
    (is (= ["a" "b"] (graph/node-order (graph/graph [{:from "a" :to "b"}]) :cluster)))
    (is (= [] (graph/node-order (graph/graph []) :cluster)))))

;; 4. chord

(def ^:private flows
  [{:from "a" :to "b" :value 3} {:from "b" :to "a" :value 1} {:from "a" :to "c" :value 2} {:from "c" :to "b" :value 4}])

(defn- width [{:keys [start end]}] (- (double end) (double start)))

(def ^:private chord-angles @#'graph/chord-angles)

(deftest chord-layout-test
  (testing "4.1 directed - group angles proportional to out + in, pads between"
    (let [{:keys [groups dropped]} (graph/chord-layout (graph/graph flows {:directed true}) {:gap-angle 2})
          sums {"a" 6.0 "b" 8.0 "c" 6.0}
          room (- 360.0 (* 3 2.0))]
      (is (zero? dropped))
      (doseq [{:keys [id] :as gr} groups]
        (is (< (m/abs (- (width gr) (* room (/ (sums id) 20.0)))) 1e-9) id))
      (is (< (m/abs (- 2.0 (- (double (:start (second groups))) (double (:end (first groups)))))) 1e-9) "the pad")
      (is (== 0.0 (:start (first groups))) "from the top")))
  (testing "4.1 undirected - group angles proportional to the pairs' summed weights"
    (let [{:keys [groups]} (graph/chord-layout (graph/graph flows) {:gap-angle 0})
          ;; a-b 4, a-c 2, b-c 4
          sums {"a" 6.0 "b" 8.0 "c" 6.0}]
      (doseq [{:keys [id] :as gr} groups]
        (is (< (m/abs (- (width gr) (* 360.0 (/ (sums id) 20.0)))) 1e-9) id))))
  (testing "4.1 ribbon end widths match the matrix"
    (let [w [[0.0 3.0 2.0] [1.0 0.0 0.0] [0.0 4.0 0.0]]
          k (/ m/TWO_PI 20.0)
          end-width (fn [[a0 a1]] (/ (- (double a1) (double a0)) k))
          [_ directed] (chord-angles w [0 1 2] true 0.0)
          [_ undirected] (chord-angles w [0 1 2] false 0.0)]
      (are [ij src tgt] (and (< (m/abs (- src (end-width (get-in directed [ij :source])))) 1e-9)
                             (< (m/abs (- tgt (end-width (get-in directed [ij :target])))) 1e-9))
        [0 1] 3.0 3.0
        [1 0] 1.0 1.0
        [2 1] 4.0 4.0)
      ;; undirected: group sums over rows only - 5, 1, 4
      (let [k (/ m/TWO_PI 10.0)]
        (is (< (m/abs (- 3.0 (/ (width (zipmap [:start :end] (get-in undirected [[0 1] :source]))) k))) 1e-9) "w(a, b) at a")
        (is (< (m/abs (- 1.0 (/ (width (zipmap [:start :end] (get-in undirected [[0 1] :target]))) k))) 1e-9) "w(b, a) at b"))))
  (testing "4.1 a node without flow - a zero-width group, no ribbons, no sector rows"
    (let [{:keys [groups ribbons arcs]} (graph/chord-layout (graph/graph flows {:nodes [{:id "q"}] :directed true}) {})
          q (first (filter #(= "q" (:id %)) groups))]
      (is (zero? (width q)))
      (is (zero? (:value q)))
      (is (not-any? #(#{"q"} (:from %)) ribbons))
      (is (not-any? #(= "q" (:group %)) arcs))))
  (testing "4.1 :min-value drops and counts weak edges; :order sets the ring order"
    (let [{:keys [groups ribbons dropped]} (graph/chord-layout (graph/graph flows {:directed true}) {:min-value 2 :order ["c"]})]
      (is (= 1 dropped))
      (is (= ["c" "a" "b"] (mapv :id (sort-by :start groups))))
      (is (= #{["a" "b"] ["a" "c"] ["c" "b"]} (set (map (juxt :from :to) ribbons))))))
  (testing "4.1 :min-value on summed weights: a pair's exchange (undirected), repeated directed edges"
    (let [weak [{:from "a" :to "b" :value 0.06} {:from "b" :to "a" :value 0.06} {:from "a" :to "c" :value 0.05}]
          undirected (graph/chord-layout (graph/graph weak) {:min-value 0.1})
          directed (graph/chord-layout (graph/graph weak {:directed true}) {:min-value 0.1})
          repeated (graph/chord-layout (graph/graph [{:from "a" :to "b" :value 0.06} {:from "a" :to "b" :value 0.06}]
                                                    {:directed true})
                                       {:min-value 0.1})]
      (is (= #{["a" "b"]} (set (map (juxt :from :to) (:ribbons undirected)))) "0.06 + 0.06 kept, a-c dropped")
      (is (= 1 (:dropped undirected)))
      (is (empty? (:ribbons directed)))
      (is (= 3 (:dropped directed)))
      (is (= #{["a" "b"]} (set (map (juxt :from :to) (:ribbons repeated)))) "two rows of 0.06 - one flow of 0.12")
      (is (zero? (:dropped repeated)))
      (is (zero? (:dropped (graph/chord-layout (graph/graph weak) {:min-value 0}))) "0 keeps every edge")))
  (testing "4.1 undirected ribbons from their end earlier in the ring"
    (let [{:keys [ribbons]} (graph/chord-layout (graph/graph flows) {:order ["c"]})]
      (is (= #{["c" "a"] ["c" "b"] ["a" "b"]} (set (map (juxt :from :to) ribbons))))))
  (testing "4.1 ribbons and sectors inside the unit circle; ribbons end at :inner"
    (let [{:keys [arcs ribbons]} (graph/chord-layout (graph/graph flows {:directed true}) {:inner-radius 0.8})
          r (fn [{:keys [x y]}] (m/hypot (double x) (double y)))]
      (is (every? #(<= (- 0.8 1e-9) (r %) (+ 1.0 1e-9)) arcs))
      (is (every? #(<= (r %) (+ 0.8 1e-9)) ribbons))))
  (testing "4.1 no edges - equal spacing, zero widths; bad :gap-angle / :inner throw"
    (let [{:keys [groups ribbons]} (graph/chord-layout (graph/graph [] {:nodes [{:id "a"} {:id "b"}]}) {})]
      (is (= [0.0 0.0] (mapv width groups)))
      (is (empty? ribbons)))
    (are [opts k] (= k (:key (error #(graph/chord-layout (graph/graph flows) opts))))
      {:gap-angle -1} :gap-angle
      {:gap-angle 120} :gap-angle
      {:gap-angle ##NaN} :gap-angle
      {:inner 1} :inner
      {:inner 0} :inner
      {:min-value "x"} :min-value
      {:min-value -1} :min-value
      {:min-value ##NaN} :min-value)))

(deftest chord-plot-test
  (testing "4.2 sectors coloured by node, ribbons by source at alpha, labels outside the ring"
    (let [p (plot/chord flows {:directed true :title "t"})
          [ribbons sectors & labels] (:charts p)]
      (is (= [:polygon :polygon] (mapv :kind [ribbons sectors])))
      (is (= :from (get-in ribbons [:options :mapping :color])))
      (is (= 0.6 (get-in ribbons [:options :alpha])))
      (is (= ["a" "b" "c"] (get-in p [:scales :color :domain])))
      (is (every? (fn [{:keys [x y]}] (> (m/hypot (double x) (double y)) 1.0)) (mapcat #(get-in % [:options :data]) labels)))
      (is (= :iso (:aspect p)))
      (is (instance? BufferedImage (plot/render-image p {:width 300 :height 300})))))
  (testing "4.2 user :scales merge key by key: a palette keeps the ring-order domain and the hidden legend"
    (let [sc (get-in (plot/chord flows {:scales {:color {:range :category10}}}) [:scales :color])]
      (is (= {:domain ["a" "b" "c"] :legend false :range :category10} sc)))
    (is (= true (get-in (plot/chord flows {:scales {:color {:legend true}}}) [:scales :color :legend])) "a legend on request"))
  (testing "4.2 :labels false; nodes without flow unlabelled"
    (is (= 2 (count (:charts (plot/chord flows {:labels false})))))
    (is (not-any? #(= "q" (:label %)) (mapcat #(get-in % [:options :data]) (drop 2 (:charts (plot/chord flows {:nodes [{:id "q"}]}))))))))

;; 5. arc

(deftest arc-layout-test
  (let [{:keys [nodes arcs]} (graph/arc-layout (graph/graph [{:from "a" :to "c" :value 2} {:from "c" :to "b"}]))]
    (testing "5.1 nodes on the line in node order"
      (is (= [["a" 0.0 0.0] ["c" 1.0 0.0] ["b" 2.0 0.0]] (mapv (juxt :id :x :y) nodes))))
    (testing "5.1 an arc from its source to its target, peak height = half the distance"
      (let [by-edge (group-by :edge arcs)
            ac (by-edge 0) cb (by-edge 1)]
        (is (= [0.0 1.0] (mapv #(m/approx (double %) 9) [(:x (first ac)) (:x (peek ac))])))
        (is (= [1.0 2.0] (mapv #(m/approx (double %) 9) [(:x (first cb)) (:x (peek cb))])))
        (is (< (m/abs (- 0.5 (double (reduce max (map :y ac))))) 1e-9))
        (is (every? #(>= (double (:y %)) -1e-12) arcs) "above the line")
        (is (= [2.0 1.0] (mapv (comp :value first) [ac cb])))))
    (testing "5.1 order"
      (is (= ["b" "a" "c"] (mapv :id (:nodes (graph/arc-layout (graph/graph [{:from "a" :to "c"} {:from "c" :to "b"}]) {:order ["b"]}))))))))

(deftest arc-plot-test
  (testing "5.2 arcs as lines with widths from :value, nodes, labels as ticks"
    (let [p (plot/arc flows {:mapping {:color :id}})
          [arcs nodes] (:charts p)]
      (is (= [:line :scatter] (mapv :kind [arcs nodes])))
      (is (= :value (get-in arcs [:options :mapping :size])))
      (is (= [0.5 4] (get-in p [:scales :size :range])))
      (is (= [0 1 2] (get-in p [:scales :x :ticks])))
      (is (= false (get-in p [:scales :y :axis])))
      (is (instance? BufferedImage (plot/render-image p {:width 400 :height 300})))))
  (testing "5.2 nodes mapping :size - no default edge widths; an aesthetic for both throws"
    (is (nil? (get-in (first (:charts (plot/arc flows {:mapping {:size :degree}}))) [:options :mapping :size])))
    (is (= :edge-mapping (:key (error #(plot/arc flows {:mapping {:size :degree} :edge-mapping {:size :value}}))))))
  (testing "5.2 equal weights - no width mapping, no size scale, no legend of one value"
    (let [p (plot/arc [{:from "a" :to "b"} {:from "b" :to "c"}] {})]
      (is (nil? (get-in (first (:charts p)) [:options :mapping :size])))
      (is (nil? (get-in p [:scales :size])))))
  (testing "5.2 user :scales merge key by key (arc, network, clustered heatmap)"
    (is (= false (get-in (plot/arc flows {:scales {:y {:title "t"}}}) [:scales :y :axis])))
    (is (= [0 1 2] (get-in (plot/arc flows {:scales {:x {:title "t"}}}) [:scales :x :ticks])))
    (is (= false (get-in (plot/network flows {:scales {:x {:title "t"}}}) [:scales :x :axis])))
    (let [rows (for [r ["r1" "r2" "r3"] c ["c1" "c2"]] {:r r :c c :v (rand)})
          tiles (first (get-in (plot/clustered-heatmap rows {:row :r :col :c :value :v :col-tree false :row-tree false
                                                             :scales {:x {:title "t"}}}) [:arrange :plots]))]
      (is (= 2 (count (get-in tiles [:scales :x :domain]))) "the leaf order kept")))
  (testing "5.2 no edges - a plot all the same"
    (is (instance? BufferedImage (plot/render-image (plot/arc [] {:nodes [{:id "a"} {:id "b"}]}) {:width 300 :height 200})))))
