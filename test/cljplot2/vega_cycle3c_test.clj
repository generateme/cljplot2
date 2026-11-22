(ns cljplot2.vega-cycle3c-test
  "Vega cycle 3c review: legend key glyphs clipped to whole pixels around their box (no cropped size keys), arc
  diagram `:labels`, the node-link tree (`data.tree/node-link-layout`, `tidy-breadths`, `plot/tree`)."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.color :as c]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.tree :as tree]
            [cljplot2.test-util :refer [error drawn-style]])
  (:import [java.awt.image BufferedImage]))

;; legend

(defn- red-box
  "`[w h]` of the bounding box of the exact red pixels of `img`."
  [^BufferedImage img]
  (let [red (unchecked-int 0xffff0000)
        pts (for [x (range (.getWidth img)) y (range (.getHeight img)) :when (== red (.getRGB img (int x) (int y)))] [x y])
        xs (map first pts) ys (map second pts)]
    [(inc (- (long (reduce max xs)) (long (reduce min xs)))) (inc (- (long (reduce max ys)) (long (reduce min ys))))]))

(deftest legend-size-key-test
  (testing "the largest size key's hollow circle is drawn whole (as tall as wide)"
    (doseq [size [14 20 23.5]]
      (let [img (plot/render-image {:data [{:x 5 :y 5 :g "a"}] :mapping {:x :x :y :y :size :g}
                                    :charts [(chart/scatter {:shape \o :color :red :stroke-width 1.5})]
                                    :scales {:x {:domain [0 1]} :y {:domain [0 1]} :size {:range [size size]}}}
                                   {:width 300 :height 150})
            [w h] (red-box img)]
        (is (<= (Math/abs (- (long w) (long h))) 1) [size w h])))))

;; arc labels

(def ^:private nodes [{:index 0 :name "Alice"} {:index 1 :name "Bob"} {:index 2 :name nil}])
(def ^:private links [{:source 0 :target 1} {:source 1 :target 2}])

(defn- arc-labels
  [options]
  (let [p (plot/arc links (merge {:from :source :to :target :nodes nodes :id :index} options))
        f (get-in p [:scales :x :formatter])]
    (mapv f [0 1 2])))

(deftest arc-labels-test
  (testing "default - the node ids; an accessor - its values (nil - empty)"
    (is (= ["0" "1" "2"] (arc-labels {})))
    (is (= ["Alice" "Bob" ""] (arc-labels {:labels :name})))
    (is (= ["ALICE" "BOB" ""] (arc-labels {:labels #(some-> (:name %) clojure.string/upper-case)}))))
  (testing ":labels? false - no axis; the theme turns the labels"
    (is (false? (get-in (plot/arc links {:from :source :to :target :nodes nodes :id :index :labels? false}) [:scales :x :axis])))
    (is (= -90 (get-in (plot/arc links {:from :source :to :target :nodes nodes :id :index
                                       :theme {:axis {:x {:label-rotation -90}}}})
                       [:theme :axis :x :label-rotation])))))

;; node-link tree

(defn- parents->tree [rows] (tree/from-parents rows {}))

(def ^:private small (parents->tree [{:id 1} {:id 2 :parent 1} {:id 3 :parent 1} {:id 4 :parent 2} {:id 5 :parent 2}
                                     {:id 6 :parent 2} {:id 7 :parent 3}]))

(def ^:private lopsided
  "A tree whose deep left subtree and wide right one must not overlap at depth 3."
  (parents->tree (concat [{:id 1} {:id 2 :parent 1} {:id 3 :parent 1} {:id 4 :parent 2} {:id 5 :parent 4}
                          {:id 6 :parent 5} {:id 7 :parent 5} {:id 8 :parent 3} {:id 9 :parent 8}]
                         (map (fn [i] {:id i :parent 9}) (range 10 16)))))

(defn- min-gap
  "The smallest breadth gap between nodes of one depth (nil - no depth has two nodes)."
  [nodes]
  (let [gaps (for [[_ ns] (group-by :depth nodes) :let [bs (sort (map :breadth ns))] [a b] (partition 2 1 bs)] (- (double b) (double a)))]
    (when (seq gaps) (reduce min gaps))))

(defn- centred?
  "Is every group at the middle of its first and last child?"
  [nodes]
  (every? (fn [[p ks]] (or (nil? p) (< (Math/abs (- (double (:breadth (nth nodes p)))
                                                 (* 0.5 (+ (double (:breadth (first ks))) (double (:breadth (last ks)))))))
                                     1e-9)))
          (group-by :parent nodes)))

(deftest tidy-layout-test
  (testing "tidy: nodes of a depth at least 1 apart, groups centred over their children, depth by level"
    (doseq [t [small lopsided]
            :let [{:keys [nodes]} (tree/node-link-layout t {})]]
      (is (>= (double (min-gap nodes)) 1.0))
      (is (centred? nodes))
      (is (every? #(== (double (:x %)) (double (:depth %))) nodes))
      (is (== 0.0 (double (reduce min (map :breadth nodes)))) "breadths from 0")))
  (testing "boundary: one node, a chain"
    (is (= [[0.0 0.0]] (mapv (juxt :x :y) (:nodes (tree/node-link-layout (parents->tree [{:id 1}]) {})))))
    (is (empty? (:links (tree/node-link-layout (parents->tree [{:id 1}]) {}))))
    (let [{:keys [nodes]} (tree/node-link-layout (parents->tree [{:id 1} {:id 2 :parent 1} {:id 3 :parent 2}]) {})]
      (is (= [0.0 0.0 0.0] (mapv :breadth nodes)))
      (is (= [0 1 2] (mapv :depth nodes)))))
  (testing "cluster: leaves at the deepest depth, one step apart"
    (let [{:keys [nodes]} (tree/node-link-layout lopsided {:layout :cluster})
          leaves (filter :leaf? nodes)]
      (is (apply = (map :x leaves)))
      (is (= (map double (range (count leaves))) (sort (map :breadth leaves))))))
  (testing "links: a path per non-root node from its parent; :line - two points, :diagonal - 17"
    (let [{:keys [nodes links]} (tree/node-link-layout small {:links :line})]
      (is (= (dec (count nodes)) (count (distinct (map :id links)))))
      (is (every? #(= 2 (count %)) (vals (group-by :id links))))
      (is (= [(:x (nth nodes (:parent (nth nodes 1)))) (:y (nth nodes (:parent (nth nodes 1))))]
             ((juxt :x :y) (first (filter #(= 1 (:id %)) links))))))
    (is (every? #(= 17 (count %)) (vals (group-by :id (:links (tree/node-link-layout small {})))))))
  (testing "circular: inside the unit circle, the root at the centre, the deepest nodes on it"
    (let [{:keys [nodes links]} (tree/node-link-layout lopsided {:circular true})]
      (is (every? #(<= (Math/hypot (double (:x %)) (double (:y %))) (+ 1.0 1e-9)) (concat nodes links)))
      (is (= [0.0 0.0] ((juxt :x :y) (first nodes))))
      (is (== 1.0 (double (reduce max (map :radius nodes))))))))

(deftest tree-builder-test
  (testing "renders: tidy / cluster, diagonal / line, cartesian / radial, a single node"
    (doseq [opts [{} {:layout :cluster} {:links :line} {:circular true} {:circular true :layout :cluster}
                  {:labels? false :nodes? false} {:mapping {:color #(str (:depth %))}}]]
      (is (some? (plot/render-image (plot/tree small opts) {:width 400 :height 300})) opts))
    (is (some? (plot/render-image (plot/tree (parents->tree [{:id 1 :name "root"}])) {:width 200 :height 150}))))
  (testing "labels: leaves right of their node, groups left; unnamed nodes none"
    (let [texts (filter #(= :text (:kind %)) (:charts (plot/tree small {})))]
      (is (= [[:left :middle] [:right :middle]] (mapv #(get-in % [:options :anchor]) texts))))
    (is (empty? (filter #(= :text (:kind %))
                        (:charts (plot/tree (tree/from-levels [{:a "x" :b "y"}] {:hierarchy [:a :b]}) {:labels? false}))))))
  (testing "bad :layout / :links"
    (is (= :layout (:key (error #(plot/tree small {:layout :radial})))))
    (is (= :links (:key (error #(plot/tree small {:links :curve})))))))

;; review 2: anchored label room, beeswarm packed around

(defn- text-room
  [plot-extra text-options]
  (get-in (plot/build (merge {:data [{:x 1 :y 1 :l "a long label"}] :mapping {:x :x :y :y}
                              :charts [(chart/text (merge {:mapping {:x :x :y :y :label :l}} text-options))]}
                             plot-extra)
                      {:width 400 :height 300})
          [:stats 0 :padding-px]))

(deftest anchored-room-test
  (testing "[:left :middle] + offset 6 - the width plus 6 on the high x side only, half the height up and down"
    (let [{[xl xh] :x [yl yh] :y} (text-room {} {:anchor [:left :middle] :offset [6 0] :reserve-room :anchor})]
      (is (zero? (double xl)))
      (is (> (double xh) 40.0))
      (is (== (double yl) (double yh)))
      (is (< (double yh) 15.0))))
  (testing "[:right :middle] - mirrored; [:center :bottom] - half the width each side, the height above"
    (let [{[xl xh] :x} (text-room {} {:anchor [:right :middle] :offset [-6 0] :reserve-room :anchor})]
      (is (zero? (double xh)))
      (is (> (double xl) 40.0)))
    (let [{[xl xh] :x [yl yh] :y} (text-room {} {:anchor [:center :bottom] :reserve-room :anchor})]
      (is (< (Math/abs (- (double xl) (double xh))) 1e-9))
      (is (zero? (double yl)))
      (is (pos? (double yh)))))
  (testing "flipped plots exchange the axes"
    (let [{[xl xh] :x [yl yh] :y} (text-room {:flip true} {:anchor [:left :middle] :reserve-room :anchor})]
      (is (zero? (double yl)))
      (is (> (double yh) 40.0))
      (is (== (double xl) (double xh)))))
  (testing "a rotation, or true - the diagonal on every side"
    (let [rooms [(text-room {} {:anchor [:left :middle] :rotation 30 :reserve-room :anchor})
                 (text-room {} {:anchor [:left :middle] :reserve-room true})]]
      (doseq [{[xl xh] :x [yl yh] :y} rooms]
        (is (apply == (map double [xl xh yl yh]))))
      (is (= (first rooms) (second rooms)))))
  (testing "an unknown value is a schema error"
    (is (some? (error #(text-room {} {:reserve-room :left}))))))

(deftest tree-defaults-test
  (let [p (plot/tree small {})]
    (testing "mid-grey links; cartesian labels keep anchored room"
      (is (= (c/to-color [176 176 176]) (c/to-color (:color (drawn-style (get-in p [:charts 0]))))))
      (is (= 0.9 (:width (drawn-style (get-in p [:charts 0])))))
      (is (= [120 0 0] (get-in (plot/tree small {:link-style {:color [120 0 0]}}) [:charts 0 :options :color]))
          ":link-style wins")
      (is (every? #(= :anchor (get-in % [:options :reserve-room])) (filter #(= :text (:kind %)) (:charts p)))))
    (testing "radial labels keep room on every side"
      (is (every? #(true? (get-in % [:options :reserve-room]))
                  (filter #(= :text (:kind %)) (:charts (plot/tree small {:circular true}))))))))

(defn- overlaps?
  "Do any two of the circles (offsets `[along cross]` added to `positions`) come closer than `d`?"
  [positions offsets d]
  (let [pts (mapv (fn [p [u c]] [(+ (double p) (double u)) (double c)]) positions offsets)]
    (some true? (for [i (range (count pts)) j (range (inc i) (count pts))
                      :let [[ax ay] (pts i) [bx by] (pts j)]]
                  (< (Math/hypot (- (double ax) (double bx)) (- (double ay) (double by))) (- (double d) 1e-6))))))

(deftest around-offsets-test
  (testing "no overlaps; equal values form a compact cluster around their position; deterministic"
    (let [ps (repeat 19 100.0)
          offs (cljplot2.chart.beeswarm/around-offsets ps 10.0 false)]
      (is (not (overlaps? ps offs 10.0)))
      (is (< (double (reduce max (map (fn [[u c]] (Math/hypot u c)) offs))) 30.0) "19 circles within 3 diameters")
      (is (= offs (cljplot2.chart.beeswarm/around-offsets ps 10.0 false)))))
  (testing "spread values: no overlaps, the first point at its position"
    (let [ps (map #(* 3.0 (double %)) (range 40))
          offs (cljplot2.chart.beeswarm/around-offsets ps 8.0 false)]
      (is (not (overlaps? ps offs 8.0)))
      (is (= [0.0 0.0] (first offs)))))
  (testing "boundary: one-sided - cross offsets never negative; zero diameter - zero offsets; no points"
    (is (every? #(>= (double (second %)) 0.0) (cljplot2.chart.beeswarm/around-offsets (repeat 10 0.0) 5.0 true)))
    (is (= [[0.0 0.0] [0.0 0.0]] (cljplot2.chart.beeswarm/around-offsets [1.0 1.0] 0.0 false)))
    (is (= [] (cljplot2.chart.beeswarm/around-offsets [] 5.0 false)))))

(deftest beeswarm-pack-test
  (testing ":pack :around renders (plain, flipped, mirrored); :across is the default; an unknown :pack is an error"
    (let [rows (map (fn [i] {:v (mod (long i) 3) :g (if (even? (long i)) "a" "b")}) (range 30))]
      (doseq [extra [{} {:flip true}]]
        (is (some? (plot/render-image (merge {:data rows :mapping {:x :v} :charts [(chart/beeswarm {:pack :around :size 10})]} extra)
                                      {:width 300 :height 200}))))
      (is (some? (plot/render-image {:data rows :mapping {:x :v :color :g}
                                     :charts [(chart/beeswarm {:pack :around :position :mirror :size 8})]}
                                    {:width 300 :height 200})))
      (is (= (seq (.getRGB ^BufferedImage (plot/render-image {:data rows :mapping {:x :v} :charts [(chart/beeswarm)]} {:width 200 :height 120})
                           0 0 200 120 nil 0 200))
             (seq (.getRGB ^BufferedImage (plot/render-image {:data rows :mapping {:x :v} :charts [(chart/beeswarm {:pack :across})]} {:width 200 :height 120})
                           0 0 200 120 nil 0 200)))))
    (is (some? (error #(plot/build {:data [{:v 1}] :mapping {:x :v} :charts [(chart/beeswarm {:pack :force})]}))))))
