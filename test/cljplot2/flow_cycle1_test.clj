(ns cljplot2.flow-cycle1-test
  "Flow cycle 1: graph data, layouts, arrows and segment widths, text angles, `plot/network`."
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure2d.color :as color]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [cljplot2.test-util :refer [error]]
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

(defn- image [p size] (plot/render-image (update p :theme #(merge plain %)) size))

(defn- stats [p] (first (:stats (plot/build p {:width 400 :height 400}))))

(def ^:private red [255 0 0])

;; 1. graph data

(deftest graph-test
  (testing "1.1 nodes: node rows first, then ids seen in edges; degrees"
    (let [g (graph/graph [{:from "a" :to "b"} {:from "b" :to "c"} {:from "a" :to "c"}]
                         {:nodes [{:id "c" :kind :x} {:id "z"}]})]
      (is (= ["c" "z" "a" "b"] (mapv :id (:nodes g))))
      (is (= {"c" 2 "z" 0 "a" 2 "b" 2} (zipmap (map :id (:nodes g)) (map :degree (:nodes g)))))
      (is (= :x (:kind (first (:nodes g)))) "node row columns kept")
      (is (every? #(== 1.0 (double (:value %))) (:edges g)) "default weight 1")))
  (testing "1.1 directed in / out; undirected in = out = degree"
    (let [d (graph/graph [{:from "a" :to "b"} {:from "a" :to "c"}] {:directed? true})
          u (graph/graph [{:from "a" :to "b"} {:from "a" :to "c"}])]
      (is (= [[2 0 2] [1 1 0] [1 1 0]] (mapv (juxt :degree :in :out) (:nodes d))))
      (is (= [[2 2 2] [1 1 1] [1 1 1]] (mapv (juxt :degree :in :out) (:nodes u))))))
  (testing "1.1 accessors, empty edges"
    (is (= ["x" "y"] (mapv :id (:nodes (graph/graph [{:s "x" :t "y" :w 2}] {:from :s :to :t :value :w})))))
    (is (= 2.0 (:value (first (:edges (graph/graph [{:s "x" :t "y" :w 2}] {:from :s :to :t :value :w}))))))
    (is (= {:nodes [] :edges [] :loops [] :directed? false :dropped 0} (graph/graph []))))
  (testing "1.2 self-loops counted in degree, kept apart; nil ends dropped; duplicates kept"
    (let [g (graph/graph [{:from "a" :to "a"} {:from "a" :to "b"} {:from "a" :to "b"} {:from nil :to "b"} {:from "b"}])]
      (is (= 1 (count (:loops g))))
      (is (= 2 (count (:edges g))) "duplicate pair kept")
      (is (= 4 (:degree (first (:nodes g)))) "loop twice, two edges")
      (is (= 2 (:dropped g)))))
  (testing "1.2 invalid weight, edges that aren't rows, repeated / nil node ids"
    (are [v] (= :value (:key (error #(graph/graph [{:from 1 :to 2 :value v}]))))
      ##NaN -1 "3" ##Inf)
    (is (= :edges (:key (error #(graph/graph [[1 2]])))))
    (is (= :edges (:key (error #(graph/graph {:from 1 :to 2})))))
    (is (= :nodes (:key (error #(graph/graph [] {:nodes [{:id 1} {:id 1}]})))))
    (is (= :nodes (:key (error #(graph/graph [] {:nodes [{:name 1}]}))))))
  (testing "1.3 adjacency matrix: directed, upper triangle, zero / nil / NaN cells"
    (let [m [[0 1 2] [3 0 nil] [##NaN 4 5]]]
      (is (= #{["a" "b" 1] ["a" "c" 2] ["b" "a" 3] ["c" "b" 4] ["c" "c" 5]}
             (set (map (juxt :from :to :value) (graph/adjacency->edges m ["a" "b" "c"])))))
      (is (= #{["a" "b" 1] ["a" "c" 2] ["c" "c" 5]}
             (set (map (juxt :from :to :value) (graph/adjacency->edges m ["a" "b" "c"] {:directed? false}))))))
    (is (= :matrix (:key (error #(graph/adjacency->edges [[0 1] [1 0]] ["a" "b" "c"])))))
    (is (= :matrix (:key (error #(graph/adjacency->edges [[0 1] [1]] ["a" "b"])))))
    (is (= [] (graph/adjacency->edges [] [])))))

;; 2. layouts

(defn- positions [g opts] (mapv (juxt :x :y) (:nodes (graph/layout g opts))))

(defn- dist [[x0 y0] [x1 y1]] (m/hypot (- (double x1) (double x0)) (- (double y1) (double y0))))

(deftest layout-test
  (let [path (graph/graph [{:from "a" :to "b"} {:from "b" :to "c"}])]
    (testing "2.1 positions on nodes and edge ends, in the unit square"
      (let [l (graph/layout path {:rng (r/rng :mersenne 1)})]
        (is (every? (fn [{:keys [x y]}] (and (<= 0.0 (double x) 1.0) (<= 0.0 (double y) 1.0))) (:nodes l)))
        (is (= (vec (mapcat (juxt :x :y) (take 2 (:nodes l)))) ((juxt :x :y :xend :yend) (first (:edges l)))) "first edge a->b")))
    (testing "2.2 force: one :rng - the same layout; a path straight with equal edges"
      (is (= (positions path {:rng (r/rng :mersenne 1)}) (positions path {:rng (r/rng :mersenne 1)})))
      (let [[a b c] (positions path {:rng (r/rng :mersenne 1)})]
        (is (< (m/abs (- (dist a c) (+ (dist a b) (dist b c)))) 0.02) "nearly collinear")
        (is (< (m/abs (- 1.0 (/ (dist a b) (dist b c)))) 0.1) "edges equal within 10 %")))
    (testing "2.2 two components apart"
      (let [g (graph/graph [{:from 1 :to 2} {:from 2 :to 3} {:from 3 :to 1} {:from 4 :to 5} {:from 5 :to 6} {:from 6 :to 4}])
            ps (positions g {:rng (r/rng :mersenne 2)})
            within (max (dist (ps 0) (ps 1)) (dist (ps 3) (ps 4)))
            between (apply min (for [i (range 3) j (range 3 6)] (dist (ps i) (ps j))))]
        (is (> between within) "nodes of different components further apart than an edge")))
    (testing "2.3 circle: from the top, clockwise, equal radii; random from :rng"
      (let [ps (positions (graph/graph [{:from 1 :to 2} {:from 3 :to 4}]) {:layout :circle})]
        (is (every? #(< (m/abs (- 0.5 (dist [0.5 0.5] %))) 1.0e-9) ps))
        (is (= [0.5 1.0] (mapv #(m/approx % 9) (ps 0))))
        (is (< 0.9 (double (first (ps 1)))) "second node on the right"))
      (is (= (positions path {:layout :random :rng (r/rng :mersenne 4)}) (positions path {:layout :random :rng (r/rng :mersenne 4)}))))
    (testing "2.4 0, 1, 2 nodes"
      (doseq [l [:force :circle :random]]
        (is (= [] (positions (graph/graph []) {:layout l :rng (r/rng :mersenne 1)})))
        (is (= 1 (count (positions (graph/graph [{:from 1 :to 1}]) {:layout l :rng (r/rng :mersenne 1)})))))
      (is (= [[0.5 0.5]] (positions (graph/graph [{:from 1 :to 1}]) {})) "one node in the centre")
      (let [[a b] (positions (graph/graph [{:from 1 :to 2}]) {:rng (r/rng :mersenne 1)})]
        (is (< (dist [0.5 0.5] [(* 0.5 (+ (double (a 0)) (double (b 0)))) (* 0.5 (+ (double (a 1)) (double (b 1))))]) 1.0e-9)
            "two nodes symmetric about the centre")))
    (testing "2.1 unknown layout; R4 :gravity a finite number >= 0"
      (is (= :layout (:key (error #(graph/layout path {:layout :spring})))))
      (are [g] (= :gravity (:key (error #(graph/layout path {:gravity g}))))
        -1 ##NaN "1" nil))
    (testing "R4 :gravity 0 lets components drift apart, more pulls them together"
      (let [g (graph/graph [{:from 1 :to 2} {:from 3 :to 4}])
            gap (fn [gravity] (let [ps (positions g {:rng (r/rng :mersenne 7) :gravity gravity})]
                                (/ (dist (ps 0) (ps 1)) (min (dist (ps 0) (ps 2)) (dist (ps 1) (ps 3))))))]
        (is (< (gap 0) (gap 5)) "edge length relative to the distance between the parts grows with gravity")))
    (testing "2.5 118 nodes within 2 s"
      (let [g (graph/graph (for [i (range 118) j (range 3)] {:from i :to (mod (+ i 1 (* 7 j)) 118)}))
            t (System/nanoTime)]
        (graph/layout g {:rng (r/rng :mersenne 2)})
        (is (< (/ (- (System/nanoTime) t) 1e9) 2.0))))))

;; 3. marks

(defn- segment-image [opts]
  (image {:data [{:x 0 :y 0 :xe 1 :ye 0}] :mapping {:x :x :y :y :xend :xe :yend :ye}
          :scales {:x {:domain [0 1] :expand [0 0]} :y {:domain [-1 1]}}
          :charts [(chart/segment (merge {:color red :width 2} opts))]}
         {:width 200 :height 100}))

(deftest arrow-test
  (testing "3.1 a closed head at the end: wider than the line there"
    (let [plain-px (painted (segment-image {}) red)
          arrow-px (painted (segment-image {:arrow {:size 10}}) red)
          near-end (fn [pts] (let [mx (long (apply max (map first pts)))] (filter #(> (long (first %)) (- mx 12)) pts)))]
      (is (<= (span (near-end plain-px) second) 3))
      (is (>= (span (near-end arrow-px) second) 8) "the head is about 10 px wide")))
  (testing "3.1 :end-gap moves the tip back; :start too; :open heads"
    (let [max-x (fn [opts] (apply max (map first (painted (segment-image opts) red))))
          min-x (fn [opts] (apply min (map first (painted (segment-image opts) red))))]
      (is (< (max-x {:arrow {} :end-gap 30}) (- (max-x {:arrow {}}) 25)))
      (is (> (min-x {:start-gap 30}) (+ (min-x {}) 25)))
      (let [pts (painted (segment-image {:arrow {:start true :end false}}) red)
            mn (long (apply min (map first pts)))]
        (is (>= (span (filter #(< (long (first %)) (+ mn 12)) pts) second) 6)))
      (let [pts (painted (segment-image {:arrow {:shape :open :size 10}}) red)
            mx (long (apply max (map first pts)))]
        (is (>= (span (filter #(> (long (first %)) (- mx 12)) pts) second) 6)))))
  (testing "3.1 a run no longer than its gaps isn't drawn"
    (is (empty? (painted (segment-image {:start-gap 120 :end-gap 120}) red))))
  (testing "3.1 schema"
    (are [o] (= :arrow (nth (:in (error #(stats {:data [{:x 0 :y 0}] :mapping {:x :x :y :y} :charts [(chart/segment o)]}))) 3))
      {:arrow {:size 0}} {:arrow {:shape :round}} {:arrow true}))
  (testing "3.1 line arrows; a filled or stacked line rejects them"
    (let [line (fn [opts] (image {:data [{:x 0 :y 0} {:x 1 :y 0}] :mapping {:x :x :y :y}
                                   :scales {:x {:domain [0 1] :expand [0 0]} :y {:domain [-1 1]}}
                                   :charts [(chart/line (merge {:color red :width 2} opts))]}
                                  {:width 200 :height 100}))]
      (let [pts (painted (line {:arrow {:size 10}}) red) mx (long (apply max (map first pts)))]
        (is (>= (span (filter #(> (long (first %)) (- mx 12)) pts) second) 8)))
      (is (= :arrow (:key (error #(line {:arrow {} :fill :color})))))
      (is (= :arrow (:key (error #(image {:data [{:x 0 :y 0 :g "a"} {:x 1 :y 1 :g "a"}] :mapping {:x :x :y :y :color :g}
                                          :charts [(chart/line {:position :stack :arrow {}})]} {:width 200 :height 100}))))))))

(deftest segment-width-test
  (testing "3.2 per-row width from a linear :size"
    (let [img (image {:data [{:x 0 :y 0 :v 1} {:x 0 :y 1 :v 5}] :mapping {:x :x :y :y :xend (constantly 1) :yend :y :size :v}
                      :scales {:x {:domain [0 1]} :y {:domain [-1 2]} :size {:range [1 9]}}
                      :charts [(chart/segment {:color red})] :legend false}
                     {:width 200 :height 300})
          pts (painted img red)
          thin (filter #(> (long (second %)) 150) pts)
          thick (filter #(< (long (second %)) 150) pts)]
      (is (<= (span thin second) 2))
      (is (>= (span thick second) 7))))
  (testing "3.2 NA width - the style width; the legend draws lines of the widths"
    (let [ch (chart/segment {:color red :width 3})
          key-width (fn [attributes] (let [canvas (c2d/canvas 40 40)]
                                       (c2d/with-canvas-> canvas (c2d/set-background :white))
                                       (c2d/with-canvas [c canvas] ((:legend ch) ch attributes c {:x 0 :y 0 :w 40 :h 40} {:theme theme/default-theme}))
                                       (span (painted (c2d/get-image canvas) red) second)))]
      (is (< (key-width {:size 1}) (key-width {:size 9})))
      (is (<= 2 (key-width {}) 4) "no size - the style width")
      (is (some? (image {:data [{:x 0 :y 0 :v nil} {:x 0 :y 1 :v 5} {:x 0 :y 2 :v 2}] :mapping {:x :x :y :y :xend (constantly 1) :yend :y :size :v}
                         :charts [(chart/segment)]} {:width 200 :height 200})) "NA rows drawn"))))

(deftest text-angle-test
  (testing "3.3 a mapped :angle turns each label; :rotation otherwise"
    (let [img (fn [angle-fn] (image {:data [{:x 0 :y 0 :t "MMMMMM" :a angle-fn}]
                                     :scales {:x {:domain [-1 1]} :y {:domain [-1 1]}}
                                     :charts [(chart/text {:color red :size 20
                                                           :mapping (cond-> {:x :x :y :y :label :t} angle-fn (assoc :angle :a))})]}
                                    {:width 300 :height 300}))
          shape (fn [i] (let [pts (painted i red)] [(span pts first) (span pts second)]))
          [w0 h0] (shape (img 0))
          [w90 h90] (shape (img 90))]
      (is (> w0 h0) "horizontal")
      (is (> h90 w90) "vertical at 90")
      (is (= (shape (img nil)) [w0 h0]) "no angle - :rotation 0")
      (is (= (shape (img ##NaN)) [w0 h0]) "not finite - :rotation"))))

;; 4. plot/network

(def ^:private star [{:from "hub" :to "a"} {:from "hub" :to "b"} {:from "hub" :to "c"} {:from "a" :to "b"}])

(deftest network-test
  (testing "4.1 builds and renders; edge ends on node centres"
    (let [p (plot/network star {:rng (r/rng :mersenne 1) :labels true})
          [edges nodes] (map #(get-in % [:options :data]) (:charts p))
          at (zipmap (map :id nodes) (map (juxt :x :y) nodes))]
      (is (instance? BufferedImage (plot/render-image p {:width 300 :height 300})))
      (is (every? (fn [e] (and (= (at (:from e)) [(:x e) (:y e)]) (= (at (:to e)) [(:xend e) (:yend e)]))) edges))
      (is (= :iso (:aspect p)))
      (is (= 3 (count (:charts p))) "edges, nodes, labels")
      (is (= 2 (count (:charts (plot/network star {:rng (r/rng :mersenne 1)})))) "no labels")))
  (testing "4.2 directed: arrows, end gaps of the target's radius (default and mapped sizes)"
    (let [gaps (fn [opts] (let [p (plot/network star (merge {:directed? true :rng (r/rng :mersenne 1)} opts))]
                            (zipmap (map :to (get-in (first (:charts p)) [:options :data]))
                                    (map :cljplot2.graphs/gap (get-in (first (:charts p)) [:options :data])))))
          default (gaps {})
          sized (gaps {:mapping {:size :in} :scales {:size {:range [4 20]}}})]
      (is (= 1 (count (set (vals default)))) "one radius")
      (is (== 7.0 (double (default "a"))) "builder node size 12: radius 6 + 1")
      (is (> (double (sized "b")) (double (sized "a"))) "b has two in-edges - larger")
      (is (some? (get-in (plot/network star {:directed? true}) [:charts 0 :options :arrow])))
      (is (instance? BufferedImage (plot/render-image (plot/network star {:directed? true :mapping {:size :in}}) {:width 300 :height 300})))))
  (testing "4.3 size by degree"
    (let [p (plot/network star {:rng (r/rng :mersenne 1) :mapping {:size :degree}})]
      (is (= {:x :x :y :y :size :degree} (get-in p [:charts 1 :options :mapping])))
      (is (instance? BufferedImage (plot/render-image p {:width 300 :height 300})))))
  (testing "R1 node and edge styles: builder defaults, options win, opaque fill"
    (let [p (plot/network star {:rng (r/rng :mersenne 1)})
          nodes (get-in p [:charts 1 :options])
          edges (get-in p [:charts 0 :options])]
      (is (= [12 :white 1] ((juxt :size :stroke-color :stroke-width) nodes)))
      (is (== 255.0 (double (color/alpha (:color nodes)))) "opaque")
      (is (= [120 120 120 200] (:color edges)))
      (is (= 20 (get-in (plot/network star {:node-style {:size 20}}) [:charts 1 :options :size])))
      (is (= :red (get-in (plot/network star {:edge-style {:color :red}}) [:charts 0 :options :color])))
      (is (nil? (get-in (plot/network star {:edge-mapping {:color :value}}) [:charts 0 :options :color])) "mapped colour: no fixed one")))
  (testing "R1 a mapped node size: range [6 24] in area unless the scales set them"
    (is (= {:range [6 24] :area true} (get-in (plot/network star {:mapping {:size :degree}}) [:scales :size])))
    (is (= {:range [2 8] :area true} (get-in (plot/network star {:mapping {:size :degree} :scales {:size {:range [2 8]}}}) [:scales :size])))
    (is (nil? (get-in (plot/network star {}) [:scales :size])) "no size mapping - no size scale"))
  (testing "R2 label offset: the largest node radius + 3"
    (is (= [9.0 0] (get-in (plot/network star {:labels true}) [:charts 2 :options :offset])))
    (is (= [15.0 0] (get-in (plot/network star {:labels true :mapping {:size :degree}}) [:charts 2 :options :offset])) "range top 24"))
  (testing "R3 arrowhead: at least 7, 3 x the widest edge; :edge-style :arrow wins"
    (let [arrow (fn [opts] (get-in (plot/network star (merge {:directed? true} opts)) [:charts 0 :options :arrow]))]
      (is (= {:size 7.0} (arrow {})) "1 px edges")
      (is (= {:size 30.0} (arrow {:edge-mapping {:size :value}})) "palette size range top 10")
      (is (= {:size 12.0} (arrow {:edge-mapping {:size :value} :scales {:size {:range [1 4]}}})))
      (is (= {:size 9.0} (arrow {:edge-style {:width 3}})))
      (is (= {:size 5 :shape :open} (arrow {:edge-style {:arrow {:size 5 :shape :open}}})))))
  (testing "4.4 validation"
    (is (= :layout (:key (error #(plot/network star {:layout :tree})))))
    (is (= :edges (:key (error #(plot/network [[1 2]] {})))))
    (is (= :edge-mapping (:key (error #(plot/network star {:mapping {:color :degree} :edge-mapping {:color :value}})))))
    (is (= :edge-mapping (:key (error #(plot/network star {:mapping {:size :degree} :edge-mapping {:size :value}}))))))
  (testing "4.1 empty edges, one edge, labels from an accessor, circle layout"
    (is (instance? BufferedImage (plot/render-image (plot/network [{:from 1 :to 2}] {:labels #(str "n" (:id %))}) {:width 200 :height 200})))
    (is (instance? BufferedImage (plot/render-image (plot/network star {:layout :circle :edge-mapping {:size :value}}) {:width 200 :height 200})))))
