(ns cljplot2.reference-r3-labels-test
  "Reference part R3 - tick labels without collisions: the shared rules (`measure/kept-label-indices`,
  `measure/kept-box-indices`), cartesian axes, polar guides, 3D frames, `:overlapping-labels` and explicit ticks."
  (:require [clojure.test :refer [deftest testing is]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.content.axis :as axis]
            [cljplot2.data.tree :as tree]
            [cljplot2.scale.position :as position]
            [cljplot2.graphics.measure :as measure]
            [fastmath.vector]
            [cljplot2.theme]
            [cljplot2.content.space3d]
            [cljplot2.content.panel]
            [cljplot2.test-util :refer [error-data]])
  (:import [java.awt.image BufferedImage]))

(def ^:private small-plot {:data [{:x 1 :y 1} {:x 2 :y 3}] :mapping {:x :x :y :y} :charts [(chart/scatter)]})

(deftest kept-label-indices-test
  (testing "the first label, then each not overlapping the last kept one"
    (is (= [0 2 4] (measure/kept-label-indices [0 5 10 15 20] [8 8 8 8 8])))
    (is (= [0 1 2] (measure/kept-label-indices [0 10 20] [8 8 8]))))
  (testing "boundary: no labels; one; touching or 1 px overlap kept, more dropped; reversed positions"
    (is (= [] (measure/kept-label-indices [] [])))
    (is (= [0] (measure/kept-label-indices [5] [100])))
    (is (= [0 1] (measure/kept-label-indices [0 10] [10 10])) "touching")
    (is (= [0 1] (measure/kept-label-indices [0 9] [10 10])) "1 px")
    (is (= [0] (measure/kept-label-indices [0 8.5] [10 10])) "1.5 px")
    (is (= [0 2] (measure/kept-label-indices [100 95 80] [8 8 8])) "pixels decreasing (a vertical axis)")))

(deftest axis-label-collisions-test
  (let [sizes (fn [n w] (vec (repeat n (fastmath.vector/vec2 w 10))))]
    (testing "continuous axes drop labels colliding with the last drawn one; bands axes keep every s-th label"
      (let [finalized (get-in (plot/build {:data [{:x 0 :y 0} {:x 4 :y 1}] :mapping {:x :x :y :y} :charts [(chart/scatter)]
                                           :scales {:x {:ticks [0 1 2 3 4] :expand [0 0] :overlapping-labels :drop}}}
                                          {:width 300 :height 200})
                              [:finalized-scales :x])
            n (count (:ticks finalized))
            step (/ (Math/abs (- (double (second (:range finalized))) (double (first (:range finalized))))) (dec n))]
        (is (= (vec (range n)) (axis/drawn-label-indices :x finalized (sizes n (* 0.5 step)) 2 nil nil)) "room for all")
        (is (= (vec (range 0 n 2)) (axis/drawn-label-indices :x finalized (sizes n (* 1.5 step)) 2 nil nil)) "every other fits")))
    (testing "a symlog axis crowded near 0: fewer labels drawn than ticks, none overlapping"
      (let [built (plot/build {:data (for [i (range -40 41)] {:x (* i i i) :y i}) :mapping {:x :x :y :y}
                               :charts [(chart/scatter)] :scales {:x {:kind :symlog :ticks 15}}}
                              {:width 300 :height 200})]
        (is (instance? BufferedImage (plot/render-image (dissoc built :subplots) {:width 300 :height 200})))))))

(defn- crowded-axis-drawn
  "`[tick-count drawn-indices]` of a crowded continuous x axis of explicit `:ticks` 0..40, position scale `spec` merged
  in."
  [spec]
  (let [finalized (get-in (plot/build {:data [{:x 0 :y 0} {:x 40 :y 1}] :mapping {:x :x :y :y} :charts [(chart/scatter)]
                                       :scales {:x (merge {:ticks (vec (range 0 41)) :expand [0 0]} spec)}}
                                      {:width 300 :height 200})
                              [:finalized-scales :x])
        sizes (vec (repeat (count (:ticks finalized)) (fastmath.vector/vec2 30 10)))]
    [(count sizes) (axis/drawn-label-indices :x finalized sizes 2 nil nil)]))

(deftest overlapping-labels-test
  (testing ":drop - colliding labels dropped; :keep - every label drawn (the default for explicit ticks: see
            explicit-ticks-keep-test)"
    (let [[n drawn] (crowded-axis-drawn {:overlapping-labels :drop})]
      (is (< (count drawn) n)))
    (let [[n drawn] (crowded-axis-drawn {:overlapping-labels :keep})]
      (is (= (vec (range n)) drawn))))
  (testing "the finalized scale carries :keep only; an :axis-domain display scale carries it too"
    (is (= :keep (:overlapping-labels (position/finalize {:kind :linear :domain [0 1] :overlapping-labels :keep} [0 100] 5))))
    (is (nil? (:overlapping-labels (position/finalize {:kind :linear :domain [0 1] :overlapping-labels :drop} [0 100] 5))))
    (is (= :keep (:overlapping-labels (position/display-scale
                                       (position/finalize {:kind :linear :domain [0 1] :axis-domain [0 10] :overlapping-labels :keep}
                                                          [0 100] 5))))))
  (testing "builders naming data items on an axis keep every name: arc, dendrogram leaves, alluvial axes"
    (is (= :keep (get-in (plot/arc [{:from "a" :to "b"}] {}) [:scales :x :overlapping-labels])))
    (is (= :keep (get-in (plot/dendrogram (tree/from-parents [{:id 1} {:id 2 :parent 1} {:id 3 :parent 1}] {}) {})
                         [:scales :x :overlapping-labels])))
    (is (= :keep (get-in (plot/alluvial [{:a "p" :b "q"} {:a "r" :b "q"}] {:variables [:a :b]}) [:scales :x :overlapping-labels]))))
  (testing "boundary: a builder's :scales override it; other values rejected"
    (is (= :drop (get-in (plot/arc [{:from "a" :to "b"}] {:scales {:x {:overlapping-labels :drop}}})
                         [:scales :x :overlapping-labels])))
    (is (= :overlapping-labels (:key (error-data #(plot/build (assoc small-plot :scales {:x {:overlapping-labels :all}}))))))))

(deftest kept-box-indices-test
  (let [box (fn [x y] {:x x :y y :w 10 :h 10})]
    (testing "a box is kept when it overlaps no kept box; 1 px overlap tolerated"
      (is (= [0 2] (measure/kept-box-indices [(box 0 0) (box 5 0) (box 20 0)])))
      (is (= [0 1] (measure/kept-box-indices [(box 0 0) (box 9 0)])) "1 px")
      (is (= [0 1] (measure/kept-box-indices [(box 0 0) (box 5 20)])) "apart across"))
    (testing "boundary: no boxes; one; the last checked against the first (a circle of labels)"
      (is (= [] (measure/kept-box-indices [])))
      (is (= [0] (measure/kept-box-indices [(box 0 0)])))
      (is (= [0 1] (measure/kept-box-indices [(box 0 0) (box 30 0) (box 3 2)]))))))

(deftest polar-label-collisions-test
  (let [placements #'cljplot2.content.panel/polar-label-placements
        polar-x (fn [spec] (get-in (plot/build {:data (for [i (range 60)] {:x i :y 1}) :mapping {:x :x :y :y}
                                                :charts [(chart/scatter)] :coord {:kind :polar}
                                                :scales {:x (merge {:ticks 30} spec)}}
                                               {:width 200 :height 200})
                                   [:finalized-scales :x]))
        ;; every label at one point: all collide
        same-place (fn [_] [50.0 50.0 [:center :middle]])
        count-placed (fn [finalized] (measure/call-with-scratch-canvas
                                      :high #(count (placements % finalized same-place {:theme cljplot2.theme/default-theme}))))]
    (testing "colliding polar labels: only the first kept"
      (is (= 1 (count-placed (polar-x {})))))
    (testing ":overlapping-labels :keep and explicit ticks keep every label"
      (let [kept (polar-x {:overlapping-labels :keep})]
        (is (= (count (:ticks kept)) (count-placed kept))))
      (let [explicit (polar-x {:ticks [0 10 20 30]})]
        (is (= (count (:ticks explicit)) (count-placed explicit))))
      (is (= 1 (count-placed (polar-x {:ticks [0 10 20 30] :overlapping-labels :drop})))))))

(deftest explicit-ticks-keep-test
  (testing "explicit :ticks keep their labels unless :drop is asked; computed ticks drop"
    (is (position/keep-labels? {:ticks [1 2]}))
    (is (not (position/keep-labels? {:ticks [1 2] :overlapping-labels :drop})))
    (is (position/keep-labels? {:ticks 5 :overlapping-labels :keep}))
    (is (not (position/keep-labels? {:ticks 5})))
    (is (not (position/keep-labels? {}))))
  (testing "a finalized scale of explicit ticks keeps; its crowded axis draws every label"
    (let [[n drawn] (crowded-axis-drawn {})]
      (is (= (vec (range n)) drawn) "the crowded helper gives explicit :ticks"))
    (let [[n drawn] (crowded-axis-drawn {:overlapping-labels :drop})]
      (is (< (count drawn) n)))))

(deftest frame-label-collisions-test
  (let [draw-axis! #'cljplot2.content.space3d/draw-axis!
        boxes (fn [length]
                (measure/call-with-scratch-canvas
                 :high
                 (fn [c]
                   (draw-axis! c :x {:axis :x :from [0 0 0] :to [1 0 0]}
                               {:corner (fn [[x _ _]] (fastmath.vector/vec2 (+ 10.0 (* (double length) (double x))) 100.0))
                                :centre (fastmath.vector/vec2 (+ 10.0 (* 0.5 (double length))) 0.0)}
                               {:domains {:x [0 1e9 ]} :levels {} :titles {}}
                               [] {:theme cljplot2.theme/default-theme :canvas c}))))
        overlap? measure/boxes-overlap?
        pairwise-free? (fn [bs] (every? true? (for [[i a] (map-indexed vector bs) [j b] (map-indexed vector bs) :when (< i j)]
                                                (not (overlap? a b)))))]
    (testing "a short edge with wide labels: the labels drawn don't overlap each other"
      (let [short (boxes 120)]
        (is (seq short))
        (is (pairwise-free? short))))
    (testing "a long edge keeps more of them, still without overlaps"
      (let [long-boxes (boxes 900)]
        (is (pairwise-free? long-boxes))
        (is (< (count (boxes 120)) (count long-boxes)))))))

(deftest explicit-bands-ticks-keep-test
  (testing "explicit :ticks on a bands scale keep every label (no every-s-th thinning) unless :drop"
    (let [levels-data (for [i (range 40)] {:k (str "level number " i) :v i})
          finalized (fn [spec] (get-in (plot/build {:data levels-data :mapping {:x :k :y :v} :charts [(chart/bar)] :scales {:x spec}}
                                                   {:width 300 :height 200})
                                       [:finalized-scales :x]))
          drawn (fn [f] (count (axis/drawn-label-indices :x f (vec (repeat (count (:ticks f)) (fastmath.vector/vec2 60 10))) 2 nil nil)))
          ticks (mapv #(str "level number " %) (range 40))]
      (is (= 40 (drawn (finalized {:ticks ticks}))))
      (is (< (drawn (finalized {:ticks ticks :overlapping-labels :drop})) 40))
      (is (< (drawn (finalized {})) 40) "computed (every band) ticks thin"))))
