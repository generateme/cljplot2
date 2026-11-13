(ns cljplot2.strips-3b-test
  "Cycle 3b: beeswarm, histogram and density ridgelines, the `:bottom` strip placement."
  (:require [clojure.test :refer [deftest testing is]]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [cljplot2.test-util :refer [error]]
            [cljplot2.chart :as chart]
            [cljplot2.chart.beeswarm :as beeswarm]
            [cljplot2.core :as plot]
            [cljplot2.data.facet :as facet]
            [cljplot2.data.strip :as strip]
            [cljplot2.layout.grid :as grid]
            [cljplot2.scale.position :as scale])
  (:import [fastmath.vector Vec2]
           [java.awt.image BufferedImage]))

(def ^:private rows
  (vec (for [i (range 90)] {:v (+ (mod (* i 37) 23) (* 4 (mod i 3))) :lvl (["a" "b" "c"] (mod i 3)) :g (["p" "q"] (mod i 2))})))

(def ^:private options {:width 500 :height 400})

(defn- built [p] (plot/build p options))
(defn- panel-data [b i] (get-in b [:stats i :data facet/single-panel]))
(defn- panel-box [b] (grid/find-content-box (:grid b) (:layout b) #(= :panel (:kind %))))

(defn- placement-of [b chart-options place]
  (let [{:keys [w h]} (panel-box b)]
    (strip/placement {:w w :h h} (:finalized-scales b) chart-options place)))

(deftest swarm-offsets-test
  (let [rng (r/rng :mersenne 7)
        ps (sort (repeatedly 200 #(* 100.0 (r/drandom rng))))
        d 6.0]
    (testing "no two circles closer than a diameter"
      (doseq [one-sided? [false true]
              :let [os (beeswarm/swarm-offsets ps d one-sided?)
                    pts (map vector ps os)]]
        (is (= (count ps) (count os)))
        (is (every? (fn [[[p1 o1] [p2 o2]]] (>= (m/hypot (- (double p1) (double p2)) (- (double o1) (double o2))) (- d 1.0e-6)))
                    (for [[i a] (map-indexed vector pts) [j b] (map-indexed vector pts) :when (< (long i) (long j))] [a b])))
        (when one-sided? (is (every? #(>= (double %) 0.0) os)))))
    (testing "isolated values stay on the centre line; equal values alternate outward"
      (is (= [0.0 0.0] (beeswarm/swarm-offsets [0.0 100.0] d false)))
      (is (= [0.0 6.0 -6.0 12.0] (mapv #(m/approx % 6) (beeswarm/swarm-offsets [1.0 1.0 1.0 1.0] d false)))))
    (testing ":fit squeezes into the room"
      (let [os (beeswarm/fit-offsets (beeswarm/swarm-offsets (repeat 20 1.0) d false) 10.0)]
        (is (every? #(<= (m/abs (double %)) 10.000001) os))
        (is (== 10.0 (reduce max (map #(m/abs (double %)) os))))))))

(deftest beeswarm-chart-test
  (doseq [flip [false true]
          c [(chart/beeswarm) (chart/beeswarm {:fit false :size 12}) (chart/beeswarm {:position :mirror})]]
    (is (instance? BufferedImage (plot/render-image {:data rows :mapping {:x :v :y :lvl :color :g} :charts [c] :flip flip} options))
        (str (:options c) " flip " flip)))
  (is (= ["a" "b" "c"] (sort (map :level (:items (panel-data (built {:data rows :mapping {:x :v :y :lvl}
                                                                     :charts [(chart/beeswarm)]})
                                                             0))))))
  (is (every? #(apply <= %) (map :values (:items (panel-data (built {:data rows :mapping {:x :v :y :lvl} :charts [(chart/beeswarm)]}) 0))))
      "values sorted"))

(deftest density-ridges-test
  (let [b (built {:data rows :mapping {:x :v :y :lvl} :charts [(chart/density)]})
        {:keys [ridges top overlap]} (panel-data b 0)
        y-scale (get-in b [:finalized-scales :y])
        pl (placement-of b {} :band)
        point (strip/ridge-point pl 0.9)
        step (double (:step (scale/band-geometry y-scale)))]
    (testing "one profile per level on a bands y scale, one height scale"
      (is (= 3 (count ridges)))
      (is (= :bands (:kind y-scale)))
      (is (== top (reduce max (mapcat :ds ridges))))
      (is (== 0.9 overlap)))
    (testing "the tallest profile is :overlap band steps high, toward higher :y"
      (let [^Vec2 base (point 5 "b" 1.0 0.0) ^Vec2 peak (point 5 "b" 1.0 1.0)]
        (is (m/delta-eq (* 0.9 step) (- (.y base) (.y peak)) 1.0e-9))))
    (testing "the y scale leaves room above the last level (after it in pixel order)"
      (let [fy (scale/forward-fn y-scale)]
        (is (= {:padding-out (* 0.5 (- (* 1.05 0.9) 0.5)) :align-outer 1.0}
               (select-keys (get-in b [:finalized-scales :y :attributes]) [:padding-out :align-outer])))
        (is (>= (- (fy "c") (* 0.9 step)) 0.0) "top ridge inside the panel"))))
  (testing "the plot's own y attributes win"
    (let [b (built {:data rows :mapping {:x :v :y :lvl} :scales {:y {:padding-out 0.0}} :charts [(chart/density)]})]
      (is (== 0.0 (get-in b [:finalized-scales :y :attributes :padding-out])))))
  (testing "mirror: the second group grows away from the next level"
    (let [{:keys [ridges]} (panel-data (built {:data rows :mapping {:x :v :y :lvl :color :g}
                                              :charts [(chart/density {:position :mirror})]}) 0)]
      (is (= #{1.0 -1.0} (set (map (comp strip/ridge-side :slot) ridges))))))
  (testing "errors"
    (is (= :position (:key (error #(built {:data rows :mapping {:x :v :y :lvl :color :g} :charts [(chart/density {:position :stack})]})))))
    (is (= :fill-gradient (:key (error #(built {:data rows :mapping {:x :v} :charts [(chart/density {:fill-gradient :mpl/viridis})]})))))
    (is (= :fill-gradient (:key (error #(built {:data rows :mapping {:x :v :y :lvl} :charts [(chart/density {:fill-gradient :no/such})]}))))))
  (testing "renders, flipped and not, with a gradient"
    (doseq [flip [false true]]
      (is (instance? BufferedImage (plot/render-image {:data rows :mapping {:x :v :y :lvl} :flip flip
                                                       :charts [(chart/density {:fill-gradient [:red :blue]})]} options))))))

(deftest histogram-ridges-test
  (let [b (built {:data rows :mapping {:x :v :y :lvl} :charts [(chart/histogram {:bins 5 :y :count})]})
        {:keys [ridges top]} (panel-data b 0)]
    (is (= 3 (count ridges)))
    (is (every? #(= 5 (count (:bars %))) ridges) "common breaks")
    (is (= 90.0 (reduce + (map #(nth % 2) (mapcat :bars ridges)))))
    (is (== top (reduce max (map #(nth % 2) (mapcat :bars ridges)))))
    (is (= :bands (get-in b [:finalized-scales :y :kind]))))
  (is (= :position (:key (error #(built {:data rows :mapping {:x :v :y :lvl :color :g} :charts [(chart/histogram {:position :dodge})]})))))
  (doseq [flip [false true]]
    (is (instance? BufferedImage (plot/render-image {:data rows :mapping {:x :v :y :lvl :color :g} :flip flip
                                                     :charts [(chart/histogram {:position :mirror})]} options)))))

(defn- red? [^long rgb]
  (let [r (bit-and (bit-shift-right rgb 16) 0xff) g (bit-and (bit-shift-right rgb 8) 0xff) b (bit-and rgb 0xff)]
    (and (> r 180) (< g 90) (< b 90))))

(deftest bottom-placement-test
  (let [p {:data rows :mapping {:x :v} :charts [(chart/density) (chart/rug {:place :bottom :color :red :line-width 2})]}
        b (built p)
        y-scale (get-in b [:finalized-scales :y])
        fy (scale/forward-fn y-scale)
        {:keys [x y w h]} (panel-box b)]
    (testing "the y domain starts below 0 (bottom share padding)"
      (is (neg? (double (first (:domain y-scale))))))
    (testing "the rug lies between y = 0 and the panel's low edge"
      (let [^BufferedImage img (plot/render-image p options)
            zero (+ (double y) (fy 0.0))
            reds (for [i (range (.getWidth img)) j (range (.getHeight img)) :when (red? (.getRGB img (int i) (int j)))] [i j])]
        (is (seq reds))
        (is (every? (fn [[i j]] (and (<= x i (+ x w)) (< zero j (+ y h)))) reds))))
    (testing "span = :width of the gap"
      (let [{:keys [span]} (placement-of b {} :bottom)]
        (is (m/delta-eq (* 0.8 (- (double h) (fy 0.0))) (span nil) 1.0e-9)))))
  (testing ":bottom-share sets the padding"
    (is (= [0.3 0.0] (get-in (built {:data rows :mapping {:x :v} :charts [(chart/rug {:place :bottom :bottom-share 0.3})]})
                             [:stats 0 :padding :y]))))
  (testing "needs a numeric y scale"
    (is (= :place (:key (error #(plot/render-image {:data rows :mapping {:x :v} :scales {:y {:domain ["a"]}}
                                                    :charts [(chart/rug {:place :bottom})]} options))))))
  (testing "every strip chart renders on :bottom, flipped and not"
    (doseq [c [(chart/box) (chart/violin) (chart/stripplot {:rng (r/rng :mersenne 1)}) (chart/beeswarm) (chart/rug)
               (chart/extent)]
            flip [false true]]
      (is (instance? BufferedImage (plot/render-image {:data rows :mapping {:x :v} :flip flip
                                                       :charts [(chart/density) (assoc-in c [:options :place] :bottom)]}
                                                      options))
          (str (:kind c) " flip " flip)))))
