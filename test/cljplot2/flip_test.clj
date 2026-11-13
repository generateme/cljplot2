(ns cljplot2.flip-test
  "Plot `:flip`: scale directions, projection, axes, titles, iso and the charts drawn through the projection."
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure2d.core :as c2d]
            [cljplot2.test-util :refer [error-ins]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.layout.grid :as grid]
            [cljplot2.scale.position :as scale])
  (:import [java.awt.image BufferedImage]
           [fastmath.vector Vec2]))

(def ^:private scatter-plot
  {:data (for [i (range 20)] {:x i :y (* 2 i)})
   :mapping {:x :x :y :y}
   :charts [(chart/scatter)]
   :labels {:x "x title" :y "y title"}})

(defn- built [p] (plot/build p {:width 500 :height 400}))

(defn- panel-box [b] (grid/find-content-box (:grid b) (:layout b) #(= :panel (:kind %))))

(deftest direction-test
  (testing "unflipped: :x horizontal [0 w], :y vertical [h 0]"
    (let [b (built scatter-plot)
          {:keys [w h]} (panel-box b)]
      (is (= :horizontal (get-in b [:finalized-scales :x :direction])))
      (is (= :vertical (get-in b [:finalized-scales :y :direction])))
      (is (= [0.0 (double w)] (get-in b [:finalized-scales :x :range])))
      (is (= [(double h) 0.0] (get-in b [:finalized-scales :y :range])))))
  (testing "flipped: :x vertical [h 0], :y horizontal [0 w]"
    (let [b (built (assoc scatter-plot :flip true))
          {:keys [w h]} (panel-box b)]
      (is (= :vertical (get-in b [:finalized-scales :x :direction])))
      (is (= :horizontal (get-in b [:finalized-scales :y :direction])))
      (is (= [(double h) 0.0] (get-in b [:finalized-scales :x :range])))
      (is (= [0.0 (double w)] (get-in b [:finalized-scales :y :range])))))
  (testing ":flip false is the default"
    (is (= (:finalized-scales (built scatter-plot)) (:finalized-scales (built (assoc scatter-plot :flip false))))))
  (testing "direction without :direction falls back to the key"
    (is (= :horizontal (scale/direction :x {})))
    (is (= :vertical (scale/direction :y {})))))

(deftest projection-test
  (let [scales (:finalized-scales (built scatter-plot))
        fx (scale/forward-fn (:x scales))
        fy (scale/forward-fn (:y scales))
        proj (scale/projection scales)]
    (testing "unflipped: bit-identical to forward-fn"
      (doseq [[x y] [[0 0] [3.3 7.1] [19 38] [-5 100]]
              :let [^Vec2 p (proj x y)]]
        (is (= (Double/doubleToLongBits (fx x)) (Double/doubleToLongBits (.x p))))
        (is (= (Double/doubleToLongBits (fy y)) (Double/doubleToLongBits (.y p))))))
    (testing "missing scales give NaN in their coordinate"
      (is (Double/isNaN (.y ^Vec2 ((scale/projection (dissoc scales :y)) 1 1))))
      (is (Double/isNaN (.x ^Vec2 ((scale/projection (dissoc scales :x)) 1 1))))
      (is (Double/isNaN (.x ^Vec2 ((scale/projection {}) 1 1))))))
  (let [scales (:finalized-scales (built (assoc scatter-plot :flip true)))
        fx (scale/forward-fn (:x scales))
        fy (scale/forward-fn (:y scales))
        proj (scale/projection scales)]
    (testing "flipped: px from y, py from x"
      (is (scale/flipped? scales))
      (is (scale/flipped? (dissoc scales :x)) "only :y, drawn horizontally")
      (doseq [[x y] [[0 0] [3.3 7.1] [19 38]]
              :let [^Vec2 p (proj x y)]]
        (is (== (fy y) (.x p)))
        (is (== (fx x) (.y p)))))))

(defn- items-by-id [b] (into {} (map (juxt :id identity)) (:items (:grid b))))

(deftest axes-and-titles-test
  (testing "single panel: the :y axis under the panel, :x left; titles follow their scale"
    (let [items (items-by-id (built (assoc scatter-plot :flip true)))]
      (is (= :y (get-in items [:x-axis :content :scale])))
      (is (= :x (get-in items [:y-axis :content :scale])))
      (is (= #{:y} (set (map #(get-in items [% :content :scale]) [:x-axis-start :x-axis-end]))))
      (is (= #{:x} (set (map #(get-in items [% :content :scale]) [:y-axis-start :y-axis-end]))))
      (is (= "y title" (get-in items [:x-title :content :text])))
      (is (= "x title" (get-in items [:y-title :content :text])))
      (is (= 270 (get-in items [:y-title :content :rotation])) "the left title keeps its default rotation")))
  (testing "facets: horizontal axes per column carry :y, vertical per row :x"
    (let [p (assoc scatter-plot :flip true :data (for [i (range 40)] {:x i :y i :f (mod i 4)}) :mapping {:x :x :y :y :facet :f})
          items (vals (items-by-id (built p)))
          scale-of (fn [pred] (set (map #(get-in % [:content :scale]) (filter #(pred (:id %)) items))))]
      (is (= #{:y} (scale-of #(and (vector? %) (= :x-axis (first %))))))
      (is (= #{:x} (scale-of #(and (vector? %) (= :y-axis (first %))))))))
  (testing "the horizontal axis is as wide as the panel, the vertical one as tall"
    (let [b (built (assoc scatter-plot :flip true))
          panel (panel-box b)]
      (is (= (:w panel) (:w (grid/item-box (:layout b) :x-axis))))
      (is (= (:h panel) (:h (grid/item-box (:layout b) :y-axis)))))))

(deftest theme-by-scale-key-test
  (testing "[:axis :x :tick-spacing] sets the tick count of the :x scale, also drawn vertically"
    (let [ticks (fn [p k] (count (get-in (built p) [:finalized-scales k :ticks])))
          flipped (assoc scatter-plot :flip true)
          sparse (assoc flipped :theme {:axis {:x {:tick-spacing 300}}})]
      (is (< (ticks sparse :x) (ticks flipped :x)))
      (is (= (ticks sparse :y) (ticks flipped :y))))))

(deftest iso-flip-test
  (let [p {:data (for [i (range 60) :let [t (* 2.0 Math/PI (/ i 60.0))]] {:x (* 2.0 (Math/cos t)) :y (Math/sin t)})
           :mapping {:x :x :y :y} :charts [(chart/scatter)] :aspect :iso :flip true}]
    (doseq [[w h] [[600 400] [400 600] [300 300]]
            :let [b (plot/build p {:width w :height h})
                  {pw :w ph :h} (panel-box b)
                  span (fn [k] (let [[a z] (get-in b [:finalized-scales k :domain])] (Math/abs (- (double z) (double a)))))
                  px (/ (double pw) (span :y)) ;; :y is horizontal
                  py (/ (double ph) (span :x))]]
      (is (<= (/ (Math/abs (- px py)) (max px py)) (/ 1.0 (double (min pw ph)))) (str w "x" h))
      (is (> ph pw) "the longer :x span is vertical"))))

(deftest flip-validation-test
  (is (= #{[:flip]} (error-ins #(plot/build {:flip "yes"}))))
  (is (= #{[:flip]} (error-ins #(plot/build {:flip true :custom-grid {:columns [{:size [:flexible]}] :rows [{:size [:flexible]}] :items []}}))))
  (is (= #{[:flip]} (error-ins #(plot/build {:flip true :arrange {:plots [{}]}}))))
  (is (some? (plot/build (plot/arrange [(assoc scatter-plot :flip true) scatter-plot] {:columns 2}))) "sub-plots flip independently"))

;; charts: data at low :x - unflipped the marks are in the left half of the panel, flipped in the bottom half

(defn- red? [^long rgb]
  (let [r (bit-and (bit-shift-right rgb 16) 0xff) g (bit-and (bit-shift-right rgb 8) 0xff) b (bit-and rgb 0xff)]
    (and (> r 180) (< g 90) (< b 90))))

(defn- red-halves
  "Red pixel counts `{:left :right :top :bottom}` inside the panel of plot `p` rendered 400x400."
  [p]
  (let [options {:width 400 :height 400}
        b (plot/build p options)
        {:keys [x y w h]} (panel-box b)
        ^BufferedImage img (plot/render-image p options)
        x (long x) y (long y) w (long w) h (long h)
        pixels (for [i (range x (+ x w)) j (range y (+ y h))
                     :when (red? (.getRGB img (int i) (int j)))]
                 [(< (- i x) (quot w 2)) (< (- j y) (quot h 2))])]
    {:left (count (filter first pixels)) :right (count (remove first pixels))
     :top (count (filter second pixels)) :bottom (count (remove second pixels))}))

(def ^:private low-x (for [i (range 30)] {:x (+ 1.0 (* 0.05 i)) :y (* 0.3 i)}))

(def ^:private charts
  {:scatter {:mapping {:x :x :y :y} :charts [(chart/scatter {:color :red :size 6})]}
   :line {:mapping {:x :x :y :y} :charts [(chart/line {:color :red :width 3})]}
   :histogram {:mapping {:x :x} :charts [(chart/histogram {:color :red :bins 3})]}
   :density {:mapping {:x :x} :charts [(chart/density {:color :red :width 3})]}
   :ecdf {:mapping {:x :x} :charts [(chart/ecdf {:color :red :width 3})]}})

(deftest charts-flip-test
  (doseq [[kind p] charts
          :let [p (assoc p :data low-x :scales {:x {:domain [0 20]}})
                plain (red-halves p)
                flipped (red-halves (assoc p :flip true))]]
    (is (pos? (:left plain)) (str kind " draws"))
    (is (> (:left plain) (:right plain)) (str kind " unflipped: low x on the left"))
    (is (> (:bottom flipped) (:top flipped)) (str kind " flipped: low x at the bottom"))))
