(ns cljplot2.annotation-test
  "Cycle 4: annotation charts (rule, rect, text) and strip n labels."
  (:require [clojure.test :refer [deftest testing is are]]
            [fastmath.core :as m]
            [cljplot2.test-util :refer [error error-ins]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.annotation :as annotation]
            [cljplot2.data.facet :as facet]
            [cljplot2.layout.grid :as grid]
            [cljplot2.scale.position :as scale])
  (:import [java.awt.image BufferedImage]))

(def ^:private rows
  (vec (for [i (range 60)] {:v (+ (mod (* i 37) 23) (* 4 (mod i 3))) :w (mod (* i 11) 17)
                            :lvl (["a" "b" "c"] (mod i 3)) :g (["p" "q"] (mod i 2))})))

(def ^:private options {:width 500 :height 400})
(def ^:private red-theme {:text {:annotation {:color [255 0 0]}}})

(defn- built [p] (plot/build p options))
(defn- panel-box [b] (grid/find-content-box (:grid b) (:layout b) #(= :panel (:kind %))))

(defn- red? [^long rgb]
  (let [r (bit-and (bit-shift-right rgb 16) 0xff) g (bit-and (bit-shift-right rgb 8) 0xff) b (bit-and rgb 0xff)]
    (and (> r 180) (< g 90) (< b 90))))

(defn- reds
  "`[x y]` of the red pixels of the rendered plot `p`."
  [p]
  (let [^BufferedImage img (plot/render-image p options)]
    (vec (for [i (range (.getWidth img)) j (range (.getHeight img)) :when (red? (.getRGB img (int i) (int j)))] [i j]))))

(defn- legends [b] (some-> (first (filter #(= :legends (:id %)) (:items (:grid b)))) :content :legends))

;; 1. contract

(deftest modes-test
  (testing "literal rule ignores the plot mapping"
    (let [b (built {:data rows :mapping {:x :v :y :w} :charts [(chart/scatter) (chart/rule {:y 0})]})
          {:keys [rules]} (get-in b [:stats 1 :data facet/all-panels])]
      (is (= [{:xs [] :ys [0] :ablines []}] rules))))
  (testing "data mode reads only the chart's own mapping"
    (let [b (built {:data rows :mapping {:x :v :y :w}
                    :charts [(chart/scatter) (chart/rule {:data [{:a 1} {:a 2}] :mapping {:y :a}})]})
          rules (mapcat :rules (vals (get-in b [:stats 1 :data])))]
      (is (= [[] [1 2]] [(vec (mapcat :xs rules)) (vec (mapcat :ys rules))]))))
  (testing "literal wins over the own mapping"
    (let [b (built {:data rows :mapping {:x :v} :charts [(chart/density) (chart/rule {:x 3 :mapping {:x :v}})]})]
      (is (= [3] (get-in b [:stats 1 :data facet/all-panels :rules 0 :xs])))))
  (testing "no literal positions and no own mapping"
    (are [c] (= :mapping (:key (error #(built {:data rows :mapping {:x :v :y :w} :charts [(chart/scatter) c]}))))
      (chart/rule) (chart/rect) (chart/text) (chart/rule {:mapping {:slope :v}})))
  (testing "data mode with no rows draws nothing"
    (is (instance? BufferedImage (plot/render-image {:data rows :mapping {:x :v :y :w}
                                                     :charts [(chart/scatter) (chart/rule {:data [] :mapping {:x :a}})]}
                                                    options)))))

(deftest facets-test
  (let [means [{:lvl "a" :m 5} {:lvl "b" :m 10}]
        b (built {:data rows :mapping {:x :v :facet :lvl}
                  :charts [(chart/density) (chart/rule {:x 3}) (chart/rule {:data means :mapping {:x :m}})]})]
    (is (= [facet/all-panels] (keys (get-in b [:stats 1 :data]))) "literal: every panel")
    (is (= #{"a" "b"} (set (keys (get-in b [:stats 2 :data])))) "data: own panels only")
    (is (= [5] (get-in b [:stats 2 :data "a" :rules 0 :xs])))))

(deftest extents-test
  (let [x-domain (fn [c] (get-in (built {:data rows :mapping {:x :v} :charts [(chart/density) c]}) [:finalized-scales :x :domain]))]
    (is (>= (double (second (x-domain (chart/rule {:x 100})))) 100.0))
    (is (< (double (second (x-domain (chart/rule {:x 100 :extent false})))) 50.0))
    (is (= (x-domain (chart/text {:x 5 :y [:panel 0.5] :text "a very long label indeed"}))
           (x-domain (chart/text {:x 5 :y [:panel 0.5] :text "a"}))) "text: only the anchor")
    (is (<= (double (first (x-domain (chart/rect {:x [-50 nil]})))) -50.0) "nil end adds nothing")
    (is (= :extents (:key (error #(built {:data rows :mapping {:x :v} :charts [(chart/density) (chart/rule {:x [1 "a"]})]})))))))

(deftest flip-test
  (testing "a flipped :x rule is horizontal"
    (let [ps (reds {:data rows :mapping {:x :v :y :w} :flip true :charts [(chart/scatter {:color :black}) (chart/rule {:x 10 :color :red})]})]
      (is (seq ps))
      (is (= 1 (count (distinct (map second ps)))))))
  (testing "flipped text stays upright (wider than tall)"
    (let [ps (reds {:data rows :mapping {:x :v :y :w} :flip true :theme red-theme
                    :charts [(chart/scatter {:color :black}) (chart/text {:x 10 :y 8 :text "WWWWWWWW"})]})
          xs (map first ps) ys (map second ps)]
      (is (> (- (reduce max xs) (reduce min xs)) (* 3 (- (reduce max ys) (reduce min ys))))))))

(deftest bands-test
  (let [b (built {:data rows :mapping {:x :v :y :lvl} :charts [(chart/box) (chart/rect {:y ["b" "b"]}) (chart/rule {:y "c"})]})
        y (get-in b [:finalized-scales :y])
        c (scale/forward y "b")
        half (* 0.5 (double (:step (scale/band-geometry y))))]
    (is (= [(- c half) (+ c half)] (annotation/interval-pixels y "b" "b")))
    (is (= (scale/forward y "c") (annotation/pixel y "c")))
    (is (m/nan? (annotation/pixel y "zzz")) "unknown level unplaceable")))

(deftest color-legend-test
  (let [means [{:g "p" :m 5} {:g "q" :m 10}]
        b (built {:data rows :mapping {:x :v :color :g}
                  :charts [(chart/density) (chart/rule {:data means :mapping {:x :m :color :g}})]})]
    (is (= [0 1] (mapv :chart (:charts (first (legends b))))))
    (is (= #{{"p" 5} {"q" 10}} (set (map (fn [r] {(get-in r [:aesthetics :color]) (first (:xs r))})
                                         (get-in b [:stats 1 :data facet/single-panel :rules]))))))
  (testing "literal annotations add no legend keys"
    (let [b (built {:data rows :mapping {:x :v :color :g} :charts [(chart/density) (chart/rule {:x 3})]})]
      (is (= [0] (mapv :chart (:charts (first (legends b)))))))))

(deftest theme-test
  (is (= #{[:theme :chart :rule :colour]} (error-ins #(plot/build {:theme {:chart {:rule {:colour :red}}}}))))
  (is (= #{[:theme :chart :rect :color]} (error-ins #(plot/build {:theme {:chart {:rect {:color :red}}}}))))
  (is (seq (error-ins #(plot/build {:theme {:text {:annotation {:size -1}}}})))))

;; 2. rule

(deftest rule-test
  (let [p {:data rows :mapping {:x :v :y :w} :charts [(chart/scatter {:color :black})]}
        b (built p)
        {:keys [x y]} (panel-box b)
        fx (scale/forward-fn (get-in b [:finalized-scales :x]))
        fy (scale/forward-fn (get-in b [:finalized-scales :y]))]
    (testing ":x at its pixel"
      (let [ps (reds (update p :charts conj (chart/rule {:x 10 :color :red})))]
        (is (= #{(long (m/floor (+ (double x) (fx 10))))} (set (map first ps))))))
    (testing "ab line through (5, 5) and (15, 10)"
      (let [ps (set (reds (update p :charts conj (chart/rule {:slope 0.5 :intercept 2.5 :color :red}))))]
        (doseq [[dx dy] [[5 5] [15 10]]]
          (is (some (fn [[i j]] (and (<= (m/abs (- (double i) (+ (double x) (fx dx)))) 1.5)
                                     (<= (m/abs (- (double j) (+ (double y) (fy dy)))) 1.5)))
                    ps)))))
    (testing "several, broadcast slopes"
      (is (= [[1 0] [2 0]] (get-in (built (update p :charts conj (chart/rule {:slope [1 2] :intercept 0})))
                                   [:stats 1 :data facet/all-panels :rules 0 :ablines]))))
    (testing "errors"
      (is (= :intercept (:key (error #(built (update p :charts conj (chart/rule {:slope 1})))))))
      (is (= :slope (:key (error #(built (update p :charts conj (chart/rule {:intercept 1})))))))
      (is (= :intercept (:key (error #(built (update p :charts conj (chart/rule {:slope [1 2 3] :intercept [1 2]})))))))
      (is (= :slope (:key (error #(plot/render-image (-> p (assoc :scales {:y {:kind :log}})
                                                          (update :data (partial map (fn [r] (update r :w inc))))
                                                          (update :charts conj (chart/rule {:slope 1 :intercept 1})))
                                                     options)))))
      (is (= #{[:charts 0 :options :x]} (error-ins #(built {:charts [(chart/rule {:x []})]})))))))

;; 3. rect

(deftest rect-test
  (let [b (built {:data rows :mapping {:x :v :y :w} :charts [(chart/scatter)]})
        xs (get-in b [:finalized-scales :x])
        [r0 r1] (:range xs)]
    (is (= [(scale/forward xs 5) (double r1)] (annotation/interval-pixels xs 5 nil)) "nil high end - panel edge")
    (is (= [(double r0) (scale/forward xs 5)] (annotation/interval-pixels xs nil 5)) "nil low end")
    (is (= [(scale/forward xs 2) (scale/forward xs 8)] (annotation/interval-pixels xs 8 2)) "reversed ends")
    (is (nil? (annotation/interval-pixels nil 1 2))))
  (testing "pairs: 1 or n"
    (let [rect-stats #(get-in (built {:data rows :mapping {:x :v :y :w} :charts [(chart/scatter) %]})
                              [:stats 1 :data facet/all-panels :rects])]
      (is (= [{:x [1 2] :y [3 4]} {:x [1 2] :y [5 6]}] (rect-stats (chart/rect {:x [1 2] :y [[3 4] [5 6]]}))))
      (is (= [{:x [1 2]} {:x [3 nil]}] (rect-stats (chart/rect {:x [[1 2] [3 nil]]}))))
      (is (= :x (:key (error #(rect-stats (chart/rect {:x [[1 2] [3 4]] :y [[1 2] [3 4] [5 6]]})))))
          "the vector of neither 1 nor n items")))
  (testing "data mode: rows with invalid ends skipped, nil = edge"
    (let [b (built {:data rows :mapping {:x :v} :charts [(chart/density)
                                                        (chart/rect {:data [{:a 1 :b 2} {:a nil :b 3} {:a "x" :b ##NaN}]
                                                                     :mapping {:x0 :a :x1 :b}})]})]
      (is (= [{:x [1 2]} {:x [nil 3]}] (get-in b [:stats 1 :data facet/single-panel :rects])))
      (is (= {:position 1} (get-in b [:stats 1 :dropped])))))
  (testing "rect listed first stays behind"
    (let [ps (reds {:data rows :mapping {:x :v} :charts [(chart/rect {:x [0 30] :fill :red}) (chart/histogram {:color :black})]})
          ps2 (reds {:data rows :mapping {:x :v} :charts [(chart/histogram {:color :black}) (chart/rect {:x [0 30] :fill :red})]})]
      (is (< (count ps) (count ps2))))))

;; 4. text

(deftest text-test
  (let [p {:data rows :mapping {:x :v :y :w} :theme red-theme :charts [(chart/scatter {:color :black})]}
        b (built p)
        {:keys [x y h]} (panel-box b)
        fx (scale/forward-fn (get-in b [:finalized-scales :x]))
        fy (scale/forward-fn (get-in b [:finalized-scales :y]))]
    (testing "anchor [:left :bottom] with an offset: the text starts right of and above the point"
      (let [ps (reds (update p :charts conj (chart/text {:x 10 :y 8 :text "Hello" :anchor [:left :bottom] :offset [5 5]})))
            px (+ (double x) (fx 10) 5.0) py (- (+ (double y) (fy 8)) 5.0)]
        (is (seq ps))
        (is (every? (fn [[i j]] (and (>= (double i) (- px 1.0)) (<= (double j) (+ py 1.0)))) ps))
        (is (< (- (double (reduce min (map first ps))) px) 6.0) "close to the anchor")))
    (testing "[:panel f]"
      (let [ys (get-in b [:finalized-scales :y])]
        (is (m/delta-eq (* 0.05 (double h)) (annotation/axis-pixel ys [:panel 0.95]) 1.0e-9))
        (is (m/nan? (annotation/axis-pixel nil 1)))))
    (testing "vectors paired, 1 broadcast"
      (is (= [{:x 1 :y 2 :text "a"} {:x 3 :y 2 :text "b"}]
             (get-in (built (update p :charts conj (chart/text {:x [1 3] :y 2 :text ["a" "b"]})))
                     [:stats 1 :data facet/all-panels :groups 0 :labels]))))
    (testing "errors"
      (is (= :y (:key (error #(built (update p :charts conj (chart/text {:x 1 :text "a"})))))))
      (is (= :text (:key (error #(built (update p :charts conj (chart/text {:x [1 2 3] :y 1 :text ["a" "b"]})))))))
      (is (seq (error-ins #(built (update p :charts conj (chart/text {:x [:panel 2] :y 1 :text "a"})))))))
    (testing "data mode with :color, rotation renders"
      (is (instance? BufferedImage (plot/render-image (update p :charts conj (chart/text {:mapping {:x :v :y :w :label :lvl :color :lvl}
                                                                                        :rotation 30}))
                                                      options))))))

;; 5. n labels

(deftest n-label-test
  (let [p (fn [c flip] {:data rows :mapping {:x :v :y :lvl} :theme red-theme :flip flip :charts [c]})]
    (testing "each label past its box's upper whisker, flipped and not"
      (doseq [flip [false true]
              :let [c (chart/box {:n-label true :color :black :outliers false})
                    b (built (p c flip))
                    {:keys [x y]} (panel-box b)
                    fx (scale/forward-fn (get-in b [:finalized-scales :x]))
                    fy (scale/forward-fn (get-in b [:finalized-scales :y]))
                    ps (reds (p c flip))]
              {:keys [level uav]} (get-in b [:stats 0 :data facet/single-panel :items])
              :let [cross (fn [[i j]] (if flip (- (double i) (double x)) (- (double j) (double y))))
                    along (fn [[i j]] (if flip (- (double j) (double y)) (- (double i) (double x))))
                    mine (filter #(< (m/abs (- (double (cross %)) (fy level))) 10.0) ps)]]
        (is (seq mine) (str level))
        (is (every? #(if flip (< (double (along %)) (fx uav)) (> (double (along %)) (fx uav))) mine)
            (str level " flip " flip))))
    (testing "every strip chart renders with n labels"
      (doseq [c [(chart/box) (chart/violin) (chart/stripplot) (chart/beeswarm) (chart/rug) (chart/extent)]
              flip [false true]]
        (is (instance? BufferedImage (plot/render-image (p (assoc-in c [:options :n-label] true) flip) options))
            (str (:kind c) " " flip))))
    (testing "n labels get value-axis room: every label inside the panel, flipped and not"
      (doseq [flip [false true]
              :let [c (chart/box {:n-label true :color :black :n-format "n = %d items"})
                    b (built (p c flip))
                    {:keys [x y w h]} (panel-box b)
                    ps (reds (p c flip))]]
        (is (seq ps))
        (is (every? (fn [[i j]] (and (< (double x) (double i) (+ (double x) (double w)))
                                     (< (double y) (double j) (+ (double y) (double h)))))
                    ps)
            (str "flip " flip))
        (is (> (double (second (get-in b [:finalized-scales :x :domain])))
               (double (second (get-in (built (p (chart/box) flip)) [:finalized-scales :x :domain])))))))
    (testing "without n labels no pixel room is asked"
      (is (nil? (get-in (built (p (chart/box) false)) [:stats 0 :padding-px])))
      (is (nil? (get-in (built (p (chart/box {:n-label false}) false)) [:scale-specs :x :padding-px]))))
    (testing ":n-format"
      (is (= :n-format (:key (error #(built (p (chart/box {:n-format "%d %d"}) false))))))
      (is (instance? BufferedImage (plot/render-image (p (chart/box {:n-label true :n-format "%d obs"}) false) options))))))

(deftest padding-px-test
  (let [spec (fn [extra] (merge {:kind :linear :domain [0.0 10.0] :nice false} extra))
        domain (fn [extra length] (:domain (scale/finalize (spec extra) [0.0 (double length)] 5)))]
    (testing "the room lies beyond the data: 10 maps to length - 50"
      (let [fin (scale/finalize (spec {:padding-px [0 50]}) [0.0 500.0] 5)]
        (is (m/delta-eq 450.0 (scale/forward fin 10.0) 1.0e-9))
        (is (m/delta-eq 0.0 (scale/forward fin 0.0) 1.0e-9))))
    (testing "both sides"
      (let [fin (scale/finalize (spec {:padding-px [20 30]}) [0.0 500.0] 5)]
        (is (m/delta-eq 20.0 (scale/forward fin 0.0) 1.0e-9))
        (is (m/delta-eq 470.0 (scale/forward fin 10.0) 1.0e-9))))
    (testing "reversed range (vertical axis)"
      (let [fin (scale/finalize (spec {:padding-px [0 50]}) [500.0 0.0] 5)]
        (is (m/delta-eq 50.0 (scale/forward fin 10.0) 1.0e-9))))
    (testing "after fraction padding"
      (let [fin (scale/finalize (spec {:padding [0.1 0.1] :padding-px [0 50]}) [0.0 500.0] 5)]
        (is (m/delta-eq 450.0 (scale/forward fin 11.0) 1.0e-9))))
    (testing "log scales in log space"
      (let [fin (scale/finalize {:kind :log :domain [1.0 100.0] :nice false :padding-px [0 50]} [0.0 500.0] 5)]
        (is (m/delta-eq 450.0 (scale/forward fin 100.0) 1.0e-6))))
    (testing "zero, over half the range, and the provisional unit range leave the domain"
      (is (= [0.0 10.0] (domain {:padding-px [0 0]} 500)))
      (is (= [0.0 10.0] (domain {:padding-px [0 251]} 500)))
      (is (= [0.0 10.0] (domain {:padding-px [0 20]} 1))))
    (testing "bands scales ignore it"
      (is (= ["a" "b"] (:domain (scale/finalize {:kind :bands :domain ["a" "b"] :padding-px [0 50]} [0.0 500.0] 2)))))))
