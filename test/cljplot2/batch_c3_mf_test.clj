(ns cljplot2.batch-c3-mf-test
  "Batch C3, item 9: plot margins with facets - a margin per facet panel, from that panel's rows, sharing its scale."
  (:require [clojure.test :refer [deftest testing is]]
            [cljplot2.test-util :refer [error-ins]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.facet :as facet]
            [cljplot2.layout.grid :as grid]
            [cljplot2.layout.plot-grid :as plot-grid])
  (:import [java.awt.image BufferedImage]))

(def ^:private xy
  (vec (for [i (range 90)] {:a (+ 1.0 (mod (* i 7) 13) (* 10 (mod i 3))) :b (+ 2.0 (mod (* i 11) 17) (* 5 (mod i 2)))
                            :s (["p" "q" "r"] (mod i 3)) :t (["u" "v"] (mod i 2)) :k (["x" "y"] (mod (quot i 3) 2))})))

(def ^:private options {:width 700 :height 500})

(defn- built [p] (plot/build p options))

(defn- items-where [b pred] (filter (comp pred :content) (grid/content-items (:grid b) (:layout b) (constantly true))))
(defn- main-panels [b] (into {} (map (juxt (comp :panel :content) :box))
                             (items-where b #(and (= :panel (:kind %)) (nil? (:margin %))))))
(defn- margin-panels [b side] (into {} (map (juxt (comp :panel :content) :box))
                                    (items-where b #(and (= :panel (:kind %)) (= side (:margin %))))))
(defn- margin-axes [b side] (items-where b #(and (= :axis (:kind %)) (= side (:margin %)))))
(defn- bottom [{:keys [y h]}] (+ (double y) (double h)))
(defn- right [{:keys [x w]}] (+ (double x) (double w)))

(def ^:private wrap {:data xy :mapping {:x :a :y :b :facet :s} :charts [(chart/scatter)]})

(deftest wrap-margins-test
  (let [b (built (assoc wrap :margins {:top (chart/density) :right {:charts (chart/histogram {:bins 5}) :axis true :size 50}}))
        panels (main-panels b)
        tops (margin-panels b :top)
        rights (margin-panels b :right)]
    (testing "one margin per panel and side"
      (is (= #{"p" "q" "r"} (set (keys panels)) (set (keys tops)) (set (keys rights)))))
    (testing "a top margin above its panel, as wide; a right margin right of it, as tall"
      (doseq [k (keys panels) :let [p (panels k) t (tops k) r (rights k)]]
        (is (= [(:x p) (:w p)] [(:x t) (:w t)]))
        (is (< (bottom t) (double (:y p))))
        (is (= [(:y p) (:h p)] [(:y r) (:h r)]))
        (is (> (double (:x r)) (right p)))))
    (testing "margin stats per panel, from that panel's rows"
      (let [data (get-in b [:margin-env :top :stats 0 :data])]
        (is (= #{"p" "q" "r"} (set (keys data))))
        (is (not= (get-in data ["p" :curves 0 :x]) (get-in data ["r" :curves 0 :x])))
        (is (every? #(< 12.0 (double %)) (get-in data ["r" :curves 0 :x])) "panel r's data lies right of 12")))
    (testing "same scales: the shared scale is the plot's, no panel scales"
      (is (= (get-in b [:finalized-scales :x]) (get-in b [:margin-env :top :scales :x])))
      (is (nil? (get-in b [:margin-env :top :panel-scales]))))
    (testing "axis: right margins' magnitude axes at the bottom panel of every column (one row - every panel)"
      (is (= 3 (count (margin-axes b :right))))))
  (testing "same scales in a 2 x 2 wrap: the top margins' axis at the first panel of each row only"
    (let [b (built {:data xy :mapping {:x :a :y :b :facet :s} :facets {:columns 2} :charts [(chart/scatter)]
                    :margins {:top {:charts (chart/density) :axis true}}})]
      (is (= 2 (count (margin-axes b :top))))
      (is (every? #(= :y-axis (:column %)) (filter #(= {:kind :axis :margin :top} (select-keys (:content %) [:kind :margin])) (:items (:grid b))))))))

(deftest grid-margins-test
  (let [b (built {:data xy :mapping {:x :a :y :b :facet-col :s :facet-row :t} :charts [(chart/scatter)]
                  :margins {:top {:charts (chart/density) :size 40} :right {:charts (chart/box) :size 40}
                            :bottom {:charts (chart/rug) :size 30} :left {:charts (chart/density) :size 40}}})
        panels (main-panels b)]
    (is (= 6 (count panels)))
    (testing "all four sides around every panel"
      (doseq [side [:top :right :bottom :left]]
        (is (= (set (keys panels)) (set (keys (margin-panels b side)))) (str side)))
      (doseq [[k p] panels]
        (is (< (bottom ((margin-panels b :top) k)) (double (:y p))))
        (is (> (double (:y ((margin-panels b :bottom) k))) (bottom p)))
        (is (< (right ((margin-panels b :left) k)) (double (:x p))))
        (is (> (double (:x ((margin-panels b :right) k))) (right p)))))
    (testing "margins of a panel column / row line up"
      (is (= 3 (count (distinct (map (juxt :x :w) (vals (margin-panels b :top)))))))
      (is (= 2 (count (distinct (map (juxt :y :h) (vals (margin-panels b :right))))))))
    (testing "the main axes stay outside the margins"
      (let [x-axes (items-where b #(and (= :axis (:kind %)) (nil? (:margin %)) (= :x (:scale %))))
            y-axes (items-where b #(and (= :axis (:kind %)) (nil? (:margin %)) (= :y (:scale %))))]
        (is (every? #(>= (double (get-in % [:box :y])) (apply max (map bottom (vals (margin-panels b :bottom))))) x-axes))
        (is (every? #(<= (right (:box %)) (apply min (map (comp double :x) (vals (margin-panels b :left))))) y-axes))))))

(deftest free-scales-test
  (testing "free x in a wrap: each top margin follows its panel's scale"
    (let [b (built (assoc wrap :facets {:scales {:x :free}} :margins {:top (chart/density) :right (chart/density)}))]
      (doseq [k ["p" "q" "r"]]
        (is (= (get-in b [:panel-scales k :x]) (get-in b [:margin-env :top :panel-scales k :x])))
        (is (= (get-in b [:panel-scales k :y]) (get-in b [:margin-env :right :panel-scales k :x]))))
      (is (not= (get-in b [:margin-env :top :panel-scales "p" :x :domain]) (get-in b [:margin-env :top :panel-scales "r" :x :domain])))))
  (testing "free y and free both: right margins follow their panels; axes at every panel"
    (doseq [scales [{:y :free} {:x :free :y :free}]]
      (let [b (built (assoc wrap :facets {:scales scales}
                            :margins {:right {:charts (chart/histogram {:bins 4}) :axis true} :top {:charts (chart/density) :axis true}}))]
        (doseq [k ["p" "q" "r"]]
          (is (= (get-in b [:panel-scales k :y]) (get-in b [:margin-env :right :panel-scales k :x]))))
        (is (= 3 (count (margin-axes b :right))) (str scales))
        (is (= 3 (count (margin-axes b :top))) (str scales)))))
  (testing "panel weights: margins as wide as their weighted panels"
    (let [b (built (assoc wrap :facets {:widths [1 2 1]} :margins {:top (chart/density)}))]
      (doseq [[k p] (main-panels b)]
        (is (= (:w p) (:w ((margin-panels b :top) k))))))))

(deftest boundary-test
  (testing "one panel"
    (let [b (built (assoc wrap :scales {:facet {:domain ["q"]}} :margins {:left (chart/box) :bottom (chart/density)}))]
      (is (= ["q"] (keys (margin-panels b :left))))
      (is (= ["q"] (keys (get-in b [:margin-env :bottom :stats 0 :data]))))))
  (testing "NA panel: its margins too (panel key nil)"
    (let [rows (map #(if (= "r" (:s %)) (dissoc % :s) %) xy)
          b (built {:data rows :mapping {:x :a :y :b :facet :s} :charts [(chart/scatter)] :margins {:top (chart/density)}})]
      (is (contains? (margin-panels b :top) nil))
      (is (contains? (get-in b [:margin-env :top :stats 0 :data]) nil)))
    (let [rows (map #(if (= "r" (:s %)) (dissoc % :s) %) xy)
          b (built {:data rows :mapping {:x :a :y :b :facet :s} :facets {:scales {:x :free} :na-panel false}
                    :charts [(chart/scatter)] :margins {:top (chart/density)}})]
      (is (= #{"p" "q"} (set (keys (margin-panels b :top)))))))
  (testing "flip: the top margins share the horizontal scale (:y), per panel with free scales"
    (let [b (built (assoc wrap :flip true :facets {:scales {:y :free}} :margins {:top (chart/density)}))]
      (doseq [k ["p" "q" "r"]]
        (is (= (get-in b [:panel-scales k :y]) (get-in b [:margin-env :top :panel-scales k :x]))))))
  (testing "a margin chart with own data without facet values is drawn at every panel"
    (let [b (built (assoc wrap :margins {:top (chart/rug {:data [{:a 5.0} {:a 20.0}]})}))]
      (is (= [facet/all-panels] (keys (get-in b [:margin-env :top :stats 0 :data]))))))
  (testing "single panel unchanged: no panel key on margin content, no panel scales"
    (let [b (built {:data xy :mapping {:x :a :y :b} :charts [(chart/scatter)] :margins {:top (chart/density)}})]
      (is (= [nil] (keys (margin-panels b :top))))
      (is (not-any? #(contains? (:content %) :panel) (items-where b :margin)))
      (is (nil? (get-in b [:margin-env :top :panel-scales]))))))

(deftest legend-test
  (testing "a margin chart's own colour mapping gives a legend, its groups per panel"
    (let [b (built (assoc wrap :margins {:top (chart/density {:mapping {:color :t}})}))]
      (is (= 2 (count (get-in b [:margin-env :top :stats 0 :data "p" :curves]))) "grouped by :t")
      (is (contains? (:scale-specs b) :color))
      (is (seq (items-where b #(= :legends (:kind %))))))))

(deftest errors-test
  (testing "facets with margins build"
    (is (empty? (error-ins #(built (assoc wrap :margins {:top (chart/density)}))))))
  (testing "still refused"
    (is (= #{[:margins :right :axis]} (error-ins #(built (assoc wrap :margins {:right {:charts (chart/box) :axis true}})))))
    (is (= :margins (:key (cljplot2.test-util/error #(built (assoc wrap :mapping {:y :b :facet :s} :charts [(chart/density {:mapping {:x :b}})]
                                                                   :margins {:top (chart/density)}))))))
    (is (seq (error-ins #(built (assoc wrap :margins {:top "x"})))) "wrong type")))

(deftest estimated-size-test
  (testing "margins repeat at every panel column / row in the size estimate"
    (let [b (plot/build (assoc wrap :facets {:columns 3}) options)
          est (fn [margins] (plot-grid/estimated-panel-size b [] margins {:columns 3 :rows 1} true false))
          [w0 h0] (est {})
          [w1 h1] (est {:right {:size 50} :top {:size 30}})
          gap (double (get-in b [:theme :facet :gap]))]
      (is (< (abs (- (- (double w0) (double w1)) (+ 50 gap))) 1e-9) "the room of 3 margins split over 3 columns")
      (is (< (abs (- (- (double h0) (double h1)) (+ 30 gap))) 1e-9)))))

(deftest render-test
  (testing "every boundary renders"
    (doseq [p [(assoc wrap :margins {:top {:charts (chart/density) :axis true} :right {:charts (chart/histogram) :axis true}
                                     :bottom {:charts (chart/histogram) :axis true :size 40} :left {:charts (chart/density) :axis true}})
               (assoc wrap :facets {:scales {:x :free :y :free}} :margins {:left (chart/rug) :bottom (chart/box)})
               {:data xy :mapping {:x :a :y :b :facet-col :s :facet-row :t :color :k} :flip true :charts [(chart/scatter)]
                :facets {:scales {:x :free}} :margins {:top {:charts (chart/density) :axis true} :left (chart/violin)}}]]
      ;; 800 wide: four margins with axes keep their labels apart from the panel axes' (more fixed room per column)
      (is (instance? BufferedImage (plot/render-image p (assoc options :width 800)))))))
