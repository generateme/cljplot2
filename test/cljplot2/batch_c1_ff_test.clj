(ns cljplot2.batch-c1-ff-test
  "Batch C1 (ff): treemap `:order`, bar `:normalize` spanning exactly its proportions, bar `:label-position :base`."
  (:require [clojure.test :refer [deftest testing is]]
            [cljplot2.test-util :refer [error]]
            [cljplot2.chart :as chart]
            [cljplot2.chart.treemap :as treemap]
            [cljplot2.core :as plot])
  (:import [java.awt.image BufferedImage]))

(def ^:private options {:width 400 :height 300})

(defn- built [p] (plot/build p options))

(defn- pixels
  "The ARGB pixels of an image."
  [^BufferedImage img]
  (vec (.getRGB img 0 0 (.getWidth img) (.getHeight img) nil 0 (.getWidth img))))

(defn- image [p] (pixels (plot/render-image p options)))

;; 1. treemap :order

(def ^:private budget
  [{:dept "science" :team "physics" :cost 40} {:dept "science" :team "biology" :cost 25}
   {:dept "arts" :team "music" :cost 15} {:dept "arts" :team "painting" :cost 7}
   {:dept "sport" :team "tennis" :cost 8} {:dept "sport" :team "football" :cost 20}])

(defn- treemap-plot [tm-options]
  {:data budget :mapping {:hierarchy [:dept :team] :value :cost} :charts [(chart/treemap tm-options)]})

(deftest treemap-order-test
  (testing "defaults keep today's layouts: :value for :squarify, :data for :slice-dice"
    (is (= (image (treemap-plot {})) (image (treemap-plot {:order :value}))))
    (is (= (image (treemap-plot {:layout :slice-dice})) (image (treemap-plot {:layout :slice-dice :order :data}))))
    (is (= :value (get-in (built (treemap-plot {})) [:stats 0 :data :panel :options :order]))
        "computed option, default squarify")
    (is (= :data (get-in (built (treemap-plot {:layout :slice-dice})) [:stats 0 :data :panel :options :order]))))
  (testing "the other order changes the layout"
    (is (not= (image (treemap-plot {})) (image (treemap-plot {:order :data}))))
    (is (not= (image (treemap-plot {:layout :slice-dice})) (image (treemap-plot {:layout :slice-dice :order :value})))))
  (testing "children in :order"
    (let [node {:children [{:name "a" :value 1.0} {:name "b" :value 3.0} {:name "c" :value 2.0}]}
          content {:x 0.0 :y 0.0 :w 60.0 :h 10.0}
          names (fn [o] (map (comp :name first) (#'treemap/children-rects node content 0 o)))]
      (is (= ["b" "c" "a"] (names {:layout :slice-dice :order :value})))
      (is (= ["a" "b" "c"] (names {:layout :slice-dice :order :data})))
      (is (= ["a" "b" "c"] (names {:layout :squarify :order :data})))
      (is (= ["b" "c" "a"] (names {:layout :squarify :order :value})))
      (testing "slice-dice by value: widths follow the order"
        (is (= [30.0 20.0 10.0] (map (comp :w second) (#'treemap/children-rects node content 0 {:layout :slice-dice :order :value})))))
      (is (empty? (#'treemap/children-rects {:children []} content 0 {:layout :squarify :order :value})) "no children")
      (is (= ["a"] (map (comp :name first) (#'treemap/children-rects {:children [{:name "a" :value 1.0}]} content 0
                                                                       {:layout :squarify :order :data}))) "one child")))
  (testing "facets lay out in order too"
    (is (instance? BufferedImage (plot/render-image (assoc (treemap-plot {:order :data}) :mapping {:hierarchy [:team] :value :cost :facet :dept})
                                                    options))))
  (testing "wrong values"
    (is (= [:charts 0 :options :order] (:in (error #(built (treemap-plot {:order :size}))))))
    (is (= [:charts 0 :options :order] (:in (error #(built (treemap-plot {:order nil}))))) "not nilable, as partition")))

;; 2. :normalize spans exactly [0 1]

(def ^:private shares
  "Three levels of three groups whose shares don't sum to exactly 1 in doubles."
  (vec (for [l ["p" "q" "r"] [g v] [["a" 0.1] ["b" 0.2] ["c" 0.7]]] {:l l :g g :v (* v 3.3)})))

(defn- value-domain [p k] (get-in (built p) [:finalized-scales k :domain]))

(deftest normalize-domain-test
  (testing "horizontal and vertical bars: no padding, the domain ends at 1"
    (is (= [0.0 1.0] (value-domain {:data shares :mapping {:x :v :y :l :color :g} :charts [(chart/bar {:position :normalize})]} :x)))
    (is (= [0.0 1.0] (value-domain {:data shares :mapping {:x :l :y :v :color :g} :charts [(chart/bar {:position :normalize})]} :y))))
  (testing "one group per level"
    (is (= [0.0 1.0] (value-domain {:data [{:l "p" :v 2} {:l "q" :v 5}] :mapping {:x :v :y :l} :charts [(chart/bar {:position :normalize})]} :x))))
  (testing "mirrored: [-1 1]"
    (is (= [-1.0 1.0] (value-domain {:data (vec (for [l ["p" "q"] [g v] [["a" 1.1] ["b" 2.2] ["c" 0.3]] side ["l" "r"]] {:l l :g g :v v :side side}))
                                     :mapping {:x :v :y :l :color :g :group :side}
                                     :charts [(chart/bar {:position :normalize :mirror :group})]} :x))))
  (testing "facets"
    (is (= [0.0 1.0] (value-domain {:data (map #(assoc % :f (:l %)) shares) :mapping {:x :v :y :g :color :g :facet :f}
                                    :charts [(chart/bar {:position :normalize})]} :x))))
  (testing "explicit :padding still wins"
    (is (< 1.05 (double (second (value-domain {:data shares :mapping {:x :v :y :l :color :g}
                                              :charts [(chart/bar {:position :normalize :padding {:x [0 0.2]}})]} :x))))))
  (testing "end labels still get pixel room"
    (is (pos? (double (second (get-in (built {:data shares :mapping {:x :v :y :l :color :g}
                                              :charts [(chart/bar {:position :normalize :value-label true :label-position :end})]})
                                      [:stats 0 :padding-px :x]))))))
  (testing ":stack keeps its padding"
    (is (pos? (double (second (get-in (built {:data shares :mapping {:x :v :y :l :color :g} :charts [(chart/bar {:position :stack})]})
                                      [:scale-specs :x :padding])))))))

;; 3. :label-position :base

(defn- bar-labels [p]
  (keep :label (get-in (built p) [:stats 0 :data :panel :bars])))

(deftest base-label-test
  (testing "positive and negative bars: at the base (0), pointing into the bar"
    (let [ls (bar-labels {:data [{:l "p" :v 3} {:l "q" :v -2}] :mapping {:x :v :y :l}
                          :charts [(chart/bar {:value-label true :label-position :base})]})]
      (is (= #{["3" 0.0 1.0] ["-2" 0.0 -1.0]} (set (map (juxt :text :at :side) ls))))
      (is (every? :inside ls))))
  (testing "stacked segments: each at its own base"
    (is (= #{0.0 1.0} (set (map :at (bar-labels {:data [{:l "p" :g "a" :v 1} {:l "p" :g "b" :v 2}] :mapping {:x :v :y :l :color :g}
                                                 :charts [(chart/bar {:position :stack :value-label true :label-position :base})]}))))))
  (testing "zero-length bars: no label; one bar: one label"
    (is (empty? (bar-labels {:data [{:l "p" :v 0}] :mapping {:x :v :y :l} :charts [(chart/bar {:value-label true :label-position :base})]})))
    (is (= 1 (count (bar-labels {:data [{:l "p" :v 1}] :mapping {:x :v :y :l} :charts [(chart/bar {:value-label true :label-position :base})]})))))
  (testing "no room reserved past the bars"
    (is (nil? (get-in (built {:data [{:l "p" :v 3}] :mapping {:x :v :y :l}
                              :charts [(chart/bar {:value-label true :label-position :base})]})
                      [:stats 0 :padding-px]))))
  (testing "draws in cartesian (flipped too) and polar plots, and differs from :end"
    (doseq [p [{:data [{:l "p" :v 3} {:l "q" :v 5}] :mapping {:x :v :y :l}}
               {:data [{:l "p" :v 3} {:l "q" :v 5}] :mapping {:x :l :y :v}}
               {:data [{:l "p" :v 3} {:l "q" :v 5}] :mapping {:x :l :y :v} :coord {:kind :polar}}]]
      (is (instance? BufferedImage (plot/render-image (assoc p :charts [(chart/bar {:value-label true :label-position :base})]) options))))
    (is (not= (image {:data [{:l "p" :v 3} {:l "q" :v 5}] :mapping {:x :v :y :l} :charts [(chart/bar {:value-label true :label-position :base})]})
              (image {:data [{:l "p" :v 3} {:l "q" :v 5}] :mapping {:x :v :y :l} :charts [(chart/bar {:value-label true :label-position :end})]})))
    (doseq [p [{:data [{:l "p" :v 3} {:l "q" :v 5}] :mapping {:x :v :y :l}}
               {:data [{:l "p" :v 3} {:l "q" :v 5}] :mapping {:x :l :y :v}}]]
      (is (not= (image (assoc p :charts [(chart/bar {:value-label true :label-position :base})]))
                (image (assoc p :charts [(chart/bar {})])))
          "labels are drawn (the image differs from the bars without labels)")))
  (testing "a base label that doesn't fit its bar is skipped"
    (is (= (image {:data [{:l "p" :v 0.01} {:l "q" :v 5}] :mapping {:x :v :y :l}
                   :scales {:x {:domain [0 5]}} :charts [(chart/bar {:value-label #(when (< (:value %) 1) "a long label text") :label-position :base})]})
           (image {:data [{:l "p" :v 0.01} {:l "q" :v 5}] :mapping {:x :v :y :l}
                   :scales {:x {:domain [0 5]}} :charts [(chart/bar {})]}))))
  (testing "wrong value"
    (is (= [:charts 0 :options :label-position]
           (:in (error #(built {:data [{:l "p" :v 3}] :mapping {:x :v :y :l} :charts [(chart/bar {:label-position :inside})]})))))))
