(ns cljplot2.batch-c1-fa-test
  "Batch C1 (fa): text `:select-within` (one end label per group) and `[:panel f]` text positions on an axis without
  a scale."
  (:require [clojure.test :refer [deftest testing is]]
            [cljplot2.test-util :refer [error error-ins]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.facet :as facet])
  (:import [java.awt.image BufferedImage]))

(def ^:private options {:width 400 :height 300})
(def ^:private red-theme {:text {:annotation {:color [255 0 0]}}})

(defn- built [p] (plot/build p options))

;; 1. :select-within

(def ^:private series
  "Two groups, each with labels changing along x (a decade label per row)."
  (vec (for [g ["a" "b"] i (range 6)] {:g g :x i :y (if (= g "a") i (- 10 i)) :t (str g (quot i 3))})))

(defn- labels
  "Labels of every panel of the text chart (chart index `i`) of plot `p`, as `[group-label text x]` sets."
  [p i]
  (set (for [[_ panel] (get-in (built p) [:stats i :data])
             {:keys [labels]} (:groups panel)
             l labels]
         [(:text l) (:x l)])))

(defn- text-plot [text-options]
  {:data series :mapping {:x :x :y :y :group :g}
   :charts [(chart/line) (chart/text (merge {:mapping {:x :x :y :y :label :t :group :g}} text-options))]})

(deftest select-within-test
  (testing "default and :group-and-label: one label per group and text"
    (is (= #{["a0" 2] ["a1" 5] ["b0" 2] ["b1" 5]} (labels (text-plot {:select :last}) 1)))
    (is (= (labels (text-plot {:select :last}) 1) (labels (text-plot {:select :last :select-within :group-and-label}) 1)))
    (is (= (labels (text-plot {:select :last}) 1) (labels (text-plot {:select :last :select-within nil}) 1)) "nil - the default"))
  (testing ":group: one label per group, whatever its text"
    (is (= #{["a1" 5] ["b1" 5]} (labels (text-plot {:select :last :select-within :group}) 1)))
    (is (= #{["a0" 0] ["b0" 0]} (labels (text-plot {:select :first :select-within :group}) 1))))
  (testing "zero rows and one row"
    (is (= #{} (labels {:data series :mapping {:x :x :y :y}
                        :charts [(chart/line) (chart/text {:data [] :mapping {:x :x :y :y :label :t} :select :last :select-within :group})]} 1)))
    (is (= #{["z" 3]} (labels {:data series :mapping {:x :x :y :y}
                               :charts [(chart/line) (chart/text {:data [{:x 3 :y 1 :t "z"}] :mapping {:x :x :y :y :label :t}
                                                                  :select :last :select-within :group})]} 1))))
  (testing "no groups: one label of all rows"
    (is (= #{["b1" 5]} (labels {:data series :mapping {:x :x :y :y}
                                :charts [(chart/text {:mapping {:x :x :y :y :label :t} :select :last :select-within :group})]} 0))))
  (testing "facets: one label per group in each panel"
    (let [b (built (assoc-in (text-plot {:select :last :select-within :group}) [:mapping :facet] :g))]
      (is (= {"a" ["a1"] "b" ["b1"]}
             (into {} (for [[k panel] (get-in b [:stats 1 :data]) :when (not= k facet/all-panels)]
                        [k (vec (map :text (mapcat :labels (:groups panel))))]))))))
  (testing "flip: the same labels"
    (is (= #{["a1" 5] ["b1" 5]} (labels (assoc (text-plot {:select :last :select-within :group}) :flip true) 1))))
  (testing "errors"
    (is (contains? (set (map last (error-ins #(built (text-plot {:select-within :group}))))) :select-within)
        "only with :select")
    (is (map? (built (text-plot {:select-within :group-and-label}))) "the default given explicitly is fine without :select")
    (is (some? (error #(built (text-plot {:select :last :select-within :label})))) "an unknown value")
    (is (some? (error #(built (text-plot {:select :last :select-within "group"})))) "a wrong type")))

;; 2. [:panel f] without a scale

(defn- red-pixels
  "`[x y]` of the red pixels of the rendered plot `p`."
  [p]
  (let [^BufferedImage img (plot/render-image p options)]
    (vec (for [i (range (.getWidth img)) j (range (.getHeight img))
               :let [rgb (.getRGB img (int i) (int j))
                     r (bit-and (bit-shift-right rgb 16) 0xff) g (bit-and (bit-shift-right rgb 8) 0xff) b (bit-and rgb 0xff)]
               :when (and (> r 180) (< g 90) (< b 90))]
           [i j]))))

(defn- mean [xs] (/ (double (reduce + xs)) (count xs)))

(def ^:private dots (mapv (fn [i] {:v (mod (* i 7) 13) :f (if (even? i) "p" "q")}) (range 40)))

(deftest panel-position-without-scale-test
  (testing "a dot plot (no y scale): [:panel f] on y is drawn"
    (let [p {:data dots :mapping {:x :v} :theme red-theme
             :charts [(chart/dotplot) (chart/text {:x [:panel 0.5] :y [:panel 0.9] :text "label"})]}
          ps (red-pixels p)]
      (is (nil? (get-in (built p) [:finalized-scales :y])) "no y scale")
      (is (seq ps))
      (is (< (mean (map second ps)) 80.0) "near the top")
      (is (< 150.0 (mean (map first ps)) 250.0) "in the middle")))
  (testing "f 0 and 1: the panel's ends"
    (let [at (fn [f] (mean (map second (red-pixels {:data dots :mapping {:x :v} :theme red-theme
                                                     :charts [(chart/dotplot) (chart/text {:x [:panel 0.5] :y [:panel f] :text "x"
                                                                                           :anchor [:center :middle]})]}))))]
      (is (> (at 0.0) (at 1.0)))))
  (testing "flipped: y runs horizontally"
    (let [ps (red-pixels {:data dots :mapping {:x :v} :flip true :theme red-theme
                          :charts [(chart/dotplot) (chart/text {:x [:panel 0.5] :y [:panel 0.9] :text "label"})]})]
      (is (seq ps))
      (is (> (mean (map first ps)) 300.0) "near the right")))
  (testing "a text chart alone: no scales at all"
    (let [ps (red-pixels {:theme red-theme :charts [(chart/text {:x [:panel 0.1] :y [:panel 0.1] :text "corner"})]})]
      (is (seq ps))
      (is (< (mean (map first ps)) 120.0))
      (is (> (mean (map second ps)) 200.0))))
  (testing "a data x with a panel y"
    (is (seq (red-pixels {:data dots :mapping {:x :v} :theme red-theme
                          :charts [(chart/dotplot) (chart/text {:x 3 :y [:panel 0.5] :text "here"})]}))))
  (testing "facets: in every panel"
    (let [ps (red-pixels {:data dots :mapping {:x :v :facet :f} :theme red-theme
                          :charts [(chart/dotplot) (chart/text {:x [:panel 0.5] :y [:panel 0.9] :text "label"})]})]
      (is (seq (filter #(< (first %) 200) ps)))
      (is (seq (filter #(> (first %) 200) ps)))))
  (testing "polar: drawn in the panel's circle"
    (is (seq (red-pixels {:data dots :mapping {:x :v} :coord {:kind :polar} :theme red-theme
                          :charts [(chart/rule {:x 3}) (chart/text {:x [:panel 0.25] :y [:panel 0.5] :text "label"})]})))))
