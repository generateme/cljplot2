(ns cljplot2.batch-c2-fix-y4-test
  "Batch C2 review fixes (y4): `:equal-panels` rounds keep the best one, the inner track of left and bottom margins,
  row spans from the last row, one list of margin sides, pairs `:layout` section."
  (:require [clojure.test :refer [deftest testing is]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.content.axis :as axis]
            [cljplot2.content.legend :as legend]
            [cljplot2.describe :as describe]
            [cljplot2.layout.grid :as grid]
            [cljplot2.layout.plot-grid :as plot-grid]
            [cljplot2.multiplot :as multiplot]
            [notebook-utils :as nu]))

(def ^:private options {:width 500 :height 400})

(def ^:private rows
  (vec (for [i (range 60)] {:a (* 3.0 (Math/sin i)) :b (* 2.0 (Math/cos (* 0.7 i))) :g (if (even? i) "u" "v")})))

(defn- scatter-plot [margins]
  {:data rows :mapping {:x :a :y :b} :charts [(chart/scatter)] :margins margins})

(defn- ids-of [tracks] (mapv :id tracks))
(defn- index-of [v id] (.indexOf ^java.util.List v id))
(defn- item [built id] (some #(when (= id (:id %)) %) (:items (:grid built))))
(defn- box [built id] (grid/item-box (:layout built) id))

;; P5: :equal-panels rounds

(def ^:private panel-spread @#'cljplot2.pipeline/panel-spread)
(def ^:private corrected-tracks @#'cljplot2.pipeline/corrected-tracks)

(deftest panel-spread-test
  (is (zero? (panel-spread {})) "no panels")
  (is (zero? (panel-spread {:columns {0 [40]} :rows {0 [30]}})) "one panel")
  (is (zero? (panel-spread {:columns {0 [40 40] 1 [40]} :rows {0 [30] 1 [30]}})) "equal panels")
  (is (= 5 (panel-spread {:columns {0 [40] 1 [43]} :rows {0 [30] 1 [28]}})) "widths 3 + heights 2"))

(deftest corrected-tracks-test
  (testing "equal panels: nil"
    (is (nil? (corrected-tracks [[:fixed 50] [:fixed 50]] {0 [40] 1 [40]} 1.0))))
  (testing "a whole step corrects the whole difference to the mean, keeping the total"
    (is (= [[:fixed 53] [:fixed 47]] (corrected-tracks [[:fixed 50] [:fixed 50]] {0 [37] 1 [43]} 1.0))))
  (testing "a half step corrects half, rounded away from 0 - a pixel's difference always moves"
    (is (= [[:fixed 52] [:fixed 48]] (corrected-tracks [[:fixed 50] [:fixed 50]] {0 [37] 1 [43]} 0.5)))
    (is (= [[:fixed 51] [:fixed 49]] (corrected-tracks [[:fixed 50] [:fixed 50]] {0 [39] 1 [41]} 0.5))))
  (testing "fixed tracks without panels are kept"
    (is (= [[:fixed 51] [:fixed 20] [:fixed 49]] (corrected-tracks [[:fixed 50] [:fixed 20] [:fixed 50]] {0 [39] 2 [41]} 0.5)))))

(defn- arranged-spread [built]
  (let [sizes (keep (fn [[_ sub]] (grid/find-content-box (:grid sub) (:layout sub) #(= :panel (:kind %))))
                    (:subplots built))
        spread (fn [xs] (- (apply max xs) (apply min xs)))]
    (+ (spread (map :w sizes)) (spread (map :h sizes)))))

(deftest equal-panels-best-round-test
  (testing "Cars93, 18 variables: the rounds oscillate (the last of 3 was 13 px apart); the best is kept"
    (let [cars93 (nu/dataset :cars93)
          vs [:min-price :price :max-price :mpg-city :mpg-highway :engine-size :horsepower :rpm :rev-per-mile
              :fuel-tank-capacity :passengers :length :wheelbase :width :turn-circle :rear-seat-room :luggage-room :weight]
          b (plot/build (plot/pairs cars93 {:variables vs}) {:width 1000 :height 800})]
      (is (<= (arranged-spread b) 6))))
  (testing "a matrix whose rounds converge keeps equal panels"
    (let [b (plot/build (plot/pairs rows {:variables [:a :b]}) options)]
      (is (zero? (arranged-spread b))))))

;; P7 / R7: left and bottom margins

(deftest margin-inner-tracks-test
  (testing "without left / bottom margins: no inner track (grids unchanged)"
    (let [b (plot/build (scatter-plot {:top (chart/density) :right {:charts (chart/histogram) :axis true}}) options)]
      (is (not-any? #{:left-margin-inner :bottom-margin-inner}
                    (concat (ids-of (:columns (:grid b))) (ids-of (:rows (:grid b))))))
      (is (= :y-axis (:column (item b :x-axis-start))))
      (is (= :x-axis (:row (item b :right-margin-axis))))))
  (testing "left margin: a measured column between its gap and the panel, holding the x axis start overhang"
    (let [b (plot/build (scatter-plot {:left {:charts (chart/histogram) :axis true}}) options)
          cols (ids-of (:columns (:grid b)))]
      (is (< (index-of cols :left-margin) (index-of cols :left-margin-gap) (index-of cols :left-margin-inner) (index-of cols :panel)))
      (is (= :left-margin-inner (:column (item b :x-axis-start))))
      (is (= :x-axis (:row (item b :left-margin-axis))) "no bottom margin: the x-axis row")))
  (testing "bottom margin: a measured row between the panel and its gap, holding the y axis start overhang"
    (let [b (plot/build (scatter-plot {:bottom {:charts (chart/histogram) :axis true}}) options)
          rs (ids-of (:rows (:grid b)))]
      (is (< (index-of rs :panel) (index-of rs :bottom-margin-inner) (index-of rs :bottom-margin-gap) (index-of rs :bottom-margin)))
      (is (= :bottom-margin-inner (:row (item b :y-axis-start))))
      (is (= :y-axis (:column (item b :bottom-margin-axis))) "no left margin: the y-axis column")))
  (testing "both: every margin axis next to its own panel, the main axes' start labels apart from the margins'"
    (doseq [flip [false true]]
      (let [b (plot/build (assoc (scatter-plot {:top {:charts (chart/histogram) :axis true}
                                                :right {:charts (chart/histogram) :axis true}
                                                :bottom {:charts (chart/histogram) :axis true}
                                                :left {:charts (chart/histogram) :axis true}})
                                 :flip flip :scales {:x {:domain [-4 4]} :y {:domain [-3 3]}})
                          options)]
        (is (= :left-margin-inner (:column (item b :top-margin-axis))))
        (is (= :left-margin-inner (:column (item b :bottom-margin-axis))))
        (is (= :bottom-margin-inner (:row (item b :left-margin-axis))))
        (is (= :bottom-margin-inner (:row (item b :right-margin-axis))))
        (let [panel (box b :panel) bottom-axis (box b :bottom-margin-axis) left-axis (box b :left-margin-axis)
              left-margin (box b :left-margin)]
          (is (== (+ (:x bottom-axis) (:w bottom-axis)) (:x panel)) "the bottom margin's axis ends at the panel")
          (is (== (:y left-axis) (+ (:y panel) (:h panel))) "the left margin's axis starts under the panel")
          (is (pos? (:w (box b :x-axis-start))) "the main horizontal axis's start overhang is measured")
          (is (< (+ (:x left-margin) (:w left-margin)) (:x (box b :x-axis-start))) "... right of the left margin"))
        (is (some? (plot/render-image (assoc (scatter-plot {:bottom {:charts (chart/histogram) :axis true}
                                                             :left {:charts (chart/histogram) :axis true}})
                                             :flip flip)
                                      options)))))))

;; P13 / D13: row spans

(deftest row-span-from-last-row-test
  (testing "a span starting in the last row adds rows (shape from placement)"
    (is (= {:rows 3 :columns 2 :cells [{:row 0 :column 0 :span 1} {:row 0 :column 1 :span 1} {:row 1 :column 0 :span 2}]}
           (plot-grid/arrange-cells {:plots [{} {} {}] :columns 2 :row-spans {2 2}})))
    (is (= 4 (:rows (plot-grid/arrange-cells {:plots [{} {}] :columns 1 :row-spans {1 3}}))) "one column")
    (is (= 1 (:rows (plot-grid/arrange-cells {:plots [{}] :columns 1}))) "one plot, no spans"))
  (testing "builds"
    (let [cell {:data rows :mapping {:x :a :y :b} :charts [(chart/scatter)]}
          b (plot/build (plot/arrange [cell cell cell] {:columns 2 :row-spans {2 2}}) options)]
      (is (= 3 (count (filter #(and (vector? %) (= :row (first %))) (ids-of (:rows (:grid b)))))))))
  (testing "with :rows given, a span past them stays an error"
    (is (some? (#'plot-grid/arrange-problem {:plots [{} {} {}] :columns 2 :rows 2 :row-spans {2 2}}))))
  (testing "a nested two-column legend arrange under :measured: the message names legend cells"
    (is (re-find #"legend cells only"
                 (#'plot-grid/arrange-problem {:plots [{} {:arrange {:plots [{:legend-only true} {:legend-only true}] :columns 2}}]
                                               :columns 2 :widths [1 :measured]}))))
  (testing ":row-spans is keyed by plot index"
    (is (re-find #"plot index \(not cell\)"
                 (:description (some #(when (= :row-spans (:key %)) %) (describe/options plot-grid/arrange-schema)))))))

;; D15: one list of margin sides

(deftest margin-sides-test
  (is (= [:top :right :bottom :left] axis/margin-sides))
  (let [b (plot/build (scatter-plot {:left (chart/rug) :bottom (chart/density) :right (chart/rug) :top (chart/histogram)}) options)]
    (is (= [:scatter :histogram :rug :density :rug] (mapv :kind (:charts (legend/legend-charts b)))) "legend key order")
    (is (= axis/margin-sides (keys (:margin-stats b))) "margin stats in that order")))

;; D17: pairs sections

(deftest pairs-layout-section-test
  (let [sections (into {} (map (juxt :key :section)) (describe/options #'multiplot/pairs))]
    (is (= :layout (sections :title) (sections :legend-span) (sections :theme)))
    (is (= :cells (sections :lower) (sections :scales)))))
