(ns cljplot2.batch-c2-lm-test
  "Batch C2 (lm): bottom and left plot margins, arrange `:row-spans` (a cell spanning rows) and clustered heatmap
  annotations."
  (:require [clojure.test :refer [deftest testing is]]
            [cljplot2.test-util :refer [error]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.content.legend :as legend]
            [cljplot2.layout.grid :as grid]
            [cljplot2.layout.plot-grid :as plot-grid])
  (:import [java.awt.image BufferedImage]))

(def ^:private options {:width 500 :height 400})

(def ^:private rows
  (vec (for [i (range 60)] {:a (Math/sin i) :b (* 2.0 (Math/cos (* 0.7 i))) :g (if (even? i) "u" "v")})))

(defn- scatter-plot [margins]
  {:data rows :mapping {:x :a :y :b :color :g} :charts [(chart/scatter)] :margins margins})

(defn- box-of [built pred]
  (grid/find-content-box (:grid built) (:layout built) pred))

(defn- main-panel [built] (box-of built #(and (= :panel (:kind %)) (nil? (:margin %)))))
(defn- margin-panel [built side] (box-of built #(and (= :panel (:kind %)) (= side (:margin %)))))
(defn- axis-box [built k] (box-of built #(and (= :axis (:kind %)) (= k (:scale %)) (nil? (:margin %)))))

;; 1. bottom and left margins

(deftest bottom-left-margin-placement-test
  (testing "bottom: below the panel, above its horizontal axis; same width as the panel"
    (let [b (plot/build (scatter-plot {:bottom (chart/density)}) options)
          p (main-panel b) m (margin-panel b :bottom) ax (axis-box b :x)]
      (is (= [(:x p) (:w p)] [(:x m) (:w m)]))
      (is (< (+ (:y p) (:h p)) (:y m)))
      (is (<= (+ (:y m) (:h m)) (:y ax)) "the main panel keeps its axis on the outer side")
      (is (= 80 (:h m)) "default thickness")))
  (testing "left: right of the vertical axis, left of the panel; same height as the panel"
    (let [b (plot/build (scatter-plot {:left {:charts (chart/density) :size 50}}) options)
          p (main-panel b) m (margin-panel b :left) ax (axis-box b :y)]
      (is (= [(:y p) (:h p)] [(:y m) (:h m)]))
      (is (< (+ (:x m) (:w m)) (:x p)))
      (is (<= (+ (:x ax) (:w ax)) (:x m)))
      (is (= 50 (:w m)))))
  (testing "all four sides at once"
    (let [b (plot/build (scatter-plot {:top (chart/density) :right (chart/density) :bottom (chart/rug) :left (chart/rug)}) options)]
      (is (every? #(some? (margin-panel b %)) [:top :right :bottom :left]))
      (is (instance? BufferedImage (plot/render-image (scatter-plot {:top (chart/density) :right (chart/density)
                                                                     :bottom (chart/rug) :left (chart/rug)})
                                                      options))))))

(deftest bottom-left-margin-scales-test
  (testing "the magnitude grows away from the panel: bottom downwards, left leftwards"
    (let [b (plot/build (scatter-plot {:bottom (chart/histogram) :left (chart/histogram)}) options)
          bottom (get-in b [:margin-env :bottom :scales :y])
          left (get-in b [:margin-env :left :scales :y])
          [b0 b1] (:range bottom) [l0 l1] (:range left)]
      (is (< b0 b1))
      (is (> l0 l1))
      (is (= :vertical (:direction bottom)))
      (is (= :horizontal (:direction left)))))
  (testing "shared scales: bottom the horizontal one, left the vertical one; flipped they swap dimensions"
    (let [b (plot/build (scatter-plot {:bottom (chart/rug) :left (chart/rug)}) options)]
      (is (= (get-in b [:finalized-scales :x :range]) (get-in b [:margin-env :bottom :scales :x :range])))
      (is (= (get-in b [:finalized-scales :y :range]) (get-in b [:margin-env :left :scales :x :range]))))
    (let [b (plot/build (assoc (scatter-plot {:bottom (chart/rug) :left (chart/rug)}) :flip true) options)]
      (is (= (get-in b [:finalized-scales :y :range]) (get-in b [:margin-env :bottom :scales :x :range])))
      (is (= (get-in b [:finalized-scales :x :range]) (get-in b [:margin-env :left :scales :x :range]))))))

(deftest bottom-left-margin-axis-test
  (testing ":axis true: the magnitude axis (y-axis column for bottom, x-axis row for left) and its end overhang"
    (let [b (plot/build (scatter-plot {:bottom {:charts (chart/histogram) :axis true}
                                       :left {:charts (chart/density) :axis true}})
                        options)
          ids (set (map :id (:items (:grid b))))]
      (is (every? ids [:bottom-margin-axis :bottom-margin-axis-end :left-margin-axis :left-margin-axis-end]))
      (is (some #{:bottom-margin-overhang} (map :id (:rows (:grid b)))))
      (is (some #{:left-margin-overhang} (map :id (:columns (:grid b)))))))
  (testing "track order: bottom margin between the panel and the x axis, left between the y axis and the panel"
    (let [b (plot/build (scatter-plot {:bottom (chart/rug) :left (chart/rug)}) options)
          rows-ids (mapv :id (:rows (:grid b)))
          col-ids (mapv :id (:columns (:grid b)))
          at (fn [v id] (.indexOf ^java.util.List v id))]
      (is (< (at rows-ids :panel) (at rows-ids :bottom-margin-gap) (at rows-ids :bottom-margin) (at rows-ids :x-axis)))
      (is (< (at col-ids :y-axis) (at col-ids :left-margin) (at col-ids :left-margin-gap) (at col-ids :panel))))))

(deftest bottom-left-margin-errors-test
  (testing "the plot must map the shared dimension"
    (is (= :margins (:key (error #(plot/build {:data rows :mapping {:x :a} :charts [(chart/rug)] :margins {:left (chart/rug)}} options))))))
  (testing "a wrong value, an unknown side"
    (is (= [:margins :bottom] (:in (error #(plot/build (scatter-plot {:bottom 3}) options)))))
    (is (= [:margins :below] (:in (error #(plot/build (scatter-plot {:below (chart/rug)}) options))))))
  (testing "not with facets or polar coordinates"
    (is (some? (error #(plot/build (assoc-in (scatter-plot {:left (chart/rug)}) [:mapping :facet] :g) options))))
    (is (= :coord (:key (error #(plot/build (assoc (scatter-plot {:bottom (chart/rug)}) :coord {:kind :polar}) options)))))))

(deftest bottom-left-margin-legend-test
  (testing "margin charts keep their legend keys; old sides first (key indexes unchanged)"
    (let [b (plot/build (scatter-plot {:top (chart/density) :bottom (chart/rug)}) options)]
      (is (= [:scatter :density :rug] (mapv :kind (:charts (legend/legend-charts b))))))))

;; 2. arrange :row-spans

(deftest arrange-cells-test
  (testing "no spans: row by row (as before)"
    (is (= {:rows 2 :columns 2 :cells [{:row 0 :column 0 :span 1} {:row 0 :column 1 :span 1} {:row 1 :column 0 :span 1}]}
           (plot-grid/arrange-cells {:plots [{} {} {}] :columns 2}))))
  (testing "nil and empty spans are no spans"
    (is (= (plot-grid/arrange-cells {:plots [{} {}] :columns 2})
           (plot-grid/arrange-cells {:plots [{} {}] :columns 2 :row-spans nil})
           (plot-grid/arrange-cells {:plots [{} {}] :columns 2 :row-spans {}}))))
  (testing "a span covers the cells below; later plots skip them"
    (is (= [{:row 0 :column 0 :span 1} {:row 0 :column 1 :span 3} {:row 1 :column 0 :span 1} {:row 2 :column 0 :span 1}]
           (:cells (plot-grid/arrange-cells {:plots [{} {} {} {}] :columns 2 :row-spans {1 3}}))))
    (is (= 3 (:rows (plot-grid/arrange-cells {:plots [{} {} {} {}] :columns 2 :row-spans {1 3}})))))
  (testing "the grid item spans the rows"
    (let [g (plot-grid/arrange-grid {:plots [{} {} {}] :columns 2 :row-spans {1 2}} nil {:multiplot {:gap 5} :plot {:margin 3}})]
      (is (= {:from [:row 0] :to [:row 1]} (:row (second (:items g)))))
      (is (= [:row 1] (:row (nth (:items g) 2)))))))

(deftest arrange-row-spans-problems-test
  (let [problem @#'plot-grid/arrange-problem]
    (is (nil? (problem {:plots [{} {:legend-only true} {}] :columns 2 :row-spans {1 2} :widths [1 :measured]})))
    (is (some? (problem {:plots [{} {} {}] :columns 2 :row-spans {5 2}})) "a plot index past the last plot")
    (is (some? (problem {:plots [{} {} {} {}] :columns 2 :rows 2 :row-spans {3 2}})) "past the last row")
    (is (some? (problem {:plots [{} {} {}] :columns 2 :rows 1 :row-spans {0 2}})) "too small for the covered cells")
    (is (some? (problem {:plots [{} {} {}] :columns 2 :row-spans {1 2} :heights [1]})) "heights per row of the spanned shape")))

(deftest arrange-row-spans-build-test
  (let [cell {:data rows :mapping {:x :a :y :b :color :b} :charts [(chart/scatter)] :legend false}
        legend {:data rows :mapping {:x :a :y :b :color :b} :charts [(chart/scatter)] :legend-only true}
        p (plot/arrange [cell legend cell] {:columns 2 :row-spans {1 2} :widths [1 :measured]})
        b (plot/build p options)
        boxes (into {} (map (juxt :id :box)) (grid/content-items (:grid b) (:layout b) #(= :plot (:kind %))))]
    (testing "the spanning cell is as tall as both rows"
      (is (= (+ (:y (boxes [:plot 2])) (:h (boxes [:plot 2])))
             (+ (:y (boxes [:plot 1])) (:h (boxes [:plot 1])))))
      (is (= (:y (boxes [:plot 0])) (:y (boxes [:plot 1])))))
    (testing ":equal-panels with a spanning cell"
      (is (instance? BufferedImage (plot/render-image (plot/arrange [cell legend cell] {:columns 2 :row-spans {1 2} :widths [1 :measured]
                                                                                        :equal-panels true})
                                                      options))))
    (testing "wrong types: keyed :row-spans (options) / :arrange (plot)"
      (is (= :row-spans (:key (error #(plot/arrange [cell cell] {:row-spans {0 0}})))))
      (is (= :row-spans (:key (error #(plot/arrange [cell cell] {:row-spans [1 2]})))))
      (is (= :arrange (:key (error #(plot/build {:arrange {:plots [cell cell] :row-spans {4 2}}} options))))))))

(deftest legend-cell-test
  (is (plot-grid/legend-cell? {:legend-only true}))
  (is (plot-grid/legend-cell? {:arrange {:plots [{:legend-only true} {:legend-only true}]}}))
  (is (plot-grid/legend-cell? {:arrange {:plots [{:legend-only true}] :columns 1}}))
  (is (not (plot-grid/legend-cell? {:arrange {:plots [{:legend-only true} {:legend-only true}] :columns 2}})))
  (is (not (plot-grid/legend-cell? {:arrange {:plots [{:legend-only true} {}]}})))
  (is (not (plot-grid/legend-cell? {}))))

;; 3. clustered heatmap annotations

(def ^:private cells
  (vec (for [[i r] (map-indexed vector ["r1" "r2" "r3" "r4"]) [j c] (map-indexed vector ["c1" "c2" "c3"])]
         {:r r :c c :v (+ (* i 1.3) (* j j 0.7) (mod (* i j 7) 3))
          :group (if (#{"r1" "r2"} r) "g1" "g2") :kind (if (= c "c1") "k1" "k2")})))

(def ^:private heat {:row :r :col :c :value :v})

(deftest heatmap-annotations-test
  (testing "none, nil or empty: the plain layout (no spans)"
    (let [plain (plot/clustered-heatmap cells heat)]
      (is (nil? (get-in plain [:arrange :row-spans])))
      (is (= plain (plot/clustered-heatmap cells (assoc heat :row-annotations nil :col-annotations []))))))
  (testing "one row annotation: a strip column beside the tiles, one legend cell spanning every row"
    (let [p (plot/clustered-heatmap cells (assoc heat :row-annotations [[:group nil]]))
          {:keys [columns widths heights row-spans plots]} (:arrange p)]
      (is (= 4 columns) "tiles, strip, row tree, legend")
      (is (= [:fixed 16] (second widths)))
      (is (= :measured (peek widths)))
      (is (= 2 (count heights)))
      (is (= {3 2} row-spans))
      (is (= 2 (count (get-in (nth plots 3) [:arrange :plots]))) "the value legend and the annotation's")
      (is (instance? BufferedImage (plot/render-image p options)))))
  (testing "many annotations on both sides, no trees, a size"
    (let [p (plot/clustered-heatmap cells (assoc heat :row-tree false :col-tree false :annotation-size 10
                                                 :row-annotations [[:group {:range :set2}] [(constantly "x") nil]]
                                                 :col-annotations [[:kind {:title "kind"}]]))
          {:keys [columns widths heights row-spans]} (:arrange p)]
      (is (= 4 columns))
      (is (= [:fixed 10] (nth widths 1) (nth widths 2)))
      (is (= [[:fixed 10] 1.0] heights))
      (is (= {3 2} row-spans))
      (is (instance? BufferedImage (plot/render-image p options)))))
  (testing "strips follow the clustered order of the tiles"
    (let [p (plot/clustered-heatmap cells (assoc heat :col-annotations [[:kind nil]]))
          tiles (nth (get-in p [:arrange :plots]) 5)
          strip (nth (get-in p [:arrange :plots]) 3)]
      (is (= (get-in tiles [:scales :x :domain]) (get-in strip [:scales :x :domain])))))
  (testing "errors: rows of one level disagree; wrong types; size not positive"
    (is (= :row-annotations (:key (error #(plot/clustered-heatmap cells (assoc heat :row-annotations [[:kind nil]]))))))
    (is (= :col-annotations (:key (error #(plot/clustered-heatmap cells (assoc heat :col-annotations [[:group nil]]))))))
    (is (= :row-annotations (:key (error #(plot/clustered-heatmap cells (assoc heat :row-annotations [:group]))))))
    (is (= :col-annotations (first (:in (error #(plot/clustered-heatmap cells (assoc heat :col-annotations [[:kind 3]])))))))
    (is (= :annotation-size (:key (error #(plot/clustered-heatmap cells (assoc heat :annotation-size 0)))))))
  (testing "an invalid colour scale spec is reported by the builder, keyed by the option"
    (is (= :row-annotations (first (:in (error #(plot/clustered-heatmap cells (assoc heat :row-annotations [[:group {:range 3}]])))))))))
