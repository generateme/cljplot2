(ns cljplot2.multiplot-test
  "Step 9: plot content, :subplots stage, sub-plot rendering, arrange, overall title."
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure2d.core :as c2d]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.grid :as grid])
  (:import [java.awt.image BufferedImage]))

(defn- error-data [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn- argb [canvas x y] (.getRGB ^BufferedImage (c2d/get-image canvas) (int x) (int y)))

(def ^:private rows (vec (for [i (range 30)] {:x i :y (mod (* i 7) 11) :g (["a" "b"] (mod i 2))})))
(def ^:private p1 {:data rows :mapping {:x :x :y :y} :charts [(chart/scatter)] :labels {:title "one"}})
(def ^:private p2 {:data rows :mapping {:x :x :y :y :color :g} :charts [(chart/line)]
                   :theme {:plot {:background [255 255 255]}}})

(defn- box-of [built id] (grid/find-item-box (:layout built) id))

;; 1.x, 2.x

(deftest custom-grid-plot-items-test
  (let [mp {:custom-grid {:columns [{:id :a :size [:flexible]} {:id :b :size [:flexible]}]
                          :rows [{:size [:flexible]}]
                          :items [{:id [:plot 0] :column :a :row 0 :content {:kind :plot :plot p1}}
                                  {:id [:plot 1] :column :b :row 0 :content {:kind :plot :plot p2}}]}}
        built (plot/build mp {:width 800 :height 300})]
    (testing "every plot item built at its box size"
      (is (= #{[:plot 0] [:plot 1]} (set (keys (:subplots built)))))
      (doseq [id [[:plot 0] [:plot 1]]
              :let [{:keys [w h]} (box-of built id)]]
        (is (= [w h] ((juxt :width :height) (get-in built [:subplots id :options]))))))
    (testing "a sub-plot builds like a standalone plot of its cell size (own theme, outer hint)"
      (let [{:keys [w h]} (box-of built [:plot 1])
            alone (plot/build p2 {:width w :height h})
            sub (get-in built [:subplots [:plot 1]])]
        (is (= (:finalized-scales alone) (:finalized-scales sub)))
        (is (= (:layout alone) (:layout sub)))
        (is (= (:stats alone) (:stats sub)))
        (is (= (:theme alone) (:theme sub)) "no theme inheritance")))
    (testing "the outer hint is passed down"
      (is (= :low (get-in (plot/build mp {:width 800 :height 300 :hint :low}) [:subplots [:plot 0] :options :hint]))))))

(deftest subplots-stage-test
  (testing "plain plot: empty :subplots"
    (is (= {} (:subplots (plot/build p1)))))
  (testing "nested: a sub-plot can be a multiplot, built at its own cell sizes"
    (let [inner (plot/arrange [p1 p2] {:columns 2})
          built (plot/build (plot/arrange [inner p1]) {:width 600 :height 600})
          inner-built (get-in built [:subplots [:plot 0]])]
      (is (= #{[:plot 0] [:plot 1]} (set (keys (:subplots inner-built)))))
      (is (= (:w (box-of inner-built [:plot 1])) (get-in inner-built [:subplots [:plot 1] :options :width])))
      (is (some? (plot/render (plot/arrange [inner p1]) {:width 600 :height 600})))))
  (testing "plot items inside nested grids are found"
    (let [mp {:custom-grid {:columns [{:size [:flexible]}] :rows [{:size [:flexible]}]
                            :items [{:id :wrap :column 0 :row 0
                                     :content {:kind :grid :columns [{:size [:flexible]}] :rows [{:size [:flexible]}]
                                               :items [{:id :deep :column 0 :row 0 :content {:kind :plot :plot p1}}]}}]}}]
      (is (= [:deep] (keys (:subplots (plot/build mp)))))))
  (testing "duplicate plot item ids across nested grids"
    (let [item {:id :same :column 0 :row 0 :content {:kind :plot :plot p1}}
          mp {:custom-grid {:columns [{:size [:flexible]} {:size [:flexible]}] :rows [{:size [:flexible]}]
                            :items [{:id :wrap :column 0 :row 0
                                     :content {:kind :grid :columns [{:size [:flexible]}] :rows [{:size [:flexible]}] :items [item]}}
                                    (assoc item :column 1)]}}]
      (is (= {:key :id :value :same} (error-data #(plot/build mp))))))
  (testing ":plot must be a map"
    (is (= {:key :plot} (error-data #(plot/build {:custom-grid {:columns [{:size [:flexible]}] :rows [{:size [:flexible]}]
                                                                :items [{:id :p :column 0 :row 0 :content {:kind :plot :plot [1]}}]}})))))
  (testing "errors carry the sub-plot id and path"
    (let [bad (assoc p1 :mapping {:x :x :y :nope})
          data (error-data #(plot/build (plot/arrange [p1 bad])))]
      (is (= [:mapping :y [:plot 1] [[:plot 1]]] ((juxt :key :value :subplot :subplot-path) data))))
    (let [bad (assoc p1 :mapping {:x :x :y :nope})
          data (error-data #(plot/build (plot/arrange [p1 (plot/arrange [bad p1])])))]
      (is (= [[:plot 1] [[:plot 1] [:plot 0]]] ((juxt :subplot :subplot-path) data)) "outer id first")))
  (testing "a cell under 1x1 px: no sub-plot, no error"
    (let [mp {:custom-grid {:columns [{:id :zero :size [:fixed 0]} {:size [:flexible]}] :rows [{:size [:flexible]}]
                            :items [{:id :tiny :column :zero :row 0 :content {:kind :plot :plot p1}}
                                    {:id :big :column 1 :row 0 :content {:kind :plot :plot p1}}]}}]
      (is (= [:big] (keys (:subplots (plot/build mp)))))
      (is (some? (plot/render mp))))))

;; 3.x rendering

(deftest subplot-pixels-test
  (testing "sub-plot pixels equal the standalone render, offset by the box"
    (let [mp (plot/arrange [p1 p2] {:columns 2 :align false})
          built (plot/build mp {:width 700 :height 300})
          c (plot/render mp {:width 700 :height 300})]
      (doseq [id [[:plot 0] [:plot 1]]
              :let [{:keys [x y w h]} (box-of built id)
                    alone (plot/render (get-in mp [:arrange :plots (second id)]) {:width w :height h})]]
        (is (every? (fn [[px py]] (= (argb alone px py) (argb c (+ x px) (+ y py))))
                    (for [px (range w) py (range h)] [px py]))
            (str id)))))
  (testing "sub-plot ink stays in its box (clipped)"
    (let [wide {:data rows :mapping {:x :x :y :y} :charts [(chart/scatter {:size 60 :color [255 0 0]})]
                :theme {:plot {:background :none}}}
          mp (plot/arrange [wide {:theme {:plot {:background [0 255 0]}}}] {:columns 2 :theme {:plot {:background [0 0 255]}}})
          built (plot/build mp {:width 600 :height 300})
          {:keys [x w]} (box-of built [:plot 1])
          c (plot/render mp {:width 600 :height 300})]
      (is (not-any? #(= 0xffff0000 (bit-or 0xff000000 (argb c % 150))) (range x (+ x w))) "no red in the second cell")))
  (testing ":debug recurses"
    (is (some? (plot/render (plot/arrange [p1 (plot/arrange [p1 p2])]) {:width 600 :height 400 :debug true})))))

;; 4.x arrange, title

(deftest arrange-test
  (let [b (fn [mp] (plot/build mp {:width 600 :height 600}))]
    (testing "default: one column; cells filled row by row"
      (let [built (b (plot/arrange [p1 p2]))
            [a c] [(box-of built [:plot 0]) (box-of built [:plot 1])]]
        (is (= (:x a) (:x c)))
        (is (< (:y a) (:y c)))
        (is (<= (Math/abs (- (long (:h a)) (long (:h c)))) 1) "equal heights")
        (is (= (+ (:y a) (:h a) 10) (:y c)) "theme gap 10")))
    (testing "columns / rows / shape"
      (let [built (b (plot/arrange [p1 p2 p1] {:columns 2}))]
        (is (= (:y (box-of built [:plot 0])) (:y (box-of built [:plot 1]))))
        (is (> (:y (box-of built [:plot 2])) (:y (box-of built [:plot 0])))))
      (let [built (b (plot/arrange [p1 p2 p1] {:rows 1}))]
        (is (= 1 (count (distinct (map #(:y (box-of built [:plot %])) (range 3)))))))
      (are [plots opts] (= :arrange (:key (error-data #(plot/build (plot/arrange plots opts)))))
        [] {}
        nil {}
        [p1 p2 p1] {:columns 1 :rows 2}
        [p1] {:columns 0}
        [p1] {:rows 1.5}
        [p1 p2] {:columns 2 :widths [1]}
        [p1 p2] {:heights [1 -1]}))
    (testing "weights"
      (let [built (b (plot/arrange [p1 p2] {:heights [2 1]}))
            ha (:h (box-of built [:plot 0])) hc (:h (box-of built [:plot 1]))]
        (is (<= (Math/abs (- (long ha) (* 2 (long hc)))) 1))))
    (testing "gap from the multiplot theme; validated"
      (let [built (b (plot/arrange [p1 p2] {:theme {:multiplot {:gap 40}}}))]
        (is (= (+ (:y (box-of built [:plot 0])) (:h (box-of built [:plot 0])) 40) (:y (box-of built [:plot 1])))))
      (is (= :gap (:key (error-data #(plot/build (plot/arrange [p1] {:theme {:multiplot {:gap -1}}})))))))
    (testing "the plot's :theme at build time controls gap and margin (not frozen by arrange)"
      (let [built (b (assoc (plot/arrange [p1 p2]) :theme {:multiplot {:gap 50} :plot {:margin 30}}))
            a (box-of built [:plot 0]) c (box-of built [:plot 1])]
        (is (= 50 (- (:y c) (+ (:y a) (:h a)))))
        (is (= 30 (:x a)))))
    (testing ":arrange spec shape"
      (is (= {:arrange {:plots [p1] :columns 1 :widths [1]} :labels {:title "T"} :theme {:x 1}}
             (plot/arrange [p1] {:columns 1 :widths [1] :title "T" :theme {:x 1} :ignored 2})))
      (is (= :arrange (:key (error-data #(plot/build {:arrange [p1]})))))
      (is (= :arrange (:key (error-data #(plot/build (assoc (plot/arrange [p1]) :custom-grid {:columns [{:size [:flexible]}] :rows [{:size [:flexible]}] :items []}))))))
      (is (= {:key :labels :value :x} (error-data #(plot/build (assoc (plot/arrange [p1]) :labels {:x "x"}))))))))

(deftest overall-title-test
  (let [mp (plot/arrange [p1 p2] {:columns 2 :title "Both" :theme {:plot {:background [255 255 255]}}})
        built (plot/build mp {:width 600 :height 300})
        title (grid/find-item-box (:layout built) :title)
        cells [(box-of built [:plot 0]) (box-of built [:plot 1])]]
    (is (some? title))
    (is (every? #(>= (:y %) (+ (:y title) (:h title))) cells) "title above every cell")
    (is (= "Both" (some #(when (= :title (:id %)) (get-in % [:content :text])) (get-in built [:grid :items]))))
    (testing "no title: plain custom grid"
      (is (nil? (grid/find-item-box (:layout (plot/build (plot/arrange [p1]))) :title))))))

;; review fixes

(deftest title-inside-margins-test
  (let [built (plot/build (plot/arrange [p1 p2] {:columns 2 :title "T"}) {:width 600 :height 300})
        title (grid/find-item-box (:layout built) :title)
        cell (box-of built [:plot 0])]
    (is (= 10 (:y title)) "one top margin above the title")
    (is (= (:x cell) (:x title)) "title within the side margins")
    (is (= (+ (:y title) (:h title)) (:y cell)) "cells right under the title")))

(deftest non-ex-info-provenance-test
  (let [boom (assoc p1 :mapping {:x (fn [_] (/ 1 0)) :y :y})]
    (testing "any exception in a sub-plot is wrapped with its id, cause kept"
      (let [e (try (plot/build (plot/arrange [p1 boom])) nil (catch clojure.lang.ExceptionInfo e e))]
        (is (= {:key :subplot :subplot [:plot 1] :subplot-path [[:plot 1]]} (ex-data e)))
        (is (instance? ArithmeticException (ex-cause e)))))
    (testing "nested: outer id first, inner kept"
      (let [data (error-data #(plot/build (plot/arrange [(plot/arrange [p1 boom]) p1])))]
        (is (= [:subplot [:plot 0] [[:plot 0] [:plot 1]]] ((juxt :key :subplot :subplot-path) data)))))))

;; panel alignment (4.4)

(defn- panel-area
  "Panel area of sub-plot `id` in outer canvas coordinates `[x0 y0 x1 y1]`."
  [built id]
  (let [{ox :x oy :y} (box-of built id)
        sub (get-in built [:subplots id])
        boxes (map :box (grid/content-items (:grid sub) (:layout sub) #(= :panel (:kind %))))]
    [(+ ox (reduce min (map :x boxes))) (+ oy (reduce min (map :y boxes)))
     (+ ox (reduce max (map #(+ (:x %) (:w %)) boxes))) (+ oy (reduce max (map #(+ (:y %) (:h %)) boxes)))]))

(def ^:private wide-labels
  {:data (for [i (range 20)] {:x i :y (* 1000000 i)}) :mapping {:x :x :y :y} :charts [(chart/scatter)]
   :labels {:title "big y labels" :y "a long y axis title" :x "x"}})
(def ^:private legend-plot
  {:data rows :mapping {:x :x :y :y :color :g} :charts [(chart/scatter)]})
(def ^:private faceted
  {:data rows :mapping {:x :x :y :y :facet :g} :charts [(chart/scatter)] :labels {:x "facet x"}})

(deftest align-test
  (let [plots [wide-labels p1 legend-plot faceted]
        built (fn [opts] (plot/build (plot/arrange plots (merge {:columns 2} opts)) {:width 900 :height 700}))
        areas (fn [b] (mapv #(panel-area b [:plot %]) (range 4)))]
    (testing "default: aligned"
      (let [[a0 _ a2 _] (areas (built {}))]
        (is (= [(a0 0) (a0 2)] [(a2 0) (a2 2)]))))
    (testing "with :align false the panels differ"
      (let [[a0 _ a2 a3] (areas (built {:align false}))]
        (is (not= (a0 0) (a2 0)) "left edges in column 0 differ")
        (is (not= (a2 1) (a3 1)) "top edges in row 1 differ (no title vs strips)")))
    (testing ":align - same column: left and right panel edges; same row: top and bottom edges"
      (let [b (built {:align true})
            [a0 a1 a2 a3] (areas b)]
        (is (= [(a0 0) (a0 2)] [(a2 0) (a2 2)]) "column 0")
        (is (= [(a1 0) (a1 2)] [(a3 0) (a3 2)]) "column 1")
        (is (= [(a0 1) (a0 3)] [(a1 1) (a1 3)]) "row 0")
        (is (= [(a2 1) (a2 3)] [(a3 1) (a3 3)]) "row 1")))
    (testing "aligned sub-plots stay standalone-equivalent apart from margins"
      (let [b (built {:align true})]
        (is (some #(get-in b [:subplots [:plot %] :alignment-padding]) [0 1 2 3]) "padding recorded on padded cells")
        (is (some? (plot/render (plot/arrange plots {:columns 2 :align true}) {:width 900 :height 700})))))
    (testing "one cell, one row, one column"
      (is (some? (plot/build (plot/arrange [p1] {:align true}))))
      (let [b (plot/build (plot/arrange [wide-labels p1] {:align true}) {:width 400 :height 600})
            [a0 a1] [(panel-area b [:plot 0]) (panel-area b [:plot 1])]]
        (is (= [(a0 0) (a0 2)] [(a1 0) (a1 2)]) "single column aligned"))
      (let [b (plot/build (plot/arrange [wide-labels legend-plot] {:rows 1 :align true}) {:width 800 :height 300})
            [a0 a1] [(panel-area b [:plot 0]) (panel-area b [:plot 1])]]
        (is (= [(a0 1) (a0 3)] [(a1 1) (a1 3)]) "single row aligned")))
    (testing "cells without panels (nested multiplots) take no part, no error"
      (let [b (plot/build (plot/arrange [(plot/arrange [p1 p2]) wide-labels p1] {:columns 2 :align true}) {:width 800 :height 600})]
        (is (= 3 (count (:subplots b))))
        (is (nil? (get-in b [:subplots [:plot 0] :alignment-padding])))))
    (testing "validation"
      (is (= :arrange (:key (error-data #(plot/build (plot/arrange [p1] {:align 1})))))))))
