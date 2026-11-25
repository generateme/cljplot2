(ns cljplot2.batch-c2-at-test
  "Batch C2 (fork at): axis titles fitted to their panels (wrapped, shrunk to the `:axis-title` role's `:min-size`, cut with
  an ellipsis; the fitting shared with strips) and radar tick labels crossing another spoke dropped."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [cljplot2.chart :as chart]
            [cljplot2.content.label :as label]
            [cljplot2.content.strip]
            [cljplot2.core :as plot]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.theme :as theme]
            [cljplot2.test-util :refer [error-ins]])
  (:import [fastmath.vector Vec2]))

(def ^:private rows
  (vec (for [i (range 30)] {:x (mod (* i 7) 13) :y (mod (* i 5) 11) :g (["a" "b" "c"] (mod i 3))})))

(def ^:private long-title
  "a very long axis title that is much longer than the panel of this small plot indeed")

(defn- with-canvas
  "Calls `f` with a measuring env over the theme with `overrides`."
  [overrides f]
  (measure/call-with-scratch-canvas :high (fn [c] (f {:canvas c :theme (theme/plot-theme overrides)}))))

(defn- fitted
  "The axis title label `text` (rotation `rotation`) fitted to `length` pixels, wrapping."
  [text rotation length]
  (with-canvas nil (fn [env] (label/fitted-label {:kind :label :role :axis-title :text text :rotation rotation}
                                                 {:length (double length) :wrap true} env))))

(defn- along [content] (with-canvas nil #(label/along-text content %)))

(defn- size-of [content box] (with-canvas nil #(measure/natural-size content (assoc % :box box))))

(defn- layout [p options] (:layout (plot/build p options)))
(defn- track-size [l axis id] (:size (first (filter #(= id (:id %)) (get l axis)))))

;; #27 axis titles fitted to their panels

(deftest fitted-label-test
  (testing "a fitting text is unchanged, empty text too"
    (let [c {:kind :label :role :axis-title :text "short" :rotation 0}]
      (is (= c (with-canvas nil #(label/fitted-label c {:length 300.0 :wrap true} %)))))
    (is (= "" (:text (fitted "" 0 0)))))
  (testing "wrapping at word boundaries: every line fits at the role size"
    (let [f (fitted long-title 0 300)]
      (is (nil? (:size f)))
      (is (< 1 (count (str/split-lines (:text f))) 4))
      (is (= (str/split long-title #" ") (str/split (:text f) #"\s+")) "no word lost or cut")
      (is (<= (along f) 300.0))))
  (testing "a rotated title fits along its height"
    (let [f (fitted long-title 270 200)]
      (is (< 1 (count (str/split-lines (:text f)))))
      (is (<= (along f) 200.0))))
  (testing "at most 3 lines; the rest shrinks, then is cut"
    (let [f (fitted (str/join " " (repeat 30 "word")) 0 60)]
      (is (= 3 (count (str/split-lines (:text f)))))
      (is (str/ends-with? (:text f) "…"))
      (is (= 8.0 (double (:size f))))
      (is (<= (along f) 60.0))))
  (testing "a single word longer than the length shrinks, then is cut"
    (let [f (fitted "averyveryverylongsinglewordwithoutanyspace" 0 100)]
      (is (= 1 (count (str/split-lines (:text f)))))
      (is (str/ends-with? (:text f) "…"))
      (is (<= (along f) 100.0))))
  (testing "a slightly long word only shrinks"
    (let [w (along {:kind :label :role :axis-title :text "passengers"})
          f (fitted "passengers" 0 (* 0.9 w))]
      (is (= "passengers" (:text f)))
      (is (< (:size f) 12))))
  (testing "given line breaks are kept, each line wrapped by itself"
    (let [f (fitted "first line is quite long here\nsecond" 0 150)]
      (is (= "second" (last (str/split-lines (:text f)))))
      (is (<= (along f) 150.0))))
  (testing "no room at all leaves the ellipsis on every wrapped line"
    (is (= ["…" "…" "…"] (str/split-lines (:text (fitted "long text here" 0 1)))))
    (is (= "…" (:text (fitted "longtext" 0 1)))))
  (testing "without wrapping (strips) the text shrinks and is cut, as before"
    (let [c {:kind :label :role :strip :text "a very long facet level name indeed" :rotation 90}
          f (with-canvas nil #(label/fitted-label c {:length 60.0} %))]
      (is (= 1 (count (str/split-lines (:text f)))))
      (is (= 7.0 (double (:size f)))))))

(deftest fit-content-test
  (let [c {:kind :label :role :axis-title :text long-title :fit true}
        plain (dissoc c :fit)]
    (testing "without a box (the first layout round) the natural size is the plain label's"
      (is (= (size-of plain nil) (size-of c nil))))
    (testing "a box long enough keeps the natural size"
      (is (= (size-of plain nil) (size-of c {:x 0 :y 0 :w 2000 :h 10}))))
    (testing "a short box wraps: narrower, taller"
      (let [^Vec2 s (size-of c {:x 0 :y 0 :w 200 :h 10})
            ^Vec2 p (size-of plain nil)]
        (is (<= (.x s) 200.0))
        (is (> (.y s) (.y p)))))
    (testing "rotated 270: fitted to the box height"
      (let [^Vec2 s (size-of (assoc c :rotation 270) {:x 0 :y 0 :w 10 :h 200})]
        (is (<= (.y s) 200.0))))
    (testing ":fit is internal content, not a plot label option"
      (is (seq (error-ins #(plot/build {:data rows :mapping {:x :x :y :y} :charts [(chart/scatter)]
                                        :labels {:x {:text "t" :fit true}}})))))))

(deftest axis-title-plot-test
  (let [base {:data rows :mapping {:x :x :y :y} :charts [(chart/scatter)]}
        opts {:width 300 :height 250}]
    (testing "a long x title takes more rows, a short one one line"
      (is (> (track-size (layout (assoc base :labels {:x long-title}) opts) :rows :x-title)
             (track-size (layout (assoc base :labels {:x "x"}) opts) :rows :x-title))))
    (testing "a long y title takes more columns"
      (is (> (track-size (layout (assoc base :labels {:y long-title}) opts) :columns :y-title)
             (track-size (layout (assoc base :labels {:y "y"}) opts) :columns :y-title))))
    (testing "a y title turned flat (rotation 0) isn't wrapped: its column is its natural width"
      (is (= (long (.x ^Vec2 (size-of {:kind :label :role :axis-title :text "flat y title"} nil)))
             (track-size (layout (assoc base :labels {:y {:text "flat y title" :rotation 0}}) opts) :columns :y-title))))
    (testing "flipped: the title under the panel is fitted to its width"
      (is (> (track-size (layout (assoc base :labels {:y long-title} :flip true) opts) :rows :x-title)
             (track-size (layout (assoc base :labels {:y "y"} :flip true) opts) :rows :x-title))))
    (testing "facets: one title over the whole grid's width"
      (let [faceted (assoc base :mapping {:x :x :y :y :facet :g})
            title "an axis title wider than one panel but narrower than three"]
        (is (= (track-size (layout (assoc faceted :labels {:x title}) {:width 600 :height 300}) :rows :x-title)
               (track-size (layout (assoc faceted :labels {:x "x"}) {:width 600 :height 300}) :rows :x-title)))))
    (testing "polar plots render long titles"
      (is (some? (plot/render-image (assoc base :coord {:kind :polar} :labels {:x long-title :y long-title}) opts))))
    (testing "a title wider than the whole image is cut, not an error"
      (is (some? (plot/render-image (assoc base :labels {:x (str/join " " (repeat 60 "word")) :y (apply str (repeat 80 "w"))})
                                    {:width 150 :height 150}))))
    (testing "a theme least size: a positive number"
      (is (seq (error-ins #(plot/build (assoc base :theme {:text {:axis-title {:min-size 0}}})))))
      (is (seq (error-ins #(plot/build (assoc base :theme {:text {:axis-title {:min-size "8"}}}))))))))

;; radar tick labels crossing other spokes

(deftest box-crosses-path-test
  (let [box {:x 10 :y 10 :w 20 :h 10}]
    (testing "zero or one point never crosses"
      (is (not (measure/box-crosses-path? box [])))
      (is (not (measure/box-crosses-path? box [[15 15]]))))
    (testing "a segment through the box crosses, one beside it doesn't"
      (is (measure/box-crosses-path? box [[0 15] [40 15]]))
      (is (measure/box-crosses-path? box [[0 0] [20 15] [50 50]]))
      (is (not (measure/box-crosses-path? box [[0 0] [40 0]]))))
    (testing "touching within 1 px doesn't count"
      (is (not (measure/box-crosses-path? box [[0 10.5] [40 10.5]]))))
    (testing "a box at most 2 px across never crosses"
      (is (not (measure/box-crosses-path? {:x 0 :y 0 :w 2 :h 10} [[-5 5] [5 5]]))))))

(deftest spoke-crossings-test
  (let [label (fn [x y] [{:kind :label :role :tick-label :text "100"} [:center :middle] x y])
        placed [{:path [[0 0] [100 0]] :ticks [{:label (label 50 0) :mark []}         ; crosses its own axis only
                                               {:label (label 50 50) :mark []}]}      ; crosses axis 1
                {:path [[0 50] [100 50]] :ticks [{:label (label 50 100) :mark []}
                                                 {:label (label 50 0) :mark [] :centre ["100" 0]}]}]
        result (with-canvas nil #(#'cljplot2.chart.parallel/without-spoke-crossings (:canvas %) placed %))]
    (testing "a label crossing another spoke loses its label, keeps its mark"
      (is (contains? (get-in result [0 :ticks 1]) :mark))
      (is (nil? (get-in result [0 :ticks 1 :label]))))
    (testing "a label crossing only its own spoke stays"
      (is (some? (get-in result [0 :ticks 0 :label])))
      (is (some? (get-in result [1 :ticks 0 :label]))))
    (testing "a label at a spoke's inner end stays"
      (is (some? (get-in result [1 :ticks 1 :label]))))
    (testing "no axes"
      (is (= [] (with-canvas nil #(#'cljplot2.chart.parallel/without-spoke-crossings (:canvas %) [] %)))))))

(deftest radar-renders-test
  (testing "a radar with spokes of their own ranges and a flipped one render"
    (let [wide (vec (for [i (range 5)] {:a (+ 2 i) :b (* 10 (inc i)) :c (- 100 (* 3 i)) :d (* 0.5 i) :m (str "r" i)}))
          p {:data wide :mapping {:color :m} :charts [(chart/parallel {:variables [:a :b :c :d]})] :coord {:kind :polar}}]
      (is (some? (plot/render-image p {:width 300 :height 300})))
      (is (some? (plot/render-image (assoc p :flip true) {:width 300 :height 300}))))))
