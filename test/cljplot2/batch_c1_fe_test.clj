(ns cljplot2.batch-c1-fe-test
  "Batch C1 (fork fe): the chart option `:legend false`, per-axis tick label styles (theme `[:axis k :tick-label]`),
  the least line width of legend keys (theme `[:legend :min-key-line-width]`) and strip texts fitted to their strips
  (theme `[:strip :min-text-size]`)."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [fastmath.vector :as v]
            [cljplot2.chart :as chart]
            [cljplot2.content.axis :as axis]
            [cljplot2.content.legend :as legend]
            [cljplot2.content.strip]
            [cljplot2.core :as plot]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.theme :as theme]
            [cljplot2.test-util :refer [error-ins same-pixels?]])
  (:import [fastmath.vector Vec2]))

(def ^:private rows
  (vec (for [i (range 30)] {:x (mod (* i 7) 13) :y (mod (* i 5) 11) :g (["a" "b" "c"] (mod i 3))})))

(defn- legend-charts
  "The `:charts` of the plot's legends."
  [p]
  (mapv :charts (legend/legends (plot/build p {:width 400 :height 300}))))

;; #2 chart option :legend

(deftest chart-legend-option-test
  (let [base {:data rows :mapping {:x :x :y :y :color :g}}]
    (testing "default and nil draw the glyph, false leaves the chart out of the keys"
      (is (= [[{:chart 0 :dims [:color]} {:chart 1 :dims [:color]}]]
             (legend-charts (assoc base :charts [(chart/scatter) (chart/rug {:mapping {:y nil}})]))
             (legend-charts (assoc base :charts [(chart/scatter) (chart/rug {:mapping {:y nil} :legend nil})]))
             (legend-charts (assoc base :charts [(chart/scatter) (chart/rug {:mapping {:y nil} :legend true})]))))
      (is (= [[{:chart 0 :dims [:color]}]]
             (legend-charts (assoc base :charts [(chart/scatter) (chart/rug {:mapping {:y nil} :legend false})])))))
    (testing "every chart without glyphs: the legend is dropped (as scale :legend false); the scale still colours"
      (let [p (assoc base :charts [(chart/scatter {:legend false})])]
        (is (= [] (legend-charts p)))
        (is (same-pixels? p (assoc base :charts [(chart/scatter)] :scales {:color {:legend false}})
                          {:width 400 :height 300}))))
    (testing "only the legends nobody keys are dropped: a shape legend keyed by another chart stays"
      (is (= [[{:chart 1 :dims [:shape]}]]
             (legend-charts (assoc base :mapping {:x :x :y :y :color :g}
                                   :charts [(chart/scatter {:legend false})
                                            (chart/scatter {:mapping {:color nil :shape :g}})])))))
    (testing "a facetted and flipped plot keeps the rule"
      (is (= [[{:chart 1 :dims [:color]}]]
             (legend-charts (assoc base :flip true :mapping {:x :x :y :y :color :g :facet :g}
                                   :charts [(chart/line {:legend false}) (chart/scatter)])))))
    (testing "the function chart (no common data) takes it too"
      (is (= [] (legend-charts {:charts [(chart/function {:f {"sin" #(Math/sin %) "cos" #(Math/cos %)}
                                                            :domain [0 3] :mapping {:color :function}
                                                            :legend false})]}))))
    (testing "a wrong type is an option error"
      (is (contains? (error-ins #(plot/build (assoc base :charts [(chart/scatter {:legend :no})]) {}))
                     [:charts 0 :options :legend])))
    (testing "with false the image differs from the default only in the legend: plot unchanged without colours"
      (is (same-pixels? {:data rows :mapping {:x :x :y :y} :charts [(chart/scatter {:legend false})]}
                        {:data rows :mapping {:x :x :y :y} :charts [(chart/scatter)]}
                        {:width 300 :height 200})))))

;; #20 per-axis tick label styles

(defn- axis-size
  "Natural size of the axis of scale `k` of a built plot."
  ^Vec2 [built k]
  (measure/call-with-scratch-canvas
   :high
   (fn [c] (measure/natural-size {:kind :axis :scale k}
                                 {:canvas c :theme (:theme built) :scales (:finalized-scales built)}))))

(deftest axis-tick-label-test
  (let [p {:data rows :mapping {:x :x :y :y} :charts [(chart/scatter)]}
        options {:width 400 :height 300}]
    (testing "the tick label content takes the axis's style keys over the role"
      (let [th (theme/plot-theme {:axis {:x {:tick-label {:size 20 :color [200 0 0]}}}})]
        (is (= {:kind :label :role :tick-label :text "1" :rotation 0 :size 20 :color [200 0 0]}
               (axis/tick-label th :x "1" 0)))
        (is (= {:kind :label :role :tick-label :text "1" :rotation 90} (axis/tick-label th :y "1" 90)))))
    (testing "an empty map (the default) and nil keep the image"
      (is (same-pixels? p (assoc p :theme {:axis {:x {:tick-label {}}}}) options)))
    (testing "a larger x font makes only the x axis taller"
      (let [big (plot/build (assoc p :theme {:axis {:x {:tick-label {:size 24}}}}) options)
            normal (plot/build p options)]
        (is (> (.y (axis-size big :x)) (.y (axis-size normal :x))))
        (is (not (same-pixels? p (assoc p :theme {:axis {:y {:tick-label {:color [255 0 0]}}}}) options)))))
    (testing "polar guides and flipped plots draw with it"
      (let [polar (assoc p :coord {:kind :polar})]
        (is (not (same-pixels? polar (assoc polar :theme {:axis {:x {:tick-label {:size 16}}}}) options))))
      (is (instance? java.awt.image.BufferedImage
                     (plot/render-image (assoc p :flip true :theme {:axis {:x {:tick-label {:font-style :bold}}}}) options))))
    (testing "wrong keys and types are theme errors"
      (is (seq (error-ins #(plot/build (assoc p :theme {:axis {:x {:tick-label {:size -1}}}}) options))))
      (is (seq (error-ins #(plot/build (assoc p :theme {:axis {:x {:tick-label {:weight 2}}}}) options))))
      (is (seq (error-ins #(plot/build (assoc p :theme {:axis {:x {:tick-label "big"}}}) options)))))))

;; #3 legend key line width

(deftest key-line-width-test
  (let [th theme/default-theme]
    (testing "thin lines are raised to the theme width; wider ones and other styles stay"
      (is (= 2 (:line-width (theme/key-line-style th {:line-width 1}))))
      (is (= 0 (:line-width (theme/key-line-style th {:line-width 0}))) "width 0 - no line - stays")
      (is (= 0.0 (:line-width (theme/key-line-style th {:line-width 0.0}))))
      (is (= 2 (:line-width (theme/key-line-style th {:line-width 0.01}))))
      (is (= 3 (:line-width (theme/key-line-style th {:line-width 3}))))
      (is (= {:color :red} (theme/key-line-style th {:color :red})))
      (is (= {:line-width nil} (theme/key-line-style th {:line-width nil}))))
    (testing "0 keeps the lines' widths"
      (is (= 0.5 (:line-width (theme/key-line-style (assoc-in th [:legend :min-key-line-width] 0) {:line-width 0.5})))))
    (testing "key-style applies it before a glyph's mapped attributes"
      (let [env {:theme th}]
        (is (= 2 (:line-width (theme/key-style env (chart/line) {} nil))))
        (is (= 5 (:line-width (theme/key-style env (chart/line) {:line-width 5} nil))))))
    (testing "a wrong type is a theme error"
      (is (seq (error-ins #(plot/build {:data rows :mapping {:x :x :y :y} :charts [(chart/line)]
                                        :theme {:legend {:min-key-line-width -1}}} {})))))))

;; #4 strip texts fitted to their strips

(defn- fitted
  "The strip label fitted to `available` pixels under `theme-overrides`."
  [content available theme-overrides]
  (measure/call-with-scratch-canvas
   :high
   (fn [c] (#'cljplot2.content.strip/fitted-label content (double available)
                                                   {:canvas c :theme (theme/plot-theme theme-overrides)}))))

(defn- along
  "Length along the text of a label content."
  [content]
  (measure/call-with-scratch-canvas
   :high
   (fn [c] (let [^Vec2 s (measure/natural-size content {:canvas c :theme theme/default-theme})]
             (if (#{90 270} (:rotation content)) (.y s) (.x s))))))

(deftest strip-text-fit-test
  (let [label (fn [text rotation] {:kind :label :role :strip :text text :rotation rotation})]
    (testing "a fitting text is unchanged"
      (let [c (label "short" 0)] (is (= c (fitted c 200 nil))))
      (let [c (label "" 90)] (is (= c (fitted c 0 nil)))))
    (testing "a slightly long text shrinks and keeps its text"
      (let [c (label "passengers" 90)
            n (along c)
            f (fitted c (* 0.85 n) nil)]
        (is (= "passengers" (:text f)))
        (is (< (:size f) 11))
        (is (<= (along f) (* 0.85 n)))))
    (testing "a very long text is cut with an ellipsis at the least size"
      (let [f (fitted (label "a very long facet level name indeed" 90) 60 nil)]
        (is (= 7.0 (double (:size f))))
        (is (str/ends-with? (:text f) "…"))
        (is (<= (along f) 60.0))))
    (testing "a larger least size cuts earlier"
      (let [c (label "a very long facet level name indeed" 0)]
        (is (< (count (:text (fitted c 80 {:strip {:min-text-size 9}})))
               (count (:text (fitted c 80 nil)))))))
    (testing "multi-line texts are cut per line"
      (let [f (fitted (label "max-price and more\nmin" 0) 50 nil)
            [a b] (str/split-lines (:text f))]
        (is (str/ends-with? a "…"))
        (is (= "min" b))))
    (testing "no room at all leaves the ellipsis"
      (is (= "…" (:text (fitted (label "long text" 0) 1 nil)))))
    (testing "a wrong type is a theme error"
      (is (seq (error-ins #(plot/build {:data rows :mapping {:x :x :y :y :facet :g} :charts [(chart/scatter)]
                                        :theme {:strip {:min-text-size 0}}} {})))))))
