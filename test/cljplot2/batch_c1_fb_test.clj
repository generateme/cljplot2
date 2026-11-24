(ns cljplot2.batch-c1-fb-test
  "Batch C1 (fb): scatter `:alpha`, line area fills keeping the colour's own alpha, cloud `:type :lines`, every
  parallel variable name drawn, a radar's centre value labelled once."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [fastmath.vector :as v]
            [cljplot2.test-util :refer [error-ins same-pixels? pixels]]
            [cljplot2.chart :as chart]
            [cljplot2.chart.cloud :as cloud]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.content.label :as label]
            [cljplot2.core :as plot]
            [cljplot2.theme :as theme]))

(def ^:private options {:width 400 :height 300})

(defn- built [p] (plot/build p options))

(defn- same-colors? [a b] (= (c/to-color a) (c/to-color b)))

;; 1. scatter :alpha

(def ^:private xy [{:x 0 :y 0 :g "a"} {:x 1 :y 2 :g "b"} {:x 2 :y 1 :g nil}])

(defn- scatter-plot [scatter-options]
  {:data xy :mapping {:x :x :y :y} :legend false :charts [(chart/scatter (merge {:size 40} scatter-options))]})

(deftest scatter-alpha-test
  (testing "nil and an opaque colour at 1 are the default"
    (is (same-pixels? (scatter-plot {}) (scatter-plot {:alpha nil}) options))
    (is (same-pixels? (scatter-plot {:color [255 0 0]}) (scatter-plot {:color [255 0 0] :alpha 1}) options)))
  (testing "an absolute opacity: the colour's own alpha replaced"
    (is (same-pixels? (scatter-plot {:color [255 0 0 255] :alpha 0.5}) (scatter-plot {:color [255 0 0 127.5]}) options))
    (is (same-pixels? (scatter-plot {:color [255 0 0 20] :alpha 0.5}) (scatter-plot {:color [255 0 0 127.5]}) options))
    (is (same-pixels? (scatter-plot {:color [255 0 0] :alpha 0}) (scatter-plot {:color [255 0 0 0]}) options) "zero"))
  (testing "the outline keeps its own colour"
    (is (same-pixels? (scatter-plot {:color [255 0 0] :stroke-color [0 0 0] :stroke-width 3 :alpha 0.4})
                      (scatter-plot {:color [255 0 0 102] :stroke-color [0 0 0] :stroke-width 3}) options)))
  (testing "mapped colours take it, NA too"
    (let [first-color (first (c/palette :category20))
          one-group [{:x 0 :y 0 :g "a"} {:x 1 :y 2 :g "a"}]]
      (is (same-pixels? {:data one-group :mapping {:x :x :y :y :color :g} :legend false :charts [(chart/scatter {:size 40 :alpha 0.5})]}
                        {:data one-group :mapping {:x :x :y :y} :legend false :charts [(chart/scatter {:size 40 :color (c/set-alpha first-color 127.5)})]}
                        options))
      (is (same-pixels? {:data [{:x 0 :y 0 :g "a"} {:x 1 :y 2 :g nil}] :mapping {:x :x :y :y :color :g} :legend false
                         :charts [(chart/scatter {:size 40 :alpha 0.5})]}
                        {:mapping {:x :x :y :y} :legend false
                         :charts [(chart/scatter {:data [{:x 0 :y 0}] :size 40 :color (c/set-alpha first-color 127.5)})
                                  (chart/scatter {:data [{:x 1 :y 2}] :size 40
                                                  :color (c/set-alpha (get-in theme/default-theme [:palette :na]) 127.5)})]}
                        options)
          "NA")))
  (testing "the legend key takes it"
    (let [key-image (fn [chart-options key-color]
                      (let [canvas (c2d/canvas 30 30)
                            ch (chart/scatter chart-options)]
                        (c2d/with-canvas [cv canvas]
                          (c2d/set-background cv :white)
                          ((:legend ch) ch {:color key-color} cv {:x 0 :y 0 :w 30 :h 30} {:theme theme/default-theme}))
                        (pixels (c2d/get-image canvas))))]
      (is (= (vec (key-image {:alpha 0.5} (c/color 255 0 0))) (vec (key-image {} (c/color 255 0 0 127.5)))) "a mapped key colour")
      (is (= (vec (key-image {} (c/color 255 0 0))) (vec (key-image {:alpha nil} (c/color 255 0 0)))) "nil")
      (is (not= (vec (key-image {:alpha 0.5} (c/color 255 0 0))) (vec (key-image {} (c/color 255 0 0)))))))
  (testing "zero rows, polar, flip, facets"
    (is (some? (plot/render-image {:data [] :mapping {:x :x :y :y} :charts [(chart/scatter {:alpha 0.3})]} options)))
    (is (some? (plot/render-image (assoc (scatter-plot {:alpha 0.3}) :coord {:kind :polar}) options)))
    (is (same-pixels? (assoc (scatter-plot {:color [255 0 0] :alpha 0.5}) :flip true)
                      (assoc (scatter-plot {:color [255 0 0 127.5]}) :flip true) options))
    (is (some? (plot/render-image (assoc-in (scatter-plot {:alpha 0.3}) [:mapping :facet] :g) options))))
  (testing "wrong values are schema errors"
    (is (contains? (error-ins #(built (scatter-plot {:alpha 2}))) [:charts 0 :options :alpha]))
    (is (contains? (error-ins #(built (scatter-plot {:alpha "half"}))) [:charts 0 :options :alpha]))))

;; 2. line area fills keep the colour's own alpha

(deftest line-area-color-test
  (testing "an opaque colour fills at :alpha"
    (is (same-colors? (c/color 255 0 0 153) (polyline/area-color {:color (c/color 255 0 0) :alpha 0.6 :fill :color} false))))
  (testing "a translucent colour: :alpha times its own"
    (is (same-colors? (c/color 255 0 0 (* 0.6 128)) (polyline/area-color {:color (c/color 255 0 0 128) :alpha 0.6 :fill :color} false)))
    (is (same-colors? (c/color 0 0 255 0) (polyline/area-color {:color (c/color 0 0 255 0) :alpha 0.6 :fill :color} false)) "zero")
    (is (same-colors? (c/color 255 0 0 (* 0.6 128)) (polyline/area-color {:color (c/color 255 0 0 128) :alpha 0.6 :fill :none} true))
        "stacked areas without a fill"))
  (testing "a fill colour, :none and a nil alpha stay"
    (is (same-colors? [0 255 0 255] (polyline/area-color {:color (c/color 255 0 0 128) :alpha 0.6 :fill [0 255 0]} false)))
    (is (= :none (polyline/area-color {:color (c/color 255 0 0 128) :alpha 0.6 :fill :none} false)))
    (is (= :none (polyline/area-color {:color :none :alpha 0.6 :fill :color} false)) ":none colour"))
  (let [rows [{:x 0 :y 1} {:x 1 :y 3} {:x 2 :y 2}]
        line-plot (fn [o] {:data rows :mapping {:x :x :y :y} :charts [(chart/line o)]})]
    (testing "drawn: a dimmed line fills dimmed, opaque lines unchanged"
      (is (same-pixels? (line-plot {:color [255 0 0 128] :fill :color :alpha 0.5})
                        (line-plot {:color [255 0 0 128] :fill (c/color 255 0 0 64)}) options))
      (is (same-pixels? (line-plot {:color [255 0 0] :fill :color :alpha 0.5})
                        (line-plot {:color [255 0 0] :fill (c/color 255 0 0 127.5)}) options)))
    (testing ":stroke-alpha-factor fades the stroke only"
      (is (same-pixels? (line-plot {:color [255 0 0] :fill :color :alpha 0.5 :stroke-alpha-factor 0.3})
                        (line-plot {:color [255 0 0] :fill (c/color 255 0 0 127.5) :stroke-alpha-factor 0.3}) options)))
    (testing "stacked groups and flip"
      (let [groups (for [g ["a" "b"] r rows] (assoc r :g g))
            stacked (fn [o] {:data groups :mapping {:x :x :y :y :color :g} :legend false :charts [(chart/line (merge {:position :stack} o))]})]
        (is (some? (plot/render-image (assoc-in (stacked {:alpha 0.5}) [:scales :color :range] [(c/color 255 0 0 128) (c/color 0 0 255 128)]) options)))
        (is (same-pixels? (assoc (line-plot {:color [255 0 0 128] :fill :color :alpha 0.5}) :flip true)
                          (assoc (line-plot {:color [255 0 0 128] :fill (c/color 255 0 0 64)}) :flip true) options))))))

;; 3. cloud :type :lines

(def ^:private helix
  (vec (for [g ["a" "b"] t (range 5)] {:x t :y (if (= g "a") t (- t)) :z (* 2 t) :g g})))

(defn- panel-data [p] (-> (built p) :stats first :data vals first))

(defn- segments [p]
  (let [project (fn [[x y z]] [(v/vec2 (double x) (double y)) (double z)])
        b (built p)
        computed (-> b :stats first :data vals first)]
    (filterv #(= :line (:kind %)) (cloud/chart-primitives nil computed project (:finalized-scales b) {:theme (:theme b)}))))

(defn- cloud-plot [data mapping cloud-options]
  {:data data :mapping (merge {:x :x :y :y :z :z} mapping) :charts [(chart/cloud (merge {:type :lines} cloud-options))]})

(deftest cloud-lines-test
  (testing "one line without :group, a line per :group value"
    (is (= 9 (count (segments (cloud-plot helix {} {})))))
    (is (= 8 (count (segments (cloud-plot helix {:group :g} {}))))))
  (testing "zero and one row: no segments"
    (is (= 0 (count (segments (cloud-plot (take 1 helix) {} {})))))
    (is (some? (plot/render-image (cloud-plot (take 1 helix) {} {}) options)))
    (is (some? (plot/render-image (assoc (cloud-plot helix {} {}) :data []) options))))
  (testing "nil group values are one line"
    (is (= 3 (count (segments (cloud-plot (map #(assoc % :g nil) (take 4 helix)) {:group :g} {}))))))
  (testing "data order; depth the mean of the ends; style of the first point"
    (let [[s] (segments (cloud-plot (take 2 helix) {} {:line-width 3 :color [255 0 0]}))]
      (is (= (v/vec2 0.0 0.0) (:from s)))
      (is (= (v/vec2 1.0 1.0) (:to s)))
      (is (== 1.0 (:depth s)))
      (is (== 3.0 (double (:line-width s))))
      (is (same-colors? [255 0 0] (:color s)))))
  (testing "mapped colours colour the segments; :group isn't read by other types"
    (is (= 2 (count (distinct (map :color (segments (cloud-plot helix {:color :g :group :g} {})))))))
    (is (not (contains? (:points (panel-data (cloud-plot helix {:group :g} {:type :points}))) :group))))
  (testing "facets: lines per panel; renders with a legend"
    (is (= 2 (count (:data (first (:stats (built (cloud-plot helix {:facet :g} {}))))))))
    (is (some? (plot/render-image (cloud-plot helix {:color :g :group :g} {}) options))))
  (testing "an unknown type is a schema error"
    (is (contains? (error-ins #(built (cloud-plot helix {} {:type :line}))) [:charts 0 :options :type]))))

;; 4. every parallel variable name

(def ^:private wide
  [{:sepal-length-cm 5.1 :sepal-width-cm 3.5 :petal-length-cm 1.4 :petal-width-cm 0.2}
   {:sepal-length-cm 7.0 :sepal-width-cm 3.2 :petal-length-cm 4.7 :petal-width-cm 1.4}
   {:sepal-length-cm 6.3 :sepal-width-cm 3.3 :petal-length-cm 6.0 :petal-width-cm 2.5}])

(defn- drawn-texts [p render-options]
  (let [log (atom [])
        draw-at label/draw-at!]
    (with-redefs-fn {#'label/draw-at! (fn [canvas content anchor x y env] (swap! log conj (:text content)) (draw-at canvas content anchor x y env))}
      #(plot/render-image p render-options))
    @log))

(deftest parallel-names-test
  (let [p {:data wide :charts [(chart/parallel {:variables [:sepal-length-cm :sepal-width-cm :petal-length-cm :petal-width-cm]})]}
        names #{"sepal-length-cm" "sepal-width-cm" "petal-length-cm" "petal-width-cm"}
        x-labels (fn [q] (get-in (plot/build q {:width 260 :height 300}) [:finalized-scales :x :overlapping-labels]))]
    (testing "every name is drawn in a narrow plot (the axis keeps every label), flipped too"
      (is (= :keep (x-labels p)))
      (is (= :keep (x-labels (assoc p :flip true))))
      (is (= :keep (x-labels (assoc p :data [])))) "zero rows")
    (testing "the plot's scale wins"
      (is (not= :keep (x-labels (assoc p :scales {:x {:overlapping-labels :drop}})))))
    (testing "polar: every name drawn around the circle"
      (is (= names (set (filter names (map str (drawn-texts (assoc p :coord {:kind :polar}) {:width 260 :height 300})))))))))

;; 5. a radar's centre value once

(def ^:private athletes
  [{:name "Ann" :speed 8 :strength 5 :stamina 9 :skill 6 :reach 4}
   {:name "Bo" :speed 5 :strength 9 :stamina 6 :skill 4 :reach 8}])

(defn- radar [parallel-options plot-options]
  (merge {:data athletes :mapping {:color :name} :coord {:kind :polar}
          :charts [(chart/parallel (merge {:variables [:speed :strength :stamina :skill :reach] :scaling :globalminmax
                                           :limits (zipmap [:speed :strength :stamina :skill :reach] (repeat [0 10]))}
                                          parallel-options))]}
         plot-options))

(defn- count-text [p text] (count (filter #{text} (map str (drawn-texts p {:width 500 :height 450})))))

(deftest radar-centre-test
  (testing "the shared inner-end value is labelled once"
    (is (= 1 (count-text (radar {} {}) "0")))
    (is (= 5 (count-text (radar {} {}) "10")) "other rings keep a label per spoke"))
  (testing "different inner ends are each labelled once"
    (let [p (radar {:limits {:speed [2 10] :strength [0 10] :stamina [2 10] :skill [0 10] :reach [2 10]}} {})]
      (is (= 1 (count-text p "0")) "the two spokes starting at 0"))
    (let [p (update-in (radar {} {}) [:charts 0 :options] dissoc :limits :scaling)
          texts #(drawn-texts p {:width 500 :height 450})]
      (is (= {"5" 1} (into {} (remove (comp zero? val)) (merge-with - (frequencies (with-redefs-fn {#'cljplot2.chart.parallel/without-repeated-centre-labels identity} texts))
                                  (frequencies (texts)))))
          "each axis its own range: only the minimum 5 that speed and strength share at the centre is dropped (once)")))
  (testing "cartesian and flipped polar plots keep a label per axis"
    (is (= 5 (count-text (radar {} {:coord nil}) "0")))
    (is (< 1 (count-text (radar {} {:flip true}) "0"))))
  (testing "a donut (inner radius): one label on the inner ring"
    (is (= 1 (count-text (radar {} {:coord {:kind :polar :inner 0.3}}) "0")))))
