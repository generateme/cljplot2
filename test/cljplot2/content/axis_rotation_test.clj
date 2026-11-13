(ns cljplot2.content.axis-rotation-test
  "Rotated tick labels (theme `[:axis k :label-rotation]`) and general label rotation."
  (:require [clojure.test :refer [deftest testing is]]
            [cljplot2.test-util :refer [error-ins]]
            [cljplot2.chart :as chart]
            [cljplot2.content.axis :as axis]
            [cljplot2.content.label :as label]
            [cljplot2.core :as plot]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.layout.grid :as grid]
            [cljplot2.theme :as theme]))

(defn- size [content]
  (measure/call-with-scratch-canvas
   :high (fn [c] (let [s (measure/natural-size content {:canvas c :theme theme/default-theme})] [(.x s) (.y s)]))))

(deftest quarter-turn-test
  (is (= [0 90 180 270 270 90 0] (map label/quarter-turn [0 90 180 270 -90 450 -360])))
  (is (nil? (label/quarter-turn 45)))
  (is (nil? (label/quarter-turn -0.5))))

(deftest rotated-label-size-test
  (let [text {:kind :label :role :tick-label :text "Wisconsin No. 38"}
        [w h] (size text)]
    (testing "multiples of 90 swap exactly"
      (is (= [h w] (size (assoc text :rotation 90))))
      (is (= [h w] (size (assoc text :rotation -90))))
      (is (= [w h] (size (assoc text :rotation 180)))))
    (testing "other angles: the rounded-up bounding box"
      (let [[w45 h45] (size (assoc text :rotation 45))
            d (/ (+ w h) (Math/sqrt 2.0))]
        (is (= (Math/ceil d) w45 h45))
        (is (= (size (assoc text :rotation -45)) [w45 h45]))))))

(def ^:private bands-plot
  {:data (for [i (range 24)] {:x (str "a rather long label " i) :y i})
   :mapping {:x :x :y :y}
   :charts [(chart/scatter)]})

(defn- built [p] (plot/build p {:width 600 :height 400}))

(defn- axis-box [b id] (grid/item-box (:layout b) id))

(deftest rotated-axis-test
  (testing "rotating -90: the horizontal axis gets taller (label lengths), every label fits"
    (let [plain (built bands-plot)
          rotated (built (assoc bands-plot :theme {:axis {:x {:label-rotation -90}}}))]
      (is (> (:h (axis-box rotated :x-axis)) (* 3 (:h (axis-box plain :x-axis)))))
      (is (< (:h (axis-box plain :x-axis)) 30))))
  (testing "rotation keyed by scale: a flipped plot rotates its horizontal :y labels with [:axis :y]"
    (let [p (assoc bands-plot :mapping {:x :y :y :x} :flip true)
          plain (built p)
          rotated (built (assoc p :theme {:axis {:y {:label-rotation -45}}}))]
      (is (> (:h (axis-box rotated :x-axis)) (:h (axis-box plain :x-axis))))))
  (testing "a vertical axis grows in width when its labels rotate"
    (let [p (assoc bands-plot :mapping {:x :y :y :x})
          plain (built p)
          rotated (built (assoc p :theme {:axis {:y {:label-rotation -90}}}))]
      (is (< (:w (axis-box rotated :y-axis)) (:w (axis-box plain :y-axis))) "-90: text height across the axis")))
  (testing "rotation 0 is the default and changes nothing"
    (is (= (:layout (built bands-plot)) (:layout (built (assoc bands-plot :theme {:axis {:x {:label-rotation 0}}})))))))

(deftest rotated-thinning-test
  (let [step (fn [rotation]
               (let [b (built (assoc bands-plot :theme {:axis {:x {:label-rotation rotation}}}))
                     env {:theme (:theme b) :scales (:finalized-scales b)}]
                 (measure/call-with-scratch-canvas
                  :high (fn [c] (let [env (assoc env :canvas c)
                                      finalized (get-in env [:scales :x])
                                      sizes (mapv #(measure/natural-size {:kind :label :role :tick-label :text % :rotation rotation} env)
                                                  (:tick-labels finalized))]
                                  (axis/label-step :x finalized sizes 2.0
                                                   (vec (repeat (count sizes) 0.0))
                                                   (axis/label-spans :x finalized env sizes)))))))]
    (is (> (step 0) (step -45)) "parallel rotated labels need less room than their boxes")
    (is (>= (step -45) (step -90)))
    (is (= 1 (step -90)) "vertical text: every band label fits")))

(deftest label-rotation-validation-test
  (is (= #{[:theme :axis :x :label-rotation]} (error-ins #(plot/build {:theme {:axis {:x {:label-rotation 91}}}}))))
  (is (= #{[:theme :axis :y :label-rotation]} (error-ins #(plot/build {:theme {:axis {:y {:label-rotation "45"}}}}))))
  (is (some? (plot/build {:theme {:axis {:x {:label-rotation -90} :y {:label-rotation 90}}}}))))

(deftest crossing-labels-test
  (testing "labels overlapping the next panel's identical labels are skipped; the next panel keeps its own"
    (is (= #{2} (axis/crossing-labels [[0.0 10.0 20.0] [8.0 8.0 8.0]] [0 1 2] 25.0)))
    (is (= #{} (axis/crossing-labels [[0.0 10.0 20.0] [8.0 8.0 8.0]] [0 1 2] 40.0)))
    (is (= #{} (axis/crossing-labels [[0.0 10.0 20.0] [8.0 8.0 8.0]] [0 1] 25.0)) "only drawn labels count")
    (is (= #{1 2} (axis/crossing-labels [[0.0 10.0 20.0] [30.0 30.0 30.0]] [0 1 2] 35.0))))
  (testing "the facet grid gives every horizontal axis but the last column :next-panel-gap"
    (let [p {:data (for [i (range 40)] {:x (str "a long band label " (mod i 5)) :y i :f (mod i 3)})
             :mapping {:x :x :y :y :facet :f} :facets {:rows 1} :charts [(chart/scatter)]}
          items (:items (:grid (built p)))
          gaps (into {} (keep (fn [{:keys [id content]}] (when (and (vector? id) (= :x-axis (first id))) [(second id) (:next-panel-gap content)])) items))]
      (let [gap (get-in theme/default-theme [:facet :gap])] (is (= {0 gap 1 gap 2 nil} gaps)))
      (is (some? (plot/render p {:width 600 :height 400}))))))
