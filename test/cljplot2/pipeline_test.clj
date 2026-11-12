(ns cljplot2.pipeline-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure2d.core :as c2d]
            [fastmath.vector :as v]
            [cljplot2.pipeline :as pipeline]
            [cljplot2.measure :as measure]
            [cljplot2.examples :as examples]))

(defn- error-data [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

;; test content kinds

(defmethod measure/natural-size ::settling
  [_ {:keys [box]}]
  (v/vec2 (if box 20.0 10.0) 0.0))

(defmethod measure/natural-size ::flip-flop
  [_ {:keys [box]}]
  (v/vec2 (if (and box (< (long (:w box)) 30)) 100.0 10.0) 0.0))

(defmethod measure/natural-size ::text
  [{:keys [text]} {:keys [canvas]}]
  (v/vec2 (c2d/text-width canvas text) (c2d/font-height canvas)))

(defn- measured-plot [content]
  {:grid {:columns [{:size [:measured]} {:size [:flexible]}]
          :rows [{:size [:flexible]}]
          :items [{:id :m :column 0 :row 0 :content content}]}})

;; 2.x stages

(deftest stage-order-test
  (is (= [:split :stats :extents :scale-specs :layout :scales] pipeline/stage-order)))

(deftest empty-plot-test
  (let [built (pipeline/build {})]
    (testing "every stage adds its key"
      (is (every? #(contains? built %) pipeline/stage-order)))
    (testing "stubs produce empty values"
      (is (= {} (:split built) (:stats built) (:extents built) (:scale-specs built) (:scales built))))
    (testing "default grid and options"
      (is (= {:x 0 :y 0 :w 600 :h 600} (get-in built [:layout :items 0 :box])))
      (is (= pipeline/default-options (:options built))))))

(deftest until-test
  (testing "partial runs stop after the given stage"
    (doseq [[idx stage] (map-indexed vector pipeline/stage-order)
            :let [built (pipeline/build {} {:until stage})]]
      (is (every? #(contains? built %) (take (inc idx) pipeline/stage-order)) (str stage))
      (is (not-any? #(contains? built %) (drop (inc idx) pipeline/stage-order)) (str stage))))
  (testing ":until :scale-specs"
    (let [built (pipeline/build {} {:until :scale-specs})]
      (is (contains? built :scale-specs))
      (is (not (contains? built :layout)))))
  (testing "stage keys from an earlier build are dropped"
    (let [rebuilt (pipeline/build (pipeline/build {:grid examples/fig-1-1-grid}) {:until :split})]
      (is (contains? rebuilt :split))
      (is (not-any? #(contains? rebuilt %) (rest pipeline/stage-order)))
      (is (= (pipeline/build {:grid examples/fig-1-1-grid}) (pipeline/build (pipeline/build {:grid examples/fig-1-1-grid}))))))
  (testing "unknown stage"
    (is (= {:key :until :value :draw :allowed pipeline/stage-order}
           (error-data #(pipeline/build {} {:until :draw}))))))

(deftest purity-test
  (testing "each stage is repeatable: running up to it twice gives = results"
    (doseq [stage pipeline/stage-order]
      (is (= (pipeline/build {:grid examples/fig-1-1-grid} {:until stage})
             (pipeline/build {:grid examples/fig-1-1-grid} {:until stage}))
          (str stage))))
  (testing "result is printable data"
    (is (string? (pr-str (pipeline/build {:grid examples/nested-grid}))))))

(deftest options-test
  (testing "plot options under build options"
    (is (= {:width 10 :height 20 :hint :low}
           (:options (pipeline/build {:options {:width 10 :height 5 :hint :low}} {:height 20})))))
  (testing "invalid options"
    (are [opts k] (= k (:key (error-data #(pipeline/build {} opts))))
      {:width 0} :width
      {:width -1} :width
      {:width 1.5} :width
      {:width nil} :width
      {:height 0} :height
      {:hint :retina} :hint
      {:hint nil} :hint))
  (testing "1x1 plot"
    (is (= {:x 0 :y 0 :w 1 :h 1} (get-in (pipeline/build {} {:width 1 :height 1}) [:layout :items 0 :box])))))

;; 5.x measurement loop

(deftest rounds-test
  (testing "static content stops after one round"
    (is (= 1 (get-in (pipeline/build {:grid examples/fig-1-1-grid}) [:layout :rounds]))))
  (testing "content settling in the second round"
    (let [built (pipeline/build (measured-plot {:kind ::settling}))]
      (is (= 2 (get-in built [:layout :rounds])))
      (is (= 20 (get-in built [:layout :columns 0 :size])))))
  (testing "flip-flopping content stops at the cap"
    (is (= pipeline/max-layout-rounds (get-in (pipeline/build (measured-plot {:kind ::flip-flop})) [:layout :rounds])))))

(deftest scratch-canvas-release-test
  (let [flush-graphics c2d/flush-graphics
        released (atom [])
        received (atom nil)
        released? (fn [] (some #(identical? @received %) @released))]
    (with-redefs [c2d/flush-graphics (fn [canvas] (swap! released conj canvas) (flush-graphics canvas))]
      (testing "graphics of the canvas given to f are released when f throws"
        (is (thrown? clojure.lang.ExceptionInfo
                     (measure/call-with-scratch-canvas :high (fn [canvas] (reset! received canvas) (throw (ex-info "boom" {}))))))
        (is (released?)))
      (testing "and when f returns"
        (is (= 42 (measure/call-with-scratch-canvas :high (fn [canvas] (reset! received canvas) 42))))
        (is (released?))))))

(deftest measuring-canvas-test
  (testing "scratch canvas carries the same Java rendering hints as a target canvas with that hint"
    (doseq [hint measure/rendering-hints]
      (is (= (:hints (c2d/canvas 800 600 hint)) (:hints (measure/scratch-canvas hint))) (str hint)))
    (is (not= (:hints (measure/scratch-canvas :low)) (:hints (measure/scratch-canvas :high)))))
  (testing "text measured during layout depends on the hint"
    (let [width (fn [hint] (get-in (pipeline/build (measured-plot {:kind ::text :text "0.123456789 Score: Golden Rain"})
                                                   {:hint hint})
                                   [:layout :columns 0 :size]))]
      (is (pos? (width :low)))
      (is (= (width :low) (width :mid)))
      (is (= (width :high) (width :highest))))))
