(ns cljplot2.batch-c2-fix-y1-test
  "Batch C2 review fixes (fork y1): one least size of fitted texts (the text role's `:min-size`), `fitted-label`
  `:cut`, text `:x-span` (along x only, non-negative), tick label gaps (theme `[:axis k :tick-label-gap]`)."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [cljplot2.chart :as chart]
            [cljplot2.content.label :as label]
            [cljplot2.core :as plot]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.theme :as theme]
            [cljplot2.test-util :refer [error error-ins]])
  (:import [java.awt.image BufferedImage]))

(def ^:private size {:width 300 :height 200})

(defn- pixels
  "Gives the pixels of a rendered plot."
  [p]
  (let [^BufferedImage img (plot/render-image p size)
        w (.getWidth img) h (.getHeight img)]
    (vec (.getRGB img 0 0 w h nil 0 w))))

(defn- with-env
  "Calls `f` with a measuring env over the theme with `overrides`."
  [overrides f]
  (measure/call-with-scratch-canvas :high (fn [c] (f {:canvas c :theme (theme/plot-theme overrides)}))))

(defn- fit
  "Gives `content` fitted with `fit-options` under the theme overrides."
  [content fit-options overrides]
  (with-env overrides #(label/fitted-label content fit-options %)))

(def ^:private long-text "a rather long label text")

;; one least size: the text role's :min-size

(deftest fitted-label-min-size-test
  (let [c {:kind :label :role :annotation :text long-text}]
    (testing "the role's :min-size is the least size; cut by default"
      (let [f (fit c {:length 30.0} nil)]
        (is (= 6.0 (double (:size f))))
        (is (str/ends-with? (:text f) "…"))))
    (testing ":cut false keeps the text whole at the least size"
      (let [f (fit c {:length 30.0 :cut false} nil)]
        (is (= long-text (:text f)))
        (is (= 6.0 (double (:size f))))))
    (testing "a fitting text is unchanged, also with :cut false"
      (is (= c (fit c {:length 1000.0 :cut false} nil))))
    (testing "a slightly long text only shrinks (rounded down), whole"
      (let [n (with-env nil #(label/along-text c %))
            f (fit c {:length (* 0.9 n) :cut false} nil)]
        (is (= long-text (:text f)))
        (is (< 6.0 (double (:size f)) 10.0))))
    (testing "a role without :min-size isn't shrunk"
      (let [plain (dissoc (get-in theme/default-theme [:text :annotation]) :min-size)
            f (fit (assoc c :role :plain) {:length 30.0 :cut false} {:text {:plain plain}})]
        (is (= 10.0 (double (:size f))))
        (is (= long-text (:text f)))))
    (testing "a :min-size above the size keeps the size"
      (is (= 10.0 (double (:size (fit c {:length 30.0 :cut false} {:text {:annotation {:min-size 14}}}))))))
    (testing "a content :size scales from itself"
      (is (= 6.0 (double (:size (fit (assoc c :size 20) {:length 30.0 :cut false} nil))))))
    (testing "zero and negative lengths: the least size"
      (is (= 6.0 (double (:size (fit c {:length 0.0 :cut false} nil)))))
      (is (= 6.0 (double (:size (fit c {:length -5.0 :cut false} nil))))))))

(deftest default-min-sizes-test
  (testing "default theme least sizes"
    (is (= 8 (get-in theme/default-theme [:text :axis-title :min-size])))
    (is (= 7 (get-in theme/default-theme [:text :strip :min-size])))
    (is (= 6 (get-in theme/default-theme [:text :annotation :min-size])))))

(deftest axis-title-role-min-size-test
  (let [c {:kind :label :role :axis-title :text "averyveryverylongsinglewordwithoutanyspace" :fit true}
        sized (fn [overrides] (with-env overrides #(measure/natural-size c (assoc % :box {:x 0 :y 0 :w 100 :h 10}))))]
    (testing "a larger role :min-size gives a larger fitted title"
      (is (> (.y ^fastmath.vector.Vec2 (sized {:text {:axis-title {:min-size 11}}}))
             (.y ^fastmath.vector.Vec2 (sized nil))))))
  (testing "the removed keys are theme errors"
    (let [base {:data [{:x 1 :y 1 :g "a"}] :mapping {:x :x :y :y :facet :g} :charts [(chart/scatter)]}]
      (is (seq (error-ins #(plot/build (assoc base :theme {:axis {:min-title-size 8}}) size))))
      (is (seq (error-ins #(plot/build (assoc base :theme {:strip {:min-text-size 7}}) size))))
      (is (map? (plot/build (assoc base :theme {:text {:strip {:min-size 9} :axis-title {:min-size 9}}}) size)))))
  (testing ":fit is a flag"
    (is (some? (error #(measure/validate-content {:kind :label :role :axis-title :text "t" :fit [:axis]}))))))

;; text :x-span

(defn- text-plot
  "Gives a plot of labels at `rows` with text options `options` and plot keys `extra`."
  ([rows options] (text-plot rows options nil))
  ([rows options extra]
   (merge {:data rows
           :charts [(chart/text (merge {:mapping (cond-> {:x :x :y :y :label :label}
                                                   (some #(contains? % :span) rows) (assoc :x-span :span))}
                                       options))]
           :scales {:x {:domain [0 10]} :y {:domain [0 10]}}}
          extra)))

(def ^:private one [{:x 1 :y 5 :label long-text}])
(defn- spanned [span] (mapv #(assoc % :span span) one))

(deftest x-span-test
  (testing "a tight span shrinks the default :annotation role; a roomy one keeps it"
    (is (not= (pixels (text-plot one {})) (pixels (text-plot (spanned 0.5) {}))))
    (is (= (pixels (text-plot one {})) (pixels (text-plot (spanned 9.0) {})))))
  (testing "zero span: the least size, as any tight span"
    (is (= (pixels (text-plot (spanned 0) {})) (pixels (text-plot (spanned 0.1) {})))))
  (testing "nil and non-finite spans aren't fitted"
    (doseq [span [nil ##NaN ##Inf "wide"]]
      (is (= (pixels (text-plot one {})) (pixels (text-plot (spanned span) {}))) (pr-str span))))
  (testing "only text along x is fitted: rotated, mapped angle, flipped, polar - not"
    (is (= (pixels (text-plot one {:rotation 90})) (pixels (text-plot (spanned 0.5) {:rotation 90}))))
    (is (= (pixels (text-plot one {:rotation 45})) (pixels (text-plot (spanned 0.5) {:rotation 45}))))
    (let [angled (mapv #(assoc % :a 90) one)]
      (is (= (pixels (text-plot angled {:mapping {:x :x :y :y :label :label :angle :a}}))
             (pixels (text-plot (mapv #(assoc % :span 0.5) angled) {:mapping {:x :x :y :y :label :label :angle :a :x-span :span}})))))
    (is (= (pixels (text-plot one {} {:flip true})) (pixels (text-plot (spanned 0.5) {} {:flip true}))))
    (is (= (pixels (text-plot one {} {:coord {:kind :polar}})) (pixels (text-plot (spanned 0.5) {} {:coord {:kind :polar}})))))
  (testing "a mapped angle of 0 is along x: fitted"
    (let [angled (mapv #(assoc % :a 0) one)]
      (is (not= (pixels (text-plot angled {:mapping {:x :x :y :y :label :label :angle :a}}))
                (pixels (text-plot (mapv #(assoc % :span 0.5) angled) {:mapping {:x :x :y :y :label :label :angle :a :x-span :span}}))))))
  (testing "facets: each panel's labels fitted"
    (let [rows [{:x 1 :y 5 :label long-text :f "a"} {:x 1 :y 5 :label long-text :f "b"}]]
      (is (not= (pixels (text-plot rows {} {:mapping {:facet :f}}))
                (pixels (text-plot (mapv #(assoc % :span 0.5) rows) {} {:mapping {:facet :f}}))))))
  (testing "a negative span is a mapping error"
    (is (= {:key :mapping :in [:mapping :x-span] :value -1.0}
           (select-keys (error #(plot/build (text-plot (spanned -1.0) {}) size)) [:key :in :value])))))

;; tick label gaps

(deftest spans-overlap-test
  (testing "gap 0: up to 1 px overlap allowed"
    (is (false? (measure/spans-overlap? 10.0 10.0 10.0 0.0)) "touching")
    (is (false? (measure/spans-overlap? 9.0 10.0 10.0 0.0)) "1 px")
    (is (true? (measure/spans-overlap? 8.5 10.0 10.0 0.0))))
  (testing "a gap: closer than gap - 1 px overlaps; any sign of distance"
    (is (true? (measure/spans-overlap? 10.0 10.0 10.0 3.0)) "touching labels overlap")
    (is (false? (measure/spans-overlap? 12.0 10.0 10.0 3.0)) "gap - 1 px apart")
    (is (true? (measure/spans-overlap? -11.5 10.0 10.0 3.0)))))

(deftest kept-label-indices-gap-test
  (is (= [0 1 2] (measure/kept-label-indices [0 10 20] [10 10 10])) "default gap 0: touching kept")
  (is (= [0 2] (measure/kept-label-indices [0 10 20] [10 10 10] 3)) "touching thinned with a gap")
  (is (= [0 1] (measure/kept-label-indices [0 12] [10 10] 3)))
  (is (= [] (measure/kept-label-indices [] [] 3)))
  (is (= [0] (measure/kept-label-indices [5] [100] 3))))

(deftest tick-label-gap-theme-test
  (let [base {:data (vec (for [i (range 50)] {:x (* 0.37 i) :y (mod i 7)})) :mapping {:x :x :y :y}
              :charts [(chart/scatter)] :scales {:x {:ticks 25}}}]
    (testing "defaults"
      (is (= 3 (get-in theme/default-theme [:axis :x :tick-label-gap])))
      (is (= 3 (get-in theme/default-theme [:axis :y :tick-label-gap]))))
    (testing "a larger gap draws fewer labels; flipped and faceted plots too"
      (is (not= (pixels (assoc base :theme {:axis {:x {:tick-label-gap 0}}}))
                (pixels (assoc base :theme {:axis {:x {:tick-label-gap 30}}}))))
      (is (map? (plot/build (assoc base :flip true :theme {:axis {:x {:tick-label-gap 30}}}) size)))
      (is (map? (plot/build (-> base (assoc-in [:mapping :facet] #(mod (:y %) 2)) (assoc :theme {:axis {:x {:tick-label-gap 30}}})) size))))
    (testing "wrong values are theme errors"
      (is (seq (error-ins #(plot/build (assoc base :theme {:axis {:x {:tick-label-gap -1}}}) size))))
      (is (seq (error-ins #(plot/build (assoc base :theme {:axis {:y {:tick-label-gap "3"}}}) size)))))
    (testing "stacked labels (a vertical axis, unrotated) take no gap; at -90 they lie end to end"
      (let [vertical (assoc base :scales {:y {:ticks 25}})]
        (is (= (pixels (assoc vertical :theme {:axis {:y {:tick-label-gap 0}}}))
               (pixels (assoc vertical :theme {:axis {:y {:tick-label-gap 30}}}))))
        (is (not= (pixels (assoc vertical :theme {:axis {:y {:tick-label-gap 0 :label-rotation -90}}}))
                  (pixels (assoc vertical :theme {:axis {:y {:tick-label-gap 30 :label-rotation -90}}}))))
        (is (= (pixels (assoc base :theme {:axis {:x {:tick-label-gap 0 :label-rotation -45}}}))
               (pixels (assoc base :theme {:axis {:x {:tick-label-gap 30 :label-rotation -45}}})))
            "rotated by another angle: no gap")))))
