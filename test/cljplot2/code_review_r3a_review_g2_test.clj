(ns cljplot2.code-review-r3a-review-g2-test
  "Code review round 3, batch A review fixes, cell / part-of-whole / 3D / text charts: all-invalid data in cloud and
  venn, pixel gaps `:gap` (pack, treemap) and text `:inset`, `:alpha-factor` multipliers, 3D primitive keys and the
  merged-frame distance error key."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [fastmath.vector :as v]
            [cljplot2.chart :as chart]
            [cljplot2.content.space3d :as space-draw]
            [cljplot2.core :as plot]
            [cljplot2.data.space3d :as space]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.test-util :refer [error error-data error-ins]]
            [cljplot2.theme :as theme]))

(def ^:private size {:width 300 :height 200})

(defn- build [p] (plot/build p size))

(def ^:private cube [{:x 0 :y 0 :z 0} {:x 1 :y 1 :z 1}])

(deftest cloud-all-invalid-test
  (testing "positions without one valid value throw {:key :mapping}"
    (doseq [rows [[{:x ##NaN :y 0 :z 0} {:x nil :y 1 :z 1}]
                  [{:x 0 :y 0 :z ##Inf}]]]
      (is (= :mapping (:key (error #(build {:data rows :mapping {:x :x :y :y :z :z} :charts [(chart/cloud)]})))))))
  (testing "no rows, one valid row and partly invalid rows build"
    (is (some? (build {:data [] :mapping {:x :x :y :y :z :z} :charts [(chart/cloud {:domain {:x [0 1] :y [0 1] :z [0 1]}})]})))
    (is (some? (build {:data [{:x 0 :y 0 :z 0}] :mapping {:x :x :y :y :z :z} :charts [(chart/cloud)]})))
    (let [b (build {:data (conj cube {:x ##NaN :y 0 :z 0}) :mapping {:x :x :y :y :z :z} :charts [(chart/cloud)]})]
      (is (= 1 (get-in b [:stats 0 :dropped :x]))))))

(deftest venn-all-invalid-test
  (testing "every :set (or :item) nil throws {:key :mapping}"
    (is (= :mapping (:key (error #(build {:data [{:s nil :i 1} {:s nil :i 2}] :mapping {:set :s :item :i} :charts [(chart/venn)]})))))
    (is (= :mapping (:key (error #(build {:data [{:s "a" :i nil}] :mapping {:set :s :item :i} :charts [(chart/venn)]}))))))
  (testing "no rows and some nil rows build"
    (is (some? (build {:data [] :mapping {:set :s :item :i} :charts [(chart/venn)]})))
    (is (= 1 (get-in (build {:data [{:s "a" :i 1} {:s "b" :i 1} {:s nil :i 2}] :mapping {:set :s :item :i} :charts [(chart/venn)]})
                     [:stats 0 :dropped :set])))))

(def ^:private tree-rows [{:a "a" :b "x" :v 3} {:a "a" :b "y" :v 2} {:a "b" :b "z" :v 4}])

(deftest pixel-gap-test
  (testing "pack and treemap take :gap (pixels, 0 allowed); :padding is no longer theirs"
    (doseq [kind [chart/pack chart/treemap]]
      (is (some? (build {:data tree-rows :mapping {:value :v :hierarchy [:a :b]} :charts [(kind {:gap 0})]})))
      (is (some? (build {:data tree-rows :mapping {:value :v :hierarchy [:a :b]} :charts [(kind {:gap 12})]})))
      (is (= :gap (:key (error #(build {:data tree-rows :mapping {:value :v :hierarchy [:a :b]} :charts [(kind {:gap -1})]})))))
      (is (contains? (error-ins #(build {:data tree-rows :mapping {:value :v :hierarchy [:a :b]} :charts [(kind {:padding 3})]}))
                     [:charts 0 :options :padding])))))

(defn- label-size
  "Natural size of a label content."
  [content]
  (measure/call-with-scratch-canvas
   nil
   (fn [cv] (let [^fastmath.vector.Vec2 size (measure/natural-size content {:canvas cv :theme theme/default-theme})]
              [(.x size) (.y size)]))))

(deftest text-inset-test
  (testing "a label's :inset adds pixels on every side; 0 - the text block only"
    (let [base {:kind :label :role :annotation :text "abc"}
          [^double w0 ^double h0] (label-size (assoc base :inset 0))
          [^double w5 ^double h5] (label-size (assoc base :inset 5))]
      (is (== (+ w0 10.0) w5))
      (is (== (+ h0 10.0) h5))))
  (testing "text roles and the text chart take :inset; :padding is rejected"
    (is (some? (build {:charts [(chart/text {:x [1] :y [1] :text ["a"] :inset 0})]})))
    (is (some? (build {:charts [(chart/text {:x [1] :y [1] :text ["a"]})] :theme {:text {:annotation {:inset 6}}}})))
    (is (= :inset (:key (error #(build {:charts [(chart/text {:x [1] :y [1] :text ["a"]})] :theme {:text {:annotation {:inset -1}}}})))))
    (is (some? (error #(build {:charts [(chart/text {:x [1] :y [1] :text ["a"] :padding 2})]}))))
    (is (some? (error #(build {:charts [(chart/text {:x [1] :y [1] :text ["a"]})] :theme {:text {:annotation {:padding 2}}}}))))))

(def ^:private grid-rows [{:x 0 :y 0 :v 1.0} {:x 1 :y 0 :v 2.0} {:x 0 :y 1 :v 3.0} {:x 1 :y 1 :v 4.0}])

(deftest alpha-factor-test
  (testing "multiplier charts take :alpha-factor in [0 1], not :alpha"
    (doseq [[label p kind] [["tile" {:data grid-rows :mapping {:x :x :y :y :color :v}} chart/tile]
                            ["bin2d" {:data grid-rows :mapping {:x :x :y :y}} chart/bin2d]
                            ["field" {:data grid-rows :mapping {:x :x :y :y}} chart/field]
                            ["contour" {:data grid-rows :mapping {:x :x :y :y}} chart/contour]]]
      (testing label
        (doseq [factor [0.0 0.5 1.0]]
          (is (some? (build (assoc p :charts [(kind {:alpha-factor factor})])))))
        (is (= :alpha-factor (:key (error #(build (assoc p :charts [(kind {:alpha-factor 1.5})]))))))
        (is (some? (error #(build (assoc p :charts [(kind {:alpha 0.5})])))))))))

(defn- polygon-pixel
  "Colour at (15, 15) of a 30x30 square primitive at (5, 5) with a 4 px outline."
  [stroke-color]
  (let [cv (c2d/canvas 40 40)]
    (c2d/with-canvas [cnv cv]
      (c2d/set-background cnv :white)
      (space-draw/draw-primitives! cnv [{:kind :polygon :depth 0 :fill :white :stroke-color stroke-color :stroke-width 4
                                         :points [(v/vec2 5 5) (v/vec2 35 5) (v/vec2 35 35) (v/vec2 5 35)]}]))
    (c/to-color (.getRGB ^java.awt.image.BufferedImage (c2d/get-image cv) 5 20))))

(deftest primitive-keys-test
  (testing "polygon outlines come from :stroke-color; :none draws none"
    (is (= (c/to-color :red) (polygon-pixel :red)))
    (is (= (c/to-color :white) (polygon-pixel :none))))
  (testing "line and path primitives take :line-width"
    (let [cv (c2d/canvas 20 20)]
      (c2d/with-canvas [cnv cv]
        (c2d/set-background cnv :white)
        (space-draw/draw-primitives! cnv [{:kind :line :depth 0 :from (v/vec2 0 10) :to (v/vec2 20 10) :color :black :line-width 6}]))
      (is (= (c/to-color :black) (c/to-color (.getRGB ^java.awt.image.BufferedImage (c2d/get-image cv) 10 8)))))))

(deftest merged-frame-distance-key-test
  (testing "the merged-frame check names the option holding the distance"
    (is (= {:key :view :in [:view :distance] :value 1.2} (select-keys (error-data #(space/check-distance! {:distance 1.2} [1 1 1])) [:key :in :value])))
    (is (nil? (space/check-distance! {:distance 0.2} [1 1 1])))
    (is (nil? (space/check-distance! {} nil)))))
