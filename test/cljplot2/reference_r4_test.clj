(ns cljplot2.reference-r4-test
  "Reference part R4 - chart pages: a chart's own rows and shared groups (`describe/own-rows`, `fragment-keys`),
  entries in a section (`schema/in-section`), the shared groups list (`nu/shared-groups`), the sections of the
  long chart tables; full-turn polar sectors without a seam, parallel axis labels without collisions; contour colours
  over the value range, the cartesian grid clipped to the panel."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [fastmath.core :as m]
            [cljplot2.chart :as chart]
            [cljplot2.chart.parallel]
            [cljplot2.core :as plot]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.scale.coord :as coord]
            [cljplot2.theme :as theme]
            [cljplot2.describe :as describe]
            [cljplot2.schema :as schema]
            [notebook-utils :as nu]
            [cljplot2.test-util :refer [error-data]]))

(def ^:private max-table-rows 12)

(deftest own-rows-test
  (let [rows (describe/options (chart/cloud))
        own (describe/own-rows rows)]
    (testing "a chart's rows without its shared groups' entries and their nested rows"
      (is (not-any? :fragment own))
      (is (some #(= [:projections :xy] (:path %)) own) "own nested rows kept")
      (is (not-any? #(= :view (first (:path %))) own) "a group's nested rows dropped")
      (is (= (remove (set own) rows) (filter #(some (fn [[_ ks]] ((set ks) (first (:path %))))
                                                     (describe/fragment-keys rows))
                                             rows))))
    (testing "boundary: no rows; only group rows; no groups"
      (is (= [] (describe/own-rows [])))
      (is (= [] (describe/own-rows (describe/fragment-rows :common))) "a group's table: every row in the group")
      (is (= [] (describe/own-rows (filter :fragment (describe/options (chart/ecdf))))))
      (is (= (describe/options (chart/image)) (describe/own-rows (describe/options (chart/image))))))))

(deftest fragment-keys-test
  (testing "groups in the order of their first row, each with the keys the rows hold"
    (is (= [[:line-style [:width :dash]] [:field [:f :domain :kernel :bandwidth :binned? :cut :interpolation :n]]
            [:common [:data :mapping]]]
           (describe/fragment-keys (describe/options (chart/contour))))))
  (testing "a chart takes part of a group: bar - 8 of the 9 strip entries"
    (let [strip (some (fn [[f ks]] (when (= :strip f) ks)) (describe/fragment-keys (describe/options (chart/bar))))]
      (is (= 8 (count strip)))
      (is (< (count strip) (count (describe/fragment-rows :strip))))))
  (testing "boundary: no rows, no groups"
    (is (= [] (describe/fragment-keys [])))
    (is (= [] (describe/fragment-keys (describe/options (chart/image)))))))

(deftest in-section-test
  (testing "entries with and without properties get the section; other properties kept"
    (is (= [[:a {:description "a" :cljplot2/section :s} :int] [:b {:cljplot2/section :s} :int]]
           (schema/in-section :s [[:a {:description "a"} :int] [:b :int]]))))
  (testing "boundary: no entries; a section replaced"
    (is (= [] (schema/in-section :s [])))
    (is (= [[:a {:cljplot2/section :t} :int]] (schema/in-section :t [[:a {:cljplot2/section :s} :int]])))))

(deftest chart-sections-test
  (testing "own options of every chart: one table of at most 12 rows, or every row in a section of at most 12"
    (doseq [[kind ctor] (describe/chart-constructors)
            rows [(describe/own-rows (describe/options (ctor))) (describe/mapping (ctor))]
            :let [sections (keep :section rows)]]
      (if (empty? sections)
        (is (<= (count rows) max-table-rows) kind)
        (do (is (every? :section rows) kind)
            (doseq [[s n] (frequencies sections)] (is (<= n max-table-rows) [kind s]))))))
  (testing "the point style of scatter is unsliced, a section of cloud / qq / pp"
    (is (every? nil? (map :section (describe/own-rows (describe/options (chart/scatter))))))
    (doseq [c [(chart/cloud) (chart/qq) (chart/pp)]]
      (is (= :style (:section (first (filter #(= [:shape] (:path %)) (describe/options c))))) (:kind c)))))

(deftest shared-groups-test
  (let [anchor #'notebook-utils/heading-anchor]
    (testing "heading ids as Quarto makes them"
      (is (= "line-style" (anchor "Line style")))
      (is (= "d-frame" (anchor "3D frame")))
      (is (= "field-a-function" (anchor "Field: a function")))
      (is (= "node-link-tree-layout" (anchor "Node-link tree layout"))))
    (testing "each group linked to its Shared options section with the keys the chart takes; code hidden"
      (let [md (nu/shared-groups (describe/options (chart/line)))
            text (first md)]
        (is (= :kind/md (:kindly/kind (meta md))))
        (is (:kindly/hide-code (meta md)))
        (is (str/includes? text "[Line style](reference.shared_options.html#line-style) (`:color` `:width` `:dash`)"))
        (is (str/includes? text "[Curves](reference.shared_options.html#curves)"))))
    (testing "boundary: rows without groups throw {:key :rows}"
      (is (= :rows (:key (error-data #(nu/shared-groups (describe/options (chart/image)))))))
      (is (= :rows (:key (error-data #(nu/shared-groups []))))))))

(deftest full-turn-test
  (let [full {:kind :polar :theta [0.0 m/TWO_PI] :radius 100.0 :cx 100.0 :cy 100.0}
        half (assoc full :theta [0.0 m/PI])]
    (testing "a sector once around a whole circle; not a part of it, not on a partial circle"
      (is (coord/full-turn? full 0.0 m/TWO_PI))
      (is (coord/full-turn? full m/TWO_PI 0.0) "reversed")
      (is (not (coord/full-turn? full 0.0 m/PI)))
      (is (not (coord/full-turn? half 0.0 m/PI)) "the whole of a half circle has radial edges"))
    (testing "its outline: the outer circle, and the inner one for a ring"
      (is (= 1 (count (coord/full-turn-circles full 0.0 0.0 m/TWO_PI 50.0))))
      (is (= 2 (count (coord/full-turn-circles full 0.0 20.0 m/TWO_PI 50.0)))))))

(deftest parallel-axis-labels-test
  (let [axis-labels #'cljplot2.chart.parallel/axis-labels
        tick (fn [text x] {:label [{:kind :label :role :tick-label :text text} [:left :middle] x 50.0]})
        kept (fn [placed] (measure/call-with-scratch-canvas
                           :high #(mapv (comp :text first) (axis-labels % placed {:theme theme/default-theme}))))]
    (testing "labels of all axes: one overlapping a label kept before it dropped (radar 0 labels at the centre)"
      (is (= ["0" "5"] (kept [{:ticks [(tick "0" 50.0) (tick "5" 150.0)]} {:ticks [(tick "0" 50.0)]}]))))
    (testing "boundary: no axes; axes without ticks; labels apart all kept"
      (is (= [] (kept [])))
      (is (= [] (kept [{:ticks []}])))
      (is (= ["a" "b"] (kept [{:ticks [(tick "a" 0.0)]} {:ticks [(tick "b" 200.0)]}]))))))

(deftest contour-color-domain-test
  (let [domain (fn [opts] (get-in (plot/build {:charts [(chart/contour (merge {:f (fn [[x y]] (+ (double x) (double y)))
                                                                               :domain {:x [0 1] :y [0 1]}}
                                                                              opts))]}
                                              {:width 300 :height 300})
                                  [:scale-specs :color :domain]))]
    (testing "lines coloured over the field's value range - the lowest level isn't the gradient's end"
      (is (= [0.0 2.0] (domain {:levels 4})))
      (is (= [0.0 2.0] (domain {:levels 4 :fill true})) "filled bands alike"))
    (testing "boundary: given levels outside the value range widen it; one level"
      (is (= [-1.0 3.0] (domain {:levels [-1 1 3]})))
      (is (= [0.0 2.0] (domain {:levels [1]}))))))

(deftest grid-clipped-to-panel-test
  (testing "a grid line at a tick on the panel's right edge stays inside the panel (none beside a field's raster)"
    (let [width 300
          field (chart/field {:f (fn [[x y]] (+ (double x) (double y))) :domain {:x [0 4] :y [0 4]}})
          img (plot/render-image {:charts [field] :legend false} {:width width :height 300})
          rgb (fn [x y] (bit-and 0xffffff (.getRGB ^java.awt.image.BufferedImage img (int x) (int y))))
          raster? (fn [x y] (< (bit-shift-right (rgb x y) 16) 100))
          edge (first (filter #(not (raster? % 150)) (range 150 width)))]
      (is (some? edge))
      (is (every? #(= (rgb (+ 3 edge) %) (rgb edge %)) (range 40 240))
          "the column past the raster is plain background"))))
