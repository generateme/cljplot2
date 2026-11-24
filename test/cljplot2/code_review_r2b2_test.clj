(ns cljplot2.code-review-r2b2-test
  "Code review round 2, batch B2 - the bar chart on empty data, chart styles resolved from the style schema
  ([[cljplot2.theme/resolved-style]]), the shared padding and golden angle, and the exposed constants: cloud
  projection theme keys and options, the venn label gap, the correlation glyph size."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [cljplot2.chart :as chart]
            [cljplot2.chart.line :as line]
            [cljplot2.core :as plot]
            [cljplot2.data.position :as position]
            [cljplot2.data.rows :as rows]
            [cljplot2.describe :as describe]
            [cljplot2.scale.coord :as coord]
            [cljplot2.schema :as schema]
            [cljplot2.test-util :refer [error]]
            [cljplot2.theme :as theme])
  (:import [java.awt.image BufferedImage]))

(defn- pixels
  "ARGB pixels of plot `p` rendered 300 x 240."
  [p]
  (let [^BufferedImage img (c2d/get-image (plot/render p {:width 300 :height 240}))]
    (vec (.getRGB img 0 0 300 240 nil 0 300))))

(deftest bar-empty-data-test
  (testing "boundary: no bars - a plot, no error (own empty data, empty plot data, no valid value)"
    (let [rows [{:c "a" :v 1} {:c "b" :v 2}]]
      (is (map? (plot/build {:data rows :mapping {:x :c :y :v} :charts [(chart/bar {:data []})]})))
      (is (map? (plot/build {:data [] :mapping {:x :c :y :v} :charts [(chart/bar)]})))
      (is (map? (plot/build {:data rows :mapping {:x :c :y :v} :charts [(chart/bar) (chart/bar {:data []})]}))))))

(deftest resolved-style-test
  (let [t theme/default-theme]
    (testing "a chart's style options over its theme style; options outside the style schema not taken"
      (let [st (theme/resolved-style t (chart/scatter {:size 11 :color :red :sort true}))]
        (is (= 11 (:size st)))
        (is (= :red (:color st)))
        (is (not (contains? st :sort)))
        (is (= (get-in t [:chart :scatter :shape]) (:shape st)))))
    (testing "the named :theme-style under the options"
      (let [st (theme/resolved-style t (chart/scatter {:theme-style :network-node :size 3}))]
        (is (= 3 (:size st)))
        (is (= (get-in t [:chart :network-node :color]) (:color st)))))
    (testing "every built-in kind takes each key of its style schema from its options"
      (doseq [[kind style] chart/builtin-styles
              :let [ks (schema/entry-keys style)
                    set-values (zipmap ks (map (fn [k] [::set k]) ks))
                    ch (@(get (describe/chart-constructors) kind) set-values)]]
        (is (= set-values (select-keys (theme/resolved-style t ch) ks)) (str kind))))
    (testing "boundary: no options - the theme style; a chart without a style schema - its theme style only"
      (is (= (theme/chart-style t :line {}) (theme/resolved-style t (chart/line))))
      (is (= {:a 1} (theme/resolved-style {:chart {:mine {:a 1}}} {:kind :mine :options {:a 2} :schema nil}))))))

(deftest entry-keys-test
  (is (= [:a :b] (schema/entry-keys [:map {:closed true} [:a int?] [:b {:optional true} int?]])))
  (is (= [:a] (schema/entry-keys [:map [:a int?]])))
  (is (= [] (vec (schema/entry-keys [:map {:closed true}]))) "boundary: no entries")
  (is (= () (schema/entry-keys nil)) "boundary: no schema"))

(deftest shared-constants-test
  (testing "the golden angle: beeswarm's former value, bit for bit"
    (is (= (* Math/PI (- 3.0 (Math/sqrt 5.0))) position/golden-angle)))
  (testing "default padding 5% each side of both axes"
    (is (= {:x [0.05 0.05] :y [0.05 0.05]} rows/default-padding))))

(def ^:private iris-like
  (vec (for [i (range 60)] {:a (mod (* 7 i) 13) :b (mod (* 5 i) 11) :c (mod (* 3 i) 7)})))

(def ^:private cloud-plot
  {:data iris-like :mapping {:x :a :y :b :z :c}
   :charts [(chart/cloud {:type :none :projections {:xy :density :xz :contour :yz :points}})]})

(deftest cloud-projection-test
  (testing "defaults: explicit default values draw the same"
    (is (= (pixels cloud-plot)
           (pixels (update-in cloud-plot [:charts 0 :options] merge {:projection-grid 50 :projection-levels 6}))))
    (is (= (pixels cloud-plot)
           (pixels (assoc cloud-plot :theme {:chart {:cloud {:projection-alpha-factor 0.35 :band-alpha-factor 0.15}}})))))
  (testing "each key changes the drawing"
    (is (not= (pixels cloud-plot) (pixels (update-in cloud-plot [:charts 0 :options] assoc :projection-grid 10))))
    (is (not= (pixels cloud-plot) (pixels (update-in cloud-plot [:charts 0 :options] assoc :projection-levels 2))))
    (is (not= (pixels cloud-plot) (pixels (assoc cloud-plot :theme {:chart {:cloud {:projection-alpha-factor 1.0}}}))))
    (is (not= (pixels cloud-plot) (pixels (assoc cloud-plot :theme {:chart {:cloud {:band-alpha-factor 0.6}}})))))
  (testing "boundary: grid 2..500, levels 1..100, alphas fractions; theme keys aren't options"
    (is (map? (plot/build (update-in cloud-plot [:charts 0 :options] merge {:projection-grid 2 :projection-levels 1}))))
    (doseq [[k v] [[:projection-grid 1] [:projection-grid 501] [:projection-grid 2.5] [:projection-levels 0] [:projection-levels 101]]]
      (is (= [:charts 0 :options k] (:in (error #(plot/build (update-in cloud-plot [:charts 0 :options] assoc k v))))) (str k " " v)))
    (doseq [[k v] [[:projection-alpha-factor 1.5] [:band-alpha-factor -0.1]]]
      (is (= [:theme :chart :cloud k] (:in (error #(plot/build (assoc cloud-plot :theme {:chart {:cloud {k v}}}))))) (str k " " v)))
    (is (= [:charts 0 :options :projection-alpha-factor]
           (:in (error #(plot/build (update-in cloud-plot [:charts 0 :options] assoc :projection-alpha-factor 0.5))))))))

(def ^:private venn-plot
  {:data (vec (concat (for [i (range 12)] {:s "a" :i i}) (for [i (range 6 18)] {:s "b" :i i})))
   :mapping {:set :s :item :i}
   :charts [(chart/venn)]})

(deftest venn-label-gap-test
  (is (= (pixels venn-plot) (pixels (assoc venn-plot :theme {:chart {:venn {:label-gap 4}}}))) "the default")
  (is (not= (pixels venn-plot) (pixels (assoc venn-plot :theme {:chart {:venn {:label-gap 20}}}))))
  (testing "boundary: 0 allowed; negative rejected in the theme; not an option"
    (is (map? (plot/build (assoc venn-plot :theme {:chart {:venn {:label-gap 0}}}))))
    (is (= [:theme :chart :venn :label-gap] (:in (error #(plot/build (assoc venn-plot :theme {:chart {:venn {:label-gap -1}}}))))))
    (is (= [:charts 0 :options :label-gap] (:in (error #(plot/build (assoc venn-plot :charts [(chart/venn {:label-gap 2})]))))))))

(def ^:private correlation-plot
  {:data iris-like :mapping {:x :a :y :b} :charts [(chart/correlation {:glyph :circle})]})

(deftest correlation-glyph-size-test
  (is (= (pixels correlation-plot) (pixels (assoc-in correlation-plot [:charts 0 :options :glyph-size] 0.9))) "the default")
  (is (not= (pixels correlation-plot) (pixels (assoc-in correlation-plot [:charts 0 :options :glyph-size] 0.5))))
  (testing "boundary: 0 and 1 allowed; outside [0 1] rejected"
    (is (map? (plot/build (assoc-in correlation-plot [:charts 0 :options :glyph-size] 0.0))))
    (is (map? (plot/build (assoc-in correlation-plot [:charts 0 :options :glyph-size] 1.0))))
    (doseq [v [1.5 -0.1]]
      (is (= [:charts 0 :options :glyph-size] (:in (error #(plot/build (assoc-in correlation-plot [:charts 0 :options :glyph-size] v)))))))))

(deftest style-rows-kind-test
  (is (= [[:projection-alpha-factor 0.35] [:band-alpha-factor 0.15]]
         (map (juxt :key :default) (describe/style-rows :cloud [:projection-alpha-factor :band-alpha-factor]))))
  (is (= [[:label-gap 4]] (map (juxt :key :default) (describe/style-rows :venn [:label-gap]))))
  (is (= :style-name (:key (error #(describe/style-rows :nope)))) "boundary: neither a named style nor a kind"))

(deftest arc-trail-widths-test
  (let [run-widths (fn [segment-points]
                     (with-redefs [coord/line-path (fn [_ pts _] (if (= 1 (count pts))
                                                                   [[0.0 0.0]]
                                                                   (mapv (fn [i] [(double i) 0.0]) (range segment-points))))]
                       (nth (#'line/arc-trail-run nil [:a :b] [1.0 5.0] [0 1]) 2)))]
    (testing "the width linear along a densified arc segment (fractions, not integer division)"
      (is (= [1.0 2.0 3.0 4.0 5.0] (run-widths 5))))
    (testing "boundary: a segment of one step - the end width"
      (is (= [1.0 5.0] (run-widths 2))))))

(deftest strip-charts-without-data-test
  (let [renders? (fn [p] (some? (plot/render p {:width 300 :height 200})))
        flat [{:c "a" :v 1} {:c "a" :v 1} {:c "b" :v 3}]]
    (testing "boundary: no data - an empty panel, no error (levels on :y and on :x; no levels)"
      (is (renders? {:data [] :mapping {:y :c :measures :v} :charts [(chart/bullet)]}) "bullet")
      (doseq [kind [:box :beeswarm :dotplot :stripplot :rug :violin :bar :extent]
              mapping [{:y :c :x :v} {:x :c :y :v} {:x :v}]
              :let [ch @(get (describe/chart-constructors) kind)]
              :when (not (and (= kind :bar) (= mapping {:x :v})))]
        (is (renders? {:data [] :mapping mapping :charts [(ch)]}) (str kind " " mapping))))
    (testing "boundary: data but nothing placeable - violins without spread, density ridges of constant levels"
      (is (renders? {:data flat :mapping {:y :c :x :v} :charts [(chart/violin)]}))
      (is (renders? {:data flat :mapping {:y :c :x :v} :charts [(chart/density)]})))
    (testing "an empty chart beside a drawn one leaves the drawn one"
      (is (renders? {:data flat :mapping {:y :c :x :v} :charts [(chart/box) (chart/violin)]})))))

(deftest tile-size-key-fill-test
  (let [p {:data (vec (for [x (range 4) y (range 3)] {:x x :y y :g (str "g" y) :s (+ x y)})) :mapping {:x :x :y :y :color :g :size :s} :charts [(chart/tile)]}]
    (is (= (pixels p) (pixels (assoc p :theme {:chart {:tile {:size-key-fill (c/gray 110)}}}))) "the default grey")
    (is (not= (pixels p) (pixels (assoc p :theme {:chart {:tile {:size-key-fill :red}}}))) "a theme colour fills the size keys")
    (is (= [:theme :chart :tile :size-key-fill] (:in (error #(plot/build (assoc p :theme {:chart {:tile {:size-key-fill "nope"}}}))))))
    (is (= [:charts 0 :options :size-key-fill] (:in (error #(plot/build (assoc p :charts [(chart/tile {:size-key-fill :red})]))))) "theme only")))
