(ns cljplot2.batch-c1-fix-x1-test
  "Batch C1 review fixes (fork x1): area fills faded by `:alpha`, theme `[:legend :min-key-line-width]` raising only
  positive widths, cloud `:line-width` for spikes and lines, a legend no chart draws into dropped, `:alpha` of the
  shared point style."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [fastmath.vector :as v]
            [cljplot2.chart :as chart]
            [cljplot2.chart.cloud :as cloud]
            [cljplot2.chart.legend :as chart-legend]
            [cljplot2.content.legend :as legend]
            [cljplot2.core :as plot]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.theme :as theme]
            [cljplot2.test-util :refer [error-ins same-pixels? pixels]]))

(def ^:private options {:width 300 :height 220})

(defn- same-colors? [a b] (= (c/to-color a) (c/to-color b)))

(def ^:private rows
  (vec (for [i (range 24)] {:x (mod (* i 7) 11) :y (mod (* i 5) 9) :z (mod i 4) :g (["a" "b" "c"] (mod i 3))})))

;; D1 area fills: the colour faded by :alpha

(deftest area-color-test
  (testing ":fill :color - the colour's own alpha times :alpha"
    (is (same-colors? [255 0 0 127.5] (polyline/area-color {:color [255 0 0] :alpha 0.5 :fill :color} false)))
    (is (same-colors? [255 0 0 64] (polyline/area-color {:color [255 0 0 128] :alpha 0.5 :fill :color} false)))
    (is (same-colors? [255 0 0 0] (polyline/area-color {:color [255 0 0] :alpha 0 :fill :color} false)) "zero")
    (is (same-colors? [255 0 0 128] (polyline/area-color {:color [255 0 0 128] :alpha 1 :fill :color} false)) "one"))
  (testing "nil :alpha keeps the colour; :none stays :none"
    (is (same-colors? [255 0 0 128] (polyline/area-color {:color [255 0 0 128] :fill :color} false)))
    (is (= :none (polyline/area-color {:color :none :alpha 0.5 :fill :color} false))))
  (testing "stacked without a fill fades too; a given fill colour is as given; no fill - none"
    (is (same-colors? [0 0 255 51] (polyline/area-color {:color [0 0 255 102] :alpha 0.5} true)))
    (is (= [1 2 3] (polyline/area-color {:color [0 0 255] :alpha 0.5 :fill [1 2 3]} false)))
    (is (= :none (polyline/area-color {:color [0 0 255] :alpha 0.5} false))))
  (testing "every area chart: a half-transparent colour at :alpha 1 fills as the colour itself"
    (doseq [[mk extra] [[chart/density {}] [chart/line {}] [chart/smooth {}]]]
      (is (same-pixels? {:data rows :mapping {:x :x :y :y} :charts [(mk (merge {:color [200 0 0 100] :fill :color :alpha 1} extra))]}
                        {:data rows :mapping {:x :x :y :y} :charts [(mk (merge {:color [200 0 0 100] :fill [200 0 0 100] :alpha 1} extra))]}
                        options)))
    (is (same-pixels? {:data rows :mapping {:x :x :ymin :y :ymax :z} :charts [(chart/ribbon {:color [0 0 200 128] :alpha 0.5})]}
                      {:data rows :mapping {:x :x :ymin :y :ymax :z} :charts [(chart/ribbon {:color [0 0 200 128] :fill [0 0 200 64]})]}
                      options))))

;; D6 / P1 legend key line width

(deftest min-key-line-width-test
  (let [env {:theme theme/default-theme}]
    (testing "an area chart without outline keeps no outline in its key; thin outlines are raised"
      (is (zero? (:line-width (theme/key-style env (chart/density {:fill :color :line-width 0}) {} nil))))
      (is (= 2 (:line-width (theme/key-style env (chart/density {:fill :color :line-width 1}) {} nil)))))
    (testing "the old name is a theme error, the new one is checked"
      (is (seq (error-ins #(plot/build {:data rows :mapping {:x :x :y :y} :charts [(chart/line)]
                                        :theme {:legend {:key-line-width 3}}} options))))
      (is (seq (error-ins #(plot/build {:data rows :mapping {:x :x :y :y} :charts [(chart/line)]
                                        :theme {:legend {:min-key-line-width "2"}}} options))))
      (is (some? (plot/render-image {:data rows :mapping {:x :x :y :y :color :g} :charts [(chart/density {:fill :color :line-width 0})]
                                     :theme {:legend {:min-key-line-width 4}}} options))))))

;; D7 / P5 cloud :line-width

(defn- cloud-lines [cloud-options]
  (let [p {:data rows :mapping {:x :x :y :y :z :z} :charts [(chart/cloud cloud-options)]}
        b (plot/build p options)
        computed (-> b :stats first :data vals first)
        project (fn [[x y z]] [(v/vec2 (double x) (double y)) (double z)])]
    (filterv #(= :line (:kind %)) (cloud/chart-primitives nil computed project (:finalized-scales b) {:theme (:theme b)}))))

(deftest cloud-line-width-test
  (testing "spikes and lines take :line-width (theme default 1); :stroke-width doesn't touch them"
    (doseq [t [:spikes :lines]]
      (is (every? #(== 1.0 (double (:line-width %))) (cloud-lines {:type t}))))
    (is (every? #(== 3.0 (double (:line-width %))) (cloud-lines {:type :spikes :line-width 3 :stroke-width 7})))
    (is (every? #(== 0.0 (double (:line-width %))) (cloud-lines {:type :lines :line-width 0}))) "zero")
  (testing "the theme sets it"
    (is (= 1 (get-in theme/default-theme [:chart :cloud :line-width]))))
  (testing "keys raise it like other line marks"
    (is (= 2 (:line-width (theme/key-style {:theme theme/default-theme} (chart/cloud {:type :spikes}) {} nil)))))
  (testing "a wrong value is a schema error"
    (is (contains? (error-ins #(plot/build {:data rows :mapping {:x :x :y :y :z :z} :charts [(chart/cloud {:type :lines :line-width -1})]} options))
                   [:charts 0 :options :line-width]))))

;; D5 / P6 a legend no chart draws into is dropped

(deftest dropped-legend-test
  (let [base {:data rows :mapping {:x :x :y :y :color :g}}
        built-legends #(legend/legends (plot/build % options))]
    (testing "one chart with :legend false: no legend, the image as with the scale's :legend false"
      (is (empty? (built-legends (assoc base :charts [(chart/scatter {:legend false})]))))
      (is (same-pixels? (assoc base :charts [(chart/scatter {:legend false})])
                        (assoc base :charts [(chart/scatter)] :scales {:color {:legend false}}) options)))
    (testing "two charts, one keys: the legend stays"
      (is (= 1 (count (built-legends (assoc base :charts [(chart/scatter {:legend false}) (chart/line)]))))))
    (testing "the colour bar stays (no keys to draw)"
      (is (= [:colorbar] (mapv :kind (built-legends {:data rows :mapping {:x :x :y :y :color :z}
                                                      :charts [(chart/scatter {:legend false})]})))))
    (testing "provisional legends agree"
      (let [p (assoc base :charts [(chart/scatter {:legend false})])
            inputs [{:mapping (:mapping base) :groups {:all rows}}]]
        (is (empty? (legend/provisional-legends p inputs)))
        (is (= 1 (count (legend/provisional-legends (assoc p :charts [(chart/scatter)]) inputs))))))
    (testing "facets and flip keep the rule"
      (is (empty? (built-legends (assoc base :flip true :mapping {:x :x :y :y :color :g :facet :g}
                                        :charts [(chart/line {:legend false})])))))))

;; D4 :alpha of the shared point style

(defn- key-pixels [chart-value key-color]
  (let [canvas (c2d/canvas 30 30)]
    (c2d/with-canvas [cv canvas]
      (c2d/set-background cv :white)
      ((:legend chart-value) chart-value {:color key-color} cv {:x 0 :y 0 :w 30 :h 30} {:theme theme/default-theme}))
    (vec (pixels (c2d/get-image canvas)))))

(deftest point-style-alpha-test
  (testing "the theme sets scatter's :alpha as the option does"
    (is (same-pixels? {:data rows :mapping {:x :x :y :y} :charts [(chart/scatter {:size 20})] :theme {:chart {:scatter {:alpha 0.3}}}}
                      {:data rows :mapping {:x :x :y :y} :charts [(chart/scatter {:size 20 :alpha 0.3})]} options))
    (is (nil? (get-in theme/default-theme [:chart :scatter :alpha])) "no theme default"))
  (testing "scatter uses the shared key glyph, which applies :alpha"
    (is (= chart-legend/stroked-point-legend (:legend (chart/scatter))))
    (is (= (key-pixels (chart/scatter {:alpha 0.5}) (c/color 255 0 0)) (key-pixels (chart/scatter) (c/color 255 0 0 127.5))))
    (is (= (key-pixels (chart/qq {:alpha 0.5}) (c/color 255 0 0)) (key-pixels (chart/qq) (c/color 255 0 0 127.5))) "qq"))
  (testing "cloud and qq points take it"
    (is (same-pixels? {:data rows :mapping {:x :x :y :y :z :z} :legend false :charts [(chart/cloud {:color [255 0 0] :alpha 0.5})]}
                      {:data rows :mapping {:x :x :y :y :z :z} :legend false :charts [(chart/cloud {:color [255 0 0 127.5]})]} options))
    (is (= (key-pixels (chart/cloud {:alpha 0.5}) (c/color 255 0 0)) (key-pixels (chart/cloud) (c/color 255 0 0 127.5))) "cloud key")
    (is (not (same-pixels? {:data rows :mapping {:x :x :y :y} :charts [(chart/qq {:size 10 :alpha 0.2})]}
                           {:data rows :mapping {:x :x :y :y} :charts [(chart/qq {:size 10})]} options))))
  (testing "nil keeps the colours' own alpha; a wrong value is an error"
    (is (same-pixels? {:data rows :mapping {:x :x :y :y} :charts [(chart/qq {:alpha nil})]}
                      {:data rows :mapping {:x :x :y :y} :charts [(chart/qq)]} options))
    (is (contains? (error-ins #(plot/build {:data rows :mapping {:x :x :y :y :z :z} :charts [(chart/cloud {:alpha 1.5})]} options))
                   [:charts 0 :options :alpha]))))
