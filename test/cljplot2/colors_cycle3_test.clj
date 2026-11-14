(ns cljplot2.colors-cycle3-test
  "Colors cycle 3: position scale `:axis false`, quantile and method bins, `arrange` option validation."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.string :as str]
            [cljplot2.test-util :refer [error error-data]]
            [fastmath.stats :as stats]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.levels :as levels]
            [cljplot2.layout.grid :as grid])
  (:import [java.awt.image BufferedImage]))

(def ^:private size {:width 400 :height 300})
(defn- build [p] (plot/build p size))
(defn- item-ids [built] (set (map :id (get-in built [:grid :items]))))

(def ^:private corners [{:x 0 :y 0 :g "a"} {:x 10 :y 10 :g "b"}])
(defn- scatter-plot [scales] {:data corners :mapping {:x :x :y :y} :scales scales :charts [(chart/scatter)]})

;; 1. axis visibility

(deftest axis-visibility-test
  (testing "1.1 no axis items for a scale with :axis false; its track takes no room"
    (let [b (build (scatter-plot {:y {:axis false}}))]
      (is (not-any? #{:y-axis :y-axis-start :y-axis-end} (item-ids b)))
      (is (contains? (item-ids b) :x-axis))
      (is (< (long (:x (grid/item-box (:layout b) :panel)))
             (long (:x (grid/item-box (:layout (build (scatter-plot {}))) :panel))))
          "the panel starts further left without the y axis")))
  (testing "1.1 under :flip the axis follows the scale (x drawn on the left)"
    (let [b (build (assoc (scatter-plot {:x {:axis false}}) :flip true))]
      (is (not (contains? (item-ids b) :y-axis)) "left side (the x scale) gone")
      (is (contains? (item-ids b) :x-axis) "bottom side (the y scale) kept")))
  (testing "1.2 grid lines go with the axis: a panel row between the points is one colour"
    (let [row-colors (fn [p]
                       (let [img ^BufferedImage (plot/render-image p size)
                             {:keys [x y w h]} (grid/item-box (:layout (build p)) :panel)
                             py (+ (long y) (quot (long h) 2) 3)]
                         (count (distinct (for [px (range (+ (long x) 20) (- (+ (long x) (long w)) 20))] (.getRGB img (int px) (int py)))))))]
      (is (> (row-colors (scatter-plot {})) 1) "default: vertical grid lines cross the row")
      (is (= 1 (row-colors (scatter-plot {:x {:axis false} :y {:axis false}}))))))
  (testing "1.3 axis titles stay"
    (is (contains? (item-ids (build (assoc (scatter-plot {:x {:axis false}}) :labels {:x "X"}))) :x-title)))
  (testing "1.4 facets: every panel drops the axis"
    (let [b (build {:data corners :mapping {:x :x :y :y :facet-col :g} :scales {:x {:axis false}} :charts [(chart/scatter)]})]
      (is (not-any? #(and (vector? %) (= :x-axis (first %))) (item-ids b)))
      (is (some #(and (vector? %) (= :y-axis (first %))) (item-ids b)))))
  (testing "1.5 rejected on aesthetic scales; must be a boolean"
    (is (= :axis (:key (error #(build {:data corners :mapping {:x :x :y :y :color :g} :scales {:color {:axis false}}
                                       :charts [(chart/scatter)]})))))
    (is (= :axis (:key (error #(build (scatter-plot {:x {:axis "no"}})))))))
  (testing "1.6 :axis true is the default"
    (is (= (item-ids (build (scatter-plot {}))) (item-ids (build (scatter-plot {:x {:axis true} :y {:axis true}}))))))
  (testing "1.7 plot margins keep their own axis"
    (let [b (build {:data corners :mapping {:x :x :y :y} :scales {:x {:axis false}}
                    :margins {:top {:charts [(chart/density)] :axis true}} :charts [(chart/scatter)]})]
      (is (contains? (item-ids b) :top-margin-axis))
      (is (not (contains? (item-ids b) :x-axis))))))

;; 2. quantile and method bins

(def ^:private hundred (mapv double (range 1 101)))

(deftest quantile-breaks-test
  (testing "2.1 equal-probability bins, inner probabilities, estimation"
    (is (= (stats/quantiles hundred [0 0.25 0.5 0.75 1.0]) (levels/breaks hundred {:quantiles 4})))
    (is (= (stats/quantiles hundred [0 0.1 0.9 1.0] :r7) (levels/breaks hundred {:quantiles [0.1 0.9] :estimation :r7})))
    (is (= [1.0 100.0] (levels/breaks hundred {:quantiles 1})) "one bin"))
  (testing "2.1 invalid quantile bins throw :bins"
    (doseq [bad [{:quantiles 0} {:quantiles -2} {:quantiles []} {:quantiles [0.5 0.2]} {:quantiles [0 0.5]}
                 {:quantiles [1.0]} {:quantiles 4 :estimation :r10} {:estimation :r7} {:quantiles 4 :zzz 1}]]
      (is (= :bins (:key (error #(levels/breaks hundred bad)))) (pr-str bad))))
  (testing "2.2 tied breaks merge; a single value gets one bin"
    (is (= [1.0 2.0] (levels/breaks [1.0 1 1 1 2] {:quantiles 4 :estimation :r1})))
    (is (= [2.5 3.5] (levels/breaks [3.0 3.0 3.0] {:quantiles 4})))
    (is (nil? (levels/breaks [] {:quantiles 4})) "no values"))
  (testing "2.3 levels/bin with quantile bins: about equal counts"
    (let [rows (mapv (fn [v] {:v v}) hundred)
          counts (vals (frequencies (map :q (levels/bin rows :v {:as :q :bins {:quantiles 4}}))))]
      (is (= 4 (count counts)))
      (is (every? #(<= 24 % 26) counts))))
  (testing "W2 histograms reject quantile bins"
    (is (= :bins (:key (error #(build {:data corners :mapping {:x :x} :charts [(chart/histogram {:bins {:quantiles 4}})]})))))
    (is (some? (build {:data corners :mapping {:x :x} :charts [(chart/histogram {:bins :sturges})]})) "methods still fine")))

(def ^:private prices (mapv (fn [i] {:x i :y i :p (* i i)}) (range 1 201)))
(defn- colorbar [built] (some #(when (= :legends (:id %)) (first (get-in % [:content :legends]))) (get-in built [:grid :items])))
(defn- binned-plot [data scale] {:data data :mapping {:x :x :y :y :color :p} :scales {:color (merge {:kind :binned} scale)}
                                 :charts [(chart/scatter)]})

(deftest binned-methods-test
  (testing "2.4 quantile bins from the data values"
    (let [b (build (binned-plot prices {:bins {:quantiles 4}}))]
      (is (= (vec (distinct (stats/quantiles (map (comp double :p) prices) [0 0.25 0.5 0.75 1.0])))
             (get-in b [:scale-specs :color :breaks])))
      (is (= 3 (count (:labels (colorbar b)))))))
  (testing "2.4 a method keyword: that many equal bins"
    (let [b (build (binned-plot prices {:bins :sturges}))
          n (stats/estimate-bins (map (comp double :p) prices) :sturges)]
      (is (= (inc (long n)) (count (get-in b [:scale-specs :color :breaks]))))))
  (testing "2.4 samples merge across charts"
    (let [b (build {:mapping {:x :x :y :y :color :p} :scales {:color {:kind :binned :bins {:quantiles 2}}}
                    :charts [(chart/scatter {:data (take 100 prices)}) (chart/scatter {:data (drop 100 prices)})]})]
      (is (= (vec (distinct (stats/quantiles (map (comp double :p) prices) [0 0.5 1.0])))
             (get-in b [:scale-specs :color :breaks])))))
  (testing "2.5 an explicit domain: values inside it, its ends as the outer breaks"
    (let [b (build (binned-plot prices {:bins {:quantiles 4} :domain [0 10000]}))
          breaks (get-in b [:scale-specs :color :breaks])]
      (is (= 0.0 (first breaks)))
      (is (= 10000.0 (peek breaks)))
      (is (= 100 (get-in b [:stats 0 :dropped :color])) "prices above 10000 dropped")
      (is (= (vec (distinct (concat [0.0] (filter #(< 0.0 % 10000.0) (stats/quantiles (map (comp double :p) (take 100 prices)) [0 0.25 0.5 0.75 1.0])) [10000.0])))
             breaks))))
  (testing "2.6 other scales carry no sample"
    (is (nil? (get-in (build (binned-plot prices {:bins 4})) [:extents :color :extent :sample])))
    (is (nil? (get-in (build {:data prices :mapping {:x :x :y :y :color :p} :charts [(chart/scatter)]}) [:extents :color :extent :sample]))))
  (testing "2.7 no drawable data (all outside the explicit domain): one bin over the domain"
    (let [b (build (binned-plot [{:x 1 :y 1 :p 50} {:x 2 :y 2 :p 60}] {:bins {:quantiles 4} :domain [0 10]}))]
      (is (= [0.0 10.0] (get-in b [:scale-specs :color :breaks])))
      (is (= 2 (get-in b [:stats 0 :dropped :color]))))
    (is (= :mapping (:key (error #(build (binned-plot [{:x 1 :y 1 :p nil}] {:bins {:quantiles 4}})))))
        "all NA: the existing no-valid-values error comes first"))
  (testing "unknown method and non-numeric data"
    (is (= :bins (:key (error #(build (binned-plot prices {:bins :no-such-method}))))))
    (is (= :kind (:key (error #(build (binned-plot [{:x 1 :y 1 :p "a"} {:x 2 :y 2 :p "b"}] {:bins {:quantiles 2}}))))))))

;; 3.3 arrange options

(deftest arrange-options-test
  (let [p (scatter-plot {})]
    (let [e (error-data #(plot/arrange [p] {:cols 3}))]
      (is (= :arrange (:key e)))
      (is (str/includes? (:message (first (:errors e))) "should be spelled :columns")))
    (is (= :arrange (:key (error #(plot/arrange [p] {:zzz 1})))) "an unknown key without a suggestion")
    (is (= {:arrange {:plots [p] :columns 2 :rows 1 :widths [1 2] :heights [1] :align false}}
           (plot/arrange [p] {:columns 2 :rows 1 :widths [1 2] :heights [1] :align false}))
        "every known key accepted")))

;; review fixes

(deftest review-fixes-test
  (testing "F1 margin charts inherit only grouping aesthetics"
    (let [rows (mapv (fn [i] {:x i :y (mod i 7) :p i :g (if (even? i) "a" "b")}) (range 40))
          margin-mapping (fn [b] (get-in b [:margin-stats :top :mappings 0]))]
      (doseq [scale [{:kind :binned :bins {:quantiles 3}} {}]]
        (let [b (build {:data rows :mapping {:x :x :y :y :color :p} :scales {:color scale}
                        :margins {:top (chart/density)} :charts [(chart/scatter)]})]
          (is (not (contains? (margin-mapping b) :color)) (str "continuous colour not inherited " scale))))
      (let [b (build {:data rows :mapping {:x :x :y :y :color :g} :margins {:top (chart/density)} :charts [(chart/scatter)]})]
        (is (= :g (:color (margin-mapping b))) "ordinal colour inherited"))
      (let [b (build {:data rows :mapping {:x :x :y :y :color :p} :scales {:color {:type :categorical :domain (vec (range 40))}}
                      :margins {:top (chart/density)} :charts [(chart/scatter)]})]
        (is (= :p (:color (margin-mapping b))) "numbers declared categorical are inherited"))
      (let [b (build {:data rows :mapping {:x :x :y :y :size :p :color :g} :margins {:top (chart/density)} :charts [(chart/scatter)]})]
        (is (not (contains? (margin-mapping b) :size)) "linear size not inherited")
        (is (= :g (:color (margin-mapping b)))))))
  (testing "F2 a hidden bands axis hides the alternating band fills"
    (let [p (fn [scales] {:data [{:x 1 :y "a"} {:x 2 :y "b"} {:x 3 :y "c"}] :mapping {:x :x :y :y} :scales scales
                          :theme {:panel {:grid {:bands-fill [255 0 0]}}} :charts [(chart/scatter {:size 0})]})
          red? (fn [scales] (let [img ^BufferedImage (plot/render-image (p scales) size)
                                  {:keys [x y w h]} (grid/item-box (:layout (build (p scales))) :panel)]
                              (boolean (some #(= (unchecked-int 0xffff0000) (.getRGB img (int (+ (long x) (quot (long w) 2))) (int %)))
                                             (range (long y) (+ (long y) (long h)))))))]
      (is (red? {}) "default: band fills drawn")
      (is (not (red? {:y {:axis false}})))))
  (testing "F3 facet scales reject :axis"
    (is (= :axis (:key (error #(build {:data corners :mapping {:x :x :y :y :facet-col :g} :scales {:facet-col {:axis false}}
                                       :charts [(chart/scatter)]})))))))
