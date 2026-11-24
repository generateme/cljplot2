(ns cljplot2.batch-c1-fc-test
  "Batch C1 (fc): partial scale domains (`[lo nil]` / `[nil hi]`, position and aesthetic scales) and binned `:size`
  scales (size classes with a legend entry per bin)."
  (:require [clojure.test :refer [deftest testing is]]
            [cljplot2.test-util :refer [error]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.scale.aesthetic :as aesthetic])
  (:import [java.awt.image BufferedImage]
           [java.time LocalDate]))

(def ^:private options {:width 400 :height 300})

(defn- built [p] (plot/build p options))

(def ^:private rows
  "Twenty rows: `:y` from 5 to 14.5, `:s` the squares 0 .. 361."
  (vec (for [i (range 20)] {:x i :y (+ 5 (* 0.5 i)) :s (* i i)})))

(defn- faceted [p] (assoc p :mapping {:facet-x (fn [row] (if (< (:x row) 10) "a" "b"))}))

(defn- line-plot [y-scale] {:data rows :charts [(chart/line {:mapping {:x :x :y :y}})] :scales {:y y-scale}})

(defn- y-domain [p] (get-in (built p) [:finalized-scales :y :domain]))

(defn- spec-domain [p k] (get-in (built p) [:scale-specs k :domain]))

;; 1. partial position domains

(deftest partial-position-domain-test
  (testing "[lo nil]: the lower end fixed, the upper from the data - as the full domain [lo data-max]"
    (is (= (spec-domain (line-plot {:domain [0 14.5]}) :y) (spec-domain (line-plot {:domain [0 nil]}) :y)))
    (is (= (y-domain (line-plot {:domain [0 14.5]})) (y-domain (line-plot {:domain [0 nil]})))))
  (testing "[nil hi]: the upper end fixed"
    (is (= [5.0 30] (spec-domain (line-plot {:domain [nil 30]}) :y)))
    (is (= (y-domain (line-plot {:domain [5.0 30]})) (y-domain (line-plot {:domain [nil 30]})))))
  (testing "[nil nil] and nil: no domain"
    (is (= (y-domain (line-plot {})) (y-domain (line-plot {:domain [nil nil]})) (y-domain (line-plot {:domain nil})))))
  (testing "a fixed end inside the data clips the data beyond it"
    (is (= [10 14.5] (spec-domain (line-plot {:domain [10 nil]}) :y))))
  (testing "one data value: the open end from the widened data domain"
    (is (= [0 5.5] (spec-domain {:data [{:x 0 :y 5} {:x 1 :y 5}] :charts [(chart/line {:mapping {:x :x :y :y}})]
                                 :scales {:y {:domain [0 nil]}}} :y))))
  (testing ":reverse runs a partial domain the other way"
    (let [[a b] (y-domain (line-plot {:domain [0 nil] :reverse true}))]
      (is (> a b))))
  (testing "datetime: a date at one end"
    (let [drows (vec (for [i (range 5)] {:x (LocalDate/of 2020 1 (inc i)) :y i}))
          p {:data drows :charts [(chart/line {:mapping {:x :x :y :y}})] :scales {:x {:domain [(LocalDate/of 2019 12 1) nil]}}}]
      (is (= (LocalDate/of 2019 12 1) (first (spec-domain p :x))))
      (is (= (LocalDate/of 2020 1 5) (second (spec-domain p :x))))))
  (testing "flip, polar and facets: the domain stays a data-space concern"
    (is (= [0 14.5] (spec-domain (assoc (line-plot {:domain [0 nil]}) :flip true) :y)))
    (is (= [0 14.5] (spec-domain (assoc (line-plot {:domain [0 nil]}) :coord {:kind :polar}) :y)))
    (is (= [0 14.5] (spec-domain (faceted (line-plot {:domain [0 nil]})) :y)))
    (is (instance? BufferedImage (plot/render-image (assoc (line-plot {:domain [0 nil]}) :flip true) options)))))

(deftest partial-position-domain-errors-test
  (testing "data entirely beyond the fixed end: {:key :domain}"
    (is (= :domain (:key (error #(built (line-plot {:domain [20 nil]}))))))
    (is (= :domain (:key (error #(built (line-plot {:domain [nil 1]})))))))
  (testing "log with a 0 end"
    (is (= [:scales :y :domain] (:in (error #(built (line-plot {:kind :log :domain [0 nil]})))))))
  (testing ":interpolated takes no nil, bands no partial domain, only two values"
    (is (= :domain (:key (error #(built (line-plot {:kind :interpolated :domain [0 nil]}))))))
    (is (= :domain (:key (error #(built (line-plot {:domain ["a" nil]}))))))
    (is (= :domain (:key (error #(built (line-plot {:domain [0 nil 3]}))))))
    (is (= :domain (:key (error #(built (line-plot {:domain [nil]})))))))
  (testing "a wrong end type"
    (is (= :domain (:key (error #(built (line-plot {:domain [##Inf nil]}))))))
    (is (= :domain (:key (error #(built (line-plot {:kind :datetime :domain [3 nil]})))))))
  (testing "no data to take the open end from"
    (is (= :domain (:key (error #(built {:charts [(chart/rule {:x 0.5})] :scales {:x {:domain [0 1]} :y {:domain [0 nil]}}}))))))
  (testing "a free facet scale takes no partial domain either"
    (is (= :facets (:key (error #(built (assoc (faceted (line-plot {:domain [0 nil]})) :facets {:scales {:y :free}}))))))))

;; 2. partial aesthetic domains

(defn- point-plot [mapping scales] {:data rows :charts [(chart/scatter {:mapping (merge {:x :x :y :y} mapping)})] :scales scales})

(deftest partial-aesthetic-domain-test
  (testing "linear colour [lo nil] / [nil hi]: the open end from the data"
    (is (= [100.0 361.0] (spec-domain (point-plot {:color :s} {:color {:domain [100 nil]}}) :color)))
    (is (= [0.0 50.0] (spec-domain (point-plot {:color :s} {:color {:domain [nil 50]}}) :color))))
  (testing "values beyond the fixed end dropped and counted"
    (is (= 10 (get-in (built (point-plot {:color :s} {:color {:domain [100 nil]}})) [:stats 0 :dropped :color])))
    (is (= 12 (get-in (built (point-plot {:size :s} {:size {:domain [nil 50]}})) [:stats 0 :dropped :size]))))
  (testing "[nil nil]: no domain"
    (is (= (spec-domain (point-plot {:color :s} {}) :color) (spec-domain (point-plot {:color :s} {:color {:domain [nil nil]}}) :color))))
  (testing "a partial domain infers a linear scale"
    (is (= :linear (aesthetic/inferred-kind :size {:domain [0 nil]} false))))
  (testing "binned: the breaks over the completed domain"
    (is (= [0.0 100.0 200.0 300.0 400.0] (get-in (built (point-plot {:color :s} {:color {:kind :binned :domain [nil 400] :bins 4}})) [:scale-specs :color :breaks]))))
  (testing "errors"
    (is (= :domain (:key (error #(built (point-plot {:color :s} {:color {:domain [1000 nil]}}))))) "every value beyond the end")
    (is (= :domain (:key (error #(built (point-plot {:color :s} {:color {:domain [0 nil 1]}}))))))
    (is (= :domain (:key (error #(built (point-plot {:shape :s} {:shape {:domain [1 nil]}}))))) "ordinal: no nil level")
    (is (= :domain (:key (error #(built (point-plot {:color :s} {:color {:domain [##NaN nil]}}))))))
    (is (= :bins (:key (error #(built (point-plot {:size :s} {:size {:kind :binned :bins [0 100 400] :domain [10 nil]}})))))
        "breaks vector: the given end must be its end")))

;; 3. binned size

(deftest binned-size-test
  (testing "default 5 equal bins, sizes over the theme range (the last bin at its end), a legend entry per bin"
    (let [p (point-plot {:size :s} {:size {:kind :binned}})
          s (get-in (built p) [:scale-specs :size])
          f (aesthetic/attribute-fn :size s :na)
          entries (aesthetic/legend-entries :size s)]
      (is (= :binned (:kind s)))
      (is (= 6 (count (:breaks s))))
      (is (= 5 (count entries)))
      (is (< (first (:range s)) (f 0)) "the first bin above the range's low end")
      (is (== (second (:range s)) (f 361)))
      (is (apply < (map (comp f :value) entries)) "sizes increase")))
  (testing "breaks vector: labels a – b, values inside their bins"
    (let [s (get-in (built (point-plot {:size :s} {:size {:kind :binned :bins [0 100 200 400] :range [2 8]}})) [:scale-specs :size])
          f (aesthetic/attribute-fn :size s :na)]
      (is (= ["0 – 100" "100 – 200" "200 – 400"] (mapv :label (aesthetic/legend-entries :size s))))
      (is (= [4.0 6.0 8.0] (mapv (comp f :value) (aesthetic/legend-entries :size s))))
      (is (= 6.0 (f 100)) "bins [a b)")
      (is (= 8.0 (f 400)) "the last bin [a b]")
      (is (nil? (f 401)) "outside the breaks")))
  (testing "one bin: the range's end"
    (let [s (get-in (built (point-plot {:size :s} {:size {:kind :binned :bins 1 :range [2 8]}})) [:scale-specs :size])]
      (is (= [8.0] (mapv (comp (aesthetic/attribute-fn :size s :na) :value) (aesthetic/legend-entries :size s))))))
  (testing ":area, :reverse, :formatter"
    (let [s (get-in (built (point-plot {:size :s} {:size {:kind :binned :bins [0 100 400] :range [0 10] :area true :reverse true
                                                          :formatter #(str (long %) "!")}})) [:scale-specs :size])
          f (aesthetic/attribute-fn :size s :na)]
      (is (= [10.0 7.0710678118654755] [(f 50) (f 300)]) "the first bin of [0 10] at t = 1/2, not 0")
      (is (= ["0! – 100!" "100! – 400!"] (mapv :label (aesthetic/legend-entries :size s))))))
  (testing "quantile and method bins"
    (is (= 4 (count (aesthetic/legend-entries :size (get-in (built (point-plot {:size :s} {:size {:kind :binned :bins {:quantiles 4}}})) [:scale-specs :size])))))
    (is (pos? (count (aesthetic/legend-entries :size (get-in (built (point-plot {:size :s} {:size {:kind :binned :bins :sturges}})) [:scale-specs :size]))))))
  (testing "NA entry last; the legend drawn"
    (let [p {:data (conj rows {:x 21 :y 3 :s nil}) :charts [(chart/scatter {:mapping {:x :x :y :y :size :s}})]
             :scales {:size {:kind :binned :bins 3}}}]
      (is (= ["0 – 120" "120 – 241" "241 – 361" "NA"] (mapv :label (aesthetic/legend-entries :size (get-in (built p) [:scale-specs :size])))))
      (is (instance? BufferedImage (plot/render-image p options)))))
  (testing "size classes on lines and segments (per-row sizes)"
    (is (instance? BufferedImage (plot/render-image {:data rows :charts [(chart/segment {:yend 0 :mapping {:x :x :y :y :size :s}})]
                                                     :scales {:size {:kind :binned :bins 3}}} options))))
  (testing "errors"
    (is (= :kind (:key (error #(built (point-plot {:shape :s} {:shape {:kind :binned}}))))))
    (is (= :midpoint (:key (error #(built (point-plot {:size :s} {:size {:kind :binned :midpoint 3}}))))))
    (is (= :kind (:key (error #(built {:data [{:x 1 :y 1 :g "a"} {:x 2 :y 2 :g "b"}] :charts [(chart/scatter {:mapping {:x :x :y :y :size :g}})]
                                     :scales {:size {:kind :binned}}})))) "non-numeric data")
    (is (= :bins (:key (error #(built (point-plot {:size :s} {:size {:bins 3}}))))) ":bins needs :binned")
    (is (= :ticks (:key (error #(built (point-plot {:size :s} {:size {:kind :binned :ticks 3}}))))))))
