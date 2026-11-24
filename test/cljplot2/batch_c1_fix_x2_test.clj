(ns cljplot2.batch-c1-fix-x2-test
  "Batch C1 review fixes (x2): one reading of `[nil nil]` and partial scale domains (`cljplot2.data.rows`), the
  `:na-level` flag of aesthetic scale specs and extents, and binned sizes keeping the first bin visible."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [fastmath.core :as m]
            [cljplot2.test-util :refer [error]]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.field :as field]
            [cljplot2.data.rows :as rows]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.series-plots :as series-plots])
  (:import [java.awt.image BufferedImage]
           [java.time LocalDate]))

(def ^:private options {:width 400 :height 300})

(defn- built [p] (plot/build p options))

(defn- message [f] (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-message e))))

(def ^:private data-rows
  "Twenty rows: `:y` from 5 to 14.5, `:s` the squares 0 .. 361, `:g` two facet levels."
  (vec (for [i (range 20)] {:x i :y (+ 5 (* 0.5 i)) :s (* i i) :g (if (< i 10) "a" "b")})))

;; D2 / D3 - the shared domain helpers

(deftest domain-shape-test
  (testing "given-domain: nil and [nil nil] are no domain"
    (is (nil? (rows/given-domain nil)))
    (is (nil? (rows/given-domain [nil nil])))
    (is (nil? (rows/given-domain 3)) "a non-sequential value: none (the schemas reject it)")
    (is (= [0 nil] (rows/given-domain [0 nil])))
    (is (= [nil nil nil] (rows/given-domain [nil nil nil])) "a wrong shape stays, to be reported")
    (is (= ["a"] (rows/given-domain ["a"]))))
  (testing "partial-domain?: a nil among non-nil values"
    (is (rows/partial-domain? [0 nil]))
    (is (rows/partial-domain? [nil 1]))
    (is (rows/partial-domain? [0 nil 3]))
    (is (not (rows/partial-domain? [nil nil])))
    (is (not (rows/partial-domain? [0 1])))
    (is (not (rows/partial-domain? nil))))
  (testing "full-domain: given, no nil"
    (is (= [0 1] (rows/full-domain [0 1])))
    (is (nil? (rows/full-domain [0 nil])))
    (is (nil? (rows/full-domain [nil nil])))
    (is (nil? (rows/full-domain nil))))
  (testing "partial-domain-problem: only two values"
    (is (nil? (rows/partial-domain-problem [0 nil])))
    (is (string? (rows/partial-domain-problem [0 nil 3])))
    (is (string? (rows/partial-domain-problem [nil])))))

(deftest completed-domain-test
  (testing "the given end kept, the other the data's"
    (is (= [0 5] (rows/completed-domain :x [0 nil] [1 5])))
    (is (= [1 10] (rows/completed-domain :x [nil 10] [1 5])))
    (is (= [3 5] (rows/completed-domain :x [3 nil] [1 5])) "an end inside the data"))
  (testing "dates compare by their keys"
    (is (= [(LocalDate/of 2019 1 1) (LocalDate/of 2020 1 1)]
           (rows/completed-domain :x [(LocalDate/of 2019 1 1) nil] [(LocalDate/of 2019 6 1) (LocalDate/of 2020 1 1)]))))
  (testing "errors keyed :domain under the scale"
    (is (= {:key :domain :in [:scales :y :domain] :value [10 nil]} (error #(rows/completed-domain :y [10 nil] [1 5]))))
    (is (= {:key :domain :in [:scales :y :domain] :value [nil 0]} (error #(rows/completed-domain :y [nil 0] [1 5]))))
    (is (= {:key :domain :in [:scales :color :domain] :value [5 nil]} (error #(rows/completed-domain :color [5 nil] nil))))
    (is (= {:key :domain :in [:scales :x :domain] :value [5 nil]} (error #(rows/completed-domain :x [5 nil] [5 5]))) "an empty domain")))

(defn- line-plot [y-scale] {:data data-rows :charts [(chart/line {:mapping {:x :x :y :y}})] :scales {:y y-scale}})

(defn- point-plot [mapping scales] {:data data-rows :charts [(chart/scatter {:mapping (merge {:x :x :y :y} mapping)})] :scales scales})

(deftest one-message-test
  (testing "position and aesthetic scales give the same messages"
    (let [position (message #(built (line-plot {:domain [100 nil]})))
          color (message #(built (point-plot {:color :s} {:color {:domain [1000 nil]}})))]
      (is (str/includes? position "leaves no data on its open side"))
      (is (str/includes? position ":reverse true"))
      (is (str/includes? color "takes its open end from continuous data") "values beyond the end are dropped at compute: none left")))
  (testing "no continuous data for the open end: one message"
    (is (str/includes? (message #(built {:charts [(chart/rule {:x 0.5})] :scales {:x {:domain [0 1]} :y {:domain [0 nil]}}}))
                       "takes its open end from continuous data")))
  (testing "a numeric end over categorical position data: the kind from the data, keyed :domain"
    (is (= :domain (:key (error #(built {:data data-rows :charts [(chart/scatter {:mapping {:x :g :y :y}})] :scales {:x {:domain [0 nil]}}})))))))

(defn- faceted [p relation] (assoc p :mapping {:facet :g} :facets {:scales {:y relation}}))

(deftest facet-domain-test
  (doseq [relation [:free :sliced]]
    (testing (str relation " - [nil nil] is no domain")
      (is (= (get-in (built (faceted (line-plot {}) relation)) [:panel-scale-specs])
             (get-in (built (faceted (line-plot {:domain [nil nil]}) relation)) [:panel-scale-specs])))
      (is (instance? BufferedImage (plot/render-image (faceted (line-plot {:domain [nil nil]}) relation) options))))
    (testing (str relation " - a partial domain: an error naming it")
      (let [e (error #(built (faceted (line-plot {:domain [0 nil]}) relation)))]
        (is (= :facets (:key e))))
      (is (str/includes? (message #(built (faceted (line-plot {:domain [nil 20]}) relation))) "partial :domain")))
    (testing (str relation " - a full domain: an error")
      (is (= :facets (:key (error #(built (faceted (line-plot {:domain [0 20]}) relation)))))))
    (testing (str relation " - an x domain beside a free y")
      (is (some? (built (assoc-in (faceted (line-plot {}) relation) [:scales :x] {:domain [0 nil]})))))))

(deftest acf-domain-test
  (let [values (map #(m/sin (m/* 0.3 %)) (range 60))]
    (testing "[nil nil] is no fixed domain: each row its own y scale"
      (is (= {:scales {:y :free}} (:facets (series-plots/acf values {:scales {:y {:domain [nil nil]}}}))))
      (is (= {:scales {:y :free}} (:facets (series-plots/acf values)))))
    (testing "a full or partial domain fixes the scale"
      (is (nil? (:facets (series-plots/acf values {:scales {:y {:domain [-1 1]}}}))))
      (is (nil? (:facets (series-plots/acf values {:scales {:y {:domain [-1 nil]}}}))))
      (is (instance? BufferedImage (plot/render-image (series-plots/acf values {:scales {:y {:domain [-1 nil]}}}) options))))
    (testing "one correlogram: no facets either way"
      (is (nil? (:facets (series-plots/acf values {:correlograms :acf :scales {:y {:domain [nil nil]}}})))))))

(deftest function-domain-test
  (let [f (chart/function {:f (fn [x] (* x x))})]
    (testing "[nil nil] x domain: as none - no extents of its own"
      (is (= (get-in (built {:charts [f] :scales {:x {:domain [0 2]}}}) [:finalized-scales :x :domain])
             (get-in (built {:charts [f] :scales {:x {:domain [0 2]} :y {:domain [nil nil]}}}) [:finalized-scales :x :domain]))))
    (testing "a partial x domain: sampled over the x scale's domain, completed by other charts"
      (let [b (built {:data data-rows :charts [(chart/scatter {:mapping {:x :x :y :y}}) f] :scales {:x {:domain [-5 nil]}}})]
        (is (= [-5 19.0] (take 2 (get-in b [:scale-specs :x :domain]))))
        (is (instance? BufferedImage (plot/render-image {:data data-rows :charts [(chart/scatter {:mapping {:x :x :y :y}}) f]
                                                         :scales {:x {:domain [-5 nil]}}} options)))))
    (testing "a partial y domain takes its open end from the samples"
      (let [[lo hi] (get-in (built {:charts [f] :scales {:x {:domain [0 2]} :y {:domain [-1 nil]}}}) [:scale-specs :y :domain])]
        (is (= -1 lo))
        (is (< 3.5 hi 4.0) "the samples' upper quantile")))))

(deftest smooth-full-range-test
  (let [p (fn [domain] {:data data-rows :charts [(chart/smooth {:mapping {:x :x :y :y} :method :lm :full-range true})]
                        :scales {:x {:domain domain}}})
        curve-x (fn [b] (let [xs (get-in b [:stats 0 :data (ffirst (get-in b [:stats 0 :data])) :curves 0 :x])]
                          [(first xs) (peek xs)]))]
    (testing "a full domain: the curve spans it"
      (is (= [-5.0 30.0] (curve-x (built (p [-5 30]))))))
    (testing "a partial domain: the given end and the data's other one"
      (is (= [-5.0 19.0] (curve-x (built (p [-5 nil])))))
      (is (= [0.0 30.0] (curve-x (built (p [nil 30]))))))
    (testing "[nil nil]: the data range"
      (is (= [0.0 19.0] (curve-x (built (p [nil nil]))))))
    (testing "data beyond the given end: the x scale reports it"
      (is (= :domain (:key (error #(built (p [50 nil])))))))))

(deftest field-function-domain-test
  (testing "full scale domains give the field domain"
    (is (= [[0.0 1.0] [2.0 3.0]] (field/function-domain nil {:x {:domain [0 1]} :y {:domain [2 3]}}))))
  (testing "[nil nil] and partial domains give none"
    (is (= :domain (:key (error #(field/function-domain nil {:x {:domain [nil nil]} :y {:domain [2 3]}})))))
    (is (= :domain (:key (error #(field/function-domain nil {:x {:domain [0 nil]} :y {:domain [2 3]}})))))
    (is (= [[0.0 1.0] [2.0 3.0]] (field/function-domain {:x [0 1]} {:x {:domain [0 nil]} :y {:domain [2 3]}})) "the option wins")))

;; D8 - :na-level

(deftest na-level-test
  (let [na-plot (point-plot {:color :c} {})
        with-na (assoc na-plot :data (conj (mapv #(assoc % :c "a") data-rows) {:x 1 :y 1 :c nil}))
        without (assoc na-plot :data (mapv #(assoc % :c "a") data-rows))]
    (testing "scale specs and extents carry :na-level"
      (is (true? (get-in (built with-na) [:scale-specs :color :na-level])))
      (is (true? (get-in (built with-na) [:extents :color :na-level])))
      (is (false? (get-in (built without) [:scale-specs :color :na-level])))
      (is (not (contains? (get-in (built with-na) [:scale-specs :color]) :na)))
      (is (not (contains? (get-in (built with-na) [:extents :color]) :na))))
    (testing "the NA legend entry follows it"
      (is (= ["a" "NA"] (mapv :label (aesthetic/legend-entries :color (get-in (built with-na) [:scale-specs :color])))))
      (is (= ["a"] (mapv :label (aesthetic/legend-entries :color (get-in (built without) [:scale-specs :color]))))))
    (testing ":groups {:na false}: no NA level"
      (is (false? (get-in (built (assoc with-na :groups {:na false})) [:scale-specs :color :na-level]))))))

;; P8 - binned sizes

(defn- size-spec [scale] (get-in (built (point-plot {:size :s} {:size scale})) [:scale-specs :size]))

(defn- bin-sizes [spec] (mapv (comp (aesthetic/attribute-fn :size spec nil) :value) (aesthetic/legend-entries :size spec)))

(deftest binned-size-visible-test
  (testing "[0 m] in area: every bin visible, the last at m"
    (let [sizes (bin-sizes (size-spec {:kind :binned :bins 4 :range [0 20] :area true}))]
      (is (= 4 (count sizes)))
      (is (every? pos? sizes))
      (is (= [10.0 14.142135623730951 17.320508075688775 20.0] sizes))))
  (testing "[0 m] in size: one step per bin"
    (is (= [5.0 10.0 15.0 20.0] (bin-sizes (size-spec {:kind :binned :bins 4 :range [0 20]})))))
  (testing "one bin: the range's end; reversed: the first bin largest"
    (is (= [20.0] (bin-sizes (size-spec {:kind :binned :bins 1 :range [0 20]}))))
    (is (= [20.0 10.0] (bin-sizes (size-spec {:kind :binned :bins 2 :range [0 20] :reverse true})))))
  (testing "ordinal sizes keep their spread from the low end"
    (let [spec (aesthetic/scale-spec :size {:domain ["a" "b" "c"] :range [0 20] :area true} nil false {})
          f (aesthetic/attribute-fn :size spec nil)]
      (is (= [0.0 14.142135623730951 20.0] (mapv f ["a" "b" "c"]))))))
