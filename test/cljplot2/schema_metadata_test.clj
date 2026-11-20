(ns cljplot2.schema-metadata-test
  "Reference part R1a: every chart option, style and mapping entry is documented (`:description`), entry properties
  are consistent, fragments are registered, and every constant `:default` is the real default (a plot rendered with
  the defaults set explicitly is pixel-equal)."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [fastmath.random :as r]
            [malli.core :as malli]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [cljplot2.demo :as demo]))

(def chart-constructors
  "Kind -> constructor of every built-in chart."
  (into (sorted-map)
        (for [[_ v] (ns-publics 'cljplot2.chart)
              :let [c (try (@v) (catch Throwable _ nil))]
              :when (and (map? c) (:compute c))]
          [(:kind c) @v])))

(defn- undocumented
  "Paths of `rows` without a `:description`."
  [rows]
  (vec (keep #(when-not (:description %) (:path %)) rows)))

(deftest every-chart-test
  (is (= 39 (count chart-constructors))))

(def ^:private without-mapping
  "Chart kinds reading no mapping keys (only the plot's facet keys reach them)."
  #{:image})

(deftest descriptions-test
  (doseq [[kind ctor] chart-constructors
          :let [c (ctor)]]
    (testing (str kind " options")
      (is (= [] (undocumented (describe/options c)))))
    (testing (str kind " mapping")
      (is (= (not (contains? without-mapping kind)) (boolean (seq (describe/mapping c))))
          "the chart lists the mapping keys it reads (none for the charts reading no mapping)")
      (is (= [] (undocumented (describe/mapping c)))))))

(deftest type-texts-test
  (doseq [[kind ctor] chart-constructors
          :let [c (ctor)]]
    (testing (str kind ": readable type texts (no schema forms or type keywords)")
      (is (= [] (vec (keep #(when (re-find #":cljplot2\.|#object|\[:(cat|\?|\*|\+|fn|map|and|or|maybe|enum)\b|\w\?(,| |$)" (:type %)) [(:path %) (:type %)])
                           (concat (describe/options c) (describe/mapping c)))))))))

(defn- entry-props
  "Properties of every entry (nested closed maps too) of map `schema`."
  [schema]
  (let [walk (fn walk [s]
               (let [s (malli/schema s)]
                 (case (malli/type s)
                   (:maybe :and) (walk (first (malli/children s)))
                   :map (mapcat (fn [[_ props v]] (cons props (walk v))) (malli/children s))
                   nil)))]
    (when schema (walk schema))))

(deftest entry-properties-test
  (doseq [[kind ctor] chart-constructors
          :let [{:keys [options style mapping]} (:schema (ctor))]]
    (testing (str kind ": not both :default and :cljplot2/default-doc")
      (is (not-any? #(and (contains? % :default) (contains? % :cljplot2/default-doc))
                    (concat (entry-props options) (entry-props mapping)))))
    (testing (str kind ": style entries take theme defaults, not :default")
      (is (not-any? #(contains? % :default) (entry-props style))))
    (testing (str kind ": every style key has a theme default, or a default text when the theme leaves it unset")
      (let [style-keys (set (map :key (describe/schema-rows style)))]
        (is (= [] (vec (keep #(when (and (contains? style-keys (:key %)) (nil? (:default %)) (nil? (:default-doc %))) (:key %))
                             (describe/options (ctor))))))))
    (testing (str kind ": fragments are registered")
      (is (every? #(contains? describe/fragments %) (keep :fragment (describe/options (ctor))))))))

;; defaults

(defn- explicit-defaults
  "Chart options `options` with every constant `:default` of `rows` set where absent (`nil` - not set); nested rows
  only inside a given map (an absent map - e.g. no `:arrow` - is no map of defaults)."
  [rows options]
  (reduce (fn [opts {:keys [path default]}]
            (if (and (some? default) (nil? (get-in opts path))
                     (or (= 1 (count path)) (map? (get-in opts (pop path)))))
              (assoc-in opts path default)
              opts))
          (into {} (remove (comp nil? val)) options)
          (remove #(nil? (:default %)) rows)))

(defn- behaviour-rows
  "Option rows of `chart` with a schema `:default` (style rows take theme defaults - not schema ones)."
  [chart]
  (let [style-keys (set (map :key (describe/schema-rows (get-in chart [:schema :style]))))]
    (remove #(and (= 1 (count (:path %))) (contains? style-keys (:key %)))
            (describe/schema-rows (get-in chart [:schema :options])))))

(defn- with-explicit-defaults
  "`plot` with every chart of `kind` rebuilt with its defaults set explicitly."
  [plot kind]
  (update plot :charts
          (fn [charts]
            (mapv (fn [c]
                    (if (= kind (:kind c))
                      ((chart-constructors kind) (explicit-defaults (behaviour-rows c) (:options c)))
                      c))
                  charts))))

(defn- pixels
  "ARGB pixels of `img`."
  ^ints [^java.awt.image.BufferedImage img]
  (.getRGB img 0 0 (.getWidth img) (.getHeight img) nil 0 (.getWidth img)))

(def fixture-plots
  "Plots of the chart kinds without a demo plot."
  {:bullet {:data [{:ranges [150 225 300] :measures [220 270] :markers [250]}]
            :mapping {:ranges :ranges :measures :measures :markers :markers}
            :charts [(chart/bullet)]}
   :correlation {:data (map (fn [i] {:a i :b (* i i) :c (- i)}) (range 10))
                 :mapping {:x :a :y :b}
                 :charts [(chart/correlation)]}
   :extent {:data (map (fn [i] {:x (* 1.5 i) :y (if (even? i) "a" "b")}) (range 10))
            :mapping {:x :x :y :y}
            :charts [(chart/extent)]}
   :function {:charts [(chart/function {:f (fn [x] (* x x)) :domain [-1 1]})]}})

(defn- demo-plot
  "The first single plot of the demos with a chart of `kind`, as `[plot render-options]`, or `nil`."
  [kind]
  (first (for [d demo/demos
               [_ p o] @(requiring-resolve (symbol (str "cljplot2.demo." d) "images"))
               :when (and (map? p) (some #(= kind (:kind %)) (:charts p)))]
           [p o])))

(deftest defaults-test
  (doseq [kind (keys chart-constructors)
          :let [[p o] (or (demo-plot kind) [(fixture-plots kind) {:width 400 :height 300}])]]
    (testing (str kind ": the plot rendered with every :default set explicitly is pixel-equal")
      (is (some? p) "a demo or fixture plot")
      (when p
        (is (java.util.Arrays/equals (pixels (plot/render-image p o))
                                     (pixels (plot/render-image (with-explicit-defaults p kind) o))))))))

(def nested-fixture-plots
  "Plots giving an empty nested option map, so its entries' defaults are exercised: kind -> plot."
  {:segment {:data [{:x 0 :y 0 :xe 1 :ye 1} {:x 1 :y 0 :xe 0 :ye 1}]
             :mapping {:x :x :y :y :xend :xe :yend :ye}
             :charts [(chart/segment {:arrow {}})]}
   :line {:data (map (fn [i] {:x i :y (* i i)}) (range 6))
          :mapping {:x :x :y :y}
          :charts [(chart/line {:arrow {}})]}
   :cloud {:data (map (fn [i] {:x i :y (- i) :z (* i i)}) (range 8))
           :mapping {:x :x :y :y :z :z}
           :charts [(chart/cloud {:view {}})]}
   :smooth {:data (map (fn [i] {:x i :y (+ i (* 3.0 (Math/sin i)))}) (range 20))
            :mapping {:x :x :y :y}
            :charts [(chart/smooth {:method :lm :band :bootstrap :bootstrap {:rng (r/rng :mersenne 7)}})]}
   :parallel {:data (map (fn [i] {:a i :b (* i i) :c (- 10 i)}) (range 6))
              :charts [(chart/parallel {:variables [:a :b :c] :padding {}})]}})

(deftest nested-defaults-test
  (doseq [[kind p] nested-fixture-plots
          :let [o {:width 300 :height 300}]]
    (testing (str kind ": nested defaults inside a given empty map are the real ones")
      (is (java.util.Arrays/equals (pixels (plot/render-image p o))
                                   (pixels (plot/render-image (with-explicit-defaults p kind) o)))))))

;; writing rules

(defn- all-rows
  "Option and mapping rows of every chart and the rows of every shared group, each with its `:kind` (or fragment)."
  []
  (concat (for [[kind ctor] chart-constructors
                :let [c (ctor)]
                row (concat (describe/options c) (describe/mapping c))]
            (assoc row :kind kind))
          (for [f (keys describe/fragments) row (describe/fragment-rows f)] (assoc row :kind f))))

(defn- rule-breaks
  "`[kind path text]` of the texts under `k` of `rows` breaking the writing rule: one sentence of at most 200
  characters, starting lowercase, without a final period."
  [rows k]
  (vec (for [{:keys [kind path] :as row} rows
             :let [t (get row k)]
             :when (and (string? t) (or (re-find #"^[A-Z]" t) (str/ends-with? t ".") (> (count t) 200)))]
         [kind path t])))

(deftest writing-rules-test
  (let [rows (all-rows)]
    (testing "descriptions"
      (is (= [] (rule-breaks rows :description))))
    (testing "default texts"
      (is (= [] (rule-breaks rows :default-doc))))
    (testing "boundary: the rule itself"
      (is (= 3 (count (rule-breaks [{:kind :k :path [:a] :description "Starts upper"}
                                    {:kind :k :path [:b] :description "ends with a period."}
                                    {:kind :k :path [:c] :description (apply str (repeat 201 "x"))}
                                    {:kind :k :path [:d] :description (apply str (repeat 200 "x"))}
                                    {:kind :k :path [:e] :description nil}]
                                   :description)))))
    (testing "a mapped :group reads the same in every chart"
      (is (every? #(str/starts-with? (:description %) "groups without a visual aesthetic")
                  (filter #(= [:group] (:path %)) (mapcat (comp describe/mapping #(%)) (vals chart-constructors))))))))

;; fragments

(deftest fragment-entries-test
  (doseq [[f {:keys [entries]}] describe/fragments]
    (testing (str f ": every top-level entry of the group is tagged with it (nested entries belong to their map)")
      (is (seq entries))
      (is (every? #(= f (:fragment %)) (filter #(= 1 (count (:path %))) (describe/fragment-rows f))))))
  (doseq [[kind ctor] chart-constructors
          :let [rows (describe/options (ctor))]]
    (testing (str kind ": a chart's group rows are entries of that group")
      (is (every? (fn [{:keys [fragment key]}]
                    (some #(= key (:key %)) (describe/fragment-rows fragment)))
                  (filter #(and (:fragment %) (= 1 (count (:path %)))) rows))))))
