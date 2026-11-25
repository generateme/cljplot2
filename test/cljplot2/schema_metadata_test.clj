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
            [cljplot2.demo :as demo]
            [cljplot2.pipeline]
            [cljplot2.schema :as schema]
            [cljplot2.scale.aesthetic]
            [cljplot2.scale.coord]
            [cljplot2.scale.position]
            [cljplot2.content.label]
            [cljplot2.data.facet]
            [cljplot2.data.rows]
            [cljplot2.layout.plot-grid]
            [cljplot2.theme]
            [cljplot2.test-util :refer [explicit-defaults migrated pixels same-pixels?]]))

(def chart-constructors
  "Kind -> constructor (var) of every built-in chart."
  (describe/chart-constructors))

(defn- undocumented
  "Paths of `rows` without a `:description`."
  [rows]
  (vec (keep #(when-not (:description %) (:path %)) rows)))

(deftest every-chart-test
  (is (= 40 (count chart-constructors))))

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
    (testing (str kind ": only style rows have both a default and a default text (the theme value and how it applies)")
      (let [style-keys (set (map :key (describe/schema-rows style)))]
        (is (every? #(and (= 1 (count (:path %))) (contains? style-keys (:key %)))
                    (filter #(and (some? (:default %)) (:default-doc %)) (describe/options (ctor)))))))
    (testing (str kind ": fragments are registered")
      (is (every? #(contains? describe/fragments %) (keep :fragment (describe/options (ctor))))))))

;; defaults

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
   :function {:charts [(chart/function {:f (fn [x] (* x x)) :domain [-1 1]})]}
   :pairwise {:data (map (fn [i] {:a i :b (* i i)}) (range 10))
              :mapping {:x :a :y :b}
              :charts [(chart/pairwise)]}})

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
  (let [rows (remove #(contains? migrated (:kind %)) (all-rows))]
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
                  (filter #(= [:group] (:path %))
                          (mapcat (comp describe/mapping #(%)) (vals (apply dissoc chart-constructors migrated)))))))))

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

;; R1b: the non-chart schemas

(def non-chart-schemas
  "Every non-chart schema users write: name -> schema."
  describe/plot-level-schemas)

(defn- all-entries
  "`[path props schema]` of every map entry anywhere in `schema` - nested maps, `:multi` branches, `:and` / `:or` /
  `:maybe` / collection children."
  [schema]
  (letfn [(walk [s path]
            (let [s (malli/schema s)]
              (case (malli/type s)
                :map (mapcat (fn [[k props v]] (cons [(conj path k) props v] (walk v (conj path k)))) (malli/children s))
                :multi (mapcat (fn [[_ _ v]] (walk v path)) (malli/children s))
                (:maybe :and :or :vector :sequential :set :map-of :tuple)
                (mapcat #(when (malli/schema? (try (malli/schema %) (catch Exception _ nil))) (walk % path)) (malli/children s))
                nil)))]
    (walk schema [])))

(deftest non-chart-descriptions-test
  (doseq [[n s] non-chart-schemas
          :let [entries (remove (fn [[_ _ v]] (schema/forbidden? v)) (all-entries s))]]
    (testing (str n ": every entry described")
      (is (seq entries))
      (is (= [] (vec (keep (fn [[path props]] (when-not (:description props) path)) entries)))))
    (testing (str n ": not both :default and a default text")
      (is (not-any? (fn [[_ props]] (and (contains? props :default) (contains? props :cljplot2/default-doc))) entries)))
    (testing (str n ": writing rule")
      (is (= [] (rule-breaks (map (fn [[path props]] {:kind n :path path :description (:description props)
                                                      :default-doc (:cljplot2/default-doc props)})
                                  entries)
                             :description)))
      (is (= [] (rule-breaks (map (fn [[path props]] {:kind n :path path :default-doc (:cljplot2/default-doc props)}) entries)
                             :default-doc))))
    (testing (str n ": readable type texts")
      (is (= [] (vec (keep #(when (re-find #":cljplot2\.|#object|\[:(cat|\?|\*|\+|fn|map|and|or|maybe|enum)\b|\w\?(,| |$)" (:type %))
                              [(:path %) (:type %)])
                           (describe/options s))))))))

(deftest theme-entries-test
  (testing "theme entries take their defaults from the default theme, not :default"
    (is (not-any? (fn [[_ props]] (contains? props :default)) (all-entries cljplot2.theme/theme-schema))))
  (testing "every text role of the default theme is described"
    (is (= (set (keys (:text cljplot2.theme/default-theme))) (set (keys cljplot2.theme/text-roles))))))

;; R1b: non-chart defaults

(defn- set-defaults-at
  "`plot` with the constant `:default`s of `schema` set explicitly in the map at `path` (nested ones inside given maps)."
  [plot path schema]
  (let [set-defaults #(explicit-defaults (describe/schema-rows schema) (or % {}))]
    (if (empty? path) (set-defaults plot) (update-in plot path set-defaults))))

(def ^:private faceted
  {:data (for [f ["a" "b" "c" nil] i (range 8)] {:f f :x i :y (* i (count (str f))) :c (if (even? i) "u" "v")})
   :mapping {:x :x :y :y :color :c :facet :f}
   :charts [(chart/scatter)]
   :facets {:scales {}}
   :groups {}
   :labels {:title {:text "T"}}})

(deftest non-chart-defaults-test
  (let [o {:width 500 :height 400}]
    (testing "plot, facets, groups, labels"
      (is (same-pixels? faceted (-> faceted
                                    (set-defaults-at [] cljplot2.pipeline/plot-schema)
                                    (set-defaults-at [:labels :title] cljplot2.content.label/label-options))
                        o)))
    (testing "calendar ticks: the nested :step default"
      (let [day (java.time.LocalDate/of 2026 1 1)
            p {:data (map (fn [i] {:x (.plusDays day i) :y i}) (range 20)) :mapping {:x :x :y :y} :charts [(chart/line)]
               :scales {:x {:ticks-method :pretty :ticks-params {:unit :days}}}}]
        (is (same-pixels? p (set-defaults-at p [:scales :x] cljplot2.scale.position/scale-spec-schema) o))
        (is (= 1 (get-in (set-defaults-at p [:scales :x] cljplot2.scale.position/scale-spec-schema) [:scales :x :ticks-params :step]))
            "the default is really set")))
    (testing "position and aesthetic scales"
      (let [p (assoc faceted :scales {:x {} :y {} :color {} :facet {}})]
        (is (same-pixels? p (-> p
                                (set-defaults-at [:scales :x] cljplot2.scale.position/scale-spec-schema)
                                (set-defaults-at [:scales :y] cljplot2.scale.position/scale-spec-schema)
                                (set-defaults-at [:scales :color] (cljplot2.scale.aesthetic/scale-spec-schema :color))
                                (set-defaults-at [:scales :facet] cljplot2.data.facet/scale-spec-schema))
                          o))))
    (testing "polar coord"
      (let [p {:data [{:x "a" :y 1} {:x "b" :y 2}] :mapping {:x :x :y :y} :charts [(chart/bar)] :coord {:kind :polar}}]
        (is (same-pixels? p (set-defaults-at p [:coord] cljplot2.scale.coord/polar-schema) o))))
    (testing "margins"
      (let [p {:data (map (fn [i] {:x i :y (* i i)}) (range 20)) :mapping {:x :x :y :y} :charts [(chart/scatter)]
               :margins {:top {:charts [(chart/histogram)]}}}
            top (some (fn [[k _ v]] (when (= :top k) v)) (malli/children (malli/schema cljplot2.pipeline/margins-schema)))
            spec (some (fn [[k _ v]] (when (= :spec k) v)) (malli/children (malli/schema top)))
            defaults (into {} (keep (fn [{:keys [key default]}] (when (some? default) [key default])))
                           (describe/schema-rows spec))]
        (is (= {:size 80 :axis false} defaults))
        (is (same-pixels? p (update-in p [:margins :top] merge defaults) o))))
    (testing "arrange"
      (let [cell {:data [{:x 1 :y 2} {:x 2 :y 3}] :mapping {:x :x :y :y} :charts [(chart/scatter)]}
            p {:arrange {:plots [cell cell] :columns 2}}]
        (is (same-pixels? p (set-defaults-at p [:arrange] cljplot2.layout.plot-grid/arrange-schema) o))))
    (testing "render options"
      (is (java.util.Arrays/equals (pixels (plot/render-image faceted))
                                   (pixels (plot/render-image faceted (explicit-defaults (describe/schema-rows cljplot2.pipeline/options-schema) {})))))))) 
