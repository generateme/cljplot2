(ns cljplot2.describe-test
  "Reference part R1a: `cljplot2.describe` rows, type texts, fragments, API vars."
  (:require [clojure.test :refer [deftest testing is]]
            [cljplot2.chart :as chart]
            [cljplot2.data.rows]
            [cljplot2.describe :as describe]
            [cljplot2.layout.plot-grid :as plot-grid]
            [cljplot2.pipeline :as pipeline]
            [cljplot2.scale.position]
            [cljplot2.schema :as schema]
            [cljplot2.test-util :refer [error-data]]
            [cljplot2.theme :as theme]))

(defn- row [rows k] (first (filter #(= [k] (:path %)) rows)))

(deftest chart-rows-test
  (let [rows (describe/options (chart/bullet))]
    (testing "style rows take theme defaults"
      (is (= 0.35 (:default (row rows :measure-fraction))))
      (is (= "a number in [0 1]" (:type (row rows :measure-fraction)))))
    (testing "fragment rows carry their fragment"
      (is (= :common (:fragment (row rows :data))))
      (is (= :strip (:fragment (row rows :place))))
      (is (nil? (:fragment (row rows :measure-fraction)))))
    (testing "every row has the documented keys (`:example` and `:example-context` only when the entry gives them, `:theme-default` only on theme defaults)"
      (is (every? #(= #{:key :path :type :required :default :default-doc :description :fragment :section :values :forms :other-values}
                      (set (keys (dissoc % :example :example-context :no-example :theme-default))))
                  rows)))))

(deftest fragment-rows-test
  (testing "a style group's rows without a default read the chart's theme style"
    (is (every? #(= "The chart's theme style." (:default-doc %)) (describe/fragment-rows :line-style))))
  (testing "boundary: other groups' rows keep their own defaults and texts; no row gets both"
    (is (= 0 (:default (first (filter #(= :corner-radius (:key %)) (describe/fragment-rows :corner-radius))))))
    (is (not-any? #(= "The chart's theme style." (:default-doc %))
                  (mapcat describe/fragment-rows (remove #(:style (describe/fragments %)) (keys describe/fragments)))))
    (is (not-any? #(and (some? (:default %)) (:default-doc %)) (mapcat describe/fragment-rows (keys describe/fragments)))))
  (testing "boundary: an unknown group has no rows"
    (is (empty? (describe/fragment-rows :no-such-group)))))

(deftest schema-rows-test
  (let [s [:map {:closed true}
           [:a {:description "a" :default 1} :int]
           (schema/optional :b {:cljplot2/default-doc "computed"} [:maybe [:map {:closed true} (schema/optional :c {:description "c"} :string)]])
           (schema/optional :d [:and :int [:> 0]])
           (schema/optional :internal (schema/forbidden "is internal"))]
        rows (describe/schema-rows s)]
    (testing "required and optional, defaults and default texts"
      (is (= [true 1 nil] ((juxt :required :default :default-doc) (row rows :a))))
      (is (= [false nil "computed"] ((juxt :required :default :default-doc) (row rows :b)))))
    (testing "a nested closed map: its own row (type map), then its entries' rows"
      (is (= [[:a] [:b] [:b :c] [:d]] (map :path rows)))
      (is (= "map" (:type (row rows :b)))))
    (testing "forbidden entries have no row, also under :maybe (nilable)"
      (is (nil? (row rows :internal)))
      (is (= [] (describe/schema-rows [:map (schema/nilable :gone (schema/forbidden "is gone"))]))))
    (testing "boundary: no entries, nil, a non-map schema"
      (is (= [] (describe/schema-rows [:map])))
      (is (= [] (describe/schema-rows nil)))
      (is (= [] (describe/schema-rows :int))))
    (testing "with-rules schemas (an :and) still give rows"
      (is (= [[:x]] (map :path (describe/schema-rows (schema/with-rules [:map [:x :int]] [(constantly nil) nil]))))))))

(deftest type-text-test
  (is (= "a non-negative number" (describe/type-text schema/non-neg)))
  (is (= "one of :a :b" (describe/type-text [:enum :a :b])))
  (is (= "integer (2..2000)" (describe/type-text [:int {:min 2 :max 2000}])))
  (is (= "[a number in [0 1], keyword]" (describe/type-text [:tuple schema/fraction keyword?])))
  (is (= "true or false or nil" (describe/type-text [:maybe schema/bool])))
  (is (= "a finite number, >= 0" (describe/type-text [:and schema/finite [:>= 0]])))
  (is (= "{:at a finite number}" (describe/type-text [:map {:closed true} [:at schema/finite]])))
  (is (= "vector of keyword (at least 1)" (describe/type-text [:vector {:min 1} keyword?])))
  (is (= "map of one of :x :y to string" (describe/type-text [:map-of [:enum :x :y] :string])))
  (is (= ":none or 1" (describe/type-text [:or [:= :none] [:= 1]])))
  (is (= "vector, [keyword, optional map]" (describe/type-text [:and vector? [:cat :keyword [:? :map]]])))
  (is (= "sequence" (describe/type-text sequential?)))
  (testing "named types: type doc over message"
    (is (re-find #"^a fastmath 1D" (describe/type-text schema/interpolator)))
    (is (re-find #"^:none, a dash style" (describe/type-text schema/dash)))))

(deftest fragments-test
  (testing "in-fragment tags both entry forms"
    (is (= [[:a {:cljplot2/fragment :f} :int] [:b {:optional true :description "b" :cljplot2/fragment :f} :int]]
           (schema/in-fragment :f [[:a :int] (schema/optional :b {:description "b"} :int)])))
    (is (= [] (schema/in-fragment :f []))))
  (testing "every fragment has a title and doc"
    (is (every? (fn [[_ v]] (and (string? (:title v)) (string? (:doc v)))) describe/fragments))))

(deftest api-vars-test
  (let [inside (create-ns 'cljplot2.describe-probe)
        outside (create-ns 'describe-probe-outside)]
    (try
      (let [api (intern inside (with-meta 'f {:cljplot2/api true
                                              :cljplot2/options [:map (schema/optional :k {:description "k"} :int)]})
                        (fn [] nil))
            _ (intern inside 'g (fn [] nil))
            _ (intern outside (with-meta 'h {:cljplot2/api true}) (fn [] nil))]
        (testing "only :cljplot2/api vars of cljplot2.* namespaces"
          (is (some #(= api %) (describe/api-vars)))
          (is (not-any? #(#{"g" "h"} (str (:name (meta %)))) (describe/api-vars))))
        (testing "sorted by namespace and name"
          (is (= (describe/api-vars) (sort-by (fn [v] [(str (:ns (meta v))) (str (:name (meta v)))]) (describe/api-vars)))))
        (testing "a var's :cljplot2/options rows; none without them"
          (is (= [[:k]] (map :path (describe/options api))))
          (is (= [] (describe/options (ns-resolve inside 'g))))))
      (finally
        (remove-ns 'cljplot2.describe-probe)
        (remove-ns 'describe-probe-outside)))))

(deftest fragment-rows-test
  (testing "one group's rows, in entry order"
    (is (= [[:corner-radius] [:corner-radius-end]] (map :path (describe/fragment-rows :corner-radius))))
    (is (= [[:mirror]] (map :path (describe/fragment-rows :mirror)))))
  (testing "boundary: an unknown group or nil has no rows"
    (is (= [] (describe/fragment-rows :nope)))
    (is (= [] (describe/fragment-rows nil)))))

(deftest api-namespaces-test
  (testing "describe loads every namespace that holds user-facing functions"
    (is (every? find-ns '[cljplot2.data.contour cljplot2.data.field cljplot2.data.graph cljplot2.data.hierarchy
                          cljplot2.data.levels cljplot2.data.series cljplot2.data.sets cljplot2.data.shingle
                          cljplot2.data.tree cljplot2.data.vector-field cljplot2.graphics.font]))))

(deftest plot-rows-test
  (let [rows (describe/options pipeline/plot-schema)
        by-path (into {} (map (juxt :path identity)) rows)]
    (testing "the plot's top-level keys, nested closed maps flattened"
      (is (= 18 (count (filter #(= 1 (count (:path %))) rows))) "19 keys, :alignment-padding internal")
      (is (contains? by-path [:facets :scales :x]))
      (is (= :same (:default (by-path [:facets :scales :x])))))
    (testing "large composite types read as their type docs"
      (is (= "a position scale spec" (:type (by-path [:scales :x]))))
      (is (re-find #"^\{:kind :cartesian\}" (:type (by-path [:coord])))))
    (testing "internal and forbidden entries have no rows"
      (is (nil? (by-path [:alignment-padding])))
      (is (nil? (by-path [:mapping :fill]))))
    (testing "boundary: maps under validation rules (:scales :x, :arrange) aren't flattened - they have tables of their own"
      (is (nil? (by-path [:arrange :columns])))
      (is (nil? (by-path [:scales :x :kind]))))
    (testing "the :arrange entries are the shared group :arrange"
      (let [arrange (describe/options plot-grid/arrange-schema)]
        (is (every? #(= :arrange (:fragment %)) arrange))
        (is (= (map :key (describe/fragment-rows :arrange)) (map :key arrange)))))))

(deftest required-rows-test
  (testing "merged maps (theme, render options): nothing is required of the user, nested maps included"
    (is (not-any? :required (describe/theme-rows)))
    (is (not-any? :required (describe/options pipeline/options-schema))))
  (testing "boundary: a required entry of an ordinary map stays required, also nested; a merged map's flag doesn't leak"
    (let [rows (describe/schema-rows [:map [:a {:description "a"} [:map {:closed true} [:b {:description "b"} :int]]]])]
      (is (every? :required rows)))
    (let [rows (describe/schema-rows [:map [:m {:description "m"} [:map {:closed true :cljplot2/merged true} [:b {:description "b"} :int]]]
                                      [:c {:description "c"} :int]])]
      (is (= [true false true] (map :required rows))))))

(deftest internal-rows-test
  (testing "an internal entry has no row; boundary: its nested entries neither"
    (is (= [[:a]] (map :path (describe/schema-rows [:map [:a :int] [:i {:cljplot2/internal true} [:map {:closed true} [:x :int]]]]))))))

(deftest container-defaults-test
  (let [with-defaults #'describe/with-defaults
        rows (describe/schema-rows [:map [:roles {:cljplot2/type-doc "a lookup"} [:map-of keyword? :map]]
                                    [:line [:or [:= :none] [:map {:closed true} [:w :int]]]]
                                    [:box [:map {:closed true} [:w :int]]]])
        filled (into {} (map (juxt :path :default)) (with-defaults rows {:roles {:a {}} :line {:w 1} :box {:w 2}}))]
    (testing "a container (map, map of entries) gets no default, whatever its type text reads"
      (is (nil? (filled [:roles])))
      (is (nil? (filled [:box]))))
    (testing "its entries do, and a map value of a non-container (a line spec) is a default"
      (is (= 2 (filled [:box :w])))
      (is (= {:w 1} (filled [:line]))))))

(deftest theme-rows-test
  (let [rows (describe/theme-rows)
        by-path (into {} (map (juxt :path identity)) rows)]
    (testing "leaves take the default theme's values"
      (is (= (get-in theme/default-theme [:legend :key-size]) (:default (by-path [:legend :key-size]))))
      (is (= (get-in theme/default-theme [:panel :grid :x]) (:default (by-path [:panel :grid :x]))) "a map value of a leaf"))
    (testing "boundary: map-typed rows hold no default (their entries do)"
      (is (nil? (:default (by-path [:panel]))))
      (is (nil? (:default (by-path [:chart]))))
      (is (nil? (:default (by-path [:text])))))
    (testing "one row per text role, sorted, its default the role's style, typed as a text style"
      (let [roles (filter #(= :text (first (:path %))) (filter #(= 2 (count (:path %))) rows))]
        (is (= (sort (keys theme/text-roles)) (map :key roles)))
        (is (every? :description roles))
        (is (= (get-in theme/default-theme [:text :title]) (:default (by-path [:text :title]))))
        (is (re-find #"^a text style" (:type (by-path [:text :title]))))
        (is (= [{:size 20 :font-style :italic}] (describe/example (by-path [:text :title]))) "its example the role's")))
    (testing "a map form whose keys a shared group documents names the group: text styles, grid line styles"
      (is (= [{:form :map :type "text style" :group :text-style}] (map #(dissoc % :meaning) (:forms (by-path [:text :title])))))
      (is (= [{:form :map :type "text style" :group :text-style}] (map #(dissoc % :meaning) (:forms (by-path [:axis :x :tick-label])))))
      (is (= {:form :map :type "line style" :group :line-style} (dissoc (first (:forms (by-path [:panel :grid :x]))) :meaning)))
      (testing "boundary: a map form without a documenting group names none; one under :maybe keeps its group"
        (let [form (fn [s] (first (:forms (first (describe/schema-rows [:map [:k {:description "k"} s]])))))]
          (is (not (contains? (form [:or [:= :none] [:map [:a :int]]]) :group)))
          (is (= :line-style (:group (form [:maybe [:map {:cljplot2/entries-fragment :line-style} [:a :int]]])))))))
    (testing "boundary: a chart's or named style's rows keep their theme values even beside a default text (only theme rows drop them)"
      (is (= 0.3 (:default (first (filter #(= [:bridge :alpha-factor] (:path %)) (describe/style-rows :line)))))))
    (testing "boundary: a theme row with a default text shows the text only (the palette); one without keeps the value"
      (is (nil? (:default (by-path [:palette :color]))))
      (is (string? (:default-doc (by-path [:palette :color]))))
      (is (= (get-in theme/default-theme [:palette :na]) (:default (by-path [:palette :na])))))
    (testing "the style keys aren't repeated per role: one shared table, the :text-style group"
      (is (not-any? #(= 3 (count (:path %))) (filter #(= :text (first (:path %))) rows)))
      (is (= [:font :size :font-style :color :inset :align :background :dollar-math] (map :key (describe/fragment-rows :text-style)))))
    (testing "role rows go through the common row code: merged, not required"
      (is (not-any? :required (filter #(= :text (first (:path %))) rows))))
    (testing "a key's rows: the rows under it, in order, without the key's own row; with the top-level rows every row"
      (is (= (filter #(and (= :legend (first (:path %))) (< 1 (count (:path %)))) rows) (describe/theme-rows :legend)))
      (is (= [:legend :gap] (:path (first (describe/theme-rows :legend)))))
      (is (= (count rows) (+ (count (keys theme/default-theme))
                             (reduce + (map (comp count describe/theme-rows) (keys theme/default-theme)))))))
    (testing "boundary: :chart has no rows under it (each chart's page documents its style), but is a theme key"
      (is (= [] (describe/theme-rows :chart))))
    (testing "boundary: an unknown theme key, nil - an error keyed :theme-key"
      (is (= :theme-key (:key (error-data #(describe/theme-rows :no-such-key)))))
      (is (= :theme-key (:key (error-data #(describe/theme-rows nil))))))
    (testing "text roles in their sections: plot, legend, hierarchy, other chart and builder texts"
      (is (= #{:plot-text :legend-text :hierarchy-text :chart-text :builder-text} (set (keep :section (describe/theme-rows :text)))))
      (is (= :hierarchy-text (:section (by-path [:text :pack-leaf]))))
      (is (= :legend-text (:section (by-path [:text :legend-title])))))))

(deftest section-rows-test
  (let [nested [:map {:closed true} [:w {:description "w"} :int]
                [:v {:description "v" :cljplot2/section :own} :int]]
        s [:map {:closed true}
           [:a {:description "a" :cljplot2/section :first} :int]
           [:b {:description "b" :cljplot2/section :second} nested]]
        by-path (into {} (map (juxt :path :section)) (describe/schema-rows s))]
    (testing "an entry's section, nested entries inherit their map entry's, their own wins"
      (is (= {[:a] :first [:b] :second [:b :w] :second [:b :v] :own} by-path)))
    (testing "boundary: no sections - nil"
      (is (every? nil? (map :section (describe/schema-rows [:map [:a {:description "a"} :int]])))))
    (testing "library schemas: plot, mapping and position scale rows are sliced"
      (is (every? :section (describe/options pipeline/plot-schema)))
      (is (every? :section (describe/options cljplot2.data.rows/mapping-schema)))
      (is (= :wadogo (:section (first (filter #(= [:exponent] (:path %))
                                                (describe/options cljplot2.scale.position/scale-spec-schema)))))))))

(deftest row-helpers-test
  (let [rows (describe/options pipeline/plot-schema)]
    (testing "top-level rows: one key paths, in order; a nested map's own sections stay with its own table"
      (is (= (filter #(= 1 (count (:path %))) rows) (describe/top-level-rows rows)))
      (is (= :layout (:section (row rows :facets))))
      (is (= :shape (:section (first (filter #(= [:facets :columns] (:path %)) rows)))))
      (is (every? #(not= :shape (:section %)) (describe/top-level-rows rows))))
    (testing "boundary: no rows; only nested rows"
      (is (= [] (describe/top-level-rows [])))
      (is (= [] (describe/top-level-rows (filter #(< 1 (count (:path %))) rows)))))
    (testing "rows under a path: the nested map's entries, not its own row"
      (is (= [[:groups :na]] (map :path (describe/rows-under rows [:groups]))))
      (is (= (filter #(and (= :facets (first (:path %))) (< 1 (count (:path %)))) rows)
             (describe/rows-under rows [:facets]))))
    (testing "boundary: a leaf, an unknown path - no rows; the empty path - every row"
      (is (= [] (describe/rows-under rows [:legend])))
      (is (= [] (describe/rows-under rows [:no-such-key])))
      (is (= rows (describe/rows-under rows []))))
    (testing "rows at paths: those rows only, in the rows' order, not their nested rows"
      (is (= [[:flip] [:coord]] (map :path (describe/rows-at rows #{[:coord] [:flip]}))))
      (is (= [[:groups]] (map :path (describe/rows-at rows #{[:groups]})))))
    (testing "boundary: no paths, an unknown path - no rows"
      (is (= [] (describe/rows-at rows #{})))
      (is (= [] (describe/rows-at rows #{[:no-such-key]}))))
    (testing "container rows: maps holding options, not the options"
      (is (describe/container-row? (row rows :facets)))
      (is (not (describe/container-row? (row rows :legend))))
      (is (not (describe/container-row? {:path [:x]})) "a row map without the metadata"))))

(deftest plot-level-schemas-test
  (testing "the registry: every plot-level schema, one aesthetic scale spec per dimension, each with rows or a choice"
    (is (every? #(contains? describe/plot-level-schemas %)
                [:plot :render-options :mapping :position-scale :facets :labels :label :arrange :polar-coord :margins
                 :margin-spec :theme :color-scale :pattern-scale]))
    (is (= 5 (count (filter #(re-find #"^(color|shape|size|dash|pattern)-scale$" (name %)) (keys describe/plot-level-schemas)))))
    (is (every? (fn [[n s]] (or (#{:coord} n) (seq (describe/options s)))) describe/plot-level-schemas))))

(deftest chart-constructors-test
  (testing "every built-in chart kind, sorted, each var returning its kind's chart value"
    (let [cs (describe/chart-constructors)]
      (is (= 40 (count cs)))
      (is (= (sort (keys cs)) (keys cs)))
      (is (every? (fn [[k v]] (= k (:kind (v)))) cs))
      (is (every? var? (vals cs))))))

(deftest entries-fragment-test
  (let [style [:map {:closed true} [:w {:description "w"} :int]]
        rows (describe/schema-rows [:map [:a {:description "a" :cljplot2/entries-fragment :line-style} style]
                                    [:b {:description "b"} style]])
        with-defaults #'describe/with-defaults
        filled (into {} (map (juxt :path :default)) (with-defaults rows {:a {:w 1} :b {:w 2}}))]
    (testing "an entry whose entries a group documents isn't flattened and keeps its value as default"
      (is (= [[:a] [:b] [:b :w]] (map :path rows)))
      (is (= {:w 1} (filled [:a])))
      (is (= "{:w integer}" (:type (first rows)))))
    (testing "boundary: an ordinary nested map is flattened, its row a container without default"
      (is (nil? (filled [:b])))
      (is (= 2 (filled [:b :w]))))))
