(ns cljplot2.reference-notebook-test
  "Reference part notebooks (`notebooks/reference/`): the part's chapters in the book; every non-chart schema, shared
  group and theme section shown by options tables - a sliced table (rows with `:section`) one slice at a time, every
  slice shown, no table longer than [[max-table-rows]]; tables and Markdown through `notebook-utils` (code hidden);
  the notebook conventions (image size, seeded rng, dataset credits); every chart kind on its chart page (R4) - a
  heading, its mapping table, its own options table and its shared groups. Notebooks are read as forms, their
  aliases resolved - a page names an item by calling or referring to it; a table's rows form is evaluated here.
  Builders and user-facing data functions (R5) likewise: a heading, their options table, called, every option shown;
  the Specific cases chapter calls every internal namespace it shows (spec 7.2)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [clojure.walk :as walk]
            [malli.core :as malli]
            [cljplot2.content.label]
            [cljplot2.core]
            [cljplot2.data.facet]
            [cljplot2.data.rows]
            [cljplot2.describe :as describe]
            [cljplot2.layout.plot-grid]
            [cljplot2.pipeline]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.coord]
            [cljplot2.scale.position]
            [cljplot2.notebook-forms :refer [resolved-forms calls]]))

(def ^:private concept-chapters
  "The concept chapters (R3), in reading order."
  ["intro" "render" "data_mapping" "aesthetics_legends" "scales_axes_coords" "layering_position" "labels_text"
   "facets_arrange" "theme" "shared_options" "errors" "limits"])

(def ^:private chart-chapters
  "The chart chapters (R4), in reading order: page -> the chart kinds it documents (spec 6.3)."
  (array-map "charts_points_paths" [:scatter :line :segment :polygon]
             "charts_series" [:ribbon :smooth :candlestick]
             "charts_annotations" [:rule :rect :text :image :function]
             "charts_bars_ranges" [:bar :extent :bullet]
             "charts_strip_distributions" [:box :violin :stripplot :beeswarm :dotplot :rug]
             "charts_distribution_stats" [:histogram :density :ecdf :qq :pp]
             "charts_grids_fields" [:tile :bin2d :field :contour :correlation]
             "charts_part_of_whole" [:treemap :partition :pack :waffle :venn]
             "charts_multivariate_3d" [:parallel :cloud :surface]))

(def ^:private builder-chapters
  "The builder chapters (R5): page -> the `cljplot2.core` builders it documents (`arrange`, `pairs` - Facets and
  multiplots)."
  (array-map "builders_graphs" '[network chord arc sankey alluvial]
             "builders_trees" '[dendrogram tree clustered-heatmap bundle sequences]
             "builders_series" '[acf horizon]))

(def ^:private data-chapters
  "The data function chapters (R5): page -> the user-facing functions (`:cljplot2/api`) it documents; fonts belong
  to Labels and text."
  (array-map "data_levels" '[cljplot2.data.levels/bin cljplot2.data.levels/categorical cljplot2.data.levels/order-levels
                             cljplot2.data.levels/rank-rows cljplot2.data.shingle/shingle]
             "data_graphs_trees" '[cljplot2.data.graph/graph cljplot2.data.graph/layout cljplot2.data.graph/adjacency->edges
                                   cljplot2.data.graph/arc-layout cljplot2.data.graph/chord-layout
                                   cljplot2.data.graph/sankey-layout cljplot2.data.graph/alluvial-graph
                                   cljplot2.data.tree/from-hclust cljplot2.data.tree/from-levels
                                   cljplot2.data.tree/from-parents cljplot2.data.tree/leaves cljplot2.data.tree/layout
                                   cljplot2.data.tree/node-link-layout cljplot2.data.tree/bundle-paths
                                   cljplot2.data.hierarchy/sequence-levels]
             "data_sets_series" '[cljplot2.data.sets/membership-rows cljplot2.data.sets/upset-rows
                                  cljplot2.data.series/lag-rows cljplot2.data.series/acf-rows
                                  cljplot2.data.series/horizon-rows cljplot2.data.series/horizon-bands]
             "data_fields" '[cljplot2.data.field/domain-color cljplot2.data.contour/isolines
                             cljplot2.data.vector-field/arrows cljplot2.data.vector-field/mapped-points
                             cljplot2.data.vector-field/traces]
             "labels_text" '[cljplot2.graphics.font/resolve-font]))

(def ^:private specific-cases-namespaces
  "Internal namespaces (K19) the Specific cases chapter shows by example, one section each."
  '[cljplot2.data.probability cljplot2.data.kde cljplot2.data.smooth cljplot2.data.dots cljplot2.layout.pack
    cljplot2.layout.treemap cljplot2.layout.venn])

(defn- book-parts
  "Parts of `notebooks/chapters.edn`: title -> chapter paths, and the titles in order (`:order`)."
  []
  (let [parts (filter map? (edn/read-string (slurp "notebooks/chapters.edn")))]
    (assoc (into {} (map (juxt :part :chapters)) parts) :order (mapv :part parts))))

(def ^:private reference-parts
  "The Reference parts of the book in order (Quarto books have one level of parts: consecutive parts, not nested)."
  ["Reference: concepts" "Reference: charts" "Reference: builders" "Reference: data functions"])

(defn- reference-paths
  "Paths (relative to `notebooks/`) of the Reference parts' chapters, in order."
  []
  (let [parts (book-parts)] (vec (mapcat parts reference-parts))))

(defn- notebook-forms
  "Notebook file name (without `.clj`) -> resolved forms, of every Reference chapter."
  []
  (into {} (map (fn [p] [(second (re-find #"([^/]+)\.clj$" p)) (resolved-forms (str "notebooks/" p))]))
        (reference-paths)))

(def ^:private options-table 'notebook-utils/options-table)

(def ^:private max-table-rows
  "The most rows a table shows - longer ones are sliced into sections."
  12)

(defn- normalized
  "`form` with the reader's gensyms of `#(...)` arguments (`p1__123#`) named by position only (`p1#`), so equal
  forms read in two places compare equal."
  [form]
  (walk/postwalk #(if (and (symbol? %) (re-matches #"p(\d+)__\d+#" (name %)))
                    (symbol (str "p" (second (re-matches #"p(\d+)__\d+#" (name %))) "#"))
                    %)
                 form))

(defn- table-calls
  "Every options table of the pages: `{:page p :rows-form f :section s}` (`s` nil - the whole rows)."
  []
  (for [[page forms] (notebook-forms)
        call (calls forms options-table)]
    {:page page :rows-form (normalized (second call)) :section (nth call 2 nil)}))

(defn- eval-rows-arg
  "The value of a (resolved) form of a table's rows or of their source."
  [form]
  (binding [*ns* (the-ns 'cljplot2.reference-notebook-test)]
    (eval form)))

(defn- eval-rows
  "The rows of a table's (resolved) rows form."
  [form]
  (vec (eval-rows-arg form)))

(defn- leaf-row?
  "Is `row` an option, not a map holding options (a container row - a heading of its entries' rows)?"
  [row]
  (not (describe/container-row? row)))

(defn- options-table-args
  "The arguments of every `describe/options` call inside an options table anywhere in `forms`."
  [forms]
  (set (for [table (calls forms options-table)
             call (calls table 'cljplot2.describe/options)]
         (second call))))

(deftest chapters-test
  (testing "the Reference parts - concepts, charts, builders, data functions with specific cases - before Use cases"
    (let [parts (book-parts)
          paths #(map (fn [page] (str "reference/" page ".clj")) %)]
      (is (= (paths concept-chapters) (parts "Reference: concepts")))
      (is (= (paths (keys chart-chapters)) (parts "Reference: charts")))
      (is (= (paths (keys builder-chapters)) (parts "Reference: builders")))
      (is (= (paths (concat (remove #{"labels_text"} (keys data-chapters)) ["specific_cases"]))
             (parts "Reference: data functions")))
      (is (= (conj reference-parts "Use cases") (take 5 (:order parts))))))
  (testing "every chapter exists and starts with a heading"
    (doseq [p (reference-paths)]
      (is (.exists (io/file "notebooks" p)) p)
      (is (re-find #"^;; # \S" (slurp (io/file "notebooks" p))) p))))

(def ^:private schemas-shown-elsewhere
  "Plot-level schemas without a `describe/options` table of their own: the theme (tables per key, `theme-rows`) and the
  coord (a choice of maps - the polar one has the table)."
  #{:theme :coord})

(deftest schema-tables-test
  (let [shown (set (map #(describe/options (eval-rows-arg %)) (options-table-args (vals (notebook-forms)))))]
    (testing "every plot-level schema (describe/plot-level-schemas) has an options table"
      (is (= #{} (into (sorted-set)
                       (keep (fn [[n s]] (when-not (shown (describe/options s)) n)))
                       (apply dissoc describe/plot-level-schemas schemas-shown-elsewhere)))))
    (testing "the builders of the concept chapters (arrange, pairs) have options tables"
      (is (every? #(shown (describe/options %)) [#'cljplot2.core/arrange #'cljplot2.core/pairs])))))

(defn- dim-sections
  "Aesthetic dimension -> the sections of its scale spec shown."
  []
  (reduce (fn [acc {:keys [rows-form section]}]
            (let [arg (and (seq? rows-form) (second rows-form))]
              (if (and (seq? arg) (= 'cljplot2.scale.aesthetic/scale-spec-schema (first arg)))
                (update acc (second arg) (fnil conj #{}) section)
                acc)))
          {} (table-calls)))

(deftest aesthetic-scale-tables-test
  (testing "the colour scale spec: every section; the other dimensions: their :range (the rest is the colour's)"
    (let [shown (dim-sections)]
      (is (= (set aesthetic/dims) (set (keys shown))))
      (is (= (set (keep :section (describe/options (aesthetic/scale-spec-schema :color)))) (shown :color)))
      (doseq [dim (remove #{:color} aesthetic/dims)]
        (is (= #{:range} (shown dim)) dim)))))

(defn- aesthetic-rows-form?
  [form]
  (let [arg (and (seq? form) (second form))]
    (and (seq? arg) (= 'cljplot2.scale.aesthetic/scale-spec-schema (first arg)))))

(deftest sliced-tables-test
  (let [by-form (group-by :rows-form (table-calls))]
    (doseq [[form tables] by-form
            :let [rows (eval-rows form)
                  sections (set (keep :section rows))
                  pages (set (map :page tables))]]
      (testing (str pages " " (pr-str form) ": a table of rows")
        (is (seq rows)))
      (if (<= (count sections) 1)
        (testing (str pages " " (pr-str form) ": rows of one slice - one whole table, at most " max-table-rows " rows")
          (is (= [nil] (distinct (map :section tables))))
          (is (<= (count rows) max-table-rows) (count rows)))
        (testing (str pages " " (pr-str form) ": sliced rows - every option in a section, tables by section")
          (is (every? :section (filter leaf-row? rows)))
          (is (not-any? nil? (map :section tables)))
          (when-not (aesthetic-rows-form? form)
            (is (= sections (set (map :section tables)))))
          (doseq [section (map :section tables)]
            (is (<= (count (filter #(= section (:section %)) rows)) max-table-rows) section)))))))

(deftest fragment-sections-test
  (testing "every shared group has a table in the Shared options chapter"
    (is (= (set (keys describe/fragments))
           (set (map second (calls (get (notebook-forms) "shared_options") 'cljplot2.describe/fragment-rows)))))))

(deftest theme-sections-test
  (testing "every top-level theme key has a table in the Theme chapter"
    (is (= (set (map (comp first :path) (describe/theme-rows)))
           (set (map second (calls (get (notebook-forms) "theme") 'cljplot2.describe/theme-rows)))))))

(defn- kind-calls
  "Calls of `scicloj.kindly.v4.kind` functions in `forms`."
  [forms]
  (filter #(and (seq? %) (symbol? (first %)) (= "scicloj.kindly.v4.kind" (namespace (first %))))
          (tree-seq coll? seq forms)))

(defn- image-size
  "`{:width :height}` of a `render-image` call: its render options over the literal plot's `:options`."
  [call]
  (let [plot (second call)]
    (merge (when (map? plot) (:options plot)) (nth call 2 nil))))

(def ^:private setup-heads
  "Heads of top-level forms that show nothing of an option: namespace, definitions, source notes, wadogo kind docs,
  data tables, option tables and shared group lists."
  #{'ns 'def 'defn 'notebook-utils/source-note 'notebook-utils/scale-kind-doc 'notebook-utils/data-table
    'notebook-utils/shared-groups options-table})

(defn- illustration?
  "Is top-level `form` an example - a call rendering a chart or computing a value shown, also a data table of a
  computed value (`(nu/data-table (f ...))`, not of a dataset as read - `(nu/data-table (nu/dataset :k))` shows input,
  like a data table of a name) - rather than setup (see [[setup-heads]]), a table or a bare literal / symbol (which
  show nothing computed)?"
  [form]
  (and (seq? form)
       (or (not (setup-heads (first form)))
           (let [arg (second form)]
             (and (= 'notebook-utils/data-table (first form)) (seq? arg)
                  (not= 'notebook-utils/dataset (first arg)))))))

(defn- tables-without-example
  "Rows forms of the tables in top-level `forms` not followed by an example before the next table or the page end."
  [forms]
  (loop [[form & more] forms, pending nil, missing []]
    (cond
      (nil? form) (cond-> missing pending (conj pending))
      (and (seq? form) (= options-table (first form))) (recur more (second form) (cond-> missing pending (conj pending)))
      (illustration? form) (recur more nil missing)
      :else (recur more pending missing))))

(deftest examples-test
  (doseq [[page forms] (notebook-forms)]
    (testing (str page ": every table is followed by its example (a chart or a value)")
      (is (= [] (tables-without-example forms))))))

(deftest examples-rule-test
  (testing "boundary: a table at the end, two tables in a row, setup between a table and its example"
    (is (= [:a] (tables-without-example [(list options-table :a)])))
    (is (= [:a] (tables-without-example [(list options-table :a) (list options-table :b) '(f)])))
    (is (= [] (tables-without-example [(list options-table :a) '(def x 1) '(notebook-utils/source-note {}) '(f)])))
    (is (= [] (tables-without-example []))))
  (testing "boundary: a literal or a bare symbol isn't an example"
    (is (= [:a] (tables-without-example [(list options-table :a) :kw "text" 42 'sym]))))
  (testing "a data table of a computed value is an example; of a defined name, setup"
    (is (= [] (tables-without-example [(list options-table :a) '(notebook-utils/data-table (f x))])))
    (is (= [:a] (tables-without-example [(list options-table :a) '(notebook-utils/data-table d)])))
    (is (= [:a] (tables-without-example [(list options-table :a) '(notebook-utils/data-table (notebook-utils/dataset :k))]))
        "a dataset as read is input, not an example")
    (is (= [] (tables-without-example [(list options-table :a)
                                       '(notebook-utils/data-table (remove p (notebook-utils/dataset :k)))]))
        "a computation over a dataset is")))

(deftest conventions-test
  (doseq [[page forms] (notebook-forms)]
    (testing (str page ": tables and Markdown through notebook-utils (their code hidden)")
      (is (empty? (kind-calls forms))))
    (testing (str page ": images at most 1000 x 800, sizes given")
      (doseq [call (calls forms 'cljplot2.core/render-image)
              :let [{:keys [width height]} (image-size call)]]
        (is (and (number? width) (number? height) (<= width 1000) (<= height 800)) [page width height])))
    (testing (str page ": every random generator seeded")
      (is (every? #(<= 3 (count %)) (calls forms 'fastmath.random/rng)) page))
    (testing (str page ": every dataset read is credited")
      (let [read (set (map second (calls forms 'notebook-utils/dataset)))
            credited (set (keep #(:dataset (second %)) (calls forms 'notebook-utils/source-note)))]
        (is (= read credited) page)))))

(defn- row-keys
  "What identifies option `rows` when comparing tables: each row's path and description."
  [rows]
  (mapv (juxt :path :description) rows))

(defn- page-tables
  "Row keys (see [[row-keys]]) of every options table of the page forms `forms`, and of every shared group list
  (`:groups`)."
  [forms]
  {:tables (set (map #(row-keys (eval-rows (normalized (second %)))) (calls forms options-table)))
   :groups (set (map #(row-keys (eval-rows (second %))) (calls forms 'notebook-utils/shared-groups)))})

(deftest chart-pages-test
  (let [forms (notebook-forms)
        constructors (describe/chart-constructors)]
    (testing "the chart chapters document every chart kind once"
      (is (= (set (keys constructors)) (set (mapcat val chart-chapters))))
      (is (apply distinct? (mapcat val chart-chapters))))
    (doseq [[page kinds] chart-chapters
            :let [{:keys [tables groups]} (page-tables (get forms page))
                  text (slurp (str "notebooks/reference/" page ".clj"))]
            kind kinds
            :let [rows (describe/options (@(constructors kind)))
                  mapping (describe/mapping (@(constructors kind)))
                  own (describe/own-rows rows)]]
      (testing (str page " " kind ": a heading, the mapping table, the own options table, the shared groups")
        (is (re-find (re-pattern (str "(?m)^;; ## " (name kind) "$")) text))
        (when (seq mapping) (is (tables (row-keys mapping))))
        (when (seq own) (is (tables (row-keys own))))
        (when (seq (describe/fragment-keys rows)) (is (groups (row-keys rows))))))))

(deftest shared-group-headings-test
  (testing "every shared group has a heading of its title in the Shared options chapter (chart pages link to it)"
    (let [headings (set (map second (re-seq #"(?m)^;; ### (.+)$" (slurp "notebooks/reference/shared_options.clj"))))]
      (is (= #{} (into (sorted-set) (remove headings) (map :title (vals describe/fragments))))))))

(def ^:private max-enum-values
  "Enums of at most this many values have every value shown on a chart page (AK7 b)."
  6)

(defn- literal-option-maps
  "The literal option maps of the calls of function `f` (a resolved symbol) in `forms` - a map literal as the call's
  last argument (`(chart/kind {...})`, `(plot/network edges {...})`)."
  [forms f]
  (filter map? (keep #(when (< 1 (count %)) (last %)) (calls forms f))))

(defn- bare-call?
  "Is `call` of a function with arities `arglists` one without options - fewer arguments than the longest arity, and
  no literal option map?"
  [call arglists]
  (and (not (map? (last call))) (< (dec (count call)) (apply max (map count arglists)))))

(defn- enum-values
  "The values of the enums in entry schema `s` - an `:enum`, also under `:maybe` / `:or` - or `nil` for no enum."
  [s]
  (let [s (malli/schema s)]
    (case (malli/type s)
      :enum (vec (malli/children s))
      (:maybe :or) (seq (mapcat enum-values (malli/children s)))
      nil)))

(defn- short-enums
  "Option key -> its enum values, of the entries `ks` of options schema `schema` (a map, also under validation rules),
  for enums of at most [[max-enum-values]]."
  [schema ks]
  (let [options (malli/schema schema)
        m (if (= :map (malli/type options)) options (first (malli/children options)))]
    (into {} (for [[k _ s] (malli/children m)
                   :when (contains? (set ks) k)
                   :let [vs (enum-values s)]
                   :when (and vs (<= (count vs) max-enum-values))]
               [k vs]))))

(defn- coverage-gaps
  "What the literal option maps `maps` of a chart leave unshown: `{:options [k ...] :values [[k v] ...]}` - own
  top-level option keys `own-keys` never set, values of `enums` (key -> values) never given. A value equal to the
  option's default `defaults` (key -> value) is shown by a call without the option (the basic example)."
  [maps own-keys enums defaults]
  (let [set-keys (set (mapcat keys maps))
        given (set (mapcat seq maps))]
    {:options (vec (remove set-keys own-keys))
     :values (vec (for [[k vs] enums, v vs
                        :when (not (given [k v]))
                        :when (not (and (= v (get defaults k)) (some #(not (contains? % k)) maps)))]
                    [k v]))}))

(defn- function-coverage-gaps
  "[[coverage-gaps]] of the calls of function `f` (resolved symbol, var `v`) in `forms` over option `rows` (their
  top-level keys) of options schema `schema`; a call without options counts as a map without any (its defaults
  shown)."
  [forms f v schema rows]
  (let [top (filter #(= 1 (count (:path %))) rows)
        bare (when (some #(bare-call? % (:arglists (meta v))) (calls forms f)) [{}])]
    (coverage-gaps (concat bare (literal-option-maps forms f)) (map :key top) (short-enums schema (map :key top))
                   (into {} (keep #(when (some? (:default %)) [(:key %) (:default %)])) top))))

(defn- chart-coverage-gaps
  "[[function-coverage-gaps]] of chart `kind` on the page of resolved `forms`: its own options."
  [forms kind]
  (let [v ((describe/chart-constructors) kind)
        chart (@v)]
    (function-coverage-gaps forms (symbol v) v (get-in chart [:schema :options])
                            (describe/own-rows (describe/options chart)))))

(deftest chart-coverage-test
  (let [forms (notebook-forms)]
    (doseq [[page kinds] chart-chapters, kind kinds]
      (testing (str page " " kind ": every own option set in an example, every value of a short enum given")
        (is (= {:options [] :values []} (chart-coverage-gaps (get forms page) kind)))))))

(deftest coverage-rule-test
  (testing "options set or not; enum values given, or the default shown by a call without the option"
    (is (= {:options [:b] :values [[:e :x] [:e :y]]}
           (coverage-gaps [{:a 1 :e :z}] [:a :b] {:e [:x :y :z]} {:e :x})) "the default needs a call without the option")
    (is (= {:options [] :values []} (coverage-gaps [{:a 1} {:e :y}] [:a :e] {:e [:x :y]} {:e :x})) "a call without it")
    (is (= {:options [] :values []} (coverage-gaps [{} {:e :y}] [:e] {:e [:x :y]} {:e :x})) "default by the bare call"))
  (testing "boundary: no maps; no options; no enums"
    (is (= {:options [:a] :values [[:e :x]]} (coverage-gaps [] [:a] {:e [:x]} {})))
    (is (= {:options [] :values []} (coverage-gaps [{:a 1}] [] {} {}))))
  (testing "bare calls: fewer arguments than the longest arity and no option map"
    (is (bare-call? '(f data) '([data] [data options])))
    (is (not (bare-call? '(f data {:a 1}) '([data] [data options]))))
    (is (not (bare-call? '(f data opts) '([data] [data options]))) "options in a symbol: unknown, not bare")
    (is (not (bare-call? '(f data) '([data]))) "a function without options"))
  (testing "enum values: plain, under :maybe and :or; none"
    (is (= [:a :b] (enum-values [:enum :a :b])))
    (is (= [:a] (enum-values [:maybe [:or [:enum :a] :int]])))
    (is (nil? (enum-values :int)))))

(defn- api-heading
  "Page heading text of user-facing function `f` (a qualified symbol): its namespace's last segment and name
  (`graph/layout`, `tree/layout`)."
  [f]
  (str (last (str/split (namespace f) #"\.")) "/" (name f)))

(defn- heading?
  "Does the notebook `page` have a `;; ## text` heading?"
  [page text]
  (boolean (re-find (re-pattern (str "(?m)^;; ## " (java.util.regex.Pattern/quote text) "$"))
                    (slurp (str "notebooks/reference/" page ".clj")))))

(deftest builder-pages-test
  (let [forms (notebook-forms)]
    (testing "every builder of cljplot2.core with options has a page (arrange, pairs: Facets and multiplots)"
      (is (= (set (for [[sym v] (ns-publics 'cljplot2.core) :when (:cljplot2/options (meta v))] sym))
             (into #{'arrange 'pairs} (mapcat val builder-chapters)))))
    (doseq [[page builders] builder-chapters
            :let [{:keys [tables groups]} (page-tables (get forms page))]
            b builders
            :let [v (ns-resolve 'cljplot2.core b)
                  rows (describe/options v)
                  own (describe/own-rows rows)
                  f (symbol "cljplot2.core" (name b))]]
      (testing (str page " " b ": a heading, its own options, its shared groups, called; every own option shown")
        (is (heading? page (name b)))
        (when (seq own) (is (tables (row-keys own))))
        (when (seq (describe/fragment-keys rows)) (is (groups (row-keys rows))))
        (is (seq (calls (get forms page) f)))
        (is (= {:options [] :values []}
               (function-coverage-gaps (get forms page) f v (:cljplot2/options (meta v)) own)))))))

(deftest data-pages-test
  (let [forms (notebook-forms)]
    (testing "every user-facing function on one page"
      (is (= (set (map symbol (describe/api-vars))) (set (mapcat val data-chapters))))
      (is (apply distinct? (mapcat val data-chapters))))
    (doseq [[page fs] data-chapters
            :let [{:keys [tables]} (page-tables (get forms page))]
            f fs
            :let [v (resolve f)
                  rows (describe/options v)]]
      (testing (str page " " f ": a heading, its options table, called; every option shown")
        (is (heading? page (api-heading f)) (api-heading f))
        (when (seq rows) (is (tables (row-keys rows))))
        (is (seq (calls (get forms page) f)))
        (when (seq rows)
          (is (= {:options [] :values []}
                 (function-coverage-gaps (get forms page) f v (:cljplot2/options (meta v)) rows))))))))

(deftest specific-cases-test
  (let [forms (get (notebook-forms) "specific_cases")
        called (set (keep #(when (and (seq? %) (symbol? (first %)) (namespace (first %))) (symbol (namespace (first %))))
                          (tree-seq coll? seq forms)))]
    (testing "every internal namespace shown (K19) is called on the page"
      (is (= #{} (into (sorted-set) (remove called) specific-cases-namespaces))))))
