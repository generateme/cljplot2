(ns cljplot2.reference-notebook-test
  "Reference part notebooks (`notebooks/reference/`): the part's chapters in the book; every non-chart schema, shared
  group and theme section shown by options tables - a sliced table (rows with `:section`) one slice at a time, every
  slice shown, no table longer than [[max-table-rows]]; tables and Markdown through `notebook-utils` (code hidden);
  the notebook conventions (image size, seeded rng, dataset credits); every chart kind on its chart page (R4) - a
  heading, its mapping table, its own options table and its shared groups. Notebooks are read as forms, their
  aliases resolved - a page names an item by calling or referring to it; a table's rows form is evaluated here.
  Builders and data functions get their pages in R5 (spec 7.2)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest testing is]]
            [clojure.walk :as walk]
            [cljplot2.content.label]
            [cljplot2.core]
            [cljplot2.data.facet]
            [cljplot2.data.rows]
            [cljplot2.describe :as describe]
            [cljplot2.layout.plot-grid]
            [cljplot2.pipeline]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.coord]
            [cljplot2.scale.position])
  (:import [java.io PushbackReader]))

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

(defn- book-parts
  "Parts of `notebooks/chapters.edn`: title -> chapter paths, and the titles in order (`:order`)."
  []
  (let [parts (filter map? (edn/read-string (slurp "notebooks/chapters.edn")))]
    (assoc (into {} (map (juxt :part :chapters)) parts) :order (mapv :part parts))))

(defn- reference-paths
  "Paths (relative to `notebooks/`) of the Reference part's chapters."
  []
  (get (book-parts) "Reference"))

(defn- read-forms
  "Every top-level form of the Clojure file at `path` (no evaluation)."
  [path]
  (binding [*read-eval* false]
    (with-open [r (PushbackReader. (io/reader path))]
      (vec (take-while #(not= ::end %) (repeatedly #(read {:eof ::end} r)))))))

(defn- aliases
  "Alias name -> namespace symbol of the `ns` form in `forms`."
  [forms]
  (let [ns-form (first (filter #(and (seq? %) (= 'ns (first %))) forms))
        requires (rest (first (filter #(and (seq? %) (= :require (first %))) ns-form)))]
    (into {} (for [spec requires
                   :when (vector? spec)
                   :let [{alias :as} (apply hash-map (rest spec))]
                   :when alias]
               [(name alias) (first spec)]))))

(defn- resolved-forms
  "Forms of the notebook at `path` with alias-qualified symbols (`plot/arrange`) fully qualified."
  [path]
  (let [forms (read-forms path)
        as (aliases forms)]
    (walk/postwalk #(if-let [n (and (symbol? %) (as (namespace %)))] (symbol (str n) (name %)) %) forms)))

(defn- notebook-forms
  "Notebook file name (without `.clj`) -> resolved forms, of every Reference chapter."
  []
  (into {} (map (fn [p] [(second (re-find #"([^/]+)\.clj$" p)) (resolved-forms (str "notebooks/" p))]))
        (reference-paths)))

(defn- calls
  "Every call of function symbol `f` anywhere in `forms`."
  [forms f]
  (filter #(and (seq? %) (= f (first %))) (tree-seq coll? seq forms)))

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
  (testing "the Reference part lists the concept chapters, then the chart chapters, before Use cases"
    (is (= (map #(str "reference/" % ".clj") (concat concept-chapters (keys chart-chapters)))
           (take (+ (count concept-chapters) (count chart-chapters)) (reference-paths))))
    (is (= ["Reference" "Use cases"] (take 2 (:order (book-parts))))))
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
  "Is top-level `form` an example - a call rendering a chart or computing a value shown - rather than setup (see
  [[setup-heads]]), a table or a bare literal / symbol (which show nothing computed)?"
  [form]
  (and (seq? form) (not (setup-heads (first form)))))

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
    (is (= [:a] (tables-without-example [(list options-table :a) :kw "text" 42 'sym])))))

(defn- data-value?
  "Is `v` data a page shows - rows (a non-empty sequence of maps) or columns (a map of equal-length non-empty
  sequences)?"
  [v]
  (or (and (sequential? v) (seq v) (every? map? (take 20 v)))
      (and (map? v) (seq v) (every? sequential? (vals v)) (pos? (count (first (vals v))))
           (apply = (map count (vals v))))))

(defn- data-vars
  "Names (symbols) of the vars of notebook namespace `ns-sym` (loaded here) holding data."
  [ns-sym]
  (require ns-sym)
  (set (for [[n v] (ns-publics ns-sym) :when (data-value? @v)] n)))

(defn- defs-without-table
  "Names of the `defs` among top-level `forms` not immediately followed by `(nu/data-table name)`."
  [forms defs]
  (vec (for [[form next-form] (partition 2 1 (concat forms [nil]))
             :when (and (seq? form) (= 'def (first form)) (contains? defs (second form)))
             :when (not (and (seq? next-form) (= 'notebook-utils/data-table (first next-form))
                             (= (second form) (second next-form))))]
         (second form))))

(deftest data-tables-test
  (doseq [[page forms] (notebook-forms)
          :let [ns-sym (second (first (filter #(and (seq? %) (= 'ns (first %))) forms)))]]
    (testing (str page ": every dataset or data defined is shown right after its definition (nu/data-table)")
      (is (= [] (defs-without-table forms (data-vars ns-sym)))))))

(deftest data-tables-rule-test
  (testing "boundary: a table right after the def; a table later; none; a non-data def"
    (is (= [] (defs-without-table ['(def d [{:a 1}]) '(notebook-utils/data-table d)] #{'d})))
    (is (= ['d] (defs-without-table ['(def d [{:a 1}]) '(f) '(notebook-utils/data-table d)] #{'d})))
    (is (= ['d] (defs-without-table ['(def d [{:a 1}])] #{'d})))
    (is (= [] (defs-without-table ['(def p {:charts []})] #{}))))
  (testing "data values: rows, columns; not plots, keyword lists or empty data"
    (is (data-value? [{:a 1}]))
    (is (data-value? {:t (range 3) :v [1 2 3]}))
    (is (not (data-value? {:data [{:a 1}] :charts []})))
    (is (not (data-value? [:a :b])))
    (is (not (data-value? [])))))

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
