(ns cljplot2.reference-notebook-test
  "Reference part notebooks (`notebooks/reference/`): the part's chapters in the book; every non-chart schema, shared
  group and theme section shown by options tables - a sliced table (rows with `:section`) one slice at a time, every
  slice shown, no table longer than [[max-table-rows]]; tables and Markdown through `notebook-utils` (code hidden);
  the notebook conventions (image size, seeded rng, dataset credits). Notebooks are read as forms, their aliases
  resolved - a page names an item by calling or referring to it; a table's rows form is evaluated here. Charts,
  builders and data functions get their pages in R4 / R5 (spec 7.2)."
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
  (testing "the Reference part lists the concept chapters, before Use cases"
    (is (= (map #(str "reference/" % ".clj") concept-chapters) (take (count concept-chapters) (reference-paths))))
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

(defn- illustration?
  "Is top-level `form` an example - a chart or a value shown - rather than setup (`ns`, `def`, a source note, a wadogo
  kind doc) or a table?"
  [form]
  (not (and (seq? form)
            (#{'ns 'def 'defn 'notebook-utils/source-note 'notebook-utils/scale-kind-doc options-table} (first form)))))

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
    (is (= [] (tables-without-example [])))))

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
