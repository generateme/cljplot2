(ns cljplot2.reference-notebook-test
  "Reference part notebooks (`notebooks/reference/`): the part's chapters in the book, every non-chart schema, shared
  group and theme section shown by an options table, and the notebook conventions (image size, seeded rng, dataset
  credits). Notebooks are read as forms, their aliases resolved - a page names an item by calling or referring to it.
  Charts, builders and data functions get their pages in R4 / R5 (spec 7.2)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest testing is]]
            [clojure.walk :as walk]
            [cljplot2.describe :as describe]
            [cljplot2.scale.aesthetic :as aesthetic])
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

(defn- options-table-args
  "The arguments of every `describe/options` call inside an options table anywhere in `forms`."
  [forms]
  (set (for [table (calls forms 'notebook-utils/options-table)
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

(def ^:private plot-level-schemas
  "Every non-chart schema users write, as the symbol a page passes to `describe/options` (the aesthetic scale specs
  are calls, see [[aesthetic-scale-tables-test]])."
  '#{cljplot2.pipeline/options-schema cljplot2.pipeline/plot-schema cljplot2.data.rows/mapping-schema
     cljplot2.scale.position/scale-spec-schema cljplot2.data.facet/scale-spec-schema cljplot2.data.facet/facet-options
     cljplot2.layout.plot-grid/labels-schema cljplot2.content.label/label-options
     cljplot2.layout.plot-grid/arrange-schema
     cljplot2.scale.coord/polar-schema cljplot2.pipeline/margins-schema cljplot2.pipeline/margin-spec-schema})

(deftest schema-tables-test
  (let [shown (options-table-args (vals (notebook-forms)))]
    (testing "every plot-level schema has an options table"
      (is (= #{} (into (sorted-set) (remove shown) plot-level-schemas))))
    (testing "the builders of the concept chapters (arrange, pairs) have options tables"
      (is (every? shown '[(var cljplot2.core/arrange) (var cljplot2.core/pairs)])))))

(deftest aesthetic-scale-tables-test
  (testing "every aesthetic dimension's scale spec has an options table"
    (is (= (set aesthetic/dims)
           (set (for [arg (options-table-args (vals (notebook-forms)))
                      :when (and (seq? arg) (= 'cljplot2.scale.aesthetic/scale-spec-schema (first arg)))]
                  (second arg)))))))

(deftest fragment-sections-test
  (testing "every shared group has a table in the Shared options chapter"
    (is (= (set (keys describe/fragments))
           (set (map second (calls (get (notebook-forms) "shared_options") 'cljplot2.describe/fragment-rows)))))))

(deftest theme-sections-test
  (testing "every top-level theme section has a table in the Theme chapter"
    (is (= (set (map (comp first :path) (describe/theme-rows)))
           (set (map second (calls (get (notebook-forms) "theme") 'cljplot2.describe/theme-rows)))))))

(deftest conventions-test
  (doseq [[page forms] (notebook-forms)]
    (testing (str page ": images at most 1000 x 800, sizes given")
      (doseq [call (calls forms 'cljplot2.core/render-image)
              :let [{:keys [width height]} (nth call 2 nil)]]
        (is (and (number? width) (number? height) (<= width 1000) (<= height 800)) [page width height])))
    (testing (str page ": every random generator seeded")
      (is (every? #(<= 3 (count %)) (calls forms 'fastmath.random/rng)) page))
    (testing (str page ": every dataset read is credited")
      (let [read (set (map second (calls forms 'notebook-utils/dataset)))
            credited (set (keep #(:dataset (second %)) (calls forms 'notebook-utils/source-note)))]
        (is (= read credited) page)))))
