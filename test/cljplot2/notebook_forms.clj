(ns cljplot2.notebook-forms
  "Book notebooks read as forms for tests: top-level forms with alias-qualified symbols resolved, calls of a function,
  the data a page defines and the datasets it reads - and whether each is shown by a data table (`nu/data-table`)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
            [notebook-utils :as nu])
  (:import [java.io PushbackReader]
           [fastmath.vector Vec2 Vec3 Vec4]))

(set! *warn-on-reflection* true)

(defn read-forms
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

(defn resolved-forms
  "Forms of the notebook at `path` with alias-qualified symbols (`plot/arrange`) fully qualified."
  [path]
  (let [forms (read-forms path)
        as (aliases forms)]
    (walk/postwalk #(if-let [n (and (symbol? %) (as (namespace %)))] (symbol (str n) (name %)) %) forms)))

(defn calls
  "Every call of function symbol `f` anywhere in `forms`."
  [forms f]
  (filter #(and (seq? %) (= f (first %))) (tree-seq coll? seq forms)))

(defn book-chapters
  "Paths (relative to `notebooks/`) of every chapter of the book (`notebooks/chapters.edn`), in order."
  []
  (vec (mapcat #(if (map? %) (:chapters %) [%]) (edn/read-string (slurp "notebooks/chapters.edn")))))

(defn page-name
  "Notebook file name of chapter `path` without `.clj` (`\"use_cases/flow.clj\"` -> `\"flow\"`)."
  [path]
  (second (re-find #"([^/]+)\.clj$" path)))

(defn page-ns
  "The namespace symbol of the notebook's resolved `forms`."
  [forms]
  (second (first (filter #(and (seq? %) (= 'ns (first %))) forms))))

;; data shown

(def ^:private data-table 'notebook-utils/data-table)
(def ^:private dataset 'notebook-utils/dataset)

(def ^{:private true :const true :tag 'long} min-column-rows
  "The fewest values of a columns map counted as data - fewer is a spec or a domain (`{:domain [a b] :range [c d]}`,
  `{:x [lo hi] :y [lo hi]}`), not a table."
  3)

(def ^{:private true :const true :tag 'long} max-non-sample-size
  "The most numbers in a sequence not counted as data - a list of breaks, ticks or a domain, not a sample."
  12)

(defn- scalar?
  "Tests whether `v` is a table cell value - a number, string, keyword, boolean, `java.time` value or `nil` (not a
  collection)."
  [v]
  (or (nil? v) (number? v) (string? v) (keyword? v) (boolean? v) (instance? java.time.temporal.Temporal v)))

(defn- column?
  "Tests whether `v` can be a column - a sequential collection, not a fastmath vector (a colour or a point, e.g.
  `Vec4`)."
  [v]
  (and (sequential? v) (not (instance? Vec2 v)) (not (instance? Vec3 v)) (not (instance? Vec4 v))))

(defn- number-row?
  "Tests whether `v` is a number or a non-empty sequential of numbers (one observation of a sample or a matrix row),
  not a fastmath vector - as `nu/data-table` accepts in a sample."
  [v]
  (or (number? v) (and (column? v) (seq v) (every? number? v))))

(defn data-value?
  "Tests whether `v` is data a page shows - rows (a non-empty sequence of maps), columns (a map of equal-length
  sequences of at least [[min-column-rows]] scalar values; scale specs, domain maps and maps of colours aren't) or a
  sample (a sequence of more than [[max-non-sample-size]] numbers or number vectors; breaks, ticks and domains are
  shorter)."
  [v]
  (or (and (sequential? v) (seq v) (every? map? (take nu/data-table-rows v)))
      (and (column? v) (< max-non-sample-size (count (take (inc max-non-sample-size) v)))
           (every? number-row? (take nu/data-table-rows v)))
      (and (map? v) (seq v) (every? column? (vals v))
           (<= min-column-rows (count (first (vals v))))
           (apply = (map count (vals v)))
           (every? #(every? scalar? (take nu/data-table-rows %)) (vals v)))))

(defn data-vars
  "Names (symbols) of the vars of notebook namespace `ns-sym` (loaded here) holding data."
  [ns-sym]
  (require ns-sym)
  (set (for [[n v] (ns-publics ns-sym) :when (data-value? @v)] n)))

(defn defs-without-table
  "Names of the `defs` among top-level `forms` not immediately followed by `(nu/data-table name)`."
  [forms defs]
  (vec (for [[form next-form] (partition 2 1 (concat forms [nil]))
             :when (and (seq? form) (= 'def (first form)) (contains? defs (second form)))
             :when (not (and (seq? next-form) (= data-table (first next-form))
                             (= (second form) (second next-form))))]
         (second form))))

(defn- datasets-read
  "The dataset keys `form` reads (`(nu/dataset :k)` calls anywhere in it), as a set."
  [form]
  (set (keep #(when (keyword? (second %)) (second %)) (calls [form] dataset))))

(defn datasets-unshown
  "Dataset keys the top-level `forms` read that no data table shows, sorted: a dataset is shown by a top-level
  `(nu/data-table x)` whose argument reads it, or whose argument is a var defined (top-level `def`) by a form
  reading it."
  [forms]
  (let [def-reads (into {} (keep #(when (and (seq? %) (= 'def (first %))) [(second %) (datasets-read %)])) forms)
        shown (set (mapcat (fn [form]
                             (when (and (seq? form) (= data-table (first form)))
                               (let [arg (second form)]
                                 (if (symbol? arg) (def-reads arg) (datasets-read arg)))))
                           forms))]
    (vec (sort (remove shown (datasets-read forms))))))

;; book conventions

(def ^:private source-note 'notebook-utils/source-note)
(def ^:private render-image 'cljplot2.core/render-image)

(defn source-notes
  "The literal option maps of every `(nu/source-note {...})` in `forms`."
  [forms]
  (filter map? (map second (calls forms source-note))))

(defn notes-without-url
  "The literal source notes in `forms` naming an outside example (`:source`) without its `:url`."
  [forms]
  (vec (filter #(and (:source %) (nil? (:url %))) (source-notes forms))))

(defn datasets-uncredited
  "Dataset keys the `forms` read that no literal source note's `:dataset` (a key or a vector of keys; anything else
  credits nothing) credits, sorted."
  [forms]
  (let [credited (set (mapcat #(let [d (:dataset %)] (cond (keyword? d) [d] (vector? d) (filter keyword? d))) (source-notes forms)))]
    (vec (sort (remove credited (datasets-read forms))))))

(defn image-sizes
  "`[width height]` of every `plot/render-image` call in `forms` given a literal size map (a missing size - `nil`);
  calls whose size is computed (a symbol or a call) are skipped."
  [forms]
  (vec (for [call (calls forms render-image)
             :let [size (nth call 2 nil)]
             :when (map? size)
             :let [{:keys [width height]} size]
             :when (or width height)]
         [width height])))
