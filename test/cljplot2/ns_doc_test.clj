(ns cljplot2.ns-doc-test
  "Namespace docs: every `cljplot2.*` namespace has a doc listing each of its public functions as a `[[name]]` link,
  and every function docstring opens with a statement."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [cljplot2.core]
            [cljplot2.describe]))

(def ^:private exempt
  "Namespaces whose doc refers to their functions by group instead of one by one: the chart constructors (`cljplot2.chart`, one per kind, listed in its table)."
  #{'cljplot2.chart})

(defn- source-namespaces
  "Gives the namespaces of `src/`, loaded, sorted by name."
  []
  (->> (file-seq (io/file "src/cljplot2"))
       (filter #(str/ends-with? (str %) ".clj"))
       (map #(with-open [r (java.io.PushbackReader. (io/reader %))] (second (read r))))
       (sort)
       (map (fn [ns-sym] (require ns-sym) (the-ns ns-sym)))))

(defn- record-factory?
  "Tests whether a var is a record's generated factory (`->R`, `map->R`)."
  [sym]
  (re-find #"^(map)?->" (name sym)))

(deftest namespace-docs-list-public-functions-test
  (doseq [n (source-namespaces)
          :let [ns-sym (ns-name n)
                doc (str (:doc (meta n)))]
          :when (not (exempt ns-sym))]
    (is (seq doc) (str ns-sym " has no doc"))
    (doseq [[sym v] (ns-publics n)
            :when (and (fn? @v) (not (record-factory? sym)))]
      (is (str/includes? doc (str "[[" sym "]]")) (str ns-sym " doc misses [[" sym "]]")))))

(deftest docstring-openings-test
  (doseq [n (source-namespaces)
          [sym v] (ns-publics n)
          :let [doc (:doc (meta v))]
          :when (and doc (fn? @v) (not (record-factory? sym)))]
    (is (not (re-find #"^[^.\n]*\?" (str/replace (str/trim doc) #"`[^`]*`|\[\[[^\]]*\]\]" "")))
        (str (ns-name n) "/" sym " opens with a question"))
    (is (and (re-find #"^[A-Z][a-z-]*s\b" (str/trim doc))
             (not (re-find #"^(This|Its|Thus|Plus|Process|Class|Status|Axis|Bounds|Is|Was)\b" (str/trim doc))))
        (str (ns-name n) "/" sym " should open with a statement verb: " (first (str/split-lines doc))))))
