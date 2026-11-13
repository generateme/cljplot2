(ns notebook-utils
  "Notebook helpers: datasets read through the manifest `data/datasets.edn` and the attribution line of an example.
  No plotting helpers - examples use the plain cljplot2 API."
  (:require [clojure.data.csv :as csv]
            [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [scicloj.kindly.v4.api :as kindly]
            [scicloj.kindly.v4.kind :as kind]))

(def ^:private data-folder "data/")

(defn datasets
  "The dataset manifest: key -> `{:file :format :title :source :licence :via :notes}` (`data/datasets.edn` on the
  classpath)."
  []
  (if-let [manifest (io/resource (str data-folder "datasets.edn"))]
    (edn/read-string (slurp manifest))
    (throw (ex-info "Dataset manifest data/datasets.edn not found on the classpath." {:key :manifest}))))

(defn- manifest-entry
  "Manifest entry of dataset `k`; throws `ex-info` naming the known keys for an unknown one."
  [k]
  (let [manifest (datasets)]
    (or (get manifest k)
        (throw (ex-info (str "Unknown dataset " (pr-str k) ", known: " (str/join ", " (sort (keys manifest))) ".")
                        {:key :dataset :value k :known (set (keys manifest))})))))

(def ^:private long-pattern #"[-+]?\d+")
(def ^:private double-pattern #"[-+]?(\d+\.?\d*|\.\d+)([eE][-+]?\d+)?")

(defn- parse-number
  "Number of the string `s` (long when integral and in range, else double), `nil` when `s` isn't a number."
  [^String s]
  (cond
    (re-matches long-pattern s) (try (Long/parseLong s) (catch NumberFormatException _ (Double/parseDouble s)))
    (re-matches double-pattern s) (Double/parseDouble s)
    :else nil))

(defn- numeric-column?
  "Does every non-empty value of `values` parse as a number (and at least one value is non-empty)?"
  [values]
  (let [present (remove str/blank? values)]
    (and (seq present) (every? parse-number present))))

(defn parse-csv
  "Rows (maps with keyword keys from the header row) of CSV `text`. Empty values become `nil`; a column whose every
  non-empty value is a number holds numbers (long or double), other columns hold strings.
  Throws `ex-info` for a row whose length differs from the header's."
  [text]
  (let [[header & rows] (remove #(= [""] %) (csv/read-csv text))
        ks (mapv keyword header)]
    (doseq [[i row] (map-indexed vector rows)
            :when (not= (count ks) (count row))]
      (throw (ex-info (str "CSV row " (inc i) " has " (count row) " values, the header has " (count ks) ".")
                      {:key :csv :row (inc i) :value row})))
    (let [numeric (set (filter #(numeric-column? (map (fn [row] (nth row %)) rows)) (range (count ks))))]
      (mapv (fn [row]
              (into {} (map-indexed (fn [i v]
                                      [(ks i) (cond (str/blank? v) nil
                                                    (numeric i) (parse-number v)
                                                    :else v)])
                                    row)))
            rows))))

(defn dataset
  "Rows (maps with keyword keys) of dataset `k` from the manifest, see [[datasets]].
  Throws `ex-info` for an unknown key, a missing file or an unknown `:format`."
  [k]
  (let [{:keys [file format]} (manifest-entry k)
        resource (and file (io/resource (str data-folder file)))]
    (when-not resource
      (throw (ex-info (str "File of dataset " (pr-str k) " not found: " (pr-str file) ".")
                      {:key :file :value file :dataset k})))
    (case format
      :json (json/read-str (slurp resource) :key-fn keyword)
      :csv (parse-csv (slurp resource))
      (throw (ex-info (str "Dataset " (pr-str k) " has an unknown :format " (pr-str format) ", known: :json, :csv.")
                      {:key :format :value format :dataset k})))))

(defn source-note
  "Attribution line of an example (Markdown, its own code hidden on the page):
  `:source` - where the use case or figure comes from (e.g. \"R Graph Gallery - Histogram\", \"Lattice Fig 1.1\"),
  `:dataset` - manifest key of the data (title and licence are read from the manifest).
  At least one of them is required; an unknown dataset throws like [[dataset]]."
  [{:keys [source dataset]}]
  (when-not (or source dataset)
    (throw (ex-info "source-note needs :source or :dataset." {:key :source-note})))
  (let [data-text (when dataset
                    (let [{:keys [title licence via]} (manifest-entry dataset)]
                      (str "Data: " title " (" licence "; via " via ").")))]
    (kindly/hide-code
     (kind/md (str/join " " (remove nil? [(when source (str "Source: " source ".")) data-text]))))))
