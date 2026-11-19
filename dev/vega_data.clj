(ns vega-data
  "Datasets of the Vega gallery (`notebooks/galleries/vega.clj`) exported from vega-datasets to `notebooks/data`.
  Run from the repository root: `clojure -M:dev -m vega-data` (downloads from jsDelivr).

  Source: vega-datasets [[version]] (`https://cdn.jsdelivr.net/npm/vega-datasets@<version>/data/`), the files of
  the gallery's \"in\" rows ([[files]]); `datapackage.json` of the same version gives titles, sources and licences.
  Normalisation (every row kept, values as given):

  * keys kebab-case (`Miles_per_Gallon` -> `miles-per-gallon`, `Beak Length (mm)` -> `beak-length-mm`,
    `budgetYear` -> `budget-year`), in the first object's order, then the keys only later objects carry, sorted
  * dates as ISO strings ([[date-columns]]: `Jan 1 2000` -> `2000-01-01`, `2001/01/01 00:47` -> `2001-01-01T00:47`),
    parsed in the notebook with `java.time`
  * nested files as rows: `weekly-weather` - a row per day and series (`record`, `normal`, `actual`, `forecast-low`,
    `forecast-high`); `miserables` - nodes and links files; `volcano` - a row per grid cell; `countries` without its
    `_comment`
  * CSV stays CSV, JSON an array of objects (one per line), `null` kept

  A name already used in `notebooks/data` (a file or a `datasets.edn` key) gets the suffix `-vega` (`barley-vega.json`,
  `:barley-vega`). The manifest entries go between the `vega-datasets` markers of `datasets.edn` (replaced on every
  run, the rest untouched). Re-running writes identical files."
  (:require [clojure.data.csv :as csv]
            [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.time LocalDate LocalDateTime]
           [java.time.format DateTimeFormatter]
           [java.util Locale]))

(set! *warn-on-reflection* true)

(def version "The pinned vega-datasets version." "3.3.1")

(def ^:private package-url (str "https://cdn.jsdelivr.net/npm/vega-datasets@" version "/"))

(def ^:private base (str package-url "data/"))

(def ^:private out "notebooks/data/")

(def files
  "vega-datasets files of the gallery's \"in\" rows."
  ["anscombe.json" "barley.json" "budgets.json" "cars.json" "co2-concentration.csv" "countries.json" "disasters.csv"
   "driving.json" "flare-dependencies.json" "flare.json" "flights-20k.json" "gapminder-health-income.csv" "github.csv"
   "global-temp.csv" "jobs.json" "lookup_groups.csv" "lookup_people.csv" "miserables.json" "monarchs.json"
   "movies.json" "normal-2d.json" "ohlc.json" "penguins.json" "population.json" "seattle-weather-hourly-normals.csv"
   "seattle-weather.csv" "sp500-2000.csv" "stocks.csv" "udistrict.json" "unemployment-across-industries.json"
   "volcano.json" "weather.csv" "weekly-weather.json" "wheat.json" "windvectors.csv"])

(def date-columns
  "File -> original column -> input `DateTimeFormatter` pattern of the dates rewritten as ISO strings (`LocalDate` /
  `LocalDateTime` by whether the pattern has a time). Columns already ISO aren't listed."
  {"stocks.csv" {"date" "MMM d yyyy"}
   "movies.json" {"Release Date" "MMM d yyyy"}
   "flights-20k.json" {"date" "yyyy/MM/dd HH:mm"}
   "github.csv" {"time" "yyyy/MM/dd HH:mm:ss"}})

(defn kebab
  "Kebab-case key text of `k`: camel-case humps and runs of other characters split by `-`, lower case."
  [k]
  (-> (str k)
      (str/replace #"([a-z0-9])([A-Z])" "$1-$2")
      (str/lower-case)
      (str/replace #"[^a-z0-9]+" "-")
      (str/replace #"^-|-$" "")))

(defn- iso-date
  "ISO text of date `s` read with `pattern` (English month names); `nil` / blank stays as is."
  [pattern s]
  (if (str/blank? (str s))
    s
    (let [fmt (DateTimeFormatter/ofPattern pattern Locale/ENGLISH)]
      (if (re-find #"H" pattern)
        (str (LocalDateTime/parse s fmt))
        (str (LocalDate/parse s fmt))))))

(defn- fetch
  "Text of vega-datasets file `file`."
  [file]
  (slurp (str base file)))

(defn- first-object-keys
  "Keys of the first object of the JSON array `text`, in their order (data.json keeps no key order)."
  [text]
  (let [obj (re-find #"\{[^{}]*\}" text)]
    (mapv second (re-seq #"\"((?:[^\"\\]|\\.)*)\"\s*:" obj))))

(defn- all-keys
  "`first-keys` followed by the keys of `objects` missing from them, sorted (an object may carry a key the first one
  lacks, e.g. monarchs' `commonwealth`)."
  [first-keys objects]
  (into first-keys (sort (remove (set first-keys) (distinct (mapcat keys objects))))))

(defn- ordered-rows
  "Rows of `objects` as ordered maps of kebab keys over the original `keys` and any later object's extra keys
  (dropping `drop-keys`; a missing value - `null`), dates of `dates` (original column -> pattern) rewritten."
  [objects keys dates drop-keys]
  (let [keys (remove (set drop-keys) (all-keys keys objects))]
    (mapv (fn [o] (apply array-map (mapcat (fn [k] [(kebab k) (if-let [p (dates k)] (iso-date p (get o k)) (get o k))]) keys)))
          objects)))

(defn- json-text
  "JSON array text of `rows` (ordered maps), one object per line."
  [rows]
  (str "[\n" (str/join ",\n" (map json/write-str rows)) "\n]\n"))

(defn- csv-text
  "CSV text of `text` (CSV) with kebab-case headers and the date columns of `dates` rewritten."
  [text dates]
  (let [[header & rows] (csv/read-csv text)
        fs (mapv (fn [h] (if-let [p (dates h)] #(iso-date p %) identity)) header)]
    (with-out-str
      (csv/write-csv *out* (cons (mapv kebab header) (map (fn [r] (mapv #(%1 %2) fs r)) rows))))))

(defn- stem
  "File name `name` without its `.json` / `.csv` extension."
  [name]
  (str/replace name #"\.(json|csv)$" ""))

(defn- table
  "Exported `[[name text] ...]` of the flat file `file` with `text`."
  [file text]
  (let [dates (get date-columns file {})
        base-name (kebab (stem file))]
    (if (str/ends-with? file ".csv")
      [[(str base-name ".csv") (csv-text text dates)]]
      [[(str base-name ".json")
        (json-text (ordered-rows (json/read-str text) (first-object-keys text) dates
                                 (when (= file "countries.json") ["_comment"])))]])))

(defn- nested
  "Exported `[[name text] ...]` of a nested file, or `nil` for a flat one."
  [file text]
  (case file
    "weekly-weather.json"
    [["weekly-weather.json"
      (json-text (vec (for [{:strs [day id] :as d} (json/read-str text)
                            [series path] [["record" ["record"]] ["normal" ["normal"]] ["actual" ["actual"]]
                                           ["forecast-low" ["forecast" "low"]] ["forecast-high" ["forecast" "high"]]]
                            :let [{:strs [high low]} (get-in d path)]]
                        (array-map "id" id "day" day "series" series "high" high "low" low))))]]
    "miserables.json"
    (let [{:strs [nodes links]} (json/read-str text)]
      [["miserables-nodes.json" (json-text (mapv (fn [{:strs [index name group]}] (array-map "index" index "name" name "group" group)) nodes))]
       ["miserables-links.json" (json-text (mapv (fn [{:strs [source target value]}] (array-map "source" source "target" target "value" value)) links))]])
    "volcano.json"
    (let [{:strs [width values]} (json/read-str text)
          w (long width)]
      [["volcano.json" (json-text (vec (map-indexed (fn [i v] (array-map "row" (quot (long i) w) "col" (rem (long i) w) "value" v)) values)))]])
    nil))

(defn- taken-names
  "Names used by the hand-written part of `datasets.edn` text `manifest` (outside the generated block): its keys and
  its files' stems."
  [manifest]
  (let [entries (edn/read-string manifest)]
    (into (set (map name (keys entries))) (map (comp stem :file) (vals entries)))))

(def ^:private begin-marker ";; --- vega-datasets (generated by dev/vega_data.clj - do not edit) ---")
(def ^:private end-marker ";; --- end of vega-datasets ---")

(defn- hand-written
  "`datasets.edn` text without its generated block (the markers and what lies between)."
  [manifest]
  (str/replace manifest (re-pattern (str "(?s)\n?" (java.util.regex.Pattern/quote begin-marker) ".*?"
                                         (java.util.regex.Pattern/quote end-marker)))
               ""))

(defn- unique-name
  "`[file-name key]` of exported file `name`: the suffix `-vega` when its stem is in `taken`."
  [name taken]
  (let [ext (re-find #"\.(?:json|csv)$" name)
        s (if (contains? taken (stem name)) (str (stem name) "-vega") (stem name))]
    [(str s ext) (keyword s)]))

(defn- resource-of
  "The `datapackage.json` resource of `file`."
  [package file]
  (first (filter #(= file (get % "path")) (get package "resources"))))

(defn- row-count
  "Rows of the exported `text` of file `name`."
  [name text]
  (if (str/ends-with? name ".csv")
    (dec (count (csv/read-csv text)))
    (count (json/read-str text))))

(defn- columns
  "Column names of the exported `text` of file `name`."
  [name text]
  (if (str/ends-with? name ".csv")
    (first (csv/read-csv text))
    (keys (first (json/read-str text)))))

(defn- entry
  "Manifest entry text of exported file `name` (key `k`, `text`) from vega-datasets `file` (its `resource`)."
  [k name text file resource]
  (let [licences (get resource "licenses")
        sources (get resource "sources")]
    (str " " k " "
         (pr-str (array-map :file name
                            :format (if (str/ends-with? name ".csv") :csv :json)
                            :title (or (get resource "title") file)
                            :source (str "vega-datasets " version " `" file "` (" base file ")"
                                         (when (seq sources)
                                           (str "; original: " (str/join ", " (map #(get % "title") sources)))))
                            :licence (if (seq licences)
                                       (str (str/join ", " (map #(get % "name") licences)) " (vega-datasets `datapackage.json`)")
                                       "none listed - see vega-datasets (cite Vega as the source)")
                            :via "dev/vega_data.clj"
                            :notes (str (row-count name text) " rows: "
                                        (str/join ", " (map #(str "`" % "`") (columns name text))) "."))))))

(defn -main
  "Download, normalise and write every file of [[files]] to `notebooks/data`; rewrite the generated block of
  `datasets.edn`. Throws when two exported files would get one name."
  [& _]
  (let [package (json/read-str (slurp (str package-url "datapackage.json")))
        manifest-file (io/file out "datasets.edn")
        manifest (hand-written (slurp manifest-file))
        taken (taken-names manifest)
        exported (vec (for [file files
                            :let [text (fetch file)]
                            [name content] (or (nested file text) (table file text))
                            :let [[name k] (unique-name name taken)]]
                        {:file file :name name :key k :content content}))]
    (when-let [dup (first (keep (fn [[n c]] (when (> (long c) 1) n)) (frequencies (map :name exported))))]
      (throw (ex-info "Two exported files share a name." {:name dup})))
    (doseq [{:keys [name content]} exported]
      (spit (io/file out name) content)
      (println "wrote" name))
    (spit manifest-file (str (str/replace manifest #"\s*\}\s*$" "") "\n" begin-marker "\n"
                             (str/join "\n" (map (fn [{:keys [file name key content]}]
                                                   (entry key name content file (resource-of package file)))
                                                 exported))
                             "\n" end-marker "\n}\n"))
    (println (count exported) "datasets in" (str out "datasets.edn"))))
