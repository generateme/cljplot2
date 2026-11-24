(ns cljplot2.demo.data
  "Datasets for the demos: `notebooks/data` - one copy for the book and the demos, licences in
  `notebooks/data/datasets.edn` - and the demo-only files of `dev/resources` (classpath). Paths are relative to the
  project root (where `clojure -M:demo` runs).

  Functions: [[csv-rows]], [[typed-csv-rows]], [[json-rows]], [[chem97-rows]], [[resource-csv-rows]]."
  (:require [clojure.data.csv :as csv]
            [clojure.data.json :as json]
            [clojure.java.io :as io]))

(def ^:private folder "notebooks/data/")

(defn- parse-csv
  "Gives the rows of CSV text: maps with keyword keys from the header, values as strings."
  [text]
  (let [[header & rows] (csv/read-csv text)
        ks (mapv keyword header)]
    (mapv #(zipmap ks %) rows)))

(defn csv-rows
  "Gives the rows of a CSV file in `notebooks/data`.

  Parameters:

  - `file` (string): the file name.

  Returns maps with keyword keys from the header, values as strings."
  [file]
  (parse-csv (slurp (io/file (str folder file)))))

(defn typed-csv-rows
  "Gives the rows of a CSV file in `notebooks/data` with numeric cells as doubles.

  Parameters:

  - `file` (string): the file name.

  Returns maps with keyword keys from the header; a cell reading as a number is a double, others stay strings."
  [file]
  (mapv (fn [row] (update-vals row #(or (parse-double %) %))) (csv-rows file)))

(defn json-rows
  "Gives the rows of a JSON file (an array of objects) in `notebooks/data`.

  Parameters:

  - `file` (string): the file name.

  Returns maps with keyword keys."
  [file]
  (json/read-str (slurp (io/file (str folder file))) :key-fn keyword))

(defn chem97-rows
  "Gives the rows of `chem97.csv` typed for the demos.

  Returns maps `{:score :gender :age :gcsescore}`: `:score` and `:age` longs, `:gcsescore` a double, `:gender` a string."
  []
  (mapv (fn [{:keys [score gender age gcsescore]}]
          {:score (parse-long score) :gender gender :age (parse-long age) :gcsescore (parse-double gcsescore)})
        (csv-rows "chem97.csv")))

(defn resource-csv-rows
  "Gives the rows of a demo-only CSV file of `dev/resources` (a classpath resource).

  Parameters:

  - `resource` (string): the resource name.

  Returns maps with keyword keys from the header, values as strings."
  [resource]
  (parse-csv (slurp (io/resource resource))))
