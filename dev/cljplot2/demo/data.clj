(ns cljplot2.demo.data
  "Datasets of `notebooks/data` for the demos - one copy for the book and the demos; licences in
  `notebooks/data/datasets.edn`. Paths are relative to the project root (where `clojure -M:demo` runs)."
  (:require [clojure.data.csv :as csv]
            [clojure.data.json :as json]
            [clojure.java.io :as io]))

(def ^:private folder "notebooks/data/")

(defn csv-rows
  "Rows of the CSV `file` in `notebooks/data`: maps with keyword keys from the header, values as strings."
  [file]
  (let [[header & rows] (csv/read-csv (slurp (io/file (str folder file))))
        ks (mapv keyword header)]
    (mapv #(zipmap ks %) rows)))

(defn json-rows
  "Rows of the JSON `file` (an array of objects) in `notebooks/data`, keyword keys."
  [file]
  (json/read-str (slurp (io/file (str folder file))) :key-fn keyword))
