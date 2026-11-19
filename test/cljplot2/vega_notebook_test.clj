(ns cljplot2.vega-notebook-test
  "Vega gallery notebook: every dataset it reads is in the manifest; the exported vega-datasets (`dev/vega_data.clj`)
  have kebab-case keys, ISO dates, every original column and every nested series."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest testing is]]
            [notebook-utils :as nu]))

(def ^:private notebook (slurp (io/file "notebooks/galleries/vega.clj")))

(defn- used-datasets
  "Dataset keys the notebook reads (`nu/dataset`) or credits (`:dataset`)."
  []
  (set (map (comp keyword second) (re-seq #"(?:nu/dataset|:dataset) :([a-z0-9-]+)" notebook))))

(defn- vega-entries
  "Manifest entries written by `dev/vega_data.clj`."
  []
  (into {} (filter #(= "dev/vega_data.clj" (:via (val %)))) (nu/datasets)))

(deftest datasets-in-manifest-test
  (testing "every dataset the notebook names is in the manifest"
    (let [used (used-datasets)]
      (is (seq used))
      (is (empty? (remove (set (keys (nu/datasets))) used))))))

(deftest vega-export-test
  (testing "36 exported files (35 vega-datasets files, miserables as nodes and links), clashes suffixed -vega"
    (let [entries (vega-entries)]
      (is (= 36 (count entries)))
      (is (every? entries [:barley-vega :seattle-weather-vega :flare-vega :flare-dependencies-vega :volcano-vega
                           :countries-vega :miserables-links-vega :miserables-nodes :cars :movies]))
      (is (not-any? entries [:barley :seattle-weather :countries]) "hand-written entries untouched")))
  (testing "kebab-case keys, ISO dates"
    (doseq [k (keys (vega-entries))
            :let [row (first (nu/dataset k))]]
      (is (every? #(re-matches #"[a-z0-9]+(-[a-z0-9]+)*" (name %)) (keys row)) (str k)))
    (is (re-matches #"\d{4}-\d{2}-\d{2}" (:date (first (nu/dataset :stocks)))))
    (is (re-matches #"\d{4}-\d{2}-\d{2}" (:release-date (first (nu/dataset :movies)))))
    (is (re-matches #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}" (:date (first (nu/dataset :flights-20k)))))))

(deftest vega-export-completeness-test
  (testing "a key missing from the first object is kept (null where absent)"
    (is (= [true] (keep :commonwealth (nu/dataset :monarchs))) "Cromwell's Commonwealth")
    (is (contains? (first (nu/dataset :monarchs)) :commonwealth))
    (is (every? #(contains? % :parent) (nu/dataset :flare-vega)))
    (is (some :size (nu/dataset :flare-vega)))
    (is (some :p-life-expect (nu/dataset :countries-vega))))
  (testing "weekly-weather: five series per day, the forecasts only for the days without observations"
    (let [rows (nu/dataset :weekly-weather)
          present (fn [series] (set (map :id (filter #(and (= series (:series %)) (:low %)) rows))))]
      (is (= 50 (count rows)))
      (is (= #{"record" "normal" "actual" "forecast-low" "forecast-high"} (set (map :series rows))))
      (is (= (present "forecast-low") (present "forecast-high")))
      (is (empty? (filter (present "actual") (present "forecast-low"))))
      (is (= (set (range 10)) (into (present "actual") (present "forecast-low")))))))
