(ns notebooks.notebook-utils-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [notebook-utils :as nu]
            [cljplot2.test-util :refer [error-data]]))

(deftest dataset-test
  (testing "rows of a manifest dataset"
    (let [rows (nu/dataset :faithful)]
      (is (= 272 (count rows)))
      (is (= #{:eruptions :waiting} (set (keys (first rows)))))
      (is (every? number? (map :waiting rows)))))
  (testing "unknown key names the known ones"
    (is (= {:key :dataset :value :nope :known (set (keys (nu/datasets)))} (error-data #(nu/dataset :nope))))
    (is (= :dataset (:key (error-data #(nu/dataset nil))))))
  (testing "manifest entries without a readable file or with an unknown format"
    (with-redefs [nu/datasets (constantly {:gone {:file "gone.json" :format :json}
                                           :no-file {:format :json}
                                           :xml {:file "faithful.json" :format :xml}})]
      (is (= {:key :file :value "gone.json" :dataset :gone} (error-data #(nu/dataset :gone))))
      (is (= {:key :file :value nil :dataset :no-file} (error-data #(nu/dataset :no-file))))
      (is (= {:key :format :value :xml :dataset :xml} (error-data #(nu/dataset :xml))))))
  (testing "a CSV entry's :strings reach parse-csv"
    (let [seen (atom nil)]
      (with-redefs [nu/datasets (constantly {:ids {:file "faithful.json" :format :csv :strings #{:id}}})
                    nu/parse-csv (fn [_ options] (reset! seen options) [])]
        (nu/dataset :ids)
        (is (= {:strings #{:id}} @seen))))))

(def ^:private full-load-limit
  "Datasets up to this many bytes are loaded in full by [[manifest-test]]; bigger ones only checked by file and header."
  100000)

(deftest manifest-test
  (testing "every manifest entry: a readable file with source, licence and via; small ones load as rows of maps,
            big CSV ones have a header row"
    (doseq [[k {:keys [file format source licence via title]}] (nu/datasets)
            :let [resource (io/resource (str "data/" file))]]
      (is (some? resource) (str k))
      (is (every? string? [source licence via title]) (str k))
      (if (<= (.length (io/file resource)) full-load-limit)
        (let [rows (nu/dataset k)]
          (is (seq rows) (str k))
          (is (every? map? rows) (str k)))
        (with-open [r (io/reader resource)]
          (let [first-line (first (line-seq r))]
            (if (= :csv format)
              (is (< 1 (count (str/split first-line #","))) (str k " header"))
              (is (str/starts-with? first-line "[") (str k " JSON array"))))))))
  (testing "CSV :strings columns stay strings (ids, dates)"
    (is (every? string? (map :date (nu/dataset :lincoln-weather))))))

(deftest parse-csv-test
  (testing "empty input and header only"
    (is (= [] (nu/parse-csv "")))
    (is (= [] (nu/parse-csv "a,b\n"))))
  (testing "numeric columns: long, double, exponent, sign, empty values"
    (is (= [{:a 1 :b 2.5 :c "x"} {:a -3 :b 1000.0 :c "1"} {:a nil :b -0.5 :c nil}]
           (nu/parse-csv "a,b,c\n1,2.5,x\n-3,1e3,1\n,-.5,\n"))))
  (testing "a column with one non-number stays strings (NA is not a number)"
    (is (= ["1" "NA" "3"] (map :a (nu/parse-csv "a\n1\nNA\n3\n")))))
  (testing "all-empty column holds nil"
    (is (= [nil nil] (map :b (nu/parse-csv "a,b\n1,\n2,\n")))))
  (testing "integral values beyond long range become doubles"
    (is (= [1.0E20 1.0] (map :a (nu/parse-csv "a\n100000000000000000000\n1.0\n")))))
  (testing "quoted values and blank lines (leading, inner, trailing)"
    (is (= [{:name "a, b" :v 1} {:name "c" :v 2}] (nu/parse-csv "\nname,v\n\"a, b\",1\n\nc,2\n\n"))))
  (testing "single column: inner blank lines are missing values, trailing ones are dropped"
    (is (= [{:a 1} {:a nil} {:a 3}] (nu/parse-csv "a\n1\n\n3\n\n\n")))
    (is (= [] (nu/parse-csv "a\n\n\n"))))
  (testing ":strings keeps id columns as strings"
    (is (= [{:id "02134" :v 1} {:id "00501" :v 2}] (nu/parse-csv "id,v\n02134,1\n00501,2\n" {:strings #{:id}})))
    (is (= [2134 501] (map :id (nu/parse-csv "id,v\n02134,1\n00501,2\n"))))
    (is (= [{:a "1"}] (nu/parse-csv "a\n1\n" {:strings [:a :unknown]}))))
  (testing "a row of the wrong length throws"
    (is (= {:key :csv :row 2 :value ["1" "2" "3"]} (error-data #(nu/parse-csv "a,b\n1,2\n1,2,3\n"))))
    (is (= {:key :csv :row 1 :value ["1"]} (error-data #(nu/parse-csv "a,b\n1\n"))))))

(deftest source-note-test
  (let [text (fn [note] (apply str (if (string? note) [note] note)))]
    (testing "markdown with hidden code"
      (let [note (nu/source-note {:source "R Graph Gallery - Histogram" :dataset :faithful})]
        (is (= :kind/md (:kindly/kind (meta note))))
        (is (true? (:kindly/hide-code (meta note))))
        (is (= (str "Source: R Graph Gallery - Histogram. Data: Old Faithful geyser eruptions, "
                    "R datasets package `faithful` (Härdle 1991; Azzalini & Bowman 1990); "
                    "licence: Part of R (GPL-2 | GPL-3).")
               (text note)))
        (is (not (re-find #"via|dev/resources" (text note))))))
    (testing "source or dataset alone"
      (is (= "Source: Lattice Fig 1.1." (text (nu/source-note {:source "Lattice Fig 1.1"}))))
      (is (re-matches #"Data: Old Faithful.*" (text (nu/source-note {:dataset :faithful})))))
    (testing "links: one URL links the source, several are numbered, a URL alone links itself"
      (is (= "Source: [RGG - Violin](https://r-graph-gallery.com/violin.html)."
             (text (nu/source-note {:source "RGG - Violin" :url "https://r-graph-gallery.com/violin.html"}))))
      (is (= "Source: RGG ([1](https://a.org/x), [2](https://b.org/y))."
             (text (nu/source-note {:source "RGG" :url ["https://a.org/x" "https://b.org/y"]}))))
      (is (= "Source: [https://a.org/x](https://a.org/x)." (text (nu/source-note {:url "https://a.org/x"}))))
      (is (re-matches #"Source: \[RGG\]\(https://a.org/x\)\. Data: Old Faithful.*"
                      (text (nu/source-note {:source "RGG" :url ["https://a.org/x"] :dataset :faithful})))))
    (testing "a bad :url throws"
      (doseq [bad [[] 12 ["ok" 3] '("https://a.org")]]
        (is (= :url (:key (error-data #(nu/source-note {:source "x" :url bad})))) (pr-str bad))))
    (testing "neither, or an unknown dataset, throws"
      (is (= {:key :source-note} (error-data #(nu/source-note {}))))
      (is (= :dataset (:key (error-data #(nu/source-note {:source "x" :dataset :nope}))))))))

(deftest levels-test
  (testing "R factor order, not alphabetical or first appearance"
    (is (= ["Grand Rapids" "Duluth" "University Farm" "Morris" "Crookston" "Waseca"] (nu/levels :barley :site)))
    (is (= ["Expected or Additional Training" "Work with Specific Person" "Training Outside PhD Field"
            "Other Employment Not Available" "Other"]
           (nu/levels :postdoc :reason))))
  (testing "typed as the dataset loads the column: numbers, or strings for :strings columns"
    (is (= [1932 1931] (nu/levels :barley :year)))
    (is (every? number? (nu/levels :gvhd10 :days)))
    (is (every? string? (nu/levels :gvhd :patient)) "patient is a :strings column"))
  (testing "every recorded level column belongs to a manifest dataset; the loaded values are levels"
    (let [recorded (edn/read-string (slurp (io/resource "data/levels.edn")))]
      (doseq [[k columns] recorded
              :let [rows (when (contains? #{:barley :titanic :postdoc :va-deaths :oats :states :seatac-weather} k)
                           (nu/dataset k))]
              [column _] columns]
        (is (contains? (nu/datasets) k) (str k))
        (when rows
          (is (every? (set (nu/levels k column)) (keep column rows)) (str k " " column))))))
  (testing "unknown dataset or column - {:key :levels}"
    (is (= :levels (:key (ex-data (try (nu/levels :barley :nope) (catch clojure.lang.ExceptionInfo e e))))))
    (is (= :levels (:key (ex-data (try (nu/levels :faithful :eruptions) (catch clojure.lang.ExceptionInfo e e))))))))
