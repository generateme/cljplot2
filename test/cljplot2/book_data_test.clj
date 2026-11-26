(ns cljplot2.book-data-test
  "Data and conventions of every book chapter (reference, use cases, galleries): every dataset or data a page defines
  is shown right after its definition (`nu/data-table`), and every dataset a page reads is shown by some data table -
  of the dataset itself or of a var defined from it - and credited by a source note; a source note naming an outside
  example links it; images are at most 1000x800 unless listed with their reason; use cases are listed in site order.
  Pages are loaded (their data vars evaluated)."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest testing is]]
            [fastmath.core :as m]
            [fastmath.vector]
            [cljplot2.notebook-forms :as nf]))

(defn- chapter-forms
  "Chapter path -> resolved forms, of every chapter of the book."
  []
  (into (array-map) (map (fn [p] [p (nf/resolved-forms (str "notebooks/" p))])) (nf/book-chapters)))

(deftest data-tables-test
  (doseq [[path forms] (chapter-forms)]
    (testing (str path ": every dataset or data defined is shown right after its definition (nu/data-table)")
      (is (= [] (nf/defs-without-table forms (nf/data-vars (nf/page-ns forms))))))))

(deftest datasets-shown-test
  (doseq [[path forms] (chapter-forms)]
    (testing (str path ": every dataset read is shown by a data table")
      (is (= [] (nf/datasets-unshown forms))))))

(deftest data-tables-rule-test
  (testing "boundary: a table right after the def; a table later; none; a non-data def"
    (is (= [] (nf/defs-without-table ['(def d [{:a 1}]) '(notebook-utils/data-table d)] #{'d})))
    (is (= ['d] (nf/defs-without-table ['(def d [{:a 1}]) '(f) '(notebook-utils/data-table d)] #{'d})))
    (is (= ['d] (nf/defs-without-table ['(def d [{:a 1}])] #{'d})))
    (is (= [] (nf/defs-without-table ['(def p {:charts []})] #{}))))
  (testing "data values: rows, columns; not plots, keyword lists or empty data"
    (is (nf/data-value? [{:a 1}]))
    (is (nf/data-value? {:t (range 3) :v [1 2 3]}))
    (is (not (nf/data-value? {:data [{:a 1}] :charts []})))
    (is (not (nf/data-value? [:a :b])))
    (is (not (nf/data-value? []))))
  (testing "boundary: samples - more than 12 numbers or number vectors; 12 numbers are breaks or ticks"
    (is (nf/data-value? (vec (range 13))))
    (is (nf/data-value? (repeat 13 [1.0 2.0])))
    (is (not (nf/data-value? (vec (range 12)))))
    (is (not (nf/data-value? (vec (repeat 13 "a")))) "text levels aren't a sample")
    (is (not (nf/data-value? (repeat 13 []))) "empty vectors aren't rows")
    (is (not (nf/data-value? (repeat 13 [1 "a"]))) "mixed vectors aren't number rows"))
  (testing "boundary: columns need 3 rows of scalars - a domain map, a scale spec, colour columns aren't data"
    (is (nf/data-value? {:a [1 2 3] :b ["x" "y" nil]}))
    (is (not (nf/data-value? {:x [0 1] :y [0 1]})) "2 rows: a domain")
    (is (not (nf/data-value? {:domain ["a" "b" "c"] :range [[1 2 3] [4 5 6] [7 8 9]]})) "colours aren't scalars")
    (is (not (nf/data-value? {:a [1 2 3] :b [1 2]})) "unequal lengths")
    (is (not (nf/data-value? {"a" (fastmath.vector/vec4 1 2 3 4) "b" (fastmath.vector/vec4 5 6 7 8)})) "a map of colours")))

(deftest datasets-shown-rule-test
  (let [ds (fn [k] (list 'notebook-utils/dataset k))
        table (fn [x] (list 'notebook-utils/data-table x))]
    (testing "shown: a table of the dataset, of a var defined from it, of a computation reading it"
      (is (= [] (nf/datasets-unshown [(list 'f {:data (ds :a)}) (table (ds :a))])))
      (is (= [] (nf/datasets-unshown [(list 'def 'x (list 'map 'g (ds :a))) (table 'x) (list 'f {:data (ds :a)})])))
      (is (= [] (nf/datasets-unshown [(table (list 'remove 'p (ds :a)))])))
      (is (= [] (nf/datasets-unshown [(list 'notebook-utils/result-table (list 'g (ds :a)))])) "a result table of a call reading it"))
    (testing "boundary: read inline only; a var from it never tabled; two datasets, one shown; none read"
      (is (= [:a] (nf/datasets-unshown [(list 'f {:data (ds :a)})])))
      (is (= [:a] (nf/datasets-unshown [(list 'def 'x (ds :a)) (list 'f 'x)])))
      (is (= [:b] (nf/datasets-unshown [(table (ds :a)) (list 'f (ds :b))])))
      (is (= [] (nf/datasets-unshown ['(f x)]))))))

(deftest source-notes-test
  (doseq [[path forms] (chapter-forms)]
    (testing (str path ": a source note naming an outside example (:source) links it (:url)")
      (is (= [] (nf/notes-without-url forms))))
    (testing (str path ": every dataset read is credited by a source note's :dataset")
      (is (= [] (nf/datasets-uncredited forms))))))

(deftest source-notes-rule-test
  (let [note (fn [m] (list 'notebook-utils/source-note m))
        ds (fn [k] (list 'notebook-utils/dataset k))]
    (testing "boundary: a source with or without a url; data only"
      (is (= [] (nf/notes-without-url [(note {:source "s" :url "u"})])))
      (is (= [{:source "s"}] (nf/notes-without-url [(note {:source "s"})])))
      (is (= [] (nf/notes-without-url [(note {:dataset :a})]))))
    (testing "boundary: credited by a key, by a vector; not credited; none read"
      (is (= [] (nf/datasets-uncredited [(note {:dataset :a}) (ds :a)])))
      (is (= [] (nf/datasets-uncredited [(note {:dataset [:a :b]}) (list 'f (ds :a) (ds :b))])))
      (is (= [:b] (nf/datasets-uncredited [(note {:dataset :a}) (list 'f (ds :a) (ds :b))])))
      (is (= [] (nf/datasets-uncredited [(note {:url "u"})])))
      (is (= [:a] (nf/datasets-uncredited [(note {:dataset 'a}) (ds :a)])) "a symbol credits nothing")
      (is (= [:a] (nf/datasets-uncredited [(note {:dataset "a"}) (ds :a)])) "a string credits nothing"))))

(def ^:private max-image-size
  "The largest image a book example renders, `[width height]` (CLAUDE.md, Notebooks)."
  [1000 800])

(def ^{:private true :const true :tag 'long} default-image-side
  "Width or height of a rendered image when its size map leaves it out (`render-image` options)."
  600)

(def ^:private oversize-images
  "Page -> sizes of images allowed over [[max-image-size]], each with the reason its prose states."
  {"galleries/vega.clj" #{[1000 1600]   ; the flare tree: its 220 leaf names need the height (the original's canvas)
                          [1200 1200]}}) ; the radial flare tree: room for the leaf names around the circle

(deftest image-sizes-test
  (let [[max-w max-h] max-image-size]
    (doseq [[path forms] (chapter-forms)]
      (testing (str path ": images at most " max-w "x" max-h ", or listed with their reason")
        (is (= [] (remove (fn [[w h]] (or (and (m/<= (double (or w default-image-side)) (double max-w))
                                                (m/<= (double (or h default-image-side)) (double max-h)))
                                           (contains? (oversize-images path) [w h])))
                          (nf/image-sizes forms))))))))

(deftest image-sizes-rule-test
  (testing "boundary: a literal size map; one key missing; a size from a var or a call; no size"
    (is (= [[1000 800]] (nf/image-sizes ['(cljplot2.core/render-image p {:width 1000 :height 800})])))
    (is (= [[1400 nil]] (nf/image-sizes ['(cljplot2.core/render-image p {:width 1400})])))
    (is (= [] (nf/image-sizes ['(cljplot2.core/render-image p size)])))
    (is (= [] (nf/image-sizes ['(cljplot2.core/render-image p (merge {:width 2000} size))])))
    (is (= [] (nf/image-sizes ['(cljplot2.core/render-image p)])))))

(def ^:private use-case-order
  "Use case pages in the R Graph Gallery site order (colours, from its general knowledge part, last)."
  ["distribution" "correlation" "ranking" "part_of_a_whole" "evolution" "flow" "colors"])

(defn- linked-use-cases
  "Use case page names in the order `text` links them (`use_cases.<name>.html`), first link of each."
  [text]
  (vec (distinct (map second (re-seq #"use_cases\.([a-z_]+)\.html" text)))))

(deftest use-case-order-test
  (testing "chapters.edn lists the use cases in site order"
    (is (= use-case-order
           (->> (edn/read-string (slurp "notebooks/chapters.edn"))
                (filter #(and (map? %) (= "Use cases" (:part %))))
                first :chapters (mapv nf/page-name)))))
  (testing "the index names them in site order"
    (let [text (slurp "notebooks/book/index.clj")
          from (str/index-of text "**Use cases**")
          words ["distribution" "correlation" "ranking" "part of a whole" "evolution" "flow" "colours"]
          positions (when from (map #(str/index-of text % from) words))]
      (is (some? from) "the index has a **Use cases** item")
      (is (and positions (every? some? positions) (apply < positions)) (str "positions " (vec positions)))))
  (testing "specific cases and the README link them in site order"
    (is (= use-case-order (linked-use-cases (slurp "notebooks/reference/specific_cases.clj"))))
    (is (= use-case-order (linked-use-cases (slurp "README.md"))))))
