(ns cljplot2.book-data-test
  "Data shown in every book chapter (reference, use cases, galleries): every dataset or data a page defines is shown
  right after its definition (`nu/data-table`), and every dataset a page reads is shown by some data table - of the
  dataset itself or of a var defined from it. Pages are loaded (their data vars evaluated)."
  (:require [clojure.test :refer [deftest testing is]]
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
      (is (= [] (nf/datasets-unshown [(table (list 'remove 'p (ds :a)))]))))
    (testing "boundary: read inline only; a var from it never tabled; two datasets, one shown; none read"
      (is (= [:a] (nf/datasets-unshown [(list 'f {:data (ds :a)})])))
      (is (= [:a] (nf/datasets-unshown [(list 'def 'x (ds :a)) (list 'f 'x)])))
      (is (= [:b] (nf/datasets-unshown [(table (ds :a)) (list 'f (ds :b))])))
      (is (= [] (nf/datasets-unshown ['(f x)]))))))
