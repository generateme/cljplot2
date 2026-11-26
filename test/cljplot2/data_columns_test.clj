(ns cljplot2.data-columns-test
  "Every user-facing data function taking a `rows` argument accepts columns too - a map of equal-length sequences,
  as a plot's `:data` (`cljplot2.data.rows/rows`) - and gives the same result as for the rows they hold."
  (:require [clojure.test :refer [deftest testing is]]
            [cljplot2.describe :as describe]
            [cljplot2.data.graph :as graph]
            [cljplot2.data.hierarchy :as hierarchy]
            [cljplot2.data.levels :as levels]
            [cljplot2.data.pairwise :as pairwise]
            [cljplot2.data.series :as series]
            [cljplot2.data.sets :as sets]
            [cljplot2.data.shingle :as shingle]
            [cljplot2.data.tree :as tree]
            [cljplot2.test-util :refer [error-data]]))

(def ^:private fixtures
  "Data function (qualified symbol) -> `[f rows args]`: the function, rows of one key set, and the arguments after
  `rows`."
  {'cljplot2.data.graph/alluvial-graph [graph/alluvial-graph [{:a "x" :b "p"} {:a "y" :b "q"} {:a "x" :b "q"}] [{:variables [:a :b]}]]
   'cljplot2.data.hierarchy/sequence-levels [hierarchy/sequence-levels [{:sequence "a-b-end"} {:sequence "a-c"} {:sequence "a-b-end"}] []]
   'cljplot2.data.levels/bin [levels/bin [{:v 1.0} {:v 2.0} {:v 3.0} {:v 4.0}] [:v {:as :bin :bins 2}]]
   'cljplot2.data.levels/order-levels [levels/order-levels [{:g "a" :v 3} {:g "b" :v 1} {:g "a" :v 1}] [:g :v :sum]]
   'cljplot2.data.levels/rank-rows [levels/rank-rows [{:g "a" :v 3} {:g "b" :v 1} {:g "c" :v 2}] [:g :v]]
   'cljplot2.data.pairwise/matrix-rows [pairwise/matrix-rows [{:a 1.0 :b 2.0} {:a 2.0 :b 1.0} {:a 3.0 :b 5.0} {:a 4.0 :b 4.0}] [{:variables [:a :b]}]]
   'cljplot2.data.series/horizon-rows [series/horizon-rows [{:x 1 :y 1.0} {:x 2 :y -2.0} {:x 3 :y 3.0}] []]
   'cljplot2.data.sets/membership-rows [sets/membership-rows [{:set "A" :item 1} {:set "B" :item 1} {:set "A" :item 2}] [{:set :set :item :item}]]
   'cljplot2.data.sets/upset-rows [sets/upset-rows [{:set "A" :item 1} {:set "B" :item 1} {:set "A" :item 2}] [{:set :set :item :item}]]
   'cljplot2.data.shingle/shingle [shingle/shingle [{:v 1.0} {:v 2.0} {:v 3.0} {:v 4.0} {:v 5.0}] [:v {:n 2 :as :interval}]]
   'cljplot2.data.tree/from-levels [tree/from-levels [{:a "x" :b "p"} {:a "x" :b "q"} {:a "y" :b "p"}] [{:hierarchy [:a :b]}]]
   'cljplot2.data.tree/from-parents [tree/from-parents [{:id :r :parent nil} {:id :a :parent :r} {:id :b :parent :r}] []]})

(defn- columns
  "The columns of `rows` (maps of one key set): key -> the vector of its values in row order."
  [rows]
  (into {} (map (fn [k] [k (mapv k rows)])) (keys (first rows))))

(defn- outcome
  "The result of calling `f` with `data` and `args`, or `{:error key}` for the `ex-info` it throws."
  [f data args]
  (try (apply f data args)
       (catch clojure.lang.ExceptionInfo e {:error (:key (ex-data e))})))

(deftest columns-test
  (testing "every data function with a rows argument has a fixture"
    (is (= (set (keys fixtures))
           (set (keep #(when (some (fn [arg] (= 'rows (:name arg))) (:cljplot2/arguments (meta %))) (symbol %))
                      (describe/api-vars))))))
  (doseq [[sym [f rows args]] fixtures]
    (testing (str sym ": columns give the result of their rows; the rows give a non-empty result")
      (is (seq (outcome f rows args)))
      (is (not (:error (outcome f rows args))))
      (is (= (outcome f rows args) (outcome f (columns rows) args))))
    (testing (str sym ": boundary - no columns as no rows; columns of one row as that row")
      (is (= (outcome f [] args) (outcome f {} args)))
      (is (= (outcome f [(first rows)] args) (outcome f (columns [(first rows)]) args))))
    (testing (str sym ": boundary - columns of different lengths, non-sequential columns and non-map rows fail on the data")
      (is (= :data (:key (error-data #(apply f (assoc (columns rows) ::longer (vec (repeat (inc (count rows)) 0))) args)))))
      (is (= :data (:key (error-data #(apply f (update-vals (columns rows) first) args)))))
      (is (= :data (:key (error-data #(apply f [1 2] args))))))))

(deftest upset-columns-test
  (testing "the reported case: upset-rows over columns counts the combinations (it returned [] before)"
    (is (= [{:combination "A" :sets ["A"] :size 1 :degree 1} {:combination "A & B" :sets ["A" "B"] :size 1 :degree 2}]
           (sets/upset-rows {:set ["A" "B" "A"] :item [1 1 2]} {:set :set :item :item})))))
