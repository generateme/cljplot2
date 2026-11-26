(ns cljplot2.reference-r2-test
  "Reference part R2: every builder and user-facing (`:cljplot2/api`) function documents and validates its options.
  Builders and API functions check their options first (before reading their data), so a bad option fails with any
  data - the tests below pass `nil` data."
  (:require [clojure.test :refer [deftest testing is]]
            [malli.core :as malli]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [cljplot2.schema :as schema]
            [cljplot2.test-util :refer [card-text? error-data schema-entries]]))

(def builders
  "The builders of `cljplot2.core`."
  [#'plot/arrange #'plot/pairs #'plot/network #'plot/dendrogram #'plot/tree #'plot/clustered-heatmap #'plot/bundle
   #'plot/chord #'plot/arc #'plot/sankey #'plot/alluvial #'plot/sequences #'plot/acf #'plot/horizon])

(def api-names
  "The user-facing data functions (spec 4.3, 4.6)."
  '#{cljplot2.data.levels/bin cljplot2.data.levels/categorical cljplot2.data.levels/order-levels
     cljplot2.data.levels/rank-rows cljplot2.data.shingle/shingle
     cljplot2.data.tree/from-hclust cljplot2.data.tree/from-levels cljplot2.data.tree/from-parents cljplot2.data.tree/leaves
     cljplot2.data.tree/layout cljplot2.data.tree/node-link-layout cljplot2.data.tree/bundle-paths
     cljplot2.data.sets/membership-rows cljplot2.data.sets/upset-rows
     cljplot2.data.series/lag-rows cljplot2.data.series/acf-rows cljplot2.data.series/horizon-rows
     cljplot2.data.series/horizon-bands
     cljplot2.data.graph/graph cljplot2.data.graph/layout cljplot2.data.graph/adjacency->edges
     cljplot2.data.graph/arc-layout cljplot2.data.graph/chord-layout cljplot2.data.graph/sankey-layout
     cljplot2.data.graph/alluvial-graph cljplot2.data.hierarchy/sequence-levels cljplot2.data.field/domain-color
     cljplot2.data.contour/isolines cljplot2.data.vector-field/arrows cljplot2.data.vector-field/mapped-points
     cljplot2.data.vector-field/traces cljplot2.graphics.font/resolve-font cljplot2.data.pairwise/matrix-rows})

(defn- var-name [v] (symbol (str (:ns (meta v))) (str (:name (meta v)))))

(defn- documented-problems
  "The paths of the entries of `schema` (nested ones too) breaking the documentation rules: no description, both
  `:default` and a default text, or a text that doesn't read as an option card's ([[card-text?]]; `option-cards-test`
  checks the rest). Forbidden entries are left out."
  [schema]
  (vec (for [[path props v] (schema-entries schema)
             :when (not (schema/forbidden? v))
             :when (or (nil? (:description props))
                       (and (contains? props :default) (contains? props :cljplot2/default-doc))
                       (not-every? card-text? (keep props [:description :cljplot2/default-doc])))]
         path)))

(deftest documented-problems-test
  (let [s [:map {:closed true}
           [:ok {:description "Card text." :cljplot2/default-doc "`0`; more."} :int]
           [:lower {:description "table text"} :int]
           [:none {} :int]
           [:both {:description "Card text." :default 1 :cljplot2/default-doc "One."} :int]
           [:bad-default {:description "Card text." :cljplot2/default-doc "no period"} :int]
           [:m {:description "Card map."} [:map [:k {:description "nested table text"} :int]]]
           [:gone {:description "not taken"} (schema/forbidden "Not taken.")]]]
    (testing "boundary: the card rule for every text, nested entries too; no description; both defaults; a forbidden entry left out"
      (is (= [[:lower] [:none] [:both] [:bad-default] [:m :k]] (documented-problems s))))
    (testing "boundary: an empty map has no problems"
      (is (= [] (documented-problems [:map]))))))



(defn- positional-args
  "`nil` for each positional argument of `v`'s longest arity taking options (the last argument)."
  [v]
  (repeat (dec (count (last (sort-by count (:arglists (meta v)))))) nil))

(defn- bad-option-error
  "Error data of `v` called with `nil` data and options holding a misspelling of its first option key."
  [v]
  (let [k (:key (first (describe/options v)))
        bad (keyword (str (name k) "x"))]
    [bad (error-data #(apply @v (concat (positional-args v) [{bad 1}])))]))

(deftest builders-test
  (testing "the builders are exactly the core vars taking options"
    (is (= (set builders) (set (filter #(:cljplot2/options (meta %)) (vals (ns-publics 'cljplot2.core)))))))
  (doseq [v builders]
    (testing (str (var-name v) ": options schema, documented in the card style (nested entries too)")
      (is (some? (:cljplot2/options (meta v))))
      (is (seq (describe/options v)))
      (is (= [] (documented-problems (:cljplot2/options (meta v))))))
    (testing (str (var-name v) ": the core var documents the implementation's schema")
      (let [n (:name (meta v))
            impl-ns (cond ('#{arrange pairs} n) "cljplot2.multiplot" ('#{acf horizon} n) "cljplot2.series-plots" :else "cljplot2.graphs")
            impl (requiring-resolve (symbol impl-ns (str n)))]
        (is (some? (:cljplot2/options (meta impl))))
        (is (= (:cljplot2/options (meta v)) (:cljplot2/options (meta impl))))))
    (testing (str (var-name v) ": a misspelled option fails first, keyed by it, with a spelling suggestion")
      (let [[bad data] (bad-option-error v)]
        (is (= bad (:key data)))
        (is (re-find #"should be spelled" (pr-str (:errors data))))))))

(defn- positional-names
  "The positional parameter names of a function with var metadata `m`, in its longest arity: a symbol, or a
  destructured map's `:as`; the options map - the last parameter of a function with `:cljplot2/options` - left out."
  [m]
  (let [arglist (last (sort-by count (:arglists m)))
        names (mapv #(if (map? %) (:as %) %) arglist)]
    (cond-> names (:cljplot2/options m) pop)))

(defn- optional-names
  "The positional parameter names (see [[positional-names]]) a call may leave out: those past the shortest arity's."
  [m]
  (set (drop (count (first (sort-by count (:arglists m)))) (positional-names m))))

(deftest positional-names-test
  (testing "boundary: symbols, a destructured map's :as, the options map left out, optional trailing parameters"
    (let [m {:arglists '([g] [{:keys [nodes] :as g} {:keys [order] :as options}]) :cljplot2/options {}}]
      (is (= '[g] (positional-names m)))
      (is (= #{} (optional-names m))))
    (let [m {:arglists '([rows mapping] [rows mapping sets])}]
      (is (= '[rows mapping sets] (positional-names m)))
      (is (= '#{sets} (optional-names m))))
    (is (= '[rows value] (positional-names {:arglists '([rows value {:keys [as] :as options}]) :cljplot2/options {}})))
    (is (= #{} (optional-names {:arglists '([rows value {:keys [as] :as options}]) :cljplot2/options {}}))
        "a required options map: every positional parameter is required"))
  (testing "boundary: the card text rule"
    (is (every? card-text? ["A graph." "`g` first." "One. Two." "One, e.g. this. Two." "Labels `[1 .. n]`. Two."]))
    (is (not-any? card-text? ["a graph." "A graph" nil 3 "One. Two. Three." (str "A" (apply str (repeat 299 "x")) ".")]))))

(deftest arguments-test
  (doseq [v (concat builders (describe/api-vars))
          :let [m (meta v)
                {:cljplot2/keys [arguments returns]} m]]
    (testing (str (var-name v) ": every positional parameter documented in order, optional ones marked; card texts; a return value")
      (is (= (positional-names m) (mapv :name arguments)))
      (is (= (optional-names m) (set (map :name (filter :optional arguments)))))
      (is (= [] (vec (for [{arg :name :keys [description columns] map-keys :keys} arguments
                           t (concat [description] (vals columns) (vals map-keys))
                           :when (not (card-text? t))]
                       [arg t]))))
      (is (card-text? returns) (str returns)))))

(deftest api-vars-test
  (testing "exactly the user-facing functions are marked"
    (is (= api-names (set (map var-name (describe/api-vars))))))
  (doseq [v (describe/api-vars)
          :when (:cljplot2/options (meta v))]
    (testing (str (var-name v) ": options documented")
      (is (= [] (documented-problems (:cljplot2/options (meta v))))))
    (testing (str (var-name v) ": a misspelled option fails first, keyed by it")
      (let [[bad data] (bad-option-error v)]
        (is (= bad (:key data)))))))

(deftest fragments-test
  (testing "every group a builder or API function uses is registered"
    (doseq [v (concat builders (describe/api-vars))
            {:keys [fragment path]} (describe/options v)
            :when fragment]
      (is (contains? describe/fragments fragment) [(var-name v) path fragment])))
  (testing "sankey: its own rows, then the rows it forwards to graph/sankey-layout"
    (is (some #(= :graph/sankey-layout (:fragment %)) (describe/options #'plot/sankey)))
    (is (some #(nil? (:fragment %)) (describe/options #'plot/sankey))))
  (testing "arrange: the plot :arrange group"
    (is (some #(= :arrange (:fragment %)) (describe/options #'plot/arrange)))))

(deftest check-options-order-test
  (let [s [:map {:closed true} [:value {:description "v"} keyword?] (schema/optional :n {:description "n"} :int)]]
    (testing "a misspelled required option: the unknown key first, with its suggestion, then the missing one"
      (let [data (error-data #(schema/check-options! s {:valeu :b}))]
        (is (= :valeu (:key data)))
        (is (re-find #"should be spelled :value" (:message (first (:errors data)))))
        (is (= [[:valeu] [:value]] (map :in (:errors data))))))
    (testing "boundary: only a missing option; nil options; valid options"
      (is (= :value (:key (error-data #(schema/check-options! s {})))))
      (is (= :value (:key (error-data #(schema/check-options! s nil)))))
      (is (nil? (schema/check-options! s {:value :b :n 1}))))
    (testing "other errors keep their order"
      (is (= [[:n] [:x]] (map :in (:errors (error-data #(schema/check-options! s {:value :b :n "1" :x 1})))))))))

(deftest required-options-test
  (testing "required options are required in the schemas: missing, and alone keyed by them"
    (is (= :row (:key (error-data #(plot/clustered-heatmap nil {:col :c :value :v})))))
    (is (= :variables (:key (error-data #(plot/alluvial nil {})))))
    (is (= :variables (:key (error-data #(plot/pairs nil {}))))))
  (testing "boundary: pairs takes 2 to 20 variables, alluvial at least 2 axes"
    (is (= :variables (:key (error-data #(plot/pairs nil {:variables [:a]})))))
    (is (= :variables (:key (error-data #(plot/pairs nil {:variables (vec (map (comp keyword str) (range 21)))})))))
    (is (nil? (:key (error-data #(plot/pairs [{:a 1 :b 2} {:a 2 :b 1}] {:variables [:a :b]})))))
    (is (= :variables (:key (error-data #(plot/alluvial nil {:variables [:a]})))))))

(deftest shared-types-test
  (testing "open-fraction: (0 1), ends excluded"
    (is (= [false true false false] (map #(malli/validate schema/open-fraction %) [0 0.5 1 "0.5"]))))
  (testing "finite-pos: > 0 and finite"
    (is (= [false true false false] (map #(malli/validate schema/finite-pos %) [0 1e-9 ##Inf ##NaN]))))
  (testing "finite-non-neg: >= 0 and finite"
    (is (= [true true false false] (map #(malli/validate schema/finite-non-neg %) [0 2.5 -0.1 ##Inf]))))
  (testing "finite-interval: a vector [lo hi] of finite numbers, lo < hi"
    (is (= [true false false false false false false]
           (map #(malli/validate schema/finite-interval %) [[0 1] [1 1] [1 0] [0 ##Inf] (list 0 1) [0] [0 1 2]])))))

(deftest options-map-test
  (let [own [(schema/optional :a {:description "own a"} :int)]
        forwarded [(schema/optional :a {:description "forwarded a"} :string) (schema/optional :b {:description "b"} :int)]]
    (testing "groups in order, an earlier entry wins over a later one with its key"
      (is (= [[:a "own a"] [:b "b"]] (map (juxt :key :description) (describe/schema-rows (schema/options-map own forwarded))))))
    (testing "boundary: no groups, empty groups - an empty closed map"
      (is (= [:map {:closed true}] (schema/options-map)))
      (is (= [:map {:closed true}] (schema/options-map [] []))))
    (testing "acf: its own :correlograms wins over acf-rows', no duplicate key"
      (let [rows (filter #(= [:correlograms] (:path %)) (describe/options #'plot/acf))]
        (is (= 1 (count rows)))
        (is (nil? (:fragment (first rows))))))))

(deftest style-defaults-test
  (testing "style options merged over a default name its registered theme style as the default text"
    (doseq [[v k] [[#'plot/network :node-style] [#'plot/network :edge-style] [#'plot/dendrogram :line-style]
                   [#'plot/tree :node-style] [#'plot/tree :link-style] [#'plot/arc :node-style] [#'plot/arc :edge-style]]]
      (let [row (first (filter #(= [k] (:path %)) (describe/options v)))
            [_ style-name] (re-find #"^The theme style `\[:chart :([a-z-]+)\]`\.$" (str (:default-doc row)))]
        (is (contains? cljplot2.theme/named-styles (keyword style-name)) [(var-name v) k])
        (is (not (re-find #"\{:" (:description row))) [(var-name v) k])))))

(def ^:private intended-overrides
  "Builder -> keys whose own entry shadows a forwarded group's (schema/options-map: own entries win), each on purpose:
  pairs' `:theme` styles the cells and the matrix (not only the built plot), its `:scales` is the cells' colour scale only
  (a matrix has no plot scales); sequences' `:keep-end` defaults to true
  (a sunburst keeps the end marker)."
  '{pairs #{:theme :scales} sequences #{:keep-end}})

(defn- shadowed-keys
  "Own (untagged) top-level option keys of builder `v` that a group it also takes defines."
  [v]
  (let [rows (filter #(= 1 (count (:path %))) (describe/options v))
        own (set (map :key (remove :fragment rows)))]
    (set (for [g (set (keep :fragment rows))
               r (describe/fragment-rows g)
               :when (and (= 1 (count (:path r))) (own (:key r)))]
           (:key r)))))

(deftest overrides-test
  (testing "a builder's own entry shadows a forwarded one only where intended (a new clash fails here)"
    (is (= intended-overrides
           (into {} (keep (fn [v] (let [ks (shadowed-keys v)] (when (seq ks) [(:name (meta v)) ks]))) builders)))))
  (testing "boundary: an overridden key has one row - the builder's own"
    (doseq [[n ks] intended-overrides
            :let [v (ns-resolve 'cljplot2.core n)]
            k ks]
      (is (= [nil] (map :fragment (filter #(= [k] (:path %)) (describe/options v)))) [n k]))))
