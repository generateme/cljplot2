(ns cljplot2.data.pairwise
  "Pairwise statistics of numeric variables: a statistic of two samples - an error, an association, an effect size, a distance or a user function - for every pair of variables.

  A pair takes the rows where both of its values are finite numbers (pairwise-complete rows); fewer than 2 such rows, a non-finite result or a statistic that throws give `##NaN`. Statistics are computed by fastmath; [[statistics]] lists the built-in ones with their labels and how they behave when the samples swap (`:symmetry`).

  Functions: [[statistics]] - the built-in statistics; [[pair-samples]] - the paired samples of two accessors; [[statistic-label]], [[statistic-symmetry]], [[statistic-value]] - one statistic; [[matrix-rows]] - the rows of a statistic matrix (a standalone matrix is `chart/tile` over them; the `pairwise` chart draws one pair per panel)."
  (:require [fastmath.core :as m]
            [fastmath.distance :as dist]
            [fastmath.stats :as stats]
            [cljplot2.data.rows :as data]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def statistics
  "The built-in pairwise statistics: keyword -> `{:fn :label :symmetry :samples :description}`.

  - `:fn` - the fastmath function of two samples `(f xs ys)`.
  - `:label` - its short name (axis, colour bar and text labels).
  - `:symmetry` - how the value changes when the samples swap: `:symmetric` (unchanged), `:antisymmetric` (negated), `:asymmetric` (otherwise), `:reciprocal` (inverted), `:complementary` (`1 - v`, for samples without ties).
  - `:samples` - `:paired` (row i of xs goes with row i of ys) or `:independent` (two groups compared).
  - `:description` - what it measures.

  Errors (paired, xs - the observed values, ys - the predicted): `:me`, `:mae`, `:mape`, `:mse`, `:rmse`, `:r2`; association (paired): `:covariance`, `:pearson`, `:spearman`, `:kendall`; effect sizes (independent groups): `:cohens-d`, `:hedges-g`, `:glass-delta`, `:cliffs-delta`, `:ameasure`, `:means-ratio`; distances (paired): `:euclidean`, `:manhattan`, `:chebyshev`, `:canberra`, `:cosine` (a similarity)."
  {:me {:fn stats/me :label "ME" :symmetry :antisymmetric :samples :paired :description "mean error, mean of xs - ys"}
   :mae {:fn stats/mae :label "MAE" :symmetry :symmetric :samples :paired :description "mean absolute error"}
   :mape {:fn stats/mape :label "MAPE" :symmetry :asymmetric :samples :paired :description "mean absolute percentage error, relative to xs"}
   :mse {:fn stats/mse :label "MSE" :symmetry :symmetric :samples :paired :description "mean squared error"}
   :rmse {:fn stats/rmse :label "RMSE" :symmetry :symmetric :samples :paired :description "root mean squared error"}
   :r2 {:fn stats/r2 :label "R²" :symmetry :asymmetric :samples :paired :description "coefficient of determination of ys predicting xs"}
   :covariance {:fn stats/covariance :label "Cov" :symmetry :symmetric :samples :paired :description "sample covariance"}
   :pearson {:fn stats/pearson-correlation :label "Pearson" :symmetry :symmetric :samples :paired :description "Pearson correlation"}
   :spearman {:fn stats/spearman-correlation :label "Spearman" :symmetry :symmetric :samples :paired :description "Spearman rank correlation"}
   :kendall {:fn stats/kendall-correlation :label "Kendall" :symmetry :symmetric :samples :paired :description "Kendall rank correlation (tau)"}
   :cohens-d {:fn stats/cohens-d :label "Cohen's d" :symmetry :antisymmetric :samples :independent :description "standardized mean difference, pooled standard deviation"}
   :hedges-g {:fn stats/hedges-g :label "Hedges' g" :symmetry :antisymmetric :samples :independent :description "standardized mean difference, unbiased pooled standard deviation"}
   :glass-delta {:fn stats/glass-delta :label "Glass's Δ" :symmetry :asymmetric :samples :independent :description "mean difference over the standard deviation of ys"}
   :cliffs-delta {:fn stats/cliffs-delta :label "Cliff's δ" :symmetry :antisymmetric :samples :independent :description "P(x > y) - P(x < y), non-parametric"}
   :ameasure {:fn stats/ameasure :label "A" :symmetry :complementary :samples :independent :description "Vargha-Delaney A, P(x > y)"}
   :means-ratio {:fn stats/means-ratio :label "Means ratio" :symmetry :reciprocal :samples :independent :description "mean of xs over mean of ys"}
   :euclidean {:fn dist/euclidean :label "Euclidean" :symmetry :symmetric :samples :paired :description "Euclidean distance"}
   :manhattan {:fn dist/manhattan :label "Manhattan" :symmetry :symmetric :samples :paired :description "Manhattan distance"}
   :chebyshev {:fn dist/chebyshev :label "Chebyshev" :symmetry :symmetric :samples :paired :description "Chebyshev distance"}
   :canberra {:fn dist/canberra :label "Canberra" :symmetry :symmetric :samples :paired :description "Canberra distance"}
   :cosine {:fn dist/cosine :label "Cosine" :symmetry :symmetric :samples :paired :description "cosine similarity"}})

(def method-schema
  "Schema of a statistic: a key of [[statistics]] or a function of two samples `(f xs ys)` giving a number."
  [:or (into [:enum] (sort (keys statistics))) schema/function])

(defn pair-samples
  "Gives the paired samples of two accessors over rows: the rows where both values are finite numbers (pairwise-complete rows).

  Parameters:

  - `rows` (sequence): the rows.
  - `x-of`, `y-of` (functions): the row accessors of the two variables.

  Returns `[xs ys]`, vectors of doubles of equal length; empty vectors when no row has both values finite."
  [rows x-of y-of]
  (let [ps (filterv (fn [[x y]] (and (schema/finite? x) (schema/finite? y))) (map (juxt x-of y-of) rows))]
    [(mapv #(double (first %)) ps) (mapv #(double (second %)) ps)]))

(defn statistic-label
  "Gives the label of a statistic.

  Parameters:

  - `method`: a key of [[statistics]] or a function.
  - `label` (string or `nil`): an explicit label, which wins.

  Returns `label`, else the statistic's `:label`; a function without a label gives `value`."
  [method label]
  (or label (get-in statistics [method :label]) "value"))

(defn statistic-symmetry
  "Gives how a statistic changes when its samples swap.

  Parameters:

  - `method`: a key of [[statistics]] or a function.

  Returns the statistic's `:symmetry` (see [[statistics]]); `:asymmetric` for a function (nothing is known of it)."
  [method]
  (get-in statistics [method :symmetry] :asymmetric))

(defn statistic-value
  "Computes a statistic of two paired samples.

  Parameters:

  - `method`: a key of [[statistics]] or a function `(f xs ys)` giving a number.
  - `xs`, `ys` (vectors of doubles): the samples, of equal length (see [[pair-samples]]).

  Returns the value as a double; `##NaN` for fewer than 2 samples, a non-number or non-finite result, or a statistic that throws.

  Throws `ex-info {:key :method}` for a keyword that isn't a key of [[statistics]]."
  ^double [method xs ys]
  (let [f (if (fn? method)
            method
            (or (get-in statistics [method :fn])
                (schema/fail! (str "Unknown pairwise statistic " method ".") {:key :method :value method})))]
    (if (m/< (count xs) 2)
      ##NaN
      (let [v (try (f xs ys) (catch Exception _ nil))]
        (if (schema/finite? v) (double v) ##NaN)))))

(def matrix-rows-options
  "Options schema of [[matrix-rows]]."
  [:map {:closed true}
   (schema/optional :variables {:description "the variables of a square matrix (x and y both), in matrix order"}
                    [:sequential {:min 2} keyword?])
   (schema/optional :x-variables {:description "the x variables of a rectangular matrix (with :y-variables)"}
                    [:sequential {:min 1} keyword?])
   (schema/optional :y-variables {:description "the y variables of a rectangular matrix (with :x-variables)"}
                    [:sequential {:min 1} keyword?])
   (schema/optional :method {:description "the statistic: a key of pairwise/statistics or a function of two samples" :default :pearson}
                    method-schema)
   (schema/nilable :label {:description "the statistic's label in the rows"
                           :cljplot2/default-doc "the statistic's label, value for a function"}
                   string?)
   (schema/optional :diagonal? {:description "rows of a variable with itself" :default false} schema/bool)
   (schema/optional :triangle {:description "the cells of a square matrix: :full, :lower (y after x in variable order) or :upper; :lower and :upper need a symmetric statistic"
                               :default :full}
                    [:enum :full :lower :upper])])

(defn- matrix-cells
  "Gives the `[x y]` variable pairs of a matrix in row order (y outer, x inner).

  Parameters:

  - `options` (map): the checked options of [[matrix-rows]].

  Returns a sequence of `[x y]`; a square matrix keeps the cells of `:triangle` (lower - y after x in variable order) and the diagonal with `:diagonal?`."
  [{:keys [variables x-variables y-variables diagonal? triangle]}]
  (if variables
    (let [indexed (map-indexed vector variables)]
      (for [[^long i y] indexed
            [^long j x] indexed
            :when (case (or triangle :full)
                    :full (or diagonal? (m/not== i j))
                    :lower (or (m/> i j) (and diagonal? (m/== i j)))
                    :upper (or (m/< i j) (and diagonal? (m/== i j))))]
        [x y]))
    (for [y y-variables x x-variables :when (or diagonal? (not= x y))] [x y])))

(defn matrix-rows
  "Computes a statistic for every pair of variables, as rows of a matrix.

  Parameters:

  - `rows` (rows or columns): the data.
  - `options` (map, see [[matrix-rows-options]]):
    - `:variables` - a square matrix of these variables (at least 2), or `:x-variables` with `:y-variables` - a rectangular one;
    - `:method` - the statistic, a key of [[statistics]] or a function of two samples (default `:pearson`);
    - `:label` - the statistic's label (default its `:label`, `value` for a function);
    - `:diagonal?` - include a variable with itself (default `false`);
    - `:triangle` - `:full` (default), `:lower` or `:upper` cells of a square matrix.

  Returns a vector of rows `{:x x :y y :value v :n n :statistic label}` in row order (y outer, x inner, in variable order): `v` - the statistic of the x sample against the y sample `(f xs ys)` over the pair's complete rows, `##NaN` when it can't be computed (see [[statistic-value]]); `n` - the count of those rows. A `:symmetric` statistic is computed once per pair of variables.

  Throws `ex-info {:key :variables}` without variables or with both forms, `{:key :triangle}` for `:lower` / `:upper` of a rectangular matrix or of a statistic that isn't `:symmetric` (a function counts as asymmetric); `{:key k}` for an invalid option `k` (checked first).

  See also [[pair-samples]], [[statistic-value]]."
  {:cljplot2/api true :cljplot2/options matrix-rows-options}
  [rows {:keys [variables x-variables y-variables label triangle] :as options}]
  (schema/check-options! matrix-rows-options options)
  (let [method (get options :method :pearson)
        symmetric? (= :symmetric (statistic-symmetry method))
        statistic (statistic-label method label)]
    (when-not (or variables (and x-variables y-variables))
      (schema/fail! "Pairwise rows need :variables, or :x-variables with :y-variables." {:key :variables}))
    (when (and variables (or x-variables y-variables))
      (schema/fail! "Pairwise rows take :variables or :x-variables with :y-variables, not both." {:key :variables}))
    (when (and (#{:lower :upper} triangle) (or (not variables) (not symmetric?)))
      (schema/fail! "A :lower or :upper triangle needs a square matrix of a symmetric statistic." {:key :triangle :value triangle}))
    (let [data-rows (data/rows rows)
          compute (fn [[x y]] (let [[xs ys] (pair-samples data-rows (data/row-accessor x x) (data/row-accessor y y))]
                                {:value (statistic-value method xs ys) :n (count xs)}))
          ;; a symmetric statistic once per unordered pair
          cell-key (fn [[x y :as cell]] (if (and symmetric? (m/pos? (long (compare (str x) (str y))))) [y x] cell))
          cells (matrix-cells options)
          values (reduce (fn [acc cell] (let [k (cell-key cell)] (if (contains? acc k) acc (assoc acc k (compute k)))))
                         {} cells)]
      (mapv (fn [[x y :as cell]] (merge {:x x :y y :statistic statistic} (values (cell-key cell)))) cells))))
