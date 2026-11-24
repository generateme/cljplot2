(ns cljplot2.data.pairwise
  "Pairwise statistics of numeric variables: a statistic of two samples - an error, an association, an effect size, a distance or a user function - for every pair of variables.

  A paired statistic takes the rows where both of its values are finite numbers (pairwise-complete rows); an independent-sample statistic (two groups compared, e.g. an effect size) takes each variable's own finite values. Fewer than 2 values in a sample, a non-finite result or a statistic that throws give `##NaN`. Statistics are computed by fastmath; [[statistics]] lists the built-in ones with their labels, families, how they behave when the samples swap (`:symmetry`) and whether zero is their neutral value (`:signed`).

  Functions: [[statistics]] - the built-in statistics; [[pair-samples]], [[statistic-samples]] - the samples of two accessors; [[statistic-label]], [[statistic-symmetry]], [[signed-statistic?]], [[statistic-value]] - one statistic; [[cell-values]] - the values of chosen cells; [[matrix-rows]] - the rows of a statistic matrix (a standalone matrix is `chart/tile` over them; the `pairwise` chart draws one pair per panel)."
  (:require [fastmath.core :as m]
            [fastmath.distance :as dist]
            [fastmath.stats :as stats]
            [cljplot2.data.rows :as data]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def statistics
  "The built-in pairwise statistics: keyword -> `{:fn :label :family :symmetry :samples :signed :description}`.

  - `:fn` - the fastmath function of two samples `(f xs ys)`.
  - `:label` - its short name (colour bar and text labels).
  - `:family` - `:error`, `:association`, `:effect-size` or `:distance`.
  - `:symmetry` - how the value changes when the samples swap: `:symmetric` (unchanged), `:antisymmetric` (negated), `:asymmetric` (otherwise), `:reciprocal` (inverted), `:complementary` (`1 - v`).
  - `:samples` - `:paired` (row i of xs goes with row i of ys; pairwise-complete rows) or `:independent` (two groups compared; each variable's own finite values).
  - `:signed` - zero is the neutral value (no error, no association, no effect): a colour scale centres on it.
  - `:description` - what it measures.

  Errors (paired, xs - the observed values, ys - the predicted): `:me`, `:mae`, `:mape`, `:mse`, `:rmse`, `:r2`; association (paired): `:covariance`, `:pearson`, `:spearman`, `:kendall`; effect sizes (independent groups): `:cohens-d`, `:hedges-g`, `:glass-delta`, `:cliffs-delta`, `:ameasure`, `:means-ratio`; distances (paired): `:euclidean`, `:manhattan`, `:chebyshev`, `:canberra`, `:cosine` (a similarity)."
  {:me {:fn stats/me :label "ME" :family :error :symmetry :antisymmetric :samples :paired :signed true :description "mean error, mean of xs - ys"}
   :mae {:fn stats/mae :label "MAE" :family :error :symmetry :symmetric :samples :paired :description "mean absolute error"}
   :mape {:fn stats/mape :label "MAPE" :family :error :symmetry :asymmetric :samples :paired :description "mean absolute percentage error, relative to xs"}
   :mse {:fn stats/mse :label "MSE" :family :error :symmetry :symmetric :samples :paired :description "mean squared error"}
   :rmse {:fn stats/rmse :label "RMSE" :family :error :symmetry :symmetric :samples :paired :description "root mean squared error"}
   :r2 {:fn stats/r2 :label "R²" :family :error :symmetry :asymmetric :samples :paired :description "coefficient of determination of ys predicting xs"}
   :covariance {:fn stats/covariance :label "Cov" :family :association :symmetry :symmetric :samples :paired :signed true :description "sample covariance"}
   :pearson {:fn stats/pearson-correlation :label "Pearson" :family :association :symmetry :symmetric :samples :paired :signed true :description "Pearson correlation"}
   :spearman {:fn stats/spearman-correlation :label "Spearman" :family :association :symmetry :symmetric :samples :paired :signed true :description "Spearman rank correlation"}
   :kendall {:fn stats/kendall-correlation :label "Kendall" :family :association :symmetry :symmetric :samples :paired :signed true :description "Kendall rank correlation (tau)"}
   :cohens-d {:fn stats/cohens-d :label "Cohen's d" :family :effect-size :symmetry :antisymmetric :samples :independent :signed true :description "standardized mean difference, pooled standard deviation"}
   :hedges-g {:fn stats/hedges-g-corrected :label "Hedges' g" :family :effect-size :symmetry :antisymmetric :samples :independent :signed true :description "Cohen's d corrected for small-sample bias"}
   :glass-delta {:fn stats/glass-delta :label "Glass's Δ" :family :effect-size :symmetry :asymmetric :samples :independent :signed true :description "mean difference over the standard deviation of ys"}
   :cliffs-delta {:fn stats/cliffs-delta :label "Cliff's δ" :family :effect-size :symmetry :antisymmetric :samples :independent :signed true :description "P(x > y) - P(x < y), non-parametric"}
   :ameasure {:fn stats/ameasure :label "A" :family :effect-size :symmetry :complementary :samples :independent :description "Vargha-Delaney A, P(x > y) (ties count half)"}
   :means-ratio {:fn stats/means-ratio :label "Means ratio" :family :effect-size :symmetry :reciprocal :samples :independent :description "mean of xs over mean of ys"}
   :euclidean {:fn dist/euclidean :label "Euclidean" :family :distance :symmetry :symmetric :samples :paired :description "Euclidean distance"}
   :manhattan {:fn dist/manhattan :label "Manhattan" :family :distance :symmetry :symmetric :samples :paired :description "Manhattan distance"}
   :chebyshev {:fn dist/chebyshev :label "Chebyshev" :family :distance :symmetry :symmetric :samples :paired :description "Chebyshev distance"}
   :canberra {:fn dist/canberra :label "Canberra" :family :distance :symmetry :symmetric :samples :paired :description "Canberra distance"}
   :cosine {:fn dist/cosine :label "Cosine" :family :distance :symmetry :symmetric :samples :paired :signed true :description "cosine similarity"}})

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

(defn- finite-values
  "Gives the finite numbers an accessor reads from rows, as doubles.

  Parameters:

  - `rows` (sequence): the rows.
  - `value-of` (function): the row accessor.

  Returns a vector of doubles, empty when no value is finite."
  [rows value-of]
  (into [] (comp (map value-of) (filter schema/finite?) (map double)) rows))

(defn statistic-samples
  "Gives the samples a statistic compares, by its `:samples` (see [[statistics]]).

  Parameters:

  - `method`: a key of [[statistics]] or a function (paired).
  - `rows` (sequence): the rows.
  - `x-of`, `y-of` (functions): the row accessors of the two variables.

  Returns `[xs ys]`, vectors of doubles: the pairwise-complete rows of a paired statistic (see [[pair-samples]]), each variable's own finite values for an independent-sample one (lengths may differ)."
  [method rows x-of y-of]
  (if (= :independent (get-in statistics [method :samples]))
    [(finite-values rows x-of) (finite-values rows y-of)]
    (pair-samples rows x-of y-of)))

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

(defn signed-statistic?
  "Tests whether zero is a statistic's neutral value (see [[statistics]] `:signed`).

  Parameters:

  - `method`: a key of [[statistics]] or a function.

  Returns `true` for a signed built-in statistic, else `nil` (a function: nothing is known of it)."
  [method]
  (get-in statistics [method :signed]))

(defn statistic-value
  "Computes a statistic of two samples.

  Parameters:

  - `method`: a key of [[statistics]] or a function `(f xs ys)` giving a number.
  - `xs`, `ys` (vectors of doubles): the samples (see [[statistic-samples]]).

  Returns the value as a double; `##NaN` for fewer than 2 values in a sample, a non-number or non-finite result, or a statistic that throws (any `Throwable`).

  Throws `ex-info {:key :method}` for a keyword that isn't a key of [[statistics]]."
  ^double [method xs ys]
  (let [f (if (fn? method)
            method
            (or (get-in statistics [method :fn])
                (schema/fail! (str "Unknown pairwise statistic " method ".") {:key :method :value method})))]
    (if (or (m/< (count xs) 2) (m/< (count ys) 2))
      ##NaN
      (let [v (try (f xs ys) (catch Throwable _ nil))]
        (if (schema/finite? v) (double v) ##NaN)))))

(defn cell-values
  "Computes a statistic for chosen pairs of variables.

  Parameters:

  - `rows` (sequence of maps): the data rows.
  - `method`: a key of [[statistics]] or a function of two samples.
  - `cells` (sequence): `[x y]` variable pairs (keywords or accessors).

  Returns `{[x y] {:value v :n n}}`: `v` - `(f xs ys)` over the cell's samples (see [[statistic-samples]], [[statistic-value]]); `n` - the sample size (paired: the complete rows; independent: the smaller sample). A `:symmetric` statistic is computed once per unordered pair, an `:antisymmetric` one negated and a `:reciprocal` one inverted from its mirror."
  [rows method cells]
  (let [symmetry (statistic-symmetry method)
        compute (fn [[x y]] (let [[xs ys] (statistic-samples method rows (data/row-accessor x x) (data/row-accessor y y))]
                              {:value (statistic-value method xs ys) :n (m/min (count xs) (count ys))}))
        mirrored (fn [{:keys [^double value] :as cell}]
                   (case symmetry
                     :symmetric cell
                     :antisymmetric (assoc cell :value (m/- value))
                     :reciprocal (assoc cell :value (let [r (m// 1.0 value)] (if (m/valid-double? r) r ##NaN)))
                     nil))]
    (reduce (fn [acc [x y :as cell]]
              (cond
                (contains? acc cell) acc
                :else (let [mirror (get acc [y x])
                            derived (when mirror (mirrored mirror))]
                        (assoc acc cell (or derived (compute cell))))))
            {} cells)))

(def matrix-rows-options
  "Options schema of [[matrix-rows]]."
  [:map {:closed true}
   (schema/optional :variables {:description "the variables of a square matrix (x and y both, distinct), in matrix order"}
                    [:and [:sequential {:min 2} keyword?] [:fn {:error/message "should be distinct variables"} #(apply distinct? %)]])
   (schema/optional :x-variables {:description "the x variables of a rectangular matrix (with :y-variables)"}
                    [:sequential {:min 1} keyword?])
   (schema/optional :y-variables {:description "the y variables of a rectangular matrix (with :x-variables)"}
                    [:sequential {:min 1} keyword?])
   (schema/optional :method {:description "the statistic: a key of pairwise/statistics or a function of two samples" :default :pearson}
                    method-schema)
   (schema/nilable :label {:description "the statistic's label in the rows"
                           :cljplot2/default-doc "the statistic's label, value for a function"}
                   string?)
   (schema/optional :self-pairs {:description "rows of a variable with itself (square matrix)" :default false} schema/bool)
   (schema/optional :triangle {:description "the cells of a square matrix: :full, :lower (y after x in variable order) or :upper; :lower and :upper need a symmetric statistic"
                               :default :full}
                    [:enum :full :lower :upper])])

(defn- matrix-cells
  "Gives the `[x y]` variable pairs of a matrix in row order (y outer, x inner).

  Parameters:

  - `options` (map): the checked options of [[matrix-rows]].

  Returns a sequence of `[x y]`: a square matrix keeps the cells of `:triangle` (lower - y after x in variable order) and the diagonal with `:self-pairs`; a rectangular one every pair."
  [{:keys [variables x-variables y-variables triangle] self-pairs :self-pairs}]
  (if variables
    (let [indexed (map-indexed vector variables)]
      (for [[^long i y] indexed
            [^long j x] indexed
            :when (case (or triangle :full)
                    :full (or self-pairs (m/not== i j))
                    :lower (or (m/> i j) (and self-pairs (m/== i j)))
                    :upper (or (m/< i j) (and self-pairs (m/== i j))))]
        [x y]))
    (for [y y-variables x x-variables] [x y])))

(defn- check-numeric-variables!
  "Checks that every variable has finite numbers in the rows.

  Parameters:

  - `rows` (sequence of maps): the data rows.
  - `variables` (sequence of keywords): the variables.

  Returns `nil`. Throws `ex-info {:key :variables}` for a variable without finite numbers or with a value that is neither a number nor `nil`."
  [rows variables]
  (doseq [k variables
          :let [vs (keep (data/row-accessor k k) rows)]]
    (when (or (not-any? schema/finite? vs) (some (complement number?) vs))
      (schema/fail! (str "Pairwise variable " k " needs numeric values.") {:key :variables :value k}))))

(defn matrix-rows
  "Computes a statistic for every pair of variables, as rows of a matrix.

  Parameters:

  - `rows` (rows or columns): the data.
  - `options` (map, see [[matrix-rows-options]]):
    - `:variables` - a square matrix of these distinct variables (at least 2), or `:x-variables` with `:y-variables` - a rectangular one (every pair, a variable in both lists too);
    - `:method` - the statistic, a key of [[statistics]] or a function of two samples (default `:pearson`);
    - `:label` - the statistic's label (default its `:label`, `value` for a function);
    - `:self-pairs` - include a variable with itself in a square matrix (default `false`);
    - `:triangle` - `:full` (default), `:lower` or `:upper` cells of a square matrix.

  Returns a vector of rows `{:x x :y y :value v :n n :statistic label}` in row order (y outer, x inner, in variable order): `v` - the statistic of the x sample against the y sample `(f xs ys)` (see [[cell-values]]), `nil` when it can't be computed (a tile paints it in the NA colour); `n` - the sample size. For a matrix like `pairs` with `chart/tile`, give the axes the variable order (x domain the variables, y domain the variables reversed - the first variable on top): tile orders categories by first appearance with y growing upward.

  Throws `ex-info {:key :variables}` without variables, with both forms, with repeated `:variables` or a variable without numeric values; `{:key :triangle}` for `:lower` / `:upper` of a rectangular matrix or of a statistic that isn't `:symmetric` (a function counts as asymmetric); `{:key k}` for an invalid option `k` (checked first).

  See also [[statistic-samples]], [[statistic-value]]."
  {:cljplot2/api true :cljplot2/options matrix-rows-options}
  [rows {:keys [variables x-variables y-variables label triangle] :as options}]
  (schema/check-options! matrix-rows-options options)
  (let [method (data/option options :method :pearson)
        symmetric? (= :symmetric (statistic-symmetry method))
        statistic (statistic-label method label)]
    (when-not (or variables (and x-variables y-variables))
      (schema/fail! "Pairwise rows need :variables, or :x-variables with :y-variables." {:key :variables}))
    (when (and variables (or x-variables y-variables))
      (schema/fail! "Pairwise rows take :variables or :x-variables with :y-variables, not both." {:key :variables}))
    (when (and (#{:lower :upper} triangle) (or (not variables) (not symmetric?)))
      (schema/fail! "A :lower or :upper triangle needs a square matrix of a symmetric statistic." {:key :triangle :value triangle}))
    (let [data-rows (data/rows rows)
          _ (check-numeric-variables! data-rows (distinct (or variables (concat x-variables y-variables))))
          cells (matrix-cells options)
          values (cell-values data-rows method cells)]
      (mapv (fn [cell]
              (let [{:keys [^double value n]} (values cell)]
                {:x (first cell) :y (second cell) :value (when-not (m/nan? value) value) :n n :statistic statistic}))
            cells))))
