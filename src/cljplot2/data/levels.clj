(ns cljplot2.data.levels
  "Level helpers - data functions users call before plotting (charts don't call them):

  * [[order-levels]] - categorical levels ordered by a statistic of a value, for a scale `:domain`
    (`:scales {:y {:domain (order-levels barley :variety :yield stats/median)}}`)
  * [[bin]] - a continuous column cut into bins: half-open intervals (see `Interval` (see [[cljplot2.data.shingle/interval]])),
    levels for strips, colours or facets (sorted ascending wherever they are levels)
  * [[breaks]] - the bin breaks shared with the histogram's `:bins`
  * [[categorical]] - a mapping marker declaring a dimension categorical
  * [[rank-rows]] - the rank of a value among the rows at each x (bump charts)

  Helpers shared with charts and scales: [[bin-index]], [[break-labels]], [[distinct-digits]], [[order-by-score]], [[order-by-rows]].

  Option schemas: [[bin-options]], [[order-levels-options]], [[rank-rows-options]]; scale-spec rules [[order-by-rules]]; [[bins]] - the bins of a column ([[histogram-bins]] - a histogram's), of the named types [[bin-rule-schema]] (over [[bin-rules]]), [[breaks-schema]] and [[quantile-bins]]."
  (:require [fastmath.core :as m]
            [malli.core :as malli]
            [fastmath.stats :as stats]
            [fastmath.vector :as v]
            [cljplot2.data.accessor :as accessor]
            [cljplot2.data.rows :as data]
            [cljplot2.data.shingle :as shingle]
            [cljplot2.schema :as schema])
  (:import [fastmath.java Array]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; breaks

(def bin-rules
  "The fastmath bin-estimation methods (`fastmath.stats/estimate-bins`), in fastmath's order."
  [:sturges :sqrt :rice :terrell-scott :doane :scott :freedman-diaconis :cross-validation :shimazaki-shinomoto])

(def bin-rule-schema
  "Schema of a bin rule: a fastmath bin-estimation method keyword (see [[bin-rules]])."
  (schema/named-type ::bin-rule (set bin-rules) (str "should be a fastmath bin-estimation method: " (apply str (interpose " " bin-rules)))
                     {:cljplot2/type-name "bin rule"
                      :cljplot2/type-meaning "A rule that picks the number of equal-width bins from the sample: a fastmath method, e.g. `:sturges` or `:freedman-diaconis`."
                      :cljplot2/type-doc "a fastmath bin-estimation method keyword"
                      :cljplot2/value-set bin-rules
                      :cljplot2/value-meanings {:sturges "`ceil(log2(n)) + 1` bins; suits roughly normal samples but gives too few bins for large ones."
                                                :sqrt "`ceil(sqrt(n))` bins."
                                                :rice "`ceil(2 * cbrt(n))` bins."
                                                :doane "Sturges' rule corrected for skewed samples."
                                                :scott "Bins of width `3.5 * sd / cbrt(n)`, `sd` the standard deviation; suits roughly normal samples."
                                                :freedman-diaconis "Bins of width `2 * IQR / cbrt(n)`, `IQR` the interquartile range; robust to outliers."}}))

(defn- breaks-problem
  "Gives the problem of explicit breaks, if any.

  Parameters:

  - `v`: any value.

  Returns the error message for a value that isn't a vector of at least 2 increasing finite numbers; `nil` for valid breaks."
  [v]
  (cond
    (not (and (vector? v) (every? schema/finite? v))) "should be a vector of finite numbers"
    (m/< (count v) 2) "should be at least 2 breaks"
    (not (apply m/< v)) "should be increasing breaks"))

(def breaks-schema
  "Schema of explicit breaks: a vector of at least 2 increasing finite numbers."
  (schema/named-type ::breaks (comp nil? breaks-problem) breaks-problem
                     {:cljplot2/type-name "breaks" :cljplot2/type-plural "breaks"
                      :cljplot2/type-meaning "The bin edges: a vector of at least 2 increasing finite numbers, e.g. `[0 10 20 50]`. Values outside them fall in no bin."
                      :cljplot2/type-doc "a vector of at least 2 increasing finite numbers"}))

(def ^:private estimation-strategies
  "Quantile estimation strategies of fastmath."
  (set (keys stats/estimation-strategies-list)))

(def ^:private probabilities-schema
  "Schema of the inner probabilities of quantile bins: increasing, strictly between 0 and 1."
  (schema/with-rules [:vector {:min 1} schema/open-fraction]
    [#(when-not (apply < %) "probabilities should be increasing")]))

(def ^:private quantiles-schema
  "Schema of quantile bins `{:quantiles q :estimation e}`."
  [:map {:closed true}
   [:quantiles [:multi {:dispatch vector?} [true probabilities-schema] [false schema/pos-int]]]
   (schema/optional :estimation (into [:enum] (sort estimation-strategies)))])

(def quantile-bins
  "Schema of quantile bins `{:quantiles q :estimation e}`, reporting one error at the `:bins` key (not inside the map)."
  (let [message (str "should be quantile bins {:quantiles q :estimation e}: q a positive count or a vector of "
                     "increasing probabilities strictly between 0 and 1, e one of " (vec (sort estimation-strategies)))]
    (schema/named-type ::quantile-bins (malli/validator quantiles-schema) message
                       {:cljplot2/type-doc (subs message (count "should be "))
                        :cljplot2/type-name "quantile bins" :cljplot2/type-plural "quantile bins"
                        :cljplot2/type-meaning (str "Bins with edges at quantiles of the values: `{:quantiles q :estimation e}`, e.g. `{:quantiles 4}`. `q` is a count of equal-share bins "
                                                    "or a vector of increasing inner probabilities strictly between 0 and 1; `e` is a fastmath quantile estimation strategy (default `:legacy`).")})))

(defn- bins-dispatch
  "Gives the form of a `:bins` value, the `:multi` branch that checks it.

  Parameters:

  - `v`: any value.

  Returns `:method` for a keyword, `:breaks` for a sequence (a list fails there with the breaks message), `:quantiles` for a map, else `:count`."
  [v]
  (cond (keyword? v) :method (sequential? v) :breaks (map? v) :quantiles :else :count))

(def bins
  "Schema of `:bins`: a bin rule (see [[bin-rule-schema]]), a positive integer (equal-width bins over the data range), explicit breaks (see [[breaks-schema]]), or quantile bins (see [[quantile-bins]])."
  [:multi {:dispatch bins-dispatch}
   [:method bin-rule-schema] [:breaks breaks-schema] [:quantiles quantile-bins] [:count schema/pos-int]])

(def histogram-bins
  "Schema of `:bins` without quantile bins (a histogram's): a bin rule, a positive integer or explicit breaks, see [[bins]]; quantile bins are rejected with their own message."
  [:multi {:dispatch bins-dispatch}
   [:method bin-rule-schema] [:breaks breaks-schema]
   [:quantiles (schema/forbidden "quantile bins aren't supported by histograms (give a breaks vector)")]
   [:count schema/pos-int]])

(defn- quantile-breaks
  "Computes breaks at quantiles of values.

  Parameters:

  - `values` (sequence of doubles): at least one.
  - `quantile-bins` (map): `:quantiles` - a count `n` (probabilities `0, 1/n, ..., 1`) or a vector of inner probabilities (`0, p..., 1`); `:estimation` - a fastmath quantile estimation strategy (default `:legacy`).

  Returns a vector of breaks; tied breaks merged (fewer bins)."
  [values {:keys [quantiles estimation]}]
  (let [ps (if (vector? quantiles)
             (concat [0.0] (map double quantiles) [1.0])
             (m/slice-range (m/long-inc (long quantiles))))]
    (vec (distinct (stats/quantiles values ps (or estimation :legacy))))))

(defn- equal-breaks
  "Computes the breaks of equal-width bins.

  Parameters:

  - `lo`, `hi` (doubles): the range, `lo < hi`.
  - `n` (long): the count of bins, positive.

  Returns a vector of `n + 1` breaks from `lo` to `hi`, `hi` exact."
  [^double lo ^double hi ^long n]
  (let [step (m// (m/- hi lo) n)]
    (conj (mapv (fn [^long i] (m/+ lo (m/* step (double i)))) (range n)) hi)))

(defn breaks
  "Computes bin breaks of values.

  Parameters:

  - `values` (sequence of doubles): finite values.
  - `bin-spec`: see [[bins]] - a bin-estimation method, a bin count, explicit breaks or quantile bins.

  Returns a vector of breaks: explicit breaks as they are, else `nil` for no values; a single distinct value gets one bin of width 1 centred on it; quantile bins merge tied breaks (fewer bins).

  Throws `ex-info {:key :bins}` for an unknown method (only reached by a direct call: option schemas check the method, see [[bin-rule-schema]]) or invalid quantile bins."
  [values bin-spec]
  (when (map? bin-spec) (schema/explain! [[quantile-bins bin-spec [:bins]]]))
  (cond
    (vector? bin-spec) (mapv double bin-spec)
    (empty? values) nil
    :else (let [lo (v/mn values)
                hi (v/mx values)]
            (cond
              (m/== lo hi) (data/widened-range [lo hi])
              (map? bin-spec) (quantile-breaks values bin-spec)
              :else
              (let [n (long (if (keyword? bin-spec)
                              (try (long (stats/estimate-bins values bin-spec))
                                   (catch IllegalArgumentException _
                                     (schema/fail! "Unknown :bins method." {:key :bins :value bin-spec})))
                              (long bin-spec)))]
                (equal-breaks lo hi (m/max 1 n)))))))

(defn bin-index
  "Finds the bin of a value among breaks.

  Parameters:

  - `arr` (double array): increasing breaks, at least 2; bins `[a b)`, the last one `[a b]`.
  - `v` (double): the value.

  Returns the bin's index (long), `-1` outside the breaks."
  ^long [^doubles arr ^double v]
  (let [n (m/long-dec (alength arr))]
    (if (and (m/<= (Array/aget arr 0) v) (m/<= v (Array/aget arr n)))
      (let [i (java.util.Arrays/binarySearch arr v)]
        (if (m/neg? i) (m/long-sub (m/long-sub i) 2) (m/min i (m/long-dec n))))
      -1)))

(defn distinct-digits
  "Finds the fewest significant digits printing numbers apart.

  Parameters:

  - `values` (sequence of numbers).
  - `at-least` (long): the fewest digits returned.

  Returns the fewest significant digits, at least `at-least` and at most 17, printing every one of `values` differently with [[cljplot2.data.shingle/rounded-str]]; `at-least` for fewer than 2 distinct values."
  ^long [values ^long at-least]
  (let [ds (distinct values)]
    (if (m/< (count ds) 2)
      at-least
      (or (first (filter (fn [^long d] (apply distinct? (map #(shingle/rounded-str % d) ds))) (range at-least 18)))
          17))))

(defn break-labels
  "Gives the labels of numeric breaks.

  Parameters:

  - `bin-breaks` (sequence of numbers).

  Returns a vector of strings: all whole numbers printed as integers, otherwise every break with the [[distinct-digits]] (at least 3) significant digits."
  [bin-breaks]
  (if (every? #(m/== (double %) (m/rint (double %))) bin-breaks)
    (mapv #(.toPlainString (.setScale (java.math.BigDecimal. (double %)) 0)) bin-breaks)
    (let [digits (distinct-digits bin-breaks 3)]
      (mapv #(shingle/rounded-str % digits) bin-breaks))))

;; bin

(def bin-options
  "Options of [[bin]]: `:as` (required, the bin column), `:bins` (see [[bins]]), `:digits` (significant digits of the
  printed bounds), `:label`."
  [:map {:closed true}
   [:as {:description "The key of the bin column the function adds to every row." :cljplot2/example :bin}
    [:fn {:error/message "should be the key of the bin column"} some?]]
   (schema/optional :bins {:description "How the function cuts the finite values into bins."
                           :default :sturges :cljplot2/example 4 :cljplot2/example-context {:as :bin}
                           :cljplot2/other-values "Other fastmath methods: one more rule of thumb and two that pick the bin count by minimising an estimated error."
                           :cljplot2/form-meanings {::schema/pos-int "That many equal-width bins over the values' range."
                                                    ::breaks "The bin edges; a value outside them gets a `nil` bin."
                                                    ::quantile-bins "Bins with edges at quantiles of the values."}}
                    bins)
   (schema/optional :digits {:description "The fewest significant digits of the printed bounds; the function prints more when fewer would make two breaks look the same."
                             :default 3 :cljplot2/example 1 :cljplot2/example-context {:as :bin}}
                    schema/pos-int)
   (schema/nilable :label {:description "The series name printed in every bin."
                           :cljplot2/default-doc "No series name."
                           :cljplot2/example true :cljplot2/example-context {:as :bin}
                           :cljplot2/value-meanings {true "The accessor's name, e.g. `x` for `:x`; only for a keyword accessor."
                                                     false "No series name."}
                           :cljplot2/form-meanings {'string? "This name."}}
                   [:or string? boolean?])])

(defn bin
  "Cuts a continuous column into bins (R `cut`).

  Parameters:

  - `rows`: rows or columns (see [[cljplot2.data.rows/rows]]).
  - `value` (keyword or function): the numeric value of a row.
  - `options` (map, see [[bin-options]]):
    - `:as` - the key the bin is stored under (required).
    - `:bins` - see [[breaks]] (default `:sturges`; computed on the finite values).
    - `:digits` - the fewest significant digits of the printed bounds (default 3, as R `cut`), more when needed to keep the printed breaks distinct; the bins keep their exact bounds.
    - `:label` - the series name printed in every bin: a string, or `true` for the accessor's name (keyword accessors only).

  Returns a vector of every row with its bin under `:as`: a `Interval` (see [[cljplot2.data.shingle/interval]]) `[lo, hi)` (`:closed :left`), the last bin `[lo, hi]`; a `nil`, non-numeric or non-finite value, or one outside explicit breaks, gets a `nil` bin (NA). The result's metadata holds `{:na n}` - the count of `nil` bins.

  Throws `ex-info` for bad options (see [[cljplot2.schema]]) or a bad accessor."
  {:cljplot2/api true
   :cljplot2/arguments [{:name 'rows :description "The data: rows (a sequence of maps) or columns (a map of equal-length sequences)."}
                         {:name 'value :description "The accessor of a row's numeric value, e.g. `:hp`."}]
   :cljplot2/returns "Every row with its bin under `:as`: an interval `[lo, hi)`, the last one closed. A missing, non-numeric or non-finite value, or one outside explicit breaks, gets a `nil` bin."
   :cljplot2/options bin-options}
  [rows value {:keys [as label] :as options}]
  (schema/check-options! bin-options options)
  (shingle/check-accessor! value label)
  (let [rs (data/rows rows)
        acc (data/row-accessor :bin value)
        brks (breaks (into [] (comp (map acc) (filter schema/finite?) (map double)) rs) (data/option options :bins :sturges))
        series (cond (string? label) label (true? label) (name value))
        arr (when brks (double-array brks))
        last-bin (m/long-sub (count brks) 2)
        digits (distinct-digits brks (long (data/option options :digits 3)))
        intervals (vec (map-indexed (fn [^long i [a b]] (shingle/->Interval a b series (when (m/< i last-bin) :left) digits))
                                    (partition 2 1 brks)))
        out (mapv (fn [row]
                    (let [v (acc row)
                          i (if (and arr (schema/finite? v)) (bin-index arr (double v)) -1)]
                      (assoc row as (when-not (m/neg? i) (intervals i)))))
                  rs)]
    (with-meta out {:na (count (remove as out))})))

;; categorical

(defn categorical
  "Makes a mapping marker declaring a dimension categorical.

  Rows are read through `wrapped-accessor` as usual; the scale is resolved as with `:type :categorical` (bands for `:x` / `:y`, ordinal for aesthetics), e.g. `:mapping {:y (categorical :cyl)}` puts numeric cylinder counts in bands.

  Parameters:

  - `wrapped-accessor` (keyword or function): the wrapped accessor.

  Returns a [[cljplot2.data.accessor/->CategoricalAccessor]], callable on a row.

  See also [[cljplot2.data.accessor]]."
  {:cljplot2/api true
   :cljplot2/arguments [{:name 'wrapped-accessor :description "The accessor whose values the mapping treats as levels, e.g. `:cyl`."}]
   :cljplot2/returns "A mapping marker: an accessor that reads the same values and makes its dimension categorical."}
  [wrapped-accessor]
  (accessor/->CategoricalAccessor wrapped-accessor))

;; order

(defn order-by-score
  "Orders levels by a score.

  Parameters:

  - `levels` (sequence): distinct, no `nil`.
  - `score` (function): a level's score.
  - `descending?`: order descending, else ascending.

  Returns a vector of `levels` ordered by score; ties keep the order of `levels`; levels whose score isn't a finite number (or `nil`) follow the scored ones, sorted when they compare (else in the order of `levels`).

  See also [[order-by-rows]], [[order-levels]]."
  [levels score descending?]
  (let [scored (keep (fn [l] (when-let [s (schema/finite-double (score l))] [l s])) levels)
        ordered (map first (sort-by second (if descending? #(compare %2 %1) compare) scored))
        unscored (remove (set (map first scored)) levels)]
    (into (vec ordered) (try (sort unscored) (catch ClassCastException _ unscored)))))

(defn order-by-rows
  "Orders levels by a statistic of their rows: the `:order-by` / `:descending` of facet and categorical position scales.

  Parameters:

  - `levels` (sequence): distinct, no `nil`.
  - `rows-of` (function): a level's rows.
  - `order-by` (function): the statistic of a vector of rows.
  - `descending?`: order descending, else ascending.

  Returns a vector of `levels` ordered as in [[order-by-score]]; a level without rows isn't scored."
  [levels rows-of order-by descending?]
  (order-by-score levels (fn [l] (when-let [rs (seq (rows-of l))] (order-by (vec rs)))) descending?))

(def order-by-rules
  "Rules (see [[cljplot2.schema/with-rules]]) of a scale spec ordering its levels by [[order-by-rows]]: `:order-by` not with `:domain`, `:descending` only with `:order-by`."
  [[#(when (and (:order-by %) (:domain %)) "give either :order-by or :domain, not both") [:order-by]]
   [#(when (and (:descending %) (nil? (:order-by %))) ":descending needs :order-by") [:descending]]])

(def order-levels-options
  "Options of [[order-levels]]."
  [:map {:closed true}
   (schema/optional :descending {:description "Whether the largest statistic comes first." :default false
                                 :cljplot2/value-meanings {true "The level of the largest statistic first." false "The level of the smallest statistic first."}}
                    schema/bool)])

(defn order-levels
  "Orders the levels of a categorical column by a statistic of a value, for a scale `:domain`.

  Parameters:

  - `rows`: rows or columns (see [[cljplot2.data.rows/rows]]).
  - `level` (keyword or function): a row's level.
  - `value` (keyword or function): a row's value; the finite numbers of a level's rows are scored.
  - `stat` (function): a function of a non-empty sequence of numbers to a number (`fastmath.stats/median`, `stats/mode`, `count`, ...).
  - `options` (map, optional, see [[order-levels-options]]): `:descending` (default `false`).

  Returns a vector of the levels ordered by `(stat values)`; ties keep first-appearance order (ascending and descending); levels without a finite value follow the ordered ones (natural order), as do levels whose statistic isn't a finite number. The `nil` (NA) level is left out: a domain lists values, the NA level is placed by the scale.

  Throws `ex-info` for bad options."
  {:cljplot2/api true
   :cljplot2/arguments [{:name 'rows :description "The data: rows (a sequence of maps) or columns (a map of equal-length sequences)."}
                         {:name 'level :description "The accessor of a row's level."}
                         {:name 'value :description "The accessor of the value the statistic scores; a level's finite values are scored."}
                         {:name 'stat :description "A function of a non-empty sequence of numbers that returns a number, e.g. `stats/median` or `count`."}]
   :cljplot2/returns "The levels ordered by their statistic; ties keep first-appearance order, and levels without a finite statistic follow. The `nil` level is left out."
   :cljplot2/options order-levels-options}
  ([rows level value stat] (order-levels rows level value stat {}))
  ([rows level value stat {:keys [descending] :as options}]
   (schema/check-options! order-levels-options options)
   (let [rs (data/rows rows)
         level-of (data/row-accessor :level level)
         value-of (data/row-accessor :value value)
         by-level (group-by level-of rs)
         score (fn [l] (let [vs (filterv schema/finite? (map value-of (by-level l)))]
                         (when (seq vs) (stat vs))))]
     (order-by-score (remove nil? (distinct (map level-of rs))) score descending))))

(def ^:private rank-ties
  "Tie strategies of [[rank-rows]] (`fastmath.core/rank`)."
  #{:average :first :last :random :min :max :dense})

(def rank-rows-options
  "Options of [[rank-rows]]."
  [:map {:closed true}
   (schema/optional :as {:description "The key of the rank column the function adds to every row." :default :rank :cljplot2/example :place}
                    [:fn {:error/message "should be the key of the rank column"} some?])
   (schema/optional :descending {:description "Whether the highest value gets rank 1." :default true
                                 :cljplot2/value-meanings {true "The highest value ranks 1." false "The lowest value ranks 1."}}
                    schema/bool)
   (schema/optional :ties {:description "How the function ranks equal values, by `fastmath.core/rank`."
                           :default :min
                           :cljplot2/value-meanings {:min "The lowest rank of the tie for each: 1, 1, 3."
                                                     :max "The highest rank of the tie for each: 2, 2, 3."
                                                     :average "The mean rank of the tie, a double: 1.5, 1.5, 3."
                                                     :dense "The lowest rank, without gaps after a tie: 1, 1, 2."
                                                     :first "Consecutive ranks in row order: 1, 2, 3."
                                                     :last "Consecutive ranks in reverse row order: 2, 1, 3."
                                                     :random "Consecutive ranks in random order."}}
                    (into [:enum] (sort rank-ties)))])

(defn rank-rows
  "Ranks the value of every row among the rows sharing its x (bump charts).

  For a bump chart draw a line through the ranks on a reversed y domain (`:scales {:y {:domain [n 1]}}`).

  Parameters:

  - `rows`: rows or columns (see [[cljplot2.data.rows/rows]]).
  - `x` (keyword or function): a row's x; rows share an x by its key (numbers and `java.time` values by [[cljplot2.data.rows/x-key]] - `1` and `1.0` are one x - levels by value).
  - `value` (keyword or function): the ranked value.
  - `options` (map, optional, see [[rank-rows-options]]):
    - `:as` - the key of the rank column (default `:rank`).
    - `:descending` - the highest value first, rank 1 (default `true`).
    - `:ties` - a `fastmath.core/rank` strategy (default `:min`: 1, 1, 3; `:average` gives doubles).

  Returns a vector of every row with its 1-based rank under `:as`; a row without a finite value or a valid x gets `nil` and isn't ranked.

  Throws `ex-info` for bad options."
  {:cljplot2/api true
   :cljplot2/arguments [{:name 'rows :description "The data: rows (a sequence of maps) or columns (a map of equal-length sequences)."}
                         {:name 'x :description "The accessor of a row's x: the function ranks the rows sharing an x together."}
                         {:name 'value :description "The accessor of the ranked value."}]
   :cljplot2/returns "Every row with its rank under `:as`, 1 the first; a row without a finite value or a valid x gets `nil`."
   :cljplot2/options rank-rows-options}
  ([rows x value] (rank-rows rows x value {}))
  ([rows x value {:keys [ties descending] rank-key :as :or {ties :min descending true rank-key :rank} :as options}]
   (schema/check-options! rank-rows-options options)
   (let [rs (data/rows rows)
         x-of (data/row-accessor :x x)
         value-of (data/row-accessor :value value)
         x-group (fn [r] (let [v (x-of r)] (when (data/valid-value? v) (or (data/x-key v) v))))
         rankable? (fn [i] (and (schema/finite-double (value-of (rs i))) (some? (x-group (rs i)))))
         ranked (into {} (mapcat (fn [indices]
                                   (let [vs (map #(schema/finite-double (value-of (rs %))) indices)]
                                     (map vector indices
                                          (map #(if (integer? %) (m/long-inc (long %)) (m/inc (double %)))
                                               (m/rank (map #(if descending (m/- (double %)) %) vs) ties))))))
                     (vals (group-by #(x-group (rs %)) (filter rankable? (range (count rs))))))]
     (vec (map-indexed (fn [i r] (assoc r rank-key (ranked i))) rs)))))
