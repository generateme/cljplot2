(ns cljplot2.data.levels
  "Level helpers - data functions users call before plotting (charts don't call them):

  * [[order-levels]] - categorical levels ordered by a statistic of a value, for a scale `:domain`
    (`:scales {:y {:domain (order-levels barley :variety :yield stats/median)}}`)
  * [[bin]] - a continuous column cut into bins: half-open intervals (see [[cljplot2.data.shingle/Interval]]),
    levels for strips, colours or facets (sorted ascending wherever they are levels)
  * [[breaks]] - the bin breaks shared with the histogram's `:bins`
  * [[categorical]] - a mapping marker declaring a dimension categorical
  * [[rank-rows]] - the rank of a value among the rows at each x (bump charts)

  Helpers shared with charts and scales: [[bin-index]], [[break-labels]], [[distinct-digits]], [[order-by-score]], [[order-by-rows]]."
  (:require [fastmath.core :as m]
            [malli.core :as malli]
            [fastmath.stats :as stats]
            [fastmath.vector :as v]
            [cljplot2.data.accessor :as accessor]
            [cljplot2.data.rows :as rows]
            [cljplot2.data.shingle :as shingle]
            [cljplot2.schema :as schema])
  (:import [fastmath.java Array]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; breaks

(def ^:private breaks-schema
  "Schema of explicit breaks: at least 2 increasing finite numbers."
  (schema/with-rules [:vector {:min 2} schema/finite]
    [#(when-not (apply < %) "breaks should be increasing")]))

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

(def ^:private quantile-bins
  "Schema of quantile bins reporting one error at the `:bins` key (not inside the map)."
  (schema/named-type ::quantile-bins (malli/validator quantiles-schema)
                     (str "should be quantile bins {:quantiles q :estimation e}: q a positive count or a vector of "
                          "increasing probabilities strictly between 0 and 1, e one of " (vec (sort estimation-strategies)))))

(def bins
  "Schema of `:bins`: a fastmath bin-estimation method keyword (`:sturges`, `:sqrt`, `:rice`, `:doane`, `:scott`,
  `:freedman-diaconis`), a positive integer (equal-width bins over the data range), a vector of at least 2
  increasing breaks, or quantile bins `{:quantiles q :estimation e}` - `q` a count of equal-probability bins or a
  vector of increasing inner probabilities (strictly between 0 and 1), `e` a fastmath quantile estimation strategy
  (`:legacy` - default, `:r1` ... `:r9`)."
  [:multi {:dispatch #(cond (keyword? %) :method (vector? %) :breaks (map? %) :quantiles :else :count)}
   [:method keyword?] [:breaks breaks-schema] [:quantiles quantile-bins] [:count schema/pos-int]])

(defn- quantile-breaks
  "Computes breaks at quantiles of values.

  Parameters:

  - `values` (sequence of doubles): at least one.
  - `quantile-bins` (map): `:quantiles` - a count `n` (probabilities `0, 1/n, ..., 1`) or a vector of inner probabilities (`0, p..., 1`); `:estimation` - a fastmath quantile estimation strategy (default `:legacy`).

  Returns a vector of breaks; tied breaks merged (fewer bins)."
  [values {:keys [quantiles estimation]}]
  (let [ps (if (vector? quantiles)
             (vec (concat [0.0] (map double quantiles) [1.0]))
             (vec (m/slice-range (m/long-inc (long quantiles)))))]
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
  - `bins`: see [[bins]] - a bin-estimation method, a bin count, explicit breaks or quantile bins.

  Returns a vector of breaks: explicit breaks as they are, else `nil` for no values; a single distinct value gets one bin of width 1 centred on it; quantile bins merge tied breaks (fewer bins).

  Throws `ex-info {:key :bins}` for an unknown method or invalid quantile bins."
  [values bins]
  (when (map? bins) (schema/explain! [[quantile-bins bins [:bins]]]))
  (cond
    (vector? bins) (mapv double bins)
    (empty? values) nil
    :else (let [lo (v/mn values)
                hi (v/mx values)]
            (cond
              (m/== lo hi) [(m/- lo 0.5) (m/+ lo 0.5)]
              (map? bins) (quantile-breaks values bins)
              :else
              (let [n (long (if (keyword? bins)
                              (try (long (stats/estimate-bins values bins))
                                   (catch IllegalArgumentException _
                                     (schema/fail! "Unknown :bins method." {:key :bins :value bins})))
                              (long bins)))]
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

  - `breaks` (sequence of numbers).

  Returns a vector of strings: all whole numbers printed as integers, otherwise every break with the [[distinct-digits]] (at least 3) significant digits."
  [breaks]
  (if (every? #(m/== (double %) (m/rint (double %))) breaks)
    (mapv #(.toPlainString (.setScale (java.math.BigDecimal. (double %)) 0)) breaks)
    (let [digits (distinct-digits breaks 3)]
      (mapv #(shingle/rounded-str % digits) breaks))))

;; bin

(def bin-options
  "Options of [[bin]]: `:as` (required, the bin column), `:bins` (see [[bins]]), `:digits` (significant digits of the
  printed bounds), `:label`."
  [:map {:closed true}
   [:as {:description "the key of the bin column added to every row (required)"}
    [:fn {:error/message "should be the key of the bin column"} some?]]
   (schema/optional :bins {:description (str "the bins: a fastmath bin-estimation method, a bin count, a vector of "
                                             "increasing breaks or quantile bins {:quantiles q :estimation e}")
                           :default :sturges}
                    bins)
   (schema/optional :digits {:description (str "the fewest significant digits of the printed bounds (more when "
                                               "needed to keep the printed breaks distinct)")
                             :default 3}
                    schema/pos-int)
   (schema/nilable :label {:description (str "the series name printed in every bin: a string, or true for the "
                                             "accessor's name (keyword accessors only); none when absent")}
                   [:or string? boolean?])])

(defn bin
  "Cuts a continuous column into bins (R `cut`).

  Parameters:

  - `data`: rows or columns (see [[cljplot2.data.rows/rows]]).
  - `value` (keyword or function): the numeric value of a row.
  - `opts` (map, see [[bin-options]]):
    - `:as` - the key the bin is stored under (required).
    - `:bins` - see [[breaks]] (default `:sturges`; computed on the finite values).
    - `:digits` - the fewest significant digits of the printed bounds (default 3, as R `cut`), more when needed to keep the printed breaks distinct; the bins keep their exact bounds.
    - `:label` - the series name printed in every bin: a string, or `true` for the accessor's name (keyword accessors only).

  Returns a vector of every row with its bin under `:as`: a [[cljplot2.data.shingle/Interval]] `[lo, hi)` (`:closed :left`), the last bin `[lo, hi]`; a `nil`, non-numeric or non-finite value, or one outside explicit breaks, gets a `nil` bin (NA). The result's metadata holds `{:na n}` - the count of `nil` bins.

  Throws `ex-info` for bad options (see [[cljplot2.schema]]) or a bad accessor."
  {:cljplot2/api true :cljplot2/options bin-options}
  [data value {:keys [as label] :as opts}]
  (schema/check-options! bin-options opts)
  (shingle/check-accessor! value label)
  (let [rs (rows/rows data)
        acc (rows/row-accessor :bin value)
        finite #(and (number? %) (m/valid-double? (double %)))
        brks (breaks (into [] (comp (map acc) (filter finite) (map double)) rs) (get opts :bins :sturges))
        series (cond (string? label) label (true? label) (name value))
        arr (when brks (double-array brks))
        last-bin (m/long-sub (count brks) 2)
        digits (distinct-digits brks (long (get opts :digits 3)))
        intervals (vec (map-indexed (fn [^long i [a b]] (shingle/->Interval a b series (when (m/< i last-bin) :left) digits))
                                    (partition 2 1 brks)))
        out (mapv (fn [row]
                    (let [v (acc row)
                          i (if (and arr (finite v)) (bin-index arr (double v)) -1)]
                      (assoc row as (when-not (m/neg? i) (intervals i)))))
                  rs)]
    (with-meta out {:na (count (remove as out))})))

;; categorical

(defn categorical
  "Makes a mapping marker declaring a dimension categorical.

  Rows are read through `accessor` as usual; the scale is resolved as with `:type :categorical` (bands for `:x` / `:y`, ordinal for aesthetics), e.g. `:mapping {:y (categorical :cyl)}` puts numeric cylinder counts in bands.

  Parameters:

  - `accessor` (keyword or function): the wrapped accessor.

  Returns a [[cljplot2.data.accessor/->CategoricalAccessor]], callable on a row.

  See also [[cljplot2.data.accessor]]."
  {:cljplot2/api true}
  [accessor]
  (accessor/->CategoricalAccessor accessor))

;; order

(defn- finite-score
  "Gives a score as a double when it is a finite number.

  Parameters:

  - `s`: any value.

  Returns `s` as a double, `nil` unless it is a finite number."
  [s]
  (when (and (number? s) (m/valid-double? (double s))) (double s)))

(defn order-by-score
  "Orders levels by a score.

  Parameters:

  - `levels` (sequence): distinct, no `nil`.
  - `score` (function): a level's score.
  - `descending?`: order descending, else ascending.

  Returns a vector of `levels` ordered by score; ties keep the order of `levels`; levels whose score isn't a finite number (or `nil`) follow the scored ones, sorted when they compare (else in the order of `levels`).

  See also [[order-by-rows]], [[order-levels]]."
  [levels score descending?]
  (let [scored (keep (fn [l] (when-let [s (finite-score (score l))] [l s])) levels)
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

(def order-levels-options
  "Options of [[order-levels]]."
  [:map {:closed true}
   (schema/optional :descending {:description "order the levels by the statistic descending" :default false}
                    schema/bool)])

(defn order-levels
  "Orders the levels of a categorical column by a statistic of a value, for a scale `:domain`.

  Parameters:

  - `data`: rows or columns (see [[cljplot2.data.rows/rows]]).
  - `level` (keyword or function): a row's level.
  - `value` (keyword or function): a row's value; the finite numbers of a level's rows are scored.
  - `stat` (function): a function of a non-empty sequence of numbers to a number (`fastmath.stats/median`, `stats/mode`, `count`, ...).
  - `opts` (map, optional, see [[order-levels-options]]): `:descending` (default `false`).

  Returns a vector of the levels ordered by `(stat values)`; ties keep first-appearance order (ascending and descending); levels without a finite value follow the ordered ones (natural order), as do levels whose statistic isn't a finite number. The `nil` (NA) level is left out: a domain lists values, the NA level is placed by the scale.

  Throws `ex-info` for bad options."
  {:cljplot2/api true :cljplot2/options order-levels-options}
  ([data level value stat] (order-levels data level value stat {}))
  ([data level value stat {:keys [descending] :as opts}]
   (schema/check-options! order-levels-options opts)
   (let [rs (rows/rows data)
         level-of (rows/row-accessor :level level)
         value-of (rows/row-accessor :value value)
         by-level (group-by level-of rs)
         score (fn [l] (let [vs (filterv #(and (number? %) (m/valid-double? (double %))) (map value-of (by-level l)))]
                         (when (seq vs) (stat vs))))]
     (order-by-score (remove nil? (distinct (map level-of rs))) score descending))))

(def rank-ties
  "Tie strategies of [[rank-rows]] (`fastmath.core/rank`)."
  #{:average :first :last :random :min :max :dense})

(def rank-rows-options
  "Options of [[rank-rows]]."
  [:map {:closed true}
   (schema/optional :key {:description "the key of the rank column added to every row" :default :rank}
                    [:fn {:error/message "should be the key of the rank column"} some?])
   (schema/optional :descending {:description "rank the highest value first (rank 1)" :default true} schema/bool)
   (schema/optional :ties {:description "the fastmath.core/rank tie strategy (:min gives 1, 1, 3; :average gives doubles)"
                           :default :min}
                    (into [:enum] (sort rank-ties)))])

(defn rank-rows
  "Ranks the value of every row among the rows sharing its x (bump charts).

  For a bump chart draw a line through the ranks on a reversed y domain (`:scales {:y {:domain [n 1]}}`).

  Parameters:

  - `data`: rows or columns (see [[cljplot2.data.rows/rows]]).
  - `x` (keyword or function): a row's x; rows share an x by its key (numbers and `java.time` values by [[cljplot2.data.rows/x-key]] - `1` and `1.0` are one x - levels by value).
  - `value` (keyword or function): the ranked value.
  - `opts` (map, optional, see [[rank-rows-options]]):
    - `:key` - the key of the rank column (default `:rank`).
    - `:descending` - the highest value first, rank 1 (default `true`).
    - `:ties` - a `fastmath.core/rank` strategy (default `:min`: 1, 1, 3; `:average` gives doubles).

  Returns a vector of every row with its 1-based rank under `:key`; a row without a finite value or a valid x gets `nil` and isn't ranked.

  Throws `ex-info` for bad options."
  {:cljplot2/api true :cljplot2/options rank-rows-options}
  ([data x value] (rank-rows data x value {}))
  ([data x value {:keys [ties descending key] :or {ties :min descending true key :rank} :as opts}]
   (schema/check-options! rank-rows-options opts)
   (let [rs (rows/rows data)
         x-of (rows/row-accessor :x x)
         value-of (rows/row-accessor :value value)
         finite (fn [v] (when (and (number? v) (m/valid-double? (double v))) (double v)))
         x-group (fn [r] (let [v (x-of r)] (when (rows/valid-value? v) (or (rows/x-key v) v))))
         rankable? (fn [i] (and (finite (value-of (rs i))) (some? (x-group (rs i)))))
         ranked (into {} (mapcat (fn [indices]
                                   (let [vs (mapv #(finite (value-of (rs %))) indices)]
                                     (map vector indices
                                          (map #(if (integer? %) (m/long-inc (long %)) (m/inc (double %)))
                                               (m/rank (mapv #(if descending (m/- (double %)) %) vs) ties))))))
                     (vals (group-by #(x-group (rs %)) (filter rankable? (range (count rs))))))]
     (vec (map-indexed (fn [i r] (assoc r key (ranked i))) rs)))))
