(ns cljplot2.data.levels
  "Level helpers - data functions users call before plotting (charts don't call them):

  * [[order-levels]] - categorical levels ordered by a statistic of a value, for a scale `:domain`
    (`:scales {:y {:domain (order-levels barley :variety :yield stats/median)}}`)
  * [[bin]] - a continuous column cut into bins: half-open intervals (see [[cljplot2.data.shingle/Interval]]),
    levels for strips, colours or facets (sorted ascending wherever they are levels)
  * [[breaks]] - the bin breaks shared with the histogram's `:bins`
  * [[categorical]] - a mapping marker declaring a dimension categorical
  * [[rank-rows]] - the rank of a value among the rows at each x (bump charts)"
  (:require [fastmath.core :as m]
            [malli.core :as malli]
            [fastmath.stats :as stats]
            [cljplot2.data.accessor :as accessor]
            [cljplot2.data.rows :as rows]
            [cljplot2.data.shingle :as shingle]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; breaks

(def ^:private breaks-schema
  (schema/with-rules [:vector {:min 2} schema/finite]
    [#(when-not (apply < %) "breaks should be increasing")]))

(def ^:private estimation-strategies (set (keys stats/estimation-strategies-list)))

(def ^:private probabilities-schema
  (schema/with-rules [:vector {:min 1} [:and schema/finite [:fn {:error/message "should be strictly between 0 and 1"}
                                                             #(< 0.0 (double %) 1.0)]]]
    [#(when-not (apply < %) "probabilities should be increasing")]))

(def ^:private quantiles-schema
  [:map {:closed true}
   [:quantiles [:multi {:dispatch vector?} [true probabilities-schema] [false schema/pos-int]]]
   (schema/optional :estimation (into [:enum] (sort estimation-strategies)))])

(def ^:private quantile-bins
  ;; one error at the :bins key (not inside the map)
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
  "Breaks at the quantiles of `values` (doubles, at least one) for quantile bins `{:quantiles q :estimation e}`:
  probabilities `0, 1/n, ..., 1` for a count `n`, else `0, p..., 1`; tied breaks merged (fewer bins)."
  [values {:keys [quantiles estimation]}]
  (let [ps (if (vector? quantiles)
             (vec (concat [0.0] (map double quantiles) [1.0]))
             (let [n (long quantiles)] (mapv #(/ (double %) (double n)) (range (inc n)))))]
    (vec (distinct (stats/quantiles values ps (or estimation :legacy))))))

(defn- equal-breaks
  [^double lo ^double hi ^long n]
  (let [step (/ (- hi lo) n)]
    (conj (mapv #(+ lo (* step (double %))) (range n)) hi)))

(defn breaks
  "Bin breaks for finite `values` (doubles) and `bins` (see [[bins]]); `nil` for no values (explicit breaks are
  returned as they are). A single distinct value gets one bin of width 1 centred on it; quantile bins merge tied
  breaks (fewer bins). Throws `ex-info {:key :bins}` for an unknown method or invalid quantile bins."
  [values bins]
  (when (map? bins) (schema/explain! [[quantile-bins bins [:bins]]]))
  (cond
    (vector? bins) (mapv double bins)
    (empty? values) nil
    :else (let [lo (double (reduce min values))
                hi (double (reduce max values))]
            (cond
              (== lo hi) [(- lo 0.5) (+ lo 0.5)]
              (map? bins) (quantile-breaks values bins)
              :else
              (let [n (long (if (keyword? bins)
                              (try (long (stats/estimate-bins values bins))
                                   (catch IllegalArgumentException _
                                     (schema/fail! "Unknown :bins method." {:key :bins :value bins})))
                              (long bins)))]
                (equal-breaks lo hi (m/max 1 n)))))))

(defn bin-index
  "Index of the bin of `v` in the breaks array `arr` (bins `[a b)`, the last one `[a b]`); `-1` outside."
  ^long [^doubles arr ^double v]
  (let [n (dec (alength arr))]
    (if (and (<= (aget arr 0) v) (<= v (aget arr n)))
      (let [i (java.util.Arrays/binarySearch arr v)]
        (if (neg? i) (- (- i) 2) (m/min i (dec n))))
      -1)))

(defn distinct-digits
  "The fewest significant digits, at least `at-least` (17 at most), printing every one of `values` differently with
  [[cljplot2.data.shingle/rounded-str]]."
  ^long [values ^long at-least]
  (let [ds (distinct values)]
    (if (< (count ds) 2)
      at-least
      (or (first (filter (fn [^long d] (apply distinct? (map #(shingle/rounded-str % d) ds))) (range at-least 18)))
          17))))

(defn break-labels
  "Labels of numeric `breaks`: whole numbers printed as integers, otherwise every break with the
  [[distinct-digits]] (at least 3) significant digits."
  [breaks]
  (if (every? #(== (double %) (Math/rint (double %))) breaks)
    (mapv #(.toPlainString (.setScale (java.math.BigDecimal. (double %)) 0)) breaks)
    (let [digits (distinct-digits breaks 3)]
      (mapv #(shingle/rounded-str % digits) breaks))))

;; bin

(def bin-options
  "Options of [[bin]]: `:as` (required, the bin column), `:bins` (see [[bins]], default `:sturges`), `:digits`
  (significant digits of the printed bounds, default 3), `:label`."
  [:map {:closed true}
   [:as [:fn {:error/message "should be the key of the bin column"} some?]]
   (schema/optional :bins bins)
   (schema/optional :digits schema/pos-int)
   (schema/nilable :label [:or string? boolean?])])

(def ^:private bin-args
  (schema/with-rules
   [:map [:accessor schema/accessor] [:options bin-options]]
   [(fn [{:keys [accessor options]}]
      (when (and (true? (:label options)) (not (keyword? accessor)))
        "label true needs a keyword accessor (its name is the label)"))
    [:options :label]]))

(defn bin
  "Rows of `data` (see [[cljplot2.data.rows/rows]]) with the bin of `value` (keyword or function) under `(:as opts)`:
  a [[cljplot2.data.shingle/Interval]] `[lo, hi)` (`:closed :left`), the last bin `[lo, hi]`. Breaks from `:bins`
  (see [[breaks]]; computed on the finite values). A `nil`, non-numeric or non-finite value, or one outside explicit
  breaks, gets a `nil` bin (NA); every row is kept, the result's metadata holds `{:na n}` - the count of `nil` bins.
  Bounds print with at least `:digits` significant digits (default 3, as R `cut`), more when needed to keep the
  printed breaks distinct; the bins keep their exact bounds. `:label` names the series in every bin: a string, or
  `true` for the accessor's name (keyword accessors only).
  Throws `ex-info` for bad options ([[bin-options]], see [[cljplot2.schema]])."
  [data value {:keys [as label] :as opts}]
  (schema/explain! [[bin-args {:accessor value :options opts} []]])
  (let [rs (rows/rows data)
        acc (rows/row-accessor :bin value)
        finite #(and (number? %) (m/valid-double? (double %)))
        brks (breaks (into [] (comp (map acc) (filter finite) (map double)) rs) (get opts :bins :sturges))
        series (cond (string? label) label (true? label) (name value))
        arr (when brks (double-array brks))
        last-bin (- (count brks) 2)
        digits (distinct-digits brks (long (get opts :digits 3)))
        intervals (vec (map-indexed (fn [i [a b]] (shingle/->Interval a b series (when (< (long i) last-bin) :left) digits))
                                    (partition 2 1 brks)))]
    (let [out (mapv (fn [row]
                      (let [v (acc row)
                            i (if (and arr (finite v)) (bin-index arr (double v)) -1)]
                        (assoc row as (when-not (neg? i) (intervals i)))))
                    rs)]
      (with-meta out {:na (count (remove as out))}))))

;; categorical

(defn categorical
  "Mapping marker: `accessor` (keyword or function) read as usual, its dimension declared categorical - the scale is
  resolved as with `:type :categorical` (bands for `:x` / `:y`, ordinal for aesthetics), e.g.
  `:mapping {:y (categorical :cyl)}` puts numeric cylinder counts in bands. See [[cljplot2.data.accessor]]."
  [accessor]
  (accessor/->CategoricalAccessor accessor))

;; order

(defn order-levels
  "Levels of `level` (keyword or function) in the rows of `data` ordered by `(stat values)` - `values` the finite
  numbers of `value` in that level's rows; for a scale `:domain`. `stat`: a function of a non-empty sequence of
  numbers to a number (`fastmath.stats/median`, `stats/mode`, `count`, ...).
  `opts`: `:descending` (default false). Ties keep first-appearance order (ascending and descending); levels without a
  finite value follow the ordered ones (natural order), as do levels whose statistic isn't a finite number. The `nil`
  (NA) level is left out: a domain lists values, the NA level is placed by the scale."
  ([data level value stat] (order-levels data level value stat {}))
  ([data level value stat {:keys [descending]}]
   (let [rs (rows/rows data)
         level-of (rows/row-accessor :level level)
         value-of (rows/row-accessor :value value)
         levels (distinct (map level-of rs))
         by-level (group-by level-of rs)
         score (fn [l] (let [vs (filterv #(and (number? %) (m/valid-double? (double %))) (map value-of (by-level l)))
                             s (when (seq vs) (stat vs))]
                         ;; a statistic without a finite value (e.g. NaN) leaves the level unordered
                         (when (and (number? s) (m/valid-double? (double s))) (double s))))
         scored (keep (fn [l] (when (some? l) (when-let [s (score l)] [l s]))) levels)
         ordered (map first (sort-by second (if descending #(compare %2 %1) compare) scored))
         unscored (remove (set (map first scored)) (remove nil? levels))]
     (into (vec ordered) (try (sort unscored) (catch ClassCastException _ unscored))))))

(def rank-ties
  "Tie strategies of [[rank-rows]] (`fastmath.core/rank`)."
  #{:average :first :last :random :min :max :dense})

(defn rank-rows
  "Rows of `data` (rows or columns, see [[cljplot2.data.rows/rows]]) with the rank of `value` among the rows sharing
  their `x` value added under `:key` (default `:rank`): 1-based, the highest value first (`:descending`, default
  true); `:ties` - a `fastmath.core/rank` strategy (default `:min`: 1, 1, 3; `:average` gives doubles). Rows share an
  x by its key (numbers and `java.time` values by [[cljplot2.data.rows/x-key]] - `1` and `1.0` are one x - levels by
  value). A row without a finite value or a valid x gets `nil` and isn't ranked. For bump charts: a line through the ranks on a reversed y
  domain (`:scales {:y {:domain [n 1]}}`). Throws `ex-info {:key :ties}` for an unknown strategy."
  ([data x value] (rank-rows data x value {}))
  ([data x value {:keys [ties descending key] :or {ties :min descending true key :rank}}]
   (when-not (contains? rank-ties ties)
     (schema/fail! (str ":ties should be one of " (vec (sort rank-ties)) ".") {:key :ties :value ties}))
   (let [rs (rows/rows data)
         x-of (rows/row-accessor :x x)
         value-of (rows/row-accessor :value value)
         finite (fn [v] (when (and (number? v) (m/valid-double? (double v))) (double v)))
         x-group (fn [r] (let [v (x-of r)] (when (rows/valid-value? v) (or (rows/x-key v) v))))
         rankable? (fn [i] (and (finite (value-of (rs i))) (some? (x-group (rs i)))))
         ranked (into {} (mapcat (fn [indices]
                                   (let [vs (mapv #(finite (value-of (rs %))) indices)]
                                     (map vector indices
                                          (map #(if (integer? %) (inc (long %)) (inc (double %)))
                                               (m/rank (mapv #(if descending (- (double %)) %) vs) ties))))))
                     (vals (group-by #(x-group (rs %)) (filter rankable? (range (count rs))))))]
     (vec (map-indexed (fn [i r] (assoc r key (ranked i))) rs)))))
