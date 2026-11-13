(ns cljplot2.data.levels
  "Level helpers - data functions users call before plotting (charts don't call them):

  * [[order-levels]] - categorical levels ordered by a statistic of a value, for a scale `:domain`
    (`:scales {:y {:domain (order-levels barley :variety :yield stats/median)}}`)
  * [[bin]] - a continuous column cut into bins: half-open intervals (see [[cljplot2.data.shingle/Interval]]),
    levels for strips, colours or facets (sorted ascending wherever they are levels)
  * [[breaks]] - the bin breaks shared with the histogram's `:bins`"
  (:require [fastmath.core :as m]
            [fastmath.stats :as stats]
            [cljplot2.data.rows :as rows]
            [cljplot2.data.shingle :as shingle]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; breaks

(def ^:private breaks-schema
  (schema/with-rules [:vector {:min 2} schema/finite]
    [#(when-not (apply < %) "breaks should be increasing")]))

(def bins
  "Schema of `:bins`: a fastmath bin-estimation method keyword (`:sturges`, `:sqrt`, `:rice`, `:doane`, `:scott`,
  `:freedman-diaconis`), a positive integer (equal-width bins over the data range) or a vector of at least 2
  increasing breaks."
  [:multi {:dispatch #(cond (keyword? %) :method (vector? %) :breaks :else :count)}
   [:method keyword?] [:breaks breaks-schema] [:count schema/pos-int]])

(defn- equal-breaks
  [^double lo ^double hi ^long n]
  (let [step (/ (- hi lo) n)]
    (conj (mapv #(+ lo (* step (double %))) (range n)) hi)))

(defn breaks
  "Bin breaks for finite `values` (doubles) and `bins` (see [[bins]]); `nil` for no values (explicit breaks are
  returned as they are). A single distinct value gets one bin of width 1 centred on it. Throws `ex-info {:key :bins}`
  for an unknown method."
  [values bins]
  (cond
    (vector? bins) (mapv double bins)
    (empty? values) nil
    :else (let [lo (double (reduce min values))
                hi (double (reduce max values))]
            (if (== lo hi)
              [(- lo 0.5) (+ lo 0.5)]
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
        ;; the fewest digits (at least :digits) printing every break differently
        digits (let [ds (distinct brks)
                     at-least (long (get opts :digits 3))]
                 (if (< (count ds) 2)
                   at-least
                   (or (first (filter (fn [^long d] (apply distinct? (map #(shingle/rounded-str % d) ds)))
                                      (range at-least 18)))
                       17)))
        intervals (vec (map-indexed (fn [i [a b]] (shingle/->Interval a b series (when (< (long i) last-bin) :left) digits))
                                    (partition 2 1 brks)))]
    (let [out (mapv (fn [row]
                      (let [v (acc row)
                            i (if (and arr (finite v)) (bin-index arr (double v)) -1)]
                        (assoc row as (when-not (neg? i) (intervals i)))))
                    rs)]
      (with-meta out {:na (count (remove as out))}))))

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
