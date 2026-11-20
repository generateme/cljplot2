(ns cljplot2.data.rows
  "Plot data: normalization to rows, mapping accessors, valid values and extents.

  `:data` is a finite sequence of maps (rows) or a map of equal-length finite sequences (columns); both become a
  vector of rows.
  A mapping accessor is a keyword or a function of a row.

  An extent describes the values of one mapped dimension:

  * `{:kind :numeric :min a :max b}` (doubles)
  * `{:kind :temporal :min a :max b}` (`java.time` values)
  * `{:kind :categorical :values [v ...]}` (strings/keywords, distinct, in order of first appearance)

  [[value-extent]] also keeps the distinct values (`:values`) of numeric and temporal data, for scales that
  need the levels (ordinal colour, shape, size) - at most [[max-levels]] of them."
  (:require [fastmath.core :as m]
            [cljplot2.data.accessor :as accessor]
            [cljplot2.schema :as schema])
  (:import [java.time Instant LocalDate LocalDateTime LocalTime OffsetDateTime ZonedDateTime ZoneOffset]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def mapping-keys
  "Mapping keys of the built-in dimensions (accessors); `:z` - the third coordinate of 3D charts; `:hierarchy` (treemap)
  holds a vector of accessors; `:set` / `:item` - set membership (Venn)."
  [:x :y :z :ymin :ymax :xend :yend :open :high :low :close :color :shape :size :dash :pattern :group :facet :facet-col :facet-row :order :value :set :item])

(def ^:private mapping-key-descriptions
  "Plot-level meaning of each mapping key (each chart's mapping rows say how it reads them)."
  {:x "x position (or the values of a strip chart with levels on y)"
   :y "y position (or the levels of a strip chart)"
   :z "third coordinate of 3D charts"
   :ymin "lower end of an interval (ribbons, error bars)"
   :ymax "upper end of an interval"
   :xend "x of a segment's end"
   :yend "y of a segment's end"
   :open "opening value (candlesticks)"
   :high "highest value (candlesticks)"
   :low "lowest value (candlesticks)"
   :close "closing value (candlesticks)"
   :color "colour aesthetic: groups, or values on a colour scale"
   :shape "point shape aesthetic"
   :size "size aesthetic"
   :dash "line dash aesthetic"
   :pattern "fill pattern aesthetic"
   :group "groups without a visual aesthetic"
   :facet "wrap facets: one panel per value"
   :facet-col "grid facets: one column per value"
   :facet-row "grid facets: one row per value"
   :order "drawing order of points"
   :value "a value: cell values, weights, sizes of hierarchy leaves"
   :set "set name of a membership row (Venn)"
   :item "item of a membership row (Venn)"})

(def mapping-schema
  "A plot or chart `:mapping`: accessors (keyword or function; `nil` - not mapped) of the built-in keys, `:hierarchy`
  a non-empty vector of accessors; `:fill` is rejected (not an aesthetic, see the density / histogram `:fill`
  options). Open: user charts may read other keys."
  (-> [:map {:cljplot2/type-doc "a mapping: key -> accessor (a keyword or a function of a row)"}]
      (into (map #(schema/nilable % {:description (mapping-key-descriptions %)} schema/accessor)) mapping-keys)
      (conj (schema/nilable :hierarchy {:description "levels of a hierarchy, root first (treemap, partition, pack)"}
                            [:vector {:min 1} schema/accessor])
            ;; no :fill aesthetic: a mapped :fill would be silently ignored
            (schema/nilable :fill (schema/forbidden "isn't an aesthetic - fill areas by group with the chart option :fill :color")))))

(def data-schema
  "Plot or chart `:data`: a sequence of rows (maps) or a map of columns (sequences); rows and columns are checked by
  [[rows]]."
  (schema/named-type ::data #(or (sequential? %) (map? %))
                     "should be a sequence of rows (maps) or a map of columns (sequences)"))

(def common-options
  "Map entries of the options every chart takes: own `:data` (see [[rows]]) and `:mapping` (over the plot's)."
  (schema/in-fragment
   :common
   [(schema/nilable :data {:description "the chart's own data instead of the plot's"
                           :cljplot2/default-doc "the plot's :data"}
                    data-schema)
    (schema/optional :mapping {:description "the chart's mapping, merged over the plot's mapping (a key set to nil removes it)"}
                     mapping-schema)]))

(def position-padding
  "Chart option `:padding`: `{:x [lo hi] :y [lo hi]}`, fractions of the data span."
  [:map-of [:enum :x :y] schema/padding])

(defn explicit-padding
  "A chart's `:padding-explicit` stat: the sides of its `:padding` that are requests, not defaults - the chart's own
  `requests` (`{dim [lo hi]}`, a `nil` side - none: zero at a baseline bars or stems start from, room for marks below
  the data) with the `:padding` option over them (the user's sides are requests). The pipeline takes per side the
  largest request of the charts whose extent reaches that end of the merged extent, the largest padding only where
  none does. `nil` when there are none."
  [requests options]
  (not-empty (merge requests (:padding options))))

(defn rows
  "Vector of rows of plot `data` (`nil` -> `[]`).
  Throws `ex-info` with `{:key :data}` for anything but a sequence of maps or a map of equal-length sequences."
  [data]
  (cond
    (nil? data) []
    (map? data) (let [ks (keys data)
                      cols (map #(get data %) ks)]
                  (when-not (every? sequential? cols)
                    (schema/fail! "Columnar :data values must be sequences." {:key :data}))
                  (let [counts (set (map count cols))]
                    (when (> (count counts) 1)
                      (schema/fail! "Columnar :data sequences must have equal length." {:key :data :lengths counts})))
                  (if (empty? ks) [] (apply mapv (fn [& vs] (zipmap ks vs)) cols)))
    (sequential? data) (let [rs (vec data)]
                         (when-let [bad (first (remove map? rs))]
                           (schema/fail! "Row :data must contain only maps." {:key :data :value (type bad)}))
                         rs)
    :else (schema/fail! ":data must be a sequence of maps or a map of sequences." {:key :data :value (type data)})))

(defn option
  "Option `k` of chart `options`, `default` when missing or `nil` (a `nil` option means \"not set\")."
  [options k default]
  (let [v (get options k)] (if (nil? v) default v)))

(defn row-accessor
  "Function of a row for mapping accessor `acc` (keyword or function, or a categorical marker of one - see
  [[cljplot2.data.accessor]]) of dimension `k`."
  [k acc]
  (cond
    (keyword? acc) acc
    (fn? acc) acc
    (accessor/categorical? acc) (row-accessor k (accessor/unwrap acc))
    :else (schema/fail! "Mapping accessor must be a keyword or a function." {:key :mapping :value k})))

(defn temporal? [v] (instance? java.time.temporal.Temporal v))

(defn x-key
  "Number aligning an x value: a number as a double, a `java.time` value as epoch milliseconds (UTC; a `LocalTime` as
  milliseconds of the day - ordered as wadogo places it, on one day); `nil` otherwise."
  [x]
  (cond
    (number? x) (double x)
    (instance? LocalDate x) (* 86400000.0 (.toEpochDay ^LocalDate x))
    (instance? LocalDateTime x) (double (.toEpochMilli (.toInstant ^LocalDateTime x ZoneOffset/UTC)))
    (instance? Instant x) (double (.toEpochMilli ^Instant x))
    (instance? ZonedDateTime x) (double (.toEpochMilli (.toInstant ^ZonedDateTime x)))
    (instance? OffsetDateTime x) (double (.toEpochMilli (.toInstant ^OffsetDateTime x)))
    (instance? LocalTime x) (/ (double (.toNanoOfDay ^LocalTime x)) 1.0e6)))

(defn key->x
  "The x value of the [[x-key]] `key` in the type of the x value `like`: a double for a number; a `java.time` value
  rounded to the millisecond - a `LocalDate` as a `LocalDateTime` (keys fall between days), a `ZonedDateTime` /
  `OffsetDateTime` in the zone / offset of `like`, a `LocalTime` within the day. `(x-key (key->x k like))` is `k`
  for a whole number of milliseconds (in the day, for a `LocalTime`)."
  [key like]
  (let [k (double key)]
    (if (number? like)
      k
      (let [instant (Instant/ofEpochMilli (Math/round k))]
        (cond
          (or (instance? LocalDate like) (instance? LocalDateTime like)) (LocalDateTime/ofInstant instant ZoneOffset/UTC)
          (instance? Instant like) instant
          (instance? ZonedDateTime like) (ZonedDateTime/ofInstant instant (.getZone ^ZonedDateTime like))
          (instance? OffsetDateTime like) (OffsetDateTime/ofInstant instant (.getOffset ^OffsetDateTime like))
          (instance? LocalTime like) (LocalTime/ofNanoOfDay (* 1000000 (m/constrain (Math/round k) 0 86399999))))))))

(definterface OrderedLevel)
;; Marker of categorical level types with a natural order (they also implement `Comparable`), e.g. shingle / bin
;; intervals: valid plot values, sorted ascending wherever they are levels.

(defn ordered-level?
  "Is `v` a categorical level with a natural order - an instance of the marker [[OrderedLevel]] (shingle / bin
  intervals)? Such levels are valid values and sorted instead of kept in first-appearance order."
  [v]
  (instance? OrderedLevel v))

(defn as-coordinates
  "A function's point-valued result `r` (e.g. `[z c]` of a surface, `[u v]` of a vector field) as a vector of its
  values: any sequential value (a vector, list, `fastmath.vector` `Vec2` / `Vec3`) or a Java array; `nil` for
  anything else (a number, `nil`, a map, a string)."
  [r]
  (cond (sequential? r) (vec r)
        (and (some? r) (.isArray (class r))) (vec (seq r))))

(defn valid-value?
  "Can `v` be plotted: a finite number, a string, a keyword, a `java.time` value or a level with a natural order
  ([[ordered-level?]], e.g. an interval)."
  [v]
  (cond
    (number? v) (m/valid-double? (double v))
    (or (string? v) (keyword? v) (temporal? v)) true
    :else (ordered-level? v)))

(defn finite-numbers
  "Finite numbers among `vs` (as doubles) and the count of other values: `{:values [..] :dropped n}`."
  [vs]
  (let [ok (into [] (comp (filter #(and (number? %) (m/valid-double? (double %)))) (map double)) vs)]
    {:values ok :dropped (- (count vs) (count ok))}))

(defn- value-kind
  [v]
  (cond (number? v) :numeric (temporal? v) :temporal :else :categorical))

(defn- categorical-levels
  "Distinct categorical `vs`: sorted ascending when every one is an [[ordered-level?]] (and they compare), else in
  first-appearance order."
  [vs]
  (let [ds (vec (distinct vs))]
    (if (every? ordered-level? ds)
      (try (vec (sort ds)) (catch ClassCastException _ ds))
      ds)))

(defn extent
  "Extent of valid values `vs` of dimension `k` (`nil` when empty): categorical values distinct, in first-appearance
  order - sorted when they have a natural order (intervals, see [[categorical-levels]]). Mixed kinds throw
  `ex-info {:key :extents}`."
  [k vs]
  (when (seq vs)
    (let [kinds (set (map value-kind vs))]
      (when (> (count kinds) 1)
        (schema/fail! "Mapped values must be all numbers, all strings/keywords/intervals or all java.time values."
                 {:key :extents :in [:mapping k] :kinds kinds}))
      (case (first kinds)
        :numeric {:kind :numeric :min (double (reduce min vs)) :max (double (reduce max vs))}
        :temporal {:kind :temporal
                   :min (reduce #(if (neg? (compare %2 %1)) %2 %1) vs)
                   :max (reduce #(if (pos? (compare %2 %1)) %2 %1) vs)}
        :categorical {:kind :categorical :values (categorical-levels vs)}))))

(def ^:const max-levels
  "Most distinct numeric / temporal values [[value-extent]] keeps; with more, an ordinal scale needs an explicit domain."
  100)

(defn- capped-values
  "Extent `e` with distinct `vs` as `:values`; numeric / temporal extents lose `:values` beyond [[max-levels]]."
  [e vs]
  (if (= :categorical (:kind e))
    e
    (let [ds (into [] (comp (distinct) (take (inc max-levels))) vs)]
      (if (> (count ds) max-levels) (dissoc e :values) (assoc e :values ds)))))

(defn value-extent
  "[[extent]] of `vs` that also keeps the distinct values (`:values`, in order of first appearance) of numeric and
  temporal data, when there are at most [[max-levels]] of them."
  [k vs]
  (when-let [e (extent k vs)]
    (capped-values e vs)))

(defn sorted-levels
  "Distinct `values` sorted (numbers ascending, strings lexicographic, ...).
  Throws `ex-info {:key error-key :in [:mapping k]}` when they aren't mutually comparable."
  [k values error-key]
  (try (vec (sort (distinct values)))
       (catch ClassCastException _
         (schema/fail! "Values must be mutually comparable to be sorted (or give an explicit :domain)."
                  {:key error-key :in [:mapping k]}))))

(defn merge-extents
  "Merge extents of dimension `k` (`nil`s skipped); `:values` are kept (merged, distinct) when every extent has them
  (numeric / temporal: at most [[max-levels]]); `:sample`s (all values, for data-driven bins) are concatenated;
  `:ordered?` (levels in a given order, not to be sorted) stays when every extent has it.
  Mixed kinds throw `ex-info {:key :extents}`."
  [k extents]
  (let [es (remove nil? extents)]
    (when (seq es)
      (let [kinds (set (map :kind es))]
        (when (> (count kinds) 1)
          (schema/fail! "Charts map different kinds of values to one scale." {:key :extents :in [:mapping k] :kinds kinds}))
        (cond-> (case (first kinds)
          :categorical {:kind :categorical :values (categorical-levels (mapcat :values es))}
          :numeric {:kind :numeric :min (reduce min (map :min es)) :max (reduce max (map :max es))}
          :temporal {:kind :temporal
                     :min (reduce #(if (neg? (compare %2 %1)) %2 %1) (map :min es))
                     :max (reduce #(if (pos? (compare %2 %1)) %2 %1) (map :max es))})
          (every? :values es) (capped-values (mapcat :values es))
          (every? :ordered? es) (assoc :ordered? true)
          (some :sample es) (assoc :sample (into [] (mapcat :sample) es)))))))

(defn weight
  "Weight of a row's value `v` (edge weights, counts of flows and sequences): `1.0` for `nil` (an unweighted row),
  else a finite number >= 0; `ex-info {:key :value}` otherwise."
  ^double [v]
  (cond
    (nil? v) 1.0
    (and (number? v) (m/valid-double? (double v)) (>= (double v) 0.0)) (double v)
    :else (schema/fail! "A :value (weight) should be a finite number >= 0." {:key :value :value v})))
