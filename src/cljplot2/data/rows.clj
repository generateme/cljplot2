(ns cljplot2.data.rows
  "Plot data as rows: normalization, mapping accessors, valid values, extents and the mapping and data schemas.

  `:data` is a finite sequence of maps (rows) or a map of equal-length finite sequences (columns); both become a vector of rows. A mapping accessor is a keyword or a function of a row. An extent describes the values of one mapped dimension:

  - `{:kind :numeric :min a :max b}` (doubles)
  - `{:kind :temporal :min a :max b}` (`java.time` values)
  - `{:kind :categorical :values [v ...]}` (strings / keywords / ordered levels, distinct, in order of first appearance)

  Functions:

  - schemas and options: [[mapping-schema]], [[data-schema]], [[common-options]], [[legend-option]], [[position-padding]], [[axis-padding]], [[default-padding]], [[explicit-padding]], [[option]], [[options-with-defaults]]
  - rows and accessors: [[rows]], [[row-accessor]], [[as-coordinates]], [[weight]]
  - time: [[temporal?]], [[period-start]], [[x-key]], [[key->x]], [[->local-date-time]], [[local-date-time->like]]
  - scale domains (`[nil nil]` - no domain, `[lo nil]` / `[nil hi]` - partial): [[given-domain]], [[partial-domain?]], [[full-domain]], [[partial-domain-problem]], [[completed-domain]]
  - values and extents: [[valid-value?]], [[ordered-level?]] (the marker interface `OrderedLevel`), [[finite-numbers]], [[finite-point?]], [[extent]], [[value-extent]] (with the distinct values, at most [[max-levels]]), [[sorted-levels]], [[merge-extents]]

  Checks and helpers: [[check-mapped!]] (with the message [[no-valid-values]]), [[fail-mapping!]], [[sortable?]], [[invalid-count]], [[widened-range]], [[padding-stats]], [[zero-padding]]."
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.data.accessor :as accessor]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec2]
           [java.time Instant LocalDate LocalDateTime LocalTime OffsetDateTime Year YearMonth ZonedDateTime ZoneOffset]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private mapping-keys
  "Mapping keys of the built-in dimensions, each mapped to an accessor: `:z` - the third coordinate of 3D charts, `:set` / `:item` - set membership (Venn). `:hierarchy` (a vector of accessors) is mapped too, but isn't listed."
  [:x :y :z :ymin :ymax :xend :yend :open :high :low :close :color :shape :size :dash :pattern :group :facet :facet-col :facet-row :order :value :set :item])

(def ^:private mapping-key-docs
  "Plot-level meaning of each mapping key (each chart's mapping rows say how it reads them) and its section of the
  mapping table: `:position`, `:interval`, `:ohlc`, `:aesthetic`, `:groups-and-order`, `:facet`, `:value`."
  {:x {:section :position :description "x position (or the values of a strip chart with levels on y)"}
   :y {:section :position :description "y position (or the levels of a strip chart)"}
   :z {:section :position :description "third coordinate of 3D charts"}
   :ymin {:section :interval :description "lower end of an interval (ribbons, error bars)"}
   :ymax {:section :interval :description "upper end of an interval"}
   :xend {:section :interval :description "x of a segment's end"}
   :yend {:section :interval :description "y of a segment's end"}
   :open {:section :ohlc :description "opening value (candlesticks)"}
   :high {:section :ohlc :description "highest value (candlesticks)"}
   :low {:section :ohlc :description "lowest value (candlesticks)"}
   :close {:section :ohlc :description "closing value (candlesticks)"}
   :color {:section :aesthetic :description "colour aesthetic: groups, or values on a colour scale"}
   :shape {:section :aesthetic :description "point shape aesthetic"}
   :size {:section :aesthetic :description "size aesthetic"}
   :dash {:section :aesthetic :description "line dash aesthetic"}
   :pattern {:section :aesthetic :description "fill pattern aesthetic"}
   :group {:section :groups-and-order :description "groups without a visual aesthetic"}
   :facet {:section :facet :description "wrap facets: one panel per value"}
   :facet-col {:section :facet :description "grid facets: one column per value"}
   :facet-row {:section :facet :description "grid facets: one row per value"}
   :order {:section :groups-and-order :description "drawing order of points"}
   :value {:section :value :description "a value: cell values, weights, sizes of hierarchy leaves"}
   :set {:section :value :description "set name of a membership row (Venn)"}
   :item {:section :value :description "item of a membership row (Venn)"}})

(def mapping-schema
  "Schema of a plot or chart `:mapping`: accessors (keyword or function; `nil` - not mapped) of the built-in [[mapping-keys]], `:hierarchy` a non-empty vector of accessors. `:fill` is rejected (not an aesthetic, see the density / histogram `:fill` options). Open: user charts may read other keys."
  (-> [:map {:cljplot2/type-doc "a mapping: key -> accessor (a keyword or a function of a row)"}]
      (into (map (fn [k]
                   (let [{:keys [section description]} (mapping-key-docs k)]
                     (schema/nilable k {:description description :cljplot2/section section} schema/accessor))))
            mapping-keys)
      (conj (schema/nilable :hierarchy {:description "levels of a hierarchy, root first (treemap, partition, pack)"
                                        :cljplot2/section :value}
                            [:vector {:min 1} schema/accessor])
            ;; no :fill aesthetic: a mapped :fill would be silently ignored
            (schema/nilable :fill (schema/forbidden "isn't an aesthetic - fill areas by group with the chart option :fill :color")))))

(def data-schema
  "Schema of plot or chart `:data`: a sequence of rows (maps) or a map of columns (sequences); rows and columns are checked by [[rows]]."
  (schema/named-type ::data #(or (sequential? %) (map? %))
                     "should be a sequence of rows (maps) or a map of columns (sequences)"))

(def legend-option
  "Map entries (one) of the chart option `:legend` (shared group `:common`), for charts taking no other common option: `false` - the chart draws no key glyph in any legend (its aesthetics still take their scales and other charts' glyphs stay; a legend no chart draws a key into is dropped)."
  (schema/in-fragment
   :common
   [(schema/nilable :legend {:description "draw the chart's key glyph in the legends (false - the chart adds none, a legend no chart keys is dropped; its aesthetics still scale)"
                             :default true}
                    schema/bool)]))

(def common-options
  "Map entries of the options every chart takes (shared group `:common`): its own `:data` (see [[rows]]), `:mapping` (merged over the plot's) and `:legend` (see [[legend-option]])."
  (schema/in-fragment
   :common
   (into [(schema/nilable :data {:description "the chart's own data instead of the plot's"
                                 :cljplot2/default-doc "the plot's :data"}
                          data-schema)
          (schema/optional :mapping {:description "the chart's mapping, merged over the plot's mapping (a key set to nil removes it)"}
                           mapping-schema)]
         legend-option)))

(def axis-padding
  "Default padding of one position axis, `[lo hi]`: 5% of the data span on each side."
  [0.05 0.05])

(def default-padding
  "Default chart padding `{:x [lo hi] :y [lo hi]}`: [[axis-padding]] on both position axes (see [[position-padding]])."
  {:x axis-padding :y axis-padding})

(def zero-padding
  "Chart padding `{:x [lo hi] :y [lo hi]}` of none on both position axes: charts filling their extent exactly (cells, rasters, partitions)."
  {:x [0.0 0.0] :y [0.0 0.0]})

(def position-padding
  "Schema of the chart option `:padding`: `{:x [lo hi] :y [lo hi]}`, fractions of the data span."
  [:map-of [:enum :x :y] schema/padding])

(defn explicit-padding
  "Builds a chart's `:padding-explicit` stat: the sides of its padding that are requests, not defaults.

  The pipeline takes per side the largest request of the charts whose extent reaches that end of the merged extent, and the largest plain padding only where none does.

  Parameters:

  - `requests` (map): the chart's own requests, `{dim [lo hi]}`; a `nil` side - none (e.g. zero at a baseline bars or stems start from, room for marks below the data).
  - `options` (map): chart options; its `:padding` (the user's sides, all requests) is merged over `requests`.

  Returns the merged map, or `nil` when there are no requests."
  [requests options]
  (not-empty (merge requests (:padding options))))

(defn padding-stats
  "Gives a chart's padding stats: `:padding` and `:padding-explicit`.

  Parameters:

  - `defaults` (map): the chart's default padding `{dim [lo hi]}` (e.g. [[default-padding]]).
  - `requests` (map or `nil`): the chart's own padding requests, as [[explicit-padding]] takes them.
  - `options` (map): the chart's options - `:padding`.

  Returns `{:padding p :padding-explicit e}`: `p` - the `:padding` option merged over `defaults`, `e` - [[explicit-padding]] of `requests` and `options`."
  [defaults requests options]
  {:padding (merge defaults (:padding options))
   :padding-explicit (explicit-padding requests options)})

(defn rows
  "Normalizes plot `data` to a vector of rows.

  Parameters:

  - `data`: a sequence of maps (rows), a map of equal-length sequences (columns, zipped into rows), or `nil`.

  Returns a vector of maps; `[]` for `nil` or an empty column map.

  Throws `ex-info {:key :data}` for anything else: non-map rows, non-sequential columns, columns of different lengths."
  [data]
  (cond
    (nil? data) []
    (map? data) (let [ks (keys data)
                      cols (map #(get data %) ks)]
                  (when-not (every? sequential? cols)
                    (schema/fail! "Columnar :data values must be sequences." {:key :data}))
                  (let [counts (set (map count cols))]
                    (when (m/> (count counts) 1)
                      (schema/fail! "Columnar :data sequences must have equal length." {:key :data :lengths counts})))
                  (if (empty? ks) [] (apply mapv (fn [& vs] (zipmap ks vs)) cols)))
    (sequential? data) (let [rs (vec data)]
                         (when-let [bad (first (remove map? rs))]
                           (schema/fail! "Row :data must contain only maps." {:key :data :value (type bad)}))
                         rs)
    :else (schema/fail! ":data must be a sequence of maps or a map of sequences." {:key :data :value (type data)})))

(defn option
  "Reads an option, treating `nil` as not set.

  Parameters:

  - `options` (map): chart options.
  - `k` (keyword): the option.
  - `default`: the value when `k` is missing or `nil`.

  Returns the option's value, else `default`."
  [options k default]
  (let [v (get options k)] (if (nil? v) default v)))

(defn options-with-defaults
  "Reads several options at once, treating `nil` as not set (see [[option]]).

  Parameters:

  - `defaults` (map): option -> its default; its keys are the options read.
  - `options` (map): chart options.

  Returns `defaults` with every option of its keys that `options` sets to a non-`nil` value merged over it; other options left out."
  [defaults options]
  (merge defaults (into {} (remove (comp nil? val)) (select-keys options (keys defaults)))))

(defn period-start
  "Turns a `java.time.Year` or `YearMonth` into the `LocalDate` its period starts.

  Parameters:

  - `v`: any value.

  Returns January 1st of a `Year`, the 1st of the month of a `YearMonth`, other values as they are."
  [v]
  (cond
    (instance? Year v) (.atDay ^Year v 1)
    (instance? YearMonth v) (.atDay ^YearMonth v 1)
    :else v))

(defn fail-mapping!
  "Throws the error of a mapped dimension: `ex-info {:key :mapping :in [:mapping k] ...}`.

  Every mapping problem a chart or data function finds in its rows (no valid value, mixed kinds, a missing or unpaired position, an accessor of the wrong kind) has this shape, so `:in` names the dimension to fix.

  Parameters:

  - `message` (string): the error message.
  - `k` (keyword): the mapping dimension.
  - `details` (map): more error data - `:value` (the dimension's mapping entry or the offending value), `:rows` (the row count) ...; `{}` for none.

  Never returns."
  [message k details]
  (schema/fail! message (merge {:key :mapping :in [:mapping k]} details)))

(defn row-accessor
  "Gives the function of a row reading a mapped dimension.

  Values are read as they are: a `Year` stays a `Year` (a level, or a date the continuous scales place at its period's start, see [[period-start]]).

  Parameters:

  - `k` (keyword): the dimension (for the error).
  - `acc`: the mapping accessor - a keyword, a function of a row, or a categorical marker of one (see [[cljplot2.data.accessor]]).

  Returns the keyword or function (a marker unwrapped).

  Throws `ex-info {:key :mapping :in [:mapping k] :value acc}` (see [[fail-mapping!]]) for any other accessor."
  [k acc]
  (cond
    (keyword? acc) acc
    (fn? acc) acc
    (accessor/categorical? acc) (row-accessor k (accessor/unwrap acc))
    :else (fail-mapping! "Mapping accessor must be a keyword or a function." k {:value acc})))

(defn temporal?
  "Tests whether a value is a `java.time` value (a `java.time.temporal.Temporal`).

  `Year` and `YearMonth` are temporal; see [[period-start]].

  Parameters:

  - `v`: any value.

  Returns true for a temporal value, else false."
  [v]
  (instance? java.time.temporal.Temporal v))

(defn sortable?
  "Tests whether values can be ordered along a continuous axis: all numbers or all `java.time` values.

  Parameters:

  - `xs` (sequence): the values; empty - vacuously sortable.

  Returns a truthy value when they can be ordered."
  [xs]
  (or (every? number? xs) (every? temporal? xs)))

(defn x-key
  "Gives the number aligning an x value across rows and charts.

  Parameters:

  - `x`: a number or a `java.time` value.

  Returns a double: a number as it is; a `java.time` value as epoch milliseconds in UTC - a `LocalTime` as milliseconds of the day (ordered as wadogo places it, on one day), a `Year` / `YearMonth` at its period's start ([[period-start]]). `nil` for other values.

  See also [[key->x]] (the inverse)."
  [x]
  (cond
    (number? x) (double x)
    (or (instance? Year x) (instance? YearMonth x)) (x-key (period-start x))
    (instance? LocalDate x) (m/* 86400000.0 (.toEpochDay ^LocalDate x))
    (instance? LocalDateTime x) (double (.toEpochMilli (.toInstant ^LocalDateTime x ZoneOffset/UTC)))
    (instance? Instant x) (double (.toEpochMilli ^Instant x))
    (instance? ZonedDateTime x) (double (.toEpochMilli (.toInstant ^ZonedDateTime x)))
    (instance? OffsetDateTime x) (double (.toEpochMilli (.toInstant ^OffsetDateTime x)))
    (instance? LocalTime x) (m// (.toNanoOfDay ^LocalTime x) 1.0e6)))

(defn key->x
  "Turns an [[x-key]] back into an x value of the type of another one.

  Parameters:

  - `k` (double): the x key.
  - `like`: an x value whose type the result takes.

  Returns a double for a number `like`; a `java.time` value rounded to the millisecond otherwise - a `LocalDate` (or `Year`, `YearMonth`) as a `LocalDateTime` (keys fall between days), an `Instant`, a `ZonedDateTime` / `OffsetDateTime` in the zone / offset of `like`, a `LocalTime` within the day. `nil` for other types. `(x-key (key->x k like))` is `k` for a whole number of milliseconds (in the day, for a `LocalTime`)."
  [^double k like]
  (if (number? like)
    k
    (let [millis (m/round k)
          instant (Instant/ofEpochMilli millis)]
      (cond
        (or (instance? LocalDate like) (instance? LocalDateTime like) (instance? Year like) (instance? YearMonth like))
        (LocalDateTime/ofInstant instant ZoneOffset/UTC)
        (instance? Instant like) instant
        (instance? ZonedDateTime like) (ZonedDateTime/ofInstant instant (.getZone ^ZonedDateTime like))
        (instance? OffsetDateTime like) (OffsetDateTime/ofInstant instant (.getOffset ^OffsetDateTime like))
        (instance? LocalTime like) (LocalTime/ofNanoOfDay (m/* 1000000 (long (m/constrain millis 0 86399999))))))))

(defn ->local-date-time
  "Turns a `java.time` value into the `LocalDateTime` wadogo `:datetime` scales tick in.

  Parameters:

  - `t`: a `java.time` value.

  Returns a `LocalDateTime`: a date at its start, a date-time as it is, a time on 1970-01-01, an instant in UTC, a zoned or offset date-time as it reads in its own zone / offset, a `Year` / `YearMonth` at its period's start. `nil` for other values.

  See also [[local-date-time->like]] (the inverse)."
  ^LocalDateTime [t]
  (cond
    (or (instance? Year t) (instance? YearMonth t)) (->local-date-time (period-start t))
    (instance? LocalDate t) (.atStartOfDay ^LocalDate t)
    (instance? LocalDateTime t) t
    (instance? LocalTime t) (LocalDateTime/of LocalDate/EPOCH ^LocalTime t)
    (instance? Instant t) (LocalDateTime/ofInstant ^Instant t ZoneOffset/UTC)
    (instance? ZonedDateTime t) (.toLocalDateTime ^ZonedDateTime t)
    (instance? OffsetDateTime t) (.toLocalDateTime ^OffsetDateTime t)))

(defn local-date-time->like
  "Turns a `LocalDateTime` into a `java.time` value of the type of another one - the inverse of [[->local-date-time]].

  Parameters:

  - `ldt` (`LocalDateTime`): the date-time.
  - `like`: a `java.time` value whose type the result takes.

  Returns a `LocalDateTime` - `ldt` itself, a date - its day, a time - its time of day, a `Year` / `YearMonth` - the one holding it, an instant in UTC, a zoned / offset date-time in the zone / offset of `like`; `nil` for other types."
  [^LocalDateTime ldt like]
  (cond
    (instance? Year like) (Year/from ldt)
    (instance? YearMonth like) (YearMonth/from ldt)
    (instance? LocalDate like) (.toLocalDate ldt)
    (instance? LocalDateTime like) ldt
    (instance? LocalTime like) (.toLocalTime ldt)
    (instance? Instant like) (.toInstant ldt ZoneOffset/UTC)
    (instance? ZonedDateTime like) (.atZone ldt (.getZone ^ZonedDateTime like))
    (instance? OffsetDateTime like) (.atOffset ldt (.getOffset ^OffsetDateTime like))))

(defn given-domain
  "Gives an explicit scale domain unless it leaves both ends to the data.

  Every reader of a scale's `:domain` (position and aesthetic scales, facet checks, charts sampling over the plot's domain) goes through this function, [[partial-domain?]] and [[full-domain]], so `[nil nil]` and partial domains read the same everywhere.

  Parameters:

  - `domain`: a scale spec's `:domain`.

  Returns `domain` when it is sequential and not `[nil nil]` - a full or a partial domain; else `nil` (`nil` and `[nil nil]` - no domain; a non-sequential value is rejected by the scale schemas)."
  [domain]
  (when (and (sequential? domain) (not (and (m/== 2 (count domain)) (every? nil? domain))))
    domain))

(defn partial-domain?
  "Tests whether an explicit scale domain fixes one end and leaves the other to the data.

  Parameters:

  - `domain`: a scale spec's `:domain`.

  Returns a truthy value for a sequential domain with a `nil` among non-`nil` values (`[lo nil]`, `[nil hi]`; another shape is invalid, see [[partial-domain-problem]]), else a falsy one.

  See also [[completed-domain]]."
  [domain]
  (and (sequential? domain) (some nil? domain) (some some? domain)))

(defn full-domain
  "Gives an explicit scale domain that fixes all its values.

  Parameters:

  - `domain`: a scale spec's `:domain`.

  Returns `domain` when it is a [[given-domain]] without `nil` values, else `nil` (no domain, or a partial one)."
  [domain]
  (when-let [given (given-domain domain)]
    (when (not-any? nil? given) given)))

(defn partial-domain-problem
  "Explains why the shape of a partial scale domain is invalid.

  Parameters:

  - `domain`: a [[partial-domain?]] domain.

  Returns the error message, or `nil` for two values (`[lo nil]` / `[nil hi]`); the scales check the given end's type."
  [domain]
  (when-not (m/== 2 (count domain)) "partial domain should be [lo nil] or [nil hi]"))

(defn completed-domain
  "Gives a partial scale domain with its open end taken from the data.

  Parameters:

  - `k` (keyword): the scale key, for errors.
  - `explicit`: the partial domain, `[lo nil]` or `[nil hi]` (see [[partial-domain?]]).
  - `data-range`: the data's `[lo hi]` (numbers or `java.time` values), `nil` without continuous data.

  Returns `[lo hi]`: the given end kept, the other the data's.

  Throws `ex-info {:key :domain :in [:scales k :domain]}` without a data range, and when the data lie entirely beyond the given end (compared by [[x-key]]: the domain would not increase)."
  [k explicit data-range]
  (let [fail! #(schema/fail! (str "Scale partial :domain " %) {:key :domain :in [:scales k :domain] :value explicit})
        _ (when-not data-range
            (fail! "[lo nil] / [nil hi] takes its open end from continuous data; there is none."))
        [lo hi] explicit
        [data-lo data-hi] data-range
        domain [(if (some? lo) lo data-lo) (if (some? hi) hi data-hi)]
        [a b] (map x-key domain)]
    (if (and a b (m/< (double a) (double b)))
      domain
      (fail! (str "leaves no data on its open side: the data end at " (if (some? lo) data-hi data-lo)
                  " (to reverse the scale, give :reverse true).")))))

(definterface OrderedLevel)
;; Marker of categorical level types with a natural order (they also implement `Comparable`), e.g. shingle / bin
;; intervals: valid plot values, sorted ascending wherever they are levels.

(defn ordered-level?
  "Tests whether a value is a categorical level with a natural order.

  Such levels (shingle / bin intervals, instances of the marker interface `OrderedLevel`) are valid values, sorted instead of kept in first-appearance order.

  Parameters:

  - `v`: any value.

  Returns true for an `OrderedLevel`, else false."
  [v]
  (instance? OrderedLevel v))

(defn as-coordinates
  "Reads a function's point-valued result as a vector of its values.

  Parameters:

  - `r`: the result, e.g. `[z c]` of a surface, `[u v]` of a vector field.

  Returns a vector of the values of any sequential value (a vector, list, `fastmath.vector` `Vec2` / `Vec3`) or Java array; `nil` for anything else (a number, `nil`, a map, a string)."
  [r]
  (cond (sequential? r) (vec r)
        (and (some? r) (.isArray (class r))) (vec (seq r))))

(defn valid-value?
  "Tests whether a value can be plotted.

  Scale-free: see [[cljplot2.scale.aesthetic/valid-value?]] for what an aesthetic scale can map (e.g. any colour, `nil` with an NA level).

  Parameters:

  - `v`: any value.

  Returns true for a finite number, a string, a keyword, a `java.time` value or a level with a natural order ([[ordered-level?]], e.g. an interval); false for NaN, infinite numbers, `nil` and other values."
  [v]
  (cond
    (number? v) (schema/finite? v)
    (or (string? v) (keyword? v) (temporal? v)) true
    :else (ordered-level? v)))

(defn invalid-count
  "Counts the rows whose value of an accessor isn't valid.

  Parameters:

  - `value-of` (function): row -> value.
  - `data-rows` (sequence): the rows.

  Returns the number of rows whose value fails [[valid-value?]] (`nil`, `NaN`, infinite numbers, unordered values)."
  ^long [value-of data-rows]
  (count (remove #(valid-value? (value-of %)) data-rows)))

(defn widened-range
  "Gives a numeric range with a positive width.

  Parameters:

  - `[lo hi]` (numbers): the range ends, `lo` at most `hi`.

  Returns `[lo hi]` when `lo` is below `hi`; a single value widened to `[lo - 0.5, hi + 0.5]`."
  [[^double lo ^double hi]]
  (if (m/< lo hi) [lo hi] [(m/- lo 0.5) (m/+ hi 0.5)]))

(def no-valid-values
  "Error message of a mapped dimension without one valid value (see [[check-mapped!]])."
  "No valid values for a mapped dimension - check the mapping.")

(defn check-mapped!
  "Checks that every mapped dimension has at least one valid value among the rows.

  Parameters:

  - `chart-rows` (sequence): the chart's rows (all panels and groups).
  - `accessors` (sequence): `[dimension accessor]` pairs, an accessor a function of a row.
  - `valid?` (function, optional): of a dimension and a value, default [[valid-value?]] of the value; aesthetic dimensions usually pass [[cljplot2.scale.aesthetic/valid-value?]] with their scale.

  Returns `nil`. Empty `chart-rows` pass. Throws `ex-info {:key :mapping :in [:mapping dimension] :rows n}` ([[no-valid-values]], see [[fail-mapping!]]) for the first dimension without a valid value."
  ([chart-rows accessors] (check-mapped! chart-rows accessors (fn [_ v] (valid-value? v))))
  ([chart-rows accessors valid?]
   (when (seq chart-rows)
     (doseq [[k f] accessors]
       (when (not-any? #(valid? k (f %)) chart-rows)
         (fail-mapping! no-valid-values k {:rows (count chart-rows)}))))))

(defn finite-point?
  "Tests whether a point can be drawn: both coordinates are finite numbers.

  Parameters:

  - `p`: a `Vec2` or an `[x y]` vector (e.g. a projected pixel point).

  Returns a truthy value for finite coordinates; false for a `NaN` or infinite coordinate, a non-number or `nil` (as `p` or a coordinate).

  See also [[valid-value?]] (one data value)."
  [p]
  (if (instance? Vec2 p)
    (and (m/valid-double? (.x ^Vec2 p)) (m/valid-double? (.y ^Vec2 p)))
    (let [[x y] p]
      (and (schema/finite? x) (schema/finite? y)))))

(defn finite-numbers
  "Picks the finite numbers of a collection of values.

  Parameters:

  - `vs` (collection): any values.

  Returns `{:values [..] :dropped n}`: the finite numbers as doubles, in order, and the count of the other values."
  [vs]
  (let [ok (into [] (comp (filter schema/finite?) (map double)) vs)]
    {:values ok :dropped (m/- (count vs) (count ok))}))

(defn- value-kind
  "Gives the kind of extent a valid value belongs to.

  Parameters:

  - `v`: a valid value (see [[valid-value?]]).

  Returns `:numeric` for a number, `:temporal` for a `java.time` value, else `:categorical`."
  [v]
  (cond (number? v) :numeric (temporal? v) :temporal :else :categorical))

(defn- categorical-levels
  "Gives the distinct levels of categorical values.

  Parameters:

  - `vs` (collection): categorical values.

  Returns a vector of the distinct values: sorted ascending when every one is an [[ordered-level?]] and they compare, else in first-appearance order."
  [vs]
  (let [ds (vec (distinct vs))]
    (if (every? ordered-level? ds)
      (try (vec (sort ds)) (catch ClassCastException _ ds))
      ds)))

(defn- temporal-min
  "Gives the earliest of `java.time` values.

  Parameters:

  - `ts` (non-empty sequence): mutually comparable `java.time` values.

  Returns the first of the smallest values.

  See also [[temporal-max]]."
  [ts]
  (reduce (fn [a b] (if (m/neg? (compare b a)) b a)) ts))

(defn- temporal-max
  "Gives the latest of `java.time` values.

  Parameters:

  - `ts` (non-empty sequence): mutually comparable `java.time` values.

  Returns the first of the largest values.

  See also [[temporal-min]]."
  [ts]
  (reduce (fn [a b] (if (m/pos? (compare b a)) b a)) ts))

(defn extent
  "Gives the extent of the valid values of a mapped dimension (see the namespace doc).

  Parameters:

  - `k` (keyword): the dimension (for the error).
  - `vs` (collection): valid values (see [[valid-value?]]).

  Returns `{:kind :numeric :min :max}` (doubles), `{:kind :temporal :min :max}` or `{:kind :categorical :values [..]}` - distinct, in first-appearance order, sorted when they have a natural order (intervals, see [[categorical-levels]]); `nil` for no values.

  Throws `ex-info {:key :mapping :in [:mapping k]}` for values of mixed kinds.

  See also [[value-extent]], [[merge-extents]]."
  [k vs]
  (when (seq vs)
    (let [kinds (set (map value-kind vs))]
      (when (m/> (count kinds) 1)
        (schema/fail! "Mapped values must be all numbers, all strings/keywords/intervals or all java.time values."
                 {:key :mapping :in [:mapping k] :kinds kinds}))
      (case (first kinds)
        :numeric {:kind :numeric :min (v/mn vs) :max (v/mx vs)}
        :temporal {:kind :temporal
                   :min (temporal-min vs)
                   :max (temporal-max vs)}
        :categorical {:kind :categorical :values (categorical-levels vs)}))))

(def ^{:const true :tag 'long} max-levels
  "Most distinct numeric / temporal values [[value-extent]] keeps; with more, an ordinal scale needs an explicit domain."
  100)

(defn- capped-values
  "Adds the distinct values to a numeric or temporal extent, when there are few enough.

  Parameters:

  - `e` (map): an extent (see [[extent]]).
  - `vs` (collection): its values.

  Returns `e` with `:values` - the distinct `vs` in first-appearance order - or without `:values` when there are more than [[max-levels]]; a categorical extent as it is."
  [e vs]
  (if (= :categorical (:kind e))
    e
    (let [ds (into [] (comp (distinct) (take (m/long-inc max-levels))) vs)]
      (if (m/> (count ds) max-levels) (dissoc e :values) (assoc e :values ds)))))

(defn value-extent
  "Gives the [[extent]] of values, keeping the distinct numeric or temporal values too - for scales needing the levels (ordinal colour, shape, size).

  Parameters:

  - `k` (keyword): the dimension (for the error).
  - `vs` (collection): valid values.

  Returns the extent with `:values` - the distinct values in first-appearance order - when numeric / temporal data has at most [[max-levels]] of them; `nil` for no values.

  Throws `ex-info {:key :mapping}` for values of mixed kinds."
  [k vs]
  (when-let [e (extent k vs)]
    (capped-values e vs)))

(defn sorted-levels
  "Sorts the distinct values of a dimension into levels.

  Parameters:

  - `k` (keyword): the dimension (for the error).
  - `values` (collection): the values.
  - `error-key` (keyword): `:key` of the error.

  Returns a vector of the distinct values sorted (numbers ascending, strings lexicographic, ...).

  Throws `ex-info {:key error-key :in [:mapping k]}` when they aren't mutually comparable."
  [k values error-key]
  (try (vec (sort (distinct values)))
       (catch ClassCastException _
         (schema/fail! "Values must be mutually comparable to be sorted (or give an explicit :domain)."
                  {:key error-key :in [:mapping k]}))))

(defn merge-extents
  "Merges the extents of one dimension from several charts or panels.

  Parameters:

  - `k` (keyword): the dimension (for the error).
  - `extents` (collection): extents (see [[extent]]); `nil`s skipped.

  Returns the merged extent: numeric / temporal - the smallest `:min` and the largest `:max`; categorical - the distinct levels (sorted when ordered levels). `:values` are kept (merged, distinct) when every extent has them (numeric / temporal: at most [[max-levels]]); `:sample`s (all values, for data-driven bins) are concatenated; `:ordered` (levels in a given order, not to be sorted) stays when every extent has it. `nil` without extents.

  Throws `ex-info {:key :mapping :in [:mapping k]}` for extents of mixed kinds."
  [k extents]
  (let [es (remove nil? extents)]
    (when (seq es)
      (let [kinds (set (map :kind es))]
        (when (m/> (count kinds) 1)
          (schema/fail! "Charts map different kinds of values to one scale." {:key :mapping :in [:mapping k] :kinds kinds}))
        (cond-> (case (first kinds)
          :categorical {:kind :categorical :values (categorical-levels (mapcat :values es))}
          :numeric {:kind :numeric :min (v/mn (map :min es)) :max (v/mx (map :max es))}
          :temporal {:kind :temporal
                     :min (temporal-min (map :min es))
                     :max (temporal-max (map :max es))})
          (every? :values es) (capped-values (mapcat :values es))
          (every? :ordered es) (assoc :ordered true)
          (some :sample es) (assoc :sample (into [] (mapcat :sample) es)))))))

(defn weight
  "Reads the weight of a row's value (edge weights, counts of flows and sequences).

  Parameters:

  - `v`: the mapped `:value` of a row; `nil` - an unweighted row.

  Returns `1.0` for `nil`, else `v` as a double.

  Throws `ex-info {:key :value}` unless `v` is `nil` or a finite number >= 0."
  ^double [v]
  (cond
    (nil? v) 1.0
    (and (schema/finite? v) (m/not-neg? (double v))) v
    :else (schema/fail! "A :value (weight) should be a finite number >= 0." {:key :value :value v})))
