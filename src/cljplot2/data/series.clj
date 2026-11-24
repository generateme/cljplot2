(ns cljplot2.data.series
  "Time series rows: autocorrelations with their significance bands ([[acf-rows]]), lagged pairs ([[lag-rows]]) and
  horizon bands ([[horizon-rows]]), for ordinary charts (see [[cljplot2.series-plots/acf]],
  [[cljplot2.series-plots/horizon]]).

  A series is a sequence of numbers in time order (fastmath `acf-ci` / `pacf-ci`).

  Functions: [[acf-rows]], [[lag-rows]], [[horizon-rows]] - the rows; [[default-lags]] - R's default lag count; [[horizon-bands]] - the band labels of a horizon."
  (:require [fastmath.core :as m]
            [fastmath.stats :as stats]
            [fastmath.vector :as v]
            [cljplot2.data.interpolation :as fit]
            [cljplot2.data.rows :as data]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- finite-series!
  "Checks a series and gives it as doubles.

  Parameters:

  - `values` (sequence of numbers): the series.

  Returns a vector of doubles.

  Throws `ex-info {:key :values}` for a non-finite value or fewer than 2 values."
  [values]
  (let [vs (vec values)]
    (when-not (every? schema/finite? vs)
      (schema/fail! "A series needs finite numbers." {:key :values :value (first (remove schema/finite? vs))}))
    (when (m/< (count vs) 2)
      (schema/fail! "A series needs at least 2 values." {:key :values :value (count vs)}))
    (mapv double vs)))

(defn default-lags
  "Gives the default number of lags of a series (R `acf`).

  Parameters:

  - `n` (long): the count of values, at least 2.

  Returns `min(floor(10 log10 n), n - 1)`."
  ^long [^long n]
  (m/min (long (m/floor (m/* 10.0 (m/log10 n)))) (m/long-dec n)))

(defn- lags!
  "Gives the largest lag of autocorrelation rows.

  Parameters:

  - `lags`: the `:lags` option, `nil` for the default ([[default-lags]]).
  - `n` (long): the count of values.

  Returns the lag as a long.

  Throws `ex-info {:key :lags}` unless `lags` is `nil` or an integer from 1 to `n - 1`."
  ^long [lags ^long n]
  (if (nil? lags)
    (default-lags n)
    (if (and (integer? lags) (m/<= 1 (long lags) (m/long-dec n)))
      (long lags)
      (schema/fail! (str "Series :lags should be an integer from 1 to " (m/long-dec n) ".") {:key :lags :value lags}))))

;; options

(def ^:private row-key [:or :keyword :string])

(def acf-rows-entries
  "Option entries of [[acf-rows]] (a shared group, `:series/acf-rows`; [[cljplot2.series-plots/acf]] forwards
  `:lags` and `:level`)."
  (schema/in-fragment
   :series/acf-rows
   [(schema/nilable :lags {:description "the largest lag, from 1 to n - 1 for n values"
                           :cljplot2/default-doc "floor(10 log10 n), at most n - 1 (R acf)"}
                    schema/pos-int)
    (schema/nilable :level {:description "confidence level of the band (0.95 - a 95% band)" :default 0.95}
                    schema/open-fraction)
    (schema/nilable :partial {:description "the partial autocorrelation (lags from 1, a constant band) instead of the autocorrelation"
                               :default false}
                    schema/bool)]))

(def acf-rows-options
  "Options schema of [[acf-rows]]."
  (into [:map {:closed true}] acf-rows-entries))

(defn acf-rows
  "Computes the rows of the autocorrelation function of a series, with its significance band.

  Parameters:

  - `values` (sequence of numbers): the series (see the namespace doc).
  - `options` (map, optional, see [[acf-rows-options]]):
    - `:lags` - the largest lag, from 1 to n - 1 (default [[default-lags]]).
    - `:level` - the confidence level of the band (default 0.95).
    - `:partial` - the partial autocorrelation instead (default `false`).

  Returns a vector of rows `{:lag :value :lo :hi :significant}`: lags 0 to `:lags`, the band `[:lo :hi]` - Bartlett's per lag (fastmath `acf-ci` `:cis`), `:significant` - the value outside the band (lag 0, always 1, isn't). With `:partial true`: lags 1 to `:lags`, the constant band `±ci` (`pacf-ci`).

  Throws `ex-info {:key :values}` for a non-finite value, fewer than 2 values or a constant series, `{:key :lags}` for lags outside 1 to n - 1; `{:key k}` for an invalid option `k` (checked first)."
  {:cljplot2/api true :cljplot2/options acf-rows-options}
  ([values] (acf-rows values {}))
  ([values {partial? :partial lags-option :lags :as options}]
   (schema/check-options! acf-rows-options options)
   (let [alpha (m/- 1.0 (double (data/option options :level 0.95)))
         vs (finite-series! values)
         lags (lags! lags-option (count vs))
         row (fn [^long lag ^double value ^double band]
               {:lag lag :value value :lo (m/- band) :hi band :significant (and (m/pos? lag) (m/> (m/abs value) band))})
         rows (if partial?
                (let [{:keys [ci pacf]} (stats/pacf-ci vs lags alpha)]
                  (mapv #(row %1 %2 ci) (range 1 (m/long-inc lags)) (rest pacf)))
                (let [{:keys [acf cis]} (stats/acf-ci vs lags alpha)]
                  (mapv row (range (m/long-inc lags)) acf cis)))]
     (when-not (every? #(m/valid-double? (double (:value %))) rows)
       (schema/fail! "A constant series has no autocorrelation." {:key :values}))
     rows)))

(defn lag-rows
  "Pairs every value of a series with the value a lag later (lag plots).

  Parameters:

  - `values` (sequence): the series; values as given (a scatter drops invalid ones).
  - `k` (positive integer): the lag.

  Returns a vector of `n - k` rows `{:t :x :y}` - `y_t` as `:x`, `y_(t+k)` as `:y`, `t` from 0; none for `k >= n`.

  Throws `ex-info {:key :lag}` unless `k` is a positive integer."
  {:cljplot2/api true}
  [values k]
  (when-not (and (integer? k) (m/pos? (long k)))
    (schema/fail! "Lag should be a positive integer." {:key :lag :value k}))
  (let [vs (vec values)
        lag (long k)]
    (mapv (fn [^long t] {:t t :x (vs t) :y (vs (m/long-add t lag))}) (range (m/max 0 (m/long-sub (count vs) lag))))))

(defn- band-levels
  "Gives the values where the bands of a horizon change.

  Parameters:

  - `n` (long): the count of bands per sign.
  - `h` (double): the band height.

  Returns the sorted distinct values `±k h` for `k` 0..n."
  [^long n ^double h]
  (sort (distinct (for [^long k (range (m/long-inc n))
                        ^double s [1.0 -1.0]]
                    (m/* s (double k) h)))))

(defn- crossings
  "Gives the points where a segment crosses levels.

  Parameters:

  - `from`, `to` (`[x y]`, numbers): the segment's ends.
  - `levels` (sequence of numbers): the levels.

  Returns the points `[x y]` where the segment crosses one of `levels` strictly between its ends, in order along it."
  [[^double x0 ^double y0] [^double x1 ^double y1] levels]
  (let [lo (m/min y0 y1)
        hi (m/max y0 y1)]
    (->> levels
         (filter (fn [^double l] (m/< lo l hi)))
         (map (fn [^double l] (let [t (m// (m/- l y0) (m/- y1 y0))] [t [(m/+ x0 (m/* t (m/- x1 x0))) l]])))
         (sort-by first)
         (map second))))

(defn- with-crossings
  "Inserts the band-edge crossings into the points of one series.

  Parameters:

  - `points` (sequence of `[x y]`): x increasing, `y` `nil` where the series breaks.
  - `levels` (sequence of numbers): the band edges, see [[band-levels]].

  Returns a vector of the points with the crossings of every segment between two valid points inserted."
  [points levels]
  (if (empty? points)
    []
    (into [(first points)]
          (mapcat (fn [[p q]] (if (and (second p) (second q)) (concat (crossings p q levels) [q]) [q])))
          (partition 2 1 points))))

(defn- band-height-of
  "Computes the height of one horizon band at a value.

  Parameters:

  - `band` (long): the band, 1..n; negative - the mirrored negative bands.
  - `h` (double): the band height.
  - `y` (number or `nil`): the value.

  Returns `clamp(±y - (|band| - 1) h, 0, h)` (`+` for positive bands), `nil` for a `nil` `y`."
  [^long band ^double h y]
  (when (some? y)
    (let [v (m/* (if (m/pos? band) 1.0 -1.0) (double y))]
      (m/constrain (m/- v (m/* (double (m/long-dec (m/abs band))) h)) 0.0 h))))

(defn horizon-bands
  "Gives the band labels of a horizon in drawing order.

  Parameters:

  - `n` (long): the count of bands per sign.

  Returns `[1 .. n -1 .. -n]`: positive bands, then negative (mirrored) ones - each band under the next, darker one."
  {:cljplot2/api true}
  [^long n]
  (let [labels (range 1 (m/long-inc n))]
    (vec (concat labels (map (fn [^long b] (m/- b)) labels)))))

(defn- resampled-run
  "Resamples a run of valid points on a fitted curve.

  Parameters:

  - `fit-of` (function): `(fn [xs ys] f)`, see [[cljplot2.data.interpolation/fit-fn]].
  - `run` (vector of `[x y]`): valid points, x strictly increasing.
  - `steps` (long): points per gap between consecutive x.

  Returns the points on the curve through the run, `steps` per gap, the data points kept; the run as is when it is too short for the method."
  [fit-of run ^long steps]
  (if-let [f (when (m/> (count run) 1) (fit/fitted fit-of (mapv first run) (mapv second run)))]
    (conj (vec (for [[[x0 y0] [x1]] (partition 2 1 run)
                     ^long i (range steps)]
                 (if (m/zero? i)
                   [x0 y0]
                   (let [x (m/+ (double x0) (m/* (m/- (double x1) (double x0)) (m// (double i) (double steps))))]
                     [x (double (f x))]))))
          (peek run))
    run))

(defn- interpolated-points
  "Resamples every run of valid points of one series ([[resampled-run]]).

  Parameters:

  - `points` (sequence of `[x y]`): x increasing, `y` `nil` where the series breaks.
  - `fit-of` (function): `(fn [xs ys] f)`.
  - `steps` (long): points per gap between consecutive x.

  Returns a vector of points: runs of valid points resampled, breaks kept.

  Throws `ex-info {:key :interpolator}` for a repeated x inside a run."
  [points fit-of steps]
  (into [] (mapcat (fn [run]
                     (cond
                       (nil? (second (first run))) run
                       (apply < (map first run)) (resampled-run fit-of (vec run) steps)
                       :else (schema/fail! "Horizon :interpolator needs distinct x values in every run."
                                           {:key :interpolator :value (mapv first run)}))))
        (partition-by #(nil? (second %)) points)))

(def horizon-rows-entries
  "Option entries of [[horizon-rows]] (a shared group, `:series/horizon-rows`; [[cljplot2.series-plots/horizon]]
  forwards them all)."
  (schema/in-fragment
   :series/horizon-rows
   [(schema/optional :x {:description "row key of the x values (numbers)" :default :x} row-key)
    (schema/optional :y {:description "row key of the y values (finite numbers, nil breaks the bands)" :default :y} row-key)
    (schema/nilable :series {:description "row key of the series, one horizon per value in first-appearance order"
                             :cljplot2/default-doc "none - one series"}
                    row-key)
    (schema/optional :bands {:description "number of bands each sign of the values is folded into" :default 2}
                     schema/pos-int)
    (schema/nilable :band-height {:description "height of one band in y units"
                                  :cljplot2/default-doc "the largest |y| of all series / :bands (1 when it is 0)"}
                    schema/finite-pos)
    (schema/nilable :interpolator {:description "a curve the valid points of every run are resampled on before folding"
                                   :cljplot2/default-doc "none - straight segments"}
                    schema/interpolator)
    (schema/optional :steps-per-gap {:description "resampled points per gap between consecutive x (with :interpolator)"
                                     :default 10}
                     schema/pos-int)]))

(def horizon-rows-options
  "Options schema of [[horizon-rows]]."
  (into [:map {:closed true}] horizon-rows-entries))

(defn horizon-rows
  "Computes the rows of a horizon chart: series folded into bands of equal height.

  Parameters:

  - `rows` (sequence of maps): the data.
  - `options` (map, optional, see [[horizon-rows-options]]):
    - `:x`, `:y` - row keys of the x values (numbers) and y values (finite numbers or `nil`), default `:x` / `:y`.
    - `:series` - row key of the series, one horizon per value in first-appearance order; default one series `nil`.
    - `:bands` - the count of bands per sign (default 2).
    - `:band-height` - the height of one band in y units; default the largest `|y|` of all series / `:bands` (1 when it is 0).
    - `:interpolator` - a fastmath 1D method of [[cljplot2.schema/interpolation-methods]] (e.g. `:monotone`) or a function `(fn [xs ys] f)`; every run of valid points is first resampled on that curve, so all bands follow one curve (a run too short for the method stays straight); default none - straight segments.
    - `:steps-per-gap` - resampled points per gap between consecutive x, the data points kept (default 10).

  Returns `{:rows [{:x :y :band :series} ...] :band-height h}`: band `k` (1..n) is `clamp(y - (k-1) h, 0, h)`, band `-k` the same of `-y` (negatives mirrored upward); every series' points sorted by x with the crossings of band edges inserted, so linear areas are exact; a `nil` `y` breaks the bands (a row with `:y nil` per band); `:series` only with the option.

  Throws `ex-info` for an invalid option (checked first: `{:key :bands}`, `{:key :band-height}`, `{:key :steps-per-gap}` ...), `{:key :x}` for a non-numeric x, `{:key :y}` for a `y` neither `nil` nor a finite number, `{:key :interpolator}` for an unknown method or a repeated x in a run.

  See also [[horizon-bands]]."
  {:cljplot2/api true :cljplot2/options horizon-rows-options}
  ([rows] (horizon-rows rows {}))
  ([rows {:keys [x y series ^long bands band-height interpolator steps-per-gap] :or {x :x y :y bands 2 steps-per-gap 10} :as options}]
   (schema/check-options! horizon-rows-options options)
   (when-let [bad (first (remove #(number? (get % x)) rows))]
     (schema/fail! "Horizon x values should be numbers." {:key :x :value (get bad x)}))
   (when-let [bad (first (remove #(let [v (get % y)] (or (nil? v) (schema/finite? v))) rows))]
     (schema/fail! "Horizon y values should be finite numbers (nil breaks the bands)." {:key :y :value (get bad y)}))
   (let [groups (if series
                  (let [order (distinct (map #(get % series) rows))
                        by (group-by #(get % series) rows)]
                    (map (fn [s] [s (by s)]) order))
                  [[nil rows]])
         top (v/mx (into [0.0] (keep #(when-let [value (get % y)] (m/abs (double value)))) rows))
         h (double (or band-height (if (m/pos? top) (m// top (double bands)) 1.0)))
         levels (band-levels bands h)
         labels (horizon-bands bands)]
     {:band-height h
      :rows (vec (for [[s rs] groups
                       :let [sorted (mapv (fn [r] [(double (get r x)) (some-> (get r y) double)])
                                          (sort-by #(double (get % x)) rs))
                             points (with-crossings (if interpolator
                                                      (interpolated-points sorted (fit/fit-fn interpolator nil)
                                                                           (long steps-per-gap))
                                                      sorted)
                                                    levels)]
                       band labels
                       [px py] points]
                   (cond-> {:x px :y (band-height-of band h py) :band band}
                     series (assoc :series s))))})))
