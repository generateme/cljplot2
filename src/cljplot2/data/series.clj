(ns cljplot2.data.series
  "Time series rows: autocorrelations with their significance bands ([[acf-rows]]), lagged pairs ([[lag-rows]]) and
  horizon bands ([[horizon-rows]]), for ordinary charts (see [[cljplot2.series-plots/acf]],
  [[cljplot2.series-plots/horizon]]).

  A series is a sequence of numbers in time order (fastmath `acf-ci` / `pacf-ci`)."
  (:require [fastmath.core :as m]
            [fastmath.stats :as stats]
            [cljplot2.data.interpolation :as fit]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- finite-series!
  "`values` as a vector of doubles; throws `ex-info {:key :values}` for a non-finite value or fewer than 2 values."
  [values]
  (let [vs (vec values)]
    (when-not (every? #(and (number? %) (m/valid-double? (double %))) vs)
      (schema/fail! "A series needs finite numbers." {:key :values :value (first (remove #(and (number? %) (m/valid-double? (double %))) vs))}))
    (when (< (count vs) 2)
      (schema/fail! "A series needs at least 2 values." {:key :values :value (count vs)}))
    (mapv double vs)))

(defn default-lags
  "The default number of lags of a series of `n` values: `min(floor(10 log10 n), n - 1)` (R `acf`)."
  ^long [^long n]
  (min (long (m/floor (* 10.0 (m/log10 n)))) (dec n)))

(defn- lags!
  "Validated `lags` (1 to `n - 1`) or the default."
  ^long [lags ^long n]
  (if (nil? lags)
    (default-lags n)
    (if (and (integer? lags) (<= 1 (long lags) (dec n)))
      (long lags)
      (schema/fail! (str "Series :lags should be an integer from 1 to " (dec n) ".") {:key :lags :value lags}))))

(defn acf-rows
  "Rows `{:lag :value :lo :hi :significant?}` of the autocorrelation function of `values` (a series, see the
  namespace doc): lags 0 to `:lags` (default [[default-lags]]), the band `[:lo :hi]` - Bartlett's per lag (fastmath
  `acf-ci` `:cis`), `:significant?` - the value outside the band (lag 0, always 1, isn't).
  With `:partial? true`: the partial autocorrelation, lags 1 to `:lags`, the constant band `±ci` (`pacf-ci`).
  `:alpha` - the significance level (default 0.05).
  Throws `ex-info {:key :values}` for a non-finite value, fewer than 2 values or a constant series, `{:key :lags}`
  for lags outside 1 to n - 1."
  [values {:keys [lags alpha partial?] :or {alpha 0.05}}]
  (let [vs (finite-series! values)
        lags (lags! lags (count vs))
        row (fn [^long lag v ^double band]
              (let [v (double v)]
                {:lag lag :value v :lo (- band) :hi band :significant? (and (pos? lag) (> (m/abs v) band))}))
        rows (if partial?
               (let [{:keys [ci pacf]} (stats/pacf-ci vs lags alpha)]
                 (mapv #(row %1 %2 (double ci)) (range 1 (inc lags)) (rest pacf)))
               (let [{:keys [acf cis]} (stats/acf-ci vs lags alpha)]
                 (mapv #(row %1 %2 (double %3)) (range (inc lags)) acf cis)))]
    (when-not (every? #(m/valid-double? (double (:value %))) rows)
      (schema/fail! "A constant series has no autocorrelation." {:key :values}))
    rows))

(defn lag-rows
  "Rows `{:t :x :y}` pairing every value `y_t` of `values` with `y_(t+k)`: `t` from 0, `n - k` rows (none for `k >=
  n`). Values are as given (a scatter drops invalid ones). Throws `ex-info {:key :lag}` unless `k` is a positive
  integer."
  [values k]
  (when-not (and (integer? k) (pos? (long k)))
    (schema/fail! "Lag should be a positive integer." {:key :lag :value k}))
  (let [vs (vec values)]
    (mapv (fn [t] {:t t :x (vs t) :y (vs (+ (long t) (long k)))}) (range (max 0 (- (count vs) (long k)))))))

(defn- band-levels
  "The values where a horizon band of `n` bands of height `h` changes: `±k h` for `k` 0..n."
  [^long n ^double h]
  (sort (distinct (for [k (range (inc n)) s [1.0 -1.0]] (* (double s) (double k) h)))))

(defn- crossings
  "Points `[x y]` where the segment from `[x0 y0]` to `[x1 y1]` crosses one of `levels` strictly between its ends, in
  order along it."
  [[x0 y0] [x1 y1] levels]
  (let [x0 (double x0) y0 (double y0) x1 (double x1) y1 (double y1)
        lo (m/min y0 y1) hi (m/max y0 y1)]
    (->> levels
         (filter #(< lo (double %) hi))
         (map (fn [l] (let [t (/ (- (double l) y0) (- y1 y0))] [t [(+ x0 (* t (- x1 x0))) (double l)]])))
         (sort-by first)
         (map second))))

(defn- with-crossings
  "Points `[x y]` of one series (x increasing, `y` `nil` where it breaks) with the band-edge crossings (`levels`) of
  every segment between two valid points inserted."
  [points levels]
  (if (empty? points)
    []
    (into [(first points)]
          (mapcat (fn [[p q]] (if (and (second p) (second q)) (concat (crossings p q levels) [q]) [q])))
          (partition 2 1 points))))

(defn- band-height-of
  "Height of band `band` (1..n; negative - the mirrored negative bands) at value `y` with band height `h`:
  `clamp(±y - (|band| - 1) h, 0, h)`; `nil` for `nil` `y`."
  [band ^double h y]
  (when (some? y)
    (let [b (long band)
          v (* (if (pos? b) 1.0 -1.0) (double y))]
      (m/constrain (- v (* (double (dec (m/abs b))) h)) 0.0 h))))

(defn horizon-bands
  "Band labels of a horizon of `n` bands in drawing order: `1..n` (positive), then `-1..-n` (negative, mirrored) - each
  band under the next, darker one."
  [^long n]
  (vec (concat (range 1 (inc n)) (map - (range 1 (inc n))))))

(defn- resampled-run
  "Points `[x y]` of `run` (valid points, x strictly increasing) on the curve `fit-of` fits through them: `steps`
  points per gap between consecutive x, the data points kept; the run as is when it is too short for the method."
  [fit-of run ^long steps]
  (if-let [f (when (> (count run) 1) (fit/fitted fit-of (mapv first run) (mapv second run)))]
    (conj (vec (for [[[x0 y0] [x1]] (partition 2 1 run)
                     i (range steps)]
                 (if (zero? (long i))
                   [x0 y0]
                   (let [x (+ (double x0) (* (- (double x1) (double x0)) (/ (double i) (double steps))))]
                     [x (double (f x))]))))
          (peek run))
    run))

(defn- interpolated-points
  "Points `[x y]` of one series (x increasing, `y` `nil` where it breaks) with every run of valid points resampled
  ([[resampled-run]]). Throws `ex-info {:key :interpolator}` for a repeated x inside a run."
  [points fit-of steps]
  (into [] (mapcat (fn [run]
                     (cond
                       (nil? (second (first run))) run
                       (apply < (map first run)) (resampled-run fit-of (vec run) steps)
                       :else (schema/fail! "Horizon :interpolator needs distinct x values in every run."
                                           {:key :interpolator :value (mapv first run)}))))
        (partition-by #(nil? (second %)) points)))

(defn horizon-rows
  "Rows of a horizon chart of `rows` (maps): values `:y` over `:x` (option keys `:x`, `:y`, default `:x` / `:y`), one
  series per `:series` value (option key; first-appearance order; without it one series `nil`).
  Returns `{:rows [{:x :y :band :series} ...] :band-height h}`: `:bands` n (default 2) bands of height `h` -
  `:band-height`, else the largest `|y|` of all series / n (1 when it is 0); band `k` (1..n) is `clamp(y - (k-1) h, 0,
  h)`, band `-k` the same of `-y` (negatives mirrored upward); every series' points sorted by x with the crossings of
  band edges inserted, so linear areas are exact; a `nil` `y` breaks the bands (a row with `:y nil` per band).
  `:interpolator` (a fastmath 1D method of [[cljplot2.schema/interpolation-methods]], e.g. `:monotone`, or a function
  `(fn [xs ys] f)`; default none - straight segments): every run of valid points is first resampled on that curve,
  `:steps-per-gap` points (default 10) per gap between consecutive x, the data points kept, and the bands folded from
  the resampled series - so all bands follow one curve (a run too short for the method stays straight).
  Throws `ex-info` `{:key :bands}` unless a positive integer, `{:key :band-height}` unless a positive finite number,
  `{:key :x}` for a non-numeric x, `{:key :y}` for a `y` neither `nil` nor a finite number, `{:key :interpolator}`
  for an unknown method or a repeated x in a run, `{:key :steps-per-gap}` unless a positive integer."
  [rows {:keys [x y series bands band-height interpolator steps-per-gap] :or {x :x y :y bands 2 steps-per-gap 10}}]
  (when-not (or (nil? interpolator) (fn? interpolator) (contains? schema/interpolation-methods interpolator))
    (schema/fail! (str "Horizon :interpolator should be a function or one of " (vec (sort schema/interpolation-methods)) ".")
                  {:key :interpolator :value interpolator}))
  (when-not (and (integer? steps-per-gap) (pos? (long steps-per-gap)))
    (schema/fail! "Horizon :steps-per-gap should be a positive integer." {:key :steps-per-gap :value steps-per-gap}))
  (when-not (and (integer? bands) (pos? (long bands)))
    (schema/fail! "Horizon :bands should be a positive integer." {:key :bands :value bands}))
  (when-not (or (nil? band-height) (and (number? band-height) (m/valid-double? (double band-height)) (pos? (double band-height))))
    (schema/fail! "Horizon :band-height should be a positive finite number." {:key :band-height :value band-height}))
  (when-let [bad (first (remove #(number? (get % x)) rows))]
    (schema/fail! "Horizon x values should be numbers." {:key :x :value (get bad x)}))
  (when-let [bad (first (remove #(let [v (get % y)] (or (nil? v) (and (number? v) (m/valid-double? (double v))))) rows))]
    (schema/fail! "Horizon y values should be finite numbers (nil breaks the bands)." {:key :y :value (get bad y)}))
  (let [n (long bands)
        groups (if series
                 (let [order (distinct (map #(get % series) rows))
                       by (group-by #(get % series) rows)]
                   (mapv (fn [s] [s (by s)]) order))
                 [[nil rows]])
        top (double (reduce max 0.0 (keep #(when-let [v (get % y)] (m/abs (double v))) rows)))
        h (double (or band-height (if (pos? top) (/ top (double n)) 1.0)))
        levels (band-levels n h)
        labels (horizon-bands n)]
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
                    series (assoc :series s))))}))
