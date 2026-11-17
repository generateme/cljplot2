(ns cljplot2.data.series
  "Time series rows: autocorrelations with their significance bands ([[acf-rows]]) and lagged pairs ([[lag-rows]]),
  for ordinary charts (see [[cljplot2.series-plots/acf]]).

  A series is a sequence of numbers in time order (fastmath `acf-ci` / `pacf-ci`)."
  (:require [fastmath.core :as m]
            [fastmath.stats :as stats]
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
