(ns cljplot2.data.smooth
  "Smoothing fits and their bands for the smooth chart: one fit of `ys` on numeric `xs` (x keys, see
  [[cljplot2.data.rows/x-key]]) evaluated on a grid, with confidence, prediction or bootstrap bands at one or more
  levels.

  Methods ([[smooth-methods]]): `:loess` (fastmath loess - by default local quadratic over 75% of the points, as
  ggplot2's `geom_smooth` ([[loess-defaults]]); bands from its standard errors and t quantiles of its `:df`),
  `:lm` (fastmath linear model, `:degree` - orthogonal polynomial terms; confidence and prediction intervals),
  `:glm` (fastmath generalized linear model; confidence intervals), `:gp` (fastmath Gaussian process, mean ± z·σ of
  the latent function) and every other fastmath 1D interpolation method or a function `(fn [xs ys] f)` (no own band;
  bootstrap). x keys of `java.time` values are epoch milliseconds: scale parameters in x units - a `:gp` kernel's
  `:sigma`, for example - are in milliseconds then. The fastmath interpolation methods (`:loess`, `:gp` and the interpolators) are fitted on the data sorted
  by x with the mean y of repeated x (they need strictly increasing nodes); `:lm` / `:glm` on the data as given.

  Functions: [[fit-curve]] - a fit with its bands on a grid; [[fit-fn]] - the fitting function of a method; [[fit-requirements]] - the data a method needs; [[prepared]] - data for the interpolation methods; [[default-band]] - a method's band."
  (:require [fastmath.core :as m]
            [fastmath.interpolation :as interpolation]
            [fastmath.interpolation.bootstrap :as bootstrap]
            [fastmath.interpolation.gp :as gp]
            [fastmath.ml.regression :as regression]
            [fastmath.random :as r]
            [fastmath.vector :as v]
            [cljplot2.data.interpolation :as fit]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def error-methods
  "Methods with their own confidence band."
  #{:loess :lm :glm :gp})

(def smooth-methods
  "Smooth methods: [[error-methods]] and the fastmath 1D interpolation methods."
  (into error-methods schema/interpolation-methods))

(def band-kinds
  "Band kinds: `:confidence`, `:prediction` (`:lm`), `:bootstrap`, `:none`."
  [:confidence :prediction :bootstrap :none])

(def loess-defaults
  "`:loess` options under the user's `:method-params`: ggplot2 `geom_smooth`'s span (`:bandwidth` 0.75) and local
  quadratic fits (`:degree` 2); fastmath's robustness iterations (`:iters`, default 2) are kept - `:iters 0` is
  ggplot2's plain fit."
  {:bandwidth 0.75 :degree 2})

(defn- loess-params
  "Gives the `:loess` options of the user's method parameters.

  Parameters:

  - `params` (map or `nil`): the user's `:method-params`.

  Returns `params` merged over [[loess-defaults]]."
  [params]
  (merge loess-defaults params))

(defn default-band
  "Gives the band of a smooth method without a `:band` option.

  Parameters:

  - `method`: a smooth method keyword or a function.

  Returns `:confidence` for [[error-methods]], else `:none`."
  [method]
  (if (contains? error-methods method) :confidence :none))

(defn fit-requirements
  "Gives the fewest distinct x values and rows a fit of a smooth method needs.

  Parameters:

  - `method`: a smooth method keyword or a function.
  - `params` (map or `nil`): the method's parameters (`:degree` of `:lm`, `:bandwidth` of `:loess`).

  Returns `{:distinct-x d :rows n}`: `:lm` - `degree + 1` distinct x (the polynomial's coefficients are determined) and `degree + 2` rows (a residual degree of freedom for the confidence band; repeated x count - R `lm` fits Anscombe's set IV, two distinct x); `:glm` - 3 and 3; `:loess` - enough distinct x for 2 neighbours at its `:bandwidth` (default 0.75 - [[loess-defaults]]: 3; a `:bandwidth` has to be a number in (0, 1]); others 2 (interpolations fit the mean y of each distinct x, see [[prepared]])."
  [method params]
  (let [both (fn [n] {:distinct-x n :rows n})]
    (case method
      :lm (let [degree (long (get params :degree 1))] {:distinct-x (m/long-inc degree) :rows (m/long-add 2 degree)})
      :glm (both 3)
      :loess (let [bandwidth (double (:bandwidth (loess-params params)))]
               (both (loop [n 2] (if (m/>= (m/floor (m/* bandwidth n)) 2.0) n (recur (m/long-inc n))))))
      (both 2))))

(defn prepared
  "Prepares data for the interpolation methods, which need strictly increasing nodes.

  Parameters:

  - `xs`, `ys` (sequences of numbers): the data.

  Returns `[xs ys]` (vectors of doubles) sorted by x, with the mean y of repeated x."
  [xs ys]
  (let [groups (reduce (fn [acc [x y]] (update acc (double x) (fnil conj []) (double y))) (sorted-map) (map vector xs ys))]
    [(vec (keys groups))
     (mapv (fn [vs] (m// (v/sum vs) (count vs))) (vals groups))]))

(defn- interpolation-method?
  "Tests whether a smooth method is fitted as an interpolation (on [[prepared]] data).

  Parameters:

  - `method`: a smooth method keyword or a function.

  Returns `true` for a function or any method but `:lm` / `:glm`."
  [method]
  (or (fn? method) (not (#{:lm :glm} method))))

(defn- lm-options
  "Gives the fastmath `lm` options of the user's parameters.

  Parameters:

  - `params` (map or `nil`): the parameters; `:degree` (default 1) above 1 becomes a polynomial `:transformer`.
  - `xs` (sequence of numbers): the x values the transformer is fitted on.

  Returns the options map, without `:degree`."
  [params xs]
  (let [degree (long (get params :degree 1))]
    (cond-> (dissoc params :degree)
      (m/> degree 1) (assoc :transformer (regression/polynomial-transformer xs degree)))))

(def ^{:private true :const true :tag 'double} default-level
  "Confidence level of fastmath's default `lm` / `glm` intervals (significance 0.05)."
  0.95)

(defn- regression-model
  "Fits a fastmath linear or generalized linear model.

  Parameters:

  - `method` (keyword): `:lm` or `:glm`.
  - `params` (map or `nil`): the model's parameters (see [[lm-options]] for `:lm`).
  - `xs`, `ys` (sequences of numbers): the data, `ys` on `xs`.
  - `level` (double): the confidence level of the model's intervals.

  Returns the model."
  [method params xs ys level]
  (let [xss (mapv vector xs)
        alpha (m/- 1.0 (double level))]
    (case method
      :lm (regression/lm ys xss (assoc (lm-options params xs) :alpha alpha))
      :glm (regression/glm ys xss (assoc params :alpha alpha)))))

(defn- gp-model
  "Fits a fastmath Gaussian process.

  Parameters:

  - `xs`, `ys` (sequences of numbers): the data.
  - `params` (map or `nil`): the process' options.

  Returns the model."
  [xs ys params]
  (gp/gaussian-process xs ys params))

(defn fit-fn
  "Makes the fitting function of a smooth method.

  Parameters:

  - `method`: a smooth method keyword (see [[smooth-methods]]) or a function `(fn [xs ys] f)`.
  - `params` (map or `nil`): the method's parameters.

  Returns `(fn [xs ys] f)`, `f` the fitted function of one x."
  [method params]
  (case method
    (:lm :glm) (fn [xs ys] (let [model (regression-model method params xs ys default-level)]
                             (fn [x] (regression/predict model [x]))))
    :loess (fn [xs ys] (interpolation/loess xs ys (loess-params params)))
    :gp (fn [xs ys] (let [model (gp-model xs ys params)] (fn [x] (gp/predict model x false))))
    (fit/fit-fn method (when (some? params) [params]))))

(defn- quantile
  "Gives a quantile of a distribution.

  Parameters:

  - `distribution`: a fastmath distribution.
  - `p` (double): the probability.

  Returns the inverse CDF at `p`."
  ^double [distribution ^double p]
  (r/icdf distribution p))

(defn- t-distribution
  "Makes Student's t distribution.

  Parameters:

  - `df` (number): the degrees of freedom.

  Returns the fastmath distribution."
  [df]
  (r/distribution :t {:degrees-of-freedom df}))

(def ^:private normal
  "The standard normal distribution."
  (r/distribution :normal))

(defn- two-sided
  "Gives the upper probability of a two-sided interval.

  Parameters:

  - `level` (double): the confidence level.

  Returns `(1 + level) / 2`."
  ^double [^double level]
  (m/* 0.5 (m/inc level)))

(defn- loess-band
  "Computes a band of a loess fit from its standard errors.

  Parameters:

  - `f`: the fastmath `Loess` fit (its `:df`, called with `true` - `[fit se]`).
  - `grid` (sequence of numbers): the grid.
  - `level` (double): the confidence level.

  Returns `[lo hi]` vectors: fit ± t((1 + level) / 2, `:df`) · stderr; `NaN` where the stderr is `NaN` (outside the data) or `:df` isn't positive."
  [f grid level]
  (let [df (double (:df f))
        t (if (m/pos? df) (quantile (t-distribution df) (two-sided level)) ##NaN)
        fits (mapv #(f % true) grid)]
    [(mapv (fn [[^double fit ^double se]] (m/- fit (m/* t se))) fits)
     (mapv (fn [[^double fit ^double se]] (m/+ fit (m/* t se))) fits)]))

(defn- gp-band
  "Computes a band of a Gaussian process from its latent standard deviations.

  Parameters:

  - `model`: the fastmath Gaussian process.
  - `grid` (sequence of numbers): the grid.
  - `level` (double): the confidence level.

  Returns `[lo hi]` vectors: mean ± z((1 + level) / 2) · σ."
  [model grid level]
  (let [z (quantile normal (two-sided level))
        fits (mapv #(gp/predict model % true) grid)]
    [(mapv (fn [[^double mean ^double sd]] (m/- mean (m/* z sd))) fits)
     (mapv (fn [[^double mean ^double sd]] (m/+ mean (m/* z sd))) fits)]))

(defn- regression-band
  "Computes a confidence or prediction band of a linear or generalized linear model.

  Parameters:

  - `model-at` (function): the model of a confidence level, see [[regression-model]].
  - `band` (keyword): `:prediction` - the prediction interval, else the confidence interval.
  - `grid` (sequence of numbers): the grid.
  - `level` (double): the confidence level.

  Returns `[lo hi]` vectors of doubles."
  [model-at band grid level]
  (let [model (model-at level)
        k (if (= :prediction band) :prediction-interval :confidence-interval)
        intervals (mapv #(k (regression/predict model [%] true)) grid)]
    [(mapv #(double (first %)) intervals) (mapv #(double (second %)) intervals)]))

(defn- bootstrap-band
  "Computes a bootstrap band.

  Parameters:

  - `b`: the fastmath bootstrap record, a function of x and a level giving `[lo hi]`.
  - `grid` (sequence of numbers): the grid.
  - `level` (double): the confidence level.

  Returns `[lo hi]` vectors."
  [b grid level]
  (let [intervals (mapv #(b % level) grid)]
    [(mapv first intervals) (mapv second intervals)]))

(defn fit-curve
  "Fits a smooth curve with its bands, evaluated on a grid.

  Parameters:

  - `spec` (map):
    - `:method` - a smooth method (see [[smooth-methods]]) or a function `(fn [xs ys] f)`.
    - `:params` - the method's parameters.
    - `:band` - one of [[band-kinds]].
    - `:levels` - confidence levels of the bands.
    - `:bootstrap` - the options of `fastmath.interpolation/bootstrap` (`:samples`, `:rng`, `:ci`, `:estimation`).
  - `xs`, `ys` (sequences of numbers): the data, at least the [[fit-requirements]] of the method.
  - `grid` (sequence of numbers): the grid.

  Returns `{:fit [..] :bands [{:level l :lo [..] :hi [..]}]}`: a band at every distinct level, widest first (none for `:band :none`); `NaN` where a band is undefined.

  Fit errors propagate."
  [{:keys [method params band bootstrap] requested-levels :levels} xs ys grid]
  (let [[fxs fys] (if (interpolation-method? method) (prepared xs ys) [(vec xs) (vec ys)])
        ;; lm / glm: one model per level (the line's - the default level - shared with a band at 0.95)
        model-at (memoize (fn [level] (regression-model method params fxs fys level)))
        gp-object (when (= :gp method) (gp-model fxs fys params))
        fitted (case method
                 (:lm :glm) (let [model (model-at default-level)] (fn [x] (regression/predict model [x])))
                 :gp (fn [x] (gp/predict gp-object x false))
                 ((fit-fn method params) fxs fys))
        levels (vec (sort > (distinct requested-levels)))
        band-at (case band
                  :none nil
                  :bootstrap (let [b (interpolation/bootstrap (fit-fn method params) xs ys bootstrap)]
                               #(bootstrap-band b grid %))
                  (case method
                    :loess #(loess-band fitted grid %)
                    :gp #(gp-band gp-object grid %)
                    (:lm :glm) #(regression-band model-at band grid %)))]
    {:fit (mapv #(double (fitted %)) grid)
     :bands (if band-at
              (mapv (fn [level] (let [[lo hi] (band-at level)] {:level level :lo lo :hi hi})) levels)
              [])}))

(def bootstrap-intervals
  "`:ci` values of the bootstrap band (see `fastmath.interpolation.bootstrap/intervals`)."
  (set (keys bootstrap/intervals)))
