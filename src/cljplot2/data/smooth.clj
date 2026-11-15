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
  by x with the mean y of repeated x (they need strictly increasing nodes); `:lm` / `:glm` on the data as given."
  (:require [fastmath.core :as m]
            [fastmath.interpolation :as interpolation]
            [fastmath.interpolation.bootstrap :as bootstrap]
            [fastmath.interpolation.gp :as gp]
            [fastmath.ml.regression :as regression]
            [fastmath.random :as r]
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

(defn- loess-params [params] (merge loess-defaults params))

(defn default-band
  "Band of `method` without a `:band` option: `:confidence` for [[error-methods]], else `:none`."
  [method]
  (if (contains? error-methods method) :confidence :none))

(defn required-points
  "Fewest distinct x values a fit of `method` with `params` needs: `:lm` - `degree + 2`, `:glm` - 3 (fastmath needs a
  residual degree of freedom beyond the coefficients, intercept included); `:loess` - enough for 2
  neighbours at its `:bandwidth` (default 0.75 - [[loess-defaults]]: 3; a `:bandwidth` has to be a number in
  (0, 1]); others 2."
  ^long [method params]
  (case method
    :lm (+ 2 (long (get params :degree 1)))
    :glm 3
    :loess (let [bandwidth (double (:bandwidth (loess-params params)))]
             (loop [n 2] (if (>= (m/floor (* bandwidth n)) 2.0) n (recur (inc n)))))
    2))

(defn prepared
  "`[xs ys]` sorted by x, with the mean y of repeated x."
  [xs ys]
  (let [groups (reduce (fn [acc [x y]] (update acc (double x) (fnil conj []) (double y))) (sorted-map) (map vector xs ys))]
    [(vec (keys groups))
     (mapv (fn [vs] (/ (double (reduce + 0.0 vs)) (double (count vs)))) (vals groups))]))

(defn- interpolation-method? [method] (or (fn? method) (not (#{:lm :glm} method))))

(defn- lm-options
  "fastmath `lm` options of `params` fitted on `xs`: `:degree` above 1 as a `:transformer`."
  [params xs]
  (let [degree (long (get params :degree 1))]
    (cond-> (dissoc params :degree)
      (> degree 1) (assoc :transformer (regression/polynomial-transformer xs degree)))))

(def ^:private ^:const default-level
  "Confidence level of fastmath's default `lm` / `glm` intervals (significance 0.05)."
  0.95)

(defn- regression-model
  "fastmath `lm` / `glm` model of `ys` on `xs` with `params`, its intervals at confidence `level`."
  [method params xs ys level]
  (let [xss (mapv vector xs)
        alpha (- 1.0 (double level))]
    (case method
      :lm (regression/lm ys xss (assoc (lm-options params xs) :alpha alpha))
      :glm (regression/glm ys xss (assoc params :alpha alpha)))))

(defn- gp-model [xs ys params] (gp/gaussian-process xs ys params))

(defn fit-fn
  "`(fn [xs ys] f)` of `method` with `params`: `f` - the fitted function of one x."
  [method params]
  (case method
    (:lm :glm) (fn [xs ys] (let [model (regression-model method params xs ys default-level)]
                             (fn [x] (regression/predict model [x]))))
    :loess (fn [xs ys] (interpolation/loess xs ys (loess-params params)))
    :gp (fn [xs ys] (let [model (gp-model xs ys params)] (fn [x] (gp/predict model x false))))
    (fit/fit-fn method (when (some? params) [params]))))

(defn- quantile ^double [distribution ^double p] (r/icdf distribution p))

(defn- t-distribution [df] (r/distribution :t {:degrees-of-freedom df}))

(def ^:private normal (r/distribution :normal))

(defn- loess-band
  "`[lo hi]` vectors of the `Loess` fit `f` at `keys`: fit ± t((1 + level) / 2, `:df`) · stderr; `NaN` where the
  stderr is `NaN` (outside the data) or `:df` isn't positive."
  [f keys level]
  (let [df (double (:df f))
        t (if (pos? df) (quantile (t-distribution df) (* 0.5 (+ 1.0 (double level)))) ##NaN)
        fits (mapv #(f % true) keys)]
    [(mapv (fn [[fit se]] (- (double fit) (* t (double se)))) fits)
     (mapv (fn [[fit se]] (+ (double fit) (* t (double se)))) fits)]))

(defn- gp-band
  "`[lo hi]` vectors of the Gaussian process `model` at `keys`: mean ± z((1 + level) / 2) · σ."
  [model keys level]
  (let [z (quantile normal (* 0.5 (+ 1.0 (double level))))
        fits (mapv #(gp/predict model % true) keys)]
    [(mapv (fn [[mean sd]] (- (double mean) (* z (double sd)))) fits)
     (mapv (fn [[mean sd]] (+ (double mean) (* z (double sd)))) fits)]))

(defn- regression-band
  "`[lo hi]` vectors of the `:confidence-interval` / `:prediction-interval` (`band`) of the model `(model-at level)`
  (intervals at confidence `level`) at `keys`."
  [model-at band keys level]
  (let [model (model-at level)
        k (if (= :prediction band) :prediction-interval :confidence-interval)
        intervals (mapv #(k (regression/predict model [%] true)) keys)]
    [(mapv #(double (first %)) intervals) (mapv #(double (second %)) intervals)]))

(defn- bootstrap-band
  "`[lo hi]` vectors of the bootstrap record `b` at `keys`."
  [b keys level]
  (let [intervals (mapv #(b % level) keys)]
    [(mapv first intervals) (mapv second intervals)]))

(defn fit-curve
  "Fit of `ys` on `xs` (numbers; at least [[required-points]] distinct `xs`) by `:method` with `:params` evaluated at
  the grid `keys`, with `:band` bands at every distinct level of `:levels` (widest first): `{:fit [..] :bands [{:level l
  :lo [..] :hi [..]}]}` (`NaN` where a band is undefined). `:bootstrap` - the options of
  `fastmath.interpolation/bootstrap` (`:samples`, `:rng`, `:ci`, `:estimation`). Fit errors propagate."
  [{:keys [method params band levels bootstrap]} xs ys keys]
  (let [[fxs fys] (if (interpolation-method? method) (prepared xs ys) [(vec xs) (vec ys)])
        ;; lm / glm: one model per level (the line's - the default level - shared with a band at 0.95)
        model-at (memoize (fn [level] (regression-model method params fxs fys level)))
        gp-object (when (= :gp method) (gp-model fxs fys params))
        fitted (case method
                 (:lm :glm) (let [model (model-at default-level)] (fn [x] (regression/predict model [x])))
                 :gp (fn [x] (gp/predict gp-object x false))
                 ((fit-fn method params) fxs fys))
        levels (vec (sort > (distinct levels)))
        band-at (case band
                  :none nil
                  :bootstrap (let [b (interpolation/bootstrap (fit-fn method params) xs ys bootstrap)]
                               #(bootstrap-band b keys %))
                  (case method
                    :loess #(loess-band fitted keys %)
                    :gp #(gp-band gp-object keys %)
                    (:lm :glm) #(regression-band model-at band keys %)))]
    {:fit (mapv #(double (fitted %)) keys)
     :bands (if band-at
              (mapv (fn [level] (let [[lo hi] (band-at level)] {:level level :lo lo :hi hi})) levels)
              [])}))

(def bootstrap-intervals
  "`:ci` values of the bootstrap band (see `fastmath.interpolation.bootstrap/intervals`)."
  (set (keys bootstrap/intervals)))
