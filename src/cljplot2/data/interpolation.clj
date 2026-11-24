(ns cljplot2.data.interpolation
  "Fits of fastmath 1D interpolation methods and of user functions.

  Shared by curves (pixel space), smoothers and horizon resampling (data space); the methods are listed by [[cljplot2.schema/interpolation-methods]].

  Functions: [[fit-fn]] makes a fitting function of an interpolator, [[fitted]] fits it, tolerating too few points.

  Fits are deterministic: `:kriging` (its variogram's random search) and `:microsphere-projection` (its random facets in one dimension) draw random values, so their fits without an own `:rng` get a fresh generator of a fixed seed - the same points always give the same function (every render of a build draws the same curve). The seed is fixed, not a chart's `:rng` (which goes through `rng/for-build` for random values drawn in compute): a fit is a function of its points only."
  (:require [fastmath.core :as m]
            [fastmath.interpolation :as interpolation]
            [fastmath.random :as r]
            [cljplot2.error :as error]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^{:private true :const true :tag 'long} fit-seed
  "Seed of the generator a random fit without its own `:rng` gets (see [[fit-fn]])."
  1)

;; The positions follow fastmath's signatures: `(kriging xss ys variogram opts)`, `(microsphere-projection xss ys opts)` -
;; recheck them when fastmath changes.
(def ^:private random-fit-options
  "Methods drawing random values while fitting -> the position of their options map (taking `:rng`) among the extra arguments: `:kriging` `[variogram options]`, `:microsphere-projection` `[options]`."
  {:kriging 1 :microsphere-projection 0})

(defn- seeded-params
  "Gives a random method's extra arguments with a fresh generator of [[fit-seed]] as `:rng` in its options.

  Parameters:

  - `params` (sequence or `nil`): the extra arguments passed after `xs` and `ys`.
  - `position` (long): the index of the options map among them (see [[random-fit-options]]).

  Returns the arguments as a vector, padded with `nil`s up to `position`, with `:rng` in the options; `params` unchanged when the options give an `:rng` or aren't a map (or `nil`) - fastmath then reports them."
  [params ^long position]
  (let [given (vec params)
        args (into given (repeat (m/long-sub (m/long-inc position) (count given)) nil))
        options (args position)]
    (if (and (or (nil? options) (map? options)) (nil? (:rng options)))
      (assoc args position (assoc options :rng (r/rng :mersenne fit-seed)))
      params)))

(defn fit-fn
  "Makes the fitting function of an interpolator.

  Parameters:

  - `interpolator`: a fastmath 1D interpolation method keyword (see [[cljplot2.schema/interpolation-methods]]) or a function `(fn [xs ys & params] f)`.
  - `params` (sequence or `nil`): extra arguments passed after `xs` and `ys` (`:kriging` `[variogram options]`, `:microsphere-projection` `[options]`).

  Returns `(fn [xs ys] f)`, `f` the fitted function of one x. A `:kriging` or `:microsphere-projection` fit whose options have no `:rng` gets a fresh generator of a fixed seed on every call, so equal points give equal functions; a given `:rng` is used (and consumed) as it is.

  See also [[fitted]]."
  [interpolator params]
  (cond
    (random-fit-options interpolator) (let [position (long (random-fit-options interpolator))]
                                        (fn [xs ys] (apply interpolation/interpolation interpolator xs ys (seeded-params params position))))
    (keyword? interpolator) (fn [xs ys] (apply interpolation/interpolation interpolator xs ys params))
    :else (fn [xs ys] (apply interpolator xs ys params))))

(defn fitted
  "Fits points with a fitting function, tolerating too few points.

  Parameters:

  - `fit-of` (function): `(fn [xs ys] f)`, see [[fit-fn]].
  - `xs`, `ys` (sequences of numbers): the points.

  Returns the fitted function, or `nil` when there are fewer points than the method needs.

  Throws `ex-info {:key :interpolator}` for other fastmath input errors; other exceptions propagate."
  [fit-of xs ys]
  (try (fit-of xs ys)
       (catch clojure.lang.ExceptionInfo e
         (let [{error-type :type property :property} (ex-data e)]
           (cond
             (= :too-few-points property) nil
             (= :invalid-input error-type) (error/fail! (str "Interpolation failed: " (ex-message e) ".")
                                                  {:key :interpolator :value (:method (ex-data e))})
             :else (throw e))))))
