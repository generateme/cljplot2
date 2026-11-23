(ns cljplot2.data.interpolation
  "Fits of fastmath 1D interpolation methods and of user functions.

  Shared by curves (pixel space), smoothers and horizon resampling (data space); the methods are listed by [[cljplot2.schema/interpolation-methods]].

  Functions: [[fit-fn]] makes a fitting function of an interpolator, [[fitted]] fits it, tolerating too few points."
  (:require [fastmath.interpolation :as interpolation]
            [cljplot2.error :as error]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn fit-fn
  "Makes the fitting function of an interpolator.

  Parameters:

  - `interpolator`: a fastmath 1D interpolation method keyword (see [[cljplot2.schema/interpolation-methods]]) or a function `(fn [xs ys & params] f)`.
  - `params` (sequence or `nil`): extra arguments passed after `xs` and `ys`.

  Returns `(fn [xs ys] f)`, `f` the fitted function of one x.

  See also [[fitted]]."
  [interpolator params]
  (if (keyword? interpolator)
    (fn [xs ys] (apply interpolation/interpolation interpolator xs ys params))
    (fn [xs ys] (apply interpolator xs ys params))))

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
