(ns cljplot2.data.interpolation
  "Fits of fastmath 1D interpolation methods (see [[cljplot2.schema/interpolation-methods]]) and of user functions,
  shared by curves (pixel space), smoothers and horizon resampling (data space)."
  (:require [fastmath.interpolation :as interpolation]
            [cljplot2.error :as error]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn fit-fn
  "`(fn [xs ys] f)` of `interpolator` (a method keyword or a function) with the extra arguments `params`."
  [interpolator params]
  (if (keyword? interpolator)
    (fn [xs ys] (apply interpolation/interpolation interpolator xs ys params))
    (fn [xs ys] (apply interpolator xs ys params))))

(defn fitted
  "`(fit-of xs ys)` - the fitted function, or `nil` when there are fewer points than the method needs. Other fastmath
  input errors throw `ex-info {:key :interpolator}`."
  [fit-of xs ys]
  (try (fit-of xs ys)
       (catch clojure.lang.ExceptionInfo e
         (let [{:keys [type property]} (ex-data e)]
           (cond
             (= :too-few-points property) nil
             (= :invalid-input type) (error/fail! (str "Interpolation failed: " (ex-message e) ".")
                                                  {:key :interpolator :value (:method (ex-data e))})
             :else (throw e))))))
