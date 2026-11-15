(ns cljplot2.data.interpolation
  "Fits of fastmath 1D interpolation methods (see [[cljplot2.schema/interpolation-methods]]) and of user functions,
  shared by curves (pixel space) and smoothers (data space)."
  (:require [fastmath.interpolation :as interpolation]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn fit-fn
  "`(fn [xs ys] f)` of `interpolator` (a method keyword or a function) with the extra arguments `params`."
  [interpolator params]
  (if (keyword? interpolator)
    (fn [xs ys] (apply interpolation/interpolation interpolator xs ys params))
    (fn [xs ys] (apply interpolator xs ys params))))
