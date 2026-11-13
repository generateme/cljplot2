(ns cljplot2.data.kde
  "Kernel density estimates (fastmath) shared by charts (density, violin): the estimate of a sample and its curve on
  an evaluation grid."
  (:require [fastmath.core :as m]
            [fastmath.kernel :as k]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn estimate
  "`{:kde f :lo :hi}` of the KDE of `values` (at least 2 distinct; options `:kernel` (default `:gaussian`),
  `:bandwidth` (number or fastmath selector, default `:nrd0`), `:cut` (default 3)): the curve spans
  `[min - cut*h, max + cut*h]`. Throws `ex-info {:key :density}` when fastmath can't estimate."
  [values {:keys [kernel bandwidth cut] :or {kernel :gaussian bandwidth :nrd0 cut 3}}]
  (let [{:keys [kde h]} (try (k/kernel-density kernel values {:bandwidth bandwidth} true)
                             (catch clojure.lang.ExceptionInfo e
                               (schema/fail! (str "Density estimation failed: " (ex-message e)) {:key :density :cause (ex-data e)})))
        h (double h)]
    {:kde kde
     :lo (- (double (reduce min values)) (* (double cut) h))
     :hi (+ (double (reduce max values)) (* (double cut) h))}))

(defn grid-points
  "`n` (at least 2) equally spaced points from `lo` to `hi`."
  [^double lo ^double hi ^long n]
  (let [step (/ (- hi lo) (dec n))]
    (mapv #(+ lo (* step (double %))) (range n))))

(defn curve
  "`{:x [..] :y [..]}` of the estimate `e` (see [[estimate]]) evaluated at `xs`."
  [{:keys [kde]} xs]
  {:x xs :y (mapv #(double (kde %)) xs)})
