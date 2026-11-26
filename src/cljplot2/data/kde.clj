(ns cljplot2.data.kde
  "Kernel density estimates of a sample, shared by the density charts.

  The estimates come from `fastmath.kernel`; charts such as `density` and `violin` evaluate them on a grid of points spanning the sample widened by a few bandwidths.

  Functions: [[estimate]] - the estimate of a sample and the span of its curve, [[grid-points]] - equally spaced evaluation points, [[curve]] - the estimate evaluated at points.

  Schemas: [[kernel-schema]] - a fastmath KDE kernel name, [[bandwidth-rule-schema]] - a fastmath bandwidth selector; option types of the density charts (density, violin) and of the 2D estimates (field, contour)."
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [fastmath.kernel :as k]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private kernels
  "The fastmath kernels of a density estimate (one- or two-dimensional), sorted."
  (vec (sort (get k/kernel-list :kde))))

(def kernel-schema
  "Schema of a density kernel: a fastmath KDE kernel name (`fastmath.kernel/kernel-list` under `:kde`)."
  (schema/named-type ::kernel (set kernels) (str "should be a fastmath KDE kernel: " (apply str (interpose " " kernels)))
                     {:cljplot2/type-name "density kernel"
                      :cljplot2/type-meaning "The shape a density estimate puts at each value: a fastmath kernel name, e.g. `:gaussian` or `:epanechnikov`."
                      :cljplot2/type-doc "a fastmath KDE kernel keyword"
                      :cljplot2/value-set kernels
                      :cljplot2/value-meanings {:gaussian "The normal curve; smooth, with long tails."
                                                :epanechnikov "A parabola, the most efficient kernel in theory."
                                                :uniform "A flat step: a moving count."
                                                :triangular "A triangle."}}))

(def ^:private bandwidth-rules
  "The fastmath bandwidth selectors of a density estimate (the 2D estimate also takes `:scott`)."
  [:nrd0 :nrd :nrd-adjust :sj :sj-dpi :lscv :lcv :rlcv])

(def bandwidth-rule-schema
  "Schema of a bandwidth rule: a fastmath bandwidth selector keyword."
  (schema/named-type ::bandwidth-rule (set bandwidth-rules) (str "should be a fastmath bandwidth selector: " (apply str (interpose " " bandwidth-rules)))
                     {:cljplot2/type-name "bandwidth rule"
                      :cljplot2/type-meaning "A rule that picks the bandwidth from the sample: a fastmath selector, e.g. `:nrd0` (R's default) or `:sj` (Sheather-Jones)."
                      :cljplot2/type-doc "a fastmath bandwidth selector keyword"
                      :cljplot2/value-set bandwidth-rules
                      :cljplot2/value-meanings {:nrd0 "Silverman's rule of thumb, R's default."
                                                :nrd "Scott's rule of thumb, wider than `:nrd0`."
                                                :sj "Sheather and Jones' plug-in selector, good for multimodal data."}}))

(defn estimate
  "Gives the kernel density estimate of a sample with the span its curve is drawn over.

  Parameters:

  - `values` (sequence of numbers): the sample, at least 2 distinct values.
  - `options` (map): `:kernel` - fastmath kernel name (default `:gaussian`); `:bandwidth` - a number or a fastmath bandwidth selector (default `:nrd0`); `:cut` - bandwidths the span extends past the sample on each side (default 3). A `nil` value takes the default.

  Returns `{:kde f :lo lo :hi hi}`: `f` - the density, a function of a number; `lo`, `hi` - the span `[min - cut h, max + cut h]` (`h` - the bandwidth).

  Throws `ex-info {:key :density}` for fewer than 2 distinct values (the callers skip those samples) and when fastmath can't estimate (the cause's data under `:cause`).

  See also [[curve]], [[grid-points]]."
  [values {:keys [kernel bandwidth cut]}]
  (let [distinct-count (count (distinct (map double values)))]
    (when (m/< distinct-count 2)
      (schema/fail! "Density estimation needs at least 2 distinct values." {:key :density :value distinct-count})))
  (let [kernel-name (or kernel :gaussian)
        bandwidth-spec (or bandwidth :nrd0)
        reach (if (nil? cut) 3.0 (double cut))
        {:keys [kde ^double h]} (try (k/kernel-density kernel-name values {:bandwidth bandwidth-spec} true)
                                     (catch clojure.lang.ExceptionInfo e
                                       (schema/fail! (str "Density estimation failed: " (ex-message e)) {:key :density :cause (ex-data e)})))]
    {:kde kde
     :lo (m/- (v/mn values) (m/* reach h))
     :hi (m/+ (v/mx values) (m/* reach h))}))

(defn grid-points
  "Gives equally spaced points from `lo` to `hi`, both included.

  Parameters:

  - `lo`, `hi` (doubles): the first and the last point.
  - `n` (long): the number of points, at least 2 (the callers' option schemas require it).

  Returns a vector of `n` doubles.

  See also [[curve]]."
  [^double lo ^double hi ^long n]
  (vec (m/slice-range lo hi n)))

(defn curve
  "Gives the curve of a density estimate at the given points.

  Parameters:

  - `e` (map): an estimate of [[estimate]], its `:kde` used.
  - `xs` (sequence of numbers): the evaluation points, e.g. of [[grid-points]].

  Returns `{:x xs :y ys}` - `ys` the densities at `xs`, a vector."
  [{:keys [kde]} xs]
  {:x xs :y (mapv kde xs)})
