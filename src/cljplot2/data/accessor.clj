(ns cljplot2.data.accessor
  "Accessor markers: a mapping accessor (keyword or function) wrapped with what it says about its dimension.

  [[->CategoricalAccessor]] (made by `cljplot2.data.levels/categorical`) marks the mapped dimension categorical: its
  scale is resolved as if it had `:type :categorical` (bands for positions, ordinal for aesthetics). Rows are read
  through the wrapped accessor; the marker itself is callable on a row.")

(set! *warn-on-reflection* true)

(defrecord CategoricalAccessor [accessor]
  clojure.lang.IFn
  (invoke [_ row] (if (keyword? accessor) (get row accessor) (accessor row)))
  (applyTo [this args] (clojure.lang.AFn/applyToHelper this args)))

(defn categorical?
  "Is `acc` a categorical accessor marker?"
  [acc]
  (instance? CategoricalAccessor acc))

(defn unwrap
  "The plain accessor of `acc` (a marker's wrapped accessor, else `acc`)."
  [acc]
  (if (categorical? acc) (:accessor acc) acc))
