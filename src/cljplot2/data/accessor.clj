(ns cljplot2.data.accessor
  "Accessor markers: a mapping accessor (keyword or function) wrapped with what it says about its dimension.

  [[->CategoricalAccessor]] (made by [[cljplot2.data.levels/categorical]]) marks the mapped dimension categorical: its scale is resolved as if it had `:type :categorical` (bands for positions, ordinal for aesthetics). Rows are read through the wrapped accessor; the marker itself is callable on a row.

  Functions: [[categorical?]] tests for a marker, [[unwrap]] gives the plain accessor of a marker or of a plain accessor.")

(set! *warn-on-reflection* true)

(defrecord CategoricalAccessor [accessor]
  clojure.lang.IFn
  (invoke [_ row] (if (keyword? accessor) (get row accessor) (accessor row)))
  (applyTo [this args] (clojure.lang.AFn/applyToHelper this args)))

(defn categorical?
  "Tests whether a mapping accessor is a categorical marker.

  Parameters:

  - `acc`: any mapping accessor (keyword, function or marker).

  Returns `true` for a [[->CategoricalAccessor]], else `false`.

  See also [[unwrap]]."
  [acc]
  (instance? CategoricalAccessor acc))

(defn unwrap
  "Gives the plain accessor of a mapping accessor.

  Parameters:

  - `acc`: a keyword, a function or a categorical marker.

  Returns the wrapped accessor of a marker, else `acc` itself.

  See also [[categorical?]]."
  [acc]
  (if (categorical? acc) (:accessor acc) acc))
