(ns cljplot2.test-util
  "Helpers shared by the tests.")

(defn error-data
  "`ex-data` of the `ExceptionInfo` thrown by calling `f`, `nil` when it doesn't throw one."
  [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

(defn error
  "[[error-data]] without `:errors` (the first error: `:key`, `:in`, `:value` and any extra data)."
  [f]
  (some-> (error-data f) (dissoc :errors)))

(defn error-ins
  "Set of the `:in` paths of every error thrown by calling `f`."
  [f]
  (set (map :in (:errors (error-data f)))))
