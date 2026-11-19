(ns cljplot2.error
  "The error shape of every input check (see [[cljplot2.schema]]): `ex-info` with `{:key k :in path :value v :errors
  [{:in :value :message} ...]}` and a message listing every error. No dependencies on other cljplot2 namespaces, so
  namespaces that [[cljplot2.schema]] itself requires (e.g. [[cljplot2.graphics.shape]]) can fail the same way."
  (:require [clojure.string :as str]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- value-str
  "`v` printed for a message: short collections, at most 60 characters."
  [v]
  (let [s (binding [*print-length* 5 *print-level* 3] (pr-str v))]
    (if (> (count s) 60) (str (subs s 0 57) "...") s)))

(defn- error-line
  "`in message, got value`; no path when `in` is unknown (`nil`), no value when the entry has none (or the key is
  missing)."
  [{:keys [in value message] :as entry}]
  (str (when in (str (pr-str in) " "))
       (str/replace message #"\.$" "")
       (when (and (contains? entry :value) (not= "missing required key" message))
         (str ", got " (value-str value)))))

(defn- message-of
  "The message of an error listing `errors` (one line each)."
  [errors]
  (let [n (count errors)]
    (str "Invalid input" (if (= 1 n) ": " (str " (" n " errors):\n  ")) (str/join "\n  " (map error-line errors)))))

(defn- key-of
  "The error `:key` of path `in`: its last keyword (`:input` without one)."
  [in]
  (or (last (filter keyword? in)) :input))

(defn throw-errors!
  "Throw one `ex-info` listing `errors` (`{:in :value :message}`, see the namespace doc for its data); `nil` when there
  are none."
  [errors]
  (when (seq errors)
    (let [{:keys [in value]} (first errors)]
      (throw (ex-info (message-of errors) {:key (key-of in) :in in :value value :errors (vec errors)})))))

(defn fail!
  "Throw the error of a check done by hand, shaped and worded as [[throw-errors!]]'s: `data` has `:key`, optionally
  `:in` and `:value` (and anything else useful); `:errors` gets its one entry (`:in` `nil` when unknown)."
  [message data]
  (let [entry (assoc (select-keys data [:value]) :in (:in data) :message message)]
    (throw (ex-info (message-of [entry]) (assoc data :errors [entry])))))
