(ns cljplot2.error
  "The error shape of every input check in cljplot2.

  Every check (see [[cljplot2.schema]]) throws `ex-info` with `{:key k :in path :value v :errors [{:in :value :message} ...]}` and a message listing every error. The namespace has no dependencies on other cljplot2 namespaces, so namespaces that [[cljplot2.schema]] itself requires (e.g. [[cljplot2.graphics.shape]]) can fail the same way.

  Functions: [[throw-errors!]] throws one error listing many entries; [[fail!]] throws the error of a check done by hand."
  (:require [clojure.string :as str]
            [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- value-str
  "Printed form of a value for an error message.

  Parameters:

  - `v` (any): the value.

  Returns its printed text with collections cut to 5 elements and 3 levels, at most 60 characters (longer text ends with `...`)."
  [v]
  (let [s (binding [*print-length* 5 *print-level* 3] (pr-str v))]
    (if (m/> (count s) 60) (str (subs s 0 57) "...") s)))

(defn- error-line
  "One line of an error message.

  Parameters:

  - `entry` (map): an error entry - `:in` (path, `nil` when unknown), `:message` (text, a final period dropped), `:value` (optional).

  Returns `in message, got value`: without the path when `:in` is `nil`, without the value when the entry has none or reports a missing key."
  [{:keys [in value message] :as entry}]
  (str (when in (str (pr-str in) " "))
       (str/replace message #"\.$" "")
       (when (and (contains? entry :value) (not= "missing required key" message))
         (str ", got " (value-str value)))))

(defn- message-of
  "The message of an error listing every entry.

  Parameters:

  - `errors` (sequence of maps): error entries, see [[error-line]].

  Returns `Invalid input: line` for one entry, else the count and one indented line per entry."
  [errors]
  (let [n (count errors)]
    (str "Invalid input" (if (m/one? n) ": " (str " (" n " errors):\n  ")) (str/join "\n  " (map error-line errors)))))

(defn- key-of
  "The error `:key` of a path.

  Parameters:

  - `in` (sequential or `nil`): the path of the first error.

  Returns the last keyword of `in`, `:input` when it has none."
  [in]
  (or (last (filter keyword? in)) :input))

(defn throw-errors!
  "Throws one `ex-info` listing every error entry.

  Parameters:

  - `errors` (sequence of maps): entries `{:in :value :message}`.
  - `k` (keyword, optional): the option at fault, for paths whose last keyword isn't one (e.g. a user's key in `[:variable-names :a]`).

  The data is `{:key :in :value :errors}`: `:key` - `k`, else the last keyword of the first entry's path (`:input` without one); `:in` and `:value` the first entry's, `:errors` all entries. Returns `nil` when `errors` is empty.

  See also [[fail!]]."
  ([errors] (throw-errors! errors nil))
  ([errors k]
   (when (seq errors)
     (let [{:keys [in value]} (first errors)]
       (throw (ex-info (message-of errors) {:key (or k (key-of in)) :in in :value value :errors (vec errors)}))))))

(defn fail!
  "Throws the error of a check done by hand, shaped and worded as the errors of [[throw-errors!]].

  Parameters:

  - `message` (string): what is wrong.
  - `data` (map): `:key` (the option at fault), optionally `:in` (its path) and `:value`, and anything else useful.

  Always throws `ex-info` with `data` plus `:errors` holding one entry (`:in` is `nil` when unknown)."
  [message data]
  (let [entry (assoc (select-keys data [:value]) :in (:in data) :message message)]
    (throw (ex-info (message-of [entry]) (assoc data :errors [entry])))))
