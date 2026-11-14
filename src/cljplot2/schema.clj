(ns cljplot2.schema
  "Input validation with malli: named types shared by the owners' schemas, [[explain!]] - every error of several
  schemas in one `ex-info` - and [[fail!]] - the same error shape for checks done by hand.

  Owners (theme, plot, scales, facets, labels, charts, shingles) declare their schemas as vars; `build` explains them
  once, before any stage runs (see [[cljplot2.pipeline/build]]).

  Error data (`ex-data`): `{:key k :in path :value v :errors [{:in :value :message} ...]}` - `:in` is the path of the
  first error in the input (e.g. `[:theme :legend :gap]`), `:key` its last keyword, `:value` the offending value;
  `:errors` lists every error (`:in` `nil` - location unknown, for some checks done by hand).
  Input maps are closed - unknown keys are errors (\"disallowed key\"), with a spelling suggestion when a known key is
  close - except those that carry user extensions: mappings, chart values, theme `:chart`, custom grids, `:data`."
  (:refer-clojure :exclude [accessor])
  (:require [clojure.string :as str]
            [clojure2d.color :as c]
            [fastmath.core :as m]
            [malli.core :as malli]
            [malli.error :as me]
            [cljplot2.data.accessor :as accessor]
            [cljplot2.graphics.font :as font]
            [cljplot2.graphics.shape :as shape])
  (:import [java.util IdentityHashMap]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; named types

(defn named-type
  "Schema type `type` (qualified keyword, printed in schema forms) of values satisfying `pred`, with error `message`."
  [type pred message]
  (malli/-simple-schema {:type type :pred pred :type-properties {:error/message message}}))

(defn- non-neg-number? [v] (and (number? v) (>= (double v) 0.0)))
(defn- pos-number? [v] (and (number? v) (pos? (double v))))

(defn finite?
  "Is `v` a finite number?"
  [v]
  (and (number? v) (m/valid-double? (double v))))

(defn color?
  "Is `v` `:none` or a color clojure2d resolves?"
  [v]
  (or (= :none v)
      (and (some? v) (not (boolean? v)) (try (some? (c/to-color v)) (catch Exception _ false)))))

(defn solid-color?
  "Is `v` a color clojure2d resolves (not `:none`)?"
  [v]
  (and (not= :none v) (color? v)))

(def non-neg (named-type ::non-neg non-neg-number? "should be a non-negative number"))
(def pos (named-type ::pos pos-number? "should be a positive number"))
(def pos-int (named-type ::pos-int #(and (integer? %) (pos? (long %))) "should be a positive integer"))
(def finite (named-type ::finite finite? "should be a finite number"))
(def fraction (named-type ::fraction #(and (number? %) (<= 0.0 (double %) 1.0)) "should be a number in [0 1]"))
(def bool (named-type ::boolean boolean? "should be true or false"))
(def malli-schema
  (named-type ::malli-schema #(try (some? (malli/schema %)) (catch Exception _ false)) "should be a malli schema"))
(def function (named-type ::function fn? "should be a function"))
(def accessor (named-type ::accessor #(or (keyword? %) (fn? %)
                                         (and (accessor/categorical? %) (let [a (accessor/unwrap %)] (or (keyword? a) (fn? a)))))
                          "should be a keyword, a function or a categorical marker of one"))

(def padding
  (named-type ::padding #(and (vector? %) (= 2 (count %)) (every? non-neg-number? %))
               "should be [lo hi], non-negative numbers"))

(def dash
  (named-type ::dash #(or (= :none %) (and (sequential? %) (seq %) (every? pos-number? %)))
               "should be :none or a non-empty sequence of positive numbers"))

(def ^:private h-aligns #{:left :center :right})
(def ^:private v-aligns #{:top :middle :bottom})

(def align
  (named-type ::align #(and (vector? %) (= 2 (count %)) (contains? h-aligns (% 0)) (contains? v-aligns (% 1)))
               "should be [h v], h one of :left :center :right, v one of :top :middle :bottom"))

(def color (named-type ::color color? "should be a clojure2d color or :none"))

(def cvd-deficiency
  "A clojure2d color vision deficiency (`clojure2d.color/cvd-filter` arguments): a keyword or `[keyword severity]`."
  (named-type ::cvd-deficiency
              (fn [v] (and (or (keyword? v) (and (vector? v) (= 2 (count v)) (keyword? (first v))))
                           (try (some? (if (vector? v) (apply c/cvd-filter v) (c/cvd-filter v))) (catch Exception _ false))))
              "should be a clojure2d color vision deficiency: a keyword (:protanopia, :deuteranopia, :tritanopia, :achromatopsia) or [keyword severity] (:protanomaly, :deuteranomaly, :tritanomaly, :achromatomaly; severity in [0 1])"))
(def solid-color (named-type ::solid-color solid-color? "should be a clojure2d color"))
(def shape (named-type ::shape #(contains? shape/shapes %) (str "should be a shape, one of " (apply str (sort shape/shapes)))))
(def font (named-type ::font font/known-font? "should be a bundled, installed or logical font name, or a java.awt.Font"))
(def font-style (into [:enum] (sort (keys font/font-styles))))

(defn optional
  "Map entry `k` that may be absent."
  [k schema]
  [k {:optional true} schema])

(defn nilable
  "Map entry `k` that may be absent or `nil` (not set)."
  [k schema]
  [k {:optional true} [:maybe schema]])

(defn with-rules
  "`schema` with rules over the whole value. Each rule is `[problem path]`: `problem` returns `nil` for a valid value,
  else the error message; `path` (may be `nil`) is appended to the error's `:in`.
  A rule sees only values valid for `schema` (others get `schema`'s errors), so it may rely on their shape; an
  exception it throws is a bug and propagates."
  [schema & rules]
  (let [valid? (malli/validator schema)]
    (into [:and schema]
          (map (fn [[problem path]]
                 (let [check (fn [v] (when (valid? v) (problem v)))]
                   [:fn (cond-> {:error/fn (fn [{:keys [value]} _] (check value))} path (assoc :error/path path))
                    #(nil? (check %))])))
          rules)))

(defn forbidden
  "Schema rejecting any value of its map entry with `message` (keys reserved for internal use)."
  [message]
  [:fn {:error/message message} (constantly false)])

(defn keyword-or
  "Schema of the keyword `kw` (e.g. `:none`, `:stretch`) or a value of `schema`; errors come from `schema` alone."
  [kw schema]
  [:multi {:dispatch #(if (keyword? %) ::keyword ::value)}
   [::keyword [:= kw]]
   [::value schema]])

;; explaining

(def ^:private ^:const max-cached-explainers 256)

(def ^:private ^IdentityHashMap explainers
  "Schema value (by identity) -> malli explainer; cleared when full, so schemas made per call can't fill memory."
  (IdentityHashMap.))

(defn- explainer
  [schema]
  (locking explainers
    (or (.get explainers schema)
        (let [e (malli/explainer schema)]
          (when (>= (.size explainers) max-cached-explainers) (.clear explainers))
          (.put explainers schema e)
          e))))

(defn- value-str
  [v]
  (let [s (binding [*print-length* 5 *print-level* 3] (pr-str v))]
    (if (> (count s) 60) (str (subs s 0 57) "...") s)))

(defn- spelling-suggestion
  "For an unknown-key `error`: the closest key of its (closed) map schema, or `nil`. Present keys count too, as
  merged inputs (the theme) always hold every known key."
  [error]
  (when (= ::malli/extra-key (:type error))
    (first (me/-most-similar-to #{} (peek (:in error)) (set (map first (malli/entries (:schema error))))))))

(defn- error-entry
  "`{:in :value :message}` of a malli `error` under `prefix`; a rule's `:error/path` extends the path, its value is
  the value there."
  [prefix error]
  (let [path (:error/path (malli/properties (:schema error)))
        suggestion (spelling-suggestion error)]
    {:in (-> (vec prefix) (into (:in error)) (into path))
     :value (if path (get-in (:value error) path) (:value error))
     :message (if suggestion
                (str "unknown key, should be spelled " (pr-str suggestion))
                (or (me/error-message error) "is invalid"))}))

(defn- error-line
  "`in message, got value`; no path when `in` is unknown (`nil`), no value when the entry has none (or the key is
  missing)."
  [{:keys [in value message] :as entry}]
  (str (when in (str (pr-str in) " "))
       (str/replace message #"\.$" "")
       (when (and (contains? entry :value) (not= "missing required key" message))
         (str ", got " (value-str value)))))

(defn- message-of
  [errors]
  (let [n (count errors)]
    (str "Invalid input" (if (= 1 n) ": " (str " (" n " errors):\n  ")) (str/join "\n  " (map error-line errors)))))

(defn- key-of
  [in]
  (or (last (filter keyword? in)) :input))

(defn throw-errors!
  "Throw one `ex-info` listing `errors` (`{:in :value :message}`, see the namespace doc for its data); `nil` when there
  are none."
  [errors]
  (when (seq errors)
    (let [{:keys [in value]} (first errors)]
      (throw (ex-info (message-of errors) {:key (key-of in) :in in :value value :errors (vec errors)})))))

(defn errors
  "Errors (`{:in :value :message}`) of `value` against `schema`, paths prefixed by `in`; empty when valid."
  [schema value in]
  (if-let [explanation ((explainer schema) value)]
    (mapv #(error-entry in %) (:errors explanation))
    []))

(defn explain!
  "Validate every `[schema value in]` of `checks` (`in` - path prefix of the value in the input); throw one `ex-info`
  listing all errors (see the namespace doc for its data), or return `nil`."
  [checks]
  (throw-errors! (into [] (mapcat (fn [[schema value in]] (errors schema value in))) checks)))

(defn fail!
  "Throw the error of a check done by hand, shaped and worded as [[explain!]]'s: `data` has `:key`, optionally `:in`
  and `:value` (and anything else useful); `:errors` gets its one entry (`:in` `nil` when unknown)."
  [message data]
  (let [entry (assoc (select-keys data [:value]) :in (:in data) :message message)]
    (throw (ex-info (message-of [entry]) (assoc data :errors [entry])))))
