(ns cljplot2.schema
  "Input validation with malli: named types shared by the option schemas, map entry helpers documenting options, and one error shape for every check.

  Owners (theme, plot, scales, facets, labels, charts, shingles) declare their schemas as vars; `build` explains them once, before any stage runs (see [[cljplot2.pipeline/build]]). Builders and user-facing data functions check their options first with [[check-options!]].

  Error data (`ex-data`): `{:key k :in path :value v :errors [{:in :value :message} ...]}` - `:in` is the path of the first error in the input (e.g. `[:theme :legend :gap]`), `:key` its last keyword, `:value` the offending value; `:errors` lists every error (`:in` `nil` - location unknown, for some checks done by hand). Input maps are closed - unknown keys are errors (`disallowed key`), with a spelling suggestion when a known key is close - except those that carry user extensions: mappings, chart values, theme `:chart`, custom grids, `:data`.

  Entry properties documenting an option (read by [[cljplot2.describe]], ignored by validation):

  - `:description` - one sentence.
  - `:default` - a constant default of a behaviour option; style options take theme defaults instead.
  - `:cljplot2/default-doc` - the text of a computed default.
  - `:cljplot2/fragment` - the shared group of options the entry belongs to, set by [[in-fragment]].
  - `:cljplot2/entries-fragment` - the shared group documenting the entries of the entry's map (one table for many entries, e.g. text roles).
  - `:cljplot2/section` - the slice of a long option table the entry belongs to (a keyword; in one map every user entry has one or none has; nested entries inherit it), see [[in-section]].
  - `:cljplot2/internal` - not a user option (no table row).

  Schema properties: `:cljplot2/type-doc` (the type's text in option tables, for a large map or composite; named types take it as an argument, see [[named-type]]) and, on a map, `:cljplot2/merged` (a merged input - theme, render options: every key present after merging, users give partial overrides).

  Functions and vars:

  - Named types: [[named-type]], numbers ([[non-neg]], [[pos]], [[pos-int]], [[finite]], [[fraction]], [[open-fraction]], [[finite-pos]], [[finite-non-neg]], [[finite-interval]], [[padding]]), [[bool]], [[function]], [[accessor]], [[malli-schema]], colours ([[color]], [[solid-color]], [[text-color]], [[cvd-deficiency]]), [[dash]] with [[dash-styles]], [[pattern]] with [[pattern-styles]], [[interpolator]], [[align]], [[temporal]], [[shape]], [[font]], [[font-style]]; predicates [[finite?]], [[color?]], [[solid-color?]], [[dash?]], [[pattern?]].
  - Entries: [[optional]], [[nilable]], [[in-fragment]], [[in-section]], [[options-map]]; composite schemas [[with-rules]], [[forbidden]] ([[forbidden?]]), [[keyword-or]].
  - Checking: [[errors]], [[explain!]], [[check-options!]], [[throw-errors!]], [[fail!]] (built by [[cljplot2.error]], which namespaces below this one - e.g. [[cljplot2.graphics.shape]] - use directly)."
  (:refer-clojure :exclude [accessor])
  (:require [clojure.string :as str]
            [clojure2d.color :as c]
            [fastmath.core :as m]
            [malli.core :as malli]
            [malli.error :as me]
            [cljplot2.data.accessor :as accessor]
            [cljplot2.error :as error]
            [cljplot2.graphics.font :as font]
            [cljplot2.graphics.shape :as shape]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; named types

(defn named-type
  "Creates a malli schema type of the values satisfying a predicate, with its own error message and table text.

  Parameters:

  - `type` (qualified keyword): the type's name, printed in schema forms.
  - `pred` (function): the predicate a valid value satisfies.
  - `message` (string or function): the error message, or a function of the invalid value returning it.
  - `type-doc` (string, optional): the type's text in option tables (type property `:cljplot2/type-doc`, see [[cljplot2.describe]]); without it the tables take a string `message` without its leading `should be`.

  Returns a malli schema (an `IntoSchema` value) usable in any schema form."
  ([type pred message] (named-type type pred message nil))
  ([type pred message type-doc]
   (malli/-simple-schema {:type type :pred pred
                          :type-properties (cond-> (if (fn? message)
                                                     {:error/fn (fn [{:keys [value]} _] (message value))}
                                                     {:error/message message})
                                             type-doc (assoc :cljplot2/type-doc type-doc))})))

(defn- non-neg-number?
  "Is `v` a number greater than or equal to zero?"
  [v]
  (and (number? v) (m/not-neg? (double v))))

(defn- pos-number?
  "Is `v` a number greater than zero?"
  [v]
  (and (number? v) (m/pos? (double v))))

(defn finite?
  "Is `v` a finite number (not `NaN`, not infinite)?

  Returns `false` for non-numbers. See also [[finite]]."
  [v]
  (and (number? v) (m/valid-double? v)))

(defn color?
  "Is `v` `:none` or a colour clojure2d resolves (any clojure2d colour representation)?

  Returns `false` for `nil` and booleans, and for values clojure2d can't convert. See also [[solid-color?]], [[color]]."
  [v]
  (or (= :none v)
      (and (some? v) (not (boolean? v)) (try (some? (c/to-color v)) (catch Exception _ false)))))

(defn solid-color?
  "Is `v` a colour clojure2d resolves, `:none` excluded?

  See also [[color?]], [[solid-color]]."
  [v]
  (and (not= :none v) (color? v)))

(def non-neg
  "Schema of a non-negative number."
  (named-type ::non-neg non-neg-number? "should be a non-negative number"))
(def pos
  "Schema of a positive number."
  (named-type ::pos pos-number? "should be a positive number"))
(def pos-int
  "Schema of a positive integer."
  (named-type ::pos-int #(and (integer? %) (m/pos? (long %))) "should be a positive integer"))
(def finite
  "Schema of a finite number, see [[finite?]]."
  (named-type ::finite finite? "should be a finite number"))
(def fraction
  "Schema of a number in `[0 1]`."
  (named-type ::fraction #(and (number? %) (m/<= 0.0 (double %) 1.0)) "should be a number in [0 1]"))
(def open-fraction
  "Schema of a number in `(0 1)`, both ends excluded."
  (named-type ::open-fraction #(and (number? %) (m/< 0.0 (double %) 1.0)) "should be a number in (0 1)"))
(def finite-pos
  "Schema of a positive finite number."
  (named-type ::finite-pos #(and (finite? %) (m/pos? (double %))) "should be a positive finite number"))
(def finite-non-neg
  "Schema of a non-negative finite number."
  (named-type ::finite-non-neg #(and (finite? %) (m/not-neg? (double %))) "should be a non-negative finite number"))
(def finite-interval
  "Schema of an interval `[lo hi]` of finite numbers with `lo < hi`."
  (named-type ::finite-interval #(and (vector? %) (m/== 2 (count %)) (every? finite? %) (m/< (double (% 0)) (double (% 1))))
              "should be [lo hi], finite numbers, lo < hi"))
(def bool
  "Schema of `true` or `false`."
  (named-type ::boolean boolean? "should be true or false"))
(def malli-schema
  "Schema of a value malli accepts as a schema."
  (named-type ::malli-schema #(try (some? (malli/schema %)) (catch Exception _ false)) "should be a malli schema"))
(def function
  "Schema of a function."
  (named-type ::function fn? "should be a function"))
(def accessor
  "Schema of a data accessor: a keyword, a function, or a categorical marker of one (see [[cljplot2.data.accessor/categorical?]])."
  (named-type ::accessor #(or (keyword? %) (fn? %)
                                         (and (accessor/categorical? %) (let [a (accessor/unwrap %)] (or (keyword? a) (fn? a)))))
                          "should be a keyword, a function or a categorical marker of one"))

(def padding
  "Schema of a padding `[lo hi]` of two non-negative numbers."
  (named-type ::padding #(and (vector? %) (m/== 2 (count %)) (every? non-neg-number? %))
              "should be [lo hi], non-negative numbers"))

(def dash-styles
  "Named dash styles: name -> dash lengths in pixels (fixed, not scaled by the line width; names after ggplot linetypes). Every pattern shows a gap within 14 pixels (a legend key), so the styles stay apart in legends; `:solid` draws a solid line, as `:none`."
  {:solid :none
   :dashed [5.0 3.0]
   :dotted [1.0 2.0]
   :dot-dash [1.0 2.0 5.0 2.0]
   :long-dash [10.0 3.0]
   :two-dash [2.0 2.0 6.0 2.0]
   :dot-dot-dash [1.0 2.0 1.0 2.0 5.0 2.0]
   :loose-dot [1.0 4.0]})

(defn dash?
  "Is `v` a dash: `:none`, a name of [[dash-styles]] or a non-empty sequence of positive lengths (pixels)?

  See also [[dash]]."
  [v]
  (or (= :none v) (contains? dash-styles v) (and (sequential? v) (seq v) (every? pos-number? v))))

(def ^:private dash-names-text
  "The names of [[dash-styles]], sorted, as one text for messages."
  (str/join " " (map pr-str (sort (keys dash-styles)))))

(defn- dash-message
  "Error message of an invalid dash.

  Parameters:

  - `value`: the invalid value.

  Returns the message: the accepted forms, preceded by the closest style name of [[dash-styles]] when `value` is a misspelt keyword."
  [value]
  (let [suggestion (when (keyword? value) (first (me/-most-similar-to #{} value (set (keys dash-styles)))))]
    (str (if suggestion (str "unknown dash style, should be spelled " (pr-str suggestion) "; ") "")
         "should be :none, a dash style (" dash-names-text ") or a non-empty sequence of positive numbers")))

(def dash
  "Schema of a dash, see [[dash?]]; a misspelt style name gets a spelling suggestion."
  (malli/-simple-schema {:type ::dash :pred dash?
                         :type-properties {:error/fn (fn [{:keys [value]} _] (dash-message value))
                                           :cljplot2/type-doc (str ":none, a dash style (" dash-names-text
                                                                   ") or a sequence of positive numbers")}}))

(def pattern-styles
  "Named fill patterns: name -> pattern spec (see [[pattern?]]); pixels, fixed. Every pattern shows at least two marks within 14 pixels (a legend key); `:solid` - no pattern (the plain fill)."
  {:solid :solid
   :hatch {:kind :lines :angle 45 :spacing 6.0 :width 1.0}
   :back-hatch {:kind :lines :angle 135 :spacing 6.0 :width 1.0}
   :horizontal {:kind :lines :angle 0 :spacing 6.0 :width 1.0}
   :vertical {:kind :lines :angle 90 :spacing 6.0 :width 1.0}
   :cross {:kind :cross :angle 0 :spacing 6.0 :width 1.0}
   :diagonal-cross {:kind :cross :angle 45 :spacing 6.0 :width 1.0}
   :dots {:kind :dots :spacing 6.0 :size 2.0}
   :dense-dots {:kind :dots :spacing 4.0 :size 1.5}})

(def pattern-angles
  "Angles (degrees) of line and cross patterns: those whose tiles repeat seamlessly."
  #{0 45 90 135})

(defn- pattern-spec?
  "Is `v` a pattern spec map: `{:kind :lines :angle a :spacing s :width w}` or `{:kind :cross ...}` (`a` one of [[pattern-angles]]), or `{:kind :dots :spacing s :size d}` - exactly these keys, positive pixels?"
  [{:keys [kind angle spacing width size] :as v}]
  (and (map? v)
       (case kind
         (:lines :cross) (and (= #{:kind :angle :spacing :width} (set (keys v)))
                              (number? angle) (some #(m/== (double angle) (double %)) pattern-angles)
                              (pos-number? spacing) (pos-number? width))
         :dots (and (= #{:kind :spacing :size} (set (keys v))) (pos-number? spacing) (pos-number? size))
         false)))

(defn pattern?
  "Is `v` a fill pattern: `:solid`, a name of [[pattern-styles]] or a spec map?

  A spec is `{:kind :lines :angle a :spacing s :width w}` or `{:kind :cross ...}` (`a` one of [[pattern-angles]]), or `{:kind :dots :spacing s :size d}` - positive pixels. See also [[pattern]]."
  [v]
  (or (contains? pattern-styles v) (pattern-spec? v)))

(def ^:private pattern-names-text
  "The names of [[pattern-styles]], sorted, as one text for messages."
  (str/join " " (map pr-str (sort (keys pattern-styles)))))

(defn- pattern-message
  "Error message of an invalid fill pattern.

  Parameters:

  - `value`: the invalid value.

  Returns the message: the accepted forms, preceded by the closest name of [[pattern-styles]] when `value` is a misspelt keyword."
  [value]
  (let [suggestion (when (keyword? value) (first (me/-most-similar-to #{} value (set (keys pattern-styles)))))]
    (str (if suggestion (str "unknown pattern, should be spelled " (pr-str suggestion) "; ") "")
         "should be a pattern name (" pattern-names-text "), {:kind :lines|:cross :angle 0|45|90|135 :spacing s :width w}"
         " or {:kind :dots :spacing s :size d} (positive pixels)")))

(def pattern
  "Schema of a fill pattern, see [[pattern?]]; a misspelt name gets a spelling suggestion."
  (malli/-simple-schema {:type ::pattern :pred pattern?
                         :type-properties {:error/fn (fn [{:keys [value]} _] (pattern-message value))
                                           :cljplot2/type-doc (str "a pattern name (" pattern-names-text ") or a pattern map")}}))

(def interpolation-methods
  "fastmath `interpolation` methods taking one-dimensional `xs`, `ys` (the 2D grid methods excluded): line curves and `:interpolated` position scales."
  #{:linear :cubic :monotone :pchip :akima :neville :divided-difference :polynomial :sprague :step-before
    :step-after :step :b-spline :barycentric :loess :cubic-smoothing :pava :cir :microsphere-projection :shepard
    :rbf :kriging :gp})

(def interpolator
  "Schema of `:interpolator`: a fastmath 1D method of [[interpolation-methods]] or a function `(fn [xs ys & params] f)`."
  (named-type ::interpolator #(or (fn? %) (contains? interpolation-methods %))
              (str "should be a function (fn [xs ys & params] f) or a fastmath 1D interpolation method: "
                   (str/join " " (sort interpolation-methods)))
              "a fastmath 1D interpolation method keyword or a function (fn [xs ys & params] f)"))

(def ^:private h-aligns
  "Horizontal alignments."
  #{:left :center :right})
(def ^:private v-aligns
  "Vertical alignments."
  #{:top :middle :bottom})

(def align
  "Schema of an alignment `[h v]`: `h` one of `:left` `:center` `:right`, `v` one of `:top` `:middle` `:bottom`."
  (named-type ::align #(and (vector? %) (m/== 2 (count %)) (contains? h-aligns (% 0)) (contains? v-aligns (% 1)))
              "should be [h v], h one of :left :center :right, v one of :top :middle :bottom"))

(def color
  "Schema of a colour or `:none`, see [[color?]]."
  (named-type ::color color? "should be a clojure2d color or :none"))

(def cvd-deficiency
  "Schema of a clojure2d colour vision deficiency (`clojure2d.color/cvd-filter` arguments): a keyword or `[keyword severity]`."
  (named-type ::cvd-deficiency
              (fn [v] (and (or (keyword? v) (and (vector? v) (m/== 2 (count v)) (keyword? (first v))))
                          (try (some? (if (vector? v) (apply c/cvd-filter v) (c/cvd-filter v))) (catch Exception _ false))))
              "should be a clojure2d color vision deficiency: a keyword (:protanopia, :deuteranopia, :tritanopia, :achromatopsia) or [keyword severity] (:protanomaly, :deuteranomaly, :tritanomaly, :achromatomaly; severity in [0 1])"))
(def solid-color
  "Schema of a colour, `:none` excluded, see [[solid-color?]]."
  (named-type ::solid-color solid-color? "should be a clojure2d color"))
(def temporal
  "Schema of a `java.time` value (a date, a date-time, an instant, a time)."
  (named-type ::temporal #(instance? java.time.temporal.Temporal %) "should be a java.time value" "a java.time value"))
(def text-color
  "Schema of a text colour: a clojure2d colour or `:contrast` - black or white, whichever reads on what's behind the text (see [[cljplot2.graphics.draw/text-color]])."
  (named-type ::text-color #(or (= :contrast %) (solid-color? %)) "should be a clojure2d color or :contrast"
              "a clojure2d color or :contrast"))
(def shape
  "Schema of a point shape: a built-in character, keyword or a shape map (see [[cljplot2.graphics.shape/shape-problem]], whose message is the error)."
  (named-type ::shape shape/shape? shape/shape-problem "a shape character, keyword or shape map"))
(def font
  "Schema of a font: a bundled, installed or logical font name, or a `java.awt.Font`."
  (named-type ::font font/known-font? "should be a bundled, installed or logical font name, or a java.awt.Font"))
(def font-style
  "Schema of a font style: one of the keys of [[cljplot2.graphics.font/font-styles]]."
  (into [:enum] (sort (keys font/font-styles))))

(defn optional
  "Creates a map entry that may be absent.

  Parameters:

  - `k` (keyword): the entry's key.
  - `props` (map, optional): its documenting properties (`:description`, `:default` ... - see the namespace doc).
  - `schema`: the schema of its value.

  Returns `[k props schema]` with `:optional true` in `props`. See also [[nilable]]."
  ([k schema] (optional k nil schema))
  ([k props schema] [k (assoc props :optional true) schema]))

(defn nilable
  "Creates a map entry that may be absent or `nil` (not set).

  Parameters:

  - `k` (keyword): the entry's key.
  - `props` (map, optional): its documenting properties (see the namespace doc).
  - `schema`: the schema of a set value.

  Returns `[k props [:maybe schema]]` with `:optional true` in `props`. See also [[optional]]."
  ([k schema] (nilable k nil schema))
  ([k props schema] [k (assoc props :optional true) [:maybe schema]]))

(defn- with-entry-property
  "Sets one property on every map entry.

  Parameters:

  - `prop` (keyword): the entry property.
  - `v`: its value.
  - `entries` (sequence): map entries, `[k props schema]` or `[k schema]`.

  Returns a vector of the entries, each `[k props schema]` with `prop` set to `v`."
  [prop v entries]
  (mapv (fn [[k a b :as entry]]
          (if (m/== 3 (count entry))
            [k (assoc a prop v) b]
            [k {prop v} a]))
        entries))

(defn in-fragment
  "Tags map entries as members of a shared group of options.

  Parameters:

  - `fragment` (keyword): the group, registered in [[cljplot2.describe/fragments]].
  - `entries` (sequence): map entries, `[k props schema]` or `[k schema]`.

  Returns a vector of the entries with `:cljplot2/fragment fragment`; option tables leave them to the group's own table. See also [[in-section]]."
  [fragment entries]
  (with-entry-property :cljplot2/fragment fragment entries))

(defn in-section
  "Puts map entries in one slice of an option table.

  For entries shared by maps sliced differently (e.g. a point style: one table of scatter, a section of cloud).

  Parameters:

  - `section` (keyword): the table slice.
  - `entries` (sequence): map entries, `[k props schema]` or `[k schema]`.

  Returns a vector of the entries with `:cljplot2/section section`. See also [[in-fragment]]."
  [section entries]
  (with-entry-property :cljplot2/section section entries))

(defn with-rules
  "Extends a schema with rules over the whole value (relations between keys).

  Parameters:

  - `schema`: the base schema.
  - `rules`: each `[problem path]` - `problem` a function of the value returning `nil` when valid, else the error message; `path` (may be `nil`) appended to the error's `:in`.

  Returns `[:and schema rule-schemas...]`. A rule sees only values valid for `schema` (others get `schema`'s errors), so it may rely on their shape; an exception a rule throws is a bug and propagates."
  [schema & rules]
  (let [valid? (malli/validator schema)]
    (into [:and schema]
          (map (fn [[problem path]]
                 (let [check (fn [v] (when (valid? v) (problem v)))]
                   [:fn (cond-> {:error/fn (fn [{:keys [value]} _] (check value))} path (assoc :error/path path))
                    #(nil? (check %))])))
          rules)))

(defn forbidden
  "Creates a schema rejecting every value of its map entry - a key reserved for internal use or not taken.

  Parameters:

  - `message` (string): the error message.

  Returns a `:fn` schema marked `:cljplot2/forbidden`: not an option, no table row (see [[forbidden?]])."
  [message]
  [:fn {:error/message message :cljplot2/forbidden true} (constantly false)])

(defn forbidden?
  "Is `schema` (a malli schema or form) a [[forbidden]] one, also under `:maybe` (as [[nilable]] wraps it)?"
  [schema]
  (let [s (malli/schema schema)]
    (if (= :maybe (malli/type s))
      (forbidden? (first (malli/children s)))
      (true? (:cljplot2/forbidden (malli/properties s))))))

(defn keyword-or
  "Creates a schema of one keyword or a value of another schema.

  Parameters:

  - `kw` (keyword): the accepted keyword, e.g. `:none`, `:stretch`.
  - `schema`: the schema of the other values.

  Returns a `:multi` schema; a non-keyword value's errors come from `schema` alone."
  [kw schema]
  [:multi {:dispatch #(if (keyword? %) ::keyword ::value)}
   [::keyword [:= kw]]
   [::value schema]])

;; explaining

(def ^{:private true :const true :tag 'long} max-cached-explainers
  "The most explainers [[explainers]] holds before it is emptied."
  256)

(defonce ^{:private true :doc "Malli explainers by schema value; emptied when it holds [[max-cached-explainers]], so schemas made per call can't fill memory."}
  explainers
  (atom {}))

(defn- explainer
  "Returns the malli explainer of `schema`, cached by schema value (see [[explainers]])."
  [schema]
  (or (get @explainers schema)
      (let [e (malli/explainer schema)]
        (swap! explainers (fn [cache] (assoc (if (m/>= (count cache) max-cached-explainers) {} cache) schema e)))
        e)))

(defn- abbreviated-key
  "Finds the known key an unknown key abbreviates (such as `:cols` for `:columns`).

  Parameters:

  - `unknown`: the unknown key.
  - `known` (collection): the known keys.

  Returns the only keyword of `known` whose name starts with the first three characters of `unknown`'s name, else `nil` (also for names shorter than three characters)."
  [unknown known]
  (let [n (str (if (keyword? unknown) (name unknown) unknown))]
    (when (m/>= (count n) 3)
      (let [matches (filter #(and (keyword? %) (str/starts-with? (name %) (subs n 0 3)) (not= % unknown)) known)]
        (when (m/one? (count matches)) (first matches))))))

(defn- swapped-neighbours?
  "Is string `a` string `b` with two adjacent characters swapped (a typo such as `szie` for `size`)?"
  [^String a ^String b]
  (and (m/== (count a) (count b))
       (let [diffs (filterv (fn [^long id] (not= (get a id) (get b id)))
                            (range (count a)))]
         (and (m/== 2 (count diffs))
              (m/== (m/inc (long (diffs 0))) (long (diffs 1)))
              (= (get a (diffs 0)) (get b (diffs 1)))
              (= (get a (diffs 1)) (get b (diffs 0)))))))

(defn- transposed-key
  "Finds the known key an unknown key misspells by two swapped neighbours (`:szie` for `:size`), a typo malli's similarity misses in short keys.

  Parameters:

  - `unknown`: the unknown key; a non-keyword has no match.
  - `known` (collection): the known keys.

  Returns the only keyword of `known` matching (see [[swapped-neighbours?]]), else `nil`."
  [unknown known]
  (when (keyword? unknown)
    (let [matches (filter #(and (keyword? %) (swapped-neighbours? (name unknown) (name %))) known)]
      (when (m/one? (count matches)) (first matches)))))

(defn- spelling-suggestion
  "Suggests the key an unknown key probably meant.

  Parameters:

  - `error` (map): a malli error.

  Returns, for an unknown-key error, the closest key of its closed map schema, else the only key it is a transposition of ([[transposed-key]]) or abbreviates ([[abbreviated-key]]); `nil` otherwise. Keys present in the value count too, as merged inputs (the theme) always hold every known key."
  [error]
  (when (= ::malli/extra-key (:type error))
    (let [unknown (peek (:in error))
          known (set (map first (malli/entries (:schema error))))]
      (or (first (me/-most-similar-to #{} unknown known))
          (transposed-key unknown known)
          (abbreviated-key unknown known)))))

(defn- error-entry
  "Converts a malli error to an error entry.

  Parameters:

  - `prefix` (sequence): the path of the checked value in the input.
  - `error` (map): a malli error.

  Returns `{:in :value :message}`: `:in` - `prefix` with the error's path (and a [[with-rules]] rule's `:error/path`), `:value` - the value there, `:message` - malli's message, or a spelling suggestion for an unknown key (see [[spelling-suggestion]])."
  [prefix error]
  (let [path (:error/path (malli/properties (:schema error)))
        suggestion (spelling-suggestion error)]
    {:in (-> (vec prefix) (into (:in error)) (into path))
     :value (if path (get-in (:value error) path) (:value error))
     :message (if suggestion
                (str "unknown key, should be spelled " (pr-str suggestion))
                (or (me/error-message error) "is invalid"))}))

(def ^{:arglists '([errors])} throw-errors!
  "Throws one `ex-info` listing `errors` (`{:in :value :message}` entries), shaped as in the namespace doc; returns `nil` for no errors. See [[cljplot2.error/throw-errors!]]."
  error/throw-errors!)

(defn errors
  "Validates a value against a schema, collecting every error.

  Parameters:

  - `schema`: a malli schema (explainers are cached by schema value).
  - `value`: the checked value.
  - `in` (sequence): the path of `value` in the input, prefixing each error's `:in`.

  Returns a vector of `{:in :value :message}` errors, empty when `value` is valid. See also [[explain!]]."
  [schema value in]
  (if-let [explanation ((explainer schema) value)]
    (mapv #(error-entry in %) (:errors explanation))
    []))

(defn explain!
  "Validates several values, throwing one error for all of them.

  Parameters:

  - `checks` (sequence): each `[schema value in]`, `in` - the path of the value in the input.

  Returns `nil` when every value is valid; else throws one `ex-info` listing all errors (its data in the namespace doc). See also [[errors]]."
  [checks]
  (throw-errors! (into [] (mapcat (fn [[schema value in]] (errors schema value in))) checks)))

(defn options-map
  "Builds the closed options schema of a builder from groups of map entries.

  Parameters:

  - `groups`: sequences of map entries in order - the builder's own entries first, then those of the functions it forwards options to, then the shared ones.

  Returns `[:map {:closed true} entries...]`; an entry whose key an earlier group already has is left out (the earlier - the builder's own - entry wins). No groups give an empty closed map."
  [& groups]
  (first (reduce (fn [[schema seen] entry]
                   (if (seen (first entry)) [schema seen] [(conj schema entry) (conj seen (first entry))]))
                 [[:map {:closed true}] #{}] (apply concat groups))))

(defn check-options!
  "Validates the options of a builder or user-facing function before it reads its data.

  Parameters:

  - `schema`: the options schema (the var's `:cljplot2/options`).
  - `options` (map or `nil`): the given options; `nil` means none.

  Returns `nil` when valid; else throws one `ex-info` like [[explain!]], each error at its option's path (`:key` - the option). A missing required option is listed after the other errors: an unknown key - usually its misspelling - comes first, with its spelling suggestion."
  [schema options]
  (throw-errors! (vec (sort-by #(if (= "missing required key" (:message %)) 1 0)
                               (errors schema (if (nil? options) {} options) [])))))

(def ^{:arglists '([message data])} fail!
  "Throws the error of a check done by hand, shaped and worded as [[explain!]]'s: `message` and `data` with at least `:key` (the option), optionally `:in` and `:value`. See [[cljplot2.error/fail!]]."
  error/fail!)
