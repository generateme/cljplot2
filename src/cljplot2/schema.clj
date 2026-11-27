(ns cljplot2.schema
  "Input validation with malli: named types shared by the option schemas, map entry helpers documenting options, and one error shape for every check.

  Owners (theme, plot, scales, facets, labels, charts, shingles) declare their schemas as vars; `build` explains them once, before any stage runs (see [[cljplot2.pipeline/build]]). Builders and user-facing data functions check their options first with [[check-options!]].

  Error data (`ex-data`): `{:key k :in path :value v :errors [{:in :value :message} ...]}` - `:in` is the path of the first error in the input (e.g. `[:theme :legend :gap]`), `:key` its last keyword, `:value` the offending value; `:errors` lists every error (`:in` `nil` - location unknown, for some checks done by hand). Input maps are closed - unknown keys are errors (`disallowed key`), with a spelling suggestion when a known key is close - except those that carry user extensions: mappings, chart values, theme `:chart`, custom grids, `:data`.

  Entry properties documenting an option (read by [[cljplot2.describe]], ignored by validation):

  - `:description` - one or two full sentences (capitalised or opening with code, ending with a period), at most 300 characters, saying what the option controls; `:cljplot2/default-doc` reads the same way.
  - `:default` - a constant default of a behaviour option; style options take theme defaults instead.
  - `:cljplot2/default-doc` - the text of a computed default.
  - `:cljplot2/value-meanings` - meanings of literal values, `{value meaning}` (`nil` too, when it isn't the default; also on an `:enum` schema's properties, shared by its entries); `:cljplot2/form-meanings` - meanings of the other accepted forms, `{form meaning}` (a form is a named type's keyword, e.g. `::function`, or a malli type, e.g. `:vector`); `:cljplot2/other-values` - one meaning for the values without their own (a long list).
  - `:cljplot2/example` - the option's example value, printed as code (a form such as `'(r/rng :mersenne 7)` for a value that doesn't print); `:cljplot2/example-context` - other options the example needs to be valid or to act (e.g. `{:band :bootstrap}`).
  - `:cljplot2/fragment` - the shared group of options the entry belongs to, set by [[in-fragment]].
  - `:cljplot2/entries-fragment` - the shared group documenting the entries of the entry's map (documented once for many entries, e.g. text roles).
  - `:cljplot2/section` - the slice of a long list of options (its own group of cards) the entry belongs to (a keyword; in one map every user entry has one or none has; nested entries inherit it), see [[in-section]].
  - `:cljplot2/internal` - not a user option (no card).

  Schema properties: `:cljplot2/type-doc` (the type's text in an option row's `:type`, for a large map or composite; named types take it as an argument, see [[named-type]]) and, on a map, `:cljplot2/type-name` (its short name on option cards, e.g. a mapping's), `:cljplot2/entries-fragment` (the shared group documenting its keys - its card form links there, e.g. a text style) and `:cljplot2/merged` (a merged input - theme, render options: every key present after merging, users give partial overrides). Named types also take `:cljplot2/type-name` (a short name; `:cljplot2/type-plural` when its plural isn't regular) and `:cljplot2/type-meaning` (what it is, with an example) - the schema layer's are [[named-types]], part of the book's registry `cljplot2.describe/documented-types`; `:cljplot2/value-set` (the literal values a predicate type accepts: a set, listed sorted, or a vector, listed in its order) and `:cljplot2/other-types` (the named types of its other values), so its forms can be listed; `:cljplot2/value-meanings` and `:cljplot2/form-meanings` (value and form meanings shared by every entry of the type).

  Functions and vars:

  - Named types: [[named-type]], numbers ([[non-neg]], [[pos]], [[pos-int]], [[finite]], [[fraction]], [[open-fraction]], [[finite-pos]], [[finite-non-neg]], [[finite-interval]], [[padding]]), [[bool]], [[function]], [[accessor]], [[malli-schema]], colours ([[color]], [[solid-color]], [[text-color]], [[cvd-deficiency]]), [[number-format]], [[count-format]] (with [[text-formatter]]), [[dash]] with [[dash-styles]], [[pattern]] with [[pattern-styles]], [[interpolator]], [[align]], [[temporal]], [[shape]], [[font]], [[font-style]]; [[named-types]] - the schema layer's documented named types; predicates [[finite?]] (with [[finite-double]]), [[color?]], [[solid-color?]], [[dash?]], [[pattern?]].
  - Entries: [[optional]], [[nilable]], [[in-fragment]], [[in-section]], [[options-map]]; composite schemas [[with-rules]] ([[rule?]]), [[forbidden]] ([[forbidden?]]), [[keyword-or]]; entry keys of a map schema [[entry-keys]], of every map inside a composite [[nested-map-keys]].
  - Checking: [[errors]], [[explain!]], [[check-options!]], [[throw-errors!]], [[fail!]] (built by [[cljplot2.error]], which namespaces below this one - e.g. [[cljplot2.graphics.shape]] - use directly)."
  (:refer-clojure :exclude [accessor])
  (:require [clojure.string :as str]
            [clojure2d.color :as c]
            [fastmath.core :as m]
            [malli.core :as malli]
            [malli.error :as me]
            [cljplot2.cache :as cache]
            [cljplot2.data.accessor :as accessor]
            [cljplot2.error :as error]
            [cljplot2.graphics.font :as font]
            [cljplot2.graphics.shape :as shape]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; named types

(defn named-type
  "Creates a malli schema type of the values satisfying a predicate, with its own error message and type text.

  Parameters:

  - `type-name` (qualified keyword): the type's name, printed in schema forms.
  - `pred` (function): the predicate a valid value satisfies.
  - `message` (string or function): the error message, or a function of the invalid value returning it.
  - `doc` (string or map, optional): a string is the type's text in option rows (type property `:cljplot2/type-doc`, see [[cljplot2.describe]]); without it the rows take a string `message` without its leading `should be`. A map holds documenting type properties (see the namespace doc): `:cljplot2/type-doc`, `:cljplot2/type-name`, `:cljplot2/type-meaning`, `:cljplot2/value-set`, `:cljplot2/other-types`, `:cljplot2/value-meanings`.

  Returns a malli schema (an `IntoSchema` value) usable in any schema form."
  ([type-name pred message] (named-type type-name pred message nil))
  ([type-name pred message doc]
   (malli/-simple-schema {:type type-name :pred pred
                          :type-properties (merge (if (fn? message)
                                                    {:error/fn (fn [{:keys [value]} _] (message value))}
                                                    {:error/message message})
                                                  (if (string? doc) {:cljplot2/type-doc doc} doc))})))

(defn- non-neg-number?
  "Tests whether `v` is a number greater than or equal to zero.

  Parameters:

  - `v`: any value.

  Returns true for such a number, else false."
  [v]
  (and (number? v) (m/not-neg? (double v))))

(defn- pos-number?
  "Tests whether `v` is a number greater than zero.

  Parameters:

  - `v`: any value.

  Returns true for such a number, else false."
  [v]
  (and (number? v) (m/pos? (double v))))

(defn finite?
  "Tests whether `v` is a finite number (not `NaN`, not infinite).

  Parameters:

  - `v`: any value.

  Returns true for a finite number, false for other numbers and non-numbers. See also [[finite]]."
  [v]
  (and (number? v) (m/valid-double? v)))

(defn finite-double
  "Gives a value as a double when it is a finite number.

  Parameters:

  - `v`: any value.

  Returns `v` as a double for a finite number; `nil` for `NaN`, infinite numbers and non-numbers. See also [[finite?]]."
  [v]
  (when (finite? v) (double v)))

(defn color?
  "Tests whether `v` is `:none` or a colour clojure2d resolves (any clojure2d colour representation).

  Parameters:

  - `v`: any value.

  Returns true for `:none` or a resolvable colour; false for `nil`, booleans and values clojure2d can't convert. See also [[solid-color?]], [[color]]."
  [v]
  (or (= :none v)
      (and (some? v) (not (boolean? v)) (try (some? (c/to-color v)) (catch Exception _ false)))))

(defn solid-color?
  "Tests whether `v` is a colour clojure2d resolves, `:none` excluded.

  Parameters:

  - `v`: any value.

  Returns true for a resolvable colour other than `:none`, else false. See also [[color?]], [[solid-color]]."
  [v]
  (and (not= :none v) (color? v)))

(def non-neg
  "Schema of a non-negative number."
  (named-type ::non-neg non-neg-number? "should be a non-negative number"
              {:cljplot2/type-name "non-negative number" :cljplot2/type-meaning "A number zero or greater, e.g. `0` or `2.5`."}))
(def pos
  "Schema of a positive number."
  (named-type ::pos pos-number? "should be a positive number"
              {:cljplot2/type-name "positive number" :cljplot2/type-meaning "A number greater than zero, e.g. `0.5`."}))
(def pos-int
  "Schema of a positive integer."
  (named-type ::pos-int #(and (integer? %) (m/pos? (long %))) "should be a positive integer"
              {:cljplot2/type-name "positive integer" :cljplot2/type-meaning "A whole number from 1 up, e.g. `200`."}))
(def finite
  "Schema of a finite number, see [[finite?]]."
  (named-type ::finite finite? "should be a finite number"
              {:cljplot2/type-name "finite number" :cljplot2/type-meaning "A number that is neither infinite nor NaN, e.g. `-3.2`."}))
(def fraction
  "Schema of a number in `[0 1]`."
  (named-type ::fraction #(and (number? %) (m/<= 0.0 (double %) 1.0)) "should be a number in [0 1]"
              {:cljplot2/type-name "fraction" :cljplot2/type-meaning "A number from 0 to 1, both ends included, e.g. `0.3`."}))
(def open-fraction
  "Schema of a number in `(0 1)`, both ends excluded."
  (named-type ::open-fraction #(and (number? %) (m/< 0.0 (double %) 1.0)) "should be a number in (0 1)"
              {:cljplot2/type-name "open fraction" :cljplot2/type-meaning "A number strictly between 0 and 1, e.g. `0.95`."}))
(def finite-pos
  "Schema of a positive finite number."
  (named-type ::finite-pos #(and (finite? %) (m/pos? (double %))) "should be a positive finite number"
              {:cljplot2/type-name "positive finite number" :cljplot2/type-meaning "A finite number greater than zero, e.g. `1.5`."}))
(def finite-non-neg
  "Schema of a non-negative finite number."
  (named-type ::finite-non-neg #(and (finite? %) (m/not-neg? (double %))) "should be a non-negative finite number"
              {:cljplot2/type-name "non-negative finite number" :cljplot2/type-meaning "A finite number zero or greater, e.g. `0` or `12`."}))
(def finite-interval
  "Schema of an interval `[lo hi]` of finite numbers with `lo < hi`."
  (named-type ::finite-interval #(and (vector? %) (m/== 2 (count %)) (every? finite? %) (m/< (double (% 0)) (double (% 1))))
              "should be [lo hi], finite numbers, lo < hi"
              {:cljplot2/type-name "interval" :cljplot2/type-meaning "Two finite numbers `[lo hi]` with `lo` below `hi`, e.g. `[0 10]`."}))
(def bool
  "Schema of `true` or `false`."
  (named-type ::boolean boolean? "should be true or false"
              {:cljplot2/type-name "boolean" :cljplot2/type-meaning "Either `true` or `false`."}))
(def malli-schema
  "Schema of a value malli accepts as a schema."
  (named-type ::malli-schema #(try (some? (malli/schema %)) (catch Exception _ false)) "should be a malli schema"
              {:cljplot2/type-name "malli schema" :cljplot2/type-meaning "Any value malli accepts as a schema, e.g. `[:int {:min 0}]`."}))
(def function
  "Schema of a function."
  (named-type ::function fn? "should be a function"
              {:cljplot2/type-name "function" :cljplot2/type-meaning "A Clojure function; its option says what it takes and returns, e.g. `(fn [x] (* 2 x))`."}))
(def accessor
  "Schema of a data accessor: a keyword, a function, or a categorical marker of one (see [[cljplot2.data.accessor/categorical?]])."
  (named-type ::accessor #(or (keyword? %) (fn? %)
                                         (and (accessor/categorical? %) (let [a (accessor/unwrap %)] (or (keyword? a) (fn? a)))))
                          "should be a keyword, a function or a categorical marker of one"
                          {:cljplot2/type-name "accessor" :cljplot2/type-meaning "How a value comes from a row: a key such as `:x`, a function of the row such as `#(* 2 (:x %))`, or either wrapped in `levels/categorical` to treat numbers or dates as levels."}))

(def padding
  "Schema of a padding `[lo hi]` of two non-negative numbers."
  (named-type ::padding #(and (vector? %) (m/== 2 (count %)) (every? non-neg-number? %))
              "should be [lo hi], non-negative numbers"
              {:cljplot2/type-name "padding" :cljplot2/type-meaning "Two non-negative fractions of the data span `[lo hi]`: the room added at the low and the high end, e.g. `[0.05 0]`."}))

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
  "Tests whether `v` is a dash: `:none`, a name of [[dash-styles]] or a non-empty sequence of positive lengths (pixels).

  Parameters:

  - `v`: any value.

  Returns truthy for a dash, falsey (`false` or `nil`) otherwise; an empty sequence isn't a dash. See also [[dash]]."
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
                                                                   ") or a sequence of positive numbers")
                                           :cljplot2/type-name "dash"
                                           :cljplot2/type-meaning "How a line is dashed: a style name such as `:dashed` or `:dotted`, a vector of dash and gap lengths in pixels such as `[5 3]`, or `:none` (also `:solid`) for a solid line."}}))

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

(def ^:private pattern-angles
  "Angles (degrees) of line and cross patterns: those whose tiles repeat seamlessly."
  #{0 45 90 135})

(defn- pattern-spec?
  "Tests whether `v` is a pattern spec map: `{:kind :lines :angle a :spacing s :width w}` or `{:kind :cross ...}` (`a` one of [[pattern-angles]]), or `{:kind :dots :spacing s :size d}` - exactly these keys, positive pixels.

  Parameters:

  - `v`: any value.

  Returns truthy for such a map, falsey otherwise (an unknown `:kind` or an extra key included)."
  [{:keys [kind angle spacing width size] :as v}]
  (and (map? v)
       (case kind
         (:lines :cross) (and (= #{:kind :angle :spacing :width} (set (keys v)))
                              (number? angle) (some #(m/== (double angle) (double %)) pattern-angles)
                              (pos-number? spacing) (pos-number? width))
         :dots (and (= #{:kind :spacing :size} (set (keys v))) (pos-number? spacing) (pos-number? size))
         false)))

(defn pattern?
  "Tests whether `v` is a fill pattern: `:solid`, a name of [[pattern-styles]] or a spec map.

  A spec is `{:kind :lines :angle a :spacing s :width w}` or `{:kind :cross ...}` (`a` one of [[pattern-angles]]), or `{:kind :dots :spacing s :size d}` - positive pixels.

  Parameters:

  - `v`: any value.

  Returns truthy for a pattern, falsey otherwise. See also [[pattern]]."
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
                                           :cljplot2/type-doc (str "a pattern name (" pattern-names-text ") or a pattern map")
                                           :cljplot2/type-name "fill pattern"
                                           :cljplot2/type-meaning "A pattern drawn over a filled mark: a name such as `:hatch` or `:dots`, or a map such as `{:kind :lines :angle 45 :spacing 6 :width 1}`."}}))

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
              {:cljplot2/type-name "interpolator" :cljplot2/type-meaning "A fastmath 1D interpolation method such as `:cubic` or `:monotone`, or a function of the points' `xs` and `ys` (and any parameters) that returns the curve as a function from x to y." :cljplot2/type-doc "a fastmath 1D interpolation method keyword or a function (fn [xs ys & params] f)"}))

(def ^:private h-aligns
  "Horizontal alignments."
  #{:left :center :right})
(def ^:private v-aligns
  "Vertical alignments."
  #{:top :middle :bottom})

(def align
  "Schema of an alignment `[h v]`: `h` one of `:left` `:center` `:right`, `v` one of `:top` `:middle` `:bottom`."
  (named-type ::align #(and (vector? %) (m/== 2 (count %)) (contains? h-aligns (% 0)) (contains? v-aligns (% 1)))
              "should be [h v], h one of :left :center :right, v one of :top :middle :bottom"
              {:cljplot2/type-name "alignment" :cljplot2/type-meaning "A pair `[h v]`: `h` is `:left`, `:center` or `:right` and `v` is `:top`, `:middle` or `:bottom`, e.g. `[:center :middle]`."}))

(def color
  "Schema of a colour or `:none`, see [[color?]]."
  (named-type ::color color? "should be a clojure2d color or :none"
              {:cljplot2/type-name "colour" :cljplot2/type-meaning "A solid colour, or `:none`." :cljplot2/value-set #{:none} :cljplot2/other-types [::solid-color] :cljplot2/value-meanings {:none "Draws nothing."} :cljplot2/form-meanings {::solid-color "Any other colour, e.g. `:black`."}}))

(def cvd-deficiency
  "Schema of a clojure2d colour vision deficiency (`clojure2d.color/cvd-filter` arguments): a keyword or `[keyword severity]`."
  (named-type ::cvd-deficiency
              (fn [v] (and (or (keyword? v) (and (vector? v) (m/== 2 (count v)) (keyword? (first v))))
                          (try (some? (if (vector? v) (apply c/cvd-filter v) (c/cvd-filter v))) (catch Exception _ false))))
              "should be a clojure2d color vision deficiency: a keyword (:protanopia, :deuteranopia, :tritanopia, :achromatopsia) or [keyword severity] (:protanomaly, :deuteranomaly, :tritanomaly, :achromatomaly; severity in [0 1])"
              {:cljplot2/type-name "colour vision deficiency" :cljplot2/type-meaning "A simulated deficiency: `:protanopia`, `:deuteranopia`, `:tritanopia` or `:achromatopsia`, or a pair `[kind severity]` of an `-anomaly` kind and a severity from 0 to 1, e.g. `[:deuteranomaly 0.5]`."}))
(def solid-color
  "Schema of a colour, `:none` excluded, see [[solid-color?]]."
  (named-type ::solid-color solid-color? "should be a clojure2d color"
              {:cljplot2/type-name "solid colour" :cljplot2/type-meaning "Any clojure2d colour: a name such as `:steelblue`, a hex string such as `#0072b2`, an integer such as `0x0072b2`, a vector of channels such as `[0 114 178]`, or `(c/color 0 114 178 128)` with an alpha."}))
(def temporal
  "Schema of a `java.time` value (a date, a date-time, an instant, a time)."
  (named-type ::temporal #(instance? java.time.temporal.Temporal %) "should be a java.time value"
              {:cljplot2/type-name "date or time" :cljplot2/type-plural "dates or times" :cljplot2/type-meaning "A `java.time` value: a date, a date-time, an instant or a time, e.g. `(java.time.LocalDate/of 2024 1 31)`." :cljplot2/type-doc "a java.time value"}))
(def text-color
  "Schema of a text colour: a clojure2d colour or `:contrast` - black or white, whichever reads on what's behind the text (see [[cljplot2.graphics.draw/text-color]])."
  (named-type ::text-color #(or (= :contrast %) (solid-color? %)) "should be a clojure2d color or :contrast"
              {:cljplot2/type-name "text colour" :cljplot2/type-meaning "A solid colour, or `:contrast`." :cljplot2/type-doc "a clojure2d color or :contrast" :cljplot2/value-set #{:contrast} :cljplot2/other-types [::solid-color] :cljplot2/value-meanings {:contrast "Black or white, whichever reads better on the fill behind the text."} :cljplot2/form-meanings {::solid-color "Any other colour, e.g. `:black`."}}))

(defn- formats?
  "Tests whether a format string formats a sample value.

  Parameters:

  - `sample`: the value tried.
  - `v`: any value.

  Returns true for a string `java.lang.String/format` accepts with `sample` as its one argument, else false."
  [sample v]
  (and (string? v) (try (format v sample) true (catch Exception _ false))))

(def number-format
  "Schema of the text format of a number: a `java.lang.String/format` pattern formatting a double (e.g. `%.1f`) or a function of the number giving its text (see [[text-formatter]])."
  (named-type ::number-format #(or (fn? %) (formats? 1.5 %))
              "should be a format string of a number (e.g. \"%.1f\") or a function"
              {:cljplot2/type-name "number format" :cljplot2/type-meaning "A format string such as `%.1f` (see `format`), or a function of the number that returns its text." :cljplot2/type-doc "a number format string or a function"}))

(def count-format
  "Schema of the text format of a count: a `java.lang.String/format` pattern formatting a long (e.g. `n=%d`) or a function of the count giving its text (see [[text-formatter]])."
  (named-type ::count-format #(or (fn? %) (formats? 1 %))
              "should be a format string of a count (e.g. \"n=%d\") or a function"
              {:cljplot2/type-name "count format" :cljplot2/type-meaning "A format string for a whole number such as `n=%d`, or a function of the count that returns its text." :cljplot2/type-doc "a count format string or a function"}))

(defn text-formatter
  "Gives the function a text format describes.

  Parameters:

  - `format-spec`: a format string of one argument or a function of the value (see [[number-format]], [[count-format]]), or `nil`.

  Returns a function of a value giving its text: `(format format-spec v)` for a string, the function itself, `nil` for `nil`."
  [format-spec]
  (if (string? format-spec) #(format format-spec %) format-spec))
(def shape
  "Schema of a point shape: a built-in character or a shape map (see [[cljplot2.graphics.shape/shape-problem]], whose message is the error)."
  (named-type ::shape shape/shape? shape/shape-problem {:cljplot2/type-name "point shape" :cljplot2/type-meaning "A built-in shape character such as `\\o`, or a map `{:path svg-path}`, `{:text string}`, `{:svg source}` or `{:image image}`." :cljplot2/type-doc "a shape character or shape map"}))
(def font
  "Schema of a font: a bundled, installed or logical font name, or a `java.awt.Font`."
  (named-type ::font font/known-font? "should be a bundled, installed or logical font name, or a java.awt.Font"
              {:cljplot2/type-name "font" :cljplot2/type-meaning "A font name - the bundled `DejaVu Sans Mono`, an installed family or a logical font such as `SansSerif` - or a `java.awt.Font`."}))
(def font-style
  "Schema of a font style: one of the keys of [[cljplot2.graphics.font/font-styles]]."
  (into [:enum] (sort (keys font/font-styles))))

(def named-types
  "The schema layer's documented named types: type keyword -> schema, each with its `:cljplot2/type-name` and `:cljplot2/type-meaning` - the part of the book's registry (`cljplot2.describe/documented-types`) defined here."
  (into {} (map (fn [s] [(malli/type s) s]))
        [non-neg pos pos-int finite fraction open-fraction finite-pos finite-non-neg finite-interval bool malli-schema
         function accessor padding dash pattern interpolator align color cvd-deficiency solid-color temporal text-color
         number-format count-format shape font]))

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

  Returns a vector of the entries with `:cljplot2/fragment fragment`; option cards leave them to the group's own cards. See also [[in-section]]."
  [fragment entries]
  (with-entry-property :cljplot2/fragment fragment entries))

(defn in-section
  "Puts map entries in one slice of a map's options (a group of cards).

  For entries shared by maps sliced differently (e.g. a point style: all of scatter's options, a section of cloud's).

  Parameters:

  - `section` (keyword): the slice.
  - `entries` (sequence): map entries, `[k props schema]` or `[k schema]`.

  Returns a vector of the entries with `:cljplot2/section section`. See also [[in-fragment]]."
  [section entries]
  (with-entry-property :cljplot2/section section entries))

(defn with-rules
  "Extends a schema with rules over the whole value (relations between keys).

  Parameters:

  - `schema`: the base schema.
  - `rules`: each `[problem path]` - `problem` a function of the value returning `nil` when valid, else the error message; `path` (may be `nil`) appended to the error's `:in`.

  Returns `[:and schema rule-schemas...]`, each rule a `:fn` schema marked `:cljplot2/rule` (see [[rule?]]). A rule sees only values valid for `schema` (others get `schema`'s errors), so it may rely on their shape; an exception a rule throws is a bug and propagates."
  [schema & rules]
  (let [valid? (malli/validator schema)]
    (into [:and schema]
          (map (fn [[problem path]]
                 (let [check (fn [v] (when (valid? v) (problem v)))]
                   [:fn (cond-> {:error/fn (fn [{:keys [value]} _] (check value)) :cljplot2/rule true} path (assoc :error/path path))
                    #(nil? (check %))])))
          rules)))

(defn rule?
  "Tests whether a schema is a rule of [[with-rules]] (a relation over the whole value, stated by its entry's description, not a type).

  Parameters:

  - `s`: a malli schema.

  Returns true for a rule, else false."
  [s]
  (true? (:cljplot2/rule (malli/properties (malli/schema s)))))

(defn forbidden
  "Creates a schema rejecting every value of its map entry - a key reserved for internal use or not taken.

  Parameters:

  - `message` (string): the error message.

  Returns a `:fn` schema marked `:cljplot2/forbidden`: not an option, no card (see [[forbidden?]])."
  [message]
  [:fn {:error/message message :cljplot2/forbidden true} (constantly false)])

(defn forbidden?
  "Tests whether a schema is a [[forbidden]] one, also under `:maybe` (as [[nilable]] wraps it).

  Parameters:

  - `schema`: a malli schema or form.

  Returns true for a forbidden schema, else false."
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

(def ^{:private true :const true :tag 'long} explainer-cache-limit
  "The most explainers [[explainers]] keeps."
  256)

(defonce ^{:private true :doc "Malli explainers by schema value, the least recently used evicted beyond [[explainer-cache-limit]] (schemas made per call can't fill memory)."}
  explainers
  (cache/lru-cache explainer-cache-limit))

(defn- explainer
  "Gives the malli explainer of a schema, cached by schema value (see [[explainers]]).

  Parameters:

  - `schema`: a malli schema or form.

  Returns the explainer, a function of a value giving its errors or `nil`."
  [schema]
  (cache/lookup-or-compute explainers schema malli/explainer))

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
  "Tests whether string `a` is string `b` with two adjacent characters swapped (a typo such as `szie` for `size`).

  Parameters:

  - `a`, `b` (strings): the unknown and the known key names.

  Returns true for exactly one swapped pair of neighbours, else false (equal strings and other lengths included)."
  [^String a ^String b]
  (and (m/== (count a) (count b))
       (let [diffs (filterv (fn [^long id] (not= (get a id) (get b id)))
                            (range (count a)))]
         (and (m/== 2 (count diffs))
              (m/== (m/long-inc (long (diffs 0))) (long (diffs 1)))
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

(def ^{:arglists '([errors] [errors k])} throw-errors!
  "Throws one `ex-info` listing errors, shaped as in the namespace doc.

  Parameters:

  - `errors` (sequence): `{:in :value :message}` entries.
  - `k` (keyword, optional): the error `:key`, else the last keyword of the first entry's path.

  Returns `nil` for no errors. See [[cljplot2.error/throw-errors!]]."
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

(defn entry-keys
  "Gives the keys of the entries of a map schema.

  Parameters:

  - `schema`: a map schema - a vector `[:map props? entries...]` (as [[options-map]] builds) or a malli schema; `nil` - none.

  Returns a sequence of the entry keys in order; empty for `nil`.

  See also [[options-map]]."
  [schema]
  (cond
    (nil? schema) ()
    (vector? schema) (keep #(when (vector? %) (first %)) (rest schema))
    :else (map first (malli/children (malli/schema schema)))))

(defn nested-map-keys
  "Gives the entry keys of every map schema inside a schema.

  Composite schemas (`:maybe`, `:and`, `:or` ...) are searched through; a `:map` gives its entries' keys (maps nested as entry values aren't searched).

  Parameters:

  - `schema`: a malli schema or form; `nil` - none.

  Returns a set of keys; empty for `nil` or a schema without maps.

  See also [[entry-keys]]."
  [schema]
  (if (nil? schema)
    #{}
    (letfn [(map-keys [s] (if (= :map (malli/type s))
                            (map first (malli/entries s))
                            (mapcat (fn [c] (when (malli/schema? c) (map-keys c))) (malli/children s))))]
      (set (map-keys (malli/schema schema))))))

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
  "Throws the error of a check done by hand, shaped and worded as [[explain!]]'s.

  Parameters:

  - `message` (string): what is wrong.
  - `data` (map): `:key` (the option at fault), optionally `:in` (its path) and `:value`, and anything else useful.

  Always throws `ex-info`. See [[cljplot2.error/fail!]]."
  error/fail!)
