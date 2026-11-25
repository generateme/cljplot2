(ns cljplot2.scale.aesthetic
  "Aesthetic scales: the non-positional dimensions `:color`, `:shape`, `:size`, `:dash` and `:pattern` mapped from data.

  `:pattern` - fill patterns of filled marks; charts without fills (points, plain lines) use it only to group, as `:shape` on lines.

  Mapping: an accessor (keyword or function), `nil` = not mapped. A row's value is kept when it is valid
  ([[cljplot2.data.rows/valid-value?]]) or `nil` - the NA level, drawn with the NA attribute and listed last as `NA`
  (plot `:groups {:na false}` drops such rows instead). Other values (NaN, infinite, ...) are dropped.

  Scale spec - plot `:scales {dim spec}`:

  * `:kind` - `:ordinal`, `:linear`, `:binned` or `:identity` (both below); default `:linear` for numeric `:color` / `:size` data or a numeric explicit
    domain of two values, otherwise `:ordinal` (`:shape`, `:dash` and `:pattern` are always ordinal or identity); numeric data with more than
    [[cljplot2.data.rows/max-levels]] distinct values can be ordinal only with an explicit `:domain`
  * `:domain` - ordinal: distinct levels (default: data levels, sorted - in the charts' order when every chart's
    extent is `:ordered`, e.g. function labels; numbers equal as doubles, `1931` = `1931.0`);
    linear / binned: `[lo hi]` (default: data min, max); `nil` at one end (`[0 nil]`) - that end from the data
    (`[nil nil]` - no domain; without data, or with the data entirely beyond the given end - `ex-info {:key :domain}`)
  * `:range` - default from theme `:palette` (`:color`, `:gradient`, `:shape`, `:size`, `:dash`, `:pattern`):
    ordinal `:color` - palette name or colors (cycled; with `:spread` also a gradient); linear / binned `:color` -
    palette name, colors (a gradient), a clojure2d gradient name or a function of `t` in `[0 1]`; `:shape` - shapes (cycled); `:size` - `[min max]` (ordinal levels evenly spread); `:dash` - dashes (cycled, see
    [[cljplot2.schema/dash]]); `:pattern` - patterns (cycled, see [[cljplot2.schema/pattern]])
    or a level map `{level attribute}` (ordinal scales; every level needs an entry, extra keys are ignored)
  * `:kind :binned` (`:color` and `:size`, never inferred) - numeric values coloured or sized by bin: `:bins` - a count of equal bins
    over the domain (default 5), a breaks vector (its ends are the domain), a bin method (`:sturges`, ...) or quantile
    bins `{:quantiles q :estimation e}` computed from the data values (see [[cljplot2.data.levels/bins]]; with an
    explicit `:domain` its ends are the outer breaks); bins `[a b)`, the last `[a b]`; bin
    colors - the range resampled to one color per bin (clojure2d `resample`: a palette of exactly that many colors
    gives its colors; one bin - e.g. one distinct value - the gradient's middle, as a linear scale); legend - a stepped color bar; bin sizes - bin `i` of `n` at `(i + 1) / n` of the `:range [min max]`
    (in area with `:area`; the first bin above `min`, so `[0 max]` leaves none invisible), legend - one entry per bin labelled `a – b`
  * `:kind :identity` (never inferred) - the values are the attributes (colors, shapes, sizes); no domain, range or
    legend; groups in first-appearance order; an invalid value throws `{:key :mapping}`
  * `:spread true` - ordinal `:color`: the range resampled to one color per level (instead of its first colors)
  * `:midpoint m` - linear / binned `:color`: `m` at the gradient's middle, both sides scaled by the larger distance
    to it (ggplot `gradient2`)
  * `:transform` - linear `:color`: values placed along the gradient by `:sqrt`, `:log` (base 10), `:sq` (square) or
    `:exp`, or by a function of the value (lattice `trans`, ggplot `trans`), e.g. counts with a few large ones; the
    domain stays in data units (`:sqrt` and `:sq` need `lo >= 0`, `:log` `lo > 0`; any transform must be finite at
    the domain ends - else `ex-info {:key :transform}`), the color bar too (its ticks in data units, spaced by the
    transform). A function must be monotonic over the domain (the caller's responsibility): the color bar inverts it
    numerically
  * `:reverse true` - the attributes in reverse (linear `t` → `1 - t`, ordinal / binned attributes reversed)
  * `:area true` - linear / ordinal / binned `:size`: the shape's area (∝ size²), not its size, linear in `t` - size
    `√(a² + t·(b² − a²))` for `:range [a b]` (ggplot `scale_size`); with `:domain [0 hi]` and `:range [0 max]` the
    area is proportional to the value (ggplot `scale_size_area`)
  * `:ticks` - linear `:size` / `:color`: the legend's entries or color bar ticks - a count (nice ticks), or the
    values themselves (see [[explicit-ticks]]; drawn as given - no added `:midpoint` tick)
  * `:integer-ticks true` - linear `:size` / `:color`: counted ticks only at whole numbers (see [[integer-ticks]]),
    e.g. for counts or degrees; an explicit `:ticks` vector and a `:midpoint` tick are drawn as given; a log colour
    bar keeps only its whole powers of ten (one tick on a domain like [0.01 1])
  * `:formatter` - legend labels (function); `:title` - legend title (`nil` - no title); `:legend` - `false` hides it
  * dates - `:color` mapped to `java.time` values gets a `:linear` scale over time, as numbers do (domain in epoch
    milliseconds, see [[cljplot2.data.rows/x-key]]; an explicit `:domain` of two dates; color bar ticks are dates);
    `:midpoint`, `:transform`, `:integer-ticks` and `:binned` need numbers

  [[scale-spec]] gives plain data (with `:na-level` - the scale has an NA level); it is also the finalized scale: no
  pixel range, `nice` or padding. [[attribute-fn]] maps data values to style attributes: NA (`nil`) to the NA
  attribute; values outside the domain (not a level, or outside `[lo hi]`) to `nil` - such points are skipped.
  Charts drop values outside an explicit domain already at compute ([[in-domain?]]), counted in `:dropped`, so
  extents only cover what is drawn.

  Functions: dimensions [[dims]], [[mapped-dims]], [[computed-aesthetics]], [[computed-scale-attributes]]; schemas [[scale-spec-schema]], [[palette-schema]]; values [[continuous-value?]], [[continuous-values?]], [[valid-attribute?]], [[valid-value?]], [[identity-scale?]], [[in-domain?]], [[kept-value?]], [[value-extent]]; scales [[inferred-kind]], [[scale-spec]], [[value-transform]]; attributes [[attribute-fn]], [[bin-attributes]], [[palette-color-fn]]; legends [[legend-entries]], [[explicit-ticks]], [[integer-ticks]].

  Also: [[dim?]], [[level-key]], [[kinds]], [[transform-names]]."
  (:require [clojure2d.color :as c]
            [wadogo.scale :as s]
            [fastmath.core :as m]
            [cljplot2.data.levels :as levels]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.shape :as shape]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def dims
  "Aesthetic dimensions, in legend order."
  [:color :shape :size :dash :pattern])

(def ^:private ordinal-only
  "Aesthetic dimensions whose scales are never linear or binned (their attributes have no order)."
  #{:shape :dash :pattern})

(def dim?
  "Predicate (a set) of the aesthetic dimensions [[dims]]."
  (set dims))

(def ^:private kinds
  "Aesthetic scale kinds."
  #{:ordinal :linear :binned :identity})

(def ^:private transform-names
  "The named linear color `:transform`s."
  #{:sqrt :log :sq :exp})

(def ^{:private true :const true :tag 'long} default-bins
  "Equal bins of a binned scale without `:bins`."
  5)

(def ^{:private true :const true :tag 'long} size-legend-ticks
  "Tick count of a linear size legend without `:ticks`."
  5)

(defn mapped-dims
  "Gives the aesthetic dimensions a mapping maps.

  Parameters:

  - `mapping` (map): dimension -> accessor.

  Returns a vector of the [[dims]] with a non-`nil` accessor, in legend order."
  [mapping]
  (filterv #(some? (get mapping %)) dims))

(defn computed-scale-attributes
  "Gives the scale attributes a computed aesthetic suggests.

  Parameters:

  - `v`: the aesthetic's [[computed-aesthetics]] value - a title (string or `nil`) or a map.

  Returns `{:title v}` for a title; for a map its `:title`, `:legend` and `:range` (e.g. a diverging palette)."
  [v]
  (if (map? v) (select-keys v [:title :legend :range]) {:title v}))

(defn computed-aesthetics
  "Gives the aesthetics a chart computes itself.

  A chart's `:computed-aesthetics` is a map, or a function of the chart's options and the mapping returning one (e.g. a colour computed only when nothing groups the chart, a title named after a mapped accessor); a top-level function keeps chart values comparable as data.

  Parameters:

  - `chart` (map): a chart value.
  - `mapping` (map): the chart's merged mapping.

  Returns `{dim title-or-attributes}` (see [[computed-scale-attributes]]); `{}` without computed aesthetics."
  [chart mapping]
  (let [computed (:computed-aesthetics chart)]
    (or (if (fn? computed) (computed (:options chart) mapping) computed) {})))

(defn- palette-colors
  "Gives the colours of a palette.

  Parameters:

  - `p`: a clojure2d palette name or a sequence of colours.

  Returns a vector of the colours, or `nil` when `p` isn't a palette (an unknown name, an empty sequence, or a value that isn't a solid colour)."
  [p]
  (let [colors (cond
                 (keyword? p) (try (vec (c/palette p)) (catch Exception _ nil))
                 (sequential? p) (vec p))]
    (when (and (seq colors) (every? schema/solid-color? colors))
      colors)))

(def ^:private palette
  "Schema of a palette: a palette name or a non-empty sequence of colours."
  (schema/named-type ::palette (comp some? palette-colors) "should be a palette name or a non-empty sequence of colors"))

(def ^:private size-range
  "Schema of a size range `[min max]`: non-negative numbers, `min <= max`."
  (schema/named-type ::size-range #(and (vector? %) (m/== 2 (count %)) (every? (fn [v] (and (number? v) (m/not-neg? (double v)))) %)
                                        (m/<= (double (% 0)) (double (% 1))))
                     "should be [min max], non-negative numbers, min <= max"))

(def ^:private shapes
  "Schema of a shape range: shapes, cycled."
  [:sequential {:min 1} schema/shape])

(def ^:private dashes
  "Schema of a dash range: dashes, cycled."
  [:sequential {:min 1} schema/dash])

(def ^:private patterns
  "Schema of a pattern range: fill patterns, cycled."
  [:sequential {:min 1} schema/pattern])

(def ^:private gradient
  "Schema of the theme gradient: a function of `t` or a palette."
  [:multi {:dispatch fn?} [true schema/function] [false palette]])

(defn- gradient-name?
  "Tests whether a value names a clojure2d gradient (and not a palette).

  Parameters:

  - `p`: any value.

  Returns a truthy value for a keyword naming a gradient but no palette."
  [p]
  (and (keyword? p) (nil? (palette-colors p)) (try (fn? (c/gradient p)) (catch Exception _ false))))

(def ^:private color-range
  "Schema of a colour range: a function of `t`, a palette or gradient name, or colours."
  [:multi {:dispatch fn?}
   [true schema/function]
   [false (schema/named-type ::color-range #(or (some? (palette-colors %)) (gradient-name? %))
                             "should be a palette or gradient name, or a non-empty sequence of colors")]])

(defn- level-map
  "Gives the schema of a level map `:range`.

  Parameters:

  - `attribute-schema`: schema of one attribute.

  Returns the schema of a non-empty map `{level attribute}`."
  [attribute-schema]
  [:map-of {:min 1} any? attribute-schema])

(defn- range-schema
  "Gives the schema of the scale `:range` of an aesthetic.

  Parameters:

  - `k` (keyword): the dimension, one of [[dims]].

  Returns the schema of a level map (ordinal scales, see [[level-map]]) or the dimension's plain range (colours, shapes, a size range, dashes, patterns)."
  [k]
  (let [plain (case k
                :color color-range
                :shape shapes
                :size size-range
                :dash dashes
                :pattern patterns)
        attribute (case k
                    :color schema/solid-color
                    :shape schema/shape
                    :size schema/non-neg
                    :dash schema/dash
                    :pattern schema/pattern)]
    [:multi {:dispatch map?} [true (level-map attribute)] [false plain]]))

(def palette-schema
  "Theme `:palette` (aesthetic defaults)."
  [:map {:closed true}
   [:color {:description "ordinal colours: a palette name or colours"} palette]
   [:gradient {:description "linear and binned colours: colours or a palette name"} gradient]
   [:shape {:description "point shapes, cycled"} shapes]
   [:size {:description "size range [min max] in pixels"} size-range]
   [:dash {:description "dash styles, cycled"} dashes]
   [:pattern {:description "fill patterns, cycled"} patterns]
   [:na {:description "colour of NA values"} schema/solid-color]])

(defn level-key
  "Gives the identity of an ordinal level, shared by aesthetic scales and grouping (see [[cljplot2.data.group]]).

  Parameters:

  - `v`: a level.

  Returns a number as a double (so `1931` equals `1931.0`), any other value as itself."
  [v]
  (if (number? v) (double v) v))

(defn- continuous-value?
  "Tests whether a value can sit on a linear scale of an aesthetic.

  Parameters:

  - `k` (keyword): the dimension.
  - `v`: the value.

  Returns true for a number, and for a `java.time` value of `:color`.

  See also [[continuous-values?]]."
  [k v]
  (or (number? v) (and (= :color k) (data/temporal? v))))

(defn continuous-values?
  "Tests whether the values of an aesthetic suit a linear scale.

  Parameters:

  - `k` (keyword): the dimension.
  - `values` (sequence): data values, `nil` (NA) ignored.

  Returns a truthy value when some value is non-`nil` and every non-`nil` one is a [[continuous-value?]], else a falsy one."
  [k values]
  (let [present (remove nil? values)]
    (and (seq present) (every? #(continuous-value? k %) present))))

(defn- continuous-extent?
  "Tests whether a data extent of an aesthetic is continuous.

  Parameters:

  - `k` (keyword): the dimension.
  - `extent` (map or `nil`): a [[cljplot2.data.rows/value-extent]].

  Returns true for a numeric extent, and a temporal one of `:color`."
  [k extent]
  (or (= :numeric (:kind extent)) (and (= :color k) (= :temporal (:kind extent)))))

(defn- explicit-kind
  "Gives the kind of an aesthetic scale with an explicit domain.

  Parameters:

  - `k` (keyword): the dimension.
  - `spec` (map): the scale spec - `:kind`, `:type`, `:domain`.

  Returns the spec's `:kind`; `:ordinal` with `:type :categorical` (e.g. two years as levels); else `:linear` for a domain of two numbers (not `:shape` / `:dash` / `:pattern`) or two dates (`:color`) - one of them may be `nil` (a partial domain), else `:ordinal`."
  [k {:keys [kind domain] scale-type :type}]
  (or kind
      (when (= :categorical scale-type) :ordinal)
      (if (and (not (ordinal-only k)) (m/== 2 (count domain)) (some some? domain)
               (every? #(or (nil? %) (continuous-value? k %)) domain))
        :linear
        :ordinal)))

(defn- domain-problem
  "Explains why the explicit domain of an aesthetic scale spec is invalid.

  Parameters:

  - `k` (keyword): the dimension.
  - `spec` (map): the scale spec; its kind as in [[explicit-kind]].

  Returns the error message, or `nil` for a valid or absent `:domain` (`[nil nil]` - absent): ordinal - distinct non-`nil` levels; linear / binned - `[lo hi]`, finite numbers (dates for `:color`), `lo < hi`, or a partial `[lo nil]` / `[nil hi]` (the open end from the data); identity - anything (rejected elsewhere)."
  [k spec]
  (when-let [domain (data/given-domain (:domain spec))]
    (case (explicit-kind k spec)
      :ordinal (when-not (and (seq domain) (not-any? nil? domain) (apply distinct? (map level-key domain)))
                 "ordinal domain should be a non-empty sequence of distinct non-nil values")
      (:linear :binned) (let [[lo hi] domain
                              end-ok? #(or (nil? %) (and (continuous-value? k %) (schema/finite? (data/x-key %))))]
                          (or (when (data/partial-domain? domain) (data/partial-domain-problem domain))
                              (when-not (and (m/== 2 (count domain)) (end-ok? lo) (end-ok? hi)
                                             (or (nil? lo) (nil? hi) (m/< (double (data/x-key lo)) (double (data/x-key hi)))))
                                "linear domain should be [lo hi], finite numbers (or dates for :color), lo < hi; nil at one end - the data's")))
      :identity nil)))

(defn- option-problems
  "Gives the options of an aesthetic scale spec its kind doesn't allow, or that contradict each other.

  Parameters:

  - `k` (keyword): the dimension.
  - `kind` (keyword): the scale's resolved kind.
  - `spec` (map): the scale spec - checked `:range` (a level map, a function or gradient name), `:spread`, `:midpoint`, `:transform`, `:reverse`, `:area`, `:bins`, `:ticks`, `:integer-ticks` and an identity scale's `:domain` / `:range`.

  Returns `[[key message] ...]`, in a fixed order; `[]` when the options hold."
  [k kind {:keys [spread midpoint area bins domain transform ticks] integer-ticks? :integer-ticks reversed? :reverse attribute-range :range :as spec}]
  (let [level-map? (map? attribute-range)
        bins-domain (when (and (vector? bins) (m/>= (count bins) 2)) [(double (first bins)) (double (peek bins))])
        domain-ends (data/given-domain (:domain spec))]
    (cond-> []
      (and (#{:linear :binned} kind) (ordinal-only k)) (conj [:kind (str (name k) " scale can't be " kind)])
      (and (= kind :identity) (some? domain)) (conj [:domain "an identity scale takes no :domain"])
      (and (= kind :identity) (contains? spec :range)) (conj [:range "an identity scale takes no :range"])
      (and level-map? (not= kind :ordinal) (not= kind :identity))
      (conj [:range "a level map :range needs an ordinal scale (declare numbers categorical: {:type :categorical})"])
      (and (or (fn? attribute-range) (gradient-name? attribute-range)) (= kind :ordinal) (not spread))
      (conj [:range "a color :range function or gradient name needs a linear or binned scale, or :spread"])
      (and spread (or (not= k :color) (not= kind :ordinal))) (conj [:spread ":spread needs an ordinal :color scale"])
      (and spread level-map?) (conj [:spread ":spread can't be combined with a level map :range"])
      (and (some? midpoint) (or (not= k :color) (not (#{:linear :binned} kind))))
      (conj [:midpoint ":midpoint needs a linear or binned :color scale"])
      (and (some? transform) (or (not= k :color) (not= kind :linear)))
      (conj [:transform ":transform needs a linear :color scale"])
      (and reversed? (= kind :identity)) (conj [:reverse "an identity scale can't be reversed"])
      (and area (or (not= k :size) (not (#{:linear :ordinal :binned} kind)))) (conj [:area ":area needs a linear, ordinal or binned :size scale"])
      (and reversed? level-map?) (conj [:reverse ":reverse can't be combined with a level map :range"])
      (and area level-map?) (conj [:area ":area can't be combined with a level map :range"])
      (and (some? bins) (not= kind :binned)) (conj [:bins ":bins needs :kind :binned"])
      (and (some? ticks) (or (not (#{:color :size} k)) (not= kind :linear)))
      (conj [:ticks ":ticks needs a linear :size or :color scale (its legend entries or color bar ticks)"])
      (and integer-ticks? (or (not (#{:color :size} k)) (not= kind :linear)))
      (conj [:integer-ticks ":integer-ticks needs a linear :size or :color scale (its legend entries or color bar ticks)"])
      (and (= kind :binned) bins-domain domain-ends (m/== 2 (count domain-ends))
           (some (fn [[^double b d]] (and (number? d) (not (m/== b (double d))))) (map vector bins-domain domain-ends)))
      (conj [:bins "breaks should span the explicit :domain (first and last break = its ends)"]))))

(defn- known-kind
  "Gives the kind of an aesthetic scale known from its spec alone.

  Parameters:

  - `k` (keyword): the dimension.
  - `spec` (map): the scale spec.

  Returns the [[explicit-kind]] when the spec has a `:kind` or a sequential `:domain` (not `[nil nil]`), else `nil`."
  [k spec]
  (when (or (:kind spec) (data/given-domain (:domain spec)))
    (explicit-kind k spec)))

(defn- option-rule
  "Gives the schema rule of one option of an aesthetic scale spec.

  Parameters:

  - `k` (keyword): the dimension.
  - `option-key` (keyword): the option.

  Returns a [[cljplot2.schema/with-rules]] rule `[problem [option-key]]` reporting the [[option-problems]] message of `option-key` when the kind is known from the spec (see [[known-kind]])."
  [k option-key]
  [(fn [spec]
     (when-let [kind (known-kind k spec)]
       (some (fn [[problem-key message]] (when (= option-key problem-key) message)) (option-problems k kind spec))))
   [option-key]])

(defn scale-spec-schema
  "Gives the schema of the plot scale spec of an aesthetic (see the namespace doc).

  Parameters:

  - `k` (keyword): the dimension, one of [[dims]].

  Returns a closed map schema with rules: a valid `:domain` ([[domain-problem]]) and the options allowed by the kind known from the spec ([[option-problems]])."
  [k]
  (apply schema/with-rules
         [:map {:closed true :cljplot2/type-doc "an aesthetic scale spec"}
          (schema/optional :kind {:cljplot2/section :levels :description "scale kind: :ordinal (levels), :linear (a gradient or size range), :binned (:color or :size by bins) or :identity (values are the attributes)"
                                  :cljplot2/default-doc ":linear for numeric :color / :size data, else :ordinal"}
                           (into [:enum] (sort kinds)))
          (schema/optional :type {:cljplot2/section :levels :description "declare numbers or dates categorical (ordinal levels) or force a numeric scale"}
                           [:enum :categorical :numeric])
          (schema/nilable :domain {:cljplot2/section :levels :description "ordinal: the levels in order; linear / binned: [lo hi] (nil at an end - the data's, e.g. [0 nil])"
                                   :cljplot2/default-doc "the data's levels (sorted) or [min max]"}
                          [:sequential any?])
          (schema/optional :range {:cljplot2/section :range :description "attributes the domain maps to: palette, gradient, shapes, sizes [min max], dashes or patterns, or a level map"
                                   :cljplot2/default-doc "theme :palette"}
                           (range-schema k))
          (schema/optional :spread {:cljplot2/section :range-mapping :description "ordinal :color: the range resampled to one colour per level" :default false}
                           schema/bool)
          (schema/optional :midpoint {:cljplot2/section :range-mapping :description "linear / binned :color: the value at the gradient's middle (a diverging scale)"}
                           schema/finite)
          (schema/optional :transform {:cljplot2/section :range-mapping :description "linear :color: values placed along the gradient by their square root, logarithm, square, exponential or a monotonic function"}
                           [:or (into [:enum] (sort transform-names)) schema/function])
          (schema/optional :reverse {:cljplot2/section :range-mapping :description "the attributes in reverse order" :default false} schema/bool)
          (schema/optional :area {:cljplot2/section :range-mapping :description ":size: the mark's area, not its size, grows linearly with the value" :default false}
                           schema/bool)
          (schema/optional :bins {:cljplot2/section :bins :description "binned :color / :size: a bin count, breaks, a bin method or quantile bins"
                                  :cljplot2/default-doc "5 equal bins"}
                           levels/bins)
          (schema/optional :formatter {:cljplot2/section :legend :description "legend labels, a function of the level or value"} schema/function)
          (schema/nilable :ticks {:cljplot2/section :legend :description "linear :size / :color: the legend entries or colour bar ticks - a count or the values (dates for a :color scale over dates)"}
                          [:multi {:dispatch sequential?} [true [:sequential [:or schema/finite schema/temporal]]] [false schema/pos-int]])
          (schema/optional :integer-ticks {:cljplot2/section :legend :description "linear :size / :color: counted legend entries or colour bar ticks only at whole numbers (an explicit :ticks vector or :midpoint tick is drawn as given)" :default false}
                           schema/bool)
          (schema/nilable :title {:cljplot2/section :legend :description "legend title (nil - none)" :cljplot2/default-doc "the mapped column's name"}
                          string?)
          (schema/nilable :legend {:cljplot2/section :legend :description "show the scale's legend" :default true} schema/bool)]
         [#(domain-problem k %) [:domain]]
         (map #(option-rule k %) [:kind :domain :range :spread :midpoint :transform :reverse :area :bins :ticks :integer-ticks])))

(defn- resolve-range
  "Gives the concrete range of an aesthetic from its validated range value.

  Parameters:

  - `k` (keyword): the dimension.
  - `r`: the range value (spec `:range` or the theme palette's).

  Returns a level map or a function as given; for `:color` the gradient (function) of a gradient name, else the colours of a palette name or sequence; shapes, dashes and patterns as a vector; a size range as given."
  [k r]
  (cond
    (or (map? r) (fn? r)) r
    (and (= k :color) (gradient-name? r)) (c/gradient r)
    (= k :color) (palette-colors r)
    (ordinal-only k) (vec r)
    :else r))

(defn- first-of-levels
  "Gives values without later repeats of a level.

  Parameters:

  - `vs` (sequence): the values.

  Returns a vector of the values in their order, each level ([[level-key]]) at its first occurrence only."
  [vs]
  (first (reduce (fn [[acc seen] v]
                   (let [level-id (level-key v)]
                     (if (contains? seen level-id) [acc seen] [(conj acc v) (conj seen level-id)])))
                 [[] #{}] vs)))

(defn- ordinal-domain
  "Gives the levels of an ordinal aesthetic scale.

  Parameters:

  - `k` (keyword): the dimension.
  - `explicit` (sequence or `nil`): the spec's `:domain`.
  - `extent` (map or `nil`): the merged data extent - `:values`, `:ordered`.

  Returns the explicit domain; else the data's levels (one value per level, see [[level-key]]) in their order when the extent is `:ordered`, otherwise sorted ([[cljplot2.data.rows/sorted-levels]]).

  Throws `ex-info {:key :domain}` without an explicit domain for data of too many levels (no `:values`)."
  [k explicit extent]
  (cond
    explicit (vec explicit)
    (and (:ordered extent) (:values extent)) (first-of-levels (:values extent))
    (:values extent) (data/sorted-levels k (vals (reduce (fn [acc v] (if (contains? acc (level-key v)) acc (assoc acc (level-key v) v)))
                                                         {} (:values extent)))
                                         :domain)
    :else (schema/fail! (str "Too many distinct values for an ordinal scale (more than " data/max-levels
                             "); give an explicit :domain.")
                        {:key :domain :in [:scales k :domain]})))

(defn- linear-domain
  "Gives the domain of a linear aesthetic scale.

  Parameters:

  - `k` (keyword): the dimension.
  - `explicit` (sequence or `nil`): the spec's `:domain`.
  - `extent` (map or `nil`): the merged data extent.

  Returns `[lo hi]` as numbers ([[cljplot2.data.rows/x-key]] - epoch milliseconds for dates): the explicit domain, else the extent's `:min` / `:max`; a partial explicit domain (`nil` at one end) takes that end from the extent.

  Throws `ex-info {:key :kind}` without an explicit domain for data that isn't continuous ([[continuous-extent?]]), `ex-info {:key :domain}` for a partial domain without continuous data or with the data entirely beyond its given end."
  [k explicit extent]
  (let [data-range (when (continuous-extent? k extent) [(:min extent) (:max extent)])]
    (cond
      (data/full-domain explicit) (mapv data/x-key explicit)
      (data/partial-domain? explicit) (mapv data/x-key (data/completed-domain k explicit data-range))
      data-range (mapv data/x-key data-range)
      :else (schema/fail! "A :linear aesthetic scale needs numeric data (or dates for :color)." {:key :kind :in [:scales k :kind]}))))

(defn inferred-kind
  "Gives the scale kind of an aesthetic.

  Parameters:

  - `k` (keyword): the dimension.
  - `spec` (map): the plot's scale spec (may be `{}`).
  - `continuous-data?`: the data is continuous - numbers, or dates for `:color` (see [[continuous-values?]]).

  Returns the kind of an explicit domain ([[explicit-kind]]); else the spec's `:kind`; else `:linear` for continuous data of `:color` / `:size`, otherwise `:ordinal`. `:shape`, `:dash` and `:pattern` are always ordinal (or identity)."
  [k spec continuous-data?]
  (if (data/given-domain (:domain spec))
    (explicit-kind k spec)
    (or (:kind spec) (if (and continuous-data? (not (ordinal-only k))) :linear :ordinal))))

(defn- spec-domain
  "Gives the explicit domain of an aesthetic scale spec.

  Parameters:

  - `spec` (map): the scale spec - `:domain`, `:kind`, `:bins`.

  Returns a non-empty `:domain` (`[nil nil]` - none; for a binned scale with a breaks vector the `nil` end of a partial one is the breaks'); else, for a binned scale with a breaks vector, its ends `[first last]`; else `nil`."
  [{:keys [kind bins] :as spec}]
  (let [domain (data/given-domain (:domain spec))
        breaks-ends (when (and (= :binned kind) (vector? bins) (m/>= (count bins) 2)) [(first bins) (peek bins)])]
    (cond
      (and (seq domain) breaks-ends (m/== 2 (count domain))) (mapv #(if (some? %1) %1 %2) domain breaks-ends)
      (seq domain) domain
      :else breaks-ends)))

(defn in-domain?
  "Tests whether a data value of an aesthetic is inside the explicit domain of the plot's scale.

  Parameters:

  - `k` (keyword): the dimension.
  - `spec` (map): the plot's scale spec (may be `{}`).
  - `v`: the data value.

  Returns a truthy value when `v` is in the domain, else a falsy one: ordinal - one of the levels (numbers equal as doubles); linear / binned - a number within `[lo hi]` (binned: also the ends of a `:bins` breaks vector; a partial domain - its given end only). Always truthy without an explicit domain (a data domain holds all data; identity scales have none), for `nil` (NA is handled separately) and for a malformed domain (rejected later by [[scale-spec]]).

  See also [[kept-value?]]."
  [k spec v]
  (let [domain (spec-domain spec)]
    (if (or (nil? v) (nil? domain))
      true
      (case (inferred-kind k spec (continuous-value? k v))
        :ordinal (some #(= (level-key %) (level-key v)) domain)
        (:linear :binned) (let [[lo hi] (map data/x-key domain)
                                x (data/x-key v)]
                            (if (and (m/== 2 (count domain)) (or lo hi))
                              (and x (or (nil? lo) (m/<= (double lo) (double x))) (or (nil? hi) (m/<= (double x) (double hi))))
                              true))))))

(defn- valid-attribute?
  "Tests whether a value is an attribute of an aesthetic as such.

  Parameters:

  - `k` (keyword): the dimension.
  - `v`: the value.

  Returns true for a solid colour (not `:none`) of `:color`, a shape of `:shape`, a non-negative finite number of `:size`, a dash ([[cljplot2.schema/dash?]]) of `:dash`, a pattern ([[cljplot2.schema/pattern?]]) of `:pattern`."
  [k v]
  (case k
    :color (schema/solid-color? v)
    :shape (shape/shape? v)
    :size (and (schema/finite? v) (m/not-neg? (double v)))
    :dash (schema/dash? v)
    :pattern (schema/pattern? v)))

(defn identity-scale?
  "Tests whether a plot scale spec declares an identity scale.

  Parameters:

  - `spec` (map or `nil`): the plot's scale spec.

  Returns true for `:kind :identity`."
  [spec]
  (= :identity (:kind spec)))

(defn valid-value?
  "Tests whether a value is valid for an aesthetic under the plot's scale, the domain aside.

  Parameters:

  - `k` (keyword): the dimension.
  - `spec` (map or `nil`): the plot's scale spec.
  - `v`: the value.

  Returns, with an identity scale, whether `v` is a [[valid-attribute?]] (e.g. any colour); otherwise [[cljplot2.data.rows/valid-value?]] (a plottable data value, no scale involved)."
  [k spec v]
  (if (identity-scale? spec) (valid-attribute? k v) (data/valid-value? v)))

(defn- sampled-bins?
  "Tests whether a scale spec bins by the data values themselves.

  Parameters:

  - `spec` (map or `nil`): the scale spec.

  Returns a truthy value for `:kind :binned` with a method keyword or quantile map `:bins`."
  [{:keys [kind bins]}]
  (and (= :binned kind) (or (keyword? bins) (map? bins))))

(defn value-extent
  "Gives the extent of the values of an aesthetic under the plot's scale.

  Parameters:

  - `k` (keyword): the dimension.
  - `spec` (map or `nil`): the plot's scale spec.
  - `vs` (sequence): the non-NA values.

  Returns `nil` for no values; with an identity scale `{:kind :categorical :values []}` (no domain needed, so values aren't inspected - colours of any form may mix); otherwise [[cljplot2.data.rows/value-extent]], with every finite value as `:sample` when the bins come from the data (method or quantile `:bins`, see [[cljplot2.data.levels/bins]])."
  [k spec vs]
  (cond
    (identity-scale? spec) (when (seq vs) {:kind :categorical :values []})
    (sampled-bins? spec) (some-> (data/value-extent k vs)
                                 (assoc :sample (into [] (comp (filter schema/finite?) (map double)) vs)))
    :else (data/value-extent k vs)))

(defn kept-value?
  "Tests whether a non-`nil` data value of an aesthetic is drawn under the plot's scale.

  Parameters:

  - `k` (keyword): the dimension.
  - `spec` (map): the plot's scale spec (may be `{}`).
  - `v`: the data value.

  Returns, with an identity scale, true for a [[valid-attribute?]]; otherwise true for a valid value ([[cljplot2.data.rows/valid-value?]]) inside the explicit domain ([[in-domain?]]). `nil` gives false - callers decide about NA.

  Throws `ex-info {:key :mapping}` for an invalid value of an identity scale, and `ex-info {:key :kind}` for a non-number on a scale declared linear (dates allowed for `:color`) or binned - by its `:kind` or a two-number domain."
  [k spec v]
  (cond
    (nil? v) false
    (identity-scale? spec) (or (valid-attribute? k v)
                               (schema/fail! (str "An identity " (name k) " scale needs " (name k) " values.")
                                             {:key :mapping :in [:mapping k] :value v}))
    :else (and (data/valid-value? v)
               (let [kind (inferred-kind k spec false)]
                 (when (or (and (= :linear kind) (not (continuous-value? k v)))
                           (and (= :binned kind) (not (number? v))))
                   (schema/fail! (str "A " kind " aesthetic scale needs numeric data"
                                      (when (= :linear kind) " (or dates for :color)") ".")
                                 {:key :kind :in [:scales k :kind] :value v}))
                 (in-domain? k spec v)))))

(defn- binned-breaks
  "Gives the breaks of a binned aesthetic scale.

  Parameters:

  - `k` (keyword): the dimension.
  - `spec` (map): the scale spec - `:bins`.
  - `explicit` (sequence or `nil`): the explicit domain.
  - `extent` (map or `nil`): the merged data extent (`:sample` for data bins).

  Returns the breaks: a `:bins` vector as given (doubles); a count of equal bins over the domain (default [[default-bins]]); a method or quantile `:bins` from the extent's `:sample` ([[cljplot2.data.levels/breaks]]) - with an explicit domain its ends are the outer breaks (one bin over it without data)."
  [k {:keys [bins]} explicit extent]
  (if (vector? bins)
    (mapv double bins)
    (let [[^double lo ^double hi] (linear-domain k explicit extent)]
      (if (or (keyword? bins) (map? bins))
        (let [brks (levels/breaks (:sample extent) bins)]
          (cond
            (nil? brks) [lo hi]
            explicit (vec (distinct (concat [lo] (filter #(m/< lo (double %) hi) brks) [hi])))
            :else brks))
        (levels/breaks [lo hi] (or bins default-bins))))))

(def ^:private value-transforms
  "Named linear colour `:transform`s: `{:forward f :inverse g}`, functions of a double."
  {:sqrt {:forward (fn ^double [^double v] (m/sqrt v)) :inverse (fn ^double [^double v] (m/* v v))}
   :log {:forward (fn ^double [^double v] (m/log10 v)) :inverse (fn ^double [^double v] (m/pow 10.0 v))}
   :sq {:forward (fn ^double [^double v] (m/* v v)) :inverse (fn ^double [^double v] (m/sqrt v))}
   :exp {:forward (fn ^double [^double v] (m/exp v)) :inverse (fn ^double [^double v] (m/log v))}})

(def ^:private identity-transform
  "The transform of a linear scale without `:transform`: both ways the value itself."
  {:forward (fn ^double [^double v] v) :inverse (fn ^double [^double v] v)})

(def ^{:private true :const true :tag 'long} inverse-steps
  "Bisection steps of a numerically inverted transform (the interval shrinks 2^steps times)."
  64)

(defn- monotonic-inverse
  "Gives the numeric inverse of a monotonic function by bisection.

  Parameters:

  - `forward`: a function of a number, monotonic over `[lo hi]`.
  - `lo`, `hi` (double): the interval.

  Returns a function of a transformed value giving the value in `[lo hi]` mapped to it ([[inverse-steps]] bisection steps); the nearest end for values outside the transformed range. Building it never calls `forward` (see [[transformable!]])."
  [forward ^double lo ^double hi]
  (let [f #(double (forward %))
        increasing? (delay (m/<= (double (f lo)) (double (f hi))))]
    (fn ^double [^double y]
      (loop [a lo
             b hi
             i 0]
        (let [mid (m/* 0.5 (m/+ a b))]
          (if (m/>= i inverse-steps)
            mid
            (if (= @increasing? (m/< (double (f mid)) y))
              (recur mid b (m/long-inc i))
              (recur a mid (m/long-inc i)))))))))

(defn value-transform
  "Gives the transform of a linear scale spec (see the namespace doc).

  Parameters:

  - `spec` (map): the scale spec - `:transform` (a name of [[transform-names]], a function or `nil`), `:domain` `[lo hi]`.

  Returns `{:forward f :inverse g}`, functions of a double: a named transform, a user function (its inverse numeric over the domain, see [[monotonic-inverse]]), or the identity without one."
  [{:keys [transform domain]}]
  (if (fn? transform)
    (let [[lo hi] domain]
      {:forward (fn ^double [^double v] (double (transform v)))
       :inverse (monotonic-inverse transform (double lo) (double hi))})
    (get value-transforms transform identity-transform)))

(defn- transformable!
  "Checks a linear colour transform against its domain.

  Parameters:

  - `k` (keyword): the dimension.
  - `transform`: a name of [[transform-names]] or a function.
  - `domain`: `[lo hi]`.

  Returns `transform` when the domain suits it (`:sqrt` and `:sq` - from 0, `:log` - above 0) and it gives finite values at both domain ends.

  Throws `ex-info {:key :transform}` otherwise (a function that throws at an end fails)."
  [k transform [lo hi :as domain]]
  (let [fail! #(schema/fail! (str "Scale :transform " (if (fn? transform) "function" transform) " " %)
                             {:key :transform :in [:scales k :transform] :value transform})
        {:keys [forward]} (value-transform {:transform transform :domain domain})
        finite-at? (fn [v] (try (m/valid-double? (double (forward (double v))))
                                (catch Exception _ false)))]
    (cond
      (and (= :log transform) (m/<= (double lo) 0.0)) (fail! (str "needs a domain above 0, got " lo " as its start."))
      (and (#{:sqrt :sq} transform) (m/neg? (double lo))) (fail! (str "needs a domain from 0, got " lo " as its start."))
      (not (and (finite-at? lo) (finite-at? hi)))
      (fail! (str "should give finite values at the domain ends " lo " and " hi "."))
      :else transform)))

(defn scale-spec
  "Gives the scale of an aesthetic dimension - plain data, also the finalized scale (no pixel range, `nice` or padding).

  Parameters:

  - `k` (keyword): the dimension, one of [[dims]].
  - `spec` (map): the plot's scale spec (may be `{}`), see the namespace doc.
  - `extent` (map or `nil`): the merged data extent ([[cljplot2.data.rows/value-extent]]).
  - `na-level`: some mapped value is NA (the scale gets an NA level).
  - `theme-palette` (map): the theme `:palette` (default ranges).

  Returns `{:kind :domain :range :na-level}` (identity: no `:domain` / `:range`; binned: also `:breaks`, the domain being their ends; a linear scale over dates: the domain in epoch milliseconds and `:temporal-sample` - a date of the data's type, for the colour bar's labels) plus the spec's `:spread`, `:midpoint`, `:transform`, `:reverse`, `:area`, `:formatter`, `:ticks`, `:integer-ticks`, `:title`, `:legend` when given; `nil` when there is neither an explicit domain nor data.

  Throws `ex-info` keyed by the offending option for options the resolved kind doesn't allow ([[option-problems]]), `:ticks` of the wrong type, numeric-only options on dates, a level map `:range` missing a level, or a domain a `:transform` doesn't suit ([[transformable!]]).

  See also [[attribute-fn]], [[legend-entries]]."
  [k spec extent na-level theme-palette]
  (let [explicit (data/given-domain (:domain spec))]
    (when (or explicit extent)
      (let [kind (inferred-kind k spec (continuous-extent? k extent))
            explicit-date (first (filter data/temporal? explicit))
            temporal? (and (or (= :temporal (:kind extent)) explicit-date) (not= kind :ordinal) (not= kind :identity))
            ticks (:ticks spec)
            _ (when (and (sequential? ticks) (not= kind :ordinal) (not= kind :identity))
                (when-not (every? (if temporal? data/temporal? number?) ticks)
                  (schema/fail! (str ":ticks of a scale over " (if temporal? "dates should be dates" "numbers should be numbers") ".")
                                {:key :ticks :in [:scales k :ticks] :value ticks})))
            _ (when temporal?
                (when-let [option-key (some #(let [v (get spec %)] (when (and (some? v) (not (false? v))) %))
                                            [:midpoint :transform :integer-ticks])]
                  (schema/fail! (str option-key " needs numeric data, not dates.") {:key option-key :in [:scales k option-key] :value (get spec option-key)}))
                (when (= :binned kind)
                  (schema/fail! "A :binned aesthetic scale needs numeric data, not dates." {:key :kind :in [:scales k :kind]})))
            _ (when-let [[option-key message] (first (option-problems k kind spec))]
                (schema/fail! (str message ".") {:key option-key :in [:scales k option-key] :value (get spec option-key)}))
            breaks (when (= kind :binned)
                     (binned-breaks k spec explicit extent))
            domain (case kind
                     :ordinal (ordinal-domain k explicit extent)
                     :linear (linear-domain k explicit extent)
                     :binned [(first breaks) (peek breaks)]
                     :identity nil)
            default-range (get theme-palette (if (and (= k :color) (#{:linear :binned} kind)) :gradient k))
            attribute-range (resolve-range k (data/option spec :range default-range))]
        (when (map? attribute-range)
          (let [index (update-keys attribute-range level-key)]
            (when-let [missing (seq (remove #(contains? index (level-key %)) domain))]
              (schema/fail! (str "Level map :range has no attribute for " (pr-str (vec missing)) ".")
                            {:key :range :in [:scales k :range] :value attribute-range}))))
        (cond-> {:kind kind :na-level (true? na-level)}
          (not= kind :identity) (assoc :domain domain :range attribute-range)
          breaks (assoc :breaks breaks)
          (:spread spec) (assoc :spread true)
          (some? (:midpoint spec)) (assoc :midpoint (double (:midpoint spec)))
          (:transform spec) (assoc :transform (transformable! k (:transform spec) domain))
          temporal? (assoc :temporal-sample (or explicit-date (:min extent)))
          (:reverse spec) (assoc :reverse true)
          (:area spec) (assoc :area true)
          (contains? spec :formatter) (assoc :formatter (:formatter spec))
          (some? (:ticks spec)) (assoc :ticks (:ticks spec))
          (:integer-ticks spec) (assoc :integer-ticks true)
          (contains? spec :title) (assoc :title (:title spec))
          (contains? spec :legend) (assoc :legend (:legend spec)))))))

(defn- size-fn
  "Gives the size mapping of a size range.

  Parameters:

  - `[a b]`: the size range.
  - `area?`: the shape's area (the size squared), not its size, is linear in `t`.

  Returns a function of `t` in `[0 1]` giving the size within `[a b]`: `a + t (b - a)`, or with `area?` the square root of `a² + t (b² - a²)`."
  [[^double a ^double b] area?]
  (if area?
    (let [a2 (m/* a a)
          b2 (m/* b b)]
      (fn ^double [^double t] (m/sqrt (m/+ a2 (m/* t (m/- b2 a2))))))
    (fn ^double [^double t] (m/+ a (m/* t (m/- b a))))))

(defn- spread-sizes
  "Gives sizes evenly spread over a size range, one per level.

  Parameters:

  - `sizes`: `[a b]`, the size range.
  - `area?`: spread in area, not in size (see [[size-fn]]).
  - `n` (long): the count of sizes.

  Returns a vector of `n` sizes from `a` to `b` (one size - the middle of the range; zero - empty)."
  [sizes area? n]
  (mapv (size-fn sizes area?) (m/slice-range n)))

(defn- bin-sizes
  "Gives the sizes of the bins of a binned size scale.

  Parameters:

  - `sizes`: `[a b]`, the size range.
  - `area?`: spread in area, not in size (see [[size-fn]]).
  - `n` (long): the bin count, at least 1.

  Returns a vector of `n` sizes, bin `i` at `t = (i + 1) / n` of the range: the last bin at `b`, the first one step above `a`, so with `:range [0 m]` every bin is visible (a single bin - `b`).

  See also [[spread-sizes]] (ordinal levels, from `a` to `b`)."
  [sizes area? n]
  (let [f (size-fn sizes area?)
        step (m// 1.0 (double n))]
    (mapv (fn [^long i] (f (m/* step (double (m/long-inc i))))) (range n))))

(defn- ordinal-attributes
  "Gives the attributes of the levels of an ordinal aesthetic scale.

  Parameters:

  - `k` (keyword): the dimension.
  - `spec` (map): the scale - `:domain`, `:range`, `:spread`, `:reverse`, `:area`.

  Returns a vector, one attribute per level: from a level map; sizes evenly over the range (in area with `:area`, see [[size-fn]]); `:spread` colours resampled from the range; otherwise the range cycled. Reversed with `:reverse`."
  [k {:keys [domain spread area] attribute-range :range reversed? :reverse}]
  (let [n (count domain)
        attributes (cond
                     (map? attribute-range) (let [index (update-keys attribute-range level-key)] (mapv #(get index (level-key %)) domain))
                     (= k :size) (spread-sizes attribute-range area n)
                     spread (vec (c/resample attribute-range (m/max 1 n)))
                     :else (mapv #(nth attribute-range (m/long-mod % (count attribute-range))) (range n)))]
    (if reversed? (vec (rseq attributes)) attributes)))

(defn- unit
  "Gives the position of a value in an interval.

  Parameters:

  - `lo`, `hi` (double): the interval.
  - `v` (double): the value, within it.

  Returns `v` normalized into `[0 1]`; 0.5 for a zero span."
  ^double [^double lo ^double hi ^double v]
  (if (m/== lo hi) 0.5 (m// (m/- v lo) (m/- hi lo))))

(defn- unit-fn
  "Gives the normalization of a linear domain, optionally around a midpoint.

  Parameters:

  - `lo`, `hi` (numbers): the domain.
  - `midpoint` (number or `nil`): the value placed at 0.5.

  Returns a function of a value within `[lo hi]` giving its position `t` in `[0 1]`: linear ([[unit]]), or with a `midpoint` symmetric - both sides scaled by the larger distance to it (a midpoint outside the domain colours one side only; 0.5 everywhere for a zero distance)."
  [lo hi midpoint]
  (let [a (double lo)
        b (double hi)]
    (if (some? midpoint)
      (let [mid (double midpoint)
            d (m/max (m/abs (m/- a mid)) (m/abs (m/- b mid)))]
        (fn ^double [^double v] (if (m/zero? d) 0.5 (m/+ 0.5 (m// (m/- v mid) (m/* 2.0 d))))))
      (fn ^double [^double v] (unit a b v)))))

(defn- gradient-of
  "Gives the gradient of a colour range.

  Parameters:

  - `range-colors`: a function of `t` or colours.

  Returns a function of `t` in `[0 1]` giving a colour: the function as is, else the colours' clojure2d gradient."
  [range-colors]
  (if (fn? range-colors) range-colors (c/gradient range-colors)))

(defn- bin-attributes
  "Gives the attributes of the bins of a binned scale.

  Parameters:

  - `k` (keyword): the dimension, `:color` or `:size`.
  - `spec` (map): the binned scale - `:breaks`, `:range`, `:midpoint`, `:area`, `:reverse`, `:domain`.

  Returns a vector, one attribute per bin: colours - the range resampled to the bin count (a palette of exactly that many colours gives its colours; one bin - the gradient's middle, as a linear scale with one value), with `:midpoint` the gradient at each bin centre's symmetric position (see [[unit-fn]]); sizes - bin `i` of `n` at `(i + 1) / n` of the size range, the last at its end (in area with `:area`, see [[bin-sizes]]). Reversed with `:reverse`."
  [k {:keys [breaks midpoint domain area] reversed? :reverse attribute-range :range}]
  (let [n (m/max 1 (m/long-dec (count breaks)))
        attributes (cond
                     (= k :size) (bin-sizes attribute-range area n)
                     (some? midpoint) (let [g (gradient-of attribute-range)
                                            t (unit-fn (first domain) (second domain) midpoint)]
                                        (mapv (fn [[^double a ^double b]] (g (t (m/* 0.5 (m/+ a b))))) (partition 2 1 breaks)))
                     ;; one bin (e.g. one distinct value): the gradient's middle, as a linear scale with one value
                     (and (m/one? n) (or (fn? attribute-range) (next attribute-range))) [((gradient-of attribute-range) 0.5)]
                     :else (vec (c/resample attribute-range n)))]
    (if reversed? (vec (rseq attributes)) attributes)))

(defn attribute-fn
  "Gives the mapping of data values of an aesthetic to style attributes.

  Parameters:

  - `k` (keyword): the dimension.
  - `spec` (map): the scale ([[scale-spec]]).
  - `na`: the NA attribute.

  Returns a function of a data value giving its attribute (colour, shape, size, dash, pattern): `nil` (NA) gives `na`; a value outside the domain (not a level; not a number within `[lo hi]`; not in a bin) gives `nil` - callers skip it. Identity: the value itself (a size as a double; `nil` for an invalid value).

  See also [[ordinal-attributes]], [[bin-attributes]]."
  [k spec na]
  (case (:kind spec)
    :ordinal (let [index (zipmap (map level-key (:domain spec)) (ordinal-attributes k spec))]
               (fn [v] (if (nil? v) na (get index (level-key v)))))
    :linear (let [[^double lo ^double hi] (:domain spec)
                  attribute-range (:range spec)
                  {:keys [forward]} (value-transform spec)
                  mid (:midpoint spec)
                  t-in (unit-fn (forward lo) (forward hi) (when (some? mid) (forward (double mid))))
                  t-forward (fn ^double [^double v] (double (t-in (double (forward v)))))
                  t-of (if (:reverse spec) (fn ^double [^double v] (m/- 1.0 (double (t-forward v)))) t-forward)
                  f (if (= k :color)
                      (let [g (gradient-of attribute-range)] (fn [^double t] (g t)))
                      (size-fn attribute-range (:area spec)))]
              (fn [v]
                (if (nil? v)
                  na
                  (let [x (data/x-key v)]
                    (when (and x (m/<= lo (double x) hi)) (f (t-of (double x))))))))
    :binned (let [arr (double-array (:breaks spec))
                  attributes (bin-attributes k spec)]
              (fn [v]
                (cond
                  (nil? v) na
                  (number? v) (let [i (levels/bin-index arr (double v))] (when-not (m/neg? i) (attributes i)))
                  :else nil)))
    :identity (fn [v]
                (cond
                  (nil? v) na
                  (valid-attribute? k v) (if (= k :size) (double v) v)
                  :else nil))))

(defn palette-color-fn
  "Gives the colours of the theme palette as an ordinal scale gives them.

  Parameters:

  - `theme` (map): a theme (`[:palette :color]`, `[:palette :na]`).
  - `n` (long): the level count.

  Returns `(f i)` - the `i`-th colour (0 <= i < `n`) an ordinal colour scale over `n` levels gives; the NA colour for other `i`."
  [theme n]
  (attribute-fn :color (scale-spec :color {:kind :ordinal :domain (vec (range (m/max 1 (long n))))} nil false (:palette theme))
                (get-in theme [:palette :na])))

(defn explicit-ticks
  "Gives the legend ticks of an explicit `:ticks` vector.

  Parameters:

  - `ticks` (sequence): numbers, or dates of a scale over dates.
  - `lo`, `hi` (numbers): the domain - epoch milliseconds for dates (see [[cljplot2.data.rows/x-key]]).

  Returns a vector of the ticks inside `[lo hi]`, sorted, equal ones once (`5` and `5.0` alike - the first given kept); `[]` when none is inside."
  [ticks ^double lo ^double hi]
  (->> ticks
       (filter #(m/<= lo (double (data/x-key %)) hi))
       (reduce (fn [acc t] (let [tick-key (double (data/x-key t))] (if (contains? acc tick-key) acc (assoc acc tick-key t)))) (sorted-map))
       vals
       vec))

(defn integer-ticks
  "Gives the counted ticks of a scale with `:integer-ticks`.

  Parameters:

  - `ticks` (sequence): the counted ticks, inside the domain.
  - `lo`, `hi` (double): the domain.
  - `n` (long): the tick count asked for.

  Returns the whole numbers among `ticks`; when fewer than two are whole, the whole numbers of `[lo hi]` if there are one to `max(n, 2)` of them; else the whole ticks found, if any; else `ticks` as they are."
  [ticks ^double lo ^double hi ^long n]
  (let [whole (filterv #(m/== (double %) (m/floor (double %))) ticks)
        in-range (mapv double (range (long (m/ceil lo)) (m/long-inc (long (m/floor hi)))))]
    (cond
      (m/<= 2 (count whole)) whole
      (m/<= 1 (count in-range) (m/max n 2)) in-range
      (seq whole) whole
      :else ticks)))

(defn- linear-legend-ticks
  "Gives the legend entries of a linear size scale.

  Parameters:

  - `spec` (map): the scale - `:domain`, `:ticks`, `:integer-ticks`, `:formatter`.

  Returns `[{:label :value}]` at ticks of the domain, labelled as axis ticks: `:ticks` a count (default [[size-legend-ticks]]) or the values themselves ([[explicit-ticks]]); counted ticks at whole numbers with `:integer-ticks` ([[integer-ticks]]); one tick for a zero span."
  [{:keys [domain formatter] explicit :ticks whole-ticks? :integer-ticks}]
  (let [lo (double (first domain))
        hi (double (second domain))
        n (if (integer? explicit) explicit size-legend-ticks)
        ws (s/scale :linear (cond-> {:domain (if (m/== lo hi) [lo (m/inc lo)] [lo hi])
                                     :ticks n}
                              formatter (assoc :formatter formatter)))
        ticks (cond
                (sequential? explicit) (explicit-ticks explicit lo hi)
                (m/== lo hi) [lo]
                :else (cond-> (filterv #(m/<= lo (double %) hi) (s/ticks ws))
                        whole-ticks? (integer-ticks lo hi n)))]
    (mapv (fn [t l] {:label l :value t}) ticks
          (scale/tick-labels ws {:kind :linear :formatter formatter} ticks))))

(defn- binned-legend-entries
  "Gives the legend entries of a binned size scale, one per bin.

  Parameters:

  - `spec` (map): the scale - `:breaks`, `:formatter`.

  Returns `[{:label :value}]`, in increasing order: the label `a – b` of the bin's breaks (through `:formatter`, else [[cljplot2.data.levels/break-labels]]), the value its centre (drawn with the bin's size)."
  [{:keys [breaks formatter]}]
  (let [labels (if formatter (mapv #(str (formatter %)) breaks) (levels/break-labels breaks))]
    (mapv (fn [[^double a ^double b] [la lb]] {:label (str la " – " lb) :value (m/* 0.5 (m/+ a b))})
          (partition 2 1 breaks) (partition 2 1 labels))))

(defn legend-entries
  "Gives the legend entries of an aesthetic scale.

  Parameters:

  - `k` (keyword): the dimension.
  - `spec` (map): the scale ([[scale-spec]]).

  Returns `[{:label :value}]` (`:value nil` - the NA entry, last when the scale has `:na-level`): ordinal - one per level (`(str level)`, or through `:formatter`); linear size - ticks of the domain ([[linear-legend-ticks]]); binned size - one per bin ([[binned-legend-entries]]). `nil` when the scale has no legend: `:legend false`, an identity scale, or linear / binned colour (a colour bar instead)."
  [k spec]
  (when-not (or (false? (:legend spec)) (= :identity (:kind spec))
                (and (= k :color) (#{:linear :binned} (:kind spec))))
    (let [fmt (or (:formatter spec) str)
          entries (case (:kind spec)
                    :ordinal (mapv (fn [level] {:label (str (fmt level)) :value level}) (:domain spec))
                    :binned (binned-legend-entries spec)
                    (linear-legend-ticks spec))]
      (cond-> entries (:na-level spec) (conj {:label "NA" :value nil})))))
