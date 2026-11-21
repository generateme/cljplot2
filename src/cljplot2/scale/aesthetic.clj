(ns cljplot2.scale.aesthetic
  "Aesthetics: the non-positional dimensions `:color`, `:shape`, `:size`, `:dash` and `:pattern` (fill patterns of
  filled marks; charts without fills - points, plain lines - use it only to group, as `:shape` on lines) mapped from
  data (D4), and their
  scales (D5).

  Mapping: an accessor (keyword or function), `nil` = not mapped. A row's value is kept when it is valid
  ([[cljplot2.data.rows/valid-value?]]) or `nil` - the NA level, drawn with the NA attribute and listed last as \"NA\"
  (plot `:groups {:na false}` drops such rows instead). Other values (NaN, infinite, ...) are dropped.

  Scale spec - plot `:scales {dim spec}`:

  * `:kind` - `:ordinal`, `:linear`, `:binned` or `:identity` (both below); default `:linear` for numeric `:color` / `:size` data or a numeric explicit
    domain of two values, otherwise `:ordinal` (`:shape`, `:dash` and `:pattern` are always ordinal or identity); numeric data with more than
    [[cljplot2.data.rows/max-levels]] distinct values can be ordinal only with an explicit `:domain`
  * `:domain` - ordinal: distinct levels (default: data levels, sorted - in the charts' order when every chart's
    extent is `:ordered?`, e.g. function labels; numbers equal as doubles, `1931` = `1931.0`);
    linear: `[lo hi]` (default: data min, max)
  * `:range` - default from theme `:palette` (`:color`, `:gradient`, `:shape`, `:size`, `:dash`, `:pattern`):
    ordinal `:color` - palette name or colors (cycled; with `:spread` also a gradient); linear / binned `:color` -
    palette name, colors (a gradient), a clojure2d gradient name or a function of `t` in `[0 1]`; `:shape` - shapes (cycled); `:size` - `[min max]` (ordinal levels evenly spread); `:dash` - dashes (cycled, see
    [[cljplot2.schema/dash]]); `:pattern` - patterns (cycled, see [[cljplot2.schema/pattern]])
    or a level map `{level attribute}` (ordinal scales; every level needs an entry, extra keys are ignored)
  * `:kind :binned` (`:color` only, never inferred) - numeric values coloured by bin: `:bins` - a count of equal bins
    over the domain (default 5), a breaks vector (its ends are the domain), a bin method (`:sturges`, ...) or quantile
    bins `{:quantiles q :estimation e}` computed from the data values (see [[cljplot2.data.levels/bins]]; with an
    explicit `:domain` its ends are the outer breaks); bins `[a b)`, the last `[a b]`; bin
    colors - the range resampled to one color per bin (clojure2d `resample`: a palette of exactly that many colors
    gives its colors); legend - a stepped color bar
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
  * `:area true` - linear / ordinal `:size`: the shape's area (∝ size²), not its size, linear in `t` - size
    `√(a² + t·(b² − a²))` for `:range [a b]` (ggplot `scale_size`); with `:domain [0 hi]` and `:range [0 max]` the
    area is proportional to the value (ggplot `scale_size_area`)
  * `:ticks` - linear `:size` / `:color`: the legend's entries or color bar ticks - a count (nice ticks), or the
    values themselves (see [[explicit-ticks]]; drawn as given - no added `:midpoint` tick)
  * `:integer-ticks true` - linear `:size` / `:color`: counted ticks only at whole numbers (see [[integer-ticks]]),
    e.g. for counts or degrees; an explicit `:ticks` vector is drawn as given
  * `:formatter` - legend labels (function); `:title` - legend title (`nil` - no title); `:legend` - `false` hides it
  * dates - `:color` mapped to `java.time` values gets a `:linear` scale over time, as numbers do (domain in epoch
    milliseconds, see [[cljplot2.data.rows/x-key]]; an explicit `:domain` of two dates; color bar ticks are dates);
    `:midpoint`, `:transform`, `:integer-ticks` and `:binned` need numbers

  [[scale-spec]] gives plain data (with `:na?` - the scale has an NA level); it is also the finalized scale: no
  pixel range, `nice` or padding. [[attribute-fn]] maps data values to style attributes: NA (`nil`) to the NA
  attribute; values outside the domain (not a level, or outside `[lo hi]`) to `nil` - such points are skipped.
  Charts drop values outside an explicit domain already at compute ([[in-domain?]]), counted in `:dropped`, so
  extents only cover what is drawn."
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

(def dim? (set dims))

(def kinds #{:ordinal :linear :binned :identity})

(def transform-names
  "The named linear color `:transform`s."
  #{:sqrt :log :sq :exp})

(def ^:private ^:const default-bins 5)

(def ^:private ^:const size-legend-ticks 5)

(defn mapped-dims
  "Aesthetic dimensions with a non-nil accessor in `mapping`."
  [mapping]
  (filterv #(some? (get mapping %)) dims))

(defn computed-scale-attributes
  "Scale attributes a computed aesthetic suggests from its `computed-aesthetics` value (see [[computed-aesthetics]]):
  a title (string or `nil`) - `{:title t}`; a map - its `:title`, `:legend` and `:range` (e.g. a diverging palette)."
  [v]
  (if (map? v) (select-keys v [:title :legend :range]) {:title v}))

(defn computed-aesthetics
  "`{dim title-or-attributes}` of the aesthetics chart value `chart` computes itself with its merged `mapping` (a
  title, or `{:title t :legend b}` - see [[computed-scale-attributes]]): its
  `:computed-aesthetics` - a map, or a function of the chart's options and the mapping returning one (e.g. a color
  computed only when nothing groups the chart, a title named after a mapped accessor; a top-level function keeps chart
  values comparable as data); `{}` without one."
  [chart mapping]
  (let [computed (:computed-aesthetics chart)]
    (or (if (fn? computed) (computed (:options chart) mapping) computed) {})))

(defn- palette-colors
  "Colors of palette `p` (a palette name or a non-empty sequence of colors), `nil` when it isn't one."
  [p]
  (let [colors (cond
                 (keyword? p) (try (vec (c/palette p)) (catch Exception _ nil))
                 (sequential? p) (vec p))]
    (when (and (seq colors) (every? schema/solid-color? colors))
      colors)))

(def ^:private palette
  (schema/named-type ::palette (comp some? palette-colors) "should be a palette name or a non-empty sequence of colors"))

(def ^:private size-range
  (schema/named-type ::size-range #(and (vector? %) (= 2 (count %)) (every? (fn [v] (and (number? v) (>= (double v) 0.0))) %)
                                        (<= (double (% 0)) (double (% 1))))
                     "should be [min max], non-negative numbers, min <= max"))

(def ^:private shapes [:sequential {:min 1} schema/shape])

(def ^:private dashes [:sequential {:min 1} schema/dash])

(def ^:private patterns [:sequential {:min 1} schema/pattern])

(def ^:private gradient [:multi {:dispatch fn?} [true schema/function] [false palette]])

(defn- gradient-name?
  "Is `p` a clojure2d gradient name (a keyword that names a gradient, not a palette)?"
  [p]
  (and (keyword? p) (nil? (palette-colors p)) (try (fn? (c/gradient p)) (catch Exception _ false))))

(def ^:private color-range
  [:multi {:dispatch fn?}
   [true schema/function]
   [false (schema/named-type ::color-range #(or (some? (palette-colors %)) (gradient-name? %))
                             "should be a palette or gradient name, or a non-empty sequence of colors")]])

(defn- level-map
  "Schema of a level map `:range` - `{level attribute}`, attributes valid by `attribute-schema`."
  [attribute-schema]
  [:map-of {:min 1} any? attribute-schema])

(defn- range-schema
  "Schema of the scale `:range` of aesthetic `k`: a level map (ordinal scales) or the dimension's plain range."
  [k]
  (let [plain (case k
                :color color-range
                :shape shapes
                :size size-range
                :dash dashes
                :pattern patterns)
        attribute (case k :color schema/solid-color :shape schema/shape :size schema/non-neg :dash schema/dash
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

(defn- level-key
  "Identity of an ordinal level: numbers compare as doubles (`1931` = `1931.0`), other values as themselves."
  [v]
  (if (number? v) (double v) v))

(defn continuous-value?
  "Can value `v` of aesthetic `k` sit on a linear scale: a number, or a `java.time` value for `:color`?"
  [k v]
  (or (number? v) (and (= :color k) (data/temporal? v))))

(defn continuous-values?
  "Are the non-`nil` `values` of aesthetic `k` some, and all [[continuous-value?]]?"
  [k values]
  (let [present (remove nil? values)]
    (boolean (and (seq present) (every? #(continuous-value? k %) present)))))

(defn- continuous-extent?
  "Is the data `extent` of aesthetic `k` continuous: numeric, or temporal for `:color`?"
  [k extent]
  (or (= :numeric (:kind extent)) (and (= :color k) (= :temporal (:kind extent)))))

(defn- explicit-kind
  "Kind of aesthetic `k` with an explicit domain in `spec`: the spec's `:kind`; `:ordinal` with `:type :categorical`
  (e.g. two years as levels); else `:linear` for two numbers (not `:shape` / `:dash` / `:pattern`) or two dates
  (`:color`), else `:ordinal`."
  [k {:keys [kind type domain]}]
  (or kind
      (when (= :categorical type) :ordinal)
      (if (and (not (ordinal-only k)) (= 2 (count domain)) (every? #(continuous-value? k %) domain)) :linear :ordinal)))

(defn- domain-problem
  "Why the explicit `:domain` of aesthetic scale `spec` of `k` is invalid, or `nil`."
  [k {:keys [domain] :as spec}]
  (when (some? domain)
    (case (explicit-kind k spec)
      :ordinal (when-not (and (seq domain) (not-any? nil? domain) (apply distinct? (map level-key domain)))
                 "ordinal domain should be a non-empty sequence of distinct non-nil values")
      (:linear :binned) (let [[lo hi] (map data/x-key domain)]
                          (when-not (and (= 2 (count domain)) (every? #(continuous-value? k %) domain)
                                         (schema/finite? lo) (schema/finite? hi) (< (double lo) (double hi)))
                            "linear domain should be [lo hi], finite numbers (or dates for :color), lo < hi"))
      :identity nil)))

(defn- option-problems
  "`[[key message] ...]` - options of aesthetic `k`'s scale `spec` that its `kind` doesn't allow or that contradict
  each other (a level map `:range`, `:spread`, `:midpoint`, `:reverse`, `:area`, `:bins`, `:ticks`,
  `:integer-ticks`, identity's `:domain` / `:range`)."
  [k kind {:keys [range spread midpoint reverse area bins domain transform ticks integer-ticks] :as spec}]
  (let [level-map? (map? range)
        bins-domain (when (and (vector? bins) (>= (count bins) 2)) [(double (first bins)) (double (peek bins))])]
    (cond-> []
      (and (= kind :linear) (ordinal-only k)) (conj [:kind (str (name k) " scale can't be :linear")])
      (and (= kind :binned) (not= k :color)) (conj [:kind "only a :color scale can be :binned"])
      (and (= kind :identity) (some? domain)) (conj [:domain "an identity scale takes no :domain"])
      (and (= kind :identity) (contains? spec :range)) (conj [:range "an identity scale takes no :range"])
      (and level-map? (not= kind :ordinal) (not= kind :identity))
      (conj [:range "a level map :range needs an ordinal scale (declare numbers categorical: {:type :categorical})"])
      (and (or (fn? range) (gradient-name? range)) (= kind :ordinal) (not spread))
      (conj [:range "a color :range function or gradient name needs a linear or binned scale, or :spread"])
      (and spread (or (not= k :color) (not= kind :ordinal))) (conj [:spread ":spread needs an ordinal :color scale"])
      (and spread level-map?) (conj [:spread ":spread can't be combined with a level map :range"])
      (and (some? midpoint) (or (not= k :color) (not (#{:linear :binned} kind))))
      (conj [:midpoint ":midpoint needs a linear or binned :color scale"])
      (and (some? transform) (or (not= k :color) (not= kind :linear)))
      (conj [:transform ":transform needs a linear :color scale"])
      (and reverse (= kind :identity)) (conj [:reverse "an identity scale can't be reversed"])
      (and area (or (not= k :size) (not (#{:linear :ordinal} kind)))) (conj [:area ":area needs a linear or ordinal :size scale"])
      (and reverse level-map?) (conj [:reverse ":reverse can't be combined with a level map :range"])
      (and area level-map?) (conj [:area ":area can't be combined with a level map :range"])
      (and (some? bins) (not= kind :binned)) (conj [:bins ":bins needs :kind :binned"])
      (and (some? ticks) (or (not (#{:color :size} k)) (not= kind :linear)))
      (conj [:ticks ":ticks needs a linear :size or :color scale (its legend entries or color bar ticks)"])
      (and integer-ticks (or (not (#{:color :size} k)) (not= kind :linear)))
      (conj [:integer-ticks ":integer-ticks needs a linear :size or :color scale (its legend entries or color bar ticks)"])
      (and (= kind :binned) bins-domain (sequential? domain) (= 2 (count domain))
           (not= bins-domain (mapv double domain)))
      (conj [:bins "breaks should span the explicit :domain (first and last break = its ends)"]))))

(defn- known-kind
  "Kind of aesthetic `k` known from `spec` alone (an explicit `:kind` or `:domain`), else `nil`."
  [k spec]
  (when (or (:kind spec) (sequential? (:domain spec)))
    (explicit-kind k spec)))

(defn- option-rule
  "Schema rule reporting the [[option-problems]] entry of `key` when the kind is known from the spec."
  [k key]
  [(fn [spec]
     (when-let [kind (known-kind k spec)]
       (some (fn [[problem-key message]] (when (= key problem-key) message)) (option-problems k kind spec))))
   [key]])

(defn scale-spec-schema
  "Schema of the plot scale spec of aesthetic `k`."
  [k]
  (apply schema/with-rules
         [:map {:closed true :cljplot2/type-doc "an aesthetic scale spec"}
          (schema/optional :kind {:cljplot2/section :levels :description "scale kind: :ordinal (levels), :linear (a gradient or size range), :binned (:color by bins) or :identity (values are the attributes)"
                                  :cljplot2/default-doc ":linear for numeric :color / :size data, else :ordinal"}
                           (into [:enum] (sort kinds)))
          (schema/optional :type {:cljplot2/section :levels :description "declare numbers or dates categorical (ordinal levels) or force a numeric scale"}
                           [:enum :categorical :numeric])
          (schema/nilable :domain {:cljplot2/section :levels :description "ordinal: the levels in order; linear / binned: [lo hi]"
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
          (schema/optional :bins {:cljplot2/section :bins :description "binned :color: a bin count, breaks, a bin method or quantile bins"
                                  :cljplot2/default-doc "5 equal bins"}
                           levels/bins)
          (schema/optional :formatter {:cljplot2/section :legend :description "legend labels, a function of the level or value"} schema/function)
          (schema/nilable :ticks {:cljplot2/section :legend :description "linear :size / :color: the legend entries or colour bar ticks - a count or the values (dates for a :color scale over dates)"}
                          [:multi {:dispatch sequential?} [true [:sequential [:or schema/finite schema/temporal]]] [false schema/pos-int]])
          (schema/optional :integer-ticks {:cljplot2/section :legend :description "linear :size / :color: counted legend entries or colour bar ticks only at whole numbers (an explicit :ticks vector is drawn as given)" :default false}
                           schema/bool)
          (schema/nilable :title {:cljplot2/section :legend :description "legend title (nil - none)" :cljplot2/default-doc "the mapped column's name"}
                          string?)
          (schema/nilable :legend {:cljplot2/section :legend :description "show the scale's legend" :default true} schema/bool)]
         [#(domain-problem k %) [:domain]]
         (map #(option-rule k %) [:kind :domain :range :spread :midpoint :transform :reverse :area :bins :ticks :integer-ticks])))

(defn- resolve-range
  "Concrete range of dimension `k` from the (validated) range value `r`: a level map or a function as given, the
  gradient (function) of a gradient name, colors of a palette name or sequence."
  [k r]
  (cond
    (or (map? r) (fn? r)) r
    (and (= k :color) (gradient-name? r)) (c/gradient r)
    (= k :color) (palette-colors r)
    (ordinal-only k) (vec r)
    :else r))

(defn- ordinal-domain
  [k explicit extent]
  (cond
    explicit (vec explicit)
    (and (:ordered? extent) (:values extent)) (first (reduce (fn [[acc seen] v] (if (contains? seen (level-key v)) [acc seen] [(conj acc v) (conj seen (level-key v))]))
                                                             [[] #{}] (:values extent)))
    (:values extent) (data/sorted-levels k (vals (reduce (fn [acc v] (if (contains? acc (level-key v)) acc (assoc acc (level-key v) v)))
                                                         {} (:values extent)))
                                         :domain)
    :else (schema/fail! (str "Too many distinct values for an ordinal scale (more than " data/max-levels
                             "); give an explicit :domain.")
                        {:key :domain :in [:scales k :domain]})))

(defn- linear-domain
  "`[lo hi]` of a linear scale of `k`: the explicit domain or the data `extent`, numbers or dates as
  [[cljplot2.data.rows/x-key]] numbers (epoch milliseconds)."
  [k explicit extent]
  (cond
    explicit [(data/x-key (first explicit)) (data/x-key (second explicit))]
    (continuous-extent? k extent) [(data/x-key (:min extent)) (data/x-key (:max extent))]
    :else (schema/fail! "A :linear aesthetic scale needs numeric data (or dates for :color)." {:key :kind :in [:scales k :kind]})))

(defn inferred-kind
  "Scale kind of aesthetic `k` with plot scale `spec` (may be `{}`) for data that is continuous (`continuous-data?` -
  numbers, or dates for `:color`, see [[continuous-values?]]) or not: the spec's `:kind`; else `:linear` for a
  continuous two-value explicit domain or, without a domain, continuous data (`:color`, `:size`); else `:ordinal`.
  `:shape`, `:dash` and `:pattern` are always ordinal (or identity)."
  [k spec continuous-data?]
  (if (sequential? (:domain spec))
    (explicit-kind k spec)
    (or (:kind spec) (if (and continuous-data? (not (ordinal-only k))) :linear :ordinal))))

(defn- spec-domain
  "Explicit domain of scale `spec`: its `:domain` (non-empty), else a binned scale's breaks ends `[first last]`."
  [spec]
  (let [domain (:domain spec)
        bins (:bins spec)]
    (cond
      (and (sequential? domain) (seq domain)) domain
      (and (= :binned (:kind spec)) (vector? bins) (>= (count bins) 2)) [(first bins) (peek bins)])))

(defn in-domain?
  "Is data value `v` of aesthetic `k` inside the explicit domain of the plot's scale `spec` (may be `{}`)?
  True without an explicit domain (a data domain holds all data; identity scales have none), for `nil` (NA is
  handled separately) and for a malformed domain (rejected later by [[scale-spec]]). Ordinal: one of the levels (numbers
  equal as doubles); linear / binned: a number within `[lo hi]` (binned: also the ends of a `:bins` breaks vector)."
  [k spec v]
  (let [domain (spec-domain spec)]
    (if (or (nil? v) (nil? domain))
      true
      (case (inferred-kind k spec (continuous-value? k v))
        :ordinal (boolean (some #(= (level-key %) (level-key v)) domain))
        (:linear :binned) (let [[lo hi] (map data/x-key domain)
                                x (data/x-key v)]
                            (if (and (= 2 (count domain)) lo hi)
                              (boolean (and x (<= (double lo) (double x) (double hi))))
                              true))))))

(defn valid-attribute?
  "Is `v` an attribute of aesthetic `k` as such: a color (not `:none`), a shape, a non-negative finite size, a dash
  ([[cljplot2.schema/dash?]]), a pattern ([[cljplot2.schema/pattern?]])?"
  [k v]
  (case k
    :color (schema/solid-color? v)
    :shape (shape/shape? v)
    :size (and (schema/finite? v) (>= (double v) 0.0))
    :dash (schema/dash? v)
    :pattern (schema/pattern? v)))

(defn identity-scale?
  "Does the plot's scale `spec` (may be `nil`) declare `:kind :identity`?"
  [spec]
  (= :identity (:kind spec)))

(defn valid-value?
  "Is `v` a valid value of aesthetic `k` under the plot's scale `spec` (may be `nil`), the domain aside: with an
  identity scale a valid attribute, otherwise [[cljplot2.data.rows/valid-value?]]."
  [k spec v]
  (if (identity-scale? spec) (valid-attribute? k v) (data/valid-value? v)))

(defn- sampled-bins?
  "Does scale `spec` bin by the data values themselves: `:kind :binned` with a method keyword or quantile `:bins`?"
  [spec]
  (and (= :binned (:kind spec)) (let [bins (:bins spec)] (or (keyword? bins) (map? bins)))))

(defn value-extent
  "Extent of the non-NA values `vs` of aesthetic `k` under the plot's scale `spec` (may be `nil`), `nil` when empty:
  an identity scale needs no domain, so its values aren't inspected (`{:kind :categorical :values []}` - colors of
  any form may mix); otherwise [[cljplot2.data.rows/value-extent]], with every finite value as `:sample` when the
  scale's bins come from the data (method or quantile `:bins`, see [[cljplot2.data.levels/bins]])."
  [k spec vs]
  (cond
    (identity-scale? spec) (when (seq vs) {:kind :categorical :values []})
    (sampled-bins? spec) (some-> (data/value-extent k vs)
                                 (assoc :sample (into [] (comp (filter schema/finite?) (map double)) vs)))
    :else (data/value-extent k vs)))

(defn kept-value?
  "Is the non-`nil` data value `v` of aesthetic `k` drawn under the plot's scale `spec` (may be `{}`): with an identity
  scale a valid attribute ([[valid-attribute?]]) - any other value throws `ex-info {:key :mapping}`; otherwise a
  valid value ([[cljplot2.data.rows/valid-value?]]) inside the explicit domain ([[in-domain?]]) - a non-number on a
  scale declared linear or binned (its `:kind`, or a two-number domain) throws `ex-info {:key :kind}`. `nil` gives
  false (callers decide about NA)."
  [k spec v]
  (cond
    (nil? v) false
    (identity-scale? spec) (or (valid-attribute? k v)
                               (schema/fail! (str "An identity " (name k) " scale needs " (name k) " values.")
                                             {:key :mapping :in [:mapping k] :value v}))
    :else (and (data/valid-value? v)
               (do (let [kind (inferred-kind k spec false)]
                     (when (or (and (= :linear kind) (not (continuous-value? k v)))
                               (and (= :binned kind) (not (number? v))))
                       (schema/fail! (str "A " kind " aesthetic scale needs numeric data"
                                          (when (= :linear kind) " (or dates for :color)") ".")
                                     {:key :kind :in [:scales k :kind] :value v})))
                   (in-domain? k spec v)))))

(defn- binned-breaks
  "Breaks of binned scale `spec` of `k` (explicit `:domain` `explicit`, data `extent`): a `:bins` vector as given; a
  count of equal bins over the domain (default 5); a method or quantile `:bins` from the extent's `:sample` - with an
  explicit domain its ends are the outer breaks (one bin over it without data)."
  [k {:keys [bins]} explicit extent]
  (if (vector? bins)
    (mapv double bins)
    (let [[lo hi] (linear-domain k explicit extent)]
      (if (or (keyword? bins) (map? bins))
        (let [brks (levels/breaks (:sample extent) bins)]
          (cond
            (nil? brks) [lo hi]
            explicit (vec (distinct (concat [lo] (filter #(< (double lo) (double %) (double hi)) brks) [hi])))
            :else brks))
        (levels/breaks [lo hi] (or bins default-bins))))))

(def ^:private value-transforms
  "Named linear color `:transform`s: `{:forward f :inverse g}`."
  {:sqrt {:forward (fn ^double [^double v] (m/sqrt v)) :inverse (fn ^double [^double v] (* v v))}
   :log {:forward (fn ^double [^double v] (m/log10 v)) :inverse (fn ^double [^double v] (m/pow 10.0 v))}
   :sq {:forward (fn ^double [^double v] (* v v)) :inverse (fn ^double [^double v] (m/sqrt v))}
   :exp {:forward (fn ^double [^double v] (m/exp v)) :inverse (fn ^double [^double v] (m/log v))}})

(def ^:private identity-transform {:forward (fn ^double [^double v] v) :inverse (fn ^double [^double v] v)})

(def ^:private ^:const inverse-steps
  "Bisection steps of a numerically inverted transform (the interval shrinks 2^steps times)."
  64)

(defn- monotonic-inverse
  "Inverse of `forward` - a function monotonic over `[lo hi]` - by bisection: a function of a transformed value giving
  the value in `[lo hi]` mapped to it (the nearest end for values outside the transformed range)."
  [forward ^double lo ^double hi]
  (let [f #(double (forward %))
        ;; evaluated on first use: building the inverse never calls `forward` (see transformable!)
        increasing? (delay (<= (double (f lo)) (double (f hi))))]
    (fn ^double [^double y]
      (loop [a lo b hi i 0]
        (let [mid (* 0.5 (+ a b))]
          (if (>= i (long inverse-steps))
            mid
            (if (= @increasing? (< (double (f mid)) y))
              (recur mid b (inc i))
              (recur a mid (inc i)))))))))

(defn value-transform
  "`{:forward f :inverse g}` (functions of a double) of the linear scale `spec`'s `:transform` (see the namespace
  doc): a named one, a user function (its inverse numeric over the spec's `:domain`, see [[monotonic-inverse]]), or
  the identity without one."
  [{:keys [transform domain]}]
  (if (fn? transform)
    (let [[lo hi] domain]
      {:forward (fn ^double [^double v] (double (transform v)))
       :inverse (monotonic-inverse transform (double lo) (double hi))})
    (get value-transforms transform identity-transform)))

(defn- transformable!
  "`transform` when the linear `domain` of aesthetic `k` suits it (`:sqrt` and `:sq` - from 0, `:log` - above 0) and
  it gives finite values at both domain ends (a function that throws there doesn't), else throws
  `ex-info {:key :transform}`."
  [k transform [lo hi :as domain]]
  (let [fail! #(schema/fail! (str "Scale :transform " (if (fn? transform) "function" transform) " " %)
                             {:key :transform :in [:scales k :transform] :value transform})
        {:keys [forward]} (value-transform {:transform transform :domain domain})
        finite-at? (fn [v] (try (m/valid-double? (double (forward (double v))))
                                (catch Exception _ false)))]
    (cond
      (and (= :log transform) (<= (double lo) 0.0)) (fail! (str "needs a domain above 0, got " lo " as its start."))
      (and (#{:sqrt :sq} transform) (neg? (double lo))) (fail! (str "needs a domain from 0, got " lo " as its start."))
      (not (and (finite-at? lo) (finite-at? hi)))
      (fail! (str "should give finite values at the domain ends " lo " and " hi "."))
      :else transform)))

(defn scale-spec
  "Scale of aesthetic dimension `k` from the plot's scale `spec` (may be `{}`), the merged data `extent`
  (a [[cljplot2.data.rows/value-extent]], may be `nil`), `na?` (some mapped value is NA) and theme `palette`.
  `nil` when there is neither an explicit domain nor data. Throws `ex-info` (`:key` - the offending option) for
  options the resolved kind doesn't allow.
  Returns `{:kind :domain :range :na?}` (identity: no `:domain` / `:range`; binned: also `:breaks`, the domain being
  their ends; a linear scale over dates: the domain in epoch milliseconds and `:temporal-sample` - a date of the
  data's type, for the color bar's labels) plus the spec's `:spread`, `:midpoint`, `:reverse`, `:area`,
  `:formatter`, `:ticks`, `:integer-ticks`, `:title`, `:legend` when given."
  [k spec extent na? palette]
  (let [explicit (:domain spec)]
    (when (or explicit extent)
      (let [kind (inferred-kind k spec (continuous-extent? k extent))
            explicit-date (when (data/temporal? (first explicit)) (first explicit))
            temporal? (and (or (= :temporal (:kind extent)) explicit-date) (not= kind :ordinal) (not= kind :identity))
            ticks (:ticks spec)
            _ (when (and (sequential? ticks) (not= kind :ordinal) (not= kind :identity))
                (when-not (every? (if temporal? data/temporal? number?) ticks)
                  (schema/fail! (str ":ticks of a scale over " (if temporal? "dates should be dates" "numbers should be numbers") ".")
                                {:key :ticks :in [:scales k :ticks] :value ticks})))
            _ (when temporal?
                (when-let [key (some #(let [v (get spec %)] (when (and (some? v) (not (false? v))) %))
                                     [:midpoint :transform :integer-ticks])]
                  (schema/fail! (str key " needs numeric data, not dates.") {:key key :in [:scales k key] :value (get spec key)}))
                (when (= :binned kind)
                  (schema/fail! "A :binned aesthetic scale needs numeric data, not dates." {:key :kind :in [:scales k :kind]})))
            _ (when-let [[key message] (first (option-problems k kind spec))]
                (schema/fail! (str message ".") {:key key :in [:scales k key] :value (get spec key)}))
            breaks (when (= kind :binned)
                     (binned-breaks k spec explicit extent))
            domain (case kind
                     :ordinal (ordinal-domain k explicit extent)
                     :linear (linear-domain k explicit extent)
                     :binned [(first breaks) (peek breaks)]
                     :identity nil)
            default-range (get palette (if (and (= k :color) (#{:linear :binned} kind)) :gradient k))
            range (resolve-range k (get spec :range default-range))]
        (when (map? range)
          (let [index (update-keys range level-key)]
            (when-let [missing (seq (remove #(contains? index (level-key %)) domain))]
              (schema/fail! (str "Level map :range has no attribute for " (pr-str (vec missing)) ".")
                            {:key :range :in [:scales k :range] :value range}))))
        (cond-> {:kind kind :na? (boolean na?)}
          (not= kind :identity) (assoc :domain domain :range range)
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

(defn- evenly
  "`n` values evenly spread over `[lo hi]` (the midpoint for one)."
  [^long n [lo hi]]
  (let [lo (double lo) hi (double hi)]
    (if (= n 1)
      [(* 0.5 (+ lo hi))]
      (mapv #(+ lo (* (- hi lo) (/ (double %) (double (dec n))))) (range n)))))

(defn- size-fn
  "Function of `t` in `[0 1]` giving the size within `[a b]` (`range`): linear, or with `area?` the size whose square
  (the shape's area) is linear in `t`."
  [[a b] area?]
  (let [a (double a) b (double b)]
    (if area?
      (let [a2 (* a a) b2 (* b b)] (fn ^double [^double t] (m/sqrt (+ a2 (* t (- b2 a2))))))
      (fn ^double [^double t] (+ a (* t (- b a)))))))

(defn- ordinal-attributes
  "Attribute of every level of ordinal scale `spec` of dimension `k`: from a level map; sizes evenly over the range
  (in area with `:area`); `:spread` colors resampled from the range; otherwise the range cycled. Reversed with
  `:reverse`."
  [k {:keys [domain range spread reverse area]}]
  (let [n (count domain)
        attributes (cond
                     (map? range) (let [index (update-keys range level-key)] (mapv #(get index (level-key %)) domain))
                     (= k :size) (mapv (size-fn range area) (evenly n [0.0 1.0]))
                     spread (vec (c/resample range (max 1 n)))
                     :else (mapv #(nth range (mod (long %) (count range))) (clojure.core/range n)))]
    (if reverse (vec (rseq attributes)) attributes)))

(defn- unit
  "`v` (within `[lo hi]`) normalized into `[0 1]`; 0.5 for a zero span."
  ^double [^double lo ^double hi ^double v]
  (if (== lo hi) 0.5 (/ (- v lo) (- hi lo))))

(defn- unit-fn
  "Function of a value within `[lo hi]` giving its position `t` in `[0 1]`: linear, or - with a `midpoint` -
  symmetric: the midpoint at 0.5, both sides scaled by the larger distance to it (a midpoint outside the domain
  colours one side only)."
  [lo hi midpoint]
  (let [lo (double lo) hi (double hi)]
    (if (some? midpoint)
      (let [mid (double midpoint)
            d (m/max (m/abs (- lo mid)) (m/abs (- hi mid)))]
        (fn ^double [^double v] (if (zero? d) 0.5 (+ 0.5 (/ (- v mid) (* 2.0 d))))))
      (fn ^double [^double v] (unit lo hi v)))))

(defn- gradient-of
  "Gradient (function of `t` in `[0 1]`) of a color range: a function as is, colors through clojure2d."
  [range]
  (if (fn? range) range (c/gradient range)))

(defn bin-colors
  "Colors of the bins of binned scale `spec`: the range resampled to one color per bin (a palette of exactly that
  many colors gives its colors); with `:midpoint` the gradient at each bin centre's symmetric position. Reversed
  with `:reverse`."
  [{:keys [breaks range midpoint reverse domain]}]
  (let [n (dec (count breaks))
        colors (if (some? midpoint)
                 (let [g (gradient-of range)
                       t (unit-fn (first domain) (second domain) midpoint)]
                   (mapv (fn [[a b]] (g (t (* 0.5 (+ (double a) (double b)))))) (partition 2 1 breaks)))
                 (vec (c/resample range (max 1 n))))]
    (if reverse (vec (rseq colors)) colors)))

(defn attribute-fn
  "Function of a data value of dimension `k` with scale `spec` giving its style attribute (color, shape, size):
  `nil` (NA) gives `na`; a value outside the domain (not a level; not a number within `[lo hi]`) gives `nil` -
  callers skip it. Identity: the value itself (`nil` for an invalid one)."
  [k spec na]
  (case (:kind spec)
    :ordinal (let [index (zipmap (map level-key (:domain spec)) (ordinal-attributes k spec))]
               (fn [v] (if (nil? v) na (get index (level-key v)))))
    :linear (let [[lo hi] (:domain spec)
                  lo (double lo) hi (double hi)
                  range (:range spec)
                  {:keys [forward]} (value-transform spec)
                  mid (:midpoint spec)
                  t-in (unit-fn (forward lo) (forward hi) (when (some? mid) (forward (double mid))))
                  t-of (fn ^double [^double v] (double (t-in (double (forward v)))))
                  t-of (if (:reverse spec) (fn ^double [^double v] (- 1.0 (double (t-of v)))) t-of)
                  f (if (= k :color)
                      (let [g (gradient-of range)] (fn [^double t] (g t)))
                      (size-fn range (:area spec)))]
              (fn [v]
                (cond
                  (nil? v) na
                  :else (let [x (data/x-key v)]
                          (when (and x (<= lo (double x) hi)) (f (t-of (double x))))))))
    :binned (let [arr (double-array (:breaks spec))
                  colors (bin-colors spec)]
              (fn [v]
                (cond
                  (nil? v) na
                  (number? v) (let [i (levels/bin-index arr (double v))] (when-not (neg? i) (colors i)))
                  :else nil)))
    :identity (fn [v]
                (cond
                  (nil? v) na
                  (valid-attribute? k v) (if (= k :size) (double v) v)
                  :else nil))))

(defn palette-color-fn
  "`(f i)` - the `i`-th color (0 <= i < `n`) of the theme's `[:palette :color]` as an ordinal scale over `n` levels
  would give it (the NA color for other `i`)."
  [theme n]
  (attribute-fn :color (scale-spec :color {:kind :ordinal :domain (vec (range (max 1 (long n))))} nil false (:palette theme))
                (get-in theme [:palette :na])))

(defn explicit-ticks
  "Legend ticks of an explicit `:ticks` vector `ticks` (numbers, or dates of a scale over dates) over the domain
  `[lo hi]` (numbers - epoch milliseconds for dates, see [[cljplot2.data.rows/x-key]]): the values inside it, sorted,
  equal ones once (`5` and `5.0` alike - the first given kept); `[]` when none is inside."
  [ticks lo hi]
  (let [lo (double lo) hi (double hi)]
    (->> ticks
         (filter #(<= lo (double (data/x-key %)) hi))
         (reduce (fn [acc t] (let [key (double (data/x-key t))] (if (contains? acc key) acc (assoc acc key t)))) (sorted-map))
         vals
         vec)))

(defn integer-ticks
  "Counted ticks `ticks` (inside `[lo hi]`) of a scale with `:integer-ticks`: the whole numbers among them; when fewer
  than two are whole, the whole numbers of `[lo hi]` if there are one to `n` of them; else `ticks` as they are (no
  whole number in the domain to show, or more than `n` - the whole ticks found, if any)."
  [ticks lo hi n]
  (let [lo (double lo) hi (double hi) n (long n)
        whole (filterv #(== (double %) (m/floor (double %))) ticks)
        in-range (mapv double (range (long (m/ceil lo)) (inc (long (m/floor hi)))))]
    (cond
      (<= 2 (count whole)) whole
      (<= 1 (count in-range) (max n 2)) in-range
      (seq whole) whole
      :else ticks)))

(defn legend-entries
  "Legend entries `[{:label :value}]` of scale `spec` of dimension `k` (`:value nil` - the NA entry, last), or `nil`
  when it has no legend: `:legend false`, an identity scale, or linear / binned color (a color bar instead).
  Ordinal: one per level (`(str level)` or `:formatter`); linear size: ticks of the domain (labels as axis ticks) -
  `:ticks` a count (default 5) or the values themselves (see [[explicit-ticks]]); counted ticks at whole numbers with
  `:integer-ticks` (see [[integer-ticks]])."
  [k spec]
  (when-not (or (false? (:legend spec)) (= :identity (:kind spec))
                (and (= k :color) (#{:linear :binned} (:kind spec))))
    (let [fmt (or (:formatter spec) str)
          entries (if (= :ordinal (:kind spec))
                    (mapv (fn [level] {:label (str (fmt level)) :value level}) (:domain spec))
                    (let [[lo hi] (:domain spec)
                          lo (double lo) hi (double hi)]
                      (let [explicit (:ticks spec)
                            ws (s/scale :linear (cond-> {:domain (if (== lo hi) [lo (inc lo)] [lo hi])
                                                         :ticks (if (integer? explicit) explicit size-legend-ticks)}
                                                  (:formatter spec) (assoc :formatter (:formatter spec))))
                            in-domain #(<= lo (double %) hi)
                            ticks (cond
                                    (sequential? explicit) (explicit-ticks explicit lo hi)
                                    (== lo hi) [lo]
                                    :else (cond-> (filterv in-domain (s/ticks ws))
                                            (:integer-ticks spec)
                                            (integer-ticks lo hi (if (integer? explicit) explicit size-legend-ticks))))]
                        (mapv (fn [t l] {:label l :value t}) ticks
                              (scale/tick-labels ws {:kind :linear :formatter (:formatter spec)} ticks)))))]
      (cond-> entries (:na? spec) (conj {:label "NA" :value nil})))))
