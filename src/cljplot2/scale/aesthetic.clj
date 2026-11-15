(ns cljplot2.scale.aesthetic
  "Aesthetics: the non-positional dimensions `:color`, `:shape`, `:size` and `:dash` mapped from data (D4), and their
  scales (D5).

  Mapping: an accessor (keyword or function), `nil` = not mapped. A row's value is kept when it is valid
  ([[cljplot2.data.rows/valid-value?]]) or `nil` - the NA level, drawn with the NA attribute and listed last as \"NA\"
  (plot `:groups {:na false}` drops such rows instead). Other values (NaN, infinite, ...) are dropped.

  Scale spec - plot `:scales {dim spec}`:

  * `:kind` - `:ordinal`, `:linear`, `:binned` or `:identity` (both below); default `:linear` for numeric `:color` / `:size` data or a numeric explicit
    domain of two values, otherwise `:ordinal` (`:shape` and `:dash` are always ordinal or identity); numeric data with more than
    [[cljplot2.data.rows/max-levels]] distinct values can be ordinal only with an explicit `:domain`
  * `:domain` - ordinal: distinct levels (default: data levels, sorted - in the charts' order when every chart's
    extent is `:ordered?`, e.g. function labels; numbers equal as doubles, `1931` = `1931.0`);
    linear: `[lo hi]` (default: data min, max)
  * `:range` - default from theme `:palette` (`:color`, `:gradient`, `:shape`, `:size`, `:dash`):
    ordinal `:color` - palette name or colors (cycled; with `:spread` also a gradient); linear / binned `:color` -
    palette name, colors (a gradient), a clojure2d gradient name or a function of `t` in `[0 1]`; `:shape` - shapes (cycled); `:size` - `[min max]` (ordinal levels evenly spread); `:dash` - dashes (cycled, see
    [[cljplot2.schema/dash]])
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
  * `:reverse true` - the attributes in reverse (linear `t` → `1 - t`, ordinal / binned attributes reversed)
  * `:formatter` - legend labels (function); `:title` - legend title (`nil` - no title); `:legend` - `false` hides it

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
  [:color :shape :size :dash])

(def ^:private ordinal-only
  "Aesthetic dimensions whose scales are never linear or binned (their attributes have no order)."
  #{:shape :dash})

(def dim? (set dims))

(def kinds #{:ordinal :linear :binned :identity})

(def ^:private ^:const default-bins 5)

(def ^:private ^:const size-legend-ticks 5)

(defn mapped-dims
  "Aesthetic dimensions with a non-nil accessor in `mapping`."
  [mapping]
  (filterv #(some? (get mapping %)) dims))

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
                :dash dashes)
        attribute (case k :color schema/solid-color :shape schema/shape :size schema/non-neg :dash schema/dash)]
    [:multi {:dispatch map?} [true (level-map attribute)] [false plain]]))

(def palette-schema
  "Theme `:palette` (aesthetic defaults)."
  [:map {:closed true}
   [:color palette]
   [:gradient gradient]
   [:shape shapes]
   [:size size-range]
   [:dash dashes]
   [:na schema/solid-color]])

(defn- level-key
  "Identity of an ordinal level: numbers compare as doubles (`1931` = `1931.0`), other values as themselves."
  [v]
  (if (number? v) (double v) v))

(defn- explicit-kind
  "Kind of aesthetic `k` with an explicit domain in `spec`: the spec's `:kind`, else `:linear` for two numbers (not
  `:shape` / `:dash`), else `:ordinal`."
  [k {:keys [kind domain]}]
  (or kind (if (and (not (ordinal-only k)) (= 2 (count domain)) (every? number? domain)) :linear :ordinal)))

(defn- domain-problem
  "Why the explicit `:domain` of aesthetic scale `spec` of `k` is invalid, or `nil`."
  [k {:keys [domain] :as spec}]
  (when (some? domain)
    (case (explicit-kind k spec)
      :ordinal (when-not (and (seq domain) (not-any? nil? domain) (apply distinct? (map level-key domain)))
                 "ordinal domain should be a non-empty sequence of distinct non-nil values")
      (:linear :binned) (when-not (and (= 2 (count domain)) (every? schema/finite? domain)
                                       (< (double (first domain)) (double (second domain))))
                          "linear domain should be [lo hi], finite numbers, lo < hi")
      :identity nil)))

(defn- option-problems
  "`[[key message] ...]` - options of aesthetic `k`'s scale `spec` that its `kind` doesn't allow or that contradict
  each other (a level map `:range`, `:spread`, `:midpoint`, `:reverse`, `:bins`, identity's `:domain` / `:range`)."
  [k kind {:keys [range spread midpoint reverse bins domain] :as spec}]
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
      (and reverse (= kind :identity)) (conj [:reverse "an identity scale can't be reversed"])
      (and reverse level-map?) (conj [:reverse ":reverse can't be combined with a level map :range"])
      (and (some? bins) (not= kind :binned)) (conj [:bins ":bins needs :kind :binned"])
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
         [:map {:closed true}
          (schema/optional :kind (into [:enum] (sort kinds)))
          (schema/optional :type [:enum :categorical :numeric])
          (schema/nilable :domain [:sequential any?])
          (schema/optional :range (range-schema k))
          (schema/optional :spread schema/bool)
          (schema/optional :midpoint schema/finite)
          (schema/optional :reverse schema/bool)
          (schema/optional :bins levels/bins)
          (schema/optional :formatter schema/function)
          (schema/nilable :title string?)
          (schema/nilable :legend schema/bool)]
         [#(domain-problem k %) [:domain]]
         (map #(option-rule k %) [:kind :domain :range :spread :midpoint :reverse :bins])))

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
  [k explicit extent]
  (cond
    explicit [(double (first explicit)) (double (second explicit))]
    (= :numeric (:kind extent)) [(double (:min extent)) (double (:max extent))]
    :else (schema/fail! "A :linear aesthetic scale needs numeric data." {:key :kind :in [:scales k :kind]})))

(defn inferred-kind
  "Scale kind of aesthetic `k` with plot scale `spec` (may be `{}`) for data that is numeric (`data-numeric?`) or not:
  the spec's `:kind`; else `:linear` for a numeric two-value explicit domain or, without a domain, numeric data
  (`:color`, `:size`); else `:ordinal`. `:shape` and `:dash` are always ordinal (or identity)."
  [k spec data-numeric?]
  (if (sequential? (:domain spec))
    (explicit-kind k spec)
    (or (:kind spec) (if (and data-numeric? (not (ordinal-only k))) :linear :ordinal))))

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
      (case (inferred-kind k spec (number? v))
        :ordinal (boolean (some #(= (level-key %) (level-key v)) domain))
        (:linear :binned) (let [[lo hi] domain]
                            (if (and (= 2 (count domain)) (number? lo) (number? hi))
                              (and (number? v) (<= (double lo) (double v) (double hi)))
                              true))))))

(defn valid-attribute?
  "Is `v` an attribute of aesthetic `k` as such: a color (not `:none`), a shape, a non-negative finite size, a dash
  ([[cljplot2.schema/dash?]])?"
  [k v]
  (case k
    :color (schema/solid-color? v)
    :shape (contains? shape/shapes v)
    :size (and (schema/finite? v) (>= (double v) 0.0))
    :dash (schema/dash? v)))

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
               (do (when (and (not (number? v)) (#{:linear :binned} (inferred-kind k spec false)))
                     (schema/fail! (str "A " (inferred-kind k spec false) " aesthetic scale needs numeric data.")
                                   {:key :kind :in [:scales k :kind] :value v}))
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

(defn scale-spec
  "Scale of aesthetic dimension `k` from the plot's scale `spec` (may be `{}`), the merged data `extent`
  (a [[cljplot2.data.rows/value-extent]], may be `nil`), `na?` (some mapped value is NA) and theme `palette`.
  `nil` when there is neither an explicit domain nor data. Throws `ex-info` (`:key` - the offending option) for
  options the resolved kind doesn't allow.
  Returns `{:kind :domain :range :na?}` (identity: no `:domain` / `:range`; binned: also `:breaks`, the domain being
  their ends) plus the spec's `:spread`, `:midpoint`, `:reverse`, `:formatter`, `:title`, `:legend` when given."
  [k spec extent na? palette]
  (let [explicit (:domain spec)]
    (when (or explicit extent)
      (let [kind (inferred-kind k spec (= :numeric (:kind extent)))
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
          (:reverse spec) (assoc :reverse true)
          (contains? spec :formatter) (assoc :formatter (:formatter spec))
          (contains? spec :title) (assoc :title (:title spec))
          (contains? spec :legend) (assoc :legend (:legend spec)))))))

(defn- evenly
  "`n` values evenly spread over `[lo hi]` (the midpoint for one)."
  [^long n [lo hi]]
  (let [lo (double lo) hi (double hi)]
    (if (= n 1)
      [(* 0.5 (+ lo hi))]
      (mapv #(+ lo (* (- hi lo) (/ (double %) (double (dec n))))) (range n)))))

(defn- ordinal-attributes
  "Attribute of every level of ordinal scale `spec` of dimension `k`: from a level map; sizes evenly over the range;
  `:spread` colors resampled from the range; otherwise the range cycled. Reversed with `:reverse`."
  [k {:keys [domain range spread reverse]}]
  (let [n (count domain)
        attributes (cond
                     (map? range) (let [index (update-keys range level-key)] (mapv #(get index (level-key %)) domain))
                     (= k :size) (evenly n range)
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
                  t-of (unit-fn lo hi (:midpoint spec))
                  t-of (if (:reverse spec) (fn ^double [^double v] (- 1.0 (double (t-of v)))) t-of)
                  f (if (= k :color)
                      (let [g (gradient-of range)] (fn [^double t] (g t)))
                      (let [a (double (range 0)) b (double (range 1))] (fn [^double t] (+ a (* t (- b a))))))]
              (fn [v]
                (cond
                  (nil? v) na
                  (and (number? v) (<= lo (double v) hi)) (f (t-of (double v)))
                  :else nil)))
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

(defn legend-entries
  "Legend entries `[{:label :value}]` of scale `spec` of dimension `k` (`:value nil` - the NA entry, last), or `nil`
  when it has no legend: `:legend false`, an identity scale, or linear / binned color (a color bar instead).
  Ordinal: one per level (`(str level)` or `:formatter`); linear size: ticks of the domain (labels as axis ticks)."
  [k spec]
  (when-not (or (false? (:legend spec)) (= :identity (:kind spec))
                (and (= k :color) (#{:linear :binned} (:kind spec))))
    (let [fmt (or (:formatter spec) str)
          entries (if (= :ordinal (:kind spec))
                    (mapv (fn [level] {:label (str (fmt level)) :value level}) (:domain spec))
                    (let [[lo hi] (:domain spec)
                          lo (double lo) hi (double hi)]
                      (let [ws (s/scale :linear (cond-> {:domain (if (== lo hi) [lo (inc lo)] [lo hi]) :ticks size-legend-ticks}
                                                  (:formatter spec) (assoc :formatter (:formatter spec))))
                            ticks (if (== lo hi) [lo] (filterv #(<= lo (double %) hi) (s/ticks ws)))]
                        (mapv (fn [t l] {:label l :value t}) ticks
                              (scale/tick-labels ws {:kind :linear :formatter (:formatter spec)} ticks)))))]
      (cond-> entries (:na? spec) (conj {:label "NA" :value nil})))))
