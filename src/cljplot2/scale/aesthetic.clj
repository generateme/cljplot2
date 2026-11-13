(ns cljplot2.scale.aesthetic
  "Aesthetics: the non-positional dimensions `:color`, `:shape` and `:size` mapped from data (D4), and their scales (D5).

  Mapping: an accessor (keyword or function), `nil` = not mapped. A row's value is kept when it is valid
  ([[cljplot2.data.rows/valid-value?]]) or `nil` - the NA level, drawn with the NA attribute and listed last as \"NA\"
  (plot `:groups {:na false}` drops such rows instead). Other values (NaN, infinite, ...) are dropped.

  Scale spec - plot `:scales {dim spec}`:

  * `:kind` - `:ordinal` or `:linear`; default `:linear` for numeric `:color` / `:size` data or a numeric explicit
    domain of two values, otherwise `:ordinal` (`:shape` is always ordinal); numeric data with more than
    [[cljplot2.data.rows/max-levels]] distinct values can be ordinal only with an explicit `:domain`
  * `:domain` - ordinal: distinct levels (default: data levels, sorted; numbers equal as doubles, `1931` = `1931.0`);
    linear: `[lo hi]` (default: data min, max)
  * `:range` - default from theme `:palette` (`:color`, `:gradient`, `:shape`, `:size`):
    ordinal `:color` - palette name or colors (cycled); linear `:color` - palette name, colors (a gradient) or a
    function of `t` in `[0 1]`; `:shape` - shapes (cycled); `:size` - `[min max]` (ordinal levels evenly spread)
  * `:formatter` - legend labels (function); `:title` - legend title (`nil` - no title); `:legend` - `false` hides it

  [[scale-spec]] gives plain data (with `:na?` - the scale has an NA level); it is also the finalized scale: no
  pixel range, `nice` or padding. [[attribute-fn]] maps data values to style attributes: NA (`nil`) to the NA
  attribute; values outside the domain (not a level, or outside `[lo hi]`) to `nil` - such points are skipped.
  Charts drop values outside an explicit domain already at compute ([[in-domain?]]), counted in `:dropped`, so
  extents only cover what is drawn."
  (:require [clojure2d.color :as c]
            [wadogo.scale :as s]
            [cljplot2.data.rows :as data]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def dims
  "Aesthetic dimensions, in legend order."
  [:color :shape :size])

(def dim? (set dims))

(def kinds #{:ordinal :linear})

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

(def ^:private gradient [:multi {:dispatch fn?} [true schema/function] [false palette]])

(def Palette
  "Theme `:palette` (aesthetic defaults)."
  [:map {:closed true}
   [:color palette]
   [:gradient gradient]
   [:shape shapes]
   [:size size-range]
   [:na schema/solid-color]])

(defn- level-key
  "Identity of an ordinal level: numbers compare as doubles (`1931` = `1931.0`), other values as themselves."
  [v]
  (if (number? v) (double v) v))

(defn- explicit-kind
  "Kind of aesthetic `k` with an explicit domain in `spec`: the spec's `:kind`, else `:linear` for two numbers (not
  `:shape`), else `:ordinal`."
  [k {:keys [kind domain]}]
  (or kind (if (and (not= k :shape) (= 2 (count domain)) (every? number? domain)) :linear :ordinal)))

(defn- domain-problem
  "Why the explicit `:domain` of aesthetic scale `spec` of `k` is invalid, or `nil`."
  [k {:keys [domain] :as spec}]
  (when (some? domain)
    (case (explicit-kind k spec)
      :ordinal (when-not (and (seq domain) (not-any? nil? domain) (apply distinct? (map level-key domain)))
                 "ordinal domain should be a non-empty sequence of distinct non-nil values")
      :linear (when-not (and (= 2 (count domain)) (every? schema/finite? domain)
                             (< (double (first domain)) (double (second domain))))
                "linear domain should be [lo hi], finite numbers, lo < hi"))))

(defn ScaleSpec
  "Schema of the plot scale spec of aesthetic `k`."
  [k]
  (schema/with-rules
   [:map {:closed true}
    (schema/optional :kind (into [:enum] (sort kinds)))
    (schema/nilable :domain [:sequential any?])
    (schema/optional :range (case k :color [:multi {:dispatch fn?} [true schema/function] [false palette]]
                                  :shape shapes
                                  :size size-range))
    (schema/optional :formatter schema/function)
    (schema/nilable :title string?)
    (schema/nilable :legend schema/bool)]
   [#(when (and (= k :shape) (= :linear (:kind %))) "shape scale can't be :linear") [:kind]]
   [#(domain-problem k %) [:domain]]))

(defn- resolve-range
  "Concrete range of dimension `k` and `kind` from the (validated) range value `r`; a function needs a linear color
  scale."
  [k kind r]
  (case k
    :color (cond
             (not (fn? r)) (palette-colors r)
             (= kind :linear) r
             :else (schema/fail! "A color :range function needs a linear scale." {:key :range :in [:scales k :range]}))
    :shape (vec r)
    :size r))

(defn- ordinal-domain
  [k explicit extent]
  (cond
    explicit (vec explicit)
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
  (`:color`, `:size`); else `:ordinal`. `:shape` is always ordinal."
  [k spec data-numeric?]
  (if (sequential? (:domain spec))
    (explicit-kind k spec)
    (or (:kind spec) (if (and data-numeric? (not= k :shape)) :linear :ordinal))))

(defn in-domain?
  "Is data value `v` of aesthetic `k` inside the explicit `:domain` of the plot's scale `spec` (may be `{}`)?
  True without an explicit domain (a data domain holds all data), for `nil` (NA is handled separately) and for a
  malformed domain (rejected later by [[scale-spec]]). Ordinal: one of the levels (numbers equal as doubles);
  linear: a number within `[lo hi]`."
  [k spec v]
  (let [domain (:domain spec)]
    (if (or (nil? v) (not (sequential? domain)) (empty? domain))
      true
      (case (inferred-kind k spec (number? v))
        :ordinal (boolean (some #(= (level-key %) (level-key v)) domain))
        :linear (let [[lo hi] domain]
                  (if (and (= 2 (count domain)) (number? lo) (number? hi))
                    (and (number? v) (<= (double lo) (double v) (double hi)))
                    true))))))

(defn scale-spec
  "Scale of aesthetic dimension `k` from the plot's scale `spec` (may be `{}`), the merged data `extent`
  (a [[cljplot2.data.rows/value-extent]], may be `nil`), `na?` (some mapped value is NA) and theme `palette`.
  `nil` when there is neither an explicit domain nor data.
  Returns `{:kind :domain :range :na?}` plus the spec's `:formatter`, `:title`, `:legend` when given."
  [k spec extent na? palette]
  (let [explicit (:domain spec)]
    (when (or explicit extent)
      (let [kind (inferred-kind k spec (= :numeric (:kind extent)))
            domain (if (= kind :ordinal) (ordinal-domain k explicit extent) (linear-domain k explicit extent))
            default-range (get palette (if (and (= k :color) (= kind :linear)) :gradient k))]
        (cond-> {:kind kind
                 :domain domain
                 :range (resolve-range k kind (get spec :range default-range))
                 :na? (boolean na?)}
          (contains? spec :formatter) (assoc :formatter (:formatter spec))
          (contains? spec :title) (assoc :title (:title spec))
          (contains? spec :legend) (assoc :legend (:legend spec)))))))

(defn- spread
  "`n` values evenly spread over `[lo hi]` (the midpoint for one)."
  [^long n [lo hi]]
  (let [lo (double lo) hi (double hi)]
    (if (= n 1)
      [(* 0.5 (+ lo hi))]
      (mapv #(+ lo (* (- hi lo) (/ (double %) (double (dec n))))) (range n)))))

(defn- ordinal-attributes
  "Attribute of every level of ordinal scale `spec` of dimension `k`."
  [k {:keys [domain range]}]
  (let [n (count domain)]
    (if (= k :size)
      (spread n range)
      (mapv #(nth range (mod (long %) (count range))) (clojure.core/range n)))))

(defn- unit
  "`v` (within `[lo hi]`) normalized into `[0 1]`; 0.5 for a zero span."
  ^double [^double lo ^double hi ^double v]
  (if (== lo hi) 0.5 (/ (- v lo) (- hi lo))))

(defn attribute-fn
  "Function of a data value of dimension `k` with scale `spec` giving its style attribute (color, shape, size):
  `nil` (NA) gives `na`; a value outside the domain (not a level; not a number within `[lo hi]`) gives `nil` -
  callers skip it."
  [k spec na]
  (case (:kind spec)
    :ordinal (let [index (zipmap (map level-key (:domain spec)) (ordinal-attributes k spec))]
               (fn [v] (if (nil? v) na (get index (level-key v)))))
    :linear (let [[lo hi] (:domain spec)
                  lo (double lo) hi (double hi)
                  range (:range spec)
                  f (if (= k :color)
                      (let [g (if (fn? range) range (c/gradient range))] (fn [^double t] (g t)))
                      (let [a (double (range 0)) b (double (range 1))] (fn [^double t] (+ a (* t (- b a))))))]
              (fn [v]
                (cond
                  (nil? v) na
                  (and (number? v) (<= lo (double v) hi)) (f (unit lo hi (double v)))
                  :else nil)))))

(defn legend-entries
  "Legend entries `[{:label :value}]` of scale `spec` of dimension `k` (`:value nil` - the NA entry, last), or `nil`
  when it has no legend: `:legend false` or linear color (a color bar, not drawn yet).
  Ordinal: one per level (`(str level)` or `:formatter`); linear size: ticks of the domain (labels as axis ticks)."
  [k spec]
  (when-not (or (false? (:legend spec)) (and (= k :color) (= :linear (:kind spec))))
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
