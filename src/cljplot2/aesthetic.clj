(ns cljplot2.aesthetic
  "Aesthetics: the non-positional dimensions `:color`, `:shape` and `:size` mapped from data (D4), and their scales (D5).

  Mapping: an accessor (keyword or function), `nil` = not mapped. A row's value is kept when it is valid
  ([[cljplot2.data/valid-value?]]) or `nil` - the NA level, drawn with the NA attribute and listed last as \"NA\"
  (plot `:groups {:na false}` drops such rows instead). Other values (NaN, infinite, ...) are dropped.

  Scale spec - plot `:scales {dim spec}`:

  * `:kind` - `:ordinal` or `:linear`; default `:linear` for numeric `:color` / `:size` data or domain, otherwise
    `:ordinal` (`:shape` is always ordinal)
  * `:domain` - ordinal: distinct levels (default: data levels, sorted); linear: `[lo hi]` (default: data min, max)
  * `:range` - default from theme `:palette` (`:color`, `:gradient`, `:shape`, `:size`):
    ordinal `:color` - palette name or colors (cycled); linear `:color` - palette name, colors (a gradient) or a
    function of `t` in `[0 1]`; `:shape` - shapes (cycled); `:size` - `[min max]` (ordinal levels evenly spread)
  * `:formatter` - legend labels (function); `:title` - legend title (`nil` - no title); `:legend` - `false` hides it

  [[scale-spec]] gives plain data (with `:na?` - the scale has an NA level); it is also the finalized scale: no
  pixel range, `nice` or padding. [[attribute-fn]] maps data values to style attributes: NA (`nil`), values outside
  an ordinal domain and non-numbers on a linear scale map to the NA attribute; linear scales clamp to the domain."
  (:require [clojure2d.color :as c]
            [fastmath.core :as m]
            [wadogo.scale :as s]
            [cljplot2.data :as data]
            [cljplot2.scale :as scale]
            [cljplot2.shape :as shape]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def dims
  "Aesthetic dimensions, in legend order."
  [:color :shape :size])

(def dim? (set dims))

(def kinds #{:ordinal :linear})

(def ^:private spec-keys #{:kind :domain :range :formatter :title :legend})

(def ^:private ^:const size-legend-ticks 5)

(defn- invalid [message data] (throw (ex-info message data)))

(defn mapped-dims
  "Aesthetic dimensions with a non-nil accessor in `mapping`."
  [mapping]
  (filterv #(some? (get mapping %)) dims))

(defn- valid-color? [col] (and (some? col) (try (c/to-color col) true (catch Exception _ false))))

(defn- palette-colors
  "Colors of palette `p` (a palette name or a non-empty sequence of colors); errors carry `{:key error-key :scale k}`."
  [k p error-key]
  (let [colors (cond
                 (keyword? p) (try (vec (c/palette p)) (catch Exception _ nil))
                 (sequential? p) (vec p))]
    (when-not (and (seq colors) (every? valid-color? colors))
      (invalid "Colors must be a palette name or a non-empty sequence of colors." {:key error-key :scale k :value p}))
    colors))

(defn validate-palette!
  "Check theme `:palette` `{:color :gradient :shape :size :na}`; throws `ex-info` naming the offending key."
  [{:keys [color gradient shape size na]}]
  (palette-colors :color color :palette)
  (when-not (fn? gradient) (palette-colors :gradient gradient :palette))
  (when-not (and (sequential? shape) (seq shape) (every? shape/shapes shape))
    (invalid "Palette :shape must be a non-empty sequence of shapes." {:key :shape :value shape}))
  (when-not (and (vector? size) (= 2 (count size)) (every? #(and (number? %) (>= (double %) 0.0)) size)
                 (<= (double (size 0)) (double (size 1))))
    (invalid "Palette :size must be [min max], non-negative numbers." {:key :size :value size}))
  (when-not (valid-color? na)
    (invalid "Palette :na must be a color." {:key :na :value na})))

(defn- resolve-range
  "Concrete range of dimension `k` and `kind` from range value `r`."
  [k kind r]
  (case k
    :color (if (and (= kind :linear) (fn? r)) r (palette-colors k r :range))
    :shape (do (when-not (and (sequential? r) (seq r) (every? shape/shapes r))
                 (invalid "Shape :range must be a non-empty sequence of shapes." {:key :shape :scale k :value r}))
               (vec r))
    :size (do (when-not (and (vector? r) (= 2 (count r)) (every? #(and (number? %) (>= (double %) 0.0)) r)
                             (<= (double (r 0)) (double (r 1))))
                (invalid "Size :range must be [min max], non-negative numbers." {:key :size :scale k :value r}))
              r)))

(defn- validate-spec!
  [k spec]
  (when-not (map? spec) (invalid "Scale spec must be a map." {:key :scales :scale k}))
  (when-let [bad (first (remove spec-keys (keys spec)))]
    (invalid "Unknown aesthetic scale attribute." {:key :scales :scale k :value bad :allowed spec-keys}))
  (when (and (contains? spec :kind) (not (kinds (:kind spec))))
    (invalid "Aesthetic scale :kind must be :ordinal or :linear." {:key :kind :scale k :value (:kind spec)}))
  (when (and (= k :shape) (= :linear (:kind spec)))
    (invalid "Shape scale can't be :linear." {:key :kind :scale k}))
  (when (and (contains? spec :formatter) (not (fn? (:formatter spec))))
    (invalid "Scale :formatter must be a function." {:key :formatter :scale k}))
  (when-not (or (nil? (:title spec)) (string? (:title spec)))
    (invalid "Scale :title must be a string or nil." {:key :title :scale k}))
  (when-not (contains? #{nil true false} (:legend spec))
    (invalid "Scale :legend must be a boolean." {:key :legend :scale k})))

(defn- ordinal-domain
  [k explicit extent]
  (if explicit
    (do (when-not (and (sequential? explicit) (seq explicit) (not-any? nil? explicit) (apply distinct? explicit))
          (invalid "Ordinal :domain must be a non-empty sequence of distinct non-nil values." {:key :domain :scale k}))
        (vec explicit))
    (data/sorted-levels k (:values extent) :domain)))

(defn- linear-domain
  [k explicit extent]
  (if explicit
    (do (when-not (and (vector? explicit) (= 2 (count explicit))
                       (every? #(and (number? %) (m/valid-double? (double %))) explicit)
                       (< (double (explicit 0)) (double (explicit 1))))
          (invalid "Linear :domain must be [lo hi], finite numbers, lo < hi." {:key :domain :scale k}))
        [(double (explicit 0)) (double (explicit 1))])
    (do (when-not (= :numeric (:kind extent))
          (invalid "A :linear aesthetic scale needs numeric data." {:key :kind :scale k}))
        [(double (:min extent)) (double (:max extent))])))

(defn scale-spec
  "Scale of aesthetic dimension `k` from the plot's scale `spec` (may be `{}`), the merged data `extent`
  (a [[cljplot2.data/value-extent]], may be `nil`), `na?` (some mapped value is NA) and theme `palette`.
  `nil` when there is neither an explicit domain nor data.
  Returns `{:kind :domain :range :na?}` plus the spec's `:formatter`, `:title`, `:legend` when given."
  [k spec extent na? palette]
  (validate-spec! k spec)
  (let [explicit (:domain spec)]
    (when (or explicit extent)
      (let [numeric? (if explicit (every? number? explicit) (= :numeric (:kind extent)))
            kind (or (:kind spec) (if (and numeric? (not= k :shape)) :linear :ordinal))
            domain (if (= kind :ordinal) (ordinal-domain k explicit extent) (linear-domain k explicit extent))
            outside? (and (= kind :ordinal) explicit (some #(not (contains? (set domain) %)) (:values extent)))
            default-range (get palette (if (and (= k :color) (= kind :linear)) :gradient k))]
        (cond-> {:kind kind
                 :domain domain
                 :range (resolve-range k kind (get spec :range default-range))
                 :na? (boolean (or na? outside?))}
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
  "`v` normalized into `[0 1]` on `[lo hi]` (clamped; 0.5 for a zero span)."
  ^double [^double lo ^double hi ^double v]
  (if (== lo hi) 0.5 (m/constrain (/ (- v lo) (- hi lo)) 0.0 1.0)))

(defn attribute-fn
  "Function of a data value of dimension `k` with scale `spec` giving its style attribute (color, shape, size);
  NA values give `na`."
  [k spec na]
  (case (:kind spec)
    :ordinal (let [index (zipmap (:domain spec) (ordinal-attributes k spec))]
               (fn [v] (get index v na)))
    :linear (let [[lo hi] (:domain spec)
                  lo (double lo) hi (double hi)
                  range (:range spec)
                  f (if (= k :color)
                      (let [g (if (fn? range) range (c/gradient range))] (fn [^double t] (g t)))
                      (let [a (double (range 0)) b (double (range 1))] (fn [^double t] (+ a (* t (- b a))))))]
              (fn [v] (if (and (number? v) (m/valid-double? (double v))) (f (unit lo hi (double v))) na)))))

(defn legend-entries
  "Legend entries `[{:label :value}]` of scale `spec` of dimension `k` (`:value nil` - the NA entry, last), or `nil`
  when it has no legend: `:legend false` or linear color (a color bar, not drawn yet).
  Ordinal: one per level (`(str level)` or `:formatter`); linear size: ticks of the domain."
  [k spec]
  (when-not (or (false? (:legend spec)) (and (= k :color) (= :linear (:kind spec))))
    (let [fmt (or (:formatter spec) str)
          entries (if (= :ordinal (:kind spec))
                    (mapv (fn [level] {:label (str (fmt level)) :value level}) (:domain spec))
                    (let [[lo hi] (:domain spec)
                          lo (double lo) hi (double hi)]
                      (if (== lo hi)
                        [{:label (str (fmt lo)) :value lo}]
                        (let [ws (s/scale :linear (cond-> {:domain [lo hi] :ticks size-legend-ticks}
                                                    (:formatter spec) (assoc :formatter (:formatter spec))))
                              ticks (filterv #(<= lo (double %) hi) (s/ticks ws))]
                          (mapv (fn [t l] {:label l :value t}) ticks
                                (scale/tick-labels ws {:kind :linear :formatter (:formatter spec)} ticks))))))]
      (cond-> entries (:na? spec) (conj {:label "NA" :value nil})))))
