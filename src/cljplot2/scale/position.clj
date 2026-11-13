(ns cljplot2.scale.position
  "Position scales in two phases (D5).

  Phase 1, [[scale-spec]]: a plot's `:scales` entry (wadogo attributes plus `:nice`) validated into a spec,
  `:kind` inferred from the domain when missing. Plain data.

  Phase 2, [[finalize]]: after layout, the spec with a pixel `:range` and a tick count hint becomes a finalized
  scale - plain data `{:kind :domain :range :ticks :tick-labels :attributes}`. Continuous kinds are extended to
  nice tick values (unless `:nice false`) with the explicit tick count, or with the pixel-based hint when the spec
  has no count (also when it lists its ticks). Ticks outside the final domain are dropped.

  [[->wadogo]] rebuilds the wadogo scale; [[forward-fn]] returns a value -> pixel function (build it once and reuse
  it); [[forward]] maps a single value."
  (:require [fastmath.core :as m]
            [wadogo.scale :as s]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def continuous-kinds #{:linear :log :pow :symlog :datetime})
(def position-kinds (conj continuous-kinds :bands))

(def ^:private ^:const max-integral-label 1.0e15)

(defn- temporal? [v] (instance? java.time.temporal.Temporal v))

(defn- domain-kind
  "Scale kind of an explicit `domain`: `:linear` (numbers), `:bands` (strings / keywords), `:datetime` (java.time
  values), or `nil` for mixed values."
  [domain]
  (cond
    (every? number? domain) :linear
    (every? #(or (string? %) (keyword? %)) domain) :bands
    (every? temporal? domain) :datetime))

(defn- domain-problem
  "Why the explicit `:domain` of `spec` is invalid for the spec's kind (given or inferred), or `nil`."
  [{:keys [domain] :as spec}]
  (when (some? domain)
    (let [kind (or (:kind spec) (domain-kind domain))]
      (case kind
        nil "domain values should be all numbers, all strings / keywords or all java.time values"
        :bands (when-not (apply distinct? domain) "bands domain values should be distinct")
        :datetime (when-not (and (every? temporal? domain) (not= (first domain) (last domain)))
                    "datetime domain should be two different java.time values")
        (cond
          (not-every? schema/finite? domain) "domain should contain finite numbers"
          (== (double (reduce min domain)) (double (reduce max domain))) "domain should not be a single value"
          (and (= kind :log) (not (every? #(pos? (double %)) domain))) "log domain should be positive")))))

(def ^:private wadogo-attributes
  "wadogo scale attributes passed through to the scale (see wadogo's kinds)."
  [:clamp :unknown :round :base :exponent :padding-in :padding-out :align-outer :align])

(def ScaleSpec
  "Plot scale spec of `:x` / `:y`: wadogo attributes plus `:nice` and `:expand`, see [[scale-spec]] and
  [[data-scale-spec]]."
  (schema/with-rules
   (into [:map {:closed true}
          (schema/optional :kind (into [:enum] (sort position-kinds)))
          (schema/nilable :domain [:sequential {:min 1} any?])
          (schema/nilable :ticks [:multi {:dispatch sequential?} [true [:sequential any?]] [false schema/pos-int]])
          (schema/nilable :nice [:multi {:dispatch boolean?} [true :boolean] [false [:and schema/finite schema/non-neg]]])
          (schema/optional :expand schema/padding)
          (schema/optional :formatter schema/function)
          (schema/optional :range (schema/forbidden "is set from the layout, it can't be given"))
          (schema/optional :padding (schema/forbidden "is internal; use :expand"))
          (schema/optional :tick-format (schema/forbidden "is internal (set by charts); use :formatter"))]
         (map #(schema/optional % any?))
         wadogo-attributes)
   [domain-problem [:domain]]))

(defn- validate-tick-list!
  [k kind domain ticks]
  (let [valid? (case kind
                 :bands (set domain)
                 :datetime temporal?
                 schema/finite?)]
    (when-let [bad (first (remove valid? ticks))]
      (schema/fail! (if (= kind :bands) "Bands :ticks must be domain values." "Scale :ticks contain an invalid value.")
                    {:key :ticks :in [:scales k :ticks] :value bad}))))

(defn scale-spec
  "Phase-1 spec of the plot scale `k` from its attributes `spec` (a map of wadogo attributes and `:nice`, valid by
  [[ScaleSpec]]): `:kind` inferred from the domain when missing; no `:domain`, or a data domain not suiting the
  kind (e.g. `:log` with 0) throws `ex-info {:key :domain}`.
  `:nice` (default `true`): extend continuous domains to nice values by at most 10% of the range; a number sets
  that limit (fraction); falsy turns it off.
  A tick list is checked against the kind and domain (`ex-info {:key :ticks}`)."
  [k spec]
  (let [{:keys [domain ticks]} spec
        _ (when (nil? domain)
            (schema/fail! "Scale :domain is required when there is no data." {:key :domain :in [:scales k :domain]}))
        kind (or (:kind spec) (domain-kind domain))]
    (when-let [problem (domain-problem spec)] ;; a data domain may not suit the spec's kind (e.g. log)
      (schema/fail! (str "Scale " problem ".") {:key :domain :in [:scales k :domain] :value domain}))
    (when (sequential? ticks) (validate-tick-list! k kind domain ticks))
    (assoc spec :kind kind :nice (get spec :nice true))))

(def ^:private ^:const collapse-widening 0.1)

(defn- widen
  "Numeric `[lo hi]`; a single value `v` widens to `[v - 0.1|v|, v + 0.1|v|]` (`[-1 1]` for 0)."
  [^double lo ^double hi]
  (cond
    (< lo hi) [lo hi]
    (zero? lo) [-1.0 1.0]
    :else (let [d (* collapse-widening (m/abs lo))] [(- lo d) (+ lo d)])))

(defn- pad
  "Extend `[lo hi]` by fractions `[plo phi]` of its span (in log10 space for `:log`)."
  [kind [^double lo ^double hi] [plo phi]]
  (let [log? (= kind :log)
        a (if log? (m/log10 lo) lo)
        b (if log? (m/log10 hi) hi)
        span (- b a)
        a (- a (* (double plo) span))
        b (+ b (* (double phi) span))]
    (if log? [(m/pow 10.0 a) (m/pow 10.0 b)] [a b])))

(defn- pad-pixels
  "Extend `[lo hi]` so that `[plo phi]` pixels of a `length` pixels long range lie beyond it (in log10 space for
  `:log`); unchanged when the room would take more than half the range."
  [kind [^double lo ^double hi] [plo phi] ^double length]
  (let [plo (double plo) phi (double phi)
        room (+ plo phi)]
    (if (or (<= room 0.0) (> room (* 0.5 length)))
      [lo hi]
      (let [log? (= kind :log)
            a (if log? (m/log10 lo) lo)
            b (if log? (m/log10 hi) hi)
            ;; data per pixel of the extended domain: (b - a) / (length - room)
            per-px (/ (- b a) (- length room))
            a (- a (* plo per-px))
            b (+ b (* phi per-px))]
        (if log? [(m/pow 10.0 a) (m/pow 10.0 b)] [a b])))))

(def ^:private extent-kinds {:numeric :linear :categorical :bands :temporal :datetime})

(defn data-scale-spec
  "Phase-1 spec of plot scale `k` from its `spec` (may be `{}`) and the merged data `extent` (may be `nil`)
  with the charts' `padding` `[lo hi]`. An explicit `:domain` wins over data (`nil` - not set). A data domain gets
  `:kind` from the spec or the extent (numeric -> `:linear`, categorical -> `:bands`, temporal -> `:datetime`); a
  numeric extent on a `:bands` scale needs an explicit `:domain` unless it carries its levels (`:values`, sorted);
  numeric data domains are widened when collapsed to one value.
  Numeric continuous domains - data or explicit - get `:padding`: the spec `:expand`, else the charts' `padding`
  (only when there is data); [[finalize]] applies it after `nice`, so the outermost ticks sit inside the panel.
  Temporal and categorical domains aren't padded."
  [k spec extent padding]
  (let [built (if (or (some? (:domain spec)) (nil? extent))
                (scale-spec k spec)
                (let [kind (or (:kind spec) (extent-kinds (:kind extent)))
                      domain (case (:kind extent)
                               :categorical (:values extent)
                               :temporal [(:min extent) (:max extent)]
                               :numeric (cond
                                          (not= :bands kind) (widen (double (:min extent)) (double (:max extent)))
                                          ;; levels reported by the charts (strip charts' numeric :y levels)
                                          (:values extent) (vec (sort (:values extent)))
                                          :else (schema/fail! "Numeric data on a bands scale needs an explicit :domain."
                                                              {:key :domain :in [:scales k :domain]})))]
                  (scale-spec k (assoc spec :kind kind :domain domain))))
        padding (or (:expand spec) (when extent padding))]
    (cond-> built
      (and padding (not= :datetime (:kind built)) (continuous-kinds (:kind built)))
      (assoc :padding (vec padding)))))

(defn tick-hint
  "Tick count hint for an axis `length` pixels long with `spacing` pixels between ticks (at least 2)."
  ^long [^double length ^double spacing]
  (m/max 2 (long (m/floor (/ length spacing)))))

(defn- integral-numbers?
  [ticks]
  (and (seq ticks)
       (every? #(and (number? %)
                     (let [v (double %)]
                       (and (m/valid-double? v) (< (m/abs v) max-integral-label) (== v (m/floor v)))))
               ticks)))

(defn- default-tick-labels
  [scale spec ticks]
  (if (and (nil? (:formatter spec)) (not= :datetime (:kind spec)) (not= :bands (:kind spec)) (integral-numbers? ticks))
    (mapv #(str (long (m/round (double %)))) ticks)
    (vec (s/format scale ticks))))

(defn tick-labels
  "Labels of `ticks` of wadogo `scale` built from `spec` (`:kind`, `:formatter`, `:tick-format`).
  Default formatter: whole-number ticks of a numeric continuous scale as integers, otherwise wadogo `format`;
  `:tick-format :absolute` formats the magnitudes of numeric ticks; a spec `:formatter` always wins (wadogo uses it)."
  [scale spec ticks]
  (let [ticks (if (and (= :absolute (:tick-format spec)) (nil? (:formatter spec)))
                (mapv #(if (number? %) (m/abs (double %)) %) ticks)
                ticks)]
    (default-tick-labels scale spec ticks)))

(defn ->wadogo
  "wadogo scale of a finalized scale."
  [finalized]
  (s/scale (:kind finalized) (:attributes finalized)))

(def ^:private ^:const range-tolerance 1.0e-6)

(defn- inside-range?
  "Does pixel `p` lie within `range` (either orientation), up to a small tolerance?"
  [[a b] ^double p]
  (let [a (double a) b (double b)]
    (<= (- (m/min a b) range-tolerance) p (+ (m/max a b) range-tolerance))))

(def ^:private ^:const default-nice-max-extension
  "Largest total extension of the domain by `nice`, as a fraction of the range span."
  0.1)

(defn- nice-domain
  "Domain of `scale` extended by `nice` (wadogo, limited extension): `nice-option` `true` -> default limit,
  a number -> that limit, falsy -> unchanged."
  [scale kind nice-option]
  (if (and nice-option (continuous-kinds kind))
    (s/domain (s/nice scale {:max-extension (if (number? nice-option) nice-option default-nice-max-extension)}))
    (s/domain scale)))

(defn finalize
  "Finalized scale of phase-1 `spec` for the pixel `range` (`[start end]`) and `tick-hint` (the count used unless
  the spec has `:ticks`). Steps: the domain is extended by `nice` (limited, see `:nice` in [[scale-spec]]) with
  the explicit tick count or the hint, then padded by `:padding` (see [[data-scale-spec]]), then by the pixel room
  `:padding-px` `[lo hi]` (charts' labels past the data; `:linear` and `:log` only, skipped when over half the
  range);
  ticks outside the final domain are dropped. A bands scale without `:ticks` has a tick at every band."
  [spec range tick-hint]
  (let [kind (:kind spec)
        explicit (:ticks spec)
        base (assoc (dissoc spec :kind :nice :expand :padding :padding-px :tick-format) :range range)
        nice-count (if (integer? explicit) explicit tick-hint)
        niced (nice-domain (s/scale kind (assoc base :ticks nice-count)) kind (:nice spec))
        domain (if-let [padding (:padding spec)]
                 (pad kind [(double (first niced)) (double (last niced))] padding)
                 niced)
        domain (if-let [padding-px (when (#{:linear :log} kind) (:padding-px spec))]
                 (pad-pixels kind [(double (first domain)) (double (last domain))] padding-px
                             (m/abs (- (double (second range)) (double (first range)))))
                 domain)
        attributes (assoc base :domain domain :ticks (or explicit (if (= kind :bands) (vec domain) tick-hint)))
        scale (s/scale kind attributes)
        ticks (if (continuous-kinds kind)
                (filterv #(inside-range? range (double (scale %))) (s/ticks scale))
                (vec (s/ticks scale)))]
    {:kind kind
     :domain domain
     :range range
     :ticks ticks
     :tick-labels (tick-labels scale spec ticks)
     :attributes attributes}))

(defn forward-fn
  "Function mapping a value to its pixel position in the finalized scale (band center for `:bands`); `##NaN` for a
  value the scale can't place (e.g. not in a bands domain) - callers skip such values.
  Builds the wadogo scale once; reuse the function for many values."
  [finalized]
  (let [scale (->wadogo finalized)]
    (fn ^double [value] (let [p (scale value)] (if (nil? p) ##NaN (double p))))))

(defn forward
  "Pixel position of a single `value` in a finalized scale; for many values use [[forward-fn]]."
  ^double [finalized value]
  ((forward-fn finalized) value))

(defn band-geometry
  "`{:bandwidth px :step px}` of a finalized `:bands` scale (wadogo's band width and the distance between band
  starts), `nil` for other scales."
  [finalized]
  (when (= :bands (:kind finalized))
    (select-keys (s/data (->wadogo finalized)) [:bandwidth :step])))

;; directions (plot `:flip`)

(defn horizontal-key
  "Position scale key drawn horizontally: `:y` when the plot is flipped (`flip?`), else `:x`."
  [flip?]
  (if flip? :y :x))

(defn direction
  "Direction of the finalized position scale `finalized` of key `k`: its `:direction` (`:horizontal` / `:vertical`,
  set by the pipeline), else by key (`:x` horizontal, `:y` vertical)."
  [k finalized]
  (or (:direction finalized) (if (= k :x) :horizontal :vertical)))

(defn horizontal?
  "Is the finalized scale `finalized` of key `k` drawn horizontally (see [[direction]])?"
  [k finalized]
  (= :horizontal (direction k finalized)))

(defn flipped?
  "Do the finalized position `scales` (`{:x .. :y ..}`, either may be missing) draw `:x` vertically?"
  [scales]
  (cond
    (:x scales) (not (horizontal? :x (:x scales)))
    (:y scales) (horizontal? :y (:y scales))
    :else false))

(defn- forward-or-nan
  [finalized]
  (if finalized (forward-fn finalized) (fn ^double [_] ##NaN)))

(defn projection
  "Function of data `x`, `y` to the pixel point (`Vec2`) of the finalized position `scales`: the horizontal scale's
  position gives the point's x, the vertical scale's its y (`:x` horizontal unless [[flipped?]]). A value the scale
  can't place, or a missing scale, gives `##NaN` in that coordinate. Charts place marks only through it.
  Builds the wadogo scales once; reuse the function for many points."
  [scales]
  (let [fx (forward-or-nan (:x scales))
        fy (forward-or-nan (:y scales))]
    (if (flipped? scales)
      (fn ^Vec2 [x y] (Vec2. (double (fy y)) (double (fx x))))
      (fn ^Vec2 [x y] (Vec2. (double (fx x)) (double (fy y)))))))
