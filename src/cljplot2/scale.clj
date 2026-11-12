(ns cljplot2.scale
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
            [wadogo.scale :as s]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def continuous-kinds #{:linear :log :pow :symlog :datetime})
(def position-kinds (conj continuous-kinds :bands))

(def ^:private ^:const max-integral-label 1.0e15)

(defn- invalid [message data] (throw (ex-info message data)))

(defn- temporal? [v] (instance? java.time.temporal.Temporal v))

(defn- infer-kind
  [k domain]
  (cond
    (every? number? domain) :linear
    (every? #(or (string? %) (keyword? %)) domain) :bands
    (every? temporal? domain) :datetime
    :else (invalid "Scale domain values must be all numbers, all strings/keywords or all java.time values."
                   {:key :domain :scale k})))

(defn- validate-domain!
  [k kind domain]
  (let [error (fn [message] (invalid message {:key :domain :scale k}))]
    (when-not (and (sequential? domain) (seq domain))
      (error "Scale :domain must be a non-empty sequence."))
    (case kind
      :bands (when-not (apply distinct? domain) (error "Bands :domain values must be distinct."))
      :datetime (when-not (and (every? temporal? domain) (not= (first domain) (last domain)))
                  (error "Datetime :domain must be two different java.time values."))
      (do (when-not (every? #(and (number? %) (m/valid-double? (double %))) domain)
            (error "Scale :domain must contain finite numbers."))
          (when (== (double (reduce min domain)) (double (reduce max domain)))
            (error "Scale :domain must not be a single value."))
          (when (and (= kind :log) (not (every? #(pos? (double %)) domain)))
            (error "Log scale :domain must be positive."))))))

(defn- validate-tick-list!
  [k kind domain ticks]
  (let [valid? (case kind
                 :bands (set domain)
                 :datetime temporal?
                 #(and (number? %) (m/valid-double? (double %))))]
    (when-let [bad (first (remove valid? ticks))]
      (invalid (if (= kind :bands) "Bands :ticks must be domain values." "Scale :ticks contain an invalid value.")
               {:key :ticks :scale k :value bad}))))

(defn scale-spec
  "Validated phase-1 spec of the plot scale `k` from its attributes `spec` (a map of wadogo attributes and `:nice`).
  `:nice` (default `true`): extend continuous domains to nice values by at most 10% of the range; a number sets
  that limit (fraction); falsy turns it off.
  Throws `ex-info` naming the offending key."
  [k spec]
  (when-not (map? spec) (invalid "Scale spec must be a map." {:key :scales :scale k}))
  (let [{:keys [domain ticks]} spec
        _ (when (nil? domain) (invalid "Scale :domain is required." {:key :domain :scale k}))
        kind (or (:kind spec) (infer-kind k domain))]
    (when-not (position-kinds kind)
      (invalid "Unsupported position scale kind." {:key :kind :scale k :value kind :allowed position-kinds}))
    (validate-domain! k kind domain)
    (when (contains? spec :range)
      (invalid "Scale :range is set from the layout, it can't be given." {:key :range :scale k}))
    (cond
      (sequential? ticks) (validate-tick-list! k kind domain ticks)
      (not (or (nil? ticks) (and (integer? ticks) (pos? (long ticks)))))
      (invalid "Scale :ticks must be a positive integer or a sequence of values." {:key :ticks :scale k :value ticks}))
    (when (and (number? (:nice spec)) (not (and (m/valid-double? (double (:nice spec))) (>= (double (:nice spec)) 0.0))))
      (invalid "Scale :nice must be a boolean or a non-negative fraction." {:key :nice :scale k :value (:nice spec)}))
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

(def ^:private extent-kinds {:numeric :linear :categorical :bands :temporal :datetime})

(defn- validate-expand!
  [k expand]
  (when-not (and (vector? expand) (= 2 (count expand)) (every? #(and (number? %) (>= (double %) 0.0)) expand))
    (invalid "Scale :expand must be [lo hi], non-negative fractions." {:key :expand :scale k :value expand})))

(defn data-scale-spec
  "Phase-1 spec of plot scale `k` from its `spec` (may be `{}`) and the merged data `extent` (may be `nil`)
  with the charts' `padding` `[lo hi]`. An explicit `:domain` wins over data. A data domain gets
  `:kind` from the spec or the extent (numeric -> `:linear`, categorical -> `:bands`, temporal -> `:datetime`);
  numeric domains are widened when collapsed to one value and get `:data-padding` - the spec `:expand` or
  `padding` - applied by [[finalize]] after `nice`, so the outermost ticks sit inside the panel
  (temporal and categorical domains aren't padded)."
  [k spec extent padding]
  (when-not (map? spec) (invalid "Scale spec must be a map." {:key :scales :scale k}))
  (when (contains? spec :expand) (validate-expand! k (:expand spec)))
  (if (or (contains? spec :domain) (nil? extent))
    (scale-spec k spec)
    (let [kind (or (:kind spec) (extent-kinds (:kind extent)))
          domain (case (:kind extent)
                   :categorical (:values extent)
                   :temporal [(:min extent) (:max extent)]
                   :numeric (if (= :bands kind)
                              (invalid "Numeric data on a bands scale needs an explicit :domain." {:key :domain :scale k})
                              (widen (double (:min extent)) (double (:max extent)))))]
      (cond-> (scale-spec k (assoc spec :kind kind :domain domain))
        (= :numeric (:kind extent)) (assoc :data-padding (vec (or (:expand spec) padding [0 0])))))))

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

(defn- tick-labels
  "Default formatter: whole-number ticks of a numeric continuous scale as integers, otherwise wadogo `format`;
  a spec `:formatter` always wins (wadogo uses it)."
  [scale spec ticks]
  (if (and (nil? (:formatter spec)) (not= :datetime (:kind spec)) (not= :bands (:kind spec)) (integral-numbers? ticks))
    (mapv #(str (long (m/round (double %)))) ticks)
    (vec (s/format scale ticks))))

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
  the explicit tick count or the hint, then padded by `:data-padding` (data domains, see [[data-scale-spec]]);
  ticks outside the final domain are dropped."
  [spec range tick-hint]
  (let [kind (:kind spec)
        explicit (:ticks spec)
        base (assoc (dissoc spec :kind :nice :expand :data-padding) :range range)
        nice-count (if (integer? explicit) explicit tick-hint)
        niced (nice-domain (s/scale kind (assoc base :ticks nice-count)) kind (:nice spec))
        domain (if-let [padding (:data-padding spec)]
                 (pad kind [(double (first niced)) (double (last niced))] padding)
                 niced)
        attributes (assoc base :domain domain :ticks (or explicit tick-hint))
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
