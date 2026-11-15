(ns cljplot2.scale.position
  "Position scales in two phases (D5).

  Phase 1, [[scale-spec]]: a plot's `:scales` entry (wadogo attributes plus `:nice`) validated into a spec,
  `:kind` inferred from the domain when missing. Plain data.

  Phase 2, [[finalize]]: after layout, the spec with a pixel `:range` and a tick count hint becomes a finalized
  scale - plain data `{:kind :domain :range :ticks :tick-labels :attributes}`. Continuous kinds are extended to
  nice tick values (unless `:nice false`) with the explicit tick count, or with the pixel-based hint when the spec
  has no count (also when it lists its ticks). Ticks outside the final domain are dropped.

  [[->wadogo]] rebuilds the wadogo scale; [[forward-fn]] returns a value -> pixel function (build it once and reuse
  it); [[forward]] maps a single value; [[inverse-fn]] a pixel -> value function.

  Kinds: `:linear`, `:log`, `:pow`, `:symlog`, `:radial` (square-root like: distances from the range start grow as
  the square root of the distance from the domain start, as radii of areas; no padding below the data from the
  charts - the steep start would stretch it), `:interpolated` (an explicit `:domain` of n increasing numbers
  mapped piecewise to `:stops` - n strictly increasing (or decreasing: a reversed axis) fractions of the panel,
  default evenly spaced - through `:interpolator`
  / `:interpolator-params`, default `:linear`; non-monotone methods as `:cubic` can overshoot; no padding and no
  nice unless `:nice` is given; explicit `:ticks` suit its stretched parts), `:datetime`, `:bands`.
  Ticks of continuous kinds: `:ticks-method` `:default` (d3) or `:pretty` (R `pretty()`; default for `:datetime`)
  with `:ticks-params` `{:min-n n}` (clamped to the tick count) and, for `:datetime` `:pretty`, calendar ticks
  `{:unit u :step s}` (a tick every s units: `:seconds` .. `:years`, see wadogo; more than wadogo's
  `max-calendar-ticks` over the domain - `ex-info {:key :ticks-params}`). `:formatter` - a function of a tick
  value, or on `:datetime` a `java.time` pattern string (`\"MMM yyyy\"`)."
  (:require [fastmath.core :as m]
            [wadogo.scale :as s]
            [wadogo.ticks.pretty-datetime :as pretty-datetime]
            [cljplot2.data.rows :as rows]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec2]
           [java.time LocalDateTime]
           [java.time.format DateTimeFormatter]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def continuous-kinds #{:linear :log :pow :symlog :radial :interpolated :datetime})
(def position-kinds (conj continuous-kinds :bands))

(def ^:private ^:const max-integral-label 1.0e15)

(defn- temporal? [v] (instance? java.time.temporal.Temporal v))

(defn- domain-kind
  "Scale kind of an explicit `domain`: `:linear` (numbers), `:bands` (strings / keywords, or ordered levels such as
  intervals - see [[cljplot2.data.rows/ordered-level?]]), `:datetime` (java.time values), or `nil` for mixed values."
  [domain]
  (cond
    (every? number? domain) :linear
    (every? #(or (string? %) (keyword? %)) domain) :bands
    (every? rows/ordered-level? domain) :bands
    (every? temporal? domain) :datetime))

(defn- domain-problem
  "Why the explicit `:domain` of `spec` is invalid for the spec's kind (given or inferred), or `nil`."
  [{:keys [domain] :as spec}]
  (when (some? domain)
    (let [kind (or (:kind spec) (domain-kind domain))]
      (case kind
        nil "domain values should be all numbers, all strings / keywords, all intervals or all java.time values"
        :bands (when-not (apply distinct? domain) "bands domain values should be distinct")
        :datetime (when-not (and (every? temporal? domain) (not= (first domain) (last domain)))
                    "datetime domain should be two different java.time values")
        :interpolated (when-not (and (>= (count domain) 2) (every? schema/finite? domain)
                                     (apply < (map double domain)))
                        "interpolated domain should be two or more strictly increasing finite numbers")
        (cond
          (not-every? schema/finite? domain) "domain should contain finite numbers"
          (== (double (reduce min domain)) (double (reduce max domain))) "domain should not be a single value"
          (and (= kind :log) (not (every? #(pos? (double %)) domain))) "log domain should be positive")))))

(def ^:private wadogo-attributes
  "wadogo scale attributes passed through to the scale (see wadogo's kinds)."
  [:clamp :unknown :round :base :exponent :padding-in :padding-out :align-outer :align :interpolator-params])

(def ^:private ticks-params
  [:map {:closed true}
   (schema/optional :min-n [:and :int [:>= 0]])
   (schema/optional :unit (into [:enum] (sort pretty-datetime/calendar-units)))
   (schema/optional :step schema/pos-int)])

(def scale-spec-schema
  "Plot scale spec of `:x` / `:y`: wadogo attributes plus `:nice` and `:expand`, see [[scale-spec]] and
  [[data-scale-spec]]; `:axis false` hides the scale's axis and its grid lines in every panel (default true; axis
  titles are separate, `:labels`)."
  (schema/with-rules
   (into [:map {:closed true}
          (schema/optional :kind (into [:enum] (sort position-kinds)))
          (schema/optional :type [:enum :categorical :numeric])
          (schema/nilable :domain [:sequential {:min 1} any?])
          (schema/nilable :ticks [:multi {:dispatch sequential?} [true [:sequential any?]] [false schema/pos-int]])
          (schema/nilable :nice [:multi {:dispatch boolean?} [true :boolean] [false [:and schema/finite schema/non-neg]]])
          (schema/optional :expand schema/padding)
          (schema/optional :formatter [:multi {:dispatch string?} [true :string] [false schema/function]])
          (schema/optional :axis schema/bool)
          (schema/optional :stops [:sequential {:min 2} schema/finite])
          (schema/optional :interpolator schema/interpolator)
          (schema/optional :ticks-method [:enum :default :pretty])
          (schema/optional :ticks-params ticks-params)
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

(defn- kind-option-problem
  "`[key message]` of the first option of `spec` its resolved `kind` doesn't take, or `nil`: `:stops`,
  `:interpolator(-params)` and `:expand` (interpolated takes no padding); tick options on bands; calendar ticks off
  `:datetime` `:pretty`; a string `:formatter` off `:datetime`; `:stops` not matching the domain."
  [kind {:keys [stops domain ticks-method ticks-params formatter] :as spec}]
  (let [interpolated? (= :interpolated kind)]
    (cond
      (and (some? stops) (not interpolated?)) [:stops ":stops need :kind :interpolated"]
      (and (some? (:interpolator spec)) (not interpolated?)) [:interpolator ":interpolator needs :kind :interpolated"]
      (and (contains? spec :interpolator-params) (not interpolated?)) [:interpolator-params ":interpolator-params needs :kind :interpolated"]
      (and interpolated? (contains? spec :expand)) [:expand "an :interpolated scale takes no :expand (its domain ends are the panel edges)"]
      (and (some? stops) (not= (count stops) (count domain))) [:stops ":stops need one fraction per :domain value"]
      (and (some? stops) (not (and (every? #(<= 0.0 (double %) 1.0) stops)
                                   (or (apply < (map double stops)) (apply > (map double stops))))))
      [:stops ":stops should be strictly increasing (or decreasing) fractions in [0 1]"]
      (and (= :bands kind) (some? ticks-method)) [:ticks-method "a :bands scale has no tick methods"]
      (and (= :bands kind) (some? ticks-params)) [:ticks-params "a :bands scale has no tick methods"]
      (and (or (:unit ticks-params) (:step ticks-params)) (not (and (= :datetime kind) (not= :default ticks-method))))
      [:ticks-params "calendar ticks (:unit / :step) need a :datetime scale with :ticks-method :pretty"]
      (and (:step ticks-params) (nil? (:unit ticks-params))) [:ticks-params "calendar ticks :step needs a :unit"]
      (and (string? formatter) (not= :datetime kind)) [:formatter "a pattern string :formatter needs a :datetime scale"])))

(defn- check-calendar-span!
  "Throw `ex-info {:key :ticks-params}` when the calendar ticks of `ticks-params` over the datetime `domain` would
  exceed wadogo's `max-calendar-ticks` (the domain read as wadogo stores it, `LocalDateTime`s)."
  [k domain {:keys [unit step] :or {step 1}}]
  (let [[lo hi] (s/domain (s/scale :datetime {:domain domain}))]
    (try (pretty-datetime/validate-calendar-span lo hi unit step)
         (catch clojure.lang.ExceptionInfo e
           (schema/fail! (ex-message e) {:key :ticks-params :in [:scales k :ticks-params] :value {:unit unit :step step}})))))

(defn- pattern-formatter
  "Formatter function of the `java.time` pattern `pattern`; an invalid pattern, or one a `LocalDateTime` can't
  format (zones), throws `ex-info {:key :formatter}`."
  [k pattern]
  (try (let [f (DateTimeFormatter/ofPattern pattern)]
         (.format f (LocalDateTime/of 2000 1 1 0 0))
         (fn [v] (.format f ^java.time.temporal.TemporalAccessor v)))
       (catch Exception e
         (schema/fail! (str "Invalid date pattern: " (ex-message e) ".") {:key :formatter :in [:scales k :formatter] :value pattern}))))

(defn scale-spec
  "Phase-1 spec of the plot scale `k` from its attributes `spec` (a map of wadogo attributes and `:nice`, valid by
  [[scale-spec-schema]]): `:kind` inferred from the domain when missing; no `:domain`, or a data domain not suiting the
  kind (e.g. `:log` with 0) throws `ex-info {:key :domain}`; options the kind doesn't take throw `ex-info` keyed
  by the option.
  `:nice` (default `true`, `false` for `:interpolated`): extend continuous domains to nice values by at most 10% of
  the range; a number sets that limit (fraction); falsy turns it off. `:datetime` gets `:ticks-method :pretty` unless
  given; a pattern string `:formatter` becomes a function.
  A tick list is checked against the kind and domain (`ex-info {:key :ticks}`)."
  [k spec]
  (let [{:keys [domain ticks]} spec
        _ (when (nil? domain)
            (schema/fail! "Scale :domain is required when there is no data." {:key :domain :in [:scales k :domain]}))
        kind (or (:kind spec) (domain-kind domain))]
    (when-let [problem (domain-problem spec)] ;; a data domain may not suit the spec's kind (e.g. log)
      (schema/fail! (str "Scale " problem ".") {:key :domain :in [:scales k :domain] :value domain}))
    (when-let [[key message] (kind-option-problem kind spec)]
      (schema/fail! (str message ".") {:key key :in [:scales k key] :value (get spec key)}))
    (when (sequential? ticks) (validate-tick-list! k kind domain ticks))
    (when (and (= :datetime kind) (get-in spec [:ticks-params :unit]))
      (check-calendar-span! k domain (:ticks-params spec)))
    (cond-> (assoc spec :kind kind :nice (get spec :nice (not= :interpolated kind)))
      (and (= :datetime kind) (nil? (:ticks-method spec))) (assoc :ticks-method :pretty)
      (string? (:formatter spec)) (update :formatter #(pattern-formatter k %)))))

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

(defn- pad-domain-pixels
  "[[pad-pixels]] of the ends of `domain`; a `:datetime` domain in epoch milliseconds (see
  [[cljplot2.data.rows/x-key]]), its ends back in their types (a `LocalDate` end becomes a `LocalDateTime`)."
  [kind domain padding-px length]
  (let [a (first domain) b (last domain)]
    (if (= :datetime kind)
      (let [[ka kb] (pad-pixels :linear [(rows/x-key a) (rows/x-key b)] padding-px length)]
        [(rows/key->x ka a) (rows/key->x kb b)])
      (pad-pixels kind [(double a) (double b)] padding-px length))))

(def ^:private extent-kinds {:numeric :linear :categorical :bands :temporal :datetime})

(defn data-scale-spec
  "Phase-1 spec of plot scale `k` from its `spec` (may be `{}`) and the merged data `extent` (may be `nil`)
  with the charts' `padding` `[lo hi]`. An explicit `:domain` wins over data (`nil` - not set). A data domain gets
  `:kind` from the spec or the extent (numeric -> `:linear`, categorical -> `:bands`, temporal -> `:datetime`); a
  numeric extent on a `:bands` scale needs an explicit `:domain` unless it carries its levels (`:values`, sorted);
  numeric data domains are widened when collapsed to one value.
  Numeric continuous domains - data or explicit - get `:padding`: the spec `:expand`, else the charts' `padding`
  (only when there is data; `:radial` - none below the data); [[finalize]] applies it after `nice`, so the
  outermost ticks sit inside the panel. Temporal, `:interpolated` and categorical domains aren't padded."
  [k spec extent padding]
  (when (and (= :interpolated (:kind spec)) (nil? (:domain spec)))
    (schema/fail! "An :interpolated scale needs an explicit :domain (its stop values)." {:key :domain :in [:scales k :domain]}))
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
                                          :else (schema/fail! "Numeric data declared categorical (or on a bands scale) needs an explicit :domain unless its charts give the levels (strip charts do)."
                                                              {:key :domain :in [:scales k :domain]})))]
                  (scale-spec k (assoc spec :kind kind :domain domain))))
        padding (or (:expand spec)
                    (when extent (if (= :radial (:kind built)) (assoc (vec padding) 0 0.0) padding)))]
    (cond-> built
      (and padding (not (#{:datetime :interpolated} (:kind built))) (continuous-kinds (:kind built)))
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
  "wadogo scale of a finalized scale (a `:radial` one is built over `[0, end - start]`: pixels come from
  [[forward-fn]], which adds the range start)."
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

(defn- pixel-offset
  "Pixels added to the wadogo position of a finalized scale of `kind` over `range`: a `:radial` scale is built over
  `[0, end - start]` (wadogo squares the range ends, so its distances grow as the square root of the value only from
  0) and shifted by the range start; 0 for other kinds."
  ^double [kind range]
  (if (= :radial kind) (double (first range)) 0.0))

(defn- wadogo-range
  "Range given to wadogo for a scale of `kind` over the pixel `range` `[start end]` (see [[pixel-offset]])."
  [kind [start end]]
  (if (= :radial kind) [0.0 (- (double end) (double start))] [start end]))

(defn- stop-pixels
  "Pixel positions of the `n` stops of an `:interpolated` scale over `range` `[start end]`: the fractions `stops`
  (default evenly spaced) of the way from start to end."
  [[start end] ^long n stops]
  (let [start (double start) end (double end)]
    (mapv (fn [f] (+ start (* (double f) (- end start))))
          (or stops (map #(/ (double %) (double (dec n))) (range n))))))

(defn finalize
  "Finalized scale of phase-1 `spec` for the pixel `range` (`[start end]`) and `tick-hint` (the count used unless
  the spec has `:ticks`). Steps: the domain is extended by `nice` (limited, see `:nice` in [[scale-spec]]) with
  the explicit tick count or the hint, then padded by `:padding` (see [[data-scale-spec]]), then by the pixel room
  `:padding-px` `[lo hi]` (charts' labels past the data; `:linear`, `:log` and `:datetime` - in epoch milliseconds,
  skipped when over half the range);
  ticks outside the final domain are dropped. A bands scale without `:ticks` has a tick at every band. A spec with
  `:axis false` gives a finalized scale with `:axis false` (no axis, no grid lines)."
  [spec range tick-hint]
  (let [kind (:kind spec)
        explicit (:ticks spec)
        nice-count (if (integer? explicit) explicit tick-hint)
        base (cond-> (assoc (dissoc spec :kind :nice :expand :padding :padding-px :tick-format :axis :stops) :range (wadogo-range kind range))
               (get-in spec [:ticks-params :min-n]) (update-in [:ticks-params :min-n] #(min (long %) (long nice-count)))
               (= :interpolated kind) (assoc :range (stop-pixels range (count (:domain spec)) (:stops spec))))
        niced (nice-domain (s/scale kind (assoc base :ticks nice-count)) kind (:nice spec))
        domain (if-let [padding (:padding spec)]
                 (pad kind [(double (first niced)) (double (last niced))] padding)
                 niced)
        domain (if-let [padding-px (when (#{:linear :log :datetime} kind) (:padding-px spec))]
                 (pad-domain-pixels kind domain padding-px (m/abs (- (double (second range)) (double (first range)))))
                 domain)
        attributes (assoc base :domain domain :ticks (or explicit (if (= kind :bands) (vec domain) tick-hint)))
        scale (s/scale kind attributes)
        offset (pixel-offset kind range)
        ticks (if (continuous-kinds kind)
                (filterv #(inside-range? range (+ offset (double (scale %)))) (s/ticks scale))
                (vec (s/ticks scale)))]
    (cond-> {:kind kind
             :domain domain
             :range range
             :ticks ticks
             :tick-labels (tick-labels scale spec ticks)
             :attributes attributes}
      (false? (:axis spec)) (assoc :axis false))))

(defn forward-fn
  "Function mapping a value to its pixel position in the finalized scale (band center for `:bands`); `##NaN` for a
  value the scale can't place (e.g. not in a bands domain) - callers skip such values.
  Builds the wadogo scale once; reuse the function for many values."
  [finalized]
  (let [scale (->wadogo finalized)
        offset (pixel-offset (:kind finalized) (:range finalized))]
    (fn ^double [value] (let [p (scale value)] (if (nil? p) ##NaN (+ offset (double p)))))))

(defn inverse-fn
  "Function mapping a pixel position to its value in the finalized continuous scale (wadogo `inverse`: a number, a
  `LocalDateTime` on a `:datetime` scale, rounded to the millisecond). A `:bands` scale has no inverse - `ex-info
  {:key :kind}`. Builds the wadogo scale once; reuse the function for many positions."
  [finalized]
  (when-not (continuous-kinds (:kind finalized))
    (schema/fail! (str "A " (:kind finalized) " scale has no inverse; a continuous scale is needed.")
                  {:key :kind :value (:kind finalized)}))
  (let [scale (->wadogo finalized)
        offset (pixel-offset (:kind finalized) (:range finalized))]
    (fn [^double pixel] (s/inverse scale (- pixel offset)))))

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
