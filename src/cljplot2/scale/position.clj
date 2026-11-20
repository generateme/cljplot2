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
  value, on `:datetime` a `java.time` pattern string (`\"MMM yyyy\"`), or on `:log` the preset `:power` (`2⁵`,
  [[power-label]]). `:minor-ticks n` - unlabelled ticks between the major ones (continuous numeric kinds, see
  [[minor-tick-values]]; finalized under `:minor-ticks`). `:axis-domain [lo hi]` (continuous numeric kinds) - display
  only: axes and grid lines show the ticks of that linear domain over the panel instead (see [[display-scale]]), e.g.
  a variable's values beside a density drawn in its own magnitude scale. Bands scales: `:order-by f` - levels ordered by `(f rows)`
  over each level's rows (`:descending true` reverses; per panel under a `:free` facet scale), not with `:domain`
  (consumed by the pipeline). `:reverse true` - the axis runs the other way: the final domain (data or explicit, after
  nice and padding) reversed - bands in reverse level order (not `:interpolated`: reverse its `:stops`)."
  (:require [fastmath.core :as m]
            [wadogo.scale :as s]
            [wadogo.ticks.pretty-datetime :as pretty-datetime]
            [cljplot2.data.rows :as rows]
            [cljplot2.scale.coord :as coord]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec2]
           [java.time LocalDateTime LocalTime]
           [java.time.temporal ChronoUnit Temporal]
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

(defn panel-scales-of
  "Finalized scales of grid content `item` in a content `env`: the entry of its panel in `env` `:panel-scales`
  (panel-local scales of free / sliced facet scales and weighted panels, see [[cljplot2.pipeline/panel-scales]]) when
  `item` has a `:panel` key - `nil` is a panel key too (the NA facet level); else, and for a panel without an entry,
  the shared `:scales`."
  [env item]
  (if-let [[_ scales] (and (contains? item :panel) (find (:panel-scales env) (:panel item)))]
    scales
    (:scales env)))

(def ^:private wadogo-attributes
  "wadogo scale attributes passed through to the scale (see wadogo's kinds, `wadogo.scale/kind-info`):
  `[key description]` pairs, in table order."
  [[:clamp "wadogo: clamp values outside the domain to its ends"]
   [:unknown "wadogo: the value returned for values outside a bands domain"]
   [:round "wadogo: round band starts and widths to whole pixels"]
   [:base "wadogo: logarithm base of a :log scale (ticks and nice only)"]
   [:exponent "wadogo: exponent of a :pow scale"]
   [:padding-in "wadogo: space between bands, a fraction of the step"]
   [:padding-out "wadogo: space before the first and after the last band, a fraction of the step"]
   [:align-outer "wadogo: how the outer band space is split, 0 - all after the last band, 1 - all before the first"]
   [:align "wadogo: position of a point inside its band, 0 - band start, 1 - band end"]
   [:interpolator-params "wadogo: parameters of the :interpolated scale's :interpolator"]])

(def ^:private ticks-params
  [:map {:closed true}
   (schema/optional :min-n {:description "the fewest ticks (clamped to the tick count)"} [:and :int [:>= 0]])
   (schema/optional :unit {:description "calendar tick unit (:datetime with :pretty ticks)"} (into [:enum] (sort pretty-datetime/calendar-units)))
   (schema/optional :step {:description "a calendar tick every :step units" :default 1} schema/pos-int)])

(def scale-spec-schema
  "Plot scale spec of `:x` / `:y`: wadogo attributes plus `:nice` and `:expand`, see [[scale-spec]] and
  [[data-scale-spec]]; `:axis false` hides the scale's axis and its grid lines in every panel, `:axis :grid` hides
  the axis (no track, ticks, tick labels) but keeps the grid lines, `:axis :no-grid` keeps the axis without grid lines
  (e.g. a tick per category of many; default true; axis titles are separate, `:labels`)."
  (schema/with-rules
   (into [:map {:closed true :cljplot2/type-doc "a position scale spec"}
          (schema/optional :kind {:cljplot2/section :kind-domain :description "scale kind: :linear, :log, :pow, :symlog, :radial, :interpolated, :datetime or :bands"
                                  :cljplot2/default-doc "from the data: :linear for numbers, :bands for levels, :datetime for java.time values"}
                           (into [:enum] (sort position-kinds)))
          (schema/optional :type {:cljplot2/section :kind-domain :description "declare numbers or dates categorical (one band per value) or force a numeric scale"}
                           [:enum :categorical :numeric])
          (schema/nilable :domain {:cljplot2/section :kind-domain :description "explicit domain: [lo hi] for continuous kinds, the levels in order for :bands"
                                   :cljplot2/default-doc "the charts' data extent"}
                          [:sequential {:min 1} any?])
          (schema/nilable :ticks {:cljplot2/section :ticks :description "a tick count hint, or the tick values themselves"
                                  :cljplot2/default-doc "a count from the panel size and theme :tick-spacing"}
                          [:multi {:dispatch sequential?} [true [:sequential any?]] [false schema/pos-int]])
          (schema/nilable :nice {:cljplot2/section :kind-domain :description "extend a continuous domain to nice tick values (a number - the most it may grow, a fraction of the span)"
                                 :cljplot2/default-doc "true (false for :interpolated)"}
                          [:multi {:dispatch boolean?} [true :boolean] [false [:and schema/finite schema/non-neg]]])
          (schema/optional :expand {:cljplot2/section :kind-domain :description "padding [lo hi] of a continuous domain, fractions of the span (wins over the charts')"
                                    :cljplot2/default-doc "the charts' padding"}
                           schema/padding)
          (schema/optional :formatter {:cljplot2/section :ticks :description "tick label text: a function of the value, a java.time pattern (:datetime) or :power (:log)"
                                       :cljplot2/default-doc "the charts' suggested format, else plain numbers, levels or dates"}
                           [:multi {:dispatch #(cond (string? %) :pattern (keyword? %) :preset :else :function)}
                            [:pattern :string] [:preset [:enum :power]] [:function schema/function]])
          (schema/nilable :order-by {:cljplot2/section :order :description "bands ordered by a function of each level's rows (not with :domain)"} schema/function)
          (schema/nilable :descending {:cljplot2/section :order :description "reverse the :order-by order (needs :order-by)" :default false} schema/bool)
          (schema/nilable :reverse {:cljplot2/section :order :description "the axis runs the other way (the final domain reversed)" :default false} schema/bool)
          (schema/nilable :minor-ticks {:cljplot2/section :ticks :description "unlabelled ticks between two major ticks (continuous numeric kinds)"} schema/pos-int)
          (schema/nilable :axis-domain {:cljplot2/section :axis :description "display only: axis and grid show the ticks of this linear domain over the panel"}
                          [:tuple schema/finite schema/finite])
          (schema/optional :overlapping-labels {:cljplot2/section :axis :description "tick labels overlapping a drawn one: :drop (not drawn) or :keep (all drawn - axes naming data items)"
                                                :cljplot2/default-doc ":keep with an explicit :ticks vector (the values asked for), else :drop"}
                           [:enum :drop :keep])
          (schema/optional :axis {:cljplot2/section :axis :description "true - axis and grid, false - neither, :grid - grid lines only, :no-grid - axis only"
                                  :default true}
                           [:or :boolean [:enum :grid :no-grid]])
          (schema/optional :stops {:cljplot2/section :interpolated :description "panel fractions the :interpolated domain values map to (increasing, or decreasing - reversed)"
                                   :cljplot2/default-doc "evenly spaced"}
                           [:sequential {:min 2} schema/finite])
          (schema/optional :interpolator {:cljplot2/section :interpolated :description "interpolation between the :stops of an :interpolated scale (only there)"
                                          :cljplot2/default-doc ":linear"}
                           schema/interpolator)
          (schema/optional :ticks-method {:cljplot2/section :ticks :description "tick algorithm of continuous kinds: :default (d3) or :pretty (R pretty())"
                                          :cljplot2/default-doc ":pretty for :datetime, else :default"}
                           [:enum :default :pretty])
          (schema/optional :ticks-params {:cljplot2/section :ticks :description "tick algorithm parameters: {:min-n n}, calendar ticks {:unit u :step s}"}
                           ticks-params)
          (schema/optional :range (schema/forbidden "is set from the layout, it can't be given"))
          (schema/optional :padding (schema/forbidden "is internal; use :expand"))
          (schema/optional :tick-format (schema/forbidden "is internal (set by charts); use :formatter"))]
         (map (fn [[k description]] (schema/optional k {:description description :cljplot2/section :wadogo} any?)))
         wadogo-attributes)
   [domain-problem [:domain]]
   [#(when (and (:order-by %) (:domain %)) "give either :order-by or :domain, not both") [:order-by]]
   [#(when (and (:descending %) (nil? (:order-by %))) ":descending needs :order-by") [:descending]]
   [#(when-let [[lo hi] (:axis-domain %)] (when (== (double lo) (double hi)) ":axis-domain should be two different numbers"))
    [:axis-domain]]))

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
  `:datetime` `:pretty`; a string `:formatter` off `:datetime`; `:formatter :power` off `:log`; `:order-by` off `:bands`;
  `:minor-ticks` on `:bands` or `:datetime`; `:stops` not matching the domain; `:reverse` on `:interpolated`."
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
      (and (string? formatter) (not= :datetime kind)) [:formatter "a pattern string :formatter needs a :datetime scale"]
      (and (= :power formatter) (not= :log kind)) [:formatter ":formatter :power needs a :log scale"]
      (and (some? (:order-by spec)) (not= :bands kind)) [:order-by ":order-by needs a categorical (:bands) scale"]
      (and (some? (:minor-ticks spec)) (#{:bands :datetime} kind)) [:minor-ticks ":minor-ticks need a numeric continuous scale (not :bands or :datetime)"]
      (and (some? (:axis-domain spec)) (#{:bands :datetime} kind)) [:axis-domain ":axis-domain needs a numeric continuous scale (not :bands or :datetime)"]
      (and (:reverse spec) interpolated?) [:reverse "an :interpolated scale reverses by its :stops (decreasing fractions)"])))

(def ^:private superscript-digits
  {\0 \u2070 \1 \u00b9 \2 \u00b2 \3 \u00b3 \4 \u2074 \5 \u2075 \6 \u2076 \7 \u2077 \8 \u2078 \9 \u2079 \- \u207b})

(defn- number-text
  "`v` as text: a whole number without decimals, else Clojure's double text."
  [v]
  (let [d (double v)]
    (if (and (== d (m/floor d)) (< (m/abs d) max-integral-label)) (str (long d)) (str d))))

(defn power-label
  "Label of tick `v` on a log scale with `base`: `base` followed by the exponent in Unicode superscript digits
  (`2⁵`, `10⁻³`) when `v` is a whole power of `base` (within 1e-9 relative), else `v` as a number."
  [base v]
  (let [b (double base) x (double v)
        k (when (pos? x) (m/round (/ (m/ln x) (m/ln b))))]
    (if (and k (< (m/abs (- (m/pow b (double k)) x)) (* 1.0e-9 x)))
      (str (number-text b) (apply str (map superscript-digits (str k))))
      (number-text v))))

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
      (string? (:formatter spec)) (update :formatter #(pattern-formatter k %))
      (= :power (:formatter spec)) (assoc :formatter (partial power-label (get spec :base 10.0))))))

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

(defn- pad-domain
  "[[pad]] of the ends of `domain`; a `:datetime` domain in epoch milliseconds (see [[cljplot2.data.rows/x-key]]), its
  ends back in their types (a `LocalDate` end becomes a `LocalDateTime`; niced domains are `LocalDateTime` already,
  `LocalTime` data included)."
  [kind domain padding]
  (let [a (first domain) b (last domain)]
    (if (= :datetime kind)
      (let [[ka kb] (pad :linear [(rows/x-key a) (rows/x-key b)] padding)]
        [(rows/key->x ka a) (rows/key->x kb b)])
      (pad kind [(double a) (double b)] padding))))

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
  [[cljplot2.data.rows/x-key]]), its ends back in their types (a `LocalDate` end becomes a `LocalDateTime`; a
  `LocalTime` end - only with `:nice false`, niced domains are `LocalDateTime` - stays within the day)."
  [kind domain padding-px length]
  (let [a (first domain) b (last domain)]
    (if (= :datetime kind)
      (let [[ka kb] (pad-pixels :linear [(rows/x-key a) (rows/x-key b)] padding-px length)]
        [(rows/key->x ka a) (rows/key->x kb b)])
      (pad-pixels kind [(double a) (double b)] padding-px length))))

(def ^:private extent-kinds {:numeric :linear :categorical :bands :temporal :datetime})

(defn- widen-temporal
  "`[lo hi]` of `java.time` values; a single value `t` widens to one day either side (a `LocalTime` - one hour, kept
  within the day)."
  [lo hi]
  (cond
    (not (zero? (compare lo hi))) [lo hi]
    (instance? LocalTime lo) (let [s (.toSecondOfDay ^LocalTime lo)]
                               [(LocalTime/ofSecondOfDay (max 0 (- s 3600))) (LocalTime/ofSecondOfDay (min 86399 (+ s 3600)))])
    :else [(.minus ^Temporal lo 1 ChronoUnit/DAYS) (.plus ^Temporal lo 1 ChronoUnit/DAYS)]))

(defn- period-starts
  "Scale `spec` with a `Year` / `YearMonth` in an explicit `:domain` or `:ticks` vector as the date its period starts
  (see [[cljplot2.data.rows/period-start]]), as data values read - except on a bands or categorical scale, where
  they are levels."
  [spec]
  (if (or (= :bands (:kind spec)) (= :categorical (:type spec)))
    spec
    (cond-> spec
      (sequential? (:domain spec)) (update :domain #(mapv rows/period-start %))
      (sequential? (:ticks spec)) (update :ticks #(mapv rows/period-start %)))))

(defn data-scale-spec
  "Phase-1 spec of plot scale `k` from its `spec` (may be `{}`) and the merged data `extent` (may be `nil`)
  with the charts' `padding` `[lo hi]`. An explicit `:domain` wins over data (`nil` - not set). A data domain gets
  `:kind` from the spec or the extent (numeric -> `:linear`, categorical -> `:bands`, temporal -> `:datetime`); a
  numeric extent on a `:bands` scale needs an explicit `:domain` unless it carries its levels (`:values`, sorted);
  numeric and temporal (`:datetime`) data domains are widened when collapsed to one value.
  Numeric continuous domains - data or explicit - get `:padding`: the spec `:expand`, else the charts' `padding`
  (only when there is data; `:radial` - none below the data); [[finalize]] applies it after `nice`, so the
  outermost ticks sit inside the panel; temporal (`:datetime`) domains too, in epoch milliseconds (marks at the
  ends stay inside). `:interpolated` and categorical domains aren't padded."
  [k spec extent padding]
  (when (and (= :interpolated (:kind spec)) (nil? (:domain spec)))
    (schema/fail! "An :interpolated scale needs an explicit :domain (its stop values)." {:key :domain :in [:scales k :domain]}))
  (let [spec (period-starts spec)
        built (if (or (some? (:domain spec)) (nil? extent))
                (scale-spec k spec)
                (let [kind (or (:kind spec) (extent-kinds (:kind extent)))
                      domain (case (:kind extent)
                               :categorical (:values extent)
                               :temporal (cond
                                           ;; a Year / YearMonth extent spans the dates its periods start
                                           (= :datetime kind) (widen-temporal (rows/period-start (:min extent))
                                                                              (rows/period-start (:max extent)))
                                           ;; dates declared categorical: one band per distinct date, in time order
                                           (= :bands kind) (if (:values extent)
                                                             (vec (sort (:values extent)))
                                                             (schema/fail! "Dates declared categorical (or on a bands scale) have too many distinct values; give an explicit :domain."
                                                                           {:key :domain :in [:scales k :domain]}))
                                           :else [(:min extent) (:max extent)])
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
      (and padding (not= :interpolated (:kind built)) (continuous-kinds (:kind built)))
      (assoc :padding (vec padding)))))

(defn tick-hint
  "Tick count hint for an axis `length` pixels long with `spacing` pixels between ticks: the nearest whole count
  (rounded, so panels a few pixels apart around a whole multiple of `spacing` ask for the same count), at least 2."
  ^long [^double length ^double spacing]
  (m/max 2 (Math/round (/ length spacing))))

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

(defn- near-any?
  "Is `v` equal to one of `values` up to a relative 1e-9 (of the larger magnitude; two zeros are equal)?"
  [values v]
  (some #(<= (m/abs (- (double %) (double v))) (* 1.0e-9 (m/max (m/abs (double %)) (m/abs (double v))))) values))

(defn minor-tick-values
  "Minor ticks (data values) inside the numeric `domain` of a scale of `kind` with major `ticks`: on `:log` the
  multiples 2 .. base-1 of every power of `base` (none for a base below 3); on other kinds `n` subdivisions of each
  interval between major ticks (`n - 1` ticks per interval; the first and last intervals repeated past the outer
  ticks). Major ticks are left out; no minor ticks with fewer than two major ticks (other kinds)."
  [kind domain ticks n base]
  (let [lo (double (reduce min domain)) hi (double (reduce max domain))
        inside (fn [vs] (filterv #(and (<= lo (double %) hi) (not (near-any? ticks %))) vs))]
    (if (= :log kind)
      (let [b (double base)
            from (long (m/floor (/ (m/ln lo) (m/ln b))))
            to (long (m/ceil (/ (m/ln hi) (m/ln b))))]
        (inside (for [e (range from (inc to)) j (range 2 (long (m/ceil b)))
                      :let [v (* (double j) (m/pow b (double e)))]
                      :when (< (double j) b)]
                  v)))
      (let [ts (vec (sort (map double ticks)))
            c (count ts)]
        (if (< c 2)
          []
          (let [t0 (double (ts 0)) tn (double (ts (dec c)))
                first-step (- (double (ts 1)) t0)
                last-step (- tn (double (ts (- c 2))))
                ;; [start step] of every interval: outer ones repeated while they still reach into the domain
                before (map (fn [^long k] [(- t0 (* k first-step)) first-step])
                            (take-while (fn [^long k] (> (- t0 (* (dec k) first-step)) lo)) (iterate inc 1)))
                inner (map (fn [a b] [a (- (double b) (double a))]) ts (rest ts))
                after (map (fn [^long k] [(+ tn (* k last-step)) last-step])
                           (take-while (fn [^long k] (< (+ tn (* k last-step)) hi)) (iterate inc 0)))]
            (inside (for [[start step] (concat before inner after) j (range 1 (long n))]
                      (+ (double start) (/ (* (double j) (double step)) (double (long n))))))))))))

(def ^:const max-tick-count
  "Largest tick count [[ticks-inside]] asks for while looking for two ticks inside a domain."
  12)

(defn ticks-inside
  "`{:count n :scale sc :ticks [..]}` - the wadogo scale `(scale-of n)` for tick count `n` and its ticks kept by
  `inside?`; when fewer
  than 2 are inside (a nice step wider than a short domain), counts `n + 1` ... [[max-tick-count]] are tried until 2
  are - an axis shows at least two ticks to read its span; the last count's when none gives two (e.g. a log domain
  within one step of its ticks)."
  [scale-of ^long n inside?]
  (loop [n n]
    (let [scale (scale-of n)
          ticks (filterv inside? (s/ticks scale))]
      (if (or (>= (count ticks) 2) (>= n max-tick-count))
        {:count n :scale scale :ticks ticks}
        (recur (inc n))))))

(defn keep-labels?
  "Does an axis of scale `spec` draw every tick label, overlapping or not: `:overlapping-labels :keep`, or an explicit
  `:ticks` vector (the values asked for) without `:overlapping-labels :drop`?"
  [{:keys [overlapping-labels ticks]}]
  (case overlapping-labels
    :keep true
    :drop false
    (sequential? ticks)))

(defn finalize
  "Finalized scale of phase-1 `spec` for the pixel `range` (`[start end]`) and `tick-hint` (the count used unless
  the spec has `:ticks`). Steps: the domain is extended by `nice` (limited, see `:nice` in [[scale-spec]]) with
  the explicit tick count or the hint, then padded by `:padding` (see [[data-scale-spec]]), then by the pixel room
  `:padding-px` `[lo hi]` (charts' labels past the data; `:linear`, `:log` and `:datetime` - in epoch milliseconds,
  skipped when over half the range);
  `:reverse` reverses the final domain; ticks outside the final domain are dropped - a continuous scale without `:ticks` gets more ticks until two are
  inside (see [[ticks-inside]]; `:attributes` `:ticks` - the count used). A bands scale without `:ticks` has a tick
  at every band. A spec with
  `:axis false` gives a finalized scale with `:axis false` (no axis, no grid lines), `:axis :grid` one with
  `:axis :grid` (no axis, grid lines kept), `:axis :no-grid` one with `:axis :no-grid` (the axis, no grid lines);
  one keeping its labels (see [[keep-labels?]]) carries `:overlapping-labels :keep` (the axis draws every tick label)."
  [spec range tick-hint]
  (let [kind (:kind spec)
        explicit (:ticks spec)
        nice-count (if (integer? explicit) explicit tick-hint)
        base (cond-> (assoc (dissoc spec :kind :nice :expand :padding :padding-px :tick-format :axis :stops :minor-ticks
                                    :axis-domain :reverse :overlapping-labels)
                            :range (wadogo-range kind range))
               (get-in spec [:ticks-params :min-n]) (update-in [:ticks-params :min-n] #(min (long %) (long nice-count)))
               (= :interpolated kind) (assoc :range (stop-pixels range (count (:domain spec)) (:stops spec))))
        niced (nice-domain (s/scale kind (assoc base :ticks nice-count)) kind (:nice spec))
        domain (if-let [padding (:padding spec)]
                 (pad-domain kind niced padding)
                 niced)
        domain (if-let [padding-px (when (#{:linear :log :datetime} kind) (:padding-px spec))]
                 (pad-domain-pixels kind domain padding-px (m/abs (- (double (second range)) (double (first range)))))
                 domain)
        domain (cond-> domain (:reverse spec) (-> reverse vec))
        attributes (assoc base :domain domain :ticks (or explicit (if (= kind :bands) (vec domain) tick-hint)))
        offset (pixel-offset kind range)
        position (s/scale kind attributes)
        inside? #(inside-range? range (+ offset (double (position %))))
        {scale :scale ticks :ticks tick-count :count}
        (cond
          (not (continuous-kinds kind)) {:scale position :ticks (vec (s/ticks position))}
          explicit {:scale position :ticks (filterv inside? (s/ticks position))}
          :else (ticks-inside #(s/scale kind (assoc attributes :ticks %)) tick-hint inside?))
        attributes (cond-> attributes tick-count (assoc :ticks tick-count))]
    (cond-> {:kind kind
             :domain domain
             :range range
             :ticks ticks
             :tick-labels (tick-labels scale spec ticks)
             :attributes attributes}
      (contains? #{false :grid :no-grid} (:axis spec)) (assoc :axis (:axis spec))
      (keep-labels? spec) (assoc :overlapping-labels :keep)
      (:minor-ticks spec) (assoc :minor-ticks (filterv inside? (minor-tick-values kind domain ticks (:minor-ticks spec)
                                                                                   (get attributes :base 10.0))))
      (:axis-domain spec) (assoc :axis-scale (finalize (merge (select-keys spec [:formatter :minor-ticks :ticks-method :ticks-params])
                                                              {:kind :linear :domain (:axis-domain spec) :nice false})
                                                       range tick-hint)))))

(defn display-scale
  "The scale an axis and the panel grid show for finalized scale `finalized`: its `:axis-scale` (from an
  `:axis-domain` - display only: the ticks and labels of that linear domain over the same pixel range, the charts
  still draw with `finalized`) carrying the scale's `:direction`, `:axis` and `:overlapping-labels`, else `finalized`
  itself."
  [finalized]
  (if-let [shown (:axis-scale finalized)]
    (merge shown (select-keys finalized [:direction :axis :overlapping-labels]))
    finalized))

(def ^:private identity-transform
  {:forward (fn ^double [^double v] v) :inverse (fn ^double [^double v] v)})

(def ^:private log10-transform
  {:forward (fn ^double [^double v] (if (pos? v) (m/log10 v) ##NaN))
   :inverse (fn ^double [^double v] (m/pow 10.0 v))})

(defn axis-drawn?
  "Is the axis (track, ticks, tick labels) of finalized scale or spec `scale` drawn: `:axis` neither `false` nor
  `:grid`."
  [scale]
  (not (contains? #{false :grid} (:axis scale))))

(defn grid-drawn?
  "Are the grid lines (and band fills) of finalized scale or spec `scale` drawn: `:axis` neither `false` nor
  `:no-grid`."
  [scale]
  (not (contains? #{false :no-grid} (:axis scale))))

(defn stat-transform
  "`{:forward f :inverse g}` - the coordinates a stat (field, contour, bin2d, smooth) works in on an axis with the plot's
  scale spec `spec` (may be `nil`): `log10` for `:log` (a non-positive value maps to `##NaN`), the identity
  otherwise. Densities and bins are then even on screen. Other non-linear kinds are not transformed yet."
  [spec]
  (if (= :log (:kind spec)) log10-transform identity-transform))

(defn forward-fn
  "Function mapping a value to its pixel position in the finalized scale (band center for `:bands`; a `Year` /
  `YearMonth` on a `:datetime` scale at its period's start); `##NaN` for a value the scale can't place (e.g. not in a
  bands domain) - callers skip such values.
  Builds the wadogo scale once; reuse the function for many values."
  [finalized]
  (let [scale (->wadogo finalized)
        offset (pixel-offset (:kind finalized) (:range finalized))
        ;; a datetime scale places a Year / YearMonth at its period's start
        value-of (if (= :datetime (:kind finalized)) rows/period-start identity)]
    (fn ^double [value] (let [p (scale (value-of value))] (if (nil? p) ##NaN (+ offset (double p)))))))

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

(defn coord-geometry
  "Polar geometry of the finalized position `scales` (`:coord` of either, see [[cljplot2.scale.coord/geometry]]);
  `nil` - cartesian (range positions are pixels)."
  [scales]
  (or (:coord (:x scales)) (:coord (:y scales))))

(defn range-projection
  "Function of data `x`, `y` to its range point `[h v]` in the finalized position `scales`: the horizontal scale's
  range position, then the vertical one's (pixels in cartesian plots - the same as [[projection]]; angle and radius
  in polar ones, see [[cljplot2.scale.coord]]); `##NaN` where a scale can't place a value."
  [scales]
  (let [fx (forward-or-nan (:x scales))
        fy (forward-or-nan (:y scales))]
    (if (flipped? scales)
      (fn [x y] [(double (fy y)) (double (fx x))])
      (fn [x y] [(double (fx x)) (double (fy y))]))))

(defn projection
  "Function of data `x`, `y` to the pixel point (`Vec2`) of the finalized position `scales`: the horizontal scale's
  position gives the point's x, the vertical scale's its y (`:x` horizontal unless [[flipped?]]); in a polar plot
  they are the angle and radius, turned into the pixel by [[cljplot2.scale.coord/to-pixel]]. A value the scale
  can't place, or a missing scale, gives `##NaN` in that coordinate. Charts place marks only through it.
  Builds the wadogo scales once; reuse the function for many points."
  [scales]
  (let [fx (forward-or-nan (:x scales))
        fy (forward-or-nan (:y scales))
        geometry (coord-geometry scales)]
    (cond
      geometry (if (flipped? scales)
                 (fn ^Vec2 [x y] (coord/to-pixel geometry (double (fy y)) (double (fx x))))
                 (fn ^Vec2 [x y] (coord/to-pixel geometry (double (fx x)) (double (fy y)))))
      (flipped? scales) (fn ^Vec2 [x y] (Vec2. (double (fy y)) (double (fx x))))
      :else (fn ^Vec2 [x y] (Vec2. (double (fx x)) (double (fy y)))))))
