(ns cljplot2.scale.position
  "Position scales (`:x`, `:y`) in two phases: plain-data specs, then scales finalized for the laid-out panels.

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
  value, on `:datetime` a `java.time` pattern string (e.g. `MMM yyyy`), or on `:log` the presets `:power` (`2⁵`,
  [[power-label]]) and `:power-math` (LaTeX `2^{5}`, [[power-math-label]]). `:minor-ticks n` - unlabelled ticks between the major ones (continuous numeric kinds, see
  [[minor-tick-values]]; finalized under `:minor-ticks`). `:axis-domain [lo hi]` (continuous numeric kinds) - display
  only: axes and grid lines show the ticks of that linear domain over the panel instead (see [[display-scale]]), e.g.
  a variable's values beside a density drawn in its own magnitude scale. Bands scales: `:order-by f` - levels ordered by `(f rows)`
  over each level's rows (`:descending true` reverses; per panel under a `:free` facet scale), not with `:domain`
  (consumed by the pipeline). A partial `:domain` `[lo nil]` / `[nil hi]` (continuous kinds, not `:interpolated`)
  fixes one end, the other comes from the data as without a domain (see [[data-scale-spec]]); `[nil nil]` - no
  domain. `:reverse true` - the axis runs the other way: the final domain (data or explicit, after
  nice and padding) reversed - bands in reverse level order (not `:interpolated`: reverse its `:stops`).

  Functions: specs [[scale-spec-schema]], [[scale-spec]], [[data-scale-spec]]; finalizing [[finalize]], [[tick-hint]], [[ticks-inside]], [[minor-tick-values]], [[tick-labels]], [[power-label]], [[power-math-label]], [[display-scale]], [[keep-labels?]], [[axis-drawn?]], [[grid-drawn?]]; mapping [[->wadogo]], [[forward-fn]], [[forward]], [[inverse-fn]], [[band-geometry]], [[stat-transform]], [[stat-transform-for]], [[value-direction]]; panels and directions [[panel-scales-of]], [[horizontal-key]], [[direction]], [[horizontal?]], [[flipped?]], [[coord-geometry]], [[range-projection]], [[projection]].

  Constants: [[position-kinds]], [[continuous-kinds]], [[max-tick-count]]."
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [wadogo.scale :as s]
            [wadogo.ticks.pretty-datetime :as pretty-datetime]
            [cljplot2.data.levels :as levels]
            [cljplot2.data.rows :as rows]
            [cljplot2.data.text :as text]
            [cljplot2.scale.coord :as coord]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec2]
           [java.time LocalDateTime LocalTime]
           [java.time.temporal ChronoUnit Temporal]
           [java.time.format DateTimeFormatter]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def continuous-kinds
  "Continuous position scale kinds (numbers or dates along the axis)."
  #{:linear :log :pow :symlog :radial :interpolated :datetime})
(def ^:private position-kinds
  "Every position scale kind: [[continuous-kinds]] and `:bands`."
  (conj continuous-kinds :bands))

(def ^{:private true :const true :tag 'double} max-integral-label
  "Magnitude below which a whole-number tick is labelled without decimals."
  1.0e15)

(defn- domain-kind
  "Gives the scale kind of an explicit domain.

  Parameters:

  - `domain` (sequence): the domain values.

  Returns `:linear` for numbers, `:bands` for strings / keywords or ordered levels such as intervals (see [[cljplot2.data.rows/ordered-level?]]), `:datetime` for `java.time` values, `nil` for mixed values."
  [domain]
  (cond
    (every? number? domain) :linear
    (every? #(or (string? %) (keyword? %)) domain) :bands
    (every? rows/ordered-level? domain) :bands
    (every? rows/temporal? domain) :datetime))

(defn- partial-domain-problem
  "Explains why a partial domain of a position scale spec is invalid.

  Parameters:

  - `spec` (map): the scale spec - `:domain` with a `nil` end, `:kind` (inferred by [[domain-kind]] from the given end when missing).

  Returns the error message (a wrong shape - [[cljplot2.data.rows/partial-domain-problem]]), or `nil` for `[lo nil]` / `[nil hi]` of a continuous kind other than `:interpolated`: a finite number (positive for `:log`) or a `java.time` value (`:datetime`)."
  [{:keys [domain] :as spec}]
  (let [given (first (remove nil? domain))
        kind (or (:kind spec) (domain-kind [given]))]
    (or (rows/partial-domain-problem domain)
        (cond
          (= :interpolated kind) "interpolated domain should be two or more strictly increasing finite numbers (no nil)"
          (not (continuous-kinds kind)) "partial domain [lo nil] / [nil hi] needs a continuous scale (not :bands)"
          (= :datetime kind) (when-not (rows/temporal? given) "datetime domain end should be a java.time value")
          (not (schema/finite? given)) "domain end should be a finite number"
          (and (= kind :log) (not (m/pos? (double given)))) "log domain should be positive"))))

(defn- domain-problem
  "Explains why the explicit domain of a position scale spec is invalid.

  Parameters:

  - `spec` (map): the scale spec - `:domain`, `:kind` (inferred by [[domain-kind]] when missing).

  Returns the error message, or `nil` for a valid or absent domain (`[nil nil]` - absent): bands - distinct values; datetime - two different `java.time` values; interpolated - two or more strictly increasing finite numbers; other kinds - finite numbers, not a single value, positive for `:log`; a partial domain as [[partial-domain-problem]]."
  [{:keys [domain] :as spec}]
  (cond
    (nil? (rows/given-domain domain)) nil
    (rows/partial-domain? domain) (partial-domain-problem spec)
    :else
    (let [kind (or (:kind spec) (domain-kind domain))]
      (case kind
        nil "domain values should be all numbers, all strings / keywords, all intervals or all java.time values"
        :bands (when-not (apply distinct? domain) "bands domain values should be distinct")
        :datetime (when-not (and (every? rows/temporal? domain) (not= (first domain) (last domain)))
                    "datetime domain should be two different java.time values")
        :interpolated (when-not (and (m/>= (count domain) 2) (every? schema/finite? domain)
                                     (apply < (map double domain)))
                        "interpolated domain should be two or more strictly increasing finite numbers")
        (cond
          (not-every? schema/finite? domain) "domain should contain finite numbers"
          (m/== (double (v/mn domain)) (double (v/mx domain))) "domain should not be a single value"
          (and (= kind :log) (not (every? #(m/pos? (double %)) domain))) "log domain should be positive")))))

(defn panel-scales-of
  "Gives the finalized scales of grid content in a content environment.

  Parameters:

  - `env` (map): the content environment - `:scales` (shared), `:panel-scales` (panel-local scales of free / sliced facet scales and weighted panels, see [[cljplot2.pipeline/panel-scales]]).
  - `item` (map): the grid content, maybe with a `:panel` key (`nil` is a panel key too - the NA facet level).

  Returns the entry of the item's panel in `:panel-scales`; else, and for a panel without an entry, the shared `:scales`."
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
  "Schema of `:ticks-params`: the fewest ticks and calendar ticks."
  [:map {:closed true}
   (schema/optional :min-n {:description "the fewest ticks (clamped to the tick count)"} [:and :int [:>= 0]])
   (schema/optional :unit {:description "calendar tick unit (:datetime with :pretty ticks)"} (into [:enum] (sort pretty-datetime/calendar-units)))
   (schema/optional :step {:description "a calendar tick every :step units" :default 1} schema/pos-int)])

(def scale-spec-schema
  "Plot scale spec of `:x` / `:y`: wadogo attributes plus `:nice` and `:expand`, see [[scale-spec]] and
  [[data-scale-spec]]; `:axis false` hides the scale's axis and its grid lines in every panel, `:axis :grid` hides
  the axis (no track, ticks, tick labels) but keeps the grid lines, `:axis :no-grid` keeps the axis without grid lines
  (e.g. a tick per category of many; default true; axis titles are separate, `:labels`)."
  (apply schema/with-rules
   (into [:map {:closed true :cljplot2/type-doc "a position scale spec"}
          (schema/optional :kind {:cljplot2/section :kind-domain :description "scale kind: :linear, :log, :pow, :symlog, :radial, :interpolated, :datetime or :bands"
                                  :cljplot2/default-doc "from the data: :linear for numbers, :bands for levels, :datetime for java.time values"}
                           (into [:enum] (sort position-kinds)))
          (schema/optional :type {:cljplot2/section :kind-domain :description "declare numbers or dates categorical (one band per value) or force a numeric scale"}
                           [:enum :categorical :numeric])
          (schema/nilable :domain {:cljplot2/section :kind-domain :description "explicit domain: [lo hi] for continuous kinds (nil at an end - the data's, e.g. [0 nil]), the levels in order for :bands"
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
          (schema/optional :formatter {:cljplot2/section :ticks :description "tick label text: a function of the value, a java.time pattern (:datetime), or :power / :power-math (:log; a power in superscript digits / typeset)"
                                       :cljplot2/default-doc "the charts' suggested format, else plain numbers, levels or dates"}
                           [:multi {:dispatch #(cond (string? %) :pattern (keyword? %) :preset :else :function)}
                            [:pattern :string] [:preset [:enum :power :power-math]] [:function schema/function]])
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
   [#(when-let [[^double lo ^double hi] (:axis-domain %)] (when (m/== lo hi) ":axis-domain should be two different numbers"))
    [:axis-domain]]
   levels/order-by-rules))

(defn- validate-tick-list!
  "Checks an explicit `:ticks` vector against a scale's kind and domain.

  Parameters:

  - `k` (keyword): the scale key.
  - `kind` (keyword): the scale kind.
  - `domain` (sequence): the domain.
  - `ticks` (sequence): the tick values.

  Returns `nil`. Throws `ex-info {:key :ticks}` for the first tick that isn't a domain value (bands), a `java.time` value (datetime) or a finite number (other kinds)."
  [k kind domain ticks]
  (let [valid? (case kind
                 :bands (set domain)
                 :datetime rows/temporal?
                 schema/finite?)]
    (when-let [bad (first (remove valid? ticks))]
      (schema/fail! (if (= kind :bands) "Bands :ticks must be domain values." "Scale :ticks contain an invalid value.")
                    {:key :ticks :in [:scales k :ticks] :value bad}))))

(defn- kind-option-problem
  "Gives the first option of a position scale spec its resolved kind doesn't take.

  Parameters:

  - `kind` (keyword): the resolved kind.
  - `spec` (map): the scale spec.

  Returns `[key message]`, or `nil` when the options hold. Checked: `:stops`, `:interpolator(-params)` and `:expand` (an interpolated scale takes no padding) off `:interpolated`; `:stops` not one fraction per domain value or not strictly monotonic in `[0 1]`; tick methods on bands; calendar ticks off `:datetime` `:pretty`; a string `:formatter` off `:datetime`; `:formatter :power` or `:power-math` off `:log`; `:order-by` off `:bands`; `:minor-ticks` and `:axis-domain` on `:bands` or `:datetime`; `:reverse` on `:interpolated`."
  [kind {:keys [stops domain ticks-method formatter] tick-params :ticks-params :as spec}]
  (let [interpolated? (= :interpolated kind)]
    (cond
      (and (some? stops) (not interpolated?)) [:stops ":stops need :kind :interpolated"]
      (and (some? (:interpolator spec)) (not interpolated?)) [:interpolator ":interpolator needs :kind :interpolated"]
      (and (contains? spec :interpolator-params) (not interpolated?)) [:interpolator-params ":interpolator-params needs :kind :interpolated"]
      (and interpolated? (contains? spec :expand)) [:expand "an :interpolated scale takes no :expand (its domain ends are the panel edges)"]
      (and (some? stops) (not= (count stops) (count domain))) [:stops ":stops need one fraction per :domain value"]
      (and (some? stops) (not (and (every? #(m/<= 0.0 (double %) 1.0) stops)
                                   (or (apply < (map double stops)) (apply > (map double stops))))))
      [:stops ":stops should be strictly increasing (or decreasing) fractions in [0 1]"]
      (and (= :bands kind) (some? ticks-method)) [:ticks-method "a :bands scale has no tick methods"]
      (and (= :bands kind) (some? tick-params)) [:ticks-params "a :bands scale has no tick methods"]
      (and (or (:unit tick-params) (:step tick-params)) (not (and (= :datetime kind) (not= :default ticks-method))))
      [:ticks-params "calendar ticks (:unit / :step) need a :datetime scale with :ticks-method :pretty"]
      (and (:step tick-params) (nil? (:unit tick-params))) [:ticks-params "calendar ticks :step needs a :unit"]
      (and (string? formatter) (not= :datetime kind)) [:formatter "a pattern string :formatter needs a :datetime scale"]
      (and (#{:power :power-math} formatter) (not= :log kind)) [:formatter (str ":formatter " formatter " needs a :log scale")]
      (and (some? (:order-by spec)) (not= :bands kind)) [:order-by ":order-by needs a categorical (:bands) scale"]
      (and (some? (:minor-ticks spec)) (#{:bands :datetime} kind)) [:minor-ticks ":minor-ticks need a numeric continuous scale (not :bands or :datetime)"]
      (and (some? (:axis-domain spec)) (#{:bands :datetime} kind)) [:axis-domain ":axis-domain needs a numeric continuous scale (not :bands or :datetime)"]
      (and (:reverse spec) interpolated?) [:reverse "an :interpolated scale reverses by its :stops (decreasing fractions)"])))

(def ^:private superscript-digits
  "Digit and minus characters -> their Unicode superscripts."
  {\0 \u2070 \1 \u00b9 \2 \u00b2 \3 \u00b3 \4 \u2074 \5 \u2075 \6 \u2076 \7 \u2077 \8 \u2078 \9 \u2079 \- \u207b})

(defn- number-text
  "Gives the text of a number.

  Parameters:

  - `v` (number): the value.

  Returns a whole number (below [[max-integral-label]] in magnitude) without decimals, else the double's Clojure text."
  [v]
  (let [d (double v)]
    (if (and (m/== d (m/floor d)) (m/< (m/abs d) max-integral-label)) (str (long d)) (str d))))

(defn- whole-power
  "Gives the exponent of a value that is a whole power of a base.

  Parameters:

  - `b` (double): the base.
  - `x` (double): the value.

  Returns the exponent (a long) when `x` is a whole power of `b` within 1e-9 relative, else `nil`."
  [^double b ^double x]
  (let [k (when (m/pos? x) (m/round (m// (m/ln x) (m/ln b))))]
    (when (and k (m/< (m/abs (m/- (m/pow b (double k)) x)) (m/* 1.0e-9 x))) k)))

(defn power-label
  "Gives the label of a tick on a log scale as a power of the base.

  Parameters:

  - `base` (number): the logarithm base.
  - `v` (number): the tick value.

  Returns `base` followed by the exponent in Unicode superscript digits (`2⁵`, `10⁻³`) when `v` is a whole power of `base` (within 1e-9 relative), else `v` as a number (see [[number-text]]).

  See also [[power-math-label]] (typeset)."
  [base v]
  (if-let [k (whole-power base v)]
    (str (number-text base) (apply str (map superscript-digits (str k))))
    (number-text v)))

(defn power-math-label
  "Gives the label of a tick on a log scale as a typeset power of the base.

  Parameters:

  - `base` (number): the logarithm base.
  - `v` (number): the tick value.

  Returns a formula value (see [[cljplot2.data.text/math]]) of `base^{k}` (`10^{-3}`; base e as `e^{k}`) when `v` is a whole power of `base` (within 1e-9 relative), else `v` as a number text (see [[number-text]]).

  See also [[power-label]] (Unicode superscripts)."
  [base v]
  (if-let [k (whole-power base v)]
    (text/math (str (if (m/== (double base) m/E) "e" (number-text base)) "^{" k "}"))
    (number-text v)))

(defn- check-calendar-span!
  "Checks that calendar ticks over a datetime domain stay within wadogo's `max-calendar-ticks`.

  Parameters:

  - `k` (keyword): the scale key.
  - `domain` (sequence): the datetime domain (read as wadogo stores it, `LocalDateTime`s).
  - `ticks-params` (map): `:unit`, `:step` (default 1).

  Returns `nil`. Throws `ex-info {:key :ticks-params}` when the ticks would exceed the limit."
  [k domain {:keys [unit step] :or {step 1}}]
  (let [[lo hi] (s/domain (s/scale :datetime {:domain domain}))]
    (try (pretty-datetime/validate-calendar-span lo hi unit step)
         (catch clojure.lang.ExceptionInfo e
           (schema/fail! (ex-message e) {:key :ticks-params :in [:scales k :ticks-params] :value {:unit unit :step step}})))))

(defn- pattern-formatter
  "Gives the formatter of a `java.time` pattern.

  Parameters:

  - `k` (keyword): the scale key.
  - `pattern` (string): a `DateTimeFormatter` pattern, e.g. `MMM yyyy`.

  Returns a function of a `java.time` value giving its text.

  Throws `ex-info {:key :formatter}` for an invalid pattern, or one a `LocalDateTime` can't format (zones)."
  [k pattern]
  (try (let [f (DateTimeFormatter/ofPattern pattern)]
         (.format f (LocalDateTime/of 2000 1 1 0 0))
         (fn [v] (.format f ^java.time.temporal.TemporalAccessor v)))
       (catch Exception e
         (schema/fail! (str "Invalid date pattern: " (ex-message e) ".") {:key :formatter :in [:scales k :formatter] :value pattern}))))

(defn scale-spec
  "Gives the phase-1 spec of a plot position scale.

  Parameters:

  - `k` (keyword): the scale key (`:x` / `:y`).
  - `spec` (map): wadogo attributes plus `:nice` and the cljplot2 options, valid by [[scale-spec-schema]]; a `:domain` is required.

  Returns `spec` with `:kind` (inferred from the domain when missing, see [[domain-kind]]) and `:nice` (default `true`, `false` for `:interpolated`: extend continuous domains to nice values by at most 10% of the range; a number sets that limit, a fraction; `false` turns it off; `nil` - the default); `:datetime` gets `:ticks-method :pretty` unless given; a pattern string `:formatter` becomes a function, `:formatter :power` the [[power-label]] (`:power-math` the [[power-math-label]]) of the scale's `:base` (default 10).

  Throws `ex-info {:key :domain}` without a domain or for a domain not suiting the kind (e.g. `:log` with 0), `ex-info {:key :ticks}` for a tick list not matching the kind and domain, `{:key :ticks-params}` for too many calendar ticks, and `ex-info` keyed by the option for options the kind doesn't take ([[kind-option-problem]]).

  See also [[data-scale-spec]], [[finalize]]."
  [k spec]
  (let [{:keys [domain ticks]} spec
        _ (when (nil? domain)
            (schema/fail! "Scale :domain is required when there is no data." {:key :domain :in [:scales k :domain]}))
        kind (or (:kind spec) (domain-kind domain))]
    (when-let [problem (domain-problem spec)] ;; a data domain may not suit the spec's kind (e.g. log)
      (schema/fail! (str "Scale " problem ".") {:key :domain :in [:scales k :domain] :value domain}))
    (when-let [[option-key message] (kind-option-problem kind spec)]
      (schema/fail! (str message ".") {:key option-key :in [:scales k option-key] :value (get spec option-key)}))
    (when (sequential? ticks) (validate-tick-list! k kind domain ticks))
    (when (and (= :datetime kind) (get-in spec [:ticks-params :unit]))
      (check-calendar-span! k domain (:ticks-params spec)))
    (cond-> (assoc spec :kind kind :nice (rows/option spec :nice (not= :interpolated kind)))
      (and (= :datetime kind) (nil? (:ticks-method spec))) (assoc :ticks-method :pretty)
      (string? (:formatter spec)) (update :formatter #(pattern-formatter k %))
      (= :power (:formatter spec)) (assoc :formatter (partial power-label (rows/option spec :base 10.0)))
      (= :power-math (:formatter spec)) (assoc :formatter (partial power-math-label (rows/option spec :base 10.0))))))

(def ^{:private true :const true :tag 'double} collapse-widening
  "Share of its magnitude a collapsed numeric domain widens by on each side."
  0.1)

(defn- widen
  "Gives the numeric domain of data limits, widened when collapsed.

  Parameters:

  - `lo`, `hi` (double): the data's minimum and maximum.

  Returns `[lo hi]`; a single value `v` widens to `[v - 0.1|v|, v + 0.1|v|]` (`[-1 1]` for 0)."
  [^double lo ^double hi]
  (cond
    (m/< lo hi) [lo hi]
    (m/zero? lo) [-1.0 1.0]
    :else (let [d (m/* collapse-widening (m/abs lo))] [(m/- lo d) (m/+ lo d)])))

(defn- pad
  "Gives the numeric domain extended by fractions of its span.

  Parameters:

  - `kind` (keyword): the scale kind - `:log` pads in log10 space.
  - `[lo hi]`: the domain.
  - `[plo phi]`: fractions of the span added below and above.

  Returns the padded `[lo hi]`."
  [kind [^double lo ^double hi] [^double plo ^double phi]]
  (let [log? (= kind :log)
        a (if log? (m/log10 lo) lo)
        b (if log? (m/log10 hi) hi)
        span (m/- b a)
        padded-a (m/- a (m/* plo span))
        padded-b (m/+ b (m/* phi span))]
    (if log? [(m/pow 10.0 padded-a) (m/pow 10.0 padded-b)] [padded-a padded-b])))

(defn- pad-domain
  "Gives the domain of any continuous kind padded by fractions of its span.

  Parameters:

  - `kind` (keyword): the scale kind.
  - `domain` (sequence): the domain; its ends are padded.
  - `padding`: `[plo phi]`, see [[pad]].

  Returns the padded `[lo hi]`; a `:datetime` domain is padded in epoch milliseconds (see [[cljplot2.data.rows/x-key]]), its ends back in their types (a `LocalDate` end becomes a `LocalDateTime`; niced domains are `LocalDateTime` already, `LocalTime` data included)."
  [kind domain padding]
  (let [a (first domain)
        b (last domain)]
    (if (= :datetime kind)
      (let [[ka kb] (pad :linear [(rows/x-key a) (rows/x-key b)] padding)]
        [(rows/key->x ka a) (rows/key->x kb b)])
      (pad kind [(double a) (double b)] padding))))

(defn- pad-pixels
  "Gives the numeric domain extended so that a number of pixels lies beyond it.

  Parameters:

  - `kind` (keyword): the scale kind - `:log` pads in log10 space.
  - `[lo hi]`: the domain.
  - `[plo phi]`: pixels of room below and above.
  - `length` (double): the range length, pixels.

  Returns the padded `[lo hi]`; unchanged when there is no room asked for or it would take more than half the range."
  [kind [^double lo ^double hi] [^double plo ^double phi] ^double length]
  (let [room (m/+ plo phi)]
    (if (or (m/<= room 0.0) (m/> room (m/* 0.5 length)))
      [lo hi]
      (let [log? (= kind :log)
            a (if log? (m/log10 lo) lo)
            b (if log? (m/log10 hi) hi)
            ;; data per pixel of the extended domain: (b - a) / (length - room)
            per-px (m// (m/- b a) (m/- length room))
            padded-a (m/- a (m/* plo per-px))
            padded-b (m/+ b (m/* phi per-px))]
        (if log? [(m/pow 10.0 padded-a) (m/pow 10.0 padded-b)] [padded-a padded-b])))))

(defn- pad-domain-pixels
  "Gives the domain of any continuous kind padded by pixels of room.

  Parameters:

  - `kind` (keyword): the scale kind.
  - `domain` (sequence): the domain; its ends are padded.
  - `padding-px`: `[plo phi]` pixels, see [[pad-pixels]].
  - `length` (double): the range length, pixels.

  Returns the padded `[lo hi]`; a `:datetime` domain is padded in epoch milliseconds (see [[cljplot2.data.rows/x-key]]), its ends back in their types (a `LocalDate` end becomes a `LocalDateTime`; a `LocalTime` end - only with `:nice false`, niced domains are `LocalDateTime` - stays within the day)."
  [kind domain padding-px length]
  (let [a (first domain)
        b (last domain)]
    (if (= :datetime kind)
      (let [[ka kb] (pad-pixels :linear [(rows/x-key a) (rows/x-key b)] padding-px length)]
        [(rows/key->x ka a) (rows/key->x kb b)])
      (pad-pixels kind [(double a) (double b)] padding-px length))))

(def ^:private extent-kinds
  "Data extent kind -> the scale kind of a data domain."
  {:numeric :linear :categorical :bands :temporal :datetime})

(defn- widen-temporal
  "Gives the datetime domain of data limits, widened when collapsed.

  Parameters:

  - `lo`, `hi`: the data's first and last `java.time` values.

  Returns `[lo hi]`; a single value widens to one day either side (a `LocalTime` - one hour, kept within the day)."
  [lo hi]
  (cond
    (not (m/zero? (compare lo hi))) [lo hi]
    (instance? LocalTime lo) (let [s (.toSecondOfDay ^LocalTime lo)]
                               [(LocalTime/ofSecondOfDay (m/max 0 (m/- s 3600))) (LocalTime/ofSecondOfDay (m/min 86399 (m/+ s 3600)))])
    :else [(.minus ^Temporal lo 1 ChronoUnit/DAYS) (.plus ^Temporal lo 1 ChronoUnit/DAYS)]))

(defn- period-starts
  "Gives the scale spec with period values read as dates.

  Parameters:

  - `spec` (map): the scale spec.

  Returns `spec` with every `Year` / `YearMonth` of an explicit `:domain` or `:ticks` vector as the date its period starts (see [[cljplot2.data.rows/period-start]]), as data values are read - unchanged on a bands or categorical scale, where they are levels."
  [spec]
  (if (or (= :bands (:kind spec)) (= :categorical (:type spec)))
    spec
    (cond-> spec
      (sequential? (:domain spec)) (update :domain #(mapv rows/period-start %))
      (sequential? (:ticks spec)) (update :ticks #(mapv rows/period-start %)))))

(defn- data-domain
  "Gives the kind and domain of a plot position scale from the data.

  Parameters:

  - `k` (keyword): the scale key.
  - `spec` (map): the scale spec (period values read as dates) - `:kind`.
  - `extent` (map): the merged data extent.

  Returns `[kind domain]`: `kind` from the spec or the extent (numeric -> `:linear`, categorical -> `:bands`, temporal -> `:datetime`); numeric and temporal domains widened when collapsed to one value.

  Throws `ex-info {:key :domain}` for numeric data or dates declared categorical (or on a bands scale) unless the extent carries its levels (`:values`, sorted)."
  [k spec extent]
  (let [kind (or (:kind spec) (extent-kinds (:kind extent)))]
    [kind
     (case (:kind extent)
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
                                      {:key :domain :in [:scales k :domain]})))]))

(defn data-scale-spec
  "Gives the phase-1 spec of a plot position scale from its spec and the data.

  Parameters:

  - `k` (keyword): the scale key.
  - `spec` (map): the plot's scale spec (may be `{}`); an explicit `:domain` wins over data (`nil` or `[nil nil]` - not set); a partial `[lo nil]` / `[nil hi]` (continuous kinds, not `:interpolated`) fixes one end, the other the data's.
  - `extent` (map or `nil`): the merged data extent.
  - `padding`: the charts' padding `[lo hi]`, fractions of the span.

  Returns the [[scale-spec]] of the explicit domain, else of the data's: `:kind` from the spec or the extent (numeric -> `:linear`, categorical -> `:bands`, temporal -> `:datetime`); numeric and temporal data domains widened when collapsed to one value; a partial domain completed by the data domain's end ([[cljplot2.data.rows/completed-domain]]). Continuous domains - data or explicit, `:interpolated` aside - get `:padding`: the spec's `:expand`, else the charts' `padding` (only with data; `:radial` - none below the data); [[finalize]] applies it after nice, so the outermost ticks sit inside the panel (datetime domains in epoch milliseconds).

  Throws `ex-info {:key :domain}` for an `:interpolated` scale without a domain, a partial domain without data or leaving no data on its open side, and for numeric data or dates declared categorical (or on a bands scale) without a domain, unless the extent carries its levels (`:values`, sorted)."
  [k spec extent padding]
  (let [set-spec (cond-> spec (nil? (rows/given-domain (:domain spec))) (dissoc :domain))]
    (when (and (= :interpolated (:kind set-spec)) (nil? (:domain set-spec)))
      (schema/fail! "An :interpolated scale needs an explicit :domain (its stop values)." {:key :domain :in [:scales k :domain]}))
    (let [period-spec (period-starts set-spec)
          explicit (:domain period-spec)
          partial? (rows/partial-domain? explicit)
          built (cond
                  (and partial? (nil? extent)) (rows/completed-domain k explicit nil)
                  (or (and (some? explicit) (not partial?)) (nil? extent)) (scale-spec k period-spec)
                  :else (let [[kind domain] (data-domain k period-spec extent)]
                          ;; the kind may come from the data (e.g. a numeric end over categorical data)
                          (when-let [problem (and partial? (partial-domain-problem (assoc period-spec :kind kind)))]
                            (schema/fail! (str "Scale :domain: " problem ".") {:key :domain :in [:scales k :domain] :value explicit}))
                          (scale-spec k (assoc period-spec :kind kind
                                               :domain (if partial? (rows/completed-domain k explicit domain) domain)))))
          domain-padding (or (:expand period-spec)
                             (when extent (if (= :radial (:kind built)) (assoc (vec padding) 0 0.0) padding)))]
      (cond-> built
        (and domain-padding (not= :interpolated (:kind built)) (continuous-kinds (:kind built)))
        (assoc :padding (vec domain-padding))))))

(defn tick-hint
  "Gives the tick count hint for an axis.

  Parameters:

  - `length` (double): the axis length, pixels.
  - `spacing` (double): pixels between ticks.

  Returns the nearest whole count (rounded, so panels a few pixels apart around a whole multiple of `spacing` ask for the same count), at least 2."
  ^long [^double length ^double spacing]
  (m/max 2 (m/round (m// length spacing))))

(defn- integral-numbers?
  "Tests whether ticks are all whole numbers.

  Parameters:

  - `ticks` (sequence): tick values.

  Returns a truthy value when there are ticks and each is a finite whole number below [[max-integral-label]] in magnitude."
  [ticks]
  (and (seq ticks)
       (every? #(and (number? %)
                     (let [v (double %)]
                       (and (m/valid-double? v) (m/< (m/abs v) max-integral-label) (m/== v (m/floor v)))))
               ticks)))

(defn- default-tick-labels
  "Gives the tick labels without a user formatter.

  Parameters:

  - `scale`: the wadogo scale.
  - `spec` (map): the scale spec - `:formatter`, `:kind`.
  - `ticks` (sequence): tick values.

  Returns a vector of texts: whole-number ticks of a numeric continuous scale without `:formatter` as integers, otherwise wadogo `format`."
  [scale spec ticks]
  (if (and (nil? (:formatter spec)) (not= :datetime (:kind spec)) (not= :bands (:kind spec)) (integral-numbers? ticks))
    (mapv #(str (m/round (double %))) ticks)
    (vec (s/format scale ticks))))

(defn tick-labels
  "Gives the labels of the ticks of a scale.

  Parameters:

  - `scale`: the wadogo scale built from `spec`.
  - `spec` (map): `:kind`, `:formatter`, `:tick-format`.
  - `ticks` (sequence): tick values.

  Returns a vector of texts: whole-number ticks of a numeric continuous scale as integers, otherwise wadogo `format` (see [[default-tick-labels]]); `:tick-format :absolute` formats the magnitudes of numeric ticks; a spec `:formatter` always wins (wadogo uses it)."
  [scale spec ticks]
  (let [shown (if (and (= :absolute (:tick-format spec)) (nil? (:formatter spec)))
                (mapv #(if (number? %) (m/abs (double %)) %) ticks)
                ticks)]
    (default-tick-labels scale spec shown)))

(defn- ->wadogo
  "Gives the wadogo scale of a finalized scale.

  Parameters:

  - `finalized` (map): a [[finalize]]d scale - `:kind`, `:attributes`.

  Returns the wadogo scale; a `:radial` one is built over `[0, end - start]` (pixels come from [[forward-fn]], which adds the range start)."
  [finalized]
  (s/scale (:kind finalized) (:attributes finalized)))

(def ^{:private true :const true :tag 'double} range-tolerance
  "Pixels a tick may lie outside its range and still count as inside."
  1.0e-6)

(defn- inside-range?
  "Tests whether a pixel lies within a range.

  Parameters:

  - `range`: `[a b]` pixels, either orientation.
  - `p` (double): the pixel.

  Returns true within the range, up to [[range-tolerance]]."
  [[^double a ^double b] ^double p]
  (m/<= (m/- (m/min a b) range-tolerance) p (m/+ (m/max a b) range-tolerance)))

(def ^{:private true :const true :tag 'double} default-nice-max-extension
  "Largest total extension of the domain by `nice`, as a fraction of the range span."
  0.1)

(defn- nice-domain
  "Gives the domain of a scale extended to nice tick values.

  Parameters:

  - `scale`: the wadogo scale.
  - `kind` (keyword): its kind; only continuous kinds are extended.
  - `nice-option`: `true` - at most [[default-nice-max-extension]] of the span, a number - that limit, falsy - none.

  Returns the (extended) domain."
  [scale kind nice-option]
  (if (and nice-option (continuous-kinds kind))
    (s/domain (s/nice scale {:max-extension (if (number? nice-option) nice-option default-nice-max-extension)}))
    (s/domain scale)))

(defn- pixel-offset
  "Gives the pixels added to the wadogo position of a finalized scale.

  A `:radial` scale is built over `[0, end - start]` (wadogo squares the range ends, so its distances grow as the square root of the value only from 0) and shifted by the range start.

  Parameters:

  - `kind` (keyword): the scale kind.
  - `pixel-range`: `[start end]` pixels.

  Returns the range start for `:radial`, else 0."
  ^double [kind pixel-range]
  (if (= :radial kind) (double (first pixel-range)) 0.0))

(defn- wadogo-range
  "Gives the range given to wadogo for a scale.

  Parameters:

  - `kind` (keyword): the scale kind.
  - `range`: `[start end]` pixels.

  Returns `[0 end - start]` for `:radial` (see [[pixel-offset]]), else the range itself."
  [kind [start end]]
  (if (= :radial kind) [0.0 (m/- (double end) (double start))] [start end]))

(defn- stop-pixels
  "Gives the pixel positions of the stops of an `:interpolated` scale.

  Parameters:

  - `range`: `[start end]` pixels.
  - `n` (long): the stop count (domain values), at least 2 (see [[domain-problem]]).
  - `stops` (sequence or `nil`): fractions of the way from start to end; `nil` - evenly spaced.

  Returns a vector of `n` pixel positions."
  [[^double start ^double end] ^long n stops]
  (if stops
    (mapv (fn [^double f] (m/+ start (m/* f (m/- end start)))) stops)
    (vec (m/slice-range start end n))))

(defn- near-any?
  "Tests whether a value equals one of some values up to a relative tolerance.

  Parameters:

  - `values` (sequence): numbers.
  - `v` (number): the value.

  Returns a truthy value when some value is within 1e-9 of the larger magnitude of the two (two zeros are equal)."
  [values v]
  (let [x (double v)]
    (some #(m/<= (m/abs (m/- (double %) x)) (m/* 1.0e-9 (m/max (m/abs (double %)) (m/abs x)))) values)))

(defn- log-minor-ticks
  "Gives the minor tick candidates of a log scale.

  Parameters:

  - `lo`, `hi` (doubles): the positive domain.
  - `base` (double): the log base.

  Returns the multiples 2 .. base-1 of every power of `base` over `[lo hi]` (none for a base below 3)."
  [^double lo ^double hi ^double base]
  (let [from (long (m/floor (m// (m/ln lo) (m/ln base))))
        to (long (m/ceil (m// (m/ln hi) (m/ln base))))]
    (for [e (range from (m/long-inc to))
          j (range 2 (long (m/ceil base)))
          :let [v (m/* (double j) (m/pow base (double e)))]
          :when (m/< (double j) base)]
      v)))

(defn- subdivided-ticks
  "Gives the minor tick candidates between major ticks.

  Parameters:

  - `ts` (vector): the sorted major ticks, at least two.
  - `lo`, `hi` (doubles): the domain.
  - `n` (long): the subdivisions of each interval.

  Returns the candidates: `n` subdivisions of each interval, the first and last intervals repeated past the outer ticks while they reach into `[lo hi]`."
  [ts ^double lo ^double hi ^long n]
  (let [c (count ts)
        t0 (double (ts 0))
        tn (double (ts (m/long-dec c)))
        first-step (m/- (double (ts 1)) t0)
        last-step (m/- tn (double (ts (m/long-sub c 2))))
        ;; [start step] of every interval: outer ones repeated while they still reach into the domain
        before (map (fn [^long k] [(m/- t0 (m/* k first-step)) first-step])
                    (take-while (fn [^long k] (m/> (m/- t0 (m/* (m/long-dec k) first-step)) lo)) (iterate m/long-inc 1)))
        inner (map (fn [^double a ^double b] [a (m/- b a)]) ts (rest ts))
        after (map (fn [^long k] [(m/+ tn (m/* k last-step)) last-step])
                   (take-while (fn [^long k] (m/< (m/+ tn (m/* k last-step)) hi)) (iterate m/long-inc 0)))]
    (for [[^double start ^double step] (concat before inner after)
          j (range 1 n)]
      (m/+ start (m// (m/* (double j) step) (double n))))))

(defn minor-tick-values
  "Gives the minor ticks of a numeric scale.

  Parameters:

  - `kind` (keyword): the scale kind.
  - `domain` (sequence): the numeric domain.
  - `ticks` (sequence): the major ticks.
  - `n` (long): subdivisions of an interval between major ticks (other kinds than `:log`).
  - `base` (number): the logarithm base (`:log`).

  Returns a vector of data values inside the domain, major ticks left out: on `:log` the multiples 2 .. base-1 of every power of `base` (none for a base below 3); on other kinds `n` subdivisions of each interval between major ticks (`n - 1` ticks per interval; the first and last intervals repeated past the outer ticks); `[]` with fewer than two major ticks (other kinds)."
  [kind domain ticks n base]
  (let [lo (double (v/mn domain))
        hi (double (v/mx domain))
        inside (fn [vs] (filterv #(and (m/<= lo (double %) hi) (not (near-any? ticks %))) vs))]
    (if (= :log kind)
      (inside (log-minor-ticks lo hi (double base)))
      (let [ts (vec (sort (map double ticks)))]
        (if (m/< (count ts) 2)
          []
          (inside (subdivided-ticks ts lo hi (long n))))))))

(def ^{:const true :tag 'long} max-tick-count
  "Largest tick count [[ticks-inside]] asks for while looking for two ticks inside a domain."
  12)

(defn ticks-inside
  "Gives the ticks of a scale, with at least two inside its domain when possible.

  An axis shows at least two ticks to read its span: when fewer than 2 are inside (a nice step wider than a short domain), counts `n + 1` ... [[max-tick-count]] are tried until 2 are; the last count's ticks when none gives two (e.g. a log domain within one step of its ticks).

  Parameters:

  - `scale-of`: a function of a tick count giving a wadogo scale.
  - `n` (long): the first count tried.
  - `inside?`: a predicate of a tick value.

  Returns `{:count n :scale sc :ticks [..]}` - the count used, its scale and its ticks kept by `inside?`."
  [scale-of ^long n inside?]
  (loop [tick-count n]
    (let [scale (scale-of tick-count)
          ticks (filterv inside? (s/ticks scale))]
      (if (or (m/>= (count ticks) 2) (m/>= tick-count max-tick-count))
        {:count tick-count :scale scale :ticks ticks}
        (recur (m/long-inc tick-count))))))

(defn keep-labels?
  "Tests whether an axis draws every tick label, overlapping or not.

  Parameters:

  - `spec` (map): the scale spec - `:overlapping-labels`, `:ticks`.

  Returns true for `:overlapping-labels :keep`, false for `:drop`; else whether `:ticks` is an explicit vector (the values asked for)."
  [{:keys [overlapping-labels ticks]}]
  (case overlapping-labels
    :keep true
    :drop false
    (sequential? ticks)))

(defn finalize
  "Gives the phase-2 finalized scale of a phase-1 spec for a pixel range.

  Steps: the domain is extended by nice (limited, see `:nice` in [[scale-spec]]) with the explicit tick count or the hint, then padded by `:padding` (see [[data-scale-spec]]), then by the pixel room `:padding-px` `[lo hi]` (charts' labels past the data; `:linear`, `:log` and `:datetime` - in epoch milliseconds, skipped when over half the range); `:reverse` reverses the final domain. Ticks outside the final domain are dropped - a continuous scale without `:ticks` gets more ticks until two are inside (see [[ticks-inside]]); a bands scale without `:ticks` has a tick at every band.

  Parameters:

  - `spec` (map): the phase-1 spec ([[scale-spec]], [[data-scale-spec]]).
  - `pixel-range`: `[start end]` pixels.
  - `tick-count-hint` (long): the tick count used unless the spec has `:ticks` (see [[tick-hint]]).

  Returns plain data `{:kind :domain :range :ticks :tick-labels :attributes}` (`:attributes` - the wadogo attributes, `:ticks` the count used), plus `:axis` for `:axis false` / `:grid` / `:no-grid`, `:overlapping-labels :keep` for a scale keeping its labels ([[keep-labels?]]), `:minor-ticks` (values, see [[minor-tick-values]]) and `:axis-scale` - the finalized linear scale of an `:axis-domain` (see [[display-scale]])."
  [spec pixel-range tick-count-hint]
  (let [kind (:kind spec)
        explicit (:ticks spec)
        nice-count (if (integer? explicit) explicit tick-count-hint)
        base (cond-> (assoc (dissoc spec :kind :nice :expand :padding :padding-px :tick-format :axis :stops :minor-ticks
                                    :axis-domain :reverse :overlapping-labels)
                            :range (wadogo-range kind pixel-range))
               (get-in spec [:ticks-params :min-n]) (update-in [:ticks-params :min-n] #(m/min (long %) (long nice-count)))
               (= :interpolated kind) (assoc :range (stop-pixels pixel-range (count (:domain spec)) (:stops spec))))
        niced (nice-domain (s/scale kind (assoc base :ticks nice-count)) kind (:nice spec))
        padded (if-let [padding (:padding spec)]
                 (pad-domain kind niced padding)
                 niced)
        room-padded (if-let [padding-px (when (#{:linear :log :datetime} kind) (:padding-px spec))]
                      (pad-domain-pixels kind padded padding-px (m/abs (m/- (double (second pixel-range)) (double (first pixel-range)))))
                      padded)
        domain (cond-> room-padded (:reverse spec) (-> reverse vec))
        tick-attributes (assoc base :domain domain :ticks (or explicit (if (= kind :bands) (vec domain) tick-count-hint)))
        offset (pixel-offset kind pixel-range)
        position (s/scale kind tick-attributes)
        inside? #(inside-range? pixel-range (m/+ offset (double (position %))))
        {scale :scale ticks :ticks tick-count :count}
        (cond
          (not (continuous-kinds kind)) {:scale position :ticks (vec (s/ticks position))}
          explicit {:scale position :ticks (filterv inside? (s/ticks position))}
          :else (ticks-inside #(s/scale kind (assoc tick-attributes :ticks %)) tick-count-hint inside?))
        attributes (cond-> tick-attributes tick-count (assoc :ticks tick-count))]
    (cond-> {:kind kind
             :domain domain
             :range pixel-range
             :ticks ticks
             :tick-labels (tick-labels scale spec ticks)
             :attributes attributes}
      (contains? #{false :grid :no-grid} (:axis spec)) (assoc :axis (:axis spec))
      (keep-labels? spec) (assoc :overlapping-labels :keep)
      (:minor-ticks spec) (assoc :minor-ticks (filterv inside? (minor-tick-values kind domain ticks (:minor-ticks spec)
                                                                                   (get attributes :base 10.0))))
      (:axis-domain spec) (assoc :axis-scale (finalize (merge (select-keys spec [:formatter :minor-ticks :ticks-method :ticks-params])
                                                              {:kind :linear :domain (:axis-domain spec) :nice false})
                                                       pixel-range tick-count-hint)))))

(defn display-scale
  "Gives the scale an axis and the panel grid show for a finalized scale.

  Parameters:

  - `finalized` (map): a [[finalize]]d scale.

  Returns its `:axis-scale` (from an `:axis-domain` - display only: the ticks and labels of that linear domain over the same pixel range, the charts still draw with `finalized`) carrying the scale's `:direction`, `:axis` and `:overlapping-labels`; else `finalized` itself."
  [finalized]
  (if-let [shown (:axis-scale finalized)]
    (merge shown (select-keys finalized [:direction :axis :overlapping-labels]))
    finalized))

(def ^:private identity-transform
  "Stat coordinates of a scale without a transform: both ways the value itself."
  {:forward (fn ^double [^double v] v) :inverse (fn ^double [^double v] v)})

(def ^:private log10-transform
  "Stat coordinates of a `:log` scale: log10 (`##NaN` for a non-positive value) and back."
  {:forward (fn ^double [^double v] (if (m/pos? v) (m/log10 v) ##NaN))
   :inverse (fn ^double [^double v] (m/pow 10.0 v))})

(defn axis-drawn?
  "Tests whether the axis (track, ticks, tick labels) of a scale is drawn.

  Parameters:

  - `scale` (map): a finalized scale or a spec - `:axis`.

  Returns true unless `:axis` is `false` or `:grid`."
  [scale]
  (not (contains? #{false :grid} (:axis scale))))

(defn grid-drawn?
  "Tests whether the grid lines (and band fills) of a scale are drawn.

  Parameters:

  - `scale` (map): a finalized scale or a spec - `:axis`.

  Returns true unless `:axis` is `false` or `:no-grid`."
  [scale]
  (not (contains? #{false :no-grid} (:axis scale))))

(defn stat-transform
  "Gives the coordinates a stat (field, contour, bin2d, smooth) works in on an axis, so densities and bins are even on screen.

  Parameters:

  - `spec` (map or `nil`): the plot's scale spec.

  Returns `{:forward f :inverse g}`, functions of a double: log10 for `:log` (a non-positive value maps to `##NaN`), the identity otherwise (other non-linear kinds are not transformed)."
  [spec]
  (if (= :log (:kind spec)) log10-transform identity-transform))

(defn stat-transform-for
  "Gives the transform of one axis a chart's statistic runs in, by the chart's stat space (its `:stat-space` option, see [[cljplot2.chart.common/stat-space-option]]).

  Parameters:

  - `space` (keyword): `:scale` - the axis's [[stat-transform]], `:data` - data units.
  - `spec` (map or `nil`): the plot's scale spec of the axis (`:kind`).

  Returns `{:forward f :inverse g}`: log10 on a `:log` axis in `:scale` space, the identity otherwise."
  [space spec]
  (stat-transform (when (= :scale space) spec)))

(defn value-direction
  "Gives the pixel direction of increasing values of a finalized continuous scale.

  Parameters:

  - `finalized` (map): a [[finalize]]d scale - `:range [r0 r1]`, `:domain [d0 d1]` (reversed by `:reverse`).

  Returns `1.0` when larger values lie toward larger range positions (pixels, or radians / radii in a polar plot), else `-1.0` - a decreasing range or a reversed domain flips it, both keep it."
  ^double [{[^double r0 ^double r1] :range [d0 d1] :domain}]
  (let [range-sign (if (m/< r1 r0) -1.0 1.0)]
    (if (m/pos? (long (compare d0 d1))) (m/- range-sign) range-sign)))

(defn forward-fn
  "Gives the mapping of values to pixel positions of a finalized scale.

  Builds the wadogo scale once; reuse the function for many values.

  Parameters:

  - `finalized` (map): a [[finalize]]d scale.

  Returns a function of a value giving its pixel position as a double (the band centre for `:bands`; a `Year` / `YearMonth` on a `:datetime` scale at its period's start); `##NaN` for a value the scale can't place (e.g. not in a bands domain) - callers skip such values.

  See also [[forward]], [[inverse-fn]], [[projection]]."
  [finalized]
  (let [scale (->wadogo finalized)
        offset (pixel-offset (:kind finalized) (:range finalized))
        ;; a datetime scale places a Year / YearMonth at its period's start
        value-of (if (= :datetime (:kind finalized)) rows/period-start identity)]
    (fn ^double [value] (let [p (scale (value-of value))] (if (nil? p) ##NaN (m/+ offset (double p)))))))

(defn inverse-fn
  "Gives the mapping of pixel positions to values of a finalized continuous scale.

  Builds the wadogo scale once; reuse the function for many positions.

  Parameters:

  - `finalized` (map): a [[finalize]]d continuous scale.

  Returns a function of a pixel (double) giving its value (wadogo `inverse`): a number, or a `LocalDateTime` on a `:datetime` scale, rounded to the millisecond.

  Throws `ex-info {:key :kind}` for a `:bands` scale (no inverse)."
  [finalized]
  (when-not (continuous-kinds (:kind finalized))
    (schema/fail! (str "A " (:kind finalized) " scale has no inverse; a continuous scale is needed.")
                  {:key :kind :value (:kind finalized)}))
  (let [scale (->wadogo finalized)
        offset (pixel-offset (:kind finalized) (:range finalized))]
    (fn [^double pixel] (s/inverse scale (m/- pixel offset)))))

(defn forward
  "Gives the pixel position of a single value in a finalized scale; for many values use [[forward-fn]].

  Parameters:

  - `finalized` (map): a [[finalize]]d scale.
  - `value`: the data value.

  Returns the pixel position as a double, `##NaN` when the scale can't place the value."
  ^double [finalized value]
  ((forward-fn finalized) value))

(defn band-geometry
  "Gives the band sizes of a finalized bands scale.

  Parameters:

  - `finalized` (map): a [[finalize]]d scale.

  Returns `{:bandwidth px :step px}` (wadogo's band width and the distance between band starts), `nil` for other kinds."
  [finalized]
  (when (= :bands (:kind finalized))
    (select-keys (s/data (->wadogo finalized)) [:bandwidth :step])))

;; directions (plot `:flip`)

(defn horizontal-key
  "Gives the position scale key drawn horizontally.

  Parameters:

  - `flip?`: the plot is flipped (`:flip`).

  Returns `:y` when flipped, else `:x`."
  [flip?]
  (if flip? :y :x))

(defn direction
  "Gives the direction of a finalized position scale.

  Parameters:

  - `k` (keyword): the scale key.
  - `finalized` (map): the finalized scale.

  Returns its `:direction` (`:horizontal` / `:vertical`, set by the pipeline), else by key - `:x` horizontal, `:y` vertical."
  [k finalized]
  (or (:direction finalized) (if (= k :x) :horizontal :vertical)))

(defn horizontal?
  "Tests whether a finalized position scale is drawn horizontally.

  Parameters:

  - `k` (keyword): the scale key.
  - `finalized` (map): the finalized scale.

  Returns true when its [[direction]] is `:horizontal`."
  [k finalized]
  (= :horizontal (direction k finalized)))

(defn flipped?
  "Tests whether finalized position scales draw `:x` vertically.

  Parameters:

  - `scales` (map): `{:x .. :y ..}`, either may be missing.

  Returns true when `:x` is vertical (or, without `:x`, `:y` horizontal); false without both."
  [scales]
  (cond
    (:x scales) (not (horizontal? :x (:x scales)))
    (:y scales) (horizontal? :y (:y scales))
    :else false))

(defn- forward-or-nan
  "Gives the forward function of a finalized scale, or one placing nothing.

  Parameters:

  - `finalized` (map or `nil`): a finalized position scale.

  Returns its [[forward-fn]]; for `nil` a function giving `##NaN`."
  [finalized]
  (if finalized (forward-fn finalized) (fn ^double [_] ##NaN)))

(defn coord-geometry
  "Gives the polar geometry of finalized position scales.

  Parameters:

  - `scales` (map): `{:x .. :y ..}`.

  Returns the `:coord` of either scale (see [[cljplot2.scale.coord/geometry]]); `nil` - cartesian (range positions are pixels)."
  [scales]
  (or (:coord (:x scales)) (:coord (:y scales))))

(defn range-projection
  "Gives the mapping of data points to range points of finalized position scales.

  Parameters:

  - `scales` (map): `{:x .. :y ..}`, finalized.

  Returns a function of data `x`, `y` giving `[h v]`: the horizontal scale's range position, then the vertical one's (pixels in cartesian plots - the same as [[projection]]; angle and radius in polar ones, see [[cljplot2.scale.coord]]); `##NaN` where a scale can't place a value."
  [scales]
  (let [fx (forward-or-nan (:x scales))
        fy (forward-or-nan (:y scales))]
    (if (flipped? scales)
      (fn [x y] [(double (fy y)) (double (fx x))])
      (fn [x y] [(double (fx x)) (double (fy y))]))))

(defn projection
  "Gives the mapping of data points to pixels of finalized position scales - charts place marks only through it.

  Builds the wadogo scales once; reuse the function for many points.

  Parameters:

  - `scales` (map): `{:x .. :y ..}`, finalized.

  Returns a function of data `x`, `y` giving the pixel `Vec2`: the horizontal scale's position gives its x, the vertical scale's its y (`:x` horizontal unless [[flipped?]]); in a polar plot they are the angle and radius, turned into the pixel by [[cljplot2.scale.coord/to-pixel]]. A value the scale can't place, or a missing scale, gives `##NaN` in that coordinate.

  See also [[range-projection]]."
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
