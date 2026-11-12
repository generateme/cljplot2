(ns cljplot2.scale
  "Position scales in two phases (D5).

  Phase 1, [[scale-spec]]: a plot's `:scales` entry (wadogo attributes plus `:nice`) validated into a spec,
  `:kind` inferred from the domain when missing. Plain data.

  Phase 2, [[finalize]]: after layout, the spec with a pixel `:range` and a tick count hint becomes a finalized
  scale - plain data `{:kind :domain :range :ticks :tick-labels :attributes}`. Continuous kinds are extended to
  nice tick values with that count (unless `:nice false`). [[->wadogo]] rebuilds the wadogo scale, [[forward]]
  maps a value to a pixel."
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

(defn scale-spec
  "Validated phase-1 spec of the plot scale `k` from its attributes `spec` (a map of wadogo attributes and `:nice`).
  Throws `ex-info` naming the offending key."
  [k spec]
  (when-not (map? spec) (invalid "Scale spec must be a map." {:key :scales :scale k}))
  (let [{:keys [domain ticks]} spec
        _ (when (nil? domain) (invalid "Scale :domain is required." {:key :domain :scale k}))
        kind (or (:kind spec) (infer-kind k domain))]
    (when-not (position-kinds kind)
      (invalid "Unsupported position scale kind." {:key :kind :scale k :value kind :allowed position-kinds}))
    (validate-domain! k kind domain)
    (when-not (or (nil? ticks) (sequential? ticks) (and (integer? ticks) (pos? (long ticks))))
      (invalid "Scale :ticks must be a positive integer or a sequence of values." {:key :ticks :scale k :value ticks}))
    (assoc spec :kind kind :nice (get spec :nice true))))

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

(defn finalize
  "Finalized scale of phase-1 `spec` for the pixel `range` (`[start end]`) and `tick-hint` (used unless the spec
  has `:ticks`)."
  [spec range tick-hint]
  (let [kind (:kind spec)
        attributes (-> (dissoc spec :kind :nice)
                       (assoc :range range :ticks (or (:ticks spec) tick-hint)))
        scale (cond-> (s/scale kind attributes)
                (and (:nice spec) (continuous-kinds kind)) s/nice)
        attributes (assoc attributes :domain (s/domain scale))
        ticks (vec (s/ticks scale))]
    {:kind kind
     :domain (s/domain scale)
     :range range
     :ticks ticks
     :tick-labels (tick-labels scale spec ticks)
     :attributes attributes}))

(defn forward
  "Pixel position of `value` in a finalized scale (band center for `:bands`)."
  ^double [finalized value]
  (double ((->wadogo finalized) value)))
