(ns cljplot2.chart.probability
  "Probability plots: Q-Q (`chart/qq` - quantiles against quantiles) and P-P (`chart/pp` - probabilities against
  probabilities) of two sides, see [[cljplot2.data.probability]] for the statistics.

  Each side is data - the mapped `:x` / `:y` - or a distribution - `:x-distribution` / `:y-distribution`: a fastmath
  distribution object, `[:name]` or `[:name params]` (fastmath validates and throws). A distribution side wins over
  the plot's mapping of that side; the chart's own `:mapping` of it throws `ex-info {:key :x-distribution}`; a side
  with neither throws `ex-info {:key :mapping}`.

  * Data sides: statistics per panel and group (ordinal aesthetics `:color`, `:shape`, `:size` and `:group`) over the
    group's finite values, each column on its own (rows with a non-finite value of a data side are dropped from that
    side and counted); a group with an empty side draws nothing. Two data sides are two columns of the same rows.
  * Both sides distributions: no data - the chart is drawn in every panel and inherits only the plot's facet keys
    (as annotations); Q-Q at `:points` plotting positions, P-P at `:points` values over the required `:domain [lo
    hi]` (`ex-info {:key :domain}` without it); `:points` default 100. `:label` (default \"<y name> vs <x name>\")
    is the chart's one row `{:label label}`: the chart's own `:mapping {:color :label}` (`:shape`, `:size`) styles it
    and gives a legend key, e.g. several distribution pairs in one plot.
    `:points`, `:domain` and `:label` with a data side throw `ex-info` keyed by the option.

  P-P evaluates at the sorted distinct pooled values of the data sides; its extents are `[0 1]` on both axes.

  Reference line `:line` - Q-Q `:quartiles` (default; through the first and third quartile pairs, R `qqline`), P-P
  `:identity` (default; `y = x`), `false` - none. One line per group in the group's colour, between the x domain
  ends; it needs linear x and y scales (`ex-info {:key :line}`) and adds nothing to the extents.

  Axis titles: a distribution side - \"<name> quantiles\" / \"<name> probabilities\" (\"Theoretical ...\" for an
  object); a data side mapped by a keyword - \"<key>\" (Q-Q) / \"<key> probabilities\" (P-P).

  Options (style defaults from theme `[:chart :qq]` / `[:chart :pp]`): point style as [[cljplot2.chart.scatter]]
  (`:size`, `:color`, `:stroke-color`, `:stroke-width`, `:shape`), `:line-width`, `:padding`, plus `:data` /
  `:mapping`."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.chart.rule :as rule]
            [cljplot2.chart.scatter :as scatter]
            [cljplot2.data.facet :as facet]
            [cljplot2.data.group :as group]
            [cljplot2.data.probability :as probability]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.graphics.shape :as shape]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private ^:const default-points 100)

(def ^:private style-entries
  (conj scatter/style-entries (schema/optional :line-width {:description "reference line width in pixels"} schema/non-neg)))

(def ^:private style-keys [:size :color :stroke-color :stroke-width :shape :line-width])

(defn- distribution-entry
  [k side]
  (schema/nilable k {:description (str "the " side " side as a distribution instead of data: a fastmath distribution, [:name] or [:name params]"
                                       " (wins over the plot's mapping of " side ")")}
                  probability/distribution-schema))

(defn- options-schema
  "Options of a probability plot; `line-props` - the documenting properties of `:line` (values `line-values`)."
  [line-values line-props extra]
  (-> [:map {:closed true}]
      (into style-entries)
      (into data/common-options)
      (conj (distribution-entry :x-distribution ":x")
            (distribution-entry :y-distribution ":y")
            (schema/optional :line line-props (into [:enum false] line-values))
            (schema/optional :points {:description "both sides distributions only: the number of points (Q-Q plotting positions, P-P values over :domain)"
                                      :cljplot2/default-doc "100; an error with a data side"}
                             [:int {:min 2}])
            (schema/nilable :label {:description "both sides distributions only: the label of the chart's one row {:label l}, styled by the chart's own mapping of :label (e.g. {:color :label})"
                                    :cljplot2/default-doc "\"<y name> vs <x name>\"; an error with a data side"}
                            [:or :string :keyword])
            (schema/optional :padding {:description "padding of the data span per position axis, {:x [lo hi] :y [lo hi]} fractions of the span"
                                       :cljplot2/default-doc "{:x [0.05 0.05] :y [0.05 0.05]}"}
                             data/position-padding))
      (into extra)))

(def ^:private mapping-schema
  [:map
   (schema/nilable :x {:description "data of the x side (unless :x-distribution is given)"} schema/accessor)
   (schema/nilable :y {:description "data of the y side (unless :y-distribution is given)"} schema/accessor)
   (schema/nilable :color {:description "groups, each with its own point and line colour"} schema/accessor)
   (schema/nilable :shape {:description "groups, each with its own point shape"} schema/accessor)
   (schema/nilable :size {:description "groups, each with its own point size"} schema/accessor)
   (schema/nilable :group {:description "groups without a visual aesthetic"} schema/accessor)])

(def qq-schemas
  "Schemas of the Q-Q chart: `:style` (theme `[:chart :qq]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (options-schema [:quartiles] {:description "reference line, one per group: :quartiles (through the first and third quartile pairs, R qqline) or false (none)"
                                          :default :quartiles}
                            [])
   :mapping mapping-schema})

(def pp-schemas
  "Schemas of the P-P chart: `:style` (theme `[:chart :pp]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (options-schema [:identity] {:description "reference line, one per group: :identity (y = x) or false (none)"
                                         :default :identity}
                            [(schema/optional :domain {:description "both sides distributions only (then required): [lo hi] of the values the CDFs are evaluated at"}
                                              (schema/with-rules [:tuple schema/finite schema/finite]
                                                [#(when-not (< (double (first %)) (double (second %)))
                                                    "should be [lo hi] with lo < hi")]))])
   :mapping mapping-schema})

(def ^:private distribution-keys {:x :x-distribution :y :y-distribution})

(defn distributions-only?
  "Are both sides of the chart `options` distributions?"
  [options]
  (and (some? (:x-distribution options)) (some? (:y-distribution options))))

(defn- sides
  "`{:x side :y side}` of the chart: `{:distribution d :name s}` or `{:accessor f :key acc}` - a distribution option
  wins over the merged `mapping`; throws for a side also in the chart's own mapping `own`, or with neither."
  [options own mapping]
  (into {} (map (fn [k]
                  (let [dk (distribution-keys k)
                        spec (get options dk)
                        acc (get mapping k)]
                    (cond
                      (and (some? spec) (some? (get own k)))
                      (schema/fail! (str "Probability plot side " k " is both in the chart's :mapping and given " dk ".")
                                    {:key dk :value spec})
                      (some? spec) [k {:distribution (probability/distribution spec) :name (probability/distribution-name spec)}]
                      (some? acc) [k {:accessor (data/row-accessor k acc) :key acc}]
                      :else (schema/fail! (str "Probability plot side " k " needs a mapping or " dk ".")
                                          {:key :mapping :value k})))))
        [:x :y]))

(defn- distributions-only!
  "Throw `ex-info {:key k}` for an option `k` of two distributions (`:points`, `:domain`, `:label`) given with a data
  side."
  [options]
  (when-not (distributions-only? options)
    (doseq [k [:points :domain :label] :when (some? (get options k))]
      (schema/fail! (str "Probability plot " k " applies only when both sides are distributions.") {:key k :value (get options k)}))))

(defn- axis-title
  [kind {:keys [distribution name key]}]
  (let [what (if (= kind :qq) "quantiles" "probabilities")]
    (cond
      distribution (str (or name "Theoretical") " " what)
      (keyword? key) (if (= kind :qq) (clojure.core/name key) (str (clojure.core/name key) " " what)))))

(defn- style
  [kind options theme]
  (merge (get-in theme [:chart kind]) (select-keys options style-keys)))

(defn- pairs
  "Points `{:x [..] :y [..]}` and `:line` of one group's sides (data sides with `:values`)."
  [kind options {:keys [x y]} at]
  (let [line (get options :line (if (= kind :qq) :quartiles :identity))]
    (assoc (if (= kind :qq)
             (probability/quantile-pairs x y (get options :points default-points))
             (probability/probability-pairs x y at))
           :line (case line
                   :quartiles (probability/quartile-line x y)
                   :identity probability/identity-line
                   nil))))

(defn- unit-extent [] {:kind :numeric :min 0.0 :max 1.0})

(defn- point-extents
  [kind xs ys]
  (if (= kind :pp)
    {:x (unit-extent) :y (unit-extent)}
    {:x (data/extent :x xs) :y (data/extent :y ys)}))

(defn- default-label
  [{:keys [x y]}]
  (str (or (:name y) "Theoretical") " vs " (or (:name x) "Theoretical")))

(defn- distribution-stats
  "Stats of a chart whose sides are both distributions: one point set drawn in every panel - the row `{:label l}`
  grouped by the chart's own aesthetics in `mapping`."
  [kind options sides st mapping env]
  (let [at (when (= kind :pp)
             (if-let [d (:domain options)]
               (probability/grid d (get options :points default-points))
               (schema/fail! "P-P plot of two distributions needs :domain [lo hi]." {:key :domain :value nil})))
        g (pairs kind options sides at)
        rows [{:label (or (:label options) (default-label sides))}]
        {:keys [panels dropped]} (group/split {facet/all-panels rows} (select-keys mapping (conj aesthetic/dims :group)) env)
        groups (mapv #(assoc g :aesthetics (:aesthetics %)) (get panels facet/all-panels))]
    {:data {facet/all-panels {:groups groups :style st}}
     :extents (merge-with merge (group/aesthetic-extents panels (:plot-scales env))
                          {facet/all-panels (point-extents kind (:x g) (:y g))})
     :na (group/na-dims panels)
     :dropped dropped}))

(defn- group-sides
  "Sides of one group's `rows`: data sides with their finite `:values` and `:dropped` count."
  [sides rows]
  (update-vals sides (fn [{:keys [accessor] :as side}]
                       (if accessor
                         (merge side (data/finite-numbers (map accessor rows)))
                         side))))

(defn- data-stats
  "Stats of a chart with data sides: per panel and group."
  [kind options sides st {:keys [groups mapping]} {:keys [plot-scales] :as env}]
  (let [{:keys [panels dropped]} (group/split groups mapping env)
        data-dims (filterv #(:accessor (sides %)) [:x :y])
        panel-sides (update-vals panels (fn [gs] (mapv #(group-sides sides (:rows %)) gs)))
        all-sides (mapcat val panel-sides)
        row-count (reduce + 0 (map count (mapcat #(map :rows %) (vals panels))))
        side-dropped (into {} (map (fn [k] [k (reduce + 0 (map #(:dropped (% k)) all-sides))])) data-dims)
        data (into {} (map (fn [[panel gs]]
                             [panel {:groups (vec (keep (fn [[g ss]]
                                                          (when (every? #(seq (:values (ss %))) data-dims)
                                                            (assoc (pairs kind options ss (when (= kind :pp) (probability/pooled-values (vals ss))))
                                                                   :aesthetics (:aesthetics g))))
                                                        (map vector gs (panel-sides panel))))
                                     :style st}]))
                   panels)]
    (doseq [k data-dims]
      (when (and (pos? (long row-count)) (= row-count (side-dropped k)))
        (schema/fail! "No valid values for a mapped dimension - check the mapping." {:key :mapping :value k :rows row-count})))
    {:data data
     :extents (merge-with merge (group/aesthetic-extents panels plot-scales)
                          (into {} (keep (fn [[panel {:keys [groups]}]]
                                           (when (seq groups)
                                             [panel (point-extents kind (mapcat :x groups) (mapcat :y groups))])))
                                data))
     :na (group/na-dims panels)
     :dropped (merge dropped side-dropped)}))

(defn- compute
  [kind {:keys [options]} chart-split {:keys [theme] :as env}]
  (distributions-only! options)
  (let [ss (sides options (:mapping options) (:mapping chart-split))
        st (style kind options theme)
        stats (if (distributions-only? options)
                (distribution-stats kind options ss st (:mapping chart-split) env)
                (data-stats kind options ss st chart-split env))
        titles (into {} (keep (fn [k] (some->> (axis-title kind (ss k)) (vector k)))) [:x :y])]
    (cond-> (assoc stats
                   :padding (merge {:x [0.05 0.05] :y [0.05 0.05]} (:padding options))
                   :padding-explicit (data/explicit-padding {} options))
      (seq titles) (assoc :axis-titles titles))))

(defn qq-compute
  "Per panel (every panel for two distributions): `{:groups [{:x [..] :y [..] :line [slope intercept] :aesthetics}]
  :style st}` - Q-Q points; extents of the points, `:dropped` per data side, `:axis-titles`."
  [chart chart-split env]
  (compute :qq chart chart-split env))

(defn pp-compute
  "Per panel (every panel for two distributions): `{:groups [..] :style st}` as [[qq-compute]] - P-P points; extents
  `[0 1]`."
  [chart chart-split env]
  (compute :pp chart chart-split env))

(defn- linear!
  [k finalized]
  (when-not (= :linear (:kind finalized))
    (schema/fail! "A probability plot :line needs linear x and y scales." {:key :line :value [k (:kind finalized)]})))

(defn draw
  "Draw one panel's groups: the reference line, then the points."
  [_chart {:keys [groups style]} canvas _box scales env]
  (let [projection (scale/projection scales)]
    (doseq [{:keys [x y line aesthetics]} groups
            :let [st (group/style style aesthetics scales (:theme env))]
            :when st]
      (when (and line (polyline/set-style! canvas {:color (:color st) :width (:line-width st) :dash :none}))
        (linear! :x (:x scales))
        (linear! :y (:y scales))
        (rule/draw-ablines! canvas scales [line]))
      (c2d/set-stroke-custom canvas {:size (double (:stroke-width st))})
      (dorun (map (fn [vx vy]
                    (let [^Vec2 p (projection vx vy)]
                      (when-not (or (m/nan? (.x p)) (m/nan? (.y p)))
                        (shape/draw! canvas (:shape st) p st))))
                  x y)))))

(defn legend
  "Key glyph: the chart's point shape fitted and centred in `box` (see [[cljplot2.graphics.shape/draw-key!]])."
  [{:keys [kind options]} attributes canvas box env]
  (let [st (merge (style kind options (:theme env)) attributes)]
    (c2d/set-stroke-custom canvas {:size (double (:stroke-width st))})
    (shape/draw-key! canvas (:shape st) box st)))
