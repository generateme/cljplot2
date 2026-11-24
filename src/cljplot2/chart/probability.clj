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
    (as annotations); Q-Q at `:n` plotting positions, P-P at `:n` values over the required `:domain [lo
    hi]` (`ex-info {:key :domain}` without it); `:n` default 100. `:label` (default \"<y name> vs <x name>\")
    is the chart's one row `{:label label}`: the chart's own `:mapping {:color :label}` (`:shape`, `:size`) styles it
    and gives a legend key, e.g. several distribution pairs in one plot.
    `:n`, `:domain` and `:label` with a data side throw `ex-info` keyed by the option.

  P-P evaluates at the sorted distinct pooled values of the data sides; its extents are `[0 1]` on both axes.

  Reference line `:line` - Q-Q `:quartiles` (default; through the first and third quartile pairs, R `qqline`), P-P
  `:identity` (default; `y = x`), `false` - none. One line per group in the group's colour, between the x domain
  ends; it needs linear x and y scales (`ex-info {:key :line}`) and adds nothing to the extents.

  Axis titles: a distribution side - \"<name> quantiles\" / \"<name> probabilities\" (\"Theoretical ...\" for an
  object); a data side mapped by a keyword - \"<key>\" (Q-Q) / \"<key> probabilities\" (P-P).

  Options (style defaults from theme `[:chart :qq]` / `[:chart :pp]`): point style as [[cljplot2.chart.scatter]]
  (`:size`, `:color`, `:stroke-color`, `:stroke-width`, `:shape`), `:line-width`, `:padding`, plus `:data` /
  `:mapping`.

  Functions: [[distributions-only?]] - whether both sides are distributions; [[qq-compute]], [[pp-compute]], [[draw]] - the chart contract of both kinds (key glyph [[cljplot2.chart.legend/stroked-point-legend]]); [[qq-schemas]], [[pp-schemas]] - their style, options and mapping."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.graphics.curve :as curve]
            [cljplot2.chart.common :as common]
            [cljplot2.data.facet :as facet]
            [cljplot2.data.group :as group]
            [cljplot2.data.probability :as probability]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.graphics.shape :as shape]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^{:const true :private true :tag 'long} default-n
  "Points of a plot of two distributions when `:n` isn't given."
  100)

(def ^:private style-entries
  "Map entries of a probability plot's style (theme `[:chart :qq]` / `[:chart :pp]`): the scatter point style and the reference line width; also its options."
  (conj common/point-style-entries (schema/optional :line-width {:description "reference line width in pixels"} schema/non-neg)))

(defn- distribution-entry
  "Gives the map entry of a distribution side option.

  Parameters:

  - `k` (keyword): the option, `:x-distribution` or `:y-distribution`.
  - `side` (string): the side's name in the description.

  Returns a nilable entry of [[cljplot2.data.probability/distribution-schema]] in section `:distributions`."
  [k side]
  (schema/nilable k {:cljplot2/section :distributions :description (str "the " side " side as a distribution instead of data: a fastmath distribution, [:name] or [:name params]"
                                       " (wins over the plot's mapping of " side ")")}
                  probability/distribution-schema))

(defn- options-schema
  "Gives the options schema of a probability plot.

  Parameters:

  - `line-values` (vector of keywords): the `:line` values besides `false`.
  - `line-props` (map): the documenting properties of `:line`.
  - `extra` (sequence): further map entries of the kind (P-P `:domain`).

  Returns the closed map schema."
  [line-values line-props extra]
  (-> [:map {:closed true}]
      (into (schema/in-section :style style-entries))
      (into data/common-options)
      (into theme/theme-style-entries)
      (conj (distribution-entry :x-distribution ":x")
            (distribution-entry :y-distribution ":y")
            (schema/optional :line (assoc line-props :cljplot2/section :distributions) (into [:enum false] line-values))
            (schema/optional :n {:cljplot2/section :distributions :description "both sides distributions only: the number of points (Q-Q plotting positions, P-P values over :domain)"
                                      :cljplot2/default-doc "100; an error with a data side"}
                             [:int {:min 2}])
            (schema/nilable :label {:cljplot2/section :distributions :description "both sides distributions only: the label of the chart's one row {:label l}, styled by the chart's own mapping of :label (e.g. {:color :label})"
                                    :cljplot2/default-doc "\"<y name> vs <x name>\"; an error with a data side"}
                            [:or :string :keyword])
            (schema/optional :padding {:cljplot2/section :distributions :description "padding of the data span per position axis, {:x [lo hi] :y [lo hi]} fractions of the span"
                                       :cljplot2/default-doc "{:x [0.05 0.05] :y [0.05 0.05]}"}
                             data/position-padding))
      (into extra)))

(def ^:private mapping-schema
  "Mapping schema of both probability plots."
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
                            [(schema/optional :domain {:cljplot2/section :distributions :description "both sides distributions only (then required): [lo hi] of the values the CDFs are evaluated at"}
                                              (schema/with-rules [:tuple schema/finite schema/finite]
                                                [#(when-not (m/< (double (first %)) (double (second %)))
                                                    "should be [lo hi] with lo < hi")]))])
   :mapping mapping-schema})

(def ^:private distribution-keys
  "Side -> its distribution option."
  {:x :x-distribution :y :y-distribution})

(defn distributions-only?
  "Tests whether both sides of a probability plot are distributions.

  Parameters:

  - `options` (map): the chart's options - `:x-distribution`, `:y-distribution`.

  Returns true when both are given."
  [options]
  (and (some? (:x-distribution options)) (some? (:y-distribution options))))

(defn- sides
  "Gives both sides of a probability plot.

  Parameters:

  - `options` (map): the chart's options - `:x-distribution`, `:y-distribution`.
  - `own` (map): the chart's own `:mapping`.
  - `mapping` (map): the merged mapping.

  Returns `{:x side :y side}`, each `{:distribution d :name s}` (a distribution option wins over the merged mapping) or `{:accessor f :key acc}`.

  Throws `ex-info {:key :x-distribution}` (`:y-distribution`) for a side both given as a distribution and in the chart's own mapping, `{:key :mapping}` for a side with neither."
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
                      :else (data/fail-mapping! (str "Probability plot side " k " needs a mapping or " dk ".") k {})))))
        [:x :y]))

(defn- distributions-only!
  "Checks that the options of two distributions aren't given with a data side.

  Parameters:

  - `options` (map): the chart's options.

  Returns `nil`.

  Throws `ex-info {:key k}` for `:n`, `:domain` or `:label` given while a side is data."
  [options]
  (when-not (distributions-only? options)
    (doseq [k [:n :domain :label] :when (some? (get options k))]
      (schema/fail! (str "Probability plot " k " applies only when both sides are distributions.") {:key k :value (get options k)}))))

(defn- axis-title
  "Gives the axis title of a side.

  Parameters:

  - `kind` (keyword): `:qq` or `:pp`.
  - `side` (map): the side - `:distribution` and `:name`, or the mapping `:key`.

  Returns `<name> quantiles` / `<name> probabilities` for a distribution (`Theoretical` for an unnamed one), the key's name (Q-Q) or `<key> probabilities` (P-P) for a keyword key; `nil` otherwise."
  [kind {:keys [distribution] dist-name :name side-key :key}]
  (let [what (if (= kind :qq) "quantiles" "probabilities")]
    (cond
      distribution (str (or dist-name "Theoretical") " " what)
      (keyword? side-key) (if (= kind :qq) (name side-key) (str (name side-key) " " what)))))

(defn- pairs
  "Gives the points and the reference line of one group.

  Parameters:

  - `kind` (keyword): `:qq` or `:pp`.
  - `options` (map): the chart's options - `:line` (Q-Q default `:quartiles`, P-P `:identity`), `:n` (default 100).
  - `group-sides` (map): `:x`, `:y` - data sides with `:values`, or distributions.
  - `at` (sequence of doubles): the P-P evaluation values (`nil` for Q-Q).

  Returns `{:x [..] :y [..] :line l}` (`:line` - `[slope intercept]` or `nil` for none).

  See also [[cljplot2.data.probability/quantile-pairs]], [[cljplot2.data.probability/probability-pairs]]."
  [kind options {:keys [x y]} at]
  (let [line (data/option options :line (if (= kind :qq) :quartiles :identity))]
    (assoc (if (= kind :qq)
             (probability/quantile-pairs x y (data/option options :n default-n))
             (probability/probability-pairs x y at))
           :line (case line
                   :quartiles (probability/quartile-line x y)
                   :identity probability/identity-line
                   nil))))

(defn- unit-extent
  "Gives the numeric extent `[0 1]`."
  []
  {:kind :numeric :min 0.0 :max 1.0})

(defn- point-extents
  "Gives the position extents of a group's points.

  Parameters:

  - `kind` (keyword): `:qq` or `:pp` (always `[0 1]` on both axes).
  - `xs`, `ys` (sequences of numbers): the points' coordinates.

  Returns `{:x extent :y extent}`."
  [kind xs ys]
  (if (= kind :pp)
    {:x (unit-extent) :y (unit-extent)}
    {:x (data/extent :x xs) :y (data/extent :y ys)}))

(defn- default-label
  "Gives the default label of a plot of two distributions: `<y name> vs <x name>` (`Theoretical` for an unnamed one).

  Parameters:

  - `chart-sides` (map): `:x`, `:y` - the distribution sides."
  [{:keys [x y]}]
  (str (or (:name y) "Theoretical") " vs " (or (:name x) "Theoretical")))

(defn- distribution-stats
  "Computes the stats of a probability plot whose sides are both distributions.

  Parameters:

  - `kind` (keyword): `:qq` or `:pp`.
  - `options` (map): the chart's options - `:n`, `:domain` (P-P, required), `:label`.
  - `chart-sides` (map): the two distribution sides.
  - `st` (map): the chart's style.
  - `mapping` (map): the merged mapping - its aesthetics style the one row.
  - `env` (map): the compute environment.

  Returns stats of one point set drawn in every panel: the row `{:label l}` grouped by the chart's own aesthetics.

  Throws `ex-info {:key :domain}` for a P-P plot without `:domain`."
  [kind options chart-sides st mapping env]
  (let [at (when (= kind :pp)
             (if-let [d (:domain options)]
               (probability/grid d (data/option options :n default-n))
               (schema/fail! "P-P plot of two distributions needs :domain [lo hi]." {:key :domain :value nil})))
        g (pairs kind options chart-sides at)
        rows [{:label (or (:label options) (default-label chart-sides))}]
        {:keys [panels dropped]} (group/split {facet/all-panels rows} (select-keys mapping (conj aesthetic/dims :group)) env)
        groups (mapv #(assoc g :aesthetics (:aesthetics %)) (get panels facet/all-panels))]
    {:data {facet/all-panels {:groups groups :style st}}
     :extents (merge-with merge (group/aesthetic-extents panels (:plot-scales env))
                          {facet/all-panels (point-extents kind (:x g) (:y g))})
     :na (group/na-dims panels)
     :dropped dropped}))

(defn- group-sides
  "Gives the sides of one group's rows.

  Parameters:

  - `chart-sides` (map): the chart's sides.
  - `rows` (sequence of maps): the group's rows.

  Returns the sides, data sides with their finite `:values` and the `:dropped` count of the others."
  [chart-sides rows]
  (update-vals chart-sides (fn [{side-accessor :accessor :as side}]
                             (if side-accessor
                               (merge side (data/finite-numbers (map side-accessor rows)))
                               side))))

(defn- data-stats
  "Computes the stats of a probability plot with data sides, per panel and group.

  Parameters:

  - `kind` (keyword): `:qq` or `:pp`.
  - `options` (map): the chart's options.
  - `chart-sides` (map): the chart's sides (see [[sides]]).
  - `st` (map): the chart's style.
  - `chart-split` (map): `:groups` (`{panel rows}`), `:mapping`.
  - `env` (map): the compute environment - `:plot-scales` ...

  Returns per panel `{:groups [..] :style st}` (a group with an empty data side has none), the points' extents and `:dropped` per data side.

  Throws `ex-info {:key :mapping}` when rows exist but a data side has no valid value."
  [kind options chart-sides st {:keys [groups mapping]} {:keys [plot-scales] :as env}]
  (let [{:keys [panels dropped]} (group/split groups mapping env)
        data-dims (filterv #(:accessor (chart-sides %)) [:x :y])
        panel-sides (update-vals panels (fn [gs] (mapv #(group-sides chart-sides (:rows %)) gs)))
        all-sides (mapcat val panel-sides)
        row-count (group/row-count panels)
        side-dropped (into {} (map (fn [k] [k (reduce m/long-add 0 (map #(:dropped (% k)) all-sides))])) data-dims)
        data (into {} (map (fn [[panel gs]]
                             [panel {:groups (vec (keep (fn [[g ss]]
                                                          (when (every? #(seq (:values (ss %))) data-dims)
                                                            (assoc (pairs kind options ss (when (= kind :pp) (probability/pooled-values (vals ss))))
                                                                   :aesthetics (:aesthetics g))))
                                                        (map vector gs (panel-sides panel))))
                                     :style st}]))
                   panels)]
    (doseq [k data-dims]
      (when (and (m/pos? row-count) (m/== row-count (long (side-dropped k))))
        (data/fail-mapping! data/no-valid-values k {:rows row-count})))
    {:data data
     :extents (merge-with merge (group/aesthetic-extents panels plot-scales)
                          (into {} (keep (fn [[panel {panel-groups :groups}]]
                                           (when (seq panel-groups)
                                             [panel (point-extents kind (mapcat :x panel-groups) (mapcat :y panel-groups))])))
                                data))
     :na (group/na-dims panels)
     :dropped (merge dropped side-dropped)}))

(defn- compute
  "Computes a probability plot's stats.

  Parameters:

  - `kind` (keyword): `:qq` or `:pp`.
  - `chart` (map): the chart value - `:options`.
  - `chart-split` (map): the chart's rows split per panel, with the merged `:mapping`.
  - `env` (map): the compute environment - `:theme` ...

  Returns the stats of [[distribution-stats]] or [[data-stats]] with the padding (default 5% each side) and the axis titles of the sides.

  Throws as [[distributions-only!]] and [[sides]]."
  [kind {:keys [options] :as chart} chart-split {:keys [theme] :as env}]
  (distributions-only! options)
  (let [ss (sides options (:mapping options) (:mapping chart-split))
        st (theme/resolved-style theme chart)
        stats (if (distributions-only? options)
                (distribution-stats kind options ss st (:mapping chart-split) env)
                (data-stats kind options ss st chart-split env))
        titles (into {} (keep (fn [k] (some->> (axis-title kind (ss k)) (vector k)))) [:x :y])]
    (cond-> (merge stats (data/padding-stats data/default-padding {} options))
      (seq titles) (assoc :axis-titles titles))))

(defn qq-compute
  "Computes the Q-Q chart's stats.

  Parameters:

  - `chart` (map): the chart value.
  - `chart-split` (map): the chart's rows split per panel.
  - `env` (map): the compute environment.

  Returns per panel (every panel for two distributions) `{:groups [{:x [..] :y [..] :line [slope intercept] :aesthetics}] :style st}` - quantile pairs; the points' extents, `:dropped` per data side, `:axis-titles`.

  See also [[pp-compute]]."
  [chart chart-split env]
  (compute :qq chart chart-split env))

(defn pp-compute
  "Computes the P-P chart's stats.

  Parameters:

  - `chart` (map): the chart value.
  - `chart-split` (map): the chart's rows split per panel.
  - `env` (map): the compute environment.

  Returns stats as [[qq-compute]] with probability pairs; extents `[0 1]` on both axes."
  [chart chart-split env]
  (compute :pp chart chart-split env))

(defn- linear!
  "Checks that a reference line's axis has a linear scale.

  Parameters:

  - `k` (keyword): the axis, `:x` or `:y`.
  - `finalized` (map): its finalized scale.

  Returns `nil`.

  Throws `ex-info {:key :line}` for another scale kind."
  [k finalized]
  (when-not (= :linear (:kind finalized))
    (schema/fail! "A probability plot :line needs linear x and y scales." {:key :line :value [k (:kind finalized)]})))

(defn draw
  "Draws one panel of a probability plot: per group the reference line, then the points.

  Parameters:

  - `_chart` (map): the chart value (unused).
  - `computed` (map): the panel's stats - `:groups`, `:style`.
  - `canvas`: the clojure2d canvas.
  - `_box` (map): the panel box (unused).
  - `scales` (map): finalized scales.
  - `env` (map): the draw environment - `:theme`.

  A group outside its scale's domain is skipped, as are points the scales can't place.

  Throws as [[linear!]] for a reference line over a non-linear scale."
  [_chart {:keys [groups style]} canvas _box scales env]
  (let [projection (scale/projection scales)]
    (doseq [{:keys [x y line aesthetics]} groups
            :let [st (group/style style aesthetics scales (:theme env))]
            :when st]
      (when (and line (polyline/set-style! canvas {:color (:color st) :line-width (:line-width st) :dash :none}))
        (linear! :x (:x scales))
        (linear! :y (:y scales))
        (curve/draw-ablines! canvas scales [line]))
      (c2d/set-stroke-custom canvas {:size (:stroke-width st)})
      (dorun (map (fn [vx vy]
                    (let [^Vec2 p (projection vx vy)]
                      (when (data/finite-point? p)
                        (shape/draw! canvas (:shape st) p st))))
                  x y)))))

