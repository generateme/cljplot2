(ns cljplot2.pipeline
  "Plot pipeline: a plot map passes through fixed stages, each a pure function adding one key.

  Stages, in order: `:split` -> `:stats` -> `:extents` -> `:scale-specs` -> `:grid` -> `:layout` -> `:finalized-scales`
  -> `:subplots`.

  Plot keys (schema: [[plot-schema]]; `build` validates them before any stage runs, see [[cljplot2.schema]]):

  * `:labels` - plot titles, see [[cljplot2.layout.plot-grid]]
  * `:scales` - `{:x spec :y spec}`, position scales, see [[cljplot2.scale.position/data-scale-spec]]; facet scales, see
    [[cljplot2.data.facet]]; aesthetic scales `:color` `:shape` `:size` `:dash` `:pattern`, see [[cljplot2.scale.aesthetic]]
  * `:data`, `:mapping` - plot data and mapping, see [[cljplot2.data.rows]]
  * `:charts` - vector of chart values, see [[cljplot2.chart]]
  * `:facets` - `{:columns :rows :order :na-panel}` facet layout options, see [[cljplot2.data.facet]]
  * `:groups` - `{:na bool}`: `nil` aesthetic values form the NA level (default `true`) or are dropped
  * `:labels` - plot titles; charts may suggest axis titles (`:axis-titles` in their stats), `:labels` win
  * `:legend` - `false` hides all legends, see [[cljplot2.content.legend]]
  * `:aspect` - `:iso`: equal pixels per data unit on x and y (linear scales, generated grid); the panel aspect ratio
    changes, the panel block is centred in the leftover space
  * `:flip` - `true` draws the `:x` scale vertically and `:y` horizontally (generated grid only). Scales, extents,
    labels, tick formats and theme `[:axis k]` keep their keys; only the finalized scales' `:direction` changes, and
    axes, grid lines, axis titles and charts follow it (charts draw through
    [[cljplot2.scale.position/projection]])
  * `:coord` - `{:kind :polar ...}`: polar coordinates - the horizontal scale maps to angles, the vertical one to radii
    (`:flip` swaps them; see [[cljplot2.scale.coord]]); polar guides inside the panel, no outer axes; only charts
    declaring `:polar` in their `:coords` (bar, segment, scatter, line, text, rule, rect, parallel, partition); not with `:aspect :iso`,
    `:margins`, `:custom-grid` or `:arrange` (sub-plots may be polar)
  * `:custom-grid` - a hand-written grid (see [[cljplot2.layout.grid]]) used instead of the generated one; only
    `:labels {:title t}` may accompany it (a title row above it). Items with `{:kind :plot :plot p}` content hold
    whole plots (see [[cljplot2.content.subplot]])
  * `:arrange` - `{:plots [p ...] :columns :rows :widths :heights}`: a multiplot, its grid generated from the merged
    theme (see [[cljplot2.multiplot/arrange]], [[cljplot2.layout.plot-grid/arrange-grid]]); only `:labels {:title t}` may
    accompany it; can't be combined with `:custom-grid`; the sub-plots' panels are aligned unless `:align false`
  * `:margins` - `{:top m :right m}`: charts beside the panel sharing its horizontal (top) / vertical (right) scale,
    `m` a chart, a vector of charts or `{:charts c :size px :axis bool}` (thickness, default 80; `:axis` - the
    magnitude axis of histogram / density margins); margin charts read the plot's data and aesthetic mapping with
    `:x` mapped to the shared dimension (their own `:mapping` wins), no axes of their own; single-panel generated
    grids only (not with facets, `:arrange` or `:custom-grid` - put margins on the sub-plots), see [[margin-stats]]
  * `:alignment-padding` - internal: `{:left :top :right :bottom}` pixels added to the generated grid's margins
    (set on sub-plots by multiplot alignment)
  * `:options` - render options (see [[default-options]]), merged under `build`'s options; stored resolved
  * `:theme` - deep-merged over [[cljplot2.theme/default-theme]]; stored resolved

  Facet mapping keys (`:facet`, `:facet-col`, `:facet-row`) split rows into panels, see [[cljplot2.data.facet]].
  Aesthetic mapping keys (`:color`, `:shape`, `:size`, `:dash`, `:pattern`) style marks, see [[cljplot2.scale.aesthetic]]; `:group` splits
  rows into groups for charts drawing one mark per group, see [[cljplot2.data.group]]."
  (:require [fastmath.core :as m]
            [cljplot2.chart :as chart]
            [cljplot2.schema :as schema]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.data.rows :as data]
            [cljplot2.data.accessor :as accessor]
            [cljplot2.data.facet :as facet]
            [cljplot2.layout.align :as align]
            [cljplot2.layout.grid :as grid]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.layout.plot-grid :as plot-grid]
            [cljplot2.scale.coord :as coord]
            [cljplot2.scale.position :as scale]
            [cljplot2.theme :as theme]
            ;; built-in content kinds (natural-size / draw-content methods)
            [cljplot2.content.label]
            [cljplot2.content.panel]
            [cljplot2.content.axis]
            [cljplot2.content.strip]
            [cljplot2.content.legend]
            [cljplot2.content.subplot :as subplot]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def default-options
  "`:width`, `:height` - target size in pixels; `:hint` - rendering hint, see [[cljplot2.graphics.measure/rendering-hints]]."
  {:width 600 :height 600 :hint :high})

(def ^:const max-layout-rounds
  "Upper bound on measure -> solve -> finalize-scales rounds."
  3)

;; stages

(def ^:private position-scales #{:x :y})

(defn- without-facet-values?
  "Does a chart with its own data (`own-data?`) have no value (all `nil`) for every facet key of its `mapping`? Such
  a chart (e.g. an annotation's summary rows) is drawn in every panel, as one without facet keys."
  [{:keys [rows mapping own-data?]}]
  (and own-data?
       (not-any? (fn [d] (let [acc (data/row-accessor d (mapping d))] (some #(some? (acc %)) rows)))
                 (facet/facet-dims mapping))))

(defn- facet-split
  "Facet dimensions, levels and per-chart groups for charts `chart-inputs` (`{:rows :mapping :own-data?}`); a chart
  without facet keys, or whose own data has no facet values, is drawn in every panel."
  [chart-inputs scales na-panel?]
  (let [in-every-panel? (fn [input] (or (empty? (facet/facet-dims (:mapping input))) (without-facet-values? input)))
        faceted (remove in-every-panel? chart-inputs)
        dims (vec (first (distinct (map (comp facet/facet-dims :mapping) faceted))))
        accessors (fn [mapping] (into {} (map (fn [d] [d (data/row-accessor d (mapping d))])) dims))
        levels (into {} (map (fn [d]
                               (let [values (mapcat (fn [{:keys [rows mapping]}] (map (get (accessors mapping) d) rows)) faceted)]
                                 [d (facet/levels d values (get scales d) na-panel?)])))
                     dims)
        active? (and (seq dims) (every? seq (vals levels)))]
    {:facets {:dims (if active? dims []) :levels (if active? levels {})}
     :charts (mapv (fn [{:keys [rows mapping] :as input}]
                     (cond
                       (not active?) {:mapping mapping :groups {facet/single-panel rows} :dropped 0}
                       (in-every-panel? input) {:mapping mapping :groups {facet/all-panels rows} :dropped 0}
                       :else (let [{:keys [groups dropped]} (facet/group-rows rows dims (accessors mapping) levels)]
                               {:mapping mapping :groups groups :dropped dropped})))
                   chart-inputs)}))

(defn- chart-mapping
  "Mapping of `chart`: the plot `mapping` - only its facet keys for a chart with `:inherit-mapping :facets`
  (annotations) - with the chart's own `:mapping` merged over."
  [mapping {:keys [options inherit-mapping]}]
  (merge (if (= :facets inherit-mapping) (select-keys mapping [:facet :facet-col :facet-row]) mapping)
         (:mapping options)))

(defn- split-stage
  "Rows of every chart (own non-nil `:data` or the plot's) grouped by facet key, with the chart's mapping (see
  [[chart-mapping]]). Stores `{:facets {:dims :levels :dropped {:facet n}}
  :charts [{:mapping :groups} ...]}`; see [[cljplot2.data.facet]]."
  [{:keys [charts data mapping scales facets] :as plot}]
  (let [plot-rows (delay (data/rows data))
        inputs (mapv (fn [{:keys [options] :as chart}]
                       {:mapping (chart-mapping mapping chart)
                        :rows (if (some? (:data options)) (data/rows (:data options)) @plot-rows)
                        :own-data? (some? (:data options))})
                     charts)
        {split-facets :facets chart-splits :charts} (facet-split inputs scales (get facets :na-panel true))]
    (assoc plot :split {:facets (assoc split-facets :dropped {:facet (reduce + (map :dropped chart-splits))})
                        :charts (mapv #(dissoc % :dropped) chart-splits)})))

;; margins

(def ^:private ^:const provisional-tick-hint 10)

(def ^:private margin-sides
  "Plot margin sides, in grid order."
  [:top :right])

(def ^:private magnitude-chart-kinds
  "Chart kinds with a magnitude dimension (counts, densities) a margin axis can show."
  #{:histogram :density})

(def ^:private ^:const default-margin-size 80)

(defn- chart-value? [v] (and (map? v) (contains? v :compute)))

(defn- margin-spec
  "Margin `m` of plot `:margins` normalized: `{:charts [chart ...] :size px :axis bool}` (a chart, a vector of charts
  or a spec map; default size 80, no axis)."
  [m]
  (merge {:size default-margin-size :axis false}
         (cond
           (chart-value? m) {:charts [m]}
           (map? m) (update m :charts #(if (chart-value? %) [%] (vec %)))
           :else {:charts (vec m)})))

(defn plot-margins
  "The plot's `:margins` normalized ([[margin-spec]]): `{side spec}` in [[margin-sides]] order."
  [plot]
  (into {} (keep (fn [side] (when-some [m (get-in plot [:margins side])] [side (margin-spec m)]))) margin-sides))

(defn- groupable-aesthetics
  "Aesthetic keys of plot `mapping` whose scale groups rows - ordinal or identity, resolved as the plot scale will be
  from `plot-scales` and the values in `rows`; a linear or binned aesthetic describes single rows (margin charts
  don't inherit it)."
  [mapping plot-scales rows]
  (filterv (fn [k]
             (when-let [acc (get mapping k)]
               (let [present (keep (data/row-accessor k acc) rows)
                     numeric? (and (seq present) (every? number? present))]
                 (#{:ordinal :identity} (aesthetic/inferred-kind k (get plot-scales k {}) numeric?)))))
           aesthetic/dims))

(defn- margin-stats
  "Computed margin charts per side: `{side (spec + :stats [..])}`. A margin chart reads the plot's data (or its own)
  with the plot's grouping aesthetics (see [[groupable-aesthetics]]) and `:group` mapping, `:x` mapped to the plot's accessor of the shared dimension - the
  horizontal one for `:top`, the vertical one for `:right` - its own `:mapping` merged over (annotation charts:
  their own mapping only); the mappings are kept under `:mappings`; one panel; the right
  margin computes as flipped (its values run vertically). Throws `ex-info {:key :margins}` when the plot doesn't map
  the shared dimension."
  [{:keys [data mapping flip] :as plot} env]
  (let [horizontal (scale/horizontal-key flip)
        vertical (if (= horizontal :x) :y :x)
        plot-rows (delay (data/rows data))]
    (into {} (for [[side {:keys [charts] :as spec}] (plot-margins plot)]
               (let [shared-key (if (= side :top) horizontal vertical)
                     shared (get mapping shared-key)
                     _ (when (nil? shared)
                         (schema/fail! (str "Plot :margins " side " needs the plot :mapping of " shared-key ".")
                                       {:key :margins :in [:margins side] :value shared-key}))
                     inherited (assoc (select-keys mapping (conj (groupable-aesthetics mapping (:plot-scales env) @plot-rows) :group))
                                      :x shared)
                     side-env (assoc env :flip? (= side :right))
                     ;; annotation charts (:inherit-mapping :facets) take only their own mapping, as in the panel
                     mappings (mapv (fn [{:keys [options inherit-mapping]}]
                                      (merge (if (= :facets inherit-mapping) {} inherited) (:mapping options)))
                                    charts)]
                 [side (assoc spec
                              :mappings mappings
                              :stats (mapv (fn [{:keys [options] :as chart} chart-mapping]
                                             (let [rows (if (some? (:data options)) (data/rows (:data options)) @plot-rows)]
                                               ((:compute chart) chart {:mapping chart-mapping :groups {facet/single-panel rows}}
                                                side-env)))
                                           charts mappings))])))))

(defn- margin-env
  "Per margin side `{:charts :stats :scales}` for drawing and measuring with `layout` (`nil` - provisional) and the
  main finalized `scales`: the plot's aesthetic scales, `:x` - the main scale of the shared dimension (its direction
  kept), `:y` - the margin's
  magnitude scale (counts, densities) finalized for the margin panel's size, across the shared one (`nil` when the
  margin charts give none, e.g. strips only)."
  [plot layout scales]
  (let [horizontal (scale/horizontal-key (:flip plot))
        vertical (if (= horizontal :x) :y :x)]
    (into {} (for [[side {:keys [charts stats spec]}] (:margin-stats plot)]
               (let [top? (= side :top)
                     shared (get scales (if top? horizontal vertical))
                     box (when layout (grid/find-content-box (:grid plot) layout #(= side (:margin %))))
                     spacing (double (get-in plot [:theme :axis :y :tick-spacing]))
                     magnitude (when spec
                                 (if-not box
                                   (scale/finalize spec [0.0 1.0] provisional-tick-hint)
                                   (let [w (long (:w box)) h (long (:h box))]
                                     (if top?
                                       (scale/finalize spec [(double h) 0.0] (scale/tick-hint h spacing))
                                       (scale/finalize spec [0.0 (double w)] (scale/tick-hint w spacing))))))]
                 [side {:charts charts
                        :stats stats
                        ;; aesthetic scales shared with the plot: margins colour their groups as the panel does
                        :scales (cond-> (into {} (filter (comp aesthetic/dim? key)) scales)
                                  shared (assoc :x shared)
                                  magnitude (assoc :y (assoc magnitude :direction (if top? :vertical :horizontal))))}])))))

(defn- stats-stage
  "Per chart: the data its `:compute` returns from its split groups; margin charts too (`:margin-stats`, see
  [[margin-stats]])."
  [{:keys [charts split theme groups scales flip facets options aspect] :as plot}]
  (let [env {:theme theme :na-level? (get groups :na true) :plot-scales scales :flip? (boolean flip)
             :hint (:hint options) :coord (:coord plot)
             :size [(:width options) (:height options)] :aspect aspect
             :panel-layout (plot-grid/panel-layout (:facets split) scales facets)}]
    (assoc plot
           :stats (mapv (fn [chart chart-split] ((:compute chart) chart chart-split env)) charts (:charts split))
           :margin-stats (margin-stats plot env))))

(defn- aesthetic-extents
  "Per aesthetic with data: extent merged over charts and panels, `:na?` - some chart has NA values of it."
  [stats]
  (into {} (keep (fn [k]
                   (let [extent (data/merge-extents k (mapcat #(keep (fn [e] (get e k)) (vals (:extents %))) stats))
                         na? (boolean (some #(contains? (:na %) k) stats))]
                     (when (or extent na?) [k {:extent extent :na? na?}]))))
        aesthetic/dims))

(defn- per-side-max
  [[a b] [c d]]
  [(m/max (double a) (double c)) (m/max (double b) (double d))])

(defn- reaches-end?
  "Does the chart extent `chart-extent` reach the merged `extent`'s end on side `i` (0 - low, 1 - high)? Categorical
  extents always do (no ends to compare)."
  [extent chart-extent i]
  (let [bound (if (zero? (long i)) :min :max)]
    (or (= :categorical (:kind extent))
        (zero? (compare (bound chart-extent) (bound extent))))))

(defn- merged-padding
  "Padding `[lo hi]` of the charts `with-extent` (`:extent`, `:padding`, `:explicit` - requested sides or `nil`) over
  their merged `extent`: per side the largest request of the charts whose extent reaches that end (see
  [[cljplot2.data.rows/explicit-padding]] - a zero at a baseline wins over other charts' defaults, room below the data
  over a zero; another chart's data beyond the baseline voids it), else the largest padding."
  [extent with-extent]
  (mapv (fn [i]
          (let [requests (keep #(when (reaches-end? extent (:extent %) i) (some-> (:explicit %) (nth i))) with-extent)]
            (double (reduce max 0.0 (if (seq requests) requests (map #(nth (:padding %) i) with-extent))))))
        [0 1]))

(defn- position-extent
  "Position dimension `k` over charts' `stats`: `{:extent :padding :padding-px}` - extents merged over charts and
  panels, padding per side from the charts with an extent there (see [[merged-padding]]), pixel room the largest per
  side; `nil` without one. A chart whose extents of `k` are all companions (`:companion? true`) joins only when
  another chart has extents of it."
  [stats k]
  (let [with-extent (keep (fn [st]
                            (when-let [es (seq (keep #(get % k) (vals (:extents st))))]
                              {:extent (data/merge-extents k es) :padding (get-in st [:padding k] [0 0])
                               :explicit (get-in st [:padding-explicit k])
                               :padding-px (get-in st [:padding-px k] [0 0])
                               :companion? (every? :companion? es)}))
                          stats)]
    (when (some (complement :companion?) with-extent)
      (let [extent (data/merge-extents k (map :extent with-extent))]
        {:extent extent
         :padding (merged-padding extent with-extent)
         :padding-px (reduce per-side-max [0.0 0.0] (map :padding-px with-extent))}))))

(defn- extents-stage
  "Per position scale: extents merged over charts and panels, padding per side from the charts with an extent there
  (the largest request, else the largest padding - see [[merged-padding]]) and pixel room `:padding-px` (the largest
  per side). A chart whose extents of a dimension are all companions (`:companion? true`) joins only a
  dimension another chart has extents of; companions alone give no extent (and no data scale). Per aesthetic: merged
  extent and `:na?`."
  [{:keys [stats] :as plot}]
  ;; aesthetic extents also over margin charts: margins share (and may extend) the plot's aesthetic scales
  (assoc plot :extents
         (into (aesthetic-extents (into (vec stats) (mapcat :stats (vals (:margin-stats plot)))))
               (keep (fn [k] (some->> (position-extent stats k) (vector k))))
               position-scales)))

(defn- position-spec
  "Phase-1 spec of position dimension `k` from the plot's `spec` (may be `{}`), the merged `{:extent :padding
  :padding-px}` and the charts' `stats`: chart suggestions - `:scale-defaults` (the first chart's, used as a whole
  unless `spec` sets any of them), `:tick-format` (the first chart's) and the pixel room `:padding-px` - applied."
  [stats k spec {:keys [extent padding padding-px]}]
  (let [defaults (some #(get-in % [:scale-defaults k]) stats)
        spec (if (some #(contains? spec %) (keys defaults)) spec (merge defaults spec))
        tick-format (some #(get-in % [:tick-format k]) stats)]
    (cond-> (scale/data-scale-spec k spec extent padding)
      tick-format (assoc :tick-format tick-format)
      (some pos? padding-px) (assoc :padding-px padding-px))))

(defn- scale-specs-stage
  "Phase-1 specs of the position and aesthetic scales with a plot `:scales` entry or data extents. Aesthetic specs are final (no layout dependence).
  A tick format suggested by a chart (`:tick-format` in its stats, the first chart's) is added to the position spec;
  pixel room asked by charts (`:padding-px`) is added to continuous position specs (see
  [[cljplot2.scale.position/finalize]]); scale attributes suggested by a chart (`:scale-defaults {dim attributes}`, the first chart's) are used as a whole
  unless the plot's own spec sets any of them (attributes like bands `:padding-out` / `:align-outer` only make
  sense together); a legend title suggested by a chart (`:legend-titles {dim title}`, the first chart's) is the aesthetic
  scale's `:title` unless the plot's spec has one."
  [{:keys [scales extents theme stats] :as plot}]
  (let [ks (distinct (concat (filter #(or (position-scales %) (aesthetic/dim? %)) (keys scales)) (keys extents)))
        suggested-titles (reduce (fn [acc titles] (merge titles acc)) {} (keep :legend-titles stats))
        aesthetic-spec (fn [k] (let [spec (get scales k {})]
                                 (if (and (not (contains? spec :title)) (contains? suggested-titles k))
                                   (assoc spec :title (suggested-titles k))
                                   spec)))]
    (assoc plot
           ;; a margin's magnitude scale: built as a position spec from the margin charts' stats
           :margin-stats (update-vals (:margin-stats plot)
                                      (fn [{:keys [stats] :as m}]
                                        (if-let [e (position-extent stats :y)]
                                          (assoc m :spec (position-spec stats :y {} e))
                                          m)))
           :scale-specs
           (into {} (keep (fn [k]
                            (let [{:keys [extent na?] :as e} (get extents k)]
                              (if (aesthetic/dim? k)
                                (some->> (aesthetic/scale-spec k (aesthetic-spec k) extent na? (:palette theme)) (vector k))
                                [k (position-spec stats k (get scales k {}) e)]))))
                 ks))))

(defn- effective-labels
  "Plot `labels` over the axis titles suggested by the charts in `stats` (per axis, the first suggestion wins);
  a `:labels` entry - also `nil` - wins over a suggestion. Non-map `labels` are passed through (rejected later)."
  [labels stats]
  (let [suggested (reduce (fn [acc titles] (merge (select-keys titles [:x :y]) acc)) {} (keep :axis-titles stats))]
    (cond
      (nil? labels) (not-empty suggested)
      (map? labels) (merge suggested labels)
      :else labels)))

(defn- grid-stage
  "The plot's grid: `:custom-grid` when given (under a title row when `:labels` has `:title`; other labels throw),
  otherwise generated from plot keys (`:labels` over the charts' suggested axis titles)."
  [{:keys [custom-grid labels stats theme arrange aspect scale-specs] :as plot}]
  (when (and (= :iso aspect)
             (or (not= :linear (get-in scale-specs [:x :kind])) (not= :linear (get-in scale-specs [:y :kind]))))
    (schema/fail! "Plot :aspect :iso needs linear :x and :y scales." {:key :aspect :in [:aspect] :value aspect}))
  (assoc plot :grid (cond
                      arrange (plot-grid/arrange-grid arrange labels theme)
                      (and custom-grid labels) (plot-grid/titled-custom-grid custom-grid labels theme)
                      custom-grid custom-grid
                      :else (plot-grid/plot-grid (cond-> (assoc plot :labels (effective-labels labels stats))
                                                   ;; polar guides are drawn inside the panel: no outer axes
                                                   (coord/polar? (:coord plot))
                                                   (update :scale-specs (fn [specs] (reduce (fn [acc k] (if (contains? acc k) (assoc-in acc [k :axis] false) acc))
                                                                                           specs [:x :y]))))))))

(defn- panel-content?
  "Is `content` a main panel (`:panel` content that isn't a plot margin's)?"
  [content]
  (and (= :panel (:kind content)) (nil? (:margin content))))

(declare cartesian-scales polar-scales)

(defn finalize-scales
  "Scales of `:scale-specs` finalized in panel-local coordinates for the reference panel of `layout` - the first
  `:panel` content of the grid (depth-first): the horizontal scale (`:x`, or `:y` when the plot has `:flip true`)
  range `[0 w]`, the vertical one `[h 0]`, each with `:direction` (`:horizontal` / `:vertical`); tick hint from the
  panel size along it and theme `[:axis k :tick-spacing]` (by scale key). All panels share these scales (they differ by at most 1 px in size).
  Before the first layout round (`layout` is `nil`) a provisional finalization is used: range `[0 1]`, 10 ticks.
  Aesthetic scales are passed through unchanged (no pixel range). A polar plot (`:coord`) gets range-space scales
  instead, see [[polar-scales]].
  Throws `ex-info` with `{:key :scales}` when the grid has no `:panel` content."
  [plot layout]
  (let [specs (into {} (filter (comp position-scales key)) (:scale-specs plot))
        horizontal (scale/horizontal-key (:flip plot))
        panel (when (and layout (seq specs))
                (or (grid/find-content-box (:grid plot) layout panel-content?)
                    (schema/fail! "Plot :scales need a :panel item in the grid." {:key :scales :in [:scales]})))]
    (if (and panel (coord/polar? (:coord plot)))
      (into (into {} (filter (comp aesthetic/dim? key)) (:scale-specs plot)) (polar-scales plot specs horizontal panel))
      (cartesian-scales plot specs horizontal panel))))

(defn- cartesian-scales
  "Finalized scales of [[finalize-scales]] in cartesian coordinates (and provisional polar ones, `panel` `nil`)."
  [plot specs horizontal panel]
  (into (into {} (filter (comp aesthetic/dim? key)) (:scale-specs plot))
        (map (fn [[k spec]]
               (let [horizontal? (= k horizontal)]
                 [k (assoc (if-not panel
                             (scale/finalize spec [0.0 1.0] provisional-tick-hint)
                             (let [{:keys [w h]} panel
                                   w (long w) h (long h)
                                   spacing (double (get-in plot [:theme :axis k :tick-spacing]))]
                               (if horizontal?
                                 (scale/finalize spec [0.0 (double w)] (scale/tick-hint w spacing))
                                 (scale/finalize spec [(double h) 0.0] (scale/tick-hint h spacing)))))
                           :direction (if horizontal? :horizontal :vertical))])))
        specs))

(defn- theta-label-room
  "Pixels kept around the circle of a polar panel for the angular tick labels of finalized `theta` (scale `k`): the
  largest extent of a label away from the centre at its angle (`|sin a|·width + |cos a|·height` - labels sit
  outside the circle, anchored away from it) plus the theme's tick length and label gap; 0 without labels or with
  `:axis false`."
  [plot k theta]
  (let [labels (when (and theta (not (false? (:axis theta)))) (seq (map vector (:ticks theta) (:tick-labels theta))))]
    (if-not labels
      0.0
      (let [forward (scale/forward-fn theta)]
        (measure/call-with-scratch-canvas
         (get-in plot [:options :hint])
         (fn [c]
           (+ (double (reduce max 0.0 (map (fn [[tick t]]
                                             (let [^fastmath.vector.Vec2 size (measure/natural-size {:kind :label :role :tick-label :text t}
                                                                                                    {:canvas c :theme (:theme plot)})
                                                   a (double (forward tick))]
                                               (if (m/valid-double? a)
                                                 (+ (* (m/abs (m/sin a)) (.x size)) (* (m/abs (m/cos a)) (.y size)))
                                                 0.0)))
                                           labels)))
              (double (get-in plot [:theme :axis k :label-gap]))
              (double (get-in plot [:theme :axis k :tick-length])))))))))

(defn- polar-scales
  "Finalized position scales of a polar plot for its reference `panel` box (see [[cljplot2.scale.coord]]): the
  horizontal scale (`horizontal` key) over the angular range (radians, ticks hinted from the circumference; a
  continuous one neither padded nor niced unless its spec sets `:expand` (the plot's scale, or a chart's
  `:scale-defaults`) / the plot's scale sets `:nice`; pixel room `:padding-px`
  in radians at the outer radius; ticks hinted at the final radius), the
  vertical one over `[inner radius, radius]` - the largest radius fitting the sector's bounding box (see
  [[cljplot2.scale.coord/sector-box]]; a full circle - half the panel's smaller side) into the panel less the angular
  labels' room on every side and the charts' `:outer-room` (`[horizontal vertical]` pixels - labels outside the
  circle, the largest over the charts); both carry `:direction` and the panel's polar geometry `:coord`."
  [plot specs horizontal {:keys [w h]}]
  (let [w (double w) h (double h)
        coord-spec (:coord plot)
        vertical (if (= horizontal :x) :y :x)
        spacing (fn [k] (double (get-in plot [:theme :axis k :tick-spacing])))
        [t0 t1 :as theta-range] (coord/theta-range coord-spec)
        [bx0 by0 bx1 by1] (coord/sector-box coord-spec)
        ;; charts' labels outside the circle (`:outer-room [horizontal vertical]` pixels, the largest)
        outer-rooms (keep :outer-room (:stats plot))
        outer-h (double (reduce max 0.0 (map first outer-rooms)))
        outer-v (double (reduce max 0.0 (map second outer-rooms)))
        ;; the radius fitting the sector's box into the panel less `room` (plus the charts' outer room) on every side
        fit-radius (fn ^double [^double room]
                     (m/min (- (/ w (- (double bx1) (double bx0))) (/ (* 2.0 (+ room outer-h)) (- (double bx1) (double bx0))))
                            (- (/ h (- (double by1) (double by0))) (/ (* 2.0 (+ room outer-v)) (- (double by1) (double by0))))))
        ;; a continuous angle spans the full circle by default: no padding, no nice (pies), unless asked - `:expand` in
        ;; the spec (the plot's scale or a chart's suggestion), `:nice` in the plot's scale (specs always carry one)
        user (get-in plot [:scales horizontal])
        theta-span (m/abs (- (double t1) (double t0)))
        ;; pixel room (labels past the data) becomes radians at radius `r`
        theta-spec (when-let [spec (get specs horizontal)]
                     (cond-> spec
                       (not (contains? spec :expand)) (dissoc :padding)
                       (not (contains? user :nice)) (assoc :nice false)))
        theta-at (fn [^double r]
                   (when theta-spec
                     (scale/finalize (cond-> theta-spec
                                       (:padding-px theta-spec) (update :padding-px (fn [p] (mapv #(/ (double %) r) p))))
                                     theta-range (scale/tick-hint (* theta-span r) (spacing horizontal)))))
        ;; the label room from a first pass at the whole panel; ticks and room again at the radius it leaves
        first-pass (theta-at (double (fit-radius 0.0)))
        radius (m/max 1.0 (double (fit-radius (double (theta-label-room plot horizontal first-pass)))))
        theta (theta-at radius)
        geometry (coord/geometry coord-spec w h radius)
        inner (double (:inner-radius geometry))
        r (when-let [spec (get specs vertical)]
            (scale/finalize spec [inner radius] (scale/tick-hint (- radius inner) (spacing vertical))))]
    (cond-> {}
      theta (assoc horizontal (assoc theta :direction :horizontal :coord geometry))
      r (assoc vertical (assoc r :direction :vertical :coord geometry)))))

(defn- solve-layout
  "Layout of `plot`'s `:grid`: repeat measure -> solve -> finalize scales until item measurements stop changing, at
  most [[max-layout-rounds]] times (the number of rounds under `:rounds`)."
  [plot canvas]
  (let [{:keys [width height]} (:options plot)
        prepared (grid/prepare (:grid plot))
        box {:x 0 :y 0 :w width :h height}
        measure-for (fn [layout]
                      (grid/measure-items prepared
                                          (let [scales (finalize-scales plot layout)]
                                            {:canvas canvas :theme (:theme plot) :scales scales
                                             :margins (margin-env plot layout scales)})
                                          layout))]
    (loop [round 1
           measurements (measure-for nil)]
      (let [layout (grid/solve prepared box measurements)
            remeasured (measure-for layout)]
        (if (or (= measurements remeasured) (>= round max-layout-rounds))
          (assoc layout :rounds round)
          (recur (inc round) remeasured))))))

(def ^:const max-iso-rounds
  "Upper bound on refits of iso panel sizes (nice domains and measured tracks may change with the panel size)."
  6)

(defn- span ^double [finalized] (let [[a b] (:domain finalized)] (m/abs (- (double b) (double a)))))

(defn- iso-spacers
  "Whole-pixel spacer totals `[column row]` (both sides of an axis together) giving the panels of `layout` (grid `g`,
  current totals `spacers`) equal pixels per unit for `scales`: the panel area available without spacers is fitted by
  the largest panels with `w / x-span = h / y-span`; the rest goes to the spacers."
  [g layout scales [column-total row-total]]
  (let [panels (map :box (grid/content-items g layout panel-content?))
        {:keys [w h]} (first panels)
        columns (count (distinct (map :x panels)))
        rows (count (distinct (map :y panels)))
        w-avail (+ (double w) (/ (double column-total) columns))
        h-avail (+ (double h) (/ (double row-total) rows))
        horizontal (if (scale/flipped? scales) :y :x)
        dx (span (get scales horizontal)) dy (span (get scales (if (= horizontal :x) :y :x)))
        k (m/min (/ w-avail dx) (/ h-avail dy))]
    [(m/max 0 (m/round (* (- w-avail (* k dx)) columns)))
     (m/max 0 (m/round (* (- h-avail (* k dy)) rows)))]))

(defn- iso-mismatch
  "Relative difference of the horizontal and vertical pixels per unit of `layout`'s first panel for finalized
  `scales`."
  ^double [g layout scales]
  (let [{:keys [w h]} (grid/find-content-box g layout panel-content?)
        horizontal (if (scale/flipped? scales) :y :x)
        px (/ (double w) (span (get scales horizontal)))
        py (/ (double h) (span (get scales (if (= horizontal :x) :y :x))))]
    (/ (m/abs (- px py)) (m/max px py))))

(defn- iso-layout
  "Iso aspect: starting from the free layout, size the spacer tracks around the (flexible) panel block so the panels
  get equal pixels per unit on x and y (see [[iso-spacers]], [[cljplot2.layout.plot-grid/iso-grid]]). Refit until the
  whole-pixel spacers repeat (settled, or cycling when measured tracks change by a pixel with the panel size), at most
  [[max-iso-rounds]] times, and keep the round with the smallest mismatch. Flexible panels absorb measured track
  changes, so the grid never overflows."
  [plot canvas]
  (let [base-grid (:grid plot)
        candidate (fn [g layout] (let [scales (finalize-scales (assoc plot :grid g) layout)]
                                   {:grid g :layout layout :scales scales :mismatch (iso-mismatch g layout scales)}))]
    (loop [round 1
           seen #{[0 0]}
           current (candidate base-grid (solve-layout plot canvas))
           best nil]
      (let [spacers' (iso-spacers (:grid current) (:layout current) (:scales current)
                                  (if best (:spacers current) [0 0]))
            g' (plot-grid/iso-grid base-grid (spacers' 0) (spacers' 1))
            next (assoc (candidate g' (solve-layout (assoc plot :grid g') canvas)) :spacers spacers')
            best (if (or (nil? best) (< (double (:mismatch next)) (double (:mismatch best)))) next best)]
        (if (or (contains? seen spacers') (>= round max-iso-rounds))
          (assoc plot :grid (:grid best) :layout (:layout best))
          (recur (inc round) (conj seen spacers') next best))))))

(defn- layout-stage
  "The plot's layout (see [[solve-layout]]); with `:aspect :iso` the panels are sized for equal pixels per unit on x
  and y (see [[iso-layout]]) and the stored `:grid` has the fixed panel tracks."
  [plot]
  (measure/call-with-scratch-canvas
   (get-in plot [:options :hint])
   (fn [canvas]
     (if (= :iso (:aspect plot))
       (iso-layout plot canvas)
       (assoc plot :layout (solve-layout plot canvas))))))

(defn- finalized-scales-stage
  "Scales finalized for the final layout (stored under `:finalized-scales`; the plot's `:scales` key is user input)."
  [plot]
  (let [scales (finalize-scales plot (:layout plot))]
    (assoc plot :finalized-scales scales :margin-env (margin-env plot (:layout plot) scales))))

(declare build)

(defn- build-subplot
  "Built sub-plot `p` of item `id` for its `box` (`:cvd` in its `:options` throws `{:key :cvd}`: it applies to the
  whole rendering). Errors are re-thrown as `ex-info` with `:subplot id` and
  `:subplot-path` (outer item ids first), the original as cause: an `ex-info` keeps its message and data, any other
  exception gets `{:key :subplot}`."
  [p id {:keys [w h]} hint]
  (try (when (some? (get-in p [:options :cvd]))
         (schema/fail! "Option :cvd applies to the whole rendering, not to a sub-plot; give it to the outer plot."
                       {:key :cvd :in [:options :cvd] :value (get-in p [:options :cvd])}))
       (build p {:width (long w) :height (long h) :hint hint})
       (catch Exception e
         (let [data (if (instance? clojure.lang.ExceptionInfo e) (ex-data e) {:key :subplot})
               message (if (instance? clojure.lang.ExceptionInfo e)
                         (ex-message e)
                         (str "Sub-plot failed: " (or (ex-message e) (.getName (class e)))))]
           (throw (ex-info message (assoc data :subplot id :subplot-path (into [id] (:subplot-path data))) e))))))

(defn- subplots-stage
  "Every `{:kind :plot}` item of the grid (nested grids included) built at its box size with the plot's hint:
  `{item-id built-plot}`. Items whose box is smaller than 1x1 px are skipped. Plot item ids must be unique across
  the grid tree (`ex-info {:key :id}`). For `:arrange` (unless `{:align false}`) the sub-plots' panels are aligned
  in rows and columns (see [[cljplot2.layout.align/align-panels]])."
  [{:keys [grid layout options] :as plot}]
  (let [items (grid/content-items grid layout subplot/plot-content?)
        hint (:hint options)]
    (when-let [dup (first (for [[id n] (frequencies (map :id items)) :when (> (long n) 1)] id))]
      (schema/fail! "Plot item ids must be unique across nested grids." {:key :id :value dup}))
    (let [cells (vec (keep (fn [{:keys [id content box]}]
                             (when (and (>= (long (:w box)) 1) (>= (long (:h box)) 1))
                               {:id id :box box :plot (:plot content)
                                :built (build-subplot (:plot content) id box hint)}))
                           items))]
      (assoc plot :subplots
             (if (and (:arrange plot) (get-in plot [:arrange :align] true))
               (align/align-panels cells (fn [{:keys [id box] p :plot} padding]
                                        (build-subplot (assoc p :alignment-padding padding) id box hint)))
               (into {} (map (juxt :id :built)) cells))))))

(def stage-order
  "Pipeline stages in execution order."
  [:split :stats :extents :scale-specs :grid :layout :finalized-scales :subplots])

(def ^:private stage-extra-keys
  "Per stage, the plot keys it adds besides its own (each stage stores its result under its name): dropped with the
  stage keys before a rebuild, so a stage adding a key lists it here."
  {:stats [:margin-stats] :finalized-scales [:margin-env]})

(def ^:private stage-fns
  {:split split-stage
   :stats stats-stage
   :extents extents-stage
   :scale-specs scale-specs-stage
   :grid grid-stage
   :layout layout-stage
   :finalized-scales finalized-scales-stage
   :subplots subplots-stage})

;; validation

(def ^:private chart-schemas
  [:map {:closed true}
   (schema/optional :options schema/malli-schema)
   (schema/optional :style schema/malli-schema)
   (schema/optional :mapping schema/malli-schema)])

(def chart-schema
  "A chart value, see [[cljplot2.chart]]; open (a chart value may carry the user's own keys)."
  [:map
   (schema/nilable :kind keyword?)
   (schema/nilable :options :map)
   [:compute schema/function]
   [:draw schema/function]
   (schema/nilable :legend schema/function)
   (schema/optional :inherit-mapping [:enum :all :facets])
   (schema/optional :coords [:set [:enum :cartesian :polar]])
   (schema/nilable :schema chart-schemas)])

(def scales-schema
  "Plot `:scales`: position (`:x`, `:y`), facet and aesthetic scale specs."
  (-> [:map {:closed true}]
      (into (map #(schema/optional % scale/scale-spec-schema)) [:x :y])
      (into (map #(schema/optional % facet/scale-spec-schema)) [:facet :facet-col :facet-row])
      (into (map #(schema/optional % (aesthetic/scale-spec-schema %))) aesthetic/dims)))

(def ^:private margin-schema
  [:multi {:dispatch #(cond (chart-value? %) :chart (map? %) :spec :else :charts)}
   [:chart chart-schema]
   [:charts [:vector {:min 1} chart-schema]]
   [:spec [:map {:closed true}
           [:charts [:multi {:dispatch chart-value?} [true chart-schema] [false [:vector {:min 1} chart-schema]]]]
           (schema/optional :size schema/pos)
           (schema/optional :axis schema/bool)]]])

(def margins-schema
  "Plot `:margins`: `{:top m :right m}`, `m` - a chart, a vector of charts or `{:charts c :size px :axis bool}`."
  [:map {:closed true} (schema/optional :top margin-schema) (schema/optional :right margin-schema)])

(def plot-schema
  "A plot map (keys in the namespace doc); `:theme` and `:options` are checked merged ([[cljplot2.theme/theme-schema]],
  [[options-schema]])."
  [:map {:closed true}
   (schema/nilable :data data/data-schema)
   (schema/nilable :mapping data/mapping-schema)
   (schema/nilable :charts [:vector chart-schema])
   (schema/nilable :scales scales-schema)
   (schema/nilable :facets facet/facet-options)
   (schema/nilable :groups [:map {:closed true} (schema/nilable :na schema/bool)])
   (schema/nilable :legend schema/bool)
   (schema/nilable :aspect [:enum :iso])
   (schema/nilable :flip schema/bool)
   (schema/nilable :coord coord/coord-schema)
   (schema/nilable :labels plot-grid/labels-schema)
   (schema/nilable :custom-grid :map)
   (schema/nilable :arrange plot-grid/arrange-schema)
   (schema/nilable :margins margins-schema)
   (schema/optional :alignment-padding [:map-of keyword? number?])
   (schema/nilable :options :map)
   (schema/optional :theme any?)])

(def options-schema
  "Merged render options of `build` (see [[default-options]]), `:debug` and `:cvd` (used by
  `cljplot2.render/render`) and `:until`."
  [:map {:closed true}
   [:width schema/pos-int]
   [:height schema/pos-int]
   [:hint (into [:enum] (sort measure/rendering-hints))]
   (schema/nilable :debug schema/bool)
   (schema/nilable :cvd schema/cvd-deficiency)
   (schema/optional :until (into [:enum] stage-order))])

(defn- merged-mappings
  "Per chart: its mapping, see [[chart-mapping]]."
  [{:keys [mapping charts]}]
  (mapv #(chart-mapping mapping %) charts))

(defn- rule-errors
  "Errors of the rules across plot keys (for a plot whose keys are valid): facet mappings, scales without a mapping,
  `:custom-grid` / `:arrange` combinations (with labels, `:aspect :iso` or `:flip`)."
  [{:keys [scales labels custom-grid arrange aspect flip charts] :as plot}]
  (let [mappings (merged-mappings plot)
        dim-sets (distinct (remove empty? (map facet/facet-dims mappings)))
        error (fn [in value message] {:in in :value value :message message})]
    (concat
     (keep-indexed (fn [i m] (when-let [problem (facet/mapping-problem m)]
                               (error [:charts i :options :mapping] (facet/facet-dims m) problem)))
                   mappings)
     (when (> (count dim-sets) 1)
       [(error [:charts] (vec dim-sets) "charts should use the same facet mapping")])
     (let [margin-mappings (when (empty? (schema/errors margins-schema (:margins plot) [:margins]))
                             (for [[_ {:keys [charts]}] (plot-margins plot) c charts] (get-in c [:options :mapping])))]
       (for [k (keys scales)
             :when (and (aesthetic/dim? k) (not-any? #(some? (get % k)) (concat mappings margin-mappings))
                        (not-any? #(contains? (:computed-aesthetics %) k) charts))]
         (error [:scales k] (get scales k) "aesthetic scale needs a chart mapping its key")))
     (for [k (keys scales)
           :when (and (facet/facet-keys k) (not (some #{k} (first dim-sets))))]
       (error [:scales k] (get scales k) "facet scale needs a facet mapping of its key"))
     (when (and custom-grid arrange)
       [(error [:arrange] arrange "can't be combined with :custom-grid")])
     (when (and (= :iso aspect) (or custom-grid arrange))
       [(error [:aspect] aspect "iso needs the generated grid, not :custom-grid or :arrange")])
     (when (and flip (or custom-grid arrange))
       [(error [:flip] flip "flip needs the generated grid, not :custom-grid or :arrange")])
     (when (coord/polar? (:coord plot))
       (concat
        (when (= :iso aspect) [(error [:coord] (:coord plot) "polar coordinates can't be combined with :aspect :iso")])
        (when (or custom-grid arrange)
          [(error [:coord] (:coord plot) "polar coordinates need the generated grid, not :custom-grid or :arrange (put them on the plots inside)")])
        (when (seq (:margins plot)) [(error [:coord] (:coord plot) "polar coordinates can't be combined with :margins")])
        (keep-indexed (fn [i c] (when-not (contains? (get c :coords #{:cartesian}) :polar)
                                  (error [:charts i] (:kind c) (str "chart " (:kind c) " doesn't support polar coordinates"))))
                      charts)))
     (when (or custom-grid arrange)
       (for [k (keys labels) :when (not= k :title)]
         (error [:labels k] (get labels k) "only :title is allowed with :custom-grid or :arrange")))
     (when (seq (:margins plot))
       (concat
        (when (seq dim-sets)
          [(error [:margins] (:margins plot) "margins can't be combined with facets (yet)")])
        (when (or custom-grid arrange)
          [(error [:margins] (:margins plot) "margins belong on the plots inside :arrange / :custom-grid")])
        (for [[side {:keys [axis charts]}] (plot-margins plot)
              :when (and axis (not-any? (comp magnitude-chart-kinds :kind) charts))]
          (error [:margins side :axis] axis "a margin axis needs a histogram or density chart (strips have no magnitude)")))))))

(defn- chart-checks
  "`[schema value in]` checks of the charts' schemas: theme style of every built-in kind (and of user kinds present in
  `theme` and in the plot), each chart's options and merged mapping."
  [{:keys [charts] :as plot} theme]
  (let [chart-theme (when (map? theme) (:chart theme))]
    (distinct
     (concat
      (when (map? chart-theme)
        (for [[k style] chart/builtin-styles] [style (get chart-theme k) [:theme :chart k]]))
      (mapcat (fn [i {:keys [kind] :as c} m]
                (let [{:keys [options style mapping]} (:schema c)]
                  (cond-> []
                    options (conj [options (:options c) [:charts i :options]])
                    mapping (conj [mapping m [:charts i :options :mapping]])
                    (and style (map? chart-theme) (not (contains? chart/builtin-styles kind)) (contains? chart-theme kind))
                    (conj [style (get chart-theme kind) [:theme :chart kind]]))))
              (range) charts (merged-mappings plot))
      ;; margin charts: their options (mappings are built from the plot's at compute)
      (for [[side {:keys [charts]}] (when (empty? (schema/errors margins-schema (:margins plot) [:margins]))
                                      (plot-margins plot))
            [i c] (map-indexed vector charts)
            :let [options-schema (get-in c [:schema :options])]
            :when options-schema]
        [options-schema (:options c) [:margins side :charts i :options]])))))

(defn- validate!
  "Check `plot` (stage keys removed), its merged `theme` and `options` against their schemas, then - when those hold -
  the rules across plot keys; throws one `ex-info` with all errors (see [[cljplot2.schema]])."
  [plot theme options]
  (let [plot-errors (schema/errors plot-schema plot [])
        errors (concat (schema/errors options-schema options [:options])
                       plot-errors
                       (theme/errors theme)
                       (when (empty? plot-errors)
                         (mapcat (fn [[s v in]] (schema/errors s v in)) (chart-checks plot theme))))]
    (schema/throw-errors! (if (seq errors) errors (rule-errors plot)))))

;; build

(defn- type-kind
  "Scale spec `spec` of dimension `k` with the declared `type` (`:categorical` / `:numeric`) resolved to a kind and
  `:type` removed: categorical - `:bands` for `:x` / `:y`, `:ordinal` for aesthetics, nothing for facets; numeric -
  `:linear` for aesthetics, nothing for `:x` / `:y` (numbers are continuous there by default). Throws `ex-info
  {:key :type}` when an explicit `:kind` contradicts it (an aesthetic `:binned` kind is numeric; `:identity` takes no
  `:type`)."
  [k type spec]
  (let [wanted (cond
                 (position-scales k) (case type :categorical :bands :numeric nil)
                 (aesthetic/dim? k) (case type :categorical :ordinal :numeric :linear)
                 :else nil)
        allowed (if (and (aesthetic/dim? k) (= :numeric type)) #{:linear :binned} #{wanted})
        given (:kind spec)
        contradiction? (cond
                         (and given wanted) (not (contains? allowed given))
                         (and given (position-scales k) (= :numeric type)) (= :bands given)
                         :else false)]
    (when contradiction?
      (schema/fail! (str "Scale :type " type " contradicts :kind " given ".") {:key :type :in [:scales k :type] :value type}))
    (cond-> (dissoc spec :type)
      (and wanted (nil? given)) (assoc :kind wanted))))

(defn- resolve-scale-types
  "Plot `scales` with every declared `:type` resolved to a kind (see [[type-kind]]); a categorical mapping marker
  (`cljplot2.data.levels/categorical`) in any of the charts' `mappings` declares its dimension's scale categorical.
  Throws `ex-info {:key :type}` for a marker on a dimension declared `:type :numeric`."
  [scales mappings]
  (let [scale-dim? #(or (position-scales %) (aesthetic/dim? %) (contains? facet/facet-keys %))
        marked (set (for [m mappings [k acc] m :when (and (scale-dim? k) (accessor/categorical? acc))] k))]
    (reduce (fn [acc k]
              (let [spec (get scales k {})
                    declared (:type spec)]
                (when (and (marked k) (= :numeric declared))
                  (schema/fail! "A categorical mapping marker contradicts the scale's :type :numeric."
                                {:key :type :in [:scales k :type] :value declared}))
                (if-let [type (if (marked k) :categorical declared)]
                  (assoc acc k (type-kind k type spec))
                  acc)))
            scales
            (distinct (concat (filter scale-dim? (keys scales)) marked)))))

(defn build
  "Run `plot` through the pipeline stages and return the plot map with each stage's key added.

  `options` (merged over `(:options plot)` and [[default-options]], stored under `:options`):
  `:width`, `:height`, `:hint`, and `:until` - last stage to run (default: the last one).
  `:theme` is stored merged with the default theme.
  Stage keys (and the margin keys stages derive) already present in `plot` (e.g. from an earlier `build`) are dropped
  first.
  Before any stage runs, the plot, its merged theme and options and its charts' schemas are validated (see
  [[validate!]]): one `ex-info` lists every error, see [[cljplot2.schema]]."
  ([plot] (build plot {}))
  ([plot options]
   (let [plot (if (map? plot) (apply dissoc plot (concat stage-order (mapcat val stage-extra-keys))) plot)
         plot-options (when (map? plot) (:options plot))
         render-options (merge default-options (when (map? plot-options) plot-options) (dissoc options :until))
         theme (theme/plot-theme (when (map? plot) (:theme plot)))]
     (validate! plot theme (cond-> render-options (contains? options :until) (assoc :until (:until options))))
     (reduce (fn [plot stage] ((stage-fns stage) plot))
             (assoc plot :options render-options :theme theme
                    :scales (resolve-scale-types (:scales plot) (merged-mappings plot)))
             (take (inc (.indexOf ^java.util.List stage-order (get options :until (peek stage-order)))) stage-order)))))
