(ns cljplot2.pipeline
  "Plot pipeline: a plot map passes through fixed stages, each a pure function adding one key.

  Stages, in order: `:split` -> `:stats` -> `:extents` -> `:scale-specs` -> `:grid` -> `:layout` -> `:finalized-scales` -> `:subplots` ([[stage-order]]). [[build]] validates the plot, its merged theme and options first (see [[cljplot2.schema]]), then runs the stages; [[finalize-scales]] and [[panel-scales]] give the finalized scales of a layout, [[plot-margins]] the normalized margins; [[plot-schema]], [[options-schema]], [[scales-schema]], [[margins-schema]] and [[chart-schema]] are the schemas `build` checks; [[default-options]] are the render option defaults.

  Plot keys (schema: [[plot-schema]]):

  - `:data`, `:mapping` - plot data and mapping, see [[cljplot2.data.rows]].
  - `:charts` - vector of chart values, see [[cljplot2.chart]]; 3D charts (`:coords #{:three-d}`: `chart/cloud`, `chart/surface`) only with other 3D charts - several each with an explicit `:domain {:x :y :z}` - and without `:flip`, `:aspect :iso`, `:margins`, polar `:coord` or free / sliced facet scales.
  - `:scales` - `{:x spec :y spec}` position scales, see [[cljplot2.scale.position/data-scale-spec]]; facet scales, see [[cljplot2.data.facet]]; aesthetic scales `:color` `:shape` `:size` `:dash` `:pattern`, see [[cljplot2.scale.aesthetic]].
  - `:facets` - `{:columns :rows :order :na-panel :scales :strips :widths :heights}` facet layout options, see [[cljplot2.data.facet]]: `:scales {:x r :y r}` - `:same` (default), `:free` (each panel's own data domain) or `:sliced` (each panel's own centre, the widest panel's span; numeric data) - panels sharing an axis share the scale (wrap: every panel has its own axis; grid: per column / row), see [[panel-scale-specs]]; `:strips false`; `:widths` / `:heights` - relative panel column / row sizes. Such plots finalize their position scales per panel (`:panel-scales`, see [[panel-scales]]); not with `:aspect :iso`, polar coordinates or explicit `:domain`s of free / sliced scales.
  - `:groups` - `{:na bool}`: `nil` aesthetic values form the NA level (default `true`) or are dropped.
  - `:labels` - plot titles, see [[cljplot2.layout.plot-grid]]; charts may suggest axis titles (`:axis-titles` in their stats), `:labels` win.
  - `:legend` - `false` hides all legends, see [[cljplot2.content.legend]].
  - `:legend-only` - `true`: the plot draws only its legends (from its charts and scales as usual; no panel, axes or axis titles; `:labels {:title t}` above them), e.g. a legend cell of an `:arrange` (see [[cljplot2.layout.plot-grid/legend-only-grid]]); not with `:custom-grid` / `:arrange`.
  - `:alternate-panel` - `true`: a single-panel plot draws theme `[:panel :alternate-fill]` over its panel fill - the shading of odd facet panels, for checkerboards of `:arrange` cells (e.g. [[cljplot2.multiplot/pairs]]); facet panels shade by position regardless.
  - `:aspect` - `:iso`: equal pixels per data unit on x and y (linear scales, generated grid); the panel aspect ratio changes, the panel block is centred in the leftover space.
  - `:flip` - `true` draws the `:x` scale vertically and `:y` horizontally (generated grid only). Scales, extents, labels, tick formats and theme `[:axis k]` keep their keys; only the finalized scales' `:direction` changes, and axes, grid lines, axis titles and charts follow it (charts draw through [[cljplot2.scale.position/projection]]).
  - `:coord` - `{:kind :polar ...}`: polar coordinates - the horizontal scale maps to angles, the vertical one to radii (`:flip` swaps them; see [[cljplot2.scale.coord]]); polar guides inside the panel, no outer axes; only charts declaring `:polar` in their `:coords` (bar, segment, scatter, line, text, rule, rect, parallel, partition); not with `:aspect :iso`, `:margins`, `:custom-grid` or `:arrange` (sub-plots may be polar).
  - `:custom-grid` - a hand-written grid (see [[cljplot2.layout.grid]]) used instead of the generated one; only `:labels {:title t}` may accompany it (a title row above it). Items with `{:kind :plot :plot p}` content hold whole plots (see [[cljplot2.content.subplot]]).
  - `:arrange` - `{:plots [p ...] :columns :rows :widths :heights}`: a multiplot, its grid generated from the merged theme (see [[cljplot2.multiplot/arrange]], [[cljplot2.layout.plot-grid/arrange-grid]]); only `:labels {:title t}` may accompany it; can't be combined with `:custom-grid`; the sub-plots' panels are aligned unless `:align false`.
  - `:margins` - `{:top m :right m}`: charts beside the panel sharing its horizontal (top) / vertical (right) scale, `m` a chart, a vector of charts or `{:charts c :size px :axis bool}` (thickness, default 80; `:axis` - the magnitude axis of histogram / density margins); margin charts read the plot's data and aesthetic mapping with `:x` mapped to the shared dimension (their own `:mapping` wins), no axes of their own; single-panel generated grids only (not with facets, `:arrange` or `:custom-grid` - put margins on the sub-plots), see [[margin-stats]].
  - `:alignment-padding` - internal: `{:left :top :right :bottom}` pixels added to the generated grid's margins (set on sub-plots by multiplot alignment).
  - `:options` - render options (see [[default-options]]), merged under `build`'s options; stored resolved.
  - `:theme` - deep-merged over [[cljplot2.theme/default-theme]]; stored resolved.

  Facet mapping keys (`:facet`, `:facet-col`, `:facet-row`) split rows into panels, see [[cljplot2.data.facet]]. Aesthetic mapping keys (`:color`, `:shape`, `:size`, `:dash`, `:pattern`) style marks, see [[cljplot2.scale.aesthetic]]; `:group` splits rows into groups for charts drawing one mark per group, see [[cljplot2.data.group]]."
  (:require [fastmath.core :as m]
            [cljplot2.chart :as chart]
            [cljplot2.schema :as schema]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.data.rows :as data]
            [cljplot2.data.accessor :as accessor]
            [cljplot2.data.facet :as facet]
            [cljplot2.data.levels :as levels]
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
            [cljplot2.content.legend :as legend]
            [cljplot2.content.subplot :as subplot]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^{:const true :tag 'long} max-layout-rounds
  "Upper bound on measure, solve and finalize-scales rounds of [[solve-layout]]."
  3)

;; stages

(def ^:private position-scales
  "The position scale keys."
  #{:x :y})

(defn- without-facet-values?
  "Tests whether a chart input with its own data has no value for every facet key of its mapping.

  Such a chart (e.g. an annotation's summary rows) is drawn in every panel, as one without facet keys.

  Parameters:

  - `input` (map): a chart input `{:rows :mapping :own-data?}` - its rows, merged mapping and whether the rows are the chart's own `:data`.

  Returns `true` when `:own-data?` is set and every facet accessor of the mapping gives `nil` for every row; falsy otherwise (a chart reading the plot's data is never such a chart)."
  [{:keys [rows mapping own-data?]}]
  (and own-data?
       (not-any? (fn [d] (let [acc (data/row-accessor d (mapping d))] (some #(some? (acc %)) rows)))
                 (facet/facet-dims mapping))))

(defn- facet-split
  "Facet dimensions, levels and per-chart panel groups of the chart inputs.

  The facet dimensions are those of the first faceted chart; levels are ordered by the facet scales (see [[cljplot2.data.facet/ordered-levels]]). A chart without facet keys, or whose own data has no facet values (see [[without-facet-values?]]), is drawn in every panel (its rows under [[cljplot2.data.facet/all-panels]]).

  Parameters:

  - `chart-inputs` (sequence of maps): per chart `{:rows :mapping :own-data?}`.
  - `scales` (map): the plot `:scales` (facet scale specs order and select levels).
  - `na-panel?` (boolean): `nil` facet values form an NA panel.

  Returns `{:facets {:dims [d ...] :levels {d [level ...]}} :charts [{:mapping :groups :dropped n} ...]}` - one chart entry per input, `:groups` keyed by panel key, `:dropped` the rows without a panel. Without facet dimensions, or when a dimension has no level, the facets are empty and every chart has one group under [[cljplot2.data.facet/single-panel]]."
  [chart-inputs scales na-panel?]
  (let [in-every-panel? (fn [input] (or (empty? (facet/facet-dims (:mapping input))) (without-facet-values? input)))
        faceted (remove in-every-panel? chart-inputs)
        dims (vec (first (distinct (map (comp facet/facet-dims :mapping) faceted))))
        accessors (fn [mapping] (into {} (map (fn [d] [d (data/row-accessor d (mapping d))])) dims))
        levels (into {} (map (fn [d]
                               (let [keyed (mapcat (fn [{:keys [rows mapping]}]
                                                     (let [acc (get (accessors mapping) d)] (map (fn [r] [(acc r) r]) rows)))
                                                   faceted)
                                     rows-of (update-vals (group-by first keyed) #(mapv second %))]
                                 [d (facet/ordered-levels (facet/levels d (map first keyed) (get scales d) na-panel?)
                                                          #(get rows-of %) (get scales d))])))
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
  "The merged mapping of a chart.

  Parameters:

  - `mapping` (map): the plot mapping.
  - `chart` (map): a chart value; `:inherit-mapping :facets` (annotations) takes only the facet keys of `mapping`, `:options :mapping` is merged over.

  Returns the chart's mapping: the inherited plot mapping with the chart's own mapping merged over it (a key set to `nil` there stays `nil`)."
  [mapping {:keys [options inherit-mapping]}]
  (merge (if (= :facets inherit-mapping)
           (select-keys mapping [:facet :facet-col :facet-row])
           mapping)
         (:mapping options)))

(defn- split-stage
  "Pipeline stage `:split`: every chart's rows grouped by panel.

  Each chart reads its own non-nil `:data` or the plot's (converted to rows once per plot), with its merged mapping (see [[chart-mapping]]), and is split by [[facet-split]].

  Parameters:

  - `plot` (map): the plot map; reads `:charts`, `:data`, `:mapping`, `:scales` and `:facets` (`:na-panel`, default `true`).

  Returns `plot` with `:split {:facets {:dims :levels :dropped {:facet n}} :charts [{:mapping :groups} ...]}`, `:dropped` summing the rows without a panel over the charts. See [[cljplot2.data.facet]]."
  [{:keys [charts data mapping scales facets] :as plot}]
  (let [plot-rows (data/rows data)
        inputs (mapv (fn [{:keys [options] :as chart}]
                       {:mapping (chart-mapping mapping chart)
                        :rows (if (some? (:data options)) (data/rows (:data options)) plot-rows)
                        :own-data? (some? (:data options))})
                     charts)
        {split-facets :facets chart-splits :charts} (facet-split inputs scales (get facets :na-panel true))]
    (assoc plot :split {:facets (assoc split-facets :dropped {:facet (reduce m/long-add (map :dropped chart-splits))})
                        :charts (mapv #(dissoc % :dropped) chart-splits)})))

;; margins

(def ^{:private true :const true :tag 'long} provisional-tick-hint
  "Tick count hint of a provisional finalization (before the first layout round)."
  10)

(def ^:private margin-sides
  "Plot margin sides, in grid order."
  [:top :right])

(def ^:private magnitude-chart-kinds
  "Chart kinds with a magnitude dimension (counts, densities) a margin axis can show."
  #{:histogram :density})

(def ^{:private true :const true :tag 'long} default-margin-size
  "Thickness of a plot margin in pixels when its spec gives none."
  80)

(defn- margin-spec
  "A plot margin in its normal form.

  Parameters:

  - `m` (chart, vector of charts or map): a value of plot `:margins` - a chart, a vector of charts, or `{:charts c :size px :axis bool}` with `c` a chart or charts.

  Returns `{:charts [chart ...] :size px :axis bool}`; `:size` defaults to 80 pixels, `:axis` to `false`."
  [m]
  (merge {:size default-margin-size :axis false}
         (cond
           (chart/chart-value? m) {:charts [m]}
           (map? m) (update m :charts #(if (chart/chart-value? %) [%] (vec %)))
           :else {:charts (vec m)})))

(defn plot-margins
  "The plot's margins in their normal form.

  Parameters:

  - `plot` (map): a plot map with optional `:margins {:top m :right m}`.

  Returns `{side spec}` of the sides present, each normalized by [[margin-spec]], in the order `:top`, `:right`; an empty map without margins. See [[margin-sides]]."
  [plot]
  (into {} (keep (fn [side] (when-some [m (get-in plot [:margins side])] [side (margin-spec m)]))) margin-sides))

(defn- groupable-aesthetics
  "Aesthetic keys of the plot mapping whose scale groups rows.

  An aesthetic groups rows when its scale is ordinal or identity, resolved as the plot scale will be from the plot's scale spec and the present values; a linear or binned aesthetic describes single rows, so margin charts don't inherit it.

  Parameters:

  - `mapping` (map): the plot mapping.
  - `plot-scales` (map): the plot `:scales`.
  - `rows` (sequence of maps): the plot's rows.

  Returns a vector of aesthetic keys (a subset of [[cljplot2.scale.aesthetic/dims]], in that order)."
  [mapping plot-scales rows]
  (filterv (fn [k]
             (when-let [acc (get mapping k)]
               (let [present (keep (data/row-accessor k acc) rows)]
                 (#{:ordinal :identity}
                  (aesthetic/inferred-kind k (get plot-scales k {}) (aesthetic/continuous-values? k present))))))
           aesthetic/dims))

(defn- margin-stats
  "Computed charts of every plot margin.

  A margin chart reads the plot's data (or its own) with the plot's grouping aesthetics (see [[groupable-aesthetics]]) and `:group` mapping, `:x` mapped to the plot's accessor of the shared dimension - the horizontal one for `:top`, the vertical one for `:right` - and its own `:mapping` merged over; annotation charts (`:inherit-mapping :facets`) take their own mapping only. Margins have one panel; the right margin computes as flipped (its values run vertically).

  Parameters:

  - `plot` (map): the plot map; reads `:data`, `:mapping`, `:flip` and `:margins`.
  - `env` (map): the charts' compute environment (see [[stats-stage]]).

  Returns `{side spec}`: the margin spec (see [[margin-spec]]) with `:mappings` (each chart's mapping) and `:stats` (each chart's computed data); an empty map without margins.

  Throws `ex-info` with `{:key :margins}` when the plot doesn't map the shared dimension."
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
                              :stats (mapv (fn [{:keys [options] :as chart} side-mapping]
                                             (let [rows (if (some? (:data options)) (data/rows (:data options)) @plot-rows)]
                                               ((:compute chart) chart {:mapping side-mapping :groups {facet/single-panel rows}}
                                                side-env)))
                                           charts mappings))])))))

(defn- margin-env
  "Per margin side, the charts, stats and scales to measure and draw it.

  Parameters:

  - `plot` (map): the plot map with `:margin-stats` (see [[margin-stats]], `:spec` - the magnitude spec, see [[scale-specs-stage]]), `:grid`, `:flip` and `:theme`.
  - `layout` (map or `nil`): the solved layout; `nil` - provisional (before the first layout round).
  - `scales` (map): the plot's finalized scales.

  Returns `{side {:charts :stats :scales}}`; `:scales` holds the plot's aesthetic scales, `:x` - the plot's scale of the shared dimension (its direction kept) and `:y` - the margin's magnitude scale (counts, densities) finalized for the margin panel's size (provisional range `[0 1]` without a layout), across the shared one; no `:y` when the margin charts give no magnitude (e.g. strips only)."
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
                                   (let [{:keys [^long w ^long h]} box]
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
  "Pipeline stage `:stats`: every chart's computed data.

  Each chart's `:compute` runs on its split groups (see [[split-stage]]) with an environment of the theme, NA grouping, plot scales, flip, rendering hint, coordinates, aspect and a delayed estimate of the panel size (see [[cljplot2.layout.plot-grid/estimated-panel-size]]; it measures text, so charts deref it only when needed); margin charts compute too (see [[margin-stats]]).

  Parameters:

  - `plot` (map): the plot map after the `:split` stage.

  Returns `plot` with `:stats` (one computed map per chart, see [[cljplot2.chart]]) and `:margin-stats`."
  [{:keys [charts split theme groups scales flip facets options aspect] :as plot}]
  (let [facet-dims (get-in split [:facets :dims])
        env {:theme theme :na-level? (get groups :na true) :plot-scales scales :flip? (boolean flip)
             :hint (:hint options) :coord (:coord plot) :aspect aspect
             ;; measured on demand only (text measuring): charts needing the panel's shape before the layout
             :panel-size (delay (plot-grid/estimated-panel-size plot (legend/provisional-legends plot (:charts split))
                                                                (plot-margins plot) (plot-grid/panel-layout (:facets split) facets)
                                                                (and (seq facet-dims) (facet/strips? facets))
                                                                (not= [:facet] facet-dims)))}]
    (assoc plot
           :stats (mapv (fn [chart chart-split] ((:compute chart) chart chart-split env)) charts (:charts split))
           :margin-stats (margin-stats plot env))))

(defn- aesthetic-extents
  "Extents of the aesthetics over every chart and panel.

  Parameters:

  - `stats` (sequence of maps): computed chart data (`:extents {panel {dim extent}}`, `:na #{dim}`).

  Returns `{dim {:extent e :na? bool}}` for each aesthetic with an extent or NA values: `e` - the extents merged over charts and panels (see [[cljplot2.data.rows/merge-extents]], `nil` with NA values only), `:na?` - some chart has NA values of it."
  [stats]
  (into {} (keep (fn [k]
                   (let [extent (data/merge-extents k (mapcat #(keep (fn [e] (get e k)) (vals (:extents %))) stats))
                         na? (boolean (some #(contains? (:na %) k) stats))]
                     (when (or extent na?) [k {:extent extent :na? na?}]))))
        aesthetic/dims))

(defn- per-side-max
  "The larger value on each side of two `[lo hi]` pairs.

  Parameters:

  - `[a b]`, `[c d]` (pairs of numbers): pixel rooms `[lo hi]`.

  Returns `[(max a c) (max b d)]` as doubles."
  [[^double a ^double b] [^double c ^double d]]
  [(m/max a c) (m/max b d)])

(defn- reaches-end?
  "Tests whether a chart's extent reaches one end of the merged extent.

  Parameters:

  - `extent` (map): the merged extent of a dimension.
  - `chart-extent` (map): one chart's extent of it.
  - `i` (long): the side - 0 the low end (`:min`), 1 the high end (`:max`).

  Returns `true` when the chart's bound equals the merged one on that side; always `true` for categorical extents (no ends to compare)."
  [extent chart-extent i]
  (let [bound (if (m/zero? (long i)) :min :max)]
    (or (= :categorical (:kind extent))
        (m/zero? (compare (bound chart-extent) (bound extent))))))

(defn- merged-padding
  "Padding of a position dimension over the charts with an extent of it.

  Per side the largest request of the charts whose extent reaches that end (see [[reaches-end?]] and [[cljplot2.data.rows/explicit-padding]]: a zero at a baseline wins over other charts' defaults, room below the data over a zero; another chart's data beyond the baseline voids it), else the largest padding of all.

  Parameters:

  - `extent` (map): the merged extent.
  - `with-extent` (sequence of maps): per chart `{:extent :padding [lo hi] :explicit [lo hi]}`, `:explicit` - the requested sides (`nil` sides or `nil` - none).

  Returns `[lo hi]` doubles, fractions of the span (at least 0)."
  [extent with-extent]
  (mapv (fn [i]
          (let [requests (keep #(when (reaches-end? extent (:extent %) i) (some-> (:explicit %) (nth i))) with-extent)]
            (double (reduce m/max 0.0 (if (seq requests) requests (map #(nth (:padding %) i) with-extent))))))
        [0 1]))

(defn- position-extent
  "Extent, padding and pixel room of a position dimension over the charts' stats.

  A chart whose extents of the dimension are all companions (`:companion? true`) joins only when another chart has extents of it.

  Parameters:

  - `stats` (sequence of maps): computed chart data.
  - `k` (keyword): the position dimension, `:x` or `:y`.

  Returns `{:extent :padding :padding-px}` - the extents merged over charts and panels, the padding per side (see [[merged-padding]]) and the largest pixel room per side (see [[per-side-max]]); `nil` when no chart (besides companions) has an extent of `k`."
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
  "Pipeline stage `:extents`: merged extents of the position and aesthetic dimensions.

  Position dimensions take only the main charts' stats (see [[position-extent]]; companions alone give no extent, so no data scale); aesthetic extents also cover the margin charts, as margins share (and may extend) the plot's aesthetic scales (see [[aesthetic-extents]]).

  Parameters:

  - `plot` (map): the plot map after the `:stats` stage.

  Returns `plot` with `:extents {dim ...}`: position dimensions `{:extent :padding :padding-px}`, aesthetics `{:extent :na?}`."
  [{:keys [stats] :as plot}]
  ;; aesthetic extents also over margin charts: margins share (and may extend) the plot's aesthetic scales
  (assoc plot :extents
         (into (aesthetic-extents (into (vec stats) (mapcat :stats (vals (:margin-stats plot)))))
               (keep (fn [k] (some->> (position-extent stats k) (vector k))))
               position-scales)))

(defn- position-spec
  "Phase-1 spec of a position dimension from the plot's spec and the merged extent.

  Chart suggestions are applied: `:scale-defaults` (the first chart's, used as a whole unless the plot's spec sets any of them), `:tick-format` (the first chart's) and the pixel room `:padding-px`.

  Parameters:

  - `stats` (sequence of maps): computed chart data.
  - `k` (keyword): `:x` or `:y`.
  - `spec` (map): the plot's scale spec of `k` (may be `{}`).
  - `merged` (map): `{:extent :padding :padding-px}`, see [[position-extent]].

  Returns a scale spec, see [[cljplot2.scale.position/data-scale-spec]]; with `:tick-format` and `:padding-px` (only when some side is positive) added."
  [stats k spec {:keys [extent padding padding-px]}]
  (let [defaults (some #(get-in % [:scale-defaults k]) stats)
        suggested-spec (if (some #(contains? spec %) (keys defaults)) spec (merge defaults spec))
        tick-format (some #(get-in % [:tick-format k]) stats)]
    (cond-> (scale/data-scale-spec k suggested-spec extent padding)
      tick-format (assoc :tick-format tick-format)
      (some pos? padding-px) (assoc :padding-px padding-px))))

(defn- level-rows
  "Rows of every chart mapping a position dimension, grouped by their value of it.

  These are the rows behind the extents of [[group-stats]], used to order bands levels by rows.

  Parameters:

  - `plot` (map): the plot map after the `:split` stage.
  - `k` (keyword): the position dimension.
  - `group-keys` (set or `nil`): the panel keys of the split groups taken; `nil` - every group. Data drawn in every panel is always taken.

  Returns `{value [row ...]}`."
  [plot k group-keys]
  (reduce (fn [by-value {:keys [mapping groups]}]
            (if-let [value-accessor (get mapping k)]
              (let [value-of (data/row-accessor k value-accessor)]
                (reduce (fn [chart-acc [g rows]]
                          (if (or (nil? group-keys) (contains? group-keys g) (= facet/all-panels g))
                            (reduce (fn [group-acc row] (update group-acc (value-of row) (fnil conj []) row)) chart-acc rows)
                            chart-acc))
                        by-value groups))
              by-value))
          {} (get-in plot [:split :charts])))

(defn- ordered-domain
  "A bands domain ordered by the plot scale's level-order options.

  Parameters:

  - `plot` (map): the plot map after the `:split` stage.
  - `k` (keyword): the position dimension.
  - `user-spec` (map): the plot's scale spec; `:order-by` and `:descending` order the levels.
  - `domain` (vector): the levels.
  - `group-keys` (set or `nil`): panels whose rows are read (`nil` - all), see [[level-rows]].

  Returns the reordered levels, see [[cljplot2.data.levels/order-by-rows]]."
  [plot k user-spec domain group-keys]
  (let [rows-of (level-rows plot k group-keys)]
    (levels/order-by-rows domain #(get rows-of %) (:order-by user-spec) (:descending user-spec))))

(defn- ordered-position-spec
  "A phase-1 position spec without the plot scale's level-order options.

  The options `:order-by` / `:descending` are consumed here: a bands domain is ordered by them (see [[ordered-domain]]); other kinds just drop them.

  Parameters:

  - `plot` (map): the plot map after the `:split` stage.
  - `k` (keyword): the position dimension.
  - `user-spec` (map): the plot's scale spec of `k`.
  - `spec` (map): the phase-1 spec.

  Returns `spec` without `:order-by` and `:descending`, its domain ordered when it's a bands scale and `user-spec` has `:order-by`."
  [plot k user-spec spec]
  (cond-> (dissoc spec :order-by :descending)
    (and (:order-by user-spec) (= :bands (:kind spec))) (update :domain #(ordered-domain plot k user-spec % nil))))

(defn- group-stats
  "The charts' stats limited to some panels.

  Parameters:

  - `stats` (sequence of maps): computed chart data.
  - `group-keys` (collection): panel keys kept.

  Returns `stats` with `:extents` only of those panels and of data drawn in every panel ([[cljplot2.data.facet/all-panels]])."
  [stats group-keys]
  (let [ks (conj (vec group-keys) facet/all-panels)]
    (mapv #(update % :extents select-keys ks) stats)))

(defn- free-spec
  "Phase-1 spec of a position dimension for a group of panels under a `:free` facet relation.

  Parameters:

  - `plot` (map): the plot map after the `:extents` stage.
  - `k` (keyword): the position dimension.
  - `user-spec` (map): the plot's scale spec of `k`.
  - `shared` (map): the shared phase-1 spec (all panels).
  - `group-keys` (collection): the panels sharing the spec.

  Returns for a bands scale the shared spec with the levels present in those panels (ordered by `:order-by` over their rows when `user-spec` has it); for a continuous scale a spec built from those panels' extents as the shared one is from all; `shared` when the panels have no data (or no present level)."
  [plot k user-spec shared group-keys]
  (let [stats (:stats plot)
        e (position-extent (group-stats stats group-keys) k)]
    (cond
      (nil? e) shared
      (= :bands (:kind shared)) (let [present (set (get-in e [:extent :values]))
                                      levels (filterv present (:domain shared))]
                                  (cond
                                    (empty? levels) shared
                                    (:order-by user-spec) (assoc shared :domain (ordered-domain plot k user-spec levels (set group-keys)))
                                    :else (assoc shared :domain levels)))
      :else (ordered-position-spec plot k user-spec (position-spec stats k user-spec e)))))

(defn- sliced-specs
  "Phase-1 specs of a position dimension for groups of panels under a `:sliced` facet relation.

  Every group gets the span of the widest group's data (in log10 space on a `:log` scale), centred on the middle of its own data, with the padding of the shared extent and without `nice` unless the plot scale sets it, so spans stay equal. When every panel holds a single value the span is that of all the data; a plot scale `:nice` extends each panel's domain separately, so spans may then differ.

  Parameters:

  - `plot` (map): the plot map after the `:extents` stage.
  - `k` (keyword): the position dimension.
  - `user-spec` (map): the plot's scale spec of `k`.
  - `shared` (map): the shared phase-1 spec.
  - `shared-extent` (map): the merged `{:extent :padding :padding-px}` of `k`.
  - `group-keysets` (sequence of sets): the panel groups.

  Returns one spec per group, in order; `shared` for a group without data.

  Throws `ex-info` with `{:key :facets}` for categorical, date or time data (only numeric data can be sliced)."
  [plot k user-spec shared shared-extent group-keysets]
  (when (= :bands (:kind shared))
    (schema/fail! (str "A :sliced " k " scale needs numeric data, not categorical levels.")
                  {:key :facets :in [:facets :scales k] :value :sliced}))
  (let [stats (:stats plot)
        log? (= :log (:kind shared))
        to (if log? m/log10 double)
        from (if log? m/exp10 double)
        extents (map #(:extent (position-extent (group-stats stats %) k)) group-keysets)
        _ (when (some #(and % (not= :numeric (:kind %))) extents)
            (schema/fail! (str "A :sliced " k " scale needs numeric data (not dates or times).")
                          {:key :facets :in [:facets :scales k] :value :sliced}))
        width (fn [e] (if e (m/- (double (to (:max e))) (double (to (:min e)))) 0.0))
        ;; every panel a single value: the span of all panels' data (0 when that is one value too - then widened)
        slice-span (let [widest (double (reduce m/max 0.0 (map width extents)))]
                     (if (m/pos? widest) widest (double (width (:extent shared-extent)))))
        spec (cond-> user-spec (not (contains? user-spec :nice)) (assoc :nice false))]
    (map (fn [extent]
           (if (nil? extent)
             shared
             (let [mid (m/* 0.5 (m/+ (double (to (:min extent))) (double (to (:max extent)))))
                   half (m/* 0.5 slice-span)]
               (ordered-position-spec plot k user-spec
                                      (position-spec stats k spec (assoc shared-extent :extent {:kind :numeric
                                                                                                :min (from (m/- mid half))
                                                                                                :max (from (m/+ mid half))}))))))
         extents)))

(defn- panel-scale-specs
  "Phase-1 position specs per panel for the `:free` / `:sliced` relations of plot `:facets`.

  Panels share a spec as they share an axis: in a wrap layout none, in a facet grid the horizontal scale per column and the vertical one per row (see [[cljplot2.data.facet/scale-relation]], [[free-spec]], [[sliced-specs]]).

  Parameters:

  - `plot` (map): the plot map after the `:extents` stage.
  - `specs` (map): the shared phase-1 specs.

  Returns `{panel-key {k spec}}` of the dimensions with such a relation; `nil` when the facets ask for no panel-local scales or the plot has no facets."
  [{:keys [facets split scales flip extents] :as plot} specs]
  (when (and (facet/panel-scales? facets) (seq (get-in split [:facets :dims])))
    (let [{:keys [panels wrap?]} (plot-grid/facet-panels (:facets split) scales facets)
          horizontal (scale/horizontal-key flip)
          group-of (fn [k panel] (cond wrap? (:key panel) (= k horizontal) (:col panel) :else (:row panel)))]
      (reduce (fn [acc k]
                (let [relation (facet/scale-relation facets k)
                      shared (get specs k)
                      user-spec (dissoc (get scales k {}) :type)]
                  (if (or (= :same relation) (nil? shared))
                    acc
                    (let [keysets (mapv #(set (map :key %)) (vals (group-by #(group-of k %) panels)))
                          group-specs (if (= :free relation)
                                        (map #(free-spec plot k user-spec shared %) keysets)
                                        (sliced-specs plot k user-spec shared (get extents k) keysets))]
                      (reduce (fn [specs-acc [ks spec]] (reduce #(assoc-in %1 [%2 k] spec) specs-acc ks))
                              acc (map vector keysets group-specs))))))
              {} position-scales))))

(defn- scale-specs-stage
  "Pipeline stage `:scale-specs`: phase-1 specs of the position and aesthetic scales.

  Specs are made for every dimension with a plot `:scales` entry or a data extent; aesthetic specs are final (no layout dependence). Position specs take the charts' suggestions (see [[position-spec]]) and level order (see [[ordered-position-spec]]); a computed aesthetic's suggested `:title`, `:legend` and `:range` (see [[cljplot2.scale.aesthetic/computed-aesthetics]], the first chart's) are the aesthetic scale's unless the plot's spec sets them. Each margin gets the spec of its magnitude scale from its charts' stats.

  Parameters:

  - `plot` (map): the plot map after the `:extents` stage.

  Returns `plot` with `:scale-specs {dim spec}` and the margins' `:spec` in `:margin-stats`. See [[cljplot2.scale.position/finalize]], [[cljplot2.scale.aesthetic/scale-spec]]."
  [{:keys [scales extents theme stats] :as plot}]
  (let [ks (distinct (concat (filter #(or (position-scales %) (aesthetic/dim? %)) (keys scales)) (keys extents)))
        suggested (reduce (fn [acc computed] (merge (update-vals computed aesthetic/computed-scale-attributes) acc)) {}
                          (map aesthetic/computed-aesthetics (:charts plot) (map :mapping (get-in plot [:split :charts]))))
        ;; the plot's own spec wins attribute by attribute
        aesthetic-spec (fn [k] (merge (get suggested k) (get scales k {})))]
    (assoc plot
           ;; a margin's magnitude scale: built as a position spec from the margin charts' stats
           :margin-stats (update-vals (:margin-stats plot)
                                      (fn [{side-stats :stats :as m}]
                                        (if-let [e (position-extent side-stats :y)]
                                          (assoc m :spec (position-spec side-stats :y {} e))
                                          m)))
           :scale-specs
           (into {} (keep (fn [k]
                            (let [{:keys [extent na?] :as e} (get extents k)]
                              (if (aesthetic/dim? k)
                                (some->> (aesthetic/scale-spec k (aesthetic-spec k) extent na? (:palette theme)) (vector k))
                                [k (ordered-position-spec plot k (get scales k {}) (position-spec stats k (get scales k {}) e))]))))
                 ks))))

(defn- panel-scale-specs-stage
  "Part of the `:scale-specs` stage: phase-1 position specs per panel for `:free` / `:sliced` facet scales.

  Parameters:

  - `plot` (map): the plot map with `:scale-specs`.

  Returns `plot` with `:panel-scale-specs` (see [[panel-scale-specs]]), or unchanged without such scales."
  [plot]
  (if-let [specs (panel-scale-specs plot (:scale-specs plot))]
    (assoc plot :panel-scale-specs specs)
    plot))

(defn- effective-labels
  "Plot labels over the axis titles the charts suggest.

  Parameters:

  - `labels` (map or `nil`): the plot `:labels`; an entry - also `nil` - wins over a suggestion.
  - `stats` (sequence of maps): computed chart data; `:axis-titles {:x t :y t}` per chart, the first suggestion per axis wins.

  Returns the merged labels map (`nil` without labels and suggestions); `labels` that aren't a map are passed through (rejected by validation)."
  [labels stats]
  (let [suggested (reduce (fn [acc titles] (merge (select-keys titles [:x :y]) acc)) {} (keep :axis-titles stats))]
    (cond
      (nil? labels) (not-empty suggested)
      (map? labels) (merge suggested labels)
      :else labels)))

(declare build)

(defn- legend-only-width
  "Pixels a `:legend-only` plot needs across.

  The plot is built up to its scale specs at the outer plot's size and hint; its legends are measured on a scratch canvas.

  Parameters:

  - `p` (map): the legend-only plot.
  - `options` (map): the outer plot's render options (`:width`, `:height`, `:hint`).

  Returns the legends' natural width plus the plot margin on both sides, a double (only the margins without legends)."
  [p options]
  (let [built (build p (assoc (select-keys options [:width :height :hint]) :until :scale-specs))
        legends (legend/legends built)]
    (measure/call-with-scratch-canvas
     (:hint options)
     (fn [canvas]
       (m/+ (m/* 2.0 (double (get-in built [:theme :plot :margin])))
            (if (seq legends)
              (.x ^fastmath.vector.Vec2 (measure/natural-size {:kind :legends :legends legends} {:canvas canvas :theme (:theme built)}))
              0.0))))))

(defn- resolved-arrange
  "An `:arrange` spec with its `:measured` widths as fixed pixels.

  Parameters:

  - `arrange` (map): the plot `:arrange` - `:plots`, `:widths` (relative weights, `[:fixed px]` or `:measured`).
  - `options` (map): the plot's render options.

  Returns `arrange` with each `:measured` width replaced by `[:fixed px]`, the widest legend-only plot of that column (see [[legend-only-width]]); `arrange` unchanged without measured widths."
  [{:keys [plots widths] :as arrange} options]
  (if-not (some #{:measured} widths)
    arrange
    (let [c (count widths)]
      (assoc arrange :widths
             (vec (map-indexed (fn [^long j w]
                                 (if (= :measured w)
                                   [:fixed (long (m/ceil (reduce m/max 0.0 (map #(legend-only-width % options)
                                                                                (keep-indexed (fn [^long i p] (when (m/== j (m/rem i c)) p)) plots)))))]
                                   w))
                               widths))))))

(defn- grid-stage
  "Pipeline stage `:grid`: the plot's layout grid.

  The grid is the `:arrange` grid (see [[cljplot2.layout.plot-grid/arrange-grid]]), the legend-only grid, the `:custom-grid` (under a title row when `:labels` has `:title`), or one generated from the plot keys (see [[cljplot2.layout.plot-grid/plot-grid]]) with `:labels` over the charts' suggested axis titles (see [[effective-labels]]); a polar plot generates no outer axes (its guides are drawn inside the panel).

  Parameters:

  - `plot` (map): the plot map after the `:scale-specs` stage.

  Returns `plot` with `:grid`.

  Throws `ex-info` with `{:key :aspect}` when `:aspect :iso` meets an `:x` or `:y` scale that isn't linear."
  [{:keys [custom-grid labels stats theme arrange aspect scale-specs] :as plot}]
  (when (and (= :iso aspect)
             (or (not= :linear (get-in scale-specs [:x :kind])) (not= :linear (get-in scale-specs [:y :kind]))))
    (schema/fail! "Plot :aspect :iso needs linear :x and :y scales." {:key :aspect :in [:aspect] :value aspect}))
  (assoc plot :grid (cond
                      arrange (plot-grid/arrange-grid (resolved-arrange arrange (:options plot)) labels theme)
                      (:legend-only plot) (plot-grid/legend-only-grid (legend/legends plot) labels theme)
                      (and custom-grid labels) (plot-grid/titled-custom-grid custom-grid labels theme)
                      custom-grid custom-grid
                      :else (plot-grid/plot-grid (cond-> (assoc plot :labels (effective-labels labels stats))
                                                   ;; polar guides are drawn inside the panel: no outer axes
                                                   (coord/polar? (:coord plot))
                                                   (update :scale-specs (fn [specs] (reduce (fn [acc k] (if (contains? acc k) (assoc-in acc [k :axis] false) acc))
                                                                                           specs [:x :y]))))))))

(defn- panel-content?
  "Tests whether grid item content is a main panel - `:panel` content that isn't a plot margin's.

  Parameters:

  - `content` (map): grid item content.

  Returns `true` for a main panel, else `false`."
  [content]
  (and (= :panel (:kind content)) (nil? (:margin content))))

(declare cartesian-scales polar-scales panel-scales)

(defn finalize-scales
  "Scales of the plot's phase-1 specs finalized for its reference panel.

  The reference panel is the first main `:panel` content of the grid (depth-first) in `layout`: the horizontal scale (`:x`, or `:y` with `:flip true`) gets range `[0 w]`, the vertical one `[h 0]`, each with `:direction` (`:horizontal` / `:vertical`) and a tick hint from the panel size along it and theme `[:axis k :tick-spacing]` (by scale key). All panels share these scales (they differ by at most a pixel in size). A polar plot (`:coord`) gets range-space scales instead (see [[polar-scales]]). Aesthetic scales are passed through unchanged.

  Parameters:

  - `plot` (map): the plot map with `:scale-specs` and `:grid`.
  - `layout` (map or `nil`): the solved layout; `nil` before the first layout round - a provisional finalization (range `[0 1]`, 10 ticks).

  Returns `{dim finalized-scale}`. A `:legend-only` plot has no panel: its position scales stay provisional.

  Throws `ex-info` with `{:key :scales}` when, with a `layout`, there are position scales and the grid has no `:panel` content. See [[panel-scales]], [[cljplot2.scale.position/finalize]]."
  [plot layout]
  (let [specs (into {} (filter (comp position-scales key)) (:scale-specs plot))
        horizontal (scale/horizontal-key (:flip plot))
        panel (when (and layout (seq specs) (not (:legend-only plot)))
                (or (grid/find-content-box (:grid plot) layout panel-content?)
                    (schema/fail! "Plot :scales need a :panel item in the grid." {:key :scales :in [:scales]})))]
    (if (and panel (coord/polar? (:coord plot)))
      (into (into {} (filter (comp aesthetic/dim? key)) (:scale-specs plot)) (polar-scales plot specs horizontal panel))
      (cartesian-scales plot specs horizontal panel))))

(defn panel-scales
  "Per main panel, the finalized scales with position scales fitted to that panel's own box.

  Only for plots whose `:facets` ask for panel-local scales (see [[cljplot2.data.facet/panel-scales?]]): each panel's position scales come from its `:panel-scale-specs` (free / sliced facet scales), else the shared specs.

  Parameters:

  - `plot` (map): the plot map with `:scale-specs`, `:panel-scale-specs` and `:grid`.
  - `layout` (map or `nil`): the solved layout.
  - `shared` (map): the plot's finalized scales (see [[finalize-scales]]).

  Returns `{panel-key scales}`; `nil` without panel-local scales, without a layout, for a legend-only or polar plot (every panel then uses the shared scales)."
  [plot layout shared]
  (when (and layout (facet/panel-scales? (:facets plot)) (not (:legend-only plot)) (not (coord/polar? (:coord plot))))
    (let [specs (into {} (filter (comp position-scales key)) (:scale-specs plot))
          horizontal (scale/horizontal-key (:flip plot))]
      (when (seq specs)
        (into {} (map (fn [{:keys [content box]}]
                        (let [k (:panel content facet/single-panel)]
                          [k (merge shared (cartesian-scales plot (merge specs (get-in plot [:panel-scale-specs k])) horizontal box))])))
              (grid/content-items (:grid plot) layout panel-content?))))))

(defn- cartesian-scales
  "Finalized scales in cartesian coordinates for a panel box.

  Parameters:

  - `plot` (map): the plot map with `:scale-specs` and `:theme`.
  - `specs` (map): the position specs to finalize.
  - `horizontal` (keyword): the scale drawn horizontally, `:x` or `:y`.
  - `panel` (map or `nil`): the panel box `{:w :h}` in whole pixels; `nil` - provisional (range `[0 1]`, 10 ticks), also used for polar plots before a layout.

  Returns the plot's aesthetic scales with the finalized position scales, each with `:direction`. See [[finalize-scales]]."
  [plot specs horizontal panel]
  (into (into {} (filter (comp aesthetic/dim? key)) (:scale-specs plot))
        (map (fn [[k spec]]
               (let [horizontal? (= k horizontal)]
                 [k (assoc (if-not panel
                             (scale/finalize spec [0.0 1.0] provisional-tick-hint)
                             (let [{:keys [^long w ^long h]} panel
                                   spacing (double (get-in plot [:theme :axis k :tick-spacing]))]
                               (if horizontal?
                                 (scale/finalize spec [0.0 (double w)] (scale/tick-hint w spacing))
                                 (scale/finalize spec [(double h) 0.0] (scale/tick-hint h spacing)))))
                           :direction (if horizontal? :horizontal :vertical))])))
        specs))

(defn- theta-label-room
  "Pixels kept around the circle of a polar panel for the angular tick labels.

  Labels sit outside the circle, anchored away from it; the room is the largest extent of a label away from the centre at its angle (its width times the absolute sine plus its height times the absolute cosine) plus the theme's tick length and label gap of the scale.

  Parameters:

  - `plot` (map): the plot map (`:theme`, `:options :hint`).
  - `k` (keyword): the angular scale's key (theme `[:axis k]`).
  - `theta` (map or `nil`): the finalized angular scale.

  Returns the room in pixels, a double; 0 without labels, with `:axis false` or `:axis :grid`."
  [plot k theta]
  (let [labels (when (and theta (scale/axis-drawn? theta)) (seq (map vector (:ticks theta) (:tick-labels theta))))]
    (if-not labels
      0.0
      (let [forward (scale/forward-fn theta)]
        (measure/call-with-scratch-canvas
         (get-in plot [:options :hint])
         (fn [c]
           (m/+ (->> labels
                     (map (fn [[tick t]]
                            (let [^fastmath.vector.Vec2 size (measure/natural-size {:kind :label
                                                                                    :role :tick-label
                                                                                    :text t}
                                                                                   {:canvas c
                                                                                    :theme (:theme plot)})
                                  a (double (forward tick))]
                              (if (m/valid-double? a)
                                (m/+ (m/* (m/abs (m/sin a)) (.x size))
                                     (m/* (m/abs (m/cos a)) (.y size)))
                                0.0))))
                     (reduce m/max 0.0)
                     (double))
                (double (get-in plot [:theme :axis k :label-gap]))
                (double (get-in plot [:theme :axis k :tick-length])))))))))

(defn- polar-scales
  "Finalized position scales of a polar plot for its reference panel box.

  The horizontal scale maps to the angular range in radians (ticks hinted from the circumference; a continuous one neither padded nor niced unless its spec sets `:expand` - the plot's scale or a chart's `:scale-defaults` - or the plot's scale sets `:nice`; pixel room `:padding-px` becomes radians at the outer radius). The vertical scale maps to `[inner radius, radius]`, the radius the largest fitting the sector's bounding box (see [[cljplot2.scale.coord/sector-box]]; a full circle - half the panel's smaller side) into the panel less the angular labels' room (see [[theta-label-room]]) and the charts' `:outer-room` (`[horizontal vertical]` pixels, the largest over the charts) on every side; at least 1 pixel. See [[cljplot2.scale.coord]].

  Parameters:

  - `plot` (map): the plot map (`:coord`, `:scales`, `:stats`, `:theme`).
  - `specs` (map): the position specs.
  - `horizontal` (keyword): the scale on the angles, `:x` or `:y` (`:flip` swaps them).
  - `panel` (map): the panel box `{:w :h}`.

  Returns `{k scale}` of the scales with a spec, each with `:direction` and the panel's polar geometry `:coord`."
  [plot specs horizontal {:keys [^double w ^double h]}]
  (let [coord-spec (:coord plot)
        vertical (if (= horizontal :x) :y :x)
        spacing (fn [k] (double (get-in plot [:theme :axis k :tick-spacing])))
        [^double t0 ^double t1 :as theta-range] (coord/theta-range coord-spec)
        [^double bx0 ^double by0 ^double bx1 ^double by1] (coord/sector-box coord-spec)
        dbx (m/- bx1 bx0)
        dby (m/- by1 by0)
        ;; charts' labels outside the circle (`:outer-room [horizontal vertical]` pixels, the largest)
        outer-rooms (keep :outer-room (:stats plot))
        outer-h (double (reduce m/max 0.0 (map first outer-rooms)))
        outer-v (double (reduce m/max 0.0 (map second outer-rooms)))
        ;; the radius fitting the sector's box into the panel less `room` (plus the charts' outer room) on every side
        fit-radius (fn ^double [^double room]
                     (m/min (m/- (m// w dbx) (m// (m/* 2.0 (m/+ room outer-h)) dbx))
                            (m/- (m// h dby) (m// (m/* 2.0 (m/+ room outer-v)) dby))))
        ;; a continuous angle spans the full circle by default: no padding, no nice (pies), unless asked - `:expand` in
        ;; the spec (the plot's scale or a chart's suggestion), `:nice` in the plot's scale (specs always carry one)
        user (get-in plot [:scales horizontal])
        theta-span (m/abs (m/- t1 t0))
        ;; pixel room (labels past the data) becomes radians at radius `r`
        theta-spec (when-let [spec (get specs horizontal)]
                     (cond-> spec
                       (not (contains? spec :expand)) (dissoc :padding)
                       (not (contains? user :nice)) (assoc :nice false)))
        theta-at (fn [^double r]
                   (when theta-spec
                     (scale/finalize (cond-> theta-spec
                                       (:padding-px theta-spec) (update :padding-px (fn [p] (mapv (fn [^double v]
                                                                                                   (m// v r)) p))))
                                     theta-range (scale/tick-hint (m/* theta-span r) (spacing horizontal)))))
        ;; the label room from a first pass at the whole panel; ticks and room again at the radius it leaves
        first-pass (theta-at (double (fit-radius 0.0)))
        radius (m/max 1.0 (double (fit-radius (theta-label-room plot horizontal first-pass))))
        theta (theta-at radius)
        geometry (coord/geometry coord-spec w h radius)
        inner (double (:inner-radius geometry))
        r (when-let [spec (get specs vertical)]
            (scale/finalize spec [inner radius] (scale/tick-hint (m/- radius inner) (spacing vertical))))]
    (cond-> {}
      theta (assoc horizontal (assoc theta :direction :horizontal :coord geometry))
      r (assoc vertical (assoc r :direction :vertical :coord geometry)))))

(defn- solve-layout
  "The layout of the plot's grid at its render size.

  Measure, solve and finalize scales repeat until the item measurements stop changing, at most [[max-layout-rounds]] times; measurements see the finalized scales, panel scales and margin environments of the previous layout.

  Parameters:

  - `plot` (map): the plot map with `:grid`, `:options` (`:width`, `:height`) and `:theme`.
  - `canvas` (canvas): a scratch canvas for measuring text, see [[cljplot2.graphics.measure]].

  Returns the solved layout (see [[cljplot2.layout.grid/solve]]) with `:rounds` - the number of rounds run."
  [plot canvas]
  (let [{:keys [width height]} (:options plot)
        prepared (grid/prepare (:grid plot))
        box {:x 0 :y 0 :w width :h height}
        measure-for (fn [layout]
                      (grid/measure-items prepared
                                          (let [scales (finalize-scales plot layout)]
                                            {:canvas canvas :theme (:theme plot) :scales scales
                                             :panel-scales (panel-scales plot layout scales)
                                             :margins (margin-env plot layout scales)})
                                          layout))]
    (loop [round (long 1)
           measurements (measure-for nil)]
      (let [layout (grid/solve prepared box measurements)
            remeasured (measure-for layout)]
        (if (or (= measurements remeasured) (m/>= round max-layout-rounds))
          (assoc layout :rounds round)
          (recur (m/inc round) remeasured))))))

(def ^{:const true :tag 'long} max-iso-rounds
  "Upper bound on refits of iso panel sizes (nice domains and measured tracks may change with the panel size), see [[iso-layout]]."
  6)

(defn- span
  "Width of a finalized scale's domain.

  Parameters:

  - `finalized` (map): a finalized continuous scale with a `[a b]` `:domain`.

  Returns the absolute difference of the domain ends, a double."
  ^double [finalized] (let [[^double a ^double b] (:domain finalized)] (m/abs (m/- b a))))

(defn- iso-spacers
  "Whole-pixel spacer totals giving the panels equal pixels per data unit on both axes.

  The panel area available without spacers is fitted by the largest panels with width over x span equal to height over y span; the rest goes to the spacers (both sides of an axis together).

  Parameters:

  - `g` (map): the grid.
  - `layout` (map): its solved layout.
  - `scales` (map): the finalized scales.
  - `[column-total row-total]` (pair of numbers): the current spacer totals.

  Returns `[column row]` spacer totals in whole pixels, at least 0. See [[iso-layout]]."
  [g layout scales [^double column-total ^double row-total]]
  (let [panels (map :box (grid/content-items g layout panel-content?))
        {:keys [^double w ^double h]} (first panels)
        columns (count (distinct (map :x panels)))
        rows (count (distinct (map :y panels)))
        w-avail (m/+ w (m// column-total columns))
        h-avail (m/+ h (m// row-total rows))
        horizontal (if (scale/flipped? scales) :y :x)
        dx (span (get scales horizontal))
        dy (span (get scales (if (= horizontal :x) :y :x)))
        k (m/min (m// w-avail dx) (m// h-avail dy))]
    [(m/max 0 (m/round (m/* (m/- w-avail (m/* k dx)) columns)))
     (m/max 0 (m/round (m/* (m/- h-avail (m/* k dy)) rows)))]))

(defn- iso-mismatch
  "Relative difference of the horizontal and vertical pixels per unit of the first panel.

  Parameters:

  - `g` (map): the grid.
  - `layout` (map): its solved layout.
  - `scales` (map): the finalized scales.

  Returns the absolute difference of the two pixel-per-unit values over the larger one, a double (0 - equal units)."
  ^double [g layout scales]
  (let [{:keys [^double w ^double h]} (grid/find-content-box g layout panel-content?)
        horizontal (if (scale/flipped? scales) :y :x)
        px (m// w (span (get scales horizontal)))
        py (m// h (span (get scales (if (= horizontal :x) :y :x))))]
    (m// (m/abs (m/- px py)) (m/max px py))))

(defn- iso-layout
  "The plot with its panels sized for `:aspect :iso`.

  Starting from the free layout, the spacer tracks around the flexible panel block are sized so the panels get equal pixels per unit on x and y (see [[iso-spacers]], [[cljplot2.layout.plot-grid/iso-grid]]). Refits repeat until the whole-pixel spacers repeat (settled, or cycling when measured tracks change by a pixel with the panel size), at most [[max-iso-rounds]] times, and the round with the smallest mismatch (see [[iso-mismatch]]) is kept. Flexible panels absorb measured track changes, so the grid never overflows.

  Parameters:

  - `plot` (map): the plot map after the `:grid` stage.
  - `canvas` (canvas): a scratch canvas for measuring text.

  Returns `plot` with the chosen `:grid` (fixed spacer tracks) and its `:layout`."
  [plot canvas]
  (let [base-grid (:grid plot)
        candidate (fn [g layout] (let [scales (finalize-scales (assoc plot :grid g) layout)]
                                  {:grid g :layout layout :scales scales :mismatch (iso-mismatch g layout scales)}))]
    (loop [round (long 1)
           seen #{[0 0]}
           current (candidate base-grid (solve-layout plot canvas))
           best nil]
      (let [spacers' (iso-spacers (:grid current) (:layout current) (:scales current)
                                  (if best (:spacers current) [0 0]))
            g' (plot-grid/iso-grid base-grid (spacers' 0) (spacers' 1))
            attempt (assoc (candidate g' (solve-layout (assoc plot :grid g') canvas)) :spacers spacers')
            kept (if (or (nil? best) (m/< (double (:mismatch attempt)) (double (:mismatch best)))) attempt best)]
        (if (or (contains? seen spacers') (m/>= round max-iso-rounds))
          (assoc plot :grid (:grid kept) :layout (:layout kept))
          (recur (m/inc round) (conj seen spacers') attempt kept))))))

(defn- layout-stage
  "Pipeline stage `:layout`: the solved layout of the plot's grid.

  Parameters:

  - `plot` (map): the plot map after the `:grid` stage.

  Returns `plot` with `:layout` (see [[solve-layout]]); with `:aspect :iso` also the `:grid` with fixed spacer tracks (see [[iso-layout]]). Text is measured on a scratch canvas with the plot's rendering hint."
  [plot]
  (measure/call-with-scratch-canvas
   (get-in plot [:options :hint])
   (fn [canvas]
     (if (= :iso (:aspect plot))
       (iso-layout plot canvas)
       (assoc plot :layout (solve-layout plot canvas))))))

(defn- finalized-scales-stage
  "Pipeline stage `:finalized-scales`: scales finalized for the final layout.

  The plot's `:scales` key stays the user's input; the finalized scales go under their own key.

  Parameters:

  - `plot` (map): the plot map after the `:layout` stage.

  Returns `plot` with `:finalized-scales` (see [[finalize-scales]]), `:margin-env` (see [[margin-env]]) and, for panel-local facet scales, `:panel-scales` (see [[panel-scales]])."
  [plot]
  (let [scales (finalize-scales plot (:layout plot))]
    (cond-> (assoc plot :finalized-scales scales :margin-env (margin-env plot (:layout plot) scales))
      (facet/panel-scales? (:facets plot)) (assoc :panel-scales (panel-scales plot (:layout plot) scales)))))

(defn- build-subplot
  "A sub-plot built at its grid item's size.

  Parameters:

  - `p` (map): the sub-plot.
  - `id` (any): its grid item id.
  - `box` (map): the item box `{:w :h}`, pixels.
  - `hint` (keyword): the outer plot's rendering hint.

  Returns the built sub-plot (see [[build]]).

  Throws `ex-info` with `{:key :cvd}` when the sub-plot has `:cvd` in its `:options` (it applies to the whole rendering). Any error is re-thrown as `ex-info` with `:subplot id` and `:subplot-path` (outer item ids first) added, the original as its cause: an `ex-info` keeps its message and data, another exception gets `{:key :subplot}`."
  [p id {:keys [w h]} hint]
  (try (when (some? (get-in p [:options :cvd]))
         (schema/fail! "Option :cvd applies to the whole rendering, not to a sub-plot; give it to the outer plot."
                       {:key :cvd :in [:options :cvd] :value (get-in p [:options :cvd])}))
       (build p {:width w :height h :hint hint})
       (catch Exception e
         (let [data (if (instance? clojure.lang.ExceptionInfo e) (ex-data e) {:key :subplot})
               message (if (instance? clojure.lang.ExceptionInfo e)
                         (ex-message e)
                         (str "Sub-plot failed: " (or (ex-message e) (.getName (class e)))))]
           (throw (ex-info message (assoc data :subplot id :subplot-path (into [id] (:subplot-path data))) e))))))

(declare stage-order stage-extra-keys)

(defn- equal-panel-tracks
  "Fixed track sizes along one axis making the panels of the flexible tracks equal.

  Parameters:

  - `sizes` (vector of numbers): the current track sizes, pixels.
  - `insets` (vector): per track the room around its panel (pixels), `nil` for a fixed track (kept as it is).

  Returns `[[:fixed px] ...]` in track order: the free room (the flexible tracks' sizes less their insets) shared equally, each panel at least a pixel (on a plot too small for its axes the panels stay visible)."
  [sizes insets]
  (let [flexible (keep-indexed (fn [i inset] (when inset i)) insets)
        free (m/- (double (reduce m/+ (map #(nth sizes %) flexible)))
                  (double (reduce m/+ (keep identity insets))))
        ;; at least a pixel: on a plot too small for its axes the panels stay visible (the cells overflow less)
        panel (if (seq flexible) (m/max 1.0 (m/floor (m// free (count flexible)))) 0.0)]
    (mapv (fn [size inset] [:fixed (long (if inset (m/+ panel (double inset)) size))]) sizes insets)))

(defn- arrange-columns
  "Column count of an `:arrange` spec.

  Parameters:

  - `arrange` (map): `:plots`, `:columns`, `:rows`.

  Returns `:columns`, else the plot count over `:rows` rounded up, else 1."
  ^long [{:keys [plots columns rows]}]
  (long (or columns (if rows (long (m/ceil (m// (count plots) (double rows)))) 1))))

(defn- equal-panel-plot
  "The plot of an `:equal-panels` arrange with fixed tracks giving every panel one size.

  Parameters:

  - `plot` (map): the arrange plot (built once).
  - `cells` (sequence of maps): its plot cells `{:id :box}`.
  - `subplots` (map): the aligned built sub-plots by cell id.

  Returns the plot map without stage keys, its `:arrange` `:widths` and `:heights` as `[:fixed px]` tracks (see [[equal-panel-tracks]]; `:measured` and fixed columns kept) and `:equal-panels` dropped."
  [{:keys [arrange] :as plot} cells subplots]
  (let [{:keys [plots rows widths]} arrange
        n (count plots)
        c (arrange-columns arrange)
        r (long (or rows (m/ceil (m// (double n) (double c)))))
        index (fn [[_ ^long k]] [(m/long-quot k c) (m/long-rem k c)])
        by-cell (into {} (map (fn [{:keys [id box]}] [(index id) {:box box :insets (align/panel-insets (subplots id))}])) cells)
        fixed-column? (fn [j] (let [w (get widths j)] (or (= :measured w) (vector? w))))
        column-size (fn [j] (long (or (some #(get-in by-cell [[% j] :box :w]) (range r)) 0)))
        row-size (fn [i] (long (or (some #(get-in by-cell [[i %] :box :h]) (range c)) 0)))
        room (fn [cells-of sides] (let [ins (keep :insets cells-of)] (when (seq ins) (reduce m/max (map #(reduce m/+ (map % sides)) ins)))))
        column-insets (mapv (fn [j] (when-not (fixed-column? j) (or (room (keep #(by-cell [% j]) (range r)) [:left :right]) 0))) (range c))
        row-insets (mapv (fn [i] (or (room (keep #(by-cell [i %]) (range c)) [:top :bottom]) 0)) (range r))]
    (-> (apply dissoc plot (concat stage-order (mapcat val stage-extra-keys)))
        (assoc-in [:arrange :widths] (equal-panel-tracks (mapv column-size (range c)) column-insets))
        (assoc-in [:arrange :heights] (equal-panel-tracks (mapv row-size (range r)) row-insets))
        (update :arrange dissoc :equal-panels))))

(def ^{:private true :const true :tag 'long} max-equal-panel-rounds
  "Upper bound on corrections of `:equal-panels` tracks, see [[equalized-arrange]]."
  3)

(defn- arranged-panel-sizes
  "Panel widths per column and heights per row of a built arrange plot.

  Parameters:

  - `built` (map): the built arrange plot (cells `[:plot k]` in `:subplots`).
  - `columns` (long): its column count.

  Returns `{:columns {j [w ...]} :rows {i [h ...]}}`; cells without a panel (e.g. legend-only) are left out."
  [built ^long columns]
  (reduce (fn [acc [[_ ^long k] sub]]
            (if-let [{:keys [w h]} (grid/find-content-box (:grid sub) (:layout sub) panel-content?)]
              (let [i (m/long-quot k columns) j (m/long-rem k columns)]
                (-> acc (update-in [:columns j] (fnil conj []) w) (update-in [:rows i] (fnil conj []) h)))
              acc))
          {} (:subplots built)))

(defn- corrected-tracks
  "Fixed tracks corrected so every panel reaches the smallest panel size.

  Parameters:

  - `tracks` (vector): `[[kind px] ...]` fixed tracks.
  - `sizes` (map): track index -> the sizes of its panels.

  Returns the tracks with each panel track grown or shrunk by its smallest panel's difference to the smallest panel over all tracks (other tracks kept); `nil` when the panels are already equal."
  [tracks sizes]
  (let [target (double (reduce m/min (mapcat val sizes)))]
    (when-not (every? (fn [^double s] (m/== target s)) (mapcat val sizes))
      (vec (map-indexed (fn [t [kind ^long px]]
                          (if-let [panel (some->> (get sizes t) (reduce m/min))]
                            [kind (m/+ px (m/- (long target) (long panel)))]
                            [kind px]))
                        tracks)))))

(defn- equalized-arrange
  "A built `:equal-panels` arrange with every panel the same size.

  The fixed tracks of [[equal-panel-plot]] come from the first layout's insets (alignment padding included), which a rebuild at the new sizes may change by a few pixels; each round measures the panels (see [[arranged-panel-sizes]]) and corrects the tracks (see [[corrected-tracks]]), at most [[max-equal-panel-rounds]] times.

  Parameters:

  - `built` (map): the built arrange plot with fixed tracks.
  - `rebuild` (function): builds a plot map, `(rebuild plot)`.

  Returns the last built plot - with equal panels, or after the last round."
  [built rebuild]
  (let [columns (arrange-columns (:arrange built))]
    (loop [current built
           round (long 1)]
      (let [{panel-columns :columns panel-rows :rows} (arranged-panel-sizes current columns)
            widths (when panel-columns (corrected-tracks (get-in current [:arrange :widths]) panel-columns))
            heights (when panel-rows (corrected-tracks (get-in current [:arrange :heights]) panel-rows))]
        (if (or (m/> round max-equal-panel-rounds) (and (nil? widths) (nil? heights)))
          current
          (recur (rebuild (cond-> (apply dissoc current (concat stage-order (mapcat val stage-extra-keys)))
                            widths (assoc-in [:arrange :widths] widths)
                            heights (assoc-in [:arrange :heights] heights)))
                 (m/inc round)))))))

(defn- subplots-stage
  "Pipeline stage `:subplots`: every plot item of the grid built at its box size.

  Items of nested grids are included; items whose box is smaller than 1x1 pixel are skipped. For `:arrange` (unless `{:align false}`) the sub-plots' panels are aligned in rows and columns (see [[cljplot2.layout.align/align-panels]]); with `:equal-panels` the plot is rebuilt with equal panels (see [[equalized-arrange]]).

  Parameters:

  - `plot` (map): the plot map after the `:finalized-scales` stage.

  Returns `plot` with `:subplots {item-id built-plot}`; for `:equal-panels` the rebuilt plot.

  Throws `ex-info` with `{:key :id}` when plot item ids repeat across the grid tree; sub-plot errors as in [[build-subplot]]."
  [{:keys [grid layout options] :as plot}]
  (let [items (grid/content-items grid layout subplot/plot-content?)
        hint (:hint options)]
    (when-let [dup (first (for [[id ^long n] (frequencies (map :id items)) :when (m/> n 1)] id))]
      (schema/fail! "Plot item ids must be unique across nested grids." {:key :id :value dup}))
    (let [cells (vec (keep (fn [{:keys [id content box]}]
                             (when (and (m/pos? (long (:w box))) (m/pos? (long (:h box))))
                               {:id id :box box :plot (:plot content)
                                :built (build-subplot (:plot content) id box hint)}))
                           items))
          subplots (if (and (:arrange plot) (get-in plot [:arrange :align] true))
                     (align/align-panels cells (fn [{:keys [id box] p :plot} padding]
                                                 (build-subplot (assoc p :alignment-padding padding) id box hint)))
                     (into {} (map (juxt :id :built)) cells))]
      (if (get-in plot [:arrange :equal-panels])
        (let [rebuild #(build % (select-keys options [:width :height :hint]))]
          (equalized-arrange (rebuild (equal-panel-plot plot cells subplots)) rebuild))
        (assoc plot :subplots subplots)))))

(def stage-order
  "Pipeline stages in execution order."
  [:split :stats :extents :scale-specs :grid :layout :finalized-scales :subplots])

(def ^:private stage-extra-keys
  "Per stage, the plot keys it adds besides its own (each stage stores its result under its name): dropped with the stage keys before a rebuild, so a stage adding a key lists it here."
  {:stats [:margin-stats] :scale-specs [:panel-scale-specs] :finalized-scales [:margin-env :panel-scales]})

(def ^:private stage-fns
  "Stage key to its stage function (plot map to plot map with the stage's key added)."
  {:split split-stage
   :stats stats-stage
   :extents extents-stage
   :scale-specs (comp panel-scale-specs-stage scale-specs-stage)
   :grid grid-stage
   :layout layout-stage
   :finalized-scales finalized-scales-stage
   :subplots subplots-stage})

;; validation

(def ^:private chart-schemas
  "Schema of a chart value's `:schema`: malli schemas of its options, theme style and merged mapping."
  [:map {:closed true}
   (schema/optional :options {:description "schema of the chart's options"} schema/malli-schema)
   (schema/optional :style {:description "schema of the chart's theme style [:chart kind]"} schema/malli-schema)
   (schema/optional :mapping {:description "schema of the merged mapping"} schema/malli-schema)])

(def chart-schema
  "Schema of a chart value, see [[cljplot2.chart]]; open (a chart value may carry the user's own keys)."
  [:map {:cljplot2/type-doc "a chart value (see cljplot2.chart)"}
   (schema/nilable :kind {:description "chart kind, the key of its theme style [:chart kind]"} keyword?)
   (schema/nilable :options {:description "the chart's options"} :map)
   [:compute {:description "(compute chart split env) - the chart's statistics for every panel"} schema/function]
   [:draw {:description "(draw chart computed canvas box scales env) - draws one panel"} schema/function]
   (schema/nilable :legend {:description "(legend chart attributes canvas box env) - draws a legend key glyph"} schema/function)
   (schema/optional :inherit-mapping {:description "which plot mapping keys the chart takes: all, or only the facet keys (annotations)"
                                      :default :all}
                    [:enum :all :facets])
   (schema/optional :coords {:description "coordinate systems the chart draws in" :cljplot2/default-doc "#{:cartesian}"}
                    [:set [:enum :cartesian :polar :three-d]])
   (schema/nilable :primitives {:description "3D charts: (primitives chart computed point scales env) - the panel's 3D primitives"}
                   schema/function)
   (schema/nilable :schema {:description "malli schemas of the chart's :options, :style and :mapping, checked by build"}
                   chart-schemas)])

(def scales-schema
  "Schema of plot `:scales`: position (`:x`, `:y`), facet and aesthetic scale specs."
  (-> [:map {:closed true}]
      (into (map #(schema/optional % {:description (str "the " (name %) " position scale")} scale/scale-spec-schema)) [:x :y])
      (into (map #(schema/optional % {:description (str "the " (name %) " facet levels (order, strip text)")} facet/scale-spec-schema))
            [:facet :facet-col :facet-row])
      (into (map #(schema/optional % {:description (str "the " (name %) " aesthetic scale")} (aesthetic/scale-spec-schema %)))
            aesthetic/dims)))

(def margin-spec-schema
  "Schema of a plot margin as a map `{:charts c :size px :axis bool}` (the other forms of a margin are its charts alone)."
  [:map {:closed true}
   [:charts {:description "a chart or a vector of charts"}
    [:multi {:dispatch chart/chart-value?} [true chart-schema] [false [:vector {:min 1} chart-schema]]]]
   (schema/optional :size {:description "margin thickness in pixels" :default 80} schema/pos)
   (schema/optional :axis {:description "draw the magnitude axis of histogram and density margins" :default false} schema/bool)])

(def ^:private margin-schema
  "Schema of one plot margin: a chart, a vector of charts or a [[margin-spec-schema]] map."
  [:multi {:dispatch #(cond (chart/chart-value? %) :chart (map? %) :spec :else :charts)
           :cljplot2/type-doc "a chart, a vector of charts or {:charts c :size px :axis bool}"}
   [:chart chart-schema]
   [:charts [:vector {:min 1} chart-schema]]
   [:spec margin-spec-schema]])

(def margins-schema
  "Schema of plot `:margins`: `{:top m :right m}`, `m` - a chart, a vector of charts or `{:charts c :size px :axis bool}`."
  [:map {:closed true}
   (schema/optional :top {:description "charts above the panel sharing its horizontal scale"} margin-schema)
   (schema/optional :right {:description "charts right of the panel sharing its vertical scale"} margin-schema)])

(def plot-schema
  "Schema of a plot map (keys in the namespace doc); `:theme` and `:options` are checked merged ([[cljplot2.theme/theme-schema]], [[options-schema]])."
  [:map {:closed true}
   (schema/nilable :data {:cljplot2/section :content :description "rows (a sequence of maps) or columns (a map of sequences) the charts read"} data/data-schema)
   (schema/nilable :mapping {:cljplot2/section :content :description "accessors of the dimensions the charts read (keywords or functions), merged under each chart's"}
                   data/mapping-schema)
   (schema/nilable :charts {:cljplot2/section :content :description "the charts, drawn in order"} [:vector chart-schema])
   (schema/nilable :scales {:cljplot2/section :scales :description "position, facet and aesthetic scale specs"} scales-schema)
   (schema/nilable :facets {:cljplot2/section :layout :description "facet layout: wrap shape, order, NA panel, scale relations, strips, panel weights"}
                   facet/facet-options)
   (schema/nilable :groups {:cljplot2/section :content :description "grouping options"}
                   [:map {:closed true}
                    (schema/nilable :na {:description "nil aesthetic values form the NA level (false - such rows are dropped)" :default true}
                                    schema/bool)])
   (schema/nilable :legend {:cljplot2/section :legend :description "draw the legends (false hides all)" :default true} schema/bool)
   (schema/nilable :legend-only {:cljplot2/section :legend :description "draw only the legends (a legend cell of an :arrange)" :default false} schema/bool)
   (schema/nilable :alternate-panel {:cljplot2/section :style :description "shade a single panel as an odd facet panel (theme [:panel :alternate-fill])"
                                     :default false}
                   schema/bool)
   (schema/nilable :aspect {:cljplot2/section :coordinates :description ":iso - equal pixels per data unit on x and y (linear scales)"} [:enum :iso])
   (schema/nilable :flip {:cljplot2/section :coordinates :description "draw the :x scale vertically and :y horizontally" :default false} schema/bool)
   (schema/nilable :coord {:cljplot2/section :coordinates :description "coordinate system: cartesian or polar" :cljplot2/default-doc "cartesian"} coord/coord-schema)
   (schema/nilable :labels {:cljplot2/section :titles :description "plot title and axis titles"} plot-grid/labels-schema)
   (schema/nilable :custom-grid {:cljplot2/section :layout :description "a hand-written layout grid instead of the generated one (only :labels {:title t} beside it)"}
                   :map)
   (schema/nilable :arrange {:cljplot2/section :layout :description "a multiplot: sub-plots in a grid (only :labels {:title t} beside it)"}
                   plot-grid/arrange-schema)
   (schema/nilable :margins {:cljplot2/section :layout :description "charts beside the panel sharing its scales (marginal plots)"} margins-schema)
   (schema/optional :alignment-padding {:description "pixels added to the generated grid's margins by multiplot alignment"
                                        :cljplot2/internal true}
                    [:map-of keyword? number?])
   (schema/nilable :options {:cljplot2/section :render :description "render options, merged under the ones given to build / render"} :map)
   (schema/optional :theme {:cljplot2/section :style :description "theme overrides, deep-merged over the default theme"} any?)])

(def options-schema
  "Schema of the merged render options of `build` (see [[default-options]]): image size and hint, `:debug` and `:cvd` (used by [[cljplot2.render/render]]) and `:until`."
  [:map {:closed true :cljplot2/merged true}
   [:width {:cljplot2/section :image :description "target width in pixels" :default 600} schema/pos-int]
   [:height {:cljplot2/section :image :description "target height in pixels" :default 600} schema/pos-int]
   [:hint {:cljplot2/section :image :description "rendering quality hint (antialiasing and text metrics)" :default :high} (into [:enum] (sort measure/rendering-hints))]
   (schema/nilable :debug {:cljplot2/section :inspection :description "paint every layout item's box (a distinct colour, its outline and id) under its content"
                           :default false}
                   schema/bool)
   (schema/nilable :cvd {:cljplot2/section :color-vision :description "render as seen with a colour vision deficiency"} schema/cvd-deficiency)
   (schema/optional :until {:cljplot2/section :inspection :description "build stops after this pipeline stage (inspect the intermediate data)"}
                    (into [:enum] stage-order))])

(def default-options
  "Render options used when neither the plot nor `build` gives them: the `:default`s of the image entries of [[options-schema]] - `:width`, `:height` (target size in pixels) and `:hint` (rendering hint, see [[cljplot2.graphics.measure/rendering-hints]])."
  (into {} (keep (fn [[k props]] (when (= :image (:cljplot2/section props)) [k (:default props)])))
        (filter vector? (rest options-schema))))

(defn- merged-mappings
  "The merged mapping of every chart of the plot.

  Parameters:

  - `plot` (map): the plot map (`:mapping`, `:charts`).

  Returns a vector with each chart's mapping, see [[chart-mapping]]."
  [{:keys [mapping charts]}]
  (mapv #(chart-mapping mapping %) charts))

(defn- facet-scale-errors
  "Errors of plot `:facets` options asking for panel-local scales.

  Free / sliced relations and panel `:widths` / `:heights` need facets and the generated cartesian grid, not `:aspect :iso` or polar coordinates, and no explicit `:domain` on a free / sliced scale.

  Parameters:

  - `plot` (map): the plot map.
  - `faceted?` (boolean): some chart has facet keys.
  - `error` (function): `(error in value message)` making an error.

  Returns a sequence with at most one error (the first problem), `nil` without panel-local scales or problems."
  [{:keys [facets scales aspect custom-grid arrange] :as plot} faceted? error]
  (when (facet/panel-scales? facets)
    (let [problem (cond
                    (not faceted?) "free / sliced scales and panel :widths / :heights need facets"
                    (or custom-grid arrange) "free / sliced scales and panel :widths / :heights need the generated grid"
                    (= :iso aspect) "free / sliced scales and panel :widths / :heights can't be combined with :aspect :iso"
                    (coord/polar? (:coord plot)) "free / sliced scales and panel :widths / :heights can't be combined with polar coordinates"
                    :else (some (fn [k] (when (and (not= :same (facet/scale-relation facets k)) (some? (get-in scales [k :domain])))
                                          (str "a " (facet/scale-relation facets k) " " k " scale takes its domains from the data, it can't have a :domain")))
                                [:x :y]))]
      (when problem [(error [:facets] facets problem)]))))

(defn- three-d?
  "Tests whether a chart is a 3D chart (`:three-d` among its `:coords`).

  Parameters:

  - `c` (map): a chart value.

  Returns `true` for a 3D chart, else `false`."
  [c]
  (contains? (get c :coords) :three-d))

(defn- three-d-contract-errors
  "Errors of 3D charts without a `:primitives` function.

  The panel draws every 3D chart's primitives in one scene (see [[cljplot2.graphics.space3d/draw-scene!]]).

  Parameters:

  - `charts` (sequence of maps): the plot's charts.
  - `error` (function): `(error in value message)` making an error.

  Returns a sequence of errors at `[:charts i :primitives]`."
  [charts error]
  (keep-indexed (fn [i c] (when (and (three-d? c) (not (fn? (:primitives c))))
                            (error [:charts i :primitives] (:kind c) "a 3D chart needs a :primitives function")))
                charts))

(defn- three-d-errors
  "Errors of a plot with 3D charts.

  A 3D plot takes only 3D charts; several of them each with an explicit `:domain` of `:x`, `:y` and `:z` (their boxes must match); no `:flip`, `:aspect :iso`, `:margins` or free / sliced facet scales. Polar coordinates are refused by the charts' `:coords`. See [[cljplot2.chart.cloud]].

  Parameters:

  - `plot` (map): the plot map (`:charts`, `:flip`, `:aspect`, `:margins`, `:facets`).
  - `error` (function): `(error in value message)` making an error.

  Returns a sequence of errors; `nil` without 3D charts."
  [{:keys [charts flip aspect margins facets]} error]
  (let [three (filter three-d? charts)]
    (when (seq three)
      (concat
       (when (not-every? three-d? charts)
         [(error [:charts] (mapv :kind charts) "3D charts can't share a plot with 2D charts")])
       (when (and (m/> (count three) 1)
                  (not-every? #(every? (fn [k] (get-in % [:options :domain k])) [:x :y :z]) three))
         [(error [:charts] (mapv :kind three) "several 3D charts need an explicit :domain {:x :y :z} each (their boxes must match)")])
       (when flip [(error [:flip] flip "3D charts lay out their own panel: no :flip")])
       (when (= :iso aspect) [(error [:aspect] aspect "3D charts lay out their own panel: use the charts' :aspect, not :aspect :iso")])
       (when (seq margins) [(error [:margins] margins "3D charts can't have plot margins")])
       (when (some #(not= :same (facet/scale-relation facets %)) [:x :y])
         [(error [:facets] facets "3D charts share their domains across panels: no free / sliced facet scales")])))))

(defn- rule-errors
  "Errors of the rules across plot keys, for a plot whose keys are valid.

  The rules: charts' facet mappings and one facet mapping for all; a computed aesthetic mapped by no other chart; aesthetic and facet scales need a mapping of their key; `:custom-grid` / `:arrange` / `:legend-only` combinations (with labels, `:aspect :iso`, `:flip`, polar coordinates or margins); polar coordinates only with charts supporting them; panel-local facet scales (see [[facet-scale-errors]]); 3D plots (see [[three-d-errors]], [[three-d-contract-errors]]); margins without facets and a margin axis only with a histogram or density chart.

  Parameters:

  - `plot` (map): the plot map.

  Returns a sequence of errors `{:in :value :message}`, empty when every rule holds. See [[validate!]]."
  [{:keys [scales labels custom-grid arrange aspect flip charts] :as plot}]
  (let [mappings (merged-mappings plot)
        dim-sets (distinct (remove empty? (map facet/facet-dims mappings)))
        error (fn [in value message] {:in in :value value :message message})]
    (concat
     (keep-indexed (fn [i m] (when-let [problem (facet/mapping-problem m)]
                              (error [:charts i :options :mapping] (facet/facet-dims m) problem)))
                   mappings)
     (when (m/> (count dim-sets) 1)
       [(error [:charts] (vec dim-sets) "charts should use the same facet mapping")])
     (for [[^long j chart] (map-indexed vector charts)
           k (keys (aesthetic/computed-aesthetics chart (nth mappings j)))
           [^long i m] (map-indexed vector mappings)
           :when (and (m/not== i j) (some? (get m k)))]
       (error [:charts i :options :mapping k] (get m k)
              (str "chart " j " computes " k " itself; another chart can't map it (set it to nil in this chart's :mapping)")))
     (let [margin-mappings (when (empty? (schema/errors margins-schema (:margins plot) [:margins]))
                             (for [[_ {margin-charts :charts}] (plot-margins plot) c margin-charts] (get-in c [:options :mapping])))]
       (for [k (keys scales)
             :when (and (aesthetic/dim? k) (not-any? #(some? (get % k)) (concat mappings margin-mappings))
                        (not-any? (fn [[chart m]] (contains? (aesthetic/computed-aesthetics chart m) k))
                                  (map vector charts mappings)))]
         (error [:scales k] (get scales k) "aesthetic scale needs a chart mapping its key")))
     (for [k (keys scales)
           :when (and (facet/facet-keys k) (not (some #{k} (first dim-sets))))]
       (error [:scales k] (get scales k) "facet scale needs a facet mapping of its key"))
     (when (and custom-grid arrange)
       [(error [:arrange] arrange "can't be combined with :custom-grid")])
     (when (and (:legend-only plot) (or custom-grid arrange))
       [(error [:legend-only] (:legend-only plot) "a legend-only plot has its own grid, not :custom-grid or :arrange")])
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
     (facet-scale-errors plot (seq dim-sets) error)
     (three-d-errors plot error)
     (three-d-contract-errors charts error)
     (when (or custom-grid arrange)
       (for [k (keys labels) :when (not= k :title)]
         (error [:labels k] (get labels k) "only :title is allowed with :custom-grid or :arrange")))
     (when (seq (:margins plot))
       (concat
        (when (seq dim-sets)
          [(error [:margins] (:margins plot) "margins can't be combined with facets (yet)")])
        (when (or custom-grid arrange)
          [(error [:margins] (:margins plot) "margins belong on the plots inside :arrange / :custom-grid")])
        (for [[side {:keys [axis] margin-charts :charts}] (plot-margins plot)
              :when (and axis (not-any? (comp magnitude-chart-kinds :kind) margin-charts))]
          (error [:margins side :axis] axis "a margin axis needs a histogram or density chart (strips have no magnitude)")))))))

(defn- named-style-names
  "Schema of a chart's `:theme-style`: one of the named styles of the theme.

  Parameters:

  - `chart-theme` (map): the merged theme's `:chart` map.

  Returns an `[:enum ...]` of its keys that aren't built-in chart kinds - the registered [[cljplot2.theme/named-styles]] and those a theme adds, sorted."
  [chart-theme]
  (into [:enum] (sort (remove #(contains? chart/builtin-styles %) (keys chart-theme)))))

(defn- kind-style-map?
  "Tests whether the theme style of a chart kind is a map or absent.

  A kind style that isn't a map is the kind's own error: no named style is checked over it (see [[named-style-checks]]).

  Parameters:

  - `chart-theme` (map): the merged theme's `:chart` map.
  - `kind` (keyword): the chart kind.

  Returns true for a map or no entry, else false."
  [chart-theme kind]
  (let [base (get chart-theme kind)]
    (or (nil? base) (map? base))))

(defn- merged-named-style
  "The named style `n` of `chart-theme` (the merged theme's `:chart` map) merged over the style of chart `kind`; a named entry that isn't a map as it is (its own error)."
  [chart-theme kind n]
  (let [named (get chart-theme n)]
    (if (map? named) (merge (get chart-theme kind) named) named)))

(defn- chart-theme-checks
  "Checks of the chart styles of a theme, independent of any plot.

  Parameters:

  - `chart-theme` (map): the merged theme's `:chart` map.

  Returns a sequence of `[schema value in]` checks: every built-in kind's style `[:chart kind]` against its style schema (see [[cljplot2.chart/builtin-styles]]), and every registered named style ([[cljplot2.theme/named-styles]]) merged over its kind's style against that kind's schema (skipped when the kind's style isn't a map)."
  [chart-theme]
  (concat
   (for [[k style] chart/builtin-styles] [style (get chart-theme k) [:theme :chart k]])
   (for [[n kind] theme/named-styles
         :when (kind-style-map? chart-theme kind)]
     [(chart/builtin-styles kind) (merged-named-style chart-theme kind n) [:theme :chart n]])))

(defonce ^{:private true :doc "`[chart-theme errors]` of the last [[chart-theme-errors]] call."}
  last-chart-theme-errors
  (atom nil))

(defn- chart-theme-errors
  "Errors of the chart styles of a theme (see [[chart-theme-checks]]); the result for the last chart theme is reused for an equal one (plots built in a row usually share it).

  Parameters:

  - `chart-theme` (map): the merged theme's `:chart` map.

  Returns a vector of errors, empty when every style holds."
  [chart-theme]
  (let [[checked cached-errs] @last-chart-theme-errors]
    (if (and (some? checked) (= checked chart-theme))
      cached-errs
      (let [errs (vec (mapcat (fn [[sch v in]] (schema/errors sch v in)) (chart-theme-checks chart-theme)))]
        (reset! last-chart-theme-errors [chart-theme errs])
        errs))))

(defn- named-style-checks
  "Checks of the `:theme-style` of the charts.

  For each chart with `:theme-style s`: `s` names a style of the theme that isn't a chart kind (else an error at the option, listing the named styles), and that style merged over the chart's kind style holds the chart's style schema - except a registered named style on a chart of its own kind, checked with the theme (see [[chart-theme-checks]]).

  Parameters:

  - `chart-theme` (map): the merged theme's `:chart` map.
  - `charts` (sequence): `[prefix i chart]` - the chart at path `(conj prefix i)`, e.g. `[[:charts] 0 c]` or `[[:margins :top :charts] 1 c]`.

  Returns a sequence of `[schema value in]` checks, see [[chart-checks]]."
  [chart-theme charts]
  (for [[prefix i {:keys [kind options] :as c}] charts
        :let [s (:theme-style options)
              style (get-in c [:schema :style])]
        :when s
        check (cond
                (or (contains? chart/builtin-styles s) (not (contains? chart-theme s)))
                [[(named-style-names chart-theme) s (conj prefix i :options :theme-style)]]

                (= kind (get theme/named-styles s)) nil

                (and style (kind-style-map? chart-theme kind))
                [[style (merged-named-style chart-theme kind s) [:theme :chart s]]])]
    check))

(defn- text-role-checks
  "Checks of the `:role` of the text charts.

  Parameters:

  - `text-roles` (map): the merged theme's `:text` map (role -> style).
  - `charts` (sequence): `[prefix i chart]`, see [[named-style-checks]].

  Returns a sequence of `[schema value in]` checks: each text chart's `:role` is a key of `text-roles`; empty when `text-roles` isn't a map."
  [text-roles charts]
  (for [[prefix i {:keys [kind options]}] charts
        :let [role (:role options)]
        :when (and (= :text kind) role (map? text-roles))]
    [(into [:enum] (sort (keys text-roles))) role (conj prefix i :options :role)]))

(defn- chart-checks
  "Checks of the charts' schemas against the plot and its merged theme.

  The checks: the theme style of user kinds present in the theme and the plot (built-in kinds and registered named styles: [[chart-theme-errors]]), the charts' named styles (see [[named-style-checks]]), text roles (see [[text-role-checks]]), each chart's options and merged mapping, and the options of margin charts (their mappings are built from the plot's at compute).

  Parameters:

  - `plot` (map): the plot map (valid keys).
  - `theme` (map): the merged theme.

  Returns a sequence of distinct `[schema value in]` checks, `in` the error path of `value`."
  [{:keys [charts] :as plot} theme]
  (let [chart-theme (when (map? theme) (:chart theme))]
    (distinct
     (concat
      (when (map? chart-theme)
        (let [placed (concat (map-indexed (fn [i c] [[:charts] i c]) charts)
                             (for [[side margin] (when (empty? (schema/errors margins-schema (:margins plot) [:margins]))
                                                   (plot-margins plot))
                                   [i c] (map-indexed vector (:charts margin))]
                               [[:margins side :charts] i c]))]
          (concat (named-style-checks chart-theme placed)
                  (text-role-checks (:text theme) placed))))
      (mapcat (fn [i {:keys [kind] :as c} m]
                (let [{:keys [options style mapping]} (:schema c)]
                  (cond-> []
                    options (conj [options (:options c) [:charts i :options]])
                    mapping (conj [mapping m [:charts i :options :mapping]])
                    (and style (map? chart-theme) (not (contains? chart/builtin-styles kind)) (contains? chart-theme kind))
                    (conj [style (get chart-theme kind) [:theme :chart kind]]))))
              (range) charts (merged-mappings plot))
      ;; margin charts: their options (mappings are built from the plot's at compute)
      (for [[side {margin-charts :charts}] (when (empty? (schema/errors margins-schema (:margins plot) [:margins]))
                                             (plot-margins plot))
            [i c] (map-indexed vector margin-charts)
            :let [chart-options-schema (get-in c [:schema :options])]
            :when chart-options-schema]
        [chart-options-schema (:options c) [:margins side :charts i :options]])))))

(defn- validate!
  "Validate a plot, its merged theme and render options before any stage runs.

  The plot (stage keys removed), the render options and the merged theme are checked against their schemas; when the plot's keys hold, the theme's chart styles (see [[chart-theme-errors]]) and the charts' schemas (see [[chart-checks]]); when all of those hold, the rules across plot keys (see [[rule-errors]]).

  Parameters:

  - `plot` (map): the plot map.
  - `theme` (map): the merged theme.
  - `options` (map): the merged render options.

  Returns `nil` when everything holds.

  Throws one `ex-info` listing every error of the failing round, see [[cljplot2.schema/throw-errors!]]."
  [plot theme options]
  (let [plot-errors (schema/errors plot-schema plot [])
        errors (concat (schema/errors options-schema options [:options])
                       plot-errors
                       (theme/errors theme)
                       (when (empty? plot-errors)
                         (concat (when (map? (:chart theme)) (chart-theme-errors (:chart theme)))
                                 (mapcat (fn [[s v in]] (schema/errors s v in)) (chart-checks plot theme)))))]
    (schema/throw-errors! (if (seq errors) errors (rule-errors plot)))))

;; build

(defn- type-kind
  "A scale spec with its declared `:type` resolved to a kind.

  Categorical gives `:bands` for `:x` / `:y`, `:ordinal` for aesthetics and nothing for facets; numeric gives `:linear` for aesthetics and nothing for `:x` / `:y` (numbers are continuous there by default).

  Parameters:

  - `k` (keyword): the scale's dimension.
  - `scale-type` (keyword): `:categorical` or `:numeric`.
  - `spec` (map): the plot's scale spec.

  Returns `spec` without `:type`, with `:kind` added when it has none and the type implies one.

  Throws `ex-info` with `{:key :type}` when an explicit `:kind` contradicts the type (an aesthetic `:binned` kind is numeric; `:identity` takes no `:type`)."
  [k scale-type spec]
  (let [wanted (cond
                 (position-scales k) (case scale-type :categorical :bands :numeric nil)
                 (aesthetic/dim? k) (case scale-type :categorical :ordinal :numeric :linear)
                 :else nil)
        allowed (if (and (aesthetic/dim? k) (= :numeric scale-type)) #{:linear :binned} #{wanted})
        given (:kind spec)
        contradiction? (cond
                         (and given wanted) (not (contains? allowed given))
                         (and given (position-scales k) (= :numeric scale-type)) (= :bands given)
                         :else false)]
    (when contradiction?
      (schema/fail! (str "Scale :type " scale-type " contradicts :kind " given ".") {:key :type :in [:scales k :type] :value scale-type}))
    (cond-> (dissoc spec :type)
      (and wanted (nil? given)) (assoc :kind wanted))))

(defn- resolve-scale-types
  "Plot scales with every declared `:type` resolved to a kind.

  A categorical mapping marker ([[cljplot2.data.levels/categorical]]) in any chart's mapping declares its dimension's scale categorical - except a position marker in a plot of 3D charts (no position scales; the charts read the marker, see [[cljplot2.chart.cloud/axis-levels]]).

  Parameters:

  - `scales` (map): the plot `:scales`.
  - `mappings` (sequence of maps): the charts' merged mappings.
  - `three-d-plot?` (boolean): the plot has 3D charts.

  Returns the scales with declared and marked types resolved (see [[type-kind]]); a marked dimension without a spec gets one.

  Throws `ex-info` with `{:key :type}` for a marker on a dimension declared `:type :numeric`."
  [scales mappings three-d-plot?]
  (let [scale-dim? #(or (and (position-scales %) (not three-d-plot?)) (aesthetic/dim? %) (contains? facet/facet-keys %))
        marked (set (for [m mappings [k acc] m :when (and (scale-dim? k) (accessor/categorical? acc))] k))]
    (reduce (fn [acc k]
              (let [spec (get scales k {})
                    declared (:type spec)]
                (when (and (marked k) (= :numeric declared))
                  (schema/fail! "A categorical mapping marker contradicts the scale's :type :numeric."
                                {:key :type :in [:scales k :type] :value declared}))
                (if-let [resolved-type (if (marked k) :categorical declared)]
                  (assoc acc k (type-kind k resolved-type spec))
                  acc)))
            scales
            (distinct (concat (filter scale-dim? (keys scales)) marked)))))

(defn build
  "Run a plot through the pipeline stages and return the plot map with each stage's key added.

  Stage keys (and the keys stages derive, e.g. margins') already present in `plot` - from an earlier `build` - are dropped first. Before any stage runs, the plot, its merged theme and options and its charts' schemas are validated (see [[validate!]]).

  Parameters:

  - `plot` (map): the plot map (keys in the namespace doc); its `:theme` is stored merged over [[cljplot2.theme/default-theme]].
  - `options` (map, optional): render options merged over `(:options plot)` and [[default-options]], stored under `:options` - `:width`, `:height` (pixels, default 600), `:hint` (default `:high`), `:debug`, `:cvd`, and `:until` - the last stage to run (default the last, see [[stage-order]]).

  Returns the built plot map.

  Throws one `ex-info` listing every validation error (see [[cljplot2.schema]]), and stage errors as `ex-info` with `{:key ..}`."
  ([plot] (build plot {}))
  ([plot options]
   (let [unbuilt (if (map? plot) (apply dissoc plot (concat stage-order (mapcat val stage-extra-keys))) plot)
         plot-options (when (map? unbuilt) (:options unbuilt))
         render-options (merge default-options (when (map? plot-options) plot-options) (dissoc options :until))
         theme (theme/plot-theme (when (map? unbuilt) (:theme unbuilt)))]
     (validate! unbuilt theme (cond-> render-options (contains? options :until) (assoc :until (:until options))))
     (reduce (fn [staged stage] ((stage-fns stage) staged))
             (assoc unbuilt :options render-options :theme theme
                    :scales (resolve-scale-types (:scales unbuilt) (merged-mappings unbuilt) (some three-d? (:charts unbuilt))))
             (take (m/inc (.indexOf ^java.util.List stage-order (get options :until (peek stage-order)))) stage-order)))))
