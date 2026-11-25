(ns cljplot2.chart.histogram
  "Histogram: counts of `:x` values in bins shared by all groups and by the panels sharing an x scale (lattice-style
  common breaks; per panel under free or sliced facet x scales).

  Options (style defaults from theme `[:chart :histogram]`):

  * `:bins` - a fastmath bin-estimation method keyword (`:sturges` default, `:sqrt`, `:rice`, `:doane`, `:scott`,
    `:freedman-diaconis`; estimated on the values binned together), a positive integer (equal-width bins over their
    range), or a vector of at least 2 increasing breaks (shared by every panel; values outside are dropped and
    counted). Values are binned together over all panels, except under a free or sliced facet x scale (compute env
    `:panel-scale-groups`): there the panels sharing an x scale (each panel of a wrap layout, a column or row of a
    facet grid) are binned together over their own values, so the bins span the panel's own range (also margin
    histograms, which follow their panel's scale); see [[panel-breaks]]
  * `:y` - bar height: `:percent` (default, of the panel's total, or the group's when grouped), `:count`, `:pmf`
    (share of the total, sums to 1), `:density` (count / (n * width), area 1), `:cumulative` (the count of the bin and
    of every bin before it)
  * `:position` - grouped bars (see [[cljplot2.data.position]]): `:overlay` (default; full-width, fill alpha
    `:alpha`), `:dodge` (bins split among the groups), `:stack` (groups on top of each other), `:normalize` (stacked, each
    bin summing to 1; y title \"Proportion\"), `:mirror` (exactly two groups, the second drawn downward; y tick labels
    show magnitudes)
  * `:zero-line` - mirror only: draw the y = 0 line on top of the bars (default true; `:zero-line-color`,
    `:zero-line-width`)
  * `:symmetric` - mirror only: `true` ties both halves to one y range `[-M M]`, `M` the tallest bar of either group
    over all panels, so the halves compare visually (default false)
  * `:color` (fill), `:stroke-color` (bar outline: a color, `:none` or `:darker` - the darkened fill, default),
    `:stroke-width`, `:alpha` (overlay fill opacity, 0..1)
  * `:corner-radius` - pixels, every bar corner rounded; `:corner-radius-end` - only the corners at the bar's top
    (value end), wins over `:corner-radius`; clamped to half the shorter side; the legend key rounded alike
    (default 0)
  * `:stat-space` - `:data` (default, bins in data units) or `:scale` (bins in the x scale's coordinates, see
    [[cljplot2.scale.position/stat-transform-for]]: equal-width log10 bins on a `:log` axis, a bin method or count
    computed on the log10 values, `:density` per log10 unit; values the axis can't place - 0, negatives - dropped and
    counted in `:dropped :x`; explicit breaks stay in data units, a break the axis can't place throws
    `ex-info {:key :bins}`); bars are drawn back in data units
  * `:invalid` - rows whose x is missing, NaN, non-numeric or not placeable in the stat space: `:drop` (default;
    counted in `:dropped :x`) or `:show` - each group's count drawn as a bar in a band beside the x axis's start,
    outside the axis line, on the same y scale (see [[cljplot2.content.invalid-band]]; a column left of the panel, a
    row under it with `:flip`; every facet panel has its band): stats `:invalid-bands {:x {:label l :data {panel
    computed}}}`, bars in band coordinates `[0 1]` (split among the groups with `:dodge`, stacked, normalized or
    mirrored as the bins), in the chart's own fill (its `:color`, or the mapped one); shown rows aren't in
    `:dropped`; `:percent` / `:pmf` are shares of all rows, the invalid ones included; no band without invalid rows;
    not with `:y :density` / `:cumulative` (no bar width; a schema rule) or ridgelines (`ex-info {:key :invalid :in
    [:invalid]}`); all rows invalid still throw `{:key :mapping}`; the main charts of generated grids only - in a
    margin chart or a `:custom-grid` plot (no band drawn) it is a rule error keyed `:invalid` at `build`.
    `:invalid-label` - the band's label (default `\"NA\"`; only with `:invalid :show`, a schema rule); of several
    charts with a band the first one's label is drawn
  * `:padding {:x [lo hi] :y [lo hi]}` - default `{:x [0.05 0.05] :y [0 0.05]}` (mirror: y `[0.05 0.05]`); bars
    stand on the panel's bottom edge, x padding keeps end ticks off facet panel joints

  Groups (see [[cljplot2.data.group]]) come from ordinal aesthetics (`:color` fills the bars) and `:group`.
  A single distinct value gets one bin of width 1 centred on it.

  Ridgelines - `:y` mapped to categorical levels or on a `:bands` scale (see [[cljplot2.data.strip/ridges?]]; a
  numeric `:y` otherwise is ignored): one histogram per level and group (the common breaks, `:y` measure per item),
  bars growing from the band's centre toward the next level, all on one height scale - the tallest bar is `:overlap`
  (default 0.9) band steps high; the y scale leaves room for the outer ridges. Drawn from the farthest level on; `:position` `:overlay` (default; with groups translucent, `:alpha`) or
  `:mirror` (two groups, the second growing away from the next level).

  Functions: [[compute]], [[draw]], [[legend]] - the chart contract; [[schemas]] - its style, options and mapping."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.chart.common :as common]
            [cljplot2.chart.strip :as strip-chart]
            [cljplot2.data.rows :as data]
            [cljplot2.data.group :as group]
            [cljplot2.data.levels :as levels]
            [cljplot2.data.measure :as measure]
            [cljplot2.data.position :as position]
            [cljplot2.data.strip :as strip]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.java Array]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries
  "Map entries of the histogram style (theme `[:chart :histogram]`), also its options."
  (into (schema/in-section
         :style
         [(schema/optional :color {:description "bar fill colour (a mapped :color wins)"} schema/solid-color)
          (schema/optional :stroke-color {:description "outline colour: a colour, :none or :darker (the darkened fill)"} draw/stroke-color)
          (schema/optional :stroke-width {:description "outline width in pixels"} schema/non-neg)
          (schema/optional :alpha {:description "fill opacity of grouped bars with :position :overlay (other bars are opaque)"}
                           schema/fraction)
          (schema/optional :zero-line-color {:description "colour of the y = 0 line drawn with :position :mirror"} schema/color)
          (schema/optional :zero-line-width {:description "width in pixels of the y = 0 line drawn with :position :mirror"} schema/non-neg)])
        draw/pattern-style-entries))

(defn- invalid-option-problem
  "Gives the problem of the `:invalid` option, if any.

  Parameters:

  - `options` (map): `:invalid`, `:y`.

  Returns the message for `:invalid :show` with a `:y` measure a bar without a width can't take (`:density`,
  `:cumulative`); `nil` for valid options."
  [{:keys [invalid y]}]
  (when (and (= :show invalid) (#{:density :cumulative} y))
    (str ":invalid :show needs a :y measure of counts or shares (:count, :percent, :pmf), not " y)))

(defn- invalid-label-problem
  "Gives the problem of the `:invalid-label` option, if any.

  Parameters:

  - `options` (map): `:invalid`, `:invalid-label`.

  Returns the message for a label given without `:invalid :show` (no band to label); `nil` for valid options."
  [{:keys [invalid invalid-label]}]
  (when (and (some? invalid-label) (not= :show invalid))
    ":invalid-label labels the band of :invalid :show (no band without it)"))

(def schemas
  "Schemas of the histogram: `:style` (theme `[:chart :histogram]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into theme/theme-style-entries)
                (conj (schema/optional :bins {:cljplot2/section :bins :description (str "bins (per panel under free or sliced facet x scales): an estimation method (:sturges :sqrt :rice :doane :scott "
                                                                       ":freedman-diaconis), a bin count or increasing breaks (shared, values outside dropped)")
                                                :default :sturges}
                                        [:and levels/bins [:fn {:error/message "quantile bins aren't supported by histograms (give a breaks vector)"} (complement map?)]])
                      (schema/optional :y {:cljplot2/section :bins :description "bar height: :percent (of the panel's or group's total), :count, :pmf (shares summing to 1), :density (area 1) or :cumulative (count of the bin and every bin before it)"
                                           :default :percent}
                                       (into [:enum :cumulative] (keys measure/titles)))
                      (schema/optional :position {:cljplot2/section :position :description (str "grouped bars: :overlay (translucent), :dodge, :stack, :normalize (stacked to 1) or :mirror (two groups, "
                                                                      "the second downward); ridgelines: :overlay or :mirror")
                                                  :default :overlay}
                                       (into [:enum] (sort position/positions)))
                      (schema/nilable :symmetric {:cljplot2/section :position :description "with :position :mirror, tie both halves to one y range [-M M], M the tallest bar over all panels"
                                                  :default false}
                                      schema/bool)
                      (schema/nilable :zero-line {:cljplot2/section :position :description "with :position :mirror, draw the y = 0 line over the bars"
                                                  :default true}
                                      schema/bool)
                      (schema/optional :padding {:cljplot2/section :position :description "padding of the data span per position axis, {:x [lo hi] :y [lo hi]} fractions of the span"
                                                 :cljplot2/default-doc "{:x [0.05 0.05] :y [0 0.05]} (bars stand on the panel's bottom edge); y [0.05 0.05] with :position :mirror"}
                                       data/position-padding)
                      (first (schema/in-section :bins [(common/stat-space-option :data "space the bins are made in: :data (data units) or :scale (the x scale's coordinates - equal-width log10 bins on a :log axis, values it can't place dropped; :density per transformed unit)")]))
                      draw/corner-radius-option
                      draw/corner-radius-end-option)
                (conj (schema/nilable :invalid {:cljplot2/section :invalid :description (str "rows with a missing, NaN, non-numeric or unplaceable x: :drop (counted in :dropped) "
                                                                       "or :show (a bar of their count in a band beside the x axis's start)")
                                                :default :drop}
                                      [:enum :drop :show])
                      (schema/nilable :invalid-label {:cljplot2/section :invalid :description "label of the band of invalid values (needs :invalid :show)"
                                                      :cljplot2/default-doc "\"NA\""}
                                      :string))
                (into strip/ridge-options)
                (schema/with-rules [invalid-option-problem [:invalid]] [invalid-label-problem [:invalid-label]]))
   :mapping [:map
             [:x {:description "values to bin (data units)"} schema/accessor]
             (schema/nilable :y {:description "ridgeline levels: categorical or on a :bands scale, one histogram per level (a numeric :y otherwise is ignored)"}
                             schema/accessor)
             (schema/nilable :color {:description "groups, each with its own bar fill colour"} schema/accessor)
             (schema/nilable :pattern {:description "groups, each with its own fill pattern"} schema/accessor)
             (schema/nilable :group {:description "groups without a visual aesthetic"} schema/accessor)]})

(defn- bin-counts
  "Counts values per bin.

  Parameters:

  - `brks` (sequence of doubles): the increasing breaks; the last bin is closed.
  - `values` (sequence of numbers): the values.

  Returns `{:counts [..] :outside n}`: a count per bin and the number of values outside the breaks."
  [brks values]
  (let [arr (double-array brks)
        counts (long-array (m/long-dec (alength arr)))]
    (doseq [v values
            :let [bin (levels/bin-index arr (double v))]
            :when (not (m/neg? bin))]
      (Array/inc counts bin))
    (let [per-bin (vec counts)]
      {:counts per-bin :outside (m/long-sub (count values) (long (reduce m/long-add 0 per-bin)))})))

(defn- heights
  "Gives the bar heights of bin counts for a count measure.

  Parameters:

  - `y` (keyword): the measure - `:cumulative` (the count of the bin and every bin before it, ggplot `after_stat(cumsum(count))`) or a measure of [[cljplot2.data.measure/heights]].
  - `brks` (sequence of doubles): the breaks of the bins.
  - `counts` (vector of longs): a count per bin, in order.
  - `invalid` (long or `nil`): the count of invalid values shown in the band (`:invalid :show`, never with
    `:cumulative` or `:density`); `nil` - not shown.

  Returns a vector of heights, one per bin, then the band's height with `invalid` (a share of all values, the invalid
  ones included, for `:percent` and `:pmf`)."
  [y brks counts invalid]
  (if (= :cumulative y)
    (vec (rest (reductions m/+ 0.0 counts)))
    (measure/heights y (cond-> counts invalid (conj invalid))
                     (cond-> (mapv (fn [[^double a ^double b]] (m/- b a)) (partition 2 1 brks)) invalid (conj 1.0)))))

(defn- measure-title
  "Gives the value axis title of a measure.

  Parameters:

  - `y` (keyword): the measure (`:cumulative` - `Cumulative Count`, else [[cljplot2.data.measure/titles]])."
  [y]
  (if (= :cumulative y) "Cumulative Count" (measure/titles y)))

(defn- outside-count
  "Counts the values outside the breaks.

  Parameters:

  - `brks` (vector of doubles or `nil`): the breaks; `nil` - none, nothing outside.
  - `values` (sequence of numbers): the values.

  Returns the number of values below the first break or above the last."
  [brks values]
  (if brks
    (let [lo (double (first brks))
          hi (double (peek brks))]
      (count (remove #(m/<= lo (double %) hi) values)))
    0))

(defn- stat-breaks
  "Gives the bin breaks in a stat space (see [[cljplot2.scale.position/stat-transform-for]]).

  Parameters:

  - `transform` (map): the x stat transform - `:forward`.
  - `values` (sequence of doubles): the values in the stat space.
  - `bins`: the `:bins` option (see [[cljplot2.data.levels/breaks]]); explicit breaks are in data units.

  Returns the breaks in the stat space: explicit breaks taken there, else computed over `values`; `nil` without values.

  Throws `ex-info {:key :bins}` for an explicit break the transform can't place (e.g. 0 on a `:log` axis)."
  [{:keys [forward]} values bins]
  (if (vector? bins)
    (let [placed (mapv #(forward (double %)) bins)]
      (when-not (every? m/valid-double? placed)
        (schema/fail! "Histogram :bins breaks must be placeable on the x scale with :stat-space :scale (positive on a :log axis)."
                      {:key :bins :value bins}))
      placed)
    (levels/breaks values bins)))

(defn- panel-breaks
  "Gives the bin breaks of every panel in a stat space.

  Breaks are shared by all panels, except under a panel-local x scale (free or sliced facet scales): there the panels
  sharing an x scale (each panel of a wrap layout, a panel column or row of a facet grid) get breaks computed over
  their own values - a bin count over their range, a method estimated on their values - so the bins follow the
  panel's own range. Explicit breaks stay shared.

  Parameters:

  - `transform` (map): the x stat transform (see [[stat-breaks]]).
  - `values-of` (map): `{panel [value ...]}` - each panel's values in the stat space.
  - `bins`: the `:bins` option.
  - `groups` (sequence of sets or `nil`): the panel keys sharing each x scale (compute env `[:panel-scale-groups :x]`);
    `nil` - one x scale for all panels.

  Returns `{panel breaks}` for every panel of `values-of`; a panel whose scale group has no values takes the breaks of
  all values (`nil` without any value).

  Throws `ex-info {:key :bins}` - see [[stat-breaks]]."
  [transform values-of bins groups]
  (let [shared (delay (stat-breaks transform (into [] (mapcat val) values-of) bins))]
    (if (or (vector? bins) (empty? groups))
      (update-vals values-of (fn [_] @shared))
      (let [own (into {} (for [ks groups
                               :let [brks (stat-breaks transform (into [] (mapcat #(get values-of %)) ks) bins)]
                               :when brks
                               panel ks]
                           [panel brks]))]
        (into {} (map (fn [[panel _]] [panel (or (own panel) @shared)])) values-of)))))

(defn- ridge-compute
  "Computes the histogram stats of ridgelines (a categorical or bands `:y`).

  Parameters:

  - `chart` (map): the chart value - `:options` (`:y` default `:percent`, `:bins` default `:sturges`, `:overlap` default 0.9, `:position`, style options).
  - `chart-split` (map): the chart's rows split per panel.
  - `env` (map): the compute environment - `:theme`, `:plot-scales`, `:flip` ...

  Returns the chart stats with `:ridges true`: per panel `{:ridges [item ...] :top h :overlap o :style st :translucent b}`, items (see [[cljplot2.data.strip/ridge-split]]) with `:bars [[x0 x1 height] ...]` on the panel's breaks (see [[panel-breaks]]); one `:top` over all panels; values outside the breaks counted in `:dropped` `:x`."
  [{:keys [options] :as chart} chart-split {:keys [theme] :as env}]
  (let [y (data/option options :y :percent)
        {:keys [position panels dropped group-keys] split-items :items} (strip/ridge-split chart-split options env)
        {:keys [inverse] :as tx} (scale/stat-transform-for (data/option options :stat-space :data) (get-in env [:plot-scales :x]))
        placed (update-vals split-items (fn [its] (mapv #(merge % (data/transformed-numbers tx {:values (:values %) :dropped 0})) its)))
        unplaced (long (reduce m/long-add 0 (map :dropped (mapcat val placed))))
        items (update-vals placed (fn [its] (mapv #(dissoc % :dropped) its)))
        values-of (update-vals items (fn [its] (into [] (mapcat :values) its)))
        brks-of (panel-breaks tx values-of (data/option options :bins :sturges) (get-in env [:panel-scale-groups :x]))
        binned (into {} (keep (fn [[panel its]]
                                (when-let [brks (brks-of panel)]
                                  [panel (mapv (fn [it] (assoc it :bars (mapv (fn [[a b] h] [(inverse (double a)) (inverse (double b)) h])
                                                                              (partition 2 1 brks)
                                                                              (heights y brks (:counts (bin-counts brks (:values it))) nil))))
                                               its)])))
                     items)
        outside (long (reduce m/long-add 0 (map (fn [[panel vs]] (outside-count (brks-of panel) vs)) values-of)))
        top (double (reduce m/max 0.0 (map #(nth % 2) (mapcat :bars (mapcat val binned)))))
        overlap (double (data/option options :overlap strip/default-ridge-overlap))
        bar-style (theme/resolved-style theme chart)]
    (merge
     (data/padding-stats {:x data/axis-padding} {} options)
     {:data (update-vals binned (fn [its] {:ridges its :top top :overlap overlap :style bar-style
                                           :translucent (and (m/> (count group-keys) 1) (= :overlay position))}))
      :ridges true
      :extents (merge-with merge (group/aesthetic-extents panels (:plot-scales env))
                           (strip/extents :band binned (fn [{:keys [bars]}] (when (seq bars) [(first (first bars)) (second (peek bars))]))))
      :scale-defaults (strip/ridge-scale-defaults overlap (= :mirror position) (:flip env) (get-in env [:plot-scales :y]))
      :na (group/na-dims panels)
      :dropped (update dropped :x #(m/long-add (long (or % 0)) (m/long-add outside unplaced)))})))

(defn compute
  "Computes the histogram's stats.

  Parameters:

  - `chart` (map): the chart value - `:options` (`:bins` default `:sturges`, `:y` default `:percent`, `:position` default `:overlay`, `:symmetric`, `:zero-line`, `:invalid` default `:drop`, `:invalid-label` default `\"NA\"`, `:padding`, style options).
  - `chart-split` (map): `:groups` (`{panel rows}`) and the merged `:mapping`.
  - `env` (map): the compute environment - `:theme`, `:plot-scales` ...

  Returns ridgeline stats for a categorical or bands `:y` ([[ridge-compute]]), else per panel `{:bars [{:x0 :x1 :y0 :y1 :aesthetics}] :style st :translucent b :zero-line b}` in data units (bars in group order, `:y0` / `:y1` placed by [[cljplot2.data.position/stack-values]]; `:translucent` - grouped overlay, fill drawn with `:alpha`); x extents - the outer breaks, y - over 0 and every bar's ends, or `[-M M]` for a `:symmetric` mirror; `:dropped` `:x` - invalid values (unless shown) and values outside the breaks; with `:invalid :show` and invalid rows `:invalid-bands {:x {:label l :data {panel {:bars .. :style .. ..}}}}` - the band bars in band coordinates (see the namespace doc); `:tick-format {:y :absolute}` for mirror; the y axis titled by the measure (`:normalize` - `Proportion`).

  Throws `ex-info {:key :mapping}` when rows exist but none has a finite x (placeable in `:stat-space :scale`), `{:key :bins}` - see [[stat-breaks]], `{:key :position}` for `:mirror` without exactly two groups, `{:key :invalid}` for `:invalid :show` on ridgelines."
  [{:keys [options] :as chart} {:keys [groups mapping] :as chart-split} {:keys [theme] :as env}]
  (if (strip/ridges? chart-split env)
    (if (= :show (data/option options :invalid :drop))
      (schema/fail! "Histogram :invalid :show isn't supported by ridgelines (a categorical or bands :y)."
                    {:key :invalid :in [:invalid] :value :show})
      (ridge-compute chart chart-split env))
    (let [st (theme/resolved-style theme chart)
          show (= :show (data/option options :invalid :drop))
          y (data/option options :y :percent)
          position (data/option options :position :overlay)
          {:keys [panels dims dropped]} (group/split groups (dissoc mapping :y) env)
          x-of (data/row-accessor :x (:x mapping))
          {:keys [inverse] :as tx} (scale/stat-transform-for (data/option options :stat-space :data) (get-in env [:plot-scales :x]))
          xs (update-vals (group/finite-values panels x-of) (fn [gs] (mapv #(data/transformed-numbers tx %) gs)))
          all-values (into [] (comp (mapcat identity) (mapcat :values)) (vals xs))
          all-count (group/row-count panels)
          _ (when (and (m/pos? all-count) (empty? all-values))
              (data/fail-mapping! data/no-valid-values :x {:rows all-count}))
          brks-of (panel-breaks tx (update-vals xs (fn [gs] (into [] (mapcat :values) gs))) (data/option options :bins :sturges)
                                (get-in env [:panel-scale-groups :x]))
          group-keys (group/panel-group-keys panels dims (:plot-scales env))
          n-groups (count group-keys)
          _ (when (and (= position :mirror) (m/not== 2 n-groups) (seq all-values))
              (schema/fail! "Histogram :position :mirror needs exactly two groups." {:key :position :groups n-groups}))
          outside (long (reduce m/long-add 0 (map (fn [[panel gs]] (outside-count (brks-of panel) (mapcat :values gs))) xs)))
          invalid (long (reduce m/long-add 0 (map :dropped (mapcat identity (vals xs)))))
          band? (and show (m/pos? invalid))
          ;; per panel: heights of every group (all of them, absent ones 0, so slots and mirror halves stay global);
          ;; with the band shown, each group's last height is its band bar's
          panel-bars (fn [brks gs group-values]
                       (let [n-bins (m/long-dec (count brks))
                             present (zipmap (map :key gs) (map vector gs group-values))
                             group-heights (mapv (fn [k] (if-let [[_ {:keys [values] group-invalid :dropped}] (present k)]
                                                           (heights y brks (:counts (bin-counts brks values)) (when band? group-invalid))
                                                           (vec (repeat (cond-> n-bins band? m/long-inc) 0.0))))
                                                 group-keys)
                             ys (position/stack-values position group-heights)
                             slot (fn [[a b] i] (if (= position :dodge) (position/dodge-interval [a b] i n-groups) [a b]))
                             present-groups (keep (fn [[i k]] (when-let [[g] (present k)] [i g])) (map-indexed vector group-keys))]
                         {:bars (vec (for [[i g] present-groups
                                           [[a b] [y0 y1]] (map vector (partition 2 1 brks) (ys i))]
                                       (let [[x0 x1] (slot [a b] i)]
                                         {:x0 (inverse (double x0)) :x1 (inverse (double x1)) :y0 y0 :y1 y1 :aesthetics (:aesthetics g)})))
                          ;; band coordinates: [0 1] across the band (see cljplot2.content.invalid-band)
                          :band (when band?
                                  (vec (for [[i g] present-groups
                                             :let [[y0 y1] (last (ys i))
                                                   [x0 x1] (slot [0.0 1.0] i)]]
                                         {:x0 x0 :x1 x1 :y0 y0 :y1 y1 :aesthetics (:aesthetics g)})))}))
          panel-data (into {} (keep (fn [[panel gs]] (when-let [brks (brks-of panel)] [panel (assoc (panel-bars brks gs (xs panel)) :breaks brks)])))
                           panels)
          panel-stats (fn [bars style] {:bars bars :style style
                                        :translucent (and (m/> n-groups 1) (= position :overlay))
                                        :zero-line (and (= position :mirror) (data/option options :zero-line true))})
          data (update-vals panel-data #(panel-stats (:bars %) st))
          bar-ys (fn [bars] (mapcat (juxt :y0 :y1) bars))
          bar-extents (into {} (map (fn [[panel {:keys [bars band breaks]}]]
                                      [panel (when (seq bars)
                                               (let [ys (bar-ys (concat bars band))]
                                                 {:x {:kind :numeric :min (inverse (double (first breaks))) :max (inverse (double (peek breaks)))}
                                                  :y {:kind :numeric :min (reduce m/min 0.0 ys) :max (reduce m/max 0.0 ys)}}))]))
                           panel-data)
          [lo hi] (position/symmetric-extent (bar-ys (mapcat (fn [{:keys [bars band]}] (concat bars band)) (vals panel-data))))
          panel-extents (if (and (= position :mirror) (:symmetric options))
                          (update-vals bar-extents #(some-> % (assoc :y {:kind :numeric :min lo :max hi})))
                          bar-extents)
          aesthetic-extents (group/aesthetic-extents panels (:plot-scales env))]
      (merge
       (data/padding-stats {:x data/axis-padding :y (if (= position :mirror) data/axis-padding [0.0 0.05])} (when-not (= position :mirror) {:y [0.0 nil]}) options)
       (cond-> {:data data
                :extents (merge-with merge aesthetic-extents (into {} (filter val) panel-extents))
                :na (group/na-dims panels)
                :dropped (assoc dropped :x (if show outside (m/long-add invalid outside)))
                :axis-titles {:y (if (= position :normalize) "Proportion" (measure-title y))}
                :tick-format (when (= position :mirror) {:y :absolute})}
         ;; the band's bars in the chart's own style: its fill, or the mapped one
         band? (assoc :invalid-bands {:x {:label (data/option options :invalid-label "NA")
                                          :data (update-vals panel-data #(panel-stats (:band %) st))}}))))))

(defn- draw-ridges!
  "Draws the ridgeline histograms of one panel, farthest level first.

  Parameters:

  - `computed` (map): the panel's ridgeline stats - `:ridges`, `:top`, `:overlap`, `:style`, `:translucent`.
  - `canvas`: the clojure2d canvas.
  - `box` (map): the panel box.
  - `scales` (map): finalized scales.
  - `env` (map): the draw environment - `:theme`.
  - `rounding`: the corner rounding (see [[cljplot2.graphics.draw/corner-rounding]]).

  Nothing is drawn when every bin is empty; empty bins, groups outside their scale's domain and bars the scales can't place are skipped."
  [{:keys [ridges ^double top ^double overlap style translucent]} canvas box scales env rounding]
  (let [pl (strip-chart/placement box scales {} :band)
        point (strip/ridge-point pl overlap)
        stroke-width (double (:stroke-width style))
        pattern-fill (draw/pattern-fill-cache)]
    (when (m/pos? top)
      (c2d/set-stroke-custom canvas {:size stroke-width :join :miter})
      (doseq [{:keys [level slot bars aesthetics]} (strip/ridge-order pl ridges)
              :let [st (group/style style aesthetics scales (:theme env))]
              :when st
              :let [side (strip/slot-side slot)
                    fill (draw/translucent-fill (:color st) (:alpha style) translucent)]
              [x0 x1 ^double h] bars
              :when (m/pos? h)
              :let [a (point x0 level side 0.0)
                    b (point x1 level side (m// h top))]
              :when (and (data/finite-point? a) (data/finite-point? b))]
        (draw/bar! canvas a b fill (draw/outline-color (:stroke-color st) fill) stroke-width (pattern-fill st fill)
                   (draw/bar-rounding rounding a (point x0 level side (m// h top))))))))

(defn draw
  "Draws the bars of one panel.

  Parameters:

  - `chart` (map): the chart value - `:options` (corner radii).
  - `computed` (map): the panel's stats - `:bars`, `:style`, `:translucent`, `:zero-line`, or ridgeline stats (`:ridges`).
  - `canvas`: the clojure2d canvas.
  - `box` (map): the panel box.
  - `scales` (map): finalized scales.
  - `env` (map): the draw environment - `:theme`.

  Grouped overlay bars get fill alpha `:alpha`; empty bins, bars under half a pixel and bars of groups outside an aesthetic domain are skipped; mirror with `:zero-line` (default on) - the y = 0 line on top. Ridgelines by [[draw-ridges!]], none without an x scale (see [[cljplot2.chart.strip/placeable?]])."
  [{:keys [options]} {:keys [bars style translucent ridges zero-line] :as computed} canvas box scales env]
  (if ridges
    (when (strip-chart/placeable? scales) (draw-ridges! computed canvas box scales env (draw/corner-rounding options)))
    (when (seq bars)
      (let [projection (scale/projection scales)
            stroke-width (double (:stroke-width style))
            pattern-fill (draw/pattern-fill-cache)
            rounding (draw/corner-rounding options)]
        (c2d/set-stroke-custom canvas {:size stroke-width :join :miter})
        (doseq [{:keys [x0 x1 y0 y1 aesthetics]} bars
                :when (m/not== (double y0) (double y1))
                :let [st (group/style style aesthetics scales (:theme env))]
                :when st]
          (let [{left :x top :y w :w h :h} (draw/rect-of projection x0 y0 x1 y1)
                fill (:color st)
                stroke (draw/outline-color (:stroke-color st) fill)
                drawn-fill (draw/translucent-fill fill (:alpha style) translucent)]
            (if rounding
              (let [[base end] (draw/value-ends y0 y1)
                    a (projection x0 base)]
                (draw/bar! canvas (draw/whole-pixel a) (draw/whole-pixel (projection x1 end)) drawn-fill stroke stroke-width
                           (pattern-fill st drawn-fill) (draw/bar-rounding rounding a (projection x0 end))))
              (when (draw/visible-bar? (double w) (double h))
                (draw/fill-patterned! canvas drawn-fill (pattern-fill st drawn-fill) #(c2d/rect % left top w h))
                (draw/outline! canvas stroke stroke-width :miter #(c2d/rect % left top w h true))))))
        (when zero-line
          (draw/zero-line! canvas box scales style))))))

(defn legend
  "Draws the histogram's key glyph: a filled square, outlined by `:stroke-color`, inset in a legend key.

  Parameters:

  - `chart` (map): the chart value - `:options` (`:position`, corner radii).
  - `attributes` (map): the key's mapped style attributes - `:color` and `:pattern` are used.
  - `canvas`: the clojure2d canvas.
  - `box` (map): the key box in pixels.
  - `env` (map): the legend environment - `:theme`.

  With a mapped `:color` and the `:overlay` position (translucent grouped bars) the fill gets `:alpha` too; corners rounded as the bars'.

  See also [[cljplot2.graphics.draw/bar-key!]]."
  [{:keys [options] :as chart} attributes canvas box env]
  (draw/bar-key! canvas box (theme/key-style env chart attributes [:color :pattern])
                 (and (contains? attributes :color) (= :overlay (data/option options :position :overlay)))
                 (draw/corner-rounding options)))
