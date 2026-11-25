(ns cljplot2.chart.bar
  "Bar chart: one bar per level and group - a strip chart (see [[cljplot2.data.strip]]): levels on the categorical
  axis - `{:x level :y value}` vertical bars, `{:x value :y level}` horizontal ones, `:level-axis` when both or neither
  axis is categorical; placement `:place`, `:width`, `:dodge-gap`, `:n-label`. Axes below are named in the strip frame
  - `:x` the values, `:y` the levels; exchange them for vertical bars (levels on `:x`), also in `:padding`:
  `{:mapping {:x :country :y :value}}` with `(chart/bar {:padding {:y [0 0.1]}})` leaves 10% above the tallest bar.

  Options (style defaults from theme `[:chart :bar]`):

  * `:stat` - the bar length from the item's rows: `:sum` (default; of the `:x` values - one row per level gives its
    value), `:mean`, `:median` (fastmath.stats), a function of the `:x` values returning a finite number, or a count
    measure of the rows (`:x` not needed, see [[cljplot2.data.measure]]): `:count`, `:percent`, `:pmf` (of the
    group's total in the panel)
  * `:position` - groups: `:dodge` (default when some level holds two or more groups; side by side in the band),
    `:overlay` (the default otherwise - one group per level fills its band; groups sharing a level translucent, fill
    `:alpha`), `:stack` (on top of each other, in group order; negative lengths stack from 0
    the other way), `:normalize` (stacked, each level summing to 1; non-negative lengths)
  * `:negative-order` - `:stack`: the groups below 0 in group order (`:forward`, default) or reversed (`:reverse` - the
    last group nearest 0): with groups ordered from one end of a scale to the other (strongly disagree ... strongly
    agree) and the disagreeing values negative, the bars diverge from the middle (Likert scales; split a neutral
    group's value half below, half above 0 with a `:group`)
  * `:mirror` - a grouping dimension (an ordinal aesthetic or `:group`) with exactly two levels (see
    [[cljplot2.data.strip]]): the second level's bars point the other way (back-to-back bars, e.g. population
    pyramids); `:position` applies on each side (a dodged pair shares its slot), `:normalize` sums to 1 on one side
    and -1 on the other; value tick labels show magnitudes; `:zero-line` (default true) draws the 0 line on top
    (`:zero-line-color`, `:zero-line-width`)
  * `:varwidth` - bar widths relative to the widest: `true` - by the square root of the count (as box plots), a
    keyword or function of a row - by the item's sum of its finite values (non-negative)
  * `:value-label` - `true` writes each bar's length, signed as its value (negative bars negative - `(fn [l]
    (Math/abs (:value l)))`-style functions drop the sign; a mirror's second group shows its positive value)
    (`:value-format` - a format string or a function of the number, default: whole
    numbers as integers, otherwise the fewest digits telling the labels apart), or a function of `{:value length :n
    count :share s :level l :group {dim value}}` (`:share` - of the absolute lengths of its level's bars; `:group` -
    the bar's aesthetic values) returning the text (`nil` / `\"\"` - none), e.g. pie labels
    `#(format \"%s\\n%.0f%%\" (get-in % [:group :color]) (* 100 (:share %)))`; zero-length bars get none. With `:n-label`
    too, both texts form one label - `:label-stack` `:horizontal` (side by side) or `:vertical` (one above the
    other), default along the bars (horizontal bars side by side, vertical ones stacked)
  * `:label-position` - `:end` (past the bar's end; the default), `:center` (centred in the bar; the default for
    `:stack` / `:normalize`), `:base` (inside the bar at its base - the end nearer 0, or the visible end nearest 0 when 0 lies outside
    the value domain - anchored toward its end; in a polar plot with the values along the angle centred just past
    the base; with the values on the radius the base is near the centre, where most wedges are too narrow for a
    label, so these labels are mostly skipped) or `:outer` (polar plots only: past the outer radius at the
    bar's middle angle, anchored outward - pie labels; the circle shrinks to keep room for them; the angle axis is
    hidden by default, its labels would sit under them (`:scales {k {:axis true}}` brings it back); `:label-leader
    true` draws a line from the bar's outer edge, the labels further out). Labels are placed largest bar first and
    skipped when they overlap one placed before; a centred or base label is skipped unless its box lies inside its bar
  * a mapped `:color` with numbers (a linear or binned colour scale, see [[cljplot2.data.group/continuous-color]])
    doesn't group: each bar is filled by its own colour value under the colour scale, with a colour bar legend -
    groups (dodged, stacked, mirrored) come from the categorical aesthetics and `:group` only, each stacked segment
    coloured by its own value. A bar's colour value is `:color-stat` over its rows' colour values (rows with a finite
    `:x` value; every row for count stats): `:sum`, `:mean`, `:median` or a function of the values, default the bar's
    `:stat` when it is a value stat (so `{:x :count :color :count}` colours each bar by its length) and `:mean` for
    count stats (`:stat :count` coloured by the mean of another column; the count itself isn't a row value - map a
    column holding it). One row gives its value (a date as well). Every row builds its bar, also with an explicit
    colour `:domain` (rows are never dropped by the colour value): a bar without a colour value, or with one outside
    the colour scale's domain (or bins), keeps its length and takes the theme's NA colour (`[:palette :na]`); only NA
    rows are dropped, with `:groups {:na false}` (counted in `:dropped :color`). The value is kept on the bar as
    `:color-value` (as rect keeps its rects') and reaches a `:value-label` function as `:color-value` (`[:group :color]`
    holds categorical colours only). `:color-stat` with a categorical or unmapped `:color` throws `{:key :color-stat}`
  * `:color` (fill), `:stroke-color` (a color, `:none` or `:darker` - the darkened fill, default), `:stroke-width`,
    `:alpha`
  * `:corner-radius` - pixels, every bar corner rounded (each stacked segment too; polar - the annular sector's
    corners, see [[cljplot2.graphics.draw/rounded-sector-path]]); `:corner-radius-end` - only the corners at the value
    end, wins over `:corner-radius`; clamped to half the shorter side; the legend key rounded alike (default 0)
  * `:padding {:x [lo hi]}` - default 5%, none at the baseline side the bars grow from (bars start on the axis) and
    none for `:normalize` (the axis spans exactly `[0 1]`, mirrored `[-1 1]`)

  Error bars: `chart/extent` with the same mapping (and the same `:mirror`) takes the same slots and side (e.g.
  `{:method :stddev}` over `:mean` bars). With a mapped `:color` the extent takes the bars' fills and disappears over
  them: map the variable to `:group` instead (`{:mapping {:color nil :group k} :color c}` - the bars' slots when
  `k` is their only grouping besides the `:mirror` dimension). Bars coloured by numbers (a linear or binned `:color`)
  don't group by them, while extent groups by every aesthetic and throws `{:key :mapping}` for a numeric `:color`:
  give the extent `{:mapping {:color nil}}` - it lines up with the bars then.

  Labels of stacked and normalized bars sit centred in their segments; labels inside a bar (`:center`, `:base`) are
  in black or white by the fill.

  Axis title suggested for the value axis: the count measure (`Count` ...), `Proportion` for `:normalize`,
  `<stat> of <x>` for `:sum`, `:mean`, `:median` of a keyword `:x` when some bar has more than one value.

  Functions: [[legend]] - the chart contract's key glyph (the chart computes and draws through [[cljplot2.chart.strip/compute]] and [[cljplot2.chart.strip/draw]])."
  (:require [clojure.string :as str]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [fastmath.stats :as stats]
            [cljplot2.chart.common :as common]
            [cljplot2.content.label :as label]
            [cljplot2.data.group :as group]
            [cljplot2.data.levels :as levels]
            [cljplot2.data.measure :as measure]
            [cljplot2.data.position :as position]
            [cljplot2.data.rows :as data]
            [cljplot2.data.text :as text]
            [cljplot2.chart.strip :as strip-chart]
            [cljplot2.data.strip :as strip]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.coord :as coord]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private count-stats
  "Count measure stats: bar lengths from row counts (see [[cljplot2.data.measure]])."
  #{:count :percent :pmf})

(def ^:private value-stats
  "Value stats: keyword -> fastmath.stats function of an item's values."
  {:sum stats/sum :mean stats/mean :median stats/median})

(def ^:private positions
  "Group positions of the bars."
  #{:overlay :dodge :stack :normalize})

(def ^:private style-entries
  "Map entries of the bar style (theme `[:chart :bar]`, also options)."
  (into [(schema/optional :color {:cljplot2/section :style :description "bar fill colour (a mapped :color wins)"} schema/solid-color)
         (schema/optional :stroke-color {:cljplot2/section :style :description "outline colour: a colour, :none or :darker (the darkened fill)"} draw/stroke-color)
         (schema/optional :stroke-width {:cljplot2/section :style :description "outline width in pixels"} schema/non-neg)
         (schema/optional :alpha {:cljplot2/section :style :description "fill opacity of overlaid groups sharing a level"} schema/fraction)
         (schema/optional :zero-line-color {:cljplot2/section :style :description "colour of the 0 line drawn over mirrored bars"} schema/color)
         (schema/optional :zero-line-width {:cljplot2/section :style :description "width in pixels of the 0 line drawn over mirrored bars"} schema/non-neg)]
        draw/pattern-style-entries))

(def schemas
  "Schemas of the bar chart: `:style` (theme `[:chart :bar]`), `:options`, `:mapping` (merged; `:x` checked at
  compute - count stats don't need it)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into theme/theme-style-entries)
                (into (remove #(= :position (first %)) strip/options))
                (conj (schema/optional :stat {:cljplot2/section :bars :description "bar length from the item's rows: :sum, :mean, :median or a function of the values, or a count measure :count, :percent, :pmf (no values needed)"
                                              :default :sum}
                                       [:or (into [:enum] (sort (into count-stats (keys value-stats)))) schema/function])
                      (schema/nilable :color-stat {:cljplot2/section :bars :description "a bar's colour value from its rows' values of a linear or binned :color: :sum, :mean, :median or a function of the values"
                                                    :cljplot2/default-doc "the :stat of value stats, :mean for count stats"}
                                       [:or (into [:enum] (sort (keys value-stats))) schema/function])
                      (schema/optional :position {:cljplot2/section :bars :description "how groups of one level share its band: :dodge (side by side), :overlay (on top of each other), :stack or :normalize (stacked, each level summing to 1)"
                                                  :cljplot2/default-doc ":dodge when a level holds two or more groups, else :overlay"}
                                       (into [:enum] (sort positions)))
                      strip/mirror-option
                      (schema/optional :label-stack {:cljplot2/section :labels :description "how the value and n texts of one label are joined: :horizontal (side by side) or :vertical (one above the other)"
                                                     :cljplot2/default-doc "along the bars: :horizontal for horizontal bars, :vertical for vertical ones"}
                                       [:enum :horizontal :vertical])
                      (schema/nilable :zero-line {:cljplot2/section :bars :description "draw the 0 line on top of mirrored bars (with :mirror only)"
                                                  :default true}
                                      schema/bool)
                      (schema/nilable :varwidth {:cljplot2/section :bars :description "bar widths relative to the widest: true - by the square root of the count, a keyword or function of a row - by the item's sum of it"
                                                 :default false}
                                      [:or schema/bool schema/accessor])
                      (schema/nilable :value-label {:cljplot2/section :labels :description "write each bar's length (true) or the text a function of {:value :n :share :level :group} returns"
                                                    :default false}
                                      [:or schema/bool schema/function])
                      (schema/optional :label-position {:cljplot2/section :labels :description "where labels sit: :end (past the bar's end), :center (in the bar), :base (in the bar at its base) or :outer (polar only: past the outer radius)"
                                                        :cljplot2/default-doc ":center for :stack / :normalize, else :end"}
                                       [:enum :end :center :base :outer])
                      (schema/optional :negative-order {:cljplot2/section :bars :description "order of the groups below 0 with :stack: :forward (group order) or :reverse (the last group nearest 0)"
                                                        :default :forward}
                                       [:enum :forward :reverse])
                      (schema/nilable :label-leader {:cljplot2/section :labels :description "draw a line from the bar's outer edge to its :outer label"
                                                     :default false}
                                      schema/bool)
                      (schema/optional :value-format {:cljplot2/section :labels :description "text of the value labels: a format string or a function of the number"
                                                      :cljplot2/default-doc "whole numbers as integers, else the fewest digits telling the labels apart"}
                                       schema/number-format)
                      (schema/optional :padding {:cljplot2/section :bars :description "padding of the data span per position axis, {:x [lo hi] :y [lo hi]} fractions of the span (strip frame: :x the values)"
                                                 :cljplot2/default-doc "values 0.05, 0 at the baseline side the bars grow from, 0 for :normalize"}
                                       data/position-padding)
                      draw/corner-radius-option
                      draw/corner-radius-end-option))
   :mapping [:map
             (schema/nilable :x {:description "x position: the levels (categorical) of vertical bars or the values of horizontal ones"}
                             schema/accessor)
             (schema/nilable :y {:description "y position: the values of vertical bars or the levels (categorical) of horizontal ones"}
                             schema/accessor)
             (schema/nilable :color {:description "fill colour: categorical values group the bars (legend keys), numeric ones colour each bar by its value (a colour bar)"} schema/accessor)
             (schema/nilable :pattern {:description "fill pattern per group (gives legend keys)"} schema/accessor)
             (schema/nilable :group {:description "groups without a visual aesthetic (dodged, stacked or mirrored as other groups)"}
                             schema/accessor)]})

(defn- check-options!
  "Checks the options against the mapping before computing.

  Parameters:

  - `options` (map): the chart options (`:stat`).
  - `mapping` (map): the merged mapping (strip frame).

  Returns `nil`.

  Throws `ex-info {:key :mapping}` for a value stat without the values mapped (`:x` in the strip frame)."
  [options mapping]
  (when (and (nil? (:x mapping)) (not (count-stats (data/option options :stat :sum))))
    (data/fail-mapping! "Bar :stat needs the values mapped - on the axis other than the levels (only :count, :percent and :pmf count rows without them)."
                  :x {})))

(defn- item-lengths
  "Adds the bar length to every item.

  Parameters:

  - `stat`: a count measure, a value stat keyword or a function of the values (see the namespace doc).
  - `items` (map): panel -> items.

  Returns `items` with `:length` on every item; count measures over each group's items of the panel.

  Throws `ex-info {:key :stat}` when a stat function returns no finite number."
  [stat items]
  (update-vals items
               (fn [its]
                 (if (count-stats stat)
                   (let [by-key (group-by :key its)
                         lengths (into {} (mapcat (fn [[_ g]] (map vector g (measure/heights stat (map :n g) (repeat 1.0))))) by-key)]
                     (mapv #(assoc % :length (lengths %)) its))
                   (let [f (get value-stats stat stat)]
                     (mapv (fn [{:keys [values] :as it}]
                             (let [length (f values)]
                               (when-not (schema/finite? length)
                                 (schema/fail! "Bar :stat should return a finite number." {:key :stat :value length}))
                               (assoc it :length (double length))))
                           its))))))

(defn- color-stat-fn
  "Gives the function turning a bar's colour values into its colour value.

  Parameters:

  - `options` (map): the chart options - `:color-stat` (`:sum`, `:mean`, `:median` or a function of the values).
  - `stat`: the bar stat - the default `:color-stat` for a value stat; count stats default to `:mean`.

  Returns a function of a sequence of doubles."
  [options stat]
  (let [color-stat (data/option options :color-stat (if (count-stats stat) :mean stat))]
    (get value-stats color-stat color-stat)))

(defn- color-value
  "Gives a bar's value of a linear or binned `:color`.

  Parameters:

  - `color-stat` (function): the colour values -> the bar's colour value (see [[color-stat-fn]]).
  - `color-of` (function): row -> its colour value.
  - `rows` (sequence): the bar's rows.

  Returns `nil` (NA) when no row has a colour value (or no number of its rows is finite); finite numbers through `color-stat` (so one row gives its value with the built-in stats); a single non-number (a date) as it is.

  Throws `ex-info {:key :color-stat}` when `color-stat` returns no finite number or several dates would be aggregated."
  [color-stat color-of rows]
  (let [vs (keep color-of rows)]
    (cond
      (empty? vs) nil
      (every? number? vs) (when-let [finite (seq (:values (data/finite-numbers vs)))]
                            (let [v (color-stat finite)]
                              (when-not (schema/finite? v)
                                (schema/fail! "Bar :color-stat should return a finite number." {:key :color-stat :value v}))
                              (double v)))
      (empty? (next vs)) (first vs)
      :else (schema/fail! "Bar :color-stat aggregates numbers: a bar with several dates as :color values has no colour value (map numbers, or one row per bar)."
                          {:key :color-stat :value (vec (take 3 vs))}))))

(defn- colored-items
  "Puts each bar's value of a linear or binned `:color` on it.

  Parameters:

  - `color-stat` (function): see [[color-value]].
  - `color-of` (function): row -> its colour value.
  - `x-of` (function or `nil`): the value accessor - only rows with a finite value count; `nil` (count stats) - every row.
  - `items` (map): panel -> items with their `:rows`.

  Returns `items`, each with `:color-value` its colour value (see [[color-value]]; as rect's per-mark colour value) - drawn through the colour scale; `nil`, or a value outside the scale's domain, in the NA colour."
  [color-stat color-of x-of items]
  (let [counted (if x-of (fn [rows] (filter #(schema/finite? (x-of %)) rows)) identity)]
    (update-vals items (fn [its] (mapv #(assoc % :color-value (color-value color-stat color-of (counted (:rows %)))) its)))))

(defn- side-ends
  "Gives the bar ends of the items of one side of one panel.

  Parameters:

  - `position`: the group position (`:overlay`, `:dodge`, `:stack`, `:normalize`).
  - `stack-options` (map): see [[cljplot2.data.position/stack-values]].
  - `levels` (vector): the panel's levels.
  - `its` (sequence of maps): the items, `:key`, `:level`, `:length`.

  Returns `{[group-key level] [y0 y1]}`; a group without a level counts 0 there; `:normalize` ends at most 1 (a rounding error of the summed shares dropped, so the axis ends at 1)."
  [position stack-options levels its]
  (let [index (zipmap levels (range))
        by-key (group-by :key its)
        group-keys (vec (distinct (map :key its)))
        lengths (mapv (fn [k] (reduce (fn [ls it] (assoc ls (index (:level it)) (:length it)))
                                      (vec (repeat (count levels) 0.0)) (by-key k)))
                      group-keys)
        ends (position/stack-values position lengths stack-options)
        end-of-share (if (= :normalize position) (fn [[^double y0 ^double y1]] [(m/min y0 1.0) (m/min y1 1.0)]) identity)]
    (into {} (for [[gi k] (map-indexed vector group-keys) it (by-key k)]
               [[k (:level it)] (end-of-share ((ends gi) (index (:level it))))]))))

(defn- placed-bars
  "Adds the bar ends to the items of one panel.

  Parameters:

  - `position`: the group position.
  - `stack-options` (map): see [[side-ends]].
  - `its` (sequence of maps): the panel's items.

  Returns the items, in order, with `:y0 :y1` (bar ends along `:x`): `position` on each side, the `:mirrored` items' ends negated (`[y0 y1]` -> `[-y1 -y0]`)."
  [position stack-options its]
  (let [levels (vec (distinct (map :level its)))
        ends (merge (side-ends position stack-options levels (remove :mirrored its))
                    (update-vals (side-ends position stack-options levels (filter :mirrored its))
                                 (fn [[^double y0 ^double y1]] [(m/- y1) (m/- y0)])))]
    (mapv (fn [it] (let [[y0 y1] (ends [(:key it) (:level it)])] (assoc it :y0 y0 :y1 y1))) its)))

(defn- weight-factors
  "Gives the width factor function of `:varwidth`.

  Parameters:

  - `varwidth`: `true`, a keyword or function of a row, `nil` / `false`.
  - `items` (sequence of maps): every item (with `:rows` for a keyword or function).

  Returns item -> width factor: `true` - [[cljplot2.data.strip/varwidth-factors]]; a keyword / function - the item's sum of the finite weights of its `:rows` relative to the largest (0 when every sum is 0); otherwise 1.

  Throws `ex-info {:key :varwidth}` for a negative sum."
  [varwidth items]
  (cond
    (true? varwidth) (strip/varwidth-factors items true)
    (or (keyword? varwidth) (fn? varwidth))
    (let [acc (data/row-accessor :varwidth varwidth)
          weight (fn [{:keys [rows]}] (v/sum (:values (data/finite-numbers (map acc rows)))))
          weights (map weight items)
          _ (when-let [w (first (filter m/neg? weights))]
              (schema/fail! "Bar :varwidth weights should be non-negative." {:key :varwidth :value w}))
          wmax (double (reduce m/max 0.0 weights))]
      (fn ^double [item] (if (m/pos? wmax) (m// (double (weight item)) wmax) 0.0)))
    :else (constantly 1.0)))

(defn- end-of
  "Gives a bar's far end from 0 along `:x` and the direction it points.

  Parameters:

  - `bar` (map): `:y0`, `:y1`.

  Returns `[end direction]`, direction `1.0` or `-1.0`; a zero bar points up."
  [{:keys [^double y0 ^double y1]}]
  (if (m/neg? (m/+ y0 y1)) [(m/min y0 y1) -1.0] [(m/max y0 y1) 1.0]))

(defn- base-of
  "Gives a bar's base - its end nearer 0 - along `:x` and the direction toward its far end.

  Parameters:

  - `bar` (map): `:y0`, `:y1`.

  Returns `[base direction]`, direction `1.0` or `-1.0` (see [[end-of]]); a zero bar points up."
  [{:keys [^double y0 ^double y1] :as bar}]
  (let [[_ ^double dir] (end-of bar)]
    [(if (m/pos? dir) (m/min y0 y1) (m/max y0 y1)) dir]))

(defn- value-texts
  "Gives the value label text function of the bars.

  Parameters:

  - `options` (map): the chart options (`:value-format`).
  - `bars` (sequence of maps): every bar with a length (`:y0`, `:y1`, `:length`).

  Returns bar -> text of its signed length - `|y1 - y0|` with the sign of its value `:length` (negative bars negative; stacked and normalized segments their own length; a mirror's second group its value, positive) - by `:value-format`, else [[cljplot2.data.levels/break-labels]] over all bars (whole numbers as integers, else the fewest digits telling them apart)."
  [options bars]
  (let [signed-length (fn [{:keys [^double y0 ^double y1 length]}] (m/* (m/signum (double length)) (m/abs (m/- y1 y0))))]
    (if-let [fmt (:value-format options)]
      (let [text-of (schema/text-formatter fmt)] (fn [bar] (text-of (signed-length bar))))
      (let [ls (distinct (map signed-length bars))]
        (if (seq ls) (let [texts (zipmap ls (levels/break-labels ls))] (comp texts signed-length)) (constantly ""))))))

(defn- label-info
  "Gives what a `:value-label` function gets for a bar.

  Parameters:

  - `bar` (map): `:length`, `:n`, `:level`, `:aesthetics`.
  - `level-total` (map): level -> the sum of the absolute lengths of its bars in the panel.

  Returns `{:value length :n count :share s :level l :group {dim value}}`, `:share` - of the level's total (0 for a zero total), with `:color-value` - the bar's value of a linear or binned `:color` (see [[colored-items]]) - when it has one."
  [{:keys [^double length n level aesthetics] :as bar} level-total]
  (let [total (double (level-total level))]
    (cond-> {:value length :n n :share (if (m/pos? total) (m// (m/abs length) total) 0.0) :level level :group aesthetics}
      (contains? bar :color-value) (assoc :color-value (:color-value bar)))))

(defn- value-label-text
  "Gives the value text of a bar.

  Parameters:

  - `labels`: the `:value-label` option - `true` (by `value-text`), a function of [[label-info]], or falsy.
  - `value-text` (function): bar -> text (see [[value-texts]]).
  - `level-total` (map): level -> its total (see [[label-info]]).
  - `bar` (map): the bar.

  Returns the text (a formula or runs value kept, see [[cljplot2.data.text/label-text]]); `nil` for none, a zero-length bar or an empty result."
  [labels value-text level-total {:keys [^double y0 ^double y1] :as bar}]
  (when (and labels (m/not== y0 y1))
    (let [t (if (fn? labels) (labels (label-info bar level-total)) (value-text bar))]
      (when (and (some? t) (not= "" t)) (text/label-text t)))))

(defn- end-label
  "Gives the end label of a bar (see [[cljplot2.chart.strip/draw-end-labels!]]).

  Parameters:

  - `options` (map): the chart options (`:value-label`, `:n-label`, `:n-format`, `:label-leader`).
  - `separator` (string): joins the value and count texts.
  - `label-position`: `:end` (past the bar's end), `:center` (centred in it, `:side 0`), `:base` (in it from its base toward its end) or `:outer` (past a polar plot's outer radius).
  - `value-text` (function): bar -> text (see [[value-texts]]).
  - `level-total` (map): level -> its total.
  - `bar` (map): the bar.

  Returns the label - its value text ([[value-label-text]]) and count joined, the bar's `:span [y0 y1]`, the text role `:bar-value` (its `:contrast` colour by the fill inside a bar, else by the plot background), `:inside true` for the labels inside the bar (`:center`, `:base`), `:outer` / `:leader` for `:outer`; `nil` without text."
  [options separator label-position value-text level-total {:keys [level slot n ^double y0 ^double y1] :as bar}]
  (let [value (value-label-text (data/option options :value-label false) value-text level-total bar)
        texts (cond-> []
                value (conj value)
                (data/option options :n-label false) (conj ((schema/text-formatter (data/option options :n-format "n=%d")) n)))]
    (when (seq texts)
      (let [[at side] (case label-position
                        :end (end-of bar)
                        :base (base-of bar)
                        [(m/* 0.5 (m/+ y0 y1)) 0.0])]
        (cond-> {:level level :slot slot :at at :side side :text (if (some text/formula-text? texts) (apply text/runs (interpose separator texts)) (str/join separator texts)) :span [y0 y1] :role :bar-value}
          (#{:center :base} label-position) (assoc :inside true)
          (= :outer label-position) (assoc :outer true :leader (data/option options :label-leader false)))))))

(defn- label-separator
  "Gives the separator of the value and count texts of one label.

  Parameters:

  - `options` (map): `:label-stack` - `:horizontal` or `:vertical`; default along the bars.
  - `flip?`: are the bars vertical?

  Returns a space for `:horizontal`, a line break for `:vertical`."
  [options flip?]
  (case (data/option options :label-stack (if flip? :vertical :horizontal))
    :horizontal " "
    :vertical "\n"))

(defn- value-title
  "Gives the suggested value-axis title.

  Parameters:

  - `stat`: the bar stat.
  - `position`: the group position.
  - `x`: the value accessor.
  - `bars` (sequence of maps): every bar (`:n`).

  Returns the count measure's title, `Proportion` for `:normalize`, `<stat> of <x>` for a value stat keyword over a keyword `:x` when some bar has more than one value; else `nil`."
  [stat position x bars]
  (cond
    (= :normalize position) "Proportion"
    (count-stats stat) (measure/titles stat)
    (and (value-stats stat) (keyword? x) (some #(m/> (long (:n %)) 1) bars)) (str (name stat) " of " (name x))
    :else nil))

(defn- baseline
  "Gives the value axis padding of the bars.

  Parameters:

  - `position`: the group position - `:normalize` bars span exactly their proportions (no padding).
  - `ys` (sequence of doubles): every bar end; empty - no bars.

  Returns `{:padding [lo hi] :request [lo hi]}`: [[cljplot2.data.rows/axis-padding]], none at a side the bars grow from (all ends on one side of 0) and none at all for `:normalize` (`[0 1]`, mirrored `[-1 1]`); `:request` - those zero sides (`nil` sides - none; `nil` when there are none)."
  [position ys]
  (let [lo (double (reduce m/min 0.0 ys))
        hi (double (reduce m/max 0.0 ys))
        normalize? (= :normalize position)
        from-lo? (or normalize? (m/zero? lo))
        from-hi? (or normalize? (m/zero? hi))
        [lo-pad hi-pad] data/axis-padding]
    {:padding [(if from-lo? 0.0 lo-pad) (if from-hi? 0.0 hi-pad)]
     :request (when (or from-lo? from-hi?) [(when from-lo? 0.0) (when from-hi? 0.0)])}))

(defn- compute-in-strip-frame
  "Computes the bars in the strip frame.

  Parameters:

  - `chart` (map): the chart value.
  - `chart-split` (map): `:groups`, `:mapping`.
  - `env` (map): the compute environment (`:theme`, `:flip`, `:coord`, `:plot-scales` ...).

  Returns per panel `{:bars [item ...] :style st :place p :options o :translucent b :zero-line b}`, items (see [[cljplot2.data.strip/split]]; a linear or binned colour value as `:color-value`, see [[colored-items]]) with `:length`, `:y0 :y1`, `:width-factor`, `:label` (the end label or `nil`); extents (x - the bar ends and 0, y - see [[cljplot2.data.strip/extents]]), padding, `:padding-px` (label room), `:outer-room`, `:scale-defaults`, `:na`, `:dropped`, `:axis-titles` (count measures, `Proportion`), `:tick-format` (mirror).

  Throws as [[check-options!]], [[item-lengths]], [[weight-factors]], [[color-value]]; `ex-info {:key :color-stat}` for `:color-stat` without a linear or binned `:color`; `ex-info {:key :label-position}` for `:outer` outside a polar plot."
  [{:keys [options] :as chart} {:keys [mapping] :as chart-split} {:keys [theme] :as env}]
  (check-options! options mapping)
  (let [stat (data/option options :stat :sum)
        position-option (get options :position)
        varwidth (:varwidth options)
        ;; a linear or binned colour doesn't group: each bar takes its own colour value
        ;; every row builds its bar: the colour domain judges the bars' colour values, not the rows
        continuous (group/continuous-color (:groups chart-split) mapping (assoc env :filter-domain false))
        _ (when (and (some? (:color-stat options)) (not continuous))
            (schema/fail! "Bar :color-stat aggregates a linear or binned :color: map :color to numbers (categorical colours group the bars)."
                          {:key :color-stat :value (:color-stat options)}))
        split-options (cond-> (dissoc options :position)
                        (some? position-option) (assoc :position (if (#{:stack :normalize} position-option) :overlay position-option))
                        (count-stats stat) (assoc ::strip/count-rows true)
                        (or continuous (keyword? varwidth) (fn? varwidth)) (assoc ::strip/rows true)
                        (:mirror options) (assoc ::strip/mirror (:mirror options)))
        {:keys [place panels dropped shared-levels mirror] grouped-items :items :as split}
        (strip/split (cond-> chart-split continuous (assoc :groups (:groups continuous) :mapping (:mapping continuous)))
                     split-options :center env)
        split-items (if continuous
                      (colored-items (color-stat-fn options stat) (:color-of continuous)
                                     (when-not (count-stats stat) (data/row-accessor :x (:x mapping)))
                                     grouped-items)
                      grouped-items)
        position (or position-option (:position split))
        placed-items (update-vals (item-lengths stat split-items) #(placed-bars position (select-keys options [:negative-order]) %))
        factor (weight-factors varwidth (mapcat val placed-items))
        sized-items (update-vals placed-items (fn [its] (mapv #(-> % (assoc :width-factor (factor %)) (dissoc :rows)) its)))
        all-bars (mapcat val sized-items)
        value-text (value-texts options (remove (fn [{:keys [^double y0 ^double y1]}] (m/== y0 y1)) all-bars))
        separator (label-separator options (:flip env))
        label-position (data/option options :label-position (if (contains? #{:stack :normalize} position) :center :end))
        _ (when (and (= :outer label-position) (not (coord/polar? (:coord env))))
            (schema/fail! "Bar :label-position :outer places labels outside a polar plot's circle: use :end or :center in cartesian plots."
                          {:key :label-position :value :outer}))
        items (update-vals sized-items
                           (fn [its]
                             (let [level-total (reduce (fn [acc {:keys [level length]}] (update acc level (fnil m/+ 0.0) (m/abs (double length))))
                                                       {} its)]
                               (mapv #(assoc % :label (end-label options separator label-position value-text level-total %)) its))))
        labels (keep :label (mapcat val items))
        {base-padding :padding request :request} (baseline position (mapcat (juxt :y0 :y1) all-bars))
        translucent? (and (= :overlay position) shared-levels)
        color-spec (get-in env [:plot-scales :color] {})
        color-values (update-vals items #(map :color-value %))
        color-stats (when continuous
                      (group/continuous-color-stats continuous color-values (:plot-scales env)))
        ;; NA colour also for a colour value outside the scale's explicit domain
        outside? (and continuous (some #(and (some? %) (not (aesthetic/kept-value? :color color-spec %)))
                                       (mapcat val color-values)))
        st (theme/resolved-style theme chart)]
    {:data (update-vals items (fn [its] {:bars its :style st :place place :options options
                                        :translucent translucent?
                                        :zero-line (and mirror (data/option options :zero-line true))}))
     :extents (merge-with merge (group/aesthetic-extents panels (:plot-scales env)) (:extents color-stats)
                          (strip/extents place items (fn [{:keys [^double y0 ^double y1]}]
                                                       [(m/min 0.0 y0 y1) (m/max 0.0 y0 y1)])))
     :padding (merge (assoc (strip/padding place (dissoc options :padding)) :x base-padding) (:padding options))
     :padding-explicit (data/explicit-padding (cond-> (or (strip/explicit-padding place {}) {})
                                                request (assoc :x request))
                                              options)
     ;; labels inside the bars (:base) need no room past them
     :padding-px (strip-chart/end-label-room (remove :inside labels) env)
     :outer-room (strip-chart/outer-label-room labels env)
     ;; outer labels replace the angle axis (it would sit under them)
     :scale-defaults (when (= :outer label-position) {(if (:flip env) :y :x) {:axis false}})
     :na (cond-> (group/na-dims panels) (or (:na color-stats) outside?) (conj :color))
     :dropped (cond-> dropped continuous (assoc :color (:dropped color-stats)))
     :axis-titles (if-let [t (value-title stat position (:x mapping) all-bars)] {:x t} {})
     :tick-format (when mirror {:x :absolute})}))

(defn- in-sector?
  "Tests whether a pixel lies inside a polar sector.

  Parameters:

  - `geometry` (map): the polar geometry (`:cx`, `:cy`).
  - `sector`: `[h0 h1 v0 v1]` - the angles (range positions, radians) and radii.
  - `point`: the pixel `[x y]`.

  Returns a truthy value when the point's angle, brought to `[h0, h0 + 2 pi)`, is at most `h1` and its radius in `[v0 v1]`."
  [{:keys [^double cx ^double cy]} [^double h0 ^double h1 ^double v0 ^double v1] [^double x ^double y]]
  (let [dx (m/- x cx)
        dy (m/- y cy)
        r (m/hypot dx dy)
        ;; the point's angle (0 up, clockwise) brought to [h0, h0 + 2 pi)
        a (m/+ h0 (m/mod (m/- (m/atan2 dx (m/- dy)) h0) m/TWO_PI))]
    (and (m/<= v0 r v1) (m/<= a h1))))

(defn- fits-segment?
  "Tests whether a centred label fits its bar's segment.

  Parameters:

  - `pl` (map): the strip placement (`:range-point`, `:geometry`).
  - `bar` (map): `:level`, `:slot`, `:y0`, `:y1`, `:width-factor`.
  - `box` (map): the label's pixel box `:x`, `:y`, `:w`, `:h`.

  Returns a truthy value when the box lies inside the segment: cartesian - its rectangle; polar - every box corner inside its sector (see [[in-sector?]])."
  [{:keys [range-point geometry]} {:keys [level slot ^double y0 ^double y1 ^double width-factor]} {:keys [^double x ^double y ^double w ^double h]}]
  (let [c0 (strip/in-slot slot 0.0)
        hw (m/* (strip/slot-half slot) width-factor)
        corners (for [v [y0 y1] cross [(m/- c0 hw) (m/+ c0 hw)]] (range-point v level cross))
        hs (map first corners)
        vs (map second corners)
        h0 (v/mn hs)
        h1 (v/mx hs)
        v0 (v/mn vs)
        v1 (v/mx vs)]
    (if geometry
      (let [sector [h0 h1 v0 v1]]
        (every? #(in-sector? geometry sector %) [[x y] [(m/+ x w) y] [x (m/+ y h)] [(m/+ x w) (m/+ y h)]]))
      (and (m/<= h0 x) (m/<= (m/+ x w) h1) (m/<= v0 y) (m/<= (m/+ y h) v1)))))

(defn- fitting-placements
  "Drops the centred labels that don't fit their segment.

  Parameters:

  - `canvas`: the clojure2d canvas (measuring).
  - `pl` (map): the strip placement.
  - `placements` (sequence of maps): end-label placements, each label carrying its `:bar`.
  - `env` (map): the drawing environment.

  Returns the placements past the bars' ends, the outer ones and the labels inside the bars (`:inside` - centred and base ones) fitting their segment (see [[fits-segment?]])."
  [canvas pl placements env]
  (filter (fn [{:keys [label content anchor x y]}]
            (or (not (:inside label))
                (fits-segment? pl (:bar label) (label/box-at canvas content anchor x y env))))
          placements))

(defn- visible-base
  "Moves a base label to the visible end nearest 0 when its bar's base lies outside the value domain.

  Parameters:

  - `label` (map): the end label - `:at` (its value), `:side`, `:inside`.
  - `x-scale` (map): the finalized value scale (`:domain`).

  Returns the label with `:at` constrained to the domain for a base label (`:inside`, a non-zero `:side`); other labels unchanged. A base label of a bar wholly outside the domain then lies outside its bar and is skipped (see [[fitting-placements]])."
  [{:keys [^double at side inside] :as label} {[d0 d1] :domain}]
  (if (and inside (m/not-zero? (double side)) (number? d0) (number? d1))
    (assoc label :at (m/constrain at (m/min (double d0) (double d1)) (m/max (double d0) (double d1))))
    label))

(defn- draw-in-strip-frame
  "Draws the bars of one panel in the strip frame, then the mirror's 0 line and the end labels.

  Parameters:

  - `_chart` (map): the chart value.
  - `computed` (map): the panel's `:bars`, `:style`, `:place`, `:options`, `:translucent`, `:zero-line`.
  - `canvas`: the clojure2d canvas.
  - `box` (map): the panel box.
  - `scales` (map): the finalized scales (strip frame).
  - `env` (map): the drawing environment (`:theme`).

  Zero-length bars, bars under half a pixel and bars of groups outside an aesthetic domain are skipped, with their labels; a bar's `:color-value` is drawn through the colour scale, the NA colour when it is `nil` or outside the scale's domain; labels of larger bars win collisions."
  [_chart {:keys [bars style place options translucent zero-line]} canvas box scales env]
  (let [{:keys [range-point geometry] :as pl} (strip-chart/placement box scales options place)
        stroke-width (double (:stroke-width style))
        pattern-fill (draw/pattern-fill-cache)
        rounding (draw/corner-rounding options)
        na (get-in env [:theme :palette :na])
        value-color (if-let [color-scale (:color scales)] (aesthetic/attribute-fn :color color-scale na) (constantly na))
        bar-style (fn [bar] (cond-> (group/style style (:aesthetics bar) scales (:theme env))
                              (contains? bar :color-value) (some-> (assoc :color (or (value-color (:color-value bar)) na)))))
        styled (keep (fn [bar] (some->> (bar-style bar) (vector bar))) bars)]
    (doseq [[{:keys [level slot ^double y0 ^double y1 ^double width-factor]} st] styled
            :when (m/not== y0 y1)
            :let [c0 (strip/in-slot slot 0.0)
                  hw (m/* (strip/slot-half slot) width-factor)
                  fill (draw/translucent-fill (:color st) (:alpha style) translucent)]]
      (let [[base end] (if rounding (draw/value-ends y0 y1) [y0 y1])]
        (draw/range-bar! canvas geometry (range-point base level (m/- c0 hw)) (range-point end level (m/+ c0 hw))
                         fill (draw/outline-color (:stroke-color st) (:color st)) stroke-width (pattern-fill st fill)
                         (draw/bar-rounding rounding (range-point base level c0) (range-point end level c0)))))
    (when zero-line
      (draw/zero-line! canvas box scales style :x))
    (let [labels (keep (fn [[{:keys [label] :as bar} st]]
                         (when label
                           (assoc (visible-base label (:x scales)) :bar bar
                                  :color (draw/text-color (:theme env) :bar-value
                                                          (if (:inside label) (:color st) (draw/plot-backdrop (:theme env)))))))
                       styled)
          ;; largest bars first: their labels win collisions
          ordered (sort-by (fn [{{:keys [^double y0 ^double y1]} :bar}] (m/- (m/abs (m/- y1 y0)))) labels)
          placements (fitting-placements canvas pl (strip-chart/end-label-placements pl ordered scales) env)]
      (strip-chart/draw-label-placements! canvas (strip-chart/without-collisions canvas placements env) env))))

(defmethod strip-chart/in-frame-compute :bar [chart chart-split env] (compute-in-strip-frame chart chart-split env))

(defmethod strip-chart/in-frame-draw :bar [chart computed canvas box scales env]
  (draw-in-strip-frame chart computed canvas box scales env))

(defn legend
  "Draws the key glyph: a filled square (see [[cljplot2.graphics.draw/bar-key!]]).

  Parameters:

  - `chart` (map): the chart value.
  - `attributes` (map): the key's attributes (`:color`, `:pattern`).
  - `canvas`: the clojure2d canvas.
  - `box` (map): the key box.
  - `env` (map): the drawing environment (`:theme`, `:chart-stats`).

  The square is translucent with a mapped `:color` when the bars are (overlay groups sharing a level, see [[cljplot2.chart.strip/compute]]) and rounded as the bars."
  [{:keys [options] :as chart} attributes canvas box env]
  (draw/bar-key! canvas box (theme/key-style env chart attributes [:color :pattern])
                 (and (contains? attributes :color) (some :translucent (vals (get-in env [:chart-stats :data]))))
                 (draw/corner-rounding options)))
