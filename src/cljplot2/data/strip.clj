(ns cljplot2.data.strip
  "1d strips: charts drawing the distribution of values (box, violin, stripplot, beeswarm, rug, extent), one value
  per item (bar) placed across the levels of a categorical axis.

  Orientation (see [[level-axis]]): the levels sit on the categorical mapping axis - `{:x level :y value}` draws
  vertical strips, `{:x value :y level}` horizontal ones; option `:level-axis` (`:x` / `:y`) decides when both or
  neither axis is categorical (and always wins). Charts compute and draw in the **strip frame** - levels on `:y`,
  values on `:x`, as described below - through [[compute-oriented]] / [[draw-oriented]], which transpose input,
  stats and scales for levels on `:x`. `:flip true` exchanges the drawn axes on top of that.

  Contract: a strip chart computes, per panel, **items** - one per `:y` level and group -
  `{:level l :key k :aesthetics a :values [..] :slot [lo hi]}` plus its own statistics, all in data units along the
  value axis and in normalized **cross** units across it: the placement's cross range is `[-0.5 0.5]`, `:slot` the
  part of it the item's group gets (see positions below). At draw, [[placement]] maps a value and a cross coordinate
  to a pixel point.

  Placement - option `:place` (axes named in the strip frame: with the levels on `:x` - e.g. a lone `{:y v}` mapping
  - read `:x` for `:y` below, so `{:at v}` and `:bottom` refer to the plot's `:x` scale):

  * `:band` (default when `:y` is mapped): `:y` levels on a `:bands` scale (strings, keywords, intervals - e.g.
    [[cljplot2.data.levels/bin]] - or numbers with
    `:scales {:y {:kind :bands}}`); an item's cross range spans `:width` (default 0.8) of the level's band
  * `{:at v}` (`:y` not mapped): centred on `y = v` of a numeric `:y` scale (from other charts; the chart's y extent
    includes `v`), `:thickness` pixels (default 20)
  * `:center` (default without `:y` for most strips): the panel's middle, `:width` of the panel's height (width when
    flipped)
  * `:edge` (the rug's default without `:y`): a `:thickness` pixels (default 8) strip along the panel's low y edge,
    inside the panel
  * `:bottom` (`:y` not mapped; lattice's layer under a density): between the panel's low y edge and `y = 0` of a
    numeric `:y` scale from other charts, `:width` of that gap; the chart's y extent (a companion - see
    [[cljplot2.chart]]) includes 0 and it asks for y padding `:bottom-share` (fraction of the y data span, default
    0.15; only with `:bottom`) below, so the gap exists. Without a `:y` scale (no other chart has y extents) the
    chart is an x-only chart placed as `:center`

  Groups (ordinal aesthetics and `:group`, see [[cljplot2.data.group]]) inside a placement - option `:position`:
  `:dodge` (default when some level holds two or more groups; the cross range split equally, each part narrowed by
  `:dodge-gap` - a fraction of it, default 0.1 - so groups don't touch), `:overlay` (the whole range; the default
  otherwise - one group per level fills its band) or `:mirror` (exactly two groups, one half each; split violins). Dodged and mirrored groups follow reading order, as the legend: the first
  group on top of horizontal strips, on the left of vertical (`:flip`) ones.

  Mirror (bar, extent) - option `:mirror` (passed to [[split]] as `::mirror`): a grouping dimension (an ordinal
  aesthetic or `:group`) with exactly two levels; items of the second level are `:mirrored? true` - the chart draws
  them on the negative side; groups differing only in that dimension share a slot (the default `:position` looks at
  the remaining groups). Not with `:position :mirror` (half slots).

  Ridgelines (histogram and density with a categorical or bands `:y`, see [[ridges?]]): one-sided profiles growing
  from the band centre toward the next level, the tallest `:overlap` (default 0.9) band steps high.

  Every strip chart throws `ex-info {:key :mapping}` when its rows have no finite `:x` value (except a chart counting
  rows, see [[split]]).

  n labels (every strip chart): `:n-label true` draws each item's count, formatted by `:n-format` (default
  `n=%d`), just past its largest drawn value (see [[draw-n-labels!]]); the chart asks for value-axis room for
  them (`:padding-px`, see [[n-label-room]]).

  Functions:

  - options: [[options]], [[mirror-option]], [[ridge-options]], [[default-ridge-overlap]], [[values-mapping]]
  - items and stats: [[split]], [[y-extent]], [[padding]], [[explicit-padding]], [[extents]]
  - geometry: [[placement]], [[in-slot]], [[slot-half]], [[slot-side]]
  - end labels: [[end-label-room]], [[end-label-placements]], [[without-collisions]], [[draw-label-placements!]], [[draw-end-labels!]], [[outer-label-room]], [[n-label-room]], [[draw-n-labels!]]
  - ridgelines: [[ridges?]], [[ridge-split]], [[ridge-scale-defaults]], [[ridge-point]], [[ridge-order]]
  - orientation: [[level-axis]], [[compute-oriented]], [[draw-oriented]]"
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.content.label :as label]
            [cljplot2.data.group :as group]
            [cljplot2.data.position :as position]
            [cljplot2.data.rows :as data]
            [cljplot2.data.shingle :as shingle]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.scale.coord :as coord]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private named-places
  "Named values of the `:place` option (besides `{:at v}`)."
  #{:band :center :edge :bottom})

(def ^:private positions
  "Values of the `:position` option."
  #{:dodge :overlay :mirror})

(def options
  "Map entries of the options every strip chart takes (shared group `:strip`, besides [[cljplot2.data.rows/common-options]]): placement, group positions, n labels and the level axis (see the namespace doc)."
  (schema/in-fragment
   :strip
   [(schema/optional :place {:cljplot2/section :placement :description "where the items sit across the value axis: :band (one band per level), {:at v} (centred on v of a numeric scale), :center, :edge or :bottom (between the panel edge and 0)"
                             :cljplot2/default-doc ":band with a level mapping, else the chart's own (:center for most, :edge for the rug)"}
                     [:multi {:dispatch map?}
                      [true [:map {:closed true} [:at schema/finite]]]
                      [false (into [:enum] (sort named-places))]])
    (schema/optional :width {:cljplot2/section :placement :description "fraction of the level's band (or of the panel for :center and :bottom) an item spans across the value axis"
                             :default 0.8}
                     schema/non-neg)
    (schema/optional :thickness {:cljplot2/section :placement :description "item thickness in pixels for :place {:at v} and :edge"
                                 :cljplot2/default-doc "20 pixels for {:at v}, 8 for :edge"}
                     schema/non-neg)
    (schema/optional :position {:cljplot2/section :groups :description "how groups of one level share its band: :dodge (side by side), :overlay (on top of each other) or :mirror (two groups, one half each)"
                                :cljplot2/default-doc ":dodge when a level holds two or more groups, else :overlay (dotplot: always :overlay)"}
                     (into [:enum] (sort positions)))
    (schema/optional :dodge-gap {:cljplot2/section :groups :description "gap between dodged groups, a fraction of a group's part of the band"
                                 :default 0.1}
                     schema/fraction)
    (schema/optional :bottom-share {:cljplot2/section :placement :description "padding below the data asked for with :place :bottom (only there), a fraction of the y data span"
                                    :cljplot2/default-doc "0.15 with :place :bottom"}
                     schema/fraction)
    (schema/nilable :n-label {:cljplot2/section :n-label :description "draw each item's row count just past its largest value"
                              :default false}
                    schema/bool)
    (schema/optional :n-format {:cljplot2/section :n-label :description "format string of the n labels"
                                :default "n=%d"}
                     :string)
    (schema/optional :level-axis {:cljplot2/section :placement :description "the mapping axis holding the levels (decides when both or neither of :x and :y are categorical)"
                                  :cljplot2/default-doc "the categorical axis of the mapping; with neither, :x when only :y is mapped, else :y"}
                     [:enum :x :y])]))

(def mirror-option
  "Map entry of the `:mirror` option (shared group `:mirror`) of strip charts drawing a negative side (bar, extent)."
  (first (schema/in-fragment
          :mirror
          [(schema/optional :mirror {:description "a grouping dimension with exactly two levels; the second level's items are drawn on the negative side"}
                            (into [:enum] (conj aesthetic/dims :group)))])))

(def ^{:const true :tag 'double} default-ridge-overlap
  "Height of the tallest ridge in band steps when `:overlap` isn't given (see [[ridge-options]])."
  0.9)

(def ridge-options
  "Map entries of the ridgeline options (shared group `:ridge`) of histogram and density with a categorical `:y`."
  (schema/in-fragment
   :ridge
   [(schema/optional :overlap {:description "height of the tallest ridge in band steps (above 1 - ridges overlap the next level)"
                               :default default-ridge-overlap}
                     schema/non-neg)]))

(defn- place-of
  "Gives the placement of a strip chart.

  Parameters:

  - `chart-options` (map): chart options - `:place`, `:bottom-share`.
  - `mapping` (map): the chart's merged mapping.
  - `default-place`: the chart's placement without a `:y` mapping.

  Returns the explicit `:place`, else `:band` with a `:y` mapping, else `default-place`.

  Throws `ex-info {:key :place}` for `:band` without a `:y` mapping or another placement with one, `ex-info {:key :bottom-share}` for `:bottom-share` without `:place :bottom`."
  [chart-options mapping default-place]
  (let [y? (some? (:y mapping))
        place (get chart-options :place (if y? :band default-place))]
    (cond
      (and (some? (:bottom-share chart-options)) (not= :bottom place))
      (schema/fail! "Strip :bottom-share needs :place :bottom." {:key :bottom-share :value (:bottom-share chart-options)})
      (and (= :band place) (not y?)) (schema/fail! "Strip :place :band needs a :y mapping." {:key :place :value place})
      (and y? (not= :band place)) (schema/fail! "A strip with a :y mapping is placed in bands (:place :band)."
                                                 {:key :place :value place})
      :else place)))

(defn- categorical-level?
  "Tests whether `v` is a categorical level by itself: a string, keyword or interval (shingle / bin)."
  [v]
  (or (string? v) (keyword? v) (shingle/interval? v)))

(defn- check-levels!
  "Checks that `:y` levels can be bands.

  Parameters:

  - `levels` (collection): the valid `:y` values.
  - `plot-scales` (map): the plot's `:scales`.

  Returns `nil`.

  Throws `ex-info {:key :place}` unless every level is a string, keyword or interval, or a number or date (`java.time` value) on an explicit `:bands` plot scale (declared categorical or a categorical marker)."
  [levels plot-scales]
  (let [bands? (= :bands (get-in plot-scales [:y :kind]))]
    (when-not (every? #(or (categorical-level? %) (and bands? (or (number? %) (data/temporal? %)))) levels)
      (schema/fail! "Strip levels (the level axis, see :level-axis) must be categorical (strings / keywords / intervals), or numbers or dates declared categorical (:scales {:y {:type :categorical}} or :mapping {:y (levels/categorical k)})."
                    {:key :place :value (first (remove categorical-level? levels))}))))

(defn- slots
  "Gives the cross sub-range of every group key.

  Parameters:

  - `position` (keyword): `:overlay` (the whole cross range `[-0.5 0.5]`), `:dodge` (equal parts) or `:mirror` (one half each).
  - `group-keys` (vector): the ordered slot keys.
  - `gap` (double): the share of a dodged part lost, split between its ends.
  - `first-high?` (boolean): the first key takes the high end of the cross range (toward higher `:y`), the following ones step down; else the low end first.

  Returns a map: group key -> `[lo hi]` in cross units.

  Throws `ex-info {:key :position}` for `:mirror` without exactly two keys."
  [position group-keys ^double gap first-high?]
  (let [n (count group-keys)]
    (case position
      :overlay (zipmap group-keys (repeat [-0.5 0.5]))
      :dodge (zipmap group-keys (map (fn [^long i]
                                       (let [[^double a ^double b] (position/dodge-interval [-0.5 0.5] (if first-high? (m/long-sub n 1 i) i) n)
                                             inset (m/* 0.5 gap (m/- b a))]
                                         [(m/+ a inset) (m/- b inset)]))
                                     (range n)))
      :mirror (if (m/== 2 n)
                (zipmap group-keys (if first-high? [[0.0 0.5] [-0.5 0.0]] [[-0.5 0.0] [0.0 0.5]]))
                (schema/fail! "Strip :position :mirror needs exactly two groups." {:key :position :groups n})))))

(defn- mirror-index
  "Gives the index of the mirror dimension among the grouping dimensions.

  Parameters:

  - `chart-options` (map): chart options - `::mirror` (the dimension), `:position`.
  - `dims` (vector): the grouping dimensions.

  Returns the index, `nil` without `::mirror`.

  Throws `ex-info {:key :mirror}` when the dimension isn't a grouping dimension or comes with `:position :mirror`."
  [chart-options dims]
  (when-let [k (::mirror chart-options)]
    (let [i (.indexOf ^java.util.List dims k)]
      (when (m/neg? i)
        (schema/fail! "Strip :mirror should be a mapped grouping dimension (an ordinal aesthetic or :group)."
                      {:key :mirror :value k}))
      (when (= :mirror (:position chart-options))
        (schema/fail! "Strip :mirror doesn't combine with :position :mirror (half slots)." {:key :mirror :value k}))
      i)))

(defn- without-index
  "Gives the group key `group-key` (a vector) without its value at index `i`."
  [group-key ^long i]
  (into (subvec group-key 0 i) (subvec group-key (m/long-inc i))))

(defn- mirrored-level
  "Gives the level of the mirror dimension drawn on the negative side.

  Parameters:

  - `group-keys` (vector): the ordered group keys.
  - `i` (long): the index of the mirror dimension in a key.
  - `k` (keyword): the mirror dimension (for the error).

  Returns the second of its levels.

  Throws `ex-info {:key :mirror}` unless the dimension has exactly two levels."
  [group-keys ^long i k]
  (let [levels (vec (distinct (map #(nth % i) group-keys)))]
    (when-not (m/== 2 (count levels))
      (schema/fail! "Strip :mirror needs exactly two levels of its dimension." {:key :mirror :value k :levels levels}))
    (levels 1)))

(defn- shared-levels?
  "Tests whether some level holds items of two or more slots.

  Parameters:

  - `items` (map): `{panel [item ...]}`; an item's `:level` is `nil` without bands - the whole panel.
  - `slot-key` (function): an item's group key -> its slot key.

  Returns a truthy value when some level of a panel holds items of two or more slot keys, else `nil` (charts keep it in their computed data, e.g. the bar's `:translucent?`)."
  [items slot-key]
  (some (fn [its] (some (fn [[_ level-items]] (m/> (count (distinct (map (comp slot-key :key) level-items))) 1))
                        (group-by :level its)))
        (vals items)))

(defn- levels-in-order
  "Splits rows by their `:y` level, levels in order.

  Parameters:

  - `y-of` (function): the `:y` row accessor.
  - `level-rank` (map): level -> its first appearance in the chart's rows (`group-by` alone keeps no order past 8 levels).
  - `rows` (sequence): the rows; rows without a valid `:y` value are left out.

  Returns `[[level rows] ...]` ordered by `level-rank`."
  [y-of level-rank rows]
  (sort-by (comp level-rank first) (group-by y-of (filter (comp data/valid-value? y-of) rows))))

(defn split
  "Splits a strip chart's rows into items: per panel, one per group (ordinal aesthetics, `:group`) and `:y` level.

  Parameters:

  - `chart-split` (map): `{:groups {panel rows} :mapping m}` - the chart's rows per panel and merged mapping.
  - `chart-options` (map): chart options (see [[options]]) and the internal options of charts built on strips:
    - `::count-rows true` - `:x` isn't read: every item has `:values []` and `:n` - its count of rows (an item per level with rows; no `:x` errors or drops)
    - `::rows true` - items also carry their `:rows`
    - `::mirror k` - the chart's `:mirror` dimension (see the namespace doc): items carry `:mirrored?`, the result `:mirror? true`; charts without a negative side never pass it
    - `::ridge true` - ridgelines: the first group takes the high cross end
  - `default-place`: the chart's placement without a `:y` mapping.
  - `env` (map): `:plot-scales`, `:na-level?` (see [[cljplot2.data.group/split]]), `:flip?`.

  Returns `{:place :position :items :group-keys :slot-keys :shared-levels? :mirror? :dims :panels :dropped}`: `:items` - `{panel [item ...]}`, an item `{:level :key :aesthetics :values :n :slot}` (`:level` `nil` without bands, `:values` - the finite `:x` numbers, `:n` - their count); `:dims` - the grouping dimensions (the order of a group key's values); `:slot-keys` - the keys slots are given to (group keys without the mirror dimension); `:shared-levels?` - some level holds two or more of them (the default `:position` is then `:dodge`, else `:overlay`); `:panels` - the split groups; `:dropped` - `{dim n}`.

  Throws `ex-info {:key :mapping}` when values aren't mapped, or there are rows but no finite `:x` value (or, placed in bands, no valid `:y` value); `ex-info {:key :n-format}` for an `:n-format` that can't format a count; `ex-info {:key :mirror}` for a mirror dimension that isn't a grouping dimension, hasn't exactly two levels or comes with `:position :mirror`; `ex-info {:key :place}` for a placement that doesn't fit the mapping or levels."
  [{:keys [groups mapping]} chart-options default-place env]
  (let [_ (when-let [fmt (:n-format chart-options)]
            (try (format fmt 1)
                 (catch Exception e
                   (schema/fail! "Strip :n-format should format a count (e.g. \"n=%d\")."
                                 {:key :n-format :value fmt :cause (ex-message e)}))))
        place (place-of chart-options mapping default-place)
        {:keys [panels dims dropped]} (group/split groups (dissoc mapping :y) env)
        count-rows? (::count-rows chart-options)
        _ (when (and (not count-rows?) (nil? (:x mapping)))
            (schema/fail! "Strip values aren't mapped: map them on the axis other than the levels (see :level-axis)."
                          {:key :mapping :value :values}))
        x-of (when-not count-rows? (data/row-accessor :x (:x mapping)))
        y-of (when (= :band place) (data/row-accessor :y (:y mapping)))
        all-rows (mapcat :rows (mapcat val panels))
        y-levels (when y-of (filter data/valid-value? (map y-of all-rows)))
        _ (when (and y-of (seq all-rows) (empty? y-levels))
            (schema/fail! "No valid values for a mapped dimension - check the mapping."
                          {:key :mapping :value :y :rows (count all-rows)}))
        _ (when y-of (check-levels! y-levels (:plot-scales env)))
        level-rank (zipmap (distinct y-levels) (range))
        group-keys (group/ordered-keys (mapcat #(map :key %) (vals panels)) dims (:plot-scales env))
        mi (mirror-index chart-options dims)
        slot-key (if mi #(without-index % mi) identity)
        slot-keys (vec (distinct (map slot-key group-keys)))
        mirrored (when (and mi (seq group-keys)) (mirrored-level group-keys mi (::mirror chart-options)))
        grouped-items (update-vals panels
                           (fn [gs]
                             (vec (for [g gs
                                        [level rows] (if y-of
                                                       (levels-in-order y-of level-rank (:rows g))
                                                       [[nil (:rows g)]])
                                        :let [values (if x-of (:values (data/finite-numbers (map x-of rows))) [])]
                                        :when (or count-rows? (seq values))]
                                    (cond-> {:level level :key (:key g) :aesthetics (:aesthetics g) :values values
                                             :n (if count-rows? (count rows) (count values))}
                                      mi (assoc :mirrored? (= mirrored (nth (:key g) mi)))
                                      (::rows chart-options) (assoc :rows rows))))))
        shared? (shared-levels? grouped-items slot-key)
        position (get chart-options :position (if shared? :dodge :overlay))
        ;; reading order: the first group on top of horizontal strips (the high cross end), on the left of vertical
        ;; (flipped) ones (the low end); ridgelines grow the first group toward the next level (the high end)
        first-high? (or (::ridge chart-options) (not (:flip? env)))
        slot-of (if (seq slot-keys) (slots position slot-keys (double (get chart-options :dodge-gap 0.1)) first-high?) {})
        items (update-vals grouped-items (fn [its] (mapv #(assoc % :slot (slot-of (slot-key (:key %)))) its)))
        x-dropped (if x-of
                    (long (count (remove #(let [v (x-of %)] (and (number? v) (m/valid-double? (double v)))) all-rows)))
                    0)]
    (when (and x-of (seq all-rows) (m/== x-dropped (count all-rows)))
      (schema/fail! "No valid values for a mapped dimension - check the mapping."
                    {:key :mapping :value :x :rows (count all-rows)}))
    {:place place
     :position position
     :items items
     :group-keys group-keys
     :slot-keys slot-keys
     :shared-levels? shared?
     :mirror? (some? mi)
     :dims dims
     :panels panels
     :dropped (cond-> (assoc dropped :x x-dropped)
                y-of (assoc :y (count (remove (comp data/valid-value? y-of) all-rows))))}))

(defn y-extent
  "Gives the `:y` extent of a panel's items.

  Parameters:

  - `place`: the chart's placement.
  - `items` (sequence): the panel's items.

  Returns the extent of the levels for `:band` (numeric and date levels also as `:values`), `v` for `{:at v}`, a companion extent of 0 for `:bottom`, else `nil`."
  [place items]
  (cond
    (= :band place) (let [levels (distinct (map :level items))
                          e (data/extent :y levels)]
                      ;; numeric and date levels (on an explicit :bands scale) travel as the extent's :values
                      (cond-> e (#{:numeric :temporal} (:kind e)) (assoc :values (vec levels))))
    (map? place) {:kind :numeric :min (double (:at place)) :max (double (:at place))}
    (= :bottom place) {:kind :numeric :min 0.0 :max 0.0 :companion? true}
    :else nil))

(defn padding
  "Gives the position padding of a strip chart.

  Parameters:

  - `place`: the chart's placement.
  - `chart-options` (map): chart options - `:bottom-share` (default 0.15), `:padding`.

  Returns `{:x [0.05 0.05]}`, for `:bottom` also `{:y [bottom-share 0]}`, with the `:padding` option merged over it."
  [place chart-options]
  (merge {:x [0.05 0.05]}
         (when (= :bottom place) {:y [(double (get chart-options :bottom-share 0.15)) 0.0]})
         (:padding chart-options)))

(defn explicit-padding
  "Gives the `:padding-explicit` stat of a strip chart (see [[cljplot2.data.rows/explicit-padding]]).

  Parameters:

  - `place`: the chart's placement.
  - `chart-options` (map): chart options - `:bottom-share` (default 0.15), `:padding`.

  Returns the requests: for `:bottom` the room below the data, `{:y [bottom-share nil]}`, with the `:padding` option over it; `nil` without any."
  [place chart-options]
  (data/explicit-padding (when (= :bottom place) {:y [(double (get chart-options :bottom-share 0.15)) nil]}) chart-options))

(defn extents
  "Gives the position extents of a strip chart per panel.

  Parameters:

  - `place`: the chart's placement.
  - `items` (map): `{panel [item ...]}` (see [[split]]).
  - `x-range` (function): item -> `[lo hi]` in data units along the value axis, `nil` - none.

  Returns `{panel {:x extent :y extent}}`: `:x` over the items' ranges, `:y` by `place` ([[y-extent]], left out when `nil`); panels without ranges left out."
  [place items x-range]
  (into {} (keep (fn [[panel its]]
                   (let [ranges (keep x-range its)]
                     (when (seq ranges)
                       [panel (cond-> {:x {:kind :numeric
                                           :min (v/mn (map first ranges))
                                           :max (v/mx (map second ranges))}}
                                (y-extent place its) (assoc :y (y-extent place its)))]))))
        items))

(defn in-slot
  "Maps a cross coordinate of the whole cross range into an item's slot.

  Parameters:

  - `slot` (`[lo hi]`): the item's slot in cross units.
  - `cross` (double): a coordinate of the cross range `[-0.5 0.5]`.

  Returns the cross coordinate inside the slot: `lo` for -0.5, `hi` for 0.5."
  ^double [[^double lo ^double hi] ^double cross]
  (m/+ lo (m/* (m/+ cross 0.5) (m/- hi lo))))

(defn slot-half
  "Gives half the length of a slot `[lo hi]` in cross units."
  ^double [[^double lo ^double hi]]
  (m/* 0.5 (m/- hi lo)))

(defn placement
  "Gives the pixel geometry of a strip chart's placement in a panel (see the namespace doc).

  Flip-aware: the cross axis is the `:y` direction (horizontal when flipped). In a polar plot range lengths and `:step` are in range units (radians across angles).

  Parameters:

  - `box` (map): the panel's `:w`, `:h` in pixels.
  - `scales` (map): finalized position scales.
  - `options` (map): chart options - `:width` (fraction of the band or panel, default 0.8), `:thickness` (pixels, default 8 for `:edge`, 20 otherwise).
  - `place`: the chart's placement; `:bottom` without a `:y` scale is placed as `:center`.

  Returns a map:

  - `:point` - `(fn [value level cross] -> Vec2)`: `value` along the `:x` scale, the cross coordinate (placement range `[-0.5 0.5]`, positive toward higher `:y`) across it
  - `:range-point` - `(fn [value level cross] -> [h v])`: the point in range space (pixels in cartesian plots, angle and radius in polar ones, see [[cljplot2.scale.coord]])
  - `:span` - `(fn [level] -> px)`: the length of the whole cross range
  - `:value-pixel` - `(fn [value] -> px)` along the value axis
  - `:centre` - `(fn [level] -> px)` of the cross range's centre
  - `:sign` - the range direction of increasing `:y`
  - `:step` - the band step (`:band`, else `nil`)
  - `:geometry` - the polar geometry or `nil`

  A level or value the scales can't place gives `##NaN` coordinates.

  Throws `ex-info {:key :place}` for `:band` without a `:bands` `:y` scale, `{:at v}` and `:bottom` on a `:bands` one."
  [{:keys [w h]} scales {:keys [^double width thickness] :or {width 0.8}} place]
  (let [_ (when (and (= :bottom place) (= :bands (:kind (:y scales))))
            (schema/fail! "Strip :place :bottom needs a numeric :y scale, not bands." {:key :place :value place}))
        placed-as (if (and (= :bottom place) (nil? (:y scales))) :center place)
        fx (scale/forward-fn (:x scales))
        y-scale (:y scales)
        fy (when y-scale (scale/forward-fn y-scale))
        cross-horizontal? (scale/flipped? scales)
        polar (scale/coord-geometry scales)
        ;; polar: the cross range is the y scale's range, else the angular / radial extent of the panel's circle
        [^double e0 ^double e1] (when polar
                                  (or (:range y-scale)
                                      (if cross-horizontal? (:theta polar) [(:inner-radius polar) (:radius polar)])))
        length (double (cond polar (m/abs (m/- e1 e0)) cross-horizontal? w :else h))
        ;; range direction of increasing :y
        sign (double (cond
                       y-scale (let [[^double a ^double b] (:range y-scale)] (if (m/< b a) -1.0 1.0))
                       polar (if (m/< e1 e0) -1.0 1.0)
                       :else (if cross-horizontal? 1.0 -1.0)))
        ;; range position of the cross range's low-value end and of its middle
        low (double (cond polar e0 (m/pos? sign) 0.0 :else length))
        middle (if polar (m/* 0.5 (m/+ e0 e1)) (m/* 0.5 length))
        thickness-px (double (or thickness (if (= :edge placed-as) 8.0 20.0)))
        ;; pixels; across angles at the outer radius
        cross-thickness (if (and polar cross-horizontal?) (m// thickness-px (double (:radius polar))) thickness-px)
        geometry (when (= :band placed-as)
                   (or (scale/band-geometry y-scale)
                       (schema/fail! "Strip :place :band needs a :bands :y scale." {:key :place :value (:kind y-scale)})))
        bandwidth (some-> geometry :bandwidth double)
        _ (when (and (map? placed-as) (= :bands (:kind y-scale)))
            (schema/fail! "Strip :place {:at v} needs a numeric :y scale, not bands." {:key :place :value placed-as}))
        centre-and-span (cond
                          (= :band placed-as) (fn [level] [(if fy (fy level) ##NaN) (m/* width (double bandwidth))])
                          (map? placed-as) (let [c (if fy (fy (:at placed-as)) ##NaN)] (constantly [c cross-thickness]))
                          (= :center placed-as) (constantly [middle (m/* width length)])
                          (= :edge placed-as) (constantly [(m/+ low (m/* sign 0.5 cross-thickness)) cross-thickness])
                          (= :bottom placed-as) (let [zero (double (fy 0.0))]
                                                  (constantly [(m/* 0.5 (m/+ low zero)) (m/* width (m/abs (m/- zero low)))])))
        range-point (fn [value level ^double cross]
                      (let [[^double c ^double span] (centre-and-span level)
                            across (m/+ c (m/* sign cross span))
                            along (double (fx value))]
                        (if cross-horizontal? [across along] [along across])))]
    {:point (if polar
              (fn ^Vec2 [value level ^double cross]
                (let [[^double horizontal ^double vertical] (range-point value level cross)]
                  (coord/to-pixel polar horizontal vertical)))
              (fn ^Vec2 [value level ^double cross]
                (let [[^double horizontal ^double vertical] (range-point value level cross)]
                  (Vec2. horizontal vertical))))
     :range-point range-point
     :geometry polar
     :span (fn ^double [level] (double (second (centre-and-span level))))
     :value-pixel (fn ^double [value] (double (fx value)))
     :centre (fn ^double [level] (double (first (centre-and-span level))))
     :sign sign
     :step (some-> geometry :step double)}))

(def ^{:const true :private true :tag 'double} n-label-gap
  "Pixels between an item's largest drawn value and its n label."
  3.0)

(defn- n-label-side
  "Gives the direction of an item's n label along the value axis: `-1.0` for a `:mirrored?` item, else `1.0`."
  ^double [item]
  (if (:mirrored? item) -1.0 1.0))

(defn- n-label-text
  "Gives the n label text of an item: its count `:n` formatted by the option `:n-format` of `chart-options` (default `n=%d`)."
  [chart-options item]
  (format (get chart-options :n-format "n=%d") (:n item)))

(defn- label-content
  "Gives the label content of an end label: `{:kind :label :role :text}` - its `:text` in its `:role` (default `:annotation`), with its `:color` when given."
  [{:keys [text color role]}]
  (cond-> {:kind :label :role (or role :annotation) :text text} color (assoc :color color)))

(defn end-label-room
  "Gives the `:padding-px` end labels need past the data.

  Parameters:

  - `labels` (sequence): end labels `{:text t :side s :role r}` (see [[draw-end-labels!]]).
  - `env` (map): `:theme`, `:hint` (measuring), `:flip?` (sizes along the value axis are label heights when flipped).

  Returns `{:x [lo hi]}`: per side of the value axis the largest label's size along it, measured in its role, plus the gap; 0 on a side without labels. `nil` without labels."
  [labels {:keys [theme flip? hint]}]
  (when (seq labels)
    (measure/call-with-scratch-canvas
     hint
     (fn [c]
       (let [room (fn [^double side]
                    (let [contents (distinct (keep #(when (m/== side (double (:side %))) (label-content (dissoc % :color)))
                                                   labels))]
                      (if (seq contents)
                        (m/+ (v/mx (map (fn [content] (let [^Vec2 size (measure/natural-size content
                                                                                            {:canvas c :theme theme})]
                                                       (if flip? (.y size) (.x size))))
                                        contents))
                             n-label-gap)
                        0.0)))]
         {:x [(room -1.0) (room 1.0)]})))))

(def ^{:const true :tag 'double} outer-label-gap
  "Pixels between a polar plot's outer radius and an `:outer?` end label."
  6.0)

(def ^{:const true :tag 'double} leader-label-gap
  "Pixels between a polar plot's outer radius and an `:outer?` end label with a leader line (`:leader?`)."
  16.0)

(defn- outer-gap
  "Gives the pixels between a polar plot's outer radius and an `:outer?` end label: [[leader-label-gap]] with `:leader?`, else [[outer-label-gap]]."
  ^double [{:keys [leader?]}]
  (if leader? leader-label-gap outer-label-gap))

(declare polar-end-label-placements cartesian-end-label-placements)

(defn end-label-placements
  "Gives where [[draw-end-labels!]] draws end labels.

  Parameters:

  - `pl` (map): the chart's [[placement]].
  - `labels` (sequence): end labels (see [[draw-end-labels!]]).
  - `scales` (map): finalized position scales.

  Returns a vector of `{:label l :content c :anchor [h v] :x px :y py :leader [[x0 y0] [x1 y1]]}`: `:content` - the label content, `:leader` (an `:outer?` label with `:leader?`) - a line from its segment's outer edge. Labels without a placeable position are left out."
  [{:keys [geometry] :as pl} labels scales]
  (if geometry
    (polar-end-label-placements pl labels scales)
    (cartesian-end-label-placements pl labels scales)))

(defn- cartesian-end-label-placements
  "Gives the [[end-label-placements]] of a cartesian plot: each label [[n-label-gap]] pixels past its value along the value axis, anchored away from it (centred for side 0).

  Parameters:

  - `pl` (map): the chart's [[placement]] - `:point`.
  - `labels` (sequence): end labels (see [[draw-end-labels!]]).
  - `scales` (map): finalized position scales.

  Returns a vector of placements; labels at a non-finite pixel left out."
  [{:keys [point]} labels scales]
  (let [x-scale (:x scales)
        [^double r0 ^double r1] (:range x-scale)
        ;; pixel direction of increasing values
        up (if (m/< r1 r0) -1.0 1.0)
        horizontal? (scale/horizontal? :x x-scale)]
    (vec (for [{:keys [level slot at ^double side] :as l} labels
               :let [dir (m/* up side)
                     anchor (cond
                              (m/zero? dir) [:center :middle]
                              horizontal? (if (m/pos? dir) [:left :middle] [:right :middle])
                              :else (if (m/pos? dir) [:center :top] [:center :bottom]))
                     ^Vec2 p (point at level (in-slot slot 0.0))
                     x (if horizontal? (m/+ (.x p) (m/* dir n-label-gap)) (.x p))
                     y (if horizontal? (.y p) (m/+ (.y p) (m/* dir n-label-gap)))]
               :when (and (m/valid-double? x) (m/valid-double? y))]
           {:label l :content (label-content l) :anchor anchor :x x :y y}))))

(defn- outer-placement
  "Gives the placement of an `:outer?` end label in a polar plot.

  Parameters:

  - `pl` (map): the chart's [[placement]] - `:range-point`, `:geometry`.
  - `label` (map): the end label - `:level`, `:slot`, `:at` (the value of its segment's middle), `:span` (its segment's values), `:leader?`.

  Returns the placement past the outer radius (by [[outer-gap]]) at the angle of the segment's middle, anchored away from the centre, with a `:leader` from the segment's farthest corner for `:leader?`; `nil` at a non-finite pixel."
  [{:keys [range-point geometry]} {:keys [level slot at span leader?] :as l}]
  (let [[^double h] (range-point at level (in-slot slot 0.0))
        r (m/+ (double (:radius geometry)) (outer-gap l))
        ^Vec2 p (coord/to-pixel geometry h r)
        edge (when leader?
               (v/mx (for [value span cross [-0.5 0.5]] (double (second (range-point value level (in-slot slot cross)))))))]
    (when (and (m/valid-double? (.x p)) (m/valid-double? (.y p)))
      (cond-> {:label l :content (label-content l) :anchor (coord/outward-anchor h) :x (.x p) :y (.y p)}
        (and edge (m/valid-double? (double edge)))
        (assoc :leader (let [^Vec2 a (coord/to-pixel geometry h (double edge))
                             ^Vec2 b (coord/to-pixel geometry h (m/- r 1.0))]
                         [[(.x a) (.y a)] [(.x b) (.y b)]]))))))

(defn- polar-end-label-placements
  "Gives the [[end-label-placements]] of a polar plot: past the value along the radius (anchored away from the centre) or along the angle (centred), by [[n-label-gap]] pixels; `:outer?` labels past the outer radius (see [[outer-placement]]).

  Parameters:

  - `pl` (map): the chart's [[placement]] - `:range-point`, `:geometry`.
  - `labels` (sequence): end labels (see [[draw-end-labels!]]).
  - `scales` (map): finalized position scales.

  Returns a vector of placements; labels at a non-finite pixel left out."
  [{:keys [range-point geometry] :as pl} labels scales]
  (let [x-scale (:x scales)
        radial? (not (scale/horizontal? :x x-scale))
        [^double r0 ^double r1] (:range x-scale)
        up (if (m/< r1 r0) -1.0 1.0)
        ;; the gap in range units: pixels along the radius, radians at the outer radius along the angle
        pad (if radial? n-label-gap (m// n-label-gap (double (:radius geometry))))]
    (vec (keep (fn [{:keys [level slot at ^double side outer?] :as l}]
                 (if outer?
                   (outer-placement pl l)
                   (let [dir (m/* up side)
                         [^double h ^double v] (range-point at level (in-slot slot 0.0))
                         shift (m/* dir pad)
                         ^Vec2 p (if radial?
                                   (coord/to-pixel geometry h (m/+ v shift))
                                   (coord/to-pixel geometry (m/+ h shift) v))
                         anchor (if (and radial? (not (m/zero? dir))) (coord/outward-anchor h) [:center :middle])]
                     (when (and (m/valid-double? (.x p)) (m/valid-double? (.y p)))
                       {:label l :content (label-content l) :anchor anchor :x (.x p) :y (.y p)}))))
               labels))))

(defn without-collisions
  "Drops end-label placements overlapping one kept before them - earlier placements win.

  Parameters:

  - `canvas`: the canvas measuring the labels.
  - `placements` (sequence): end-label placements (see [[end-label-placements]]).
  - `env` (map): the drawing environment (`:theme`).

  Returns a vector of the kept placements, each with its pixel `:box` (see [[cljplot2.content.label/box-at]])."
  [canvas placements env]
  (reduce (fn [kept {:keys [content anchor x y] :as p}]
            (let [box (label/box-at canvas content anchor x y env)]
              (if (some #(measure/boxes-overlap? box (:box %)) kept)
                kept
                (conj kept (assoc p :box box)))))
          []
          placements))

(defn draw-label-placements!
  "Draws end-label placements: leader lines first, then the labels.

  Parameters:

  - `canvas`: the canvas.
  - `placements` (sequence): end-label placements (see [[end-label-placements]]).
  - `env` (map): the drawing environment (`:theme`).

  Leader lines are 1 pixel wide, in the label's colour, else its role's over the plot background. Returns `nil`."
  [canvas placements env]
  (doseq [{:keys [leader content]} placements
          :when leader
          :let [[[^double x0 ^double y0] [^double x1 ^double y1]] leader]]
    (c2d/set-color canvas (or (:color content)
                              (draw/text-color (:theme env) (:role content) (draw/plot-backdrop (:theme env)))))
    (c2d/set-stroke-custom canvas {:size 1.0})
    (c2d/line canvas x0 y0 x1 y1))
  (doseq [{:keys [content anchor x y]} placements]
    (label/draw-at! canvas content anchor x y env)))

(defn draw-end-labels!
  "Draws end labels, each just past a value along the value axis, centred on its slot.

  Polar plots: past the value along the radius (anchored away from the centre) or along the angle (centred), see [[cljplot2.scale.coord]]. [[end-label-room]] (and [[outer-label-room]]) keep room for them past the data.

  Parameters:

  - `canvas`: the canvas.
  - `pl` (map): the chart's [[placement]].
  - `labels` (sequence): `{:level l :slot s :at v :side d :text t}` - past value `v` toward increasing values (`d` 1) or decreasing ones (`d` -1), or centred on `v` (`d` 0, e.g. inside a stacked bar; no room needed); optional `:role` (text role, default `:annotation`), `:color` (overrides the role's); `:outer?` - past the outer radius of a polar plot at its segment's middle angle, anchored outward, with `:span [v0 v1]` (its segment's values) and `:leader?` (a leader line).
  - `scales` (map): finalized position scales.
  - `env` (map): the drawing environment (`:theme`).

  A label overlapping one drawn before it (in `labels` order) is left out. Returns `nil`."
  [canvas pl labels scales env]
  (draw-label-placements! canvas (without-collisions canvas (end-label-placements pl labels scales) env) env))

(defn outer-label-room
  "Gives the room `:outer?` end labels need around a polar plot's circle.

  Parameters:

  - `labels` (sequence): end labels (see [[draw-end-labels!]]); only `:outer?` ones count.
  - `env` (map): `:theme`, `:hint` (measuring).

  Returns `[horizontal vertical]` pixels: the widest and the tallest label, measured in its role, plus the gap ([[leader-label-gap]] when some label has a leader, else [[outer-label-gap]]); `nil` without `:outer?` labels."
  [labels {:keys [theme hint]}]
  (let [outer (filter :outer? labels)]
    (when-let [contents (seq (distinct (map #(label-content (dissoc % :color)) outer)))]
      (measure/call-with-scratch-canvas
       hint
       (fn [c]
         (let [sizes (map #(measure/natural-size % {:canvas c :theme theme}) contents)
               gap (v/mx (map outer-gap outer))]
           [(m/+ gap (v/mx (map (fn [^Vec2 s] (.x s)) sizes)))
            (m/+ gap (v/mx (map (fn [^Vec2 s] (.y s)) sizes)))]))))))

(defn n-label-room
  "Gives the `:padding-px` a strip chart's n labels need.

  Parameters:

  - `chart-options` (map): chart options - `:n-label` (default false), `:n-format`.
  - `items` (map): `{panel [item ...]}` (see [[split]]).
  - `env` (map): `:theme`, `:hint`, `:flip?` (see [[end-label-room]]).

  Returns room past the largest values for the count labels, past the smallest for `:mirrored?` items; `nil` without n labels or items."
  [chart-options items env]
  (when (data/option chart-options :n-label false)
    (end-label-room (map (fn [item] {:text (n-label-text chart-options item) :side (n-label-side item)}) (mapcat val items))
                    env)))

(defn draw-n-labels!
  "Draws each item's count just past its largest drawn value, when the chart has n labels.

  [[n-label-room]] keeps room for them past the data.

  Parameters:

  - `canvas`: the canvas.
  - `pl` (map): the chart's [[placement]].
  - `items` (sequence): one panel's items.
  - `top-value` (function): item -> its largest drawn value (the smallest for a `:mirrored?` item, which is labelled toward decreasing values).
  - `chart-options` (map): chart options - `:n-label` (default false), `:n-format` (default `n=%d`).
  - `scales` (map): finalized position scales.
  - `env` (map): the drawing environment.

  Returns `nil`. See also [[draw-end-labels!]]."
  [canvas pl items top-value chart-options scales env]
  (when (data/option chart-options :n-label false)
    (draw-end-labels! canvas pl
                      (map (fn [{:keys [level slot] :as item}]
                             {:level level :slot slot :at (top-value item) :side (n-label-side item)
                              :text (n-label-text chart-options item)})
                           items)
                      scales env)))

(defn ridges?
  "Tests whether a histogram or density draws ridgelines.

  A numeric `:y` otherwise is ignored, as before ridgelines (a plot mapping shared with xy charts).

  Parameters:

  - `chart-split` (map): `{:groups {panel rows} :mapping m}`.
  - `env` (map): `:plot-scales` - the plot's `:scales`.

  Returns truthy when `:y` is mapped on an explicit `:bands` plot scale, or with some categorical (string / keyword / interval) value; else falsy."
  [{:keys [groups mapping]} env]
  (when-let [acc (:y mapping)]
    (or (= :bands (get-in env [:plot-scales :y :kind]))
        (let [y-of (data/row-accessor :y acc)]
          (some #(categorical-level? (y-of %)) (mapcat val groups))))))

(defn ridge-split
  "Splits the rows of a ridgeline chart (histogram, density - see [[ridges?]]) into items placed in bands (see [[split]]).

  Parameters:

  - `chart-split` (map): `{:groups {panel rows} :mapping m}`.
  - `chart-options` (map): chart options - `:position`: `:overlay` (default) or `:mirror` (a double ridgeline: the first group grows toward higher `:y`, the second away from it).
  - `env` (map): as in [[split]].

  Returns the result of [[split]].

  Throws `ex-info {:key :position}` for other positions."
  [chart-split chart-options env]
  (let [position (get chart-options :position :overlay)]
    (when-not (#{:overlay :mirror} position)
      (schema/fail! "Ridgelines (a categorical or bands :y) take :position :overlay or :mirror."
                    {:key :position :value position}))
    (split chart-split {:position position ::ridge true} :band env)))

(defn slot-side
  "Gives the side of the cross centre a slot `[lo hi]` lies on - the direction one-sided marks (ridges, mirrored swarms) grow in.

  Returns `1.0` toward higher `:y` (also for a slot around the centre), `-1.0` away (the second group of a mirror)."
  ^double [[^double lo ^double hi]]
  (if (m/neg? (m/+ lo hi)) -1.0 1.0))

(defn ridge-scale-defaults
  "Gives the `:y` scale attributes leaving room for the ridges of the outer levels.

  The tallest profile reaches `overlap` band steps (plus 5%) from its band centre. wadogo's `:align-outer` is in pixel order: unflipped the `:y` range runs from the bottom up (higher levels at lower pixels), flipped from the left.

  Parameters:

  - `overlap` (double): the tallest ridge's height in band steps.
  - `mirror?` (boolean): double ridgelines - room on both sides.
  - `flipped?` (boolean): is the plot flipped?
  - `y-spec` (map): the plot's own `:y` scale spec - its `:padding-in` (default 0) counts.

  Returns `{:y {:padding-out p :align-outer a}}`: the outer padding all after the last band, or split on both sides for `mirror?`."
  [^double overlap mirror? flipped? y-spec]
  (let [padding-in (double (or (:padding-in y-spec) 0.0))
        need (m/max 0.0 (m/- (m/* 1.05 overlap) (m/* 0.5 (m/- 1.0 padding-in))))]
    {:y (if mirror?
          {:padding-out need :align-outer 0.5}
          {:padding-out (m/* 0.5 need) :align-outer (if flipped? 0.0 1.0)})}))

(defn ridge-point
  "Gives the pixel function of ridge profiles.

  Parameters:

  - `pl` (map): a `:band` [[placement]] - `:point`, `:span`, `:step`.
  - `overlap` (double): the tallest ridge's height in band steps.

  Returns `(fn [value level side height] -> Vec2)`: `height` - a fraction of the tallest profile, drawn up to `overlap` band steps from the level's centre in direction `side` (see [[slot-side]]); `##NaN` across for a level without span."
  [{:keys [point span ^double step]} ^double overlap]
  (fn ^Vec2 [value level ^double side ^double height]
    (let [s (double (span level))]
      (point value level (if (m/pos? s) (m// (m/* side height overlap step) s) ##NaN)))))

(defn ridge-order
  "Orders ridge items for drawing: levels farthest toward higher `:y` first, so nearer ridges cover them.

  Parameters:

  - `pl` (map): the chart's [[placement]] - `:centre`, `:sign`.
  - `items` (sequence): one panel's items.

  Returns the items sorted."
  [{:keys [centre ^double sign]} items]
  (sort-by #(m/- (m/* sign (double (centre (:level %))))) items))

;; orientation

(defn values-mapping
  "Builds the mapping schema of a strip chart reading values.

  Parameters:

  - `entries` (sequence): the map entries documenting the keys the chart reads.

  Returns a map schema requiring `:x` or `:y` (the values sit on the axis other than the levels, see [[level-axis]])."
  [entries]
  (schema/with-rules (into [:map] entries)
    [#(when-not (or (some? (:x %)) (some? (:y %))) "should map :x or :y (the values)") nil]))

(defn- categorical-axis?
  "Tests whether a mapped dimension holds strip levels.

  Parameters:

  - `k` (keyword): `:x` or `:y`.
  - `mapping` (map): the chart's merged mapping.
  - `rows` (sequence): the chart's rows (all panels).
  - `plot-scales` (map): the plot's `:scales`.

  Returns truthy when `k` is mapped on a `:bands` plot scale (`:type :categorical` and categorical mapping markers resolve to it) or some value is a string, keyword or interval; else falsy."
  [k mapping rows plot-scales]
  (when-let [acc (get mapping k)]
    (or (= :bands (get-in plot-scales [k :kind]))
        (let [value-of (data/row-accessor k acc)]
          (some #(categorical-level? (value-of %)) rows)))))

(defn- numeric-axis?
  "Tests whether a mapped dimension has some finite number among its values.

  Parameters:

  - `k` (keyword): `:x` or `:y`.
  - `mapping` (map): the chart's merged mapping.
  - `rows` (sequence): the chart's rows.

  Returns truthy or falsy."
  [k mapping rows]
  (when-let [acc (get mapping k)]
    (let [value-of (data/row-accessor k acc)]
      (some #(let [v (value-of %)] (and (number? v) (m/valid-double? (double v)))) rows))))

(defn level-axis
  "Gives the mapping axis holding a strip chart's levels.

  Parameters:

  - `chart-options` (map): chart options - `:level-axis` always wins.
  - `mapping` (map): the chart's merged mapping.
  - `rows` (sequence): the chart's rows (all panels).
  - `plot-scales` (map): the plot's `:scales`.

  Returns `:x` or `:y`: the categorical axis (see [[categorical-axis?]]); with neither categorical, `:x` when only `:y` is mapped (one vertical level), else `:y` (one level, a count, or an axis without values - the strip frame reports it).

  Throws `ex-info {:key :level-axis}` when both axes are categorical, or both have numbers."
  [chart-options mapping rows plot-scales]
  (or (:level-axis chart-options)
      (let [x-categorical? (categorical-axis? :x mapping rows plot-scales)
            y-categorical? (categorical-axis? :y mapping rows plot-scales)]
        (cond
          (and x-categorical? y-categorical?)
          (schema/fail! (str "Both :x and :y are categorical: set :level-axis (:x or :y - the axis holding the strip "
                             "levels), or check that the values are numbers.")
                        {:key :level-axis :value [:x :y]})
          x-categorical? :x
          y-categorical? :y
          (and (numeric-axis? :x mapping rows) (numeric-axis? :y mapping rows))
          (schema/fail! (str "Strip levels must be categorical (strings / keywords / intervals) on :x or :y, or numbers or dates "
                             "declared categorical (:scales {:y {:type :categorical}} or :mapping {:y (levels/categorical k)}); "
                             "or set :level-axis.")
                        {:key :level-axis :value [:x :y]})
          (and (some? (:y mapping)) (nil? (:x mapping))) :x
          :else :y))))

(defn- swap-xy
  "Exchanges the entries of `:x` and `:y` of a map `m` (an absent one stays absent); `nil` for `nil`."
  [m]
  (when m
    (cond-> (dissoc m :x :y)
      (contains? m :y) (assoc :x (get m :y))
      (contains? m :x) (assoc :y (get m :x)))))

(def ^:private stats-dimension-maps
  "Stats keys of a strip chart whose maps are keyed by dimension (exchanged by [[transposed-stats]])."
  [:padding :padding-explicit :padding-px :axis-titles :tick-format :scale-defaults :dropped])

(defn- transposed-stats
  "Transposes the stats of a chart computed in the strip frame (levels on `:y`) for levels on `:x`.

  Parameters:

  - `stats` (map): the chart's stats.

  Returns the stats with `:x` and `:y` exchanged in the dimension-keyed maps ([[stats-dimension-maps]]), every panel's extents and the `:na` dimensions."
  [stats]
  (cond-> (reduce (fn [s k] (if (contains? s k) (update s k swap-xy) s)) stats stats-dimension-maps)
    (contains? stats :extents) (update :extents update-vals swap-xy)
    (contains? stats :na) (update :na #(set (map (fn [d] (get {:x :y :y :x} d d)) %)))))

(defn compute-oriented
  "Computes a strip chart in the strip frame, with its levels on the mapping axis of [[level-axis]].

  For levels on `:x` the chart computes the transposed input - mapping, the `:padding` option and the plot's scales with `:x` and `:y` exchanged, `:flip?` inverted - and its stats are transposed back (see [[transposed-stats]]).

  Parameters:

  - `compute` (function): the chart's compute in the strip frame (levels on `:y`, values on `:x`), of `chart`, `chart-split`, `env`.
  - `chart` (map): the chart value.
  - `chart-split` (map): `{:groups {panel rows} :mapping m}`.
  - `env` (map): the compute environment (`:plot-scales`, `:flip?`, ...).

  Returns the stats with `:level-axis`, every panel's data with `::level-axis` (for [[draw-oriented]])."
  [compute chart {:keys [groups mapping] :as chart-split} env]
  (let [axis (level-axis (:options chart) mapping (mapcat val groups) (:plot-scales env))
        stats (if (= :y axis)
                (compute chart chart-split env)
                (transposed-stats
                 (compute (cond-> chart (get-in chart [:options :padding]) (update-in [:options :padding] swap-xy))
                          (assoc chart-split :mapping (swap-xy mapping))
                          (-> env (update :flip? not) (update :plot-scales swap-xy)))))]
    (-> stats
        (update :data update-vals #(assoc % ::level-axis axis))
        (assoc :level-axis axis))))

(defn draw-oriented
  "Draws one panel of a strip chart whose draw works in the strip frame.

  The position scales are exchanged when the levels are on `:x` (see [[compute-oriented]]); placement and projections take their directions from the scales.

  Parameters:

  - `draw` (function): the chart's draw in the strip frame, of `chart`, `computed`, `canvas`, `box`, `scales`, `env`.
  - `chart` (map): the chart value.
  - `computed` (map): the panel's computed data (with `::level-axis`).
  - `canvas`, `box`, `scales`, `env`: as the chart contract's draw.

  Returns what `draw` returns."
  [draw chart computed canvas box scales env]
  (draw chart computed canvas box (if (= :x (::level-axis computed)) (swap-xy scales) scales) env))
