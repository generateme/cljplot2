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
  `\"n=%d\"`), just past its largest drawn value (see [[draw-n-labels!]]); the chart asks for value-axis room for
  them (`:padding-px`, see [[n-label-room]])."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.content.label :as label]
            [cljplot2.data.group :as group]
            [cljplot2.data.position :as position]
            [cljplot2.data.rows :as data]
            [cljplot2.data.shingle :as shingle]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.scale.coord :as coord]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private named-places #{:band :center :edge :bottom})
(def ^:private positions #{:dodge :overlay :mirror})

(def options
  "Map entries of the options every strip chart takes (besides [[cljplot2.data.rows/common-options]])."
  (schema/in-fragment
   :strip
   [(schema/optional :place {:description "where the items sit across the value axis: :band (one band per level), {:at v} (centred on v of a numeric scale), :center, :edge or :bottom (between the panel edge and 0)"
                             :cljplot2/default-doc ":band with a level mapping, else the chart's own (:center for most, :edge for the rug)"}
                     [:multi {:dispatch map?}
                      [true [:map {:closed true} [:at schema/finite]]]
                      [false (into [:enum] (sort named-places))]])
    (schema/optional :width {:description "fraction of the level's band (or of the panel for :center and :bottom) an item spans across the value axis"
                             :default 0.8}
                     schema/non-neg)
    (schema/optional :thickness {:description "item thickness in pixels for :place {:at v} and :edge"
                                 :cljplot2/default-doc "20 pixels for {:at v}, 8 for :edge"}
                     schema/non-neg)
    (schema/optional :position {:description "how groups of one level share its band: :dodge (side by side), :overlay (on top of each other) or :mirror (two groups, one half each)"
                                :cljplot2/default-doc ":dodge when a level holds two or more groups, else :overlay (dotplot: always :overlay)"}
                     (into [:enum] (sort positions)))
    (schema/optional :dodge-gap {:description "gap between dodged groups, a fraction of a group's part of the band"
                                 :default 0.1}
                     schema/fraction)
    (schema/optional :bottom-share {:description "padding below the data asked for with :place :bottom (only there), a fraction of the y data span"
                                    :cljplot2/default-doc "0.15 with :place :bottom"}
                     schema/fraction)
    (schema/nilable :n-label {:description "draw each item's row count just past its largest value"
                              :default false}
                    schema/bool)
    (schema/optional :n-format {:description "format string of the n labels"
                                :default "n=%d"}
                     :string)
    (schema/optional :level-axis {:description "the mapping axis holding the levels (decides when both or neither of :x and :y are categorical)"
                                  :cljplot2/default-doc "the categorical axis of the mapping; with neither, :x when only :y is mapped, else :y"}
                     [:enum :x :y])]))

(def mirror-option
  "Map entry of the `:mirror` option of strip charts drawing a negative side (bar, extent)."
  (first (schema/in-fragment
          :mirror
          [(schema/optional :mirror {:description "a grouping dimension with exactly two levels; the second level's items are drawn on the negative side"}
                            (into [:enum] (conj aesthetic/dims :group)))])))

(def ridge-options
  "Map entries of the ridgeline options (histogram and density with a categorical `:y`)."
  (schema/in-fragment
   :ridge
   [(schema/optional :overlap {:description "height of the tallest ridge in band steps (above 1 - ridges overlap the next level)"
                               :default 0.9}
                     schema/non-neg)]))

(defn- place-of
  "The placement of a chart: explicit `:place` or the default (`:band` with a `:y` mapping, else `default-place`).
  Throws `ex-info {:key :place}` for `:band` without a `:y` mapping or another placement with one, `ex-info
  {:key :bottom-share}` for `:bottom-share` without `:bottom`."
  [options mapping default-place]
  (let [y? (some? (:y mapping))
        place (get options :place (if y? :band default-place))]
    (cond
      (and (some? (:bottom-share options)) (not= :bottom place))
      (schema/fail! "Strip :bottom-share needs :place :bottom." {:key :bottom-share :value (:bottom-share options)})
      (and (= :band place) (not y?)) (schema/fail! "Strip :place :band needs a :y mapping." {:key :place :value place})
      (and y? (not= :band place)) (schema/fail! "A strip with a :y mapping is placed in bands (:place :band)."
                                                 {:key :place :value place})
      :else place)))

(defn- categorical-level?
  "Is `v` a categorical `:y` level: a string, keyword or interval (shingle / bin)?"
  [v]
  (or (string? v) (keyword? v) (shingle/interval? v)))

(defn- check-levels!
  "Throw `ex-info {:key :place}` unless the `:y` `levels` can be bands: strings / keywords / intervals, or numbers
  with an explicit `:bands` plot scale."
  [levels plot-scales]
  (let [bands? (= :bands (get-in plot-scales [:y :kind]))]
    (when-not (every? #(or (categorical-level? %) (and bands? (number? %))) levels)
      (schema/fail! "Strip levels (the level axis, see :level-axis) must be categorical (strings / keywords / intervals), or numbers declared categorical (:scales {:y {:type :categorical}} or :mapping {:y (levels/categorical k)})."
                    {:key :place :value (first (remove categorical-level? levels))}))))

(defn- slots
  "Cross sub-range per group key for `position` over the ordered `group-keys`; dodged sub-ranges lose `gap` (a
  fraction of each) split between their ends. `first-high?` - the first group takes the high end of the cross range
  (toward higher `:y`), the following ones step down; else the low end first."
  [position group-keys gap first-high?]
  (let [n (count group-keys)]
    (case position
      :overlay (zipmap group-keys (repeat [-0.5 0.5]))
      :dodge (zipmap group-keys (map (fn [^long i]
                                       (let [[a b] (position/dodge-interval [-0.5 0.5] (if first-high? (- n 1 i) i) n)
                                             inset (* 0.5 (double gap) (- (double b) (double a)))]
                                         [(+ (double a) inset) (- (double b) inset)]))
                                     (range n)))
      :mirror (if (= 2 n)
                (zipmap group-keys (if first-high? [[0.0 0.5] [-0.5 0.0]] [[-0.5 0.0] [0.0 0.5]]))
                (schema/fail! "Strip :position :mirror needs exactly two groups." {:key :position :groups n})))))

(defn- mirror-index
  "Index of the `::mirror` dimension in the grouping `dims`; `nil` without it. Throws `ex-info {:key :mirror}`
  when it isn't a grouping dimension or comes with `:position :mirror`."
  [options dims]
  (when-let [k (::mirror options)]
    (let [i (.indexOf ^java.util.List dims k)]
      (when (neg? i)
        (schema/fail! "Strip :mirror should be a mapped grouping dimension (an ordinal aesthetic or :group)."
                      {:key :mirror :value k}))
      (when (= :mirror (:position options))
        (schema/fail! "Strip :mirror doesn't combine with :position :mirror (half slots)." {:key :mirror :value k}))
      i)))

(defn- without-index [key ^long i] (into (subvec key 0 i) (subvec key (inc i))))

(defn- mirrored-level
  "The level of the mirror dimension `k` (index `i`) drawn on the negative side - the second of exactly two over the
  ordered `group-keys`. Throws `ex-info {:key :mirror}` for another count of levels."
  [group-keys ^long i k]
  (let [levels (vec (distinct (map #(nth % i) group-keys)))]
    (when-not (= 2 (count levels))
      (schema/fail! "Strip :mirror needs exactly two levels of its dimension." {:key :mirror :value k :levels levels}))
    (levels 1)))

(defn- shared-levels?
  "Does some level of a panel (`items` - `{panel [item ...]}`, `:level` `nil` without bands - the whole panel) hold
  items of two or more slot keys (`slot-key` of their group key)?"
  [items slot-key]
  (boolean (some (fn [its] (some (fn [[_ level-items]] (> (count (distinct (map (comp slot-key :key) level-items))) 1))
                                 (group-by :level its)))
                 (vals items))))

(defn- levels-in-order
  "`[[level rows] ...]` of the `rows` with a valid `y-of` value, levels ordered by `level-rank` (level -> its first
  appearance in the chart's rows; `group-by` alone keeps no order past 8 levels)."
  [y-of level-rank rows]
  (sort-by (comp level-rank first) (group-by y-of (filter (comp data/valid-value? y-of) rows))))

(defn split
  "Items of a strip chart: the rows of every panel (`chart-split` - `{:groups {panel rows} :mapping m}`) split into
  groups (ordinal aesthetics, `:group`) and `:y` levels, `:values` - the finite `:x` numbers.
  Returns `{:place p :position pos :items {panel [item ...]} :group-keys [..] :dims [..] :panels split-panels
  :dropped {dim n}}` (`:dims` - the grouping dimensions, the order of a group key's values); items carry `:level`
  (`nil` without bands), `:key`, `:aesthetics`, `:values`, `:n` (the count of values), `:slot`. `default-place` - the
  chart's placement without `:y`. Internal options of charts built on strips:

  * `::count-rows true` - `:x` isn't read: every item has `:values []` and `:n` - its count of rows (an item per
    level with rows; no `:x` errors or drops)
  * `::rows true` - items also carry their `:rows`
  * `::mirror k` - the chart's `:mirror` dimension (see the namespace doc): items carry `:mirrored?` and the result
    `:mirror? true`; charts without a negative side never pass it

  `:slot-keys` - the keys slots are given to (group keys, without the mirror dimension); `:shared-levels?` - some level
  holds two or more of them (the default `:position` is then `:dodge`, else `:overlay`).
  Throws `ex-info {:key :mapping}` when there are rows but no finite `:x` value (or, placed in bands, no valid `:y`
  value), `ex-info {:key :n-format}` for an `:n-format` that can't format a count, `ex-info {:key :mirror}` for a
  mirror dimension that isn't a grouping dimension, doesn't have exactly two levels or comes with `:position
  :mirror`."
  [{:keys [groups mapping]} options default-place env]
  (let [_ (when-let [fmt (:n-format options)]
            (try (format fmt 1)
                 (catch Exception e
                   (schema/fail! "Strip :n-format should format a count (e.g. \"n=%d\")."
                                 {:key :n-format :value fmt :cause (ex-message e)}))))
        place (place-of options mapping default-place)
        {:keys [panels dims dropped]} (group/split groups (dissoc mapping :y) env)
        count-rows? (boolean (::count-rows options))
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
        mi (mirror-index options dims)
        slot-key (if mi #(without-index % mi) identity)
        slot-keys (vec (distinct (map slot-key group-keys)))
        mirrored (when (and mi (seq group-keys)) (mirrored-level group-keys mi (::mirror options)))
        items (update-vals panels
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
                                      (::rows options) (assoc :rows rows))))))
        shared? (shared-levels? items slot-key)
        position (get options :position (if shared? :dodge :overlay))
        ;; reading order: the first group on top of horizontal strips (the high cross end), on the left of vertical
        ;; (flipped) ones (the low end); ridgelines grow the first group toward the next level (the high end)
        first-high? (or (::ridge options) (not (:flip? env)))
        slot-of (if (seq slot-keys) (slots position slot-keys (double (get options :dodge-gap 0.1)) first-high?) {})
        items (update-vals items (fn [its] (mapv #(assoc % :slot (slot-of (slot-key (:key %)))) its)))
        x-dropped (if x-of
                    (count (remove #(let [v (x-of %)] (and (number? v) (m/valid-double? (double v)))) all-rows))
                    0)]
    (when (and x-of (seq all-rows) (= x-dropped (count all-rows)))
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
  "`:y` extent of a panel's `items` for `place`: the levels (bands), `v` of `{:at v}`, 0 for `:bottom` (a companion
  extent), else `nil`."
  [place items]
  (cond
    (= :band place) (let [levels (distinct (map :level items))
                          e (data/extent :y levels)]
                      ;; numeric levels (on an explicit :bands scale) travel as the extent's :values
                      (cond-> e (= :numeric (:kind e)) (assoc :values (vec levels))))
    (map? place) {:kind :numeric :min (double (:at place)) :max (double (:at place))}
    (= :bottom place) {:kind :numeric :min 0.0 :max 0.0 :companion? true}
    :else nil))

(defn padding
  "Position padding of a strip chart for `place`: x `[0.05 0.05]`, `:bottom` - y `[:bottom-share 0]` (default
  0.15), overridden by the `:padding` option."
  [place options]
  (merge {:x [0.05 0.05]}
         (when (= :bottom place) {:y [(double (get options :bottom-share 0.15)) 0.0]})
         (:padding options)))

(defn explicit-padding
  "`:padding-explicit` of a strip chart for `place` (see [[cljplot2.data.rows/explicit-padding]]): `:bottom` - the
  room below the data (y `[:bottom-share nil]`), and the `:padding` option."
  [place options]
  (data/explicit-padding (when (= :bottom place) {:y [(double (get options :bottom-share 0.15)) nil]}) options))

(defn extents
  "Per panel extents of `items` (see [[split]]): `:x` from `x-range` (item -> `[lo hi]` in data units, `nil` -
  none), `:y` by `place` ([[y-extent]]); panels without items are left out."
  [place items x-range]
  (into {} (keep (fn [[panel its]]
                   (let [ranges (keep x-range its)]
                     (when (seq ranges)
                       [panel (cond-> {:x {:kind :numeric
                                           :min (double (reduce min (map first ranges)))
                                           :max (double (reduce max (map second ranges)))}}
                                (y-extent place its) (assoc :y (y-extent place its)))]))))
        items))

(defn in-slot
  "Cross coordinate of the two-sided `cross` (`[-0.5 0.5]`) inside the item's `slot` `[lo hi]`."
  ^double [[lo hi] ^double cross]
  (let [lo (double lo)]
    (+ lo (* (+ cross 0.5) (- (double hi) lo)))))

(defn slot-half
  "Half the length of `slot` in cross units."
  ^double [[lo hi]]
  (* 0.5 (- (double hi) (double lo))))

(defn placement
  "Pixel geometry of a strip chart's placement in a panel (`box` - `{:w :h}`, finalized `scales`, chart `options`,
  `place`): `{:point (fn [value level cross] -> Vec2) :span (fn [level] -> px)}`. `:point` places `value` along the
  `:x` scale and the cross coordinate (placement range `[-0.5 0.5]`, positive toward higher `:y`) across it;
  `:span` - the pixel length of the whole cross range; `:value-pixel` - `(fn [value] -> px)` along the value axis;
  `:centre` - `(fn [level] -> px)` of the cross range's centre; `:sign` - the pixel direction of increasing `:y`;
  `:step` - the band step in pixels (`:band`, else `nil`); `:range-point` - `(fn [value level cross] -> [h v])`, the
  point in range space (pixels in cartesian plots, angle and radius in polar ones, see [[cljplot2.scale.coord]]),
  `:geometry` - the polar geometry or `nil`. In a polar plot range lengths and `:step` are in range units (radians
  across angles). A level or value the scales can't place gives `##NaN` coordinates. Flip-aware: the cross axis is
  the `:y` direction (horizontal when flipped). `:bottom` without a `:y`
  scale is placed as `:center`.
  Throws `ex-info {:key :place}` for `:band` without a `:bands` `:y` scale, `{:at v}` and `:bottom` on a `:bands` one."
  [{:keys [w h]} scales options place]
  (let [_ (when (and (= :bottom place) (= :bands (:kind (:y scales))))
            (schema/fail! "Strip :place :bottom needs a numeric :y scale, not bands." {:key :place :value place}))
        place (if (and (= :bottom place) (nil? (:y scales))) :center place)
        fx (scale/forward-fn (:x scales))
        y-scale (:y scales)
        fy (when y-scale (scale/forward-fn y-scale))
        cross-horizontal? (scale/flipped? scales)
        polar (scale/coord-geometry scales)
        ;; polar: the cross range is the y scale's range, else the angular / radial extent of the panel's circle
        [e0 e1] (when polar
                  (mapv double (or (:range y-scale)
                                   (if cross-horizontal? (:theta polar) [(:inner-radius polar) (:radius polar)]))))
        length (double (cond polar (m/abs (- (double e1) (double e0))) cross-horizontal? w :else h))
        ;; range direction of increasing :y
        sign (double (cond
                       y-scale (let [[a b] (:range y-scale)] (if (< (double b) (double a)) -1.0 1.0))
                       polar (if (< (double e1) (double e0)) -1.0 1.0)
                       :else (if cross-horizontal? 1.0 -1.0)))
        ;; range position of the cross range's low-value end and of its middle
        low (double (cond polar e0 (pos? sign) 0.0 :else length))
        middle (if polar (* 0.5 (+ (double e0) (double e1))) (* 0.5 length))
        width (double (get options :width 0.8))
        thickness (let [t (double (get options :thickness (if (= :edge place) 8.0 20.0)))]
                    ;; pixels; across angles at the outer radius
                    (if (and polar cross-horizontal?) (/ t (double (:radius polar))) t))
        geometry (when (= :band place)
                   (or (scale/band-geometry y-scale)
                       (schema/fail! "Strip :place :band needs a :bands :y scale." {:key :place :value (:kind y-scale)})))
        bandwidth (some-> geometry :bandwidth double)
        _ (when (and (map? place) (= :bands (:kind y-scale)))
            (schema/fail! "Strip :place {:at v} needs a numeric :y scale, not bands." {:key :place :value place}))
        centre-and-span (cond
                          (= :band place) (fn [level] [(if fy (fy level) ##NaN) (* width (double bandwidth))])
                          (map? place) (let [c (if fy (fy (:at place)) ##NaN)] (constantly [c thickness]))
                          (= :center place) (constantly [middle (* width length)])
                          (= :edge place) (constantly [(+ low (* sign 0.5 thickness)) thickness])
                          (= :bottom place) (let [zero (double (fy 0.0))]
                                              (constantly [(* 0.5 (+ low zero)) (* width (m/abs (- zero low)))])))
        range-point (fn [value level ^double cross]
                      (let [[c span] (centre-and-span level)
                            p (+ (double c) (* sign cross (double span)))
                            v (double (fx value))]
                        (if cross-horizontal? [p v] [v p])))]
    {:point (if polar
              (fn ^Vec2 [value level ^double cross]
                (let [[h v] (range-point value level cross)] (coord/to-pixel polar (double h) (double v))))
              (fn ^Vec2 [value level ^double cross]
                (let [[h v] (range-point value level cross)] (Vec2. (double h) (double v)))))
     :range-point range-point
     :geometry polar
     :span (fn ^double [level] (double (second (centre-and-span level))))
     :value-pixel (fn ^double [value] (double (fx value)))
     :centre (fn ^double [level] (double (first (centre-and-span level))))
     :sign sign
     :step (some-> geometry :step double)}))

(def ^:private ^:const n-label-gap
  "Pixels between an item's largest drawn value and its n label."
  3.0)

(defn- n-label-side
  "Direction of an item's n label along the value axis: `-1.0` for a `:mirrored?` item, else `1.0`."
  ^double [item]
  (if (:mirrored? item) -1.0 1.0))

(defn- n-label-text [options item] (format (get options :n-format "n=%d") (:n item)))

(defn end-label-room
  "`:padding-px` `{:x [lo hi]}` for end labels `labels` (`[{:text t :side s} ...]`, see [[draw-end-labels!]]): per
  side of the value axis the largest label's size along it (its height when the plot is flipped, env `:flip?`) plus
  the gap, measured with the `:annotation` role (env `:theme`, `:hint`); 0 on a side without labels; `nil` without
  labels."
  [labels {:keys [theme flip? hint]}]
  (when (seq labels)
    (measure/call-with-scratch-canvas
     hint
     (fn [c]
       (let [room (fn [^double side]
                    (let [texts (distinct (keep #(when (== side (double (:side %))) (:text %)) labels))]
                      (if (seq texts)
                        (+ (double (reduce max (map (fn [t] (let [^Vec2 size (measure/natural-size {:kind :label :role :annotation :text t}
                                                                                                     {:canvas c :theme theme})]
                                                              (if flip? (.y size) (.x size))))
                                                    texts)))
                           n-label-gap)
                        0.0)))]
         {:x [(room -1.0) (room 1.0)]})))))

(def ^:const outer-label-gap
  "Pixels between a polar plot's outer radius and an `:outer?` end label."
  6.0)

(def ^:const leader-label-gap
  "Pixels between a polar plot's outer radius and an `:outer?` end label with a leader line (`:leader?`)."
  16.0)

(defn- outer-gap
  ^double [{:keys [leader?]}]
  (if leader? leader-label-gap outer-label-gap))

(declare polar-end-label-placements cartesian-end-label-placements)

(defn end-label-placements
  "Where [[draw-end-labels!]] draws `labels` (see there): `[{:label l :content c :anchor [h v] :x px :y py :leader
  [[x0 y0] [x1 y1]]} ...]` - `:content` the label content, `:leader` (an `:outer?` label with `:leader?`) a line from
  its segment's outer edge; labels without a placeable position left out."
  [{:keys [geometry] :as pl} labels scales]
  (if geometry
    (polar-end-label-placements pl labels scales)
    (cartesian-end-label-placements pl labels scales)))

(defn- label-content
  [{:keys [text color]}]
  (cond-> {:kind :label :role :annotation :text text} color (assoc :color color)))

(defn- cartesian-end-label-placements
  [{:keys [point]} labels scales]
  (let [x-scale (:x scales)
        [r0 r1] (:range x-scale)
        ;; pixel direction of increasing values
        up (if (< (double r1) (double r0)) -1.0 1.0)
        horizontal? (scale/horizontal? :x x-scale)
        pad n-label-gap]
    (vec (for [{:keys [level slot at side] :as l} labels
               :let [dir (* up (double side))
                     anchor (cond
                              (zero? dir) [:center :middle]
                              horizontal? (if (pos? dir) [:left :middle] [:right :middle])
                              :else (if (pos? dir) [:center :top] [:center :bottom]))
                     ^Vec2 p (point at level (in-slot slot 0.0))
                     x (if horizontal? (+ (.x p) (* dir pad)) (.x p))
                     y (if horizontal? (.y p) (+ (.y p) (* dir pad)))]
               :when (and (m/valid-double? x) (m/valid-double? y))]
           {:label l :content (label-content l) :anchor anchor :x x :y y}))))

(defn- outer-placement
  "Placement of an `:outer?` label: past the outer radius at the angle of its segment's middle (value `at`, the slot's
  centre), anchored away from the centre; the leader from the segment's farthest corner (values `:span`)."
  [{:keys [range-point geometry]} {:keys [level slot at span leader?] :as l}]
  (let [[h _] (range-point at level (in-slot slot 0.0))
        h (double h)
        r (+ (double (:radius geometry)) (outer-gap l))
        ^Vec2 p (coord/to-pixel geometry h r)
        edge (when leader?
               (reduce max (for [value span cross [-0.5 0.5]] (double (second (range-point value level (in-slot slot cross)))))))]
    (when (and (m/valid-double? (.x p)) (m/valid-double? (.y p)))
      (cond-> {:label l :content (label-content l) :anchor (coord/outward-anchor h) :x (.x p) :y (.y p)}
        (and edge (m/valid-double? (double edge)))
        (assoc :leader (let [^Vec2 a (coord/to-pixel geometry h (double edge))
                             ^Vec2 b (coord/to-pixel geometry h (- r 1.0))]
                         [[(.x a) (.y a)] [(.x b) (.y b)]]))))))

(defn- polar-end-label-placements
  [{:keys [range-point geometry] :as pl} labels scales]
  (let [x-scale (:x scales)
        radial? (not (scale/horizontal? :x x-scale))
        [r0 r1] (:range x-scale)
        up (if (< (double r1) (double r0)) -1.0 1.0)
        ;; the gap in range units: pixels along the radius, radians at the outer radius along the angle
        pad (if radial? n-label-gap (/ n-label-gap (double (:radius geometry))))]
    (vec (keep (fn [{:keys [level slot at side outer?] :as l}]
                 (if outer?
                   (outer-placement pl l)
                   (let [dir (* up (double side))
                         [h v] (range-point at level (in-slot slot 0.0))
                         [h v] (if radial? [h (+ (double v) (* dir pad))] [(+ (double h) (* dir pad)) v])
                         ^Vec2 p (coord/to-pixel geometry (double h) (double v))
                         anchor (if (and radial? (not (zero? dir))) (coord/outward-anchor (double h)) [:center :middle])]
                     (when (and (m/valid-double? (.x p)) (m/valid-double? (.y p)))
                       {:label l :content (label-content l) :anchor anchor :x (.x p) :y (.y p)}))))
               labels))))

(defn- boxes-overlap?
  [a b]
  (and (< (double (:x a)) (+ (double (:x b)) (double (:w b)))) (< (double (:x b)) (+ (double (:x a)) (double (:w a))))
       (< (double (:y a)) (+ (double (:y b)) (double (:h b)))) (< (double (:y b)) (+ (double (:y a)) (double (:h a))))))

(defn without-collisions
  "End-label `placements` (see [[end-label-placements]]) with their pixel `:box` (see
  [[cljplot2.content.label/box-at]]), each overlapping one kept before it left out - earlier placements win."
  [canvas placements env]
  (reduce (fn [kept {:keys [content anchor x y] :as p}]
            (let [box (label/box-at canvas content anchor x y env)]
              (if (some #(boxes-overlap? box (:box %)) kept)
                kept
                (conj kept (assoc p :box box)))))
          []
          placements))

(defn draw-label-placements!
  "Draw end-label `placements` (see [[end-label-placements]]): leader lines (1 pixel, the label's color or the
  `:annotation` role's), then the labels."
  [canvas placements env]
  (doseq [{:keys [leader content]} placements
          :when leader
          :let [[[x0 y0] [x1 y1]] leader]]
    (c2d/set-color canvas (or (:color content) (get-in env [:theme :text :annotation :color])))
    (c2d/set-stroke-custom canvas {:size 1.0})
    (c2d/line canvas (double x0) (double y0) (double x1) (double y1)))
  (doseq [{:keys [content anchor x y]} placements]
    (label/draw-at! canvas content anchor x y env)))

(defn draw-end-labels!
  "Draw end labels `labels` - `[{:level l :slot s :at v :side d :text t} ...]` - each just past value `v` along the
  value axis, toward increasing values (`d` 1) or decreasing ones (`d` -1), or centred on `v` (`d` 0, e.g. inside a
  stacked bar; no room needed), centred on its slot (text role `:annotation`; optional `:color` overrides the role's).
  `pl` - the chart's [[placement]]; [[end-label-room]] keeps room for them past the data. Polar plots: past the value
  along the radius (anchored away from the centre) or along the angle (centred), see [[cljplot2.scale.coord]]; an
  `:outer?` label (`:span [v0 v1]` - its segment's values, `:leader?` - a leader line) past the outer radius at its
  segment's middle angle, anchored outward (room: [[outer-label-room]]). A label overlapping one drawn before it
  (in `labels` order) is left out."
  [canvas pl labels scales env]
  (draw-label-placements! canvas (without-collisions canvas (end-label-placements pl labels scales) env) env))

(defn outer-label-room
  "`[horizontal vertical]` pixels `:outer?` end labels `labels` need around a polar plot's circle (the widest and
  the tallest label plus the gap - [[leader-label-gap]] when some label has a leader, else [[outer-label-gap]]),
  measured with the `:annotation` role (env `:theme`, `:hint`); `nil` without such labels."
  [labels {:keys [theme hint]}]
  (let [outer (filter :outer? labels)]
    (when-let [texts (seq (distinct (map :text outer)))]
      (measure/call-with-scratch-canvas
       hint
       (fn [c]
         (let [sizes (map #(measure/natural-size {:kind :label :role :annotation :text %} {:canvas c :theme theme}) texts)
               gap (double (reduce max (map outer-gap outer)))]
           [(+ gap (double (reduce max (map (fn [^Vec2 s] (.x s)) sizes))))
            (+ gap (double (reduce max (map (fn [^Vec2 s] (.y s)) sizes))))]))))))

(defn n-label-room
  "`:padding-px` of a strip chart (`items` - `{panel [item ...]}`) with `:n-label`: room past the largest values for
  the count labels, past the smallest for `:mirrored?` items (see [[end-label-room]]); `nil` without n labels or
  items."
  [options items env]
  (when (data/option options :n-label false)
    (end-label-room (map (fn [item] {:text (n-label-text options item) :side (n-label-side item)}) (mapcat val items))
                    env)))

(defn draw-n-labels!
  "With option `:n-label`, draw each item's count `:n` (`:n-format`, default `\"n=%d\"`) just past `(top-value item)` -
  its largest drawn value, the smallest for a `:mirrored?` item, which is labelled toward decreasing values (see
  [[draw-end-labels!]]); [[n-label-room]] keeps room for them past the data."
  [canvas pl items top-value options scales env]
  (when (data/option options :n-label false)
    (draw-end-labels! canvas pl
                      (map (fn [{:keys [level slot] :as item}]
                             {:level level :slot slot :at (top-value item) :side (n-label-side item)
                              :text (n-label-text options item)})
                           items)
                      scales env)))

(defn ridges?
  "Does a histogram / density of `chart-split` (`{:groups {panel rows} :mapping m}`) draw ridgelines: `:y` mapped
  with some categorical (string / keyword / interval) value, or on an explicit `:bands` plot scale? A numeric `:y` otherwise is
  ignored, as before ridgelines (a plot mapping shared with xy charts)."
  [{:keys [groups mapping]} env]
  (when-let [acc (:y mapping)]
    (or (= :bands (get-in env [:plot-scales :y :kind]))
        (let [y-of (data/row-accessor :y acc)]
          (boolean (some #(categorical-level? (y-of %)) (mapcat val groups)))))))

(defn ridge-split
  "Items of a ridgeline chart (histogram / density, see [[ridges?]] and [[split]]): placed in bands, groups
  `:overlay` (default) or `:mirror` (a double ridgeline: the first group grows toward higher `:y`, the second away
  from it). Throws `ex-info {:key :position}` for other positions."
  [chart-split options env]
  (let [position (get options :position :overlay)]
    (when-not (#{:overlay :mirror} position)
      (schema/fail! "Ridgelines (a categorical or bands :y) take :position :overlay or :mirror."
                    {:key :position :value position}))
    (split chart-split {:position position ::ridge true} :band env)))

(defn slot-side
  "Side of the cross centre `slot` lies on: `1.0` toward higher `:y` (also a slot around the centre), `-1.0` away
  (the second group of a mirror). One-sided marks (ridges, mirrored swarms) grow in this direction."
  ^double [[lo hi]]
  (if (neg? (+ (double lo) (double hi))) -1.0 1.0))

(defn ridge-scale-defaults
  "`:y` scale attributes (under the plot's own `y-spec`) leaving room for the ridges of the outer levels: the tallest
  profile is `overlap` band steps (plus 5%) from its band centre - outer padding all after the last band, or on both
  sides for `mirror?`. wadogo's `:align-outer` is in pixel order: unflipped the `:y` range runs from the bottom up
  (higher levels at lower pixels), `flipped?` from the left."
  [^double overlap mirror? flipped? y-spec]
  (let [padding-in (double (or (:padding-in y-spec) 0.0))
        need (m/max 0.0 (- (* 1.05 overlap) (* 0.5 (- 1.0 padding-in))))]
    {:y (if mirror?
          {:padding-out need :align-outer 0.5}
          {:padding-out (* 0.5 need) :align-outer (if flipped? 0.0 1.0)})}))

(defn ridge-point
  "`(fn [value level side height] -> Vec2)` of ridge profiles for a `:band` placement `pl` (see [[placement]]):
  `height` - a fraction of the tallest profile, drawn up to `overlap` band steps from the level's centre in direction
  `side` (see [[slot-side]])."
  [{:keys [point span step]} ^double overlap]
  (let [step (double step)]
    (fn ^Vec2 [value level ^double side ^double height]
      (let [s (double (span level))]
        (point value level (if (pos? s) (/ (* side height overlap step) s) ##NaN))))))

(defn ridge-order
  "`items` in drawing order for placement `pl`: levels farthest toward higher `:y` first, so nearer ridges cover them."
  [{:keys [centre sign]} items]
  (sort-by #(- (* (double sign) (double (centre (:level %))))) items))

;; orientation

(defn values-mapping
  "Mapping schema of strip charts reading values: `:x` or `:y` mapped (the values sit on the axis other than the
  levels, see [[level-axis]]); `entries` - the map entries documenting the keys the chart reads."
  [entries]
  (schema/with-rules (into [:map] entries)
    [#(when-not (or (some? (:x %)) (some? (:y %))) "should map :x or :y (the values)") nil]))

(defn- categorical-axis?
  "Does dimension `k` of `mapping` hold strip levels: a `:bands` plot scale in `plot-scales` (`:type :categorical`
  and categorical mapping markers resolve to it) or some value of `rows` a string, keyword or interval?"
  [k mapping rows plot-scales]
  (when-let [acc (get mapping k)]
    (or (= :bands (get-in plot-scales [k :kind]))
        (let [value-of (data/row-accessor k acc)]
          (boolean (some #(categorical-level? (value-of %)) rows))))))

(defn- numeric-axis?
  "Does dimension `k` of `mapping` have some finite number among the values of `rows`?"
  [k mapping rows]
  (when-let [acc (get mapping k)]
    (let [value-of (data/row-accessor k acc)]
      (boolean (some #(let [v (value-of %)] (and (number? v) (m/valid-double? (double v)))) rows)))))

(defn level-axis
  "The mapping axis holding a strip chart's levels: `:level-axis` of `options` (always wins), else from `mapping`
  over `rows` (all panels) and the plot's scales `plot-scales` - the categorical axis (see [[categorical-axis?]]);
  with neither categorical, `:x` when only `:y` is mapped (one vertical level), else `:y` (one level, a count, or an
  axis without values - the strip frame reports it). Throws `ex-info {:key :level-axis}` when both axes are
  categorical, or both have numbers."
  [options mapping rows plot-scales]
  (or (:level-axis options)
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
          (schema/fail! (str "Strip levels must be categorical (strings / keywords / intervals) on :x or :y, or numbers "
                             "declared categorical (:scales {:y {:type :categorical}} or :mapping {:y (levels/categorical k)}); "
                             "or set :level-axis.")
                        {:key :level-axis :value [:x :y]})
          (and (some? (:y mapping)) (nil? (:x mapping))) :x
          :else :y))))

(defn- swap-xy
  "`m` with the entries of `:x` and `:y` exchanged (an absent one stays absent); `nil` for `nil`."
  [m]
  (when m
    (cond-> (dissoc m :x :y)
      (contains? m :y) (assoc :x (get m :y))
      (contains? m :x) (assoc :y (get m :x)))))

(def ^:private stats-dimension-maps
  "Stats keys of a strip chart whose maps are keyed by dimension."
  [:padding :padding-explicit :padding-px :axis-titles :tick-format :scale-defaults :dropped])

(defn- transposed-stats
  "`stats` of a chart computed in the strip frame (levels on `:y`) with their dimensions exchanged for levels on
  `:x`: the dimension-keyed maps, every panel's extents and the `:na` dimensions."
  [stats]
  (cond-> (reduce (fn [s k] (if (contains? s k) (update s k swap-xy) s)) stats stats-dimension-maps)
    (contains? stats :extents) (update :extents update-vals swap-xy)
    (contains? stats :na) (update :na #(set (map (fn [d] (get {:x :y :y :x} d d)) %)))))

(defn compute-oriented
  "Stats of a strip chart whose `compute` works in the strip frame (levels on `:y`, values on `:x`), with the
  levels on the mapping axis of [[level-axis]]: on `:x` it computes the transposed input - mapping, the `:padding`
  option and the plot's scales with `:x` and `:y` exchanged, `:flip?` inverted - and its stats are transposed back
  (see [[transposed-stats]]). The stats carry `:level-axis`, every panel's data `::level-axis` (for
  [[draw-oriented]])."
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
  "Draw one panel's `computed` data with a strip chart's `draw` working in the strip frame: the position scales
  exchanged when the levels are on `:x` (see [[compute-oriented]]) - placement and projections take their
  directions from the scales."
  [draw chart computed canvas box scales env]
  (draw chart computed canvas box (if (= :x (::level-axis computed)) (swap-xy scales) scales) env))
