(ns cljplot2.data.strip
  "1d strips: charts drawing the distribution of `:x` values (box, violin, stripplot, rug, extent) placed across the
  `:y` dimension (value on `:x`, horizontal by default; `:flip true` makes strips vertical).

  Contract: a strip chart computes, per panel, **items** - one per `:y` level and group -
  `{:level l :key k :aesthetics a :values [..] :slot [lo hi]}` plus its own statistics, all in data units along the
  value axis and in normalized **cross** units across it: the placement's cross range is `[-0.5 0.5]`, `:slot` the
  part of it the item's group gets (see positions below). At draw, [[placement]] maps a value and a cross coordinate
  to a pixel point.

  Placement - option `:place`:

  * `:band` (default when `:y` is mapped): `:y` levels on a `:bands` scale (categorical values, or numbers with
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
  `:dodge` (default with two or more groups; the cross range split equally in group order, each part narrowed by
  `:dodge-gap` - a fraction of it, default 0.1 - so groups don't touch), `:overlay` (the whole
  range) or `:mirror` (exactly two groups: the first gets `[0 0.5]`, the second `[-0.5 0]`; split violins).

  Ridgelines (histogram and density with a categorical or bands `:y`, see [[ridges?]]): one-sided profiles growing
  from the band centre toward the next level, the tallest `:overlap` (default 0.9) band steps high.

  Every strip chart throws `ex-info {:key :mapping}` when its rows have no finite `:x` value.

  n labels (every strip chart): `:n-label true` draws each item's count, formatted by `:n-format` (default
  `\"n=%d\"`), just past its largest drawn value (see [[draw-n-labels!]]); the chart asks for value-axis room for
  them (`:padding-px`, see [[n-label-room]])."
  (:require [fastmath.core :as m]
            [cljplot2.content.label :as label]
            [cljplot2.data.group :as group]
            [cljplot2.data.position :as position]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private named-places #{:band :center :edge :bottom})
(def ^:private positions #{:dodge :overlay :mirror})

(def options
  "Map entries of the options every strip chart takes (besides [[cljplot2.data.rows/common-options]])."
  [(schema/optional :place [:multi {:dispatch map?}
                            [true [:map {:closed true} [:at schema/finite]]]
                            [false (into [:enum] (sort named-places))]])
   (schema/optional :width schema/non-neg)
   (schema/optional :thickness schema/non-neg)
   (schema/optional :position (into [:enum] (sort positions)))
   (schema/optional :dodge-gap schema/fraction)
   (schema/optional :bottom-share schema/fraction)
   (schema/nilable :n-label schema/bool)
   (schema/optional :n-format :string)])

(def ridge-options
  "Map entries of the ridgeline options (histogram and density with a categorical `:y`)."
  [(schema/optional :overlap schema/non-neg)])

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

(defn- check-levels!
  "Throw `ex-info {:key :place}` unless the `:y` `levels` can be bands: strings / keywords, or numbers with an explicit
  `:bands` plot scale."
  [levels plot-scales]
  (let [bands? (= :bands (get-in plot-scales [:y :kind]))]
    (when-not (every? #(or (string? %) (keyword? %) (and bands? (number? %))) levels)
      (schema/fail! "Strip :y levels must be categorical (strings / keywords), or numbers with :scales {:y {:kind :bands}}."
                    {:key :place :value (first (remove #(or (string? %) (keyword? %)) levels))}))))

(defn- slots
  "Cross sub-range per group key for `position` over the ordered `group-keys`; dodged sub-ranges lose `gap` (a
  fraction of each) split between their ends."
  [position group-keys gap]
  (let [n (count group-keys)]
    (case position
      :overlay (zipmap group-keys (repeat [-0.5 0.5]))
      :dodge (zipmap group-keys (map (fn [i] (let [[a b] (position/dodge-interval [-0.5 0.5] i n)
                                                    inset (* 0.5 (double gap) (- (double b) (double a)))]
                                                [(+ (double a) inset) (- (double b) inset)]))
                                     (range n)))
      :mirror (if (= 2 n)
                (zipmap group-keys [[0.0 0.5] [-0.5 0.0]])
                (schema/fail! "Strip :position :mirror needs exactly two groups." {:key :position :groups n})))))

(defn split
  "Items of a strip chart: the rows of every panel (`chart-split` - `{:groups {panel rows} :mapping m}`) split into
  groups (ordinal aesthetics, `:group`) and `:y` levels, `:values` - the finite `:x` numbers.
  Returns `{:place p :position pos :items {panel [item ...]} :group-keys [..] :panels split-panels :dropped {dim n}}`;
  items carry `:level` (`nil` without bands), `:key`, `:aesthetics`, `:values`, `:slot`. `default-place` - the
  chart's placement without `:y`. Throws `ex-info {:key :mapping}` when there are rows but no finite `:x` value,
  `ex-info {:key :n-format}` for an `:n-format` that can't format a count."
  [{:keys [groups mapping]} options default-place env]
  (let [_ (when-let [fmt (:n-format options)]
            (try (format fmt 1)
                 (catch Exception e
                   (schema/fail! "Strip :n-format should format a count (e.g. \"n=%d\")."
                                 {:key :n-format :value fmt :cause (ex-message e)}))))
        place (place-of options mapping default-place)
        {:keys [panels dims dropped]} (group/split groups (dissoc mapping :y) env)
        x-of (data/row-accessor :x (:x mapping))
        y-of (when (= :band place) (data/row-accessor :y (:y mapping)))
        all-rows (mapcat :rows (mapcat val panels))
        _ (when y-of (check-levels! (filter data/valid-value? (map y-of all-rows)) (:plot-scales env)))
        group-keys (group/ordered-keys (mapcat #(map :key %) (vals panels)) dims (:plot-scales env))
        position (get options :position (if (> (count group-keys) 1) :dodge :overlay))
        slot-of (if (seq group-keys) (slots position group-keys (double (get options :dodge-gap 0.1))) {})
        items (update-vals panels
                           (fn [gs]
                             (vec (for [g gs
                                        [level rows] (if y-of
                                                       (group-by y-of (filter (comp data/valid-value? y-of) (:rows g)))
                                                       [[nil (:rows g)]])
                                        :let [{:keys [values]} (data/finite-numbers (map x-of rows))]
                                        :when (seq values)]
                                    {:level level :key (:key g) :aesthetics (:aesthetics g) :values values
                                     :slot (slot-of (:key g))}))))
        x-dropped (count (remove #(let [v (x-of %)] (and (number? v) (m/valid-double? (double v)))) all-rows))]
    (when (and (seq all-rows) (= x-dropped (count all-rows)))
      (schema/fail! "No valid values for a mapped dimension - check the mapping."
                    {:key :mapping :value :x :rows (count all-rows)}))
    {:place place
     :position position
     :items items
     :group-keys group-keys
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
  `:step` - the band step in pixels (`:band`, else `nil`). A level or value the scales can't place gives `##NaN`
  coordinates. Flip-aware: the cross axis is the `:y` direction (horizontal when flipped). `:bottom` without a `:y`
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
        length (double (if cross-horizontal? w h))
        ;; pixel direction of increasing :y
        sign (if y-scale
               (let [[a b] (:range y-scale)] (if (< (double b) (double a)) -1.0 1.0))
               (if cross-horizontal? 1.0 -1.0))
        width (double (get options :width 0.8))
        thickness (double (get options :thickness (if (= :edge place) 8.0 20.0)))
        geometry (when (= :band place)
                   (or (scale/band-geometry y-scale)
                       (schema/fail! "Strip :place :band needs a :bands :y scale." {:key :place :value (:kind y-scale)})))
        bandwidth (some-> geometry :bandwidth double)
        _ (when (and (map? place) (= :bands (:kind y-scale)))
            (schema/fail! "Strip :place {:at v} needs a numeric :y scale, not bands." {:key :place :value place}))
        centre-and-span (cond
                          (= :band place) (fn [level] [(if fy (fy level) ##NaN) (* width (double bandwidth))])
                          (map? place) (let [c (if fy (fy (:at place)) ##NaN)] (constantly [c thickness]))
                          (= :center place) (constantly [(* 0.5 length) (* width length)])
                          (= :edge place) (let [low (if (pos? sign) 0.0 length)]
                                            (constantly [(+ low (* sign 0.5 thickness)) thickness]))
                          (= :bottom place) (let [low (if (pos? sign) 0.0 length)
                                                  zero (double (fy 0.0))]
                                              (constantly [(* 0.5 (+ low zero)) (* width (m/abs (- zero low)))])))]
    {:point (fn ^Vec2 [value level ^double cross]
              (let [[c span] (centre-and-span level)
                    p (+ (double c) (* sign cross (double span)))
                    v (double (fx value))]
                (if cross-horizontal? (Vec2. p v) (Vec2. v p))))
     :span (fn ^double [level] (double (second (centre-and-span level))))
     :value-pixel (fn ^double [value] (double (fx value)))
     :centre (fn ^double [level] (double (first (centre-and-span level))))
     :sign sign
     :step (some-> geometry :step double)}))

(def ^:private ^:const n-label-gap
  "Pixels between an item's largest drawn value and its n label."
  3.0)

(defn- n-label-text [options item] (format (get options :n-format "n=%d") (count (:values item))))

(defn n-label-room
  "`:padding-px` of a strip chart (`items` - `{panel [item ...]}`) with `:n-label`: `{:x [0 px]}`, `px` - the largest
  label's size along the value axis (its height when the plot is flipped, env `:flip?`) plus the gap, measured with
  the `:annotation` role (env `:theme`, `:hint`), so the labels past the largest values fit; `nil` without n labels
  or items."
  [options items {:keys [theme flip? hint]}]
  (let [texts (when (data/option options :n-label false)
                (distinct (map #(n-label-text options %) (mapcat val items))))]
    (when (seq texts)
      (measure/call-with-scratch-canvas
       (or hint :high)
       (fn [c]
         (let [along (reduce max (map (fn [t] (let [^Vec2 size (measure/natural-size {:kind :label :role :annotation :text t}
                                                                                       {:canvas c :theme theme})]
                                                (if flip? (.y size) (.x size))))
                                      texts))]
           {:x [0.0 (+ (double along) n-label-gap)]}))))))

(defn draw-n-labels!
  "With option `:n-label`, draw each item's count of values (`:n-format`, default `\"n=%d\"`; text role
  `:annotation`) just past `(top-value item)` - its largest drawn value - along the value axis, centred on its slot.
  `pl` - the chart's [[placement]]; [[n-label-room]] keeps room for them past the data."
  [canvas {:keys [point]} items top-value options scales env]
  (when (data/option options :n-label false)
    (let [x-scale (:x scales)
          [r0 r1] (:range x-scale)
          ;; pixel direction of increasing values
          up (if (< (double r1) (double r0)) -1.0 1.0)
          horizontal? (scale/horizontal? :x x-scale)
          pad n-label-gap
          anchor (if horizontal?
                   (if (pos? up) [:left :middle] [:right :middle])
                   (if (pos? up) [:center :top] [:center :bottom]))]
      (doseq [{:keys [level slot] :as item} items
              :let [^Vec2 p (point (top-value item) level (in-slot slot 0.0))
                    x (if horizontal? (+ (.x p) (* up pad)) (.x p))
                    y (if horizontal? (.y p) (+ (.y p) (* up pad)))]
              :when (and (m/valid-double? x) (m/valid-double? y))]
        (label/draw-at! canvas {:kind :label :role :annotation :text (n-label-text options item)} anchor x y env)))))

(defn ridges?
  "Does a histogram / density of `chart-split` (`{:groups {panel rows} :mapping m}`) draw ridgelines: `:y` mapped
  with some categorical (string / keyword) value, or on an explicit `:bands` plot scale? A numeric `:y` otherwise is
  ignored, as before ridgelines (a plot mapping shared with xy charts)."
  [{:keys [groups mapping]} env]
  (when-let [acc (:y mapping)]
    (or (= :bands (get-in env [:plot-scales :y :kind]))
        (let [y-of (data/row-accessor :y acc)]
          (boolean (some #(let [v (y-of %)] (or (string? v) (keyword? v))) (mapcat val groups)))))))

(defn ridge-split
  "Items of a ridgeline chart (histogram / density, see [[ridges?]] and [[split]]): placed in bands, groups
  `:overlay` (default) or `:mirror` (a double ridgeline: the first group grows toward higher `:y`, the second away
  from it). Throws `ex-info {:key :position}` for other positions."
  [chart-split options env]
  (let [position (get options :position :overlay)]
    (when-not (#{:overlay :mirror} position)
      (schema/fail! "Ridgelines (a categorical or bands :y) take :position :overlay or :mirror."
                    {:key :position :value position}))
    (split chart-split {:position position} :band env)))

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
