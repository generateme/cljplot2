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

  Groups (ordinal aesthetics and `:group`, see [[cljplot2.data.group]]) inside a placement - option `:position`:
  `:dodge` (default with two or more groups; the cross range split equally in group order, each part narrowed by
  `:dodge-gap` - a fraction of it, default 0.1 - so groups don't touch), `:overlay` (the whole
  range) or `:mirror` (exactly two groups: the first gets `[0 0.5]`, the second `[-0.5 0]`; split violins)."
  (:require [fastmath.core :as m]
            [cljplot2.data.group :as group]
            [cljplot2.data.position :as position]
            [cljplot2.data.rows :as data]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private named-places #{:band :center :edge})
(def ^:private positions #{:dodge :overlay :mirror})

(def options
  "Map entries of the options every strip chart takes (besides [[cljplot2.data.rows/common-options]])."
  [(schema/optional :place [:multi {:dispatch map?}
                            [true [:map {:closed true} [:at schema/finite]]]
                            [false (into [:enum] (sort named-places))]])
   (schema/optional :width schema/non-neg)
   (schema/optional :thickness schema/non-neg)
   (schema/optional :position (into [:enum] (sort positions)))
   (schema/optional :dodge-gap schema/fraction)])

(defn- place-of
  "The placement of a chart: explicit `:place` or the default (`:band` with a `:y` mapping, else `default-place`).
  Throws `ex-info {:key :place}` for `:band` without a `:y` mapping or another placement with one."
  [options mapping default-place]
  (let [y? (some? (:y mapping))
        place (get options :place (if y? :band default-place))]
    (cond
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
  chart's placement without `:y`."
  [{:keys [groups mapping]} options default-place env]
  (let [place (place-of options mapping default-place)
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
                                     :slot (slot-of (:key g))}))))]
    {:place place
     :position position
     :items items
     :group-keys group-keys
     :panels panels
     :dropped (cond-> (assoc dropped :x (count (remove #(let [v (x-of %)] (and (number? v) (m/valid-double? (double v))))
                                                       all-rows)))
                y-of (assoc :y (count (remove (comp data/valid-value? y-of) all-rows))))}))

(defn y-extent
  "`:y` extent of a panel's `items` for `place`: the levels (bands), `v` of `{:at v}`, else `nil`."
  [place items]
  (cond
    (= :band place) (let [levels (distinct (map :level items))
                          e (data/extent :y levels)]
                      ;; numeric levels (on an explicit :bands scale) travel as the extent's :values
                      (cond-> e (= :numeric (:kind e)) (assoc :values (vec levels))))
    (map? place) {:kind :numeric :min (double (:at place)) :max (double (:at place))}
    :else nil))

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
  `:span` - the pixel length of the whole cross range. A level or value the scales can't place gives `##NaN`
  coordinates. Flip-aware: the cross axis is the `:y` direction (horizontal when flipped).
  Throws `ex-info {:key :place}` for `:band` without a `:bands` `:y` scale and `{:at v}` on a `:bands` one."
  [{:keys [w h]} scales options place]
  (let [fx (scale/forward-fn (:x scales))
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
        bandwidth (when (= :band place)
                    (if-let [geometry (scale/band-geometry y-scale)]
                      (double (:bandwidth geometry))
                      (schema/fail! "Strip :place :band needs a :bands :y scale." {:key :place :value (:kind y-scale)})))
        _ (when (and (map? place) (= :bands (:kind y-scale)))
            (schema/fail! "Strip :place {:at v} needs a numeric :y scale, not bands." {:key :place :value place}))
        centre-and-span (cond
                          (= :band place) (fn [level] [(if fy (fy level) ##NaN) (* width (double bandwidth))])
                          (map? place) (let [c (if fy (fy (:at place)) ##NaN)] (constantly [c thickness]))
                          (= :center place) (constantly [(* 0.5 length) (* width length)])
                          (= :edge place) (let [low (if (pos? sign) 0.0 length)]
                                            (constantly [(+ low (* sign 0.5 thickness)) thickness])))]
    {:point (fn ^Vec2 [value level ^double cross]
              (let [[c span] (centre-and-span level)
                    p (+ (double c) (* sign cross (double span)))
                    v (double (fx value))]
                (if cross-horizontal? (Vec2. p v) (Vec2. v p))))
     :span (fn ^double [level] (double (second (centre-and-span level))))}))
