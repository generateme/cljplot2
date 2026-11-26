(ns cljplot2.chart.beeswarm
  "Beeswarm: one point per `:x` value, packed across its strip so points don't overlap - a strip chart (see
  [[cljplot2.data.strip]]).

  Packing happens at draw, in pixels ([[swarm-offsets]]): values in increasing order, each at the smallest cross offset
  (absolute) where its circle of diameter `:size` touches no point placed before. With `:fit` (default true) a swarm
  wider than its item's cross range is squeezed into it (points may then overlap); `:fit false` keeps the packing
  and lets the swarm leave its range. With `:position :mirror` each group swarms to its own side only.

  `:pack` - `:across` (default, the packing above: points move only across the value axis, so equal values form a
  line) or `:around` - in both directions: each point, in value order, at the free spot nearest its value position
  ([[around-offsets]]) - equal or close values form round clusters, as d3 / Vega force beeswarms (points then sit up to
  a few diameters off their value along the axis); `:fit` squeezes the cross offsets only.

  Options (style defaults from theme `[:chart :beeswarm]`): `:size`, `:color`, `:stroke-color`, `:stroke-width`,
  `:shape` (as scatter, see [[cljplot2.graphics.shape]]); `:fit`, `:pack`, `:padding {:x [lo hi]}` (default
  `[0.05 0.05]`), plus the strip options and `:data` / `:mapping`.

  Functions: [[swarm-offsets]], [[around-offsets]] and [[fit-offsets]] - the packing; the key glyph is [[cljplot2.chart.legend/point-legend]] (the chart computes and draws through [[cljplot2.chart.strip/compute]] and [[cljplot2.chart.strip/draw]]); [[schemas]] - its style, options and mapping."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.data.group :as group]
            [cljplot2.data.position :as position]
            [cljplot2.data.rows :as data]
            [cljplot2.chart.strip :as strip-chart]
            [cljplot2.data.strip :as strip]
            [cljplot2.graphics.shape :as shape]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries
  "Map entries of the beeswarm's point style (theme `[:chart :beeswarm]`), also its options."
  [(schema/optional :size {:description "The point size in pixels, also the diameter the packing keeps apart; a mapped `:size` wins." :cljplot2/example 12} schema/non-neg)
   (schema/optional :color {:description "The colour of the points; a mapped `:color` wins." :cljplot2/value-meanings {:none "No fill."} :cljplot2/example :orange} schema/color)
   (schema/optional :stroke-color {:description "The colour of each point's outline." :cljplot2/value-meanings {:none "No outline."} :cljplot2/example :black} schema/color)
   (schema/optional :stroke-width {:description "The width of the outline in pixels; `0` draws no outline." :cljplot2/example 3 :cljplot2/example-context {:stroke-color :black}} schema/non-neg)
   (schema/optional :shape {:description "The shape of every point; a mapped `:shape` wins." :cljplot2/example \s} schema/shape)])

(def schemas
  "Schemas of the beeswarm chart: `:style` (theme `[:chart :beeswarm]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into theme/theme-style-entries)
                (into strip/options)
                (conj (schema/nilable :fit {:description "Whether the chart squeezes a swarm wider than its slot into it; the points may then overlap."
                                             :default true :cljplot2/value-meanings {true "Squeezes the swarm into its slot." false "Keeps the packing; the swarm may leave its slot."}
                                             :cljplot2/example-context {:size 40}}
                                      schema/bool)
                      (schema/optional :pack {:description "How the chart packs the points."
                                              :default :across
                                              :cljplot2/value-meanings {:across "Across the value axis only, so equal values form a line."
                                                                        :around "In both directions: close values form round clusters, the points up to a few diameters off their value."}}
                                       [:enum :across :around])
                      strip/padding-option))
   :mapping (strip/values-mapping
              [(schema/nilable :x {:description "The values of horizontal swarms, or the levels of vertical ones."} schema/accessor)
               (schema/nilable :y {:description "The levels of horizontal swarms, or the values of vertical ones; with no levels the chart draws one swarm."}
                               schema/accessor)
               (schema/nilable :color {:description "Splits the rows into groups, each in its own point colour."} schema/accessor)
               (schema/nilable :shape {:description "Splits the rows into groups, each with its own point shape."} schema/accessor)
               (schema/nilable :size {:description "Splits the rows into groups, each with its own point size."} schema/accessor)
               (schema/nilable :group {:description "Splits the rows into groups that look the same, dodged as other groups."} schema/accessor)])})

(def ^{:const true :private true :tag 'double} eps
  "Tolerance (pixels) of the packing: offsets closer than this to an interval end count as on it."
  1.0e-9)

(defn- nearest-free
  "Gives the offset nearest 0 that lies in none of some open intervals.

  Parameters:

  - `intervals` (sequence of `[lo hi]`): forbidden open intervals, sorted by `lo`.
  - `one-sided?`: only non-negative offsets.

  Returns the offset: 0 when 0 is free, else an end of the merged interval holding 0 (overlapping intervals are merged, so its ends lie in no interval) - its upper end with `one-sided?`, else the nearer end, ties preferring the positive side."
  ^double [intervals one-sided?]
  (let [merged (reduce (fn [acc [^double lo ^double hi]]
                         (let [[plo ^double phi] (peek acc)]
                           (if (and plo (m/< lo (m/- phi eps)))
                             (conj (pop acc) [plo (m/max phi hi)])
                             (conj acc [lo hi]))))
                       [] intervals)
        [low high] (first (filter (fn [[^double lo ^double hi]] (m/< (m/+ lo eps) 0.0 (m/- hi eps))) merged))]
    (cond
      (nil? low) 0.0
      one-sided? (double high)
      (m/< (m/- (double low)) (m/- (double high) eps)) (double low)
      :else (double high))))

(defn swarm-offsets
  "Gives the cross offsets packing circles along the value axis without overlap.

  Each circle, in order, takes the smallest absolute offset where it is at least `diameter` from every circle placed before it; each earlier circle within a diameter along the axis forbids an open offset interval, and the answer is the free offset nearest 0 (see [[nearest-free]]) - `O(k log k)` for `k` such circles.

  Parameters:

  - `positions` (sequence of doubles): the circles' pixel positions along the value axis, increasing.
  - `diameter` (double): the circle diameter in pixels.
  - `one-sided?`: only non-negative offsets (a mirrored group).

  Returns a vector of offsets in pixels, one per position; ties prefer the positive side.

  See also [[around-offsets]], [[fit-offsets]]."
  [positions ^double diameter one-sided?]
  (let [d2 (m/* diameter diameter)]
    (loop [ps (seq positions)
           placed []
           offsets (transient [])]
      (if-not ps
        (persistent! offsets)
        (let [p (double (first ps))
              ;; forbidden intervals of the circles placed before within one diameter (positions are increasing)
              intervals (loop [i (m/long-dec (count placed))
                               acc []]
                          (let [[q c] (when (m/>= i 0) (placed i))]
                            (if (and q (m/< (m/- p (double q)) diameter))
                              (let [dp (m/- p (double q))
                                    h (m/sqrt (m/max 0.0 (m/- d2 (m/* dp dp))))]
                                (recur (m/long-dec i) (if (m/> h eps) (conj acc [(m/- (double c) h) (m/+ (double c) h)]) acc)))
                              acc)))
              o (nearest-free (sort-by first intervals) one-sided?)]
          (recur (next ps) (conj placed [p o]) (conj! offsets o)))))))

(def ^{:private true :const true :tag 'long} max-spiral-steps
  "Most spiral spots [[around-offsets]] tests for one circle."
  100000)

(defn- free-spot?
  "Tests whether a circle is clear of every circle placed before it.

  Parameters:

  - `grid` (map): cell `[i j]` of side `diameter` -> the centres `[u c]` placed in it.
  - `u`, `c` (doubles): the circle's centre - along the value axis and across it, pixels.
  - `diameter` (double): the circle diameter.

  Returns true when no centre in the 3 x 3 cells around it is closer than `diameter`."
  [grid ^double u ^double c ^double diameter]
  (let [i (long (m/floor (m// u diameter)))
        j (long (m/floor (m// c diameter)))
        limit (m/- (m/* diameter diameter) eps)]
    (not-any? (fn [[^double pu ^double pc]] (m/< (m/+ (m/sq (m/- u pu)) (m/sq (m/- c pc))) limit))
              (for [di [-1 0 1] dj [-1 0 1] pt (get grid [(m/long-add i di) (m/long-add j dj)])] pt))))

(defn around-offsets
  "Gives the offsets packing circles around their value positions in both directions.

  Each circle, in order, takes the first spot of a Vogel spiral around `[position 0]` (spacing a quarter diameter, turning by [[cljplot2.data.position/golden-angle]] - closely the free spot nearest its target) where it is at least `diameter` from every circle placed before. Deterministic; a grid of cell side `diameter` keeps each test local. Each search restarts at the target, so many equal values cost more steps per point (a cluster of `k` circles - about 16 `k` spots for the last one).

  Parameters:

  - `positions` (sequence of doubles): the circles' pixel positions along the value axis, increasing.
  - `diameter` (double): the circle diameter in pixels.
  - `one-sided?`: spots with a non-negative cross offset only.

  Returns a vector of `[along cross]` offsets in pixels, one per position; all zero for a zero `diameter`; a circle with no free spot within [[max-spiral-steps]] stays at its target (overlapping).

  See also [[swarm-offsets]]."
  [positions ^double diameter one-sided?]
  (if-not (m/pos? diameter)
    (mapv (constantly [0.0 0.0]) positions)
    (let [step (m/* 0.25 diameter)]
      (loop [ps (seq positions)
             grid {}
             offsets (transient [])]
        (if-not ps
          (persistent! offsets)
          (let [p (double (first ps))
                [^double u ^double c] (or (first (for [k (range max-spiral-steps)
                                                       :let [r (m/* step (m/sqrt (double k)))
                                                             a (m/* (double k) position/golden-angle)
                                                             spot-u (m/+ p (m/* r (m/cos a)))
                                                             spiral-c (m/* r (m/sin a))
                                                             spot-c (if one-sided? (m/abs spiral-c) spiral-c)]
                                                       :when (free-spot? grid spot-u spot-c diameter)]
                                                   [spot-u spot-c]))
                                          [p 0.0])
                cell [(long (m/floor (m// u diameter))) (long (m/floor (m// c diameter)))]]
            (recur (next ps) (update grid cell conj [u c]) (conj! offsets [(m/- u p) c]))))))))

(defn fit-offsets
  "Squeezes offsets proportionally into a room.

  Parameters:

  - `offsets` (sequence of doubles): cross offsets in pixels.
  - `room` (double): the largest absolute offset allowed.

  Returns a vector of the offsets, each scaled by `room / widest` when the widest exceeds `room`, else unchanged."
  [offsets ^double room]
  (let [widest (double (reduce m/max 0.0 (map #(m/abs (double %)) offsets)))]
    (if (m/> widest room) (mapv #(m/* (double %) (m// room widest)) offsets) (vec offsets))))

(defn- compute-in-strip-frame
  "Computes the beeswarm's stats in the strip frame (levels on `:y`).

  Parameters:

  - `chart` (map): the chart value - `:options` (the strip options, style options).
  - `chart-split` (map): the chart's rows split per panel.
  - `env` (map): the compute environment - `:theme`, `:plot-scales` ...

  Returns `{:data :extents :padding :padding-explicit :padding-px :na :dropped}`: per panel `{:items [..] :style st :place p :options o :one-sided b}` (`:one-sided` - mirrored groups swarm to their own side); x extents - the values. The packing happens at draw, in pixels."
  [{:keys [options] :as chart} chart-split {:keys [theme] :as env}]
  (let [{:keys [place position items panels dropped]} (strip/split chart-split options :center env)
        point-style (theme/resolved-style theme chart)]
    {:data (update-vals items (fn [its] {:items its :style point-style :place place :options options
                                         :one-sided (= :mirror position)}))
     :extents (merge-with merge (group/aesthetic-extents panels (:plot-scales env))
                          (strip/extents place items (fn [{:keys [values]}] [(v/mn values) (v/mx values)])))
     :padding (strip/padding place options)
     :padding-explicit (strip/explicit-padding place options)
     :padding-px (strip-chart/n-label-room options items env)
     :na (group/na-dims panels)
     :dropped dropped}))

(defn- value-direction
  "Gives the unit pixel vector of increasing value along the value axis at a value.

  Parameters:

  - `point` (function): of a value, a level and a cross position giving the pixel `Vec2`.
  - `v` (double): the value.
  - `level`: the item's level.
  - `cross` (double): the cross position.

  Returns the direction from a nearby value (`v` + 0.1% of its magnitude, at least 0.001); a zero vector when the axis doesn't move there."
  ^Vec2 [point ^double v level cross]
  (let [^Vec2 a (point v level cross)
        ^Vec2 b (point (m/+ v (m/* 1.0e-3 (m/max 1.0 (m/abs v)))) level cross)
        d (v/sub b a)
        len (v/mag d)]
    (if (and (m/valid-double? len) (m/pos? len)) (v/div d len) (v/vec2 0.0 0.0))))

(defn- draw-in-strip-frame
  "Draws the points of one panel in the strip frame, each item swarmed around its centre line.

  Parameters:

  - `_chart` (map): the chart value (unused).
  - `computed` (map): the panel's stats - `:items`, `:style`, `:place`, `:options` (`:pack` default `:across`, `:fit` default true), `:one-sided` (mirrored groups swarm from 0 outward).
  - `canvas`: the clojure2d canvas.
  - `box` (map): the panel box.
  - `scales` (map): finalized scales (in the strip frame).
  - `env` (map): the draw environment - `:theme`.

  Packs each item's values by [[swarm-offsets]] (`:pack :around` - [[around-offsets]]) at the point size, fitted into its slot with `:fit` ([[fit-offsets]]); skips a group outside its scale's domain, a level without span and points the scales can't place; then the n labels."
  [_chart {:keys [items style place options one-sided]} canvas box scales env]
  (let [{:keys [point span value-pixel] :as pl} (strip-chart/placement box scales options place)
        fit? (data/option options :fit true)
        around? (= :around (:pack options))]
    (c2d/set-stroke-custom canvas {:size (:stroke-width style)})
    (doseq [{:keys [level slot values aesthetics]} items
            :let [st (group/style style aesthetics scales (:theme env))
                  s (double (span level))]
            :when (and st (m/pos? s))]
      (let [[^double lo ^double hi] slot
            side (strip/slot-side slot)
            base (if one-sided 0.0 (strip/in-slot slot 0.0))
            room (m/* s (if one-sided (m/abs (m/+ lo hi)) (strip/slot-half slot)))
            ;; [value pixel] pairs in increasing pixel order (a reversed value axis reverses the values)
            kept (sort-by second (filter #(m/valid-double? (double (second %))) (map (juxt identity value-pixel) values)))
            diameter (double (:size st))
            pairs (if around?
                    (around-offsets (map second kept) diameter one-sided)
                    (mapv (fn [o] [0.0 o]) (swarm-offsets (map second kept) diameter one-sided)))
            crosses (cond->> (map second pairs) fit? (#(fit-offsets % room)))]
        (doseq [[[v] [^double along] ^double o] (map vector kept pairs crosses)
                :let [cross (m/+ base (m// (m/* (if one-sided side 1.0) o) s))
                      ^Vec2 at (point v level cross)
                      ^Vec2 p (if (m/zero? along)
                                at
                                (v/add at (v/mult (value-direction point v level cross) along)))]
                :when (data/finite-point? p)]
          (shape/draw! canvas (:shape st) p st))))
    (strip-chart/draw-n-labels! canvas pl items (fn [{:keys [values]}] (v/mx values)) options scales env)))

(defmethod strip-chart/in-frame-compute :beeswarm [chart chart-split env] (compute-in-strip-frame chart chart-split env))

(defmethod strip-chart/in-frame-draw :beeswarm [chart computed canvas box scales env]
  (draw-in-strip-frame chart computed canvas box scales env))

