(ns cljplot2.chart.strip
  "Strip chart framework: the chart contract shared by every strip chart (box, violin, stripplot, beeswarm, rug, extent, bar, bullet, dotplot, histogram and density ridges) and the labels drawn past their items.

  Strip charts compute and draw in the strip frame of [[cljplot2.data.strip]] (levels on `:y`, values on `:x`). A strip chart value takes [[compute]] and [[draw]], which call its kind's strip-frame methods [[in-frame-compute]] and [[in-frame-draw]] (multimethods on the chart's `:kind`: a new strip kind adds a method of each - a kind without one throws `No method in multimethod`).

  n labels (every strip chart): `:n-label true` draws each item's count, formatted by `:n-format` (default `n=%d`), just past its largest drawn value (see [[draw-n-labels!]]); the chart asks for value-axis room for them (`:padding-px`, see [[n-label-room]]).

  Functions:

  - chart contract: [[compute]], [[draw]], [[in-frame-compute]], [[in-frame-draw]]
  - drawing in the strip frame: [[placement]], [[placeable?]], [[draw-oriented]]
  - end labels: [[end-label-room]], [[end-label-placements]], [[without-collisions]], [[draw-label-placements!]], [[draw-end-labels!]], [[outer-label-room]], [[n-label-room]], [[draw-n-labels!]]"
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.content.label :as label]
            [cljplot2.data.rows :as data]
            [cljplot2.data.strip :as strip]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.scale.coord :as coord]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^{:const true :private true :tag 'double} n-label-gap
  "Pixels between an item's largest drawn value and its n label."
  3.0)

(defn- n-label-side
  "Gives the direction of an item's n label along the value axis.

  Parameters:

  - `item` (map): the item - `:mirrored`.

  Returns `-1.0` for a `:mirrored` item, else `1.0`."
  ^double [item]
  (if (:mirrored item) -1.0 1.0))

(defn- n-label-text
  "Gives the n label text of an item.

  Parameters:

  - `chart-options` (map): chart options - `:n-format` (a format string or a function of the count, default `n=%d`).
  - `item` (map): the item - its count `:n`.

  Returns the count formatted by `:n-format`."
  [chart-options item]
  ((schema/text-formatter (data/option chart-options :n-format "n=%d")) (:n item)))

(defn- label-content
  "Gives the label content of an end label.

  Parameters:

  - `label` (map): the end label - `:text`, `:role` (text role, default `:annotation`), `:color` (optional).

  Returns `{:kind :label :role r :text t}`, with `:color` when the label has one."
  [{:keys [text color role]}]
  (cond-> {:kind :label :role (or role :annotation) :text text} color (assoc :color color)))

(defn end-label-room
  "Gives the `:padding-px` end labels need past the data.

  Parameters:

  - `labels` (sequence): end labels `{:text t :side s :role r}` (see [[draw-end-labels!]]).
  - `env` (map): `:theme`, `:hint` (measuring), `:flip` (sizes along the value axis are label heights when flipped).

  Returns `{:x [lo hi]}`: per side of the value axis the largest label's size along it, measured in its role, plus the gap; 0 on a side without labels. `nil` without labels."
  [labels {:keys [theme flip hint]}]
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
                                                       (if flip (.y size) (.x size))))
                                        contents))
                             n-label-gap)
                        0.0)))]
         {:x [(room -1.0) (room 1.0)]})))))

(def ^{:private true :const true :tag 'double} outer-label-gap
  "Pixels between a polar plot's outer radius and an `:outer` end label."
  6.0)

(def ^{:private true :const true :tag 'double} leader-label-gap
  "Pixels between a polar plot's outer radius and an `:outer` end label with a leader line (`:leader`)."
  16.0)

(defn- outer-gap
  "Gives the pixels between a polar plot's outer radius and an `:outer` end label.

  Parameters:

  - `label` (map): the end label - `:leader` (a leader line).

  Returns [[leader-label-gap]] with `:leader`, else [[outer-label-gap]]."
  ^double [{:keys [leader]}]
  (if leader leader-label-gap outer-label-gap))

(declare polar-end-label-placements cartesian-end-label-placements)

(defn end-label-placements
  "Gives where [[draw-end-labels!]] draws end labels.

  Parameters:

  - `pl` (map): the chart's [[cljplot2.chart.strip/placement]].
  - `labels` (sequence): end labels (see [[draw-end-labels!]]).
  - `scales` (map): finalized position scales.

  Returns a vector of `{:label l :content c :anchor [h v] :x px :y py :leader [[x0 y0] [x1 y1]]}`: `:content` - the label content, `:leader` (an `:outer` label with `:leader`) - a line from its segment's outer edge. Labels without a placeable position are left out."
  [{:keys [geometry] :as pl} labels scales]
  (if geometry
    (polar-end-label-placements pl labels scales)
    (cartesian-end-label-placements pl labels scales)))

(defn- cartesian-end-label-placements
  "Gives the [[end-label-placements]] of a cartesian plot: each label [[n-label-gap]] pixels past its value along the value axis, anchored away from it (centred for side 0).

  Parameters:

  - `pl` (map): the chart's [[cljplot2.chart.strip/placement]] - `:point`.
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
                     ^Vec2 p (point at level (strip/in-slot slot 0.0))
                     x (if horizontal? (m/+ (.x p) (m/* dir n-label-gap)) (.x p))
                     y (if horizontal? (.y p) (m/+ (.y p) (m/* dir n-label-gap)))]
               :when (data/finite-point? [x y])]
           {:label l :content (label-content l) :anchor anchor :x x :y y}))))

(defn- outer-placement
  "Gives the placement of an `:outer` end label in a polar plot.

  Parameters:

  - `pl` (map): the chart's [[cljplot2.chart.strip/placement]] - `:range-point`, `:geometry`.
  - `label` (map): the end label - `:level`, `:slot`, `:at` (the value of its segment's middle), `:span` (its segment's values), `:leader`.

  Returns the placement past the outer radius (by [[outer-gap]]) at the angle of the segment's middle, anchored away from the centre, with a `:leader` from the segment's farthest corner for `:leader`; `nil` at a non-finite pixel."
  [{:keys [range-point geometry]} {:keys [level slot at span leader] :as l}]
  (let [[^double h] (range-point at level (strip/in-slot slot 0.0))
        r (m/+ (double (:radius geometry)) (outer-gap l))
        ^Vec2 p (coord/to-pixel geometry h r)
        edge (when leader
               (v/mx (for [value span cross [-0.5 0.5]] (double (second (range-point value level (strip/in-slot slot cross)))))))]
    (when (data/finite-point? p)
      (cond-> {:label l :content (label-content l) :anchor (coord/outward-anchor h) :x (.x p) :y (.y p)}
        (and edge (m/valid-double? (double edge)))
        (assoc :leader (let [^Vec2 a (coord/to-pixel geometry h (double edge))
                             ^Vec2 b (coord/to-pixel geometry h (m/- r 1.0))]
                         [[(.x a) (.y a)] [(.x b) (.y b)]]))))))

(defn- polar-end-label-placements
  "Gives the [[end-label-placements]] of a polar plot: past the value along the radius (anchored away from the centre) or along the angle (centred), by [[n-label-gap]] pixels; `:outer` labels past the outer radius (see [[outer-placement]]).

  Parameters:

  - `pl` (map): the chart's [[cljplot2.chart.strip/placement]] - `:range-point`, `:geometry`.
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
    (vec (keep (fn [{:keys [level slot at ^double side outer] :as l}]
                 (if outer
                   (outer-placement pl l)
                   (let [dir (m/* up side)
                         [^double h ^double v] (range-point at level (strip/in-slot slot 0.0))
                         shift (m/* dir pad)
                         ^Vec2 p (if radial?
                                   (coord/to-pixel geometry h (m/+ v shift))
                                   (coord/to-pixel geometry (m/+ h shift) v))
                         anchor (if (and radial? (not (m/zero? dir))) (coord/outward-anchor h) [:center :middle])]
                     (when (data/finite-point? p)
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
  - `pl` (map): the chart's [[cljplot2.chart.strip/placement]].
  - `labels` (sequence): `{:level l :slot s :at v :side d :text t}` - past value `v` toward increasing values (`d` 1) or decreasing ones (`d` -1), or centred on `v` (`d` 0, e.g. inside a stacked bar; no room needed); optional `:role` (text role, default `:annotation`), `:color` (overrides the role's); `:outer` - past the outer radius of a polar plot at its segment's middle angle, anchored outward, with `:span [v0 v1]` (its segment's values) and `:leader` (a leader line).
  - `scales` (map): finalized position scales.
  - `env` (map): the drawing environment (`:theme`).

  A label overlapping one drawn before it (in `labels` order) is left out. Returns `nil`."
  [canvas pl labels scales env]
  (draw-label-placements! canvas (without-collisions canvas (end-label-placements pl labels scales) env) env))

(defn outer-label-room
  "Gives the room `:outer` end labels need around a polar plot's circle.

  Parameters:

  - `labels` (sequence): end labels (see [[draw-end-labels!]]); only `:outer` ones count.
  - `env` (map): `:theme`, `:hint` (measuring).

  Returns `[horizontal vertical]` pixels: the widest and the tallest label, measured in its role, plus the gap (16 pixels when some label has a leader, else 6); `nil` without `:outer` labels."
  [labels {:keys [theme hint]}]
  (let [outer (filter :outer labels)]
    (when-let [contents (seq (distinct (map #(label-content (dissoc % :color)) outer)))]
      (measure/call-with-scratch-canvas
       hint
       (fn [c]
         (let [^Vec2 widest (measure/max-size (map #(measure/natural-size % {:canvas c :theme theme}) contents))
               gap (v/mx (map outer-gap outer))]
           [(m/+ gap (.x widest)) (m/+ gap (.y widest))]))))))

(defn n-label-room
  "Gives the `:padding-px` a strip chart's n labels need.

  Parameters:

  - `chart-options` (map): chart options - `:n-label` (default false), `:n-format`.
  - `items` (map): `{panel [item ...]}` (see [[cljplot2.data.strip/split]]).
  - `env` (map): `:theme`, `:hint`, `:flip` (see [[end-label-room]]).

  Returns room past the largest values for the count labels, past the smallest for `:mirrored` items; `nil` without n labels or items."
  [chart-options items env]
  (when (data/option chart-options :n-label false)
    (end-label-room (map (fn [item] {:text (n-label-text chart-options item) :side (n-label-side item)}) (mapcat val items))
                    env)))

(defn draw-n-labels!
  "Draws each item's count just past its largest drawn value, when the chart has n labels.

  [[n-label-room]] keeps room for them past the data.

  Parameters:

  - `canvas`: the canvas.
  - `pl` (map): the chart's [[cljplot2.chart.strip/placement]].
  - `items` (sequence): one panel's items.
  - `top-value` (function): item -> its largest drawn value (the smallest for a `:mirrored` item, which is labelled toward decreasing values).
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

(defn placeable?
  "Tests whether a strip panel's values can be placed: its value scale `:x` (strip frame) exists.

  Parameters:

  - `scales` (map): finalized position scales in the strip frame.

  Returns a truthy value when there is an `:x` scale; `nil` when no chart gave the plot an x extent (no data, or no item with a drawable value) - nothing to draw.

  See also [[placement]], [[draw-oriented]]."
  [scales]
  (some? (:x scales)))

(defn draw-oriented
  "Draws one panel of a strip chart whose draw works in the strip frame.

  The position scales are exchanged when the levels are on `:x` (see [[cljplot2.data.strip/compute-oriented]]); placement and projections take their directions from the scales.

  Parameters:

  - `frame-draw` (function): the chart's draw in the strip frame, of `chart`, `computed`, `canvas`, `box`, `scales`, `env`.
  - `chart` (map): the chart value.
  - `computed` (map): the panel's computed data (with `:cljplot2.data.strip/level-axis`).
  - `canvas`, `box`, `scales`, `env`: as the chart contract's draw.

  Returns what `frame-draw` returns; `nil` without drawing when nothing can be placed (see [[placeable?]])."
  [frame-draw chart computed canvas box scales env]
  (let [frame-scales (if (= :x (::strip/level-axis computed)) (strip/swap-xy scales) scales)]
    (when (placeable? frame-scales)
      (frame-draw chart computed canvas box frame-scales env))))

(defmulti in-frame-compute
  "Computes a strip chart's stats in the strip frame (levels on `:y`, values on `:x`); a method per strip chart kind.

  Parameters:

  - `chart` (map): the chart value, dispatched on its `:kind`.
  - `chart-split` (map): `{:groups {panel rows} :mapping m}` in the strip frame.
  - `env` (map): the compute environment in the strip frame.

  Returns the chart's stats (see the chart contract in [[cljplot2.chart]]).

  Throws `No method in multimethod` for a kind without a method.

  See also [[compute]]."
  (fn [chart _chart-split _env] (:kind chart)))

(defmulti in-frame-draw
  "Draws one panel of a strip chart in the strip frame; a method per strip chart kind.

  Parameters:

  - `chart` (map): the chart value, dispatched on its `:kind`.
  - `computed` (map): the panel's computed data.
  - `canvas`, `box`: as the chart contract's draw.
  - `scales` (map): the panel's position scales in the strip frame.
  - `env` (map): the drawing environment.

  Drawn for its effect on the canvas (the result is unspecified); throws `No method in multimethod` for a kind without a method.

  See also [[draw]]."
  (fn [chart _computed _canvas _box _scales _env] (:kind chart)))

(defn compute
  "Computes a strip chart's stats with its levels on the categorical axis: the chart contract's compute of every strip chart.

  Parameters:

  - `chart` (map): the chart value; its kind has an [[in-frame-compute]] method.
  - `chart-split` (map): `{:groups {panel rows} :mapping m}`.
  - `env` (map): the compute environment.

  Returns the stats of [[in-frame-compute]] turned to the mapping axis of the levels (see [[cljplot2.data.strip/compute-oriented]])."
  [chart chart-split env]
  (strip/compute-oriented in-frame-compute chart chart-split env))

(defn draw
  "Draws one panel of a strip chart: the chart contract's draw of every strip chart.

  Parameters:

  - `chart` (map): the chart value; its kind has an [[in-frame-draw]] method.
  - `computed` (map): the panel's computed data (from [[compute]]).
  - `canvas`, `box`, `scales`, `env`: as the chart contract's draw.

  Draws [[in-frame-draw]] in the strip frame (see [[cljplot2.chart.strip/draw-oriented]]); `nil` when nothing can be placed."
  [chart computed canvas box scales env]
  (draw-oriented in-frame-draw chart computed canvas box scales env))
