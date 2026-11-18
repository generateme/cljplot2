(ns cljplot2.chart.contour
  "Contour: lines (or filled regions) where a 2D field crosses levels - a kernel density of points, an interpolated
  gridded `:value` or a function of `x`, `y` (see [[cljplot2.data.field]] for the sources and their options), sampled
  on a grid over the domain of every panel together (cartesian plots only; marching squares, see
  [[cljplot2.data.contour]]). Vertices are projected one by one: lines follow non-linear scales.

  Options (style defaults from theme `[:chart :contour]`):

  * `:levels` - an integer: that many levels evenly spread over the range of every sampled value (the ends left
    out; default 10), or a vector of level values
  * `:fill` - `true`: fill the region at or above each level (lowest first, so the bands stack; holes stay open)
    instead of drawing lines; each band colored by its middle value (between its level and the next, the top band up
    to the largest value); grouped filled contours are translucent by default (alpha × 0.5), the groups overlap
  * colors: by default each level its color through the plot's color scale (a computed color, legend titled like
    [[cljplot2.chart.field]]); option `:color` - one color for every line; a grouping mapping (`:color` with
    categorical values, `:group`, other ordinal aesthetics) - a field per group (points / gridded data of the group)
    with its lines in the group's style, levels shared by the groups
  * `:width`, `:dash`, `:alpha` (multiplies the colors' alpha, a fraction)
  * `:labels` (line contours only; default none) - `true`: each line labelled with its level (a number to at most 2
    decimals, see [[cljplot2.content.label/value-text]]), or a function of `{:level v}` returning the text (`nil` - no
    label, e.g. every second level): horizontal text centred halfway along the line (by its pixel length), text role
    `:contour-label` (a translucent background over the line, which isn't cut); a line shorter than twice its label
    gets none, a label leaving the panel or overlapping one already drawn is skipped, lines not drawn (no color) none (lowest levels first); labels are drawn after all
    lines. With `:fill true` - `ex-info {:key :labels}`.

  On a `:log` axis the field is sampled (a density estimated) in log10 coordinates, see [[cljplot2.data.field]]."
  (:require [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.chart.field :as field-chart]
            [cljplot2.content.label :as label]
            [cljplot2.data.contour :as contour]
            [cljplot2.data.field :as field]
            [cljplot2.data.group :as group]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries
  (conj polyline/style-entries (schema/optional :alpha schema/fraction)))

(def schemas
  "Schemas of the contour chart: `:style` (theme `[:chart :contour]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (schema/with-rules
             (-> [:map {:closed true}]
                 (into style-entries)
                 (into field/options)
                 (into data/common-options)
                 (conj (schema/optional :levels [:or [:int {:min 1 :max 1000}] [:vector {:min 1} schema/finite]])
                       (schema/optional :fill :boolean)
                       (schema/nilable :labels [:or :boolean schema/function])))
             [#(when (and (:labels %) (:fill %)) "labels need line contours, not :fill true") [:labels]])
   :mapping [:maybe [:map]]})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :contour]) (select-keys options [:color :width :dash :alpha])))

(def ^:private ^:const grouped-fill-alpha
  "Alpha factor of grouped filled contours without an `:alpha` option: the groups' regions overlap."
  0.5)

(defn- grouped?
  "Do the chart's merged `mapping` group it (a mapped aesthetic or `:group`)?"
  [mapping]
  (boolean (seq (group/group-dims mapping))))

(defn computed-aesthetics
  "The contour chart's computed color - by level - unless a fixed `:color` option or a grouping mapping colors it."
  [options mapping]
  (if (or (:color options) (grouped? mapping))
    {}
    {:color (field-chart/value-title options mapping)}))

(defn- group-fields
  "`[[panel aesthetics field] ..]` of the split `groups`: a field per panel, or per group in a panel when grouped."
  [options mapping groups env]
  (let [kind (field/source-kind options mapping)
        plot-scales (:plot-scales env)]
    (if (and (grouped? mapping) (not= kind :function))
      (let [{:keys [panels]} (group/split groups mapping env)]
        (for [[panel gs] panels {:keys [aesthetics rows]} gs
              :let [f (field/source-field kind options mapping rows plot-scales)] :when f]
          [panel aesthetics f]))
      (for [[panel rows] groups
            :let [f (field/source-field kind options mapping rows plot-scales)] :when f]
        [panel {} f]))))

(defn compute
  "Per panel: `{:contours [{:level l :aesthetics a :lines [{:points :closed?} ..] :rings [[..] ..]} ..] :style st
  :fill? b :by-level? b}`; extents x / y - the sampled domain, color - the levels (lines colored by level), the value
  range (filled bands), or the groups' values; no padding, `:na`, `:dropped`."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme plot-scales] :as env}]
  (let [fields (group-fields options mapping groups env)
        domain (field/union-domain (map (comp :domain #(nth % 2)) fields))
        size (field/grid-size (:n options))
        transforms (field/axis-transforms plot-scales)
        sampled (mapv (fn [[panel aesthetics {:keys [f]}]] [panel aesthetics (field/sample f domain size transforms)]) fields)
        [lo hi :as value-range] (field/value-range (map #(nth % 2) sampled))
        levels (contour/levels value-range (get options :levels 10))
        fill? (boolean (:fill options))
        by-level? (not (or (:color options) (grouped? mapping)))
        ;; a filled band's color - the middle of its level and the next (the top band: up to the largest value)
        color-values (if (and fill? (seq levels))
                       (mapv (fn [l nxt] (* 0.5 (+ (double l) (double nxt)))) levels (concat (rest levels) [(m/max (double hi) (double (peek levels)))]))
                       levels)
        in-data (fn [grid points] (mapv #(field/to-data grid %) points))
        panels (reduce (fn [acc [panel aesthetics grid]]
                         (update acc panel (fnil into [])
                                 (for [[level color-value] (map vector levels color-values)]
                                   (if fill?
                                     {:level level :color-value color-value :aesthetics aesthetics
                                      :rings (mapv #(in-data grid %) (contour/level-rings grid level))}
                                     {:level level :color-value color-value :aesthetics aesthetics
                                      :lines (mapv #(update % :points (partial in-data grid)) (contour/isolines grid level))}))))
                       {} sampled)
        st (cond-> (style options theme)
             (and fill? (grouped? mapping) (nil? (:alpha options))) (update :alpha #(* grouped-fill-alpha (double (or % 1.0)))))
        [[x0 x1] [y0 y1]] domain
        group-extents (when (grouped? mapping)
                        (group/aesthetic-extents (update-vals (group-by first fields)
                                                              (fn [fs] (mapv (fn [[_ a]] {:aesthetics a}) fs)))
                                                 plot-scales))]
    {:data (update-vals panels (fn [cs] {:contours cs :style st :fill? fill? :by-level? by-level? :labels (:labels options)}))
     :extents (into {} (map (fn [panel]
                              [panel (cond-> {:x {:kind :numeric :min (double x0) :max (double x1)}
                                              :y {:kind :numeric :min (double y0) :max (double y1)}}
                                       (and by-level? (seq levels))
                                       (assoc :color (if fill?
                                                       ;; bands over the whole value range: the lowest band isn't the gradient's end
                                                       {:kind :numeric :min (double lo) :max (double hi)}
                                                       {:kind :numeric :min (double (first levels)) :max (double (peek levels))}))
                                       group-extents (merge (get group-extents panel)))]))
                    (keys panels))
     :padding {:x [0.0 0.0] :y [0.0 0.0]}
     :na (if (some (fn [[_ a]] (some nil? (vals a))) fields) (set (for [[_ a] fields [k v] a :when (nil? v)] k)) #{})
     :dropped (field/dropped-positions (field/source-kind options mapping) mapping groups)}))

(defn- level-style
  "Line / fill style of a contour: by its color value (the level, or a filled band's middle) through the color
  scale, else the group's style over the chart's."
  [{:keys [color-value aesthetics]} st scales env by-level?]
  (if by-level?
    (when-let [col (aesthetic/attribute-fn :color (:color scales) (get-in env [:theme :palette :na]))]
      (some->> (col color-value) (assoc st :color)))
    (group/style st aesthetics scales (:theme env))))

(defn- with-alpha
  [color alpha]
  (if alpha (c/set-alpha (c/to-color color) (* (double alpha) (double (c/alpha (c/to-color color))))) color))

(defn line-midpoint
  "`[point length]` of the pixel polyline `points` (`[x y]` pairs): the point halfway along it and its total length;
  `nil` for fewer than two points or a non-finite one."
  [points]
  (when (and (>= (count points) 2) (every? (fn [[x y]] (and (m/valid-double? (double x)) (m/valid-double? (double y)))) points))
    (let [segments (mapv (fn [[x0 y0] [x1 y1]] (m/hypot (- (double x1) (double x0)) (- (double y1) (double y0))))
                         points (rest points))
          total (double (reduce + 0.0 segments))
          half (* 0.5 total)]
      (loop [i 0 walked 0.0]
        (let [len (double (segments i))
              [x0 y0] (points i) [x1 y1] (points (inc i))]
          (if (or (>= (+ walked len) half) (= i (dec (count segments))))
            (let [t (if (pos? len) (m/constrain (/ (- half walked) len) 0.0 1.0) 0.0)]
              [[(+ (double x0) (* t (- (double x1) (double x0)))) (+ (double y0) (* t (- (double y1) (double y0))))] total])
            (recur (inc i) (+ walked len))))))))

(defn- level-text
  "Label text of `level` for the `:labels` option `labels` (`true` or a function of `{:level v}`), `nil` - none."
  [labels level]
  (if (fn? labels) (some-> (labels {:level level}) str) (label/value-text level)))

(defn- boxes-overlap?
  [{ax :x ay :y aw :w ah :h} {bx :x by :y bw :w bh :h}]
  (and (< (double ax) (+ (double bx) (double bw))) (< (double bx) (+ (double ax) (double aw)))
       (< (double ay) (+ (double by) (double bh))) (< (double by) (+ (double ay) (double ah)))))

(defn- inside?
  "Is pixel box `b` within the panel box `{:w :h}` (at the origin)?"
  [{:keys [x y w h]} panel]
  (and (>= (double x) 0.0) (>= (double y) 0.0)
       (<= (+ (double x) (double w)) (double (:w panel))) (<= (+ (double y) (double h)) (double (:h panel)))))

(defn- draw-labels!
  "Level labels of the line `contours` (pixel points by `to-pixels`), lowest level first: centred halfway along each
  line long enough (twice the label width), skipped where they'd leave the `panel` box or overlap a label already
  drawn."
  [canvas contours to-pixels labels {:keys [panel] :as env}]
  (reduce (fn [drawn {:keys [level lines]}]
            (if-let [text (level-text labels level)]
              (let [content {:kind :label :role :contour-label :text text}]
                (reduce (fn [drawn {:keys [points closed?]}]
                          (if-let [[[x y] length] (line-midpoint (to-pixels (if closed? (conj points (first points)) points)))]
                            (let [box (label/box-at canvas content [:center :middle] x y env)]
                              (if (or (< (double length) (* 2.0 (double (:w box)))) (not (inside? box panel))
                                      (some #(boxes-overlap? box %) drawn))
                                drawn
                                (do (label/draw-at! canvas content [:center :middle] x y env)
                                    (conj drawn box))))
                            drawn))
                        drawn lines))
              drawn))
          [] (sort-by :level contours)))

(defn draw
  "Draw one panel's contours: filled regions lowest level first, or lines (then their `:labels`)."
  [_chart {:keys [contours style fill? by-level? labels]} canvas box scales env]
  (let [projection (scale/projection scales)
        to-pixels (fn [points] (mapv (fn [[x y]] (let [^fastmath.vector.Vec2 p (projection x y)] [(.x p) (.y p)])) points))
        alpha (:alpha style)
        ;; contours with a drawn color, lowest level first: only their lines get labels
        visible (keep (fn [contour]
                        (let [st (level-style contour style scales env by-level?)]
                          (when (and st (not (theme/none? (:color st)))) [contour st])))
                      (sort-by :level contours))]
    (doseq [[contour st] visible]
      (if fill?
        (do (c2d/set-color canvas (with-alpha (:color st) alpha))
            (c2d/shape canvas (polyline/rings-path (map to-pixels (:rings contour))) false))
        (polyline/stroke-runs! canvas
                               (map (fn [{:keys [points closed?]}] (to-pixels (if closed? (conj points (first points)) points)))
                                    (:lines contour))
                               (assoc st :color (with-alpha (:color st) alpha)))))
    (when (and labels (not fill?))
      (draw-labels! canvas (map first visible) to-pixels labels (assoc env :panel box)))))

(defn legend
  "Key glyph: a line in the key's color and the chart's line style; filled contours - a filled square."
  [{:keys [options]} attributes canvas box env]
  (let [st (merge (style options (:theme env)) (select-keys attributes [:color :dash]))]
    (if (:fill options)
      (draw/bar-key! canvas box {:color (:color st) :stroke-width 0} false)
      (polyline/key-glyph! canvas box st :none))))
