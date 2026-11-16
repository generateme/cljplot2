(ns cljplot2.chart.contour
  "Contour: lines (or filled regions) where a 2D field crosses levels - a kernel density of points, an interpolated
  gridded `:value` or a function of `x`, `y` (see [[cljplot2.data.field]] for the sources and their options), sampled
  on a grid over the domain of every panel together (cartesian plots only; marching squares, see
  [[cljplot2.data.contour]]). Vertices are projected one by one: lines follow non-linear scales.

  Options (style defaults from theme `[:chart :contour]`):

  * `:levels` - an integer: that many levels evenly spread over the range of every sampled value (the ends left
    out; default 10), or a vector of level values
  * `:fill` - `true`: fill the region at or above each level (lowest first, so the bands stack; holes stay open)
    instead of drawing lines
  * colors: by default each level its color through the plot's color scale (a computed color, legend titled like
    [[cljplot2.chart.field]]); option `:color` - one color for every line; a grouping mapping (`:color` with
    categorical values, `:group`, other ordinal aesthetics) - a field per group (points / gridded data of the group)
    with its lines in the group's style, levels shared by the groups
  * `:width`, `:dash`, `:alpha` (multiplies the colors' alpha, a fraction)"
  (:require [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.chart.field :as field-chart]
            [cljplot2.data.contour :as contour]
            [cljplot2.data.field :as field]
            [cljplot2.data.group :as group]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [java.awt.geom Path2D Path2D$Double]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries
  (conj polyline/style-entries (schema/optional :alpha schema/fraction)))

(def schemas
  "Schemas of the contour chart: `:style` (theme `[:chart :contour]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into field/options)
                (into data/common-options)
                (conj (schema/optional :levels [:or [:int {:min 1 :max 1000}] [:vector {:min 1} schema/finite]])
                      (schema/optional :fill :boolean)))
   :mapping [:maybe [:map]]})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :contour]) (select-keys options [:color :width :dash :alpha])))

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
  :fill? b :by-level? b}`; extents x / y - the sampled domain, color - the levels (by-level coloring) or the groups'
  values; no padding, `:na`, `:dropped`."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme plot-scales] :as env}]
  (let [fields (group-fields options mapping groups env)
        domain (field/union-domain (map (comp :domain #(nth % 2)) fields))
        size (field/grid-size (:n options))
        sampled (mapv (fn [[panel aesthetics {:keys [f]}]] [panel aesthetics (field/sample f domain size)]) fields)
        levels (contour/levels (field/value-range (map #(nth % 2) sampled)) (get options :levels 10))
        fill? (boolean (:fill options))
        by-level? (not (or (:color options) (grouped? mapping)))
        panels (reduce (fn [acc [panel aesthetics grid]]
                         (update acc panel (fnil into [])
                                 (for [level levels]
                                   (if fill?
                                     {:level level :aesthetics aesthetics :rings (contour/level-rings grid level)}
                                     {:level level :aesthetics aesthetics :lines (contour/isolines grid level)}))))
                       {} sampled)
        st (style options theme)
        [[x0 x1] [y0 y1]] domain
        group-extents (when (grouped? mapping)
                        (group/aesthetic-extents (update-vals (group-by first fields)
                                                              (fn [fs] (mapv (fn [[_ a]] {:aesthetics a}) fs)))
                                                 plot-scales))]
    {:data (update-vals panels (fn [cs] {:contours cs :style st :fill? fill? :by-level? by-level?}))
     :extents (into {} (map (fn [panel]
                              [panel (cond-> {:x {:kind :numeric :min (double x0) :max (double x1)}
                                              :y {:kind :numeric :min (double y0) :max (double y1)}}
                                       (and by-level? (seq levels)) (assoc :color {:kind :numeric :min (double (first levels))
                                                                                   :max (double (peek levels))})
                                       group-extents (merge (get group-extents panel)))]))
                    (keys panels))
     :padding {:x [0.0 0.0] :y [0.0 0.0]}
     :na (if (some (fn [[_ a]] (some nil? (vals a))) fields) (set (for [[_ a] fields [k v] a :when (nil? v)] k)) #{})
     :dropped (field/dropped-positions (field/source-kind options mapping) mapping groups)}))

(defn- level-style
  "Line / fill style of a contour: by level through the color scale, else the group's style over the chart's."
  [{:keys [level aesthetics]} st scales env by-level?]
  (if by-level?
    (when-let [col (aesthetic/attribute-fn :color (:color scales) (get-in env [:theme :palette :na]))]
      (some->> (col level) (assoc st :color)))
    (group/style st aesthetics scales (:theme env))))

(defn- with-alpha
  [color alpha]
  (if alpha (c/set-alpha (c/to-color color) (* (double alpha) (double (c/alpha (c/to-color color))))) color))

(defn- rings-shape
  "Even-odd `Path2D` of the pixel `rings`."
  ^Path2D [rings]
  (let [path (Path2D$Double. Path2D/WIND_EVEN_ODD)]
    (doseq [ring rings :when (> (count ring) 2)]
      (let [[[x y] & more] ring]
        (.moveTo path (double x) (double y))
        (doseq [[x y] more] (.lineTo path (double x) (double y)))
        (.closePath path)))
    path))

(defn draw
  "Draw one panel's contours: filled regions lowest level first, or lines."
  [_chart {:keys [contours style fill? by-level?]} canvas _box scales env]
  (let [projection (scale/projection scales)
        to-pixels (fn [points] (mapv (fn [[x y]] (let [^fastmath.vector.Vec2 p (projection x y)] [(.x p) (.y p)])) points))
        alpha (:alpha style)]
    (doseq [contour (sort-by :level contours)
            :let [st (level-style contour style scales env by-level?)]
            :when (and st (not (theme/none? (:color st))))]
      (if fill?
        (do (c2d/set-color canvas (with-alpha (:color st) alpha))
            (c2d/shape canvas (rings-shape (map to-pixels (:rings contour))) false))
        (polyline/stroke-runs! canvas
                               (map (fn [{:keys [points closed?]}] (to-pixels (if closed? (conj points (first points)) points)))
                                    (:lines contour))
                               (assoc st :color (with-alpha (:color st) alpha)))))))

(defn legend
  "Key glyph: a line in the key's color and the chart's line style; filled contours - a filled square."
  [{:keys [options]} attributes canvas box env]
  (let [st (merge (style options (:theme env)) (select-keys attributes [:color :dash]))]
    (if (:fill options)
      (draw/bar-key! canvas box {:color (:color st) :stroke-width 0} false)
      (polyline/key-glyph! canvas box st :none))))
