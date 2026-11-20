(ns cljplot2.chart.polygon
  "Polygon: the points `(x, y)` of a group joined in row order into a closed, filled polygon, per panel and group.

  Options (style defaults from theme `[:chart :polygon]`): `:color` (the fill), `:alpha` (its opacity, 0..1, default
  1), `:stroke-color` (the outline: default `:none`; `:color` - the group colour, opaque) with `:stroke-width`.
  Groups (see [[cljplot2.data.group]]) come from ordinal aesthetics (`:color`) and `:group`; a linear or binned
  `:color` fills each polygon by the value of its group's first row (NA - the theme NA colour; rows outside the colour
  scale's explicit domain dropped). A row with an invalid x or y is dropped (counted per key); a polygon with fewer
  than 3 points is not drawn (counted in `:dropped {:group n}`, as violin and density count groups without a mark;
  its aesthetic values don't reach the legend). Cartesian plots only (the pipeline rejects the chart
  under `:coord :polar`).
  `:padding {:x [lo hi] :y [lo hi]}` (default 5% each side).

  Equivalents: ggplot2 `geom_polygon`, Vega-Lite `line` with a closed path filled."
  (:require [clojure2d.color :as c]
            [cljplot2.data.group :as group]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.curve :as curve]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries
  [(schema/optional :color {:description "fill colour (a mapped :color wins)"} schema/color)
   (schema/optional :alpha {:description "fill opacity"} schema/fraction)
   (schema/optional :stroke-color {:description "outline colour: a colour, :none or :color (the group colour, opaque)"}
                    [:or [:= :color] schema/color])
   (schema/optional :stroke-width {:description "outline width in pixels"} schema/non-neg)])

(def schemas
  "Schemas of the polygon chart: `:style` (theme `[:chart :polygon]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (conj (schema/optional :padding {:description "padding of the data span per position axis, {:x [lo hi] :y [lo hi]} fractions of the span"
                                                 :cljplot2/default-doc "0.05 on every side"}
                                       data/position-padding)))
   :mapping [:map
             [:x {:description "x position of a vertex (data units)"} schema/accessor]
             [:y {:description "y position of a vertex (data units)"} schema/accessor]
             (schema/nilable :color {:description "fill colour: per group (ordinal), or by the group's first row (linear or binned)"}
                             schema/accessor)
             (schema/nilable :group {:description "groups the rows into separate polygons"} schema/accessor)]})

(def ^:private ^:const min-points 3)

(defn- style
  [options theme]
  (merge (get-in theme [:chart :polygon]) (select-keys options [:color :alpha :stroke-color :stroke-width])))

(defn compute
  "Per panel: `{:polygons [{:x [..] :y [..] :aesthetics}] :style style}` (polygons of at least 3 valid points; a
  polygon filled along a linear / binned `:color` also has `:color-value` - its first row's value, `nil` for NA);
  extents, padding, `:na`, `:dropped` (`:x`, `:y`, `:group` - polygons under 3 points, `:color`)."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme plot-scales] :as env}]
  (let [st (style options theme)
        continuous (group/continuous-color groups mapping env)
        color-of (:color-of continuous)
        {split-panels :panels dropped :dropped} (if continuous
                                                  (group/split (:groups continuous) (:mapping continuous) env)
                                                  (group/split groups mapping env))
        x-of (data/row-accessor :x (:x mapping))
        y-of (data/row-accessor :y (:y mapping))
        valid? (fn [r] (and (data/valid-value? (x-of r)) (data/valid-value? (y-of r))))
        rows (mapcat (fn [gs] (mapcat :rows gs)) (vals split-panels))
        _ (doseq [[k f] [[:x x-of] [:y y-of]]]
            (when (and (seq rows) (not-any? #(data/valid-value? (f %)) rows))
              (schema/fail! "No valid values for a mapped dimension - check the mapping." {:key :mapping :value k :rows (count rows)})))
        polygon (fn [g] (let [rs (filterv valid? (:rows g))]
                          (cond-> {:x (mapv x-of rs) :y (mapv y-of rs) :aesthetics (:aesthetics g)}
                            continuous (assoc :color-value (some-> (first (:rows g)) color-of)))))
        drawn? (fn [g] (>= (count (filter valid? (:rows g))) min-points))
        panels (update-vals split-panels #(filterv drawn? %))
        data (update-vals panels (fn [gs] {:polygons (mapv polygon gs) :style st}))
        color-stats (when continuous
                      (group/continuous-color-stats continuous (update-vals data #(map :color-value (:polygons %))) plot-scales))
        invalid (fn [f] (count (remove #(data/valid-value? (f %)) rows)))]
    {:data data
     :extents (merge-with merge (group/aesthetic-extents panels plot-scales) (:extents color-stats)
                          (into {} (keep (fn [[panel {:keys [polygons]}]]
                                           (when (seq polygons)
                                             [panel {:x (data/extent :x (mapcat :x polygons))
                                                     :y (data/extent :y (mapcat :y polygons))}])))
                                data))
     :padding (merge {:x [0.05 0.05] :y [0.05 0.05]} (:padding options))
     :padding-explicit (data/explicit-padding {} options)
     :na (cond-> (group/na-dims panels) (:na? color-stats) (conj :color))
     :dropped (cond-> (assoc dropped :x (invalid x-of) :y (invalid y-of)
                             :group (count (remove drawn? (mapcat val split-panels))))
                continuous (assoc :color (:dropped color-stats)))}))

(defn- fill-color
  "Fill of a polygon with style `st` (group attributes merged): its colour at `:alpha`."
  [st]
  (let [col (c/to-color (:color st))]
    (c/set-alpha col (* (double (:alpha st 1.0)) (double (c/alpha col))))))

(defn- outline
  "Line style of a polygon's outline with style `st`: `:stroke-color` (`:color` - the group colour) at
  `:stroke-width`; `nil` without one."
  [st]
  (let [color (if (= :color (:stroke-color st)) (:color st) (:stroke-color st :none))]
    (when-not (theme/none? color)
      {:color color :width (:stroke-width st 1)})))

(defn- value-style
  "Group style `st` with the colour of a linear / binned `value` under the finalized colour scale (NA - the theme NA
  colour); `nil` outside the domain."
  [st value scales theme]
  (let [na (get-in theme [:palette :na])]
    (if (nil? value)
      (assoc st :color na)
      (when-let [col (if-let [sc (:color scales)] ((aesthetic/attribute-fn :color sc na) (double value)) na)]
        (assoc st :color col)))))

(defn draw
  "Fill the polygons of one panel in order, each then outlined; points the scales can't place are skipped (a polygon
  left with fewer than 3 points isn't drawn)."
  [_chart {:keys [polygons style]} canvas _box scales env]
  (let [projection (scale/projection scales)
        theme (:theme env)]
    (doseq [{:keys [x y aesthetics color-value] :as p} polygons
            :let [st (group/style style aesthetics scales theme)
                  st (if (and st (contains? p :color-value)) (value-style st color-value scales theme) st)]
            :when st
            :let [points (into [] (keep (fn [[vx vy]]
                                          (let [^Vec2 q (projection vx vy)]
                                            (when (and (Double/isFinite (.x q)) (Double/isFinite (.y q)))
                                              (curve/drawn-point [(.x q) (.y q)])))))
                               (map vector x y))]
            :when (>= (count points) min-points)]
      (polyline/fill-polygons! canvas [points] (fill-color st) nil)
      (when-let [line (outline st)]
        (polyline/stroke-runs! canvas [(conj points (first points))] line)))))

(defn legend
  "Key glyph: a filled square in the polygon fill, outlined like the polygons."
  [{:keys [options]} attributes canvas box env]
  (let [st (merge (style options (:theme env)) (select-keys attributes [:color]))]
    (polyline/key-glyph! canvas box (or (outline st) {:color :none :width 0}) (fill-color st))))
