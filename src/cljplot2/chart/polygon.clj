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

  Equivalents: ggplot2 `geom_polygon`, Vega-Lite `line` with a closed path filled.

  Functions: [[schemas]] - the chart's schemas; [[compute]] - the panels' polygons; [[draw]] - one panel's polygons;
  [[legend]] - the key glyph."
  (:require [clojure2d.color :as c]
            [fastmath.core :as m]
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
  "Map entries of the polygon style (all optional; defaults from the theme)."
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
                (into theme/theme-style-entries)
                (conj (schema/optional :padding {:description "padding of the data span per position axis, {:x [lo hi] :y [lo hi]} fractions of the span"
                                                 :cljplot2/default-doc "0.05 on every side"}
                                       data/position-padding)))
   :mapping [:map
             [:x {:description "x position of a vertex (data units)"} schema/accessor]
             [:y {:description "y position of a vertex (data units)"} schema/accessor]
             (schema/nilable :color {:description "fill colour: per group (ordinal), or by the group's first row (linear or binned)"}
                             schema/accessor)
             (schema/nilable :group {:description "groups without a visual aesthetic, each its own polygon"} schema/accessor)]})

(def ^{:private true :const true :tag 'long} min-points
  "Fewest points a drawn polygon has."
  3)

(defn compute
  "Computes the polygons of every panel.

  Parameters:

  - `chart` (map): the chart value - `:options` (see the namespace doc).
  - `split` (map): `:groups` - panel -> rows, `:mapping` - the merged mapping.
  - `env` (map): `:theme`, `:plot-scales` - the plot's `:scales` (and the grouping keys of [[cljplot2.data.group/split]]).

  Returns `{:data :extents :padding :padding-explicit :na :dropped}`: per panel `{:polygons [{:x [..] :y [..] :aesthetics}] :style st}` - the groups with at least 3 valid points, a polygon filled along a linear / binned `:color` also with `:color-value` (its first row's value, `nil` for NA), `st` the resolved style; `:dropped` - `:x`, `:y` (invalid values), `:group` (polygons under 3 points), `:color`.

  Throws `ex-info {:key :mapping}` when `:x` or `:y` has no valid value in non-empty data."
  [{:keys [options] :as chart} {:keys [groups mapping]} {:keys [theme plot-scales] :as env}]
  (let [st (theme/resolved-style theme chart)
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
        drawn? (fn [g] (m/>= (count (filter valid? (:rows g))) min-points))
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
     :padding (merge data/default-padding (:padding options))
     :padding-explicit (data/explicit-padding {} options)
     :na (cond-> (group/na-dims panels) (:na? color-stats) (conj :color))
     :dropped (cond-> (assoc dropped :x (invalid x-of) :y (invalid y-of)
                             :group (count (remove drawn? (mapcat val split-panels))))
                continuous (assoc :color (:dropped color-stats)))}))

(defn- fill-color
  "Gives the fill of a polygon.

  Parameters:

  - `st` (map): the polygon's style (group attributes merged) - `:color`, `:alpha` (default 1).

  Returns the colour with its alpha multiplied by `:alpha`."
  [{:keys [color ^double alpha] :or {alpha 1.0}}]
  (c/set-alpha color (m/* alpha (c/alpha color))))

(defn- outline
  "Gives the line style of a polygon's outline.

  Parameters:

  - `st` (map): the polygon's style - `:stroke-color` (`:color` - the group colour; default `:none`), `:stroke-width` (default 1).

  Returns `{:color :width}`; `nil` for no outline (`:none`)."
  [st]
  (let [color (if (= :color (:stroke-color st)) (:color st) (:stroke-color st :none))]
    (when-not (theme/none? color)
      {:color color :width (:stroke-width st 1)})))

(defn- value-style
  "Colours a polygon's style by its continuous colour value.

  Parameters:

  - `st` (map): the group style.
  - `value`: the polygon's linear / binned colour value; `nil` - NA.
  - `scales` (map): the finalized scales - `:color`.
  - `theme` (map): the merged theme - `[:palette :na]`.

  Returns `st` with `:color` the value's colour under the colour scale (the NA colour for `nil` or without a scale); `nil` for a value outside the scale's domain."
  [st value scales theme]
  (let [na (get-in theme [:palette :na])]
    (if (nil? value)
      (assoc st :color na)
      (when-let [col (if-let [sc (:color scales)] ((aesthetic/attribute-fn :color sc na) value) na)]
        (assoc st :color col)))))

(defn draw
  "Draws the polygons of one panel, each filled then outlined, in order.

  Parameters:

  - `computed` (map): `:polygons`, `:style` (see [[compute]]).
  - `canvas`: the clojure2d canvas.
  - `scales` (map): the finalized scales.
  - `env` (map): `:theme`.

  Points the scales can't place are skipped; a polygon left with fewer than 3 points, or a group whose aesthetic value is outside its scale, isn't drawn."
  [_chart {:keys [polygons style]} canvas _box scales env]
  (let [projection (scale/projection scales)
        theme (:theme env)]
    (doseq [{:keys [x y aesthetics color-value] :as p} polygons
            :let [group-st (group/style style aesthetics scales theme)
                  st (if (and group-st (contains? p :color-value)) (value-style group-st color-value scales theme) group-st)]
            :when st
            :let [points (into [] (keep (fn [[vx vy]]
                                          (let [^Vec2 q (projection vx vy)]
                                            (when (and (m/valid-double? (.x q)) (m/valid-double? (.y q)))
                                              (curve/drawn-point [(.x q) (.y q)])))))
                               (map vector x y))]
            :when (m/>= (count points) min-points)]
      (polyline/fill-polygons! canvas [points] (fill-color st) nil)
      (when-let [line (outline st)]
        (polyline/stroke-runs! canvas [(conj points (first points))] line)))))

(defn legend
  "Draws the chart's key glyph: a filled square in the polygon fill, outlined like the polygons.

  Parameters:

  - `chart` (map): the chart value.
  - `attributes` (map): the key's mapped attributes - `:color`.
  - `canvas`: the clojure2d canvas.
  - `box` (map): the key's box.
  - `env` (map): `:theme`."
  [chart attributes canvas box env]
  (let [st (merge (theme/resolved-style (:theme env) chart) (select-keys attributes [:color]))]
    (polyline/key-glyph! canvas box (or (outline st) {:color :none :width 0}) (fill-color st))))
