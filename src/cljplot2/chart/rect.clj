(ns cljplot2.chart.rect
  "Rect: rectangles and bands - an annotation chart (see [[cljplot2.data.annotation]]: literal options or data through
  the chart's own `:mapping`, `:extent`). Drawn in chart order: list it before the charts it should stay behind.

  * `:x [a b]` - a band over the x interval, across the whole panel
  * `:y [a b]` - a band over the y interval
  * both - a rectangle

  Literal: an interval `[a b]` or a vector of them; with both `:x` and `:y`, vectors of 1 or n intervals are paired;
  `:fill` - a color, or a vector of colors paired with the intervals the same way (1 or n; a vector that is itself a
  color, e.g. `[255 0 0]`, is one color). An end is a position (a level covers its band step) or `nil` - the panel
  edge on that side (`[5 nil]` - from 5 to the high end). Data: `:x0 :x1 :y0 :y1` in the chart's own `:mapping` (an
  `x` pair, a `y` pair or both), one rect per row (each pair mapped whole, else `ex-info {:key :mapping}`); a `nil`
  value is the panel edge, other invalid values skip the row. Aesthetics in the chart's own mapping group the rects
  (as for `chart/rule`): a mapped `:color` is the fill (any color scale kind, e.g. identity colors for swatches) and
  gives legend keys; `:shape` / `:size` only group.

  Options (style defaults from theme `[:chart :rect]`): `:fill`, `:stroke-color` (a color, `:none` or `:darker` -
  the darkened fill, none without a fill), `:stroke-width`, `:alpha` (the fill's alpha, a fraction; default - colors
  as they are), `:corner-radius` (pixels, every corner rounded, clamped to half the shorter side; polar - the annular
  sector's corners; default 0), `:width` (the fraction of its band step a level end covers, centred, `(0 1]`;
  default 1 - the whole step, rects of neighbouring levels touch), `:extent`, plus `:data` / `:mapping`.

  Functions: [[compute]], [[draw]], [[legend]] - the chart contract."
  (:require [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.data.annotation :as annotation]
            [cljplot2.data.group :as group]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private transparent
  "Fully transparent colour: the fill of an unfilled rect drawn through the bar functions (a `:none` fill draws only the outline)."
  (c/color 0 0 0 0))

(def ^:private style-entries
  "Map entries of the rect style (theme `[:chart :rect]`); `:fill` as an option also takes a vector of colours."
  (into [(schema/optional :fill {:description "rect fill colour (a mapped :color wins)"} schema/color)
         (schema/optional :stroke-color {:description "outline colour: a colour, :none or :darker (the darkened fill; no outline without a fill)"}
                          draw/stroke-color)
         (schema/optional :stroke-width {:description "outline width in pixels"} schema/non-neg)]
        draw/pattern-style-entries))

(def ^:private end-schema
  "Schema of an interval end: a position or `nil` - the panel edge."
  [:maybe annotation/position])

(def ^:private interval-schema
  "Schema of an interval `[a b]`."
  [:tuple end-schema end-schema])

(def ^:private fill-option
  "A color, or a non-empty vector of colors (literal rects)."
  [:multi {:dispatch schema/color?} [true schema/color] [false [:vector {:min 1} schema/color]]])

(def schemas
  "Schemas of the rect chart: `:style` (theme `[:chart :rect]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into (rest style-entries))
                (into data/common-options)
                (into theme/theme-style-entries)
                (into annotation/options)
                (conj (schema/optional :fill {:description "fill colour, or a vector of 1 or n colours paired with literal intervals (a mapped :color wins)"}
                                       fill-option)
                      (schema/optional :alpha {:description "fill opacity"
                                               :cljplot2/default-doc "colours as they are"}
                                       schema/fraction)
                      (schema/nilable :x {:description "literal x interval(s) [a b] (an end nil - the panel edge), alone a band across the panel (wins over the mapping)"}
                                      (annotation/positions-schema interval-schema))
                      (schema/nilable :y {:description "literal y interval(s) [a b] (an end nil - the panel edge), alone a band across the panel (wins over the mapping)"}
                                      (annotation/positions-schema interval-schema))
                      draw/corner-radius-option
                      (schema/optional :width {:description "fraction of its band step a level end covers, centred"
                                                    :default 1.0}
                                       [:and schema/finite [:> 0] [:<= 1]])))
   :mapping [:map
             (schema/nilable :x0 {:description "x start of a rect, one per row (with :x1; nil - the panel edge)"} schema/accessor)
             (schema/nilable :x1 {:description "x end of a rect, one per row (with :x0; nil - the panel edge)"} schema/accessor)
             (schema/nilable :y0 {:description "y start of a rect, one per row (with :y1; nil - the panel edge)"} schema/accessor)
             (schema/nilable :y1 {:description "y end of a rect, one per row (with :y0; nil - the panel edge)"} schema/accessor)
             (schema/nilable :color {:description "fill colour per group or row (gives legend keys)"} schema/accessor)
             (schema/nilable :pattern {:description "fill pattern per group (gives legend keys)"} schema/accessor)
             (schema/nilable :shape {:description "groups the rects (no visual effect)"} schema/accessor)
             (schema/nilable :size {:description "groups the rects (no visual effect)"} schema/accessor)
             (schema/nilable :group {:description "groups without a visual aesthetic"} schema/accessor)]})

(defn- rect-style
  "Gives the style the rects draw with.

  Parameters:

  - `theme` (map): the merged theme.
  - `chart` (map): the chart value.

  Returns [[cljplot2.theme/resolved-style]] with the option `:alpha` merged over it; a vector of fills (literal, per rect) leaves the theme fill as the base."
  [theme {:keys [options] :as chart}]
  (merge (theme/resolved-style theme (cond-> chart (not (schema/color? (:fill options))) (update :options dissoc :fill)))
         (select-keys options [:alpha])))

(defn- interval?
  "Tests whether a literal value is one interval `[a b]` rather than a vector of intervals.

  Parameters:

  - `v`: the literal value.

  Returns a truthy value for a two-element vector whose ends aren't vectors."
  [v]
  (and (vector? v) (m/== 2 (count v)) (not (vector? (first v))) (not (vector? (second v)))))

(defn- literal-rects
  "Gives the rects of the literal options.

  Parameters:

  - `options` (map): `:x`, `:y` - an interval or a vector of them; `:fill` - a colour or a vector of colours.

  Returns a vector of rects `{:x [a b] :y [c d] :fill f}` - the intervals paired (1 or n each), a `:fill` vector paired as well (see [[cljplot2.data.annotation/broadcast]]).

  Throws as [[cljplot2.data.annotation/broadcast]] for vectors of different lengths."
  [options]
  (let [xs (annotation/as-vector (:x options) interval?)
        ys (annotation/as-vector (:y options) interval?)
        fills (when-not (schema/color? (:fill options)) (:fill options))
        entries (filterv (comp seq second) [[:x xs] [:y ys] [:fill fills]])
        option-keys (map first entries)]
    (mapv #(zipmap option-keys %) (annotation/broadcast option-keys (map second entries)))))

(defn compute
  "Computes the rects of the chart.

  Parameters:

  - `chart` (map): the chart value (`:options` - literal `:x`, `:y`, `:fill`, style options ...).
  - `chart-split` (map): `:groups`, `:mapping` (the chart's own: `:x0 :x1 :y0 :y1`, aesthetics).
  - `env` (map): the compute environment (`:theme`, `:plot-scales` ...).

  Returns literal - `{:rects [{:x [a b] :y [c d] :fill f}] :style st}` in every panel (`nil` interval - the whole panel side, `:fill` only for a vector of fills), extents of the interval ends; data - per panel the rects of its rows, each with its group's `:aesthetics` when the chart maps any and its `:color-value` when the colour is linear or binned (see [[cljplot2.data.group/continuous-color]]: per rect, not grouped), `:dropped` per mapped end (`:x0 :x1 :y0 :y1`) the rows with an invalid value there (skipped before grouping).

  Throws `ex-info {:key :fill}` for a `:fill` vector in data mode, `{:key :mapping}` for one end of a pair mapped or no pair."
  [{:keys [options] :as chart} {:keys [mapping] :as chart-split} {:keys [theme] :as env}]
  (let [st (rect-style theme chart)
        extents-of (fn [rects] {:x (mapcat :x rects) :y (mapcat :y rects)})]
    (if (annotation/literal? options [:x :y])
      (let [rects (literal-rects options)]
        (annotation/literal-stats {:rects rects :style st} options (extents-of rects)))
      (let [_ (annotation/require-mapping! "Rect" mapping [[:x0 :x1] [:y0 :y1]])
            _ (doseq [[a b] [[:x0 :x1] [:y0 :y1]]
                      :when (not= (some? (get mapping a)) (some? (get mapping b)))]
                (data/fail-mapping! (str "Rect maps " a " and " b " together (a pair), got only one of them.")
                                    (if (some? (get mapping a)) b a) {}))
            acc (fn [k] (when-let [a (get mapping k)] (data/row-accessor k a)))
            [x0 x1 y0 y1] (map acc [:x0 :x1 :y0 :y1])
            end-ok? (fn [v] (or (nil? v) (annotation/position? v)))
            rect-of (fn [row]
                      (let [x (when x0 [(x0 row) (x1 row)])
                            y (when y0 [(y0 row) (y1 row)])]
                        (when (every? end-ok? (concat x y))
                          (cond-> {} x (assoc :x x) y (assoc :y y)))))
            _ (when (and (some? (:fill options)) (not (schema/color? (:fill options))))
                (schema/fail! "A rect :fill vector pairs with literal :x / :y intervals, not with data rows (map :color instead)."
                              {:key :fill :value (:fill options)}))
            all-rows (mapcat val (:groups chart-split))
            ;; rows with an unplaceable end are skipped before grouping
            placed (update-vals (:groups chart-split) #(filterv rect-of %))
            end-dropped (into {} (for [[k end-of] (map vector [:x0 :x1 :y0 :y1] [x0 x1 y0 y1]) :when end-of]
                                   [k (count (remove #(end-ok? (end-of %)) all-rows))]))
            continuous (group/continuous-color placed mapping env)
            color-of (:color-of continuous)
            stats (annotation/data-stats (if continuous
                                           (assoc chart-split :groups (:groups continuous) :mapping (:mapping continuous))
                                           (assoc chart-split :groups placed))
                                         options env
                                         (fn [gs]
                                           (let [rects (into [] (mapcat (fn [{:keys [rows aesthetics]}]
                                                                          (map #(cond-> (rect-of %)
                                                                                  (seq aesthetics) (assoc :aesthetics aesthetics)
                                                                                  color-of (assoc :color-value (color-of %)))
                                                                               rows)))
                                                             gs)]
                                             [{:rects rects :style st} (extents-of rects)])))
            color-stats (when continuous
                          (group/continuous-color-stats continuous
                                                        (update-vals (:data stats) (fn [d] (map :color-value (:rects d))))
                                                        (:plot-scales env)))]
        (cond-> (update stats :dropped merge end-dropped)
          color-stats (-> (update :extents #(merge-with merge % (:extents color-stats)))
                          (assoc-in [:dropped :color] (:dropped color-stats))
                          (cond-> (:na color-stats) (update :na conj :color))))))))

(defn- outline
  "Gives the outline colour of a rect.

  Parameters:

  - `stroke`: the `:stroke-color` - a colour, `:none` or `:darker`.
  - `fill`: the rect's fill colour or `:none`.

  Returns `:none` for `:darker` on an unfilled rect, else the outline colour (`:darker` - the darkened fill, see [[cljplot2.graphics.draw/outline-color]])."
  [stroke fill]
  (if (and (= :darker stroke) (theme/none? fill)) :none (draw/outline-color stroke fill)))

(declare draw-pixel-rect!)

(defn draw
  "Draws the rects of one panel with whole-pixel edges.

  Parameters:

  - `chart` (map): the chart value (`:options` - `:corner-radius`, `:width`).
  - `computed` (map): the panel's `:rects` and `:style`.
  - `canvas`: the clojure2d canvas.
  - `box` (map): the panel's `:w`, `:h`.
  - `scales` (map): the finalized scales (position, `:color`).
  - `env` (map): the drawing environment (`:theme`).

  A rect is filled with its group's mapped `:color` (or its `:color-value` through the colour scale), else its literal fill or the style fill, with `:alpha`; a `nil` end reaches the panel edge (polar: the angular range or the radii); a rect with an unplaceable end, or outside its colour scale, is skipped."
  [{:keys [options]} {:keys [rects style]} canvas {:keys [w h]} scales env]
  (let [stroke-option (:stroke-color style)
        rounding (draw/corner-rounding options)
        sw (double (:stroke-width style))
        pattern-fill (draw/pattern-fill-cache)
        x-horizontal? (scale/horizontal? :x (:x scales))
        geometry (scale/coord-geometry scales)
        band-fraction (double (data/option options :width 1.0))
        interval-of {:x (annotation/interval-fn (:x scales) band-fraction) :y (annotation/interval-fn (:y scales) band-fraction)}
        ;; the whole extent along an axis: panel pixels, polar - the angular range or the radii
        panel-side (fn [horizontal?] (cond
                                       (and geometry horizontal?) (vec (sort (:theta geometry)))
                                       geometry [(:inner-radius geometry) (:radius geometry)]
                                       :else [0.0 (double (if horizontal? w h))]))
        side (fn [k horizontal? interval]
               (if interval
                 ((interval-of k) (first interval) (second interval))
                 (panel-side horizontal?)))
        group-fill (memoize (fn [aesthetics]
                              (when-let [st (group/style style aesthetics scales (:theme env))]
                                [(if (contains? aesthetics :color) (:color st) (:fill st)) st])))
        na (get-in env [:theme :palette :na])
        value-color (if-let [color-scale (:color scales)] (aesthetic/attribute-fn :color color-scale na) (constantly na))]
    (doseq [{:keys [x y aesthetics] rect-fill :fill :as r} rects
            :let [[g-fill st :as styled] (if (contains? r :color-value)
                                           (some-> (value-color (:color-value r)) (vector style))
                                           (group-fill aesthetics))]
            :when styled
            :let [px (side :x x-horizontal? x)
                  py (side :y (not x-horizontal?) y)]
            :when (and px py)]
      (let [[h0 h1] (if x-horizontal? px py)
            [v0 v1] (if x-horizontal? py px)
            fill (draw/with-alpha (or rect-fill g-fill) (:alpha style))
            stroke (outline stroke-option fill)]
        (if geometry
          (do (c2d/set-stroke-custom canvas {:size sw :join :miter})
              (draw/range-bar! canvas geometry [h0 v0] [h1 v1] (if (theme/none? fill) transparent fill)
                               (if (m/pos? sw) stroke :none) sw (when-not (theme/none? fill) (pattern-fill st fill)) rounding))
          (draw-pixel-rect! canvas [h0 v0 h1 v1] fill st [stroke sw] pattern-fill rounding))))))

(defn- draw-square-rect!
  "Draws a rect with square corners.

  Parameters:

  - `canvas`: the clojure2d canvas.
  - `[left top w h]` (whole pixels): the rect.
  - `fill`: the fill colour or `:none`.
  - `st` (map): the rect's style (`:pattern` ...).
  - `[stroke sw]`: the outline colour (or `:none`) and width in pixels.
  - `pattern-fill` (function): style, fill -> pattern fill (see [[cljplot2.graphics.draw/pattern-fill]]).

  The fill and its pattern are drawn unless the fill is `:none`; the outline for a positive width and a colour."
  [canvas [left top rw rh] fill st [stroke ^double sw] pattern-fill]
  (when-not (theme/none? fill)
    (draw/fill-patterned! canvas fill (pattern-fill st fill) #(c2d/rect % left top rw rh)))
  (draw/outline! canvas stroke sw :miter #(c2d/rect % left top rw rh true)))

(defn- draw-pixel-rect!
  "Draws a rect between range (pixel) positions with whole-pixel edges.

  Parameters:

  - `canvas`: the clojure2d canvas.
  - `[h0 v0 h1 v1]` (doubles): the corners in pixels.
  - `fill`: the fill colour or `:none`.
  - `st` (map): the rect's style.
  - `[stroke sw]`: the outline colour (or `:none`) and width in pixels.
  - `pattern-fill` (function): style, fill -> pattern fill.
  - `rounding`: see [[cljplot2.graphics.draw/corner-rounding]]; `nil` - square corners.

  See also [[draw-square-rect!]]."
  [canvas [^double h0 ^double v0 ^double h1 ^double v1] fill st [stroke ^double sw] pattern-fill rounding]
  (let [left (m/round h0)
        top (m/round v0)
        rw (m/long-sub (m/round h1) left)
        rh (m/long-sub (m/round v1) top)]
    (if rounding
      (draw/bar! canvas (Vec2. (double left) (double top)) (Vec2. (double (m/long-add left rw)) (double (m/long-add top rh))) (if (theme/none? fill) transparent fill)
                 (if (m/pos? sw) stroke :none) sw (when-not (theme/none? fill) (pattern-fill st fill)) rounding)
      (draw-square-rect! canvas [left top rw rh] fill st [stroke sw] pattern-fill))))

(defn legend
  "Draws the key glyph: a filled square (the entry's colour, else the style fill, with `:alpha`) with the rect's outline, centred in the key box.

  Parameters:

  - `chart` (map): the chart value.
  - `attributes` (map): the key's attributes (`:color`, `:pattern`).
  - `canvas`: the clojure2d canvas.
  - `box` (map): the key box `:x`, `:y`, `:w`, `:h`.
  - `env` (map): the drawing environment (`:theme`)."
  [{:keys [options] :as chart} attributes canvas {:keys [^double x ^double y ^double w ^double h]} env]
  (let [st (merge (rect-style (:theme env) chart) (select-keys attributes [:pattern]))
        fill (draw/with-alpha (get attributes :color (:fill st)) (:alpha st))
        stroke (outline (:stroke-color st) fill)
        sw (double (:stroke-width st))
        side (m/round (m/* draw/key-glyph-share (m/min w h)))
        left (m/round (m/+ x (m/* 0.5 (m/- w side))))
        top (m/round (m/+ y (m/* 0.5 (m/- h side))))]
    (draw-pixel-rect! canvas [left top (m/long-add left side) (m/long-add top side)] fill st [stroke sw] draw/pattern-fill
                      (draw/key-rounding (draw/corner-rounding options) side))))
