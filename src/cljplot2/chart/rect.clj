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

  Options (style defaults from theme `[:chart :rect]`): `:fill`, `:stroke-color`, `:stroke-width`, `:alpha` (the
  fill's alpha, a fraction; default - colors as they are), `:corner-radius` (pixels, every corner rounded, clamped to
  half the shorter side; polar - the annular sector's corners; default 0), `:band-width` (the fraction of its band step
  a level end covers, centred, `(0 1]`; default 1 - the whole step, rects of neighbouring levels touch), `:extent`, plus
  `:data` / `:mapping`."
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

(def ^:private style-entries
  (into [(schema/optional :fill schema/color)
         (schema/optional :stroke-color schema/color)
         (schema/optional :stroke-width schema/non-neg)]
        draw/pattern-style-entries))

(def ^:private end-schema [:maybe annotation/position])
(def ^:private interval-schema [:tuple end-schema end-schema])

(def ^:private fill-option
  "A color, or a non-empty vector of colors (literal rects)."
  [:multi {:dispatch schema/color?} [true schema/color] [false [:vector {:min 1} schema/color]]])

(def schemas
  "Schemas of the rect chart: `:style` (theme `[:chart :rect]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into (rest style-entries))
                (into data/common-options)
                (into annotation/options)
                (conj (schema/optional :fill fill-option)
                      (schema/optional :alpha schema/fraction)
                      (schema/nilable :x (annotation/positions-schema interval-schema))
                      (schema/nilable :y (annotation/positions-schema interval-schema))
                      draw/corner-radius-option
                      (schema/optional :band-width [:and schema/finite [:> 0] [:<= 1]])))
   :mapping [:map]})

(defn- style
  "Theme style under the options; a vector of fills (literal, per rect) leaves the theme fill as the base."
  [options theme]
  (merge (get-in theme [:chart :rect])
         (select-keys options [:stroke-color :stroke-width :alpha :pattern :pattern-color])
         (when (schema/color? (:fill options)) (select-keys options [:fill]))))

(defn- with-alpha
  "`fill` with the style's `:alpha` (a fraction) as its alpha; `:none` and an unset alpha unchanged."
  [fill alpha]
  (if (or (nil? alpha) (theme/none? fill)) fill (c/set-alpha fill (* 255.0 (double alpha)))))

(defn- interval? [v] (and (vector? v) (= 2 (count v)) (not (vector? (first v))) (not (vector? (second v)))))

(defn- literal-rects
  "Rects `[{:x :y :fill}]` of the literal options: intervals paired (1 or n each), `:fill` vectors paired as well."
  [options]
  (let [xs (annotation/as-vector (:x options) interval?)
        ys (annotation/as-vector (:y options) interval?)
        fills (when-not (schema/color? (:fill options)) (:fill options))
        entries (filterv (comp seq second) [[:x xs] [:y ys] [:fill fills]])]
    (mapv #(zipmap (map first entries) %) (annotation/broadcast (mapv first entries) (mapv second entries)))))

(defn compute
  "Literal: `{:rects [{:x [a b] :y [c d] :fill f}] :style st}` in every panel (`nil` interval - the whole panel
  side, `:fill` only for a vector of fills); extents of the interval ends. Data: per panel the rects of its rows,
  each with its group's `:aesthetics` when the chart maps any, and its `:color-value` when the color is linear or
  binned (see [[cljplot2.data.group/continuous-color]]: per rect, not grouped); `:dropped :position` - rows with an
  invalid end (skipped before grouping). A `:fill` vector in data mode throws `ex-info {:key :fill}`."
  [{:keys [options]} {:keys [mapping] :as chart-split} {:keys [theme] :as env}]
  (let [st (style options theme)
        extents-of (fn [rects] {:x (mapcat :x rects) :y (mapcat :y rects)})]
    (if (annotation/literal? options [:x :y])
      (let [rects (literal-rects options)]
        (annotation/literal-stats {:rects rects :style st} options (extents-of rects)))
      (let [_ (annotation/require-mapping! "Rect" mapping [[:x0 :x1] [:y0 :y1]])
            _ (doseq [[a b] [[:x0 :x1] [:y0 :y1]]
                      :when (not= (some? (get mapping a)) (some? (get mapping b)))]
                (schema/fail! (str "Rect maps " a " and " b " together (a pair), got only one of them.")
                              {:key :mapping :value (select-keys mapping [a b])}))
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
            skipped (- (count all-rows) (count (mapcat val placed)))
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
        (cond-> (assoc-in stats [:dropped :position] skipped)
          color-stats (-> (update :extents #(merge-with merge % (:extents color-stats)))
                          (assoc-in [:dropped :color] (:dropped color-stats))
                          (cond-> (:na? color-stats) (update :na conj :color))))))))

(declare draw-pixel-rect!)

(defn draw
  "Draw the rects of one panel (whole-pixel edges), filled with the group's mapped `:color`, else the rect's literal
  or the style fill (with `:alpha`); a rect with an unplaceable end, or outside its color scale, is skipped."
  [{:keys [options]} {:keys [rects style]} canvas {:keys [w h]} scales env]
  (let [stroke (:stroke-color style)
        rounding (draw/corner-rounding options)
        sw (double (:stroke-width style))
        pattern-fill (draw/pattern-fill-cache)
        x-horizontal? (scale/horizontal? :x (:x scales))
        geometry (scale/coord-geometry scales)
        band-width (double (get options :band-width 1.0))
        interval-of {:x (annotation/interval-fn (:x scales) band-width) :y (annotation/interval-fn (:y scales) band-width)}
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
        value-color (if-let [scale (:color scales)] (aesthetic/attribute-fn :color scale na) (constantly na))]
    (doseq [{:keys [x y fill aesthetics] :as r} rects
            :let [[g-fill st :as styled] (if (contains? r :color-value)
                                           (some-> (value-color (:color-value r)) (vector style))
                                           (group-fill aesthetics))]
            :when styled
            :let [px (side :x x-horizontal? x)
                  py (side :y (not x-horizontal?) y)]
            :when (and px py)]
      (let [[h0 h1] (if x-horizontal? px py)
            [v0 v1] (if x-horizontal? py px)
            fill (with-alpha (or fill g-fill) (:alpha style))]
        (if geometry
          (do (c2d/set-stroke-custom canvas {:size sw :join :miter})
              (draw/range-bar! canvas geometry [h0 v0] [h1 v1] (if (theme/none? fill) [0 0 0 0] fill)
                               (if (pos? sw) stroke :none) sw (when-not (theme/none? fill) (pattern-fill st fill)) rounding))
          (draw-pixel-rect! canvas [h0 v0 h1 v1] fill st [stroke sw] pattern-fill rounding))))))

(defn- draw-square-rect!
  "A rect `[left top w h]` (whole pixels): fill with its pattern, stroke `[color width]`."
  [canvas [left top rw rh] fill st [stroke sw] pattern-fill]
  (let [sw (double sw)]
    (when-not (theme/none? fill)
      (c2d/set-color canvas fill)
      (c2d/rect canvas left top rw rh)
      (draw/fill-with-pattern! canvas (pattern-fill st fill) #(c2d/rect canvas left top rw rh)))
    (when (and (not (theme/none? stroke)) (pos? sw))
      (c2d/set-stroke-custom canvas {:size sw :join :miter})
      (c2d/set-color canvas stroke)
      (c2d/rect canvas left top rw rh true))))

(defn- draw-pixel-rect!
  "A rect between range (pixel) positions `h0..h1` x `v0..v1` (whole-pixel edges): fill with its pattern, stroke
  `[color width]`; `rounding` (see [[cljplot2.graphics.draw/corner-rounding]], `nil` - square) rounds every corner."
  [canvas [h0 v0 h1 v1] fill st [stroke sw] pattern-fill rounding]
  (let [left (m/round (double h0)) top (m/round (double v0))
        rw (- (m/round (double h1)) left) rh (- (m/round (double v1)) top)
        sw (double sw)]
    (if rounding
      (draw/bar! canvas (Vec2. (double left) (double top)) (Vec2. (double (+ left rw)) (double (+ top rh))) (if (theme/none? fill) [0 0 0 0] fill)
                 (if (pos? sw) stroke :none) sw (when-not (theme/none? fill) (pattern-fill st fill)) rounding)
      (draw-square-rect! canvas [left top rw rh] fill st [stroke sw] pattern-fill))))

(defn legend
  "Key glyph: a filled square (the entry's color, else the style fill, with `:alpha`) with the rect's stroke,
  centred in `box`."
  [{:keys [options]} attributes canvas {:keys [x y w h]} env]
  (let [st (merge (style options (:theme env)) (select-keys attributes [:pattern]))
        fill (with-alpha (get attributes :color (:fill st)) (:alpha st))
        stroke (:stroke-color st)
        sw (double (:stroke-width st))
        side (m/round (* 0.7 (m/min (double w) (double h))))
        left (m/round (+ (double x) (* 0.5 (- (double w) side))))
        top (m/round (+ (double y) (* 0.5 (- (double h) side))))]
    (draw-pixel-rect! canvas [left top (+ left side) (+ top side)] fill st [stroke sw] draw/pattern-fill
                      (draw/key-rounding (draw/corner-rounding options) side))))
