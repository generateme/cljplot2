(ns cljplot2.chart.candlestick
  "Candlestick: open-high-low-close prices per `:x`, per panel.

  Mapping `:x :open :high :low :close` (all required). A candle: the wick from low to high, the body from open to
  close - `:up-color` when close >= open, else `:down-color`; filled, outlined in `:stroke-color` (default `:none`) `:stroke-width` pixels wide;
  the wick in the body colour, `:line-width` pixels; open = close - a horizontal tick across the body. `:style
  :ohlc` - OHLC bars instead: the low-high bar, the open tick to the left, the close tick to the right (each half the
  body width).

  `:width` (0..1, default 0.7) - the body width, a fraction of the smallest pixel gap between distinct x (of a band's width on
  a bands scale; 10 pixels with a single x). Options (style defaults from theme `[:chart :candlestick]`):
  `:up-color`, `:down-color`, `:width`, `:line-width`, `:stroke-color`, `:style`, `:padding {:x [lo hi] :y [lo
  hi]}` (default 5% each side; the x extent also reaches half the smallest x gap beyond the data, so the outer bodies
  fit whatever the padding). A row with an invalid value is dropped (counted per key), as is one whose values
  don't hold `low <= open, close <= high` (counted as `:ohlc`). Colour means up / down: an aesthetic or `:group` in
  the chart's own `:mapping` throws `ex-info {:key :mapping}`; ones inherited from the plot mapping are ignored (the
  candles draw nothing into their legend keys).

  Functions: [[compute]], [[draw]], [[legend]] - the chart contract."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]
           [java.time LocalDate]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^{:private true :const true :tag 'double} single-body-width
  "Body width in pixels when a panel has one distinct x: a fallback geometry with no gap to take `:width` of, not a style (kept, not exposed)."
  10.0)

(def ^:private style-entries
  "Map entries of the candlestick style (theme `[:chart :candlestick]`, also options)."
  [(schema/optional :up-color {:description "colour of a candle closing at or above its open"} schema/color)
   (schema/optional :down-color {:description "colour of a candle closing below its open"} schema/color)
   (schema/optional :width {:description "body width, a fraction of the smallest pixel gap between distinct x (of a band's width on a bands scale)"}
                    schema/fraction)
   (schema/optional :line-width {:description "wick (and OHLC bar) line width in pixels"} schema/non-neg)
   (schema/optional :stroke-color {:description "body outline colour: a colour or :none"} schema/color)
   (schema/optional :stroke-width {:description "body outline width in pixels"} schema/non-neg)
   (schema/optional :up-pattern {:description "pattern drawn over the bodies of up candles"} schema/pattern)
   (schema/optional :down-pattern {:description "pattern drawn over the bodies of down candles"} schema/pattern)
   (schema/optional :pattern-color {:description "pattern ink: a colour, :none or :darker (the darkened body colour)"} draw/stroke-color)])

(def ^:private value-keys
  "Mapping keys of a candle's prices."
  [:open :high :low :close])

(def schemas
  "Schemas of the candlestick chart: `:style` (theme `[:chart :candlestick]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into theme/theme-style-entries)
                (conj (schema/optional :style {:description ":candle (wick and body) or :ohlc (a low-high bar with open and close ticks)"
                                               :default :candle}
                                       [:enum :candle :ohlc])
                      (schema/optional :padding {:description "padding of the data span per position axis, {:x [lo hi] :y [lo hi]} fractions of the span"
                                                 :cljplot2/default-doc "0.05 on every side (the x extent also reaches half the smallest x gap beyond the data)"}
                                       data/position-padding)))
   :mapping [:map
             [:x {:description "x position (data units)"} schema/accessor]
             [:open {:description "opening price"} schema/accessor]
             [:high {:description "highest price"} schema/accessor]
             [:low {:description "lowest price"} schema/accessor]
             [:close {:description "closing price"} schema/accessor]]})

(defn- smallest-gap
  "Gives the smallest positive gap between neighbouring sorted numbers.

  Parameters:

  - `sorted-values` (sequence of numbers): ascending; repeats allowed.

  Returns the smallest positive difference of neighbours as a double; `nil` without two distinct values."
  [sorted-values]
  (let [gaps (filter (fn [^double g] (m/pos? g))
                     (map (fn [^double a ^double b] (m/- b a)) sorted-values (rest sorted-values)))]
    (when (seq gaps) (v/mn gaps))))

(defn- half-gap-ends
  "Extends x values by half the smallest gap between distinct x beyond the data, so the outer bodies fit with any padding.

  Parameters:

  - `xs` (sequence): the x values - numbers or `java.time` values, keyed by [[cljplot2.data.rows/x-key]].

  Returns `xs` with two more values, half the smallest gap below the lowest and above the highest (a `LocalDate` moved by whole days, keeping its type); `xs` unchanged with fewer than 2 distinct keyed values or with values that have no key (levels)."
  [xs]
  (let [keyed (sort-by first (distinct (keep (fn [x] (when-let [k (data/x-key x)] [k x])) xs)))
        gap (smallest-gap (map first keyed))]
    (if (or (m/not== (count keyed) (count (distinct xs))) (nil? gap))
      xs
      (let [half (m/* 0.5 (double gap))
            [^double k0 x0] (first keyed)
            [^double k1 x1] (last keyed)
            beyond (fn [^double k x ^long sign]
                     (if (instance? LocalDate x)
                       (.plusDays ^LocalDate x (m/* sign (long (m/ceil (m// half 86400000.0)))))
                       (data/key->x (m/+ k (m/* sign half)) x)))]
        (concat xs [(beyond k0 x0 -1) (beyond k1 x1 1)])))))

(defn compute
  "Computes the candles per panel.

  Parameters:

  - `chart` (map): the chart value (`:options` - `:style`, `:padding`, the chart's own `:mapping`).
  - `chart-split` (map): `:groups` - panel -> rows, `:mapping`.
  - `env` (map): the compute environment (`:theme`).

  Returns per panel `{:candles [{:x :open :high :low :close}] :style style :kind :candle|:ohlc}`; extents (x reaching half the smallest gap beyond the data, see [[half-gap-ends]]; y over all four values), padding, `:dropped` (`:x`, every value key, `:ohlc` - inconsistent values).

  Throws `ex-info {:key :mapping}` for aesthetics or `:group` in the chart's own mapping, or a mapped dimension without a valid value."
  [{:keys [options] :as chart} {:keys [groups mapping]} {:keys [theme]}]
  (when-let [dims (let [own (:mapping options)] (seq (cond-> (aesthetic/mapped-dims own) (some? (:group own)) (conj :group))))]
    (data/fail-mapping! "Candlestick colours mean up / down; it takes no aesthetics or :group (unmap them in the chart's :mapping)."
                        (first dims) {:value (get (:mapping options) (first dims))}))
  (let [accessors (into {:x (data/row-accessor :x (:x mapping))}
                        (map (fn [k] [k (data/row-accessor k (get mapping k))]))
                        value-keys)
        complete? (fn [r] (and (data/valid-value? ((:x accessors) r)) (every? #(schema/finite? ((accessors %) r)) value-keys)))
        consistent? (fn [r] (let [[^double o ^double h ^double l ^double c] (mapv #(double ((accessors %) r)) value-keys)]
                              (and (m/<= l (m/min o c)) (m/<= (m/max o c) h))))
        valid? (fn [r] (and (complete? r) (consistent? r)))
        rows (mapcat val groups)
        _ (data/check-mapped! rows accessors (fn [k v] (if (= :x k) (data/valid-value? v) (schema/finite? v))))
        candle (fn [r] (into {} (map (fn [[k f]] [k (let [value (f r)] (if (= :x k) value (double value)))])) accessors))
        st (theme/resolved-style theme chart)
        data (update-vals groups (fn [rs] {:candles (mapv candle (filter valid? rs))
                                           :style st
                                           :kind (data/option options :style :candle)}))]
    (into (data/padding-stats data/default-padding {} options)
          {:data data
           :extents (into {} (keep (fn [[panel {:keys [candles]}]]
                                     (when (seq candles)
                                       [panel {:x (data/extent :x (half-gap-ends (map :x candles)))
                                               :y (data/extent :y (mapcat (apply juxt value-keys) candles))}])))
                          data)
           :na #{}
           ;; per dimension its invalid values; :ohlc - complete rows (every value valid) with inconsistent values, so no row counts twice for one reason
           :dropped (into {:x (data/invalid-count (:x accessors) rows)
                           :ohlc (count (remove consistent? (filter complete? rows)))}
                          (map (fn [k] [k (count (remove #(schema/finite? ((accessors k) %)) rows))]))
                          value-keys)})))

(defn- body-width
  "Gives the body width in pixels.

  Parameters:

  - `ps` (sequence of doubles): the candles' pixel positions along x (`NaN` - not placed).
  - `fraction` (double): the `:width` fraction.
  - `x-scale` (map): the finalized x scale.

  Returns `fraction` of the bandwidth on a bands x scale, else of the smallest gap between the distinct finite positions; [[single-body-width]] for one position."
  ^double [ps ^double fraction x-scale]
  (if-let [bandwidth (:bandwidth (scale/band-geometry x-scale))]
    (m/* fraction (double bandwidth))
    (if-let [gap (smallest-gap (sort (distinct (filter (fn [^double p] (m/valid-double? p)) ps))))]
      (m/* fraction (double gap))
      single-body-width)))

(defn- stroke!
  "Strokes one polyline.

  Parameters:

  - `canvas`: the clojure2d canvas.
  - `color`: the line colour.
  - `width` (double): the line width in pixels.
  - `points` (sequence): the pixel points `[x y]`.

  See also [[cljplot2.graphics.polyline/stroke-runs!]]."
  [canvas color width points]
  (polyline/stroke-runs! canvas [points] {:color color :line-width width}))

(defn draw
  "Draws the candles (or OHLC bars) of one panel.

  Parameters:

  - `_chart` (map): the chart value.
  - `computed` (map): the panel's `:candles`, `:style`, `:kind`.
  - `canvas`: the clojure2d canvas.
  - `_box` (map): the panel box.
  - `scales` (map): the finalized position scales.
  - `_env` (map): the drawing environment.

  A candle whose values the scales can't place is skipped; open equal to close draws a tick across the body."
  [_chart {:keys [candles style kind]} canvas _box scales _env]
  (when (seq candles)
    (let [projection (scale/projection scales)
          flipped? (scale/flipped? scales)
          ;; along: the pixel axis of x; across: the pixel axis of the values
          along (fn [^Vec2 p] (if flipped? (.y p) (.x p)))
          across (fn [^Vec2 p] (if flipped? (.x p) (.y p)))
          point (fn [a c] (if flipped? [c a] [a c]))
          xs (mapv #(along (projection (:x %) (:low %))) candles)
          half (m/* 0.5 (body-width xs (:width style) (:x scales)))
          wick (double (:line-width style))
          pattern-fill (draw/pattern-fill-cache)]
      (doseq [[c ^double a] (map vector candles xs)
              :let [[^double o ^double h ^double l ^double cl] (mapv #(double (across (projection (:x c) (% c)))) value-keys)
                    up? (m/>= (double (:close c)) (double (:open c)))
                    color (if up? (:up-color style) (:down-color style))]
              :when (and (m/valid-double? a) (m/valid-double? o) (m/valid-double? h) (m/valid-double? l) (m/valid-double? cl))]
        (if (= :ohlc kind)
          (do (stroke! canvas color wick [(point a l) (point a h)])
              (stroke! canvas color wick [(point (m/- a half) o) (point a o)])
              (stroke! canvas color wick [(point a cl) (point (m/+ a half) cl)]))
          (do (stroke! canvas color wick [(point a l) (point a h)])
              (if (m/== o cl)
                (stroke! canvas color (m/max 1.0 wick) [(point (m/- a half) o) (point (m/+ a half) o)])
                (let [lo (m/min o cl)
                      span (m/abs (m/- o cl))
                      [rx ry rw rh] (if flipped? [lo (m/- a half) span (m/* 2.0 half)] [(m/- a half) lo (m/* 2.0 half) span])]
                  (draw/fill-patterned! canvas color
                                        (pattern-fill {:pattern (if up? (:up-pattern style) (:down-pattern style))
                                                       :pattern-color (:pattern-color style)}
                                                      color)
                                        #(c2d/rect % rx ry rw rh))
                  (draw/outline! canvas (:stroke-color style) (:stroke-width style) :bevel #(c2d/rect % rx ry rw rh true))))))))))

(defn legend
  "Draws no key glyph: candle colours mean up / down, not an inherited aesthetic's level.

  Parameters:

  - `_chart`, `_attributes`, `_canvas`, `_box`, `_env`: as the chart contract's legend.

  Returns `nil`."
  [_chart _attributes _canvas _box _env]
  nil)
