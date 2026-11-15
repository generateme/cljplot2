(ns cljplot2.chart.candlestick
  "Candlestick: open-high-low-close prices per `:x`, per panel.

  Mapping `:x :open :high :low :close` (all required). A candle: the wick from low to high, the body from open to
  close - `:up-color` when close >= open, else `:down-color`; filled, outlined in `:stroke-color` (default `:none`);
  the wick in the body colour, `:wick-width` pixels; open = close - a horizontal tick across the body. `:style
  :ohlc` - OHLC bars instead: the low-high bar, the open tick to the left, the close tick to the right (each half the
  body width).

  `:body-width` (0..1, default 0.7) - the fraction of the smallest pixel gap between distinct x (of a band's width on
  a bands scale; 10 pixels with a single x). Options (style defaults from theme `[:chart :candlestick]`):
  `:up-color`, `:down-color`, `:body-width`, `:wick-width`, `:stroke-color`, `:style`, `:padding {:x [lo hi] :y [lo
  hi]}` (default 5% each side). A row with an invalid value is dropped (counted per key). Colour means up / down: a
  mapped aesthetic or `:group` throws `ex-info {:key :mapping}`; no legend."
  (:require [clojure2d.core :as c2d]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private ^:const single-body-width
  "Body width (pixels) when a panel has one distinct x."
  10.0)

(def ^:private style-entries
  [(schema/optional :up-color schema/color)
   (schema/optional :down-color schema/color)
   (schema/optional :body-width schema/fraction)
   (schema/optional :wick-width schema/non-neg)
   (schema/optional :stroke-color schema/color)])

(def ^:private value-keys [:open :high :low :close])

(def schemas
  "Schemas of the candlestick chart: `:style` (theme `[:chart :candlestick]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (conj (schema/optional :style [:enum :candle :ohlc])
                      (schema/optional :padding data/position-padding)))
   :mapping (into [:map [:x schema/accessor]] (map (fn [k] [k schema/accessor])) value-keys)})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :candlestick]) (select-keys options [:up-color :down-color :body-width :wick-width :stroke-color])))

(defn- finite? [v] (and (number? v) (Double/isFinite (double v))))

(defn compute
  "Per panel: `{:candles [{:x :open :high :low :close}] :style style :kind :candle|:ohlc}`; extents (y over all four
  values), padding, `:dropped` (`:x` and every value key)."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme]}]
  (when-let [dims (seq (cond-> (aesthetic/mapped-dims mapping) (some? (:group mapping)) (conj :group)))]
    (schema/fail! "Candlestick colours mean up / down; it takes no aesthetics or :group (unmap them in the chart's :mapping)."
                  {:key :mapping :value (vec dims)}))
  (let [accessors (into {:x (data/row-accessor :x (:x mapping))}
                        (map (fn [k] [k (data/row-accessor k (get mapping k))]))
                        value-keys)
        valid? (fn [r] (and (data/valid-value? ((:x accessors) r)) (every? #(finite? ((accessors %) r)) value-keys)))
        rows (mapcat val groups)
        _ (doseq [[k f] accessors]
            (when (and (seq rows) (not-any? #(if (= :x k) (data/valid-value? (f %)) (finite? (f %))) rows))
              (schema/fail! "No valid values for a mapped dimension - check the mapping." {:key :mapping :value k :rows (count rows)})))
        candle (fn [r] (into {} (map (fn [[k f]] [k (let [v (f r)] (if (= :x k) v (double v)))])) accessors))
        data (update-vals groups (fn [rs] {:candles (mapv candle (filter valid? rs))
                                           :style (style options theme)
                                           :kind (get options :style :candle)}))]
    {:data data
     :extents (into {} (keep (fn [[panel {:keys [candles]}]]
                               (when (seq candles)
                                 [panel {:x (data/extent :x (map :x candles))
                                         :y (data/extent :y (mapcat (apply juxt value-keys) candles))}])))
                    data)
     :padding (merge {:x [0.05 0.05] :y [0.05 0.05]} (:padding options))
     :na #{}
     :dropped (into {:x (count (remove #(data/valid-value? ((:x accessors) %)) rows))}
                    (map (fn [k] [k (count (remove #(finite? ((accessors k) %)) rows))]))
                    value-keys)}))

(defn- body-width
  "Body width in pixels: `fraction` of the smallest gap between the distinct pixel positions `ps` along x, of the
  bandwidth on a bands x scale, [[single-body-width]] for one position."
  ^double [ps fraction x-scale]
  (let [fraction (double fraction)]
    (if-let [bandwidth (:bandwidth (scale/band-geometry x-scale))]
      (* fraction (double bandwidth))
      (let [sorted (sort (distinct (filter #(Double/isFinite (double %)) ps)))
            gaps (filter pos? (map (fn [a b] (- (double b) (double a))) sorted (rest sorted)))]
        (if (seq gaps) (* fraction (double (reduce min gaps))) single-body-width)))))

(defn- stroke!
  [canvas color width points]
  (polyline/stroke-runs! canvas [points] {:color color :width width}))

(defn draw
  "Draw the candles (or OHLC bars) of one panel; candles whose values the scales can't place are skipped."
  [_chart {:keys [candles style kind]} canvas _box scales _env]
  (when (seq candles)
    (let [projection (scale/projection scales)
          flipped? (scale/flipped? scales)
          ;; along: the pixel axis of x; across: the pixel axis of the values
          along (fn [^Vec2 p] (if flipped? (.y p) (.x p)))
          across (fn [^Vec2 p] (if flipped? (.x p) (.y p)))
          point (fn [a c] (if flipped? [c a] [a c]))
          xs (mapv #(along (projection (:x %) (:low %))) candles)
          half (* 0.5 (body-width xs (:body-width style) (:x scales)))
          wick (double (:wick-width style))]
      (doseq [[c a] (map vector candles xs)
              :let [a (double a)
                    [^double o ^double h ^double l ^double cl] (mapv #(double (across (projection (:x c) (% c)))) value-keys)
                    color (if (>= (double (:close c)) (double (:open c))) (:up-color style) (:down-color style))]
              :when (every? #(Double/isFinite (double %)) [a o h l cl])]
        (if (= :ohlc kind)
          (do (stroke! canvas color wick [(point a l) (point a h)])
              (stroke! canvas color wick [(point (- a half) o) (point a o)])
              (stroke! canvas color wick [(point a cl) (point (+ a half) cl)]))
          (do (stroke! canvas color wick [(point a l) (point a h)])
              (if (== o cl)
                (stroke! canvas color (max 1.0 wick) [(point (- a half) o) (point (+ a half) o)])
                (let [lo (Math/min o cl) span (Math/abs (- o cl))
                      [rx ry rw rh] (if flipped? [lo (- a half) span (* 2.0 half)] [(- a half) lo (* 2.0 half) span])]
                  (c2d/set-color canvas color)
                  (c2d/rect canvas rx ry rw rh)
                  (when-not (theme/none? (:stroke-color style))
                    (c2d/set-stroke canvas 1.0)
                    (c2d/set-color canvas (:stroke-color style))
                    (c2d/rect canvas rx ry rw rh true))))))))))

(defn legend
  "Key glyph: an up candle (never called by the pipeline - the chart maps no legend dimension)."
  [{:keys [options]} _attributes canvas {:keys [x y w h]} env]
  (let [st (style options (:theme env))]
    (c2d/set-color canvas (:up-color st))
    (c2d/rect canvas (+ (double x) (* 0.35 (double w))) (+ (double y) (* 0.25 (double h))) (* 0.3 (double w)) (* 0.5 (double h)))))
