(ns cljplot2.chart.rect
  "Rect: rectangles and bands - an annotation chart (see [[cljplot2.data.annotation]]: literal options or data through
  the chart's own `:mapping`, `:extent`). Drawn in chart order: list it before the charts it should stay behind.

  * `:x [a b]` - a band over the x interval, across the whole panel
  * `:y [a b]` - a band over the y interval
  * both - a rectangle

  Literal: an interval `[a b]` or a vector of them; with both `:x` and `:y`, vectors of 1 or n intervals are paired.
  An end is a position (a level covers its band step) or `nil` - the panel edge on that side (`[5 nil]` - from 5 to
  the high end). Data: `:x0 :x1 :y0 :y1` in the chart's own `:mapping` (an `x` pair, a `y` pair or both), one rect
  per row (each pair mapped whole, else `ex-info {:key :mapping}`); a `nil` value is the panel edge, other invalid
  values skip the row. Aesthetic mappings are not used.

  Options (style defaults from theme `[:chart :rect]`): `:fill`, `:stroke-color`, `:stroke-width`, `:extent`, plus
  `:data` / `:mapping`."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.data.annotation :as annotation]
            [cljplot2.data.rows :as data]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries
  [(schema/optional :fill schema/color)
   (schema/optional :stroke-color schema/color)
   (schema/optional :stroke-width schema/non-neg)])

(def ^:private end-schema [:maybe annotation/position])
(def ^:private interval-schema [:tuple end-schema end-schema])

(def schemas
  "Schemas of the rect chart: `:style` (theme `[:chart :rect]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into annotation/options)
                (conj (schema/nilable :x (annotation/positions-schema interval-schema))
                      (schema/nilable :y (annotation/positions-schema interval-schema))))
   :mapping [:map]})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :rect]) (select-keys options [:fill :stroke-color :stroke-width])))

(defn- interval? [v] (and (vector? v) (= 2 (count v)) (not (vector? (first v))) (not (vector? (second v)))))

(defn compute
  "Literal: `{:rects [{:x [a b] :y [c d]}] :style st}` in every panel (`nil` interval - the whole panel side);
  extents of the interval ends. Data: per panel the rects of its rows."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme]}]
  (let [st (style options theme)
        extents-of (fn [rects] {:x (mapcat :x rects) :y (mapcat :y rects)})]
    (if (annotation/literal? options [:x :y])
      (let [xs (annotation/as-vector (:x options) interval?)
            ys (annotation/as-vector (:y options) interval?)
            rects (cond
                    (empty? ys) (mapv (fn [x] {:x x}) xs)
                    (empty? xs) (mapv (fn [y] {:y y}) ys)
                    :else (mapv (fn [[x y]] {:x x :y y}) (annotation/broadcast [:x :y] [xs ys])))]
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
            panels (update-vals groups #(vec (keep rect-of %)))
            e (fn [rects] (annotation/extents options (extents-of rects)))]
        {:data (update-vals panels (fn [rects] {:rects rects :style st}))
         :extents (into {} (keep (fn [[panel rects]] (let [x (e rects)] (when (seq x) [panel x])))) panels)
         :na #{}
         :dropped {:position (- (count (mapcat val groups)) (count (mapcat val panels)))}}))))

(defn draw
  "Draw the rects of one panel (whole-pixel edges); a rect with an unplaceable end is skipped."
  [_chart {:keys [rects style]} canvas {:keys [w h]} scales _env]
  (let [fill (:fill style)
        stroke (:stroke-color style)
        sw (double (:stroke-width style))
        x-horizontal? (scale/horizontal? :x (:x scales))
        interval-of {:x (annotation/interval-fn (:x scales)) :y (annotation/interval-fn (:y scales))}
        panel-side (fn [horizontal?] [0.0 (double (if horizontal? w h))])
        side (fn [k horizontal? interval]
               (if interval
                 ((interval-of k) (first interval) (second interval))
                 (panel-side horizontal?)))]
    (doseq [{:keys [x y]} rects
            :let [px (side :x x-horizontal? x)
                  py (side :y (not x-horizontal?) y)]
            :when (and px py)]
      (let [[h0 h1] (if x-horizontal? px py)
            [v0 v1] (if x-horizontal? py px)
            left (m/round (double h0)) top (m/round (double v0))
            rw (- (m/round (double h1)) left) rh (- (m/round (double v1)) top)]
        (when-not (theme/none? fill)
          (c2d/set-color canvas fill)
          (c2d/rect canvas left top rw rh))
        (when (and (not (theme/none? stroke)) (pos? sw))
          (c2d/set-stroke-custom canvas {:size sw :join :miter})
          (c2d/set-color canvas stroke)
          (c2d/rect canvas left top rw rh true))))))
