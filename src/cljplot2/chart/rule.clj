(ns cljplot2.chart.rule
  "Rule: reference lines across the panel - an annotation chart (see [[cljplot2.data.annotation]]: literal options or
  data through the chart's own `:mapping`, `:extent`).

  * `:x` - vertical lines (along the `:y` direction) at these positions
  * `:y` - horizontal lines
  * `:slope` and `:intercept` (both) - the line `y = intercept + slope * x`, drawn between the x scale's domain ends
    (clipped to the panel); linear x and y scales only, else `ex-info {:key :slope}`; adds nothing to the extents

  Literal: a position or a vector of them for `:x` / `:y`, numbers (or vectors of 1 or n) for `:slope` /
  `:intercept`. Data: the same keys in the chart's own `:mapping`, one line per row; ordinal aesthetics (`:color`)
  through the chart's own mapping style the lines and give legend keys.

  Options (style defaults from theme `[:chart :rule]`): `:color`, `:width`, `:dash`, `:extent`, plus `:data` /
  `:mapping`."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.data.annotation :as annotation]
            [cljplot2.data.group :as group]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.scale.coord :as coord]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private position-keys [:x :y :slope :intercept])

(def schemas
  "Schemas of the rule chart: `:style` (theme `[:chart :rule]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] polyline/style-entries)
   :options (-> [:map {:closed true}]
                (into polyline/style-entries)
                (into data/common-options)
                (into annotation/options)
                (conj (schema/nilable :x (annotation/positions-schema annotation/position))
                      (schema/nilable :y (annotation/positions-schema annotation/position))
                      (schema/nilable :slope (annotation/positions-schema schema/finite))
                      (schema/nilable :intercept (annotation/positions-schema schema/finite))))
   :mapping [:map]})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :rule]) (select-keys options [:color :width :dash])))

(defn- abline-pairs
  "`[[slope intercept] ..]` of `slopes` / `intercepts` (vectors); both or neither, else `ex-info {:key k}`."
  [slopes intercepts]
  (cond
    (and (empty? slopes) (empty? intercepts)) []
    (empty? intercepts) (schema/fail! "Rule :slope needs :intercept." {:key :intercept :value nil})
    (empty? slopes) (schema/fail! "Rule :intercept needs :slope." {:key :slope :value nil})
    :else (annotation/broadcast [:slope :intercept] [slopes intercepts])))

(defn- lines
  "`{:xs [..] :ys [..] :ablines [[slope intercept] ..]}` of one group's rows (data mode) or the literal options."
  [xs ys slopes intercepts]
  {:xs (vec xs) :ys (vec ys) :ablines (abline-pairs (vec slopes) (vec intercepts))})

(defn compute
  "Literal: `{:rules [{:xs :ys :ablines}] :style st}` in every panel, extents of `:x` / `:y`. Data: per panel one
  rule set per group (with `:aesthetics`); rows with an invalid value of a mapped key are skipped."
  [{:keys [options]} {:keys [mapping] :as chart-split} {:keys [theme] :as env}]
  (when (and (coord/polar? (:coord env)) (or (some? (:slope options)) (some? (:intercept options))
                                             (some? (:slope mapping)) (some? (:intercept mapping))))
    (schema/fail! "Rule :slope / :intercept lines aren't supported with :coord :polar." {:key :coord :value :slope}))
  (let [st (style options theme)
        many (fn [v] (annotation/as-vector v #(not (vector? %))))]
    (if (annotation/literal? options position-keys)
      (let [r (lines (many (:x options)) (many (:y options)) (many (:slope options)) (many (:intercept options)))]
        (annotation/literal-stats {:rules [r] :style st} options {:x (:xs r) :y (:ys r)}))
      (let [_ (annotation/require-mapping! "Rule" mapping [[:x] [:y] [:slope :intercept]])
            column (fn [k rows] (if-let [acc (get mapping k)]
                                  (filterv annotation/position? (map (data/row-accessor k acc) rows))
                                  []))
            abline-rows (fn [rows] (if (and (:slope mapping) (:intercept mapping))
                                     (let [s (data/row-accessor :slope (:slope mapping))
                                           i (data/row-accessor :intercept (:intercept mapping))]
                                       (filterv (fn [[a b]] (and (number? a) (number? b)
                                                                 (m/valid-double? (double a)) (m/valid-double? (double b))))
                                                (map (juxt s i) rows)))
                                     []))
            rule-of (fn [{:keys [rows aesthetics]}]
                      (assoc (lines (column :x rows) (column :y rows) [] []) :ablines (abline-rows rows)
                             :aesthetics aesthetics))]
        (annotation/data-stats chart-split options env
                               (fn [gs]
                                 (let [rules (mapv rule-of gs)]
                                   [{:rules rules :style st} {:x (mapcat :xs rules) :y (mapcat :ys rules)}])))))))

(defn- linear!
  "Throw `ex-info {:key :slope}` unless scale `k` is linear."
  [k finalized]
  (when-not (= :linear (:kind finalized))
    (schema/fail! "Rule :slope / :intercept needs linear x and y scales." {:key :slope :value [k (:kind finalized)]})))

(defn draw-ablines!
  "Draw the lines `y = intercept + slope * x` of `ablines` (`[[slope intercept] ..]`) between the ends of the x scale's
  domain in the canvas' current style; linear x and y `scales` (the caller checks). Lines with a non-finite end are
  skipped."
  [canvas scales ablines]
  (let [projection (scale/projection scales)
        [x0 x1] (:domain (:x scales))]
    (doseq [[b a] ablines
            :let [^Vec2 p (projection x0 (+ (double a) (* (double b) (double x0))))
                  ^Vec2 q (projection x1 (+ (double a) (* (double b) (double x1))))]
            :when (every? #(m/valid-double? (double %)) [(.x p) (.y p) (.x q) (.y q)])]
      (c2d/line canvas (.x p) (.y p) (.x q) (.y q)))))

(defn draw
  "Draw the rules of one panel: `:x` / `:y` lines across the panel (whole-pixel centred), ab lines between the x
  domain ends; unplaceable positions are skipped."
  [_chart {:keys [rules style]} canvas {:keys [w h]} scales env]
  (let [x-pixel (annotation/pixel-fn (:x scales))
        y-pixel (annotation/pixel-fn (:y scales))
        x-horizontal? (scale/horizontal? :x (:x scales))
        y-horizontal? (scale/horizontal? :y (:y scales))
        geometry (scale/coord-geometry scales)
        across! (if geometry
                  ;; polar: a position on the angular (horizontal) axis is a spoke, on the radial one a circle
                  (fn [horizontal-axis? p]
                    (when (m/valid-double? p)
                      (c2d/path canvas (if horizontal-axis?
                                         (coord/line-path geometry [[p (:inner-radius geometry)] [p (:radius geometry)]] false)
                                         (coord/circle-path geometry p))
                                false true)))
                  (fn [horizontal-axis? p]
                    (when (m/valid-double? p)
                      (let [p (draw/pixel-center p)]
                        ;; a position on a horizontal axis is a vertical line
                        (if horizontal-axis?
                          (c2d/line canvas p 0.0 p (double h))
                          (c2d/line canvas 0.0 p (double w) p))))))]
    (doseq [{:keys [xs ys ablines aesthetics]} rules
            :let [st (group/style style aesthetics scales (:theme env))]
            :when (and st (polyline/set-style! canvas st))]
      (doseq [x xs] (across! x-horizontal? (x-pixel x)))
      (doseq [y ys] (across! y-horizontal? (y-pixel y)))
      (when (seq ablines)
        (linear! :x (:x scales))
        (linear! :y (:y scales))
        (draw-ablines! canvas scales ablines)))))

(defn legend
  "Key glyph: a horizontal line through the middle of `box`."
  [{:keys [options]} attributes canvas box env]
  (polyline/key-glyph! canvas box (merge (style options (:theme env)) (select-keys attributes [:color :dash])) :none))
