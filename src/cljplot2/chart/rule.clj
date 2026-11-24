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

  Options (style defaults from theme `[:chart :rule]`): `:color`, `:line-width`, `:dash`, `:extent`, plus `:data` /
  `:mapping`.

  Functions: [[schemas]] - the chart's schemas; [[compute]] - the rules of every panel; [[cljplot2.graphics.curve/draw-ablines!]] - lines
  `y = intercept + slope * x`; [[draw]] - one panel's rules; the key glyph is [[cljplot2.chart.legend/line-legend]]."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.graphics.curve :as curve]
            [cljplot2.chart.common :as common]
            [cljplot2.data.annotation :as annotation]
            [cljplot2.data.group :as group]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.scale.coord :as coord]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private position-keys
  "Options that make a rule chart literal (see [[cljplot2.data.annotation/literal?]])."
  [:x :y :slope :intercept])

(def ^:private option-rules
  "Rules (see [[cljplot2.schema/with-rules]]) of the options: a literal `:slope` needs `:intercept` and an `:intercept` needs `:slope`."
  [[#(when (and (some? (:slope %)) (nil? (:intercept %))) ":slope needs :intercept") [:intercept]]
   [#(when (and (some? (:intercept %)) (nil? (:slope %))) ":intercept needs :slope") [:slope]]])

(def schemas
  "Schemas of the rule chart: `:style` (theme `[:chart :rule]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] polyline/style-entries)
   :options (apply schema/with-rules
                   (-> [:map {:closed true}]
                       (into polyline/style-entries)
                       (into data/common-options)
                       (into theme/theme-style-entries)
                       (into annotation/options)
                       (conj (schema/nilable :x {:description "literal x position(s) of lines along the y direction: a position or a vector of them (wins over the mapping)"}
                                             (annotation/positions-schema annotation/position))
                             (schema/nilable :y {:description "literal y position(s) of lines along the x direction: a position or a vector of them (wins over the mapping)"}
                                             (annotation/positions-schema annotation/position))
                             (schema/nilable :slope {:description "literal slope(s) of lines y = intercept + slope * x, a number or a vector of 1 or n (needs :intercept; linear scales only)"}
                                             (annotation/positions-schema schema/finite))
                             (schema/nilable :intercept {:description "literal intercept(s) of lines y = intercept + slope * x, a number or a vector of 1 or n (needs :slope)"}
                                             (annotation/positions-schema schema/finite))))
                   option-rules)
   :mapping [:map
             (schema/nilable :x {:description "x position of a line along the y direction, one per row"} schema/accessor)
             (schema/nilable :y {:description "y position of a line along the x direction, one per row"} schema/accessor)
             (schema/nilable :slope {:description "slope of a line y = intercept + slope * x, one per row (with :intercept)"}
                             schema/accessor)
             (schema/nilable :intercept {:description "intercept of a line y = intercept + slope * x, one per row (with :slope)"}
                             schema/accessor)
             (schema/nilable :color {:description "line colour per group (gives legend keys)"} schema/accessor)
             (schema/nilable :dash {:description "dash style per group (gives legend keys)"} schema/accessor)
             (schema/nilable :group {:description "groups without a visual aesthetic"} schema/accessor)]})

(defn- abline-pairs
  "Pairs the slopes and intercepts of ab lines.

  Parameters:

  - `slopes` (vector): slopes, 1 or n.
  - `intercepts` (vector): intercepts, 1 or n.

  Returns `[[slope intercept] ..]` (a single value repeated, see [[cljplot2.data.annotation/broadcast]]); `[]` when either is empty (a literal one without the other fails the options schema, see [[option-rules]]).

  Throws `ex-info {:key k}` for lengths neither 1 nor n."
  [slopes intercepts]
  (if (or (empty? slopes) (empty? intercepts))
    []
    (annotation/broadcast [:slope :intercept] [slopes intercepts])))

(defn- lines
  "Gives the rule set of one group's rows (data mode) or of the literal options.

  Parameters:

  - `xs`, `ys` (sequences): positions of the vertical and horizontal lines.
  - `slopes`, `intercepts` (sequences): of the ab lines (see [[abline-pairs]]).

  Returns `{:xs [..] :ys [..] :ablines [[slope intercept] ..]}`."
  [xs ys slopes intercepts]
  {:xs (vec xs) :ys (vec ys) :ablines (abline-pairs (vec slopes) (vec intercepts))})

(defn compute
  "Computes the rules of every panel.

  Parameters:

  - `chart` (map): the chart value - `:options` (see the namespace doc).
  - `chart-split` (map): `:groups`, `:mapping` - the chart's own mapping merged.
  - `env` (map): `:theme`, `:coord` (and the keys of [[cljplot2.data.annotation/data-stats]]).

  Returns the chart stats: literal - `{:rules [{:xs :ys :ablines}] :style st}` in every panel with the extents of `:x` / `:y` (see [[cljplot2.data.annotation/literal-stats]]); data - per panel a rule set per group (with `:aesthetics`), rows with an invalid value of a mapped key skipped. `st` - the resolved style.

  Throws `ex-info {:key :coord}` for `:slope` / `:intercept` under `:coord :polar`, `{:key :mapping}` for data mode without a position mapping, and the errors of [[abline-pairs]]; `:dropped` counts per mapped position the rows with an invalid value (data mode)."
  [{:keys [options] :as chart} {:keys [mapping] :as chart-split} {:keys [theme] :as env}]
  (when (and (coord/polar? (:coord env)) (or (some? (:slope options)) (some? (:intercept options))
                                             (some? (:slope mapping)) (some? (:intercept mapping))))
    (schema/fail! "Rule :slope / :intercept lines aren't supported with :coord :polar." {:key :coord :value :slope}))
  (let [st (theme/resolved-style theme chart)
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
                                       (filterv data/finite-point? (map (juxt s i) rows)))
                                     []))
            rule-of (fn [{:keys [rows aesthetics]}]
                      (assoc (lines (column :x rows) (column :y rows) [] []) :ablines (abline-rows rows)
                             :aesthetics aesthetics))]
        (annotation/data-stats chart-split options env
                               (fn [gs]
                                 (let [rules (mapv rule-of gs)]
                                   [{:rules rules :style st} {:x (mapcat :xs rules) :y (mapcat :ys rules)}]))
                               (merge (into {} (for [k [:x :y] :let [acc (get mapping k)] :when acc
                                                     :let [value-of (data/row-accessor k acc)]]
                                                 [k #(not (annotation/position? (value-of %)))]))
                                      (when (and (:slope mapping) (:intercept mapping))
                                        (into {} (for [k [:slope :intercept] :let [value-of (data/row-accessor k (get mapping k))]]
                                                   [k #(not (schema/finite? (value-of %)))])))))))))

(defn- linear!
  "Checks that a finalized position scale is linear (ab lines need it).

  Parameters:

  - `k` (keyword): the scale's key, for the error.
  - `finalized` (map): the finalized scale.

  Returns `nil`; throws `ex-info {:key :slope}` for another kind."
  [k finalized]
  (when-not (= :linear (:kind finalized))
    (schema/fail! "Rule :slope / :intercept needs linear x and y scales." {:key :slope :value [k (:kind finalized)]})))

(defn draw
  "Draws the rules of one panel.

  Parameters:

  - `computed` (map): `:rules`, `:style` (see [[compute]]).
  - `canvas`: the clojure2d canvas.
  - `box` (map): the panel's `:w`, `:h`.
  - `scales` (map): the finalized scales.
  - `env` (map): `:theme`.

  Draws `:x` / `:y` lines across the panel (whole-pixel centred; polar - spokes and circles) and ab lines between the x domain ends (see [[cljplot2.graphics.curve/draw-ablines!]]). Unplaceable positions are skipped.

  Throws `ex-info {:key :slope}` for ab lines on a scale that isn't linear."
  [_chart {:keys [rules style]} canvas {:keys [^double w ^double h]} scales env]
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
                      (let [centred (draw/pixel-center p)]
                        ;; a position on a horizontal axis is a vertical line
                        (if horizontal-axis?
                          (c2d/line canvas centred 0.0 centred h)
                          (c2d/line canvas 0.0 centred w centred))))))]
    (doseq [{:keys [xs ys ablines aesthetics]} rules
            :let [st (group/style style aesthetics scales (:theme env))]
            :when (polyline/set-style! canvas st)]
      (doseq [x xs] (across! x-horizontal? (x-pixel x)))
      (doseq [y ys] (across! y-horizontal? (y-pixel y)))
      (when (seq ablines)
        (linear! :x (:x scales))
        (linear! :y (:y scales))
        (curve/draw-ablines! canvas scales ablines)))))

