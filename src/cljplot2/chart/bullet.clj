(ns cljplot2.chart.bullet
  "Bullet chart (S. Few): per level, qualitative ranges as shaded bands, measures as narrower bars and markers as
  ticks across them - a strip chart (see [[cljplot2.data.strip]]: levels on the categorical axis, `:level-axis`;
  placement `:place`, `:width`, `:thickness`, groups `:position`). Axes below are named in the strip frame - `:x`
  the values, `:y` the levels; exchange them for vertical bullets (levels on `:x`).

  Mapping: `:ranges`, `:measures`, `:markers` (at least one) - each a finite number or a sequential collection of
  them (vector or list - the measures' order sets their colours; `nil` - none), read from exactly one row per level
  and group (Vega-Lite `facet_bullet`: `{:ranges [150 225 300] :measures [220 270] :markers [250]}`); `:x` isn't
  read; optional `:y` levels (without them one bullet, placed `:center`). Give every measure its own value scale
  with facets: `{:mapping {:facet :title} :facets {:columns 1 :scales {:x :free}}}`.

  Drawn from 0 to each value: the ranges across the whole item slot, largest first, shaded from the first colour of
  `:range-colors` (the smallest range) to the second (the largest); the measures `:measure-width` of the slot
  (fraction), largest first so every one stays visible, coloured from the first colour of `:measure-colors` (the
  first measure - the current value) to the second (the last); the markers as lines of `:marker-width` pixels across
  `:marker-length` of the slot in `:marker-color`. One value of a kind takes the first colour. Groups (`:group`) are
  dodged as other strips. No aesthetics: a mapped `:color`, `:shape`, `:size`, `:dash` or `:pattern` is a schema
  error (set it to `nil` when the plot maps it for other charts); no legend key, no n labels (`:n-label`).

  Options (style defaults from theme `[:chart :bullet]`): `:range-colors`, `:measure-colors` (two colours each),
  `:measure-width`, `:marker-color`, `:marker-width`, `:marker-length`; `:padding {:x [lo hi]}` (default 0.05 on a
  side holding values, 0 on the side of 0 - the bars start at the axis; one padding for the chart, from the values
  of all panels: with free facet scales a panel of positive values beside one of negative values gets 0.05 on both
  sides too), plus the strip options (without `:n-label`,
  `:n-format`) and `:data` / `:mapping`. The x extent covers 0 and every value.
  Throws `ex-info {:key :mapping}` for two or more rows of one level and group or a value that isn't a finite number
  (or a sequential collection of them).

  Equivalents: Vega-Lite `facet_bullet`, d3 bullet charts."
  (:require [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.data.rows :as data]
            [cljplot2.data.strip :as strip]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private value-keys
  "Mapping keys of a bullet's values."
  [:ranges :measures :markers])

(def ^:private style-entries
  [(schema/optional :range-colors {:description "[first last] colours shading the ranges from the smallest to the largest"}
                    [:tuple schema/color schema/color])
   (schema/optional :measure-colors {:description "[first last] colours shading the measures from the first (the current value) to the last"}
                    [:tuple schema/color schema/color])
   (schema/optional :measure-width {:description "measure bar width, a fraction of the item slot"} schema/fraction)
   (schema/optional :marker-color {:description "marker line colour"} schema/color)
   (schema/optional :marker-width {:description "marker line width in pixels (0 - no markers)"} schema/non-neg)
   (schema/optional :marker-length {:description "marker line length, a fraction of the item slot"} schema/fraction)])

(def schemas
  "Schemas of the bullet chart: `:style` (theme `[:chart :bullet]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into (remove #(#{:n-label :n-format} (first %)) strip/options))
                (conj (schema/optional :padding {:description "padding of the data span per position axis, {:x [lo hi] :y [lo hi]} fractions of the span (strip frame: :x the values)"
                                                 :cljplot2/default-doc "values 0.05 on a side holding values, 0 on the side of 0"}
                                       data/position-padding)))
   :mapping (schema/with-rules [:map
                                (schema/nilable :ranges {:description "qualitative ranges of a level: a number or a vector of them (one row per level and group)"}
                                                schema/accessor)
                                (schema/nilable :measures {:description "measures of a level: a number or a vector of them, the first the current value"}
                                                schema/accessor)
                                (schema/nilable :markers {:description "marker values of a level: a number or a vector of them"}
                                                schema/accessor)
                                (schema/nilable :y {:description "levels on the categorical axis (absent - one bullet)"} schema/accessor)
                                (schema/nilable :x {:description "levels of vertical bullets (values aren't read from it)"} schema/accessor)
                                (schema/nilable :group {:description "groups without a visual aesthetic (dodged as other groups)"} schema/accessor)]
              [#(when-not (some (fn [k] (some? (get % k))) value-keys)
                  "should map :ranges, :measures or :markers")
               nil]
              [#(when-let [mapped (seq (filter (fn [k] (some? (get % k))) aesthetic/dims))]
                  (str "doesn't take aesthetics, got " (pr-str (vec mapped))
                       " mapped (set them to nil when the plot maps them)"))
               nil])})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :bullet]) (select-keys options (map first style-entries))))

(defn- as-values
  "`v` - `nil`, a finite number or a sequential collection of them - as a vector of doubles; `nil` for anything
  else (a set, a map, a string ...)."
  [v]
  (cond
    (nil? v) []
    (schema/finite? v) [(double v)]
    (and (sequential? v) (every? schema/finite? v)) (mapv double v)))

(defn bullet-values
  "`{:ranges [..] :measures [..] :markers [..]}` (doubles, in the given order) of a bullet item (`:rows` - exactly
  one) read through `mapping`. Throws `ex-info {:key :mapping}` for two or more rows of one level and group or a
  value that isn't a finite number or a sequential collection of them."
  [{:keys [rows level aesthetics]} mapping]
  (when (not= 1 (count rows))
    (schema/fail! "Bullet takes one row per level and group." {:key :mapping :level level :group aesthetics
                                                               :value (count rows)}))
  (let [row (first rows)]
    (into {} (for [k value-keys
                   :let [raw (when-let [acc (get mapping k)] ((data/row-accessor k acc) row))
                         vs (as-values raw)]]
               (if vs
                 [k vs]
                 (schema/fail! (str "Bullet " k " should be a finite number or a sequential collection of them.")
                               {:key :mapping :level level :value raw}))))))

(defn- zero-side-padding
  "x padding `[lo hi]` of bullet `items` (panel -> items, all panels together - a chart has one padding): 0.05 on a
  side holding values, 0 on the side of 0 - so the bars start at the axis (`[0 0]` without values off 0)."
  [items]
  (let [vs (mapcat (fn [item] (mapcat item value-keys)) (mapcat val items))]
    [(if (some neg? vs) 0.05 0.0) (if (some pos? vs) 0.05 0.0)]))

(defn- padding
  "Position padding: the strip padding with x from [[zero-side-padding]] unless the `:padding` option gives x."
  [place options items]
  (cond-> (strip/padding place options)
    (nil? (get-in options [:padding :x])) (assoc :x (zero-side-padding items))))

(defn- compute-in-strip-frame
  "Per panel: `{:items [item ...] :style st :place p :options o}`, items with [[bullet-values]]; extents (x - 0 and
  every value), padding, `:dropped` (no aesthetics - no aesthetic extents or NA dimensions)."
  [{:keys [options]} {:keys [mapping] :as chart-split} {:keys [theme] :as env}]
  (let [{:keys [place items dropped]} (strip/split chart-split (assoc options ::strip/count-rows true ::strip/rows true)
                                                          :center env)
        items (update-vals items (fn [its] (mapv #(dissoc (merge % (bullet-values % mapping)) :rows) its)))
        x-range (fn [item] (let [vs (into [0.0] (mapcat item value-keys))] [(reduce min vs) (reduce max vs)]))]
    {:data (update-vals items (fn [its] {:items its :style (style options theme) :place place :options options}))
     :extents (strip/extents place items x-range)
     :padding (padding place options items)
     :padding-explicit (strip/explicit-padding place options)
     :dropped dropped}))

(defn shade
  "Colour of the `i`-th of `n` values between `[first-color last-color]`: the first for `n` 1."
  [[first-color last-color] ^long i ^long n]
  (if (< n 2) first-color (c/lerp first-color last-color (/ (double i) (double (dec n))))))

(defn- bar!
  "Fill the bar from 0 to `v` across the cross range `[c0 c1]` of `level` (`p` - value, level, cross -> pixel)."
  [canvas p v level c0 c1]
  (let [^Vec2 a (p 0.0 level c0)
        ^Vec2 b (p v level c1)]
    (when (and (m/valid-double? (.x a)) (m/valid-double? (.y a))
               (m/valid-double? (.x b)) (m/valid-double? (.y b)))
      (c2d/rect canvas (m/min (.x a) (.x b)) (m/min (.y a) (.y b))
                (m/abs (- (.x b) (.x a))) (m/abs (- (.y b) (.y a)))))))

(defn- draw-bars!
  "Fill the bars of `values` (largest magnitude first), the i-th coloured by [[shade]] of `colors` at its rank in
  `rank-of` order, across the cross half-width `half` around `c0`."
  [canvas p level values colors rank-of c0 half]
  (let [n (count values)
        rank (zipmap (rank-of (range n)) (range n))]
    (doseq [i (sort-by #(- (m/abs (double (nth values %)))) (range n))]
      (c2d/set-color canvas (shade colors (long (rank i)) n))
      (bar! canvas p (nth values i) level (- (double c0) (double half)) (+ (double c0) (double half))))))

(defn- draw-in-strip-frame
  "Draw the bullets of one panel."
  [_chart {:keys [items style place options]} canvas box scales _env]
  (let [{:keys [point]} (strip/placement box scales options place)]
    (doseq [{:keys [level slot ranges measures markers]} items
            :let [c0 (strip/in-slot slot 0.0)
                  half (strip/slot-half slot)]]
      (draw-bars! canvas point level ranges (:range-colors style) (fn [is] (sort-by #(nth ranges %) is)) c0 half)
      (draw-bars! canvas point level measures (:measure-colors style) identity c0
                  (* half (double (:measure-width style))))
      (when (pos? (double (:marker-width style)))
        (c2d/set-color canvas (:marker-color style))
        (c2d/set-stroke-custom canvas {:size (double (:marker-width style)) :cap :butt})
        (let [reach (* half (double (:marker-length style)))]
          (doseq [v markers
                  :let [^Vec2 a (point v level (- c0 reach))
                        ^Vec2 b (point v level (+ c0 reach))]
                  :when (and (m/valid-double? (.x a)) (m/valid-double? (.y a)) (m/valid-double? (.y b)))]
            (c2d/line canvas (.x a) (.y a) (.x b) (.y b))))))))

(defn compute
  "Stats of the chart with its levels on the categorical axis (see [[cljplot2.data.strip/compute-oriented]]):
  [[compute-in-strip-frame]] in the strip frame."
  [chart chart-split env]
  (strip/compute-oriented compute-in-strip-frame chart chart-split env))

(defn draw
  "Draw one panel (see [[cljplot2.data.strip/draw-oriented]]): [[draw-in-strip-frame]] in the strip frame."
  [chart computed canvas box scales env]
  (strip/draw-oriented draw-in-strip-frame chart computed canvas box scales env))
