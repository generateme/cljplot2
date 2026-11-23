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

  Equivalents: Vega-Lite `facet_bullet`, d3 bullet charts.

  Functions: [[compute]] and [[draw]] - the chart contract (through the strip frame); [[bullet-values]] - an item's values; [[shade]] - the colour of a ranked value."
  (:require [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.data.rows :as data]
            [cljplot2.data.strip :as strip]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private value-keys
  "Mapping keys of a bullet's values."
  [:ranges :measures :markers])

(def ^:private style-entries
  "Map entries of the bullet style (theme `[:chart :bullet]`, also options)."
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
                (into theme/theme-style-entries)
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

(defn- as-values
  "Gives a bullet value as a vector of doubles.

  Parameters:

  - `v`: `nil`, a finite number or a sequential collection of finite numbers.

  Returns `[]` for `nil`, `[v]` for a number, the collection's numbers in order; `nil` for anything else (a set, a map, a string ...)."
  [v]
  (cond
    (nil? v) []
    (schema/finite? v) [(double v)]
    (and (sequential? v) (every? schema/finite? v)) (mapv double v)))

(defn bullet-values
  "Reads the values of a bullet item.

  Parameters:

  - `item` (map): a strip item - `:rows` (exactly one row), `:level`, `:aesthetics`.
  - `mapping` (map): the merged mapping; `:ranges`, `:measures`, `:markers` accessors (`nil` - none).

  Returns `{:ranges [..] :measures [..] :markers [..]}`, doubles in the given order.

  Throws `ex-info {:key :mapping}` for two or more rows of one level and group, or a value that isn't a finite number or a sequential collection of them.

  See also [[compute]]."
  [{:keys [rows level aesthetics]} mapping]
  (when (m/not== 1 (count rows))
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
  "Gives the x padding of bullet items: [[cljplot2.data.rows/axis-padding]] on a side holding values, 0 on the side of 0, so the bars start at the axis.

  Parameters:

  - `items` (map): panel -> items with their values - every panel together (a chart has one padding).

  Returns `[lo hi]`; `[0 0]` without values off 0."
  [items]
  (let [vs (mapcat (fn [item] (mapcat item value-keys)) (mapcat val items))
        [lo-pad hi-pad] data/axis-padding]
    [(if (some (fn [^double v] (m/neg? v)) vs) lo-pad 0.0)
     (if (some (fn [^double v] (m/pos? v)) vs) hi-pad 0.0)]))

(defn- padding
  "Gives the position padding of the chart.

  Parameters:

  - `place`: the chart's placement.
  - `options` (map): the chart options (`:padding`).
  - `items` (map): panel -> items with their values.

  Returns the strip padding (see [[cljplot2.data.strip/padding]]) with x from [[zero-side-padding]] unless `:padding` gives x."
  [place options items]
  (cond-> (strip/padding place options)
    (nil? (get-in options [:padding :x])) (assoc :x (zero-side-padding items))))

(defn- value-range
  "Gives the x range of a bullet item: 0 and every value.

  Parameters:

  - `item` (map): an item with its values.

  Returns `[lo hi]`."
  [item]
  (let [vs (into [0.0] (mapcat item value-keys))]
    [(v/mn vs) (v/mx vs)]))

(defn- compute-in-strip-frame
  "Computes the chart's stats in the strip frame.

  Parameters:

  - `chart` (map): the chart value.
  - `chart-split` (map): `:groups`, `:mapping`.
  - `env` (map): the compute environment (`:theme` ...).

  Returns per panel `{:items [item ...] :style st :place p :options o}`, items with [[bullet-values]]; extents (x - 0 and every value), padding, `:dropped` (no aesthetics - no aesthetic extents or NA dimensions).

  Throws as [[bullet-values]]."
  [{:keys [options] :as chart} {:keys [mapping] :as chart-split} {:keys [theme] :as env}]
  (let [{:keys [place dropped] split-items :items} (strip/split chart-split (assoc options ::strip/count-rows true ::strip/rows true)
                                                                 :center env)
        items (update-vals split-items (fn [its] (mapv #(dissoc (merge % (bullet-values % mapping)) :rows) its)))
        st (theme/resolved-style theme chart)]
    {:data (update-vals items (fn [its] {:items its :style st :place place :options options}))
     :extents (strip/extents place items value-range)
     :padding (padding place options items)
     :padding-explicit (strip/explicit-padding place options)
     :dropped dropped}))

(defn shade
  "Gives the colour of a ranked value of a bullet kind (ranges or measures).

  Parameters:

  - `[first-color last-color]`: the colours of the first and the last rank.
  - `i` (long): the value's rank, from 0.
  - `n` (long): the number of values.

  Returns `first-color` for `n` below 2, else the colour `i / (n - 1)` of the way between them."
  [[first-color last-color] ^long i ^long n]
  (if (m/< n 2) first-color (c/lerp first-color last-color (m// (double i) (m/long-dec n)))))

(defn- bar!
  "Fills a bar from 0 to a value across a cross range.

  Parameters:

  - `canvas`: the clojure2d canvas.
  - `p` (function): value, level, cross -> pixel `Vec2`.
  - `v` (double): the value.
  - `level`: the bar's level.
  - `c0`, `c1` (doubles): the cross range.

  Nothing is drawn when a corner can't be placed."
  [canvas p v level c0 c1]
  (let [^Vec2 a (p 0.0 level c0)
        ^Vec2 b (p v level c1)]
    (when (and (m/valid-double? (.x a)) (m/valid-double? (.y a))
               (m/valid-double? (.x b)) (m/valid-double? (.y b)))
      (c2d/rect canvas (m/min (.x a) (.x b)) (m/min (.y a) (.y b))
                (m/abs (m/- (.x b) (.x a))) (m/abs (m/- (.y b) (.y a)))))))

(defn- draw-bars!
  "Fills the bars of a bullet kind's values, the largest magnitude first so every bar stays visible.

  Parameters:

  - `canvas`: the clojure2d canvas.
  - `p` (function): value, level, cross -> pixel `Vec2`.
  - `level`: the item's level.
  - `values` (vector of doubles): the values.
  - `colors`: `[first last]` colours (see [[shade]]).
  - `rank-of` (function): value indices -> the indices in rank order.
  - `c0` (double): the cross centre.
  - `half` (double): the cross half-width.

  See also [[bar!]]."
  [canvas p level values colors rank-of c0 half]
  (let [n (count values)
        rank (zipmap (rank-of (range n)) (range n))
        cross-lo (m/- (double c0) (double half))
        cross-hi (m/+ (double c0) (double half))]
    (doseq [i (sort-by #(m/- (m/abs (double (nth values %)))) (range n))]
      (c2d/set-color canvas (shade colors (long (rank i)) n))
      (bar! canvas p (nth values i) level cross-lo cross-hi))))

(defn- draw-in-strip-frame
  "Draws the bullets of one panel in the strip frame.

  Parameters:

  - `_chart` (map): the chart value.
  - `computed` (map): the panel's `:items`, `:style`, `:place`, `:options`.
  - `canvas`: the clojure2d canvas.
  - `box` (map): the panel box.
  - `scales` (map): the finalized position scales (strip frame).
  - `_env` (map): the drawing environment.

  Ranges are drawn across the item slot, measures `:measure-width` of it, markers as lines `:marker-length` of it (none for a zero `:marker-width`)."
  [_chart {:keys [items style place options]} canvas box scales _env]
  (let [{:keys [point]} (strip/placement box scales options place)]
    (doseq [{:keys [level slot ranges measures markers]} items
            :let [c0 (strip/in-slot slot 0.0)
                  half (strip/slot-half slot)]]
      (draw-bars! canvas point level ranges (:range-colors style) (fn [is] (sort-by #(nth ranges %) is)) c0 half)
      (draw-bars! canvas point level measures (:measure-colors style) identity c0
                  (m/* half (double (:measure-width style))))
      (when (m/pos? (double (:marker-width style)))
        (c2d/set-color canvas (:marker-color style))
        (c2d/set-stroke-custom canvas {:size (double (:marker-width style)) :cap :butt})
        (let [reach (m/* half (double (:marker-length style)))]
          (doseq [v markers
                  :let [^Vec2 a (point v level (m/- c0 reach))
                        ^Vec2 b (point v level (m/+ c0 reach))]
                  :when (and (m/valid-double? (.x a)) (m/valid-double? (.y a)) (m/valid-double? (.y b)))]
            (c2d/line canvas (.x a) (.y a) (.x b) (.y b))))))))

(defn compute
  "Computes the chart's stats with its levels on the categorical axis.

  Parameters:

  - `chart` (map): the chart value.
  - `chart-split` (map): `:groups`, `:mapping`.
  - `env` (map): the compute environment.

  Returns the stats of [[compute-in-strip-frame]] oriented to the level axis (see [[cljplot2.data.strip/compute-oriented]])."
  [chart chart-split env]
  (strip/compute-oriented compute-in-strip-frame chart chart-split env))

(defn draw
  "Draws one panel of the chart.

  Parameters:

  - `chart` (map): the chart value.
  - `computed` (map): the panel's computed data.
  - `canvas`, `box`, `scales`, `env`: as the chart contract's draw.

  Draws [[draw-in-strip-frame]] in the strip frame (see [[cljplot2.data.strip/draw-oriented]])."
  [chart computed canvas box scales env]
  (strip/draw-oriented draw-in-strip-frame chart computed canvas box scales env))
