(ns cljplot2.chart.scatter
  "Scatter chart: one shape per row at its mapped (x, y).

  Options (defaults from theme `[:chart :scatter]`): `:size` (pixels), `:color`, `:stroke-color`, `:stroke-width`,
  `:shape` (see [[cljplot2.graphics.shape]]); mapped `:color`, `:shape`, `:size` override them per point
  (see [[cljplot2.scale.aesthetic]]); `:padding {:x [lo hi] :y [lo hi]}` (fractions of the data span),
  `:order-direction` (`:ascending` default or `:descending`, see draw order below), plus `:data` and `:mapping`
  (see [[cljplot2.chart]]).

  Jitter (ggplot `geom_jitter`): `:jitter [dx dy]` - uniform offsets in `±dx` / `±dy` data units added at compute, so
  the jittered points make the extents (on a `:log` axis in decades - see
  [[cljplot2.scale.position/stat-transform]]; a value the axis can't place - non-positive on `:log` - stays as it is,
  for the scale to report); a dimension with a positive jitter needs numeric values. `:rng` - a fastmath random
  generator, copied per build (see [[cljplot2.data.rng/for-build]]): one generator gives the same points in every
  build; without it builds differ. Points along a categorical axis: [[cljplot2.chart.stripplot]] (Gaussian jitter
  across each level's strip, a fraction of the strip).

  Draw order: data order, or - with mapping `:order` (an accessor) - ascending order values within each panel
  (later points drawn on top; ties keep data order; rows with a `nil` or invalid order value drawn first)."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.vector :as v]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.data.rng :as rng]
            [cljplot2.data.rows :as data]
            [cljplot2.scale.position :as scale]
            [cljplot2.graphics.shape :as shape]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private position-dims [:x :y])

(def style-entries
  "Map entries of a point style (all optional; defaults from the theme)."
  [(schema/optional :size {:description "point size in pixels (a mapped :size wins)"} schema/non-neg)
   (schema/optional :color {:description "point colour (a mapped :color wins)"} schema/color)
   (schema/optional :stroke-color {:description "outline colour: a colour or :none"} schema/color)
   (schema/optional :stroke-width {:description "outline width in pixels"} schema/non-neg)
   (schema/optional :shape {:description "point shape (a mapped :shape wins)"} schema/shape)])

(def schemas
  "Schemas of the scatter chart: `:style` (theme `[:chart :scatter]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into theme/theme-style-entries)
                (conj (schema/optional :padding {:description "padding of the data span per position axis, {:x [lo hi] :y [lo hi]} fractions of the span"
                                                 :cljplot2/default-doc "0.05 on every side"}
                                       data/position-padding)
                      (schema/optional :order-direction {:description "draw order of the values of a mapped :order (later points on top)"
                                                         :default :ascending}
                                       [:enum :ascending :descending])
                      (schema/optional :jitter {:description "[dx dy] - uniform random offsets within ±dx / ±dy data units added to the positions"}
                                       [:tuple schema/non-neg schema/non-neg])
                      (schema/nilable :rng {:description "random number generator for jitter (seeded for reproducible plots)"
                                            :cljplot2/default-doc "an unseeded generator (builds differ)"}
                                      rng/schema)))
   :mapping [:map
             [:x {:description "x position (data units)"} schema/accessor]
             [:y {:description "y position (data units)"} schema/accessor]
             (schema/nilable :color {:description "point colour per row"} schema/accessor)
             (schema/nilable :shape {:description "point shape per row"} schema/accessor)
             (schema/nilable :size {:description "point size per row"} schema/accessor)
             (schema/nilable :order {:description "draw order: points drawn by its values (see :order-direction), else in data order"}
                             schema/accessor)]})

(defn- style
  "Chart options merged over the theme defaults."
  [options theme]
  (merge (theme/chart-style theme :scatter options) (select-keys options [:size :color :stroke-color :stroke-width :shape])))

(defn kept?
  "Is `v` of dimension `k` kept: valid (for an aesthetic: kept by its plot scale, see
  [[cljplot2.scale.aesthetic/kept-value?]]), or `nil` (NA) for an aesthetic when `na-level?`."
  [k na-level? plot-scales v]
  (boolean (if (aesthetic/dim? k)
             (or (aesthetic/kept-value? k (get plot-scales k {}) v) (and na-level? (nil? v)))
             (data/valid-value? v))))

(defn- order-rows
  "`rows` in draw order for the `:order` accessor `order-of` and `direction`: stable, rows with an invalid order value
  first. Throws `ex-info {:key :order}` for incomparable order values."
  [rows order-of direction]
  (let [cmp (if (= direction :descending) #(compare %2 %1) compare)
        {valid true invalid false} (group-by #(data/valid-value? (order-of %)) rows)]
    (try (into (vec invalid) (sort-by order-of cmp valid))
         (catch ClassCastException _
           (schema/fail! "Scatter :order values must be mutually comparable." {:key :order})))))

(defn- jittered
  "`values` (numbers) each moved by a uniform offset in `±d` drawn from `rng`, in the coordinates of `transform`
  (see [[cljplot2.scale.position/stat-transform]]); `values` themselves for `d` 0; a value the transform can't place
  (`##NaN`, e.g. non-positive on `:log`) unchanged, so the scale reports it. Throws `ex-info {:key :jitter}` for a
  non-numeric value."
  [values rng ^double d {:keys [forward inverse]}]
  (if (zero? d)
    values
    (mapv (fn [v]
            (when-not (number? v)
              (schema/fail! "Scatter :jitter needs numeric positions; along a categorical axis use chart/stripplot."
                            {:key :jitter :value v}))
            (let [t (double (forward v))]
              (if (m/nan? t) v (inverse (+ t (r/drandom rng (- d) d))))))
          values)))

(defn- jitter-points
  "Panel `points` with x and y jittered by `[dx dy]` (`:jitter`) from the generator of `:rng` (one per build), in the
  plot's axis coordinates (`plot-scales`); unchanged without `:jitter`."
  [points {:keys [jitter] :as options} plot-scales]
  (if-let [[dx dy] jitter]
    (let [g (rng/for-build (:rng options))
          tx (scale/stat-transform (:x plot-scales))
          ty (scale/stat-transform (:y plot-scales))]
      (into {} (map (fn [[panel p]]
                      [panel (assoc p :x (jittered (:x p) g (double dx) tx) :y (jittered (:y p) g (double dy) ty))]))
            points))
    points))

(defn compute
  "Per panel: points (rows with valid mapped x and y and kept aesthetic values, see [[cljplot2.scale.aesthetic]]) as
  `{:x [..] :y [..] :style style}` plus a vector per mapped aesthetic (`:color`, `:shape`, `:size`; `nil` = NA),
  `style` being the validated options merged over the theme; extents per panel (aesthetics: value extents of
  non-NA values), padding, `:na` (set of aesthetics with NA values) and per-dimension counts of dropped rows (zero
  when nothing is dropped; aesthetic values outside an explicit plot scale domain count as dropped).
  `env` `:na-level?` (default true) keeps `nil` aesthetic values as NA; `:plot-scales` - the plot's `:scales`.
  Throws `ex-info` with `{:key :mapping}` when a mapped dimension has no valid value in a non-empty data set
  (usually a misspelt mapping)."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme na-level? plot-scales] :or {na-level? true}}]
  (let [st (style options theme)
        padding (merge {:x [0.05 0.05] :y [0.05 0.05]} (:padding options))
        direction (get options :order-direction :ascending)
        order-of (when (some? (:order mapping)) (data/row-accessor :order (:order mapping)))
        groups (if order-of (update-vals groups #(order-rows % order-of direction)) groups)
        dims (into position-dims (aesthetic/mapped-dims mapping))
        accessors (mapv #(data/row-accessor % (mapping %)) dims)
        values (update-vals groups (fn [rows] (mapv (fn [row] (mapv #(% row) accessors)) rows)))
        all-values (apply concat (vals values))
        row-count (count all-values)
        dropped (into {} (map-indexed (fn [i k] [k (count (remove #(kept? k na-level? plot-scales (% i)) all-values))]) dims))
        keep-row? (fn [vs] (every? true? (map-indexed (fn [i k] (kept? k na-level? plot-scales (vs i))) dims)))
        points (update-vals values (fn [vs] (let [kept (filterv keep-row? vs)]
                                              (into {:style st}
                                                    (map-indexed (fn [i k] [k (mapv #(% i) kept)]))
                                                    dims))))
        points (jitter-points points options plot-scales)]
    (doseq [[i k] (map-indexed vector dims)]
      (when (and (pos? row-count) (not-any? #(aesthetic/valid-value? k (get plot-scales k) (% i)) all-values))
        (schema/fail! "No valid values for a mapped dimension - check the mapping." {:key :mapping :value k :rows row-count})))
    {:data points
     :extents (update-vals points (fn [p] (into {:x (data/extent :x (:x p)) :y (data/extent :y (:y p))}
                                                (map (fn [k] [k (aesthetic/value-extent k (get plot-scales k) (remove nil? (p k)))]))
                                                (drop 2 dims))))
     :padding padding
     :padding-explicit (data/explicit-padding {} options)
     :na (set (filter (fn [k] (some #(some nil? (% k)) (vals points))) (drop 2 dims)))
     :dropped dropped}))

(defn- na-attributes
  "Attribute used for NA values per aesthetic: theme `[:palette :na]` color; the chart's own shape and size."
  [style theme]
  {:color (get-in theme [:palette :na]) :shape (:shape style) :size (:size style)})

(defn draw
  "Draw the points of one panel (`computed` - `{:x [..] :y [..] :style}` and aesthetic vectors) on `canvas` with the
  finalized `scales` (aesthetic scales map values to the point's color, shape, size). No points draw nothing
  (there may be no scales then); points the scales can't place - a position `##NaN` (e.g. outside an explicit bands
  domain) or an aesthetic value outside its domain (`nil` attribute) - are skipped."
  [_chart {:keys [x y style] :as computed} canvas _box scales env]
  (when (seq x)
    (let [{:keys [stroke-width]} style
          projection (scale/projection scales)
          na (na-attributes style (:theme env))
          mapped (vec (for [k aesthetic/dims :when (contains? computed k)]
                        [k (aesthetic/attribute-fn k (get scales k) (na k)) (computed k)]))
          point-style (if (seq mapped)
                        (fn [^long i] (reduce (fn [st [k f vs]]
                                                (if-some [a (f (nth vs i))] (assoc st k a) (reduced nil)))
                                              style mapped))
                        (constantly style))]
      (c2d/set-stroke-custom canvas {:size (double stroke-width)})
      (dorun (map-indexed (fn [^long i vx]
                            (let [^Vec2 p (projection vx (nth y i))]
                              (when-not (or (m/nan? (.x p)) (m/nan? (.y p)))
                                (when-let [st (point-style i)]
                                  (shape/draw! canvas (:shape st) p st)))))
                          x)))))

(defn legend
  "Draw the key glyph: the chart's shape (style from options and theme, `attributes` - mapped color, shape, size -
  merged over) fitted and centred in `box` (see [[cljplot2.graphics.shape/draw-key!]])."
  [{:keys [options]} attributes canvas box env]
  (let [st (merge (style options (:theme env)) attributes)]
    (c2d/set-stroke-custom canvas {:size (double (:stroke-width st))})
    (shape/draw-key! canvas (:shape st) box st)))
