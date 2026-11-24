(ns cljplot2.chart.scatter
  "Scatter chart: one shape per row at its mapped (x, y).

  Options (defaults from theme `[:chart :scatter]`): `:size` (pixels), `:color`, `:stroke-color`, `:stroke-width`,
  `:shape` (see [[cljplot2.graphics.shape]]); mapped `:color`, `:shape`, `:size` override them per point
  (see [[cljplot2.scale.aesthetic]]); `:padding {:x [lo hi] :y [lo hi]}` (fractions of the data span),
  `:descending` (default false - values of a mapped `:order` drawn ascending, see draw order below), plus `:data` and `:mapping`
  (see [[cljplot2.chart]]).

  Jitter (ggplot `geom_jitter`): `:jitter [dx dy]` - uniform offsets in `±dx` / `±dy` data units added at compute, so
  the jittered points make the extents (on a `:log` axis in decades - see
  [[cljplot2.scale.position/stat-transform]]; a value the axis can't place - non-positive on `:log` - stays as it is,
  for the scale to report); a dimension with a positive jitter needs numeric values. `:rng` - a fastmath random
  generator, copied per build (see [[cljplot2.data.rng/for-build]]): one generator gives the same points in every
  build; without it builds differ. Points along a categorical axis: [[cljplot2.chart.stripplot]] (Gaussian jitter
  across each level's strip, a fraction of the strip).

  Draw order: data order, or - with mapping `:order` (an accessor) - ascending order values within each panel
  (later points drawn on top; ties keep data order; rows with a `nil` or invalid order value drawn first).

  Functions: [[cljplot2.chart.common/point-style-entries]] and [[schemas]] - the chart's schemas; [[cljplot2.data.group/kept-value?]] - whether a row value is kept;
  [[compute]] - the panels' points, extents and drops; [[draw]] - one panel's points; the key glyph is [[cljplot2.chart.legend/stroked-point-legend]]."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.vector :as v]
            [cljplot2.chart.common :as common]
            [cljplot2.data.group :as group]
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

(def ^:private position-dims
  "Position dimensions of a scatter point, in the order of a row's values."
  [:x :y])

(def schemas
  "Schemas of the scatter chart: `:style` (theme `[:chart :scatter]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] common/point-style-entries)
   :options (-> [:map {:closed true}]
                (into common/point-style-entries)
                (into data/common-options)
                (into theme/theme-style-entries)
                (conj (schema/optional :padding {:description "padding of the data span per position axis, {:x [lo hi] :y [lo hi]} fractions of the span"
                                                 :cljplot2/default-doc "0.05 on every side"}
                                       data/position-padding)
                      (schema/nilable :descending {:description "draw the values of a mapped :order in descending order (later points on top)"
                                                    :default false}
                                       schema/bool)
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
             (schema/nilable :order {:description "draw order: points drawn by its values (ascending, :descending reverses it), else in data order"}
                             schema/accessor)]})

(defn- order-rows
  "Orders a panel's rows for drawing by their `:order` values.

  Parameters:

  - `rows` (sequence of maps): the panel's rows.
  - `order-of` (function): the `:order` accessor.
  - `descending?` (boolean): sort descending instead of ascending.

  Returns the rows sorted stably by their order values, rows with an invalid (or `nil`) order value first.

  Throws `ex-info {:key :order}` for order values that aren't mutually comparable."
  [rows order-of descending?]
  (let [cmp (if descending? #(compare %2 %1) compare)
        valid-order? #(data/valid-value? (order-of %))]
    (try (concat (remove valid-order? rows) (sort-by order-of cmp (filter valid-order? rows)))
         (catch ClassCastException _
           (schema/fail! "Scatter :order values must be mutually comparable." {:key :order})))))

(defn- jittered
  "Moves position values by uniform random offsets in an axis's coordinates.

  Parameters:

  - `values` (vector of numbers): one dimension's values of a panel.
  - `rng`: the fastmath random generator drawing the offsets.
  - `d` (double): the largest offset, data units in the axis's coordinates.
  - `transform` (map): `:forward`, `:inverse` - the axis's stat transform (see [[cljplot2.scale.position/stat-transform]]).

  Returns a vector of the values each moved by an offset in `[-d d]`; `values` themselves for `d` 0. A value the transform can't place (`##NaN`, e.g. non-positive on `:log`) stays as it is, for the scale to report.

  Throws `ex-info {:key :jitter}` for a non-numeric value."
  [values rng ^double d {:keys [forward inverse]}]
  (if (m/zero? d)
    values
    (mapv (fn [v]
            (when-not (number? v)
              (schema/fail! "Scatter :jitter needs numeric positions; along a categorical axis use chart/stripplot."
                            {:key :jitter :value v}))
            (let [t (double (forward v))]
              (if (m/nan? t) v (inverse (m/+ t (r/drandom rng (m/- d) d))))))
          values)))

(defn- jitter-points
  "Jitters the positions of every panel's points.

  Parameters:

  - `points` (map): panel -> `{:x [..] :y [..] ...}`.
  - `options` (map): the chart's options - `:jitter [dx dy]` (data units; none - no jitter), `:rng` (copied per build, see [[cljplot2.data.rng/for-build]]).
  - `plot-scales` (map): the plot's `:scales` - the axes' stat transforms.

  Returns `points` with x and y moved by uniform offsets within `±dx` / `±dy` in the axes' coordinates (see [[jittered]]); unchanged without `:jitter`."
  [points {:keys [jitter] :as options} plot-scales]
  (if-let [[^double dx ^double dy] jitter]
    (let [g (rng/for-build (:rng options))
          tx (scale/stat-transform (:x plot-scales))
          ty (scale/stat-transform (:y plot-scales))]
      (into {} (map (fn [[panel p]]
                      [panel (assoc p :x (jittered (:x p) g dx tx) :y (jittered (:y p) g dy ty))]))
            points))
    points))

(defn compute
  "Computes the points of every panel.

  Parameters:

  - `chart` (map): the chart value - `:options` (see the namespace doc).
  - `split` (map): `:groups` - panel -> rows, `:mapping` - the merged mapping.
  - `env` (map): `:theme`, `:na-level?` (default true - `nil` aesthetic values are the NA level), `:plot-scales` - the plot's `:scales`.

  Returns `{:data :extents :padding :padding-explicit :na :dropped}`: per panel `{:x [..] :y [..] :style st}` plus a vector per mapped aesthetic (`:color`, `:shape`, `:size`; `nil` - NA) of the rows with valid positions and kept aesthetic values (see [[cljplot2.data.group/kept-value?]]), in draw order, jittered with `:jitter`, `st` the resolved style; extents per panel (aesthetics: value extents of non-NA values); `:na` - aesthetics with NA values; `:dropped` - per dimension the rows dropped (aesthetic values outside an explicit domain count).

  Throws `ex-info {:key :mapping}` when a mapped dimension has no valid value in a non-empty data set (usually a misspelt mapping), `{:key :order}` for incomparable order values, `{:key :jitter}` for jitter on non-numeric positions."
  [{:keys [options] :as chart} {:keys [groups mapping]} {:keys [theme na-level? plot-scales] :or {na-level? true}}]
  (let [st (theme/resolved-style theme chart)
        descending? (data/option options :descending false)
        order-of (when (some? (:order mapping)) (data/row-accessor :order (:order mapping)))
        ordered-groups (if order-of (update-vals groups #(order-rows % order-of descending?)) groups)
        dims (into position-dims (aesthetic/mapped-dims mapping))
        accessors (mapv #(data/row-accessor % (mapping %)) dims)
        values (update-vals ordered-groups (fn [rows] (mapv (fn [row] (mapv #(% row) accessors)) rows)))
        all-values (apply concat (vals values))
        dropped (into {} (map-indexed (fn [i k] [k (count (remove #(group/kept-value? k na-level? plot-scales (% i)) all-values))]) dims))
        keep-row? (fn [vs] (every? identity (map-indexed (fn [i k] (group/kept-value? k na-level? plot-scales (vs i))) dims)))
        kept-points (update-vals values (fn [vs] (let [kept (filterv keep-row? vs)]
                                                   (into {:style st}
                                                         (map-indexed (fn [i k] [k (mapv #(% i) kept)]))
                                                         dims))))
        points (jitter-points kept-points options plot-scales)]
    (data/check-mapped! all-values (map-indexed (fn [i k] [k #(% i)]) dims)
                        (fn [k v] (aesthetic/valid-value? k (get plot-scales k) v)))
    (into (data/padding-stats data/default-padding {} options)
          {:data points
           :extents (update-vals points (fn [p] (into {:x (data/extent :x (:x p)) :y (data/extent :y (:y p))}
                                                      (map (fn [k] [k (aesthetic/value-extent k (get plot-scales k) (remove nil? (p k)))]))
                                                      (drop 2 dims))))
           :na (set (filter (fn [k] (some #(some nil? (% k)) (vals points))) (drop 2 dims)))
           :dropped dropped})))

(defn- na-attributes
  "Gives the attributes drawing NA (`nil`) aesthetic values.

  Parameters:

  - `style` (map): the chart's resolved style.
  - `theme` (map): the merged theme.

  Returns `{:color :shape :size}`: the theme's `[:palette :na]` colour and the style's own shape and size."
  [style theme]
  {:color (get-in theme [:palette :na]) :shape (:shape style) :size (:size style)})

(defn draw
  "Draws the points of one panel.

  Parameters:

  - `computed` (map): the panel's points - `:x`, `:y`, `:style` and a vector per mapped aesthetic (see [[compute]]).
  - `canvas`: the clojure2d canvas.
  - `scales` (map): the finalized scales - positions place a point, aesthetic scales give its colour, shape and size.
  - `env` (map): `:theme`.

  Draws nothing for no points (there may be no scales then). Points the scales can't place - a position `##NaN` (e.g. outside an explicit bands domain) or an aesthetic value outside its domain (`nil` attribute) - are skipped."
  [_chart {:keys [x y style] :as computed} canvas _box scales env]
  (when (seq x)
    (let [{:keys [^double stroke-width]} style
          projection (scale/projection scales)
          na (na-attributes style (:theme env))
          mapped (vec (for [k aesthetic/dims :when (contains? computed k)]
                        [k (aesthetic/attribute-fn k (get scales k) (na k)) (computed k)]))
          point-style (if (seq mapped)
                        (fn [^long i] (reduce (fn [st [k f vs]]
                                                (if-some [a (f (nth vs i))] (assoc st k a) (reduced nil)))
                                              style mapped))
                        (constantly style))]
      (c2d/set-stroke-custom canvas {:size stroke-width})
      (dorun (map-indexed (fn [^long i vx]
                            (let [^Vec2 p (projection vx (nth y i))]
                              (when (data/finite-point? p)
                                (when-let [st (point-style i)]
                                  (shape/draw! canvas (:shape st) p st)))))
                          x)))))

