(ns cljplot2.chart.stripplot
  "Strip plot (lattice `stripplot`): one point per `:x` value, jittered across its strip - a strip chart (see [[cljplot2.data.strip]]).

  Jitter: Gaussian offsets (fastmath `grandom` on the option `:rng`, a fastmath random generator; without it every
  build creates a fresh one, so builds differ - pass `:rng` for reproducible plots; it is copied per build, see
  [[cljplot2.data.rng/for-build]]) with standard deviation
  `:jitter` / 4 of the item's cross range, clamped to ±`:jitter` / 2 (`:jitter` default 0.25; 0 - all points on the
  centre line). Offsets are drawn at compute and kept in the stats. Jitter of numeric x / y in data units (uniform):
  scatter `:jitter [dx dy]`, see [[cljplot2.chart.scatter]].

  Options (style defaults from theme `[:chart :stripplot]`): `:size`, `:color`, `:stroke-color`, `:stroke-width`,
  `:shape` (as scatter, see [[cljplot2.graphics.shape]]); `:jitter`, `:rng`, `:padding {:x [lo hi]}` (default
  `[0.05 0.05]`), plus the strip options and `:data` / `:mapping`.

  Functions: [[schemas]] - its style, options and mapping. The key glyph is [[cljplot2.chart.legend/point-legend]] (the chart computes and draws through [[cljplot2.chart.strip/compute]] and [[cljplot2.chart.strip/draw]])."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.vector :as v]
            [cljplot2.data.group :as group]
            [cljplot2.data.rng :as rng]
            [cljplot2.data.rows :as data]
            [cljplot2.chart.strip :as strip-chart]
            [cljplot2.data.strip :as strip]
            [cljplot2.graphics.shape :as shape]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries
  "Map entries of the strip plot's point style (theme `[:chart :stripplot]`), also its options."
  [(schema/optional :size {:description "The point size in pixels; a mapped `:size` wins." :cljplot2/example 12} schema/non-neg)
   (schema/optional :color {:description "The colour of the points; a mapped `:color` wins." :cljplot2/value-meanings {:none "No fill."} :cljplot2/example :orange} schema/color)
   (schema/optional :stroke-color {:description "The colour of each point's outline." :cljplot2/value-meanings {:none "No outline."} :cljplot2/example :black} schema/color)
   (schema/optional :stroke-width {:description "The width of the outline in pixels; `0` draws no outline." :cljplot2/example 3 :cljplot2/example-context {:stroke-color :black}} schema/non-neg)
   (schema/optional :shape {:description "The shape of every point; a mapped `:shape` wins." :cljplot2/example \s} schema/shape)])

(def schemas
  "Schemas of the stripplot chart: `:style` (theme `[:chart :stripplot]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into theme/theme-style-entries)
                (into strip/options)
                (conj (schema/optional :jitter {:description "How far the chart scatters the points across their strip, as a fraction of its width: Gaussian offsets with a standard deviation of a quarter of `:jitter`, cut off at half of it. `0` puts the points on the centre line."
                                                :default 0.25 :cljplot2/example 0.8 :cljplot2/example-context {:rng '(r/rng :mersenne 7)}}
                                       schema/fraction)
                      (schema/nilable :rng {:description "The random generator of the jitter. A seeded one gives the same jitter on every build."
                                            :cljplot2/default-doc "An unseeded generator, so each build jitters differently."
                                            :cljplot2/example '(r/rng :mersenne 7)}
                                      rng/schema)
                      strip/padding-option))
   :mapping (strip/values-mapping
              [(schema/nilable :x {:description "The values of horizontal strips, or the levels of vertical ones."} schema/accessor)
               (schema/nilable :y {:description "The levels of horizontal strips, or the values of vertical ones; with no levels the chart draws one strip."}
                               schema/accessor)
               (schema/nilable :color {:description "Splits the rows into groups, each in its own point colour."} schema/accessor)
               (schema/nilable :shape {:description "Splits the rows into groups, each with its own point shape."} schema/accessor)
               (schema/nilable :size {:description "Splits the rows into groups, each with its own point size."} schema/accessor)
               (schema/nilable :group {:description "Splits the rows into groups that look the same, dodged as other groups."} schema/accessor)])})

(def ^{:const true :private true :tag 'double} default-jitter
  "Jitter spread when `:jitter` isn't given, a fraction of an item's cross range."
  0.25)

(defn- jitter-offsets
  "Gives the jitter offsets of an item's points across its strip.

  Parameters:

  - `rng`: a fastmath random generator, drawn from in order.
  - `n` (long): the number of offsets.
  - `jitter` (double): the spread, a fraction of the item's cross range.

  Returns a vector of `n` offsets in two-sided cross units: Gaussian with standard deviation `jitter` / 4, clamped to ±`jitter` / 2; all 0.0 (no draws) for a `jitter` of 0.

  See also [[cljplot2.data.strip/in-slot]]."
  [rng ^long n ^double jitter]
  (let [half (m/* 0.5 jitter)
        sd (m/* 0.25 jitter)]
    ;; realized at once: the draws follow the generator's order
    (vec (repeatedly n #(if (m/zero? jitter) 0.0 (m/constrain (double (r/grandom rng 0.0 sd)) (m/- half) half))))))

(defn- compute-in-strip-frame
  "Computes the strip plot's stats in the strip frame (levels on `:y`).

  Parameters:

  - `chart` (map): the chart value - `:options` (`:jitter` default 0.25, `:rng`, the strip options, style options).
  - `chart-split` (map): the chart's rows split per panel.
  - `env` (map): the compute environment - `:theme`, `:plot-scales` ...

  Returns `{:data :extents :padding :padding-explicit :padding-px :na :dropped}`: per panel `{:items [..] :style st :place p :options o}`, each item with `:offsets` - one jitter offset per value (see [[jitter-offsets]], drawn here so a build draws the same points every render); x extents - the values.

  See also [[cljplot2.data.strip/split]], [[cljplot2.data.rng/for-build]]."
  [{:keys [options] :as chart} chart-split {:keys [theme] :as env}]
  (let [{:keys [place items panels dropped]} (strip/split chart-split options :center env)
        rng (rng/for-build (:rng options))
        jitter (double (data/option options :jitter default-jitter))
        jittered (update-vals items (fn [its] (mapv #(assoc % :offsets (jitter-offsets rng (count (:values %)) jitter)) its)))
        point-style (theme/resolved-style theme chart)]
    {:data (update-vals jittered (fn [its] {:items its :style point-style :place place :options options}))
     :extents (merge-with merge (group/aesthetic-extents panels (:plot-scales env))
                          (strip/extents place jittered (fn [{:keys [values]}] [(v/mn values) (v/mx values)])))
     :padding (strip/padding place options)
     :padding-explicit (strip/explicit-padding place options)
     :padding-px (strip-chart/n-label-room options jittered env)
     :na (group/na-dims panels)
     :dropped dropped}))

(defn- draw-in-strip-frame
  "Draws the points of one panel in the strip frame.

  Parameters:

  - `_chart` (map): the chart value (unused).
  - `computed` (map): the panel's stats - `:items` (with `:offsets`), `:style`, `:place`, `:options`.
  - `canvas`: the clojure2d canvas.
  - `box` (map): the panel box.
  - `scales` (map): finalized scales (in the strip frame).
  - `env` (map): the draw environment - `:theme`.

  Draws each value at its offset across the item's slot in the group style (a group outside its scale's domain is skipped, as are points the scales can't place), then the n labels.

  See also [[cljplot2.chart.strip/placement]]."
  [_chart {:keys [items style place options]} canvas box scales env]
  (let [{:keys [point] :as pl} (strip-chart/placement box scales options place)]
    (c2d/set-stroke-custom canvas {:size (:stroke-width style)})
    (doseq [{:keys [level slot values offsets aesthetics]} items
            :let [st (group/style style aesthetics scales (:theme env))]
            :when st
            [v off] (map vector values offsets)
            :let [^Vec2 p (point v level (strip/in-slot slot (double off)))]
            :when (data/finite-point? p)]
      (shape/draw! canvas (:shape st) p st))
    (strip-chart/draw-n-labels! canvas pl items (fn [{:keys [values]}] (v/mx values)) options scales env)))

(defmethod strip-chart/in-frame-compute :stripplot [chart chart-split env] (compute-in-strip-frame chart chart-split env))

(defmethod strip-chart/in-frame-draw :stripplot [chart computed canvas box scales env]
  (draw-in-strip-frame chart computed canvas box scales env))

