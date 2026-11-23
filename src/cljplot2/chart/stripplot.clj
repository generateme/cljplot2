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

  Functions: [[jitter-offsets]] - the jitter of an item's points; [[compute]], [[draw]], [[legend]] - the chart contract; [[schemas]] - its style, options and mapping."
  (:require [fastmath.core :as m]
            [fastmath.random :as r]
            [fastmath.vector :as v]
            [cljplot2.data.group :as group]
            [cljplot2.data.rng :as rng]
            [cljplot2.data.rows :as data]
            [cljplot2.data.strip :as strip]
            [cljplot2.graphics.shape :as shape]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries
  "Map entries of the strip plot's point style (theme `[:chart :stripplot]`), also its options."
  [(schema/optional :size {:description "point size in pixels (a mapped :size wins)"} schema/non-neg)
   (schema/optional :color {:description "point colour (a mapped :color wins)"} schema/color)
   (schema/optional :stroke-color {:description "point outline colour (or :none)"} schema/color)
   (schema/optional :stroke-width {:description "outline width in pixels"} schema/non-neg)
   (schema/optional :shape {:description "point shape (a mapped :shape wins)"} schema/shape)])

(def schemas
  "Schemas of the stripplot chart: `:style` (theme `[:chart :stripplot]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into theme/theme-style-entries)
                (into strip/options)
                (conj (schema/optional :jitter {:description "jitter spread across the strip, a fraction of the item's cross range (Gaussian, sd :jitter / 4, clamped to ±:jitter / 2; 0 - on the centre line)"
                                                :default 0.25}
                                       schema/fraction)
                      (schema/nilable :rng {:description "random number generator for jitter (seeded for reproducible plots)"
                                            :cljplot2/default-doc "a fresh unseeded generator per build (builds differ)"}
                                      rng/schema)
                      (schema/optional :padding {:description "padding of the data span per position axis, {:x [lo hi] :y [lo hi]} fractions of the span"
                                                 :cljplot2/default-doc "[0.05 0.05] along the value axis; with :place :bottom also :bottom-share below the data"}
                                       data/position-padding)))
   :mapping (strip/values-mapping
              [(schema/nilable :x {:description "values (data units) on the value axis, or levels for vertical strips"} schema/accessor)
               (schema/nilable :y {:description "levels on the categorical axis (absent - one strip), or values for vertical strips"}
                               schema/accessor)
               (schema/nilable :color {:description "groups, each in its own point colour"} schema/accessor)
               (schema/nilable :shape {:description "groups, each with its own point shape"} schema/accessor)
               (schema/nilable :size {:description "groups, each with its own point size"} schema/accessor)
               (schema/nilable :group {:description "groups without a visual aesthetic"} schema/accessor)])})

(def ^{:const true :private true :tag 'double} default-jitter
  "Jitter spread when `:jitter` isn't given, a fraction of an item's cross range."
  0.25)

(defn jitter-offsets
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
        jitter (double (get options :jitter default-jitter))
        jittered (update-vals items (fn [its] (mapv #(assoc % :offsets (jitter-offsets rng (count (:values %)) jitter)) its)))
        point-style (theme/resolved-style theme chart)]
    {:data (update-vals jittered (fn [its] {:items its :style point-style :place place :options options}))
     :extents (merge-with merge (group/aesthetic-extents panels (:plot-scales env))
                          (strip/extents place jittered (fn [{:keys [values]}] [(v/mn values) (v/mx values)])))
     :padding (strip/padding place options)
     :padding-explicit (strip/explicit-padding place options)
     :padding-px (strip/n-label-room options jittered env)
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

  See also [[cljplot2.data.strip/placement]]."
  [_chart {:keys [items style place options]} canvas box scales env]
  (let [{:keys [point] :as pl} (strip/placement box scales options place)]
    (doseq [{:keys [level slot values offsets aesthetics]} items
            :let [st (group/style style aesthetics scales (:theme env))]
            :when st
            [v off] (map vector values offsets)
            :let [^Vec2 p (point v level (strip/in-slot slot (double off)))]
            :when (and (m/valid-double? (.x p)) (m/valid-double? (.y p)))]
      (shape/draw! canvas (:shape st) p st))
    (strip/draw-n-labels! canvas pl items (fn [{:keys [values]}] (v/mx values)) options scales env)))

(defn compute
  "Computes the strip plot's stats with its levels on the categorical axis.

  Parameters:

  - `chart` (map): the chart value.
  - `chart-split` (map): the chart's rows split per panel.
  - `env` (map): the compute environment.

  Returns the stats of [[compute-in-strip-frame]], turned to the mapping axis of the levels (see [[cljplot2.data.strip/compute-oriented]])."
  [chart chart-split env]
  (strip/compute-oriented compute-in-strip-frame chart chart-split env))

(defn draw
  "Draws one panel of the strip plot.

  Parameters:

  - `chart` (map): the chart value.
  - `computed` (map): the panel's stats from [[compute]].
  - `canvas`: the clojure2d canvas.
  - `box` (map): the panel box.
  - `scales` (map): finalized scales.
  - `env` (map): the draw environment.

  Draws [[draw-in-strip-frame]] in the strip frame (see [[cljplot2.data.strip/draw-oriented]])."
  [chart computed canvas box scales env]
  (strip/draw-oriented draw-in-strip-frame chart computed canvas box scales env))

(defn legend
  "Draws the strip plot's key glyph: its point shape fitted and centred in a legend key.

  Parameters:

  - `chart` (map): the chart value.
  - `attributes` (map): the key's mapped style attributes, over the chart's style.
  - `canvas`: the clojure2d canvas.
  - `box` (map): the key box in pixels.
  - `env` (map): the legend environment - `:theme`.

  See also [[cljplot2.graphics.shape/draw-key!]]."
  [chart attributes canvas box env]
  (let [st (merge (theme/resolved-style (:theme env) chart) attributes)]
    (shape/draw-key! canvas (:shape st) box st)))
