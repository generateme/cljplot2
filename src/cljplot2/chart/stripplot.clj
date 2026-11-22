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
  `[0.05 0.05]`), plus the strip options and `:data` / `:mapping`."
  (:require [fastmath.core :as m]
            [fastmath.random :as r]
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

(defn- style
  [options theme]
  (merge (theme/chart-style theme :stripplot options) (select-keys options [:size :color :stroke-color :stroke-width :shape])))

(defn jitter-offsets
  "`n` jitter offsets (two-sided cross units) from `rng`: Gaussian, sd `jitter` / 4, clamped to ±`jitter` / 2."
  [rng ^long n ^double jitter]
  (let [half (* 0.5 jitter)]
    (vec (repeatedly n #(if (zero? jitter) 0.0 (m/constrain (double (r/grandom rng 0.0 (* 0.25 jitter))) (- half) half))))))

(defn- compute-in-strip-frame
  "Per panel: `{:items [..] :style st :place p :options o}`, items with `:offsets` (one per value); extents (x - the
  values), padding, `:na`, `:dropped`."
  [{:keys [options]} chart-split {:keys [theme] :as env}]
  (let [{:keys [place items panels dropped]} (strip/split chart-split options :center env)
        rng (rng/for-build (:rng options))
        jitter (double (get options :jitter 0.25))
        items (update-vals items (fn [its] (mapv #(assoc % :offsets (jitter-offsets rng (count (:values %)) jitter)) its)))]
    {:data (update-vals items (fn [its] {:items its :style (style options theme) :place place :options options}))
     :extents (merge-with merge (group/aesthetic-extents panels (:plot-scales env))
                          (strip/extents place items (fn [{:keys [values]}] [(reduce min values) (reduce max values)])))
     :padding (strip/padding place options)
     :padding-explicit (strip/explicit-padding place options)
     :padding-px (strip/n-label-room options items env)
     :na (group/na-dims panels)
     :dropped dropped}))

(defn- draw-in-strip-frame
  "Draw the points of one panel."
  [_chart {:keys [items style place options]} canvas box scales env]
  (let [{:keys [point] :as pl} (strip/placement box scales options place)]
    (doseq [{:keys [level slot values offsets aesthetics]} items
            :let [st (group/style style aesthetics scales (:theme env))]
            :when st
            [v off] (map vector values offsets)
            :let [^Vec2 p (point v level (strip/in-slot slot (double off)))]
            :when (and (m/valid-double? (.x p)) (m/valid-double? (.y p)))]
      (shape/draw! canvas (:shape st) p st))
    (strip/draw-n-labels! canvas pl items (fn [{:keys [values]}] (reduce max values)) options scales env)))


(defn compute
  "Stats of the chart with its levels on the categorical axis (see [[cljplot2.data.strip/compute-oriented]]):
  [[compute-in-strip-frame]] in the strip frame."
  [chart chart-split env]
  (strip/compute-oriented compute-in-strip-frame chart chart-split env))

(defn draw
  "Draw one panel (see [[cljplot2.data.strip/draw-oriented]]): [[draw-in-strip-frame]] in the strip frame."
  [chart computed canvas box scales env]
  (strip/draw-oriented draw-in-strip-frame chart computed canvas box scales env))
(defn legend
  "Key glyph: the chart's shape fitted and centred in `box` (see [[cljplot2.graphics.shape/draw-key!]])."
  [{:keys [options]} attributes canvas box env]
  (let [st (merge (style options (:theme env)) attributes)]
    (shape/draw-key! canvas (:shape st) box st)))
