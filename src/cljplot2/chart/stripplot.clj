(ns cljplot2.chart.stripplot
  "Strip plot (lattice `stripplot`): one point per `:x` value, jittered across its strip - a strip chart (see [[cljplot2.data.strip]]).

  Jitter: Gaussian offsets (fastmath `grandom` on the option `:rng`, a fastmath random generator; without it every
  build creates a fresh one, so builds differ - pass `:rng` for reproducible plots) with standard deviation
  `:jitter` / 4 of the item's cross range, clamped to ±`:jitter` / 2 (`:jitter` default 0.8; 0 - all points on the
  centre line). Offsets are drawn at compute and kept in the stats.

  Options (style defaults from theme `[:chart :stripplot]`): `:size`, `:color`, `:stroke-color`, `:stroke-width`,
  `:shape` (as scatter, see [[cljplot2.graphics.shape]]); `:jitter`, `:rng`, `:padding {:x [lo hi]}` (default
  `[0.05 0.05]`), plus the strip options and `:data` / `:mapping`."
  (:require [fastmath.core :as m]
            [fastmath.random :as r]
            [cljplot2.data.group :as group]
            [cljplot2.data.rows :as data]
            [cljplot2.data.strip :as strip]
            [cljplot2.graphics.shape :as shape]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries
  [(schema/optional :size schema/non-neg)
   (schema/optional :color schema/color)
   (schema/optional :stroke-color schema/color)
   (schema/optional :stroke-width schema/non-neg)
   (schema/optional :shape schema/shape)])

(def RNG
  "A fastmath random generator (see `fastmath.random/rng`)."
  (schema/named-type ::rng #(satisfies? fastmath.protocols/RNGProto %) "should be a fastmath random generator"))

(def Schema
  "Schemas of the stripplot chart: `:style` (theme `[:chart :stripplot]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into strip/options)
                (conj (schema/optional :jitter schema/fraction)
                      (schema/nilable :rng RNG)
                      (schema/optional :padding data/position-padding)))
   :mapping [:map [:x schema/accessor]]})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :stripplot]) (select-keys options [:size :color :stroke-color :stroke-width :shape])))

(defn jitter-offsets
  "`n` jitter offsets (two-sided cross units) from `rng`: Gaussian, sd `jitter` / 4, clamped to ±`jitter` / 2."
  [rng ^long n ^double jitter]
  (let [half (* 0.5 jitter)]
    (vec (repeatedly n #(if (zero? jitter) 0.0 (m/constrain (double (r/grandom rng 0.0 (* 0.25 jitter))) (- half) half))))))

(defn compute
  "Per panel: `{:items [..] :style st :place p :options o}`, items with `:offsets` (one per value); extents (x - the
  values), padding, `:na`, `:dropped`."
  [{:keys [options]} chart-split {:keys [theme] :as env}]
  (let [{:keys [place items panels dropped]} (strip/split chart-split options :center env)
        rng (or (:rng options) (r/rng :mersenne))
        jitter (double (get options :jitter 0.8))
        items (update-vals items (fn [its] (mapv #(assoc % :offsets (jitter-offsets rng (count (:values %)) jitter)) its)))]
    {:data (update-vals items (fn [its] {:items its :style (style options theme) :place place :options options}))
     :extents (merge-with merge (group/aesthetic-extents panels)
                          (strip/extents place items (fn [{:keys [values]}] [(reduce min values) (reduce max values)])))
     :padding (merge {:x [0.05 0.05]} (:padding options))
     :na (group/na-dims panels)
     :dropped dropped}))

(defn draw
  "Draw the points of one panel."
  [_chart {:keys [items style place options]} canvas box scales env]
  (let [{:keys [point]} (strip/placement box scales options place)]
    (doseq [{:keys [level slot values offsets aesthetics]} items
            :let [st (group/style style aesthetics scales (:theme env))]
            :when st
            [v off] (map vector values offsets)
            :let [^Vec2 p (point v level (strip/in-slot slot (double off)))]
            :when (and (m/valid-double? (.x p)) (m/valid-double? (.y p)))]
      (shape/draw! canvas (:shape st) p st))))

(defn legend
  "Key glyph: the chart's shape centred in `box`."
  [{:keys [options]} attributes canvas {:keys [x y w h]} env]
  (let [st (merge (style options (:theme env)) attributes)]
    (shape/draw! canvas (:shape st) (Vec2. (+ (double x) (* 0.5 (double w))) (+ (double y) (* 0.5 (double h)))) st)))
