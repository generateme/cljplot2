(ns cljplot2.chart.beeswarm
  "Beeswarm: one point per `:x` value, packed across its strip so points don't overlap - a strip chart (see
  [[cljplot2.data.strip]]).

  Packing happens at draw, in pixels ([[swarm-offsets]]): values in increasing order, each at the smallest cross offset
  (absolute) where its circle of diameter `:size` touches no point placed before. With `:fit` (default true) a swarm
  wider than its item's cross range is squeezed into it (points may then overlap); `:fit false` keeps the packing
  and lets the swarm leave its range. With `:position :mirror` each group swarms to its own side only.

  Options (style defaults from theme `[:chart :beeswarm]`): `:size`, `:color`, `:stroke-color`, `:stroke-width`,
  `:shape` (as scatter, see [[cljplot2.graphics.shape]]); `:fit`, `:padding {:x [lo hi]}` (default `[0.05 0.05]`),
  plus the strip options and `:data` / `:mapping`."
  (:require [fastmath.core :as m]
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

(def schemas
  "Schemas of the beeswarm chart: `:style` (theme `[:chart :beeswarm]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into strip/options)
                (conj (schema/nilable :fit schema/bool)
                      (schema/optional :padding data/position-padding)))
   :mapping strip/values-mapping})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :beeswarm]) (select-keys options [:size :color :stroke-color :stroke-width :shape])))

(def ^:private ^:const eps 1.0e-9)

(defn- nearest-free
  "The offset nearest 0 (`one-sided?` - non-negative) outside every open interval of `intervals` (`[lo hi]` pairs,
  sorted by `lo`); ties prefer the positive side. Overlapping intervals are merged; the ends of a merged interval
  lie in no interval."
  ^double [intervals one-sided?]
  (let [merged (reduce (fn [acc [lo hi]]
                         (let [[plo phi] (peek acc)]
                           (if (and plo (< (double lo) (- (double phi) eps)))
                             (conj (pop acc) [plo (m/max (double phi) (double hi))])
                             (conj acc [lo hi]))))
                       [] intervals)
        [lo hi] (first (filter (fn [[lo hi]] (< (+ (double lo) eps) 0.0 (- (double hi) eps))) merged))]
    (cond
      (nil? lo) 0.0
      one-sided? (double hi)
      (< (- (double lo)) (- (double hi) eps)) (double lo)
      :else (double hi))))

(defn swarm-offsets
  "Cross offsets (pixels) packing circles of `diameter` at the pixel `positions` along the value axis (increasing):
  each at the smallest absolute offset (`one-sided?` - smallest non-negative) where it is at least `diameter` from
  every circle placed before it; ties prefer the positive side. Each earlier circle within a diameter along the
  axis forbids an open offset interval; the answer is the free offset nearest 0 - `O(k log k)` for `k` such circles."
  [positions ^double diameter one-sided?]
  (let [d2 (* diameter diameter)]
    (loop [ps (seq positions) placed [] offsets (transient [])]
      (if-not ps
        (persistent! offsets)
        (let [p (double (first ps))
              ;; forbidden intervals of the circles placed before within one diameter (positions are increasing)
              intervals (loop [i (dec (count placed)) acc []]
                          (let [[q c] (when (>= i 0) (placed i))]
                            (if (and q (< (- p (double q)) diameter))
                              (let [dp (- p (double q))
                                    h (m/sqrt (m/max 0.0 (- d2 (* dp dp))))]
                                (recur (dec i) (if (> h eps) (conj acc [(- (double c) h) (+ (double c) h)]) acc)))
                              acc)))
              o (nearest-free (sort-by first intervals) one-sided?)]
          (recur (next ps) (conj placed [p o]) (conj! offsets o)))))))

(defn fit-offsets
  "`offsets` squeezed proportionally so none is farther than `room` from 0 (unchanged when they fit)."
  [offsets ^double room]
  (let [widest (double (reduce max 0.0 (map #(m/abs (double %)) offsets)))]
    (if (> widest room) (mapv #(* (double %) (/ room widest)) offsets) (vec offsets))))

(defn- compute-in-strip-frame
  "Per panel: `{:items [..] :style st :place p :options o :one-sided? b}`; extents (x - the values), padding, `:na`,
  `:dropped`."
  [{:keys [options]} chart-split {:keys [theme] :as env}]
  (let [{:keys [place position items panels dropped]} (strip/split chart-split options :center env)]
    {:data (update-vals items (fn [its] {:items its :style (style options theme) :place place :options options
                                         :one-sided? (= :mirror position)}))
     :extents (merge-with merge (group/aesthetic-extents panels (:plot-scales env))
                          (strip/extents place items (fn [{:keys [values]}] [(reduce min values) (reduce max values)])))
     :padding (strip/padding place options)
     :padding-explicit (strip/explicit-padding place options)
     :padding-px (strip/n-label-room options items env)
     :na (group/na-dims panels)
     :dropped dropped}))

(defn- draw-in-strip-frame
  "Draw the points of one panel, each item swarmed by [[swarm-offsets]] around its centre line (one-sided: from 0
  outward), fitted into its slot with `:fit`."
  [_chart {:keys [items style place options one-sided?]} canvas box scales env]
  (let [{:keys [point span value-pixel] :as pl} (strip/placement box scales options place)
        fit? (data/option options :fit true)]
    (doseq [{:keys [level slot values aesthetics]} items
            :let [st (group/style style aesthetics scales (:theme env))
                  s (double (span level))]
            :when (and st (pos? s))]
      (let [[lo hi] slot
            side (strip/slot-side slot)
            base (if one-sided? 0.0 (strip/in-slot slot 0.0))
            room (* s (if one-sided? (m/abs (+ (double lo) (double hi))) (strip/slot-half slot)))
            ;; [value pixel] pairs in increasing pixel order (a reversed value axis reverses the values)
            kept (sort-by second (filter #(m/valid-double? (double (second %))) (map (juxt identity value-pixel) values)))
            offsets (cond-> (swarm-offsets (map second kept) (double (:size st)) one-sided?)
                      fit? (fit-offsets room))]
        (doseq [[[v] o] (map vector kept offsets)
                :let [^Vec2 p (point v level (+ base (/ (* (if one-sided? side 1.0) (double o)) s)))]
                :when (and (m/valid-double? (.x p)) (m/valid-double? (.y p)))]
          (shape/draw! canvas (:shape st) p st))))
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
