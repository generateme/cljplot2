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

(def Schema
  "Schemas of the beeswarm chart: `:style` (theme `[:chart :beeswarm]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into strip/options)
                (conj (schema/nilable :fit schema/bool)
                      (schema/optional :padding data/position-padding)))
   :mapping [:map [:x schema/accessor]]})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :beeswarm]) (select-keys options [:size :color :stroke-color :stroke-width :shape])))

(def ^:private ^:const eps 1.0e-9)

(defn swarm-offsets
  "Cross offsets (pixels) packing circles of `diameter` at the pixel `positions` along the value axis (increasing):
  each at the smallest absolute offset (`one-sided?` - smallest non-negative) where it is at least `diameter` from
  every circle placed before it. Ties prefer the positive side."
  [positions ^double diameter one-sided?]
  (let [d2 (* diameter diameter)]
    (loop [ps (seq positions) placed [] offsets (transient [])]
      (if-not ps
        (persistent! offsets)
        (let [p (double (first ps))
              ;; circles placed before within one diameter along the value axis (positions are increasing)
              near (loop [i (dec (count placed)) acc []]
                     (if (and (>= i 0) (< (- p (double (first (placed i)))) diameter))
                       (recur (dec i) (conj acc (placed i)))
                       acc))
              free? (fn [^double o] (every? (fn [[q c]] (let [dp (- p (double q)) dc (- o (double c))]
                                                          (>= (+ (* dp dp) (* dc dc)) (- d2 eps))))
                                            near))
              candidates (cons 0.0 (mapcat (fn [[q c]] (let [dp (- p (double q))
                                                             h (m/sqrt (m/max 0.0 (- d2 (* dp dp))))]
                                                         [(+ (double c) h) (- (double c) h)]))
                                           near))
              o (->> candidates
                     (filter #(or (not one-sided?) (>= (double %) (- eps))))
                     (filter free?)
                     (reduce (fn [^double best ^double o]
                               (let [ab (m/abs best) ao (m/abs o)]
                                 (if (or (< ao (- ab eps)) (and (< (m/abs (- ao ab)) eps) (> o best))) o best)))
                             ##Inf))]
          (recur (next ps) (conj placed [p o]) (conj! offsets o)))))))

(defn fit-offsets
  "`offsets` squeezed proportionally so none is farther than `room` from 0 (unchanged when they fit)."
  [offsets ^double room]
  (let [widest (double (reduce max 0.0 (map #(m/abs (double %)) offsets)))]
    (if (> widest room) (mapv #(* (double %) (/ room widest)) offsets) (vec offsets))))

(defn compute
  "Per panel: `{:items [..] :style st :place p :options o :one-sided? b}`, item values sorted; extents (x - the
  values), padding, `:na`, `:dropped`."
  [{:keys [options]} chart-split {:keys [theme] :as env}]
  (let [{:keys [place position items panels dropped]} (strip/split chart-split options :center env)
        items (update-vals items (fn [its] (mapv #(update % :values (comp vec sort)) its)))]
    {:data (update-vals items (fn [its] {:items its :style (style options theme) :place place :options options
                                         :one-sided? (= :mirror position)}))
     :extents (merge-with merge (group/aesthetic-extents panels)
                          (strip/extents place items (fn [{:keys [values]}] [(first values) (peek values)])))
     :padding (strip/padding place options)
     :na (group/na-dims panels)
     :dropped dropped}))

(defn draw
  "Draw the points of one panel, each item swarmed by [[swarm-offsets]] around its centre line (one-sided: from 0
  outward), fitted into its slot with `:fit`."
  [_chart {:keys [items style place options one-sided?]} canvas box scales env]
  (let [{:keys [point span]} (strip/placement box scales options place)
        fit? (data/option options :fit true)]
    (doseq [{:keys [level slot values aesthetics]} items
            :let [st (group/style style aesthetics scales (:theme env))
                  s (double (span level))]
            :when (and st (pos? s))]
      (let [[lo hi] slot
            side (strip/ridge-side slot)
            base (if one-sided? 0.0 (strip/in-slot slot 0.0))
            room (* s (if one-sided? (m/abs (+ (double lo) (double hi))) (strip/slot-half slot)))
            ;; pixel position along the value axis: the moving coordinate between two cross offsets
            along (fn [v] (let [^Vec2 a (point v level base) ^Vec2 b (point v level (+ base 0.25))]
                            (if (== (.x a) (.x b)) (.x a) (.y a))))
            ;; [value pixel] pairs in increasing pixel order (a reversed value axis reverses the values)
            kept (sort-by second (filter #(m/valid-double? (double (second %))) (map (juxt identity along) values)))
            offsets (cond-> (swarm-offsets (map second kept) (double (:size st)) one-sided?)
                      fit? (fit-offsets room))]
        (doseq [[[v] o] (map vector kept offsets)
                :let [^Vec2 p (point v level (+ base (/ (* (if one-sided? side 1.0) (double o)) s)))]
                :when (and (m/valid-double? (.x p)) (m/valid-double? (.y p)))]
          (shape/draw! canvas (:shape st) p st))))))

(defn legend
  "Key glyph: the chart's shape centred in `box`."
  [{:keys [options]} attributes canvas {:keys [x y w h]} env]
  (let [st (merge (style options (:theme env)) attributes)]
    (shape/draw! canvas (:shape st) (Vec2. (+ (double x) (* 0.5 (double w))) (+ (double y) (* 0.5 (double h)))) st)))
