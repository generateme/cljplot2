(ns cljplot2.chart.dotplot
  "Dot plot: the `:x` values binned and drawn as stacks of dots - a strip chart (see [[cljplot2.data.strip]]): each
  level of the categorical axis its own baseline (Wilkinson dot plot, wheat plot, quantile dot plot).

  Bins (see [[cljplot2.data.dots]]), `:binwidth` in data units (default: the range of all values / 30): `:method`
  `:dot-density` (default; Wilkinson) or `:histogram` (fixed bins from `:origin`, default the smallest value). `:at`
  `:center` (default) puts a bin's dots at its centre, `:value` each dot at its own value (the wheat plot).
  Groups of one slot share stacks: by default (`:position :overlay`) every group of a level is binned and stacked
  together, each dot in its group's style, ordered by group, then value; `:position :dodge` gives each group its own
  slot and stacks, `:mirror` stacks the two groups away from the level's centre line.
  Stacks (`:stack-dir`): `:up` (default) from the slot's low cross edge toward the next level, `:center` centred on
  the slot. Dot diameter: the bin width in pixels, shrunk so the tallest stack (over all panels) fits its slot; dots
  one diameter apart. The stacking axis has no scale.

  Options (style defaults from theme `[:chart :dotplot]`): `:color`, `:stroke-color`, `:stroke-width`, `:shape` (as
  scatter, see [[cljplot2.graphics.shape]]), `:size` (the drawn dot size; default the diameter above); `:method`,
  `:binwidth`, `:origin` (`:histogram` only - `ex-info {:key :origin}` otherwise), `:at`, `:stack-dir`, `:padding {:x
  [lo hi]}` (default `[0.05 0.05]`), plus the strip options and `:data` / `:mapping`."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.data.dots :as dots]
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
  "Schemas of the dot plot: `:style` (theme `[:chart :dotplot]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into strip/options)
                (conj (schema/optional :method [:enum :dot-density :histogram])
                      (schema/optional :binwidth [:and schema/finite schema/pos])
                      (schema/optional :origin schema/finite)
                      (schema/optional :at [:enum :center :value])
                      (schema/optional :stack-dir [:enum :up :center])
                      (schema/optional :padding data/position-padding)))
   :mapping strip/values-mapping})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :dotplot]) (select-keys options [:size :color :stroke-color :stroke-width :shape])))

(defn- bins-of
  "Bins (see [[cljplot2.data.dots]]) of the increasing `values` under the chart `options` and `binwidth`; `origin` -
  the `:histogram` default origin."
  [values options ^double binwidth ^double origin]
  (if (= :histogram (:method options))
    (dots/fixed-bins values binwidth (double (get options :origin origin)))
    (dots/dot-density-bins values binwidth)))

(defn- stack-dots
  "Dot stacks of the `items` of one slot (one level): their values binned together - `[{:dots [{:x :rank
  :aesthetics}] :n}]` per bin, dots ordered by item (group) order, then value; `:x` the bin centre, or with `:at
  :value` the dot's value."
  [items options binwidth origin]
  (let [dots (sort-by :value (for [[i {:keys [values aesthetics]}] (map-indexed vector items)
                                   v values]
                               {:value (double v) :item i :aesthetics aesthetics}))
        bins (bins-of (map :value dots) options binwidth origin)
        at-value? (= :value (:at options))]
    (loop [bins bins dots dots acc []]
      (if-let [{:keys [center values]} (first bins)]
        (let [[in more] (split-at (count values) dots)
              ordered (sort-by (juxt :item :value) in)]
          (recur (rest bins) more
                 (conj acc {:n (count ordered)
                            :dots (vec (map-indexed (fn [rank {:keys [value aesthetics]}]
                                                      {:x (if at-value? value center) :rank rank :aesthetics aesthetics})
                                                    ordered))})))
        acc))))

(defn- slot-stacks
  "Stacks of one panel's `items`: items of one level and slot binned together (see [[stack-dots]]) -
  `[{:level :slot :stacks [..]}]`."
  [items options binwidth origin]
  (vec (for [[[level slot] its] (group-by (juxt :level :slot) items)]
         {:level level :slot slot :stacks (stack-dots its options binwidth origin)})))

(defn- compute-in-strip-frame
  "Per panel: `{:slots [..] :items [..] :style st :place p :options o :binwidth w :max-count n}` (see
  [[slot-stacks]]; `:max-count` - the tallest stack over all panels); extents (x - the dots' positions widened by
  half a bin), padding, `:na`, `:dropped`. Throws `ex-info {:key :origin}` for `:origin` without `:method
  :histogram`."
  [{:keys [options]} chart-split {:keys [theme] :as env}]
  (when (and (some? (:origin options)) (not= :histogram (:method options)))
    (schema/fail! "Dot plot :origin anchors fixed bins: it needs :method :histogram." {:key :origin :value (:origin options)}))
  (let [split-options (cond-> options (nil? (:position options)) (assoc :position :overlay))
        {:keys [place position items panels dropped]} (strip/split chart-split split-options :center env)
        all-values (mapcat :values (mapcat val items))
        binwidth (if (seq all-values) (double (get options :binwidth (dots/default-binwidth all-values))) 1.0)
        origin (if (seq all-values) (double (reduce min all-values)) 0.0)
        slots (update-vals items #(slot-stacks % options binwidth origin))
        max-count (long (reduce max 1 (for [ss (vals slots) s ss st (:stacks s)] (:n st))))
        half (* 0.5 binwidth)]
    {:data (into {} (map (fn [[panel ss]]
                           [panel {:slots ss :items (items panel) :style (style options theme) :place place :options options
                                   :binwidth binwidth :max-count max-count :mirror? (= :mirror position)}])
                         slots))
     :extents (merge-with merge (group/aesthetic-extents panels (:plot-scales env))
                          (strip/extents place
                                         (update-vals slots (fn [ss] (mapv #(assoc % :xs (for [st (:stacks %) d (:dots st)] (:x d))) ss)))
                                         (fn [{:keys [xs]}] [(- (double (reduce min xs)) half) (+ (double (reduce max xs)) half)])))
     :padding (strip/padding place options)
     :padding-explicit (strip/explicit-padding place options)
     :padding-px (strip/n-label-room options items env)
     :na (group/na-dims panels)
     :dropped dropped}))

(defn- dot-diameter
  "Pixel diameter of the dots of a panel: the bin width (`binwidth`) in pixels along the value axis (`value-pixel`,
  measured from value `x0`), at most the cross length of every slot (`slots`, pixels via `span`) over `max-count`
  dots."
  ^double [value-pixel span slots {:keys [x0 binwidth max-count]}]
  (let [x0 (double x0) binwidth (double binwidth)
        bw (m/abs (- (double (value-pixel (+ x0 binwidth))) (double (value-pixel x0))))
        fit (reduce min ##Inf (for [{:keys [level slot]} slots
                                    :let [[lo hi] slot s (double (span level))]
                                    :when (pos? s)]
                                (/ (* s (- (double hi) (double lo))) (double max-count))))]
    (m/min bw (double fit))))

(defn- cross-of
  "Cross coordinate of the dot `[rank n]` (its rank in a stack of `n`) in `slot` (cross units; `d` - the diameter in
  cross units; `stacking` - `{:stack-dir :mirror?}`):
  `:up` - from the slot's low edge (mirror: from the centre line toward the slot's side), `:center` - centred."
  ^double [slot {:keys [stack-dir mirror?]} [rank n] d]
  (let [[lo hi] slot lo (double lo) hi (double hi)
        rank (long rank) n (long n) d (double d)]
    (cond
      (= :center stack-dir) (+ (* 0.5 (+ lo hi)) (* d (- (double rank) (* 0.5 (dec n)))))
      mirror? (* (strip/slot-side slot) d (+ (double rank) 0.5))
      :else (+ lo (* d (+ (double rank) 0.5))))))

(defn- draw-in-strip-frame
  "Draw the dot stacks of one panel (see [[dot-diameter]], [[cross-of]]), then the n labels."
  [_chart {:keys [slots items style place options binwidth max-count mirror?]} canvas box scales env]
  (let [{:keys [point span value-pixel] :as pl} (strip/placement box scales options place)
        x0 (some (fn [{:keys [stacks]}] (some #(:x (first (:dots %))) stacks)) slots)
        stack-dir (get options :stack-dir :up)]
    (when x0
      (let [d (dot-diameter value-pixel span slots {:x0 x0 :binwidth binwidth :max-count max-count})
            stacking {:stack-dir stack-dir :mirror? mirror?}]
        (c2d/set-stroke-custom canvas {:size (double (:stroke-width style))})
        (doseq [{:keys [level slot stacks]} slots
                :let [s (double (span level))]
                :when (pos? s)
                {:keys [n dots]} stacks
                {:keys [x rank aesthetics]} dots
                :let [st (group/style style aesthetics scales (:theme env))
                      ^Vec2 p (point x level (cross-of slot stacking [rank n] (/ d s)))]
                :when (and st (m/valid-double? (.x p)) (m/valid-double? (.y p)))]
          (shape/draw! canvas (:shape st) p (update st :size #(if (some? %) % d))))))
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
  "Key glyph: the chart's shape fitted and centred in `box` (see [[cljplot2.graphics.shape/draw-key!]]); without a
  `:size` the key box's size."
  [{:keys [options]} attributes canvas {:keys [w h] :as box} env]
  (let [st (merge (style options (:theme env)) attributes)
        st (update st :size #(if (some? %) % (* 0.7 (m/min (double w) (double h)))))]
    (c2d/set-stroke-custom canvas {:size (double (:stroke-width st))})
    (shape/draw-key! canvas (:shape st) box st)))
