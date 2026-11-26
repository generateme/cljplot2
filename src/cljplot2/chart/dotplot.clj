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
  one diameter apart (dot density bins centred on their values may come closer than a bin width - neighbouring stacks
  then overlap a little, as in ggplot). The stacking axis has no scale. `:n-label` sits past the farthest dot of the
  item's slot.

  Options (style defaults from theme `[:chart :dotplot]`): `:color`, `:stroke-color`, `:stroke-width`, `:shape` (as
  scatter, see [[cljplot2.graphics.shape]]), `:size` (the drawn dot size; default the diameter above); `:method`,
  `:binwidth`, `:origin` (`:histogram` only - a validation error otherwise), `:at`, `:stack-dir`, `:padding {:x
  [lo hi]}` (default `[0.05 0.05]`), plus the strip options and `:data` / `:mapping`.

  Functions: [[legend]] - the chart contract's key glyph (the chart computes and draws through [[cljplot2.chart.strip/compute]] and [[cljplot2.chart.strip/draw]]); [[schemas]] - its style, options and mapping."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.data.dots :as dots]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.data.group :as group]
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
  "Map entries of the dot plot's point style (theme `[:chart :dotplot]`), also its options."
  [(schema/optional :size {:description "The drawn size of the dots in pixels; a mapped `:size` wins."
                           :cljplot2/default-doc "The stacking diameter: the bin width in pixels, shrunk so the tallest stack fits its slot."
                           :cljplot2/example 4}
                    schema/non-neg)
   (schema/optional :color {:description "The colour of the dots; a mapped `:color` wins." :cljplot2/value-meanings {:none "No fill."} :cljplot2/example :orange} schema/color)
   (schema/optional :stroke-color {:description "The colour of each dot's outline." :cljplot2/value-meanings {:none "No outline."} :cljplot2/example :black} schema/color)
   (schema/optional :stroke-width {:description "The width of the outline in pixels; `0` draws no outline." :cljplot2/example 3 :cljplot2/example-context {:stroke-color :black}} schema/non-neg)
   (schema/optional :shape {:description "The shape of every dot; a mapped `:shape` wins." :cljplot2/example \s} schema/shape)])

(def schemas
  "Schemas of the dot plot: `:style` (theme `[:chart :dotplot]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (schema/with-rules
             (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into theme/theme-style-entries)
                (into strip/options)
                (conj (schema/optional :method {:description "How the chart bins the values."
                                                :default :dot-density
                                                :cljplot2/value-meanings {:dot-density "Wilkinson's dot density bins, each centred between its smallest and largest value." :histogram "Fixed bins from `:origin`."}}
                                       [:enum :dot-density :histogram])
                      (schema/optional :binwidth {:description "The width of a bin in data units."
                                                  :cljplot2/default-doc "The range of all values over 30, or `1` for a single distinct value."
                                                  :cljplot2/example 1}
                                       schema/finite-pos)
                      (schema/optional :origin {:description "The low edge of the first fixed bin in data units; it needs `:method :histogram`."
                                                :cljplot2/default-doc "The smallest value."
                                                :cljplot2/example 0.25 :cljplot2/example-context {:method :histogram :binwidth 1}}
                                       schema/finite)
                      (schema/optional :at {:description "Where a bin's dots sit along the value axis."
                                            :default :center
                                            :cljplot2/value-meanings {:center "At the bin's centre." :value "Each dot at its own value, as in a wheat plot."}}
                                       [:enum :center :value])
                      (schema/optional :stack-dir {:description "Which way the stacks grow."
                                                   :default :up
                                                   :cljplot2/value-meanings {:up "From the slot's low edge towards the next level." :center "Both ways from the slot's centre line."}}
                                       [:enum :up :center])
                      strip/padding-option))
             [#(when (and (some? (:origin %)) (not= :histogram (:method %))) "the :origin of fixed bins needs :method :histogram") [:origin]])
   :mapping (strip/values-mapping
              [(schema/nilable :x {:description "The values of horizontal dot plots, or the levels of vertical ones."} schema/accessor)
               (schema/nilable :y {:description "The levels of horizontal dot plots, or the values of vertical ones; with no levels the chart draws one dot plot."}
                               schema/accessor)
               (schema/nilable :color {:description "Splits the rows into groups, each in its own point colour."} schema/accessor)
               (schema/nilable :shape {:description "Splits the rows into groups, each with its own point shape."} schema/accessor)
               (schema/nilable :size {:description "Splits the rows into groups, each with its own point size."} schema/accessor)
               (schema/nilable :group {:description "Splits the rows into groups that look the same, sharing stacks unless dodged."} schema/accessor)])})

(defn- bins-of
  "Gives the bins of increasing values under the chart's method.

  Parameters:

  - `values` (sequence of doubles): the values, increasing.
  - `options` (map): the chart's options - `:method` (`:histogram` - fixed bins, else dot density), `:origin`.
  - `binwidth` (double): the bin width in data units.
  - `origin` (double): the `:histogram` origin when `:origin` isn't given.

  Returns the bins (see [[cljplot2.data.dots/fixed-bins]], [[cljplot2.data.dots/dot-density-bins]])."
  [values options ^double binwidth ^double origin]
  (if (= :histogram (:method options))
    (dots/fixed-bins values binwidth (double (data/option options :origin origin)))
    (dots/dot-density-bins values binwidth)))

(defn- stack-dots
  "Gives the dot stacks of the items of one slot, their values binned together.

  Parameters:

  - `items` (sequence of maps): the slot's items - `:values`, `:aesthetics`, in group order.
  - `options` (map): the chart's options - `:method`, `:origin`, `:at` (`:value` - each dot at its own value).
  - `binwidth` (double): the bin width in data units.
  - `origin` (double): the default `:histogram` origin.

  Returns a vector per bin of `{:dots [{:x :rank :aesthetics}] :n}`: dots ordered by item (group), then value; `:x` the bin centre, or with `:at :value` the dot's value.

  See also [[bins-of]]."
  [items options binwidth origin]
  (let [dots (sort-by :value (for [[i {:keys [values aesthetics]}] (map-indexed vector items)
                                   v values]
                               {:value (double v) :item i :aesthetics aesthetics}))
        bins (bins-of (map :value dots) options binwidth origin)
        at-value? (= :value (:at options))]
    (loop [bins-left bins
           dots-left dots
           acc []]
      (if-let [{:keys [center values]} (first bins-left)]
        (let [[in-bin more] (split-at (count values) dots-left)
              ordered (sort-by (juxt :item :value) in-bin)]
          (recur (rest bins-left) more
                 (conj acc {:n (count ordered)
                            :dots (vec (map-indexed (fn [rank {:keys [value aesthetics]}]
                                                      {:x (if at-value? value center) :rank rank :aesthetics aesthetics})
                                                    ordered))})))
        acc))))

(defn- slot-stacks
  "Gives the stacks of one panel's items, the items of one level and slot binned together.

  Parameters:

  - `items` (sequence of maps): the panel's items - `:level`, `:slot`, `:values`, `:aesthetics`.
  - `options`, `binwidth`, `origin`: as [[stack-dots]].

  Returns a vector of `{:level :slot :stacks [..]}`, one per level and slot (see [[stack-dots]])."
  [items options binwidth origin]
  (vec (for [[[level slot] its] (group-by (juxt :level :slot) items)]
         {:level level :slot slot :stacks (stack-dots its options binwidth origin)})))

(defn- with-label-x
  "Gives a panel's items with the value where their n label goes.

  Parameters:

  - `items` (sequence of maps): the panel's items - `:level`, `:slot`.
  - `slots` (sequence of maps): the panel's stacks (see [[slot-stacks]]).
  - `half` (double): half the bin width.

  Returns a vector of the items, each with `:label-x` - the far edge (value units) of the farthest dot of its slot."
  [items slots ^double half]
  (let [far (into {} (for [{:keys [level slot stacks]} slots]
                       [[level slot] (m/+ half (v/mx (for [st stacks d (:dots st)] (:x d))))]))]
    (mapv #(assoc % :label-x (far [(:level %) (:slot %)])) items)))

(defn- compute-in-strip-frame
  "Computes the dot plot's stats in the strip frame (levels on `:y`).

  Parameters:

  - `chart` (map): the chart value - `:options` (`:method`, `:binwidth` default the value range / 30, `:origin`, `:at`, `:position` default `:overlay`, the strip options, style options).
  - `chart-split` (map): the chart's rows split per panel.
  - `env` (map): the compute environment - `:theme`, `:plot-scales` ...

  Returns `{:data :extents :padding :padding-explicit :padding-px :na :dropped}`: per panel `{:slots [..] :items [..] :style st :place p :options o :binwidth w :max-count n :mirror b}` (see [[slot-stacks]]; `:max-count` - the tallest stack over all panels, at least 1); x extents - the dots' positions widened by half a bin."
  [{:keys [options] :as chart} chart-split {:keys [theme] :as env}]
  (let [split-options (cond-> options (nil? (:position options)) (assoc :position :overlay))
        {:keys [place position items panels dropped]} (strip/split chart-split split-options :center env)
        all-values (mapcat :values (mapcat val items))
        binwidth (if (seq all-values) (double (data/option options :binwidth (dots/default-binwidth all-values))) 1.0)
        origin (if (seq all-values) (v/mn all-values) 0.0)
        slots (update-vals items #(slot-stacks % options binwidth origin))
        half (m/* 0.5 binwidth)
        labelled (into {} (map (fn [[panel its]] [panel (with-label-x its (slots panel) half)]) items))
        max-count (long (reduce m/long-max 1 (for [ss (vals slots) s ss st (:stacks s)] (:n st))))
        dot-style (theme/resolved-style theme chart)]
    {:data (into {} (map (fn [[panel ss]]
                           [panel {:slots ss :items (labelled panel) :style dot-style :place place :options options
                                   :binwidth binwidth :max-count max-count :mirror (= :mirror position)}])
                         slots))
     :extents (merge-with merge (group/aesthetic-extents panels (:plot-scales env))
                          (strip/extents place
                                         (update-vals slots (fn [ss] (map #(assoc % :xs (for [st (:stacks %) d (:dots st)] (:x d))) ss)))
                                         (fn [{:keys [xs]}] [(m/- (v/mn xs) half) (m/+ (v/mx xs) half)])))
     :padding (strip/padding place options)
     :padding-explicit (strip/explicit-padding place options)
     :padding-px (strip-chart/n-label-room options labelled env)
     :na (group/na-dims panels)
     :dropped dropped}))

(defn- dot-diameter
  "Gives the pixel diameter of the dots of a panel.

  Parameters:

  - `value-pixel` (function): of a value giving its pixel along the value axis.
  - `span` (function): of a level giving the pixel length of its cross range.
  - `slots` (sequence of maps): the panel's stacks - `:level`, `:slot`.
  - `sizes` (map): `:x0` - a value to measure the bin width from, `:binwidth` - data units, `:max-count` - the tallest stack.

  Returns the bin width in pixels, at most the cross length of every slot over `:max-count` dots."
  ^double [value-pixel span slots {:keys [^double x0 ^double binwidth max-count]}]
  (let [bw (m/abs (m/- (double (value-pixel (m/+ x0 binwidth))) (double (value-pixel x0))))
        fit (reduce m/min ##Inf (for [{:keys [level slot]} slots
                                      :let [[^double lo ^double hi] slot
                                            s (double (span level))]
                                      :when (m/pos? s)]
                                  (m// (m/* s (m/- hi lo)) (double max-count))))]
    (m/min bw (double fit))))

(defn- cross-of
  "Gives the cross coordinate of a dot in its stack.

  Parameters:

  - `slot` (`[lo hi]`): the stack's slot, cross units.
  - `stacking` (map): `:stack-dir` (`:up` - from the slot's low edge, `:center` - centred on the slot), `:mirror` (from the centre line toward the slot's side).
  - `[rank n]` (longs): the dot's rank in a stack of `n`.
  - `d` (double): the diameter in cross units.

  Returns the dot centre's cross coordinate."
  ^double [slot {:keys [stack-dir mirror]} [^long rank ^long n] ^double d]
  (let [[^double lo ^double hi] slot]
    (cond
      (= :center stack-dir) (m/+ (m/* 0.5 (m/+ lo hi)) (m/* d (m/- (double rank) (m/* 0.5 (m/long-dec n)))))
      mirror (m/* (strip/slot-side slot) d (m/+ (double rank) 0.5))
      :else (m/+ lo (m/* d (m/+ (double rank) 0.5))))))

(defn- draw-in-strip-frame
  "Draws the dot stacks of one panel in the strip frame, then the n labels.

  Parameters:

  - `_chart` (map): the chart value (unused).
  - `computed` (map): the panel's stats - `:slots`, `:items`, `:style`, `:place`, `:options` (`:stack-dir` default `:up`), `:binwidth`, `:max-count`, `:mirror`.
  - `canvas`: the clojure2d canvas.
  - `box` (map): the panel box.
  - `scales` (map): finalized scales (in the strip frame).
  - `env` (map): the draw environment - `:theme`.

  Dots are drawn at [[dot-diameter]] unless the style has a `:size`; a group outside its scale's domain, a level without span and dots the scales can't place are skipped.

  See also [[cross-of]]."
  [_chart {:keys [slots items style place options binwidth max-count mirror]} canvas box scales env]
  (let [{:keys [point span value-pixel] :as pl} (strip-chart/placement box scales options place)
        x0 (some (fn [{:keys [stacks]}] (some #(:x (first (:dots %))) stacks)) slots)
        stack-dir (data/option options :stack-dir :up)]
    (when x0
      (let [d (dot-diameter value-pixel span slots {:x0 x0 :binwidth binwidth :max-count max-count})
            stacking {:stack-dir stack-dir :mirror mirror}]
        (c2d/set-stroke-custom canvas {:size (:stroke-width style)})
        (doseq [{:keys [level slot stacks]} slots
                :let [s (double (span level))]
                :when (m/pos? s)
                {:keys [n dots]} stacks
                {:keys [x rank aesthetics]} dots
                :let [st (group/style style aesthetics scales (:theme env))
                      ^Vec2 p (point x level (cross-of slot stacking [rank n] (m// d s)))]
                :when (and st (data/finite-point? p))]
          (shape/draw! canvas (:shape st) p (update st :size #(if (some? %) % d))))))
    (strip-chart/draw-n-labels! canvas pl items :label-x options scales env)))

(defmethod strip-chart/in-frame-compute :dotplot [chart chart-split env] (compute-in-strip-frame chart chart-split env))

(defmethod strip-chart/in-frame-draw :dotplot [chart computed canvas box scales env]
  (draw-in-strip-frame chart computed canvas box scales env))

(defn legend
  "Draws the dot plot's key glyph: its point shape fitted and centred in a legend key.

  Parameters:

  - `chart` (map): the chart value.
  - `attributes` (map): the key's mapped style attributes, over the chart's style.
  - `canvas`: the clojure2d canvas.
  - `box` (map): the key box `{:x :y :w :h}` in pixels.
  - `env` (map): the legend environment - `:theme`.

  Without a `:size` the dot is 70% of the key box's smaller side.

  See also [[cljplot2.graphics.shape/draw-key!]]."
  [chart attributes canvas {:keys [^double w ^double h] :as box} env]
  (let [st (update (theme/key-style env chart attributes nil)
                   :size #(if (some? %) % (m/* draw/key-glyph-share (m/min w h))))]
    (shape/styled-key! canvas box st true)))
