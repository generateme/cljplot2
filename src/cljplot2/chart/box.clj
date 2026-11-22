(ns cljplot2.chart.box
  "Box plot: a Tukey box per level and group of the values - a strip chart (see [[cljplot2.data.strip]]: levels on
  the categorical axis, `:level-axis`; placement `:place`, `:width`, `:thickness`, groups `:position`). Axes below are
  named in the strip frame - `:x` the values, `:y` the levels; exchange them for vertical boxes (levels on `:x`).

  Per item: quartiles Q1, median, Q3 (fastmath `percentiles`, legacy estimation), whiskers to the adjacent values
  (the most extreme values within 1.5 IQR of the box, fastmath `adjacent-values`), outliers beyond them.
  Drawn as the old cljplot box: the IQR rectangle filled with `:color` and outlined with `:stroke-color` (a color,
  `:none`, or `:darker` - the darkened fill, default; whiskers and outlier outlines use it too, whiskers the
  darkened fill under `:none`), the median line black or white by the fill's luma, whiskers with caps, outliers as
  `:shape` points of `:size`.

  Options (style defaults from theme `[:chart :box]`): `:color`, `:stroke-color`, `:stroke-width`, `:shape`, `:size`
  (outliers);
  `:outliers` (default true), `:mean` (a mean marker, default false), `:varwidth` (box width proportional to the
  square root of its count, relative to the largest, default false), `:padding {:x [lo hi]}` (default
  `[0.05 0.05]`), plus the strip options and `:data` / `:mapping` (`:x` values, optional `:y` levels).

  `:stat` - `:tukey` (default, the statistics above) or `:identity` (pre-aggregated, Vega-Lite
  `boxplot_preaggregated`): one row per level and group carries its statistics through the mapping keys `:lower :q1
  :median :q3 :upper` (whisker ends, quartiles, median - finite and ordered), optional `:outliers` (a collection of
  numbers), `:mean` (needed by the `:mean` marker) and `:n` (needed by `:varwidth` and `:n-label`); `:x` isn't read
  (map the levels, or nothing for one box) - see [[identity-stats]].

  `:variant` - `:classic` (default, the box above) or `:tufte` (E. Tufte's quartile plot, lattice `panel.bwtufte`): no
  box and no median line - the median a `:shape` point of `:size` in `:color`, a line from each whisker end to its
  quartile (the inter-quartile gap left empty), no caps; outliers as above, the `:mean` marker a hollow circle in
  `:color`; `:varwidth` and the fill patterns (`:pattern`, `:pattern-color`) have no effect (nothing has a width
  or a fill)."
  (:require [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.stats :as stats]
            [fastmath.vector :as v]
            [cljplot2.data.group :as group]
            [cljplot2.data.rows :as data]
            [cljplot2.data.strip :as strip]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.shape :as shape]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries
  (into [(schema/optional :color {:description "box fill colour; the Tufte variant's median point and mean marker (a mapped :color wins)"}
                          schema/solid-color)
         (schema/optional :stroke-color {:description "outline colour: a colour, :none or :darker (the darkened fill); also the whiskers and outlier outlines"}
                          draw/stroke-color)
         (schema/optional :stroke-width {:description "outline and whisker width in pixels"} schema/non-neg)
         (schema/optional :shape {:description "outlier point shape, also the Tufte median point (a mapped :shape wins)"} schema/shape)
         (schema/optional :size {:description "outlier point size in pixels, also the Tufte median point; the mean marker is half of it (a mapped :size wins)"}
                          schema/non-neg)]
        draw/pattern-style-entries))

(def ^:private padding-entry
  (schema/optional :padding {:description "padding of the data span per position axis, {:x [lo hi] :y [lo hi]} fractions of the span"
                             :cljplot2/default-doc "[0.05 0.05] along the value axis; with :place :bottom also :bottom-share below the data"}
                   data/position-padding))

(def ^:private stat-keys-doc
  "with :stat :identity, the statistics of a level and group")

(def schemas
  "Schemas of the box chart: `:style` (theme `[:chart :box]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into theme/theme-style-entries)
                (into strip/options)
                (conj (schema/optional :stat {:description "statistics: :tukey (computed from the values) or :identity (pre-aggregated, one row per level and group read through the mapping keys :lower :q1 :median :q3 :upper)"
                                              :default :tukey}
                                       [:enum :tukey :identity])
                      (schema/nilable :outliers {:description "draw the outliers (and include them in the value extent)" :default true} schema/bool)
                      (schema/nilable :mean {:description "draw a mean marker (with :stat :identity it needs :mean mapped)" :default false} schema/bool)
                      (schema/nilable :varwidth {:description "box width proportional to the square root of its count, relative to the largest (with :stat :identity it needs :n mapped)"
                                                 :default false}
                                      schema/bool)
                      (schema/nilable :variant {:description "drawing: :classic (box, median line, whiskers with caps) or :tufte (Tufte's quartile plot: whisker lines and a median point, no box)"
                                                :default :classic}
                                      [:enum :classic :tufte])
                      padding-entry))
   :mapping (schema/with-rules
              [:map
               (schema/nilable :x {:cljplot2/section :values :description "values (data units) on the value axis (not read with :stat :identity), or levels for vertical boxes"}
                               schema/accessor)
               (schema/nilable :y {:cljplot2/section :values :description "levels on the categorical axis (absent - one box), or values for vertical boxes"} schema/accessor)
               (schema/nilable :color {:cljplot2/section :values :description "groups, each a box in its own fill colour"} schema/accessor)
               (schema/nilable :pattern {:cljplot2/section :values :description "groups, each a box with its own fill pattern"} schema/accessor)
               (schema/nilable :shape {:cljplot2/section :values :description "groups, each with its own outlier shape"} schema/accessor)
               (schema/nilable :size {:cljplot2/section :values :description "groups, each with its own outlier size"} schema/accessor)
               (schema/nilable :group {:cljplot2/section :values :description "groups without a visual aesthetic, each its own box"} schema/accessor)
               (schema/nilable :lower {:cljplot2/section :summary :description (str stat-keys-doc ": the lower whisker end")} schema/accessor)
               (schema/nilable :q1 {:cljplot2/section :summary :description (str stat-keys-doc ": the first quartile")} schema/accessor)
               (schema/nilable :median {:cljplot2/section :summary :description (str stat-keys-doc ": the median")} schema/accessor)
               (schema/nilable :q3 {:cljplot2/section :summary :description (str stat-keys-doc ": the third quartile")} schema/accessor)
               (schema/nilable :upper {:cljplot2/section :summary :description (str stat-keys-doc ": the upper whisker end")} schema/accessor)
               (schema/nilable :outliers {:cljplot2/section :summary :description (str stat-keys-doc ": a collection of outlier values (optional)")} schema/accessor)
               (schema/nilable :mean {:cljplot2/section :summary :description (str stat-keys-doc ": the mean (needed by the :mean option)")} schema/accessor)
               (schema/nilable :n {:cljplot2/section :summary :description (str stat-keys-doc ": the count (needed by :varwidth and :n-label)")} schema/accessor)]
              [#(when-not (or (some? (:x %)) (some? (:y %)) (some? (:q1 %)))
                  "should map :x or :y (the values), or the statistics (:q1 ... with :stat :identity)")
               nil])})

(defn- style
  [options theme]
  (merge (theme/chart-style theme :box options) (select-keys options [:color :stroke-color :stroke-width :shape :size :pattern :pattern-color])))

(defn box-stats
  "Tukey box statistics of `values` (non-empty): `{:q1 :median :q3 :lav :uav :outliers [..] :mean :n}`."
  [values]
  (let [[q1 median q3] (stats/percentiles values [25.0 50.0 75.0])
        [lav uav] (stats/adjacent-values values q1 q3 median)]
    {:q1 q1 :median median :q3 q3 :lav lav :uav uav
     :outliers (vec (stats/outliers values q1 q3))
     :mean (stats/mean values)
     :n (count values)}))

(def ^:private identity-keys
  "Mapping keys of the statistics a `:stat :identity` row carries, in their required order."
  [:lower :q1 :median :q3 :upper])

(defn- check-identity-mapping!
  "Throw for a `:stat :identity` box whose `mapping` lacks a statistic (`{:key :mapping}`) or an option's input -
  `:mean` without `:mean`, `:varwidth` / `:n-label` without `:n` mapped (`{:key option}`)."
  [mapping options]
  (when-let [missing (seq (remove #(some? (get mapping %)) identity-keys))]
    (schema/fail! (str "Box :stat :identity needs the statistics mapped: " (pr-str (vec missing)) ".")
                  {:key :mapping :value (vec missing)}))
  (doseq [[option k] [[:mean :mean] [:varwidth :n] [:n-label :n]]
          :when (and (get options option) (nil? (get mapping k)))]
    (schema/fail! (str "Box " option " with :stat :identity needs " k " mapped.") {:key option :value k})))

(defn identity-stats
  "Box statistics (as [[box-stats]]) of a `:stat :identity` item (`:rows` - exactly one) read through `mapping`:
  `:lower :q1 :median :q3 :upper` (finite, ordered), `:outliers` (finite numbers; absent - none), `:mean`, `:n`.
  Throws `ex-info {:key :mapping}` for two or more rows of one level and group, a missing, non-finite or unordered
  statistic, bad outliers or `:n`."
  [{:keys [rows level aesthetics]} mapping]
  (when (not= 1 (count rows))
    (schema/fail! "Box :stat :identity takes one row per level and group." {:key :mapping :level level :group aesthetics
                                                                            :value (count rows)}))
  (let [row (first rows)
        read (fn [k] (when-let [acc (get mapping k)] ((data/row-accessor k acc) row)))
        [lower q1 median q3 upper :as vs] (mapv read identity-keys)
        outliers (read :outliers)
        mean (read :mean)
        n (read :n)]
    (when-not (and (every? schema/finite? vs) (apply <= (map double vs)))
      (schema/fail! "Box :stat :identity needs finite statistics with :lower <= :q1 <= :median <= :q3 <= :upper."
                    {:key :mapping :level level :value (zipmap identity-keys vs)}))
    (when-not (or (nil? outliers) (and (coll? outliers) (every? schema/finite? outliers)))
      (schema/fail! "Box :outliers should be a collection of finite numbers." {:key :mapping :level level :value outliers}))
    (when-not (or (nil? mean) (schema/finite? mean))
      (schema/fail! "Box :mean should be a finite number." {:key :mapping :level level :value mean}))
    (when-not (or (nil? n) (and (integer? n) (pos? (long n))))
      (schema/fail! "Box :n should be a positive integer." {:key :mapping :level level :value n}))
    {:q1 q1 :median median :q3 q3 :lav lower :uav upper :outliers (vec outliers) :mean mean :n (or n 1)}))

(defn width-factors
  "Item -> width factor: `sqrt(n / max n)` over all `items` with `varwidth?`, else 1."
  [items varwidth?]
  (let [nmax (double (reduce max 1 (map :n items)))]
    (fn ^double [item] (if varwidth? (m/sqrt (/ (double (:n item)) nmax)) 1.0))))

(defn- compute-in-strip-frame
  "Per panel: `{:items [item ...] :style st :place p :options o}`, items with [[box-stats]] (`:stat :identity` -
  [[identity-stats]]) and `:width-factor`;
  extents (x - whiskers and, with `:outliers`, outliers; y - see [[cljplot2.data.strip/extents]]), padding, `:na`,
  `:dropped`."
  [{:keys [options]} {:keys [mapping] :as chart-split} {:keys [theme] :as env}]
  (let [identity? (= :identity (:stat options))
        _ (when identity? (check-identity-mapping! mapping options))
        split-options (cond-> options identity? (assoc ::strip/count-rows true ::strip/rows true))
        {:keys [place items panels dropped]} (strip/split chart-split split-options :center env)
        stats-of (if identity? #(identity-stats % mapping) #(box-stats (:values %)))
        items (update-vals items (fn [its] (mapv #(dissoc (merge % (stats-of %)) :rows) its)))
        factor (width-factors (mapcat val items) (:varwidth options))
        items (update-vals items (fn [its] (mapv #(assoc % :width-factor (factor %)) its)))
        outliers? (data/option options :outliers true)
        x-range (fn [{:keys [lav uav outliers]}]
                  (let [vs (cond-> [lav uav] outliers? (into outliers))]
                    [(reduce min vs) (reduce max vs)]))]
    {:data (update-vals items (fn [its] {:items its :style (style options theme) :place place :options options}))
     :extents (merge-with merge (group/aesthetic-extents panels (:plot-scales env)) (strip/extents place items x-range))
     :padding (strip/padding place options)
     :padding-explicit (strip/explicit-padding place options)
     :padding-px (strip/n-label-room options items env)
     :na (group/na-dims panels)
     :dropped dropped}))

(defn- line! [canvas ^Vec2 a ^Vec2 b] (c2d/line canvas (.x a) (.y a) (.x b) (.y b)))

(defn- finite? [^Vec2 p] (and (m/valid-double? (.x p)) (m/valid-double? (.y p))))

(defn- draw-tufte!
  "Draw a Tufte quartile item: whisker-end-to-quartile lines along the item's centre `c0` and the median point (`p` -
  value, cross position -> pixel), outliers and the mean marker as asked."
  [canvas p {:keys [q1 median q3 lav uav outliers mean]} c0 st outliers? mean?]
  (let [fill (:color st)
        stroke (draw/outline-color (:stroke-color st) fill)]
    (c2d/set-stroke-custom canvas {:size (double (:stroke-width st)) :cap :butt})
    (c2d/set-color canvas fill)
    (line! canvas (p lav c0) (p q1 c0))
    (line! canvas (p q3 c0) (p uav c0))
    (shape/draw! canvas (:shape st) (p median c0) (assoc st :stroke-color stroke))
    (when outliers?
      (doseq [o outliers]
        (shape/draw! canvas (:shape st) (p o c0) (assoc st :stroke-color stroke))))
    (when mean?
      (shape/draw! canvas \o (p mean c0) {:size (m/max 3.0 (* 0.5 (double (:size st)))) :color fill
                                         :stroke-width (double (:stroke-width st))}))))

(defn- draw-in-strip-frame
  "Draw the boxes of one panel (classic boxes or Tufte quartile plots, see the namespace doc)."
  [_chart {:keys [items style place options]} canvas box scales env]
  (let [{:keys [point] :as pl} (strip/placement box scales options place)
        outliers? (data/option options :outliers true)
        pattern-fill (draw/pattern-fill-cache)]
    (doseq [{:keys [level slot q1 median q3 lav uav outliers mean width-factor aesthetics]} items
            :let [st (group/style style aesthetics scales (:theme env))]
            :when st]
      (let [c0 (strip/in-slot slot 0.0)
            hw (* (strip/slot-half slot) (double width-factor))
            p (fn [v cross] (point v level cross))
            a (p q1 (- c0 hw)) b (p q3 (+ c0 hw))]
        (cond
          (not (and (finite? a) (finite? b))) nil
          (= :tufte (:variant options)) (draw-tufte! canvas p {:q1 q1 :median median :q3 q3 :lav lav :uav uav
                                                               :outliers outliers :mean mean}
                                                     c0 st outliers? (:mean options))
          :else
          (let [fill (:color st)
                stroke (draw/outline-color (:stroke-color st) fill)
                ;; whiskers stay visible without an outline: the darkened fill
                whisker (if (theme/none? stroke) (c/darken fill) stroke)
                sw (double (:stroke-width st))]
            (c2d/set-stroke-custom canvas {:size sw :cap :butt})
            (c2d/set-color canvas whisker)
            (line! canvas (p lav c0) (p q1 c0))
            (line! canvas (p q3 c0) (p uav c0))
            (line! canvas (p lav (- c0 (* 0.5 hw))) (p lav (+ c0 (* 0.5 hw))))
            (line! canvas (p uav (- c0 (* 0.5 hw))) (p uav (+ c0 (* 0.5 hw))))
            (let [x (m/min (.x ^Vec2 a) (.x ^Vec2 b)) y (m/min (.y ^Vec2 a) (.y ^Vec2 b))
                  w (m/abs (- (.x ^Vec2 b) (.x ^Vec2 a))) h (m/abs (- (.y ^Vec2 b) (.y ^Vec2 a)))]
              (c2d/set-color canvas fill)
              (c2d/rect canvas x y w h)
              (draw/fill-with-pattern! canvas (pattern-fill st fill) #(c2d/rect canvas x y w h))
              (when-not (theme/none? stroke)
                (c2d/set-color canvas stroke)
                (c2d/rect canvas x y w h true)))
            (c2d/set-color canvas (draw/contrast-color fill))
            (line! canvas (p median (- c0 hw)) (p median (+ c0 hw)))
            (when outliers?
              (doseq [o outliers]
                (shape/draw! canvas (:shape st) (p o c0) (assoc st :stroke-color stroke))))
            (when (:mean options)
              (shape/draw! canvas \O (p mean c0) {:size (m/max 3.0 (* 0.5 (double (:size st))))
                                                   :color (draw/contrast-color fill) :stroke-color stroke
                                                   :stroke-width sw}))))))
    (strip/draw-n-labels! canvas pl items
                          (fn [{:keys [uav outliers]}] (if (and outliers? (seq outliers)) (reduce max uav outliers) uav))
                          options scales env)))


(defn compute
  "Stats of the chart with its levels on the categorical axis (see [[cljplot2.data.strip/compute-oriented]]):
  [[compute-in-strip-frame]] in the strip frame."
  [chart chart-split env]
  (strip/compute-oriented compute-in-strip-frame chart chart-split env))

(defn draw
  "Draw one panel (see [[cljplot2.data.strip/draw-oriented]]): [[draw-in-strip-frame]] in the strip frame."
  [chart computed canvas box scales env]
  (strip/draw-oriented draw-in-strip-frame chart computed canvas box scales env))
(defn- tufte-key!
  "Key glyph of the Tufte variant: a point between two short lines, centred in `box`."
  [canvas st {:keys [x y w h]}]
  (let [cx (+ (double x) (* 0.5 (double w))) cy (+ (double y) (* 0.5 (double h)))
        reach (* 0.4 (double w)) gap (* 0.15 (double w))]
    (c2d/set-stroke-custom canvas {:size (double (:stroke-width st)) :cap :butt})
    (c2d/set-color canvas (:color st))
    (c2d/line canvas (- cx reach) cy (- cx gap) cy)
    (c2d/line canvas (+ cx gap) cy (+ cx reach) cy)
    (shape/draw! canvas (:shape st) (v/vec2 cx cy)
                 (assoc st :size (m/min (double (:size st)) (* 0.5 (double h)))
                        :stroke-color (draw/outline-color (:stroke-color st) (:color st))))))

(defn- classic-key!
  "Key glyph of the classic box: a small box with a median line, centred in `box`."
  [canvas st {:keys [x y w h]}]
  (let [bw (* 0.7 (double w)) bh (* 0.5 (double h))
        bx (+ (double x) (* 0.5 (- (double w) bw))) by (+ (double y) (* 0.5 (- (double h) bh)))]
    (c2d/set-stroke-custom canvas {:size (double (:stroke-width st))})
    (c2d/set-color canvas (:color st))
    (c2d/rect canvas bx by bw bh)
    (draw/fill-with-pattern! canvas (draw/pattern-fill st (:color st)) #(c2d/rect canvas bx by bw bh))
    (let [stroke (draw/outline-color (:stroke-color st) (:color st))]
      (when-not (theme/none? stroke)
        (c2d/set-color canvas stroke)
        (c2d/rect canvas bx by bw bh true)))
    (c2d/set-color canvas (draw/contrast-color (:color st)))
    (c2d/line canvas (+ bx (* 0.5 bw)) by (+ bx (* 0.5 bw)) (+ by bh))))

(defn legend
  "Key glyph: a small box with a median line (Tufte variant: a point between two short lines), centred in `box`."
  [{:keys [options]} attributes canvas box env]
  (let [st (merge (style options (:theme env)) (select-keys attributes [:color :pattern]))]
    (if (= :tufte (:variant options))
      (tufte-key! canvas st box)
      (classic-key! canvas st box))))
