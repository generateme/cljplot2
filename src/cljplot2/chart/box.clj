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
  `[0.05 0.05]`), plus the strip options and `:data` / `:mapping` (`:x` values, optional `:y` levels). An n label
  (`:n-label`) sits past the upper whisker, or past the mark of the largest outlier drawn.

  `:stat` - `:tukey` (default, the statistics above) or `:identity` (pre-aggregated, Vega-Lite
  `boxplot_preaggregated`): one row per level and group carries its statistics through the mapping keys `:lower :q1
  :median :q3 :upper` (whisker ends, quartiles, median - finite and ordered), optional `:outliers` (a collection of
  numbers), `:mean` (needed by the `:mean` marker) and `:n` (needed by `:varwidth` and `:n-label`); `:x` isn't read
  (map the levels, or nothing for one box) - see [[identity-stats]].

  `:variant` - `:classic` (default, the box above) or `:tufte` (E. Tufte's quartile plot, lattice `panel.bwtufte`): no
  box and no median line - the median a `:shape` point of `:size` in `:color`, a line from each whisker end to its
  quartile (the inter-quartile gap left empty), no caps; outliers as above, the `:mean` marker a hollow circle in
  `:color`; `:varwidth` and the fill patterns (`:pattern`, `:pattern-color`) have no effect (nothing has a width
  or a fill).

  Functions: [[cljplot2.data.strip/box-stats]] - an item's statistics, [[cljplot2.data.strip/varwidth-factors]] - `:varwidth` box widths; [[legend]] - the chart contract's key glyph (the chart computes and draws through [[cljplot2.chart.strip/compute]] and [[cljplot2.chart.strip/draw]]); [[schemas]] - its style, options and mapping."
  (:require [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.chart.common :as common]
            [cljplot2.data.group :as group]
            [cljplot2.data.rows :as data]
            [cljplot2.chart.strip :as strip-chart]
            [cljplot2.data.strip :as strip]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.shape :as shape]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries
  "Map entries of the box style (theme `[:chart :box]`), also its options."
  (into [(schema/optional :color {:description "box fill colour; the Tufte variant's median point and mean marker (a mapped :color wins)"}
                          schema/solid-color)
         (schema/optional :stroke-color {:description "outline colour: a colour, :none or :darker (the darkened fill); also the whiskers and outlier outlines"}
                          draw/stroke-color)
         (schema/optional :stroke-width {:description "outline and whisker width in pixels (0 - no box outline)"} schema/non-neg)
         (schema/optional :shape {:description "outlier point shape, also the Tufte median point (a mapped :shape wins)"} schema/shape)
         (schema/optional :size {:description "outlier point size in pixels, also the Tufte median point; the mean marker is half of it (a mapped :size wins)"}
                          schema/non-neg)]
        draw/pattern-style-entries))

(def ^:private padding-entry
  "Map entry of the `:padding` option."
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

(def ^:private identity-keys
  "Mapping keys of the statistics a `:stat :identity` row carries, in their required order."
  [:lower :q1 :median :q3 :upper])

(defn- check-identity-mapping!
  "Checks that a `:stat :identity` box maps every statistic and the inputs of its options.

  Parameters:

  - `mapping` (map): the chart's merged mapping.
  - `options` (map): the chart's options - `:mean`, `:varwidth`, `:n-label`.

  Returns `nil`.

  Throws `ex-info {:key :mapping}` for an unmapped statistic of [[identity-keys]], `{:key option}` for `:mean` without `:mean` mapped or `:varwidth` / `:n-label` without `:n` mapped."
  [mapping options]
  (when-let [missing (seq (remove #(some? (get mapping %)) identity-keys))]
    (data/fail-mapping! (str "Box :stat :identity needs the statistics mapped: " (pr-str (vec missing)) ".")
                  (first missing) {:missing (vec missing)}))
  (doseq [[option k] [[:mean :mean] [:varwidth :n] [:n-label :n]]
          :when (and (get options option) (nil? (get mapping k)))]
    (schema/fail! (str "Box " option " with :stat :identity needs " k " mapped.") {:key option :value k})))

(defn- identity-stats
  "Gives the box statistics of a pre-aggregated item (`:stat :identity`), read from its row.

  Parameters:

  - `item` (map): a strip item - `:rows` (exactly one), `:level`, `:aesthetics` (for errors).
  - `mapping` (map): the chart's mapping - accessors of `:lower :q1 :median :q3 :upper` (finite, ordered), optional `:outliers` (a collection of finite numbers; absent - none), `:mean` (finite), `:n` (a positive integer).

  Returns the statistics as [[cljplot2.data.strip/box-stats]] gives them; `:n` 1 when not mapped.

  Throws `ex-info {:key :mapping}` for two or more rows of one level and group, a missing, non-finite or unordered statistic, bad outliers, a non-finite mean or a bad `:n`."
  [{:keys [rows level aesthetics]} mapping]
  (when (m/not== 1 (count rows))
    (schema/fail! "Box :stat :identity takes one row per level and group." {:key :mapping :level level :group aesthetics
                                                                            :value (count rows)}))
  (let [row (first rows)
        stat-of (fn [k] (when-let [acc (get mapping k)] ((data/row-accessor k acc) row)))
        [lower q1 median q3 upper :as vs] (mapv stat-of identity-keys)
        outliers (stat-of :outliers)
        mean (stat-of :mean)
        n (stat-of :n)]
    (when-not (and (every? schema/finite? vs) (apply m/<= vs))
      (schema/fail! "Box :stat :identity needs finite statistics with :lower <= :q1 <= :median <= :q3 <= :upper."
                    {:key :mapping :level level :value (zipmap identity-keys vs)}))
    (when-not (or (nil? outliers) (and (coll? outliers) (every? schema/finite? outliers)))
      (schema/fail! "Box :outliers should be a collection of finite numbers." {:key :mapping :level level :value outliers}))
    (when-not (or (nil? mean) (schema/finite? mean))
      (schema/fail! "Box :mean should be a finite number." {:key :mapping :level level :value mean}))
    (when-not (or (nil? n) (and (integer? n) (m/pos? (long n))))
      (schema/fail! "Box :n should be a positive integer." {:key :mapping :level level :value n}))
    {:q1 q1 :median median :q3 q3 :lav lower :uav upper :outliers (vec outliers) :mean mean :n (or n 1)}))

(defn- n-label-gap
  "Gives the pixels an item's n label keeps past its largest drawn value, besides the usual gap: past the mark drawn there.

  Parameters:

  - `item` (map): the item - `:uav` (the upper whisker end), `:outliers`.
  - `st` (map): the item's style - `:size`, `:stroke-width`.
  - `options` (map): the chart options - `:outliers` (default true).

  Returns half the outlier size and stroke width when the largest value is a drawn outlier, else 0 (the whisker end)."
  ^double [{:keys [^double uav outliers]} st options]
  (if (and (data/option options :outliers true) (some #(m/> (double %) uav) outliers))
    (m/* 0.5 (m/+ (double (:size st)) (double (:stroke-width st))))
    0.0))

(defn- compute-in-strip-frame
  "Computes the box chart's stats in the strip frame (levels on `:y`).

  Parameters:

  - `chart` (map): the chart value - `:options` (`:stat`, `:outliers`, `:varwidth`, the strip options, style options).
  - `chart-split` (map): the chart's rows split per panel, with the merged `:mapping`.
  - `env` (map): the compute environment - `:theme`, `:plot-scales` ...

  Returns `{:data :extents :padding :padding-explicit :padding-px :na :dropped}`: per panel `{:items [item ...] :style st :place p :options o}`, each item with [[cljplot2.data.strip/box-stats]] (`:stat :identity` - [[identity-stats]], its row dropped) and `:width-factor` (see [[cljplot2.data.strip/varwidth-factors]]); x extents - the whisker ends and, with `:outliers`, the outliers.

  Throws as [[identity-stats]] and `check-identity-mapping!` for a bad `:stat :identity` mapping."
  [{:keys [options] :as chart} {:keys [mapping] :as chart-split} {:keys [theme] :as env}]
  (let [identity? (= :identity (:stat options))
        _ (when identity? (check-identity-mapping! mapping options))
        split-options (cond-> options identity? (assoc ::strip/count-rows true ::strip/rows true))
        {:keys [place items panels dropped]} (strip/split chart-split split-options :center env)
        stats-of (if identity? #(identity-stats % mapping) #(strip/box-stats (:values %)))
        stat-items (update-vals items (fn [its] (mapv #(dissoc (merge % (stats-of %)) :rows) its)))
        factor (strip/varwidth-factors (mapcat val stat-items) (:varwidth options))
        sized-items (update-vals stat-items (fn [its] (mapv #(assoc % :width-factor (factor %)) its)))
        outliers? (data/option options :outliers true)
        x-range (fn [{:keys [lav uav outliers]}]
                  (let [vs (cond->> [lav uav] outliers? (concat outliers))]
                    [(v/mn vs) (v/mx vs)]))
        box-style (theme/resolved-style theme chart)]
    {:data (update-vals sized-items (fn [its] {:items its :style box-style :place place :options options}))
     :extents (merge-with merge (group/aesthetic-extents panels (:plot-scales env)) (strip/extents place sized-items x-range))
     :padding (strip/padding place options)
     :padding-explicit (strip/explicit-padding place options)
     ;; room past the top outlier's mark of the chart's style (a mapped :size draws with its own, see draw-in-strip-frame)
     :padding-px (strip-chart/n-label-room options
                                           (update-vals sized-items (fn [its] (map #(assoc % :n-label-gap (n-label-gap % box-style options)) its)))
                                           env)
     :na (group/na-dims panels)
     :dropped dropped}))

(defn- mean-marker-size
  "Gives the size of the mean marker: half the style's `:size`, at least 3 pixels.

  Parameters:

  - `st` (map): the item's style - `:size`."
  ^double [st]
  (m/max 3.0 (m/* 0.5 (double (:size st)))))

(defn- draw-tufte!
  "Draws a Tufte quartile item: whisker-end-to-quartile lines along the item's centre, the median point, and the outliers and the mean marker as asked.

  Parameters:

  - `canvas`: the clojure2d canvas.
  - `p` (function): of a value and a cross position giving the pixel `Vec2`.
  - `stats` (map): the item's `:q1 :median :q3 :lav :uav :outliers :mean`.
  - `c0` (double): the item's centre across its slot.
  - `st` (map): the item's style - `:color`, `:stroke-color`, `:stroke-width`, `:shape`, `:size`.
  - `outliers?`, `mean?`: draw the outliers / the mean marker (a hollow circle in the fill colour).

  See also [[draw-in-strip-frame]]."
  [canvas p {:keys [q1 median q3 lav uav outliers mean]} c0 st outliers? mean?]
  (let [fill (:color st)
        stroke (draw/outline-color (:stroke-color st) fill)]
    (c2d/set-stroke-custom canvas {:size (:stroke-width st) :cap :butt})
    (c2d/set-color canvas fill)
    (c2d/line canvas (p lav c0) (p q1 c0))
    (c2d/line canvas (p q3 c0) (p uav c0))
    (shape/draw! canvas (:shape st) (p median c0) (assoc st :stroke-color stroke))
    (when outliers?
      (doseq [o outliers]
        (shape/draw! canvas (:shape st) (p o c0) (assoc st :stroke-color stroke))))
    (when mean?
      (shape/draw! canvas \o (p mean c0) {:size (mean-marker-size st) :color fill
                                         :stroke-width (double (:stroke-width st))}))))

(defn- draw-in-strip-frame
  "Draws the boxes of one panel in the strip frame: classic boxes or Tufte quartile plots (see the namespace doc).

  Parameters:

  - `_chart` (map): the chart value (unused).
  - `computed` (map): the panel's stats - `:items` (with box statistics and `:width-factor`), `:style`, `:place`, `:options` (`:variant`, `:outliers`, `:mean`).
  - `canvas`: the clojure2d canvas.
  - `box` (map): the panel box.
  - `scales` (map): finalized scales (in the strip frame).
  - `env` (map): the draw environment - `:theme`.

  An item whose group is outside its scale's domain, or whose box corners the scales can't place, is skipped; the n labels sit past the upper whisker (or past the mark of the largest outlier drawn - see `n-label-gap`).

  See also [[draw-tufte!]], [[cljplot2.chart.strip/placement]]."
  [_chart {:keys [items style place options]} canvas box scales env]
  (let [{:keys [point] :as pl} (strip-chart/placement box scales options place)
        outliers? (data/option options :outliers true)
        pattern-fill (draw/pattern-fill-cache)]
    (doseq [{:keys [level slot q1 median q3 lav uav outliers mean ^double width-factor aesthetics]} items
            :let [st (group/style style aesthetics scales (:theme env))]
            :when st]
      (let [c0 (strip/in-slot slot 0.0)
            hw (m/* (strip/slot-half slot) width-factor)
            cap (m/* 0.5 hw)
            p (fn [v cross] (point v level cross))
            ^Vec2 a (p q1 (m/- c0 hw))
            ^Vec2 b (p q3 (m/+ c0 hw))]
        (cond
          (not (and (data/finite-point? a) (data/finite-point? b))) nil
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
            (c2d/line canvas (p lav c0) (p q1 c0))
            (c2d/line canvas (p q3 c0) (p uav c0))
            (c2d/line canvas (p lav (m/- c0 cap)) (p lav (m/+ c0 cap)))
            (c2d/line canvas (p uav (m/- c0 cap)) (p uav (m/+ c0 cap)))
            (let [x (m/min (.x a) (.x b))
                  y (m/min (.y a) (.y b))
                  w (m/abs (m/- (.x b) (.x a)))
                  h (m/abs (m/- (.y b) (.y a)))]
              (draw/fill-patterned! canvas fill (pattern-fill st fill) #(c2d/rect % x y w h))
              ;; the stroke set above (butt caps) is kept for the median line: no `draw/outline!`
              (when (draw/visible-stroke? stroke sw)
                (c2d/set-color canvas stroke)
                (c2d/rect canvas x y w h true)))
            (c2d/set-color canvas (draw/contrast-color fill))
            (c2d/line canvas (p median (m/- c0 hw)) (p median (m/+ c0 hw)))
            (when outliers?
              (doseq [o outliers]
                (shape/draw! canvas (:shape st) (p o c0) (assoc st :stroke-color stroke))))
            (when (:mean options)
              (shape/draw! canvas \O (p mean c0) {:size (mean-marker-size st)
                                                   :color (draw/contrast-color fill) :stroke-color stroke
                                                   :stroke-width sw}))))))
    (strip-chart/draw-n-labels! canvas pl
                                ;; the gap past the top outlier's mark travels on the item
                                (map (fn [{:keys [aesthetics] :as item}]
                                       (if-let [st (group/style style aesthetics scales (:theme env))]
                                         (assoc item :n-label-gap (n-label-gap item st options))
                                         item))
                                     items)
                                (fn [{:keys [uav outliers]}] (if (and outliers? (seq outliers)) (reduce m/max uav outliers) uav))
                                options scales env)))

(defmethod strip-chart/in-frame-compute :box [chart chart-split env] (compute-in-strip-frame chart chart-split env))

(defmethod strip-chart/in-frame-draw :box [chart computed canvas box scales env]
  (draw-in-strip-frame chart computed canvas box scales env))

(defn- tufte-key!
  "Draws the key glyph of the Tufte variant: a point between two short lines, centred in a legend key.

  Parameters:

  - `canvas`: the clojure2d canvas.
  - `st` (map): the key's style - `:color`, `:stroke-color`, `:stroke-width`, `:shape`, `:size` (at most half the key height).
  - `box` (map): the key box `{:x :y :w :h}` in pixels."
  [canvas st {:keys [^double x ^double y ^double w ^double h]}]
  (let [cx (m/+ x (m/* 0.5 w))
        cy (m/+ y (m/* 0.5 h))
        reach (m/* 0.4 w)
        gap (m/* 0.15 w)]
    (c2d/set-stroke-custom canvas {:size (:stroke-width st) :cap :butt})
    (c2d/set-color canvas (:color st))
    (c2d/line canvas (m/- cx reach) cy (m/- cx gap) cy)
    (c2d/line canvas (m/+ cx gap) cy (m/+ cx reach) cy)
    (shape/draw! canvas (:shape st) (v/vec2 cx cy)
                 (assoc st :size (m/min (double (:size st)) (m/* 0.5 h))
                        :stroke-color (draw/outline-color (:stroke-color st) (:color st))))))

(defn- classic-key!
  "Draws the key glyph of the classic box: a small box with a median line, centred in a legend key.

  Parameters:

  - `canvas`: the clojure2d canvas.
  - `st` (map): the key's style - `:color`, `:stroke-color`, `:stroke-width`, the pattern keys.
  - `box` (map): the key box `{:x :y :w :h}` in pixels."
  [canvas st {:keys [^double x ^double y ^double w ^double h]}]
  (let [bw (m/* 0.7 w)
        bh (m/* 0.5 h)
        bx (m/+ x (m/* 0.5 (m/- w bw)))
        by (m/+ y (m/* 0.5 (m/- h bh)))]
    (c2d/set-stroke-custom canvas {:size (:stroke-width st)})
    (draw/fill-patterned! canvas (:color st) (draw/pattern-fill st (:color st)) #(c2d/rect % bx by bw bh))
    (let [stroke (draw/outline-color (:stroke-color st) (:color st))]
      (draw/outline! canvas stroke (:stroke-width st) #(c2d/rect % bx by bw bh true)))
    (c2d/set-color canvas (draw/contrast-color (:color st)))
    (c2d/line canvas (m/+ bx (m/* 0.5 bw)) by (m/+ bx (m/* 0.5 bw)) (m/+ by bh))))

(defn legend
  "Draws the box chart's key glyph: a small box with a median line (the Tufte variant: a point between two short lines), centred in a legend key.

  Parameters:

  - `chart` (map): the chart value - `:options` `:variant`.
  - `attributes` (map): the key's mapped style attributes - `:color` and `:pattern` are used.
  - `canvas`: the clojure2d canvas.
  - `box` (map): the key box in pixels.
  - `env` (map): the legend environment - `:theme`."
  [{:keys [options] :as chart} attributes canvas box env]
  (let [st (theme/key-style env chart attributes [:color :pattern])]
    (if (= :tufte (:variant options))
      (tufte-key! canvas st box)
      (classic-key! canvas st box))))
