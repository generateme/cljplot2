(ns cljplot2.chart.histogram
  "Histogram: counts of `:x` values in bins shared by all panels and groups (lattice-style common breaks).

  Options (style defaults from theme `[:chart :histogram]`):

  * `:bins` - a fastmath bin-estimation method keyword (`:sturges` default, `:sqrt`, `:rice`, `:doane`, `:scott`,
    `:freedman-diaconis`; computed on all data), a positive integer (equal-width bins over the data range), or a
    vector of at least 2 increasing breaks (values outside are dropped and counted)
  * `:y` - bar height: `:percent` (default, of the panel's total, or the group's when grouped), `:count`, `:pmf`
    (share of the total, sums to 1), `:density` (count / (n * width), area 1), `:cumulative` (the count of the bin and
    of every bin before it)
  * `:position` - grouped bars (see [[cljplot2.data.position]]): `:overlay` (default; full-width, fill alpha
    `:alpha`), `:dodge` (bins split among the groups), `:stack` (groups on top of each other), `:normalize` (stacked, each
    bin summing to 1; y title \"Proportion\"), `:mirror` (exactly two groups, the second drawn downward; y tick labels
    show magnitudes)
  * `:zero-line` - mirror only: draw the y = 0 line on top of the bars (default true; `:zero-line-color`,
    `:zero-line-width`)
  * `:symmetric` - mirror only: `true` ties both halves to one y range `[-M M]`, `M` the tallest bar of either group
    over all panels, so the halves compare visually (default false)
  * `:color` (fill), `:stroke-color` (bar outline: a color, `:none` or `:darker` - the darkened fill, default),
    `:stroke-width`, `:alpha` (overlay fill opacity, 0..1)
  * `:corner-radius` - pixels, every bar corner rounded; `:corner-radius-end` - only the corners at the bar's top
    (value end), wins over `:corner-radius`; clamped to half the shorter side; the legend key rounded alike
    (default 0)
  * `:padding {:x [lo hi] :y [lo hi]}` - default `{:x [0.05 0.05] :y [0 0.05]}` (mirror: y `[0.05 0.05]`); bars
    stand on the panel's bottom edge, x padding keeps end ticks off facet panel joints

  Groups (see [[cljplot2.data.group]]) come from ordinal aesthetics (`:color` fills the bars) and `:group`.
  A single distinct value gets one bin of width 1 centred on it.

  Ridgelines - `:y` mapped to categorical levels or on a `:bands` scale (see [[cljplot2.data.strip/ridges?]]; a
  numeric `:y` otherwise is ignored): one histogram per level and group (the common breaks, `:y` measure per item),
  bars growing from the band's centre toward the next level, all on one height scale - the tallest bar is `:overlap`
  (default 0.9) band steps high; the y scale leaves room for the outer ridges. Drawn from the farthest level on; `:position` `:overlay` (default; with groups translucent, `:alpha`) or
  `:mirror` (two groups, the second growing away from the next level).

  Functions: [[compute]], [[draw]], [[legend]] - the chart contract; [[schemas]] - its style, options and mapping."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.stats :as stats]
            [cljplot2.data.rows :as data]
            [cljplot2.data.group :as group]
            [cljplot2.data.levels :as levels]
            [cljplot2.data.measure :as measure]
            [cljplot2.data.position :as position]
            [cljplot2.data.strip :as strip]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.java Array]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries
  "Map entries of the histogram style (theme `[:chart :histogram]`), also its options."
  (into [(schema/optional :color {:description "bar fill colour (a mapped :color wins)"} schema/solid-color)
         (schema/optional :stroke-color {:description "outline colour: a colour, :none or :darker (the darkened fill)"} draw/stroke-color)
         (schema/optional :stroke-width {:description "outline width in pixels"} schema/non-neg)
         (schema/optional :alpha {:description "fill opacity of grouped bars with :position :overlay (other bars are opaque)"}
                          schema/fraction)
         (schema/optional :zero-line-color {:description "colour of the y = 0 line drawn with :position :mirror"} schema/color)
         (schema/optional :zero-line-width {:description "width in pixels of the y = 0 line drawn with :position :mirror"} schema/non-neg)]
        draw/pattern-style-entries))

(def schemas
  "Schemas of the histogram: `:style` (theme `[:chart :histogram]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into theme/theme-style-entries)
                (conj (schema/optional :bins {:description (str "bins shared by all panels and groups: a fastmath estimation method (:sturges :sqrt :rice :doane :scott "
                                                                  ":freedman-diaconis), a bin count or increasing breaks (values outside dropped)")
                                                :default :sturges}
                                        [:and levels/bins [:fn {:error/message "quantile bins aren't supported by histograms (give a breaks vector)"} (complement map?)]])
                      (schema/optional :y {:description "bar height: :percent (of the panel's or group's total), :count, :pmf (shares summing to 1), :density (area 1) or :cumulative (count of the bin and every bin before it)"
                                           :default :percent}
                                       (into [:enum :cumulative] (keys measure/titles)))
                      (schema/optional :position {:description (str "grouped bars: :overlay (translucent), :dodge, :stack, :normalize (stacked to 1) or :mirror (two groups, "
                                                                      "the second downward); ridgelines: :overlay or :mirror")
                                                  :default :overlay}
                                       (into [:enum] (sort position/positions)))
                      (schema/nilable :symmetric {:description "with :position :mirror, tie both halves to one y range [-M M], M the tallest bar over all panels"
                                                  :default false}
                                      schema/bool)
                      (schema/nilable :zero-line {:description "with :position :mirror, draw the y = 0 line over the bars"
                                                  :default true}
                                      schema/bool)
                      (schema/optional :padding {:description "padding of the data span per position axis, {:x [lo hi] :y [lo hi]} fractions of the span"
                                                 :cljplot2/default-doc "{:x [0.05 0.05] :y [0 0.05]} (bars stand on the panel's bottom edge); y [0.05 0.05] with :position :mirror"}
                                       data/position-padding)
                      draw/corner-radius-option
                      draw/corner-radius-end-option)
                (into strip/ridge-options))
   :mapping [:map
             [:x {:description "values to bin (data units)"} schema/accessor]
             (schema/nilable :y {:description "ridgeline levels: categorical or on a :bands scale, one histogram per level (a numeric :y otherwise is ignored)"}
                             schema/accessor)
             (schema/nilable :color {:description "groups, each with its own bar fill colour"} schema/accessor)
             (schema/nilable :pattern {:description "groups, each with its own fill pattern"} schema/accessor)
             (schema/nilable :group {:description "groups without a visual aesthetic"} schema/accessor)]})

(defn- bin-counts
  "Counts values per bin.

  Parameters:

  - `brks` (sequence of doubles): the increasing breaks; the last bin is closed.
  - `values` (sequence of numbers): the values.

  Returns `{:counts [..] :outside n}`: a count per bin and the number of values outside the breaks."
  [brks values]
  (let [arr (double-array brks)
        counts (long-array (m/long-dec (alength arr)))]
    (doseq [v values
            :let [bin (levels/bin-index arr (double v))]
            :when (not (m/neg? bin))]
      (Array/inc counts bin))
    (let [per-bin (vec counts)]
      {:counts per-bin :outside (m/long-sub (count values) (long (reduce m/long-add 0 per-bin)))})))

(defn- heights
  "Gives the bar heights of bin counts for a count measure.

  Parameters:

  - `y` (keyword): the measure - `:cumulative` (the count of the bin and every bin before it, ggplot `after_stat(cumsum(count))`) or a measure of [[cljplot2.data.measure/heights]].
  - `brks` (sequence of doubles): the breaks of the bins.
  - `counts` (sequence of longs): a count per bin, in order.

  Returns a vector of heights, one per bin."
  [y brks counts]
  (if (= :cumulative y)
    (vec (rest (reductions m/+ 0.0 counts)))
    (measure/heights y counts (map (fn [[^double a ^double b]] (m/- b a)) (partition 2 1 brks)))))

(defn- measure-title
  "Gives the value axis title of a measure.

  Parameters:

  - `y` (keyword): the measure (`:cumulative` - `Cumulative Count`, else [[cljplot2.data.measure/titles]])."
  [y]
  (if (= :cumulative y) "Cumulative Count" (measure/titles y)))

(defn- outside-count
  "Counts the values outside the breaks.

  Parameters:

  - `brks` (vector of doubles or `nil`): the breaks; `nil` - none, nothing outside.
  - `values` (sequence of numbers): the values.

  Returns the number of values below the first break or above the last."
  [brks values]
  (if brks
    (let [lo (double (first brks))
          hi (double (peek brks))]
      (count (remove #(m/<= lo (double %) hi) values)))
    0))

(defn- ridge-compute
  "Computes the histogram stats of ridgelines (a categorical or bands `:y`).

  Parameters:

  - `chart` (map): the chart value - `:options` (`:y` default `:percent`, `:bins` default `:sturges`, `:overlap` default 0.9, `:position`, style options).
  - `chart-split` (map): the chart's rows split per panel.
  - `env` (map): the compute environment - `:theme`, `:plot-scales`, `:flip?` ...

  Returns the chart stats with `:ridges? true`: per panel `{:ridges [item ...] :top h :overlap o :style st :translucent? b}`, items (see [[cljplot2.data.strip/ridge-split]]) with `:bars [[x0 x1 height] ...]` on the common breaks; one `:top` over all panels; values outside the breaks counted in `:dropped` `:x`."
  [{:keys [options] :as chart} chart-split {:keys [theme] :as env}]
  (let [y (get options :y :percent)
        {:keys [position items panels dropped group-keys]} (strip/ridge-split chart-split options env)
        all-values (into [] (comp (mapcat val) (mapcat :values)) items)
        brks (levels/breaks all-values (get options :bins :sturges))
        binned (if brks
                 (update-vals items (fn [its] (mapv (fn [it] (assoc it :bars (mapv (fn [[a b] h] [a b h])
                                                                                   (partition 2 1 brks)
                                                                                   (heights y brks (:counts (bin-counts brks (:values it)))))))
                                                    its)))
                 {})
        outside (outside-count brks all-values)
        top (double (reduce m/max 0.0 (map #(nth % 2) (mapcat :bars (mapcat val binned)))))
        overlap (double (get options :overlap strip/default-ridge-overlap))
        bar-style (theme/resolved-style theme chart)]
    {:data (update-vals binned (fn [its] {:ridges its :top top :overlap overlap :style bar-style
                                          :translucent? (and (m/> (count group-keys) 1) (= :overlay position))}))
     :ridges? true
     :extents (merge-with merge (group/aesthetic-extents panels (:plot-scales env))
                          (strip/extents :band binned (fn [_] [(first brks) (peek brks)])))
     :padding (merge {:x data/axis-padding} (:padding options))
     :padding-explicit (data/explicit-padding {} options)
     :scale-defaults (strip/ridge-scale-defaults overlap (= :mirror position) (:flip? env) (get-in env [:plot-scales :y]))
     :na (group/na-dims panels)
     :dropped (update dropped :x #(m/long-add (long (or % 0)) outside))}))

(defn compute
  "Computes the histogram's stats.

  Parameters:

  - `chart` (map): the chart value - `:options` (`:bins` default `:sturges`, `:y` default `:percent`, `:position` default `:overlay`, `:symmetric`, `:zero-line`, `:padding`, style options).
  - `chart-split` (map): `:groups` (`{panel rows}`) and the merged `:mapping`.
  - `env` (map): the compute environment - `:theme`, `:plot-scales` ...

  Returns ridgeline stats for a categorical or bands `:y` ([[ridge-compute]]), else per panel `{:bars [{:x0 :x1 :y0 :y1 :aesthetics}] :style st :translucent? b :zero-line? b}` in data units (bars in group order, `:y0` / `:y1` placed by [[cljplot2.data.position/stack-values]]; `:translucent?` - grouped overlay, fill drawn with `:alpha`); x extents - the outer breaks, y - over 0 and every bar's ends, or `[-M M]` for a `:symmetric` mirror; `:dropped` `:x` - invalid values and values outside the breaks; `:tick-format {:y :absolute}` for mirror; the y axis titled by the measure (`:normalize` - `Proportion`).

  Throws `ex-info {:key :mapping}` when rows exist but none has a finite x, `{:key :position}` for `:mirror` without exactly two groups."
  [{:keys [options] :as chart} {:keys [groups mapping] :as chart-split} {:keys [theme] :as env}]
  (if (strip/ridges? chart-split env)
    (ridge-compute chart chart-split env)
    (let [st (theme/resolved-style theme chart)
          y (get options :y :percent)
          position (get options :position :overlay)
          {:keys [panels dims dropped]} (group/split groups (dissoc mapping :y) env)
          x-of (data/row-accessor :x (:x mapping))
          xs (update-vals panels (fn [gs] (mapv #(data/finite-numbers (map x-of (:rows %))) gs)))
          all-values (into [] (comp (mapcat identity) (mapcat :values)) (vals xs))
          all-count (long (reduce m/long-add 0 (map count (mapcat #(map :rows %) (vals panels)))))
          _ (when (and (m/pos? all-count) (empty? all-values))
              (schema/fail! data/no-valid-values {:key :mapping :value :x :rows all-count}))
          brks (levels/breaks all-values (get options :bins :sturges))
          group-keys (group/ordered-keys (mapcat #(map :key %) (vals panels)) dims (:plot-scales env))
          n-groups (count group-keys)
          _ (when (and (= position :mirror) (m/not== 2 n-groups) (seq all-values))
              (schema/fail! "Histogram :position :mirror needs exactly two groups." {:key :position :groups n-groups}))
          outside (outside-count brks all-values)
          ;; per panel: heights of every group (all of them, absent ones 0, so slots and mirror halves stay global)
          panel-bars (fn [gs group-values]
                       (let [present (zipmap (map :key gs) (map vector gs group-values))
                             group-heights (mapv (fn [k] (if-let [[_ {:keys [values]}] (present k)]
                                                           (heights y brks (:counts (bin-counts brks values)))
                                                           (vec (repeat (m/long-dec (count brks)) 0.0))))
                                                 group-keys)
                             ys (position/stack-values position group-heights)]
                         (vec (for [[i k] (map-indexed vector group-keys)
                                    :let [[g] (present k)]
                                    :when g
                                    [[a b] [y0 y1]] (map vector (partition 2 1 brks) (ys i))]
                                (let [[x0 x1] (if (= position :dodge) (position/dodge-interval [a b] i n-groups) [a b])]
                                  {:x0 x0 :x1 x1 :y0 y0 :y1 y1 :aesthetics (:aesthetics g)})))))
          data (if brks
                 (into {} (map (fn [[panel gs]]
                                 [panel {:bars (panel-bars gs (xs panel)) :style st
                                         :translucent? (and (m/> n-groups 1) (= position :overlay))
                                         :zero-line? (and (= position :mirror) (data/option options :zero-line true))}]))
                       panels)
                 {})
          bar-ys (fn [bars] (mapcat (juxt :y0 :y1) bars))
          bar-extents (update-vals data (fn [{:keys [bars]}]
                                          (when (seq bars)
                                            {:x {:kind :numeric :min (first brks) :max (peek brks)}
                                             :y {:kind :numeric
                                                 :min (reduce m/min 0.0 (bar-ys bars))
                                                 :max (reduce m/max 0.0 (bar-ys bars))}})))
          [lo hi] (position/symmetric-extent (bar-ys (mapcat :bars (vals data))))
          panel-extents (if (and (= position :mirror) (:symmetric options))
                          (update-vals bar-extents #(some-> % (assoc :y {:kind :numeric :min lo :max hi})))
                          bar-extents)
          aesthetic-extents (group/aesthetic-extents panels (:plot-scales env))]
      {:data data
       :extents (merge-with merge aesthetic-extents (into {} (filter val) panel-extents))
       :padding (merge {:x data/axis-padding :y (if (= position :mirror) data/axis-padding [0.0 0.05])} (:padding options))
       :padding-explicit (data/explicit-padding (when-not (= position :mirror) {:y [0.0 nil]}) options)
       :na (group/na-dims panels)
       :dropped (assoc dropped :x (m/long-add (reduce m/long-add 0 (map :dropped (mapcat identity (vals xs)))) outside))
       :axis-titles {:y (if (= position :normalize) "Proportion" (measure-title y))}
       :tick-format (when (= position :mirror) {:y :absolute})})))

(defn- translucent-fill
  "Gives a bar's fill colour, with the style's `:alpha` when the bars are translucent.

  Parameters:

  - `color`: the bar colour.
  - `alpha`: the chart style's `:alpha`, a fraction.
  - `translucent?`: grouped overlay bars."
  [color alpha translucent?]
  (if translucent? (draw/with-alpha color alpha) color))

(defn- draw-ridges!
  "Draws the ridgeline histograms of one panel, farthest level first.

  Parameters:

  - `computed` (map): the panel's ridgeline stats - `:ridges`, `:top`, `:overlap`, `:style`, `:translucent?`.
  - `canvas`: the clojure2d canvas.
  - `box` (map): the panel box.
  - `scales` (map): finalized scales.
  - `env` (map): the draw environment - `:theme`.
  - `rounding`: the corner rounding (see [[cljplot2.graphics.draw/corner-rounding]]).

  Nothing is drawn when every bin is empty; empty bins, groups outside their scale's domain and bars the scales can't place are skipped."
  [{:keys [ridges ^double top ^double overlap style translucent?]} canvas box scales env rounding]
  (let [pl (strip/placement box scales {} :band)
        point (strip/ridge-point pl overlap)
        stroke-width (double (:stroke-width style))
        pattern-fill (draw/pattern-fill-cache)]
    (when (m/pos? top)
      (c2d/set-stroke-custom canvas {:size stroke-width :join :miter})
      (doseq [{:keys [level slot bars aesthetics]} (strip/ridge-order pl ridges)
              :let [st (group/style style aesthetics scales (:theme env))]
              :when st
              :let [side (strip/slot-side slot)
                    fill (translucent-fill (:color st) (:alpha style) translucent?)]
              [x0 x1 ^double h] bars
              :when (m/pos? h)
              :let [a (point x0 level side 0.0)
                    b (point x1 level side (m// h top))]
              :when (and (data/finite-point? a) (data/finite-point? b))]
        (draw/bar! canvas a b fill (draw/outline-color (:stroke-color st) fill) stroke-width (pattern-fill st fill)
                   (draw/bar-rounding rounding a (point x0 level side (m// h top))))))))

(defn draw
  "Draws the bars of one panel.

  Parameters:

  - `chart` (map): the chart value - `:options` (corner radii).
  - `computed` (map): the panel's stats - `:bars`, `:style`, `:translucent?`, `:zero-line?`, or ridgeline stats (`:ridges`).
  - `canvas`: the clojure2d canvas.
  - `box` (map): the panel box.
  - `scales` (map): finalized scales.
  - `env` (map): the draw environment - `:theme`.

  Grouped overlay bars get fill alpha `:alpha`; empty bins, bars under half a pixel and bars of groups outside an aesthetic domain are skipped; mirror with `:zero-line` (default on) - the y = 0 line on top. Ridgelines by [[draw-ridges!]], none without an x scale (see [[cljplot2.data.strip/placeable?]])."
  [{:keys [options]} {:keys [bars style translucent? ridges zero-line?] :as computed} canvas box scales env]
  (if ridges
    (when (strip/placeable? scales) (draw-ridges! computed canvas box scales env (draw/corner-rounding options)))
    (when (seq bars)
      (let [projection (scale/projection scales)
            stroke-width (double (:stroke-width style))
            pattern-fill (draw/pattern-fill-cache)
            rounding (draw/corner-rounding options)]
        (c2d/set-stroke-custom canvas {:size stroke-width :join :miter})
        (doseq [{:keys [x0 x1 y0 y1 aesthetics]} bars
                :when (m/not== (double y0) (double y1))
                :let [st (group/style style aesthetics scales (:theme env))]
                :when st]
          (let [{left :x top :y w :w h :h} (draw/rect-of projection x0 y0 x1 y1)
                fill (:color st)
                stroke (draw/outline-color (:stroke-color st) fill)
                drawn-fill (translucent-fill fill (:alpha style) translucent?)]
            (if rounding
              (let [[base end] (draw/value-ends y0 y1)
                    a (projection x0 base)]
                (draw/bar! canvas (draw/whole-pixel a) (draw/whole-pixel (projection x1 end)) drawn-fill stroke stroke-width
                           (pattern-fill st drawn-fill) (draw/bar-rounding rounding a (projection x0 end))))
              (when (draw/visible-bar? (double w) (double h))
                (draw/fill-patterned! canvas drawn-fill (pattern-fill st drawn-fill) #(c2d/rect % left top w h))
                (when (and (m/pos? stroke-width) (not (theme/none? stroke)))
                  (c2d/set-color canvas stroke)
                  (c2d/rect canvas left top w h true))))))
        (when zero-line?
          (draw/zero-line! canvas box scales style))))))

(defn legend
  "Draws the histogram's key glyph: a filled square, outlined by `:stroke-color`, inset in a legend key.

  Parameters:

  - `chart` (map): the chart value - `:options` (`:position`, corner radii).
  - `attributes` (map): the key's mapped style attributes - `:color` and `:pattern` are used.
  - `canvas`: the clojure2d canvas.
  - `box` (map): the key box in pixels.
  - `env` (map): the legend environment - `:theme`.

  With a mapped `:color` and the `:overlay` position (translucent grouped bars) the fill gets `:alpha` too; corners rounded as the bars'.

  See also [[cljplot2.graphics.draw/bar-key!]]."
  [{:keys [options] :as chart} attributes canvas box env]
  (draw/bar-key! canvas box (merge (theme/resolved-style (:theme env) chart) (select-keys attributes [:color :pattern]))
                 (and (contains? attributes :color) (= :overlay (get options :position :overlay)))
                 (draw/corner-rounding options)))
