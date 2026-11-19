(ns cljplot2.chart.bar
  "Bar chart: one bar per level and group - a strip chart (see [[cljplot2.data.strip]]): levels on the categorical
  axis - `{:x level :y value}` vertical bars, `{:x value :y level}` horizontal ones, `:level-axis` when both or neither
  axis is categorical; placement `:place`, `:width`, `:dodge-gap`, `:n-label`. Axes below are named in the strip frame
  - `:x` the values, `:y` the levels; exchange them for vertical bars (levels on `:x`), also in `:padding`:
  `{:mapping {:x :country :y :value}}` with `(chart/bar {:padding {:y [0 0.1]}})` leaves 10% above the tallest bar.

  Options (style defaults from theme `[:chart :bar]`):

  * `:stat` - the bar length from the item's rows: `:sum` (default; of the `:x` values - one row per level gives its
    value), `:mean`, `:median` (fastmath.stats), a function of the `:x` values returning a finite number, or a count
    measure of the rows (`:x` not needed, see [[cljplot2.data.measure]]): `:count`, `:percent`, `:pmf` (of the
    group's total in the panel)
  * `:position` - groups: `:dodge` (default when some level holds two or more groups; side by side in the band),
    `:overlay` (the default otherwise - one group per level fills its band; groups sharing a level translucent, fill
    `:alpha`), `:stack` (on top of each other, in group order; negative lengths stack from 0
    the other way), `:normalize` (stacked, each level summing to 1; non-negative lengths)
  * `:negative-order` - `:stack`: the groups below 0 in group order (`:forward`, default) or reversed (`:reverse` - the
    last group nearest 0): with groups ordered from one end of a scale to the other (strongly disagree ... strongly
    agree) and the disagreeing values negative, the bars diverge from the middle (Likert scales; split a neutral
    group's value half below, half above 0 with a `:group`)
  * `:mirror` - a grouping dimension (an ordinal aesthetic or `:group`) with exactly two levels (see
    [[cljplot2.data.strip]]): the second level's bars point the other way (back-to-back bars, e.g. population
    pyramids); `:position` applies on each side (a dodged pair shares its slot), `:normalize` sums to 1 on one side
    and -1 on the other; value tick labels show magnitudes; `:zero-line` (default true) draws the 0 line on top
    (`:zero-line-color`, `:zero-line-width`)
  * `:varwidth` - bar widths relative to the widest: `true` - by the square root of the count (as box plots), a
    keyword or function of a row - by the item's sum of its finite values (non-negative)
  * `:value-label` - `true` writes each bar's length, signed as its value (negative bars negative - `(fn [l]
    (Math/abs (:value l)))`-style functions drop the sign; a mirror's second group shows its positive value)
    (`:value-format` - a format string of the number, default: whole
    numbers as integers, otherwise the fewest digits telling the labels apart), or a function of `{:value length :n
    count :share s :level l :group {dim value}}` (`:share` - of the absolute lengths of its level's bars; `:group` -
    the bar's aesthetic values) returning the text (`nil` / `\"\"` - none), e.g. pie labels
    `#(format \"%s\\n%.0f%%\" (get-in % [:group :color]) (* 100 (:share %)))`; zero-length bars get none. With `:n-label`
    too, both texts form one label - `:label-stack` `:horizontal` (side by side) or `:vertical` (one above the
    other), default along the bars (horizontal bars side by side, vertical ones stacked)
  * `:label-position` - `:end` (past the bar's end; the default), `:center` (centred in the bar, in black or white by
    its fill; the default for `:stack` / `:normalize`) or `:outer` (polar plots only: past the outer radius at the
    bar's middle angle, anchored outward - pie labels; the circle shrinks to keep room for them; the angle axis is
    hidden by default, its labels would sit under them (`:scales {k {:axis true}}` brings it back); `:label-leader
    true` draws a line from the bar's outer edge, the labels further out). Labels are placed largest bar first and
    skipped when they overlap one placed before; a centred label is skipped unless its box lies inside its bar
  * `:color` (fill), `:stroke-color` (a color, `:none` or `:darker` - the darkened fill, default), `:stroke-width`,
    `:alpha`
  * `:corner-radius` - pixels, every bar corner rounded (each stacked segment too; polar - the annular sector's
    corners, see [[cljplot2.graphics.draw/rounded-sector-path]]); `:corner-radius-end` - only the corners at the value
    end, wins over `:corner-radius`; clamped to half the shorter side; the legend key rounded alike (default 0)
  * `:padding {:x [lo hi]}` - default 5%, none at the baseline side the bars grow from (bars start on the axis)

  Error bars: `chart/extent` with the same mapping (and the same `:mirror`) takes the same slots and side (e.g.
  `{:method :stddev}` over `:mean` bars). With a mapped `:color` the extent takes the bars' fills and disappears over
  them: map the variable to `:group` instead (`{:mapping {:color nil :group k} :color c}` - the bars' slots when
  `k` is their only grouping besides the `:mirror` dimension).

  Labels of stacked and normalized bars sit centred in their segments, in black or white by the fill.

  Axis title suggested for the value axis: the count measure (`\"Count\"` ...), `\"Proportion\"` for `:normalize`,
  `\"<stat> of <x>\"` for `:sum`, `:mean`, `:median` of a keyword `:x` when some bar has more than one value."
  (:require [clojure.string :as str]
            [clojure2d.color :as c]
            [fastmath.core :as m]
            [fastmath.stats :as stats]
            [cljplot2.chart.box :as box]
            [cljplot2.content.label :as label]
            [cljplot2.data.group :as group]
            [cljplot2.data.levels :as levels]
            [cljplot2.data.measure :as measure]
            [cljplot2.data.position :as position]
            [cljplot2.data.rows :as data]
            [cljplot2.data.strip :as strip]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.scale.coord :as coord]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private count-stats #{:count :percent :pmf})
(def ^:private value-stats {:sum stats/sum :mean stats/mean :median stats/median})
(def ^:private positions #{:overlay :dodge :stack :normalize})

(def ^:private style-entries
  (into [(schema/optional :color schema/solid-color)
         (schema/optional :stroke-color draw/stroke-color)
         (schema/optional :stroke-width schema/non-neg)
         (schema/optional :alpha schema/fraction)
         (schema/optional :zero-line-color schema/color)
         (schema/optional :zero-line-width schema/non-neg)]
        draw/pattern-style-entries))

(def schemas
  "Schemas of the bar chart: `:style` (theme `[:chart :bar]`), `:options`, `:mapping` (merged; `:x` checked at
  compute - count stats don't need it)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into (remove #(= :position (first %)) strip/options))
                (conj (schema/optional :stat [:or (into [:enum] (sort (into count-stats (keys value-stats)))) schema/function])
                      (schema/optional :position (into [:enum] (sort positions)))
                      strip/mirror-option
                      (schema/optional :label-stack [:enum :horizontal :vertical])
                      (schema/nilable :zero-line schema/bool)
                      (schema/nilable :varwidth [:or schema/bool schema/accessor])
                      (schema/nilable :value-label [:or schema/bool schema/function])
                      (schema/optional :label-position [:enum :end :center :outer])
                      (schema/optional :negative-order [:enum :forward :reverse])
                      (schema/nilable :label-leader schema/bool)
                      (schema/optional :value-format :string)
                      (schema/optional :padding data/position-padding)
                      draw/corner-radius-option
                      draw/corner-radius-end-option))
   :mapping [:map]})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :bar])
         (select-keys options [:color :stroke-color :stroke-width :alpha :zero-line-color :zero-line-width :pattern
                               :pattern-color])))

(defn- check-options!
  "Throw for a value stat without a value mapping (`:x` in the strip frame; `{:key :mapping}`) and a `:value-format`
  that can't format a number (`{:key :value-format}`)."
  [options mapping]
  (when (and (nil? (:x mapping)) (not (count-stats (get options :stat :sum))))
    (schema/fail! "Bar :stat needs the values mapped - on the axis other than the levels (only :count, :percent and :pmf count rows without them)."
                  {:key :mapping :value :values}))
  (when-let [fmt (:value-format options)]
    (try (format fmt 1.5)
         (catch Exception e
           (schema/fail! "Bar :value-format should format a number (e.g. \"%.1f\")."
                         {:key :value-format :value fmt :cause (ex-message e)})))))

(defn- item-lengths
  "Bar length `:length` of every item (`{panel [item ...]}`) by `stat`; count measures over each group's items of
  the panel. Throws `ex-info {:key :stat}` when a stat function returns no finite number."
  [stat items]
  (update-vals items
               (fn [its]
                 (if (count-stats stat)
                   (let [by-key (group-by :key its)
                         lengths (into {} (mapcat (fn [[_ g]] (map vector g (measure/heights stat (map :n g) (repeat 1.0))))) by-key)]
                     (mapv #(assoc % :length (lengths %)) its))
                   (let [f (get value-stats stat stat)]
                     (mapv (fn [{:keys [values] :as it}]
                             (let [v (f values)]
                               (when-not (and (number? v) (m/valid-double? (double v)))
                                 (schema/fail! "Bar :stat should return a finite number." {:key :stat :value v}))
                               (assoc it :length (double v))))
                           its))))))

(defn- side-ends
  "`[y0 y1]` of the items `its` (one side of one panel) placed by `position` (`stack-options` - see
  [[cljplot2.data.position/stack-values]]) over the panel's `levels` (a group without a level counts 0), keyed by
  `[group-key level]`."
  [position stack-options levels its]
  (let [index (zipmap levels (range))
        by-key (group-by :key its)
        group-keys (vec (distinct (map :key its)))
        lengths (mapv (fn [k] (reduce (fn [ls it] (assoc ls (index (:level it)) (:length it)))
                                      (vec (repeat (count levels) 0.0)) (by-key k)))
                      group-keys)
        ends (position/stack-values position lengths stack-options)]
    (into {} (for [[gi k] (map-indexed vector group-keys) it (by-key k)]
               [[k (:level it)] ((ends gi) (index (:level it)))]))))

(defn- placed-bars
  "Items of one panel with `:y0 :y1` (bar ends along `:x`): `position` on each side (`stack-options`, see
  [[side-ends]]), the `:mirrored?` items' ends negated (`[y0 y1]` -> `[-y1 -y0]`); item order kept."
  [position stack-options its]
  (let [levels (vec (distinct (map :level its)))
        ends (merge (side-ends position stack-options levels (remove :mirrored? its))
                    (update-vals (side-ends position stack-options levels (filter :mirrored? its))
                                 (fn [[^double y0 ^double y1]] [(- y1) (- y0)])))]
    (mapv (fn [it] (let [[y0 y1] (ends [(:key it) (:level it)])] (assoc it :y0 y0 :y1 y1))) its)))

(defn- weight-factors
  "Item -> width factor for `:varwidth`: `true` - [[cljplot2.chart.box/width-factors]], a keyword / function - the
  item's sum of finite weights of its `:rows` relative to the largest; `nil` / `false` - 1. Throws `ex-info {:key
  :varwidth}` for a negative sum."
  [varwidth items]
  (cond
    (true? varwidth) (box/width-factors items true)
    (or (keyword? varwidth) (fn? varwidth))
    (let [acc (data/row-accessor :varwidth varwidth)
          weight (fn [{:keys [rows]}] (double (reduce + 0.0 (:values (data/finite-numbers (map acc rows))))))
          weights (map weight items)
          _ (when-let [w (first (filter neg? weights))]
              (schema/fail! "Bar :varwidth weights should be non-negative." {:key :varwidth :value w}))
          wmax (double (reduce max 0.0 weights))]
      (fn ^double [item] (if (pos? wmax) (/ (double (weight item)) wmax) 0.0)))
    :else (constantly 1.0)))

(defn- end-of
  "The bar's far end from 0 along `:x` and the direction it points (`1.0` / `-1.0`; a zero bar points up)."
  [{:keys [^double y0 ^double y1]}]
  (if (neg? (+ y0 y1)) [(m/min y0 y1) -1.0] [(m/max y0 y1) 1.0]))

(defn- value-texts
  "Bar -> value label text: the bar's signed length - `|y1 - y0|` with the sign of its value `:length` (negative bars
  negative; stacked and normalized segments their own length; a mirror's second group its value, positive) - by
  `:value-format`, else [[cljplot2.data.levels/break-labels]] over all `bars` (whole numbers as integers, else the
  fewest digits telling them apart)."
  [options bars]
  (let [length (fn [{:keys [^double y0 ^double y1 length]}] (* (m/signum (double length)) (m/abs (- y1 y0))))]
    (if-let [fmt (:value-format options)]
      (fn [bar] (format fmt (length bar)))
      (let [ls (distinct (map length bars))]
        (if (seq ls) (let [texts (zipmap ls (levels/break-labels ls))] (comp texts length)) (constantly ""))))))

(defn- label-info
  "What a `:value-label` function gets for `bar`: `{:value length :n count :share s :level l :group {dim value}}` -
  `:share` of the absolute lengths of its level's bars in the panel (`level-total`)."
  [{:keys [length n level aesthetics]} level-total]
  (let [total (double (level-total level))]
    {:value length :n n :share (if (pos? total) (/ (m/abs (double length)) total) 0.0) :level level :group aesthetics}))

(defn- value-label-text
  "Value text of `bar` for `:value-label` `labels` (`true` - by `value-text`, a function of [[label-info]]); `nil`
  for none, a zero-length bar or an empty result."
  [labels value-text level-total {:keys [^double y0 ^double y1] :as bar}]
  (when (and labels (not (== y0 y1)))
    (let [t (if (fn? labels) (labels (label-info bar level-total)) (value-text bar))]
      (when (and (some? t) (not= "" t)) (str t)))))

(defn- end-label
  "Label (see [[cljplot2.data.strip/draw-end-labels!]]) of `bar`: its value text (`:value-label`, see
  [[value-label-text]]) and count (`:n-label`, `:n-format`), joined by `separator`; placed by `label-position` -
  `:end` past the bar's end, `:center` centred in it (`:side 0`, drawn in the fill's contrast colour), `:outer` past
  a polar plot's outer radius (`:outer?`, `:leader?` from `:label-leader`); every label carries its bar's `:span
  [y0 y1]`; `nil` without text."
  [options separator label-position value-text level-total {:keys [level slot n ^double y0 ^double y1] :as bar}]
  (let [value (value-label-text (data/option options :value-label false) value-text level-total bar)
        texts (cond-> []
                value (conj value)
                (data/option options :n-label false) (conj (format (get options :n-format "n=%d") n)))]
    (when (seq texts)
      (let [[at side] (if (= :end label-position) (end-of bar) [(* 0.5 (+ y0 y1)) 0.0])]
        (cond-> {:level level :slot slot :at at :side side :text (str/join separator texts) :span [y0 y1]}
          (= :outer label-position) (assoc :outer? true :leader? (data/option options :label-leader false)))))))

(defn- label-separator
  "Separator of the value and count texts of one label: `:label-stack` `:horizontal` - a space, `:vertical` - a line
  break; default along the bars (`flip?` - vertical bars)."
  [options flip?]
  (case (get options :label-stack (if flip? :vertical :horizontal))
    :horizontal " "
    :vertical "\n"))

(defn- value-title
  "Suggested value-axis title: count measure titles, `\"Proportion\"` for `:normalize`, `\"<stat> of <x>\"` for a
  value stat keyword over a keyword `:x` when some bar has more than one value (`bars`); else `nil`."
  [stat position x bars]
  (cond
    (= :normalize position) "Proportion"
    (count-stats stat) (measure/titles stat)
    (and (value-stats stat) (keyword? x) (some #(> (long (:n %)) 1) bars)) (str (name stat) " of " (name x))
    :else nil))

(defn- baseline
  "Padding `[lo hi]` and padding request (`nil` sides - none) of the value axis over every bar end `ys`: no padding
  at a side the bars grow from (all ends on one side of 0)."
  [ys]
  (let [lo (double (reduce min 0.0 ys)) hi (double (reduce max 0.0 ys))
        from-lo? (zero? lo) from-hi? (zero? hi)]
    {:padding [(if from-lo? 0.0 0.05) (if from-hi? 0.0 0.05)]
     :request (when (or from-lo? from-hi?) [(when from-lo? 0.0) (when from-hi? 0.0)])}))

(defn- compute-in-strip-frame
  "Per panel: `{:bars [item ...] :style st :place p :options o :translucent? b :zero-line? b}`, items (see
  [[cljplot2.data.strip/split]]) with `:length`, `:y0 :y1`, `:width-factor`, `:label` (the end label or `nil`); extents (x - the
  bar ends and 0, y - see [[cljplot2.data.strip/extents]]), padding, `:padding-px` (label room), `:na`, `:dropped`,
  `:axis-titles` (count measures, `\"Proportion\"`), `:tick-format` (mirror)."
  [{:keys [options]} {:keys [mapping] :as chart-split} {:keys [theme] :as env}]
  (check-options! options mapping)
  (let [stat (get options :stat :sum)
        position (get options :position)
        varwidth (:varwidth options)
        split-options (cond-> (dissoc options :position)
                        (some? position) (assoc :position (if (#{:stack :normalize} position) :overlay position))
                        (count-stats stat) (assoc ::strip/count-rows true)
                        (or (keyword? varwidth) (fn? varwidth)) (assoc ::strip/rows true)
                        (:mirror options) (assoc ::strip/mirror (:mirror options)))
        {:keys [place items panels dropped shared-levels? mirror?] :as split} (strip/split chart-split split-options :center env)
        position (or position (:position split))
        items (item-lengths stat items)
        items (update-vals items #(placed-bars position (select-keys options [:negative-order]) %))
        factor (weight-factors varwidth (mapcat val items))
        items (update-vals items (fn [its] (mapv #(-> % (assoc :width-factor (factor %)) (dissoc :rows)) its)))
        all-bars (mapcat val items)
        value-text (value-texts options (remove (fn [{:keys [^double y0 ^double y1]}] (== y0 y1)) all-bars))
        separator (label-separator options (:flip? env))
        label-position (get options :label-position (if (contains? #{:stack :normalize} position) :center :end))
        _ (when (and (= :outer label-position) (not (coord/polar? (:coord env))))
            (schema/fail! "Bar :label-position :outer places labels outside a polar plot's circle: use :end or :center in cartesian plots."
                          {:key :label-position :value :outer}))
        items (update-vals items
                           (fn [its]
                             (let [level-total (reduce (fn [acc {:keys [level length]}] (update acc level (fnil + 0.0) (m/abs (double length))))
                                                       {} its)]
                               (mapv #(assoc % :label (end-label options separator label-position value-text level-total %)) its))))
        labels (keep :label (mapcat val items))
        {base-padding :padding request :request} (baseline (mapcat (juxt :y0 :y1) all-bars))
        translucent? (and (= :overlay position) shared-levels?)]
    {:data (update-vals items (fn [its] {:bars its :style (style options theme) :place place :options options
                                         :translucent? translucent?
                                         :zero-line? (and mirror? (data/option options :zero-line true))}))
     :extents (merge-with merge (group/aesthetic-extents panels (:plot-scales env))
                          (strip/extents place items (fn [{:keys [^double y0 ^double y1]}]
                                                       [(m/min 0.0 y0 y1) (m/max 0.0 y0 y1)])))
     :padding (merge (assoc (strip/padding place (dissoc options :padding)) :x base-padding) (:padding options))
     :padding-explicit (data/explicit-padding (cond-> (or (strip/explicit-padding place {}) {})
                                                request (assoc :x request))
                                              options)
     :padding-px (strip/end-label-room labels env)
     :outer-room (strip/outer-label-room labels env)
     ;; outer labels replace the angle axis (it would sit under them)
     :scale-defaults (when (= :outer label-position) {(if (:flip? env) :y :x) {:axis false}})
     :na (group/na-dims panels)
     :dropped dropped
     :axis-titles (if-let [t (value-title stat position (:x mapping) all-bars)] {:x t} {})
     :tick-format (when mirror? {:x :absolute})}))

(defn- in-sector?
  "Is pixel `[x y]` inside the polar sector of angles `h0..h1` (range positions, radians) and radii `v0..v1` of
  `geometry`?"
  [{:keys [cx cy]} h0 h1 v0 v1 [x y]]
  (let [h0 (double h0) h1 (double h1) v0 (double v0) v1 (double v1)
        dx (- (double x) (double cx)) dy (- (double y) (double cy))
        r (m/hypot dx dy)
        ;; the point's angle (0 up, clockwise) brought to [h0, h0 + 2 pi)
        a (+ h0 (double (mod (- (m/atan2 dx (- dy)) h0) m/TWO_PI)))]
    (and (<= v0 r v1) (<= a h1))))

(defn- fits-segment?
  "Does a centred label with pixel `box` fit its bar's segment (`bar` - `:level :slot :y0 :y1 :width-factor`)? The
  box inside the segment: cartesian - its rectangle; polar - every box corner inside its sector."
  [{:keys [range-point geometry]} {:keys [level slot ^double y0 ^double y1 width-factor]} {:keys [x y w h]}]
  (let [c0 (strip/in-slot slot 0.0)
        hw (* (strip/slot-half slot) (double width-factor))
        corners (for [v [y0 y1] cross [(- c0 hw) (+ c0 hw)]] (range-point v level cross))
        hs (map #(double (first %)) corners) vs (map #(double (second %)) corners)
        h0 (double (reduce min hs)) h1 (double (reduce max hs))
        v0 (double (reduce min vs)) v1 (double (reduce max vs))
        x (double x) y (double y) w (double w) h (double h)]
    (if geometry
      (every? #(in-sector? geometry h0 h1 v0 v1 %) [[x y] [(+ x w) y] [x (+ y h)] [(+ x w) (+ y h)]])
      (and (<= h0 x) (<= (+ x w) h1) (<= v0 y) (<= (+ y h) v1)))))

(defn- fitting-placements
  "End-label `placements` without the centred labels that don't fit their segment (each label carries its `:bar`)."
  [canvas pl placements env]
  (filter (fn [{:keys [label content anchor x y]}]
            (or (not (zero? (double (:side label)))) (:outer? label)
                (fits-segment? pl (:bar label) (label/box-at canvas content anchor x y env))))
          placements))

(defn- draw-in-strip-frame
  "Draw the bars of one panel (zero-length bars, bars under half a pixel and bars of groups outside an aesthetic
  domain skipped), then the mirror's 0 line and the end labels (of the bars whose group is drawn)."
  [_chart {:keys [bars style place options translucent? zero-line?]} canvas box scales env]
  (let [{:keys [range-point geometry] :as pl} (strip/placement box scales options place)
        stroke-width (double (:stroke-width style))
        pattern-fill (draw/pattern-fill-cache)
        rounding (draw/corner-rounding options)
        styled (keep (fn [bar] (some->> (group/style style (:aesthetics bar) scales (:theme env)) (vector bar))) bars)]
    (doseq [[{:keys [level slot ^double y0 ^double y1 width-factor]} st] styled
            :when (not (== y0 y1))
            :let [c0 (strip/in-slot slot 0.0)
                  hw (* (strip/slot-half slot) (double width-factor))
                  fill (if translucent? (c/set-alpha (:color st) (* 255.0 (double (:alpha style)))) (:color st))]]
      (let [[base end] (if rounding (draw/value-ends y0 y1) [y0 y1])]
        (draw/range-bar! canvas geometry (range-point base level (- c0 hw)) (range-point end level (+ c0 hw))
                         fill (draw/outline-color (:stroke-color st) (:color st)) stroke-width (pattern-fill st fill)
                         (draw/bar-rounding rounding (range-point base level c0) (range-point end level c0)))))
    (when zero-line?
      (draw/zero-line! canvas box scales style :x))
    (let [labels (keep (fn [[{:keys [label] :as bar} st]]
                         (when label
                           (cond-> (assoc label :bar bar)
                             (and (zero? (double (:side label))) (not (:outer? label)))
                             (assoc :color (draw/contrast-color (:color st))))))
                       styled)
          ;; largest bars first: their labels win collisions
          ordered (sort-by (fn [{{:keys [^double y0 ^double y1]} :bar}] (- (m/abs (- y1 y0)))) labels)
          placements (fitting-placements canvas pl (strip/end-label-placements pl ordered scales) env)]
      (strip/draw-label-placements! canvas (strip/without-collisions canvas placements env) env))))


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
  "Key glyph: a filled square (see [[cljplot2.graphics.draw/bar-key!]]); translucent with a mapped `:color` when the
  bars are (overlay groups sharing a level, see [[compute]] - env `:chart-stats`)."
  [{:keys [options]} attributes canvas box env]
  (draw/bar-key! canvas box (merge (style options (:theme env)) (select-keys attributes [:color :pattern]))
                 (boolean (and (contains? attributes :color) (some :translucent? (vals (get-in env [:chart-stats :data])))))
                 (draw/corner-rounding options)))
