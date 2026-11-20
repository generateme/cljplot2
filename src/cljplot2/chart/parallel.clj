(ns cljplot2.chart.parallel
  "Parallel coordinates: every row of wide data drawn as a line across one axis per variable; under a polar plot
  `:coord` the same chart is a radar (spider) chart (see Polar below).

  `:variables` (required) - at least two keywords or `[label accessor]` pairs, in axis order; labels (a keyword's
  name, or the pair's label as a string) must differ. They are the levels of the chart's `:x` (categorical, in the
  given order: the plot's x axis names the variables); the x mapping isn't used. Values must be numbers: a missing,
  non-numeric or non-finite value breaks the row's line there (`:missing :break`, default) or drops the row
  (`:missing :drop`); such rows are counted in `:dropped {:variables n}`. Axes are computed from every row, also
  the dropped ones (filter the data to leave them out).

  Per variable, over the rows of all panels (facets share the axes): the domain `[lo hi]` - its finite data range,
  or `(get-in options [:limits key])` (`key` - the keyword, or the pair's label) - and the affine scaling
  `(v - offset) / scale` of `:scaling`:

  * `:uniminmax` (default) - `(v - lo) / (hi - lo)`, every axis spans 0..1
  * `:std` - `(v - mean) / stddev`; `:center` - `v - mean`
  * `:robust` - `(v - centre) / spread`: `:robust-center` `:median` (default) or `:mean`, `:robust-spread` - a
    `fastmath.stats/robust-stddev` estimator of the standard deviation: `:mad` (default; R `mad`), `:iqr`, `:sn`,
    `:qn` (both O(n^2)), `:tukey`, `:biweight`, `:stddev`, `:meanad`, `:gini`, or a quantile `p` in (0 0.5)
  * `:globalminmax` - the values as they are (one common scale)

  A zero spread puts the variable's values at 0.5 (`:uniminmax`) or 0. The chart's `:y` is the scaled value; its
  extent covers every scaled value and axis end. The plot's y axis and grid are hidden (`:scale-defaults`, the
  plot's `:scales :y :axis` wins); with `:axes` (default true) the chart draws each variable's axis - a line from its
  scaled `lo` to `hi`, ticks of a linear scale over the domain (original units; as many as theme
  `[:axis :y :tick-spacing]` allows along the axis's pixel length) with labels (text role `:tick-label`, styled by
  theme `[:axis :y]`): right of the axis, below it in a flipped plot, beside the spoke in a polar one (outside the
  ring in a flipped polar one).

  `:curve :smooth` joins consecutive axes with smooth curves instead of straight segments: a cubic bezier in range
  space that meets every axis perpendicularly, at its value, and never leaves the range between the two values;
  `:smoothness` (0 1], default 1 - the bend spread over the whole gap, smaller values keep it near the axes. In polar
  plots the curves are sampled in range space, so they follow the circle whether or not `:coord :arcs` is set.

  Polar (radar): variables on angles, values on the radius, chords or arcs (`:coord :arcs`); on a full circle each
  row's line closes (last variable -> first), on a partial one (`:span` < 360) it doesn't. With `:flip` the
  variables are rings and the values angles: lines don't close, and the angle is padded by `:padding :y` (suggested
  as its `:expand`, so the ends of an axis don't meet; the plot's `:scales :y` wins). `:fill`
  (`:color` - the line's colour at `:alpha`, or a colour) fills every complete row's polygon, with `:pattern`; polar
  only (a theme fill is ignored in cartesian plots). While the plot's y axis is hidden, a web (theme
  `[:panel :grid :y]`) joins the axes at equal fractions of their length, as the rows do: closed polygons on a full
  circle, else open lines (across the rings with `:flip`).

  Options (style defaults from theme `[:chart :parallel]`): `:color`, `:width`, `:dash`, `:fill`, `:alpha`,
  `:pattern`, `:pattern-color`, `:variables`, `:scaling`, `:robust-center`, `:robust-spread`, `:limits {key [lo hi]}`,
  `:axes`, `:missing`, `:curve`, `:smoothness`, `:padding {:y [lo hi]}` (fractions of the scaled extent, default 0.05
  each side; the variables' `:x` is a bands scale).
  Groups (see [[cljplot2.data.group]]) come from ordinal aesthetics (`:color`, `:dash`) and `:group`; a line per
  row, in its group's style."
  (:require [fastmath.core :as m]
            [fastmath.stats :as stats]
            [wadogo.scale :as s]
            [cljplot2.content.label :as label]
            [cljplot2.data.group :as group]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.curve :as curve]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.scale.coord :as coord]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; variables

(defn- labelled-variable? [v] (and (vector? v) (= 2 (count v)) (string? (first v)) (or (keyword? (second v)) (fn? (second v)))))

(def ^:private variable
  (schema/named-type ::variable #(or (keyword? %) (labelled-variable? %))
                     "should be a keyword or a [label accessor] pair (a string and a keyword or function)"))

(defn variable-key
  "Key of a `:variables` entry in `:limits`: the keyword, or the pair's label."
  [v]
  (if (keyword? v) v (first v)))

(defn variable-label
  "Axis label of a `:variables` entry: the keyword's name, or the pair's label."
  [v]
  (if (keyword? v) (name v) (first v)))

(defn- duplicate-labels
  [vs]
  (let [labels (map variable-label vs)]
    (when (not= (count labels) (count (distinct labels)))
      (str "variable labels must differ, repeated: " (pr-str (vec (for [[l n] (frequencies labels) :when (> (long n) 1)] l)))))))

(def ^:private variables
  (schema/with-rules [:vector variable]
    [#(when (< (count %) 2) "should have at least 2 variables") nil]
    [duplicate-labels nil]))

(def scalings
  "Values of `:scaling`."
  #{:uniminmax :std :robust :center :globalminmax})

(def robust-spreads
  "Named `:robust-spread` estimators (`fastmath.stats/robust-stddev` methods)."
  #{:mad :iqr :sn :qn :tukey :biweight :stddev :meanad :gini})

(def ^:private robust-spread
  (schema/named-type ::robust-spread #(or (contains? robust-spreads %) (and (schema/finite? %) (< 0.0 (double %) 0.5)))
                     (str "should be one of " (pr-str (vec (sort robust-spreads))) " or a quantile p, 0 < p < 0.5")))

(def ^:private style-entries
  (into (conj polyline/style-entries
              (schema/optional :fill {:description "radar polygon fill (polar only): :none, :color (the line's colour at :alpha) or a colour"}
                               [:or [:= :color] schema/color])
              (schema/optional :alpha {:description "fill opacity"} schema/fraction))
        draw/pattern-style-entries))

(defn- unknown-limits
  [{vs :variables lims :limits}]
  (when (and (vector? vs) (map? lims))
    (let [ks (set (map variable-key vs))
          unknown (remove ks (keys lims))]
      (when (seq unknown)
        (str ":limits keys should be variables (keywords, or pair labels), unknown: " (pr-str (vec unknown)))))))

(defn- needs
  "Rule: option `k` is used only when `(pred options)`; `message` otherwise."
  [k pred message]
  [(fn [options] (when (and (contains? options k) (not (pred options))) message)) [k]])

(def schemas
  "Schemas of the parallel chart: `:style` (theme `[:chart :parallel]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (schema/with-rules
              (-> [:map {:closed true}]
                  (into style-entries)
                  (into data/common-options)
                  (conj [:variables {:description "the variables in axis order: keywords or [label accessor] pairs (2 or more)"}
                         variables]
                        (schema/optional :scaling {:description "per variable scaling: :uniminmax (0..1), :std, :robust, :center or :globalminmax"
                                                   :default :uniminmax}
                                         (into [:enum] (sort scalings)))
                        (schema/optional :limits {:description "explicit variable domains {key [lo hi]}, key - the keyword or the pair's label"
                                                  :cljplot2/default-doc "each variable's finite data range"}
                                         [:map-of any? schema/finite-interval])
                        (schema/nilable :axes {:description "draw each variable's axis with ticks and labels" :default true} schema/bool)
                        (schema/optional :missing {:description "a missing value: :break (the line breaks there) or :drop (the row is dropped)"
                                                   :default :break}
                                         [:enum :break :drop])
                        (schema/optional :robust-center {:description "centre of :scaling :robust: :median or :mean"
                                                         :cljplot2/default-doc ":median (only with :scaling :robust)"}
                                         [:enum :median :mean])
                        (schema/optional :robust-spread {:description (str "spread of :scaling :robust: a fastmath.stats/robust-stddev "
                                                                           "estimator (:mad, :iqr, :sn, :qn, ...) or a quantile p in (0 0.5)")
                                                         :cljplot2/default-doc ":mad (only with :scaling :robust)"}
                                         robust-spread)
                        (schema/optional :curve {:description "join consecutive axes with smooth curves (:smooth)"
                                                 :cljplot2/default-doc "straight segments"}
                                         [:enum :smooth])
                        (schema/optional :smoothness {:description "how far the smooth bend spreads over the gap between axes, (0 1]"
                                                      :cljplot2/default-doc "1 (only with :curve :smooth)"}
                                         [:and schema/finite [:> 0] [:<= 1]])
                        (schema/optional :padding {:description "padding of the scaled values' span, {:y [lo hi]} fractions of the span"}
                                         [:map {:closed true}
                                          (schema/optional :y {:description "padding of the value axis [lo hi], fractions of the span"
                                                               :default [0.05 0.05]}
                                                           schema/padding)])))
              [unknown-limits [:limits]]
              (needs :robust-center #(= :robust (:scaling %)) ":robust-center needs :scaling :robust")
              (needs :robust-spread #(= :robust (:scaling %)) ":robust-spread needs :scaling :robust")
              (needs :smoothness #(= :smooth (:curve %)) ":smoothness needs :curve :smooth"))
   :mapping [:maybe [:map
                     (schema/nilable :color {:description "line colour, grouping the rows"} schema/accessor)
                     (schema/nilable :dash {:description "line dash style, grouping the rows"} schema/accessor)
                     (schema/nilable :pattern {:description "radar fill pattern, grouping the rows"} schema/accessor)
                     (schema/nilable :group {:description "groups without a visual aesthetic"} schema/accessor)]]})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :parallel]) (select-keys options [:color :width :dash :fill :alpha :pattern :pattern-color])))

;; scaling

(defn- numeric-value
  "`v` as a double when it is a finite number, else `nil` (missing)."
  [v]
  (when (and (number? v) (m/valid-double? (double v))) (double v)))

(defn- affine
  "`[offset scale]` of the `:scaling` of `options` for a variable with finite `values` and domain `[lo hi]`; a zero
  (or undefined) spread gives the values 0.5 (`:uniminmax`) or 0."
  [{:keys [scaling robust-center robust-spread] :or {scaling :uniminmax robust-center :median robust-spread :mad}}
   values [^double lo ^double hi]]
  (let [[o s] (case scaling
                :uniminmax [lo (- hi lo)]
                :std [(stats/mean values) (stats/stddev values)]
                :robust [(if (= :mean robust-center) (stats/mean values) (stats/median values))
                         (stats/robust-stddev values robust-spread)]
                :center [(stats/mean values) 1.0]
                :globalminmax [0.0 1.0])
        o (double o) s (double s)]
    (cond
      (and (m/valid-double? s) (pos? s)) [o s]
      (= :uniminmax scaling) [(- lo 0.5) 1.0]
      :else [o 1.0])))

(defn- scaled
  "Scaled value of `v` on `axis` (`{:offset :scale}`)."
  ^double [{:keys [^double offset ^double scale]} ^double v]
  (/ (- v offset) scale))

(defn variable-axes
  "Axis of every variable of `options` over `rows` (all panels): `{:label :key :accessor :domain [lo hi] :offset
  :scale}` (see the namespace doc). Throws `ex-info {:key :variables}` for a variable without a numeric value in
  non-empty `rows` and no `:limits`."
  [options rows]
  (mapv (fn [v]
            (let [k (variable-key v)
                  acc (data/row-accessor :variables (if (keyword? v) v (second v)))
                  values (into [] (keep (comp numeric-value acc)) rows)
                  domain (or (some->> (get-in options [:limits k]) (mapv double))
                             (when (seq values) [(reduce min values) (reduce max values)]))]
              (when (and (seq rows) (nil? domain))
                (schema/fail! (str "Variable " (pr-str k) " has no numeric values.") {:key :variables :value k}))
              (let [domain (or domain [0.0 1.0])
                    [o s] (affine options (if (seq values) values domain) domain)]
                {:label (variable-label v) :key k :accessor acc :domain domain :offset o :scale s})))
          (:variables options)))

(defn- row-values
  "Scaled value of every axis of `axes` for `row` (`nil` - missing)."
  [axes row]
  (mapv (fn [axis] (some->> ((:accessor axis) row) numeric-value (scaled axis))) axes))

(defn- y-extent
  "Extent of the scaled `values` (all rows) and every axis end."
  [axes values]
  (data/extent :y (into (vec (remove nil? values))
                        (mapcat (fn [{[lo hi] :domain :as axis}] [(scaled axis lo) (scaled axis hi)]))
                        axes)))

;; compute

(defn- check-fill!
  "Throw `ex-info {:key :fill}` for a `:fill` option in a cartesian plot (`coord` - the plot's `:coord`)."
  [options coord]
  (let [fill (:fill options)]
    (when (and (not (coord/polar? coord)) (some? fill) (not= :none fill))
      (schema/fail! "Parallel coordinates are lines: :fill fills radar polygons, with :coord :polar."
                    {:key :fill :value fill}))))

(defn compute
  "Per panel: `{:lines [{:rows [[scaled-or-nil ..] ..] :aesthetics}] :axes axes :style style}` (`axes` - see
  [[variable-axes]], without accessors); extents (`:x` - the variable labels, ordered; `:y` - the scaled values and
  axis ends), aesthetic extents, `:na`, `:dropped` (`:variables` - rows with a missing value), `:scale-defaults`
  hiding the plot's y axis (in a flipped polar plot also padding the value angle, `:expand`), `:style` - the chart
  style (`:fill :none` in cartesian plots; the legend's too)."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme plot-scales coord flip?] :as env}]
  (check-fill! options coord)
  (let [{:keys [panels dropped]} (group/split groups mapping env)
        axes (variable-axes options (mapcat (fn [gs] (mapcat :rows gs)) (vals panels)))
        drop? (= :drop (:missing options))
        panel-lines (update-vals panels
                                 (fn [gs]
                                   (mapv (fn [g]
                                           (let [rows (mapv #(row-values axes %) (:rows g))]
                                             {:rows (if drop? (filterv #(every? some? %) rows) rows)
                                              :aesthetics (:aesthetics g)
                                              :incomplete (count (remove #(every? some? %) rows))}))
                                         gs)))
        labels (mapv :label axes)
        x-extent (assoc (data/extent :x labels) :ordered? true)
        y (y-extent axes (mapcat (fn [ls] (mapcat (comp (partial apply concat) :rows) ls)) (vals panel-lines)))
        st (cond-> (style options theme) (not (coord/polar? coord)) (assoc :fill :none))
        padding (merge {:x [0.05 0.05] :y [0.05 0.05]} (:padding options))
        drawn-axes (mapv #(dissoc % :accessor) axes)]
    {:data (update-vals panel-lines (fn [ls] {:lines (mapv #(dissoc % :incomplete) ls) :axes drawn-axes :style st}))
     :extents (merge-with merge
                          (group/aesthetic-extents panels plot-scales)
                          (update-vals panels (constantly {:x x-extent :y y})))
     :padding padding
     :na (group/na-dims panels)
     :dropped (assoc dropped :variables (reduce + 0 (mapcat #(map :incomplete %) (vals panel-lines))))
     :scale-defaults {:y (cond-> {:axis false} (and (coord/polar? coord) flip?) (assoc :expand (:y padding)))}
     :style st}))

;; draw

(defn- full-circle?
  "Does polar `geometry` cover the whole circle?"
  [geometry]
  (let [[t0 t1] (:theta geometry)]
    (< (m/abs (- (m/abs (- (double t1) (double t0))) m/TWO_PI)) 1.0e-9)))

(defn- closed
  "Range points `pts` of a row, closed with its first point a turn further on when `closes?` (a radar with the
  variables on the angles of a full circle), so arcs and smooth curves to it run forward."
  [geometry closes? pts]
  (if closes?
    (let [[h v] (first pts)
          [t0 t1] (:theta geometry)]
      (conj pts [(+ (double h) (- (double t1) (double t0))) v]))
    pts))

(def ^:private ^:const smooth-step-pixels
  "Largest distance (pixels, measured along the straight or arc connection) between samples of a smooth curve."
  2.0)

(def ^:private ^:const max-smooth-steps
  "Most samples of one smooth connection."
  1000.0)

(defn smooth-segment
  "Range points of the smooth connection from `p` to `q` (`[h v]` each; `p` left out), `n` samples: a cubic bezier
  whose control points differ from the ends only along the axis coordinate - the value coordinate (index
  `value-index` of a point) eases from `p`'s value to `q`'s (smoothstep), the axis coordinate runs between the axes
  with its control points `smoothness` / 2 of the gap in from each end. The curve meets both axes perpendicularly and
  stays between the two values."
  [p q value-index smoothness n]
  (let [value-index (long value-index)
        n (long n)
        axis-index (- 1 value-index)
        a0 (double (nth p axis-index)) da (- (double (nth q axis-index)) a0)
        b0 (double (nth p value-index)) db (- (double (nth q value-index)) b0)
        c (* 0.5 (double smoothness))]
    (mapv (fn [^long i]
            (let [t (/ (double i) (double n))
                  u (- 1.0 t)
                  a (+ a0 (* da (+ (* 3.0 u u t c) (* 3.0 u t t (- 1.0 c)) (* t t t))))
                  b (+ b0 (* db t t (- 3.0 (* 2.0 t))))]
              (if (zero? value-index) [b a] [a b])))
          (range 1 (inc n)))))

(defn- smooth-run
  "Range points of the run `run` (finite `[h v]` points) joined by smooth connections (see [[smooth-segment]]) in
  `geometry`, sampled every [[smooth-step-pixels]] along the straight or arc connection."
  [geometry {:keys [value-index smoothness]} run]
  (into [(first run)]
        (mapcat (fn [[p q]]
                  (let [length (polyline/path-length (coord/line-path geometry [p q] true))]
                    (smooth-segment p q value-index smoothness
                                    (long (m/constrain (m/ceil (/ length smooth-step-pixels)) 2.0 max-smooth-steps))))))
        (partition 2 1 run)))

(defn- point-runs
  "Drawn pixel runs of range points `pts` (`[h v]`, `NaN` where missing) in `geometry`: each run of finite points
  joined - smooth curves with `smooth` (`{:value-index :smoothness}`, see [[smooth-segment]]), else straight - by
  chords, or arcs with the geometry's `:arcs`."
  [geometry pts smooth]
  (mapv (fn [run] (mapv curve/drawn-point (coord/line-path geometry (cond->> run smooth (smooth-run geometry smooth))
                                                          (:arcs geometry))))
        (polyline/runs (mapv first pts) (mapv second pts))))

(defn- axis-frame
  "Placement of a tick mark and its label at range point `h` (its first coordinate): `:normal` - the unit pixel
  direction from the axis toward the label, `:anchor` - the label anchor. Right of a vertical axis, below a horizontal
  one (flipped plots); polar - beside a spoke (toward decreasing angles, as the plot's radial labels), or outward from
  a ring at the tick's angle `h` (flipped)."
  [geometry flipped? h]
  (cond
    geometry (let [side (if flipped? (double h) (- (double h) m/HALF_PI))]
               {:normal [(m/sin side) (- (m/cos side))] :anchor (coord/outward-anchor side)})
    flipped? {:normal [0.0 1.0] :anchor [:center :top]}
    :else {:normal [1.0 0.0] :anchor [:left :middle]}))

(defn axis-ticks
  "Ticks (original units) and their labels `[[tick label] ..]` of an axis over `domain` `[lo hi]` about `length`
  pixels long, with theme `spacing` pixels between ticks - at least two (see
  [[cljplot2.scale.position/ticks-inside]]); a single tick for a zero-length domain."
  [[^double lo ^double hi] ^double length ^double spacing]
  (if (== lo hi)
    (let [sc (s/scale :linear {:domain [(- lo 1.0) (+ hi 1.0)] :ticks 2})]
      [[lo (first (scale/tick-labels sc {:kind :linear} [lo]))]])
    (let [{sc :scale ticks :ticks} (scale/ticks-inside #(s/scale :linear {:domain [lo hi] :ticks %})
                                                       (scale/tick-hint length spacing)
                                                       #(<= lo (double %) hi))]
      (mapv vector ticks (scale/tick-labels sc {:kind :linear} ticks)))))

(defn- pixel [geometry [h v]]
  (let [^fastmath.vector.Vec2 p (coord/to-pixel geometry (double h) (double v))] [(.x p) (.y p)]))

(defn- axis-ends
  "Range points `[start end]` of `axis` (its scaled `lo` and `hi`) through `rp`; `nil` when the scales can't place
  them."
  [rp {:keys [label domain] :as axis}]
  (let [ends [(rp label (scaled axis (first domain))) (rp label (scaled axis (second domain)))]]
    (when (every? #(m/valid-double? (double %)) (apply concat ends)) ends)))

(defn- axis-path
  "Pixel points of an axis from range point `start` to `end`: a spoke, a straight line, or (variables on rings) an
  arc."
  [geometry start end]
  (coord/line-path geometry [start end] true))

(defn- draw-axes!
  "Each variable's axis: its line, ticks and tick labels (see the namespace doc)."
  [canvas axes rp geometry flipped? env]
  (let [{:keys [line-color line-width tick-color tick-length tick-width label-gap tick-spacing]} (get-in env [:theme :axis :y])
        tick-length (double tick-length)
        off (+ tick-length (double label-gap))]
    (doseq [{:keys [label domain] :as axis} axes
            :let [[start end] (axis-ends rp axis)]
            :when start
            :let [path (axis-path geometry start end)]]
      (polyline/stroke-runs! canvas [path] {:color line-color :width line-width})
      (doseq [[tick text] (axis-ticks domain (polyline/path-length path) (double tick-spacing))
              :let [tp (rp label (scaled axis (double tick)))
                    [px py] (pixel geometry tp)
                    {[nx ny] :normal anchor :anchor} (axis-frame geometry flipped? (first tp))
                    px (double px) py (double py) nx (double nx) ny (double ny)]]
        (polyline/stroke-runs! canvas [[[px py] [(+ px (* nx tick-length)) (+ py (* ny tick-length))]]]
                               {:color tick-color :width tick-width})
        (label/draw-at! canvas {:kind :label :role :tick-label :text text} anchor (+ px (* nx off)) (+ py (* ny off)) env)))))

(defn- draw-web!
  "Radar web: lines joining the axes at equal fractions of their length (ticks of 0..1 over the longest axis) -
  closed polygons when `closes?`, else open lines (partial circles, rings) - in theme `[:panel :grid :y]`."
  [canvas axes rp geometry closes? env]
  (let [line (get-in env [:theme :panel :grid :y])
        placed (keep #(when-let [ends (axis-ends rp %)] [(:label %) ends]) axes)
        length (double (reduce max 0.0 (map (fn [[_ [start end]]] (polyline/path-length (axis-path geometry start end))) placed)))
        ends (mapv (fn [{:keys [label domain] :as axis}] [label (scaled axis (first domain)) (scaled axis (second domain))]) axes)]
    (when (and (not (theme/none? line)) (= (count placed) (count axes)) (pos? length))
      (doseq [[f _] (axis-ticks [0.0 1.0] length (double (get-in env [:theme :axis :y :tick-spacing])))
              :let [f (double f)
                    pts (mapv (fn [[label ^double lo ^double hi]] (rp label (+ lo (* f (- hi lo))))) ends)]]
        (polyline/stroke-runs! canvas (point-runs geometry (closed geometry closes? pts) nil) line)))))

(defn draw
  "Draw one panel: the radar web (polar, while the plot's y axis is hidden), every row's line (filled radar polygons
  first), then the variable axes."
  [{:keys [options]} {:keys [lines axes style]} canvas _box scales env]
  (let [geometry (scale/coord-geometry scales)
        flipped? (scale/flipped? scales)
        closes? (and (some? geometry) (not flipped?) (full-circle? geometry))
        rp (scale/range-projection scales)
        smooth (when (= :smooth (:curve options))
                 {:value-index (if flipped? 0 1) :smoothness (double (:smoothness options 1.0))})
        theme (:theme env)]
    (when (and geometry (false? (:axis (:y scales))))
      (draw-web! canvas axes rp geometry closes? env))
    (doseq [{:keys [rows aesthetics]} lines
            :let [st (group/style style aesthetics scales theme)]
            :when st
            :let [fill (polyline/area-color st false)
                  pattern-fill (draw/pattern-fill st fill)]
            row rows
            :let [pts (mapv (fn [axis v] (if (nil? v) [##NaN ##NaN] (rp (:label axis) v))) axes row)
                  runs (point-runs geometry (closed geometry closes? pts) smooth)]]
      (when (every? some? row)
        (polyline/fill-closed-runs! canvas runs fill pattern-fill))
      (polyline/stroke-runs! canvas runs st))
    (when (:axes options true)
      (draw-axes! canvas axes rp geometry flipped? env))))

(defn legend
  "Key glyph: a line through the middle of `box`; with a radar `:fill` a filled square outlined in the line style,
  see [[cljplot2.graphics.polyline/key-glyph!]]. The style is the computed one (env `:chart-stats`), so a theme fill
  shows only where it is drawn."
  [{:keys [options]} attributes canvas box env]
  (let [st (merge (or (get-in env [:chart-stats :style]) (style options (:theme env)))
                  (select-keys attributes [:color :dash :pattern]))
        area (polyline/area-color st false)]
    (polyline/key-glyph! canvas box st area (draw/pattern-fill st area))))
