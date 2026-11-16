(ns cljplot2.chart.parallel
  "Parallel coordinates: every row of wide data drawn as a line across one axis per variable; under a polar plot
  `:coord` the same chart is a radar (spider) chart - variables on angles, the line closed.

  `:variables` (required) - at least two keywords or `[label accessor]` pairs, in axis order; labels (a keyword's
  name, or the pair's label as a string) must differ. They are the levels of the chart's `:x` (categorical, in the
  given order: the plot's x axis names the variables); the x mapping isn't used. Values must be numbers: a missing,
  non-numeric or non-finite value breaks the row's line there (`:missing :break`, default) or drops the row
  (`:missing :drop`); such rows are counted in `:dropped {:variables n}`.

  Per variable, over the rows of all panels (facets share the axes): the domain `[lo hi]` - its finite data range,
  or `(get-in options [:limits key])` (`key` - the keyword, or the pair's label) - and the affine scaling
  `(v - offset) / scale` of `:scaling`:

  * `:uniminmax` (default) - `(v - lo) / (hi - lo)`, every axis spans 0..1
  * `:std` - `(v - mean) / stddev`; `:robust` - `(v - median) / MAD`; `:center` - `v - mean`
  * `:globalminmax` - the values as they are (one common scale)

  A zero spread puts the variable's values at 0.5 (`:uniminmax`) or 0. The chart's `:y` is the scaled value; its
  extent covers every scaled value and axis end. The plot's y axis and grid are hidden (`:scale-defaults`, the
  plot's `:scales :y :axis` wins); with `:axes` (default true) the chart draws each variable's axis - a line from its
  scaled `lo` to `hi`, ticks of a linear scale over the domain (original units; as many as theme
  `[:axis :y :tick-spacing]` allows along the axis's pixel length) with labels (text role `:tick-label`, styled by
  theme `[:axis :y]`): right of the axis, below it in a flipped plot, beside the spoke in a polar one.

  Polar (radar): each row's line closes (last variable -> first), chords or arcs (`:coord :arcs`); `:fill` (`:color`
  - the line's colour at `:alpha`, or a colour) fills every complete row's polygon, with `:pattern`; a web of
  polygons (theme `[:panel :grid :y]`) joins the axes at equal fractions of their length. `:fill` is polar only and
  `:curve :bezier` (smooth lines) cartesian only.

  Options (style defaults from theme `[:chart :parallel]`): `:color`, `:width`, `:dash`, `:fill`, `:alpha`,
  `:pattern`, `:pattern-color`, `:variables`, `:scaling`, `:limits {key [lo hi]}`, `:axes`, `:missing`, `:curve`.
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

(def ^:private limits
  (schema/named-type ::limits #(and (vector? %) (= 2 (count %)) (every? schema/finite? %) (< (double (% 0)) (double (% 1))))
                     "should be [lo hi], finite numbers, lo < hi"))

(def scalings
  "Values of `:scaling`."
  #{:uniminmax :std :robust :center :globalminmax})

(def ^:private style-entries
  (into (conj polyline/style-entries
              (schema/optional :fill [:or [:= :color] schema/color])
              (schema/optional :alpha schema/fraction))
        draw/pattern-style-entries))

(defn- unknown-limits
  [{vs :variables lims :limits}]
  (when (and (vector? vs) (map? lims))
    (let [ks (set (map variable-key vs))
          unknown (remove ks (keys lims))]
      (when (seq unknown)
        (str ":limits keys should be variables (keywords, or pair labels), unknown: " (pr-str (vec unknown)))))))

(def schemas
  "Schemas of the parallel chart: `:style` (theme `[:chart :parallel]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (schema/with-rules
              (-> [:map {:closed true}]
                  (into style-entries)
                  (into data/common-options)
                  (conj [:variables variables]
                        (schema/optional :scaling (into [:enum] (sort scalings)))
                        (schema/optional :limits [:map-of any? limits])
                        (schema/nilable :axes schema/bool)
                        (schema/optional :missing [:enum :break :drop])
                        (schema/optional :curve [:enum :bezier])))
              [unknown-limits [:limits]])
   :mapping [:maybe [:map]]})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :parallel]) (select-keys options [:color :width :dash :fill :alpha :pattern :pattern-color])))

;; scaling

(defn- numeric-value
  "`v` as a double when it is a finite number, else `nil` (missing)."
  [v]
  (when (and (number? v) (m/valid-double? (double v))) (double v)))

(defn- affine
  "`[offset scale]` of `scaling` for a variable with finite `values` and domain `[lo hi]`; a zero (or undefined)
  spread gives the values 0.5 (`:uniminmax`) or 0."
  [scaling values [^double lo ^double hi]]
  (let [[o s] (case scaling
                :uniminmax [lo (- hi lo)]
                :std [(stats/mean values) (stats/stddev values)]
                :robust [(stats/median values) (stats/median-absolute-deviation values)]
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
  (let [scaling (:scaling options :uniminmax)]
    (mapv (fn [v]
            (let [k (variable-key v)
                  acc (data/row-accessor :variables (if (keyword? v) v (second v)))
                  values (into [] (keep (comp numeric-value acc)) rows)
                  domain (or (some->> (get-in options [:limits k]) (mapv double))
                             (when (seq values) [(reduce min values) (reduce max values)]))]
              (when (and (seq rows) (nil? domain))
                (schema/fail! (str "Variable " (pr-str k) " has no numeric values.") {:key :variables :value k}))
              (let [domain (or domain [0.0 1.0])
                    [o s] (affine scaling (if (seq values) values domain) domain)]
                {:label (variable-label v) :key k :accessor acc :domain domain :offset o :scale s})))
          (:variables options))))

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

(defn- check-coord!
  "Throw for an option the plot's `coord` doesn't support: `:fill` in cartesian plots (`{:key :fill}`), `:curve` in
  polar ones (`{:key :coord}`)."
  [options coord]
  (let [polar? (coord/polar? coord)
        fill (:fill options)]
    (when (and (not polar?) (some? fill) (not= :none fill))
      (schema/fail! "Parallel coordinates are lines: :fill fills radar polygons, with :coord :polar."
                    {:key :fill :value fill}))
    (when (and polar? (:curve options))
      (schema/fail! "Radar lines are straight or arcs: :curve isn't supported with :coord :polar."
                    {:key :coord :value :curve}))))

(defn compute
  "Per panel: `{:lines [{:rows [[scaled-or-nil ..] ..] :aesthetics}] :axes axes :style style}` (`axes` - see
  [[variable-axes]], without accessors); extents (`:x` - the variable labels, ordered; `:y` - the scaled values and
  axis ends), aesthetic extents, `:na`, `:dropped` (`:variables` - rows with a missing value), `:scale-defaults`
  hiding the plot's y axis."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme plot-scales coord] :as env}]
  (check-coord! options coord)
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
        st (style options theme)
        drawn-axes (mapv #(dissoc % :accessor) axes)]
    {:data (update-vals panel-lines (fn [ls] {:lines (mapv #(dissoc % :incomplete) ls) :axes drawn-axes :style st}))
     :extents (merge-with merge
                          (group/aesthetic-extents panels plot-scales)
                          (update-vals panels (constantly {:x x-extent :y y})))
     :padding {:x [0.05 0.05] :y [0.05 0.05]}
     :na (group/na-dims panels)
     :dropped (assoc dropped :variables (reduce + 0 (mapcat #(map :incomplete %) (vals panel-lines))))
     :scale-defaults {:y {:axis false}}}))

;; draw

(defn- full-circle?
  "Does polar `geometry` cover the whole circle?"
  [geometry]
  (let [[t0 t1] (:theta geometry)]
    (< (m/abs (- (m/abs (- (double t1) (double t0))) m/TWO_PI)) 1.0e-9)))

(defn- closed
  "Range points `pts` of a radar row closed with its first point (`nil` - cartesian: `pts` as they are); on a full
  circle the closing point is a turn further on, so an arc to it runs forward."
  [geometry pts]
  (if geometry
    (let [[h v] (first pts)
          [t0 t1] (:theta geometry)
          turn (if (full-circle? geometry) (- (double t1) (double t0)) 0.0)]
      (conj pts [(+ (double h) turn) v]))
    pts))

(defn- point-runs
  "Drawn pixel runs of range points `pts` (`[h v]`, `NaN` where missing) in `geometry`: each run of finite points
  joined (chords, or arcs with the geometry's `:arcs`), bezier-smoothed with `bezier?`."
  [geometry pts bezier?]
  (let [runs (polyline/runs (mapv first pts) (mapv second pts))]
    (mapv (fn [run] (cond->> (mapv curve/drawn-point (coord/line-path geometry run (:arcs geometry)))
                      bezier? curve/bezier-points))
          runs)))

(defn- axis-frame
  "Placement of a variable's tick marks and labels: `:normal` - the unit pixel direction from the axis toward its
  labels, `:anchor` - the label anchor. Right of a vertical axis, below a horizontal one (flipped plots), beside a
  polar spoke at angle `theta` (toward decreasing angles, as the plot's radial labels)."
  [geometry flipped? theta]
  (cond
    geometry (let [side (- (double theta) m/HALF_PI)]
               {:normal [(m/sin side) (- (m/cos side))] :anchor (coord/outward-anchor side)})
    flipped? {:normal [0.0 1.0] :anchor [:center :top]}
    :else {:normal [1.0 0.0] :anchor [:left :middle]}))

(defn axis-ticks
  "Ticks (original units) and their labels `[[tick label] ..]` of an axis over `domain` `[lo hi]` about `length`
  pixels long, with theme `spacing` pixels between ticks; a single tick for a zero-length domain."
  [[^double lo ^double hi] ^double length ^double spacing]
  (let [sc (s/scale :linear {:domain (if (== lo hi) [(- lo 1.0) (+ hi 1.0)] [lo hi]) :ticks (scale/tick-hint length spacing)})
        ticks (if (== lo hi) [lo] (filterv #(<= lo (double %) hi) (s/ticks sc)))]
    (mapv vector ticks (scale/tick-labels sc {:kind :linear} ticks))))

(defn- pixel [geometry [h v]]
  (let [^fastmath.vector.Vec2 p (coord/to-pixel geometry (double h) (double v))] [(.x p) (.y p)]))

(defn- draw-axes!
  "Each variable's axis: its line, ticks and tick labels (see the namespace doc)."
  [canvas axes rp geometry flipped? env]
  (let [{:keys [line-color line-width tick-color tick-length tick-width label-gap tick-spacing]} (get-in env [:theme :axis :y])
        tick-length (double tick-length)]
    (doseq [{:keys [label domain] :as axis} axes
            :let [[lo hi] domain
                  start (rp label (scaled axis lo))
                  end (rp label (scaled axis hi))]
            :when (every? #(m/valid-double? (double %)) (concat start end))
            :let [[x0 y0 :as p0] (pixel geometry start)
                  [x1 y1] (pixel geometry end)
                  {[nx ny] :normal anchor :anchor} (axis-frame geometry flipped? (first start))
                  nx (double nx) ny (double ny)]]
      (polyline/stroke-runs! canvas [[p0 [x1 y1]]] {:color line-color :width line-width})
      (doseq [[tick text] (axis-ticks domain (m/hypot (- (double x1) (double x0)) (- (double y1) (double y0)))
                                      (double tick-spacing))
              :let [[px py] (pixel geometry (rp label (scaled axis (double tick))))
                    px (double px) py (double py)
                    off (+ tick-length (double label-gap))]]
        (polyline/stroke-runs! canvas [[[px py] [(+ px (* nx tick-length)) (+ py (* ny tick-length))]]]
                               {:color tick-color :width tick-width})
        (label/draw-at! canvas {:kind :label :role :tick-label :text text} anchor (+ px (* nx off)) (+ py (* ny off)) env)))))

(defn- draw-web!
  "Radar web: closed polygons joining the axes at equal fractions of their length (ticks of 0..1 over the longest
  axis), in theme `[:panel :grid :y]`."
  [canvas axes rp geometry env]
  (let [line (get-in env [:theme :panel :grid :y])
        ends (mapv (fn [{:keys [label domain] :as axis}]
                     [label (scaled axis (first domain)) (scaled axis (second domain))])
                   axes)
        length (reduce max 0.0 (map (fn [[label lo hi]]
                                      (let [[x0 y0] (pixel geometry (rp label lo)) [x1 y1] (pixel geometry (rp label hi))]
                                        (m/hypot (- (double x1) (double x0)) (- (double y1) (double y0)))))
                                    ends))]
    (when (and (not (theme/none? line)) (m/valid-double? length))
      (doseq [[f _] (axis-ticks [0.0 1.0] length (double (get-in env [:theme :axis :y :tick-spacing])))
              :let [f (double f)
                    pts (mapv (fn [[label ^double lo ^double hi]] (rp label (+ lo (* f (- hi lo))))) ends)]]
        (polyline/stroke-runs! canvas (point-runs geometry (closed geometry pts) false) line)))))

(defn draw
  "Draw one panel: the radar web (polar), every row's line (filled radar polygons first), then the variable axes."
  [{:keys [options]} {:keys [lines axes style]} canvas _box scales env]
  (let [geometry (scale/coord-geometry scales)
        rp (scale/range-projection scales)
        bezier? (= :bezier (:curve options))
        theme (:theme env)]
    (when geometry (draw-web! canvas axes rp geometry env))
    (doseq [{:keys [rows aesthetics]} lines
            :let [st (group/style style aesthetics scales theme)]
            :when st
            :let [fill (if geometry (polyline/area-color st false) :none)
                  st (assoc st :fill fill)]
            row rows
            :let [pts (mapv (fn [axis v] (if (nil? v) [##NaN ##NaN] (rp (:label axis) v))) axes row)]]
      (curve/draw-runs! canvas (point-runs geometry (closed geometry pts) bezier?) st
                        (when (and geometry (every? some? row)) :closed) :break))
    (when (:axes options true)
      (draw-axes! canvas axes rp geometry (scale/flipped? scales) env))))

(defn legend
  "Key glyph: a line through the middle of `box`; with a radar `:fill` a filled square outlined in the line style,
  see [[cljplot2.graphics.polyline/key-glyph!]]."
  [{:keys [options]} attributes canvas box env]
  (let [st (merge (style options (:theme env)) (select-keys attributes [:color :dash :pattern]))
        area (polyline/area-color st false)]
    (polyline/key-glyph! canvas box st area (draw/pattern-fill st area))))
