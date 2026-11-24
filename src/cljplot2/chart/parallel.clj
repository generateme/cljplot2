(ns cljplot2.chart.parallel
  "Parallel coordinates: every row of wide data drawn as a line across one axis per variable; under a polar plot
  `:coord` the same chart is a radar (spider) chart (see Polar below).

  `:variables` (required) - at least two keywords or `[label accessor]` pairs, in axis order; labels (a keyword's
  name, or the pair's label as a string) must differ. They are the levels of the chart's `:x` (categorical, in the
  given order: the plot's x axis names the variables - every name drawn, also when names collide, as the axis scale
  default `:overlapping-labels :keep` (the plot's `:scales :x :overlapping-labels` wins; long names: rotate them
  with theme `[:axis :x :label-rotation]`)); the x mapping isn't used. Values must be numbers: a missing,
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
  theme `[:axis :y]`, its `:tick-label` too): right of the axis, below it in a flipped plot, beside the spoke in a polar one (outside the
  ring in a flipped polar one). In a radar (variables on angles) a label at a spoke's inner end repeating one at
  the inner end of an earlier spoke (the same text at the same radius - e.g. 0 at the centre) is drawn once, on the
  first spoke. A label overlapping one drawn before it (any axis, in axis and tick order) is dropped.

  `:curve :smooth` joins consecutive axes with smooth curves instead of straight segments: a cubic bezier in range
  space that meets every axis perpendicularly, at its value, and never leaves the range between the two values;
  `:smoothness` (0 1], default 1 - the bend spread over the whole gap, smaller values keep it near the axes. In polar
  plots the curves are sampled in range space, so they follow the circle whether or not `:coord :arcs` is set.

  Polar (radar): variables on angles, values on the radius, chords or arcs (`:coord :arcs`); on a full circle each
  row's line closes (last variable -> first), on a partial one (`:span` < 360) it doesn't. With `:flip` the
  variables are rings and the values angles: lines don't close, and the angle is padded by `:padding :y` (suggested
  as its `:expand`, so the ends of an axis don't meet; the plot's `:scales :y` wins). `:fill`
  (`:color` - the line's colour faded by `:alpha`: its own alpha times `:alpha`; or a colour) fills every complete row's polygon, with `:pattern`; polar
  only (a theme fill is ignored in cartesian plots). While the plot's y axis is hidden, a web (theme
  `[:panel :grid :y]`) joins the axes at equal fractions of their length, as the rows do: closed polygons on a full
  circle, else open lines (across the rings with `:flip`).

  Options (style defaults from theme `[:chart :parallel]`): `:color`, `:line-width`, `:dash`, `:fill`, `:alpha`,
  `:pattern`, `:pattern-color`, `:variables`, `:scaling`, `:robust-center`, `:robust-spread`, `:limits {key [lo hi]}`,
  `:axes`, `:missing`, `:curve`, `:smoothness`, `:padding {:y [lo hi]}` (fractions of the scaled extent, default 0.05
  each side; the variables' `:x` is a bands scale).
  Groups (see [[cljplot2.data.group]]) come from ordinal aesthetics (`:color`, `:dash`) and `:group`; a line per
  row, in its group's style.

  Functions: [[schemas]] - the chart's schemas; [[scalings]], [[robust-spreads]] - the scaling values; [[variable-key]], [[variable-label]] - a variable's limits key and label; [[variable-axes]] - the axes over the rows; [[compute]] - per panel the scaled lines; [[smooth-segment]] - a smooth connection between axes; [[axis-ticks]] - an axis's ticks and labels; [[draw]] - one panel; [[legend]] - the key glyph."
  (:require [fastmath.core :as m]
            [fastmath.stats :as stats]
            [fastmath.vector :as v]
            [wadogo.scale :as s]
            [cljplot2.content.axis :as axis-content]
            [cljplot2.content.label :as label]
            [cljplot2.data.group :as group]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.curve :as curve]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.scale.coord :as coord]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; variables

(defn- labelled-variable?
  "Tests whether a `:variables` entry is a `[label accessor]` pair.

  Parameters:

  - `v`: the entry.

  Returns a truthy value for a vector of a string and a keyword or function, else a falsy one."
  [v]
  (and (vector? v) (m/== 2 (count v)) (string? (first v)) (or (keyword? (second v)) (fn? (second v)))))

(def ^:private variable
  "Schema of one `:variables` entry: a keyword or a `[label accessor]` pair."
  (schema/named-type ::variable #(or (keyword? %) (labelled-variable? %))
                     "should be a keyword or a [label accessor] pair (a string and a keyword or function)"))

(defn- variable-key
  "Gives the key of a `:variables` entry in `:limits`.

  Parameters:

  - `v`: the entry - a keyword or a `[label accessor]` pair.

  Returns the keyword, or the pair's label.

  See also [[variable-label]]."
  [v]
  (if (keyword? v) v (first v)))

(defn- variable-label
  "Gives the axis label of a `:variables` entry.

  Parameters:

  - `v`: the entry - a keyword or a `[label accessor]` pair.

  Returns the keyword's name, or the pair's label.

  See also [[variable-key]]."
  [v]
  (if (keyword? v) (name v) (first v)))

(defn- duplicate-labels
  "Explains why the labels of `:variables` entries repeat.

  Parameters:

  - `vs` (sequence): the entries.

  Returns the error message naming the repeated labels, or `nil` when every label differs."
  [vs]
  (let [labels (map variable-label vs)]
    (when (not= (count labels) (count (distinct labels)))
      (str "variable labels must differ, repeated: " (pr-str (vec (for [[l n] (frequencies labels) :when (m/> (long n) 1)] l)))))))

(def ^:private variables
  "Schema of `:variables`: at least two entries with different labels."
  (schema/with-rules [:vector variable]
    [#(when (m/< (count %) 2) "should have at least 2 variables") nil]
    [duplicate-labels nil]))

(def ^:private scalings
  "Values of `:scaling`."
  #{:uniminmax :std :robust :center :globalminmax})

(def ^:private robust-spreads
  "Named `:robust-spread` estimators (`fastmath.stats/robust-stddev` methods)."
  #{:mad :iqr :sn :qn :tukey :biweight :stddev :meanad :gini})

(def ^:private robust-spread-schema
  "Schema of `:robust-spread`: a named estimator or a quantile in (0 0.5)."
  (schema/named-type ::robust-spread #(or (contains? robust-spreads %) (and (schema/finite? %) (m/< 0.0 (double %) 0.5)))
                     (str "should be one of " (pr-str (vec (sort robust-spreads))) " or a quantile p, 0 < p < 0.5")))

(def ^:private style-entries
  "Map entries of the parallel style (theme `[:chart :parallel]`; also options): the line style, the radar fill and its alpha, the pattern."
  (into (conj polyline/style-entries
              (schema/optional :fill {:cljplot2/section :style :description "radar polygon fill (polar only): :none, :color (the line's colour, its own alpha times :alpha) or a colour"}
                               [:or [:= :color] schema/color])
              (schema/optional :alpha {:cljplot2/section :style :description "factor of the line colour's alpha filling :fill :color"} schema/fraction))
        draw/pattern-style-entries))

(defn- unknown-limits
  "Explains which `:limits` keys name no variable.

  Parameters:

  - `options` (map): `:variables`, `:limits`.

  Returns the error message listing the unknown keys, or `nil` (also when either option is missing or malformed)."
  [{vs :variables lims :limits}]
  (when (and (vector? vs) (map? lims))
    (let [ks (set (map variable-key vs))
          unknown (remove ks (keys lims))]
      (when (seq unknown)
        (str ":limits keys should be variables (keywords, or pair labels), unknown: " (pr-str (vec unknown)))))))

(defn- needs
  "Makes an options rule: an option is used only when a predicate of the options holds.

  Parameters:

  - `k` (keyword): the option.
  - `pred` (function): of the options.
  - `message` (string): the error message.

  Returns a `[problem path]` rule (see [[cljplot2.schema/with-rules]]) giving `message` at `[k]` for options holding `k` where `pred` fails."
  [k pred message]
  [(fn [options] (when (and (contains? options k) (not (pred options))) message)) [k]])

(def schemas
  "Schemas of the parallel chart: `:style` (theme `[:chart :parallel]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (schema/with-rules
              (-> [:map {:closed true}]
                  (into style-entries)
                  (into data/common-options)
                  (into theme/theme-style-entries)
                  (conj [:variables {:cljplot2/section :axes :description "the variables in axis order: keywords or [label accessor] pairs (2 or more)"}
                         variables]
                        (schema/optional :scaling {:cljplot2/section :axes :description "per variable scaling: :uniminmax (0..1), :std, :robust, :center or :globalminmax"
                                                   :default :uniminmax}
                                         (into [:enum] (sort scalings)))
                        (schema/optional :limits {:cljplot2/section :axes :description "explicit variable domains {key [lo hi]}, key - the keyword or the pair's label"
                                                  :cljplot2/default-doc "each variable's finite data range"}
                                         [:map-of any? schema/finite-interval])
                        (schema/nilable :axes {:cljplot2/section :axes :description "draw each variable's axis with ticks and labels" :default true} schema/bool)
                        (schema/optional :missing {:cljplot2/section :axes :description "a missing value: :break (the line breaks there) or :drop (the row is dropped)"
                                                   :default :break}
                                         [:enum :break :drop])
                        (schema/optional :robust-center {:cljplot2/section :axes :description "centre of :scaling :robust: :median or :mean"
                                                         :cljplot2/default-doc ":median (only with :scaling :robust)"}
                                         [:enum :median :mean])
                        (schema/optional :robust-spread {:cljplot2/section :axes :description (str "spread of :scaling :robust: a fastmath.stats/robust-stddev "
                                                                           "estimator (:mad, :iqr, :sn, :qn, ...) or a quantile p in (0 0.5)")
                                                         :cljplot2/default-doc ":mad (only with :scaling :robust)"}
                                         robust-spread-schema)
                        (schema/optional :curve {:cljplot2/section :style :description "join consecutive axes with smooth curves (:smooth)"
                                                 :cljplot2/default-doc "straight segments"}
                                         [:enum :smooth])
                        (schema/optional :smoothness {:cljplot2/section :style :description "how far the smooth bend spreads over the gap between axes, (0 1]"
                                                      :cljplot2/default-doc "1 (only with :curve :smooth)"}
                                         [:and schema/finite [:> 0] [:<= 1]])
                        (schema/optional :padding {:cljplot2/section :axes :description "padding of the scaled values' span, {:y [lo hi]} fractions of the span"}
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

;; scaling

(defn- affine
  "Gives the affine scaling of a variable, `(v - offset) / scale`.

  Parameters:

  - `options` (map): `:scaling` (default `:uniminmax`), `:robust-center` (default `:median`), `:robust-spread` (default `:mad`) - see the namespace doc.
  - `values` (sequence): the variable's finite values (its domain ends without values).
  - `[lo hi]`: the variable's domain.

  Returns `[offset scale]`; a zero or undefined spread gives the values 0.5 (`:uniminmax`) or 0 (scale 1)."
  [{:keys [scaling robust-center robust-spread] :or {scaling :uniminmax robust-center :median robust-spread :mad}}
   values [^double lo ^double hi]]
  (let [[centre spread] (case scaling
                          :uniminmax [lo (m/- hi lo)]
                          :std [(stats/mean values) (stats/stddev values)]
                          :robust [(if (= :mean robust-center) (stats/mean values) (stats/median values))
                                   (stats/robust-stddev values robust-spread)]
                          :center [(stats/mean values) 1.0]
                          :globalminmax [0.0 1.0])
        o (double centre)
        s (double spread)]
    (cond
      (and (m/valid-double? s) (m/pos? s)) [o s]
      (= :uniminmax scaling) [(m/- lo 0.5) 1.0]
      :else [o 1.0])))

(defn- scaled
  "Gives a value scaled on an axis.

  Parameters:

  - `axis` (map): `:offset`, `:scale` (see [[affine]]).
  - `v` (double): the value in original units.

  Returns `(v - offset) / scale`."
  ^double [{:keys [^double offset ^double scale]} ^double v]
  (m// (m/- v offset) scale))

(defn- variable-axes
  "Gives the axis of every variable over the rows of all panels.

  Parameters:

  - `options` (map): `:variables`, `:limits` and the scaling options (see the namespace doc).
  - `rows` (sequence): the rows of all panels.

  Returns a vector of `{:label :key :accessor :domain [lo hi] :offset :scale}` in `:variables` order: the domain from `:limits`, else the finite data range, `[0 1]` without rows.

  Throws `ex-info {:key :variables}` for a variable without a numeric value in non-empty `rows` and no `:limits`.

  See also [[affine]]."
  [options rows]
  (mapv (fn [entry]
            (let [k (variable-key entry)
                  acc (data/row-accessor :variables (if (keyword? entry) entry (second entry)))
                  values (into [] (keep (comp schema/finite-double acc)) rows)
                  limits-or-data-domain (or (some->> (get-in options [:limits k]) (mapv double))
                                            (when (seq values) [(v/mn values) (v/mx values)]))]
              (when (and (seq rows) (nil? limits-or-data-domain))
                (schema/fail! (str "Variable " (pr-str k) " has no numeric values.") {:key :variables :value k}))
              (let [domain (or limits-or-data-domain [0.0 1.0])
                    [o s] (affine options (if (seq values) values domain) domain)]
                {:label (variable-label entry) :key k :accessor acc :domain domain :offset o :scale s})))
          (:variables options)))

(defn- row-values
  "Gives a row's scaled value on every axis.

  Parameters:

  - `axes` (sequence): the axes (see [[variable-axes]]).
  - `row` (map): the row.

  Returns a vector of scaled values in axis order, `nil` where the value is missing (see [[cljplot2.schema/finite-double]])."
  [axes row]
  (mapv (fn [axis] (some->> ((:accessor axis) row) schema/finite-double (scaled axis))) axes))

(defn- y-extent
  "Gives the extent of the chart's `:y`: the scaled values and every axis end.

  Parameters:

  - `axes` (sequence): the axes (see [[variable-axes]]).
  - `values` (sequence): the scaled values of all rows, `nil` where missing.

  Returns the `:y` extent (see [[cljplot2.data.rows/extent]])."
  [axes values]
  (data/extent :y (into (vec (remove nil? values))
                        (mapcat (fn [{[lo hi] :domain :as axis}] [(scaled axis lo) (scaled axis hi)]))
                        axes)))

;; compute

(defn- check-fill!
  "Checks that a radar fill is asked for only in a polar plot.

  Parameters:

  - `options` (map): the chart's options - `:fill`.
  - `coord` (map or `nil`): the plot's `:coord`.

  Returns `nil`.

  Throws `ex-info {:key :fill}` for a `:fill` other than `:none` in a cartesian plot."
  [options coord]
  (let [fill (:fill options)]
    (when (and (not (coord/polar? coord)) (not (theme/none? (data/option options :fill :none))))
      (schema/fail! "Parallel coordinates are lines: :fill fills radar polygons, with :coord :polar."
                    {:key :fill :value fill}))))

(defn compute
  "Computes the parallel chart's stats: per panel every row's scaled values, by group.

  Parameters:

  - `chart` (map): the chart value - `:options` (see the namespace doc), `:kind`, `:schema` (its style keys).
  - `split` (map): `:groups` - `{panel rows}`, `:mapping` - the merged mapping.
  - `env` (map): `:theme`, `:plot-scales`, `:coord`, `:flip` and the rest of the compute environment.

  Returns `{:data :extents :padding :padding-explicit :na :dropped :scale-defaults :style}`: per panel `{:lines [{:rows [[scaled-or-nil ..] ..] :aesthetics}] :axes axes :style st}` (`axes` - see [[variable-axes]], without accessors; rows with a missing value left out with `:missing :drop`); extents `:x` (the variable labels, ordered) and `:y` (the scaled values and axis ends) and the aesthetic extents; `:padding` - [[cljplot2.data.rows/default-padding]] under the `:padding` option; `:dropped :variables` - rows with a missing value; `:scale-defaults` drawing every variable name on the plot's x axis (`:overlapping-labels :keep`) and hiding its y axis (in a flipped polar plot also padding the value angle, `:expand`); `:style` - the chart style (`:fill :none` in cartesian plots; the legend's too).

  Throws `ex-info {:key :fill}` for a radar fill in a cartesian plot, `{:key :variables}` for a variable without numbers (see [[variable-axes]])."
  [{:keys [options] :as chart} {:keys [groups mapping]} {:keys [theme plot-scales coord flip] :as env}]
  (check-fill! options coord)
  (let [{:keys [panels dropped]} (group/split groups mapping env)
        axes (variable-axes options (mapcat (fn [gs] (mapcat :rows gs)) (vals panels)))
        drop? (= :drop (data/option options :missing :break))
        panel-lines (update-vals panels
                                 (fn [gs]
                                   (mapv (fn [g]
                                           (let [rows (mapv #(row-values axes %) (:rows g))]
                                             {:rows (if drop? (filterv #(every? some? %) rows) rows)
                                              :aesthetics (:aesthetics g)
                                              :incomplete (count (remove #(every? some? %) rows))}))
                                         gs)))
        labels (mapv :label axes)
        x-extent (assoc (data/extent :x labels) :ordered true)
        y (y-extent axes (mapcat (fn [ls] (mapcat (comp (partial apply concat) :rows) ls)) (vals panel-lines)))
        st (cond-> (theme/resolved-style theme chart) (not (coord/polar? coord)) (assoc :fill :none))
        {:keys [padding] :as padding-stats} (data/padding-stats data/default-padding {} options)
        drawn-axes (mapv #(dissoc % :accessor) axes)]
    (into padding-stats
          {:data (update-vals panel-lines (fn [ls] {:lines (mapv #(dissoc % :incomplete) ls) :axes drawn-axes :style st}))
           :extents (merge-with merge
                                (group/aesthetic-extents panels plot-scales)
                                (update-vals panels (constantly {:x x-extent :y y})))
           :na (group/na-dims panels)
           :dropped (assoc dropped :variables (reduce m/long-add 0 (mapcat #(map :incomplete %) (vals panel-lines))))
           ;; every variable name drawn: they name separate axes, not ticks of one scale
           :scale-defaults {:x {:overlapping-labels :keep}
                            :y (cond-> {:axis false} (and (coord/polar? coord) flip) (assoc :expand (:y padding)))}
           :style st})))

;; draw

(defn- closed
  "Closes a radar row's range points.

  Parameters:

  - `geometry` (map or `nil`): the polar geometry (its `:theta` range).
  - `closes?`: does the row close (a radar with the variables on the angles of a full circle)?
  - `pts` (vector): the row's range points `[h v]`.

  Returns `pts` with its first point a turn further on appended when `closes?`, so arcs and smooth curves to it run forward; else `pts`."
  [geometry closes? pts]
  (if closes?
    (let [[^double h v] (first pts)
          [^double t0 ^double t1] (:theta geometry)]
      (conj pts [(m/+ h (m/- t1 t0)) v]))
    pts))

(def ^{:private true :const true :tag 'double} smooth-step-pixels
  "Largest distance (pixels, measured along the straight or arc connection) between samples of a smooth curve (render quality, not an option)."
  2.0)

(def ^{:private true :const true :tag 'double} max-smooth-steps
  "Most samples of one smooth connection (a safety limit, not an option)."
  1000.0)

(defn smooth-segment
  "Gives the range points of a smooth connection between two axes.

  The connection is a cubic bezier whose control points differ from the ends only along the axis coordinate: the value coordinate eases from `p`'s value to `q`'s (smoothstep), the axis coordinate runs between the axes with its control points `smoothness` / 2 of the gap in from each end. The curve meets both axes perpendicularly and stays between the two values.

  Parameters:

  - `p`, `q`: the ends `[h v]` in range space.
  - `value-index` (long): the index of the value coordinate in a point (0 or 1).
  - `smoothness` (number): in (0 1], how far the bend spreads over the gap.
  - `n` (long): the number of samples.

  Returns a vector of `n` points `[h v]`, `p` left out, `q` last.

  See also [[smooth-run]]."
  [p q value-index smoothness n]
  ;; more than 4 arguments: no primitive hints
  (let [vi (long value-index)
        steps (long n)
        axis-index (m/long-sub 1 vi)
        a0 (double (nth p axis-index))
        da (m/- (double (nth q axis-index)) a0)
        b0 (double (nth p vi))
        db (m/- (double (nth q vi)) b0)
        c (m/* 0.5 (double smoothness))]
    (mapv (fn [^long i]
            (let [t (m// (double i) (double steps))
                  u (m/- 1.0 t)
                  a (m/+ a0 (m/* da (m/+ (m/* 3.0 u u t c) (m/* 3.0 u t t (m/- 1.0 c)) (m/* t t t))))
                  b (m/+ b0 (m/* db t t (m/- 3.0 (m/* 2.0 t))))]
              (if (m/zero? vi) [b a] [a b])))
          (range 1 (m/long-inc steps)))))

(defn- smooth-run
  "Joins a run of range points by smooth connections.

  Parameters:

  - `geometry` (map or `nil`): the polar geometry; `nil` - cartesian.
  - `smooth` (map): `:value-index`, `:smoothness` (see [[smooth-segment]]).
  - `run` (sequence): finite range points `[h v]`, at least one.

  Returns a vector of range points: the first point, then each connection sampled every [[smooth-step-pixels]] along the straight or arc connection (2 to [[max-smooth-steps]] samples)."
  [geometry {:keys [value-index smoothness]} run]
  (into [(first run)]
        (mapcat (fn [[p q]]
                  (let [length (polyline/path-length (coord/line-path geometry [p q] true))]
                    (smooth-segment p q value-index smoothness
                                    (long (m/constrain (m/ceil (m// length smooth-step-pixels)) 2.0 max-smooth-steps))))))
        (partition 2 1 run)))

(defn- point-runs
  "Gives the drawn pixel runs of a row's range points.

  Each run of finite points is joined - by smooth curves with `smooth`, else straight - by chords, or by arcs with the geometry's `:arcs`.

  Parameters:

  - `geometry` (map or `nil`): the polar geometry; `nil` - cartesian.
  - `pts` (sequence): range points `[h v]`, `NaN` where missing.
  - `smooth` (map or `nil`): `{:value-index :smoothness}` (see [[smooth-segment]]); `nil` - straight.

  Returns a vector of runs, each a vector of drawn pixel points."
  [geometry pts smooth]
  (mapv (fn [run] (mapv curve/drawn-point (coord/line-path geometry (cond->> run smooth (smooth-run geometry smooth))
                                                          (:arcs geometry))))
        (polyline/runs (mapv first pts) (mapv second pts))))

(defn- axis-frame
  "Places a tick mark and its label relative to an axis.

  Right of a vertical axis, below a horizontal one (flipped plots); polar - beside a spoke (toward decreasing angles, as the plot's radial labels), or outward from a ring at the tick's angle (flipped).

  Parameters:

  - `geometry` (map or `nil`): the polar geometry; `nil` - cartesian.
  - `flipped?`: is the plot flipped?
  - `h` (number): the tick's first range coordinate (its angle in polar plots).

  Returns `{:normal [nx ny] :anchor a}`: the unit pixel direction from the axis toward the label and the label anchor."
  [geometry flipped? ^double h]
  (cond
    geometry (let [side (if flipped? h (m/- h m/HALF_PI))]
               {:normal [(m/sin side) (m/- (m/cos side))] :anchor (coord/outward-anchor side)})
    flipped? {:normal [0.0 1.0] :anchor [:center :top]}
    :else {:normal [1.0 0.0] :anchor [:left :middle]}))

(defn axis-ticks
  "Gives the ticks and labels of a variable's axis.

  Parameters:

  - `[lo hi]`: the axis domain in original units.
  - `length` (double): the axis length in pixels.
  - `spacing` (double): theme pixels between ticks.

  Returns a vector of `[tick label]`: at least two ticks inside the domain (see [[cljplot2.scale.position/ticks-inside]]); a single tick for a zero-length domain."
  [[^double lo ^double hi] ^double length ^double spacing]
  (if (m/== lo hi)
    (let [sc (s/scale :linear {:domain [(m/- lo 1.0) (m/+ hi 1.0)] :ticks 2})]
      [[lo (first (scale/tick-labels sc {:kind :linear} [lo]))]])
    (let [{sc :scale ticks :ticks} (scale/ticks-inside #(s/scale :linear {:domain [lo hi] :ticks %})
                                                       (scale/tick-hint length spacing)
                                                       #(m/<= lo (double %) hi))]
      (mapv vector ticks (scale/tick-labels sc {:kind :linear} ticks)))))

(defn- pixel
  "Gives the pixel of a range point.

  Parameters:

  - `geometry` (map or `nil`): the polar geometry; `nil` - cartesian.
  - `[h v]`: the range point.

  Returns `[x y]` (see [[cljplot2.scale.coord/to-pixel]])."
  [geometry [^double h ^double v]]
  (let [^fastmath.vector.Vec2 p (coord/to-pixel geometry h v)] [(.x p) (.y p)]))

(defn- axis-ends
  "Gives the range points of an axis's ends.

  Parameters:

  - `rp` (function): the range projection of a label and a scaled value.
  - `axis` (map): `:label`, `:domain` and the scaling (see [[variable-axes]]).

  Returns `[start end]` - its scaled `lo` and `hi`; `nil` when the scales can't place them."
  [rp {:keys [label domain] :as axis}]
  (let [ends [(rp label (scaled axis (first domain))) (rp label (scaled axis (second domain)))]]
    (when (every? #(m/valid-double? (double %)) (apply concat ends)) ends)))

(defn- axis-path
  "Gives the pixel points of an axis between two range points.

  Parameters:

  - `geometry` (map or `nil`): the polar geometry; `nil` - cartesian.
  - `start`, `end`: the range points.

  Returns the pixel points: a spoke, a straight line, or (variables on rings) an arc."
  [geometry start end]
  (coord/line-path geometry [start end] true))

(defn- axis-labels
  "Gives the tick labels of every axis to draw, without collisions across all axes.

  A label is kept when its box overlaps no label kept before it.

  Parameters:

  - `canvas`: the canvas (for measuring).
  - `placed` (sequence): per axis `{:ticks [{:label [content anchor x y]} ..]}`; a tick without a `:label` has none.
  - `env` (map): the drawing environment.

  Returns a vector of `[content anchor x y]`.

  See also [[cljplot2.graphics.measure/kept-box-indices]]."
  [canvas placed env]
  (let [labels (vec (for [{:keys [ticks]} placed, {:keys [label]} ticks :when label] label))]
    (mapv labels (measure/kept-box-indices (mapv (fn [[content anchor x y]] (label/box-at canvas content anchor x y env))
                                                 labels)))))

(def ^{:private true :const true :tag 'double} centre-tolerance
  "Pixels a radar tick may lie from its spoke's inner end and still count as a centre label (see [[without-repeated-centre-labels]])."
  0.5)

(defn- without-repeated-centre-labels
  "Leaves out the tick labels repeating a centre label of an earlier axis.

  Parameters:

  - `placed` (sequence): per axis `{:ticks [{:label .. :centre c} ..]}` in axis order; `c` - `[text radius]` of a tick at the axis's inner end of a radar spoke, else `nil`.

  Returns a vector of the axes, the ticks whose `:centre` an earlier axis has already had without their `:label` (their marks stay)."
  [placed]
  (first (reduce (fn [[kept seen] {:keys [ticks] :as axis}]
                   [(conj kept (assoc axis :ticks (mapv (fn [{:keys [centre] :as tick}] (if (and centre (seen centre)) (dissoc tick :label) tick)) ticks)))
                    (into seen (keep :centre) ticks)])
                 [[] #{}]
                 placed)))

(defn- draw-axes!
  "Draws each variable's axis: its line, ticks and tick labels in theme `[:axis :y]`.

  Parameters:

  - `canvas`: the panel canvas.
  - `axes` (sequence): the axes (see [[variable-axes]]).
  - `rp` (function): the range projection.
  - `geometry` (map or `nil`): the polar geometry.
  - `flipped?`: is the plot flipped?
  - `env` (map): the drawing environment (`:theme`).

  Returns `nil`; an axis the scales can't place is skipped; in a radar a label repeating an earlier spoke's at the inner end is drawn once (see [[without-repeated-centre-labels]]), colliding labels are dropped (see [[axis-labels]])."
  [canvas axes rp geometry flipped? env]
  (let [{:keys [line-color line-width tick-color ^double tick-length tick-width ^double label-gap tick-spacing]} (get-in env [:theme :axis :y])
        off (m/+ tick-length label-gap)
        spokes? (and geometry (not flipped?))
        placed (vec (for [{:keys [label domain] :as axis} axes
                          :let [[start end] (axis-ends rp axis)]
                          :when start
                          :let [path (axis-path geometry start end)]]
                      {:path path
                       :ticks (vec (for [[tick text] (axis-ticks domain (polyline/path-length path) (double tick-spacing))
                                         :let [tp (rp label (scaled axis (double tick)))
                                               [^double px ^double py] (pixel geometry tp)
                                               {[^double nx ^double ny] :normal anchor :anchor} (axis-frame geometry flipped? (first tp))
                                               ;; a radar spoke's range v is its radius in pixels
                                               r (double (second tp))]]
                                     {:centre (when (and spokes? (m/< (m/abs (m/- r (double (second start)))) centre-tolerance)) [text (m/round r)])
                                      :mark [[px py] [(m/+ px (m/* nx tick-length)) (m/+ py (m/* ny tick-length))]]
                                      :label [(axis-content/tick-label (:theme env) :y text) anchor
                                              (m/+ px (m/* nx off)) (m/+ py (m/* ny off))]}))}))]
    (doseq [{:keys [path ticks]} placed]
      (polyline/stroke-runs! canvas [path] {:color line-color :line-width line-width})
      (doseq [{:keys [mark]} ticks]
        (polyline/stroke-runs! canvas [mark] {:color tick-color :line-width tick-width})))
    (doseq [[content anchor x y] (axis-labels canvas (without-repeated-centre-labels placed) env)]
      (label/draw-at! canvas content anchor x y env))))

(defn- draw-web!
  "Draws the radar web: lines joining the axes at equal fractions of their length, in theme `[:panel :grid :y]`.

  The fractions are the ticks of 0..1 over the longest axis; closed polygons when `closes?`, else open lines (partial circles, rings).

  Parameters:

  - `canvas`: the panel canvas.
  - `axes` (sequence): the axes (see [[variable-axes]]).
  - `rp` (function): the range projection.
  - `geometry` (map): the polar geometry.
  - `closes?`: do the rows close?
  - `env` (map): the drawing environment (`:theme`).

  Returns `nil`; nothing is drawn with a `:none` grid line, an axis the scales can't place, or zero-length axes."
  [canvas axes rp geometry closes? env]
  (let [line (get-in env [:theme :panel :grid :y])
        placed (keep #(when-let [axis-end-points (axis-ends rp %)] [(:label %) axis-end-points]) axes)
        length (double (reduce m/max 0.0 (map (fn [[_ [start end]]] (polyline/path-length (axis-path geometry start end))) placed)))
        ends (mapv (fn [{:keys [label domain] :as axis}] [label (scaled axis (first domain)) (scaled axis (second domain))]) axes)]
    (when (and (not (theme/none? line)) (m/== (count placed) (count axes)) (m/pos? length))
      (doseq [[^double f _] (axis-ticks [0.0 1.0] length (double (get-in env [:theme :axis :y :tick-spacing])))
              :let [pts (mapv (fn [[label ^double lo ^double hi]] (rp label (m/+ lo (m/* f (m/- hi lo))))) ends)]]
        (polyline/stroke-runs! canvas (point-runs geometry (closed geometry closes? pts) nil) line)))))

(defn draw
  "Draws one panel: the radar web (polar, while the plot's y axis is hidden), every row's line (filled radar polygons first), then the variable axes.

  Parameters:

  - `chart` (map): the chart value - `:options` (`:curve`, `:smoothness`, `:axes`).
  - `computed` (map): the panel's stats - `:lines`, `:axes`, `:style` (see [[compute]]).
  - `canvas`: the panel canvas.
  - `_box`: the panel box (unused - the scales place the lines).
  - `scales` (map): the finalized scales.
  - `env` (map): the drawing environment (`:theme`).

  Returns `nil`; a row whose group style is `nil` (an aesthetic outside its scale) isn't drawn, a row with a missing value isn't filled."
  [{:keys [options]} {:keys [lines axes style]} canvas _box scales env]
  (let [geometry (scale/coord-geometry scales)
        flipped? (scale/flipped? scales)
        closes? (and (some? geometry) (not flipped?) (coord/full-circle? geometry))
        rp (scale/range-projection scales)
        smooth (when (= :smooth (:curve options))
                 {:value-index (if flipped? 0 1) :smoothness (double (data/option options :smoothness 1.0))})
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
    (when (data/option options :axes true)
      (draw-axes! canvas axes rp geometry flipped? env))))

(defn legend
  "Draws the key glyph: a line through the middle of the key box; with a radar fill a filled square outlined in the line style.

  The style is the computed one (env `:chart-stats`), so a theme fill shows only where it is drawn.

  Parameters:

  - `chart` (map): the chart value (its style options, used without computed stats).
  - `attributes` (map): the key's attributes - `:color`, `:dash`, `:pattern`.
  - `canvas`: the legend canvas.
  - `box` (map): the key box.
  - `env` (map): the legend environment (`:theme`, `:chart-stats`).

  Returns `nil`.

  See also [[cljplot2.graphics.polyline/key-glyph!]]."
  [chart attributes canvas box env]
  (let [st (merge (theme/key-line-style (:theme env) (or (get-in env [:chart-stats :style]) (theme/resolved-style (:theme env) chart)))
                  (select-keys attributes [:color :dash :pattern]))
        area (polyline/area-color st false)]
    (polyline/key-glyph! canvas box st area (draw/pattern-fill st area))))
