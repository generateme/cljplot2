(ns cljplot2.chart.density
  "Density: a kernel density estimate (fastmath) of `:x` per panel and group, drawn as a line.

  Options (style defaults from theme `[:chart :density]`):

  * `:kernel` - fastmath KDE kernel keyword (default `:gaussian`)
  * `:bandwidth` - a number or a fastmath selector keyword (default `:nrd0`, as R)
  * `:n` - evaluation points (default 512), `:cut` - the curve spans `[min - cut*h, max + cut*h]` (default 3, as R)
  * `:position` - grouped curves (see [[cljplot2.data.position]]): `:overlay` (default), `:stack` and `:normalize` (every
    group on one common range, stacked; normalize - summing to 1, y title \"Proportion\"; areas filled with `:fill` or
    the group colour faded by `:alpha`), streamgraphs `:silhouette` and `:wiggle` (stacked around a moving
    baseline, see [[cljplot2.data.position]]), `:mirror` (exactly two groups, the second drawn downward; y tick labels show
    magnitudes); `:symmetric` - mirror only: both halves on one y range `[-M M]`; `:zero-line` - mirror only: draw
    the y = 0 line (default true; `:zero-line-color`, `:zero-line-width`)
  * `:color`, `:line-width`, `:dash` (line), `:fill` (area under the curve: a colour, `:color` - the curve's (group) colour
    faded by `:alpha`: its own alpha times `:alpha` - or `:none`, default), `:alpha` (0..1, default 0.6; also stacked
    areas without `:fill`)
  * `:padding {:x [lo hi] :y [lo hi]}` - default `{:x [0.05 0.05] :y [0 0.05]}`

  Groups (see [[cljplot2.data.group]]) come from ordinal aesthetics (`:color` colors the line) and `:group`; a group with
  fewer than 2 distinct values has no curve and is counted in `:dropped {:group n}`.

  Ridgelines - `:y` mapped to categorical levels or on a `:bands` scale (see [[cljplot2.data.strip/ridges?]]; a
  numeric `:y` otherwise is ignored): one curve per level and group, drawn from its band's centre toward the next
  level, all on one height scale - the tallest curve is `:overlap` (default 0.9) band steps high; the y scale leaves room for the outer ridges (bands `:padding-out` / `:align-outer` unless the
  plot sets them). Areas are filled (`:fill`, else the group colour faded by `:alpha`) and drawn from the farthest level
  on, so nearer ridges cover farther ones. `:position` `:overlay` (default) or `:mirror` (two groups, the second
  growing away from the next level); `:fill-gradient` (ridges only) - fill colour: a clojure2d gradient (keyword
  preset, colour list or function of `t` in `[0 1]`) along `:fill-gradient-axis` - `:x` (default, over the x scale's
  domain) or `:y` (from each ridge's baseline to the top of the tallest curve).

  Functions: [[compute]], [[draw]], [[legend]] - the chart contract; [[schemas]] - its style, options and mapping."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.chart.strip :as strip-chart]
            [cljplot2.chart.common :as common]
            [cljplot2.data.rows :as data]
            [cljplot2.data.group :as group]
            [cljplot2.data.kde :as kde]
            [cljplot2.data.position :as position]
            [cljplot2.data.strip :as strip]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries
  "Map entries of the density style (theme `[:chart :density]`), also its options."
  (into (conj polyline/style-entries
              (schema/optional :fill {:cljplot2/section :style :description "area under the curve: a colour, :color (the curve's colour, its own alpha times :alpha) or :none; stacked positions and ridgelines fill with the curve's colour unless set"}
                               [:or [:= :color] schema/color])
              (schema/optional :alpha {:cljplot2/section :style :description "factor of the curve colour's alpha filling areas in the curve's colour"} schema/fraction)
              (schema/optional :zero-line-color {:cljplot2/section :style :description "colour of the y = 0 line drawn with :position :mirror"} schema/color)
              (schema/optional :zero-line-width {:cljplot2/section :style :description "width in pixels of the y = 0 line drawn with :position :mirror"} schema/non-neg))
        draw/pattern-style-entries))

(def ^:private positions
  "Positions of grouped curves: [[cljplot2.data.position/positions]] without `:dodge`, plus the streamgraphs."
  (into (disj position/positions :dodge) position/stream-positions))

(def ^:private stacked-positions
  "Positions that stack the curves, filling their areas."
  (into #{:stack :normalize} position/stream-positions))

(def schemas
  "Schemas of the density chart: `:style` (theme `[:chart :density]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (schema/with-rules
             (-> [:map {:closed true}]
                 (into style-entries)
                 (into data/common-options)
                 (into theme/theme-style-entries)
                 (conj (schema/optional :kernel {:cljplot2/section :estimate :description "fastmath KDE kernel" :default :gaussian} keyword?)
                       (schema/optional :bandwidth {:cljplot2/section :estimate :description "kernel bandwidth in data units, or a fastmath bandwidth selector"
                                                    :default :nrd0}
                                        [:or schema/pos keyword?])
                       (schema/nilable :n {:cljplot2/section :estimate :description "number of evaluation points of each curve" :default 512} [:and :int [:>= 2]])
                       (schema/nilable :cut {:cljplot2/section :estimate :description "how many bandwidths the curve extends past the extreme values on each side"
                                             :default 3}
                                       schema/non-neg)
                       (schema/optional :position {:cljplot2/section :position :description "grouped curves: :overlay, :stack, :normalize (stacked, summing to 1), streamgraphs :silhouette and :wiggle, or :mirror (two groups, the second drawn downward); ridgelines take :overlay or :mirror"
                                                   :default :overlay}
                                        (into [:enum] (sort positions)))
                       (schema/nilable :symmetric {:cljplot2/section :position :description "with :position :mirror, tie both halves to one y range [-M M], M the tallest curve over all panels"
                                                   :default false}
                                       schema/bool)
                       (schema/nilable :zero-line {:cljplot2/section :position :description "with :position :mirror, draw the y = 0 line over the curves"
                                                   :default true}
                                       schema/bool)
                       (schema/optional :fill-gradient {:cljplot2/section :style :description "ridgelines only: fill colour along :fill-gradient-axis, a clojure2d gradient (preset keyword, colour list or function of t in [0 1]) (wins over :fill)"}
                                        polyline/fill-gradient-schema)
                       (schema/optional :fill-gradient-axis {:cljplot2/section :style :description "axis of the :fill-gradient: :x (over the x scale's domain) or :y (from each ridge's baseline to the top of the tallest curve)"
                                                             :cljplot2/default-doc ":x; only with ridgelines and :fill-gradient"}
                                        [:enum :x :y])
                       (schema/optional :padding {:cljplot2/section :position :description "padding of the data span per position axis, {:x [lo hi] :y [lo hi]} fractions of the span"
                                                  :cljplot2/default-doc "{:x [0.05 0.05] :y [0 0.05]} (curves stand on the panel's bottom edge); y [0.05 0.05] with :mirror, :silhouette and :wiggle"}
                                        data/position-padding))
                 (into strip/ridge-options))
             common/fill-gradient-rule)
   :mapping [:map
             [:x {:description "values whose density is estimated (data units)"} schema/accessor]
             (schema/nilable :y {:description "ridgeline levels: categorical or on a :bands scale, one curve per level (a numeric :y otherwise is ignored)"}
                             schema/accessor)
             (schema/nilable :color {:description "groups, each a curve in its own colour"} schema/accessor)
             (schema/nilable :dash {:description "groups, each a curve with its own dash style"} schema/accessor)
             (schema/nilable :pattern {:description "groups, each area with its own fill pattern"} schema/accessor)
             (schema/nilable :group {:description "groups without a visual aesthetic, each its own curve"} schema/accessor)]})

(def ^{:const true :private true :tag 'long} default-points
  "Evaluation points of each curve when `:n` isn't given."
  512)

(defn- panel-curves
  "Gives the density curves of a panel's groups for a position.

  Overlay and mirror: each group on its own range (mirror: the second group of `group-keys` negated). Stacked positions (`:stack`, `:normalize`, `:silhouette`, `:wiggle`): every group on one common range of the panel's curves, `:y0` / `:y` from [[cljplot2.data.position/stack-values]].

  Parameters:

  - `gs` (sequence of maps): the panel's groups - `:key`, `:aesthetics`.
  - `group-values` (sequence of maps): per group its valid values `{:values [..]}`.
  - `group-keys` (sequence): every group key of the plot, in order.
  - `position` (keyword): the chart's `:position`.
  - `options` (map): the chart's options - `:kernel`, `:bandwidth`, `:cut`, `:n` (default 512).

  Returns a vector of curves `{:x [..] :y [..] :aesthetics}` (stacked - also `:y0 [..]`); a group with fewer than 2 distinct values has none."
  [gs group-values group-keys position options]
  (let [n (long (or (:n options) default-points))
        estimates (vec (keep (fn [[g {:keys [values]}]]
                               (when (m/>= (count (distinct values)) 2)
                                 (assoc (kde/estimate values options) :group g)))
                             (map vector gs group-values)))]
    (case position
      (:stack :normalize :silhouette :wiggle) (if (empty? estimates)
                                                []
                                                (let [xs (kde/grid-points (v/mn (map :lo estimates)) (v/mx (map :hi estimates)) n)
                                                      curves (mapv #(kde/curve % xs) estimates)
                                                      ys (position/stack-values position (mapv :y curves))]
                                                  (mapv (fn [{:keys [group]} c y01]
                                                          (assoc c :y0 (mapv first y01) :y (mapv second y01) :aesthetics (:aesthetics group)))
                                                        estimates curves ys)))
      (mapv (fn [{:keys [group lo hi] :as e}]
              (let [c (kde/curve e (kde/grid-points lo hi n))
                    down? (and (= position :mirror) (= (:key group) (second group-keys)))]
                (cond-> (assoc c :aesthetics (:aesthetics group))
                  down? (update :y #(mapv m/- %)))))
            estimates))))

(defn- ridge-compute
  "Computes the density stats of ridgelines (a categorical or bands `:y`).

  Parameters:

  - `chart` (map): the chart value - `:options` (`:overlap` default 0.9, `:position`, `:fill-gradient`, `:fill-gradient-axis` default `:x`, the density options, style options).
  - `chart-split` (map): the chart's rows split per panel.
  - `env` (map): the compute environment - `:theme`, `:plot-scales`, `:flip` ...

  Returns the chart stats with `:ridges true`: per panel `{:ridges [item ...] :top max-density :overlap o :style st :gradient g :gradient-axis a}`, items (see [[cljplot2.data.strip/ridge-split]]) with `:xs` / `:ds` - the curve; one `:top` over all panels; `:dropped` `:group` - items with fewer than 2 distinct values."
  [{:keys [options] :as chart} chart-split {:keys [theme] :as env}]
  (let [{:keys [position items panels dropped]} (strip/ridge-split chart-split options env)
        n (long (or (:n options) default-points))
        curve? #(m/>= (count (distinct (:values %))) 2)
        no-curve (count (remove curve? (mapcat val items)))
        curved (update-vals items (fn [its]
                                    (mapv (fn [it] (let [e (kde/estimate (:values it) options)
                                                         {:keys [x y]} (kde/curve e (kde/grid-points (:lo e) (:hi e) n))]
                                                     (assoc it :xs x :ds y)))
                                          (filter curve? its))))
        top (double (reduce m/max 0.0 (mapcat :ds (mapcat val curved))))
        overlap (double (data/option options :overlap strip/default-ridge-overlap))
        gradient (some-> (:fill-gradient options) polyline/gradient-fn)
        ridge-style (theme/resolved-style theme chart)]
    (merge
     (data/padding-stats {:x data/axis-padding} {} options)
     {:data (update-vals curved (fn [its] {:ridges its :top top :overlap overlap :style ridge-style
                                           :gradient gradient :gradient-axis (data/option options :fill-gradient-axis :x)}))
      :ridges true
      :extents (merge-with merge (group/aesthetic-extents panels (:plot-scales env))
                           (strip/extents :band curved (fn [{:keys [xs]}] [(first xs) (peek xs)])))
      :scale-defaults (strip/ridge-scale-defaults overlap (= :mirror position) (:flip env) (get-in env [:plot-scales :y]))
      :na (group/na-dims panels)
      :dropped (assoc dropped :group no-curve)})))

(defn- flat-compute
  "Computes the density stats without ridgelines (no categorical or bands `:y`).

  Parameters:

  - `chart` (map): the chart value - `:options` (`:position` default `:overlay`, `:symmetric`, `:zero-line`, `:padding`, the density options, style options).
  - `chart-split` (map): `:groups` (`{panel rows}`) and the merged `:mapping`.
  - `env` (map): the compute environment - `:theme`, `:plot-scales` ...

  Returns the chart stats: per panel `{:curves [..] :style st :zero-line b}` (see [[panel-curves]]); y extents from 0 (mirror with `:symmetric` - one range `[-M M]` over all panels); `:dropped` `:x` - rows without a finite x, `:group` - groups without a curve; y titled `Density` (`:normalize` - `Proportion`); mirror ticks show magnitudes.

  Throws `ex-info {:key :position}` for `:mirror` without exactly two groups, `{:key :mapping}` when rows exist but none has a finite x."
  [{:keys [options] :as chart} {:keys [groups mapping]} {:keys [theme] :as env}]
  (let [st (theme/resolved-style theme chart)
        position (data/option options :position :overlay)
        {:keys [panels dims dropped]} (group/split groups mapping env)
        group-keys (group/panel-group-keys panels dims (:plot-scales env))
        x-of (data/row-accessor :x (:x mapping))
        ;; per group: valid x values and the count of invalid ones
        xs (group/finite-values panels x-of)
        x-results (mapcat identity (vals xs))
        row-count (group/row-count panels)
        x-dropped (long (reduce m/long-add 0 (map :dropped x-results)))
        no-curve (count (filter #(m/< (count (distinct (:values %))) 2) x-results))
        _ (when (and (= position :mirror) (m/not== 2 (count group-keys)) (seq x-results))
            (schema/fail! "Density :position :mirror needs exactly two groups." {:key :position :groups (count group-keys)}))
        centred? (#{:mirror :silhouette :wiggle} position)
        data (into {} (map (fn [[panel gs]]
                             [panel {:curves (panel-curves gs (xs panel) group-keys position options) :style st
                                     :zero-line (and (= position :mirror) (data/option options :zero-line true))}]))
                   panels)
        curve-ys (fn [curves] (concat (mapcat :y curves) (mapcat :y0 curves)))
        [lo hi] (position/symmetric-extent (curve-ys (mapcat :curves (vals data))))]
    (when (and (m/pos? row-count) (m/== row-count x-dropped))
      (data/fail-mapping! data/no-valid-values :x {:rows row-count}))
    (merge
     (data/padding-stats {:x data/axis-padding :y (if centred? data/axis-padding [0.0 0.05])} (when-not centred? {:y [0.0 nil]}) options)
     {:data data
      :extents (merge-with merge (group/aesthetic-extents panels (:plot-scales env))
                           (into {} (keep (fn [[panel {:keys [curves]}]]
                                            (when (seq curves)
                                              [panel {:x (data/extent :x (mapcat :x curves))
                                                      :y (if (and (= position :mirror) (:symmetric options))
                                                           {:kind :numeric :min lo :max hi}
                                                           {:kind :numeric
                                                            :min (double (reduce m/min 0.0 (curve-ys curves)))
                                                            :max (double (reduce m/max 0.0 (curve-ys curves)))})}])))
                                 data))
      :na (group/na-dims panels)
      :dropped (assoc dropped :x x-dropped :group no-curve)
      :axis-titles {:y (if (= position :normalize) "Proportion" "Density")}
      :tick-format (when (= position :mirror) {:y :absolute})})))

(defn compute
  "Computes the density chart's stats.

  Parameters:

  - `chart` (map): the chart value - `:options`.
  - `chart-split` (map): the chart's rows split per panel, with the merged `:mapping`.
  - `env` (map): the compute environment.

  Returns ridgeline stats for a categorical or bands `:y` ([[ridge-compute]]), else per panel `{:curves [{:x [..] :y [..] :aesthetics}] :style st}` with extents, padding, `:na`, `:dropped` and `:axis-titles` ([[flat-compute]]).

  Throws `ex-info {:key :fill-gradient}` / `{:key :fill-gradient-axis}` for those options without ridgelines."
  [{:keys [options] :as chart} chart-split env]
  (cond
    (strip/ridges? chart-split env) (ridge-compute chart chart-split env)
    (:fill-gradient options) (schema/fail! "Density :fill-gradient is for ridgelines (a categorical or bands :y)."
                                           {:key :fill-gradient :value (:fill-gradient options)})
    (:fill-gradient-axis options) (schema/fail! "Density :fill-gradient-axis is for ridgelines with :fill-gradient."
                                                {:key :fill-gradient-axis :value (:fill-gradient-axis options)})
    :else (flat-compute chart chart-split env)))

(defn- fill-gradient-quads!
  "Fills between two paired point runs quad by quad, each quad in the gradient's colour at its middle position.

  Parameters:

  - `canvas`: the clojure2d canvas.
  - `upper`, `lower` (vectors of `[x y]`): the paired pixel runs (the curve and its base).
  - `ps` (vector of numbers): one position per point - the gradient's axis values.
  - `gradient` (function): of `t` in `[0 1]` giving a colour.
  - `[p0 p1]`: the positions at `t` 0 and 1 (`t` 0.5 for an empty range).

  A quad with a non-finite corner is skipped."
  [canvas upper lower ps gradient [^double p0 ^double p1]]
  (let [span (m/- p1 p0)]
    (c2d/set-stroke-custom canvas {:size 1.0 :join :miter})
    (doseq [^long i (range (m/long-dec (count upper)))
            :let [a (upper i)
                  b (upper (m/long-inc i))
                  c (lower (m/long-inc i))
                  d (lower i)]
            :when (and (data/finite-point? a) (data/finite-point? b) (data/finite-point? c) (data/finite-point? d))]
      (let [t (if (m/pos? span) (m// (m/- (m/* 0.5 (m/+ (double (ps i)) (double (ps (m/long-inc i))))) p0) span) 0.5)
            quad [a b c d]]
        (c2d/set-color canvas (gradient (m/constrain t 0.0 1.0)))
        (c2d/path canvas quad true false)
        (c2d/path canvas quad true true)))))

(defn- pixel-pair
  "Gives a pixel point as a pair `[x y]`.

  Parameters:

  - `p` (`Vec2`): the pixel point."
  [^Vec2 p]
  [(.x p) (.y p)])

(defn- coordinates
  "Gives the coordinate vectors of pixel points.

  Parameters:

  - `points` (sequence of `Vec2`): pixel points.

  Returns `[xs ys]`, two vectors."
  [points]
  [(mapv #(.x ^Vec2 %) points) (mapv #(.y ^Vec2 %) points)])

(defn- draw-ridges!
  "Draws the ridgelines of one panel, farthest level first: the area (a gradient, `:fill` or the colour faded by `:alpha`), then the line.

  Parameters:

  - `computed` (map): the panel's ridgeline stats - `:ridges`, `:top`, `:overlap`, `:style`, `:gradient`, `:gradient-axis`.
  - `canvas`: the clojure2d canvas.
  - `box` (map): the panel box.
  - `scales` (map): finalized scales.
  - `env` (map): the draw environment - `:theme`.

  Nothing is drawn when every density is 0; a group outside its scale's domain is skipped.

  See also [[cljplot2.data.strip/ridge-point]], [[cljplot2.data.strip/ridge-order]]."
  [{:keys [ridges ^double top ^double overlap style gradient gradient-axis]} canvas box scales env]
  (let [pl (strip-chart/placement box scales {} :band)
        point (strip/ridge-point pl overlap)]
    (when (m/pos? top)
      (doseq [{:keys [level slot xs ds aesthetics]} (strip/ridge-order pl ridges)
              :let [st (group/style style aesthetics scales (:theme env))]
              :when st]
        (let [side (strip/slot-side slot)
              curve (mapv (fn [x d] (point x level side (m// (double d) top))) xs ds)
              base (mapv (fn [x] (point x level side 0.0)) xs)
              [px py] (coordinates curve)
              [bx by] (coordinates base)]
          (cond
            (and gradient (= :y gradient-axis))
            (let [x0 (first xs)
                  polygon (into (mapv pixel-pair curve) (rseq (mapv pixel-pair base)))]
              (polyline/fill-gradient-paint! canvas [polygon] gradient
                                             (pixel-pair (point x0 level side 0.0)) (pixel-pair (point x0 level side 1.0)))
              (draw/fill-with-pattern! canvas (draw/pattern-fill st (gradient 0.5)) #(c2d/path % polygon true false)))
            gradient
            (let [upper (mapv pixel-pair curve)
                  lower (mapv pixel-pair base)
                  polygon (into upper (rseq lower))]
              (fill-gradient-quads! canvas upper lower xs gradient (:domain (:x scales)))
              (draw/fill-with-pattern! canvas (draw/pattern-fill st (gradient 0.5)) #(c2d/path % polygon true false)))
            :else
            (let [fill (polyline/area-color st true)]
              (polyline/fill-under! canvas px py bx by fill (draw/pattern-fill st fill))))
          (polyline/draw! canvas px py st))))))

(defn draw
  "Draws the curves of one panel.

  Parameters:

  - `_chart` (map): the chart value (unused).
  - `computed` (map): the panel's stats - `:curves`, `:style`, `:zero-line`, or ridgeline stats (`:ridges`).
  - `canvas`: the clojure2d canvas.
  - `box` (map): the panel box.
  - `scales` (map): finalized scales.
  - `env` (map): the draw environment - `:theme`.

  Draws every group's area (under the curve to y = 0 with `:fill`; stacked curves - between `:y0` and `:y`, `:fill` or the group colour faded by `:alpha`), then every curve's line, then - mirror with `:zero-line` (default on) - the y = 0 line on top (curve tails along y = 0 don't hide it); ridgelines by [[draw-ridges!]] (none without an x scale, see [[cljplot2.chart.strip/placeable?]]). A group outside its scale's domain is skipped."
  [_chart {:keys [curves style zero-line ridges] :as computed} canvas box scales env]
  (if ridges
    (when (strip-chart/placeable? scales) (draw-ridges! computed canvas box scales env))
    (when (seq curves)
      (let [projection (scale/projection scales)
            drawn (vec (keep (fn [{:keys [x y y0 aesthetics]}]
                               (when-let [st (group/style style aesthetics scales (:theme env))]
                                 {:st st :stacked (some? y0)
                                  :curve (coordinates (mapv projection x y))
                                  :base (coordinates (mapv projection x (or y0 (repeat 0.0))))}))
                             curves))]
        (doseq [{:keys [st stacked curve base]} drawn
                :let [[px py] curve
                      [bx by] base]]
          (if stacked
            (let [fill (polyline/area-color st true)] (polyline/fill-between! canvas px py bx by fill (draw/pattern-fill st fill)))
            (let [fill (polyline/area-color st false)] (polyline/fill-under! canvas px py bx by fill (draw/pattern-fill st fill)))))
        (doseq [{:keys [st curve]} drawn
                :let [[px py] curve]]
          (polyline/draw! canvas px py st))
        (when zero-line
          (draw/zero-line! canvas box scales style))))))

(defn legend
  "Draws the density chart's key glyph in a legend key.

  A horizontal line through the middle of the key; when the chart fills areas (`:fill`, stacked positions, ridgelines) a filled square in the area colour outlined in the line colour.

  Parameters:

  - `chart` (map): the chart value - `:options` `:position`.
  - `attributes` (map): the key's mapped style attributes - `:color`, `:dash`, `:pattern` are used.
  - `canvas`: the clojure2d canvas.
  - `box` (map): the key box in pixels.
  - `env` (map): the legend environment - `:theme`, `:chart-stats` (`:ridges`).

  See also [[cljplot2.graphics.polyline/key-glyph!]]."
  [{:keys [options] :as chart} attributes canvas box env]
  (let [st (theme/key-style env chart attributes [:color :dash :pattern])
        area (polyline/area-color st (or (contains? stacked-positions (:position options)) (:ridges (:chart-stats env))))]
    (polyline/key-glyph! canvas box st area (draw/pattern-fill st area))))
