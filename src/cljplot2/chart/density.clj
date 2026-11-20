(ns cljplot2.chart.density
  "Density: a kernel density estimate (fastmath) of `:x` per panel and group, drawn as a line.

  Options (style defaults from theme `[:chart :density]`):

  * `:kernel` - fastmath KDE kernel keyword (default `:gaussian`)
  * `:bandwidth` - a number or a fastmath selector keyword (default `:nrd0`, as R)
  * `:n` - evaluation points (default 512), `:cut` - the curve spans `[min - cut*h, max + cut*h]` (default 3, as R)
  * `:position` - grouped curves (see [[cljplot2.data.position]]): `:overlay` (default), `:stack` and `:normalize` (every
    group on one common range, stacked; normalize - summing to 1, y title \"Proportion\"; areas filled with `:fill` or
    the group colour with opacity `:alpha`), streamgraphs `:silhouette` and `:wiggle` (stacked around a moving
    baseline, see [[cljplot2.data.position]]), `:mirror` (exactly two groups, the second drawn downward; y tick labels show
    magnitudes); `:symmetric` - mirror only: both halves on one y range `[-M M]`; `:zero-line` - mirror only: draw
    the y = 0 line (default true; `:zero-line-color`, `:zero-line-width`)
  * `:color`, `:width`, `:dash` (line), `:fill` (area under the curve: a colour, `:color` - the curve's (group) colour
    with opacity `:alpha`, or `:none`, default), `:alpha` (0..1, default 0.6; also stacked areas without `:fill`)
  * `:padding {:x [lo hi] :y [lo hi]}` - default `{:x [0.05 0.05] :y [0 0.05]}`

  Groups (see [[cljplot2.data.group]]) come from ordinal aesthetics (`:color` colors the line) and `:group`; a group with
  fewer than 2 distinct values has no curve and is counted in `:dropped {:group n}`.

  Ridgelines - `:y` mapped to categorical levels or on a `:bands` scale (see [[cljplot2.data.strip/ridges?]]; a
  numeric `:y` otherwise is ignored): one curve per level and group, drawn from its band's centre toward the next
  level, all on one height scale - the tallest curve is `:overlap` (default 0.9) band steps high; the y scale leaves room for the outer ridges (bands `:padding-out` / `:align-outer` unless the
  plot sets them). Areas are filled (`:fill`, else the group colour with `:alpha`) and drawn from the farthest level
  on, so nearer ridges cover farther ones. `:position` `:overlay` (default) or `:mirror` (two groups, the second
  growing away from the next level); `:fill-gradient` (ridges only) - fill colour: a clojure2d gradient (keyword
  preset, colour list or function of `t` in `[0 1]`) along `:fill-gradient-axis` - `:x` (default, over the x scale's
  domain) or `:y` (from each ridge's baseline to the top of the tallest curve)."
  (:require [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
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
  (into (conj polyline/style-entries
              (schema/optional :fill {:description "area under the curve: a colour, :color (the curve's colour with opacity :alpha) or :none; stacked positions and ridgelines fill with the curve's colour unless set"}
                               [:or [:= :color] schema/color])
              (schema/optional :alpha {:description "fill opacity of areas in the curve's colour"} schema/fraction)
              (schema/optional :zero-line-color {:description "colour of the y = 0 line drawn with :position :mirror"} schema/color)
              (schema/optional :zero-line-width {:description "width in pixels of the y = 0 line drawn with :position :mirror"} schema/non-neg))
        draw/pattern-style-entries))

(def ^:private positions (into (disj position/positions :dodge) position/stream-positions))

(def ^:private stacked-positions (into #{:stack :normalize} position/stream-positions))

(def schemas
  "Schemas of the density chart: `:style` (theme `[:chart :density]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (conj (schema/optional :kernel {:description "fastmath KDE kernel" :default :gaussian} keyword?)
                      (schema/optional :bandwidth {:description "kernel bandwidth in data units, or a fastmath bandwidth selector"
                                                   :default :nrd0}
                                       [:or schema/pos keyword?])
                      (schema/nilable :n {:description "number of evaluation points of each curve" :default 512} [:and :int [:>= 2]])
                      (schema/nilable :cut {:description "how many bandwidths the curve extends past the extreme values on each side"
                                            :default 3}
                                      schema/non-neg)
                      (schema/optional :position {:description "grouped curves: :overlay, :stack, :normalize (stacked, summing to 1), streamgraphs :silhouette and :wiggle, or :mirror (two groups, the second drawn downward); ridgelines take :overlay or :mirror"
                                                  :default :overlay}
                                       (into [:enum] (sort positions)))
                      (schema/nilable :symmetric {:description "with :position :mirror, tie both halves to one y range [-M M], M the tallest curve over all panels"
                                                  :default false}
                                      schema/bool)
                      (schema/nilable :zero-line {:description "with :position :mirror, draw the y = 0 line over the curves"
                                                  :default true}
                                      schema/bool)
                      (schema/optional :fill-gradient {:description "ridgelines only: fill colour along :fill-gradient-axis, a clojure2d gradient (preset keyword, colour list or function of t in [0 1]) (wins over :fill)"}
                                       polyline/fill-gradient-schema)
                      (schema/optional :fill-gradient-axis {:description "axis of the :fill-gradient: :x (over the x scale's domain) or :y (from each ridge's baseline to the top of the tallest curve)"
                                                            :cljplot2/default-doc ":x; only with ridgelines and :fill-gradient"}
                                       [:enum :x :y])
                      (schema/optional :padding {:description "padding of the data span per position axis, {:x [lo hi] :y [lo hi]} fractions of the span"
                                                 :cljplot2/default-doc "{:x [0.05 0.05] :y [0 0.05]} (curves stand on the panel's bottom edge); y [0.05 0.05] with :mirror, :silhouette and :wiggle"}
                                       data/position-padding))
                (into strip/ridge-options))
   :mapping [:map
             [:x {:description "values whose density is estimated (data units)"} schema/accessor]
             (schema/nilable :y {:description "ridgeline levels: categorical or on a :bands scale, one curve per level (a numeric :y otherwise is ignored)"}
                             schema/accessor)
             (schema/nilable :color {:description "groups, each a curve in its own colour"} schema/accessor)
             (schema/nilable :dash {:description "groups, each a curve with its own dash style"} schema/accessor)
             (schema/nilable :pattern {:description "groups, each area with its own fill pattern"} schema/accessor)
             (schema/nilable :group {:description "groups without a visual aesthetic, each its own curve"} schema/accessor)]})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :density]) (select-keys options [:color :width :dash :fill :alpha :zero-line-color :zero-line-width :pattern
                                                                 :pattern-color])))

(defn- panel-curves
  "Curves of a panel's groups `gs` (with their `group-values`) for `position`; `group-keys` - all groups in order.
  Overlay and mirror: each group on its own range (mirror: the second group of `group-keys` negated). Stack and fill:
  every group on one common range of the panel's curves, `:y0` / `:y` from
  [[cljplot2.data.position/stack-values]]."
  [gs group-values group-keys position options]
  (let [n (long (or (:n options) 512))
        estimates (vec (keep (fn [[g {:keys [values]}]]
                               (when (>= (count (distinct values)) 2)
                                 (assoc (kde/estimate values options) :group g)))
                             (map vector gs group-values)))]
    (case position
      (:stack :normalize :silhouette :wiggle) (if (empty? estimates)
                       []
                       (let [xs (kde/grid-points (reduce min (map :lo estimates)) (reduce max (map :hi estimates)) n)
                             curves (mapv #(kde/curve % xs) estimates)
                             ys (position/stack-values position (mapv :y curves))]
                         (mapv (fn [{:keys [group]} c y01]
                                 (assoc c :y0 (mapv first y01) :y (mapv second y01) :aesthetics (:aesthetics group)))
                               estimates curves ys)))
      (mapv (fn [{:keys [group lo hi] :as e}]
              (let [c (kde/curve e (kde/grid-points lo hi n))
                    down? (and (= position :mirror) (= (:key group) (second group-keys)))]
                (cond-> (assoc c :aesthetics (:aesthetics group))
                  down? (update :y #(mapv - %)))))
            estimates))))

(defn- ridge-compute
  "Ridgeline stats (categorical or bands `:y`): per panel `{:ridges [item ...] :top max-density :overlap o :style st :gradient g}`,
  items (see [[cljplot2.data.strip/ridge-split]]) with `:xs` / `:ds` - the curve; one `:top` over all panels."
  [options chart-split {:keys [theme] :as env}]
  (when (and (:fill-gradient-axis options) (nil? (:fill-gradient options)))
    (schema/fail! ":fill-gradient-axis needs :fill-gradient." {:key :fill-gradient-axis :value (:fill-gradient-axis options)}))
  (let [{:keys [position items panels dropped]} (strip/ridge-split chart-split options env)
        n (long (or (:n options) 512))
        curve? #(>= (count (distinct (:values %))) 2)
        no-curve (count (remove curve? (mapcat val items)))
        items (update-vals items (fn [its]
                                   (mapv (fn [it] (let [e (kde/estimate (:values it) options)
                                                        {:keys [x y]} (kde/curve e (kde/grid-points (:lo e) (:hi e) n))]
                                                    (assoc it :xs x :ds y)))
                                         (filter curve? its))))
        top (double (reduce max 0.0 (mapcat :ds (mapcat val items))))
        overlap (double (get options :overlap 0.9))
        gradient (some-> (:fill-gradient options) polyline/gradient-fn)]
    {:data (update-vals items (fn [its] {:ridges its :top top :overlap overlap :style (style options theme)
                                         :gradient gradient :gradient-axis (:fill-gradient-axis options :x)}))
     :ridges? true
     :extents (merge-with merge (group/aesthetic-extents panels (:plot-scales env))
                          (strip/extents :band items (fn [{:keys [xs]}] [(first xs) (peek xs)])))
     :padding (merge {:x [0.05 0.05]} (:padding options))
     :padding-explicit (data/explicit-padding {} options)
     :scale-defaults (strip/ridge-scale-defaults overlap (= :mirror position) (:flip? env) (get-in env [:plot-scales :y]))
     :na (group/na-dims panels)
     :dropped (assoc dropped :group no-curve)}))

(defn- flat-compute
  "Density stats without a `:y` mapping (see [[compute]])."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme] :as env}]
  (let [st (style options theme)
        position (get options :position :overlay)
        {:keys [panels dims dropped]} (group/split groups mapping env)
        group-keys (group/ordered-keys (mapcat #(map :key %) (vals panels)) dims (:plot-scales env))
        x-of (data/row-accessor :x (:x mapping))
        ;; per group: valid x values and the count of invalid ones
        xs (update-vals panels (fn [gs] (mapv #(data/finite-numbers (map x-of (:rows %))) gs)))
        x-results (mapcat identity (vals xs))
        row-count (long (reduce + 0 (map count (mapcat #(map :rows %) (vals panels)))))
        x-dropped (long (reduce + 0 (map :dropped x-results)))
        no-curve (count (filter #(< (count (distinct (:values %))) 2) x-results))
        _ (when (and (= position :mirror) (not= 2 (count group-keys)) (seq x-results))
            (schema/fail! "Density :position :mirror needs exactly two groups." {:key :position :groups (count group-keys)}))
        data (into {} (map (fn [[panel gs]]
                             [panel {:curves (panel-curves gs (xs panel) group-keys position options) :style st
                                     :zero-line? (and (= position :mirror) (data/option options :zero-line true))}]))
                   panels)
        curve-ys (fn [curves] (concat (mapcat :y curves) (mapcat :y0 curves)))
        [lo hi] (position/symmetric-extent (curve-ys (mapcat :curves (vals data))))]
    (when (and (pos? row-count) (= row-count x-dropped))
      (schema/fail! "No valid values for a mapped dimension - check the mapping." {:key :mapping :value :x :rows row-count}))
    {:data data
     :extents (merge-with merge (group/aesthetic-extents panels (:plot-scales env))
                          (into {} (keep (fn [[panel {:keys [curves]}]]
                                           (when (seq curves)
                                             [panel {:x (data/extent :x (mapcat :x curves))
                                                     :y (if (and (= position :mirror) (:symmetric options))
                                                          {:kind :numeric :min lo :max hi}
                                                          {:kind :numeric
                                                           :min (double (reduce min 0.0 (curve-ys curves)))
                                                           :max (double (reduce max 0.0 (curve-ys curves)))})}])))
                                data))
     :padding (merge {:x [0.05 0.05] :y (if (#{:mirror :silhouette :wiggle} position) [0.05 0.05] [0.0 0.05])} (:padding options))
     :padding-explicit (data/explicit-padding (when-not (#{:mirror :silhouette :wiggle} position) {:y [0.0 nil]}) options)
     :na (group/na-dims panels)
     :dropped (assoc dropped :x x-dropped :group no-curve)
     :axis-titles {:y (if (= position :normalize) "Proportion" "Density")}
     :tick-format (when (= position :mirror) {:y :absolute})}))

(defn compute
  "Per panel: `{:curves [{:x [..] :y [..] :aesthetics}] :style style}`; extents, padding, `:na`, `:dropped`
  (`:x` invalid values, `:group` groups without a curve), `:axis-titles`. With a categorical or bands `:y` -
  ridgelines, see [[ridge-compute]]."
  [{:keys [options] :as chart} chart-split env]
  (cond
    (strip/ridges? chart-split env) (ridge-compute options chart-split env)
    (:fill-gradient options) (schema/fail! "Density :fill-gradient is for ridgelines (a categorical or bands :y)."
                                           {:key :fill-gradient :value (:fill-gradient options)})
    (:fill-gradient-axis options) (schema/fail! "Density :fill-gradient-axis is for ridgelines with :fill-gradient."
                                                {:key :fill-gradient-axis :value (:fill-gradient-axis options)})
    :else (flat-compute chart chart-split env)))

(defn- fill-gradient-quads!
  "Fill between the paired point runs `upper` and `lower` (`[x y]` pixels) quad by quad, each coloured by `gradient`
  at its middle position (`ps`, one per point) relative to `[p0 p1]`; quads with a non-finite corner are skipped."
  [canvas upper lower ps gradient [p0 p1]]
  (let [p0 (double p0) span (- (double p1) p0)
        ok? (fn [[x y]] (and (m/valid-double? (double x)) (m/valid-double? (double y))))]
    (c2d/set-stroke-custom canvas {:size 1.0 :join :miter})
    (doseq [^long i (range (dec (count upper)))
            :let [a (upper i) b (upper (inc i)) c (lower (inc i)) d (lower i)]
            :when (and (ok? a) (ok? b) (ok? c) (ok? d))]
      (let [t (if (pos? span) (/ (- (* 0.5 (+ (double (ps i)) (double (ps (inc i))))) p0) span) 0.5)
            quad [a b c d]]
        (c2d/set-color canvas (gradient (m/constrain t 0.0 1.0)))
        (c2d/path canvas quad true false)
        (c2d/path canvas quad true true)))))

(defn- draw-ridges!
  "Draw ridgelines of one panel, farthest level first: area (gradient, `:fill` or the colour with `:alpha`), line."
  [{:keys [ridges top overlap style gradient gradient-axis]} canvas box scales env]
  (let [pl (strip/placement box scales {} :band)
        point (strip/ridge-point pl (double overlap))
        top (double top)
        coords (fn [points] [(mapv #(.x ^Vec2 %) points) (mapv #(.y ^Vec2 %) points)])]
    (when (pos? top)
      (doseq [{:keys [level slot xs ds aesthetics]} (strip/ridge-order pl ridges)
              :let [st (group/style style aesthetics scales (:theme env))]
              :when st]
        (let [side (strip/slot-side slot)
              curve (mapv (fn [x d] (point x level side (/ (double d) top))) xs ds)
              base (mapv (fn [x] (point x level side 0.0)) xs)
              [px py] (coords curve)
              [bx by] (coords base)]
          (cond
            (and gradient (= :y gradient-axis))
            (let [x0 (first xs)
                  pixel (fn [^Vec2 p] [(.x p) (.y p)])
                  polygon (into (mapv pixel curve) (rseq (mapv pixel base)))]
              (polyline/fill-gradient-paint! canvas [polygon] gradient
                                             (pixel (point x0 level side 0.0)) (pixel (point x0 level side 1.0)))
              (draw/fill-with-pattern! canvas (draw/pattern-fill st (gradient 0.5)) #(c2d/path canvas polygon true false)))
            gradient
            (let [pixel (fn [^Vec2 p] [(.x p) (.y p)])
                  polygon (into (mapv pixel curve) (rseq (mapv pixel base)))]
              (fill-gradient-quads! canvas (mapv pixel curve) (mapv pixel base) xs gradient (:domain (:x scales)))
              (draw/fill-with-pattern! canvas (draw/pattern-fill st (gradient 0.5)) #(c2d/path canvas polygon true false)))
            :else
            (let [fill (polyline/area-color st true)]
              (polyline/fill-under! canvas px py bx by fill (draw/pattern-fill st fill))))
          (polyline/draw! canvas px py st))))))

(defn draw
  "Draw the curves of one panel: every group's area (under the curve to y = 0, `:fill`; stacked curves - between
  `:y0` and `:y`, `:fill` or the group colour with `:alpha`), then every curve's line, then - mirror with
  `:zero-line` (default on) - the y = 0 line on top (curve tails running along y = 0 don't hide it)."
  [_chart {:keys [curves style zero-line? ridges] :as computed} canvas box scales env]
  (if ridges
    (draw-ridges! computed canvas box scales env)
    (when (seq curves)
      (let [projection (scale/projection scales)
            coords (fn [points] [(mapv #(.x ^Vec2 %) points) (mapv #(.y ^Vec2 %) points)])
            drawn (vec (keep (fn [{:keys [x y y0 aesthetics]}]
                               (when-let [st (group/style style aesthetics scales (:theme env))]
                                 {:st st :y0? (some? y0)
                                  :curve (coords (mapv projection x y))
                                  :base (coords (mapv projection x (or y0 (repeat 0.0))))}))
                             curves))]
        (doseq [{:keys [st y0? curve base]} drawn
                :let [[px py] curve [bx by] base]]
          (if y0?
            (let [fill (polyline/area-color st true)] (polyline/fill-between! canvas px py bx by fill (draw/pattern-fill st fill)))
            (let [fill (polyline/area-color st false)] (polyline/fill-under! canvas px py bx by fill (draw/pattern-fill st fill)))))
        (doseq [{:keys [st curve]} drawn
                :let [[px py] curve]]
          (polyline/draw! canvas px py st))
        (when zero-line?
          (draw/zero-line! canvas box scales style))))))

(defn legend
  "Key glyph: a horizontal line through the middle of `box`; when the chart fills areas (`:fill`, or `:stack` /
  `:normalize` positions, ridgelines) a filled square (the area colour) outlined in the line colour."
  [{:keys [options]} attributes canvas box env]
  (let [st (merge (style options (:theme env)) (select-keys attributes [:color :dash :pattern]))
        area (polyline/area-color st (or (contains? stacked-positions (:position options)) (:ridges? (:chart-stats env))))]
    (polyline/key-glyph! canvas box st area (draw/pattern-fill st area))))
