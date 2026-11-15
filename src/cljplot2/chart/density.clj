(ns cljplot2.chart.density
  "Density: a kernel density estimate (fastmath) of `:x` per panel and group, drawn as a line.

  Options (style defaults from theme `[:chart :density]`):

  * `:kernel` - fastmath KDE kernel keyword (default `:gaussian`)
  * `:bandwidth` - a number or a fastmath selector keyword (default `:nrd0`, as R)
  * `:n` - evaluation points (default 512), `:cut` - the curve spans `[min - cut*h, max + cut*h]` (default 3, as R)
  * `:position` - grouped curves (see [[cljplot2.data.position]]): `:overlay` (default), `:stack` and `:normalize` (every
    group on one common range, stacked; normalize - summing to 1, y title \"Proportion\"; areas filled with `:fill` or
    the group colour with opacity `:alpha`), `:mirror` (exactly two groups, the second drawn downward; y tick labels show
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
  growing away from the next level); `:fill-gradient` (ridges only) - fill colour along `:x`: a clojure2d gradient
  (keyword preset, colour list or function of `t` in `[0 1]` over the x scale's domain)."
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
  (conj polyline/style-entries
        (schema/optional :fill [:or [:= :color] schema/color])
        (schema/optional :alpha schema/fraction)
        (schema/optional :zero-line-color schema/color)
        (schema/optional :zero-line-width schema/non-neg)))

(def ^:private gradient-schema
  (schema/named-type ::gradient #(or (keyword? %) (sequential? %) (fn? %))
                     "should be a clojure2d gradient: a preset keyword, a colour list or a function"))

(def ^:private positions (disj position/positions :dodge))

(def schemas
  "Schemas of the density chart: `:style` (theme `[:chart :density]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (conj (schema/optional :kernel keyword?)
                      (schema/optional :bandwidth [:or schema/pos keyword?])
                      (schema/nilable :n [:and :int [:>= 2]])
                      (schema/nilable :cut schema/non-neg)
                      (schema/optional :position (into [:enum] (sort positions)))
                      (schema/nilable :symmetric schema/bool)
                      (schema/nilable :zero-line schema/bool)
                      (schema/optional :fill-gradient gradient-schema)
                      (schema/optional :padding data/position-padding))
                (into strip/ridge-options))
   :mapping [:map [:x schema/accessor]]})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :density]) (select-keys options [:color :width :dash :fill :alpha :zero-line-color :zero-line-width])))

(defn- area-color
  "Fill of a group's area with style `st` (group attributes merged): the group colour with opacity `:alpha` (0..1) for
  `:fill :color` or for stacked curves (`stacked?`) without a fill, else `:fill` (`:none` - no area)."
  [st stacked?]
  (if (or (= :color (:fill st)) (and stacked? (theme/none? (:fill st))))
    (c/set-alpha (:color st) (* 255.0 (double (:alpha st))))
    (:fill st)))

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
      (:stack :normalize) (if (empty? estimates)
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

(defn- gradient-fn
  "clojure2d gradient of the `:fill-gradient` option `g` (a function is taken as is); throws `ex-info
  {:key :fill-gradient}` for an unknown one."
  [g]
  (if (fn? g)
    g
    (try (c/gradient g)
         (catch Exception e
           (schema/fail! "Unknown :fill-gradient." {:key :fill-gradient :value g :cause (ex-message e)})))))

(defn- ridge-compute
  "Ridgeline stats (categorical or bands `:y`): per panel `{:ridges [item ...] :top max-density :overlap o :style st :gradient g}`,
  items (see [[cljplot2.data.strip/ridge-split]]) with `:xs` / `:ds` - the curve; one `:top` over all panels."
  [options chart-split {:keys [theme] :as env}]
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
        gradient (some-> (:fill-gradient options) gradient-fn)]
    {:data (update-vals items (fn [its] {:ridges its :top top :overlap overlap :style (style options theme)
                                         :gradient gradient}))
     :ridges? true
     :extents (merge-with merge (group/aesthetic-extents panels (:plot-scales env))
                          (strip/extents :band items (fn [{:keys [xs]}] [(first xs) (peek xs)])))
     :padding (merge {:x [0.05 0.05]} (:padding options))
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
     :padding (merge {:x [0.05 0.05] :y (if (= position :mirror) [0.05 0.05] [0.0 0.05])} (:padding options))
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
    :else (flat-compute chart chart-split env)))

(defn- fill-gradient!
  "Fill under the curve `points` down to `base` (`Vec2`s, pairwise) quad by quad, each coloured by `gradient` at its
  middle `x` (`xs`) relative to `[d0 d1]`; quads with a non-finite corner are skipped."
  [canvas points base xs gradient [d0 d1]]
  (let [d0 (double d0) span (- (double d1) d0)
        ok? (fn [^Vec2 p] (and (m/valid-double? (.x p)) (m/valid-double? (.y p))))]
    (c2d/set-stroke-custom canvas {:size 1.0 :join :miter})
    (doseq [^long i (range (dec (count points)))
            :let [^Vec2 a (points i) ^Vec2 b (points (inc i)) ^Vec2 c (base (inc i)) ^Vec2 d (base i)]
            :when (and (ok? a) (ok? b) (ok? c) (ok? d))]
      (let [t (if (pos? span) (/ (- (* 0.5 (+ (double (xs i)) (double (xs (inc i))))) d0) span) 0.5)
            quad [[(.x a) (.y a)] [(.x b) (.y b)] [(.x c) (.y c)] [(.x d) (.y d)]]]
        (c2d/set-color canvas (gradient (m/constrain t 0.0 1.0)))
        (c2d/path canvas quad true false)
        (c2d/path canvas quad true true)))))

(defn- draw-ridges!
  "Draw ridgelines of one panel, farthest level first: area (gradient, `:fill` or the colour with `:alpha`), line."
  [{:keys [ridges top overlap style gradient]} canvas box scales env]
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
          (if gradient
            (fill-gradient! canvas curve base xs gradient (:domain (:x scales)))
            (polyline/fill-under! canvas px py bx by (area-color st true)))
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
            (polyline/fill-between! canvas px py bx by (area-color st true))
            (polyline/fill-under! canvas px py bx by (area-color st false))))
        (doseq [{:keys [st curve]} drawn
                :let [[px py] curve]]
          (polyline/draw! canvas px py st))
        (when zero-line?
          (draw/zero-line! canvas box scales style))))))

(defn legend
  "Key glyph: a horizontal line through the middle of `box`; when the chart fills areas (`:fill`, or `:stack` /
  `:normalize` positions, ridgelines) a filled square (the area colour) outlined in the line colour."
  [{:keys [options]} attributes canvas box env]
  (let [st (merge (style options (:theme env)) (select-keys attributes [:color :dash]))
        area (area-color st (or (contains? #{:stack :normalize} (:position options)) (:ridges? (:chart-stats env))))]
    (polyline/key-glyph! canvas box st area)))
