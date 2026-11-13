(ns cljplot2.chart.density
  "Density: a kernel density estimate (fastmath) of `:x` per panel and group, drawn as a line.

  Options (style defaults from theme `[:chart :density]`):

  * `:kernel` - fastmath KDE kernel keyword (default `:gaussian`)
  * `:bandwidth` - a number or a fastmath selector keyword (default `:nrd0`, as R)
  * `:n` - evaluation points (default 512), `:cut` - the curve spans `[min - cut*h, max + cut*h]` (default 3, as R)
  * `:position` - grouped curves (see [[cljplot2.data.position]]): `:overlay` (default), `:stack` and `:fill` (every
    group on one common range, stacked; fill - summing to 1, y title \"Proportion\"; areas filled with `:fill` or the
    group colour, translucent), `:mirror` (exactly two groups, the second drawn downward; y tick labels show
    magnitudes); `:symmetric` - mirror only: both halves on one y range `[-M M]`; `:zero-line` - mirror only: draw
    the y = 0 line (default true; `:zero-line-color`, `:zero-line-width`)
  * `:color`, `:width`, `:dash` (line), `:fill` (area under the curve, default `:none`)
  * `:padding {:x [lo hi] :y [lo hi]}` - default `{:x [0.05 0.05] :y [0 0.05]}`

  Groups (see [[cljplot2.data.group]]) come from ordinal aesthetics (`:color` colors the line) and `:group`; a group with
  fewer than 2 distinct values has no curve and is counted in `:dropped {:group n}`."
  (:require [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.kernel :as k]
            [cljplot2.data.rows :as data]
            [cljplot2.data.group :as group]
            [cljplot2.data.position :as position]
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
        (schema/optional :fill schema/color)
        (schema/optional :zero-line-color schema/color)
        (schema/optional :zero-line-width schema/non-neg)))

(def ^:private positions (disj position/positions :dodge))

(def ^:private ^:const stacked-alpha
  "Fill opacity (0-255) of stacked areas without a `:fill` option: the group colour, translucent."
  160)

(def Schema
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
                      (schema/optional :padding data/position-padding)))
   :mapping [:map [:x schema/accessor]]})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :density]) (select-keys options [:color :width :dash :fill :zero-line-color :zero-line-width])))

(defn- area-color
  "Fill of a group's area with style `st` (group attributes merged): `:fill`, or for stacked curves (`stacked?`) without
  one the group colour, translucent; `:none` - no area."
  [st stacked?]
  (if (and stacked? (theme/none? (:fill st)))
    (c/set-alpha (:color st) stacked-alpha)
    (:fill st)))

(defn- estimate
  "`{:kde f :lo :hi}` of the KDE of `values` (at least 2 distinct): the curve spans `[min - cut*h, max + cut*h]`."
  [values {:keys [kernel bandwidth cut] :or {kernel :gaussian bandwidth :nrd0 cut 3}}]
  (let [{:keys [kde h]} (try (k/kernel-density kernel values {:bandwidth bandwidth} true)
                             (catch clojure.lang.ExceptionInfo e
                               (schema/fail! (str "Density estimation failed: " (ex-message e)) {:key :density :cause (ex-data e)})))
        h (double h)]
    {:kde kde
     :lo (- (double (reduce min values)) (* (double cut) h))
     :hi (+ (double (reduce max values)) (* (double cut) h))}))

(defn- grid-points
  "`n` equally spaced points from `lo` to `hi`."
  [^double lo ^double hi ^long n]
  (let [step (/ (- hi lo) (dec n))]
    (mapv #(+ lo (* step (double %))) (range n))))

(defn- curve
  "`{:x [..] :y [..]}` of the KDE estimate `e` evaluated at `xs`."
  [{:keys [kde]} xs]
  {:x xs :y (mapv #(double (kde %)) xs)})

(defn- panel-curves
  "Curves of a panel's groups `gs` (with their `group-values`) for `position`; `group-keys` - all groups in order.
  Overlay and mirror: each group on its own range (mirror: the second group of `group-keys` negated). Stack and fill:
  every group on one common range of the panel's curves, `:y0` / `:y` from
  [[cljplot2.data.position/stack-values]]."
  [gs group-values group-keys position options]
  (let [n (long (or (:n options) 512))
        estimates (vec (keep (fn [[g {:keys [values]}]]
                               (when (>= (count (distinct values)) 2)
                                 (assoc (estimate values options) :group g)))
                             (map vector gs group-values)))]
    (case position
      (:stack :fill) (if (empty? estimates)
                       []
                       (let [xs (grid-points (reduce min (map :lo estimates)) (reduce max (map :hi estimates)) n)
                             curves (mapv #(curve % xs) estimates)
                             ys (position/stack-values position (mapv :y curves))]
                         (mapv (fn [{:keys [group]} c y01]
                                 (assoc c :y0 (mapv first y01) :y (mapv second y01) :aesthetics (:aesthetics group)))
                               estimates curves ys)))
      (mapv (fn [{:keys [group lo hi] :as e}]
              (let [c (curve e (grid-points lo hi n))
                    down? (and (= position :mirror) (= (:key group) (second group-keys)))]
                (cond-> (assoc c :aesthetics (:aesthetics group))
                  down? (update :y #(mapv - %)))))
            estimates))))

(defn compute
  "Per panel: `{:curves [{:x [..] :y [..] :aesthetics}] :style style}`; extents, padding, `:na`, `:dropped`
  (`:x` invalid values, `:group` groups without a curve), `:axis-titles`."
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
                                     :zero-line? (and (= position :mirror) (get options :zero-line true))}]))
                   panels)
        curve-ys (fn [curves] (concat (mapcat :y curves) (mapcat :y0 curves)))
        [lo hi] (position/symmetric-extent (curve-ys (mapcat :curves (vals data))))]
    (when (and (pos? row-count) (= row-count x-dropped))
      (schema/fail! "No valid values for a mapped dimension - check the mapping." {:key :mapping :value :x :rows row-count}))
    {:data data
     :extents (merge-with merge (group/aesthetic-extents panels)
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
     :axis-titles {:y (if (= position :fill) "Proportion" "Density")}
     :tick-format (when (= position :mirror) {:y :absolute})}))

(defn- draw-zero-line!
  "The y = 0 line across the panel `box` (along the x scale's direction), style `:zero-line-color` /
  `:zero-line-width`; nothing when 0 isn't in the y scale or the line draws nothing."
  [canvas {:keys [w h]} scales style]
  (let [p (scale/forward (:y scales) 0.0)
        color (:zero-line-color style)
        width (double (:zero-line-width style))]
    (when (and (not (m/nan? p)) (not (theme/none? color)) (pos? width))
      (let [p (draw/pixel-center p)]
        (c2d/set-color canvas color)
        (c2d/set-stroke-custom canvas {:size width :cap :butt})
        (if (scale/horizontal? :y (:y scales))
          (c2d/line canvas p 0 p h)
          (c2d/line canvas 0 p w p))))))

(defn draw
  "Draw the curves of one panel: fill (under the curve to y = 0; stacked curves - between `:y0` and `:y`, `:fill` or
  the group colour, translucent), then the line; mirror (with `:zero-line`, default on) - then the y = 0 line."
  [_chart {:keys [curves style zero-line?]} canvas box scales env]
  (when (seq curves)
    (let [projection (scale/projection scales)]
      (doseq [{:keys [x y y0 aesthetics]} curves
              :let [st (group/style style aesthetics scales (:theme env))]
              :when st]
        (let [points (mapv projection x y)
              bases (mapv projection x (or y0 (repeat 0.0)))
              px (mapv #(.x ^Vec2 %) points) py (mapv #(.y ^Vec2 %) points)
              bx (mapv #(.x ^Vec2 %) bases) by (mapv #(.y ^Vec2 %) bases)]
          (if y0
            (polyline/fill-between! canvas px py bx by (area-color st true))
            (polyline/fill-under! canvas px py bx by (:fill st)))
          (polyline/draw! canvas px py st)))
      (when zero-line?
        (draw-zero-line! canvas box scales style)))))

(defn legend
  "Key glyph: a horizontal line through the middle of `box`; when the chart fills areas (`:fill`, or `:stack` /
  `:fill` positions) a filled square (the area colour) outlined in the line colour."
  [{:keys [options]} attributes canvas {:keys [x y w h]} env]
  (let [st (merge (style options (:theme env)) (select-keys attributes [:color]))
        area (area-color st (contains? #{:stack :fill} (:position options)))]
    (if (theme/none? area)
      (let [cy (+ (double y) (* 0.5 (double h)))]
        (polyline/draw! canvas [(double x) (+ (double x) (double w))] [cy cy] st))
      (let [side (* 0.7 (m/min (double w) (double h)))
            cx (+ (double x) (* 0.5 (double w)))
            cy (+ (double y) (* 0.5 (double h)))]
        (c2d/set-color canvas area)
        (c2d/crect canvas cx cy side side)
        (when (polyline/set-style! canvas st)
          (c2d/crect canvas cx cy side side true))))))
