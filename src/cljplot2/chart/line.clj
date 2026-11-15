(ns cljplot2.chart.line
  "Line: points `(x, y)` joined per panel and group.

  Options (style defaults from theme `[:chart :line]`): `:sort` (default true - points ordered by numeric or temporal
  x, ties in data order; `false` keeps data order), `:color`, `:width`, `:dash`, `:fill` (area chart: the area
  between the line and y = 0 - or the nearer panel edge when 0 is outside the y range - default `:none`),
  `:padding {:x [lo hi] :y [lo hi]}` (default 5% each side).
  Curves (see [[cljplot2.graphics.curve]]): `:interpolator` (a fastmath 1D method or a function) with
  `:interpolator-params` - each run fitted along x in pixels and evaluated every `:sample-step` (pixels, default 1,
  or `{:count n}`); `:curve :parametric` / `:bezier` - smooth paths in data order (connected scatter); the area follows
  the curve.
  A row with an invalid x is dropped (counted); an invalid y breaks the line there (counted); `:gaps` - `:break`
  (default, empty space) or `:bridge` (a straight segment over the gap in the theme `:bridge` style - the line alpha
  multiplied by `:alpha`, optional `:width` / `:dash`; the area stays empty).
  Groups (see [[cljplot2.data.group]]) come from ordinal aesthetics (`:color` colors the line) and `:group`.
  A `:color` with a linear or binned scale doesn't group: it colors each line along its length - every segment in
  the color of the mean of its two end values (the theme NA color when one is NA), round caps, the dash restarting
  per segment; rows outside the color scale's explicit domain are dropped (counted), NA rows follow the plot's
  `:groups {:na}`. With an `:interpolator` the sampled points take the color value interpolated linearly between
  their data points (`:curve` can't color along the length - `ex-info {:key :curve}`); a bridge takes the color of
  its two end values."
  (:require [cljplot2.data.rows :as data]
            [cljplot2.data.group :as group]
            [cljplot2.graphics.curve :as curve]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries
  (conj polyline/style-entries (schema/optional :fill schema/color) (schema/optional :bridge polyline/bridge-schema)))

(def schemas
  "Schemas of the line chart: `:style` (theme `[:chart :line]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into curve/options)
                (conj (schema/nilable :sort schema/bool)
                      (schema/optional :gaps [:enum :break :bridge])
                      (schema/optional :padding data/position-padding)))
   :mapping [:map [:x schema/accessor] [:y schema/accessor]]})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :line]) (select-keys options [:color :width :dash :fill :bridge])))

(defn- sortable? [xs] (or (every? number? xs) (every? data/temporal? xs)))

(defn compute
  "Per panel: `{:lines [{:x [..] :y [..] :aesthetics}] :style style}` (`y` `nil` where the line breaks; a gradient
  line also has `:colors` - the color value of every point); extents, padding, `:na`, `:dropped`."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme plot-scales] :as env}]
  (let [st (style options theme)
        continuous (group/continuous-color groups mapping env)
        gradient? (some? continuous)
        color-of (:color-of continuous)
        {:keys [panels dropped]} (if continuous
                                   (group/split (:groups continuous) (:mapping continuous) env)
                                   (group/split groups mapping env))
        x-of (data/row-accessor :x (:x mapping))
        y-of (data/row-accessor :y (:y mapping))
        sort? (data/option options :sort true)
        rows (mapcat (fn [gs] (mapcat :rows gs)) (vals panels))
        row-count (count rows)
        _ (doseq [[k f] [[:x x-of] [:y y-of]]]
            (when (and (pos? row-count) (not-any? #(data/valid-value? (f %)) rows))
              (schema/fail! "No valid values for a mapped dimension - check the mapping." {:key :mapping :value k :rows row-count})))
        data (update-vals panels
                          (fn [gs]
                            {:lines (mapv (fn [g]
                                            (let [pts (->> (:rows g)
                                                           (map (fn [r] [(x-of r) (y-of r) (when color-of (color-of r))]))
                                                           (filter #(data/valid-value? (first %))))
                                                  pts (if (and sort? (sortable? (map first pts))) (sort-by first pts) pts)]
                                              (cond-> {:x (mapv first pts)
                                                       :y (mapv #(let [v (second %)] (when (data/valid-value? v) v)) pts)
                                                       :aesthetics (:aesthetics g)}
                                                gradient? (assoc :colors (mapv #(nth % 2) pts)))))
                                          gs)
                             :style st}))]
    (let [color-stats (when gradient?
                        (group/continuous-color-stats continuous (update-vals data #(mapcat :colors (:lines %))) plot-scales))]
    {:data data
     :extents (merge-with merge (group/aesthetic-extents panels (:plot-scales env))
                          (:extents color-stats)
                          (into {} (keep (fn [[panel {:keys [lines]}]]
                                           (let [xs (mapcat :x lines) ys (remove nil? (mapcat :y lines))]
                                             (when (seq ys)
                                               [panel {:x (data/extent :x xs) :y (data/extent :y ys)}]))))
                                data))
     :padding (merge {:x [0.05 0.05] :y [0.05 0.05]} (:padding options))
     :na (cond-> (group/na-dims panels) (:na? color-stats) (conj :color))
     :dropped (cond-> (assoc dropped
                     :x (count (remove #(data/valid-value? (x-of %)) rows))
                     :y (count (remove #(data/valid-value? (y-of %)) rows)))
                gradient? (assoc :color (:dropped color-stats)))})))

(defn- segment-colors
  "Colors of the segments of a gradient line with point color values `colors` under the finalized color `scale`:
  segment `i` - the color of the mean of values `i` and `i + 1`, the theme NA color when either is NA (every
  segment without a scale - all values NA)."
  [colors scale theme]
  (let [na (get-in theme [:palette :na])
        color-of (if scale (aesthetic/attribute-fn :color scale na) (constantly na))]
    (mapv (fn [a b] (if (or (nil? a) (nil? b)) na (color-of (* 0.5 (+ (double a) (double b))))))
          colors (rest colors))))

(defn- lerp-values
  "Values at positions `ts` interpolated linearly from `values` at strictly monotone `positions` (`nil` where an
  enclosing value is `nil`); positions outside take the nearest segment."
  [positions values ts]
  (let [[ps vs] (if (> (double (first positions)) (double (peek positions)))
                  [(vec (rseq positions)) (vec (rseq values))]
                  [positions values])
        last-segment (- (long (count ps)) 2)]
    (mapv (fn [t]
            (let [t (double t)
                  i (long (loop [i 0] (if (and (< i last-segment) (> t (double (ps (inc i))))) (recur (inc i)) i)))
                  a (vs i) b (vs (inc i))]
              (when (and (some? a) (some? b))
                (let [p0 (double (ps i)) p1 (double (ps (inc i)))
                      f (if (== p0 p1) 0.0 (/ (- t p0) (- p1 p0)))]
                  (+ (double a) (* f (- (double b) (double a))))))))
          ts)))

(defn- curve-options
  "Curve options of the chart `options` for a line whose x runs along pixel axis `:y` when `flipped?`."
  [options flipped?]
  (assoc (select-keys options [:interpolator :interpolator-params :curve :sample-step]) :along (if flipped? :y :x)))

(defn- drawn-runs
  "Drawn point runs (pixels) of the index runs `index-runs` of `px` / `py` under `opts`; a curve error names the
  line's group `aesthetics`."
  [index-runs px py opts aesthetics]
  (try (mapv (fn [run] (mapv curve/clamp-point (curve/curve-points (mapv (fn [i] [(px i) (py i)]) run) opts))) index-runs)
       (catch clojure.lang.ExceptionInfo e
         (if (= :interpolator (:key (ex-data e)))
           (let [message (:message (first (:errors (ex-data e))))]
             (schema/fail! (str message (when (seq aesthetics) (str " Group: " (pr-str aesthetics) ".")))
                           (cond-> (dissoc (ex-data e) :errors) (seq aesthetics) (assoc :group aesthetics))))
           (throw e)))))

(defn- draw-gradient!
  "Stroke a gradient line: `index-runs` of the data points `px` / `py` with color values `colors`, drawn as
  `point-runs`; curved runs take color values interpolated along `along`; bridges (`gaps` `:bridge`) the color of
  their end values."
  [canvas px py colors index-runs point-runs curved? along st gaps scales theme]
  (let [a (if (= :y along) 1 0)]
    (if curved?
      (doseq [[run points] (map vector index-runs point-runs)
              :when (> (count points) 1)]
        (let [values (lerp-values (mapv #(if (zero? a) (px %) (py %)) run) (mapv colors run) (mapv #(nth % a) points))]
          (polyline/draw-segments! canvas (mapv first points) (mapv second points)
                                   (segment-colors values (:color scales) theme) st)))
      (polyline/draw-segments! canvas px py (segment-colors colors (:color scales) theme) st))
    (when (= :bridge gaps)
      (doseq [[r0 r1] (map vector index-runs (rest index-runs))
              :let [color (first (segment-colors [(colors (peek r0)) (colors (first r1))] (:color scales) theme))]]
        (polyline/stroke-runs! canvas [[[(px (peek r0)) (py (peek r0))] [(px (first r1)) (py (first r1))]]]
                               (polyline/bridge-style (assoc st :color color) (:bridge st)))))))

(defn draw
  "Draw the lines of one panel (area fill first, if any); `nil` y values and values the scales can't place break a
  line (bridged with `:gaps :bridge`). The fill baseline is y = 0 clamped into the panel (`box`) along the y scale's
  direction, see [[cljplot2.graphics.curve/area-baseline]]."
  [{:keys [options]} {:keys [lines style]} canvas {:keys [w h]} scales env]
  (when (seq lines)
    (let [projection (scale/projection scales)
          y-scale (:y scales)
          y-horizontal? (scale/horizontal? :y y-scale)
          area {:baseline (curve/area-baseline y-scale w h) :horizontal? (not y-horizontal?)}
          opts (curve-options options (scale/flipped? scales))
          curved? (curve/curved? opts)
          gaps (:gaps options :break)]
      (when (and curved? (:curve opts) (some :colors lines))
        (schema/fail! "A line colored along its length can't take :curve (use :interpolator)."
                      {:key :curve :value (:curve opts)}))
      (doseq [{:keys [x y aesthetics colors]} lines
              :let [st (group/style style aesthetics scales (:theme env))]
              :when st]
        (let [points (mapv (fn [vx vy] (if (nil? vy) (Vec2. ##NaN ##NaN) (projection vx vy))) x y)
              px (mapv #(.x ^Vec2 %) points)
              py (mapv #(.y ^Vec2 %) points)
              index-runs (polyline/finite-runs px py)
              point-runs (if curved?
                           (drawn-runs index-runs px py opts aesthetics)
                           (mapv (fn [run] (mapv (fn [i] (curve/clamp-point [(px i) (py i)])) run)) index-runs))]
          (if colors
            (do (polyline/fill-runs-to-baseline! canvas point-runs (:baseline area) (:horizontal? area) (:fill st))
                (draw-gradient! canvas px py colors index-runs point-runs curved? (:along opts) st gaps scales (:theme env)))
            (curve/draw-runs! canvas point-runs st area gaps)))))))

(defn legend
  "Key glyph: a horizontal line through the middle of `box`."
  [{:keys [options]} attributes canvas {:keys [x y w h]} env]
  (let [st (merge (style options (:theme env)) (select-keys attributes [:color :dash]))
        cy (+ (double y) (* 0.5 (double h)))]
    (polyline/draw! canvas [(double x) (+ (double x) (double w))] [cy cy] st)))
