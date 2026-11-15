(ns cljplot2.chart.line
  "Line: points `(x, y)` joined per panel and group.

  Options (style defaults from theme `[:chart :line]`): `:sort` (default true - points ordered by numeric or temporal
  x, ties in data order; `false` keeps data order), `:color`, `:width`, `:dash`, `:fill` (area chart: the area
  between the line and y = 0 - or the nearer panel edge when 0 is outside the y range - default `:none`),
  `:padding {:x [lo hi] :y [lo hi]}` (default 5% each side; stacked `[0 0.05]` for y).
  `:fill :color` - the area in the line (group) colour at `:alpha` (0..1, theme default 0.6). `:fill-gradient` (a
  clojure2d gradient: preset, colours or function of `t`) colours areas along `:fill-gradient-axis` - `:x` (default,
  over the x scale's range) or `:y` (a linear gradient over the y scale's range); it wins over `:fill`.
  `:position` (see [[cljplot2.data.position]]): `:overlay` (default), `:stack`, `:normalize` (y title
  \"Proportion\"), streamgraphs `:silhouette` / `:wiggle` - groups aligned on the union of their x values (numeric
  or temporal; linear between a group's points, 0 outside its range; NA y left out and interpolated across; repeated
  x in a group - `ex-info {:key :x}`), stacked, every band filled (`:fill`, default the group colour at `:alpha`)
  and its top stroked; `:interpolator` smooths the bands; `:curve`, `:gaps` and a line coloured along its length
  can't be stacked.
  Curves (see [[cljplot2.graphics.curve]]): `:interpolator` (a fastmath 1D method or a function) with
  `:interpolator-params` - each run fitted along x in pixels and evaluated every `:sample-step` (pixels, default 1,
  or `{:count n}`); `:curve :parametric` / `:bezier` - smooth paths in data order (connected scatter); the area follows
  the curve.
  A row with an invalid x is dropped (counted); an invalid y breaks the line there (counted); `:gaps` - `:break`
  (default, empty space) or `:bridge` (a straight segment over the gap in the theme `:bridge` style - the line alpha
  multiplied by `:alpha`, optional `:width` / `:dash`; the area stays empty).
  Groups (see [[cljplot2.data.group]]) come from ordinal aesthetics (`:color` colors the line) and `:group`.
  A `:color` with a linear or binned scale doesn't group: it colors each line along its length - every segment in
  the color of the mean of its two end values (the theme NA color when one is NA), round caps, the dash running on
  along the line; rows outside the color scale's explicit domain are dropped (counted), NA rows follow the plot's
  `:groups {:na}`. With an `:interpolator` the sampled curve takes the color value interpolated linearly between
  its data points, in steps of a few pixels (`:curve` can't color along the length - `ex-info {:key :curve}`); a bridge takes the color of
  its two end values."
  (:require [fastmath.core :as m]
            [cljplot2.data.rows :as data]
            [cljplot2.data.group :as group]
            [cljplot2.data.position :as position]
            [cljplot2.graphics.curve :as curve]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries
  (conj polyline/style-entries
        (schema/optional :fill [:or [:= :color] schema/color])
        (schema/optional :alpha schema/fraction)
        (schema/optional :bridge polyline/bridge-schema)))

(def positions
  "Line positions: `:overlay` (default), stacked `:stack` / `:normalize` and the stream positions (see
  [[cljplot2.data.position]])."
  (into #{:overlay :stack :normalize} position/stream-positions))

(def ^:private stacked-positions (disj positions :overlay))

(def schemas
  "Schemas of the line chart: `:style` (theme `[:chart :line]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into curve/options)
                (conj (schema/nilable :sort schema/bool)
                      (schema/optional :gaps [:enum :break :bridge])
                      (schema/optional :position (into [:enum] (sort positions)))
                      (schema/optional :fill-gradient polyline/fill-gradient-schema)
                      (schema/optional :fill-gradient-axis [:enum :x :y])
                      (schema/optional :padding data/position-padding)))
   :mapping [:map [:x schema/accessor] [:y schema/accessor]]})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :line]) (select-keys options [:color :width :dash :fill :alpha :bridge])))

(defn- sortable? [xs] (or (every? number? xs) (every? data/temporal? xs)))

(defn- aligned-heights
  "Heights of a group with points at x keys `ks` (strictly increasing) and values `vs` at the union keys `union`
  (increasing): linear between its points, 0 outside its range (one walk over both)."
  [ks vs union]
  (let [n (count ks)
        lo (double (first ks)) hi (double (peek ks))]
    (loop [i 0 us (seq union) acc []]
      (if-not us
        acc
        (let [u (double (first us))]
          (cond
            (or (< u lo) (> u hi)) (recur i (next us) (conj acc 0.0))
            (and (< i (- n 2)) (> u (double (ks (inc i))))) (recur (inc i) us acc)
            (== n 1) (recur i (next us) (conj acc (double (vs 0))))
            :else (let [k0 (double (ks i)) k1 (double (ks (inc i)))
                        v0 (double (vs i)) v1 (double (vs (inc i)))
                        f (if (== k0 k1) 0.0 (/ (- u k0) (- k1 k0)))]
                    (recur i (next us) (conj acc (+ v0 (* f (- v1 v0))))))))))))

(defn- stacked-lines
  "The lines of one panel stacked by `position` (see [[cljplot2.data.position/stack-values]]): every group on the
  union of the groups' x (keys of [[cljplot2.data.rows/x-key]]), interpolated linearly inside its own range and 0 outside; NA y points
  left out (interpolated across); points in x order whatever `:sort` says. Each line `{:x union :y upper :y0 lower
  :heights aligned :aesthetics}` (`:heights` - the group's aligned values, for smoothing at drawing). Non-numeric,
  non-temporal x or a repeated x in a group throws `ex-info {:key :x}`."
  [lines position]
  (let [groups (mapv (fn [{:keys [x y aesthetics]}]
                       (let [keyed (->> (map vector x y)
                                        (filter #(some? (second %)))
                                        (map (fn [[vx vy]] [(or (data/x-key vx)
                                                                (schema/fail! "Stacked lines need numeric or temporal (java.time) x."
                                                                              {:key :x :value vx}))
                                                            vx vy]))
                                        (sort-by first))
                             ks (mapv first keyed)
                             pts (mapv (fn [[_ vx vy]] [vx vy]) keyed)]
                         (when-not (or (< (count ks) 2) (apply < ks))
                           (schema/fail! "Stacked lines need distinct x values within a group." {:key :x :group aesthetics}))
                         {:ks ks :vs (mapv #(double (second %)) pts) :xs (mapv first pts) :aesthetics aesthetics}))
                     lines)
        groups (filterv #(seq (:ks %)) groups)
        by-key (into (sorted-map) (mapcat (fn [{:keys [ks xs]}] (map vector ks xs)) groups))
        union (vec (keys by-key))
        heights (mapv (fn [{:keys [ks vs]}] (aligned-heights ks vs union)) groups)
        ys (position/stack-values position heights)
        xs (vec (vals by-key))]
    (mapv (fn [{:keys [aesthetics]} hs y01] {:x xs :y (mapv second y01) :y0 (mapv first y01) :heights hs :aesthetics aesthetics})
          groups heights ys)))

(defn- check-stacking!
  "Throw `ex-info` for options a stacked `position` can't take: a line colored along its length (`gradient?`),
  `:curve`, `:gaps`."
  [position gradient? options]
  (when (stacked-positions position)
    (cond
      gradient? (schema/fail! "A line colored along its length (linear or binned :color) can't be stacked."
                              {:key :position :value position})
      (:curve options) (schema/fail! "Stacked lines can't take :curve (use :interpolator)." {:key :curve :value (:curve options)})
      (:gaps options) (schema/fail! "Stacked lines interpolate across missing values; :gaps doesn't apply."
                                    {:key :gaps :value (:gaps options)}))))

(defn compute
  "Per panel: `{:lines [{:x [..] :y [..] :aesthetics}] :style style}` (`y` `nil` where the line breaks; a gradient
  line also has `:colors` - the color value of every point); extents, padding, `:na`, `:dropped`."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme plot-scales] :as env}]
  (let [st (style options theme)
        position (get options :position :overlay)
        continuous (group/continuous-color groups mapping env)
        gradient? (some? continuous)
        _ (check-stacking! position gradient? options)
        _ (when (and (:fill-gradient-axis options) (nil? (:fill-gradient options)))
            (schema/fail! ":fill-gradient-axis needs :fill-gradient." {:key :fill-gradient-axis :value (:fill-gradient-axis options)}))
        _ (when (and gradient? (:fill-gradient options))
            (schema/fail! "A line colored along its length can't take :fill-gradient." {:key :fill-gradient :value (:fill-gradient options)}))
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
                             :style st}))
        data (if (stacked-positions position)
               (update-vals data #(update % :lines stacked-lines position))
               data)
        negative? (some neg? (mapcat (fn [{:keys [lines]}] (mapcat :y0 lines)) (vals data)))]
    (let [color-stats (when gradient?
                        (group/continuous-color-stats continuous (update-vals data #(mapcat :colors (:lines %))) plot-scales))]
    {:data data
     :extents (merge-with merge (group/aesthetic-extents panels (:plot-scales env))
                          (:extents color-stats)
                          (into {} (keep (fn [[panel {:keys [lines]}]]
                                           (let [xs (mapcat :x lines) ys (remove nil? (concat (mapcat :y lines) (mapcat :y0 lines)))]
                                             (when (seq ys)
                                               [panel {:x (data/extent :x xs) :y (data/extent :y ys)}]))))
                                data))
     :padding (merge {:x [0.05 0.05]
                      :y (if (and (#{:stack :normalize} position) (not negative?)) [0.0 0.05] [0.05 0.05])}
                     (:padding options))
     :axis-titles (when (= :normalize position) {:y "Proportion"})
     :na (cond-> (group/na-dims panels) (:na? color-stats) (conj :color))
     :dropped (cond-> (assoc dropped
                     :x (count (remove #(data/valid-value? (x-of %)) rows))
                     :y (count (remove #(data/valid-value? (y-of %)) rows)))
                gradient? (assoc :color (:dropped color-stats)))})))

(defn- value-colors
  "Colors of the color values `values` under the finalized color `scale`: the theme NA color for `nil` (every value
  without a scale - all values NA)."
  [values scale theme]
  (let [na (get-in theme [:palette :na])
        color-of (if scale (aesthetic/attribute-fn :color scale na) (constantly na))]
    (mapv #(if (nil? %) na (color-of (double %))) values)))

(defn- segment-colors
  "Colors of the segments of a gradient line with point color values `colors` under the finalized color `scale`:
  segment `i` - the color of the mean of values `i` and `i + 1`, the theme NA color when either is NA (every
  segment without a scale - all values NA)."
  [colors scale theme]
  (value-colors (mapv (fn [a b] (when (and (some? a) (some? b)) (* 0.5 (+ (double a) (double b))))) colors (rest colors))
                scale theme))

(defn- lerp-values
  "Values at positions `ts` interpolated linearly from `values` at strictly monotone `positions` (`nil` where an
  enclosing value is `nil`); positions outside take the nearest segment. `ts` run in the direction of `positions`
  (one walk over both)."
  [positions values ts]
  (let [reversed? (> (double (first positions)) (double (peek positions)))
        [ps vs ts] (if reversed? [(vec (rseq positions)) (vec (rseq values)) (reverse ts)] [positions values ts])
        last-segment (- (long (count ps)) 2)
        lerped (loop [i 0 ts (seq ts) acc []]
                 (if-not ts
                   acc
                   (let [t (double (first ts))]
                     (if (and (< i last-segment) (> t (double (ps (inc i)))))
                       (recur (inc i) ts acc)
                       (let [a (vs i) b (vs (inc i))
                             p0 (double (ps i)) p1 (double (ps (inc i)))
                             f (if (== p0 p1) 0.0 (/ (- t p0) (- p1 p0)))]
                         (recur i (next ts) (conj acc (when (and (some? a) (some? b))
                                                        (+ (double a) (* f (- (double b) (double a))))))))))))]
    (if reversed? (vec (rseq lerped)) lerped)))

(def ^:private ^:const color-chunk
  "Pixels of a sampled gradient line drawn in one color (colors change in steps this long, so the segments of a step
  join into one path)."
  4.0)

(defn- chunk-colors
  "Color of every segment of the sampled `points` of a curved gradient run: the color value interpolated (see
  [[lerp-values]]) from the run's data `positions` / `values` along axis index `a` at the middle of the segment's
  [[color-chunk]] (chunks by length along the curve)."
  [points positions values a scale theme]
  (let [lengths (vec (reductions + 0.0 (map (fn [[x0 y0] [x1 y1]] (m/hypot (- (double x1) (double x0)) (- (double y1) (double y0))))
                                            points (rest points))))
        chunk-of (mapv #(long (quot (double (lengths %)) color-chunk)) (range (dec (count points))))
        chunks (partition-by chunk-of (range (dec (count points))))
        middles (mapv (fn [segments] (let [i (long (nth segments (quot (count segments) 2)))]
                                       (* 0.5 (+ (double (nth (points i) a)) (double (nth (points (inc i)) a))))))
                      chunks)
        colors (value-colors (lerp-values positions values middles) scale theme)]
    (vec (mapcat (fn [segments color] (repeat (count segments) color)) chunks colors))))

(defn- draw-gradient!
  "Stroke a gradient line: `index-runs` of the data points `px` / `py` with color values `colors`, drawn as
  `point-runs`; curved runs take color values interpolated along `along` in [[color-chunk]] steps; bridges (`gaps`
  `:bridge`) the color of their end values."
  [canvas px py colors index-runs point-runs curved? along st gaps scales theme]
  (let [a (if (= :y along) 1 0)]
    (if curved?
      (doseq [[run points] (map vector index-runs point-runs)
              :when (> (count points) 1)]
        (polyline/draw-segments! canvas (mapv first points) (mapv second points)
                                 (chunk-colors points (mapv #(if (zero? a) (px %) (py %)) run) (mapv colors run) a
                                               (:color scales) theme)
                                 st))
      (polyline/draw-segments! canvas px py (segment-colors colors (:color scales) theme) st))
    (when (= :bridge gaps)
      (doseq [[r0 r1] (map vector index-runs (rest index-runs))
              :let [color (first (segment-colors [(colors (peek r0)) (colors (first r1))] (:color scales) theme))]]
        (polyline/stroke-runs! canvas [[[(px (peek r0)) (py (peek r0))] [(px (first r1)) (py (first r1))]]]
                               (polyline/bridge-style (assoc st :color color) (:bridge st)))))))

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

(defn stacked-band-points
  "Drawn `[upper lower]` pixel runs of every stacked line of a panel (`lines` from [[stacked-lines]], one shared x).
  Without an `:interpolator` in `opts` the data boundaries; with one, the groups' heights are sampled on one pixel
  grid (see [[cljplot2.graphics.curve/sample-columns]], signs kept) and stacked there again by `position` -
  `:wiggle` on its baseline interpolated from the data (its offsets accumulate per index) - so adjacent bands share
  their boundaries and no band turns inside out."
  [lines position scales opts]
  (let [projection (scale/projection scales)
        pixel (fn [vx vy] (let [^Vec2 p (projection vx vy)] (curve/clamp-point [(.x p) (.y p)])))]
    (if-not (:interpolator opts)
      (mapv (fn [{:keys [x y y0]}] [(mapv pixel x y) (mapv pixel x y0)]) lines)
      (let [flipped? (scale/flipped? scales)
            x-fwd (scale/forward-fn (:x scales))
            y-fwd (scale/forward-fn (:y scales))
            positions (mapv x-fwd (:x (first lines)))
            [grid hs] (curve/sample-columns positions (mapv :heights lines) opts true)
            stacks (if (= :wiggle position)
                     (let [[_ [base]] (curve/sample-columns positions [(:y0 (first lines))] opts false)]
                       (mapv (fn [ys] (mapv (fn [[^double y0 ^double y1] ^double b] [(+ y0 b) (+ y1 b)]) ys base))
                             (position/stack-values :stack hs)))
                     (position/stack-values position hs))
            point (fn [t v] (let [q (double (y-fwd v))] (curve/clamp-point (if flipped? [q t] [t q]))))]
        (mapv (fn [ys] [(mapv (fn [t [_ y1]] (point t y1)) grid ys) (mapv (fn [t [y0 _]] (point t y0)) grid ys)]) stacks)))))

(defn- draw-stacked!
  "Draw the stacked lines of a panel (see [[stacked-band-points]]): every band filled - `gradient` along `axis`, else
  `:fill` or the group colour at `:alpha` - then every upper boundary stroked; groups outside an aesthetic domain are
  stacked but not drawn."
  [canvas lines style position opts gradient axis scales theme]
  (let [bands (vec (keep (fn [[line [upper lower]]]
                           (when-let [st (group/style style (:aesthetics line) scales theme)]
                             [upper lower st]))
                         (map vector lines (stacked-band-points lines position scales opts))))]
    ;; all areas first, then the boundaries (as density): no band covers a stroke
    (doseq [[upper lower st] bands]
      (polyline/fill-area! canvas upper lower (polyline/area-color st true) gradient axis scales))
    (doseq [[upper _ st] bands]
      (polyline/stroke-runs! canvas [upper] st))))

(defn- base-run
  "The base points of `run` on the area baseline `{:baseline px :horizontal? h}`."
  [run {:keys [baseline horizontal?]}]
  (mapv (fn [[x y]] (if horizontal? [x baseline] [baseline y])) run))

(defn draw
  "Draw the lines of one panel (area fill first, if any); `nil` y values and values the scales can't place break a
  line (bridged with `:gaps :bridge`). The fill baseline is y = 0 clamped into the panel (`box`) along the y scale's
  direction, see [[cljplot2.graphics.curve/area-baseline]]; stacked lines fill their bands. `:fill-gradient` colours
  areas along `:fill-gradient-axis`."
  [{:keys [options]} {:keys [lines style]} canvas {:keys [w h]} scales env]
  (when (seq lines)
    (let [projection (scale/projection scales)
          y-scale (:y scales)
          y-horizontal? (scale/horizontal? :y y-scale)
          area {:baseline (curve/area-baseline y-scale w h) :horizontal? (not y-horizontal?)}
          opts (curve-options options (scale/flipped? scales))
          curved? (curve/curved? opts)
          gaps (:gaps options :break)
          gradient (some-> (:fill-gradient options) polyline/gradient-fn)
          axis (:fill-gradient-axis options :x)]
      (when (and curved? (:curve opts) (some :colors lines))
        (schema/fail! "A line colored along its length can't take :curve (use :interpolator)."
                      {:key :curve :value (:curve opts)}))
      (if (some :y0 lines)
        (draw-stacked! canvas lines style (:position options) opts gradient axis scales (:theme env))
        (doseq [{:keys [x y aesthetics colors]} lines
                :let [st (group/style style aesthetics scales (:theme env))]
                :when st]
          (let [points (mapv (fn [vx vy] (if (nil? vy) (Vec2. ##NaN ##NaN) (projection vx vy))) x y)
                px (mapv #(.x ^Vec2 %) points)
                py (mapv #(.y ^Vec2 %) points)
                index-runs (polyline/finite-runs px py)
                point-runs (if curved?
                             (drawn-runs index-runs px py opts aesthetics)
                             (mapv (fn [run] (mapv (fn [i] (curve/clamp-point [(px i) (py i)])) run)) index-runs))
                st (assoc st :fill (polyline/area-color st false))]
            (cond
              colors
              (do (polyline/fill-runs-to-baseline! canvas point-runs (:baseline area) (:horizontal? area) (:fill st))
                  (draw-gradient! canvas px py colors index-runs point-runs curved? (:along opts) st gaps scales (:theme env)))
              gradient
              (do (doseq [run point-runs :when (> (count run) 1)]
                    (polyline/fill-area! canvas run (base-run run area) nil gradient axis scales))
                  (curve/draw-runs! canvas point-runs (assoc st :fill :none) nil gaps))
              :else
              (curve/draw-runs! canvas point-runs st area gaps))))))))

(defn legend
  "Key glyph: a horizontal line through the middle of `box`; with an area (`:fill`, a stacked position or
  `:fill-gradient` - its middle colour) a filled square outlined in the line style, see
  [[cljplot2.graphics.polyline/key-glyph!]]."
  [{:keys [options]} attributes canvas box env]
  (let [st (merge (style options (:theme env)) (select-keys attributes [:color :dash]))
        area (if-let [g (:fill-gradient options)]
               ((polyline/gradient-fn g) 0.5)
               (polyline/area-color st (contains? stacked-positions (:position options))))]
    (polyline/key-glyph! canvas box st area)))
