(ns cljplot2.chart.function
  "Function: curves of functions sampled across the panel - `y = f(x)` (`:f`) or parametric `[x y] = f(t)`
  (`:parametric`). No data: the chart draws in every panel and inherits only the plot's facet keys (as annotations).

  * `:f` / `:parametric` (exactly one) - a function, a vector of functions, a vector of `[label function]` pairs or
    a map label -> function. Each function is a row `{:function label}` (label - the pair's or map's key, or the
    index in a vector, 0 for a single function), grouped by the chart's own `:mapping` (`{:color :function}`,
    `:dash`, `:group`); labels are levels in the given order (vector order, pair order, a map's iteration order - a
    hash map of more than 8 entries has none, use pairs), not sorted, unless the plot's scale has an explicit
    `:domain`: a mapped `:function` is declared categorical ([[categorical-labels]]), so numeric labels get ordinal
    scales - the plot's scale of that aesthetic, shared with the other charts (numeric data of another chart on it
    then needs few distinct values).
    Without a mapping every function takes the theme style.
  * `:domain [lo hi]` (`:f`) - numbers or `java.time` values; without it the plot's explicit x domain, else the x
    scale's domain at drawing (the chart then adds no extents). On a temporal domain or a `:datetime` x scale `f`
    receives a `LocalDateTime`.
  * `:t [lo hi]` (`:parametric`, required) - numbers or `java.time` values (then `f` receives a `LocalDateTime`,
    sampled evenly in elapsed time).
  * `:sample-step` - `:f`: pixels between samples along x (default 1) or `{:count n}`; `:parametric`: only
    `{:count n}` (default 500).
  * `:y-limits [lo hi]` - samples outside don't count for the y extent (the curve is only clipped).
  * `:y-extent` - y extent of the samples without `:y-limits` or an explicit plot y domain: `:quantiles` (default,
    the 1% and 99% sample quantiles), `{:quantiles [p q]}`, `:min-max` or `:none` (no y extent).
  * `:gaps` - `nil` / non-finite results break the curve: `:break` (default) or `:bridge` (see
    [[cljplot2.chart.line]]); consecutive samples beyond opposite panel edges (asymptotes) cut the curve, never
    bridged.
  * `:fill` - `:f`: the area to y = 0 (clamped into the panel); `:parametric`: the polygon the curve closes.
  * `:color`, `:width`, `:dash`, `:bridge` - style, defaults from theme `[:chart :function]`; `:padding` as line.

  Exceptions thrown by a function propagate."
  (:require [fastmath.core :as m]
            [fastmath.stats :as stats]
            [wadogo.scale :as s]
            [cljplot2.data.accessor :as accessor]
            [cljplot2.data.facet :as facet]
            [cljplot2.data.group :as group]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.curve :as curve]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private ^:const extent-samples 1000)
(def ^:private ^:const parametric-samples 500)
(def ^:private default-quantiles [0.01 0.99])

;; schema

(defn- labelled-pair? [p] (and (vector? p) (= 2 (count p)) (fn? (second p))))

(def ^:private functions
  (schema/named-type ::functions
                     #(or (fn? %) (and (vector? %) (seq %) (or (every? fn? %) (every? labelled-pair? %)))
                          (and (map? %) (seq %) (every? fn? (vals %))))
                     "should be a function, a non-empty vector of functions or of [label function] pairs, or a non-empty map label -> function"))

(defn- span-value? [v] (or (schema/finite? v) (data/temporal? v)))

(def ^:private span
  (schema/named-type ::span
                     #(and (vector? %) (= 2 (count %)) (every? span-value? %)
                           (or (every? number? %) (every? data/temporal? %))
                           (if (every? number? %) (< (double (% 0)) (double (% 1))) (not= (% 0) (% 1))))
                     "should be [lo hi]: finite numbers lo < hi, or two different java.time values"))

(def ^:private y-extent
  (schema/named-type ::y-extent
                     #(or (contains? #{:quantiles :min-max :none} %)
                          (and (map? %) (= [:quantiles] (keys %))
                               (let [q (:quantiles %)]
                                 (and (vector? q) (= 2 (count q)) (every? number? q)
                                      (<= 0.0 (double (q 0))) (< (double (q 0)) (double (q 1))) (<= (double (q 1)) 1.0)))))
                     "should be :quantiles, :min-max, :none or {:quantiles [p q]}, 0 <= p < q <= 1"))

(def ^:private style-entries
  (conj polyline/style-entries
        (schema/optional :fill {:cljplot2/section :curve :description "fill colour (or :none) of the area to y = 0 (:f) or of the polygon the curve closes (:parametric)"}
                         schema/color)
        (schema/optional :bridge {:cljplot2/section :gaps :description "style of the segments bridging gaps with :gaps :bridge: :alpha (a fraction of the line alpha), optional :width and :dash"}
                         polyline/bridge-schema)))

(def schemas
  "Schemas of the function chart: `:style` (theme `[:chart :function]`), `:options`, `:mapping` (the chart's own)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into theme/theme-style-entries)
                (conj (schema/optional :data (schema/forbidden "isn't used: a function chart samples its functions"))
                      (schema/optional :mapping {:cljplot2/section :curve :description "the chart's own mapping of the function labels (e.g. {:color :function}); only facet keys come from the plot"}
                                       data/mapping-schema)
                      (schema/optional :f {:cljplot2/section :curve :description "y = f(x): a function, a vector of functions or of [label function] pairs, or a map label -> function (this or :parametric)"}
                                       functions)
                      (schema/optional :parametric {:cljplot2/section :curve :description "[x y] = f(t): functions given as for :f (this or :f)"} functions)
                      (schema/optional :domain {:cljplot2/section :curve :description "x span [lo hi] the :f functions are sampled over: numbers or java.time values"
                                                :cljplot2/default-doc "the plot's explicit x domain, else the x scale's domain at drawing"}
                                       span)
                      (schema/optional :t {:cljplot2/section :curve :description "t span [lo hi] of :parametric (required there): numbers or java.time values"} span)
                      (schema/optional :sample-step {:cljplot2/section :curve :description "distance between samples: pixels along x or {:count n} (:parametric - only {:count n})"
                                                     :cljplot2/default-doc "1 pixel for :f, {:count 500} for :parametric"}
                                       curve/sample-step)
                      (schema/optional :y-limits {:cljplot2/section :extent :description "[lo hi] - samples outside don't count for the y extent (the curve is only clipped)"}
                                       schema/finite-interval)
                      (schema/optional :y-extent {:cljplot2/section :extent :description "y extent of the samples without :y-limits or an explicit y domain: :quantiles (1% and 99%), {:quantiles [p q]}, :min-max or :none"
                                                  :default :quantiles}
                                       y-extent)
                      (schema/optional :gaps {:cljplot2/section :gaps :description "nil or non-finite results break the curve: :break (empty space) or :bridge (a straight segment in the :bridge style)"
                                              :default :break}
                                       [:enum :break :bridge])
                      (schema/optional :padding {:cljplot2/section :extent :description "padding of the data span per position axis, {:x [lo hi] :y [lo hi]} fractions of the span"
                                                 :cljplot2/default-doc "0.05 on every side"}
                                       data/position-padding)))
   :mapping [:map
             (schema/nilable :color {:description "line colour per function label (e.g. :function)"} schema/accessor)
             (schema/nilable :dash {:description "dash style per function label"} schema/accessor)
             (schema/nilable :group {:description "groups without a visual aesthetic, each its own function"} schema/accessor)]})

(defn categorical-labels
  "`options` with every accessor `:function` of the chart's own `:mapping` marked categorical (see
  [[cljplot2.data.accessor]]): function labels are levels, also when they're numbers (vector indices)."
  [options]
  (if-let [mapping (:mapping options)]
    (assoc options :mapping (update-vals mapping #(if (= :function %) (accessor/->CategoricalAccessor :function) %)))
    options))

(defn- style
  [options theme]
  (merge (theme/chart-style theme :function options) (select-keys options [:color :width :dash :fill :bridge])))

;; input

(defn- labelled
  "`[[label f] ..]` of a function, a vector of functions or of `[label f]` pairs, or a map label -> function."
  [fs]
  (cond
    (fn? fs) [[0 fs]]
    (map? fs) (vec fs)
    (every? labelled-pair? fs) (vec fs)
    :else (vec (map-indexed vector fs))))

(defn- label-ordered-extents
  "Aesthetic `extents` (`{panel {dim extent}}`) with the dims `mapping` maps to `:function` listing the `labels` in
  their given order, marked `:ordered?` (ordinal scales keep it, see [[cljplot2.scale.aesthetic]])."
  [extents mapping labels]
  (let [dims (filter #(= :function (some-> (get mapping %) accessor/unwrap)) aesthetic/dims)
        order (vec (distinct labels))]
    (update-vals extents (fn [es] (reduce (fn [es k] (if-let [e (get es k)]
                                                         (assoc es k (assoc e :values (filterv (set (:values e)) order) :ordered? true))
                                                         es))
                                          es dims)))))

(defn- checked-input!
  "`[parametric? labelled-functions]` of the options; throws `ex-info` for a missing or doubled function, a
  parametric chart without `:t` or with a pixel `:sample-step`, `:t` / `:domain` on the wrong form."
  [{:keys [f parametric t domain sample-step]}]
  (cond
    (and (nil? f) (nil? parametric)) (schema/fail! "A function chart needs :f or :parametric." {:key :f :value nil})
    (and f parametric) (schema/fail! "A function chart takes :f or :parametric, not both." {:key :f :value f})
    (and parametric (nil? t)) (schema/fail! "A :parametric function chart needs :t [lo hi]." {:key :t :value nil})
    (and parametric (some? sample-step) (not (map? sample-step)))
    (schema/fail! "A :parametric function chart samples by count: :sample-step {:count n}." {:key :sample-step :value sample-step})
    (and parametric domain) (schema/fail! ":domain is for :f; a :parametric chart takes :t." {:key :domain :value domain})
    (and f t) (schema/fail! ":t is for :parametric; an :f chart takes :domain." {:key :t :value t}))
  [(some? parametric) (labelled (or f parametric))])

;; sampling

(def ^:private sampled-kinds
  "Continuous position scale kinds samples follow (evenly in the scale's transform)."
  #{:linear :log :pow :symlog})

(defn- span-samples
  "`n` values evenly spread over the span `[lo hi]`: numbers - evenly in the transform of the position scale spec
  `spec` (`:log`, `:pow`, `:symlog` with its `:base` / `:exponent`; else linear), or `LocalDateTime`s evenly in
  elapsed time."
  ([span n] (span-samples span n nil))
  ([[lo hi] ^long n spec]
   (let [ws (if (number? lo)
              (s/scale (get sampled-kinds (:kind spec) :linear)
                       (merge (select-keys spec [:base :exponent]) {:domain [(double lo) (double hi)] :range [0.0 1.0]}))
              (s/scale :datetime {:domain [lo hi] :range [0.0 1.0]}))
         n- (m/long-dec n)]
     (mapv (fn [^double v] (s/inverse ws (/ v n-))) (range n)))))

(defn- sample-count ^long [sample-step default] (long (if (map? sample-step) (:count sample-step) default)))

(defn- plottable [v] (when (or (schema/finite? v) (data/temporal? v)) v))

(defn- point-of
  "`[x y]` of a parametric result with plottable coordinates, else `nil`."
  [v]
  (when (and (sequential? v) (= 2 (count v)))
    (let [[x y] v] (when (and (plottable x) (plottable y)) [x y]))))

(defn- explicit-span
  "The plot's explicit domain of position scale `k` when it can be sampled (two numbers or `java.time` values)."
  [plot-scales k]
  (let [d (get-in plot-scales [k :domain])]
    (when (and (sequential? d) (= 2 (count d)) (every? span-value? d)) (vec d))))

(defn- y-extent-of
  "Extent of the y samples `ys` (plottable values): within `y-limits` - their min / max; else by `mode` (see the
  namespace doc); temporal values - their min / max; `nil` with an explicit plot y domain or nothing left."
  [ys {:keys [y-limits y-extent]} explicit-y?]
  (when-not explicit-y?
    (let [mode (or y-extent :quantiles)
          numeric? (every? number? ys)]
      (cond
        (empty? ys) nil
        y-limits (data/extent :y (filter #(<= (double (y-limits 0)) (double %) (double (y-limits 1))) ys))
        (= :none mode) nil
        (or (= :min-max mode) (not numeric?)) (data/extent :y ys)
        :else (let [[p q] (if (map? mode) (:quantiles mode) default-quantiles)
                    [lo hi] (stats/quantiles ys [p q])]
                (data/extent :y [lo hi]))))))

(defn- sampled-extents
  "Position extents of the labelled functions from samples, or `{}` when x has nothing to sample yet."
  [parametric? fs {:keys [t domain sample-step] :as options} plot-scales]
  (let [explicit-y? (some? (get-in plot-scales [:y :domain]))]
    (if parametric?
      (let [points (for [[_ f] fs v (span-samples t (sample-count sample-step parametric-samples))
                         :let [p (point-of (f v))] :when p]
                     p)
            xs (map first points)
            ey (y-extent-of (map second points) options explicit-y?)]
        (cond-> {}
          (seq xs) (assoc :x (data/extent :x xs))
          ey (assoc :y ey)))
      (if-let [d (or domain (explicit-span plot-scales :x))]
        (let [ys (for [[_ f] fs v (span-samples d (sample-count sample-step extent-samples) (:x plot-scales))
                       :let [y (plottable (f v))] :when y]
                   y)
              e (y-extent-of ys options explicit-y?)]
          (cond-> {:x (data/extent :x d)} e (assoc :y e)))
        {}))))

(defn compute
  "One panel - every panel (`facet/all-panels`): `{:functions [{:f :aesthetics}] :style :parametric? :domain :t
  :sample-step :gaps}`, the functions grouped by the chart's own mapping; position extents from samples (see the
  namespace doc), aesthetic extents of the labels."
  [{:keys [options]} {:keys [mapping]} {:keys [theme plot-scales] :as env}]
  (let [[parametric? fs] (checked-input! options)
        rows (mapv (fn [[label f]] {:function label ::f f}) fs)
        {:keys [panels dropped]} (group/split {facet/all-panels rows} (select-keys mapping (conj aesthetic/dims :group)) env)
        functions (vec (for [g (get panels facet/all-panels) row (:rows g)] {:f (::f row) :aesthetics (:aesthetics g)}))
        extents (sampled-extents parametric? fs options plot-scales)]
    {:data {facet/all-panels (merge {:functions functions :style (style options theme) :parametric? parametric?}
                                    (select-keys options [:domain :t :sample-step :gaps]))}
     :extents (merge-with merge (label-ordered-extents (group/aesthetic-extents panels plot-scales) mapping (map first fs))
                          (if (seq extents) {facet/all-panels extents} {}))
     :padding (merge {:x [0.05 0.05] :y [0.05 0.05]} (:padding options))
     :padding-explicit (data/explicit-padding {} options)
     :na (group/na-dims panels)
     :dropped dropped}))

;; drawing

(defn- jump?
  "Do consecutive pixel positions `a`, `b` jump farther than the panel `extent` (an asymptote between two samples:
  a sampled continuous curve moves less; this includes samples beyond opposite panel edges)?"
  [^double extent ^double a ^double b]
  (> (m/abs (- b a)) extent))

(defn- stub
  "Point continuing the segment from `from` to `to` past `to` by `reach` pixels (a piece's end running out of the
  panel at a cut), `nil` when the segment has no direction or isn't finite."
  [[x0 y0] [x1 y1] ^double reach]
  (let [dx (- (double x1) (double x0)) dy (- (double y1) (double y0))
        len (m/hypot dx dy)]
    (when (and (m/valid-double? len) (pos? len))
      [(+ (double x1) (* reach (/ dx len))) (+ (double y1) (* reach (/ dy len)))])))

(defn- cut-pieces
  "Points `[[x y] ..]` (NaN coordinates break) split where consecutive points jump farther than the `w` x `h`
  panel in x or y (asymptotes, see [[jump?]]); at a cut each piece runs on out of the panel along its end segment
  (see [[stub]])."
  [points ^double w ^double h]
  (let [n (count points)
        reach (+ w h)
        cut? (fn [[x0 y0] [x1 y1]] (or (jump? w x0 x1) (jump? h y0 y1)))
        point (fn [^long i] (when (< -1 i n) (points i)))]
    (loop [i 0 piece [] pieces []]
      (if (== i n)
        (conj pieces piece)
        (let [p (points i)]
          (if (and (pos? i) (cut? (points (dec i)) p))
            (let [before (some-> (point (- i 2)) (stub (points (dec i)) reach))
                  after (some-> (point (inc i)) (stub p reach))]
              (recur (inc i) (cond-> [] after (conj after) true (conj p)) (conj pieces (cond-> piece before (conj before)))))
            (recur (inc i) (conj piece p) pieces)))))))

(defn- piece-runs
  "Drawn runs of a piece: its finite runs as drawn (see [[cljplot2.graphics.curve/drawn-point]])."
  [piece]
  (let [xs (mapv first piece) ys (mapv second piece)]
    (mapv (fn [run] (mapv (fn [i] (curve/drawn-point [(xs i) (ys i)])) run)) (polyline/finite-runs xs ys))))

(defn- require-scales!
  "Throw `ex-info` unless the panel has both position scales (an `:f` chart without data elsewhere: no `:domain` and
  no explicit x domain - `{:key :domain}`; no y samples and no y data - `{:key :y-extent}`) and a continuous x scale
  to sample (categorical x - `{:key :f}`)."
  [scales]
  (cond
    (nil? (:x scales))
    (schema/fail! "A function chart needs an x domain: its :domain, an explicit {:x {:domain ..}} or another chart's x data."
                  {:key :domain :value nil})
    (nil? (:y scales))
    (schema/fail! "A function chart needs a y range: samples counted by :y-extent, an explicit {:y {:domain ..}} or another chart's y data."
                  {:key :y-extent :value nil})
    (not (scale/continuous-kinds (:kind (:x scales))))
    (schema/fail! (str "A function chart samples a continuous x scale; the plot's x scale is " (:kind (:x scales))
                       " (categorical data).")
                  {:key :f :value (:kind (:x scales))})))

(defn- f-points
  "Pixel points of `f` sampled along the x scale over `domain` (default the scale's) every `sample-step`."
  [f domain sample-step scales]
  (let [x-scale (:x scales)
        flipped? (scale/flipped? scales)
        fx (scale/forward-fn x-scale)
        fy (scale/forward-fn (:y scales))
        inv (scale/inverse-fn x-scale)
        r0 (double (first (:range x-scale)))
        r1 (double (second (:range x-scale)))
        [lo hi] (or domain (:domain x-scale))
        clamp (fn [^double p] (m/constrain p (m/min r0 r1) (m/max r0 r1)))
        p0 (double (clamp (fx lo))) p1 (double (clamp (fx hi)))]
    (when (and (m/valid-double? p0) (m/valid-double? p1) (not (== p0 p1)))
      (mapv (fn [p] (let [y (plottable (f (inv p)))
                          q (if (and y (not (data/temporal? y))) (double (fy y)) ##NaN)]
                      (if flipped? [q p] [p q])))
            (curve/sample-grid p0 p1 (or sample-step curve/default-sample-step))))))

(defn- parametric-points
  "Pixel points of the parametric `f` sampled over `t` by count."
  [f t sample-step scales]
  (let [projection (scale/projection scales)]
    (mapv (fn [v] (if-let [[x y] (point-of (f v))]
                    (let [^Vec2 p (projection x y)] [(.x p) (.y p)])
                    [##NaN ##NaN]))
          (span-samples t (sample-count sample-step parametric-samples)))))

(defn draw
  "Draw every function of the panel: sampled, cut at asymptotes, each piece filled (`:fill`), stroked and bridged
  (`:gaps :bridge`) as a line (see [[cljplot2.graphics.curve/draw-runs!]])."
  [_chart {:keys [functions style parametric? domain t sample-step gaps]} canvas {:keys [w h]} scales env]
  (when-not parametric? (require-scales! scales))
  (let [w (double w) h (double h)
        y-scale (:y scales)
        area (if parametric?
               :closed
               {:baseline (curve/area-baseline y-scale w h) :horizontal? (not (scale/horizontal? :y y-scale))})]
    (doseq [{:keys [f aesthetics]} functions
            :let [st (group/style style aesthetics scales (:theme env))]
            :when st
            piece (cut-pieces (if parametric?
                                (parametric-points f t sample-step scales)
                                (f-points f domain sample-step scales))
                              w h)]
      (curve/draw-runs! canvas (piece-runs piece) st area (or gaps :break)))))

(defn legend
  "Key glyph: a horizontal line through the middle of `box`; with `:fill` a filled square (the fill) outlined in
  the line style, see [[cljplot2.graphics.polyline/key-glyph!]]."
  [{:keys [options]} attributes canvas box env]
  (let [st (merge (style options (:theme env)) (select-keys attributes [:color :dash]))]
    (polyline/key-glyph! canvas box st (:fill st :none))))
