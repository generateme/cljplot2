(ns cljplot2.chart.ribbon
  "Ribbon: the band between `:ymin` and `:ymax` along `:x`, per panel and group.

  Options (style defaults from theme `[:chart :ribbon]`): `:color` (the group colour), `:fill` - the band: `:color`
  (default, the group colour at `:alpha`, default 0.3), a colour (as given) or `:none`; the edge - both bounds
  stroked in `:stroke-color` (default `:none`; `:color` - the group colour) with `:stroke-width` and `:dash`.
  `:fill-gradient` (a clojure2d gradient: preset, colours or function of `t`) fills along `:fill-gradient-axis` -
  `:x` (default) or `:y`, over that scale's range; it wins over `:fill`.
  `:interpolator` (a fastmath 1D method or a function) with `:interpolator-params` smooths both bounds - each run
  fitted along x in pixels and evaluated every `:sample-step` (pixels, default 1, or `{:count n}`; see
  [[cljplot2.graphics.curve/sample-columns]]); repeated x in a run throws `ex-info {:key :interpolator}`.
  `:padding {:x [lo hi] :y [lo hi]}` (default 5% each side).
  Points are ordered by numeric or temporal x (other x in data order). A row with an invalid x is dropped (counted);
  an invalid `:ymin` or `:ymax` breaks the band there (counted); a run of one point draws nothing. `:ymin` above
  `:ymax` is filled the same as swapped.
  Groups (see [[cljplot2.data.group]]) come from ordinal aesthetics (`:color`) and `:group`.

  [[band-runs]] and [[draw-bands!]] are shared with the smooth chart."
  (:require [fastmath.core :as m]
            [cljplot2.data.rows :as data]
            [cljplot2.data.group :as group]
            [cljplot2.graphics.curve :as curve]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def band-style-entries
  "Map entries of a band's fill style: `:color`, `:fill` (`:color`, a colour or `:none`), `:alpha`."
  [(schema/optional :color schema/color)
   (schema/optional :fill [:or [:= :color] schema/color])
   (schema/optional :alpha schema/fraction)])

(def ^:private style-entries
  (conj band-style-entries
        (schema/optional :stroke-color [:or [:= :color] schema/color])
        (schema/optional :stroke-width schema/non-neg)
        (schema/nilable :dash schema/dash)))

(def schemas
  "Schemas of the ribbon chart: `:style` (theme `[:chart :ribbon]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (conj (schema/optional :interpolator schema/interpolator)
                      (schema/optional :interpolator-params [:vector any?])
                      (schema/optional :sample-step curve/sample-step)
                      (schema/optional :fill-gradient polyline/fill-gradient-schema)
                      (schema/optional :fill-gradient-axis [:enum :x :y])
                      (schema/optional :padding data/position-padding)))
   :mapping [:map [:x schema/accessor] [:ymin schema/accessor] [:ymax schema/accessor]]})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :ribbon]) (select-keys options [:color :fill :alpha :stroke-color :stroke-width :dash])))

(defn- sortable? [xs] (or (every? number? xs) (every? data/temporal? xs)))

(defn- band
  "Band of group `g`: points with a valid x (ordered by x when sortable), `nil` bounds where a value is invalid."
  [g x-of lo-of hi-of]
  (let [valid (fn [v] (when (data/valid-value? v) v))
        pts (->> (:rows g)
                 (map (fn [r] [(x-of r) (valid (lo-of r)) (valid (hi-of r))]))
                 (filter #(data/valid-value? (first %))))
        pts (if (sortable? (map first pts)) (sort-by first pts) pts)]
    {:x (mapv first pts) :ymin (mapv second pts) :ymax (mapv #(nth % 2) pts) :aesthetics (:aesthetics g)}))

(defn compute
  "Per panel: `{:bands [{:x [..] :ymin [..] :ymax [..] :aesthetics}] :style style}` (a bound `nil` where the band
  breaks); extents (y over both bounds), padding, `:na`, `:dropped` (`:x`, `:ymin`, `:ymax`)."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme] :as env}]
  (when (and (:fill-gradient-axis options) (nil? (:fill-gradient options)))
    (schema/fail! ":fill-gradient-axis needs :fill-gradient." {:key :fill-gradient-axis :value (:fill-gradient-axis options)}))
  (let [st (style options theme)
        {:keys [panels dropped]} (group/split groups mapping env)
        accessors (into {} (map (fn [k] [k (data/row-accessor k (k mapping))])) [:x :ymin :ymax])
        rows (mapcat (fn [gs] (mapcat :rows gs)) (vals panels))
        row-count (count rows)
        _ (doseq [[k f] accessors]
            (when (and (pos? row-count) (not-any? #(data/valid-value? (f %)) rows))
              (schema/fail! "No valid values for a mapped dimension - check the mapping." {:key :mapping :value k :rows row-count})))
        data (update-vals panels (fn [gs] {:bands (mapv #(band % (:x accessors) (:ymin accessors) (:ymax accessors)) gs)
                                           :style st}))]
    {:data data
     :extents (merge-with merge (group/aesthetic-extents panels (:plot-scales env))
                          (into {} (keep (fn [[panel {:keys [bands]}]]
                                           (let [ys (remove nil? (concat (mapcat :ymin bands) (mapcat :ymax bands)))]
                                             (when (seq ys)
                                               [panel {:x (data/extent :x (mapcat :x bands)) :y (data/extent :y ys)}]))))
                                data))
     :padding (merge {:x [0.05 0.05] :y [0.05 0.05]} (:padding options))
     :padding-explicit (data/explicit-padding {} options)
     :na (group/na-dims panels)
     :dropped (into dropped (map (fn [[k f]] [k (count (remove #(data/valid-value? (f %)) rows))])) accessors)}))

(defn- pixel ^Vec2 [projection x v] (if (nil? v) (Vec2. ##NaN ##NaN) (projection x v)))

(defn- smoothed
  "`[upper lower]` runs of the pair `[upper lower]` with both bounds sampled under the curve options `opts` along
  `:along` (`:x` - pixel x, `:y` - pixel y; see [[cljplot2.graphics.curve/sample-columns]])."
  [[upper lower] {:keys [along] :as opts}]
  (let [[a b] (if (= :y along) [1 0] [0 1])
        [grid [us ls]] (curve/sample-columns (mapv #(nth % a) upper) [(mapv #(nth % b) upper) (mapv #(nth % b) lower)]
                                             opts false)
        point (fn [t v] (curve/drawn-point (if (= :y along) [v t] [t v])))]
    [(mapv point grid us) (mapv point grid ls)]))

(defn band-runs
  "Paired `[upper lower]` pixel runs of a band: `xs` with the bounds `his` / `los` (`nil` breaks) placed by
  `projection`; a pair with a non-finite coordinate ends a run, runs of fewer than 2 pairs are left out. With an
  `:interpolator` in the curve options `opts` (`:along` the pixel axis of x) the bounds of every run are sampled
  (see [[smoothed]])."
  [xs los his projection opts]
  (let [upper (mapv #(pixel projection %1 %2) xs his)
        lower (mapv #(pixel projection %1 %2) xs los)
        finite? (fn [^Vec2 p] (and (m/valid-double? (.x p)) (m/valid-double? (.y p))))
        paired? (fn [i] (and (finite? (upper i)) (finite? (lower i))))
        point (fn [^Vec2 p] (curve/drawn-point [(.x p) (.y p)]))
        pairs (into [] (comp (partition-by paired?)
                             (filter #(and (paired? (first %)) (> (count %) 1)))
                             (map (fn [run] [(mapv #(point (upper %)) run) (mapv #(point (lower %)) run)])))
                    (range (count xs)))]
    (if (:interpolator opts) (mapv #(smoothed % opts) pairs) pairs)))

(defn band-fill
  "Fill of a band with style `st` (group attributes merged): `:fill :color` - the group colour at `:alpha`, a colour,
  or `:none`."
  [st]
  (polyline/area-color st false))

(defn draw-bands!
  "Draw `bands` - `{:runs [[upper lower] ..] :fill color :edge line-style}` (`:edge` `nil` - no edge): every band
  filled (`gradient` along `axis` of `scales` when given, else its `:fill`), then the edges of every band - both
  bounds - stroked."
  [canvas bands gradient axis scales]
  (doseq [{:keys [runs fill]} bands
          [upper lower] runs]
    (polyline/fill-area! canvas upper lower fill gradient axis scales))
  (doseq [{:keys [runs edge]} bands :when edge]
    (polyline/stroke-runs! canvas (mapcat identity runs) edge)))

(defn- edge-style
  "Line style of the edge of a band with style `st`: `:stroke-color` (`:color` - the group colour), `:stroke-width`,
  `:dash`; `nil` without an edge."
  [st]
  (let [color (if (= :color (:stroke-color st)) (:color st) (:stroke-color st :none))]
    (when-not (theme/none? color)
      {:color color :width (:stroke-width st 1) :dash (:dash st)})))

(defn draw
  "Draw the bands of one panel: all fills first, then the edges."
  [{:keys [options]} {:keys [bands style]} canvas _box scales env]
  (when (seq bands)
    (let [projection (scale/projection scales)
          opts (assoc (select-keys options [:interpolator :interpolator-params :sample-step])
                      :along (if (scale/flipped? scales) :y :x))
          gradient (some-> (:fill-gradient options) polyline/gradient-fn)]
      (draw-bands! canvas
                   (vec (keep (fn [{:keys [x ymin ymax aesthetics]}]
                                (when-let [st (group/style style aesthetics scales (:theme env))]
                                  {:runs (band-runs x ymin ymax projection opts) :fill (band-fill st) :edge (edge-style st)}))
                              bands))
                   gradient (:fill-gradient-axis options :x) scales))))

(defn legend
  "Key glyph: a filled square in the band colour (`:fill-gradient` - its middle colour), outlined with the edge."
  [{:keys [options]} attributes canvas box env]
  (let [st (merge (style options (:theme env)) (select-keys attributes [:color :dash]))
        area (if-let [g (:fill-gradient options)] ((polyline/gradient-fn g) 0.5) (band-fill st))]
    (polyline/key-glyph! canvas box (or (edge-style st) {:color :none :width 0}) area)))
