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

  [[band-runs]] and [[draw-bands!]] are shared with the smooth chart.

  Functions: [[schemas]] - the chart's schemas; [[compute]] - the bands of every panel; [[band-runs]] - a band's
  paired pixel runs; [[band-fill]] - a band's fill; [[draw-bands!]] - fills, then edges; [[draw]] - one panel's
  bands; [[legend]] - the key glyph."
  (:require [fastmath.core :as m]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.data.group :as group]
            [cljplot2.graphics.curve :as curve]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def band-style-entries
  "Map entries of a band's fill style: `:color`, `:fill` (`:color`, a colour or `:none`), `:alpha`, `:pattern`,
  `:pattern-color` (see [[cljplot2.graphics.draw/pattern-style-entries]])."
  (into [(schema/optional :color {:description "band colour (a mapped :color wins)"} schema/color)
         (schema/optional :fill {:description "band fill: :color (the band colour at :alpha), a colour or :none"}
                          [:or [:= :color] schema/color])
         (schema/optional :alpha {:description "fill opacity of :fill :color"} schema/fraction)]
        draw/pattern-style-entries))

(def ^:private style-entries
  (conj band-style-entries
        (schema/optional :stroke-color {:description "colour of both bounds' edges: a colour, :none or :color (the band colour)"}
                         [:or [:= :color] schema/color])
        (schema/optional :stroke-width {:description "edge width in pixels"} schema/non-neg)
        (schema/nilable :dash {:description "dash style of the edges (a mapped :dash wins)"} schema/dash)))

(def schemas
  "Schemas of the ribbon chart: `:style` (theme `[:chart :ribbon]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into theme/theme-style-entries)
                (into (remove #(= :curve (first %))) curve/options)
                (conj (schema/optional :fill-gradient {:description "clojure2d gradient (preset, colours or function of t) filling the bands; wins over :fill"}
                                       polyline/fill-gradient-schema)
                      (schema/optional :fill-gradient-axis {:description "axis the :fill-gradient runs along, over that scale's range (needs :fill-gradient)"
                                                            :cljplot2/default-doc ":x"}
                                       [:enum :x :y])
                      (schema/optional :padding {:description "padding of the data span per position axis, {:x [lo hi] :y [lo hi]} fractions of the span"
                                                 :cljplot2/default-doc "0.05 on every side"}
                                       data/position-padding)))
   :mapping [:map
             [:x {:description "x position (data units)"} schema/accessor]
             [:ymin {:description "lower bound of the band (data units)"} schema/accessor]
             [:ymax {:description "upper bound of the band (data units)"} schema/accessor]
             (schema/nilable :color {:description "band colour per group"} schema/accessor)
             (schema/nilable :dash {:description "edge dash style per group"} schema/accessor)
             (schema/nilable :pattern {:description "fill pattern per group"} schema/accessor)
             (schema/nilable :group {:description "groups without a visual aesthetic, each its own band"} schema/accessor)]})

(defn- band
  "Gives the band of one group.

  Parameters:

  - `g` (map): the group - `:rows`, `:aesthetics`.
  - `x-of`, `lo-of`, `hi-of` (functions): accessors of x, `:ymin`, `:ymax`.

  Returns `{:x [..] :ymin [..] :ymax [..] :aesthetics}` of the rows with a valid x, ordered by x when sortable (see [[sortable?]]); a bound is `nil` where its value is invalid (the band breaks there)."
  [g x-of lo-of hi-of]
  (let [valid (fn [v] (when (data/valid-value? v) v))
        points (->> (:rows g)
                    (map (fn [r] [(x-of r) (valid (lo-of r)) (valid (hi-of r))]))
                    (filter #(data/valid-value? (first %))))
        ordered (if (data/sortable? (map first points)) (sort-by first points) points)]
    {:x (mapv first ordered) :ymin (mapv second ordered) :ymax (mapv #(nth % 2) ordered) :aesthetics (:aesthetics g)}))

(defn compute
  "Computes the bands of every panel.

  Parameters:

  - `chart` (map): the chart value - `:options` (see the namespace doc).
  - `split` (map): `:groups` - panel -> rows, `:mapping` - the merged mapping.
  - `env` (map): `:theme`, `:plot-scales` (and the grouping keys of [[cljplot2.data.group/split]]).

  Returns `{:data :extents :padding :padding-explicit :na :dropped}`: per panel `{:bands [{:x [..] :ymin [..] :ymax [..] :aesthetics}] :style st}` (see [[band]]), `st` the resolved style; extents - y over both bounds; `:dropped` - invalid `:x`, `:ymin`, `:ymax` values.

  Throws `ex-info {:key :fill-gradient-axis}` for `:fill-gradient-axis` without `:fill-gradient`, `{:key :mapping}` when `:x`, `:ymin` or `:ymax` has no valid value in non-empty data."
  [{:keys [options] :as chart} {:keys [groups mapping]} {:keys [theme] :as env}]
  (when (and (:fill-gradient-axis options) (nil? (:fill-gradient options)))
    (schema/fail! ":fill-gradient-axis needs :fill-gradient." {:key :fill-gradient-axis :value (:fill-gradient-axis options)}))
  (let [st (theme/resolved-style theme chart)
        {:keys [panels dropped]} (group/split groups mapping env)
        accessors (into {} (map (fn [k] [k (data/row-accessor k (k mapping))])) [:x :ymin :ymax])
        rows (mapcat (fn [gs] (mapcat :rows gs)) (vals panels))
        _ (data/check-mapped! rows accessors)
        data (update-vals panels (fn [gs] {:bands (mapv #(band % (:x accessors) (:ymin accessors) (:ymax accessors)) gs)
                                           :style st}))]
    {:data data
     :extents (merge-with merge (group/aesthetic-extents panels (:plot-scales env))
                          (into {} (keep (fn [[panel {:keys [bands]}]]
                                           (let [ys (remove nil? (concat (mapcat :ymin bands) (mapcat :ymax bands)))]
                                             (when (seq ys)
                                               [panel {:x (data/extent :x (mapcat :x bands)) :y (data/extent :y ys)}]))))
                                data))
     :padding (merge data/default-padding (:padding options))
     :padding-explicit (data/explicit-padding {} options)
     :na (group/na-dims panels)
     :dropped (into dropped (map (fn [[k f]] [k (count (remove #(data/valid-value? (f %)) rows))])) accessors)}))

(defn- pixel
  "Gives the pixel of a band bound.

  Parameters:

  - `projection` (function): data `x`, `y` -> pixel `Vec2`.
  - `x`: the x value.
  - `v`: the bound's value; `nil` - a break.

  Returns the projected `Vec2`; a `Vec2` of `##NaN` for `nil`."
  ^Vec2 [projection x v]
  (if (nil? v) (Vec2. ##NaN ##NaN) (projection x v)))

(defn- smoothed
  "Samples both bounds of a paired run under an interpolator.

  Parameters:

  - `[upper lower]`: the run's pixel points of both bounds.
  - `opts` (map): the curve options (see [[cljplot2.graphics.curve/sample-columns]]); `:along` - `:x` (pixel x, default) or `:y`, the pixel axis of x.

  Returns `[upper lower]`, both sampled on one grid along `:along`."
  [[upper lower] {:keys [along] :as opts}]
  (let [[a b] (if (= :y along) [1 0] [0 1])
        [grid [us ls]] (curve/sample-columns (mapv #(nth % a) upper) [(mapv #(nth % b) upper) (mapv #(nth % b) lower)]
                                             opts false)
        point (fn [t v] (curve/drawn-point (if (= :y along) [v t] [t v])))]
    [(mapv point grid us) (mapv point grid ls)]))

(defn band-runs
  "Gives the paired pixel runs of a band.

  Parameters:

  - `xs` (vector): the x values.
  - `los`, `his` (vectors): the lower and upper bounds; `nil` - a break.
  - `projection` (function): data `x`, `y` -> pixel `Vec2`.
  - `opts` (map): curve options - `:interpolator` (with `:interpolator-params`, `:sample-step`) samples the bounds of every run, `:along` - the pixel axis of x (see [[smoothed]]).

  Returns a vector of `[upper lower]` runs of pixel points: a pair with a non-finite coordinate ends a run, runs of fewer than 2 pairs are left out; `[]` for none.

  Throws `ex-info {:key :interpolator}` for a repeated x in a run under an interpolator."
  [xs los his projection opts]
  (let [upper (mapv #(pixel projection %1 %2) xs his)
        lower (mapv #(pixel projection %1 %2) xs los)
        paired? (fn [i] (and (measure/finite-point? (upper i)) (measure/finite-point? (lower i))))
        point (fn [^Vec2 p] (curve/drawn-point [(.x p) (.y p)]))
        pairs (into [] (comp (partition-by paired?)
                             (filter #(and (paired? (first %)) (m/> (count %) 1)))
                             (map (fn [run] [(mapv #(point (upper %)) run) (mapv #(point (lower %)) run)])))
                    (range (count xs)))]
    (if (:interpolator opts) (mapv #(smoothed % opts) pairs) pairs)))

(defn band-fill
  "Gives the fill of a band.

  Parameters:

  - `st` (map): the band's style (group attributes merged) - `:fill` (`:color` - the group colour at `:alpha`, a colour, or `:none`), `:color`, `:alpha`.

  Returns the fill colour (or `:none`), see [[cljplot2.graphics.polyline/area-color]]."
  [st]
  (polyline/area-color st false))

(defn draw-bands!
  "Draws bands: every band filled first, then the edges of every band.

  Parameters:

  - `canvas`: the clojure2d canvas.
  - `bands` (sequence): `{:runs [[upper lower] ..] :fill color :edge line-style :pattern-fill pf}` - `:edge` `nil` for no edge, `:pattern-fill` (see [[cljplot2.graphics.draw/pattern-fill]]) `nil` for none.
  - `gradient` (function or `nil`): a fill gradient of `t`, winning over `:fill`.
  - `axis` (keyword): the scale the gradient runs along, `:x` or `:y`.
  - `scales` (map): the finalized scales.

  Fills each run with its pattern, then strokes both bounds of every band with an edge."
  [canvas bands gradient axis scales]
  (doseq [{:keys [runs fill pattern-fill]} bands
          [upper lower] runs]
    (polyline/fill-area! canvas upper lower fill gradient axis scales pattern-fill))
  (doseq [{:keys [runs edge]} bands :when edge]
    (polyline/stroke-runs! canvas (mapcat identity runs) edge)))

(defn- edge-style
  "Gives the line style of a band's edge.

  Parameters:

  - `st` (map): the band's style - `:stroke-color` (`:color` - the group colour; default `:none`), `:stroke-width` (default 1), `:dash`.

  Returns `{:color :width :dash}`; `nil` for no edge."
  [st]
  (let [color (if (= :color (:stroke-color st)) (:color st) (:stroke-color st :none))]
    (when-not (theme/none? color)
      {:color color :width (:stroke-width st 1) :dash (:dash st)})))

(defn draw
  "Draws the bands of one panel: all fills first, then the edges (see [[draw-bands!]]).

  Parameters:

  - `chart` (map): the chart value - `:options` (`:interpolator`, `:interpolator-params`, `:sample-step`, `:fill-gradient`, `:fill-gradient-axis`).
  - `computed` (map): `:bands`, `:style` (see [[compute]]).
  - `canvas`: the clojure2d canvas.
  - `scales` (map): the finalized scales.
  - `env` (map): `:theme`.

  A group whose aesthetic value is outside its scale isn't drawn."
  [{:keys [options]} {:keys [bands style]} canvas _box scales env]
  (when (seq bands)
    (let [projection (scale/projection scales)
          opts (assoc (select-keys options [:interpolator :interpolator-params :sample-step])
                      :along (if (scale/flipped? scales) :y :x))
          gradient (some-> (:fill-gradient options) polyline/gradient-fn)]
      (draw-bands! canvas
                   (vec (keep (fn [{:keys [x ymin ymax aesthetics]}]
                                (when-let [st (group/style style aesthetics scales (:theme env))]
                                  {:runs (band-runs x ymin ymax projection opts) :fill (band-fill st) :edge (edge-style st)
                                   :pattern-fill (draw/pattern-fill st (if gradient (gradient 0.5) (band-fill st)))}))
                              bands))
                   gradient (:fill-gradient-axis options :x) scales))))

(defn legend
  "Draws the chart's key glyph: a filled square in the band colour (`:fill-gradient` - its middle colour), outlined with the edge.

  Parameters:

  - `chart` (map): the chart value - `:options` `:fill-gradient`.
  - `attributes` (map): the key's mapped attributes - `:color`, `:dash`, `:pattern`.
  - `canvas`: the clojure2d canvas.
  - `box` (map): the key's box.
  - `env` (map): `:theme`."
  [{:keys [options] :as chart} attributes canvas box env]
  (let [st (merge (theme/resolved-style (:theme env) chart) (select-keys attributes [:color :dash :pattern]))
        area (if-let [g (:fill-gradient options)] ((polyline/gradient-fn g) 0.5) (band-fill st))]
    (polyline/key-glyph! canvas box (or (edge-style st) {:color :none :width 0}) area (draw/pattern-fill st area))))
