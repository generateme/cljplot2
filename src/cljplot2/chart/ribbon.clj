(ns cljplot2.chart.ribbon
  "Ribbon: the band between `:ymin` and `:ymax` along `:x`, per panel and group.

  Options (style defaults from theme `[:chart :ribbon]`): `:color` (the group colour), `:fill` - the band: `:color`
  (default, the group colour faded by `:alpha` - its own alpha times `:alpha`, default 0.3), a colour (as given) or
  `:none`; the edge - both bounds stroked in `:stroke-color` (default `:none`; `:color` - the group colour) with `:stroke-width` and `:dash`.
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

  [[cljplot2.graphics.curve/band-runs]] and [[cljplot2.graphics.curve/draw-bands!]] are shared with the smooth chart.

  Functions: [[schemas]] - the chart's schemas; [[compute]] - the bands of every panel; [[cljplot2.graphics.curve/band-runs]] - a band's
  paired pixel runs; [[cljplot2.graphics.polyline/area-color]] - a band's fill; [[cljplot2.graphics.curve/draw-bands!]] - fills, then edges; [[draw]] - one panel's
  bands; [[legend]] - the key glyph."
  (:require [cljplot2.data.rows :as data]
            [cljplot2.chart.common :as common]
            [cljplot2.data.group :as group]
            [cljplot2.graphics.curve :as curve]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private band-style-entries
  "Map entries of a band's fill style: `:color`, `:fill` (`:color`, a colour or `:none`), `:alpha`, `:pattern`,
  `:pattern-color` (see [[cljplot2.graphics.draw/pattern-style-entries]])."
  (into [(schema/optional :color {:description "band colour (a mapped :color wins)"} schema/color)
         (schema/optional :fill {:description "band fill: :color (the band colour, its own alpha times :alpha), a colour or :none"}
                          [:or [:= :color] schema/color])
         (schema/optional :alpha {:description "factor of the band colour's alpha filling :fill :color"} schema/fraction)]
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
   :options (schema/with-rules
             (-> [:map {:closed true}]
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
             common/fill-gradient-rule)
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

  Returns `{:x [..] :ymin [..] :ymax [..] :aesthetics}` of the rows with a valid x, ordered by x when sortable (see [[cljplot2.data.rows/sortable?]]); a bound is `nil` where its value is invalid (the band breaks there)."
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

  Throws `ex-info {:key :mapping}` when `:x`, `:ymin` or `:ymax` has no valid value in non-empty data (`:fill-gradient-axis` without `:fill-gradient` fails the options schema)."
  [{:keys [options] :as chart} {:keys [groups mapping]} {:keys [theme] :as env}]
  (let [st (theme/resolved-style theme chart)
        {:keys [panels dropped]} (group/split groups mapping env)
        accessors (into {} (map (fn [k] [k (data/row-accessor k (k mapping))])) [:x :ymin :ymax])
        rows (mapcat (fn [gs] (mapcat :rows gs)) (vals panels))
        _ (data/check-mapped! rows accessors)
        data (update-vals panels (fn [gs] {:bands (mapv #(band % (:x accessors) (:ymin accessors) (:ymax accessors)) gs)
                                           :style st}))]
    (into (data/padding-stats data/default-padding {} options)
          {:data data
           :extents (merge-with merge (group/aesthetic-extents panels (:plot-scales env))
                                (into {} (keep (fn [[panel {:keys [bands]}]]
                                                 (let [ys (remove nil? (concat (mapcat :ymin bands) (mapcat :ymax bands)))]
                                                   (when (seq ys)
                                                     [panel {:x (data/extent :x (mapcat :x bands)) :y (data/extent :y ys)}]))))
                                      data))
           :na (group/na-dims panels)
           :dropped (into dropped (map (fn [[k f]] [k (data/invalid-count f rows)])) accessors)})))

(defn- edge-style
  "Gives the line style of a band's edge.

  Parameters:

  - `st` (map): the band's style - `:stroke-color` (`:color` - the group colour; default `:none`), `:stroke-width` (default 1), `:dash`.

  Returns `{:color :line-width :dash}`; `nil` for no edge."
  [st]
  (let [color (if (= :color (:stroke-color st)) (:color st) (data/option st :stroke-color :none))]
    (when-not (theme/none? color)
      {:color color :line-width (data/option st :stroke-width 1) :dash (:dash st)})))

(defn draw
  "Draws the bands of one panel: all fills first, then the edges (see [[cljplot2.graphics.curve/draw-bands!]]).

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
      (curve/draw-bands! canvas
                   (vec (keep (fn [{:keys [x ymin ymax aesthetics]}]
                                (when-let [st (group/style style aesthetics scales (:theme env))]
                                  {:runs (curve/band-runs x ymax ymin projection opts) :fill (polyline/area-color st false) :edge (edge-style st)
                                   :pattern-fill (draw/pattern-fill st (if gradient (gradient 0.5) (polyline/area-color st false)))}))
                              bands))
                   {:gradient gradient :axis (data/option options :fill-gradient-axis :x) :scales scales}))))

(defn legend
  "Draws the chart's key glyph: a filled square in the band colour (`:fill-gradient` - its middle colour), outlined with the edge.

  Parameters:

  - `chart` (map): the chart value - `:options` `:fill-gradient`.
  - `attributes` (map): the key's mapped attributes - `:color`, `:dash`, `:pattern`.
  - `canvas`: the clojure2d canvas.
  - `box` (map): the key's box.
  - `env` (map): `:theme`."
  [{:keys [options] :as chart} attributes canvas box env]
  (let [st (theme/key-style env chart attributes [:color :dash :pattern])
        area (if-let [g (:fill-gradient options)] ((polyline/gradient-fn g) 0.5) (polyline/area-color st false))]
    (polyline/key-glyph! canvas box (or (edge-style st) {:color :none :line-width 0}) area (draw/pattern-fill st area))))
