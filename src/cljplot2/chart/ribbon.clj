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
  (into [(schema/optional :color {:description "The band colour; a mapped `:color` wins." :cljplot2/example :steelblue} schema/color)
         (schema/optional :fill {:description "How the band is filled." :cljplot2/value-meanings {:color "The band colour, faded by `:alpha`." :none "No fill; only the edges, when `:stroke-color` draws them."}}
                          [:or [:= :color] schema/color])
         (schema/optional :alpha {:description "How opaque the fill is with `:fill :color`; it multiplies the band colour's alpha." :cljplot2/example 0.5} schema/fraction)]
        draw/pattern-style-entries))

(def ^:private style-entries
  (conj band-style-entries
        (schema/optional :stroke-color {:description "The colour of both edges." :cljplot2/value-meanings {:color "The band colour." :none "No edges."} :cljplot2/example :black}
                         [:or [:= :color] schema/color])
        (schema/optional :stroke-width {:description "The width of the edges in pixels." :cljplot2/example 2 :cljplot2/example-context {:stroke-color :black}} schema/non-neg)
        (schema/nilable :dash {:description "The dash style of the edges; a mapped `:dash` wins." :cljplot2/example :dashed :cljplot2/example-context {:stroke-color :black}} schema/dash)))

(def schemas
  "Schemas of the ribbon chart: `:style` (theme `[:chart :ribbon]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (schema/with-rules
             (-> [:map {:closed true}]
                 (into style-entries)
                 (into data/common-options)
                 (into theme/theme-style-entries)
                 (into (remove #(= :curve (first %))) curve/options)
                 (conj (schema/optional :fill-gradient {:description "A gradient that colours the bands instead of `:fill`." :cljplot2/default-doc "No gradient; `:fill` colours the bands." :cljplot2/example :mpl/viridis}
                                        polyline/fill-gradient-schema)
                       (schema/optional :fill-gradient-axis {:description "The axis the gradient runs along, across its scale's range; it needs `:fill-gradient`." :cljplot2/value-meanings {:x "Colours change with x." :y "Colours change with y."}
                                                             :cljplot2/default-doc "`:x`." :cljplot2/example :y :cljplot2/example-context {:fill-gradient :mpl/viridis}}
                                        [:enum :x :y])
                       (schema/optional :padding {:description "Room around the data on x and y, as fractions of the data span." :cljplot2/example {:x [0 0]}
                                                  :cljplot2/default-doc "`0.05` on every side."}
                                        data/position-padding)))
             common/fill-gradient-rule)
   :mapping [:map
             [:x {:description "The x of the band's points."} schema/accessor]
             [:ymin {:description "The lower bound of the band."} schema/accessor]
             [:ymax {:description "The upper bound of the band."} schema/accessor]
             (schema/nilable :color {:description "Splits the rows into groups, each band with its own colour."} schema/accessor)
             (schema/nilable :dash {:description "Splits the rows into groups, each band's edges with their own dash style."} schema/accessor)
             (schema/nilable :pattern {:description "Splits the rows into groups, each band with its own fill pattern."} schema/accessor)
             (schema/nilable :group {:description "Splits the rows into groups that look the same, each its own band."} schema/accessor)]})

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
