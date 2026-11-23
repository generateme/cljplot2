(ns cljplot2.chart.segment
  "Segment: one straight segment per row from `(x, y)` to `(xend, yend)`, per panel and group.

  Each end coordinate is the literal option `:xend` / `:yend` (a number, a `java.time` value or a level; it wins over
  the mapping), the mapped `:xend` / `:yend`, or - both absent - the start coordinate: `(chart/segment {:yend 0})`
  with `:x :y` mapped draws stems from 0 (lollipops with a scatter; horizontal ones in a flipped plot).

  Options (style defaults from theme `[:chart :segment]`): `:color`, `:width`, `:dash`, `:xend`, `:yend`,
  `:padding {:x [lo hi] :y [lo hi]}` (default 5% each side; none on the side where a literal `:xend` / `:yend` is the
  end of the axis extent - a request that wins over other charts' default padding, so lollipop stems start at the
  axis; voided when another chart's data goes beyond the baseline, see [[cljplot2.data.rows/explicit-padding]]). A row with an invalid coordinate it needs is dropped
  (counted per key). Groups (see [[cljplot2.data.group]]) come from ordinal aesthetics (`:color`, `:dash`) and
  `:group`; a linear or binned `:color` colours each segment by its row's value (rows outside the colour scale's
  explicit domain dropped, NA rows in the theme NA colour). A mapped `:size` is the line width: per row through a
  linear size scale (NA rows in the style `:width`; the legend shows lines of those widths), per group when ordinal.

  Arrows (see [[cljplot2.graphics.arrow]]): `:arrow {:end :start :size :shape}`; `:start-gap` / `:end-gap` (pixels)
  shorten the ends, a mapped `:end-gap` (a number of pixels per row; `nil` - the option) wins over the option - e.g.
  the radius of the node an edge points at.

  Functions: [[schemas]] - the chart's schemas; [[compute]] - the segments of every panel; [[draw]] - one panel's
  segments; [[legend]] - the key glyph."
  (:require [fastmath.core :as m]
            [cljplot2.data.annotation :as annotation]
            [cljplot2.graphics.arrow :as arrow]
            [cljplot2.data.group :as group]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.curve :as curve]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.scale.coord :as coord]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def schemas
  "Schemas of the segment chart: `:style` (theme `[:chart :segment]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] polyline/style-entries)
   :options (-> [:map {:closed true}]
                (into polyline/style-entries)
                (into data/common-options)
                (into theme/theme-style-entries)
                (conj (schema/nilable :xend {:description "literal end x of every segment: a number, a java.time value or a level (wins over a mapped :xend)"
                                             :cljplot2/default-doc "the mapped :xend, else the start x"}
                                      annotation/position)
                      (schema/nilable :yend {:description "literal end y of every segment: a number, a java.time value or a level (wins over a mapped :yend)"
                                             :cljplot2/default-doc "the mapped :yend, else the start y"}
                                      annotation/position)
                      (schema/optional :padding {:description "padding of the data span per position axis, {:x [lo hi] :y [lo hi]} fractions of the span"
                                                 :cljplot2/default-doc "0.05 on every side; 0 on a side where a literal :xend / :yend is the end of the axis extent"}
                                       data/position-padding)
                      (schema/optional :arrow {:description "arrowheads at the ends of every segment"} arrow/schema)
                      (schema/optional :start-gap {:description "shortens every segment at its start, in pixels"} schema/non-neg)
                      (schema/optional :end-gap {:description "shortens every segment at its end, in pixels (a mapped :end-gap wins)"}
                                       schema/non-neg)))
   :mapping [:map
             [:x {:description "start x position (data units)"} schema/accessor]
             [:y {:description "start y position (data units)"} schema/accessor]
             (schema/nilable :xend {:description "end x position (data units)"} schema/accessor)
             (schema/nilable :yend {:description "end y position (data units)"} schema/accessor)
             (schema/nilable :end-gap {:description "end gap per row in pixels (e.g. the radius of the node an edge points at)"}
                             schema/accessor)
             (schema/nilable :color {:description "segment colour: per group (ordinal) or per row (linear or binned)"} schema/accessor)
             (schema/nilable :size {:description "line width: per row (linear) or per group (ordinal)"} schema/accessor)
             (schema/nilable :dash {:description "dash style per group"} schema/accessor)
             (schema/nilable :group {:description "groups without a visual aesthetic (one stroke setup per group)"} schema/accessor)]})

(defn- end-accessor
  "Gives the row accessor of a segment's end coordinate.

  Parameters:

  - `k` (keyword): `:xend` or `:yend`.
  - `options` (map): the chart's options - the literal end wins.
  - `mapping` (map): the merged mapping - the mapped end.
  - `start-of` (function): the start coordinate's accessor - used without either.

  Returns `[accessor mapped?]`, `mapped?` - whether the end is mapped (its invalid values are counted)."
  [k options mapping start-of]
  (cond
    (some? (get options k)) [(constantly (get options k)) false]
    (some? (get mapping k)) [(data/row-accessor k (get mapping k)) true]
    :else [start-of false]))

(defn- baseline-request
  "Gives the padding request of one axis for a literal segment end (see [[cljplot2.data.rows/explicit-padding]]).

  Parameters:

  - `end`: the literal `:xend` / `:yend`; `nil` - none.
  - `extents` (sequence): the axis's extents over the panels (`nil`s ignored).

  Returns `[lo hi]` with 0 on the side where `end` is the merged extent's end (stems stand on the baseline, as bars on 0) and `nil` on the other; `nil` without such a side or for a categorical axis."
  [end extents]
  (let [e (data/merge-extents :end (remove nil? extents))
        at? (fn [bound] (and (some? end) (some? bound) (not= :categorical (:kind e)) (m/zero? (compare (if (number? end) (double end) end) bound))))
        request [(when (at? (:min e)) 0.0) (when (at? (:max e)) 0.0)]]
    (when (some some? request) request)))

(defn compute
  "Computes the segments of every panel.

  Parameters:

  - `chart` (map): the chart value - `:options` (see the namespace doc).
  - `split` (map): `:groups` - panel -> rows, `:mapping` - the merged mapping.
  - `env` (map): `:theme`, `:plot-scales` - the plot's `:scales` (and the grouping keys of [[cljplot2.data.group/split]]).

  Returns `{:data :extents :padding :padding-explicit :na :dropped}`: per panel `{:segments [{:x [..] :y [..] :xend [..] :yend [..] :aesthetics}] :style st}` (a segment coloured along a linear / binned `:color` also has `:colors`, sized by a linear `:size` `:sizes`, with a mapped `:end-gap` `:end-gaps`), `st` the resolved style; extents - x over x and xend, y over y and yend; padding 5% except a zero request where a literal end is the extent's end (see [[baseline-request]]); `:dropped` - `:x`, `:y`, mapped `:xend` / `:yend`, `:color`, `:size`.

  Throws `ex-info {:key :mapping}` when a needed mapped dimension has no valid value in non-empty data."
  [{:keys [options] :as chart} {:keys [groups mapping]} {:keys [theme plot-scales] :as env}]
  (let [st (theme/resolved-style theme chart)
        continuous (group/continuous-color groups mapping env)
        color-of (:color-of continuous)
        [colored-groups colored-mapping] (if continuous [(:groups continuous) (:mapping continuous)] [groups mapping])
        sized (group/continuous :size colored-groups colored-mapping env)
        size-of (:value-of sized)
        gap-of (when (some? (:end-gap colored-mapping)) (data/row-accessor :end-gap (:end-gap colored-mapping)))
        {:keys [panels dropped]} (if sized
                                   (group/split (:groups sized) (:mapping sized) env)
                                   (group/split colored-groups colored-mapping env))
        x-of (data/row-accessor :x (:x colored-mapping))
        y-of (data/row-accessor :y (:y colored-mapping))
        [xend-of xend-mapped?] (end-accessor :xend options colored-mapping x-of)
        [yend-of yend-mapped?] (end-accessor :yend options colored-mapping y-of)
        accessors [x-of y-of xend-of yend-of]
        valid? (fn [r] (every? #(data/valid-value? (% r)) accessors))
        rows (mapcat (fn [gs] (mapcat :rows gs)) (vals panels))
        _ (data/check-mapped! rows (cond-> [[:x x-of] [:y y-of]] xend-mapped? (conj [:xend xend-of]) yend-mapped? (conj [:yend yend-of])))
        data (update-vals panels
                          (fn [gs]
                            {:segments (mapv (fn [g]
                                               (let [rs (filterv valid? (:rows g))]
                                                 (cond-> {:x (mapv x-of rs) :y (mapv y-of rs) :xend (mapv xend-of rs)
                                                          :yend (mapv yend-of rs) :aesthetics (:aesthetics g)}
                                                   continuous (assoc :colors (mapv color-of rs))
                                                   sized (assoc :sizes (mapv size-of rs))
                                                   gap-of (assoc :end-gaps (mapv gap-of rs)))))
                                             gs)
                             :style st}))
        color-stats (when continuous
                      (group/continuous-color-stats continuous (update-vals data #(mapcat :colors (:segments %))) plot-scales))
        size-stats (when sized
                     (group/continuous-stats :size sized (update-vals data #(mapcat :sizes (:segments %))) plot-scales))
        invalid (fn [f] (count (remove #(data/valid-value? (f %)) rows)))
        position-extents (into {} (keep (fn [[panel {:keys [segments]}]]
                                          (let [xs (concat (mapcat :x segments) (mapcat :xend segments))]
                                            (when (seq xs)
                                              [panel {:x (data/extent :x xs)
                                                      :y (data/extent :y (concat (mapcat :y segments) (mapcat :yend segments)))}]))))
                               data)
        requests (into {} (keep (fn [[k end]] (some->> (baseline-request end (map k (vals position-extents))) (vector k))))
                       {:x (:xend options) :y (:yend options)})]
    {:data data
     :extents (merge-with merge (group/aesthetic-extents panels plot-scales) (:extents color-stats) (:extents size-stats)
                          position-extents)
     :padding (merge (into {} (map (fn [k] [k (mapv (fn [request default] (if (nil? request) default request))
                                                     (get requests k [nil nil]) data/axis-padding)]))
                           [:x :y])
                     (:padding options))
     :padding-explicit (data/explicit-padding requests options)
     :na (cond-> (group/na-dims panels) (:na? color-stats) (conj :color) (:na? size-stats) (conj :size))
     :dropped (cond-> (assoc dropped :x (invalid x-of) :y (invalid y-of))
                xend-mapped? (assoc :xend (invalid xend-of))
                yend-mapped? (assoc :yend (invalid yend-of))
                continuous (assoc :color (:dropped color-stats))
                sized (assoc :size (:dropped size-stats)))}))

(defn- segment-colors
  "Gives the colour of every segment of a group.

  Parameters:

  - `colors` (sequence or `nil`): the rows' linear / binned colour values; `nil` - the group style's colour for all.
  - `n` (long): the segment count.
  - `st` (map): the group style.
  - `scales` (map): the finalized scales - `:color`.
  - `theme` (map): the merged theme - `[:palette :na]`.

  Returns a sequence of `n` colours: a value's colour under the colour scale, the NA colour for `nil`, `nil` (the segment skipped) outside the scale's domain."
  [colors n st scales theme]
  (if colors
    (let [na (get-in theme [:palette :na])
          color-of (if-let [sc (:color scales)] (aesthetic/attribute-fn :color sc na) (constantly na))]
      (map #(if (nil? %) na (color-of %)) colors))
    (repeat n (:color st))))

(defn- row-widths
  "Gives the line width of every segment of a group.

  Parameters:

  - `sizes` (sequence or `nil`): the rows' linear size values; `nil` - the group style's width for all.
  - `n` (long): the segment count.
  - `st` (map): the group style - `:width` (also for NA).
  - `scales` (map): the finalized scales - `:size`.

  Returns a sequence of `n` widths; `nil` (the segment skipped) for a value outside the size scale's domain."
  [sizes n st scales]
  (if sizes
    (let [width-of (if-let [sc (:size scales)] (aesthetic/attribute-fn :size sc (:width st)) (constantly (:width st)))]
      (map width-of sizes))
    (repeat n (:width st))))

(defn- gap
  "Gives the end gap of a segment.

  Parameters:

  - `row-gap`: the row's mapped gap (pixels); used when a finite number >= 0.
  - `option`: the `:end-gap` option; `nil` - 0.

  Returns the gap in pixels."
  ^double [row-gap option]
  (if (and (number? row-gap) (m/valid-double? (double row-gap)) (m/not-neg? (double row-gap)))
    (double row-gap)
    (double (or option 0.0))))

(defn- draw-each!
  "Draws every segment of a group on its own, with its colour, width, gaps and arrows (see [[cljplot2.graphics.arrow]]).

  Parameters:

  - `canvas`: the clojure2d canvas.
  - `paths` (sequence): the segments' pixel paths.
  - `st` (map): the group style.
  - `[colors widths end-gaps options]`: per segment colours and widths (a `nil` one - skipped), mapped end gaps (`nil` - none), and the chart's options (`:arrow`, `:start-gap`, `:end-gap`)."
  [canvas paths st [colors widths end-gaps options]]
  (doseq [[path color width row-gap] (map vector paths colors widths (or end-gaps (repeat nil)))
          :when (and (some? color) (some? width))]
    (arrow/draw-run! canvas path (assoc st :color color :width width)
                     [(:arrow options) (:start-gap options) (gap row-gap (:end-gap options))])))

(defn draw
  "Draws the segments of one panel.

  Parameters:

  - `chart` (map): the chart value - `:options` (`:arrow`, `:start-gap`, `:end-gap`).
  - `computed` (map): `:segments`, `:style` (see [[compute]]).
  - `canvas`: the clojure2d canvas.
  - `scales` (map): the finalized scales.
  - `env` (map): `:theme`.

  Consecutive segments of one colour are stroked with one stroke setup; with per-row widths, gaps or arrows each segment is drawn on its own (polar: chords, arcs with `:arcs`). A segment with an end the scales can't place is skipped."
  [{:keys [options]} {:keys [segments style]} canvas _box scales env]
  (let [geometry (scale/coord-geometry scales)
        point (if geometry
                (scale/range-projection scales)
                (let [projection (scale/projection scales)] (fn [vx vy] (let [^Vec2 p (projection vx vy)] [(.x p) (.y p)]))))
        theme (:theme env)
        placed? (fn [[[^double x0 ^double y0] [^double x1 ^double y1]]]
                  (and (m/valid-double? x0) (m/valid-double? y0) (m/valid-double? x1) (m/valid-double? y1)))
        ;; polar: range ends to pixels - a chord, or an arc with :arcs
        drawn (if geometry #(coord/line-path geometry % (:arcs geometry)) identity)]
    (doseq [{:keys [x y xend yend colors sizes end-gaps aesthetics]} segments
            :let [group-st (group/style style aesthetics scales theme)
                  st (when group-st (cond-> group-st (:size group-st) (-> (assoc :width (:size group-st)) (dissoc :size))))]
            :when st
            :when (or (not (or sizes end-gaps (:arrow options) (:start-gap options) (:end-gap options)))
                      (let [n (count x)
                            pairs (mapv (fn [vx vy vxe vye] [(point vx vy) (point vxe vye)]) x y xend yend)
                            keep? (mapv placed? pairs)
                            pick (fn [v] (keep-indexed (fn [i e] (when (keep? i) e)) v))]
                        (draw-each! canvas (map #(mapv curve/drawn-point (drawn %)) (filter placed? pairs)) st
                                    [(pick (segment-colors colors n st scales theme)) (pick (row-widths sizes n st scales))
                                     (some-> end-gaps pick) options])
                        false))
            same-color (partition-by second (map (fn [vx vy vxe vye color] [[(point vx vy) (point vxe vye)] color])
                                                 x y xend yend (segment-colors colors (count x) st scales theme)))
            :let [color (second (first same-color))]
            :when (some? color)]
      (polyline/stroke-runs! canvas (map #(mapv curve/drawn-point (drawn %)) (filter placed? (map first same-color)))
                             (assoc st :color color)))))

(defn legend
  "Draws the chart's key glyph: a horizontal line through the middle of the key's box in the segment style.

  Parameters:

  - `chart` (map): the chart value.
  - `attributes` (map): the key's mapped attributes - `:color`, `:dash`, `:size` (the line width).
  - `canvas`: the clojure2d canvas.
  - `box` (map): the key's box.
  - `env` (map): `:theme`."
  [chart attributes canvas box env]
  (polyline/key-glyph! canvas box (cond-> (merge (theme/resolved-style (:theme env) chart) (select-keys attributes [:color :dash]))
                                    (:size attributes) (assoc :width (:size attributes)))
                       :none))
