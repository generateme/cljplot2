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
  the radius of the node an edge points at."
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
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def schemas
  "Schemas of the segment chart: `:style` (theme `[:chart :segment]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] polyline/style-entries)
   :options (-> [:map {:closed true}]
                (into polyline/style-entries)
                (into data/common-options)
                (conj (schema/nilable :xend annotation/position)
                      (schema/nilable :yend annotation/position)
                      (schema/optional :padding data/position-padding)
                      (schema/optional :arrow arrow/schema)
                      (schema/optional :start-gap schema/non-neg)
                      (schema/optional :end-gap schema/non-neg)))
   :mapping [:map [:x schema/accessor] [:y schema/accessor]]})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :segment]) (select-keys options [:color :width :dash])))

(defn- end-accessor
  "Row accessor of the end coordinate `k` (`:xend` / `:yend`): the literal option, the mapped accessor, or the start
  accessor `start-of`; `[accessor mapped?]`."
  [k options mapping start-of]
  (cond
    (some? (get options k)) [(constantly (get options k)) false]
    (some? (get mapping k)) [(data/row-accessor k (get mapping k)) true]
    :else [start-of false]))

(defn- baseline-request
  "Padding request `[lo hi]` of one axis (see [[cljplot2.data.rows/explicit-padding]]): zero on the side where the
  literal end `end` (`nil` - none) is the axis extent's end over the panels' `extents` - stems stand on the baseline,
  as bars on 0 - else `nil` (no request); `nil` without such a side."
  [end extents]
  (let [e (data/merge-extents :end (remove nil? extents))
        at? (fn [bound] (and (some? end) (some? bound) (not= :categorical (:kind e)) (zero? (compare (if (number? end) (double end) end) bound))))
        request [(when (at? (:min e)) 0.0) (when (at? (:max e)) 0.0)]]
    (when (some some? request) request)))

(defn compute
  "Per panel: `{:segments [{:x [..] :y [..] :xend [..] :yend [..] :aesthetics}] :style style}` (a segment coloured
  along a linear / binned `:color` also has `:colors`); extents (x over x and xend, y over y and yend), padding,
  `:na`, `:dropped` (`:x`, `:y`, mapped `:xend` / `:yend`)."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme plot-scales] :as env}]
  (let [st (style options theme)
        continuous (group/continuous-color groups mapping env)
        color-of (:color-of continuous)
        [groups mapping] (if continuous [(:groups continuous) (:mapping continuous)] [groups mapping])
        sized (group/continuous :size groups mapping env)
        size-of (:value-of sized)
        gap-of (when (some? (:end-gap mapping)) (data/row-accessor :end-gap (:end-gap mapping)))
        {:keys [panels dropped]} (if sized
                                   (group/split (:groups sized) (:mapping sized) env)
                                   (group/split groups mapping env))
        x-of (data/row-accessor :x (:x mapping))
        y-of (data/row-accessor :y (:y mapping))
        [xend-of xend-mapped?] (end-accessor :xend options mapping x-of)
        [yend-of yend-mapped?] (end-accessor :yend options mapping y-of)
        accessors [x-of y-of xend-of yend-of]
        valid? (fn [r] (every? #(data/valid-value? (% r)) accessors))
        rows (mapcat (fn [gs] (mapcat :rows gs)) (vals panels))
        _ (doseq [[k f] (cond-> [[:x x-of] [:y y-of]] xend-mapped? (conj [:xend xend-of]) yend-mapped? (conj [:yend yend-of]))]
            (when (and (seq rows) (not-any? #(data/valid-value? (f %)) rows))
              (schema/fail! "No valid values for a mapped dimension - check the mapping." {:key :mapping :value k :rows (count rows)})))
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
     :padding (merge (into {} (map (fn [k] [k (mapv #(if (nil? %) 0.05 %) (get requests k [nil nil]))])) [:x :y])
                     (:padding options))
     :padding-explicit (data/explicit-padding requests options)
     :na (cond-> (group/na-dims panels) (:na? color-stats) (conj :color) (:na? size-stats) (conj :size))
     :dropped (cond-> (assoc dropped :x (invalid x-of) :y (invalid y-of))
                xend-mapped? (assoc :xend (invalid xend-of))
                yend-mapped? (assoc :yend (invalid yend-of))
                continuous (assoc :color (:dropped color-stats))
                sized (assoc :size (:dropped size-stats)))}))

(defn- segment-colors
  "Colour of every segment of a group: its row's colour value under the finalized `:color` scale (`colors` `nil` -
  the group style's colour); the theme NA colour for NA; `nil` (skipped) outside the domain."
  [colors n st scales theme]
  (if colors
    (let [na (get-in theme [:palette :na])
          color-of (if-let [sc (:color scales)] (aesthetic/attribute-fn :color sc na) (constantly na))]
      (mapv #(if (nil? %) na (color-of (double %))) colors))
    (vec (repeat n (:color st)))))

(defn- row-widths
  "Line width of every segment of a group: its row's size under the finalized `:size` scale (`sizes` `nil` - the group
  style's width; NA - the style width); `nil` (skipped) outside the domain."
  [sizes n st scales]
  (if sizes
    (let [width-of (if-let [sc (:size scales)] (aesthetic/attribute-fn :size sc (:width st)) (constantly (:width st)))]
      (mapv width-of sizes))
    (vec (repeat n (:width st)))))

(defn- gap
  "End gap of a row: its mapped gap when a finite number >= 0, else the option's (0 without one)."
  ^double [row-gap option]
  (if (and (number? row-gap) (m/valid-double? (double row-gap)) (>= (double row-gap) 0.0))
    (double row-gap)
    (double (or option 0.0))))

(defn- draw-each!
  "Draw every segment of a group on its own: its colour, width, gaps and arrows (see [[cljplot2.graphics.arrow]])."
  [canvas paths st [colors widths end-gaps options]]
  (doseq [[path color width row-gap] (map vector paths colors widths (or end-gaps (repeat nil)))
          :when (and (some? color) (some? width))]
    (arrow/draw-run! canvas path (assoc st :color color :width width)
                     [(:arrow options) (:start-gap options) (gap row-gap (:end-gap options))])))

(defn draw
  "Draw the segments of one panel, consecutive segments of one colour with one stroke setup (each segment on its own
  with per-row widths, gaps or arrows); ends the scales can't place skip the segment."
  [{:keys [options]} {:keys [segments style]} canvas _box scales env]
  (let [geometry (scale/coord-geometry scales)
        point (if geometry
                (scale/range-projection scales)
                (let [projection (scale/projection scales)] (fn [vx vy] (let [^Vec2 p (projection vx vy)] [(.x p) (.y p)]))))
        theme (:theme env)
        placed? (fn [[[x0 y0] [x1 y1]]] (every? #(Double/isFinite (double %)) [x0 y0 x1 y1]))
        ;; polar: range ends to pixels - a chord, or an arc with :arcs
        drawn (if geometry #(coord/line-path geometry % (:arcs geometry)) identity)]
    (doseq [{:keys [x y xend yend colors sizes end-gaps aesthetics]} segments
            :let [st (group/style style aesthetics scales theme)
                  st (when st (cond-> st (:size st) (-> (assoc :width (:size st)) (dissoc :size))))]
            :when st
            :when (or (not (or sizes end-gaps (:arrow options) (:start-gap options) (:end-gap options)))
                      (let [n (count x)
                            pairs (mapv (fn [vx vy vxe vye] [(point vx vy) (point vxe vye)]) x y xend yend)
                            keep? (mapv placed? pairs)
                            pick (fn [v] (vec (keep-indexed (fn [i e] (when (keep? i) e)) v)))]
                        (draw-each! canvas (mapv #(mapv curve/drawn-point (drawn %)) (filter placed? pairs)) st
                                    [(pick (segment-colors colors n st scales theme)) (pick (row-widths sizes n st scales))
                                     (some-> end-gaps pick) options])
                        false))
            same-color (partition-by second (map (fn [vx vy vxe vye color] [[(point vx vy) (point vxe vye)] color])
                                                 x y xend yend (segment-colors colors (count x) st scales theme)))
            :let [color (second (first same-color))]
            :when (some? color)]
      (polyline/stroke-runs! canvas (mapv #(mapv curve/drawn-point (drawn %)) (filter placed? (map first same-color)))
                             (assoc st :color color)))))

(defn legend
  "Key glyph: a horizontal line through the middle of `box` in the segment style."
  [{:keys [options]} attributes canvas box env]
  (polyline/key-glyph! canvas box (cond-> (merge (style options (:theme env)) (select-keys attributes [:color :dash]))
                                    (:size attributes) (assoc :width (:size attributes)))
                       :none))
