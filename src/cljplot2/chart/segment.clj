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
  explicit domain dropped, NA rows in the theme NA colour)."
  (:require [cljplot2.data.annotation :as annotation]
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
                      (schema/optional :padding data/position-padding)))
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
        {:keys [panels dropped]} (if continuous
                                   (group/split (:groups continuous) (:mapping continuous) env)
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
                                                   continuous (assoc :colors (mapv color-of rs)))))
                                             gs)
                             :style st}))
        color-stats (when continuous
                      (group/continuous-color-stats continuous (update-vals data #(mapcat :colors (:segments %))) plot-scales))
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
     :extents (merge-with merge (group/aesthetic-extents panels plot-scales) (:extents color-stats) position-extents)
     :padding (merge (into {} (map (fn [k] [k (mapv #(if (nil? %) 0.05 %) (get requests k [nil nil]))])) [:x :y])
                     (:padding options))
     :padding-explicit (data/explicit-padding requests options)
     :na (cond-> (group/na-dims panels) (:na? color-stats) (conj :color))
     :dropped (cond-> (assoc dropped :x (invalid x-of) :y (invalid y-of))
                xend-mapped? (assoc :xend (invalid xend-of))
                yend-mapped? (assoc :yend (invalid yend-of))
                continuous (assoc :color (:dropped color-stats)))}))

(defn- segment-colors
  "Colour of every segment of a group: its row's colour value under the finalized `:color` scale (`colors` `nil` -
  the group style's colour); the theme NA colour for NA; `nil` (skipped) outside the domain."
  [colors n st scales theme]
  (if colors
    (let [na (get-in theme [:palette :na])
          color-of (if-let [sc (:color scales)] (aesthetic/attribute-fn :color sc na) (constantly na))]
      (mapv #(if (nil? %) na (color-of (double %))) colors))
    (vec (repeat n (:color st)))))

(defn draw
  "Draw the segments of one panel, consecutive segments of one colour with one stroke setup; ends the scales can't
  place skip the segment."
  [_chart {:keys [segments style]} canvas _box scales env]
  (let [geometry (scale/coord-geometry scales)
        point (if geometry
                (scale/range-projection scales)
                (let [projection (scale/projection scales)] (fn [vx vy] (let [^Vec2 p (projection vx vy)] [(.x p) (.y p)]))))
        theme (:theme env)
        placed? (fn [[[x0 y0] [x1 y1]]] (every? #(Double/isFinite (double %)) [x0 y0 x1 y1]))
        ;; polar: range ends to pixels - a chord, or an arc with :arcs
        drawn (if geometry #(coord/line-path geometry % (:arcs geometry)) identity)]
    (doseq [{:keys [x y xend yend colors aesthetics]} segments
            :let [st (group/style style aesthetics scales theme)]
            :when st
            same-color (partition-by second (map (fn [vx vy vxe vye color] [[(point vx vy) (point vxe vye)] color])
                                                 x y xend yend (segment-colors colors (count x) st scales theme)))
            :let [color (second (first same-color))]
            :when (some? color)]
      (polyline/stroke-runs! canvas (mapv #(mapv curve/drawn-point (drawn %)) (filter placed? (map first same-color)))
                             (assoc st :color color)))))

(defn legend
  "Key glyph: a horizontal line through the middle of `box` in the segment style."
  [{:keys [options]} attributes canvas box env]
  (polyline/key-glyph! canvas box (merge (style options (:theme env)) (select-keys attributes [:color :dash])) :none))
