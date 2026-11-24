(ns cljplot2.chart.cloud
  "Cloud: a 3D scatter of rows at their mapped (x, y, z) in a box projected onto the panel (see
  [[cljplot2.data.space3d]]; a chart laying out its own panel: no x / y axes - the box carries three axes).

  Mapping: `:x`, `:y`, `:z` - numbers (`:z` other values or a categorical marker, or no `:z` without `:bins`, throw
  `ex-info {:key :mapping}`); `:x` / `:y` may be categorical - text, keywords, intervals, or numbers marked
  [[cljplot2.data.levels/categorical]] (levels as in [[axis-levels]], level i at position i + 1, domain `[0.5 n +
  0.5]`, a tick per level; no explicit `:domain` - `{:key :domain}`, no `:bins` - `{:key :bins}`); aesthetics
  `:color`, `:size`, `:shape` as in [[cljplot2.chart.scatter]] (scales, legends, NA). Rows with a non-finite coordinate, or a coordinate outside an explicit `:domain`, are
  dropped and counted (`:dropped {:x n :y n :z n ..}`).

  Options (point style defaults from theme `[:chart :cloud]`: `:size`, `:color`, `:stroke-color`, `:stroke-width`,
  `:shape`):

  * `:type` - `:points` (default), `:none` (no marks - only `:projections`), `:spikes` (a line from z = `:base` - default the low z limit - to each point, width
    `:stroke-width`, lattice `type = h`) or `:bars` (a box per row from `:base` to z, `:bar-size [fx fy]` (default
    `[0.8 0.8]`) of the x / y resolution - the smallest gap between distinct values; faces shaded - `:ambient`
    (default 0.4) and `:light` (default `[-0.5 0.6 -1.0]`, see [[cljplot2.data.space3d/shader]]) - outlined by
    `:stroke-color`)
  * `:view {:azimuth deg :elevation deg :distance δ}` (default 40 / 30 / 0), `:aspect [ax ay az]` (box proportions,
    default `[1 1 1]`); `:distance` > 0 - perspective as lattice's `distance` (0.2 there; the eye `L/δ` from the box
    centre, `L` the box's longest side; an eye within the sphere around the box - a schema error, any `:distance`
    below 2/√3 is fine; see [[cljplot2.data.space3d]])
  * `:box` (default true - the box: hidden edges dashed and a floor grid behind the data, visible edges over it), `:axes` (default true - ticks, labels and titles), `:axis-titles
    {:x t :y t :z t}` (default the mapped column names; `nil` - none)
  * `:reverse #{:x :y :z}` - axes running the other way (their final domains, data or explicit, reversed - see
    [[cljplot2.data.space3d/reverse-domains]]); reversing one axis mirrors the scene (turning the `:view` doesn't)
  * `:domain {:x [lo hi] :y .. :z ..}` - explicit data domains (each optional); otherwise the data's over all panels
    (bars widen x / y by half a bar), made nice ([[cljplot2.data.space3d/nice-domain]]). Several 3D charts in one plot
    need every domain explicit, so their boxes match; they're drawn in one box - the first chart's frame with the
    frame options the later charts set merged over it (see [[cljplot2.content.space3d/scene-frame]]) - all their
    marks sorted by depth together. `:base` is kept within an explicit z domain.
  * `:projections {:xy k :xz k :yz k}` (each optional) - side projections of the panel's points on the far wall of
    each plane (the wall across the third axis turned away from the viewer; none on a wall seen edge-on), drawn
    before the cloud: `k` - `:points` (each point faded), `:density` (bands of the points' 2D kernel density at
    levels shared by the panels, in the chart's `:color`) or `:contour` (lines at those levels); a panel whose points
    give no density (fewer than 2, all on a line) gets no density marks; `:projection-grid` (default 50) - grid
    points per axis of the density, `:projection-levels` (default 6) - its levels; projected points are faded by the
    style's `:projection-alpha-factor`, density bands by `:band-alpha-factor` (theme `[:chart :cloud]`)
  * `:bins [nx ny]` - a 2D histogram: the rows counted on an `nx` x `ny` grid over the x / y domains (explicit or the
    data's made nice), each non-empty cell a bar or spike at its centre with its count as z (`:type :bars` or `:spikes` only;
    `:base` default 0; bars `:bar-size` of a cell; z titled Count); `:z` isn't read, no aesthetic mapping
    (`ex-info {:key :bins}`), no `:projections`

  Facets: a box per panel, domains shared by all panels. Points, spikes and bar faces are drawn far to near.

  Functions: [[compute]] and [[chart-primitives]] - the chart contract (with [[legend]]); [[axis-levels]] - the levels of a categorical axis; [[bin-counts]] - the cells of a 2D histogram; [[projection-layers]], [[projection-primitives]] - the side projections; [[primitives]] - the points, spikes or bars of a panel; schema pieces [[schemas]], [[cljplot2.data.space3d/distance-rule]], [[projection-kinds]], [[projection-style-entries]]; option defaults [[default-projection-grid]], [[default-projection-levels]]."
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.data.cells :as cells]
            [cljplot2.chart.common :as common]
            [cljplot2.data.accessor :as accessor]
            [cljplot2.data.contour :as contour]
            [cljplot2.data.field :as field]
            [cljplot2.data.group :as group]
            [cljplot2.data.rows :as data]
            [cljplot2.data.space3d :as space]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.shape :as shape]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private projection-kinds
  "Marks of a side projection: `:points`, `:density` (KDE bands), `:contour` (KDE lines)."
  [:enum :points :density :contour])

(def ^{:private true :const true :tag 'long} default-projection-grid
  "Grid points per axis of a projection's density, unless `:projection-grid` says otherwise."
  50)

(def ^{:private true :const true :tag 'long} default-projection-levels
  "Levels of a projection's density bands / lines, unless `:projection-levels` says otherwise."
  6)

(def ^:private projection-style-entries
  "Style entries of the side projections (theme `[:chart :cloud]` only, not chart options)."
  [(schema/optional :projection-alpha-factor {:description "alpha factor of projected points (their style's colour)"} schema/fraction)
   (schema/optional :band-alpha-factor {:description "alpha factor of a projected density band (the chart's colour)"} schema/fraction)])

(def schemas
  "Schemas of the cloud chart: `:style` (theme `[:chart :cloud]`), `:options`, `:mapping` (merged)."
  {:style (-> [:map {:closed true}]
             (into common/point-style-entries)
             (into projection-style-entries))
   :options (schema/with-rules
             (-> [:map {:closed true}]
                 (into (schema/in-section :style common/point-style-entries))
                 (into data/common-options)
                 (into theme/theme-style-entries)
                 (into space/frame-options)
                 (into space/shading-options)
                 (conj (schema/optional :type {:cljplot2/section :marks :description (str "marks: :points, :spikes (a line from :base to each point), "
                                                                 ":bars (a box per row from :base to z) or :none (only :projections)")
                                               :default :points}
                                        [:enum :points :spikes :bars :none])
                       (schema/optional :base {:cljplot2/section :marks :description "z where spikes and bars start (kept within an explicit z domain)"
                                               :cljplot2/default-doc "the low z limit (0 with :bins)"}
                                        schema/finite)
                       (schema/optional :bar-size {:cljplot2/section :marks :description "bar footprint [fx fy], fractions of the x / y resolution (of a cell with :bins)"
                                                   :default [0.8 0.8]}
                                        [:tuple schema/fraction schema/fraction])
                       (schema/optional :projections {:cljplot2/section :marks :description "side projections of the points on the far walls, per plane"}
                                        [:map {:closed true}
                                         (schema/optional :xy {:description "xy wall: :points, :density (KDE bands) or :contour (KDE lines)"}
                                                          projection-kinds)
                                         (schema/optional :xz {:description "xz wall: :points, :density (KDE bands) or :contour (KDE lines)"}
                                                          projection-kinds)
                                         (schema/optional :yz {:description "yz wall: :points, :density (KDE bands) or :contour (KDE lines)"}
                                                          projection-kinds)])
                       (schema/optional :projection-grid {:cljplot2/section :marks :description "grid points per axis of a :density / :contour projection"
                                                          :default default-projection-grid}
                                        [:int {:min 2 :max 500}])
                       (schema/optional :projection-levels {:cljplot2/section :marks :description "levels of the :density bands / :contour lines of the projections"
                                                            :default default-projection-levels}
                                        [:int {:min 1 :max 100}])
                       (schema/optional :bins {:cljplot2/section :marks :description "a 2D histogram: rows counted on an [nx ny] grid over x / y, counts as z"}
                                        [:tuple [:int {:min 1 :max 1000}] [:int {:min 1 :max 1000}]])))
             [#(when (and (:bins %) (not (#{:bars :spikes} (:type %)))) "counts of :bins are drawn as :type :bars or :spikes") [:bins]]
             [#(when (and (:bins %) (seq (:projections %))) "side :projections show the rows, not :bins counts") [:projections]]
             space/distance-rule)
   :mapping [:map
             [:x {:description "x coordinate: numbers or categories"} schema/accessor]
             [:y {:description "y coordinate: numbers or categories"} schema/accessor]
             [:z {:optional true :description "z coordinate, numeric (not read with :bins)"} schema/accessor]
             (schema/nilable :color {:description "point colour"} schema/accessor)
             (schema/nilable :size {:description "point size"} schema/accessor)
             (schema/nilable :shape {:description "point shape"} schema/accessor)]})

(def ^:private position-dims
  "Position dimensions of a cloud row, in the order of its values."
  [:x :y :z])

(defn- axis-levels
  "Gives the levels of a categorical position axis of a cloud.

  Parameters:

  - `k` (keyword): the axis, `:x` or `:y`.
  - `axis-accessor`: the axis's accessor in the mapping; a categorical marker ([[cljplot2.data.levels/categorical]]) makes numbers levels.
  - `vs` (sequence): the axis's values over every panel; `nil`s and non-finite numbers (dropped rows) are ignored.

  Returns `nil` for numbers - a numeric axis - unless the accessor is marked categorical: then the distinct numbers sorted. Other values give their distinct levels in first-appearance order, sorted when they have a natural order (intervals, see [[cljplot2.data.rows/extent]]).

  Throws `ex-info {:key :mapping}` for numbers mixed with other values."
  [k axis-accessor vs]
  (let [present (remove #(or (nil? %) (and (number? %) (not (schema/finite? %)))) vs)
        numbers (filter number? present)]
    (cond
      (accessor/categorical? axis-accessor) (data/sorted-levels k present :mapping)
      (m/== (count numbers) (count present)) nil
      (seq numbers) (schema/fail! (str "3D coordinates of " k " must be all numbers or all categories.")
                                  {:key :mapping :in [:mapping k] :value (first numbers)})
      :else (:values (data/extent k present)))))

(defn- level-positions
  "Places the levels of the categorical axes of rows at their positions.

  Parameters:

  - `rows` (sequence of vectors): each `[x y ..]`, the values of one row.
  - `levels` (map): categorical axis -> its levels in order.
  - `index-of` (map): axis -> its index in a row.

  Returns a vector of the rows with every level of a categorical axis replaced by its position 1, 2, .. (as a double); `nil` stays `nil`."
  [rows levels index-of]
  (let [positions (update-vals levels #(zipmap % (iterate m/inc 1.0)))]
    (mapv (fn [row] (reduce (fn [positioned [k ps]] (let [i (index-of k)] (assoc positioned i (some-> (positioned i) ps))))
                            row positions))
          rows)))

(defn- check-mapping!
  "Checks the mapping of a cloud against its options.

  Parameters:

  - `mapping` (map): the merged mapping.
  - `bins` (`[nx ny]` or `nil`): the `:bins` option.

  Returns `nil`.

  Throws `ex-info {:key :mapping}` for no mapped `:z` without `bins` or a categorical marker on `:z`, `{:key :bins}` for `bins` with a mapped aesthetic (a count has no colour, size or shape)."
  [mapping bins]
  (when (and (not bins) (nil? (:z mapping)))
    (schema/fail! "A cloud needs a mapped :z (or :bins - row counts as z)." {:key :mapping :in [:mapping :z]}))
  (when (accessor/categorical? (:z mapping))
    (schema/fail! "A cloud's :z is numeric: no categorical marker." {:key :mapping :in [:mapping :z]}))
  (when-let [ks (and bins (seq (aesthetic/mapped-dims mapping)))]
    (schema/fail! (str ":bins counts rows per cell: no mapped " (vec ks) ".") {:key :bins :value (vec ks)})))

(defn bin-counts
  "Counts points on a grid over two domains - the cells of a 2D histogram.

  Parameters:

  - `xs`, `ys` (sequences of numbers): the points' coordinates, inside the domains.
  - `[xd yd]`: the x and y domains, each `[a b]` in either direction.
  - `[nx ny]` (longs): cells per axis.

  Returns `{:x [..] :y [..] :z [..]}` - per non-empty cell its centre and its count, cells in index order. A point on a cell's upper edge counts in the next cell, at the domain's far end in the last."
  [xs ys [xd yd] [nx ny]]
  (let [cells (fn [[^double lo ^double hi] ^long n]
                (let [w (m// (m/- hi lo) n)]
                  [(fn [v] (m/long-max (m/long-min (long (m/floor (m// (m/- (double v) lo) w))) (m/long-dec n)) 0))
                   (fn [^long i] (m/+ lo (m/* w (m/+ i 0.5))))]))
        [ix cx] (cells xd nx)
        [iy cy] (cells yd ny)
        counts (frequencies (map (fn [x y] [(ix x) (iy y)]) xs ys))
        cell-keys (sort (keys counts))]
    {:x (mapv #(cx (first %)) cell-keys)
     :y (mapv #(cy (second %)) cell-keys)
     :z (mapv counts cell-keys)}))

(def ^:private planes
  "Per projection plane its two in-plane axes and the axis across it (its far wall carries the projection)."
  {:xy [:x :y :z] :xz [:x :z :y] :yz [:y :z :x]})

(defn- plane-density
  "Samples the 2D kernel density of a panel's points on one projection plane.

  Parameters:

  - `points` (map): the panel's kept points, axis -> values.
  - `domains` (map): the box's domains, axis -> `[a b]` in either direction (sampled low to high).
  - `[a b]`: the plane's two axes.
  - `grid-points` (long): grid points per axis.

  Returns the density's grid (see [[cljplot2.data.field/sample]]); `nil` without 2 distinct values on each axis - no spread to estimate a bandwidth from."
  [points domains [a b] ^long grid-points]
  (when (and (m/>= (count (distinct (points a))) 2) (m/>= (count (distinct (points b))) 2))
    (let [{:keys [f]} (field/density (points a) (points b) {} [(scale/stat-transform nil) (scale/stat-transform nil)])]
      ;; a reversed domain (explicit or :reverse) is sampled low to high, as the grid expects
      (field/sample f [(vec (sort (domains a))) (vec (sort (domains b)))] [grid-points grid-points]))))

(defn- projection-layers
  "Builds the side projections of every panel's points.

  Parameters:

  - `projections` (map): plane (`:xy`, `:xz`, `:yz`) -> its kind (`:points`, `:density`, `:contour`).
  - `points-by-panel` (map): panel -> its kept points (axis -> values).
  - `domains` (map): the box's domains.
  - `options` (map): `:grid-points` - grid points per axis of a density; `:level-count` - the levels of the bands or lines.

  Returns `{panel {plane {:kind k :grid g :levels [..]}}}`: `:grid` - the panel's density grid (`nil` for `:points`, or a panel without a density); `:levels` - spread over the range of every panel's grid of that plane, so the panels share them.

  See also [[projection-primitives]]."
  [projections points-by-panel domains {:keys [grid-points level-count]}]
  (let [grids (into {} (for [[plane kind] projections :when (not= kind :points)]
                         [plane (update-vals points-by-panel #(plane-density % domains (planes plane) grid-points))]))
        levels (update-vals grids (fn [by-panel] (contour/levels (field/value-range (remove nil? (vals by-panel))) level-count)))]
    (into {} (map (fn [panel]
                    [panel (into {} (map (fn [[plane kind]] [plane {:kind kind :grid (get-in grids [plane panel]) :levels (get levels plane [])}]))
                                 projections)]))
          (keys points-by-panel))))

(defn compute
  "Computes the cloud's stats: the kept points of each panel in their box (the chart contract, see [[cljplot2.chart]]).

  Parameters:

  - `chart` (map): the chart value - `:kind`, `:options` (see the namespace doc), `:schema` (its style keys, see [[cljplot2.theme/resolved-style]]).
  - `split` (map): `:groups` - panel -> rows; `:mapping` - the merged mapping.
  - `env` (map): `:theme`, `:na-level?` (default true), `:plot-scales`.

  Returns `{:data {panel computed} :extents :na :dropped}`; per panel `{:points {:x [..] :y [..] :z [..] aesthetic [..]} :domains {:x :y :z} :frame f :frame-overrides o :style st :type t :base b :half [hx hy] :shading sh}` - `f` see [[cljplot2.data.space3d/frame-settings]], `st` the resolved style ([[cljplot2.theme/resolved-style]]), `hx` / `hy` half a bar in data units, `sh` the chart's `:ambient` / `:light`; with `:bins` the points are the non-empty cells ([[bin-counts]]), with `:projections` also `:projections` ([[projection-layers]]). Extents are the aesthetics' only (the chart lays out its own panel); `:dropped` - per dimension the rows with a non-finite or out-of-domain coordinate, or an aesthetic value off its scale.

  Throws `ex-info {:key :mapping}`, `{:key :bins}` or `{:key :domain}` for a mapping its options can't draw (see [[check-mapping!]], [[axis-levels]]); `{:key :mapping}` when rows exist but a position has no valid value (see [[cljplot2.data.rows/check-mapped!]])."
  [{:keys [options] :as chart} {:keys [groups mapping]} {:keys [theme na-level? plot-scales] :or {na-level? true}}]
  (let [st (theme/resolved-style theme chart)
        explicit (:domain options)
        bins (:bins options)
        _ (check-mapping! mapping bins)
        positions (if bins [:x :y] position-dims)
        npos (count positions)
        dims (into positions (aesthetic/mapped-dims mapping))
        accessors (mapv #(data/row-accessor % (mapping %)) dims)
        _ (data/check-mapped! (mapcat val groups) (map vector positions accessors))
        raw-values (update-vals groups (fn [rows] (mapv (fn [row] (mapv #(% row) accessors)) rows)))
        levels (into {} (keep (fn [[i k]] (when (#{:x :y} k)
                                            (some->> (axis-levels k (mapping k) (mapcat #(map (fn [vs] (vs i)) %) (vals raw-values)))
                                                     (vector k)))))
                     (map-indexed vector positions))
        _ (when (and bins (seq levels))
            (schema/fail! ":bins counts numeric x / y; an axis is categorical." {:key :bins :value (vec (keys levels))}))
        _ (doseq [k (keys levels) :when (get explicit k)]
            (schema/fail! (str "A categorical " k " axis has no explicit :domain (its levels are its domain).")
                          {:key :domain :in [:domain k] :value (get explicit k)}))
        values (if (seq levels) (update-vals raw-values #(level-positions % levels {:x 0 :y 1})) raw-values)
        all-values (apply concat (vals values))
        _ (doseq [[i k] (map-indexed vector positions)
                  :let [bad (first (remove #(or (nil? %) (number? %)) (map #(% i) all-values)))]
                  :when (some? bad)]
            (schema/fail! (str "3D coordinates must be numbers; " k " has " (pr-str bad) ".")
                          {:key :mapping :in [:mapping k] :value bad}))
        kept-at? (fn [^long i k v] (if (m/< i npos)
                                     (and (schema/finite? v) (space/in-domain? (get explicit k) v))
                                     (group/kept-value? k na-level? plot-scales v)))
        keep-row? (fn [vs] (every? identity (map-indexed (fn [i k] (kept-at? i k (vs i))) dims)))
        dropped (into {} (map-indexed (fn [i k] [k (count (remove #(kept-at? i k (% i)) all-values))]) dims))
        row-points (update-vals values (fn [vs] (let [kept (filterv keep-row? vs)]
                                                  (into {} (map-indexed (fn [i k] [k (mapv #(% i) kept)])) dims))))
        bin-domains (when bins
                      (into {} (map (fn [k] [k (or (k explicit) (space/data-domain (mapcat k (vals row-points)) 0.0))])) [:x :y]))
        points (if bins
                 (update-vals row-points #(bin-counts (:x %) (:y %) [(:x bin-domains) (:y bin-domains)] bins))
                 row-points)
        mark-type (data/option options :type :points)
        bars? (= mark-type :bars)
        [fx fy] (data/option options :bar-size [0.8 0.8])
        coords (fn [k] (mapcat k (vals points)))
        half-bar (fn [k ^double f n]
                   (cond bins (let [[^double a ^double b] (bin-domains k)] (m/* 0.5 f (m/abs (m// (m/- b a) (double n)))))
                         bars? (m/* 0.5 f (cells/resolution (coords k)))
                         :else 0.0))
        hx (double (half-bar :x fx (first bins)))
        hy (double (half-bar :y fy (second bins)))
        z-values (coords :z)
        z-limits (when-let [[^double za ^double zb] (:z explicit)] [(m/min za zb) (m/max za zb)])
        start (double (or (:base options) (cond z-limits (first z-limits) bins 0.0 (seq z-values) (v/mn z-values) :else 0.0)))
        ;; spikes and bars stay inside an explicit z domain
        base (if-let [[^double z-lo ^double z-hi] z-limits] (m/constrain start z-lo z-hi) start)
        level-domain (fn [k] (when-let [ls (levels k)] [0.5 (m/+ 0.5 (count ls))]))
        domains (space/reverse-domains
                 {:x (or (:x explicit) (:x bin-domains) (level-domain :x) (space/data-domain (coords :x) hx))
                  :y (or (:y explicit) (:y bin-domains) (level-domain :y) (space/data-domain (coords :y) hy))
                  :z (or (:z explicit) (space/data-domain (cond-> z-values (and (seq z-values) (#{:spikes :bars} mark-type)) (conj base)) 0.0))}
                 (:reverse options))
        frame (cond-> (space/frame-settings options mapping)
                (and bins (not (contains? options :axis-titles))) (assoc-in [:titles :z] "Count")
                (seq levels) (assoc :levels levels))
        layers (when-let [projections (not-empty (:projections options))]
                 (projection-layers projections points domains
                                    {:grid-points (data/option options :projection-grid default-projection-grid)
                                     :level-count (data/option options :projection-levels default-projection-levels)}))]
    {:data (into {} (map (fn [[panel p]]
                           [panel (cond-> {:points p :domains domains :frame frame :frame-overrides (space/frame-overrides options)
                                           :style st :type mark-type :base base :half [hx hy]
                                           :shading (select-keys options [:ambient :light])}
                                    layers (assoc :projections (layers panel)))]))
                 points)
     :extents (update-vals points (fn [p] (into {} (map (fn [k] [k (aesthetic/value-extent k (get plot-scales k) (remove nil? (p k)))]))
                                                (drop npos dims))))
     :na (set (filter (fn [k] (some #(some nil? (% k)) (vals points))) (drop npos dims)))
     :dropped dropped}))

;; drawing

(defn- point-styles
  "Gives the style of each point of a panel.

  Parameters:

  - `points` (map): the panel's points - positions and mapped aesthetic values (axis or dimension -> values).
  - `style` (map): the chart's resolved style.
  - `scales` (map): the plot's aesthetic scales.
  - `env` (map): the drawing environment (`:theme` - its `[:palette :na]` colours NA values).

  Returns a function of a point index giving its style - `style` with the point's mapped attributes; `nil` for an aesthetic value outside its scale (the point is skipped)."
  [points style scales env]
  (let [na {:color (get-in env [:theme :palette :na]) :shape (:shape style) :size (:size style)}
        mapped (vec (for [k aesthetic/dims :when (contains? points k)]
                      [k (aesthetic/attribute-fn k (get scales k) (na k)) (points k)]))]
    (if (seq mapped)
      (fn [^long i] (reduce (fn [st [k f vs]] (if-some [a (f (nth vs i))] (assoc st k a) (reduced nil))) style mapped))
      (constantly style))))

(defn- bar-faces
  "Gives the visible faces of one bar as polygon primitives.

  Parameters:

  - `point` (function): the projection, data point -> `[pixel depth]`.
  - `orientation` (function): a face's outward normal and whether it faces the eye (see [[cljplot2.data.space3d/face-orientation]]), by the face's centre.
  - `shade` (function): Lambert shading of a colour by a normal (see [[cljplot2.data.space3d/shader]]).
  - `[x y z]`: the bar's top (or bottom) point; it stands from `base` to `z`.
  - `base` (double): z where the bar starts.
  - `[hx hy]` (doubles): half the bar's footprint in data units.
  - `st` (map): the point's style - `:color` (shaded per face), `:stroke-color`, `:stroke-width`.

  Returns the faces facing the eye, each a `:polygon` shaded by its normal, all at the depth of the bar's footprint centre on the base: bars sort as wholes by where they stand (a face's own depth would rise with its height and draw a tall back bar over a short front one); a convex box's front faces never overlap, so their order within a bar doesn't matter."
  [point orientation shade [^double x ^double y ^double z] base [^double hx ^double hy] st]
  (let [lo (m/min (double base) z)
        hi (m/max (double base) z)
        at (fn [[^long ix ^long iy ^long iz]] [(if (m/zero? ix) (m/- x hx) (m/+ x hx)) (if (m/zero? iy) (m/- y hy) (m/+ y hy)) (if (m/zero? iz) lo hi)])
        stroke (draw/outline-color (:stroke-color st) (:color st))
        depth (second (point [x y base]))]
    (keep (fn [{:keys [axis side corners]}]
            (let [face (mapv at corners)
                  centre (mapv (fn [i] (m/* 0.25 (v/sum (map #(% i) face)))) [0 1 2])
                  [normal front?] (orientation axis side centre)]
              (when front?
                {:kind :polygon :points (mapv #(first (point %)) face) :depth depth
                 :fill (shade (:color st) normal) :stroke-color stroke :stroke-width (:stroke-width st)})))
          space/faces)))

(defn primitives
  "Gives the primitives of one panel's points by the chart's mark type (see [[cljplot2.content.space3d]]).

  Parameters:

  - `computed` (map): the panel's computed data - `:points`, `:type`, `:base`, `:half`, `:frame` (the scene's, with its `:domains`; bars turn their faces by it), `:shading`.
  - `point` (function): the projection, data point -> `[pixel depth]`.
  - `styles` (function): point index -> its style, `nil` - skipped (see [[point-styles]]).

  Returns a sequence of primitives: `:points` - a `:point` each; `:spikes` - a `:line` from the base, at the depth where it stands; `:bars` - the visible faces ([[bar-faces]]); `:none` - nothing."
  [{:keys [points base half frame shading] mark-type :type} point styles]
  (let [{:keys [x y z]} points
        bars? (= mark-type :bars)
        orientation (when bars? (space/face-orientation frame))
        shade (when bars? (space/shader shading))]
    (mapcat (fn [i]
              (when-let [st (styles i)]
                (let [p [(x i) (y i) (z i)]
                      [px depth] (point p)]
                  (case mark-type
                    :points [{:kind :point :at px :depth depth :style st}]
                    :none nil
                    ;; a spike sorts by where it stands, as a bar
                    :spikes (let [[b bd] (point [(x i) (y i) base])]
                              [{:kind :line :from b :to px :depth bd :color (:color st) :line-width (:stroke-width st)}])
                    :bars (bar-faces point orientation shade p base half st)))))
            (range (count x)))))

(defn projection-primitives
  "Gives the primitives of one panel's side projections on the far walls.

  Parameters:

  - `computed` (map): the panel's computed data - `:points`, `:projections` (see [[projection-layers]]), `:domains`, `:frame`, `:style` (`:color`, `:projection-alpha-factor`, `:band-alpha-factor`).
  - `point` (function): the projection, data point -> `[pixel depth]`.
  - `styles` (function): point index -> its style, `nil` - skipped.

  Returns the projections' primitives on the far wall of each plane in the view (none for a wall seen edge-on), at depth `##Inf` - drawn before the cloud: `:points` - each point's style with its colour faded by `:projection-alpha-factor`; `:density` - a region at or above each level in the chart's colour faded by `:band-alpha-factor`; `:contour` - lines at the levels in the chart's colour, 1 px."
  [{:keys [points projections domains frame style]} point styles]
  (mapcat (fn [[plane {:keys [kind grid levels]}]]
            (let [[a b across] (planes plane)]
              (when-let [side (space/far-side (:view frame) (:aspect frame) across)]
                (let [wall (nth (domains across) side)
                      clamp (fn [k value] (let [[^double lo ^double hi] (domains k)] (m/constrain (double value) (m/min lo hi) (m/max lo hi))))
                      place (fn [[u w]] (first (point (mapv {a (clamp a u) b (clamp b w) across wall} space/axes))))]
                  (case kind
                    :points (keep (fn [i] (when-let [st (styles i)]
                                            {:kind :point :at (place [((points a) i) ((points b) i)]) :depth ##Inf
                                             :style (assoc st :color (draw/faded (:color st) (:projection-alpha-factor style)) :stroke-color :none)}))
                                  (range (count (points a))))
                    :density (when grid
                               (for [level levels :let [rings (contour/level-rings grid level)] :when (seq rings)]
                                 {:kind :region :rings (mapv #(mapv place %) rings) :depth ##Inf :fill (draw/faded (:color style) (:band-alpha-factor style))}))
                    :contour (when grid
                               (for [level levels {line-points :points closed? :closed?} (contour/isolines grid level)]
                                 {:kind :path :points (mapv place line-points) :closed? closed? :depth ##Inf
                                  :color (:color style) :line-width 1.0})))))))
          projections))

(defn chart-primitives
  "Gives the chart's primitives for one panel (the 3D chart contract, see [[cljplot2.content.space3d/draw-scene!]]).

  Parameters:

  - `chart` (map): the chart value (unused).
  - `computed` (map): the panel's computed data (see [[compute]]).
  - `point` (function): the projection, data point -> `[pixel depth]`.
  - `scales` (map): the plot's aesthetic scales.
  - `env` (map): the drawing environment.

  Returns its side projections ([[projection-primitives]]), then its points, spikes or bars ([[primitives]]), styled by the aesthetic `scales`."
  [_chart {:keys [points style] :as computed} point scales env]
  (let [styles (point-styles points style scales env)]
    (concat (projection-primitives computed point styles) (primitives computed point styles))))

(defn legend
  "Draws the cloud's key glyph in a legend key box.

  Parameters:

  - `chart` (map): the chart value - its `:type` option picks the glyph; `:kind` and `:schema` give its style (see [[cljplot2.theme/resolved-style]]).
  - `attributes` (map): the key's mapped style attributes.
  - `canvas`: the clojure2d canvas.
  - `box` (map): the key box `{:x :y :w :h}` in pixels.
  - `env` (map): the drawing environment (`:theme`).

  Returns `nil`; draws the point (points), a vertical line (spikes) or a filled square (bars), styled by the chart's resolved style with the attributes over it."
  [chart attributes canvas box env]
  (let [st (theme/key-style env chart attributes nil)]
    (case (data/option (:options chart) :type :points)
      :bars (draw/bar-key! canvas box {:color (:color st) :stroke-color (:stroke-color st) :stroke-width (:stroke-width st)} false)
      :spikes (draw/spike-key! canvas box {:color (:color st) :line-width (:stroke-width st) :span [0.15 0.85]})
      (shape/draw-key! canvas (:shape st) box st))))
