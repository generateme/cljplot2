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
    `:stroke-width`, lattice `type = \"h\"`) or `:bars` (a box per row from `:base` to z, `:bar-size [fx fy]` (default
    `[0.8 0.8]`) of the x / y resolution - the smallest gap between distinct values; faces shaded, outlined by
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
    frame options the later charts set merged over it (see [[cljplot2.graphics.space3d/scene-frame]]) - all their
    marks sorted by depth together. `:base` is kept within an explicit z domain.
  * `:projections {:xy k :xz k :yz k}` (each optional) - side projections of the panel's points on the far wall of
    each plane (the wall across the third axis turned away from the viewer; none on a wall seen edge-on), drawn
    before the cloud: `k` - `:points` (each point faded), `:density` (bands of the points' 2D kernel density at
    levels shared by the panels, in the chart's `:color`) or `:contour` (lines at those levels); a panel whose points
    give no density (fewer than 2, all on a line) gets no density marks
  * `:bins [nx ny]` - a 2D histogram: the rows counted on an `nx` x `ny` grid over the x / y domains (explicit or the
    data's made nice), each non-empty cell a bar or spike at its centre with its count as z (`:type :bars` or `:spikes` only;
    `:base` default 0; bars `:bar-size` of a cell; z titled \"Count\"); `:z` isn't read, no aesthetic mapping
    (`ex-info {:key :bins}`), no `:projections`

  Facets: a box per panel, domains shared by all panels. Points, spikes and bar faces are drawn far to near."
  (:require [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.chart.scatter :as scatter]
            [cljplot2.chart.tile :as tile]
            [cljplot2.data.accessor :as accessor]
            [cljplot2.data.contour :as contour]
            [cljplot2.data.field :as field]
            [cljplot2.data.rows :as data]
            [cljplot2.data.space3d :as space]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.shape :as shape]
            [cljplot2.graphics.space3d :as space-draw]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def distance-rule
  "Rule (see [[cljplot2.schema/with-rules]]) of the 3D charts' options: a `:view` `:distance` putting the eye within
  the sphere around the box of `:aspect` is an error at `[:view :distance]`
  ([[cljplot2.data.space3d/distance-problem]])."
  [#(space/distance-problem (:view %) (:aspect %)) [:view :distance]])

(def projection-kinds
  "Marks of a side projection: `:points`, `:density` (KDE bands), `:contour` (KDE lines)."
  [:enum :points :density :contour])

(def schemas
  "Schemas of the cloud chart: `:style` (theme `[:chart :cloud]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] scatter/style-entries)
   :options (schema/with-rules
             (-> [:map {:closed true}]
                 (into scatter/style-entries)
                 (into data/common-options)
                 (into space/frame-options)
                 (conj (schema/optional :type {:description (str "marks: :points, :spikes (a line from :base to each point), "
                                                                 ":bars (a box per row from :base to z) or :none (only :projections)")
                                               :default :points}
                                        [:enum :points :spikes :bars :none])
                       (schema/optional :base {:description "z where spikes and bars start (kept within an explicit z domain)"
                                               :cljplot2/default-doc "the low z limit (0 with :bins)"}
                                        schema/finite)
                       (schema/optional :bar-size {:description "bar footprint [fx fy], fractions of the x / y resolution (of a cell with :bins)"
                                                   :default [0.8 0.8]}
                                        [:tuple schema/fraction schema/fraction])
                       (schema/optional :projections {:description "side projections of the points on the far walls, per plane"}
                                        [:map {:closed true}
                                         (schema/optional :xy {:description "xy wall: :points, :density (KDE bands) or :contour (KDE lines)"}
                                                          projection-kinds)
                                         (schema/optional :xz {:description "xz wall: :points, :density (KDE bands) or :contour (KDE lines)"}
                                                          projection-kinds)
                                         (schema/optional :yz {:description "yz wall: :points, :density (KDE bands) or :contour (KDE lines)"}
                                                          projection-kinds)])
                       (schema/optional :bins {:description "a 2D histogram: rows counted on an [nx ny] grid over x / y, counts as z"}
                                        [:tuple [:int {:min 1 :max 1000}] [:int {:min 1 :max 1000}]])))
             [#(when (and (:bins %) (not (#{:bars :spikes} (:type %)))) "counts of :bins are drawn as :type :bars or :spikes") [:bins]]
             [#(when (and (:bins %) (seq (:projections %))) "side :projections show the rows, not :bins counts") [:projections]]
             distance-rule)
   :mapping [:map
             [:x {:description "x coordinate: numbers or categories"} schema/accessor]
             [:y {:description "y coordinate: numbers or categories"} schema/accessor]
             [:z {:optional true :description "z coordinate, numeric (not read with :bins)"} schema/accessor]
             (schema/nilable :color {:description "point colour"} schema/accessor)
             (schema/nilable :size {:description "point size"} schema/accessor)
             (schema/nilable :shape {:description "point shape"} schema/accessor)]})

(def ^:private position-dims [:x :y :z])

(defn- style
  [options theme]
  (merge (get-in theme [:chart :cloud]) (select-keys options [:size :color :stroke-color :stroke-width :shape])))

(defn axis-levels
  "Levels of the position axis `k` (`:x` / `:y`) from its values `vs` over every panel (`nil`s and non-finite numbers
  ignored - dropped rows): `nil` for numbers (a numeric axis) unless `accessor` is marked categorical
  ([[cljplot2.data.levels/categorical]]) - then the distinct numbers sorted; other values distinct in first-appearance order, sorted when they have a natural order
  (intervals, see [[cljplot2.data.rows/extent]]). Numbers mixed with other values throw `ex-info {:key :mapping}`."
  [k accessor vs]
  (let [vs (remove #(or (nil? %) (and (number? %) (not (m/valid-double? (double %))))) vs)
        numbers (filter number? vs)]
    (cond
      (accessor/categorical? accessor) (data/sorted-levels k vs :mapping)
      (= (count numbers) (count vs)) nil
      (seq numbers) (schema/fail! (str "3D coordinates of " k " must be all numbers or all categories.")
                                  {:key :mapping :in [:mapping k] :value (first numbers)})
      :else (:values (data/extent k vs)))))

(defn- level-positions
  "Rows of values `vs` (`[x y ..]`) with the levels of each categorical axis (`levels` - `{k [level ..]}`, `k` at
  index `(index-of k)`) replaced by their positions 1, 2, .. (`nil` stays `nil`)."
  [vs levels index-of]
  (let [positions (update-vals levels (fn [ls] (zipmap ls (map #(double (inc (long %))) (range)))))]
    (mapv (fn [row] (reduce (fn [row [k ps]] (let [i (index-of k)] (assoc row i (some-> (row i) ps)))) row positions)) vs)))

(defn- check-mapping!
  "Throw `ex-info {:key :mapping}` for a cloud without a mapped `:z` (unless `bins`) or with a categorical marker on
  `:z`, `{:key :bins}` for `bins` with a mapped aesthetic (a count has no color, size or shape)."
  [mapping bins]
  (when (and (not bins) (nil? (:z mapping)))
    (schema/fail! "A cloud needs a mapped :z (or :bins - row counts as z)." {:key :mapping :in [:mapping :z]}))
  (when (accessor/categorical? (:z mapping))
    (schema/fail! "A cloud's :z is numeric: no categorical marker." {:key :mapping :in [:mapping :z]}))
  (when-let [ks (and bins (seq (aesthetic/mapped-dims mapping)))]
    (schema/fail! (str ":bins counts rows per cell: no mapped " (vec ks) ".") {:key :bins :value (vec ks)})))

(defn bin-counts
  "`{:x [..] :y [..] :z [..]}` - per non-empty cell of the `[nx ny]` grid over the domains `[xd yd]` (`[a b]` each,
  either direction) its centre and its count of the points `xs`, `ys` (inside the domains): a point on a cell's
  upper edge counts in the next cell, at the domain's far end in the last."
  [xs ys [xd yd] [nx ny]]
  (let [cells (fn [[a b] n]
                (let [a (double a) n (long n) w (/ (- (double b) a) n)]
                  [(fn [v] (long (m/constrain (long (m/floor (/ (- (double v) a) w))) 0 (dec n))))
                   (fn [i] (+ a (* w (+ (double i) 0.5))))]))
        [ix cx] (cells xd nx)
        [iy cy] (cells yd ny)
        counts (frequencies (map (fn [x y] [(ix x) (iy y)]) xs ys))
        keys (sort (keys counts))]
    {:x (mapv #(cx (first %)) keys)
     :y (mapv #(cy (second %)) keys)
     :z (mapv counts keys)}))

(def ^:private planes
  "Per projection plane its two in-plane axes and the axis across it (its far wall carries the projection)."
  {:xy [:x :y :z] :xz [:x :z :y] :yz [:y :z :x]})

(def ^:const projection-grid
  "Grid points per axis of a projection's density."
  50)

(def ^:const projection-levels
  "Levels of a projection's density bands / lines, shared by the panels."
  6)

(defn- plane-density
  "Grid (see [[cljplot2.data.field/sample]]) of the 2D kernel density of a panel's `points` on the axes `a`, `b`,
  sampled over their box `domains` (low to high, either direction); `nil` without 2 distinct values on each axis (no spread to estimate a bandwidth
  from)."
  [points domains [a b]]
  (when (and (>= (count (distinct (points a))) 2) (>= (count (distinct (points b))) 2))
    (let [{:keys [f]} (field/density (points a) (points b) {} [(scale/stat-transform nil) (scale/stat-transform nil)])]
      ;; a reversed domain (explicit or :reverse) is sampled low to high, as the grid expects
      (field/sample f [(vec (sort (domains a))) (vec (sort (domains b)))] [projection-grid projection-grid]))))

(defn projection-layers
  "`{panel {plane {:kind k :grid g :levels [..]}}}` of the side `projections` (`{plane kind}`) of each panel's kept
  `points` (`{panel points}`) in the box `domains`: density grids (`nil` for `:points`, or a panel without a density)
  and levels spread over the range of every panel's grid of that plane."
  [projections points-by-panel domains]
  (let [grids (into {} (for [[plane kind] projections :when (not= kind :points)]
                         [plane (update-vals points-by-panel #(plane-density % domains (planes plane)))]))
        levels (update-vals grids (fn [by-panel] (contour/levels (field/value-range (remove nil? (vals by-panel))) projection-levels)))]
    (into {} (map (fn [panel]
                    [panel (into {} (map (fn [[plane kind]] [plane {:kind kind :grid (get-in grids [plane panel]) :levels (get levels plane [])}]))
                                 projections)]))
          (keys points-by-panel))))

(defn compute
  "Per panel: `{:points {:x [..] :y [..] :z [..] aesthetic [..]} :domains {:x :y :z} :frame f :style st :type t :base b
  :half [hx hy]}` (`f` - see [[cljplot2.data.space3d/frame-settings]], `hx` / `hy` - half a bar in data units; with `:bins` the points are
  the non-empty cells, see [[bin-counts]]; with `:projections` also `:projections`, see [[projection-layers]]);
  aesthetic extents only (no position extents: the chart lays out its panel), `:na`, `:dropped` per dimension."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme na-level? plot-scales] :or {na-level? true}}]
  (let [st (style options theme)
        explicit (:domain options)
        bins (:bins options)
        _ (check-mapping! mapping bins)
        positions (if bins [:x :y] position-dims)
        npos (count positions)
        dims (into positions (aesthetic/mapped-dims mapping))
        accessors (mapv #(data/row-accessor % (mapping %)) dims)
        values (update-vals groups (fn [rows] (mapv (fn [row] (mapv #(% row) accessors)) rows)))
        levels (into {} (keep (fn [[i k]] (when (#{:x :y} k)
                                            (some->> (axis-levels k (mapping k) (mapcat #(map (fn [vs] (vs i)) %) (vals values)))
                                                     (vector k)))))
                     (map-indexed vector positions))
        _ (when (and bins (seq levels))
            (schema/fail! ":bins counts numeric x / y; an axis is categorical." {:key :bins :value (vec (keys levels))}))
        _ (doseq [k (keys levels) :when (get explicit k)]
            (schema/fail! (str "A categorical " k " axis has no explicit :domain (its levels are its domain).")
                          {:key :domain :in [:domain k] :value (get explicit k)}))
        values (if (seq levels) (update-vals values #(level-positions % levels {:x 0 :y 1})) values)
        all-values (apply concat (vals values))
        _ (doseq [[i k] (map-indexed vector positions)
                  :let [bad (first (remove #(or (nil? %) (number? %)) (map #(% i) all-values)))]
                  :when (some? bad)]
            (schema/fail! (str "3D coordinates must be numbers; " k " has " (pr-str bad) ".")
                          {:key :mapping :in [:mapping k] :value bad}))
        kept-at? (fn [i k v] (if (< (long i) npos)
                               (and (number? v) (m/valid-double? (double v)) (space/in-domain? (get explicit k) v))
                               (scatter/kept? k na-level? plot-scales v)))
        keep-row? (fn [vs] (every? true? (map-indexed (fn [i k] (kept-at? i k (vs i))) dims)))
        dropped (into {} (map-indexed (fn [i k] [k (count (remove #(kept-at? i k (% i)) all-values))]) dims))
        points (update-vals values (fn [vs] (let [kept (filterv keep-row? vs)]
                                              (into {} (map-indexed (fn [i k] [k (mapv #(% i) kept)])) dims))))
        bin-domains (when bins
                      (into {} (map (fn [k] [k (or (k explicit) (space/data-domain (mapcat k (vals points)) 0.0))])) [:x :y]))
        points (if bins
                 (update-vals points #(bin-counts (:x %) (:y %) [(:x bin-domains) (:y bin-domains)] bins))
                 points)
        type (get options :type :points)
        bars? (= type :bars)
        [fx fy] (get options :bar-size [0.8 0.8])
        coords (fn [k] (mapcat k (vals points)))
        half-bar (fn [k ^double f n]
                   (cond bins (let [[a b] (bin-domains k)] (* 0.5 f (m/abs (/ (- (double b) (double a)) (double n)))))
                         bars? (* 0.5 f (tile/resolution (coords k)))
                         :else 0.0))
        half {:x (half-bar :x fx (first bins)) :y (half-bar :y fy (second bins))}
        z-values (coords :z)
        z-limits (when-let [[a b] (:z explicit)] [(m/min (double a) (double b)) (m/max (double a) (double b))])
        base (double (or (:base options) (cond z-limits (first z-limits) bins 0.0 (seq z-values) (reduce min z-values) :else 0.0)))
        ;; spikes and bars stay inside an explicit z domain
        base (if z-limits (m/constrain base (double (first z-limits)) (double (second z-limits))) base)
        level-domain (fn [k] (when-let [ls (levels k)] [0.5 (+ 0.5 (count ls))]))
        domains (space/reverse-domains
                 {:x (or (:x explicit) (:x bin-domains) (level-domain :x) (space/data-domain (coords :x) (double (half :x))))
                  :y (or (:y explicit) (:y bin-domains) (level-domain :y) (space/data-domain (coords :y) (double (half :y))))
                  :z (or (:z explicit) (space/data-domain (cond-> (vec z-values) (and (seq z-values) (#{:spikes :bars} type)) (conj base)) 0.0))}
                 (:reverse options))
        frame (cond-> (space/frame-settings options mapping)
                (and bins (not (contains? options :axis-titles))) (assoc-in [:titles :z] "Count")
                (seq levels) (assoc :levels levels))
        layers (when-let [projections (not-empty (:projections options))] (projection-layers projections points domains))]
    {:data (into {} (map (fn [[panel p]]
                           [panel (cond-> {:points p :domains domains :frame frame :frame-overrides (space/frame-overrides options)
                                           :style st :type type :base base :half [(half :x) (half :y)]}
                                    layers (assoc :projections (layers panel)))]))
                 points)
     :extents (update-vals points (fn [p] (into {} (map (fn [k] [k (aesthetic/value-extent k (get plot-scales k) (remove nil? (p k)))]))
                                                (drop npos dims))))
     :na (set (filter (fn [k] (some #(some nil? (% k)) (vals points))) (drop npos dims)))
     :dropped dropped}))

;; drawing

(defn- point-styles
  "Per point index its style (`nil` - an aesthetic value outside its scale: skipped) from the panel's `points`."
  [points style scales env]
  (let [na {:color (get-in env [:theme :palette :na]) :shape (:shape style) :size (:size style)}
        mapped (vec (for [k aesthetic/dims :when (contains? points k)]
                      [k (aesthetic/attribute-fn k (get scales k) (na k)) (points k)]))]
    (if (seq mapped)
      (fn [^long i] (reduce (fn [st [k f vs]] (if-some [a (f (nth vs i))] (assoc st k a) (reduced nil))) style mapped))
      (constantly style))))

(defn- bar-faces
  "Visible faces of the bar at `x`, `y` from `base` to `z` (half sizes `hx`, `hy`) as polygon primitives: the box's
  faces facing the eye (`orientation` - see [[cljplot2.data.space3d/face-orientation]], by each face's centre), each
  shaded by its normal, all with the depth of the bar's footprint centre on the base - bars sort as wholes by where they stand (a face's own depth would rise with its height and draw a tall back bar over a
  short front one); a convex box's front faces never overlap, so their order within a bar doesn't matter."
  [point orientation [x y z] base [hx hy] st]
  (let [x (double x) y (double y) hx (double hx) hy (double hy)
        lo (m/min (double base) (double z)) hi (m/max (double base) (double z))
        at (fn [[ix iy iz]] [(if (zero? (long ix)) (- x hx) (+ x hx)) (if (zero? (long iy)) (- y hy) (+ y hy)) (if (zero? (long iz)) lo hi)])
        stroke (draw/outline-color (:stroke-color st) (:color st))
        depth (second (point [x y base]))]
    (keep (fn [{:keys [axis side corners]}]
            (let [face (mapv at corners)
                  centre (mapv (fn [i] (* 0.25 (double (reduce + (map #(% i) face))))) [0 1 2])
                  [normal front?] (orientation axis side centre)]
              (when front?
                {:kind :polygon :points (mapv #(first (point %)) face) :depth depth
                 :fill (space/shade (:color st) normal) :stroke stroke :stroke-width (:stroke-width st)})))
          space/faces)))

(defn primitives
  "Primitives of one panel's points (see [[cljplot2.graphics.space3d]]) for the projection `point` (data -> `[pixel
  depth]`), by the chart `type`; bars turn their faces by `frame` - the scene's, with its `:domains` (see
  [[cljplot2.graphics.space3d/draw-scene!]])."
  [{:keys [points type base half frame]} point styles]
  (let [{:keys [x y z]} points
        orientation (when (= type :bars) (space/face-orientation frame))]
    (mapcat (fn [i]
              (when-let [st (styles i)]
                (let [p [(x i) (y i) (z i)]
                      [px depth] (point p)]
                  (case type
                    :points [{:kind :point :at px :depth depth :style st}]
                    :none nil
                    ;; a spike sorts by where it stands, as a bar
                    :spikes (let [[b bd] (point [(x i) (y i) base])]
                              [{:kind :line :from b :to px :depth bd :color (:color st) :width (:stroke-width st)}])
                    :bars (bar-faces point orientation p base half st)))))
            (range (count x)))))

(def ^:const projection-alpha
  "Alpha factor of projected points (their style's color): shadows of the cloud, not marks."
  0.35)

(def ^:const band-alpha
  "Alpha factor of a projected density band (the chart's color): bands stack, denser regions darker."
  0.15)

(defn- faded
  [color ^double k]
  (let [col (c/to-color color)] (c/set-alpha col (* k (double (c/alpha col))))))

(defn projection-primitives
  "Primitives of one panel's side `projections` (see [[projection-layers]]) on the far wall of each plane in the view
  (none for a wall seen edge-on), at depth `##Inf` - drawn before the cloud: `:points` - each point's style faded
  ([[projection-alpha]]), `:density` - a region at or above each level in the chart's color ([[band-alpha]]),
  `:contour` - lines at the levels in the chart's color, 1 px."
  [{:keys [points projections domains frame style]} point styles]
  (mapcat (fn [[plane {:keys [kind grid levels]}]]
            (let [[a b across] (planes plane)]
              (when-let [side (space/far-side (:view frame) (:aspect frame) across)]
                (let [wall (nth (domains across) side)
                      clamp (fn [k v] (let [[lo hi] (domains k)] (m/constrain (double v) (m/min (double lo) (double hi)) (m/max (double lo) (double hi)))))
                      place (fn [[u w]] (first (point (mapv {a (clamp a u) b (clamp b w) across wall} space/axes))))]
                  (case kind
                    :points (keep (fn [i] (when-let [st (styles i)]
                                            {:kind :point :at (place [((points a) i) ((points b) i)]) :depth ##Inf
                                             :style (assoc st :color (faded (:color st) projection-alpha) :stroke-color :none)}))
                                  (range (count (points a))))
                    :density (when grid
                               (for [level levels :let [rings (contour/level-rings grid level)] :when (seq rings)]
                                 {:kind :region :rings (mapv #(mapv place %) rings) :depth ##Inf :fill (faded (:color style) band-alpha)}))
                    :contour (when grid
                               (for [level levels {:keys [points closed?]} (contour/isolines grid level)]
                                 {:kind :path :points (mapv place points) :closed? closed? :depth ##Inf
                                  :color (:color style) :width 1.0})))))))
          projections))

(defn chart-primitives
  "The chart's primitives for one panel's `computed` data and the projection `point` (the 3D chart contract, see
  [[cljplot2.graphics.space3d/draw-scene!]]): its side projections, then its points, spikes or bars styled by the
  aesthetic `scales`."
  [_chart {:keys [points style] :as computed} point scales env]
  (let [styles (point-styles points style scales env)]
    (concat (projection-primitives computed point styles) (primitives computed point styles))))

(defn legend
  "Key glyph: the point (points), a vertical line (spikes) or a filled square (bars), styled by the key's attributes."
  [{:keys [options]} attributes canvas {:keys [x y w h] :as box} env]
  (let [st (merge (style options (:theme env)) attributes)
        cx (+ (double x) (* 0.5 (double w)))]
    (case (get options :type :points)
      :bars (draw/bar-key! canvas box {:color (:color st) :stroke-color (:stroke-color st) :stroke-width (:stroke-width st)} false)
      :spikes (do (c2d/set-color canvas (:color st))
                  (c2d/set-stroke-custom canvas {:size (double (:stroke-width st)) :cap :butt})
                  (c2d/line canvas cx (+ (double y) (* 0.15 (double h))) cx (+ (double y) (* 0.85 (double h)))))
      (shape/draw-key! canvas (:shape st) box st))))
