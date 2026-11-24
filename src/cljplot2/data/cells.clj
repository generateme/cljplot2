(ns cljplot2.data.cells
  "Grid cells of the cell charts: lattice cells of [[cljplot2.chart.tile]] and binned cells of [[cljplot2.chart.bin2d]], of any `fastmath.grid` type (`:square`, `:shifted-square`, `:triangle`, `:rhombus`, `:flat-hex`, `:pointy-hex`).

  Lattice cells (tile): cell `[i j]` of a lattice - column `i`, row `j` (level indices of categorical axes, or
  steps of a numeric axis's resolution) - is the cell type's polygon in index space, where neighbouring columns and
  rows are 1 apart: squares fill their unit square, hexagons form a honeycomb (`:pointy-hex` - odd rows shifted half a
  column right; `:flat-hex` - odd columns shifted half a row up), triangles alternate up / down, `:shifted-square` and
  `:rhombus` rows are offset as bricks. [[lattice-offsets]] gives a cell's corners relative to `[i j]`.

  Binned cells (bin2d): points binned on a grid laid over a normalized space, see [[bin-points]]: x maps to
  `u = (x - x0) / (x1 - x0)` in `[0 1]`, y to `v = s (y - y0) / (y1 - y0)`, `s` the [[shape-factor]] - the expected
  panel height / width - so cells of one size in `u` / `v` are regular in pixels when the panel's pixels per unit
  match: exactly with plot `:aspect :iso` or a given `:shape` matching the panel, approximately otherwise (the panel
  size is estimated before the layout - from the plot size less titles, axes, legends and facet strips). The points
  come in the axes' screen-linear coordinates (bin2d takes a log axis to log10, see
  [[cljplot2.scale.position/stat-transform]]); cell corners go back the same way and are projected corner by corner.

  Drawing: [[draw-cell!]] - a cell polygon through a projection, shrunk by `:cell-scale` toward its centre; draw a
  chart's cells inside [[call-with-cell-rendering]] (abutting cells without seams).

  Functions: options - [[cell-schema]], [[style-entries]], [[style-keys]]; lattice cells - [[lattice-offsets]], [[lattice-extent]], [[lattice-reach]]; binned cells - [[shape-factor]], [[size-methods]], [[bin-points]]; drawing - [[shrunk]], [[call-with-cell-rendering]], [[draw-cell!]]."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [fastmath.grid :as grid]
            [fastmath.stats :as stats]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def cell-schema
  "Schema of a cell type."
  (into [:enum] grid/cell-names))

(def style-entries
  "Style map entries of a cell chart: `:cell` (type), `:cell-scale` (polygon shrink factor, `(0 1]`), `:stroke-color`
  (a color, `:none` or `:darker`), `:stroke-width`, `:alpha` (fill alpha multiplier, a fraction; theme default 1.0 - colors as they are)."
  (schema/in-fragment
   :cell
   [(schema/optional :cell {:description "cell shape of the grid (a fastmath grid cell type)"} cell-schema)
    (schema/optional :cell-scale {:description "cell polygon size relative to its grid cell, in (0 1]"} [:and schema/finite [:> 0] [:<= 1]])
    (schema/optional :stroke-color {:description "cell outline: a colour, :none or :darker (the darkened fill)"} draw/stroke-color)
    (schema/optional :stroke-width {:description "cell outline width in pixels"} schema/non-neg)
    (schema/optional :alpha {:description "fill opacity multiplier"} schema/fraction)]))

(def style-keys
  "Option keys of [[style-entries]], merged over a cell chart's theme style."
  [:cell :cell-scale :stroke-color :stroke-width :alpha])

;; lattice cells

(defn- floor-half
  "Gives half of a long rounded toward negative infinity.

  Parameters:

  - `k` (long): any long.

  Returns `k / 2` floored, e.g. -1 for -1."
  ^long [^long k] (m/>> k 1))

(def ^:private lattices
  "Per cell type: `:cell` - fastmath cell `[q r]` of lattice cell `[i j]`; `:origin` - the fastmath-space centre of
  lattice cell `[0 0]`; `:pitch` - fastmath-space distance between neighbouring columns and rows."
  {:square {:cell (fn [^long i ^long j] [i j]) :origin [0.5 0.5] :pitch [1.0 1.0]}
   :shifted-square {:cell (fn [^long i ^long j] [(m/long-sub i (floor-half j)) j]) :origin [0.5 0.5] :pitch [1.0 1.0]}
   :pointy-hex {:cell (fn [^long i ^long j] [(m/long-sub i (floor-half j)) j]) :origin [0.0 0.0] :pitch [1.0 m/SQRT3_2]}
   :flat-hex {:cell (fn [^long i ^long j] [i (m/long-sub j (floor-half i))]) :origin [0.0 0.0] :pitch [m/SQRT3_2 1.0]}
   :rhombus {:cell (fn [^long i ^long j] [(m/long-sub i (floor-half j)) j]) :origin [0.25 m/SQRT3_4] :pitch [1.0 m/SQRT3_2]}
   :triangle {:cell (fn [^long i ^long j] [(m/long-sub i j) j]) :origin [0.0 m/SQRT3_4] :pitch [0.5 m/SQRT3_2]}})

(defn- lattice-cell-offsets
  "Gives the corners of one lattice cell relative to its indices, in index space (see the namespace doc).

  Parameters:

  - `cell-type` (keyword): a `fastmath.grid` cell type.
  - `i`, `j` (longs): the cell's column and row.

  Returns a vector of the cell's corners `[x y]` less `[i j]`.

  See also [[lattice-offsets]]."
  [cell-type ^long i ^long j]
  (let [{:keys [cell origin pitch]} (lattices cell-type)
        [^double ox ^double oy] origin
        [^double px ^double py] pitch
        [q r] (cell i j)
        g (grid/grid cell-type 1.0)]
    (mapv (fn [[^double x ^double y]] [(m/- (m// (m/- x ox) px) i)
                                      (m/- (m// (m/- y oy) py) j)])
          (grid/corners g (grid/cell->mid g q r)))))

(def ^:private parity-offsets
  "Per cell type: corner offsets of the lattice cells by `[i mod 2, j mod 2]` (the lattice repeats every 2 columns
  and rows)."
  (into {} (for [t grid/cell-names]
             [t (into {} (for [a [0 1] b [0 1]] [[a b] (lattice-cell-offsets t a b)]))])))

(defn- centred
  "Gives the corner offsets moved along one axis so their extent is centred on 0.

  Parameters:

  - `offsets` (sequence of `[x y]`): corner offsets.
  - `k` (long): the axis, 0 - columns, 1 - rows.

  Returns a vector of the moved offsets."
  [offsets k]
  (let [vs (map #(double (nth % k)) offsets)
        mid (m/* 0.5 (m/+ (v/mn vs) (v/mx vs)))]
    (mapv #(update % k (fn [^double v] (m/- v mid))) offsets)))

(defn lattice-offsets
  "Gives the corners of a lattice cell relative to its indices, in index space (see the namespace doc).

  Parameters:

  - `cell-type` (keyword): a `fastmath.grid` cell type.
  - `i`, `j` (integers): the cell's column and row.
  - `centre-x?`, `centre-y?`: truthy - centre the polygon on its point along that axis (numeric axes: the cell sits on its value; the honeycomb shift of odd rows or columns is dropped).

  Returns a vector of corners `[x y]` less `[i j]`; the lattice repeats every 2 columns and rows.

  See also [[lattice-extent]], [[lattice-reach]]."
  [cell-type i j centre-x? centre-y?]
  (cond-> (get-in parity-offsets [cell-type [(m/long-mod i 2) (m/long-mod j 2)]])
    centre-x? (centred 0)
    centre-y? (centred 1)))

(defn lattice-extent
  "Gives the extent of the lattice cells of a type around their points, in index units.

  Parameters:

  - `cell-type` (keyword): a `fastmath.grid` cell type.
  - `centre-x?`, `centre-y?`: centre the axes as in [[lattice-offsets]].

  Returns `[[x-lo x-hi] [y-lo y-hi]]`: the lowest and highest offset of any cell from its point along each axis (e.g. `:pointy-hex` x `[-0.5 1.0]` - odd rows shifted right).

  See also [[lattice-reach]]."
  [cell-type centre-x? centre-y?]
  (let [offsets (apply concat (for [a [0 1] b [0 1]] (lattice-offsets cell-type a b centre-x? centre-y?)))
        extent (fn [k] (let [vs (map #(double (nth % k)) offsets)] [(v/mn vs) (v/mx vs)]))]
    [(extent 0) (extent 1)]))

(defn lattice-reach
  "Gives the reach of the lattice cells of a type from their points, in index units.

  Parameters:

  - `cell-type` (keyword): a `fastmath.grid` cell type.
  - `centre-x?`, `centre-y?`: centre the axes as in [[lattice-offsets]].

  Returns `[rx ry]`: per axis the larger magnitude of the two sides of [[lattice-extent]]."
  [cell-type centre-x? centre-y?]
  (mapv (fn [[^double lo ^double hi]] (m/max (m/abs lo) (m/abs hi))) (lattice-extent cell-type centre-x? centre-y?)))

;; binned cells

(defn shape-factor
  "Gives the shape factor `s` of the normalized space of binned cells (see the namespace doc).

  Parameters:

  - `options` (map): the chart's options, `:shape` read.
  - `x-span`, `y-span` (doubles): the data spans, positive.
  - `env` (map): `:aspect` - the plot's aspect; `:panel-size` - a delay of the expected panel `[w h]` (see [[cljplot2.layout.plot-grid/estimated-panel-size]]).

  Returns `:shape` when given; with `:aspect :iso` - `y-span / x-span` (equal pixels per data unit); else the expected panel height over width; 1.0 without a panel size."
  ^double [{:keys [shape]} ^double x-span ^double y-span {:keys [aspect panel-size]}]
  (cond
    shape (double shape)
    (= :iso aspect) (m// y-span x-span)
    panel-size (let [[^double w ^double h] (force panel-size)] (m// h w))
    :else 1.0))

(defn- span
  "Gives the low end and span of values.

  Parameters:

  - `vs` (sequence of doubles): not empty.

  Returns `[lo span]`; a single distinct value gives a span of 1 centred on it."
  [vs]
  (let [lo (v/mn vs)
        hi (v/mx vs)]
    (if (m/== lo hi) [(m/- lo 0.5) 1.0] [lo (m/- hi lo)])))

(def size-methods
  "fastmath `estimate-grid-size` methods `:bins` may name: normal-reference rules (cheap) and data-driven ones (a
  pass over the points per candidate size)."
  [:scott-correlated :scott :freedman-diaconis :cross-validation :shimazaki-shinomoto])

(def default-size-method
  "The [[size-methods]] method used when `:bins` is not given."
  :scott-correlated)

(defn- grid-size
  "Gives the cell size of the binning grid in the normalized space.

  Parameters:

  - `options` (map): `:binwidth` - cell size in x data units; `:bins` - a count of cells across, or a [[size-methods]] keyword (default [[default-size-method]]); `:cell` - the cell type.
  - `x-span` (double): the x data span.
  - `points` (sequence of `[u v]`): the normalized points.

  Returns `:binwidth` over `x-span`, 1 over an integer `:bins`, else fastmath's estimate for `points` on the `:cell` grid."
  ^double [{:keys [bins binwidth cell]} ^double x-span points]
  (cond
    binwidth (m// (double binwidth) x-span)
    (integer? bins) (m// 1.0 (double bins))
    :else (stats/estimate-grid-size points (or bins default-size-method) {:grid cell})))

(defn- proportional-box
  "Gives the bounding box of normalized points grown to a given proportion.

  Parameters:

  - `points` (sequence of `[u v]`): normalized points, not empty.
  - `s` (double): the shape factor, see [[shape-factor]].

  Returns `[[u0 u1] [v0 v1]]`: the points' bounding box with its shorter side grown (centred) so that its height over width is `s` - its pixels per unit are equal on both axes exactly when the panel's height over width is `s`."
  [points ^double s]
  (let [us (map first points)
        vs (map second points)
        u0 (v/mn us)
        u1 (v/mx us)
        v0 (v/mn vs)
        v1 (v/mx vs)
        w (m/- u1 u0)
        h (m/- v1 v0)]
    (if (m/< h (m/* s w))
      (let [grow (m/* 0.5 (m/- (m/* s w) h))] [[u0 u1] [(m/- v0 grow) (m/+ v1 grow)]])
      (let [grow (m/* 0.5 (m/- (m// h s) w))] [[(m/- u0 grow) (m/+ u1 grow)] [v0 v1]]))))

(defn bin-points
  "Bins the points of every panel on one grid over the normalized space of all points (see the namespace doc).

  Panels share the grid: equal cells in every panel. The points' units are the screen-linear coordinates of the axes (data units, log10 of them on a log axis - the caller transforms).

  Parameters:

  - `panel-points` (map): panel -> its points `[[x y] ...]`, finite numbers.
  - `options` (map): `:cell` - the cell type; `:bins`, `:binwidth` - the cell size (see [[size-methods]]); `:shape` - the shape factor.
  - `env` (map): the compute environment for [[shape-factor]].

  Returns `{:panels {panel [{:corners [[x y] ...] :count n} ...]} :area a :extent {:x [lo hi] :y [lo hi]}}`: the occupied cells only, corners in the points' units; `a` - a cell's area in those units; `:extent` - the cells' bounding box widened to the shape factor's proportions (the plot extents keeping cells regular). Without points `{:panels {} :area 0.0}`."
  [panel-points options env]
  (let [all (apply concat (vals panel-points))]
    (if (empty? all)
      {:panels {} :area 0.0}
      (let [[^double x0 ^double xs] (span (map first all))
            [^double y0 ^double ys] (span (map second all))
            s (shape-factor options xs ys env)
            to-unit (fn [[^double x ^double y]] [(m// (m/- x x0) xs) (m// (m/* s (m/- y y0)) ys)])
            from-unit (fn [[^double u ^double v]] [(m/+ x0 (m/* u xs)) (m/+ y0 (m// (m/* v ys) s))])
            cell (:cell options)
            size (grid-size options xs (map to-unit all))
            g (grid/grid cell size)
            unit-cells (update-vals panel-points
                                    (fn [pts]
                                      (mapv (fn [{:keys [mid] cell-count :count}] {:corners (grid/corners g mid) :count cell-count})
                                            (:bins-maps (stats/histogram2d (map to-unit pts) size {:grid cell})))))
            [[u0 u1] [v0 v1]] (proportional-box (mapcat :corners (mapcat val unit-cells)) s)
            [lo-x lo-y] (from-unit [u0 v0])
            [hi-x hi-y] (from-unit [u1 v1])]
        {:panels (update-vals unit-cells (fn [cs] (mapv #(update % :corners (partial mapv from-unit)) cs)))
         :area (m// (m/* (grid/area g) xs ys) s)
         :extent {:x [lo-x hi-x] :y [lo-y hi-y]}}))))

;; drawing

(defn shrunk
  "Gives the cell corners shrunk toward their mean.

  Parameters:

  - `corners` (sequence of `[x y]`): the corners, not empty.
  - `k` (double): the scale factor, 1 - unchanged.

  Returns the corners scaled by `k` about their mean (`corners` itself for `k` 1)."
  [corners ^double k]
  (if (m/one? k)
    corners
    (let [n (count corners)
          cx (m// (v/sum (map first corners)) n)
          cy (m// (v/sum (map second corners)) n)]
      (mapv (fn [[^double x ^double y]] [(m/+ cx (m/* k (m/- x cx))) (m/+ cy (m/* k (m/- y cy)))]) corners))))

(defn call-with-cell-rendering
  "Calls a drawing function with rendering suited to a chart's cells.

  Abutting cells are filled without antialiasing, so neighbours share their edges - no seams of the panel between them; spaced cells keep smooth edges.

  Parameters:

  - `canvas`: the clojure2d canvas.
  - `style` (map): the chart's style, `:cell-scale` read (default 1; at least 1 - abutting).
  - `f` (function): of the canvas to draw on.

  Returns the result of `f`.

  See also [[draw-cell!]]."
  [canvas {:keys [cell-scale]} f]
  (if (m/>= (double (or cell-scale 1.0)) 1.0)
    ;; antialiasing off: cells side by side share their edges exactly (no seams of the background between them)
    (c2d/with-isolated-canvas [unaliased canvas]
      (c2d/set-rendering-hint unaliased :antialiasing :off)
      (f unaliased))
    (f canvas)))

(defn draw-cell!
  "Draws one cell polygon through a projection.

  Parameters:

  - `canvas`: the clojure2d canvas.
  - `to-pixel` (function): of a corner `[a b]` (any 2d units) giving its pixel `Vec2`.
  - `corners` (sequence of `[a b]`): the cell's corners, shrunk by `:cell-scale` (see [[shrunk]]).
  - `fill`: the fill colour.
  - `style` (map): `:cell-scale` (default 1), `:alpha` - fill alpha multiplier (none when `nil`), `:stroke-color` - a colour, `:none` or `:darker` (see [[cljplot2.graphics.draw/outline-color]]), `:stroke-width` - outline width in pixels (0 - none).

  Returns `nil`. A corner the projection can't place (`##NaN`) skips the cell.

  See also [[call-with-cell-rendering]]."
  [canvas to-pixel corners fill {:keys [cell-scale stroke-color stroke-width alpha]}]
  (let [points (mapv (fn [p] (to-pixel p)) (shrunk corners (or cell-scale 1.0)))]
    (when (every? data/finite-point? points)
      (let [cell-fill (if alpha (draw/faded fill alpha) fill)
            stroke (draw/outline-color stroke-color cell-fill)]
        (c2d/set-color canvas cell-fill)
        (c2d/path canvas points true false)
        (when (and (not (theme/none? stroke)) (m/pos? (double stroke-width)))
          (c2d/set-stroke-custom canvas {:size stroke-width})
          (c2d/set-color canvas stroke)
          (c2d/path canvas points true true))))))
