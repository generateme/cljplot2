(ns cljplot2.data.cells
  "Grid cells of any `fastmath.grid` type (`:square`, `:shifted-square`, `:triangle`, `:rhombus`, `:flat-hex`,
  `:pointy-hex`) for [[cljplot2.chart.tile]] and [[cljplot2.chart.bin2d]].

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

  Drawing: [[draw-cell!]] - a cell polygon through a projection, shrunk by `:cell-scale` toward its centre."
  (:require [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.grid :as grid]
            [fastmath.stats :as stats]
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
  (a color, `:none` or `:darker`), `:stroke-width`, `:alpha` (fill alpha, a fraction; default - colors as they are)."
  [(schema/optional :cell cell-schema)
   (schema/optional :cell-scale [:and schema/finite [:> 0] [:<= 1]])
   (schema/optional :stroke-color draw/stroke-color)
   (schema/optional :stroke-width schema/non-neg)
   (schema/optional :alpha schema/fraction)])

(def style-keys
  "Option keys of [[style-entries]], merged over a cell chart's theme style."
  [:cell :cell-scale :stroke-color :stroke-width :alpha])

;; lattice cells

(def ^:private ^:const half-sqrt3 (* 0.5 m/SQRT3))

(defn- floor-half ^long [^long k] (Math/floorDiv k 2))

(def ^:private lattices
  "Per cell type: `:cell` - fastmath cell `[q r]` of lattice cell `[i j]`; `:origin` - the fastmath-space centre of
  lattice cell `[0 0]`; `:pitch` - fastmath-space distance between neighbouring columns and rows."
  {:square {:cell (fn [^long i ^long j] [i j]) :origin [0.5 0.5] :pitch [1.0 1.0]}
   :shifted-square {:cell (fn [^long i ^long j] [(- i (floor-half j)) j]) :origin [0.5 0.5] :pitch [1.0 1.0]}
   :pointy-hex {:cell (fn [^long i ^long j] [(- i (floor-half j)) j]) :origin [0.0 0.0] :pitch [1.0 half-sqrt3]}
   :flat-hex {:cell (fn [^long i ^long j] [i (- j (floor-half i))]) :origin [0.0 0.0] :pitch [half-sqrt3 1.0]}
   :rhombus {:cell (fn [^long i ^long j] [(- i (floor-half j)) j]) :origin [0.25 (* 0.5 half-sqrt3)] :pitch [1.0 half-sqrt3]}
   :triangle {:cell (fn [^long i ^long j] [(- i j) j]) :origin [0.0 (* 0.5 half-sqrt3)] :pitch [0.5 half-sqrt3]}})

(defn- lattice-cell-offsets
  "Corners of lattice cell `[i j]` of `cell-type` relative to `[i j]`, in index space."
  [cell-type ^long i ^long j]
  (let [{:keys [cell origin pitch]} (lattices cell-type)
        [ox oy] origin [px py] pitch
        [q r] (cell i j)
        g (grid/grid cell-type 1.0)]
    (mapv (fn [[x y]] [(- (/ (- (double x) (double ox)) (double px)) i)
                       (- (/ (- (double y) (double oy)) (double py)) j)])
          (grid/corners g (grid/cell->mid g q r)))))

(def ^:private parity-offsets
  "Per cell type: corner offsets of the lattice cells by `[i mod 2, j mod 2]` (the lattice repeats every 2 columns
  and rows)."
  (into {} (for [t grid/cell-names]
             [t (into {} (for [a [0 1] b [0 1]] [[a b] (lattice-cell-offsets t a b)]))])))

(defn- centred
  "`offsets` moved along axis `k` (0 - columns, 1 - rows) so their extent is centred on 0."
  [offsets k]
  (let [vs (map #(double (nth % k)) offsets)
        mid (* 0.5 (+ (double (reduce min vs)) (double (reduce max vs))))]
    (mapv #(update % k (fn [v] (- (double v) mid))) offsets)))

(defn lattice-offsets
  "Corners of lattice cell `[i j]` (integers) of `cell-type` relative to `[i j]` in index space (see the namespace
  doc); `centre-x?` / `centre-y?` - centre the polygon on its point along that axis (numeric axes: the cell sits on
  its value; the honeycomb shift of odd rows / columns is dropped)."
  [cell-type i j centre-x? centre-y?]
  (cond-> (get-in parity-offsets [cell-type [(Math/floorMod (long i) 2) (Math/floorMod (long j) 2)]])
    centre-x? (centred 0)
    centre-y? (centred 1)))

(defn lattice-extent
  "`[[x-lo x-hi] [y-lo y-hi]]`: the lowest and highest offsets of any lattice cell of `cell-type` from its point
  along each axis, in index units, with the axes centred as in [[lattice-offsets]] (e.g. `:pointy-hex` x
  `[-0.5 1.0]` - odd rows shifted right)."
  [cell-type centre-x? centre-y?]
  (let [offsets (apply concat (for [a [0 1] b [0 1]] (lattice-offsets cell-type a b centre-x? centre-y?)))
        extent (fn [k] (let [vs (map #(double (nth % k)) offsets)] [(reduce min vs) (reduce max vs)]))]
    [(extent 0) (extent 1)]))

(defn lattice-reach
  "`[rx ry]`: how far any lattice cell of `cell-type` reaches from its point along each axis, in index units (the
  larger side of [[lattice-extent]])."
  [cell-type centre-x? centre-y?]
  (mapv (fn [[lo hi]] (m/max (m/abs (double lo)) (m/abs (double hi)))) (lattice-extent cell-type centre-x? centre-y?)))

;; binned cells

(defn shape-factor
  "Shape factor `s` of the normalized space (see the namespace doc) for data spans `x-span`, `y-span` (positive):
  option `:shape` when given; with `env` `:aspect :iso` - `y-span / x-span` (equal pixels per data unit); else the
  expected panel height / width from `env` `:panel-size` (a delay of `[w h]`, see
  [[cljplot2.layout.plot-grid/estimated-panel-size]]); 1.0 without one."
  ^double [{:keys [shape]} x-span y-span {:keys [aspect panel-size]}]
  (cond
    shape (double shape)
    (= :iso aspect) (/ (double y-span) (double x-span))
    panel-size (let [[w h] (force panel-size)] (/ (double h) (double w)))
    :else 1.0))

(defn- span
  "`[lo span]` of `vs` (non-empty doubles); a zero span is 1, centred on the value."
  [vs]
  (let [lo (double (reduce min vs)) hi (double (reduce max vs))]
    (if (== lo hi) [(- lo 0.5) 1.0] [lo (- hi lo)])))

(def size-methods
  "fastmath `estimate-grid-size` methods `:bins` may name: normal-reference rules (cheap) and data-driven ones (a
  pass over the points per candidate size)."
  [:scott-correlated :scott :freedman-diaconis :cross-validation :shimazaki-shinomoto])

(def default-size-method :scott-correlated)

(defn- grid-size
  "Cell size in the normalized space: `:binwidth` (x data units) over `x-span`, `:bins` an integer - 1 / bins, a
  [[size-methods]] keyword (default [[default-size-method]]) - fastmath's estimate for the normalized `points` on
  the `:cell` grid."
  ^double [{:keys [bins binwidth cell]} x-span points]
  (cond
    binwidth (/ (double binwidth) (double x-span))
    (integer? bins) (/ 1.0 (double bins))
    :else (stats/estimate-grid-size points (or bins default-size-method) {:grid cell})))

(defn- proportional-box
  "`[[u0 u1] [v0 v1]]`: the bounding box of the normalized `points`, its shorter side grown (centred) so that its
  height / width is `s` - the box's pixels per unit are equal on both axes exactly when the panel's height / width
  is `s`."
  [points ^double s]
  (let [us (map #(double (first %)) points) vs (map #(double (second %)) points)
        u0 (double (reduce min us)) u1 (double (reduce max us))
        v0 (double (reduce min vs)) v1 (double (reduce max vs))
        w (- u1 u0) h (- v1 v0)]
    (cond
      (< h (* s w))
      (let [grow (* 0.5 (- (* s w) h))] [[u0 u1] [(- v0 grow) (+ v1 grow)]])
      :else (let [grow (* 0.5 (- (/ h s) w))] [[(- u0 grow) (+ u1 grow)] [v0 v1]]))))

(defn bin-points
  "Points of every panel (`panel-points` - `{panel [[x y] ..]}`, finite numbers) binned on one grid of `:cell` type
  over the normalized space of all points (options `:cell`, `:bins`, `:binwidth`, `:shape`; `env` for
  [[shape-factor]]). Returns `{:panels {panel [{:corners [[x y] ..] :count n} ..]} :area a :extent {:x [lo hi] :y
  [lo hi]}}` - occupied cells only, corners in the points' units, `a` - a cell's area in those units, `:extent` - the
  cells' bounding box widened to the shape factor's proportions (the plot extents keeping cells regular, see
  [[proportional-box]]); `{:panels {} :area 0.0}` without points. Panels share the grid: equal cells in every panel.
  The points' units are the screen-linear coordinates of the axes (data units, log10 of them on a log axis - the
  caller transforms)."
  [panel-points options env]
  (let [all (apply concat (vals panel-points))]
    (if (empty? all)
      {:panels {} :area 0.0}
      (let [[x0 xs] (span (map #(double (first %)) all))
            [y0 ys] (span (map #(double (second %)) all))
            x0 (double x0) xs (double xs) y0 (double y0) ys (double ys)
            s (shape-factor options xs ys env)
            to-unit (fn [[x y]] [(/ (- (double x) x0) xs) (/ (* s (- (double y) y0)) ys)])
            from-unit (fn [[u v]] [(+ x0 (* (double u) xs)) (+ y0 (/ (* (double v) ys) s))])
            cell (:cell options)
            size (grid-size options xs (map to-unit all))
            g (grid/grid cell size)
            unit-cells (update-vals panel-points
                                    (fn [pts]
                                      (mapv (fn [{:keys [mid count]}] {:corners (grid/corners g mid) :count count})
                                            (:bins-maps (stats/histogram2d (map to-unit pts) size {:grid cell})))))
            [[u0 u1] [v0 v1]] (proportional-box (mapcat :corners (mapcat val unit-cells)) s)
            [lo-x lo-y] (from-unit [u0 v0])
            [hi-x hi-y] (from-unit [u1 v1])]
        {:panels (update-vals unit-cells (fn [cs] (mapv #(update % :corners (partial mapv from-unit)) cs)))
         :area (/ (* (double (grid/area g)) xs ys) s)
         :extent {:x [lo-x hi-x] :y [lo-y hi-y]}}))))

;; drawing

(defn shrunk
  "`corners` shrunk toward their mean by `k` (1 - unchanged)."
  [corners k]
  (let [k (double k)]
    (if (== k 1.0)
      corners
      (let [n (double (count corners))
            cx (/ (double (reduce + (map #(double (first %)) corners))) n)
            cy (/ (double (reduce + (map #(double (second %)) corners))) n)]
        (mapv (fn [[x y]] [(+ cx (* k (- (double x) cx))) (+ cy (* k (- (double y) cy)))]) corners)))))

(defn draw-cell!
  "Draw a cell: `corners` (any 2d units, shrunk by the style's `:cell-scale`) turned into pixel points by `to-pixel`
  (a function of `[a b]` -> `Vec2`), filled with `fill` (its alpha multiplied by `:alpha` when set), outlined with
  `:stroke-color` / `:stroke-width`. A corner the projection can't place (`##NaN`) skips the cell."
  [canvas to-pixel corners fill {:keys [cell-scale stroke-color stroke-width alpha]}]
  (let [points (mapv (fn [p] (to-pixel p)) (shrunk corners (or cell-scale 1.0)))]
    (when-not (some (fn [^Vec2 p] (or (m/nan? (.x p)) (m/nan? (.y p)))) points)
      (let [fill (if alpha (c/set-alpha fill (* (double (c/alpha fill)) (double alpha))) fill)
            stroke (draw/outline-color stroke-color fill)]
        (c2d/set-color canvas fill)
        (c2d/path canvas points true false)
        (when (and (not (theme/none? stroke)) (pos? (double stroke-width)))
          (c2d/set-stroke-custom canvas {:size (double stroke-width)})
          (c2d/set-color canvas stroke)
          (c2d/path canvas points true true))))))
