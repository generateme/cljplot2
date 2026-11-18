(ns cljplot2.data.field
  "Fields: a value at every point of a 2D domain, sampled on a regular grid, for [[cljplot2.chart.field]] (raster)
  and [[cljplot2.chart.contour]] (contour lines).

  Sources ([[source-kind]]):

  * `:points` - mapped `:x`, `:y`: the 2D kernel density estimate of the points (`fastmath.kernel/kernel-density-2d`,
    a product kernel, a bandwidth per axis); options `:kernel` (default `:gaussian`), `:bandwidth` (a number, a
    fastmath selector, `:scott`, or a pair `[hx hy]` of them; default `:nrd` per axis - `MASS::kde2d`, as ggplot2),
    `:binned?`; the domain is the points' range widened by `:cut` (default 3) bandwidths on each axis
  * `:gridded` - mapped `:x`, `:y`, `:value` on a full regular lattice (every x with every y, once):
    `fastmath.interpolation` `:interpolation` - `:bilinear` (default), `:bicubic`, `:cubic-2d`; the domain is the
    lattice
  * `:function` - option `:f`, a function of `x`, `y` returning a number; the domain - option `:domain {:x [lo hi] :y
    [lo hi]}`, else the plot's explicit `:x` / `:y` scale domains

  A grid (see [[sample]]) holds `:n` x values by y values (option `:n`, default 100, or `[nx ny]`) spanning the
  domain end to end; values that aren't finite numbers are `##NaN` (not drawn, not contoured).

  Scales: on a `:log` axis the density is estimated, and every grid sampled, in log10 coordinates (see
  [[cljplot2.scale.position/stat-transform]]) - contours follow the points as the axis shows them, grid points are
  even on screen; the density value is then per log10 unit of that axis."
  (:require [clojure2d.color :as c]
            [fastmath.complex :as cx]
            [fastmath.core :as m]
            [fastmath.interpolation :as interp]
            [fastmath.kernel :as k]
            [cljplot2.data.rows :as data]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema])
  (:import [fastmath.java Array]
           [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def bandwidth-schema
  "Schema of `:bandwidth`: a positive number, a selector keyword, or a pair of them."
  (let [one [:or [:and schema/finite [:> 0]] [:enum :nrd :nrd0 :nrd-adjust :sj :sj-dpi :rlcv :lcv :lscv :scott]]]
    [:or one [:tuple one one]]))

(def n-schema
  "Schema of `:n`, a grid size: an integer for both axes or a pair `[nx ny]`, each in [2 2000]."
  [:or [:int {:min 2 :max 2000}] [:tuple [:int {:min 2 :max 2000}] [:int {:min 2 :max 2000}]]])

(def options
  "Map entries of the field options shared by the field and contour charts."
  [(schema/optional :f schema/function)
   (schema/optional :domain [:map {:closed true} [:x [:tuple schema/finite schema/finite]] [:y [:tuple schema/finite schema/finite]]])
   (schema/optional :kernel keyword?)
   (schema/optional :bandwidth bandwidth-schema)
   (schema/optional :binned? :boolean)
   (schema/optional :cut [:and schema/finite [:>= 0]])
   (schema/optional :interpolation [:enum :bilinear :bicubic :cubic-2d])
   (schema/optional :n n-schema)])

(defn source-kind
  "The source of a field chart: `:function` with option `:f`, `:gridded` with a mapped `:value`, else `:points`."
  [options mapping]
  (cond
    (:f options) :function
    (some? (:value mapping)) :gridded
    :else :points))

(defn- finite? [v] (and (number? v) (m/valid-double? (double v))))

(def ^:private estimate-error-keys
  "fastmath `:property` of a density error -> the chart option to blame."
  {:kernel :kernel :binned? :binned? :bandwidth :bandwidth :bandwidth-options :bandwidth})

(defn density
  "`{:f f :domain [[x0 x1] [y0 y1]]}` - the 2D KDE of the points `xs`, `ys` (finite numbers, equal counts, at least
  one; see the namespace doc for `:kernel`, `:bandwidth`, `:binned?`, `:cut`), estimated in the coordinates of the
  axis `transforms` (`[tx ty]`, see [[cljplot2.scale.position/stat-transform]]; points a transform can't place left
  out): `f` takes data units, the domain - the points' transformed range widened by `:cut` bandwidths - is in data
  units. On a log axis the density is per log10 unit. Throws `ex-info` keyed by the option fastmath blames (`:kernel`,
  `:binned?`, else `:bandwidth`) when it can't estimate (e.g. a selector on one point)."
  [xs ys {:keys [kernel bandwidth binned? cut]} [tx ty]]
  (let [fx (:forward tx) fy (:forward ty)
        pts (filterv (fn [[u v]] (and (m/valid-double? u) (m/valid-double? v)))
                     (map (fn [x y] [(fx (double x)) (fy (double y))]) xs ys))
        _ (when (empty? pts)
            (schema/fail! "No points the axis scales can place (e.g. only non-positive values on a log axis)." {:key :x}))
        us (mapv first pts) vs (mapv second pts)
        {:keys [kde h]} (try (k/kernel-density-2d (or kernel :gaussian) us vs
                                                   {:bandwidth (or bandwidth :nrd) :binned? (boolean binned?)} true)
                             (catch clojure.lang.ExceptionInfo e
                               (schema/fail! (str "Density estimation failed: " (ex-message e))
                                             {:key (get estimate-error-keys (:property (ex-data e)) :bandwidth) :cause (ex-data e)})))
        [hx hy] h
        cut (double (if (nil? cut) 3.0 cut))
        widen (fn [ws ^double hw inverse]
                [(inverse (- (double (reduce min ws)) (* cut hw))) (inverse (+ (double (reduce max ws)) (* cut hw)))])]
    {:f (fn [x y] (kde (fx (double x)) (fy (double y))))
     :domain [(widen us (double hx) (:inverse tx)) (widen vs (double hy) (:inverse ty))]}))

(defn lattice-values
  "`{:xs [..] :ys [..] :vss [[..] ..]}` of the gridded `rows` (`x-of`, `y-of`, `value-of` - row accessors): the sorted
  distinct x and y values and per x the values at every y (`##NaN` for a non-finite value); every distinct x with every
  distinct y exactly once, at least 2 of each. Rows with a non-finite x or y are left out; a missing or repeated
  lattice point, or fewer than 2 x / y values, throws `ex-info {:key value-key}`."
  [rows x-of y-of value-of value-key]
  (let [points (filterv (fn [[x y]] (and (finite? x) (finite? y))) (map (juxt x-of y-of value-of) rows))
        xs (vec (sort (distinct (map #(double (first %)) points))))
        ys (vec (sort (distinct (map #(double (second %)) points))))
        by-point (group-by (fn [[x y]] [(double x) (double y)]) points)
        accessor (name value-key)]
    (when (or (< (count xs) 2) (< (count ys) 2))
      (schema/fail! "A gridded field needs at least 2 distinct x and 2 distinct y values." {:key value-key :xs (count xs) :ys (count ys)}))
    (when-let [[point] (first (filter #(> (count (val %)) 1) by-point))]
      (schema/fail! (str "A gridded field has one :" accessor " per (x, y); a lattice point repeats.") {:key value-key :value point}))
    (when-let [missing (first (for [x xs y ys :when (not (contains? by-point [x y]))] [x y]))]
      (schema/fail! (str "A gridded field needs a :" accessor " at every (x, y) of its lattice; a point is missing.")
                    {:key value-key :value missing}))
    (let [value (fn [x y] (let [v (nth (first (by-point [x y])) 2)] (if (finite? v) (double v) ##NaN)))]
      {:xs xs :ys ys :vss (mapv (fn [x] (mapv (fn [y] (value x y)) ys)) xs)})))

(defn lattice
  "`{:f f :domain [[x0 x1] [y0 y1]]}` - the interpolation (`interpolation`, see the namespace doc) of the gridded
  `rows` (`x-of`, `y-of`, `value-of` - row accessors), see [[lattice-values]] (errors keyed `:value`)."
  [rows x-of y-of value-of interpolation]
  (let [{:keys [xs ys vss]} (lattice-values rows x-of y-of value-of :value)
        make (case (or interpolation :bilinear) :bilinear interp/bilinear :bicubic interp/bicubic :cubic-2d interp/cubic-2d)]
    {:f (make xs ys vss) :domain [[(first xs) (peek xs)] [(first ys) (peek ys)]]}))

(defn function-domain
  "`[[x0 x1] [y0 y1]]` of a function field: option `domain` (`{:x [lo hi] :y [lo hi]}`), else the explicit numeric
  `:domain`s of the plot's `:x` / `:y` scales (`plot-scales`). Throws `ex-info {:key :domain}` without either."
  [domain plot-scales]
  (let [explicit (fn [k] (let [d (get-in plot-scales [k :domain])] (when (and (= 2 (count d)) (every? finite? d)) (mapv double d))))
        x (or (:x domain) (explicit :x))
        y (or (:y domain) (explicit :y))]
    (when-not (and x y)
      (schema/fail! "A function field needs a :domain {:x [lo hi] :y [lo hi]} (or explicit :x and :y scale domains)."
                    {:key :domain}))
    [(mapv double x) (mapv double y)]))

(defn grid-size
  "`[nx ny]` of option `n` (an integer for both axes or a pair; default 100)."
  [n]
  (cond (nil? n) [100 100] (sequential? n) (mapv long n) :else [(long n) (long n)]))

(defn union-domain
  "The smallest domain `[[x0 x1] [y0 y1]]` covering `domains`; `nil` for none."
  [domains]
  (when (seq domains)
    (let [axis (fn [k] [(reduce min (map #(first (nth % k)) domains)) (reduce max (map #(second (nth % k)) domains))])]
      [(axis 0) (axis 1)])))

(defn- axis-points
  "`n` equally spaced values from `lo` to `hi` (a zero-width axis: all at `lo`)."
  ^doubles [lo hi ^long n]
  (let [lo (double lo) step (/ (- (double hi) lo) (dec n))
        a (double-array n)]
    (dotimes [i n] (Array/aset a i (+ lo (* step i))))
    a))

(defn sample
  "The grid of field function `f` (data units) over `domain` (`[[x0 x1] [y0 y1]]`, data units) with `[nx ny]`
  points evenly spaced in the coordinates of the axis `transforms` (`[tx ty]`, see
  [[cljplot2.scale.position/stat-transform]]; `nil` - identity): `{:xs doubles :ys doubles :nx :ny :values doubles
  :transforms [tx ty]}` - `xs`, `ys` in transformed coordinates, `values` row by row (`i + j nx` for `xs[i]`,
  `ys[j]`), `##NaN` where `f` doesn't return a finite number. Exceptions `f` throws propagate."
  ([f domain size] (sample f domain size nil))
  ([f [[x0 x1] [y0 y1]] [nx ny] transforms]
   (let [[tx ty] (or transforms [(scale/stat-transform nil) (scale/stat-transform nil)])
         nx (long nx) ny (long ny)
         fx (:forward tx) fy (:forward ty) ix (:inverse tx) iy (:inverse ty)
         ^doubles xs (axis-points (fx (double x0)) (fx (double x1)) nx)
         ^doubles ys (axis-points (fy (double y0)) (fy (double y1)) ny)
         values (double-array (* nx ny))]
     (dotimes [j ny]
       (dotimes [i nx]
         (let [v (f (ix (Array/aget xs i)) (iy (Array/aget ys j)))]
           (Array/aset values (+ i (* j nx)) (if (finite? v) (double v) ##NaN)))))
     {:xs xs :ys ys :nx nx :ny ny :values values :transforms [tx ty]})))

(defn to-data
  "Data-unit point `[x y]` of a point `[u v]` in `grid`'s transformed coordinates."
  [{[tx ty] :transforms} [u v]]
  [((:inverse tx) (double u)) ((:inverse ty) (double v))])

(defn value-range
  "`[lo hi]` of the finite values of `grids`; `nil` without any."
  [grids]
  (let [vs (for [{:keys [^doubles values]} grids i (range (alength values))
                 :let [v (Array/aget values (long i))] :when (not (m/nan? v))] v)]
    (when (seq vs) [(reduce min vs) (reduce max vs)])))

(defn- cell-start
  "Index of the first of the two grid points around fractional position `t` (in steps from the first point) on an
  axis of `n` points: the last cell for `t` at the far end."
  ^long [^double t ^long n]
  (min (- n 2) (long (m/floor t))))

(defn grid-value
  "Bilinear value of `grid` (see [[sample]]) at data point `x`, `y` (taken into the grid's coordinates): `##NaN`
  outside its domain or next to a `##NaN` value."
  ^double [{:keys [^doubles xs ^doubles ys nx ny ^doubles values] [tx ty] :transforms} ^double x ^double y]
  (let [x (double ((:forward tx) x)) y (double ((:forward ty) y))
        nx (long nx) ny (long ny)
        x0 (Array/aget xs 0) y0 (Array/aget ys 0)
        sx (/ (- (Array/aget xs (dec nx)) x0) (dec nx))
        sy (/ (- (Array/aget ys (dec ny)) y0) (dec ny))
        tx (/ (- x x0) sx)
        ty (/ (- y y0) sy)]
    (if (and (>= tx 0.0) (<= tx (double (dec nx))) (>= ty 0.0) (<= ty (double (dec ny))))
      (let [i (cell-start tx nx) j (cell-start ty ny)
            fx (- tx i) fy (- ty j)
            row (* j nx) row1 (* (inc j) nx)]
        (+ (* (- 1.0 fx) (- 1.0 fy) (Array/aget values (+ i row))) (* fx (- 1.0 fy) (Array/aget values (+ (inc i) row)))
           (* (- 1.0 fx) fy (Array/aget values (+ i row1))) (* fx fy (Array/aget values (+ (inc i) row1)))))
      ##NaN)))

(defn dropped-positions
  "`{:x n :y n}` - rows of the split `groups` (`{panel rows}`) without a finite x / y in `mapping` (zero for a
  function source, which reads no rows)."
  [kind mapping groups]
  (if (= kind :function)
    {:x 0 :y 0}
    (let [rows (mapcat val groups)
          bad (fn [k] (let [acc (data/row-accessor k (mapping k))] (count (remove (comp finite? acc) rows))))]
      {:x (bad :x) :y (bad :y)})))

(defn axis-transforms
  "`[tx ty]` - the stat transforms of the plot's `:x` / `:y` scale specs (see
  [[cljplot2.scale.position/stat-transform]])."
  [plot-scales]
  [(scale/stat-transform (:x plot-scales)) (scale/stat-transform (:y plot-scales))])

(defn source-field
  "`{:f f :domain d}` of the field of one panel / group's `rows` from a chart's `options`, merged `mapping` and the
  plot's scale specs `plot-scales` (see the namespace doc); `nil` for points / gridded data without rows."
  [kind options mapping rows plot-scales]
  (case kind
    :function {:f (:f options) :domain (function-domain (:domain options) plot-scales)}
    :gridded (when (seq rows)
               (lattice rows (data/row-accessor :x (:x mapping)) (data/row-accessor :y (:y mapping))
                        (data/row-accessor :value (:value mapping)) (:interpolation options)))
    :points (let [x-of (data/row-accessor :x (:x mapping))
                  y-of (data/row-accessor :y (:y mapping))
                  pts (filterv (fn [[x y]] (and (finite? x) (finite? y))) (map (juxt x-of y-of) rows))]
              (when (seq pts)
                (density (map first pts) (map second pts) options (axis-transforms plot-scales))))))

;; complex domain colouring

(def wrap-methods
  "Wraps of a magnitude into `[0 1]` for [[domain-color]]."
  #{:log2 :log10 :sin :exp :sigmoid :frac})

(defn wrap
  "Magnitude `v` (>= 0) wrapped into `[0 1]` by `method` (old cljplot's complex plot): `:log2` / `:log10` - the
  fraction of log(v + 1) (bands where |f| + 1 doubles / grows tenfold), `:sin` - sin v scaled to `[0 1]`, `:exp` -
  1 - e^-v, `:sigmoid`, `:frac` - the fraction of v."
  ^double [method ^double v]
  (case method
    :log2 (m/frac (m/log2 (inc v)))
    :log10 (m/frac (m/log10 (inc v)))
    :sin (m/norm (m/sin v) -1.0 1.0 0.0 1.0)
    :exp (- 1.0 (m/exp (- v)))
    :sigmoid (m/sigmoid v)
    :frac (m/frac v)))

(def hue-colorspaces
  "Colour spaces of [[domain-color]]: hue first (clojure2d `colorspaces*`, channels 0-255)."
  #{:HSI :HSB :HSV :HSL :HCL :HWB})

(defn- complex-color
  "Color of complex `z` (old cljplot) through `from-space` (a clojure2d `from-*` converter of a hue-first space):
  hue - arg z, the third channel - the wrapped |z|, the second - 3/4 to full along it; a non-finite argument counts
  as 0, a non-finite wrapped magnitude (a pole) as 1."
  [from-space method ^Vec2 z]
  (let [angle (cx/arg z)
        angle (if (m/valid-double? angle) angle 0.0)
        bright (wrap method (cx/abs z))
        bright (if (m/valid-double? bright) bright 1.0)
        b (* 255.0 bright)]
    (from-space (c/color (m/norm angle m/-PI m/PI 0.0 255.0) (- 255.0 (/ (- 255.0 b) 4.0)) b))))

(defn domain-color
  "A `:pixel-color` function (see [[cljplot2.chart.field]]) colouring a complex function `f` (a `Vec2` -> `Vec2`
  function of `fastmath.complex`, e.g. `cx/log`) by domain colouring: the point `(x, y)` is `x + iy`, the hue the
  argument of f there, the brightness its magnitude wrapped by `:wrap` (default `:log2`, see [[wrap]]), in
  `:colorspace` (default `:HSI` - old cljplot's example; one of [[hue-colorspaces]]). Zeros and poles get a colour.
  Throws `ex-info {:key :wrap}` / `{:key :colorspace}` for an unknown wrap / colour space."
  ([f] (domain-color f {}))
  ([f {:keys [wrap colorspace] :or {wrap :log2 colorspace :HSI}}]
   (when-not (wrap-methods wrap)
     (schema/fail! (str "Domain colouring :wrap should be one of " (sort wrap-methods) ".") {:key :wrap :value wrap}))
   (when-not (hue-colorspaces colorspace)
     (schema/fail! (str "Domain colouring :colorspace should be one of " (sort hue-colorspaces) ".")
                   {:key :colorspace :value colorspace}))
   (let [from-space (second (c/colorspaces* colorspace))]
     (fn [x y] (complex-color from-space wrap (f (cx/complex x y)))))))
