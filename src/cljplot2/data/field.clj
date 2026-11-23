(ns cljplot2.data.field
  "Fields - a value at every point of a 2D domain - sampled on a regular grid for [[cljplot2.chart.field]] (raster)
  and [[cljplot2.chart.contour]] (contour lines).

  Sources ([[source-kind]]):

  * `:points` - mapped `:x`, `:y`: the 2D kernel density estimate of the points (`fastmath.kernel/kernel-density-2d`,
    a product kernel, a bandwidth per axis); options `:kernel` (default `:gaussian`), `:bandwidth` (a number, a
    fastmath selector, `:scott`, or a pair `[hx hy]` of them; default `:nrd` per axis - `MASS::kde2d`, as ggplot2),
    `:binned?`; the domain is the points' range widened by `:cut` (default 3) bandwidths on each axis
  * `:gridded` - mapped `:x`, `:y`, `:value` on a full regular lattice (every x with every y, once):
    `fastmath.interpolation` `:interpolation` - `:bilinear` (default), `:bicubic`, `:cubic-2d`; the domain is the
    lattice
  * `:function` - option `:f`, a function of a point `[x y]` (see Points below) returning a number; the domain - option `:domain {:x [lo hi] :y
    [lo hi]}`, else the plot's explicit `:x` / `:y` scale domains

  A grid (see [[sample]]) holds `:n` x values by y values (option `:n`, default 100, or `[nx ny]`) spanning the
  domain end to end; values that aren't finite numbers are `##NaN` (not drawn, not contoured).

  Scales: on a `:log` axis the density is estimated, and every grid sampled, in log10 coordinates (see
  [[cljplot2.scale.position/stat-transform]]) - contours follow the points as the axis shows them, grid points are
  even on screen; the density value is then per log10 unit of that axis.

  Points: every function of a 2D point here (`:f`, `:pixel-color`, [[domain-color]]'s result, the 3D surfaces' `:f`
  / `:fu`, vector fields) receives one argument, a `fastmath.vector.Vec2` - sequential and indexed, so `(fn [[x y]]
  ..)`, `nth`, `seq` and fastmath vector functions all work; point-valued results may be any sequential value or array
  (see [[cljplot2.data.rows/as-coordinates]]).

  Functions: options - [[options]], [[bandwidth-schema]], [[n-schema]], [[grid-size]]; sources - [[source-kind]], [[source-field]], [[density]], [[lattice]], [[lattice-values]], [[function-domain]]; grids - [[sample]], [[union-domain]], [[to-data]], [[value-range]], [[grid-value]], [[axis-transforms]], [[dropped-positions]]; complex domain colouring - [[domain-color]], [[wrap]]."
  (:require [clojure2d.color :as c]
            [fastmath.complex :as cx]
            [fastmath.core :as m]
            [fastmath.vector :as v]
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
  (let [one [:or schema/finite-pos [:enum :nrd :nrd0 :nrd-adjust :sj :sj-dpi :rlcv :lcv :lscv :scott]]]
    [:or one [:tuple one one]]))

(def n-schema
  "Schema of `:n`, a grid size: an integer for both axes or a pair `[nx ny]`, each in [2 2000]."
  [:or [:int {:min 2 :max 2000}] [:tuple [:int {:min 2 :max 2000}] [:int {:min 2 :max 2000}]]])

(def options
  "Map entries of the field options shared by the field and contour charts."
  (schema/in-fragment
   :field
   [(schema/optional :f {:cljplot2/section :function :description "the field as a function of a point [x y] returning a number (instead of mapped data)"}
                     schema/function)
    (schema/optional :domain {:cljplot2/section :function :description "the domain of a function field"
                              :cljplot2/default-doc "the plot's explicit :x and :y scale domains"}
                     [:map {:closed true}
                      [:x {:description "x domain [lo hi]"} [:tuple schema/finite schema/finite]]
                      [:y {:description "y domain [lo hi]"} [:tuple schema/finite schema/finite]]])
    (schema/optional :kernel {:cljplot2/section :density :description "fastmath kernel of the 2D density estimate of mapped points"
                              :default :gaussian}
                     keyword?)
    (schema/optional :bandwidth {:cljplot2/section :density :description "bandwidth of the density estimate: a number, a fastmath selector or :scott, or a pair [hx hy] of them"
                                 :cljplot2/default-doc ":nrd per axis (MASS::kde2d, as ggplot2)"}
                     bandwidth-schema)
    (schema/optional :binned? {:cljplot2/section :density :description "estimate the density from binned points (faster for many points)"
                               :default false}
                     :boolean)
    (schema/optional :cut {:cljplot2/section :density :description "how many bandwidths the density domain extends past the points on each side"
                           :default 3}
                     schema/finite-non-neg)
    (schema/optional :interpolation {:cljplot2/section :grid :description "interpolation of gridded data (mapped :x, :y, :value on a full lattice)"
                                     :default :bilinear}
                     [:enum :bilinear :bicubic :cubic-2d])
    (schema/optional :n {:cljplot2/section :grid :description "grid size: values sampled along each axis, or [nx ny]"
                         :default 100}
                     n-schema)]))

(defn source-kind
  "Gives the source kind of a field chart (see the namespace doc).

  Parameters:

  - `field-options` (map): the chart's options, `:f` read.
  - `mapping` (map): the merged mapping, `:value` read.

  Returns `:function` with option `:f`, `:gridded` with a mapped `:value`, else `:points`.

  See also [[source-field]]."
  [field-options mapping]
  (cond
    (:f field-options) :function
    (some? (:value mapping)) :gridded
    :else :points))

(defn- finite?
  "Tests whether a value is a finite number.

  Parameters:

  - `v`: any value.

  Returns true for a number neither infinite nor `NaN`, else false."
  [v]
  (and (number? v) (m/valid-double? (double v))))

(def ^:private estimate-error-keys
  "fastmath `:property` of a density error -> the chart option to blame."
  {:kernel :kernel :binned? :binned? :bandwidth :bandwidth :bandwidth-options :bandwidth})

(defn density
  "Gives the 2D kernel density estimate of points, estimated in the coordinates of the axis transforms.

  Parameters:

  - `xs`, `ys` (sequences of finite numbers): the points' coordinates, equal counts, at least one point.
  - `options` (map): `:kernel` (default `:gaussian`), `:bandwidth` (default `:nrd` per axis), `:binned?` (default false), `:cut` - bandwidths the domain extends past the points (default 3); see the namespace doc.
  - `[tx ty]`: the axis stat transforms (see [[cljplot2.scale.position/stat-transform]]); points a transform can't place are left out.

  Returns `{:f f :domain [[x0 x1] [y0 y1]]}`: `f` - the density, a function of a point `[x y]` in data units (per log10 unit on a log axis); the domain - the points' transformed range widened by `:cut` bandwidths, in data units.

  Throws `ex-info {:key :x}` when no point can be placed, and `ex-info` keyed by the option fastmath blames (`:kernel`, `:binned?`, else `:bandwidth`) when it can't estimate (e.g. a selector on one point).

  See also [[source-field]]."
  [xs ys {:keys [kernel bandwidth binned? cut]} [tx ty]]
  (let [fx (:forward tx)
        fy (:forward ty)
        pts (filterv (fn [[^double u ^double v]] (and (m/valid-double? u) (m/valid-double? v)))
                     (map (fn [^double x ^double y] [(fx x) (fy y)]) xs ys))
        _ (when (empty? pts)
            (schema/fail! "No points the axis scales can place (e.g. only non-positive values on a log axis)." {:key :x}))
        us (mapv first pts)
        vs (mapv second pts)
        {:keys [kde h]} (try (k/kernel-density-2d (or kernel :gaussian) us vs
                                                  {:bandwidth (or bandwidth :nrd)
                                                   :binned? binned?} true)
                             (catch clojure.lang.ExceptionInfo e
                               (schema/fail! (str "Density estimation failed: " (ex-message e))
                                             {:key (get estimate-error-keys (:property (ex-data e)) :bandwidth) :cause (ex-data e)})))
        [^double hx ^double hy] h
        reach (double (if (nil? cut) 3.0 cut))
        widen (fn [ws ^double hw inverse]
                [(inverse (m/- (v/mn ws) (m/* reach hw)))
                 (inverse (m/+ (v/mx ws) (m/* reach hw)))])]
    {:f (fn [[^double x ^double y]] (kde (fx x) (fy y)))
     :domain [(widen us hx (:inverse tx)) (widen vs hy (:inverse ty))]}))

(defn lattice-values
  "Gives the values of gridded rows on their full lattice.

  Every distinct x must come with every distinct y exactly once, at least 2 of each; rows with a non-finite x or y are left out.

  Parameters:

  - `rows` (sequence): the data rows.
  - `x-of`, `y-of`, `value-of` (functions): row accessors of x, y and the value.
  - `value-key` (keyword): the option or mapping key errors are keyed by.

  Returns `{:xs [..] :ys [..] :vss [[..] ..]}`: the sorted distinct x and y values (doubles) and per x the values at every y (`##NaN` for a non-finite value).

  Throws `ex-info {:key value-key}` for fewer than 2 distinct x or y values, a repeated or a missing lattice point.

  See also [[lattice]]."
  [rows x-of y-of value-of value-key]
  (let [points (->> (map (juxt x-of y-of value-of) rows)
                    (filterv (fn [[x y]] (and (finite? x) (finite? y)))))
        xs (vec (sort (distinct (map (comp double first) points))))
        ys (vec (sort (distinct (map (comp double second) points))))
        by-point (group-by v/vec2 points)
        value-name (name value-key)]
    (when (or (m/< (count xs) 2) (m/< (count ys) 2))
      (schema/fail! "A gridded field needs at least 2 distinct x and 2 distinct y values." {:key value-key :xs (count xs) :ys (count ys)}))
    (when-let [[point] (first (filter #(m/> (count (val %)) 1) by-point))]
      (schema/fail! (str "A gridded field has one :" value-name " per (x, y); a lattice point repeats.") {:key value-key :value point}))
    (when-let [missing (first (for [x xs y ys :when (not (contains? by-point [x y]))] [x y]))]
      (schema/fail! (str "A gridded field needs a :" value-name " at every (x, y) of its lattice; a point is missing.")
                    {:key value-key :value missing}))
    (let [value (fn [x y] (let [v (nth (first (by-point [x y])) 2)] (if (finite? v) (double v) ##NaN)))]
      {:xs xs :ys ys :vss (mapv (fn [x] (mapv (fn [y] (value x y)) ys)) xs)})))

(defn lattice
  "Gives the interpolated field of gridded rows.

  Parameters:

  - `rows` (sequence): the data rows on a full lattice (see [[lattice-values]]).
  - `x-of`, `y-of`, `value-of` (functions): row accessors of x, y and the value.
  - `interpolation` (keyword): `:bilinear` (default when `nil`), `:bicubic` or `:cubic-2d` (`fastmath.interpolation`).

  Returns `{:f f :domain [[x0 x1] [y0 y1]]}`: `f` - the interpolation, a function of a point `[x y]`; the domain - the lattice's.

  Throws the errors of [[lattice-values]], keyed `:value`."
  [rows x-of y-of value-of interpolation]
  (let [{:keys [xs ys vss]} (lattice-values rows x-of y-of value-of :value)
        make (case (or interpolation :bilinear) :bilinear interp/bilinear :bicubic interp/bicubic :cubic-2d interp/cubic-2d)
        g (make xs ys vss)]
    {:f (fn [[x y]] (g x y)) :domain [[(first xs) (peek xs)] [(first ys) (peek ys)]]}))

(defn function-domain
  "Gives the domain of a function field.

  Parameters:

  - `domain` (map): the option `{:x [lo hi] :y [lo hi]}`, or `nil`.
  - `plot-scales` (map): the plot's scale specs; the explicit numeric `:domain`s of `:x` and `:y` used when the option lacks an axis.

  Returns `[[x0 x1] [y0 y1]]` of doubles.

  Throws `ex-info {:key :domain}` when an axis has neither."
  [domain plot-scales]
  (let [explicit (fn [k] (let [d (get-in plot-scales [k :domain])] (when (and (m/== 2 (count d)) (every? finite? d)) (mapv double d))))
        x (or (:x domain) (explicit :x))
        y (or (:y domain) (explicit :y))]
    (when-not (and x y)
      (schema/fail! "A function field needs a :domain {:x [lo hi] :y [lo hi]} (or explicit :x and :y scale domains)."
                    {:key :domain}))
    [(mapv double x) (mapv double y)]))

(defn grid-size
  "Gives the grid size of the option `:n`.

  Parameters:

  - `n`: an integer for both axes, a pair `[nx ny]`, or `nil` (default 100).

  Returns `[nx ny]` of longs."
  [n]
  (cond
    (nil? n) [100 100]
    (sequential? n) (mapv long n)
    :else [(long n) (long n)]))

(defn union-domain
  "Gives the smallest domain covering several domains.

  Parameters:

  - `domains` (sequence): domains `[[x0 x1] [y0 y1]]`.

  Returns the covering domain `[[x0 x1] [y0 y1]]`; `nil` for no domains."
  [domains]
  (when (seq domains)
    (let [axis (fn [k] [(v/mn (map #(first (nth % k)) domains))
                       (v/mx (map #(second (nth % k)) domains))])]
      [(axis 0) (axis 1)])))

(defn- axis-points
  "Gives equally spaced values along a grid axis.

  Parameters:

  - `lo`, `hi` (doubles): the first and the last value.
  - `n` (long): the number of values.

  Returns a new double array of `n` values from `lo` to `hi` (a zero-width axis: all at `lo`)."
  ^doubles [^double lo ^double hi ^long n]
  (double-array (m/slice-range lo hi n)))

(defn sample
  "Samples a field function on a regular grid.

  Parameters:

  - `f` (function): of a point `Vec2` `[x y]` in data units, returning a number.
  - `[[x0 x1] [y0 y1]]`: the domain, data units.
  - `[nx ny]`: points per axis, evenly spaced in the transformed coordinates.
  - `transforms`: `[tx ty]` - the axis stat transforms (see [[cljplot2.scale.position/stat-transform]]); `nil` or omitted - identity.

  Returns `{:xs :ys :nx :ny :values :transforms [tx ty]}`: `xs`, `ys` - double arrays in transformed coordinates; `values` - a double array row by row (`i + j nx` for `xs[i]`, `ys[j]`), `##NaN` where `f` doesn't return a finite number.

  Exceptions `f` throws propagate.

  See also [[grid-value]], [[value-range]], [[to-data]]."
  ([f domain size] (sample f domain size nil))
  ([f [[^double x0 ^double x1] [^double y0 ^double y1]] [^long nx ^long ny] transforms]
   (let [[tx ty] (or transforms [(scale/stat-transform nil) (scale/stat-transform nil)])
         fx (:forward tx)
         fy (:forward ty)
         ix (:inverse tx)
         iy (:inverse ty)
         ^doubles xs (axis-points (fx x0) (fx x1) nx)
         ^doubles ys (axis-points (fy y0) (fy y1) ny)
         values (double-array (m/long-mult nx ny))]
     (dotimes [j ny]
       (dotimes [i nx]
         (let [v (f (Vec2. (ix (Array/aget xs i)) (iy (Array/aget ys j))))]
           (Array/aset values (m/+ i (m/* j nx)) (if (finite? v) (double v) ##NaN)))))
     {:xs xs :ys ys :nx nx :ny ny :values values :transforms [tx ty]})))

(defn to-data
  "Gives the data-unit point of a point in a grid's transformed coordinates.

  Parameters:

  - `grid` (map): a grid of [[sample]], its `:transforms` used.
  - `[u v]`: the point in transformed coordinates.

  Returns `[x y]` in data units."
  [{[tx ty] :transforms} [^double u ^double v]]
  [((:inverse tx) u) ((:inverse ty) v)])

(defn value-range
  "Gives the range of the values of grids.

  Parameters:

  - `grids` (sequence): grids of [[sample]].

  Returns `[lo hi]` of their non-`NaN` values; `nil` without any."
  [grids]
  (let [vs (for [{:keys [^doubles values]} grids
                 ^long i (range (alength values))
                 :let [v (Array/aget values i)]
                 :when (not (m/nan? v))] v)]
    (when (seq vs) [(v/mn vs) (v/mx vs)])))

(defn- cell-start
  "Gives the index of the grid cell holding a fractional position on an axis.

  Parameters:

  - `t` (double): the position in steps from the first point, non-negative.
  - `n` (long): the axis's number of points, at least 2.

  Returns the index of the first of the two points around `t`; the last cell for `t` at the far end."
  ^long [^double t ^long n]
  (m/long-min (m/long-sub n 2) (long (m/floor t))))

(defn grid-value
  "Gives the bilinear value of a grid at a data point.

  Parameters:

  - `grid` (map): a grid of [[sample]].
  - `x`, `y` (doubles): the point in data units, taken into the grid's coordinates.

  Returns the interpolated value; `##NaN` outside the grid's domain or next to a `##NaN` value."
  ^double [{:keys [^doubles xs ^doubles ys ^long nx ^long ny ^doubles values] [tx ty] :transforms} ^double x ^double y]
  (let [gx (double ((:forward tx) x))
        gy (double ((:forward ty) y))
        x0 (Array/aget xs 0)
        y0 (Array/aget ys 0)
        nx- (m/long-dec nx)
        ny- (m/long-dec ny)
        sx (m// (m/- (Array/aget xs nx-) x0) nx-)
        sy (m// (m/- (Array/aget ys ny-) y0) ny-)
        steps-x (m// (m/- gx x0) sx)
        steps-y (m// (m/- gy y0) sy)]
    (if (and (m/not-neg? steps-x) (m/<= steps-x nx-) (m/not-neg? steps-y) (m/<= steps-y ny-))
      (let [i (cell-start steps-x nx)
            j (cell-start steps-y ny)
            fx (m/- steps-x i)
            fy (m/- steps-y j)
            row (m/* j nx)
            row1 (m/* (m/long-inc j) nx)]
        (m/+ (m/* (m/- 1.0 fx) (m/- 1.0 fy) (Array/aget values (m/long-add i row)))
             (m/* fx (m/- 1.0 fy) (Array/aget values (m/long-add (m/long-inc i) row)))
             (m/* (m/- 1.0 fx) fy (Array/aget values (m/long-add i row1))) (m/* fx fy (Array/aget values (m/long-add (m/long-inc i) row1)))))
      ##NaN)))

(defn dropped-positions
  "Counts of the rows a field chart drops for a non-finite position.

  Parameters:

  - `kind` (keyword): the source kind (see [[source-kind]]).
  - `mapping` (map): the merged mapping, `:x` and `:y` read.
  - `groups` (map): the split rows, panel -> rows.

  Returns `{:x n :y n}` - the rows without a finite x and y; zeros for a function source, which reads no rows."
  [kind mapping groups]
  (if (= kind :function)
    {:x 0 :y 0}
    (let [rows (mapcat val groups)
          bad (fn [k] (let [acc (data/row-accessor k (mapping k))] (count (remove (comp finite? acc) rows))))]
      {:x (bad :x) :y (bad :y)})))

(defn axis-transforms
  "Gives the stat transforms of the plot's position axes.

  Parameters:

  - `plot-scales` (map): the plot's scale specs, `:x` and `:y` read.

  Returns `[tx ty]` (see [[cljplot2.scale.position/stat-transform]])."
  [plot-scales]
  [(scale/stat-transform (:x plot-scales)) (scale/stat-transform (:y plot-scales))])

(defn source-field
  "Gives the field of one panel's or group's rows (see the namespace doc).

  Parameters:

  - `kind` (keyword): the source kind of [[source-kind]].
  - `field-options` (map): the chart's field options (see [[options]]).
  - `mapping` (map): the merged mapping.
  - `rows` (sequence): the rows.
  - `plot-scales` (map): the plot's scale specs.

  Returns `{:f f :domain [[x0 x1] [y0 y1]]}`; `nil` for points or gridded data without rows (points: without finite ones).

  Throws the errors of [[function-domain]], [[lattice]] and [[density]]."
  [kind field-options mapping rows plot-scales]
  (case kind
    :function {:f (:f field-options) :domain (function-domain (:domain field-options) plot-scales)}
    :gridded (when (seq rows)
               (lattice rows (data/row-accessor :x (:x mapping)) (data/row-accessor :y (:y mapping))
                        (data/row-accessor :value (:value mapping)) (:interpolation field-options)))
    :points (let [x-of (data/row-accessor :x (:x mapping))
                  y-of (data/row-accessor :y (:y mapping))
                  pts (filterv (fn [[x y]] (and (finite? x) (finite? y))) (map (juxt x-of y-of) rows))]
              (when (seq pts)
                (density (map first pts) (map second pts) field-options (axis-transforms plot-scales))))))

;; complex domain colouring

(def wrap-methods
  "Wraps of a magnitude into `[0 1]` for [[domain-color]]."
  #{:log2 :log10 :sin :exp :sigmoid :frac})

(defn wrap
  "Wraps a magnitude into `[0 1]` (old cljplot's complex plot).

  Parameters:

  - `method` (keyword): one of [[wrap-methods]] - `:log2` and `:log10` - the fraction of log(v + 1) (bands where |f| + 1 doubles or grows tenfold), `:sin` - sin v scaled to `[0 1]`, `:exp` - 1 - e^-v, `:sigmoid`, `:frac` - the fraction of v.
  - `v` (double): the magnitude, non-negative.

  Returns the wrapped value as a double (`NaN` or infinite for a non-finite `v`)."
  ^double [method ^double v]
  (case method
    :log2 (m/frac (m/log2 (m/inc v)))
    :log10 (m/frac (m/log10 (m/inc v)))
    :sin (m/norm (m/sin v) -1.0 1.0 0.0 1.0)
    :exp (m/- 1.0 (m/exp (m/- v)))
    :sigmoid (m/sigmoid v)
    :frac (m/frac v)))

(def hue-colorspaces
  "Colour spaces of [[domain-color]]: hue first (clojure2d `colorspaces*`, channels 0-255)."
  #{:HSI :HSB :HSV :HSL :HCL :HWB})

(defn- complex-color
  "Gives the colour of a complex number by domain colouring (old cljplot).

  Parameters:

  - `from-space` (function): a clojure2d `from-*` converter of a hue-first colour space.
  - `method` (keyword): the magnitude wrap, see [[wrap]].
  - `z` (`Vec2`): the complex number.

  Returns the colour: hue - arg z, the third channel - the wrapped |z|, the second - 3/4 to full along it; a non-finite argument counts as 0, a non-finite wrapped magnitude (a pole) as 1."
  [from-space method ^Vec2 z]
  (let [raw-angle (cx/arg z)
        angle (if (m/valid-double? raw-angle) raw-angle 0.0)
        wrapped (wrap method (cx/abs z))
        bright (if (m/valid-double? wrapped) wrapped 1.0)
        b (m/* 255.0 bright)]
    (from-space (c/color (m/norm angle m/-PI m/PI 0.0 255.0) (m/- 255.0 (m// (m/- 255.0 b) 4.0)) b))))

(def domain-color-options
  "Options of [[domain-color]]."
  [:map {:closed true}
   (schema/optional :wrap {:description "how the magnitude of f is wrapped into [0 1] for the brightness (see cljplot2.data.field/wrap)"
                           :default :log2}
                    (into [:enum] (sort wrap-methods)))
   (schema/optional :colorspace {:description "the hue-first colour space of the colours (:HSI - old cljplot's example)"
                                 :default :HSI}
                    (into [:enum] (sort hue-colorspaces)))])

(defn domain-color
  "Gives the domain colouring of a complex function, as a `:pixel-color` function of [[cljplot2.chart.field]].

  Parameters:

  - `f` (function): a complex function `Vec2` -> `Vec2` of `fastmath.complex`, e.g. `cx/log`.
  - `opts` (map): `:wrap` - how the magnitude is wrapped for the brightness (default `:log2`, see [[wrap]]); `:colorspace` - one of [[hue-colorspaces]] (default `:HSI` - old cljplot's example).

  Returns a function of a point `[x y]` giving the colour of `f(x + iy)`: hue - its argument, brightness - its wrapped magnitude. Zeros and poles get a colour.

  Throws `ex-info` for bad options ([[domain-color-options]])."
  {:cljplot2/api true :cljplot2/options domain-color-options}
  ([f] (domain-color f {}))
  ([f {wrap-method :wrap colorspace :colorspace :or {wrap-method :log2 colorspace :HSI} :as opts}]
   (schema/check-options! domain-color-options opts)
   (let [from-space (second (c/colorspaces* colorspace))]
     (fn [[x y]] (complex-color from-space wrap-method (f (cx/complex x y)))))))

