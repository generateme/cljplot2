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
  domain end to end; values that aren't finite numbers are `##NaN` (not drawn, not contoured)."
  (:require [fastmath.core :as m]
            [fastmath.interpolation :as interp]
            [fastmath.kernel :as k]
            [cljplot2.data.rows :as data]
            [cljplot2.schema :as schema])
  (:import [fastmath.java Array]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def bandwidth-schema
  "Schema of `:bandwidth`: a positive number, a selector keyword, or a pair of them."
  (let [one [:or [:and schema/finite [:> 0]] [:enum :nrd :nrd0 :nrd-adjust :sj :sj-dpi :rlcv :lcv :lscv :scott]]]
    [:or one [:tuple one one]]))

(def options
  "Map entries of the field options shared by the field and contour charts."
  [(schema/optional :f schema/function)
   (schema/optional :domain [:map {:closed true} [:x [:tuple schema/finite schema/finite]] [:y [:tuple schema/finite schema/finite]]])
   (schema/optional :kernel keyword?)
   (schema/optional :bandwidth bandwidth-schema)
   (schema/optional :binned? :boolean)
   (schema/optional :cut [:and schema/finite [:>= 0]])
   (schema/optional :interpolation [:enum :bilinear :bicubic :cubic-2d])
   (schema/optional :n [:or [:int {:min 2 :max 2000}] [:tuple [:int {:min 2 :max 2000}] [:int {:min 2 :max 2000}]]])])

(defn source-kind
  "The source of a field chart: `:function` with option `:f`, `:gridded` with a mapped `:value`, else `:points`."
  [options mapping]
  (cond
    (:f options) :function
    (some? (:value mapping)) :gridded
    :else :points))

(defn- finite? [v] (and (number? v) (m/valid-double? (double v))))

(defn density
  "`{:f f :domain [[x0 x1] [y0 y1]]}` - the 2D KDE of the points `xs`, `ys` (finite numbers, equal counts, at least
  one; see the namespace doc for `:kernel`, `:bandwidth`, `:binned?`, `:cut`); `positive` - `[x? y?]`, axes whose
  domain must stay positive (log scales): widening stops at the data minimum when it would reach 0. Throws `ex-info
  {:key :bandwidth}` when fastmath can't estimate (e.g. a selector on one point)."
  [xs ys {:keys [kernel bandwidth binned? cut]} positive]
  (let [{:keys [kde h]} (try (k/kernel-density-2d (or kernel :gaussian) xs ys
                                                   {:bandwidth (or bandwidth :nrd) :binned? (boolean binned?)} true)
                             (catch clojure.lang.ExceptionInfo e
                               (schema/fail! (str "Density estimation failed: " (ex-message e)) {:key :bandwidth :cause (ex-data e)})))
        [hx hy] h
        cut (double (if (nil? cut) 3.0 cut))
        widen (fn [vs ^double hw positive?]
                (let [lo (double (reduce min vs)) wide (- lo (* cut hw))]
                  [(if (and positive? (<= wide 0.0)) lo wide) (+ (double (reduce max vs)) (* cut hw))]))
        [px py] positive]
    {:f kde :domain [(widen xs (double hx) px) (widen ys (double hy) py)]}))

(defn lattice
  "`{:f f :domain [[x0 x1] [y0 y1]]}` - the interpolation (`interpolation`, see the namespace doc) of the gridded
  `rows` (`x-of`, `y-of`, `value-of` - row accessors): every distinct x with every distinct y exactly once, at least 2
  of each. Rows with a non-finite x or y are left out; a missing or repeated lattice point, or fewer than 2 x / y
  values, throws `ex-info {:key :value}`."
  [rows x-of y-of value-of interpolation]
  (let [points (filterv (fn [[x y]] (and (finite? x) (finite? y))) (map (juxt x-of y-of value-of) rows))
        xs (vec (sort (distinct (map #(double (first %)) points))))
        ys (vec (sort (distinct (map #(double (second %)) points))))
        by-point (group-by (fn [[x y]] [(double x) (double y)]) points)]
    (when (or (< (count xs) 2) (< (count ys) 2))
      (schema/fail! "A gridded field needs at least 2 distinct x and 2 distinct y values." {:key :value :xs (count xs) :ys (count ys)}))
    (when-let [[point] (first (filter #(> (count (val %)) 1) by-point))]
      (schema/fail! "A gridded field has one :value per (x, y); a lattice point repeats." {:key :value :value point}))
    (when-let [missing (first (for [x xs y ys :when (not (contains? by-point [x y]))] [x y]))]
      (schema/fail! "A gridded field needs a :value at every (x, y) of its lattice; a point is missing." {:key :value :value missing}))
    (let [value (fn [x y] (let [v (nth (first (by-point [x y])) 2)] (if (finite? v) (double v) ##NaN)))
          vss (mapv (fn [x] (mapv (fn [y] (value x y)) ys)) xs)
          make (case (or interpolation :bilinear) :bilinear interp/bilinear :bicubic interp/bicubic :cubic-2d interp/cubic-2d)]
      {:f (make xs ys vss) :domain [[(first xs) (peek xs)] [(first ys) (peek ys)]]})))

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
  "The grid of field function `f` over `domain` (`[[x0 x1] [y0 y1]]`) with `[nx ny]` points:
  `{:xs doubles :ys doubles :nx :ny :values doubles}` - `values` row by row (`i + j nx` for `xs[i]`, `ys[j]`),
  `##NaN` where `f` doesn't return a finite number. Exceptions `f` throws propagate."
  [f [[x0 x1] [y0 y1]] [nx ny]]
  (let [nx (long nx) ny (long ny)
        ^doubles xs (axis-points x0 x1 nx)
        ^doubles ys (axis-points y0 y1 ny)
        values (double-array (* nx ny))]
    (dotimes [j ny]
      (dotimes [i nx]
        (let [v (f (Array/aget xs i) (Array/aget ys j))]
          (Array/aset values (+ i (* j nx)) (if (finite? v) (double v) ##NaN)))))
    {:xs xs :ys ys :nx nx :ny ny :values values}))

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
  "Bilinear value of `grid` (see [[sample]]) at `x`, `y`: `##NaN` outside its domain or next to a `##NaN` value."
  ^double [{:keys [^doubles xs ^doubles ys nx ny ^doubles values]} ^double x ^double y]
  (let [nx (long nx) ny (long ny)
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
                (density (map first pts) (map second pts) options
                         (mapv #(= :log (get-in plot-scales [% :kind])) [:x :y]))))))
