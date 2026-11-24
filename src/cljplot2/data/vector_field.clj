(ns cljplot2.data.vector-field
  "Vector fields `f : R² -> R²` as rows for ordinary charts: points mapped through the field ([[mapped-points]] -
  scatter, bin2d), arrows on a grid ([[arrows]] - `chart/segment` with `:arrow`), traces following the field
  ([[traces]] - `chart/line` grouped by `:id`).

  A field is any function of a pair returning a pair: it receives a `fastmath.vector.Vec2` (so fastmath fields -
  `(fastmath.fields/field :sinusoidal)` - and functions destructuring it as `[x y]` both work, see Points in [[cljplot2.data.field]]) and returns a pair `[u v]` - any sequential value
  or array (a `Vec2`, a vector, a list, a double array; see [[cljplot2.data.rows/as-coordinates]]).
  A result that isn't a pair throws `ex-info {:key :f}`; a pair with a non-finite coordinate is skipped (no row).
  Exceptions thrown by `f` propagate.

  Functions: `field-value` - the field at a point; [[mapped-points]], [[arrows]] (on `grid-points`) and [[traces]] - the rows."
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.data.rows :as data]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- pair?
  "Tests whether a value is a pair.

  Parameters:

  - `r`: any value.

  Returns `true` for any sequential value or array of 2 values (see [[cljplot2.data.rows/as-coordinates]]), else `false`."
  [r]
  (m/== 2 (count (data/as-coordinates r))))

(defn- field-value
  "Evaluates a vector field at a point.

  Parameters:

  - `f` (function): the field, a function of a `Vec2` returning a pair `[u v]` (see the namespace doc).
  - `x`, `y` (numbers): the point.

  Returns the value as a `Vec2`, or `nil` when a coordinate of it isn't a finite number.

  Throws `ex-info {:key :f}` when `f` doesn't return a pair; exceptions thrown by `f` propagate."
  [f x y]
  (let [r (f (v/vec2 x y))
        uv (data/as-coordinates r)]
    (when-not (m/== 2 (count uv))
      (schema/fail! "A vector field should return a pair [u v]." {:key :f :value r}))
    (let [[u w] uv]
      (when (and (schema/finite? u) (schema/finite? w))
        (v/vec2 u w)))))

(defn- check-seeds!
  "Checks seeds of mapped points or traces.

  Parameters:

  - `seeds`: the seeds, expected a sequence of pairs `[x y]` of numbers.

  Returns `nil`.

  Throws `ex-info {:key :seeds}` for anything else."
  [seeds]
  (when-not (and (sequential? seeds) (every? #(and (pair? %) (every? number? %)) seeds))
    (schema/fail! "Seeds should be a sequence of pairs [x y]." {:key :seeds})))

(defn mapped-points
  "Maps seed points through a vector field.

  Parameters:

  - `f` (function): the field (see the namespace doc).
  - `seeds` (sequence of pairs `[x y]`): the points.

  Returns a vector of rows `{:x :y :x0 :y0}`: `(x, y)` = f(x0, y0); seeds whose image isn't finite are left out.

  Throws `ex-info {:key :seeds}` for bad seeds, `{:key :f}` for a field not returning a pair."
  {:cljplot2/api true}
  [f seeds]
  (check-seeds! seeds)
  (vec (keep (fn [[x0 y0]]
               (when-let [^Vec2 p (field-value f x0 y0)]
                 {:x (.x p) :y (.y p) :x0 x0 :y0 y0}))
             seeds)))

(def arrows-options
  "Options of [[arrows]]."
  [:map {:closed true}
   [:domain {:description "the domain of the arrow grid {:x [lo hi] :y [lo hi]} (required)"}
    [:map {:closed true}
     [:x {:description "x domain [lo hi], lo < hi"} schema/finite-interval]
     [:y {:description "y domain [lo hi], lo < hi"} schema/finite-interval]]]
   (schema/optional :n {:description "grid cells per axis, or [nx ny]" :default 20}
                    [:or schema/pos-int [:tuple schema/pos-int schema/pos-int]])
   (schema/optional :grid {:description "arrow starts: cell centres (:square) or every second row moved half a cell along x (:shifted)"
                           :default :square}
                    [:enum :square :shifted])
   (schema/optional :length-scale {:description "arrow length factor: an arrow is length-scale * cell * (1 - e^-|f|) long" :default 1.0}
                    schema/finite-pos)])

(defn- grid-counts
  "Gives the cell counts of an arrow grid.

  Parameters:

  - `n`: a positive integer or a pair of them.

  Returns `[nx ny]` (longs)."
  [n]
  (if (sequential? n) (mapv long n) [(long n) (long n)]))

(defn- arrow-length
  "Computes the length of an arrow of a field vector.

  Parameters:

  - `scale` (double): the length factor.
  - `cell` (double): the grid cell side.
  - `magnitude` (double): the vector's magnitude.

  Returns `scale * cell * (1 - e^-magnitude)` - below `scale * cell`."
  ^double [^double scale ^double cell ^double magnitude]
  (m/* scale cell (m/- 1.0 (m/exp (m/- magnitude)))))

(defn- grid-points
  "Computes the arrow starts of a grid over a domain.

  Parameters:

  - `domain` (map): `:x`, `:y` - `[lo hi]` intervals.
  - `counts` (`[nx ny]`, longs): cells per axis.
  - `shifted?`: move every second row (from the second) half a cell along x.

  Returns `{:points [[x y] ..] :cell c}`: the `nx` x `ny` cell centres, rows bottom to top; `c` - the smaller cell side."
  [{[^double x0 ^double x1] :x [^double y0 ^double y1] :y} [^long nx ^long ny] shifted?]
  (let [dx (m// (m/- x1 x0) nx)
        dy (m// (m/- y1 y0) ny)]
    {:points (vec (for [^long j (range ny) ^long i (range nx)]
                    [(m/+ x0 (m/* dx (m/+ 0.5 i)) (if (and shifted? (m/odd? (long j))) (m/* 0.5 dx) 0.0))
                     (m/+ y0 (m/* dy (m/+ 0.5 j)))]))
     :cell (m/min dx dy)}))

(defn arrows
  "Computes arrows of a vector field on a grid (old cljplot's vector field).

  Parameters:

  - `f` (function): the field (see the namespace doc).
  - `options` (map, see [[arrows-options]]):
    - `:domain` - `{:x [lo hi] :y [lo hi]}`, the grid's domain (required).
    - `:n` - cells per axis, or `[nx ny]` (default 20).
    - `:grid` - `:square` (default; cell centres) or `:shifted` (every second row moved half a cell along x).
    - `:length-scale` - the arrow length factor (default 1.0).

  Returns a vector of rows `{:x :y :xend :yend :magnitude}`: an arrow from each grid point along f, of length `:length-scale * cell * (1 - e^-|f|)` (below a cell for `:length-scale` 1), `:magnitude` |f|. Zero and non-finite vectors are left out.

  Throws `ex-info` for bad options, `{:key :f}` for a field not returning a pair.

  See also `grid-points`, [[traces]]."
  {:cljplot2/api true :cljplot2/options arrows-options}
  [f {:keys [domain n grid ^double length-scale] :or {n 20 grid :square length-scale 1.0} :as options}]
  (schema/check-options! arrows-options options)
  (let [{:keys [points cell]} (grid-points domain (grid-counts n) (= grid :shifted))]
    (vec (keep (fn [[x y]]
                 (when-let [p (field-value f x y)]
                   (let [magnitude (v/mag p)]
                     (when (m/pos? magnitude)
                       (let [^Vec2 d (v/mult (v/div p magnitude) (arrow-length length-scale cell magnitude))]
                         {:x x :y y :xend (m/+ (double x) (.x d)) :yend (m/+ (double y) (.y d)) :magnitude magnitude})))))
               points))))

(defn- trace-step
  "Computes the next point of a trace following a vector field.

  Parameters:

  - `f` (function): the field.
  - `p` (`Vec2`): the current point.
  - `step` (double): the step factor.

  Returns `p + f(p) * (1 - e^-|f(p)|) * step` as a `Vec2`, `nil` when f isn't finite at `p`."
  [f ^Vec2 p ^double step]
  (when-let [d (field-value f (.x p) (.y p))]
    (v/add p (v/mult d (m/* step (m/- 1.0 (m/exp (m/- (v/mag d)))))))))

(def traces-options
  "Options of [[traces]]."
  [:map {:closed true}
   (schema/optional :steps {:description "the number of steps of every trace" :default 100} schema/pos-int)
   (schema/optional :step {:description "the step factor: each step moves p by f(p) * (1 - e^-|f(p)|) * step" :default 0.01}
                    schema/finite-pos)])

(defn traces
  "Computes paths following a vector field from seed points (old cljplot's trace).

  Parameters:

  - `f` (function): the field (see the namespace doc).
  - `seeds` (sequence of pairs `[x y]`): the path starts.
  - `options` (map, optional, see [[traces-options]]):
    - `:steps` - the number of steps of every path (default 100).
    - `:step` - the step factor (default 0.01): each step moves `p` by `f(p) * (1 - e^-|f(p)|) * step`.

  Returns a vector of rows `{:id :i :x :y}`: `:id` - the seed's index, `:i` - the step, 0 at the seed; a path ends before a non-finite point.

  Throws `ex-info` for bad options, `{:key :seeds}` for bad seeds, `{:key :f}` for a field not returning a pair.

  See also [[arrows]], [[mapped-points]]."
  {:cljplot2/api true :cljplot2/options traces-options}
  ([f seeds] (traces f seeds {}))
  ([f seeds {:keys [^long steps ^double step] :or {steps 100 step 0.01} :as options}]
   (schema/check-options! traces-options options)
   (check-seeds! seeds)
   (vec (mapcat (fn [id [x y]]
                  (let [path (->> (iterate #(when % (trace-step f % step)) (v/vec2 x y))
                                  (take (m/long-inc steps))
                                  (take-while data/finite-point?))]
                    (map-indexed (fn [i ^Vec2 p] {:id id :i i :x (.x p) :y (.y p)}) path)))
                (range) seeds))))
