(ns cljplot2.data.vector-field
  "Vector fields `f : R² -> R²` as rows for ordinary charts: points mapped through the field ([[mapped-points]] -
  scatter, bin2d), arrows on a grid ([[arrows]] - `chart/segment` with `:arrow`), traces following the field
  ([[traces]] - `chart/line` grouped by `:id`).

  A field is any function of a pair returning a pair: it receives a `fastmath.vector.Vec2` (so fastmath fields -
  `(fastmath.fields/field :sinusoidal)` - and `(fn [[x y]] ..)` both work, see Points in [[cljplot2.data.field]]) and returns a pair `[u v]` - any sequential value
  or array (a `Vec2`, a vector, a list, a double array; see [[cljplot2.data.rows/as-coordinates]]).
  A result that isn't a pair throws `ex-info {:key :f}`; a pair with a non-finite coordinate is skipped (no row).
  Exceptions thrown by `f` propagate."
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.data.rows :as data]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- pair?
  "Is `r` a pair - any sequential value or array of 2 values (see [[cljplot2.data.rows/as-coordinates]])?"
  [r]
  (= 2 (count (data/as-coordinates r))))

(defn field-value
  "`f` at `(x, y)` as a `Vec2`, `nil` when a coordinate isn't finite; throws `ex-info {:key :f}` when `f` doesn't
  return a pair."
  [f x y]
  (let [r (f (v/vec2 x y))
        uv (data/as-coordinates r)]
    (when-not (= 2 (count uv))
      (schema/fail! "A vector field should return a pair [u v]." {:key :f :value r}))
    (let [[u w] uv]
      (when (and (number? u) (number? w) (m/valid-double? (double u)) (m/valid-double? (double w)))
        (v/vec2 u w)))))

(defn- check-seeds!
  [seeds]
  (when-not (and (sequential? seeds) (every? #(and (pair? %) (every? number? %)) seeds))
    (schema/fail! "Seeds should be a sequence of pairs [x y]." {:key :seeds})))

(defn mapped-points
  "Rows `{:x :y :x0 :y0}` of `seeds` (pairs `[x y]`) mapped through the field `f`: `(x, y)` = f(x0, y0); seeds whose
  image isn't finite are left out."
  {:cljplot2/api true}
  [f seeds]
  (check-seeds! seeds)
  (vec (keep (fn [[x0 y0]]
               (when-let [^Vec2 p (field-value f x0 y0)]
                 {:x (.x p) :y (.y p) :x0 x0 :y0 y0}))
             seeds)))

(def ^:private axis-range
  "`[lo hi]`, finite numbers, `lo < hi`."
  (schema/with-rules [:tuple schema/finite schema/finite]
    [(fn [[lo hi]] (when-not (< (double lo) (double hi)) "should be [lo hi] with lo < hi"))]))

(def arrows-options
  "Options of [[arrows]]."
  [:map {:closed true}
   [:domain {:description "the domain of the arrow grid {:x [lo hi] :y [lo hi]} (required)"}
    [:map {:closed true}
     [:x {:description "x domain [lo hi], lo < hi"} axis-range]
     [:y {:description "y domain [lo hi], lo < hi"} axis-range]]]
   (schema/optional :n {:description "grid cells per axis, or [nx ny]" :default 20}
                    [:or schema/pos-int [:tuple schema/pos-int schema/pos-int]])
   (schema/optional :grid {:description "arrow starts: cell centres (:square) or every second row moved half a cell along x (:shifted)"
                           :default :square}
                    [:enum :square :shifted])
   (schema/optional :scale {:description "arrow length factor: an arrow is scale * cell * (1 - e^-|f|) long" :default 1.0}
                    schema/finite-pos)])

(defn- grid-counts
  "`[nx ny]` of `:n` (a positive integer or a pair of them)."
  [n]
  (if (sequential? n) (mapv long n) [(long n) (long n)]))

(defn- arrow-length
  "Length of the arrow of a vector of magnitude `magnitude` in a cell of side `cell`: `scale * cell * (1 - e^-|v|)`."
  ^double [^double scale ^double cell ^double magnitude]
  (* scale cell (- 1.0 (m/exp (- magnitude)))))

(defn grid-points
  "Arrow starts over `domain` `{:x [lo hi] :y [lo hi]}`: `nx` x `ny` cell centres, rows bottom to top; with
  `shifted?` every second row (from the second) moved half a cell along x. Returns `{:points [[x y] ..] :cell c}`,
  `c` - the smaller cell side."
  [{:keys [x y]} [nx ny] shifted?]
  (let [[x0 x1] x [y0 y1] y
        dx (/ (- (double x1) (double x0)) (long nx))
        dy (/ (- (double y1) (double y0)) (long ny))]
    {:points (vec (for [j (range ny) i (range nx)]
                    [(+ (double x0) (* dx (+ 0.5 (long i))) (if (and shifted? (odd? (long j))) (* 0.5 dx) 0.0))
                     (+ (double y0) (* dy (+ 0.5 (long j))))]))
     :cell (min dx dy)}))

(defn arrows
  "Rows `{:x :y :xend :yend :magnitude}` of the field `f` on a grid over `:domain` `{:x [lo hi] :y [lo hi]}`
  (required): an arrow from each cell centre along f, of length `:scale * cell * (1 - e^-|f|)` (below a cell for
  `:scale` 1 - old cljplot's vector field), `:magnitude` |f|. `:n` - cells per axis (default 20, or `[nx ny]`);
  `:grid` - `:square` (default) or `:shifted` (every second row moved half a cell); `:scale` - default 1.0. Zero
  and non-finite vectors are left out.
  Throws `ex-info` for bad options ([[arrows-options]])."
  {:cljplot2/api true :cljplot2/options arrows-options}
  [f {:keys [domain n grid scale] :or {n 20 grid :square scale 1.0} :as opts}]
  (schema/check-options! arrows-options opts)
  (let [{:keys [points cell]} (grid-points domain (grid-counts n) (= grid :shifted))]
    (vec (keep (fn [[x y]]
                 (when-let [p (field-value f x y)]
                   (let [magnitude (v/mag p)]
                     (when (pos? magnitude)
                       (let [^Vec2 d (v/mult (v/div p magnitude) (arrow-length (double scale) cell magnitude))]
                         {:x x :y y :xend (+ (double x) (.x d)) :yend (+ (double y) (.y d)) :magnitude magnitude})))))
               points))))

(defn- trace-step
  "Next point of a trace at `p`: `p + f(p) * (1 - e^-|f(p)|) * step`, `nil` when f isn't finite there."
  [f ^Vec2 p ^double step]
  (when-let [d (field-value f (.x p) (.y p))]
    (v/add p (v/mult d (* step (- 1.0 (m/exp (- (v/mag d)))))))))

(def traces-options
  "Options of [[traces]]."
  [:map {:closed true}
   (schema/optional :steps {:description "the number of steps of every trace" :default 100} schema/pos-int)
   (schema/optional :step {:description "the step factor: each step moves p by f(p) * (1 - e^-|f(p)|) * step" :default 0.01}
                    schema/finite-pos)])

(defn traces
  "Rows `{:id :i :x :y}` of paths following the field `f` from `seeds` (pairs `[x y]`; `:id` - the seed's index, `:i`
  - the step, 0 at the seed): each step `p + f(p) * (1 - e^-|f(p)|) * :step` (old cljplot's trace), `:steps` steps
  (default 100, `:step` default 0.01); a path ends before a non-finite point.
  Throws `ex-info` for bad options ([[traces-options]]) or seeds (`{:key :seeds}`)."
  {:cljplot2/api true :cljplot2/options traces-options}
  [f seeds {:keys [steps step] :or {steps 100 step 0.01} :as opts}]
  (schema/check-options! traces-options opts)
  (check-seeds! seeds)
  (vec (mapcat (fn [id [x y]]
                 (let [path (->> (iterate #(when % (trace-step f % (double step))) (v/vec2 x y))
                                 (take (inc (long steps)))
                                 (take-while #(and % (m/valid-double? (.x ^Vec2 %)) (m/valid-double? (.y ^Vec2 %)))))]
                   (map-indexed (fn [i ^Vec2 p] {:id id :i i :x (.x p) :y (.y p)}) path)))
               (range) seeds)))
