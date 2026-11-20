(ns cljplot2.data.contour
  "Contours of a sampled field (a grid, see [[cljplot2.data.field/sample]]) by marching squares.

  [[isolines]] - the lines where the field crosses a level: each grid cell (4 corner values, none `##NaN`) gives 0,
  1 or 2 segments between points on its edges (linear interpolation between the corner values); a saddle (diagonal
  corners on the same side) is resolved by the cell's average value. Segments sharing an edge point are joined into
  polylines, closed when they come back to their start. A value equal to the level counts as above it.

  [[level-rings]] - the outline of the region at or above a level, for filling: the grid padded with `-Inf` (and
  `##NaN` values taken as `-Inf`) so every line closes; on the domain edge the outline runs along the grid. Filled with
  the even-odd rule the rings give the region, holes included.

  [[levels]] - contour levels evenly spread over a range."
  (:require [fastmath.core :as m])
  (:import [fastmath.java Array]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn levels
  "Contour levels for the value range `[lo hi]`: `n` (an integer) levels evenly spread inside it (the ends left out),
  or the given vector of levels (sorted). `[]` for no range or a zero-width one with an integer `n`."
  [[lo hi] n]
  (cond
    (sequential? n) (vec (sort n))
    (or (nil? lo) (== (double lo) (double hi))) []
    :else (let [lo (double lo) step (/ (- (double hi) lo) (inc (long n)))]
            (mapv #(+ lo (* step (double %))) (range 1 (inc (long n)))))))

;; marching squares

(def ^:private cell-segments
  "Edge pairs of the segments of each corner case (bit 0 - bottom-left at or above the level, 1 - bottom-right,
  2 - top-right, 3 - top-left); saddles 5 and 10 by `:high` / `:low` cell centre."
  {1 [[:b :l]] 2 [[:b :r]] 3 [[:l :r]] 4 [[:r :t]]
   5 {:high [[:b :r] [:t :l]] :low [[:b :l] [:r :t]]}
   6 [[:b :t]] 7 [[:l :t]] 8 [[:t :l]] 9 [[:b :t]]
   10 {:high [[:b :l] [:r :t]] :low [[:b :r] [:t :l]]}
   11 [[:r :t]] 12 [[:l :r]] 13 [[:b :r]] 14 [[:b :l]]})

(defn- edge-id
  "Id of edge `side` of cell `(i, j)`: `[:h i j]` - the horizontal edge from point `(i, j)` to `(i+1, j)`, `[:v i j]` -
  the vertical edge from `(i, j)` to `(i, j+1)`; neighbouring cells share it."
  [side ^long i ^long j]
  (case side
    :b [:h i j]
    :t [:h i (inc j)]
    :l [:v i j]
    :r [:v (inc i) j]))

(defn- crossing
  "The point where the level crosses edge `[a b]` (`a`, `b` - `[x y v]`): linear between the values; at the finite end
  when the other value is `-Inf`."
  [^double level [ax ay av] [bx by bv]]
  (let [av (double av) bv (double bv)]
    (cond
      (== av ##-Inf) [bx by]
      (== bv ##-Inf) [ax ay]
      :else (let [t (/ (- level av) (- bv av))]
              [(+ (double ax) (* t (- (double bx) (double ax)))) (+ (double ay) (* t (- (double by) (double ay))))]))))

(defn- edge-ends
  "The two grid points `[x y v]` of edge `id`."
  [{:keys [^doubles xs ^doubles ys nx ^doubles values]} [kind i j]]
  (let [nx (long nx) i (long i) j (long j)
        point (fn [^long a ^long b] [(Array/aget xs a) (Array/aget ys b) (Array/aget values (+ a (* b nx)))])]
    (if (= kind :h) [(point i j) (point (inc i) j)] [(point i j) (point i (inc j))])))

(defn- segments
  "Segments `[edge-id edge-id]` of every cell of `grid` crossing `level` (cells with a `##NaN` corner skipped)."
  [{:keys [nx ny ^doubles values]} ^double level]
  (let [nx (long nx) ny (long ny)]
    (vec (for [j (range (dec ny)) i (range (dec nx))
               :let [i (long i) j (long j)
                     v00 (Array/aget values (+ i (* j nx)))
                     v10 (Array/aget values (+ (inc i) (* j nx)))
                     v11 (Array/aget values (+ (inc i) (* (inc j) nx)))
                     v01 (Array/aget values (+ i (* (inc j) nx)))]
               :when (not (or (m/nan? v00) (m/nan? v10) (m/nan? v11) (m/nan? v01)))
               :let [case-index (+ (if (>= v00 level) 1 0) (if (>= v10 level) 2 0) (if (>= v11 level) 4 0) (if (>= v01 level) 8 0))
                     pairs (cell-segments case-index)
                     pairs (if (map? pairs)
                             (pairs (if (>= (* 0.25 (+ v00 v10 v11 v01)) level) :high :low))
                             pairs)]
               [a b] pairs]
           [(edge-id a i j) (edge-id b i j)]))))

(defn- chains
  "Segments joined at shared edges: `[{:edges [id ..] :closed? bool}]` - open chains first (from an edge used once),
  then closed loops."
  [segs]
  (let [by-edge (reduce (fn [acc [idx [a b]]] (-> acc (update a (fnil conj []) idx) (update b (fnil conj []) idx)))
                        {} (map-indexed vector segs))
        walk (fn [used start-edge start-seg]
               (loop [used used edge start-edge seg start-seg path [start-edge]]
                 (let [[a b] (segs seg)
                       next-edge (if (= a edge) b a)
                       used (conj used seg)
                       next-seg (first (remove used (by-edge next-edge)))]
                   (if next-seg
                     (recur used next-edge next-seg (conj path next-edge))
                     [used (conj path next-edge)]))))
        ends (keep (fn [[edge idxs]] (when (= 1 (count idxs)) [edge (first idxs)])) by-edge)
        [used open] (reduce (fn [[used acc] [edge seg]]
                              (if (used seg) [used acc] (let [[used path] (walk used edge seg)] [used (conj acc {:edges path :closed? false})])))
                            [#{} []] ends)
        [_ loops] (reduce (fn [[used acc] seg]
                            (if (used seg) [used acc] (let [[used path] (walk used (first (segs seg)) seg)]
                                                        [used (conj acc {:edges (pop path) :closed? true})])))
                          [used []] (range (count segs)))]
    (into open loops)))

(defn isolines
  "Lines of `grid` at `level`: `[{:points [[x y] ..] :closed? bool}]` (see the namespace doc); `[]` when the field
  doesn't cross it."
  {:cljplot2/api true}
  [grid level]
  (let [level (double level)]
    (mapv (fn [{:keys [edges closed?]}]
            {:points (mapv #(apply crossing level (edge-ends grid %)) edges) :closed? closed?})
          (chains (segments grid level)))))

(defn padded
  "`grid` with a border of `-Inf` values one step outside it, `##NaN` values as `-Inf` (see [[level-rings]])."
  [{:keys [^doubles xs ^doubles ys nx ny ^doubles values]}]
  (let [nx (long nx) ny (long ny)
        px (+ nx 2) py (+ ny 2)
        extend (fn ^doubles [^doubles a ^long n]
                 (let [step (if (> n 1) (- (Array/aget a 1) (Array/aget a 0)) 1.0)
                       out (double-array (+ n 2))]
                   (Array/aset out 0 (- (Array/aget a 0) step))
                   (dotimes [i n] (Array/aset out (inc i) (Array/aget a i)))
                   (Array/aset out (inc n) (+ (Array/aget a (dec n)) step))
                   out))
        out (double-array (* px py) ##-Inf)]
    (dotimes [j ny]
      (dotimes [i nx]
        (let [v (Array/aget values (+ i (* j nx)))]
          (Array/aset out (+ (inc i) (* (inc j) px)) (if (m/nan? v) ##-Inf v)))))
    {:xs (extend xs nx) :ys (extend ys ny) :nx px :ny py :values out}))

(defn level-rings
  "Closed rings `[[[x y] ..] ..]` outlining the region of `grid` at or above `level` (see the namespace doc); `[]`
  for an empty region."
  [grid level]
  (mapv :points (isolines (padded grid) level)))
