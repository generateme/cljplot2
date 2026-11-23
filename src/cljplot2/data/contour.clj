(ns cljplot2.data.contour
  "Contours of a sampled field by marching squares.

  The field is a grid of [[cljplot2.data.field/sample]]: `{:xs :ys :nx :ny :values}`, primitive arrays of the grid coordinates and the row-major values.

  [[isolines]] - the lines where the field crosses a level: each grid cell (4 corner values, none `##NaN`) gives 0,
  1 or 2 segments between points on its edges (linear interpolation between the corner values); a saddle (diagonal
  corners on the same side) is resolved by the cell's average value. Segments sharing an edge point are joined into
  polylines, closed when they come back to their start. A value equal to the level counts as above it.

  [[level-rings]] - the outline of the region at or above a level, for filling: the grid padded with `-Inf` (and
  `##NaN` values taken as `-Inf`) so every line closes; on the domain edge the outline runs along the grid. Filled with
  the even-odd rule the rings give the region, holes included.

  [[levels]] - contour levels evenly spread over a range; [[padded]] - the grid with the `-Inf` border of [[level-rings]]."
  (:require [fastmath.core :as m])
  (:import [fastmath.java Array]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn levels
  "Gives the contour levels for a value range.

  Parameters:

  - `[lo hi]`: the value range; `lo` `nil` - no range.
  - `n`: an integer - that many levels evenly spread inside the range (its ends left out); a sequence - the levels themselves.

  Returns a vector of levels, increasing (given levels sorted). `[]` for no range or a zero-width one with an integer `n`.

  See also [[isolines]], [[level-rings]]."
  [[lo hi] n]
  (cond
    (sequential? n) (vec (sort n))
    (or (nil? lo) (m/== (double lo) (double hi))) []
    :else (let [low (double lo)
                n+ (m/long-inc (long n))
                step (m// (m/- (double hi) low) n+)]
            (mapv (fn [^long i] (m/+ low (m/* step i))) (range 1 n+)))))

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
  "Gives the id of an edge of a grid cell, shared by the neighbouring cell.

  Parameters:

  - `side` (keyword): `:b` bottom, `:t` top, `:l` left, `:r` right.
  - `i`, `j` (longs): the cell's column and row.

  Returns `[:h i j]` - the horizontal edge from grid point `(i, j)` to `(i+1, j)`, or `[:v i j]` - the vertical edge from `(i, j)` to `(i, j+1)`."
  [side ^long i ^long j]
  (case side
    :b [:h i j]
    :t [:h i (m/long-inc j)]
    :l [:v i j]
    :r [:v (m/long-inc i) j]))

(defn- crossing
  "Gives the point where a level crosses a grid edge.

  Parameters:

  - `level` (double): the contour level.
  - `a`, `b` (`[x y v]`): the edge's grid points with their values.

  Returns `[x y]`, linear between the values; the finite end when the other value is `-Inf`."
  [^double level [^double ax ^double ay ^double av] [^double bx ^double by ^double bv]]
  (cond
    (m/neg-inf? av) [bx by]
    (m/neg-inf? bv) [ax ay]
    :else (let [t (m// (m/- level av) (m/- bv av))]
            [(m/+ ax (m/* t (m/- bx ax)))
             (m/+ ay (m/* t (m/- by ay)))])))

(defn- edge-ends
  "Gives the two grid points of an edge.

  Parameters:

  - `grid` (map): the sampled field, `:xs :ys :nx :values`.
  - `id`: an edge id of [[edge-id]].

  Returns `[[x y v] [x y v]]` - the edge's ends with their values."
  [{:keys [^doubles xs ^doubles ys ^long nx ^doubles values]} [kind ^long i ^long j]]
  (let [point (fn [^long a ^long b] [(Array/aget xs a)
                                    (Array/aget ys b)
                                    (Array/aget values (m/+ a (m/* b nx)))])]
    (if (= kind :h)
      [(point i j) (point (m/long-inc i) j)]
      [(point i j) (point i (m/long-inc j))])))

(defn- segments
  "Gives the marching-squares segments of a grid at a level.

  Parameters:

  - `grid` (map): the sampled field, `:nx :ny :values`.
  - `level` (double): the contour level; a value equal to it counts as above.

  Returns a vector of segments `[edge-id edge-id]` of every cell the level crosses; cells with a `##NaN` corner are skipped, saddles resolved by the cell's mean value."
  [{:keys [^long nx ^long ny ^doubles values]} ^double level]
  (vec (for [^long j (range (m/long-dec ny))
             ^long i (range (m/long-dec nx))
             :let [v00 (Array/aget values (m/+ i (m/* j nx)))
                   v10 (Array/aget values (m/+ (m/long-inc i) (m/* j nx)))
                   v11 (Array/aget values (m/+ (m/long-inc i) (m/* (m/long-inc j) nx)))
                   v01 (Array/aget values (m/+ i (m/* (m/long-inc j) nx)))]
             :when (not (or (m/nan? v00) (m/nan? v10) (m/nan? v11) (m/nan? v01)))
             :let [case-index (m/+ (if (m/>= v00 level) 1 0)
                                   (if (m/>= v10 level) 2 0)
                                   (if (m/>= v11 level) 4 0)
                                   (if (m/>= v01 level) 8 0))
                   case-pairs (cell-segments case-index)
                   pairs (if (map? case-pairs)
                           (case-pairs (if (m/>= (m/* 0.25 (m/+ v00 v10 v11 v01)) level) :high :low))
                           case-pairs)]
             [a b] pairs]
         [(edge-id a i j) (edge-id b i j)])))

(defn- chains
  "Gives the segments joined into chains at their shared edges.

  Parameters:

  - `segs` (vector): segments `[edge-id edge-id]` of [[segments]].

  Returns a vector of `{:edges [id ...] :closed? bool}`: open chains first (each from an edge used once), then closed loops (without the repeated first edge)."
  [segs]
  (let [by-edge (reduce (fn [acc [idx [a b]]] (-> acc
                                                 (update a (fnil conj []) idx)
                                                 (update b (fnil conj []) idx)))
                        {} (map-indexed vector segs))
        walk (fn [used-before start-edge start-seg]
               (loop [used used-before
                      edge start-edge
                      seg start-seg
                      path [start-edge]]
                 (let [[a b] (segs seg)
                       next-edge (if (= a edge) b a)
                       used-now (conj used seg)
                       next-seg (first (remove used-now (by-edge next-edge)))]
                   (if next-seg
                     (recur used-now next-edge next-seg (conj path next-edge))
                     [used-now (conj path next-edge)]))))
        ends (keep (fn [[edge idxs]] (when (m/one? (count idxs)) [edge (first idxs)])) by-edge)
        [used-open open] (reduce (fn [[used acc] [edge seg]]
                                   (if (used seg)
                                     [used acc]
                                     (let [[walked path] (walk used edge seg)] [walked (conj acc {:edges path :closed? false})])))
                                 [#{} []] ends)
        [_ loops] (reduce (fn [[used acc] seg]
                            (if (used seg)
                              [used acc]
                              (let [[walked path] (walk used (first (segs seg)) seg)]
                                [walked (conj acc {:edges (pop path) :closed? true})])))
                          [used-open []] (range (count segs)))]
    (into open loops)))

(defn isolines
  "Gives the contour lines of a sampled field at a level (see the namespace doc).

  Parameters:

  - `grid` (map): a sampled field of [[cljplot2.data.field/sample]].
  - `level` (double): the contour level; a value equal to it counts as above.

  Returns a vector of `{:points [[x y] ...] :closed? bool}` - open lines first, then closed ones; `[]` when the field doesn't cross the level.

  See also [[level-rings]], [[levels]]."
  {:cljplot2/api true}
  [grid ^double level]
  (mapv (fn [{:keys [edges closed?]}]
          {:points (mapv #(apply crossing level (edge-ends grid %)) edges) :closed? closed?})
        (chains (segments grid level))))

(defn padded
  "Gives the sampled field with a border of `-Inf` values one step outside it, so every contour closes.

  Parameters:

  - `grid` (map): a sampled field `{:xs :ys :nx :ny :values}`; not mutated.

  Returns a new grid two points wider and higher: the border at one grid step (1 for a single point) outside the coordinates, `-Inf` there, `##NaN` values as `-Inf`.

  See also [[level-rings]]."
  [{:keys [^doubles xs ^doubles ys ^long nx ^long ny ^doubles values]}]
  (let [px (m/+ nx 2)
        py (m/+ ny 2)
        extended-axis (fn ^doubles [^doubles a ^long n]
                        (let [step (if (m/> n 1) (m/- (Array/aget a 1) (Array/aget a 0)) 1.0)
                              out (double-array (m/+ n 2))]
                          (Array/aset out 0 (m/- (Array/aget a 0) step))
                          (dotimes [i n] (Array/aset out (m/long-inc i) (Array/aget a i)))
                          (Array/aset out (m/long-inc n) (m/+ (Array/aget a (m/long-dec n)) step))
                          out))
        out (double-array (m/* px py) ##-Inf)]
    (dotimes [j ny]
      (dotimes [i nx]
        (let [v (Array/aget values (m/+ i (m/* j nx)))]
          (Array/aset out (m/+ (m/long-inc i) (m/* (m/long-inc j) px)) (if (m/nan? v) ##-Inf v)))))
    {:xs (extended-axis xs nx) :ys (extended-axis ys ny) :nx px :ny py :values out}))

(defn level-rings
  "Gives the closed rings outlining the region of a sampled field at or above a level, for filling (see the namespace doc).

  Parameters:

  - `grid` (map): a sampled field of [[cljplot2.data.field/sample]].
  - `level` (double): the level.

  Returns a vector of rings `[[x y] ...]`; filled with the even-odd rule they give the region, holes included. `[]` for an empty region.

  See also [[padded]], [[isolines]]."
  [grid level]
  (mapv :points (isolines (padded grid) level)))
