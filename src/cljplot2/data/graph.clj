(ns cljplot2.data.graph
  "Graphs from edge rows, and their layouts - node positions as data, drawn by ordinary charts (see
  [[cljplot2.graphs/network]]).

  A graph is `{:nodes [node ..] :edges [edge ..] :loops [edge ..] :directed? b :dropped n}`:

  * nodes - the `:nodes` rows (each with an `:id`), then the ids seen only in edges, in first-appearance order; every
    node is its row (or `{}`) with `:id`, `:degree`, `:in`, `:out` (undirected graphs: `:in` = `:out` = `:degree`)
  * edges - the edge rows with `:from`, `:to` and `:value` (the weight, default `1.0`) set; self-loops (`:from` =
    `:to`) go to `:loops` instead: counted in the degrees (twice, as igraph), not laid out or drawn
  * `:dropped` - edge rows with a `nil` end

  A layout adds `:x` / `:y` to the nodes and `:x :y :xend :yend` to the edges (and loops), in the unit square:
  `:force` (Fruchterman-Reingold, igraph `layout_with_fr`), `:circle`, `:random`. Random starts and random layouts
  draw from `:rng` (a fastmath random generator, copied per call - see [[cljplot2.data.rng/for-build]]): one
  generator gives the same layout every time; without it layouts differ.

  Ordered layouts: [[node-order]] (input, given or clustered order of the nodes), [[chord-layout]] (d3 `chord` /
  `chordDirected` - arcs and ribbons in the unit circle), [[arc-layout]] (nodes on a line, edges as semicircles)."
  (:require [fastmath.core :as m]
            [fastmath.ml.clustering :as cl]
            [fastmath.random :as r]
            [cljplot2.data.rng :as rng]
            [cljplot2.data.rows :as data]
            [cljplot2.data.tree :as tree]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def layouts #{:force :circle :random})

(defn- weight
  "Edge weight of the value `v`: `1.0` for `nil`, else a finite number >= 0 (`ex-info {:key :value}` otherwise)."
  ^double [v]
  (cond
    (nil? v) 1.0
    (and (number? v) (m/valid-double? (double v)) (>= (double v) 0.0)) (double v)
    :else (schema/fail! "Edge :value should be a finite number >= 0." {:key :value :value v})))

(defn- edge-rows!
  "`edges` checked: a sequence of maps (`ex-info {:key :edges}` otherwise)."
  [edges]
  (when-not (and (sequential? edges) (every? map? edges))
    (schema/fail! "Edges should be a sequence of maps (rows)." {:key :edges}))
  edges)

(defn- node-rows
  "`{id row}` and the ids in order of the `nodes` rows under accessor `id-of`; a repeated or `nil` id throws
  `ex-info {:key :nodes}`."
  [nodes id-of]
  (reduce (fn [[by-id order] row]
            (let [id (id-of row)]
              (when (or (nil? id) (contains? by-id id))
                (schema/fail! "Node ids should be unique and not nil." {:key :nodes :value id}))
              [(assoc by-id id row) (conj order id)]))
          [{} []] nodes))

(defn graph
  "The graph of `edges` (rows) - see the namespace doc. `opts`: `:from`, `:to`, `:value` - accessors (keywords or
  functions; defaults `:from`, `:to`, `:value`), `:nodes` - node rows, `:id` - their id accessor (default `:id`),
  `:directed?` (default `false`). Throws `ex-info` for edges that aren't rows (`:edges`), an invalid weight
  (`:value`), repeated or `nil` node ids (`:nodes`)."
  ([edges] (graph edges {}))
  ([edges {:keys [from to value nodes id directed?] :or {from :from to :to value :value id :id}}]
   (let [from-of (data/row-accessor :from from)
         to-of (data/row-accessor :to to)
         value-of (data/row-accessor :value value)
         [by-id order] (node-rows nodes (data/row-accessor :id id))
         all (mapv (fn [row] (assoc row :from (from-of row) :to (to-of row) :value (weight (value-of row))))
                   (edge-rows! edges))
         kept (filterv #(and (some? (:from %)) (some? (:to %))) all)
         {loops true edges false} (group-by #(= (:from %) (:to %)) kept)
         ids (into order (comp (mapcat (juxt :from :to)) (remove by-id) (distinct)) kept)
         outs (frequencies (map :from kept))
         ins (frequencies (map :to kept))]
     {:nodes (mapv (fn [k] (let [o (long (get outs k 0)) i (long (get ins k 0))]
                             (assoc (get by-id k {}) :id k :degree (+ o i)
                                    :in (if directed? i (+ o i)) :out (if directed? o (+ o i)))))
                   ids)
      :edges (vec edges)
      :loops (vec loops)
      :directed? (boolean directed?)
      :dropped (- (count all) (count kept))})))

(defn adjacency->edges
  "Edge rows `{:from :to :value}` of the square `matrix` (rows of numbers, `names` - row / column names in order): a
  zero, `nil` or non-finite cell is no edge; with `:directed? false` (default `true`) only the upper triangle (and
  the diagonal) is read. Throws `ex-info {:key :matrix}` when the matrix isn't `n x n` for `n` names."
  ([matrix names] (adjacency->edges matrix names {}))
  ([matrix names {:keys [directed?] :or {directed? true}}]
   (let [n (count names)
         rows (vec matrix)]
     (when-not (and (= n (count rows)) (every? #(= n (count %)) rows))
       (schema/fail! "Adjacency matrix should be n x n for n names." {:key :matrix :value [(count rows) n]}))
     (vec (for [i (range n) j (range n)
                :when (or directed? (<= (long i) (long j)))
                :let [v (nth (rows i) j)]
                :when (and (number? v) (m/valid-double? (double v)) (not (zero? (double v))))]
            {:from (names i) :to (names j) :value v})))))

;; layouts - positions in double arrays, node index order

(defn- circle-positions
  "`[xs ys]` of `n` nodes on the circle of the unit square, from the top clockwise; one node in the centre."
  [^long n]
  (let [xs (double-array n) ys (double-array n)]
    (dotimes [i n]
      (let [a (/ (* m/TWO_PI i) (double n))]
        (aset xs i (if (== n 1) 0.5 (+ 0.5 (* 0.5 (m/sin a)))))
        (aset ys i (if (== n 1) 0.5 (+ 0.5 (* 0.5 (m/cos a)))))))
    [xs ys]))

(defn- random-positions
  "`[xs ys]` of `n` nodes uniform in the unit square from generator `g`."
  [g ^long n]
  (let [xs (double-array n) ys (double-array n)]
    (dotimes [i n]
      (aset xs i (r/drandom g))
      (aset ys i (r/drandom g)))
    [xs ys]))

(defn- repel!
  "Add the Fruchterman-Reingold repulsion `k2 / d` between every pair of nodes at `xs` / `ys` to `dx` / `dy`
  (coincident nodes pushed apart along a fixed direction by their indices)."
  [^doubles xs ^doubles ys [^doubles dx ^doubles dy] ^double k2]
  (let [n (alength xs)]
    (dotimes [i n]
      (loop [j (inc i)]
        (when (< j n)
          (let [ex (- (aget xs i) (aget xs j))
                ey (- (aget ys i) (aget ys j))
                d0 (m/hypot ex ey)
                close? (< d0 1.0e-9)
                a (* 2.399963 (double (+ i j)))
                d (if close? 1.0e-9 d0)
                f (/ k2 d)
                fx (* f (if close? (m/cos a) (/ ex d0)))
                fy (* f (if close? (m/sin a) (/ ey d0)))]
            (aset dx i (+ (aget dx i) fx)) (aset dy i (+ (aget dy i) fy))
            (aset dx j (- (aget dx j) fx)) (aset dy j (- (aget dy j) fy))
            (recur (inc j))))))))

(defn- attract!
  "Add the attraction `w d^2 / k` along every edge (`ends` - `[i j w]`, `w` the weight over the mean weight) to `dx` /
  `dy`."
  [^doubles xs ^doubles ys [^doubles dx ^doubles dy] [ends ^double k]]
  (doseq [[i j w] ends]
    (let [i (long i) j (long j)
          ex (- (aget xs i) (aget xs j))
          ey (- (aget ys i) (aget ys j))
          d (m/hypot ex ey)
          f (/ (* (double w) d) k)
          fx (* ex f) fy (* ey f)]
      (aset dx i (- (aget dx i) fx)) (aset dy i (- (aget dy i) fy))
      (aset dx j (+ (aget dx j) fx)) (aset dy j (+ (aget dy j) fy)))))

(defn- gravitate!
  "Add a pull `g * d` towards the nodes' centroid to `dx` / `dy` (keeps loosely connected nodes and components near
  the rest instead of drifting away; the pulls sum to zero, so a symmetric layout stays undistorted)."
  [^doubles xs ^doubles ys [^doubles dx ^doubles dy] ^double g]
  (let [n (alength xs)
        cx (/ (double (areduce xs i acc 0.0 (+ acc (aget xs i)))) (double (max 1 n)))
        cy (/ (double (areduce ys i acc 0.0 (+ acc (aget ys i)))) (double (max 1 n)))]
    (dotimes [i n]
      (aset dx i (- (aget dx i) (* g (- (aget xs i) cx))))
      (aset dy i (- (aget dy i) (* g (- (aget ys i) cy)))))))

(defn- move!
  "Move every node by its displacement `dx` / `dy`, limited to `t`."
  [^doubles xs ^doubles ys [^doubles dx ^doubles dy] ^double t]
  (dotimes [i (alength xs)]
    (let [ex (aget dx i) ey (aget dy i)
          d (m/hypot ex ey)
          s (if (> d t) (/ t d) 1.0)]
      (aset xs i (+ (aget xs i) (* ex s)))
      (aset ys i (+ (aget ys i) (* ey s))))))

(defn- force-positions
  "`[xs ys]` of the Fruchterman-Reingold layout of `n` nodes with weighted edge `ends` (`[i j w]`) after
  `iterations` steps from random positions of generator `g`: ideal distance `k = sqrt(1 / n)`, temperature from 0.1
  falling linearly to 0, a pull to the centroid (`gravity * k` per unit distance) instead of a frame - the result is
  scaled into the unit square afterwards."
  [g ^long n ends [^long iterations ^double gravity]]
  (let [[^doubles xs ^doubles ys] (random-positions g n)
        k (m/sqrt (/ 1.0 (double (max 1 n))))
        mean-w (if (seq ends) (/ (double (reduce + (map #(double (nth % 2)) ends))) (count ends)) 1.0)
        ends (if (pos? mean-w) (mapv (fn [[i j w]] [i j (/ (double w) mean-w)]) ends) ends)]
    (dotimes [step iterations]
      (let [disp [(double-array n) (double-array n)]]
        (repel! xs ys disp (* k k))
        (attract! xs ys disp [ends k])
        (gravitate! xs ys disp (* gravity k))
        (move! xs ys disp (* 0.1 (- 1.0 (/ (double step) (double iterations)))))))
    [xs ys]))

(defn- fitted
  "`[xs ys]` scaled uniformly into the unit square and centred (a single position or no spread - the centre)."
  [[^doubles xs ^doubles ys]]
  (let [n (alength xs)]
    (if (zero? n)
      [xs ys]
      (let [x0 (reduce min (seq xs)) x1 (reduce max (seq xs))
            y0 (reduce min (seq ys)) y1 (reduce max (seq ys))
            span (max (- (double x1) (double x0)) (- (double y1) (double y0)))
            s (if (pos? span) (/ 1.0 span) 0.0)
            cx (* 0.5 (+ (double x0) (double x1))) cy (* 0.5 (+ (double y0) (double y1)))]
        [(amap xs i _ (+ 0.5 (* s (- (aget xs i) cx))))
         (amap ys i _ (+ 0.5 (* s (- (aget ys i) cy))))]))))

(defn layout
  "Graph `g` (see [[graph]]) with positions (see the namespace doc). `opts`: `:layout` - `:force` (default),
  `:circle`, `:random`; force: `:iterations` (default 500), `:gravity` - the pull to the centroid in units of the
  ideal edge length (default 1; 0 - none: loose parts drift apart; more - a tighter layout); `:rng`. Force layouts
  are scaled into the unit square keeping their proportions. Throws `ex-info` for an unknown layout (`:layout`), a
  `:gravity` that isn't a finite number >= 0 (`:gravity`)."
  ([g] (layout g {}))
  ([{:keys [nodes edges loops] :as g} {:keys [iterations gravity rng] kind :layout :or {kind :force iterations 500 gravity 1.0}}]
   (when-not (layouts kind)
     (schema/fail! (str "Unknown layout, known: " (sort layouts) ".") {:key :layout :value kind}))
   (when-not (and (number? gravity) (m/valid-double? (double gravity)) (>= (double gravity) 0.0))
     (schema/fail! "Layout :gravity should be a finite number >= 0." {:key :gravity :value gravity}))
   (let [n (count nodes)
         index (zipmap (map :id nodes) (range))
         [^doubles xs ^doubles ys] (case kind
                                     :circle (circle-positions n)
                                     :random (random-positions (rng/for-build rng) n)
                                     :force (fitted (force-positions (rng/for-build rng) n
                                                                     (mapv (fn [e] [(index (:from e)) (index (:to e)) (:value e)]) edges)
                                                                     [(long iterations) (double gravity)])))
         placed (fn [e] (let [i (long (index (:from e))) j (long (index (:to e)))]
                          (assoc e :x (aget xs i) :y (aget ys i) :xend (aget xs j) :yend (aget ys j))))]
     (assoc g
            :nodes (vec (map-indexed (fn [i node] (assoc node :x (aget xs (long i)) :y (aget ys (long i)))) nodes))
            :edges (mapv placed edges)
            :loops (mapv placed loops)))))

;; ordered layouts

(defn- weight-matrix
  "Weights between the nodes of `ids` (`n x n` vectors of doubles; row - the source): the summed `:value` of the edges
  from row to column; an undirected graph's edge counts both ways. Loops aren't edges (see [[graph]])."
  [{:keys [edges directed?]} ids]
  (let [n (count ids)
        index (zipmap ids (range))
        add (fn [w i j v] (update-in w [i j] + (double v)))]
    (reduce (fn [w {:keys [from to value]}]
              (let [i (index from) j (index to)]
                (cond-> (add w i j value) (not directed?) (add j i value))))
            (vec (repeat n (vec (repeat n 0.0))))
            edges)))

(defn- weight-at
  "Weight from node index `i` to `j` in the weights `w`."
  ^double [w i j]
  (double ((w i) j)))

(defn- unit-row
  "`row` (numbers) scaled to length 1; an all-zero row as it is."
  [row]
  (let [len (m/sqrt (reduce + 0.0 (map #(* (double %) (double %)) row)))]
    (if (pos? len) (mapv #(/ (double %) len) row) row)))

(defn- cluster-order
  "Node ids of `g` in the leaf order of fastmath `hclust` (average linkage, Euclidean distance) on the rows of
  W + W^T (W - [[weight-matrix]]) scaled to unit length - the cosine distance: nodes linked to the same others side
  by side, whatever their weights' size (a hub stays among its neighbours); fewer than 3 nodes - input order."
  [{:keys [nodes] :as g}]
  (let [ids (mapv :id nodes)
        n (count ids)]
    (if (< n 3)
      ids
      (let [w (weight-matrix g ids)
            rows (mapv (fn [i] (unit-row (mapv (fn [j] (+ (weight-at w i j) (weight-at w j i))) (range n)))) (range n))]
        (mapv ids (:order (:tree (cl/hclust rows {:method :average :distance :euclidean}))))))))

(defn node-order
  "Ids of the nodes of graph `g` (see [[graph]]) in `order`: `nil` - input order; a sequence of ids - those first (a
  repeated id once), the rest after them in input order; `:cluster` - [[cluster-order]]. Throws `ex-info {:key :order}`
  for an id not in the graph or another `order`."
  [{:keys [nodes] :as g} order]
  (let [ids (mapv :id nodes)]
    (cond
      (nil? order) ids
      (= :cluster order) (cluster-order g)
      (sequential? order) (let [known (set ids)
                                given (vec (distinct order))]
                            (when-let [unknown (seq (remove known given))]
                              (schema/fail! "Node order names ids that aren't in the graph." {:key :order :value (vec unknown)}))
                            (into given (remove (set given)) ids))
      :else (schema/fail! "Node order should be nil, a sequence of ids or :cluster." {:key :order :value order}))))

(def ^:private ^:const bezier-steps 24)

(defn- through-centre
  "Points of the quadratic Bezier curve from `[x0 y0]` to `[x1 y1]` with its control point in the centre, ends
  excluded."
  [[x0 y0] [x1 y1]]
  (mapv (fn [^long s] (let [t (/ (double s) bezier-steps) u (- 1.0 t)]
                        [(+ (* u u (double x0)) (* t t (double x1))) (+ (* u u (double y0)) (* t t (double y1)))]))
        (range 1 bezier-steps)))

(defn- chord-angles
  "d3-chord angles (radians, clockwise from the top) of the weights `w` with the nodes in `order` (indices):
  `[groups ends]` - `groups` `{i [start end sum]}`; `ends` `{[i j] {:source [a0 a1] :target [a0 a1]}}` (`directed?`
  - `chordDirected`: group sums over rows and columns, ribbon `i -> j` of width w(i, j) at both ends, a group's
  incoming ends (from the last node down) before its outgoing ones; else `chord`: group sums over rows, the ribbon
  of a pair from w(i, j) at `i` - the end earlier in `order` - to w(j, i) at `j`); `pad` - radians after every group."
  [w order directed? ^double pad]
  (let [n (count w)
        rank (zipmap order (range))
        sums (mapv (fn [i] (reduce + 0.0 (map #(+ (weight-at w i %) (if directed? (weight-at w % i) 0.0)) (range n)))) (range n))
        total (double (reduce + 0.0 sums))
        k (if (pos? total) (/ (max 0.0 (- m/TWO_PI (* pad n))) total) 0.0)
        dx (if (pos? k) pad (/ m/TWO_PI (double (max 1 n))))
        subgroups (fn [i] (if directed?
                            (concat (for [j (range (dec n) -1 -1) :when (pos? (weight-at w j i))] [:in j (weight-at w j i)])
                                    (for [j (range n) :when (pos? (weight-at w i j))] [:out j (weight-at w i j)]))
                            (for [j (range n) :when (or (pos? (weight-at w i j)) (pos? (weight-at w j i)))]
                              [(if (< (long (rank i)) (long (rank j))) :out :in) j (weight-at w i j)])))]
    (loop [[i & more :as left] (seq order) x 0.0 groups {} ends {}]
      (if-not left
        [groups ends]
        (let [[x' ends] (reduce (fn [[^double x ends] [dir j v]]
                                  (let [x1 (+ x (* k (double v)))]
                                    [x1 (if (= :out dir)
                                          (assoc-in ends [[i j] :source] [x x1])
                                          (assoc-in ends [[j i] :target] [x x1]))]))
                                [x ends] (subgroups i))]
          (recur more (+ (double x') dx) (assoc groups i [x x' (sums i)]) ends))))))

(defn- arc-band
  "Points of the ring sector between radii `inner` and 1 from angle `a0` to `a1` (radians): the outer arc, then the
  inner one back."
  [^double inner ^double a0 ^double a1]
  (into (tree/arc-points 1.0 a0 a1) (rseq (tree/arc-points inner a0 a1))))

(defn- ribbon-points
  "Points of a ribbon at radius `r` from the source arc `[s0 s1]` to the target arc `[t0 t1]` (radians): the source
  arc, a curve through the centre, the target arc, a curve back (d3 `ribbon`)."
  [^double r [s0 s1] [t0 t1]]
  (let [source (tree/arc-points r s0 s1)
        target (tree/arc-points r t0 t1)]
    (-> source
        (into (through-centre (peek source) (first target)))
        (into target)
        (into (through-centre (peek target) (first source))))))

(defn chord-layout
  "Chord diagram of graph `g` (see [[graph]]) as data in the unit circle (d3 `chord` for an undirected graph,
  `chordDirected` for a directed one - see [[chord-angles]]): a ring sector per node, its angle proportional to the
  node's flow (undirected: its edges' weights; directed: out plus in), clockwise from the top in node order with
  `:pad` degrees after each (default 2); a ribbon per linked pair (undirected) or edge (directed) joining its ends'
  arcs through the centre (an undirected ribbon's `:from` - its end earlier in the ring). `opts`: `:order` (see
  [[node-order]]), `:pad`, `:inner` - the ring's inner radius (default 0.92; ribbons end there), `:min-value` - a
  summed weight below it is left out: of an ordered pair (directed; repeated edges summed) or of a pair both ways
  (undirected - the pair's exchange); its edges are counted in `:dropped`.
  Returns `{:groups [{:id :start :end :value}] :arcs [{:group :x :y}] :ribbons [{:ribbon :from :to :value :x :y}]
  :dropped n}` (angles in degrees; `:value` of a ribbon - its source end's weight; polygon rows in point order).
  Throws `ex-info` for a `:pad` that isn't a finite number >= 0 or leaves no room (`:pad`), an `:inner` outside
  (0, 1) (`:inner`), a `:min-value` that isn't a finite number >= 0 (`:min-value`)."
  ([g] (chord-layout g {}))
  ([{:keys [edges directed?] :as g} {:keys [order pad inner min-value] :or {pad 2.0 inner 0.92}}]
   (when-not (and (number? pad) (m/valid-double? (double pad)) (<= 0.0 (double pad)) (< (* (double pad) (count (:nodes g))) 360.0))
     (schema/fail! "Chord :pad should be a finite number of degrees >= 0, leaving room for the arcs." {:key :pad :value pad}))
   (when-not (and (number? inner) (< 0.0 (double inner) 1.0))
     (schema/fail! "Chord :inner should be a radius in (0, 1)." {:key :inner :value inner}))
   (when-not (or (nil? min-value) (and (number? min-value) (m/valid-double? (double min-value)) (<= 0.0 (double min-value))))
     (schema/fail! "Chord :min-value should be a finite number >= 0." {:key :min-value :value min-value}))
   (let [ids (mapv :id (:nodes g))
         index (zipmap ids (range))
         summed (weight-matrix g ids)
         weak? (fn [i j] (and min-value (< (weight-at summed i j) (double min-value))))
         w (mapv (fn [i] (mapv (fn [j] (if (weak? i j) 0.0 (weight-at summed i j))) (range (count ids)))) (range (count ids)))
         [groups ends] (chord-angles w (mapv index (node-order g order)) directed? (m/radians (double pad)))
         inner (double inner)]
     {:groups (vec (for [i (range (count ids)) :let [[a0 a1 v] (groups i)]]
                     {:id (ids i) :start (m/degrees (double a0)) :end (m/degrees (double a1)) :value v}))
      :arcs (vec (for [i (range (count ids)) :let [[a0 a1] (groups i)] :when (> (double a1) (double a0))
                       [x y] (arc-band inner a0 a1)]
                   {:group (ids i) :x x :y y}))
      :ribbons (vec (for [[[i j] {:keys [source target]}] (sort-by key ends)
                          :let [k (str (ids i) " -> " (ids j))]
                          [x y] (ribbon-points inner source target)]
                      {:ribbon k :from (ids i) :to (ids j) :value ((w i) j) :x x :y y}))
      :dropped (count (filter #(weak? (index (:from %)) (index (:to %))) edges))})))

(defn arc-layout
  "Arc diagram of graph `g` (see [[graph]]) as data: nodes on the x axis at 0, 1, ... in node order (`opts` `:order`,
  see [[node-order]]), y = 0; every edge a semicircle above the axis between its ends (peak height half their
  distance), sampled every ~2 degrees. Returns `{:nodes [node + :x :y] :arcs [{:edge :from :to :value :x :y}]}`
  (`:edge` - the edge's index; arcs in edge order)."
  ([g] (arc-layout g {}))
  ([{:keys [nodes edges] :as g} {:keys [order]}]
   (let [ids (node-order g order)
         x-of (zipmap ids (range))
         by-id (zipmap (map :id nodes) nodes)]
     {:nodes (mapv (fn [id] (assoc (by-id id) :x (double (x-of id)) :y 0.0)) ids)
      :arcs (vec (for [[k {:keys [from to value]}] (map-indexed vector edges)
                       :let [x0 (double (x-of from)) x1 (double (x-of to))
                             c (* 0.5 (+ x0 x1)) r (* 0.5 (m/abs (- x1 x0)))
                             ;; from the left end over the top to the right end
                             [a0 a1] (if (<= x0 x1) [(- m/HALF_PI) m/HALF_PI] [m/HALF_PI (- m/HALF_PI)])]
                       [x y] (tree/arc-points r a0 a1)]
                   {:edge k :from from :to to :value value :x (+ c (double x)) :y y}))})))
