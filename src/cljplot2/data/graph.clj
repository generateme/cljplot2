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
  `chordDirected` - arcs and ribbons in the unit circle), [[arc-layout]] (nodes on a line, edges as semicircles).

  Flows: [[sankey-layout]] (d3-sankey - nodes in columns, bands between them in the unit square), [[alluvial-graph]]
  (the graph of an alluvial diagram: a node per level of every axis, strands between consecutive axes)."
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
         all (mapv (fn [row] (assoc row :from (from-of row) :to (to-of row) :value (data/weight (value-of row))))
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

(defn- components
  "Connected components of nodes `0..n-1` linked by `ends` (`[i j w]`): vectors of node indices in increasing order,
  the components ordered by their smallest index."
  [^long n ends]
  (let [neighbours (reduce (fn [acc [i j]] (-> acc (update i conj j) (update j conj i))) (vec (repeat n [])) ends)]
    (loop [i 0 seen #{} out []]
      (cond
        (= i n) out
        (seen i) (recur (inc i) seen out)
        :else (let [part (loop [queue [i] part #{i}]
                           (if-let [k (first queue)]
                             (let [new (remove part (neighbours k))]
                               (recur (into (subvec queue 1) new) (into part new)))
                             part))]
                (recur (inc i) (into seen part) (conj out (vec (sort part)))))))))

(def ^:private ^:const component-gap
  "Gap between packed components, in units of a one-node component's side: wider than an edge inside a fitted
  component (about 1 - 1.7), so separate parts don't read as linked."
  2.0)

(defn- component-side
  "Side of the square a component of the nodes `part` is fitted into: the square root of its node count."
  ^double [part]
  (m/sqrt (double (count part))))

(defn- packed-force-positions
  "`[xs ys]` of the force layout (see [[force-positions]]) of every connected component of `n` nodes with `ends` on
  its own, each fitted into a square of side sqrt(its node count), the squares packed in rows - largest first, a row
  at most as wide as the square root of their total area - with [[component-gap]] between them (as igraph lays
  out and merges components); one component - its layout as it is. Components draw their random starts from `g`
  in packing order."
  [g ^long n ends opts]
  (let [parts (components n ends)]
    (if (<= (count parts) 1)
      (force-positions g n ends opts)
      (let [parts (sort-by (comp - count) parts)
            row-width (max (component-side (first parts))
                           (m/sqrt (double (reduce + (map #(m/sq (+ (component-side %) component-gap)) parts)))))
            xs (double-array n) ys (double-array n)]
        (loop [[part & more] parts x 0.0 y 0.0 row-height 0.0]
          (when part
            (let [s (component-side part)
                  [x y row-height] (if (and (pos? x) (> (+ x s) row-width))
                                     ;; the next row below: y decreasing
                                     [0.0 (- (double y) (double row-height) component-gap) 0.0]
                                     [x y row-height])
                  index (zipmap part (range))
                  local (filterv (fn [[i j]] (and (index i) (index j))) ends)
                  [^doubles px ^doubles py] (fitted (force-positions g (count part) (mapv (fn [[i j w]] [(index i) (index j) w]) local) opts))]
              (doseq [[k i] (map-indexed vector part)]
                (aset xs (long i) (+ (double x) (* s (aget px (long k)))))
                (aset ys (long i) (- (double y) (* s (aget py (long k))))))
              (recur more (+ (double x) s component-gap) (double y) (max (double row-height) s)))))
        [xs ys]))))

(defn layout
  "Graph `g` (see [[graph]]) with positions (see the namespace doc). `opts`: `:layout` - `:force` (default),
  `:circle`, `:random`; force: `:iterations` (default 500), `:gravity` - the pull to the centroid in units of the
  ideal edge length, inside each connected component (default 1; 0 - none; more - a tighter layout); `:rng`. A force layout places every connected
  component on its own and packs them in rows, largest first (see [[packed-force-positions]]); it is scaled into
  the unit square keeping its proportions. Throws `ex-info` for an unknown layout (`:layout`), a
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
                                     :force (fitted (packed-force-positions (rng/for-build rng) n
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

;; sankey (d3-sankey port; vertical positions computed top-down as d3, returned y-up)

(def sankey-aligns #{:justify :left :right :center})

(defn- sankey-options!
  "Sankey `opts` checked: `:align`, `:node-width` in (0, 1), `:pad` a finite number >= 0, `:iterations` an integer
  >= 0, `:node-sort` `nil` / `:input`; `ex-info` with the option's key otherwise."
  [{:keys [align node-width pad iterations node-sort]}]
  (let [fail (fn [k v msg] (schema/fail! msg {:key k :value v}))
        finite? #(and (number? %) (m/valid-double? (double %)))]
    (when-not (sankey-aligns align) (fail :align align (str "Sankey :align should be one of " (sort sankey-aligns) ".")))
    (when-not (and (finite? node-width) (< 0.0 (double node-width) 1.0)) (fail :node-width node-width "Sankey :node-width should be in (0, 1)."))
    (when-not (and (finite? pad) (<= 0.0 (double pad))) (fail :pad pad "Sankey :pad should be a finite number >= 0."))
    (when-not (and (integer? iterations) (<= 0 (long iterations))) (fail :iterations iterations "Sankey :iterations should be an integer >= 0."))
    (when-not (contains? #{nil :input} node-sort) (fail :node-sort node-sort "Sankey :node-sort should be nil or :input."))))

(defn- longest-paths
  "Longest path length (in links) of every node index `0..n-1` from the start of `next-of` (node index -> next node
  indices): from the sources for outgoing links (depth), from the sinks for incoming ones (height). A cycle throws
  `ex-info {:key :edges}`."
  [^long n next-of]
  (let [result (long-array n)]
    (loop [current (set (range n)) x 0]
      (when (seq current)
        (when (> x n)
          (schema/fail! "A sankey needs a graph without cycles." {:key :edges}))
        (doseq [i current] (aset result (long i) x))
        (recur (set (mapcat next-of current)) (inc x))))
    (vec result)))

(defn- sankey-layers
  "Column of every node index: the `layer-of` value of its node row (`:layer` given - non-negative integers, links
  forward only; else `ex-info {:key :layer}`), or by `align` from the depths and heights (see d3-sankey)."
  [nodes links [outs ins] [align layer-of]]
  (if layer-of
    (let [layers (mapv layer-of nodes)]
      (when-not (every? #(and (integer? %) (<= 0 (long %))) layers)
        (schema/fail! "Sankey :layer should give every node an integer >= 0." {:key :layer :value layers}))
      (when-let [back (first (filter (fn [{:keys [s t]}] (<= (long (layers t)) (long (layers s)))) links))]
        (schema/fail! "Sankey links should go to a later :layer." {:key :layer :value [(:id (nodes (:s back))) (:id (nodes (:t back)))]}))
      layers)
    (let [n (count nodes)
          depth (longest-paths n (fn [i] (map #(:t (links %)) (outs i))))
          height (longest-paths n (fn [i] (map #(:s (links %)) (ins i))))
          columns (inc (long (reduce max 0 depth)))
          last-column (dec columns)]
      (mapv (fn [i]
              (let [d (long (depth i))]
                (max 0 (min last-column
                            (long (case align
                                    :left d
                                    :right (- last-column (long (height i)))
                                    :justify (if (seq (outs i)) d last-column)
                                    :center (cond (seq (ins i)) d
                                                  (seq (outs i)) (dec (long (reduce min (map #(depth (:t (links %))) (outs i)))))
                                                  :else 0)))))))
            (range n)))))

(defn- link-order
  "`ids` (link indices) sorted by the top `y0s` of the node `end-of` each link leads to, ties by index (d3
  `ascendingSourceBreadth` / `ascendingTargetBreadth`)."
  [ids end-of ^doubles y0s]
  (vec (sort-by (fn [l] [(aget y0s (long (end-of l))) l]) ids)))

(defn- reorder-links!
  "Re-sort the outgoing (by target top) and incoming (by source top) links of the nodes `is`."
  [is [^objects outs ^objects ins] links y0s]
  (doseq [i is]
    (aset outs (long i) (link-order (aget outs (long i)) #(:t (links %)) y0s))
    (aset ins (long i) (link-order (aget ins (long i)) #(:s (links %)) y0s))))

(defn- level-node-top
  "Top of the node at the far end of link `l` that lets the link run level from its place at the `near` end
  (`:source` - its source node, as d3 `targetTop`; `:target` - its target node, as d3 `sourceTop`): the near node's
  links stacked from its top with gaps `pad` (centred), the far node's links stacked without."
  ^double [l near [^objects outs ^objects ins] [links ^doubles y0s ^doubles widths ^double pad]]
  (let [{:keys [s t]} (links l)
        [node own other] (if (= :source near) [s (aget outs (long s)) (aget ins (long t))] [t (aget ins (long t)) (aget outs (long s))])
        start (- (aget y0s (long node)) (/ (* (dec (count own)) pad) 2.0))
        before (fn [ls] (reduce + 0.0 (map #(aget widths (long %)) (take-while #(not= l %) ls))))]
    ;; the link's position in its own node's stack (with gaps), less its position at the other end (without)
    (- (+ start (double (before own)) (* pad (count (take-while #(not= l %) own)))) (double (before other)))))

(defn- shift!
  "Move node `i` down by `dy`."
  [^doubles y0s ^doubles y1s ^long i ^double dy]
  (aset y0s i (+ (aget y0s i) dy))
  (aset y1s i (+ (aget y1s i) dy)))

(defn- resolve-collisions!
  "Push the nodes of `column` (indices, top to bottom) apart to gaps of at least `pad` inside [0, 1], by the fraction
  `alpha` per step (d3 `resolveCollisions`)."
  [column [^doubles y0s ^doubles y1s] ^double pad ^double alpha]
  (let [k (count column)
        down! (fn [^double y from] (loop [y y i (long from)]
                                     (when (< i k)
                                       (let [node (long (column i))
                                             dy (* (- y (aget y0s node)) alpha)]
                                         (when (> dy 1.0e-6) (shift! y0s y1s node dy))
                                         (recur (+ (aget y1s node) pad) (inc i))))))
        up! (fn [^double y from] (loop [y y i (long from)]
                                   (when (>= i 0)
                                     (let [node (long (column i))
                                           dy (* (- (aget y1s node) y) alpha)]
                                       (when (> dy 1.0e-6) (shift! y0s y1s node (- dy)))
                                       (recur (- (aget y0s node) pad) (dec i))))))
        middle (long (quot k 2))]
    ;; a fixed :layer can leave a column empty
    (when (pos? k)
      (let [subject (long (column middle))]
        (up! (- (aget y0s subject) pad) (dec middle))
        (down! (+ (aget y1s subject) pad) (inc middle)))
      (up! 1.0 (dec k))
      (down! 0.0 0))))

(defn- relax!
  "One relaxation sweep over `columns` (a vector of node index vectors, mutated through `column-cells`): every node
  of the swept columns moves by `alpha` towards the weighted mean position its links ask for (`:right-to-left` -
  from its targets, else from its sources; weights - value times layer distance), then each column is re-sorted
  (unless `fixed-order?`) and its collisions resolved by `beta` (d3 `relaxRightToLeft` / `relaxLeftToRight`)."
  [^objects column-cells direction state [alpha beta fixed-order?]]
  (let [{:keys [links layers y0s y1s pad link-state]} state
        ^doubles y0s y0s ^doubles y1s y1s
        [^objects outs ^objects ins] link-state
        n (alength column-cells)
        order (if (= :right-to-left direction) (range (- n 2) -1 -1) (range 1 n))]
    (doseq [c order]
      (let [column (aget column-cells (long c))]
        (doseq [node column]
          (let [node (long node)
                [ls near] (if (= :right-to-left direction) [(aget outs node) :target] [(aget ins node) :source])
                [y w] (reduce (fn [[^double y ^double w] l]
                                (let [{:keys [s t value]} (links l)
                                      v (* (double value) (double (- (long (layers t)) (long (layers s)))))]
                                  [(+ y (* v (level-node-top l near link-state [links y0s (:widths state) pad]))) (+ w v)]))
                              [0.0 0.0] ls)]
            (when (pos? (double w))
              (shift! y0s y1s node (* (- (/ (double y) (double w)) (aget y0s node)) (double alpha)))
              (reorder-links! (concat [node] (map #(:s (links %)) (aget ins node)) (map #(:t (links %)) (aget outs node)))
                              link-state links y0s))))
        (let [column (if fixed-order? column (vec (sort-by #(aget y0s (long %)) column)))]
          (aset column-cells (long c) column)
          (resolve-collisions! column [y0s y1s] pad beta))))))

(def ^:private ^:const band-steps 24)

(defn- band-points
  "Points of a sankey band from `[x0 top0]` to `[x1 top1]`, `width` thick (y-up: the bottom edge `width` below): the
  top cubic Bezier (control points at the middle x), then the bottom one back."
  [[^double x0 ^double top0] [^double x1 ^double top1] ^double width]
  (let [xm (* 0.5 (+ x0 x1))
        curve (fn [^double a ^double b]
                (mapv (fn [^long s] (let [t (/ (double s) band-steps) u (- 1.0 t)]
                                      [(+ (* u u u x0) (* 3.0 u u t xm) (* 3.0 u t t xm) (* t t t x1))
                                       (+ (* u u u a) (* 3.0 u u t a) (* 3.0 u t t b) (* t t t b))]))
                      (range (inc band-steps))))]
    (into (curve top0 top1) (rseq (curve (- top0 width) (- top1 width))))))

(defn sankey-layout
  "Sankey diagram of graph `g` (see [[graph]]; edges are directed flows whatever `:directed?`) as data in the unit
  square (a d3-sankey port): nodes in columns (left to right) as rectangles `:node-width` wide, their heights
  proportional to their value (the larger of the flows in and out, one scale for all), at least `:pad` apart in a
  column; every edge a band from its source's right side to its target's left side, as thick as its value on that
  scale, stacked at both nodes in the order of the nodes at their other ends.

  `opts`: `:align` - the column of a node: `:justify` (default; the longest path from a source, sinks in the last
  column), `:left` (sinks where their path ends), `:right` (by the longest path to a sink), `:center`; `:layer` - an
  accessor of the node rows giving the column (integers >= 0, every edge to a later one; wins over `:align`);
  `:node-width` (default 0.03), `:pad` (default 0.02), `:iterations` - relaxation sweeps moving nodes towards their
  links' ends (default 6; 0 - spread in the start order); `:node-sort` - `nil` (relaxation reorders a column) or
  `:input` (input order kept).

  Returns `{:nodes [node + :layer :x0 :x1 :y0 :y1 :value] :links [edge + :link :sy :ty :width] :rects [{:rect :node
  :x :y}] :bands [edge + :link :x :y] :dropped {:edges n :nodes n}}` - nodes in column order, top to bottom; `:rect`
  the node's index there, `:link` the edge's index (group keys of the polygons, whatever the ids); `:sy` / `:ty` the
  band's top at the source / target (y up); rect and band rows are polygon points; edges of zero value and self-loops
  are left out, as are nodes without flow (counted; `:edges` also counts the graph's `:dropped` - edges with a `nil`
  end).
  Throws `ex-info` for a cycle (`:edges`), a bad option (its key), a `:pad` leaving no room in the fullest column
  (`:pad`)."
  ([g] (sankey-layout g {}))
  ([{:keys [nodes edges loops] :as g} opts]
   (let [{:keys [align layer node-width pad iterations node-sort] :as opts}
         (merge {:align :justify :node-width 0.03 :pad 0.02 :iterations 6} opts)
         _ (sankey-options! opts)
         flows (filterv #(pos? (double (:value %))) edges)
         used (set (mapcat (juxt :from :to) flows))
         all-count (count nodes)
         nodes (filterv #(used (:id %)) nodes)
         n (count nodes)
         index (zipmap (map :id nodes) (range))
         links (vec (map-indexed (fn [k e] {:link k :s (index (:from e)) :t (index (:to e)) :value (double (:value e)) :edge e}) flows))
         by-node (fn [k] (let [g (group-by k links)] (object-array (map #(mapv :link (get g % [])) (range n)))))
         ^objects outs (by-node :s)
         ^objects ins (by-node :t)
         ;; the graph's own drops (edges with a nil end), self-loops, zero flows
         dropped {:edges (+ (long (or (:dropped g) 0)) (count loops) (- (count edges) (count flows))) :nodes (- all-count n)}]
     (if (zero? n)
       {:nodes [] :links [] :rects [] :bands [] :dropped dropped}
       (let [values (mapv (fn [i] (max (double (reduce + 0.0 (map #(:value (links %)) (aget outs (long i)))))
                                       (double (reduce + 0.0 (map #(:value (links %)) (aget ins (long i)))))))
                          (range n))
             layers (sankey-layers nodes links [(vec outs) (vec ins)] [align (some->> layer (data/row-accessor :layer))])
             column-count (inc (long (reduce max 0 layers)))
             column-cells (object-array (map (fn [c] (filterv #(= c (layers %)) (range n))) (range column-count)))
             fullest (long (reduce max (map count column-cells)))
             pad (double pad)
             _ (when (>= (* (dec fullest) pad) 1.0)
                 (schema/fail! "Sankey :pad leaves no room for the nodes of the fullest column." {:key :pad :value pad}))
             ky (double (reduce min (map (fn [column] (/ (- 1.0 (* (dec (count column)) pad)) (double (reduce + 0.0 (map values column)))))
                                         column-cells)))
             y0s (double-array n) y1s (double-array n)
             widths (double-array (map #(* ky (double (:value %))) links))
             link-state [outs ins]]
         ;; start: each column stacked from the top, the free room spread around its nodes
         (doseq [column column-cells]
           (let [bottom (reduce (fn [^double y i] (aset y0s (long i) y) (aset y1s (long i) (+ y (* ky (double (values i)))))
                                  (+ (aget y1s (long i)) pad))
                                0.0 column)
                 spread (/ (+ (- 1.0 (double bottom)) pad) (double (inc (count column))))]
             (doseq [[k i] (map-indexed vector column)]
               (shift! y0s y1s (long i) (* spread (double (inc (long k))))))))
         (reorder-links! (range n) link-state links y0s)
         (let [state {:links links :layers layers :y0s y0s :y1s y1s :pad pad :link-state link-state :widths widths}
               fixed? (= :input node-sort)]
           (dotimes [i iterations]
             (let [alpha (m/pow 0.99 (double i))
                   beta (max (- 1.0 alpha) (/ (double (inc i)) (double iterations)))]
               (relax! column-cells :right-to-left state [alpha beta fixed?])
               (relax! column-cells :left-to-right state [alpha beta fixed?]))))
         (reorder-links! (range n) link-state links y0s)
         (let [kx (if (> column-count 1) (/ (- 1.0 (double node-width)) (double (dec column-count))) 0.0)
               x0-of (fn [i] (* kx (double (layers i))))
               ;; band tops (top-down) at the source and the target
               tops (reduce (fn [acc i]
                              (let [stack (fn [acc ls k]
                                            (first (reduce (fn [[acc ^double y] l] [(assoc-in acc [l k] y) (+ y (aget widths (long l)))])
                                                           [acc (aget y0s (long i))] ls)))]
                                (-> acc (stack (aget outs (long i)) :sy) (stack (aget ins (long i)) :ty))))
                            {} (range n))
               up (fn ^double [^double y] (- 1.0 y))
               placed (mapv (fn [{:keys [link s t edge]}]
                              (let [w (aget widths (long link))
                                    sy (up (double (get-in tops [link :sy])))
                                    ty (up (double (get-in tops [link :ty])))]
                                (assoc edge :link link :sy sy :ty ty :width w
                                       ::source-x (+ (double (x0-of s)) (double node-width)) ::target-x (x0-of t))))
                            links)
               order (vec (mapcat (fn [column] (sort-by #(aget y0s (long %)) column)) column-cells))
               node-of (fn [i] (let [x0 (double (x0-of i))]
                                 (assoc (nodes i) :layer (layers i) :x0 x0 :x1 (+ x0 (double node-width))
                                        :y0 (up (aget y1s (long i))) :y1 (up (aget y0s (long i))) :value (values i))))
               laid (mapv node-of order)]
           {:nodes laid
            :links (mapv #(dissoc % ::source-x ::target-x) placed)
            :rects (vec (for [[k {:keys [id x0 x1 y0 y1]}] (map-indexed vector laid) [x y] [[x0 y0] [x1 y0] [x1 y1] [x0 y1]]]
                          {:rect k :node id :x x :y y}))
            :bands (vec (for [{:keys [sy ty width] :as l} placed
                              [x y] (band-points [(::source-x l) sy] [(::target-x l) ty] width)]
                          (-> l (dissoc ::source-x ::target-x :sy :ty :width) (assoc :x x :y y))))
            :dropped dropped}))))))

;; alluvial

(defn- level-order
  "Distinct `values` in natural order, first-appearance order when they don't compare."
  [values]
  (let [vs (vec (distinct values))]
    (try (vec (sort vs)) (catch ClassCastException _ vs))))

(defn alluvial-graph
  "Directed graph (see [[graph]]) of an alluvial diagram of `rows` over `:axes` (accessors, at least 2): a node per
  level of every axis - `{:id [axis-index level] :name level :layer axis-index}`, levels in natural order (first
  appearance when they don't compare) - and an edge per pair of levels of consecutive axes and `:fill` level (an
  accessor, default the first axis) with the summed `:value` (an accessor, default 1 per row; see
  [[cljplot2.data.rows/weight]]) and `:fill` set; edges
  ordered by fill level, so strands of one level lie together at every node. For [[sankey-layout]] with
  `{:layer :layer :node-sort :input}`. Throws `ex-info {:key :axes}` for fewer than 2 axes or a row without a value
  of an axis."
  [rows {:keys [axes value fill]}]
  (when-not (and (sequential? axes) (<= 2 (count axes)))
    (schema/fail! "An alluvial diagram needs at least 2 :axes." {:key :axes :value axes}))
  (let [axis-of (mapv #(data/row-accessor :axes %) axes)
        value-of (if value (data/row-accessor :value value) (constantly 1.0))
        fill-of (data/row-accessor :fill (or fill (first axes)))
        levels-of (fn [row] (let [ls (mapv #(% row) axis-of)]
                              (when (some nil? ls)
                                (schema/fail! "Every row needs a value of every alluvial axis." {:key :axes :value row}))
                              ls))
        keyed (mapv (fn [row] [(levels-of row) (fill-of row) (data/weight (value-of row))]) rows)
        fill-rank (zipmap (level-order (map second keyed)) (range))
        nodes (vec (for [i (range (count axes)) level (level-order (map #(nth (first %) i) keyed))]
                     {:id [i level] :name level :layer i}))
        sums (reduce (fn [acc [ls f v]]
                       (reduce (fn [acc i] (update acc [i (ls i) (ls (inc (long i))) f] (fnil + 0.0) (double v)))
                               acc (range (dec (count axes)))))
                     {} keyed)
        edges (->> sums
                   (sort-by (fn [[[i _ _ f] _]] [(fill-rank f) i]))
                   (mapv (fn [[[i a b f] v]] {:from [i a] :to [(inc (long i)) b] :value v :fill f})))]
    (graph edges {:nodes nodes :directed? true})))
