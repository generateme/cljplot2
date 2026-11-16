(ns cljplot2.data.graph
  "Graphs from edge rows, and their layouts - node positions as data, drawn by ordinary charts (see
  [[cljplot2.multiplot/network]]).

  A graph is `{:nodes [node ..] :edges [edge ..] :loops [edge ..] :directed? b :dropped n}`:

  * nodes - the `:nodes` rows (each with an `:id`), then the ids seen only in edges, in first-appearance order; every
    node is its row (or `{}`) with `:id`, `:degree`, `:in`, `:out` (undirected graphs: `:in` = `:out` = `:degree`)
  * edges - the edge rows with `:from`, `:to` and `:value` (the weight, default `1.0`) set; self-loops (`:from` =
    `:to`) go to `:loops` instead: counted in the degrees (twice, as igraph), not laid out or drawn
  * `:dropped` - edge rows with a `nil` end

  A layout adds `:x` / `:y` to the nodes and `:x :y :xend :yend` to the edges (and loops), in the unit square:
  `:force` (Fruchterman-Reingold, igraph `layout_with_fr`), `:circle`, `:random`. Random starts and random layouts
  draw from `:rng` (a fastmath random generator, copied per call - see [[cljplot2.data.rng/for-build]]): one
  generator gives the same layout every time; without it layouts differ."
  (:require [fastmath.core :as m]
            [fastmath.random :as r]
            [cljplot2.data.rng :as rng]
            [cljplot2.data.rows :as data]
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
