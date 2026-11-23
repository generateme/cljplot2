(ns cljplot2.data.graph
  "Graphs built from edge rows, and their layouts as data.

  Node positions, arcs, ribbons and bands are plain rows drawn by ordinary charts (see [[cljplot2.graphs/network]],
  [[cljplot2.graphs/chord]], [[cljplot2.graphs/arc]], [[cljplot2.graphs/sankey]], [[cljplot2.graphs/alluvial]]).

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
  (the graph of an alluvial diagram: a node per level of every axis, strands between consecutive axes).

  Functions: [[graph]] and [[adjacency->edges]] build graphs and edges; [[layout]] places nodes; [[node-order]],
  [[chord-layout]], [[arc-layout]] lay out ordered diagrams; [[sankey-layout]] and [[alluvial-graph]] lay out flows.
  Every one with options takes a closed schema: [[graph-options]], [[adjacency->edges-options]], [[layout-options]],
  [[chord-layout-options]], [[arc-layout-options]], [[sankey-layout-options]], [[alluvial-graph-options]]; the
  `*-entries` defs are their shared groups forwarded by the builders."
  (:require [fastmath.core :as m]
            [fastmath.ml.clustering :as cl]
            [fastmath.random :as r]
            [fastmath.vector :as v]
            [cljplot2.data.rng :as rng]
            [cljplot2.data.rows :as data]
            [cljplot2.data.tree :as tree]
            [cljplot2.schema :as schema])
  (:import [fastmath.java Array]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def layouts
  "Node placements of [[layout]]: `:force` (Fruchterman-Reingold), `:circle`, `:random`."
  #{:force :circle :random})

;; option schemas

(def graph-entries
  "Option entries of [[graph]] (a shared group, `:graph/graph`; the graph builders of [[cljplot2.graphs]] forward
  them)."
  (schema/in-fragment
   :graph/graph
   [(schema/optional :from {:description "accessor of an edge row's source node id" :default :from} schema/accessor)
    (schema/optional :to {:description "accessor of an edge row's target node id" :default :to} schema/accessor)
    (schema/optional :value {:description "accessor of an edge row's weight, a finite number >= 0 (nil - 1)" :default :value}
                     schema/accessor)
    (schema/nilable :nodes {:description "node rows (each with an id), placed first in their order; ids seen only in edges follow"}
                    [:sequential map?])
    (schema/optional :id {:description "accessor of a node row's id" :default :id} schema/accessor)
    (schema/nilable :directed? {:description "a directed graph: degrees split into :in and :out" :default false}
                    schema/bool)]))

(def graph-options
  "Options schema of [[graph]]."
  (into [:map {:closed true}] graph-entries))

(defn- edge-rows!
  "Checks that `edges` are rows.

  Parameters:

  - `edges`: the edge rows of [[graph]].

  Returns `edges` unchanged. Throws `ex-info {:key :edges}` when they aren't a sequence of maps."
  [edges]
  (when-not (and (sequential? edges) (every? map? edges))
    (schema/fail! "Edges should be a sequence of maps (rows)." {:key :edges}))
  edges)

(defn- node-rows
  "Indexes the node rows of a graph by their ids.

  Parameters:

  - `nodes` (sequence of maps, may be `nil`): the node rows.
  - `id-of` (function): the id accessor of a row.

  Returns `[by-id order]` - `{id row}` and the ids in row order. Throws `ex-info {:key :nodes}` for a repeated or `nil` id."
  [nodes id-of]
  (reduce (fn [[by-id order] row]
            (let [id (id-of row)]
              (when (or (nil? id) (contains? by-id id))
                (schema/fail! "Node ids should be unique and not nil." {:key :nodes :value id}))
              [(assoc by-id id row) (conj order id)]))
          [{} []] nodes))

(defn graph
  "Builds the graph of edge rows (see the namespace doc for its shape).

  Parameters:

  - `edges` (sequence of maps): the edge rows.
  - `options` (map, optional):
    - `:from`, `:to` - accessors (keywords or functions) of an edge's source and target node ids (defaults `:from`, `:to`).
    - `:value` - accessor of an edge's weight, a finite number >= 0, `nil` read as 1 (default `:value`).
    - `:nodes` - node rows, placed first in their order; ids seen only in edges follow in first-appearance order.
    - `:id` - accessor of a node row's id (default `:id`).
    - `:directed?` - degrees split into `:in` and `:out` (default `false`).

  Returns `{:nodes :edges :loops :directed? :dropped}`: self-loops go to `:loops`, edges with a `nil` end are counted in `:dropped`.

  Throws `ex-info {:key k}` for an invalid option `k` (checked first, see [[graph-options]]), `{:key :edges}` for edges that aren't rows, `{:key :value}` for an invalid weight, `{:key :nodes}` for a repeated or `nil` node id.

  See also [[layout]], [[adjacency->edges]]."
  {:cljplot2/api true :cljplot2/options graph-options}
  ([edges] (graph edges {}))
  ([edges {:keys [from to value nodes id directed?] :or {from :from to :to value :value id :id directed? false} :as options}]
   (schema/check-options! graph-options options)
   (let [from-of (data/row-accessor :from from)
         to-of (data/row-accessor :to to)
         value-of (data/row-accessor :value value)
         [by-id order] (node-rows nodes (data/row-accessor :id id))
         all (mapv (fn [row] (assoc row :from (from-of row) :to (to-of row) :value (data/weight (value-of row))))
                   (edge-rows! edges))
         kept (filterv #(and (some? (:from %)) (some? (:to %))) all)
         {loops true links false} (group-by #(= (:from %) (:to %)) kept)
         ids (into order (comp (mapcat (juxt :from :to)) (remove by-id) (distinct)) kept)
         outs (frequencies (map :from kept))
         ins (frequencies (map :to kept))]
     {:nodes (mapv (fn [k] (let [out-degree (long (get outs k 0))
                                 in-degree (long (get ins k 0))
                                 degree (m/+ out-degree in-degree)]
                             (assoc (get by-id k {}) :id k :degree degree
                                    :in (if directed? in-degree degree) :out (if directed? out-degree degree))))
                   ids)
      :edges (vec links)
      :loops (vec loops)
      :directed? directed?
      :dropped (m/- (count all) (count kept))})))

(def adjacency->edges-options
  "Options schema of [[adjacency->edges]]."
  [:map {:closed true}
   (schema/optional :directed? {:description "read the whole matrix (false - only the upper triangle and the diagonal)"
                                :default true}
                    schema/bool)])

(defn adjacency->edges
  "Turns a square adjacency matrix into edge rows for [[graph]].

  Parameters:

  - `matrix` (sequence of sequences of numbers): row `i`, column `j` - the weight from node `i` to node `j`.
  - `names` (vector): the row / column names in order.
  - `options` (map, optional): `:directed?` - read the whole matrix; `false` - only the upper triangle and the diagonal (default `true`).

  Returns a vector of `{:from :to :value}`, row by row; a zero, `nil` or non-finite cell is no edge.

  Throws `ex-info {:key k}` for an invalid option `k` (checked first, see [[adjacency->edges-options]]), `{:key :matrix}` when the matrix isn't `n x n` for `n` names."
  {:cljplot2/api true :cljplot2/options adjacency->edges-options}
  ([matrix names] (adjacency->edges matrix names {}))
  ([matrix names {:keys [directed?] :or {directed? true} :as options}]
   (schema/check-options! adjacency->edges-options options)
   (let [n (count names)
         rows (vec matrix)]
     (when-not (and (m/== n (count rows)) (every? #(m/== n (count %)) rows))
       (schema/fail! "Adjacency matrix should be n x n for n names." {:key :matrix :value [(count rows) n]}))
     (vec (for [^long i (range n)
                ^long j (range n)
                :when (or directed? (m/<= i j))
                :let [v (nth (rows i) j)]
                :when (and (number? v) (m/valid-double? (double v)) (not (m/zero? (double v))))]
            {:from (names i) :to (names j) :value v})))))

;; layouts - positions in double arrays, node index order

(defn- circle-positions
  "Places nodes evenly on the circle of the unit square.

  Parameters:

  - `n` (long): the node count.

  Returns `[xs ys]` (double arrays): from the top clockwise around the centre `(0.5, 0.5)`, radius 0.5; one node - the centre."
  [^long n]
  (let [xs (double-array n)
        ys (double-array n)
        single? (m/one? n)]
    (dotimes [i n]
      (let [a (m// (m/* m/TWO_PI i) n)]
        (Array/aset xs i (if single? 0.5 (m/+ 0.5 (m/* 0.5 (m/sin a)))))
        (Array/aset ys i (if single? 0.5 (m/+ 0.5 (m/* 0.5 (m/cos a)))))))
    [xs ys]))

(defn- random-positions
  "Places nodes uniformly at random in the unit square.

  Parameters:

  - `g`: a fastmath random generator.
  - `n` (long): the node count.

  Returns `[xs ys]` (double arrays), each node's x then y drawn from `g`."
  [g ^long n]
  (let [xs (double-array n)
        ys (double-array n)]
    (dotimes [i n]
      (Array/aset xs i (r/drandom g))
      (Array/aset ys i (r/drandom g)))
    [xs ys]))

(def ^{:const true :private true :tag 'double} coincident-distance
  "Distance below which two nodes of the force layout count as coincident."
  1.0e-9)

(def ^{:const true :private true :tag 'double} golden-angle
  "The golden angle in radians: directions pushing coincident nodes apart, by the sum of their indices."
  2.399963)

(defn- repel!
  "Adds the Fruchterman-Reingold repulsion between every pair of nodes to their displacements.

  Parameters:

  - `xs`, `ys` (double arrays): node positions.
  - `[dx dy]` (double arrays): node displacements, added to in place.
  - `k2` (double): the squared ideal distance; the repulsion is `k2 / d`.

  Coincident nodes are pushed apart along a fixed direction given by their indices. Returns `nil`."
  [^doubles xs ^doubles ys [^doubles dx ^doubles dy] ^double k2]
  (let [n (alength xs)]
    (dotimes [i n]
      (loop [j (m/long-inc i)]
        (when (m/< j n)
          (let [ex (m/- (Array/aget xs i) (Array/aget xs j))
                ey (m/- (Array/aget ys i) (Array/aget ys j))
                d0 (m/hypot ex ey)
                close? (m/< d0 coincident-distance)
                a (m/* golden-angle (m/+ i j))
                d (if close? coincident-distance d0)
                f (m// k2 d)
                fx (m/* f (if close? (m/cos a) (m// ex d0)))
                fy (m/* f (if close? (m/sin a) (m// ey d0)))]
            (Array/add dx i fx)
            (Array/add dy i fy)
            (Array/sub dx j fx)
            (Array/sub dy j fy)
            (recur (m/long-inc j))))))))

(defn- attract!
  "Adds the Fruchterman-Reingold attraction along every edge to the displacements of its ends.

  Parameters:

  - `xs`, `ys` (double arrays): node positions.
  - `[dx dy]` (double arrays): node displacements, added to in place.
  - `[ends k]`: `ends` - edges as `[i j w]` (node indices, `w` the weight over the mean weight); `k` (double) - the ideal distance; the attraction is `w d^2 / k`.

  Returns `nil`."
  [^doubles xs ^doubles ys [^doubles dx ^doubles dy] [ends ^double k]]
  (doseq [[^long i ^long j ^double w] ends]
    (let [ex (m/- (Array/aget xs i) (Array/aget xs j))
          ey (m/- (Array/aget ys i) (Array/aget ys j))
          d (m/hypot ex ey)
          f (m// (m/* w d) k)
          fx (m/* ex f)
          fy (m/* ey f)]
      (Array/sub dx i fx)
      (Array/sub dy i fy)
      (Array/add dx j fx)
      (Array/add dy j fy))))

(defn- gravitate!
  "Adds a pull towards the nodes' centroid to their displacements.

  The pull keeps loosely connected nodes and components near the rest instead of drifting away; the pulls sum to zero, so a symmetric layout stays undistorted.

  Parameters:

  - `xs`, `ys` (double arrays): node positions.
  - `[dx dy]` (double arrays): node displacements, added to in place.
  - `g` (double): the pull per unit distance.

  Returns `nil`."
  [^doubles xs ^doubles ys [^doubles dx ^doubles dy] ^double g]
  (let [n (alength xs)
        cx (m// (v/sum xs) (m/max 1 n))
        cy (m// (v/sum ys) (m/max 1 n))]
    (dotimes [i n]
      (Array/sub dx i (m/* g (m/- (Array/aget xs i) cx)))
      (Array/sub dy i (m/* g (m/- (Array/aget ys i) cy))))))

(defn- move!
  "Moves every node by its displacement, limited to a temperature.

  Parameters:

  - `xs`, `ys` (double arrays): node positions, moved in place.
  - `[dx dy]` (double arrays): node displacements.
  - `t` (double): the longest step.

  Returns `nil`."
  [^doubles xs ^doubles ys [^doubles dx ^doubles dy] ^double t]
  (dotimes [i (alength xs)]
    (let [ex (Array/aget dx i)
          ey (Array/aget dy i)
          d (m/hypot ex ey)
          s (if (m/> d t) (m// t d) 1.0)]
      (Array/add xs i (m/* ex s))
      (Array/add ys i (m/* ey s)))))

(defn- force-positions
  "Lays out nodes by Fruchterman-Reingold from random positions.

  The ideal distance is `k = sqrt(1 / n)`; the temperature falls linearly from 0.1 to 0; a pull to the centroid replaces a frame. The result is scaled into the unit square afterwards (see [[fitted]]).

  Parameters:

  - `g`: a fastmath random generator of the start positions.
  - `n` (long): the node count.
  - `ends`: edges as `[i j w]` - node indices and weight.
  - `[iterations gravity]`: steps of the layout (long) and the pull to the centroid per unit distance in units of `k` (double).

  Returns `[xs ys]` (double arrays)."
  [g ^long n ends [^long iterations ^double gravity]]
  (let [[^doubles xs ^doubles ys] (random-positions g n)
        k (m/sqrt (m// 1.0 (m/max 1 n)))
        mean-w (if (seq ends) (m// (v/sum (map #(nth % 2) ends)) (count ends)) 1.0)
        relative-ends (if (m/pos? mean-w) (mapv (fn [[i j ^double w]] [i j (m// w mean-w)]) ends) ends)]
    (dotimes [step iterations]
      (let [disp [(double-array n) (double-array n)]]
        (repel! xs ys disp (m/* k k))
        (attract! xs ys disp [relative-ends k])
        (gravitate! xs ys disp (m/* gravity k))
        (move! xs ys disp (m/* 0.1 (m/- 1.0 (m// (double step) iterations))))))
    [xs ys]))

(defn- fitted
  "Scales positions uniformly into the unit square, centred.

  Parameters:

  - `[xs ys]` (double arrays): positions, not changed.

  Returns `[xs ys]` (new double arrays); a single position or positions without spread - the centre `(0.5, 0.5)`; no positions - the arrays as given."
  [[^doubles xs ^doubles ys]]
  (if (m/zero? (alength xs))
    [xs ys]
    (let [x0 (v/mn xs)
          x1 (v/mx xs)
          y0 (v/mn ys)
          y1 (v/mx ys)
          span (m/max (m/- x1 x0) (m/- y1 y0))
          s (if (m/pos? span) (m// 1.0 span) 0.0)
          cx (m/* 0.5 (m/+ x0 x1))
          cy (m/* 0.5 (m/+ y0 y1))]
      [(amap xs i _ (m/+ 0.5 (m/* s (m/- (Array/aget xs i) cx))))
       (amap ys i _ (m/+ 0.5 (m/* s (m/- (Array/aget ys i) cy))))])))

(defn- components
  "Finds the connected components of a graph given by node indices.

  Parameters:

  - `n` (long): the node count; nodes are `0..n-1`.
  - `ends`: edges as `[i j ...]` (node indices).

  Returns a vector of components, each a vector of node indices in increasing order, the components ordered by their smallest index."
  [^long n ends]
  (let [neighbours (reduce (fn [acc [i j]] (-> acc
                                               (update i conj j)
                                               (update j conj i)))
                           (vec (repeat n [])) ends)]
    (loop [i 0
           seen #{}
           out []]
      (cond
        (m/== i n) out
        (seen i) (recur (m/long-inc i) seen out)
        :else (let [part (loop [queue [i]
                                reached #{i}]
                           (if-let [k (first queue)]
                             (let [fresh (remove reached (neighbours k))]
                               (recur (into (subvec queue 1) fresh) (into reached fresh)))
                             reached))]
                (recur (m/long-inc i) (into seen part) (conj out (vec (sort part)))))))))

(def ^{:private true :const true :tag 'double} component-gap
  "Gap between packed components, in units of a one-node component's side: wider than an edge inside a fitted
  component (about 1 - 1.7), so separate parts don't read as linked."
  2.0)

(defn- component-side
  "Side of the square a component is fitted into.

  Parameters:

  - `part` (collection): the component's node indices.

  Returns the square root of its node count (double)."
  ^double [part]
  (m/sqrt (count part)))

(defn- packed-force-positions
  "Lays out every connected component by force on its own and packs them in rows.

  Each component is fitted into a square of side sqrt(its node count); the squares are packed in rows - largest first, a row at most as wide as the square root of their total area - with [[component-gap]] between them (as igraph lays out and merges components). Components draw their random starts from `g` in packing order.

  Parameters:

  - `g`: a fastmath random generator.
  - `n` (long): the node count.
  - `ends`: edges as `[i j w]` - node indices and weight.
  - `opts`: `[iterations gravity]` of [[force-positions]].

  Returns `[xs ys]` (double arrays); one component - its [[force-positions]] as they are."
  [g ^long n ends opts]
  (let [parts (components n ends)]
    (if (m/<= (count parts) 1)
      (force-positions g n ends opts)
      (let [largest-first (sort-by count > parts)
            row-width (m/max (component-side (first largest-first))
                             (m/sqrt (v/sum (map #(m/sq (m/+ (component-side %) component-gap)) largest-first))))
            xs (double-array n)
            ys (double-array n)]
        (loop [[part & more] largest-first
               x 0.0
               y 0.0
               row-height 0.0]
          (when part
            (let [s (component-side part)
                  ;; the next row below: y decreasing
                  wrap? (and (m/pos? x) (m/> (m/+ x s) row-width))
                  left (if wrap? 0.0 x)
                  top (if wrap? (m/- y row-height component-gap) y)
                  height (if wrap? 0.0 row-height)
                  index (zipmap part (range))
                  local (filterv (fn [[i j]] (and (index i) (index j))) ends)
                  [^doubles px ^doubles py] (fitted (force-positions g (count part) (mapv (fn [[i j w]] [(index i) (index j) w]) local) opts))]
              (doseq [[^long k ^long i] (map-indexed vector part)]
                (Array/aset xs i (m/+ left (m/* s (Array/aget px k))))
                (Array/aset ys i (m/- top (m/* s (Array/aget py k)))))
              (recur more (m/+ left s component-gap) top (m/max height s)))))
        [xs ys]))))

(def layout-entries
  "Option entries of [[layout]] (a shared group, `:graph/layout`; [[cljplot2.graphs/network]] forwards them)."
  (schema/in-fragment
   :graph/layout
   [(schema/optional :layout {:description "node placement: :force (Fruchterman-Reingold), :circle or :random" :default :force}
                     (into [:enum] (sort layouts)))
    (schema/optional :iterations {:description "steps of the force layout" :default 500} [:int {:min 0}])
    (schema/optional :gravity {:description "pull of the force layout to each component's centroid, in units of the ideal edge length (0 - none)"
                               :default 1.0}
                     schema/finite-non-neg)
    (schema/nilable :rng {:description "fastmath random generator of the force and random layouts, copied per call (seeded - the same layout every time)"
                          :cljplot2/default-doc "a fresh unseeded generator every call"}
                    rng/schema)]))

(def layout-options
  "Options schema of [[layout]]."
  (into [:map {:closed true}] layout-entries))

(defn layout
  "Places the nodes of a graph in the unit square.

  A force layout places every connected component on its own and packs them in rows, largest first (see [[packed-force-positions]]); it is scaled into the unit square keeping its proportions.

  Parameters:

  - `g` (map): a graph of [[graph]].
  - `options` (map, optional):
    - `:layout` - `:force` (Fruchterman-Reingold, default), `:circle` (from the top clockwise) or `:random`.
    - `:iterations` - steps of the force layout (default 500).
    - `:gravity` - the force layout's pull to each component's centroid in units of the ideal edge length (default 1; 0 - none; more - a tighter layout).
    - `:rng` - a fastmath random generator of the force and random layouts, copied per call (default a fresh unseeded one: layouts differ).

  Returns `g` with `:x` / `:y` on its nodes and `:x :y :xend :yend` on its edges and loops.

  Throws `ex-info {:key k}` for an invalid option `k` (checked first, see [[layout-options]]).

  See also [[chord-layout]], [[arc-layout]], [[sankey-layout]]."
  {:cljplot2/api true :cljplot2/options layout-options}
  ([g] (layout g {}))
  ([{:keys [nodes edges loops] :as g} {:keys [iterations gravity rng] kind :layout :or {kind :force iterations 500 gravity 1.0}
                                       :as options}]
   (schema/check-options! layout-options options)
   (let [n (count nodes)
         index (zipmap (map :id nodes) (range))
         [^doubles xs ^doubles ys] (case kind
                                     :circle (circle-positions n)
                                     :random (random-positions (rng/for-build rng) n)
                                     :force (fitted (packed-force-positions (rng/for-build rng) n
                                                                     (mapv (fn [e] [(index (:from e)) (index (:to e)) (:value e)]) edges)
                                                                     [iterations gravity])))
         placed (fn [e] (let [i (long (index (:from e)))
                              j (long (index (:to e)))]
                          (assoc e :x (Array/aget xs i) :y (Array/aget ys i) :xend (Array/aget xs j) :yend (Array/aget ys j))))]
     (assoc g
            :nodes (vec (map-indexed (fn [^long i node] (assoc node :x (Array/aget xs i) :y (Array/aget ys i))) nodes))
            :edges (mapv placed edges)
            :loops (mapv placed loops)))))

;; ordered layouts

(defn- weight-matrix
  "Sums the edge weights between every pair of nodes.

  Parameters:

  - `g` (map): a graph of [[graph]] - `:edges`, `:directed?`.
  - `ids` (vector): node ids, the matrix's row and column order.

  Returns `n x n` vectors of doubles: row - the source, column - the target, each the summed `:value` of the edges between them; an undirected graph's edge counts both ways. Loops aren't edges."
  [{:keys [edges directed?]} ids]
  (let [n (count ids)
        index (zipmap ids (range))
        add (fn [w i j value] (update-in w [i j] m/+ value))]
    (reduce (fn [w {:keys [from to value]}]
              (let [i (index from) j (index to)]
                (cond-> (add w i j value) (not directed?) (add j i value))))
            (vec (repeat n (vec (repeat n 0.0))))
            edges)))

(defn- weight-at
  "Reads a weight of a weight matrix.

  Parameters:

  - `w`: weights of [[weight-matrix]].
  - `i`, `j` (long): source and target node indices.

  Returns the weight from node `i` to node `j` (double)."
  ^double [w i j]
  ((w i) j))

(defn- unit-row
  "Scales a row of numbers to length 1.

  Parameters:

  - `row` (vector of numbers).

  Returns the row divided by its Euclidean length; an all-zero row as it is."
  [row]
  (let [len (v/mag row)]
    (if (m/pos? len) (mapv (fn [^double x] (m// x len)) row) row)))

(defn- cluster-order
  "Orders the nodes of a graph so that nodes linked to the same others are side by side.

  The order is the leaf order of fastmath `hclust` (average linkage, Euclidean distance) on the rows of W + W^T (W - [[weight-matrix]]) scaled to unit length - the cosine distance, whatever the weights' size (a hub stays among its neighbours).

  Parameters:

  - `g` (map): a graph of [[graph]].

  Returns a vector of node ids; fewer than 3 nodes - their input order."
  [{:keys [nodes] :as g}]
  (let [ids (mapv :id nodes)
        n (count ids)]
    (if (m/< n 3)
      ids
      (let [w (weight-matrix g ids)
            rows (mapv (fn [i] (unit-row (mapv (fn [j] (m/+ (weight-at w i j) (weight-at w j i))) (range n)))) (range n))]
        (mapv ids (:order (:tree (cl/hclust rows {:method :average :distance :euclidean}))))))))

(defn node-order
  "Orders the node ids of a graph for the ordered layouts.

  Parameters:

  - `g` (map): a graph of [[graph]].
  - `order`: `nil` - the input order; a sequence of ids - those first (a repeated id once), the rest after them in input order; `:cluster` - nodes linked to the same others side by side (see [[cluster-order]]).

  Returns a vector of every node id.

  Throws `ex-info {:key :order}` for an id not in the graph or another kind of `order`.

  See also [[chord-layout]], [[arc-layout]]."
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

(def ^{:private true :const true :tag 'long} bezier-steps
  "Steps of a chord ribbon's curve through the centre (see [[through-centre]])."
  24)

(defn- through-centre
  "Samples the quadratic Bezier curve between two points with its control point in the centre.

  Parameters:

  - `[x0 y0]`, `[x1 y1]` (numbers): the curve's ends.

  Returns `[x y]` points at [[bezier-steps]] steps, ends excluded."
  [[^double x0 ^double y0] [^double x1 ^double y1]]
  (mapv (fn [^long s] (let [t (m// (double s) bezier-steps)
                            u (m/- 1.0 t)]
                        [(m/+ (m/* u u x0) (m/* t t x1))
                         (m/+ (m/* u u y0) (m/* t t y1))]))
        (range 1 bezier-steps)))

(defn- chord-angles
  "Computes d3-chord angles of the ring sectors and ribbon ends.

  Angles are radians, clockwise from the top. Directed (d3 `chordDirected`): group sums over rows and columns, the ribbon `i -> j` of width w(i, j) at both ends, a group's incoming ends (from the last node down) before its outgoing ones. Undirected (d3 `chord`): group sums over rows, the ribbon of a pair from w(i, j) at `i` - the end earlier in `order` - to w(j, i) at `j`.

  Parameters:

  - `w`: weights of [[weight-matrix]].
  - `order` (sequence of node indices): the ring order.
  - `directed?`: the directed variant.
  - `pad` (double): radians after every group.

  Returns `[groups ends]` - `groups` `{i [start end sum]}`, `ends` `{[i j] {:source [a0 a1] :target [a0 a1]}}`."
  [w order directed? ^double pad]
  (let [n (count w)
        rank (zipmap order (range))
        sums (mapv (fn [i] (v/sum (map #(m/+ (weight-at w i %) (if directed? (weight-at w % i) 0.0)) (range n)))) (range n))
        total (v/sum sums)
        k (if (m/pos? total) (m// (m/max 0.0 (m/- m/TWO_PI (m/* pad n))) total) 0.0)
        dx (if (m/pos? k) pad (m// m/TWO_PI (m/max 1 n)))
        subgroups (fn [i] (if directed?
                            (concat (for [j (range (m/long-dec n) -1 -1) :when (m/pos? (weight-at w j i))] [:in j (weight-at w j i)])
                                    (for [j (range n) :when (m/pos? (weight-at w i j))] [:out j (weight-at w i j)]))
                            (for [j (range n) :when (or (m/pos? (weight-at w i j)) (m/pos? (weight-at w j i)))]
                              [(if (m/< (long (rank i)) (long (rank j))) :out :in) j (weight-at w i j)])))]
    (loop [[i & more :as left] (seq order)
           start 0.0
           groups {}
           ends {}]
      (if-not left
        [groups ends]
        (let [[^double end grown] (reduce (fn [[^double from placed] [dir j ^double value]]
                                            (let [to (m/+ from (m/* k value))]
                                              [to (if (= :out dir)
                                                    (assoc-in placed [[i j] :source] [from to])
                                                    (assoc-in placed [[j i] :target] [from to]))]))
                                          [start ends] (subgroups i))]
          (recur more (m/+ end dx) (assoc groups i [start end (sums i)]) grown))))))

(defn- arc-band
  "Samples the ring sector between radius `inner` and 1.

  Parameters:

  - `inner` (double): the inner radius.
  - `a0`, `a1` (double): the start and end angles (radians, clockwise from the top).

  Returns `[x y]` points: the outer arc, then the inner one back."
  [^double inner ^double a0 ^double a1]
  (into (tree/arc-points 1.0 a0 a1) (rseq (tree/arc-points inner a0 a1))))

(defn- ribbon-points
  "Samples a chord ribbon between two arcs (d3 `ribbon`).

  Parameters:

  - `r` (double): the radius of both arcs.
  - `[s0 s1]`, `[t0 t1]`: the source and target arcs' angles (radians).

  Returns `[x y]` points: the source arc, a curve through the centre, the target arc, a curve back."
  [^double r [s0 s1] [t0 t1]]
  (let [source (tree/arc-points r s0 s1)
        target (tree/arc-points r t0 t1)]
    (-> source
        (into (through-centre (peek source) (first target)))
        (into target)
        (into (through-centre (peek target) (first source))))))

(def order-entry
  "Option entry `:order` of the ordered layouts (see [[node-order]])."
  (schema/nilable :order {:description "node order: a sequence of ids (those first, the rest after them in input order) or :cluster (nodes linked to the same others side by side)"
                          :cljplot2/default-doc "the nodes' input order"}
                  [:or [:= :cluster] [:sequential any?]]))

(def chord-layout-entries
  "Option entries of [[chord-layout]] (a shared group, `:graph/chord-layout`; [[cljplot2.graphs/chord]] forwards
  them)."
  (schema/in-fragment
   :graph/chord-layout
   [order-entry
    (schema/optional :pad {:description "gap after every ring sector in degrees (must leave room for the sectors)" :default 2.0}
                     schema/finite-non-neg)
    (schema/optional :inner {:description "inner radius of the ring, where the ribbons end (the outer one is 1)" :default 0.92}
                     schema/open-fraction)
    (schema/nilable :min-value {:description "flows of a pair (summed) below it are left out and counted in :dropped (nil - none)"}
                    schema/finite-non-neg)]))

(def chord-layout-options
  "Options schema of [[chord-layout]]."
  (into [:map {:closed true}] chord-layout-entries))

(defn chord-layout
  "Lays out a chord diagram of a graph as data in the unit circle.

  A ring sector per node, its angle proportional to the node's flow (undirected: its edges' weights; directed: out plus in), clockwise from the top in node order; a ribbon per linked pair (undirected) or edge (directed) joins its ends' arcs through the centre - d3 `chord` / `chordDirected` (see [[chord-angles]]); an undirected ribbon's `:from` is its end earlier in the ring.

  Parameters:

  - `g` (map): a graph of [[graph]].
  - `options` (map, optional):
    - `:order` - the node order (see [[node-order]]; default the input order).
    - `:pad` - gap after every sector in degrees (default 2).
    - `:inner` - the ring's inner radius, where the ribbons end (default 0.92; the outer one is 1).
    - `:min-value` - a summed weight below it is left out: of an ordered pair (directed; repeated edges summed) or of a pair both ways (undirected); its edges are counted in `:dropped` (default `nil` - none).

  Returns `{:groups [{:id :start :end :value}] :arcs [{:group :x :y}] :ribbons [{:ribbon :from :to :value :x :y}] :dropped n}` - angles in degrees; a ribbon's `:value` is its source end's weight; arc and ribbon rows are polygon points in order.

  Throws `ex-info {:key k}` for an invalid option `k` (checked first, see [[chord-layout-options]]), `{:key :pad}` for a `:pad` leaving no room for the sectors, `{:key :order}` for a bad order.

  See also [[layout]], [[arc-layout]]."
  {:cljplot2/api true :cljplot2/options chord-layout-options}
  ([g] (chord-layout g {}))
  ([{:keys [edges directed?] :as g} {:keys [order ^double pad ^double inner min-value] :or {pad 2.0 inner 0.92} :as options}]
   (schema/check-options! chord-layout-options options)
   (when-not (m/< (m/* pad (count (:nodes g))) 360.0)
     (schema/fail! "Chord :pad leaves no room for the arcs." {:key :pad :value pad}))
   (let [ids (mapv :id (:nodes g))
         index (zipmap ids (range))
         summed (weight-matrix g ids)
         weak? (fn [i j] (and min-value (m/< (weight-at summed i j) (double min-value))))
         w (mapv (fn [i] (mapv (fn [j] (if (weak? i j) 0.0 (weight-at summed i j))) (range (count ids)))) (range (count ids)))
         [groups ends] (chord-angles w (mapv index (node-order g order)) directed? (m/radians pad))]
     {:groups (vec (for [i (range (count ids))
                         :let [[^double a0 ^double a1 value] (groups i)]]
                     {:id (ids i) :start (m/degrees a0) :end (m/degrees a1) :value value}))
      :arcs (vec (for [i (range (count ids))
                       :let [[^double a0 ^double a1] (groups i)]
                       :when (m/> a1 a0)
                       [x y] (arc-band inner a0 a1)]
                   {:group (ids i) :x x :y y}))
      :ribbons (vec (for [[[i j] {:keys [source target]}] (sort-by key ends)
                          :let [k (str (ids i) " -> " (ids j))]
                          [x y] (ribbon-points inner source target)]
                      {:ribbon k :from (ids i) :to (ids j) :value ((w i) j) :x x :y y}))
      :dropped (count (filter #(weak? (index (:from %)) (index (:to %))) edges))})))

(def arc-layout-entries
  "Option entries of [[arc-layout]] (a shared group, `:graph/arc-layout`; [[cljplot2.graphs/arc]] forwards them)."
  (schema/in-fragment :graph/arc-layout [order-entry]))

(def arc-layout-options
  "Options schema of [[arc-layout]]."
  (into [:map {:closed true}] arc-layout-entries))

(defn arc-layout
  "Lays out an arc diagram of a graph as data.

  Nodes lie on the x axis at 0, 1, ... in node order, y = 0; every edge is a semicircle above the axis between its ends (peak height half their distance), sampled every ~2 degrees.

  Parameters:

  - `g` (map): a graph of [[graph]].
  - `options` (map, optional): `:order` - the node order (see [[node-order]]; default the input order).

  Returns `{:nodes [node + :x :y] :arcs [{:edge :from :to :value :x :y}]}` - `:edge` the edge's index, arcs in edge order.

  Throws `ex-info {:key k}` for an invalid option `k` (checked first, see [[arc-layout-options]]), `{:key :order}` for an id not in the graph.

  See also [[chord-layout]], [[layout]]."
  {:cljplot2/api true :cljplot2/options arc-layout-options}
  ([g] (arc-layout g {}))
  ([{:keys [nodes edges] :as g} {:keys [order] :as options}]
   (schema/check-options! arc-layout-options options)
   (let [ids (node-order g order)
         x-of (zipmap ids (range))
         by-id (zipmap (map :id nodes) nodes)]
     {:nodes (mapv (fn [id] (assoc (by-id id) :x (double (x-of id)) :y 0.0)) ids)
      :arcs (vec (for [[k {:keys [from to value]}] (map-indexed vector edges)
                       :let [x0 (double (x-of from))
                             x1 (double (x-of to))
                             c (m/* 0.5 (m/+ x0 x1))
                             r (m/* 0.5 (m/abs (m/- x1 x0)))
                             ;; from the left end over the top to the right end
                             [a0 a1] (if (m/<= x0 x1) [(m/- m/HALF_PI) m/HALF_PI] [m/HALF_PI (m/- m/HALF_PI)])]
                       [^double x y] (tree/arc-points r a0 a1)]
                   {:edge k :from from :to to :value value :x (m/+ c x) :y y}))})))

;; sankey (d3-sankey port; vertical positions computed top-down as d3, returned y-up)

(def sankey-aligns
  "Column alignments of [[sankey-layout]]: `:justify`, `:left`, `:right`, `:center`."
  #{:justify :left :right :center})

(def sankey-layout-entries
  "Option entries of [[sankey-layout]] (a shared group, `:graph/sankey-layout`; [[cljplot2.graphs/sankey]] forwards
  them)."
  (schema/in-fragment
   :graph/sankey-layout
   [(schema/optional :align {:description "the column of a node: :justify (sinks in the last column), :left, :right (by the longest path to a sink) or :center"
                             :default :justify}
                     (into [:enum] (sort sankey-aligns)))
    (schema/nilable :layer {:description "accessor of the node rows giving the column (integers >= 0, every edge to a later one; wins over :align)"
                            :cljplot2/default-doc "columns from :align"}
                    schema/accessor)
    (schema/optional :node-width {:description "node width, a fraction of the unit square's width" :default 0.03}
                     schema/open-fraction)
    (schema/optional :pad {:description "least gap between the nodes of a column, a fraction of the unit square's height"
                           :default 0.02}
                     schema/finite-non-neg)
    (schema/optional :iterations {:description "relaxation sweeps moving the nodes towards their links' ends (0 - spread in the start order)"
                                  :default 6}
                     [:int {:min 0}])
    (schema/nilable :node-sort {:description "nil - relaxation reorders a column, :input - the input order kept"}
                    [:enum :input])]))

(def sankey-layout-options
  "Options schema of [[sankey-layout]]."
  (into [:map {:closed true}] sankey-layout-entries))

(defn- longest-paths
  "Measures the longest path (in links) reaching every node.

  Parameters:

  - `n` (long): the node count; nodes are `0..n-1`.
  - `next-of` (function): node index -> the next node indices - targets for the depth from the sources, sources for the height from the sinks.

  Returns a vector of path lengths by node index.

  Throws `ex-info {:key :edges}` for a cycle."
  [^long n next-of]
  (let [result (long-array n)]
    (loop [current (set (range n))
           x 0]
      (when (seq current)
        (when (m/> x n)
          (schema/fail! "A sankey needs a graph without cycles." {:key :edges}))
        (doseq [^long i current] (Array/aset result i x))
        (recur (set (mapcat next-of current)) (m/long-inc x))))
    (vec result)))

(defn- sankey-layers
  "Assigns every node of a sankey to a column.

  Parameters:

  - `nodes` (vector): the node rows.
  - `links` (vector): links `{:s :t ...}` by index (source and target node indices).
  - `[outs ins]`: per node index its outgoing and incoming link indices.
  - `[align layer-of]`: `align` - `:justify`, `:left`, `:right` or `:center` (as d3-sankey, from the nodes' depths and heights); `layer-of` - accessor of a node row's column, winning over `align` (`nil` - none).

  Returns a vector of columns by node index.

  Throws `ex-info {:key :layer}` when `layer-of` gives a value that isn't a non-negative integer, or a link not going to a later column; `{:key :edges}` for a cycle."
  [nodes links [outs ins] [align layer-of]]
  (if layer-of
    (let [layers (mapv layer-of nodes)]
      (when-not (every? #(and (integer? %) (m/not-neg? (long %))) layers)
        (schema/fail! "Sankey :layer should give every node an integer >= 0." {:key :layer :value layers}))
      (when-let [back (first (filter (fn [{:keys [s t]}] (m/<= (long (layers t)) (long (layers s)))) links))]
        (schema/fail! "Sankey links should go to a later :layer." {:key :layer :value [(:id (nodes (:s back))) (:id (nodes (:t back)))]}))
      layers)
    (let [n (count nodes)
          depth (longest-paths n (fn [i] (map #(:t (links %)) (outs i))))
          height (longest-paths n (fn [i] (map #(:s (links %)) (ins i))))
          last-column (long (v/mx depth))]
      (mapv (fn [i]
              (let [d (long (depth i))]
                (m/max 0 (m/min last-column
                                (long (case align
                                        :left d
                                        :right (m/- last-column (long (height i)))
                                        :justify (if (seq (outs i)) d last-column)
                                        :center (cond (seq (ins i)) d
                                                      (seq (outs i)) (m/long-dec (long (v/mn (map #(depth (:t (links %))) (outs i)))))
                                                      :else 0)))))))
            (range n)))))

(defn- link-order
  "Sorts links by the top of the node at one of their ends (d3 `ascendingSourceBreadth` / `ascendingTargetBreadth`).

  Parameters:

  - `ids` (sequence): link indices.
  - `end-of` (function): link index -> the node index it leads to.
  - `y0s` (double array): node tops by node index.

  Returns a vector of the link indices, ties by index."
  [ids end-of ^doubles y0s]
  (vec (sort-by (fn [l] [(Array/aget y0s (long (end-of l))) l]) ids)))

(defn- reorder-links!
  "Re-sorts the links of nodes by the tops of the nodes at their other ends.

  Parameters:

  - `is` (sequence): node indices.
  - `[outs ins]` (object arrays): per node index its outgoing and incoming link indices, replaced in place - outgoing by target top, incoming by source top.
  - `links` (vector): links `{:s :t ...}` by index.
  - `y0s` (double array): node tops.

  Returns `nil`."
  [is [^objects outs ^objects ins] links y0s]
  (doseq [^long i is]
    (aset outs i (link-order (aget outs i) #(:t (links %)) y0s))
    (aset ins i (link-order (aget ins i) #(:s (links %)) y0s))))

(defn- level-node-top
  "Computes where the far node of a link should start for the link to run level.

  The near node's links are stacked from its top with gaps `pad` (centred), the far node's links stacked without (d3 `targetTop` for the source near, `sourceTop` for the target near).

  Parameters:

  - `l` (long): the link index.
  - `near`: `:source` (the link's source node is near) or `:target`.
  - `[outs ins]` (object arrays): per node index its outgoing and incoming link indices.
  - `[links y0s widths pad]`: links `{:s :t ...}` by index, node tops (double array), link widths (double array), the gap between links at a node (double).

  Returns the far node's top (double)."
  ^double [l near [^objects outs ^objects ins] [links ^doubles y0s ^doubles widths ^double pad]]
  (let [{:keys [^long s ^long t]} (links l)
        [^long node own other] (if (= :source near) [s (aget outs s) (aget ins t)] [t (aget ins t) (aget outs s)])
        start (m/- (Array/aget y0s node) (m// (m/* (m/long-dec (count own)) pad) 2.0))
        before (fn [ls] (v/sum (map #(Array/aget widths (long %)) (take-while #(not= l %) ls))))]
    ;; the link's position in its own node's stack (with gaps), less its position at the other end (without)
    (m/- (m/+ start (double (before own)) (m/* pad (count (take-while #(not= l %) own)))) (double (before other)))))

(defn- shift!
  "Moves a sankey node down.

  Parameters:

  - `y0s`, `y1s` (double arrays): node tops and bottoms, changed in place.
  - `i` (long): the node index.
  - `dy` (double): the shift (negative - up).

  Returns the node's new bottom."
  [^doubles y0s ^doubles y1s ^long i ^double dy]
  (Array/add y0s i dy)
  (Array/add y1s i dy))

(def ^{:private true :const true :tag 'double} least-shift
  "Shortest move of a node resolving a collision; smaller moves are skipped."
  1.0e-6)

(defn- resolve-collisions!
  "Pushes the nodes of a sankey column apart to gaps of at least `pad` inside [0, 1] (d3 `resolveCollisions`).

  Parameters:

  - `column` (vector): node indices, top to bottom.
  - `[y0s y1s]` (double arrays): node tops and bottoms, changed in place.
  - `pad` (double): the least gap.
  - `alpha` (double): the fraction of an overlap moved per step.

  An empty column (possible with a fixed `:layer`) is left alone. Returns `nil`."
  [column [^doubles y0s ^doubles y1s] ^double pad ^double alpha]
  (let [k (count column)
        down! (fn [^double top ^long from] (loop [y top
                                                  i from]
                                             (when (m/< i k)
                                               (let [node (long (column i))
                                                     dy (m/* (m/- y (Array/aget y0s node)) alpha)]
                                                 (when (m/> dy least-shift) (shift! y0s y1s node dy))
                                                 (recur (m/+ (Array/aget y1s node) pad) (m/long-inc i))))))
        up! (fn [^double bottom ^long from] (loop [y bottom
                                                   i from]
                                              (when (m/not-neg? i)
                                                (let [node (long (column i))
                                                      dy (m/* (m/- (Array/aget y1s node) y) alpha)]
                                                  (when (m/> dy least-shift) (shift! y0s y1s node (m/- dy)))
                                                  (recur (m/- (Array/aget y0s node) pad) (m/long-dec i))))))
        middle (m/long-quot k 2)]
    (when (m/pos? k)
      (let [subject (long (column middle))]
        (up! (m/- (Array/aget y0s subject) pad) (m/long-dec middle))
        (down! (m/+ (Array/aget y1s subject) pad) (m/long-inc middle)))
      (up! 1.0 (m/long-dec k))
      (down! 0.0 0))))

(defn- relax!
  "Runs one relaxation sweep of a sankey (d3 `relaxRightToLeft` / `relaxLeftToRight`).

  Every node of the swept columns moves by `alpha` towards the weighted mean position its links ask for (from its targets right to left, else from its sources; weights - value times layer distance), then each column is re-sorted (unless `fixed-order?`) and its collisions resolved by `beta`.

  Parameters:

  - `column-cells` (object array): per column a vector of node indices, replaced in place.
  - `direction`: `:right-to-left` or `:left-to-right`.
  - `state` (map): `:links`, `:layers`, `:y0s` / `:y1s` (double arrays, changed in place), `:pad`, `:widths`, `:link-state` (`[outs ins]`, changed in place).
  - `[alpha beta fixed-order?]`: the move fraction, the collision fraction (doubles) and whether columns keep their order.

  Returns `nil`."
  [^objects column-cells direction state [^double alpha ^double beta fixed-order?]]
  (let [{:keys [links layers ^doubles y0s ^doubles y1s ^double pad widths]
         [^objects outs ^objects ins :as link-state] :link-state} state
        n (alength column-cells)
        order (if (= :right-to-left direction) (range (m/- n 2) -1 -1) (range 1 n))]
    (doseq [^long c order]
      (let [column (aget column-cells c)]
        (doseq [^long node column]
          (let [[ls near] (if (= :right-to-left direction) [(aget outs node) :target] [(aget ins node) :source])
                [^double y ^double w] (reduce (fn [[^double y-sum ^double w-sum] l]
                                                (let [{:keys [s t ^double value]} (links l)
                                                      weight (m/* value (m/- (long (layers t)) (long (layers s))))]
                                                  [(m/+ y-sum (m/* weight (level-node-top l near link-state [links y0s widths pad])))
                                                   (m/+ w-sum weight)]))
                                              [0.0 0.0] ls)]
            (when (m/pos? w)
              (shift! y0s y1s node (m/* (m/- (m// y w) (Array/aget y0s node)) alpha))
              (reorder-links! (concat [node] (map #(:s (links %)) (aget ins node)) (map #(:t (links %)) (aget outs node)))
                              link-state links y0s))))
        (let [ordered (if fixed-order? column (vec (sort-by #(Array/aget y0s (long %)) column)))]
          (aset column-cells c ordered)
          (resolve-collisions! ordered [y0s y1s] pad beta))))))

(def ^{:private true :const true :tag 'long} band-steps
  "Steps of a sankey band's curves (see [[band-points]])."
  24)

(defn- band-points
  "Samples a sankey band between two node sides (y up).

  Parameters:

  - `[x0 top0]`, `[x1 top1]` (doubles): the band's top at its source and target sides.
  - `width` (double): the band's thickness; the bottom edge lies `width` below the top.

  Returns `[x y]` points: the top cubic Bezier (control points at the middle x), then the bottom one back, each at [[band-steps]] steps."
  [[^double x0 ^double top0] [^double x1 ^double top1] ^double width]
  (let [xm (m/* 0.5 (m/+ x0 x1))
        curve (fn [^double a ^double b]
                (mapv (fn [^long s] (let [t (m// (double s) band-steps)
                                          u (m/- 1.0 t)]
                                      [(m/+ (m/* u u u x0) (m/* 3.0 u u t xm) (m/* 3.0 u t t xm) (m/* t t t x1))
                                       (m/+ (m/* u u u a) (m/* 3.0 u u t a) (m/* 3.0 u t t b) (m/* t t t b))]))
                      (range (m/long-inc band-steps))))]
    (into (curve top0 top1) (rseq (curve (m/- top0 width) (m/- top1 width))))))

(defn sankey-layout
  "Lays out a sankey diagram of a graph as data in the unit square (a d3-sankey port).

  Edges are directed flows whatever `:directed?`. Nodes stand in columns (left to right) as rectangles `:node-width` wide, their heights proportional to their value (the larger of the flows in and out, one scale for all), at least `:pad` apart in a column; every edge is a band from its source's right side to its target's left side, as thick as its value on that scale, stacked at both nodes in the order of the nodes at their other ends.

  Parameters:

  - `g` (map): a graph of [[graph]].
  - `opts` (map, optional):
    - `:align` - the column of a node: `:justify` (default; the longest path from a source, sinks in the last column), `:left` (sinks where their path ends), `:right` (by the longest path to a sink), `:center`.
    - `:layer` - accessor of the node rows giving the column (integers >= 0, every edge to a later one; wins over `:align`).
    - `:node-width` - node width, a fraction of the square's width (default 0.03).
    - `:pad` - least gap between the nodes of a column, a fraction of the square's height (default 0.02).
    - `:iterations` - relaxation sweeps moving nodes towards their links' ends (default 6; 0 - spread in the start order).
    - `:node-sort` - `nil` (relaxation reorders a column, default) or `:input` (the input order kept).

  Returns `{:nodes [node + :layer :x0 :x1 :y0 :y1 :value] :links [edge + :link :sy :ty :width] :rects [{:rect :node :x :y}] :bands [edge + :link :x :y] :dropped {:edges n :nodes n}}`:
  - nodes in column order, top to bottom; `:rect` - the node's index there, `:link` - the edge's index (group keys of the polygons, whatever the ids);
  - `:sy` / `:ty` - the band's top at the source / target (y up); rect and band rows are polygon points;
  - edges of zero value and self-loops are left out, as are nodes without flow, and counted (`:edges` also counts the graph's `:dropped` - edges with a `nil` end); no flow at all - empty rows.

  Throws `ex-info {:key k}` for an invalid option `k` (checked first, see [[sankey-layout-options]]), `{:key :edges}` for a cycle, `{:key :layer}` for a bad `:layer`, `{:key :pad}` for a `:pad` leaving no room in the fullest column.

  See also [[alluvial-graph]]."
  {:cljplot2/api true :cljplot2/options sankey-layout-options}
  ([g] (sankey-layout g {}))
  ([{:keys [nodes edges loops] :as g} opts]
   (schema/check-options! sankey-layout-options opts)
   (let [{:keys [align layer ^double node-width ^double pad ^long iterations node-sort]}
         (merge {:align :justify :node-width 0.03 :pad 0.02 :iterations 6} opts)
         flows (filterv #(m/pos? (double (:value %))) edges)
         used (set (mapcat (juxt :from :to) flows))
         all-count (count nodes)
         flow-nodes (filterv #(used (:id %)) nodes)
         n (count flow-nodes)
         index (zipmap (map :id flow-nodes) (range))
         links (vec (map-indexed (fn [k e] {:link k :s (index (:from e)) :t (index (:to e)) :value (double (:value e)) :edge e}) flows))
         by-end (fn [k] (let [grouped (group-by k links)] (object-array (map #(mapv :link (get grouped % [])) (range n)))))
         ^objects outs (by-end :s)
         ^objects ins (by-end :t)
         ;; the graph's own drops (edges with a nil end), self-loops, zero flows
         dropped {:edges (m/+ (long (or (:dropped g) 0)) (count loops) (m/- (count edges) (count flows))) :nodes (m/- all-count n)}]
     (if (m/zero? n)
       {:nodes [] :links [] :rects [] :bands [] :dropped dropped}
       (let [values (mapv (fn [^long i] (m/max (v/sum (map #(:value (links %)) (aget outs i)))
                                               (v/sum (map #(:value (links %)) (aget ins i)))))
                          (range n))
             layers (sankey-layers flow-nodes links [(vec outs) (vec ins)] [align (some->> layer (data/row-accessor :layer))])
             column-count (m/long-inc (long (v/mx layers)))
             column-cells (object-array (map (fn [c] (filterv #(= c (layers %)) (range n))) (range column-count)))
             fullest (long (v/mx (map count column-cells)))
             _ (when (m/>= (m/* (m/long-dec fullest) pad) 1.0)
                 (schema/fail! "Sankey :pad leaves no room for the nodes of the fullest column." {:key :pad :value pad}))
             ky (v/mn (map (fn [column] (m// (m/- 1.0 (m/* (m/long-dec (count column)) pad)) (v/sum (map values column))))
                           column-cells))
             y0s (double-array n)
             y1s (double-array n)
             widths (double-array (map (fn [{:keys [^double value]}] (m/* ky value)) links))
             link-state [outs ins]]
         ;; start: each column stacked from the top, the free room spread around its nodes
         (doseq [column column-cells]
           (let [bottom (reduce (fn [^double y ^long i]
                                  (Array/aset y0s i y)
                                  (Array/aset y1s i (m/+ y (m/* ky (double (values i)))))
                                  (m/+ (Array/aget y1s i) pad))
                                0.0 column)
                 spread (m// (m/+ (m/- 1.0 (double bottom)) pad) (m/long-inc (count column)))]
             (doseq [[^long k ^long i] (map-indexed vector column)]
               (shift! y0s y1s i (m/* spread (m/long-inc k))))))
         (reorder-links! (range n) link-state links y0s)
         (let [state {:links links :layers layers :y0s y0s :y1s y1s :pad pad :link-state link-state :widths widths}
               fixed? (= :input node-sort)]
           (dotimes [i iterations]
             (let [alpha (m/pow 0.99 (double i))
                   beta (m/max (m/- 1.0 alpha) (m// (double (m/long-inc i)) iterations))]
               (relax! column-cells :right-to-left state [alpha beta fixed?])
               (relax! column-cells :left-to-right state [alpha beta fixed?]))))
         (reorder-links! (range n) link-state links y0s)
         (let [kx (if (m/> column-count 1) (m// (m/- 1.0 node-width) (m/long-dec column-count)) 0.0)
               x0-of (fn [^long i] (m/* kx (long (layers i))))
               ;; band tops (top-down) at the source and the target
               tops (reduce (fn [acc ^long i]
                              (let [stack (fn [stacked ls k]
                                            (first (reduce (fn [[placed ^double y] l]
                                                             [(assoc-in placed [l k] y) (m/+ y (Array/aget widths (long l)))])
                                                           [stacked (Array/aget y0s i)] ls)))]
                                (-> acc
                                    (stack (aget outs i) :sy)
                                    (stack (aget ins i) :ty))))
                            {} (range n))
               up (fn ^double [^double y] (m/- 1.0 y))
               placed (mapv (fn [{:keys [^long link s t edge]}]
                              (let [w (Array/aget widths link)
                                    sy (up (get-in tops [link :sy]))
                                    ty (up (get-in tops [link :ty]))]
                                (assoc edge :link link :sy sy :ty ty :width w
                                       ::source-x (m/+ (double (x0-of s)) node-width) ::target-x (x0-of t))))
                            links)
               order (vec (mapcat (fn [column] (sort-by #(Array/aget y0s (long %)) column)) column-cells))
               node-of (fn [^long i] (let [x0 (double (x0-of i))]
                                       (assoc (flow-nodes i) :layer (layers i) :x0 x0 :x1 (m/+ x0 node-width)
                                              :y0 (up (Array/aget y1s i)) :y1 (up (Array/aget y0s i)) :value (values i))))
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
  "Orders the distinct levels of an alluvial axis.

  Parameters:

  - `values` (sequence): the axis values of the rows.

  Returns a vector of the distinct values in natural order; first-appearance order when they don't compare."
  [values]
  (let [vs (vec (distinct values))]
    (try (vec (sort vs)) (catch ClassCastException _ vs))))

(def alluvial-graph-entries
  "Option entries of [[alluvial-graph]] (a shared group, `:graph/alluvial-graph`; [[cljplot2.graphs/alluvial]]
  forwards them)."
  (schema/in-fragment
   :graph/alluvial-graph
   [[:axes {:description "accessors of the axes, in column order (at least 2)"} [:sequential {:min 2} schema/accessor]]
    (schema/nilable :value {:description "accessor of a row's weight, a finite number >= 0 (nil - 1)"
                            :cljplot2/default-doc "1 per row"}
                    schema/accessor)
    (schema/nilable :fill {:description "accessor of the level the strands are grouped (and coloured) by"
                           :cljplot2/default-doc "the first axis"}
                    schema/accessor)]))

(def alluvial-graph-options
  "Options schema of [[alluvial-graph]]."
  (into [:map {:closed true}] alluvial-graph-entries))

(defn alluvial-graph
  "Builds the directed graph of an alluvial diagram, for [[sankey-layout]] with `{:layer :layer :node-sort :input}`.

  A node per level of every axis - `{:id [axis-index level] :name level :layer axis-index}`, levels in natural order (first appearance when they don't compare) - and an edge per pair of levels of consecutive axes and `:fill` level, ordered by fill level, so strands of one level lie together at every node.

  Parameters:

  - `rows` (sequence of maps): the data rows.
  - `options` (map):
    - `:axes` - accessors of the axes in column order (at least 2, required).
    - `:value` - accessor of a row's weight, a finite number >= 0, `nil` read as 1 (see [[cljplot2.data.rows/weight]]; default 1 per row).
    - `:fill` - accessor of the level the strands are grouped and coloured by (default the first axis).

  Returns a graph of [[graph]] (directed); every edge `{:from :to :value :fill}` with the summed weight.

  Throws `ex-info {:key k}` for an invalid option `k` (checked first, see [[alluvial-graph-options]]; fewer than 2 axes), `{:key :axes}` for a row without a value of an axis, `{:key :value}` for an invalid weight."
  {:cljplot2/api true :cljplot2/options alluvial-graph-options}
  [rows {:keys [axes value fill] :as options}]
  (schema/check-options! alluvial-graph-options options)
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
        sums (reduce (fn [acc [ls f weight]]
                       (reduce (fn [summed ^long i] (update summed [i (ls i) (ls (m/long-inc i)) f] (fnil m/+ 0.0) weight))
                               acc (range (m/long-dec (count axes)))))
                     {} keyed)
        edges (->> sums
                   (sort-by (fn [[[i _ _ f] _]] [(fill-rank f) i]))
                   (mapv (fn [[[^long i a b f] total]] {:from [i a] :to [(m/long-inc i) b] :value total :fill f})))]
    (graph edges {:nodes nodes :directed? true})))
