(ns cljplot2.data.tree
  "Trees for dendrograms and edge bundling, and their layout as data (drawn by ordinary charts, see
  [[cljplot2.graphs]]).

  A tree node is a group `{:name n :height h :children [..]}` or a leaf `{:name n :height 0 ...}`; three sources:
  [[from-hclust]] (fastmath hierarchical clustering - heights are merge heights), [[from-levels]] (level columns, as
  [[cljplot2.data.hierarchy]]) and [[from-parents]] (rows with an id and a parent id, as d3 `stratify`) - for these two
  a group's height is its distance from the deepest leaf, every leaf 0.

  [[layout]] places the leaves at x = 0, 1, ... in order, a group at the mean x of its children, y = height, and
  returns nodes and one elbow path per branch (child up to its parent's height, then across to the parent); with
  `:circular` x becomes an angle and height a radius. [[bundle-paths]] routes edges between leaves along the tree
  (Holten's hierarchical edge bundling)."
  (:require [fastmath.core :as m]
            [cljplot2.data.hierarchy :as hierarchy]
            [cljplot2.data.rows :as data]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn leaf? [node] (empty? (:children node)))

(defn leaves
  "Leaves of the tree under `node`, in order."
  [node]
  (if (leaf? node) [node] (vec (mapcat leaves (:children node)))))

;; sources

(defn from-hclust
  "Tree of a fastmath `hclust` `result` (its `:tree` - R's `:merge` / `:height` / `:order`): groups `{:height
  :children}`, leaves `{:name :index :height 0}` named by `labels` (a vector of the observations' names, `nil` -
  their indices); children ordered as `:order`. Throws `ex-info {:key :labels}` when `labels` doesn't have one name
  per observation."
  ([result] (from-hclust result nil))
  ([result labels]
   (let [{:keys [merge height order n]} (:tree result)
         n (long n)
         labels (or labels (mapv str (range n)))]
     (when-not (= n (count labels))
       (schema/fail! "One label per observation is needed." {:key :labels :value [(count labels) n]}))
     (let [rank (zipmap order (range))
           leaf (fn [i] {:name (nth labels i) :index i :height 0.0})
           first-rank (fn [node] (rank (:index (first (leaves node)))))
           built (reduce (fn [nodes [j [a b]]]
                           (let [node-of (fn [v] (if (neg? (long v)) (leaf (dec (- (long v)))) (nodes (dec (long v)))))]
                             (conj nodes {:height (double (nth height j))
                                          :children (vec (sort-by first-rank [(node-of a) (node-of b)]))})))
                         [] (map-indexed vector merge))]
       (if (seq built) (peek built) (leaf 0))))))

(defn- with-heights
  "`node` (and its subtree) with `:height` = `max-depth` - depth for groups, 0 for leaves."
  [node ^long depth ^long max-depth]
  (if (leaf? node)
    (assoc node :height 0.0)
    (assoc node :height (double (- max-depth depth))
           :children (mapv #(with-heights % (inc depth) max-depth) (:children node)))))

(defn- depth-of ^long [node] (if (leaf? node) 0 (inc (long (reduce max (map depth-of (:children node)))))))

(defn from-levels
  "Tree of `rows` grouped by the level accessors `:hierarchy` (outer level first; see [[cljplot2.data.hierarchy]] - a
  path ends at the first `nil` / `\"\"`); heights as in the namespace doc. Rows without a level are left out; no rows -
  a root without children."
  [rows {:keys [hierarchy]}]
  (let [accessors (hierarchy/level-accessors hierarchy)
        entries (keep (fn [row] (let [p (hierarchy/path-of accessors row)] (when (seq p) {:path p :value 1.0}))) rows)]
    (if (empty? entries)
      {:name nil :height 0.0 :children []}
      (let [root (hierarchy/root entries)]
        (with-heights root 0 (depth-of root))))))

(defn from-parents
  "Tree of `rows` with an id (`:id` accessor, default `:id`) and a parent id (`:parent`, default `:parent`; the root
  has none), as d3 `stratify`: nodes are the rows with `:id` and `:name` (the `:name` accessor, default the id),
  children in row order; heights as in the namespace doc. Throws `ex-info {:key :parent}` for no root or several
  roots, a parent id that isn't a row's id, a repeated id, or a cycle."
  [rows {:keys [id parent name] :or {id :id parent :parent}}]
  (let [id-of (data/row-accessor :id id)
        parent-of (data/row-accessor :parent parent)
        name-of (if name (data/row-accessor :name name) id-of)
        ids (map id-of rows)
        known (set ids)
        _ (when-not (= (count known) (count rows))
            (schema/fail! "Node ids should be unique." {:key :parent}))
        roots (filter #(nil? (parent-of %)) rows)
        _ (when-not (= 1 (count roots))
            (schema/fail! "A tree needs exactly one root (a row without a parent)." {:key :parent :value (count roots)}))
        _ (when-let [p (some #(let [p (parent-of %)] (when (and (some? p) (not (known p))) p)) rows)]
            (schema/fail! "A parent id isn't a node id." {:key :parent :value p}))
        children-of (group-by parent-of rows)
        build (fn build [row seen]
                (let [k (id-of row)]
                  (when (seen k) (schema/fail! "The parents form a cycle." {:key :parent :value k}))
                  (let [kids (mapv #(build % (conj seen k)) (get children-of k))]
                    (cond-> (assoc row :id k :name (name-of row)) (seq kids) (assoc :children kids)))))
        root (build (first roots) #{})]
    (when-not (= (count rows) (count (tree-seq :children :children root)))
      (schema/fail! "The parents form a cycle (some rows don't reach the root)." {:key :parent}))
    (with-heights root 0 (depth-of root))))

;; layout

(defn- cut-nodes
  "The `k` subtree roots below a cut of the `k - 1` highest groups (from the root, repeatedly splitting the highest
  group of the frontier), in leaf order; throws `ex-info {:key :k}` unless 1 <= `k` <= the number of leaves."
  [root ^long k]
  (let [n (count (leaves root))]
    (when-not (<= 1 k n)
      (schema/fail! "Cluster count :k should be between 1 and the number of leaves." {:key :k :value k}))
    (loop [frontier [root]]
      (if (>= (count frontier) k)
        frontier
        (let [groups (remove leaf? frontier)
              top (apply max-key #(double (:height %)) groups)
              i (.indexOf ^java.util.List frontier top)]
          (recur (into (into (subvec frontier 0 i) (:children top)) (subvec frontier (inc i)))))))))

(defn- placed
  "Nodes of the tree under `node` with `:x` (leaf order position, a group the mean of its children), `:y` (height),
  `:depth`, `:parent` (index or `nil`), `:cluster` (the cluster of a cut subtree root - `cut-of`, by identity - or the
  `inherited` one), in depth-first order: `[nodes next-leaf-x]`."
  [node [parent depth inherited] [nodes ^long next-x] cut-of]
  (let [i (count nodes)
        cluster (or (cut-of node) inherited)]
    (if (leaf? node)
      [(conj nodes (assoc (dissoc node :children) :x (double next-x) :y (double (:height node)) :depth depth
                          :parent parent :leaf? true :cluster cluster))
       (inc next-x)]
      (let [[nodes' x' kids] (reduce (fn [[ns x ks] child]
                                       (let [[ns' x'] (placed child [i (inc (long depth)) cluster] [ns x] cut-of)]
                                         [ns' x' (conj ks (count ns))]))
                                     [(conj nodes nil) next-x []] (:children node))
            kids (map nodes' kids)]
        [(assoc nodes' i (assoc (dissoc node :children) :x (/ (double (reduce + (map :x kids))) (count kids))
                                :y (double (:height node)) :depth depth :parent parent :leaf? false :cluster cluster))
         x']))))

(defn- arc-points
  "Points of the arc at radius `r` from angle `a0` to `a1` (radians, clockwise from the top), every ~2 degrees."
  [^double r ^double a0 ^double a1]
  (let [steps (max 1 (long (m/ceil (/ (m/abs (- a1 a0)) (m/radians 2.0)))))]
    (mapv (fn [^long s] (let [a (+ a0 (* (- a1 a0) (/ (double s) steps)))] [(* r (m/sin a)) (* r (m/cos a))]))
          (range (inc steps)))))

(defn layout
  "Layout of tree `root`: `{:nodes [node ..] :paths [{:id i :x x :y y :cluster c} ..]}` - nodes with `:x`, `:y`,
  `:depth`, `:parent`, `:leaf?`, `:cluster` (and with `:circular` also `:angle` - degrees clockwise from the top - and
  `:radius`); paths as rows, one elbow per non-root node (`:id` - the node's index; the points in order).

  `opts`:

  * `:k` - cluster colours: cutting the `k - 1` highest groups leaves `k` subtrees, clusters 1..k in leaf order; a
    branch inside a cluster takes its number, branches above the cut `nil` (absent - all `nil`)
  * `:circular true` - x as the angle over `:span` degrees (default 360; leaves one step apart, the last one step
    before the first on a full circle), height as the radius from 1 (leaves) to 0 (the root's height); elbows' cross
    pieces become arcs; points in [-1, 1]"
  [root {:keys [k circular span] :or {span 360.0}}]
  (let [clusters (when k (vec (map-indexed (fn [i sub] [sub (inc (long i))]) (cut-nodes root (long k)))))
        cut-of (fn [node] (some (fn [[sub c]] (when (identical? sub node) c)) clusters))
        [nodes n] (placed root [nil 0 nil] [[] 0] cut-of)
        n (long n)
        top (double (:y (first nodes)))
        step (/ (m/radians (double span)) (double (max 1 (if (and circular (< (double span) 360.0)) (dec n) n))))
        angle (fn ^double [^double x] (* x (double step)))
        radius (fn ^double [^double y] (if (pos? top) (- 1.0 (/ y top)) 1.0))
        nodes (if circular
                (mapv (fn [{:keys [x y] :as node}]
                        (let [a (double (angle (double x))) r (double (radius (double y)))]
                          (assoc node :x (* r (m/sin a)) :y (* r (m/cos a)) :angle (m/degrees a) :radius r :leaf-x x :height y)))
                      nodes)
                nodes)
        path (fn [{:keys [parent] :as node}]
               (let [p (nodes parent)
                     same? (and (some? (:cluster node)) (= (:cluster node) (:cluster p)))]
                 {:cluster (when same? (:cluster node))
                  :points (if circular
                            (let [a0 (double (angle (double (:leaf-x node)))) a1 (double (angle (double (:leaf-x p))))
                                  r0 (double (:radius node)) r1 (double (:radius p))]
                              (into [[(* r0 (m/sin a0)) (* r0 (m/cos a0))]] (arc-points r1 a0 a1)))
                            [[(:x node) (:y node)] [(:x node) (:y p)] [(:x p) (:y p)]])}))]
    {:nodes nodes
     :paths (vec (for [[i node] (map-indexed vector nodes)
                       :when (some? (:parent node))
                       :let [{:keys [cluster points]} (path node)]
                       [x y] points]
                   {:id i :x x :y y :cluster cluster}))}))

;; edge bundling

(defn- ancestors-of
  "Indices from node `i` up to the root in `nodes` (with `:parent`)."
  [nodes i]
  (take-while some? (iterate #(:parent (nodes %)) i)))

(defn- tree-route
  "Node indices of the tree path between nodes `a` and `b`: up from `a` to their lowest common ancestor, then down to
  `b` (as d3 `node.path`)."
  [nodes a b]
  (let [up-a (vec (ancestors-of nodes a))
        on-b (set (ancestors-of nodes b))
        lca (first (filter on-b up-a))
        to-lca (vec (take-while #(not= lca %) up-a))
        from-lca (vec (reverse (take-while #(not= lca %) (ancestors-of nodes b))))]
    (-> to-lca (conj lca) (into from-lca))))

(defn- straightened
  "Control `points` pulled towards the straight line from the first to the last: `beta * p + (1 - beta) * line`."
  [points ^double beta]
  (let [n (count points)
        [x0 y0] (first points) [x1 y1] (peek points)]
    (vec (map-indexed (fn [i [x y]]
                        (let [t (if (> n 1) (/ (double i) (double (dec n))) 0.0)
                              lx (+ (double x0) (* t (- (double x1) (double x0))))
                              ly (+ (double y0) (* t (- (double y1) (double y0))))]
                          [(+ (* beta (double x)) (* (- 1.0 beta) lx)) (+ (* beta (double y)) (* (- 1.0 beta) ly))]))
                      points))))

(defn basis-spline
  "Points of the uniform cubic B-spline of control `points` (the ends repeated, so it starts and ends at them, as d3
  `curveBasis`), `steps` points per span; two points - the segment."
  [points ^long steps]
  (if (< (count points) 3)
    (vec points)
    (let [ps (vec (concat [(first points) (first points)] points [(peek points) (peek points)]))]
      (vec (for [i (range (- (count ps) 3))
                 s (range (if (= i (- (count ps) 4)) (inc steps) steps))
                 :let [t (/ (double s) (double steps))
                       t2 (* t t) t3 (* t2 t)
                       b0 (/ (* (- 1.0 t) (- 1.0 t) (- 1.0 t)) 6.0)
                       b1 (/ (+ (* 3.0 t3) (* -6.0 t2) 4.0) 6.0)
                       b2 (/ (+ (* -3.0 t3) (* 3.0 t2) (* 3.0 t) 1.0) 6.0)
                       b3 (/ t3 6.0)
                       [[ax ay] [bx by] [cx cy] [dx dy]] (subvec ps i (+ (long i) 4))]]
             [(+ (* b0 (double ax)) (* b1 (double bx)) (* b2 (double cx)) (* b3 (double dx)))
              (+ (* b0 (double ay)) (* b1 (double by)) (* b2 (double cy)) (* b3 (double dy)))])))))

(defn bundle-paths
  "Edges (rows; `:from` / `:to` accessors, default `:from` / `:to` - leaf ids, `:id` of the nodes, see
  [[from-parents]]) bundled along the laid out tree `laid` (see [[layout]], `:circular`): each a B-spline through the
  positions of the tree path between its leaves, straightened by `:beta` (default 0.85; 1 - the tree path, 0 - a
  straight line). Returns `{:paths [{:id i :x :y :from :to} ..] :dropped n}` - path rows (edge index `:id`, its row's
  keys kept), edges with an end that isn't a leaf (or the same leaf at both ends) dropped and counted. Throws
  `ex-info {:key :beta}` for a `:beta` outside [0, 1]."
  [edges laid {:keys [from to beta] :or {from :from to :to beta 0.85}}]
  (when-not (and (number? beta) (<= 0.0 (double beta) 1.0))
    (schema/fail! "Bundle :beta should be a number in [0, 1]." {:key :beta :value beta}))
  (let [nodes (:nodes laid)
        leaf-index (into {} (keep-indexed (fn [i node] (when (:leaf? node) [(:id node) i])) nodes))
        from-of (data/row-accessor :from from)
        to-of (data/row-accessor :to to)
        routed (keep-indexed (fn [i e]
                               (let [a (leaf-index (from-of e)) b (leaf-index (to-of e))]
                                 (when (and a b (not= a b)) [i e (tree-route nodes a b)])))
                             edges)]
    {:paths (vec (for [[i e route] routed
                       [x y] (basis-spline (straightened (mapv (fn [j] [(:x (nodes j)) (:y (nodes j))]) route) (double beta)) 8)]
                   (assoc e :id i :x x :y y)))
     :dropped (- (count edges) (count routed))}))
