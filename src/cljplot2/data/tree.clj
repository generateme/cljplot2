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
  {:cljplot2/api true}
  [node]
  (if (leaf? node) [node] (vec (mapcat leaves (:children node)))))

;; sources

(defn from-hclust
  "Tree of a fastmath `hclust` `result` (its `:tree` - R's `:merge` / `:height` / `:order`): groups `{:height
  :children}`, leaves `{:name :index :height 0}` named by `labels` (a vector of the observations' names, `nil` -
  their indices); children ordered as `:order`. Throws `ex-info {:key :labels}` when `labels` doesn't have one name
  per observation."
  {:cljplot2/api true}
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

(def from-levels-options
  "Options schema of [[from-levels]]."
  [:map {:closed true}
   (schema/nilable :hierarchy {:description "level accessors, outer level first (none - a root without children)"}
                   [:sequential schema/accessor])])

(defn from-levels
  "Tree of `rows` grouped by the level accessors `:hierarchy` (outer level first; see [[cljplot2.data.hierarchy]] - a
  path ends at the first `nil` / `\"\"`); heights as in the namespace doc. Rows without a level are left out; no rows -
  a root without children. Throws `ex-info {:key k}` for an invalid option `k` (checked first, see
  [[from-levels-options]])."
  {:cljplot2/api true :cljplot2/options from-levels-options}
  [rows {:keys [hierarchy] :as options}]
  (schema/check-options! from-levels-options options)
  (let [accessors (hierarchy/level-accessors hierarchy)
        entries (keep (fn [row] (let [p (hierarchy/path-of accessors row)] (when (seq p) {:path p :value 1.0}))) rows)]
    (if (empty? entries)
      {:name nil :height 0.0 :children []}
      (let [root (hierarchy/root entries)]
        (with-heights root 0 (depth-of root))))))

(def from-parents-options
  "Options schema of [[from-parents]]."
  [:map {:closed true}
   (schema/optional :id {:description "accessor of a row's node id" :default :id} schema/accessor)
   (schema/optional :parent {:description "accessor of a row's parent id (nil - the root)" :default :parent} schema/accessor)
   (schema/nilable :name {:description "accessor of a node's name" :cljplot2/default-doc "the id accessor"} schema/accessor)])

(defn from-parents
  "Tree of `rows` with an id (`:id` accessor, default `:id`) and a parent id (`:parent`, default `:parent`; the root
  has none), as d3 `stratify`: nodes are the rows with `:id` and `:name` (the `:name` accessor, default the id),
  children in row order; heights as in the namespace doc. Throws `ex-info {:key :parent}` for no root or several
  roots, a parent id that isn't a row's id, a repeated id, or a cycle; `{:key k}` for an invalid option `k` (checked
  first, see [[from-parents-options]])."
  {:cljplot2/api true :cljplot2/options from-parents-options}
  [rows {:keys [id parent name] :or {id :id parent :parent} :as options}]
  (schema/check-options! from-parents-options options)
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

(defn arc-points
  "Points of the arc at radius `r` from angle `a0` to `a1` (radians, clockwise from the top), every ~2 degrees."
  [^double r ^double a0 ^double a1]
  (let [steps (max 1 (long (m/ceil (/ (m/abs (- a1 a0)) (m/radians 2.0)))))]
    (mapv (fn [^long s] (let [a (+ a0 (* (- a1 a0) (/ (double s) steps)))] [(* r (m/sin a)) (* r (m/cos a))]))
          (range (inc steps)))))

(def circular-entry
  "Option entry `:circular` of the tree layouts."
  (schema/nilable :circular {:description "radial: breadth as the angle over :span degrees, height (or depth) as the radius; points in [-1, 1]"
                             :default false}
                  schema/bool))

(def span-entry
  "Option entry `:span` of the circular tree layouts."
  (schema/optional :span {:description "degrees a circular layout covers, clockwise from the top" :default 360.0} schema/finite-pos))

(def layout-entries
  "Option entries of [[layout]] (a shared group, `:tree/layout`; [[cljplot2.graphs/dendrogram]] forwards them)."
  (schema/in-fragment
   :tree/layout
   [(schema/nilable :k {:description "cluster count: cutting the k - 1 highest groups leaves k subtrees, clusters 1..k in leaf order (at most the leaf count)"
                        :cljplot2/default-doc "no clusters (every :cluster nil)"}
                    schema/pos-int)
    circular-entry
    span-entry]))

(def layout-options
  "Options schema of [[layout]]."
  (into [:map {:closed true}] layout-entries))

(defn layout
  "Layout of tree `root`: `{:nodes [node ..] :paths [{:id i :x x :y y :cluster c} ..]}` - nodes with `:x`, `:y`,
  `:depth`, `:parent`, `:leaf?`, `:cluster` (and with `:circular` also `:angle` - degrees clockwise from the top - and
  `:radius`); paths as rows, one elbow per non-root node (`:id` - the node's index; the points in order).

  `opts`:

  * `:k` - cluster colours: cutting the `k - 1` highest groups leaves `k` subtrees, clusters 1..k in leaf order; a
    branch inside a cluster takes its number, branches above the cut `nil` (absent - all `nil`)
  * `:circular true` - x as the angle over `:span` degrees (default 360; leaves one step apart, the last one step
    before the first on a full circle), height as the radius from 1 (leaves) to 0 (the root's height); elbows' cross
    pieces become arcs; points in [-1, 1]

  Throws `ex-info {:key :k}` for more clusters than leaves; `{:key k}` for an invalid option `k` (checked first, see
  [[layout-options]])."
  {:cljplot2/api true :cljplot2/options layout-options}
  [root {:keys [k circular span] :or {span 360.0} :as options}]
  (schema/check-options! layout-options options)
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

;; node-link layout (d3 tree / cluster)

(defn- children-indices
  "Child indices of every node of `nodes` (with `:parent` indices, depth-first order), in order."
  [nodes]
  (reduce (fn [acc [i {:keys [parent]}]] (if (some? parent) (update acc parent conj i) acc))
          (vec (repeat (count nodes) [])) (map-indexed vector nodes)))

(defn tidy-breadths
  "Breadth positions of the tidy tree layout of `nodes` (depth-first, `:parent` indices, the root first) - Reingold and
  Tilford's tree drawing in Buchheim, Jünger and Leipert's linear-time version (d3 `tree`): every node above its
  children's middle, subtrees as close as `separation` allows (1 between siblings, 2 between cousins), nodes of one
  depth in order and never overlapping; the root at 0. Returns a vector of doubles (index order)."
  [nodes]
  (let [n (count nodes)
        kids (children-indices nodes)
        parent (mapv #(if-let [p (:parent %)] (long p) -1) nodes)
        number (let [a (long-array n)] (doseq [ks kids [j k] (map-indexed vector ks)] (aset a (long k) (long j))) a)
        prelim (double-array n) modifier (double-array n) change (double-array n) shift (double-array n)
        ancestor (long-array (range n)) thread (long-array n -1)
        leaf? (fn [^long v] (empty? (kids v)))
        left-sibling (fn ^long [^long v] (let [j (aget number v)] (if (or (neg? (long (parent v))) (zero? j)) -1 (long ((kids (parent v)) (dec j))))))
        leftmost-sibling (fn ^long [^long v] (if (neg? (long (parent v))) v (long (first (kids (parent v))))))
        next-left (fn ^long [^long v] (if (leaf? v) (aget thread v) (long (first (kids v)))))
        next-right (fn ^long [^long v] (if (leaf? v) (aget thread v) (long (peek (kids v)))))
        separation (fn ^double [^long a ^long b] (if (== (long (parent a)) (long (parent b))) 1.0 2.0))
        move-subtree! (fn [^long wl ^long wr ^double sh]
                        (let [subtrees (double (- (aget number wr) (aget number wl)))]
                          (aset change wr (- (aget change wr) (/ sh subtrees)))
                          (aset shift wr (+ (aget shift wr) sh))
                          (aset change wl (+ (aget change wl) (/ sh subtrees)))
                          (aset prelim wr (+ (aget prelim wr) sh))
                          (aset modifier wr (+ (aget modifier wr) sh))))
        execute-shifts! (fn [^long v]
                          (loop [ws (rseq (kids v)) sh 0.0 ch 0.0]
                            (when-let [w (first ws)]
                              (let [w (long w)]
                                (aset prelim w (+ (aget prelim w) sh))
                                (aset modifier w (+ (aget modifier w) sh))
                                (let [ch (+ ch (aget change w))]
                                  (recur (next ws) (+ sh (aget shift w) ch) ch))))))
        ancestor-of (fn ^long [^long vil ^long v ^long default]
                      (let [a (aget ancestor vil)] (if (== (long (parent a)) (long (parent v))) a default)))
        apportion (fn ^long [^long v ^long default]
                    (let [w (long (left-sibling v))]
                      (if (neg? w)
                        default
                        (loop [vir v vor v vil w vol (long (leftmost-sibling v))
                               sir (aget modifier v) sor (aget modifier v) sil (aget modifier w) sol (aget modifier (long (leftmost-sibling v)))
                               default default]
                          (let [nr (long (next-right vil)) nl (long (next-left vir))]
                            (if (and (>= nr 0) (>= nl 0))
                              (let [vil nr vir nl vol (long (next-left vol)) vor (long (next-right vor))]
                                (aset ancestor vor v)
                                (let [sh (+ (- (+ (aget prelim vil) sil) (+ (aget prelim vir) sir)) (double (separation vil vir)))
                                      [sir sor] (if (pos? sh)
                                                  (do (move-subtree! (ancestor-of vil v default) v sh) [(+ sir sh) (+ sor sh)])
                                                  [sir sor])]
                                  (recur vir vor vil vol
                                         (+ (double sir) (aget modifier vir)) (+ (double sor) (aget modifier vor))
                                         (+ sil (aget modifier vil)) (+ sol (aget modifier vol)) default)))
                              (do (when (and (>= nr 0) (neg? (long (next-right vor))))
                                    (aset thread vor nr)
                                    (aset modifier vor (+ (aget modifier vor) (- sil sor))))
                                  (if (and (>= nl 0) (neg? (long (next-left vol))))
                                    (do (aset thread vol nl)
                                        (aset modifier vol (+ (aget modifier vol) (- sir sol)))
                                        v)
                                    default))))))))
        first-walk (fn first-walk [^long v]
                     (let [w (long (left-sibling v))]
                       (if (leaf? v)
                         (aset prelim v (if (neg? w) 0.0 (+ (aget prelim w) (double (separation v w)))))
                         (let [ks (kids v)]
                           (reduce (fn [default k] (first-walk (long k)) (apportion (long k) (long default))) (long (first ks)) ks)
                           (execute-shifts! v)
                           (let [mid (* 0.5 (+ (aget prelim (long (first ks))) (aget prelim (long (peek ks)))))]
                             (if (neg? w)
                               (aset prelim v mid)
                               (do (aset prelim v (+ (aget prelim w) (double (separation v w))))
                                   (aset modifier v (- (aget prelim v) mid)))))))))
        xs (double-array n)
        second-walk (fn second-walk [^long v ^double m]
                      (aset xs v (+ (aget prelim v) m))
                      (doseq [k (kids v)] (second-walk (long k) (+ m (aget modifier v)))))]
    (when (pos? n)
      (first-walk 0)
      (second-walk 0 (- (aget prelim 0))))
    (vec xs)))

(defn- curve-points
  "Points of the link from `[s0 b0]` to `[s1 b1]` (depth, breadth): `:line` - the two ends; `:diagonal` - a cubic
  bezier with both control points at the middle depth (d3 `linkHorizontal`), 16 segments."
  [links [s0 b0] [s1 b1]]
  (if (= :line links)
    [[s0 b0] [s1 b1]]
    (let [s0 (double s0) b0 (double b0) s1 (double s1) b1 (double b1) mid (* 0.5 (+ s0 s1))]
      (mapv (fn [^long i]
              (let [t (/ (double i) 16.0) u (- 1.0 t)
                    a (* u u u) b (* 3.0 u u t) c (* 3.0 u t t) d (* t t t)]
                [(+ (* a s0) (* b mid) (* c mid) (* d s1)) (+ (* a b0) (* b b0) (* c b1) (* d b1))]))
            (range 17)))))

(def node-link-layout-entries
  "Option entries of [[node-link-layout]] (a shared group, `:tree/node-link-layout`; [[cljplot2.graphs/tree]] forwards
  them)."
  (schema/in-fragment
   :tree/node-link-layout
   [(schema/optional :layout {:description "node placement: :tidy (every node at its depth, d3 tree) or :cluster (leaves at the deepest depth, d3 cluster)"
                              :default :tidy}
                     [:enum :tidy :cluster])
    (schema/optional :links {:description "link shape: :diagonal (curves along the depth axis) or :line" :default :diagonal}
                     [:enum :diagonal :line])
    circular-entry
    span-entry]))

(def node-link-layout-options
  "Options schema of [[node-link-layout]]."
  (into [:map {:closed true}] node-link-layout-entries))

(defn node-link-layout
  "Node-link layout of tree `root` (d3 `tree` / `cluster`, Vega `tree`): `{:nodes [node ..] :links [{:id i :x x :y y}
  ..]}` - nodes in depth-first order with `:depth`, `:parent` (index), `:leaf?`, `:breadth` and `:x` / `:y`; one link
  path per non-root node (`:id` - the node's index; points from the parent to the node).

  `opts`:

  * `:layout` - `:tidy` (default; [[tidy-breadths]], every node at its depth) or `:cluster` (leaves one step apart in
    order and all at the deepest depth, a group above its children's middle - as [[layout]])
  * `:links` - `:diagonal` (default; curves leaving and entering along the depth axis) or `:line`
  * `:circular` - breadth as the angle over `:span` degrees (default 360; on a full circle the breadth range plus one
    step, so the ends don't meet), depth as the radius from 0 (the root) to 1 (the deepest node); nodes also get
    `:angle` (degrees clockwise from the top) and `:radius`; diagonal links are curves in (angle, radius) - d3
    `linkRadial`; points in [-1, 1]
  * else depth along x from 0 (the root) and breadth along y (the first node of a depth on top: y = -breadth)

  Throws `ex-info {:key k}` for an invalid option `k` (checked first, see [[node-link-layout-options]])."
  {:cljplot2/api true :cljplot2/options node-link-layout-options}
  [root {:keys [layout links circular span] :or {layout :tidy links :diagonal span 360.0} :as options}]
  (schema/check-options! node-link-layout-options options)
  (let [[placed-nodes] (placed root [nil 0 nil] [[] 0] (constantly nil))
        breadths (if (= :cluster layout) (mapv :x placed-nodes) (tidy-breadths placed-nodes))
        max-depth (double (reduce max 1 (map :depth placed-nodes)))
        depth-of (fn ^double [node] (if (and (= :cluster layout) (:leaf? node)) max-depth (double (:depth node))))
        lo (double (reduce min breadths)) hi (double (reduce max breadths))
        nodes (mapv (fn [node b] (assoc node :breadth (- (double b) lo) :depth-pos (depth-of node))) placed-nodes breadths)
        span-b (- hi lo)
        step (/ (m/radians (double span)) (if (>= (double span) 360.0) (+ span-b 1.0) (max span-b 1.0)))
        polar (fn [[d b]] (let [a (* (double b) step) r (/ (double d) max-depth)] [(* r (m/sin a)) (* r (m/cos a))]))
        place (fn [[d b]] (if circular (polar [d b]) [(double d) (- (double b))]))
        nodes (mapv (fn [{:keys [breadth depth-pos] :as node}]
                      (let [[x y] (place [depth-pos breadth])]
                        (cond-> (assoc node :x x :y y)
                          circular (assoc :angle (m/degrees (* (double breadth) step)) :radius (/ (double depth-pos) max-depth)))))
                    nodes)]
    {:nodes nodes
     :links (vec (for [[i {:keys [parent] :as node}] (map-indexed vector nodes)
                       :when (some? parent)
                       :let [p (nodes parent)]
                       pt (curve-points links [(:depth-pos p) (:breadth p)] [(:depth-pos node) (:breadth node)])
                       :let [[x y] (place pt)]]
                   {:id i :x x :y y}))}))

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

(def bundle-paths-entries
  "Option entries of [[bundle-paths]] (a shared group, `:tree/bundle-paths`; [[cljplot2.graphs/bundle]] forwards
  them)."
  (schema/in-fragment
   :tree/bundle-paths
   [(schema/optional :from {:description "accessor of an edge row's source leaf id" :default :from} schema/accessor)
    (schema/optional :to {:description "accessor of an edge row's target leaf id" :default :to} schema/accessor)
    (schema/optional :beta {:description "bundling strength: 1 - along the tree path, 0 - a straight line" :default 0.85}
                     schema/fraction)]))

(def bundle-paths-options
  "Options schema of [[bundle-paths]]."
  (into [:map {:closed true}] bundle-paths-entries))

(defn leaf-key
  "The value edges name `node` (a leaf) by: its `:id` ([[from-parents]] trees), else its `:name` ([[from-levels]],
  [[from-hclust]] leaves have no id). Used by edge bundling ([[bundle-paths]], `plot/bundle`); not part of the
  documented API (no `:cljplot2/api`)."
  [node]
  (if (contains? node :id) (:id node) (:name node)))

(defn bundle-paths
  "Edges (rows; `:from` / `:to` accessors, default `:from` / `:to` - leaf keys: the `:id` of a [[from-parents]] node,
  else the leaf's `:name`; with repeated names the first leaf is used) bundled along the laid out
  tree `laid` (see [[layout]], `:circular`): each a B-spline through the positions of the tree path between its
  leaves, straightened by `:beta` (default 0.85; 1 - the tree path, 0 - a straight line). Returns `{:paths [{:id i :x
  :y :from :to} ..] :dropped n}` - path rows (edge index `:id`, its row's keys kept), edges with an end that isn't a
  leaf (or the same leaf at both ends) dropped and counted. Throws `ex-info {:key k}` for an invalid option `k`
  (checked first, see [[bundle-paths-options]])."
  {:cljplot2/api true :cljplot2/options bundle-paths-options}
  [edges laid {:keys [from to beta] :or {from :from to :to beta 0.85} :as options}]
  (schema/check-options! bundle-paths-options options)
  (let [nodes (:nodes laid)
        leaf-index (reduce (fn [acc [i node]] (cond-> acc (and (:leaf? node) (not (contains? acc (leaf-key node))))
                                                (assoc (leaf-key node) i)))
                           {} (map-indexed vector nodes))
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
