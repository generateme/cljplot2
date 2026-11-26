(ns cljplot2.data.tree
  "Trees for dendrograms, node-link trees and edge bundling, and their layouts as data rows drawn by ordinary charts (see [[cljplot2.graphs]]).

  A tree node is a group `{:name n :height h :children [..]}` or a leaf `{:name n :height 0 ...}`. Trees built from fastmath clustering keep its merge heights; trees built from levels or parent ids give a group its distance from the deepest leaf, every leaf 0.

  Functions:

  - sources: [[from-hclust]] (fastmath hierarchical clustering), [[from-levels]] (level columns, as [[cljplot2.data.hierarchy]]), [[from-parents]] (rows with an id and a parent id, as d3 `stratify`); [[leaves]], `leaf?`.
  - dendrogram layout: [[layout]] - leaves at x = 0, 1, ... in order, a group at the mean x of its children, y = height, one elbow path per branch; `:circular` turns x into an angle and height into a radius ([[arc-points]] draws the arcs).
  - node-link layout: [[node-link-layout]] (d3 `tree` / `cluster`) with `tidy-breadths` (Reingold-Tilford in Buchheim's linear-time version).
  - edge bundling: [[bundle-paths]] - edges between leaves routed along the tree (Holten's hierarchical edge bundling), smoothed by `basis-spline`; [[leaf-key]] names a leaf for edges.
  - option schemas: [[from-levels-options]], [[from-parents-options]], [[layout-options]], [[node-link-layout-options]], [[bundle-paths-options]] and their shared entries."
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.data.hierarchy :as hierarchy]
            [cljplot2.data.rows :as data]
            [cljplot2.schema :as schema])
  (:import [fastmath.java Array]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- leaf?
  "Tests whether a tree node is a leaf.

  Parameters:

  - `node` (map): a tree node.

  Returns true when `node` has no `:children` (absent or empty).

  See also [[leaves]]."
  [node]
  (empty? (:children node)))

(defn leaves
  "Collects the leaves of a tree.

  Parameters:

  - `node` (map): the root of the (sub)tree.

  Returns a vector of the leaves under `node` in order; a leaf gives itself.

  See also `leaf?`."
  {:cljplot2/api true
   :cljplot2/arguments [{:name 'node :description "A tree node, usually the root."}]
   :cljplot2/returns "The leaves under the node in order; a leaf returns itself."}
  [node]
  (if (leaf? node) [node] (vec (mapcat leaves (:children node)))))

;; sources

(defn from-hclust
  "Builds the tree of a fastmath hierarchical clustering result.

  Groups are the clustering's merges at their merge heights, leaves its observations; children are ordered as the clustering's leaf order.

  Parameters:

  - `result` (map): a fastmath `hclust` result; its `:tree` holds R's `:merge`, `:height`, `:order` and the observation count `:n`.
  - `leaf-names` (vector, optional): the observations' names, one per observation; `nil` (default) - their indices as text.

  Returns the root node: groups `{:height h :children [a b]}`, leaves `{:name n :index i :height 0.0}`; a single observation - its leaf.

  Throws `ex-info {:key :leaf-names}` when `leaf-names` doesn't have one name per observation.

  See also [[from-levels]], [[from-parents]], [[layout]]."
  {:cljplot2/api true
   :cljplot2/arguments [{:name 'result :description "A fastmath `hclust` result."}
                         {:name 'leaf-names :optional true :description "The observations' names, one per observation. Without them the leaves are named by their indices."}]
   :cljplot2/returns "The root node: groups `{:height :children}` and leaves `{:name :index :height}`."}
  ([result] (from-hclust result nil))
  ([result leaf-names]
   (let [{:keys [height order ^long n] merges :merge} (:tree result)
         names (or leaf-names (mapv str (range n)))]
     (when-not (m/== n (count names))
       (schema/fail! "One leaf name per observation is needed." {:key :leaf-names :value [(count names) n]}))
     (let [rank (zipmap order (range))
           leaf (fn [i] {:name (nth names i) :index i :height 0.0})
           first-rank (fn [node] (rank (:index (first (leaves node)))))
           built (reduce (fn [nodes [j [a b]]]
                           (let [node-of (fn [^long id] (if (m/neg? id) (leaf (m/long-dec (m/- id))) (nodes (m/long-dec id))))]
                             (conj nodes {:height (double (nth height j))
                                          :children (vec (sort-by first-rank [(node-of a) (node-of b)]))})))
                         [] (map-indexed vector merges))]
       (if (seq built) (peek built) (leaf 0))))))

(defn- with-heights
  "Sets the heights of a subtree by depth.

  Parameters:

  - `node` (map): the subtree's root.
  - `depth` (long): the depth of `node` (the root 0).
  - `max-depth` (long): the depth of the deepest leaf of the whole tree.

  Returns `node` with every group's `:height` `max-depth - depth` and every leaf's 0.0.

  See also [[depth-of]]."
  [node ^long depth ^long max-depth]
  (if (leaf? node)
    (assoc node :height 0.0)
    (assoc node :height (double (m/- max-depth depth))
           :children (mapv #(with-heights % (m/long-inc depth) max-depth) (:children node)))))

(defn- depth-of
  "Measures the depth of a tree.

  Parameters:

  - `node` (map): the root.

  Returns the number of levels below `node` to its deepest leaf (a leaf 0)."
  ^long [node]
  (if (leaf? node) 0 (m/long-inc (long (v/mx (map depth-of (:children node)))))))

(def from-levels-options
  "Options schema of [[from-levels]]."
  [:map {:closed true}
   (schema/nilable :hierarchy {:description "The accessors of the level columns, outer level first."
                               :cljplot2/default-doc "No levels: a root without children."
                               :cljplot2/example [:region :country]}
                   [:sequential schema/accessor])])

(defn from-levels
  "Builds the tree of rows grouped by level columns.

  A row's path is its level values up to the first `nil` or empty text (see [[cljplot2.data.hierarchy]]); a group's height is its distance from the deepest leaf, every leaf 0.

  Parameters:

  - `rows`: rows or columns (see [[cljplot2.data.rows/rows]]).
  - `options` (map, optional): `:hierarchy` - level accessors, outer level first (none - a root without children).

  Returns the root group `{:name nil :value n :height h :children [..]}` (also over a single top-level group): every other node's `:name` is its level value and `:value` the number of rows under it; a leaf is the end of a row path - one per distinct path, not per row - `{:name v :value n :color nil :height 0.0}`. A path cut short by a missing level ends at its last level's node when that node has no other children, else in a leaf `{:name nil ...}` under it. Rows without a first level are left out; no such rows - `{:name nil :height 0.0 :children []}` (no `:value`).

  Throws `ex-info {:key k}` for an invalid option `k` (checked first, see [[from-levels-options]]). Data that are neither rows (a sequence of maps) nor columns throw `{:key :data}` (see [[cljplot2.data.rows/rows]]).

  See also [[from-parents]], [[from-hclust]], [[layout]]."
  {:cljplot2/api true
   :cljplot2/arguments [{:name 'rows :description "The data, a level column for each accessor of `:hierarchy`: rows (a sequence of maps) or columns (a map of equal-length sequences)."}]
   :cljplot2/returns "The root group: every node with its level value under `:name` and its number of rows under `:value`, a leaf per distinct level path."
   :cljplot2/options from-levels-options}
  ([rows] (from-levels rows {}))
  ([rows {:keys [hierarchy] :as options}]
   (schema/check-options! from-levels-options options)
   (let [accessors (hierarchy/level-accessors hierarchy)
         entries (keep (fn [row] (let [p (hierarchy/path-of accessors row)] (when (seq p) {:path p :value 1.0}))) (data/rows rows))]
     (if (empty? entries)
       {:name nil :height 0.0 :children []}
       (let [root (hierarchy/root entries)]
         (with-heights root 0 (depth-of root)))))))

(def from-parents-options
  "Options schema of [[from-parents]]."
  [:map {:closed true}
   (schema/optional :id {:description "The accessor of a row's node id." :default :id :cljplot2/example :key} schema/accessor)
   (schema/optional :parent {:description "The accessor of a row's parent id; the root's is `nil`." :default :parent :cljplot2/example :up}
                    schema/accessor)
   (schema/nilable :name {:description "The accessor of a node's name." :cljplot2/default-doc "The id accessor." :cljplot2/example :label}
                   schema/accessor)])

(defn from-parents
  "Builds the tree of rows linked by parent ids, as d3 `stratify`.

  A group's height is its distance from the deepest leaf, every leaf 0.

  Parameters:

  - `rows`: rows or columns (see [[cljplot2.data.rows/rows]]), one row per node; the root has no parent id.
  - `options` (map, optional):
    - `:id` - accessor of a row's node id (default `:id`).
    - `:parent` - accessor of a row's parent id (default `:parent`).
    - `:name` - accessor of a node's name (default the id accessor).

  Returns the root node: each node its row with `:id`, `:name`, `:height` and `:children` (in row order, absent for a leaf).

  Throws `ex-info {:key :parent}` for no root or several roots, a parent id that isn't a row's id, a repeated id, or a cycle; `{:key k}` for an invalid option `k` (checked first, see [[from-parents-options]]). Data that are neither rows (a sequence of maps) nor columns throw `{:key :data}` (see [[cljplot2.data.rows/rows]]).

  See also [[from-levels]], [[from-hclust]], [[leaf-key]]."
  {:cljplot2/api true
   :cljplot2/arguments [{:name 'rows :description "The nodes, a row each: rows (a sequence of maps) or columns (a map of equal-length sequences)." :columns {:id "The node's id; the option `:id` names another column." :parent "The parent's id, `nil` for the root; the option `:parent` names another column."}}]
   :cljplot2/returns "The root node: each node is its row with `:id`, `:name` and `:height`, a group also with its `:children` in row order."
   :cljplot2/options from-parents-options}
  ([rows] (from-parents rows {}))
  ([rows {:keys [id parent] name-accessor :name :or {id :id parent :parent} :as options}]
   (schema/check-options! from-parents-options options)
   (let [rs (data/rows rows)
         id-of (data/row-accessor :id id)
         parent-of (data/row-accessor :parent parent)
         name-of (if name-accessor (data/row-accessor :name name-accessor) id-of)
         ids (map id-of rs)
         known (set ids)
         _ (when-not (= (count known) (count rs))
             (schema/fail! "Node ids should be unique." {:key :parent}))
         roots (filter #(nil? (parent-of %)) rs)
         _ (when-not (= 1 (count roots))
             (schema/fail! "A tree needs exactly one root (a row without a parent)." {:key :parent :value (count roots)}))
         _ (when-let [p (some #(let [p (parent-of %)] (when (and (some? p) (not (known p))) p)) rs)]
             (schema/fail! "A parent id isn't a node id." {:key :parent :value p}))
         children-of (group-by parent-of rs)
         build (fn build [row seen]
                 (let [k (id-of row)]
                   (when (seen k) (schema/fail! "The parents form a cycle." {:key :parent :value k}))
                   (let [kids (mapv #(build % (conj seen k)) (get children-of k))]
                     (cond-> (assoc row :id k :name (name-of row)) (seq kids) (assoc :children kids)))))
         root (build (first roots) #{})]
     (when-not (= (count rs) (count (tree-seq :children :children root)))
       (schema/fail! "The parents form a cycle (some rows don't reach the root)." {:key :parent}))
     (with-heights root 0 (depth-of root)))))

;; layout

(defn- cut-nodes
  "Cuts a tree into clusters below its highest groups.

  Starting from the root, the highest group of the frontier is split into its children until the frontier holds `k` nodes.

  Parameters:

  - `root` (map): the tree's root.
  - `k` (long): the cluster count.

  Returns the vector of the `k` subtree roots in leaf order.

  Throws `ex-info {:key :k}` unless 1 <= `k` <= the number of leaves."
  [root ^long k]
  (let [n (count (leaves root))]
    (when-not (m/<= 1 k n)
      (schema/fail! "Cluster count :k should be between 1 and the number of leaves." {:key :k :value k}))
    (loop [frontier [root]]
      (if (m/>= (count frontier) k)
        frontier
        (let [groups (remove leaf? frontier)
              top (apply max-key #(double (:height %)) groups)
              i (.indexOf ^java.util.List frontier top)]
          (recur (into (into (subvec frontier 0 i) (:children top)) (subvec frontier (m/long-inc i)))))))))

(defn- placed
  "Places the nodes of a subtree in depth-first order, appending them to the nodes placed so far.

  Parameters:

  - `node` (map): the subtree's root.
  - `[parent depth inherited]`: the index of `node`'s parent (`nil` for the root), its depth, and the cluster it inherits.
  - `[nodes next-x]`: the nodes placed so far and the x of the next leaf.
  - `cut-of` (function): of a node, its cluster number when it is a cut subtree root (by identity), else `nil`.

  Returns `[nodes next-x]` - the nodes with this subtree's appended and the x after its last leaf. Each node gets `:x` (a leaf its position in leaf order, a group the mean of its children), `:y` (its height), `:depth`, `:parent`, `:leaf` and `:cluster` (its own cut cluster, else the inherited one); `:children` is removed."
  [node [parent depth inherited] [nodes ^long next-x] cut-of]
  (let [i (count nodes)
        cluster (or (cut-of node) inherited)]
    (if (leaf? node)
      [(conj nodes (assoc (dissoc node :children) :x (double next-x) :y (double (:height node)) :depth depth
                          :parent parent :leaf true :cluster cluster))
       (m/long-inc next-x)]
      (let [[nodes' x' kid-indices] (reduce (fn [[placed-so-far x ks] child]
                                              (let [[placed' x'] (placed child [i (m/long-inc depth) cluster] [placed-so-far x] cut-of)]
                                                [placed' x' (conj ks (count placed-so-far))]))
                                            [(conj nodes nil) next-x []] (:children node))
            kids (map nodes' kid-indices)]
        [(assoc nodes' i (assoc (dissoc node :children) :x (m// (v/sum (map :x kids)) (double (count kids)))
                                :y (double (:height node)) :depth depth :parent parent :leaf false :cluster cluster))
         x']))))

(def ^{:const true :private true :tag 'double} arc-step
  "Largest angle (radians) between consecutive points of an arc: 2 degrees."
  (m/radians 2.0))

(defn arc-points
  "Samples an arc around the origin.

  Parameters:

  - `r` (double): the radius.
  - `a0`, `a1` (doubles): the start and end angles in radians, clockwise from the top.

  Returns a vector of `[x y]` points from `a0` to `a1` at most 2 degrees apart (y up); equal angles - the point twice.

  See also [[layout]]."
  [^double r ^double a0 ^double a1]
  (let [steps (m/max 1 (long (m/ceil (m// (m/abs (m/- a1 a0)) arc-step))))]
    (mapv (fn [^double a] [(m/* r (m/sin a)) (m/* r (m/cos a))])
          (m/slice-range a0 a1 (m/long-inc steps)))))

(def ^:private circular-entry
  "Option entry `:circular` of the tree layouts."
  (schema/nilable :circular {:description "Whether the layout is radial."
                             :default false
                             :cljplot2/value-meanings {true "Radial: the breadth as the angle over `:span` degrees, the height or depth as the radius; points in [-1, 1]."
                                                       false "Along the axes."}}
                  schema/bool))

(def ^:private span-entry
  "Option entry `:span` of the circular tree layouts."
  (schema/optional :span {:description "The degrees a circular layout covers, clockwise from the top." :default 360.0
                          :cljplot2/example 180 :cljplot2/example-context {:circular true}}
                   schema/finite-pos))

(def layout-entries
  "Option entries of [[layout]] (a shared group, `:tree/layout`; [[cljplot2.graphs/dendrogram]] forwards them)."
  (schema/in-fragment
   :tree/layout
   [(schema/nilable :k {:description "The number of clusters, at most the leaf count: cutting the k - 1 highest groups leaves k subtrees, numbered 1 to k in leaf order under `:cluster`."
                        :cljplot2/default-doc "No clusters: every `:cluster` is `nil`."
                        :cljplot2/example 2}
                    schema/pos-int)
    circular-entry
    span-entry]))

(def layout-options
  "Options schema of [[layout]]."
  (into [:map {:closed true}] layout-entries))

(defn layout
  "Lays out a tree as a dendrogram.

  Leaves sit at x = 0, 1, ... in leaf order, a group at the mean x of its children, y its height; each non-root node gets an elbow path up to its parent's height, then across to the parent.

  Parameters:

  - `root` (map): the tree's root (see [[from-hclust]], [[from-levels]], [[from-parents]]).
  - `options` (map, optional):
    - `:k` - cluster count: cutting the `k - 1` highest groups leaves `k` subtrees, clusters 1..k in leaf order; a branch inside a cluster takes its number, branches above the cut `nil` (absent - all `nil`).
    - `:circular` - `true`: x as the angle over `:span` degrees (leaves one step apart, the last one step before the first on a full circle), height as the radius from 1 (leaves) to 0 (the root's height); elbows' cross pieces become arcs; points in [-1, 1] (default false).
    - `:span` - degrees a circular layout covers, clockwise from the top (default 360).

  Returns `{:nodes [node ..] :paths [{:id i :x x :y y :cluster c} ..]}` - nodes in depth-first order with `:x`, `:y`, `:depth`, `:parent` (index), `:leaf`, `:cluster` (circular: also `:angle` in degrees clockwise from the top, `:radius`, `:leaf-x`, `:height`); path rows in point order, `:id` the node's index, `:cluster` set when the node and its parent share it.

  Throws `ex-info {:key :k}` for more clusters than leaves; `{:key k}` for an invalid option `k` (checked first, see [[layout-options]]).

  See also [[node-link-layout]], [[bundle-paths]], [[arc-points]]."
  {:cljplot2/api true
   :cljplot2/arguments [{:name 'root :description "The root of a tree of `tree/from-hclust`, `tree/from-levels` or `tree/from-parents`."}]
   :cljplot2/returns "`{:nodes :paths}`: the nodes with their positions and clusters, and an elbow path per branch as rows `{:id :x :y :cluster}`."
   :cljplot2/options layout-options}
  ([root] (layout root {}))
  ([root {:keys [k circular ^double span] :or {span 360.0} :as options}]
   (schema/check-options! layout-options options)
   (let [clusters (when k (vec (map-indexed (fn [i sub] [sub (m/long-inc i)]) (cut-nodes root (long k)))))
         cut-of (fn [node] (some (fn [[sub c]] (when (identical? sub node) c)) clusters))
         [placed-nodes ^long leaf-count] (placed root [nil 0 nil] [[] 0] cut-of)
         top (double (:y (first placed-nodes)))
         leaf-steps (if (and circular (m/< span 360.0)) (m/long-dec leaf-count) leaf-count)
         step (m// (m/radians span) (double (m/max 1 leaf-steps)))
         angle (fn ^double [^double x] (m/* x step))
         radius (fn ^double [^double y] (if (m/pos? top) (m/- 1.0 (m// y top)) 1.0))
         nodes (if circular
                 (mapv (fn [{:keys [x y] :as node}]
                         (let [a (double (angle x))
                               r (double (radius y))]
                           (assoc node :x (m/* r (m/sin a)) :y (m/* r (m/cos a)) :angle (m/degrees a) :radius r :leaf-x x :height y)))
                       placed-nodes)
                 placed-nodes)
         path (fn [{:keys [parent] :as node}]
                (let [p (nodes parent)
                      same? (and (some? (:cluster node)) (= (:cluster node) (:cluster p)))]
                  {:cluster (when same? (:cluster node))
                   :points (if circular
                             (let [a0 (double (angle (:leaf-x node)))
                                   a1 (double (angle (:leaf-x p)))
                                   r0 (double (:radius node))
                                   r1 (double (:radius p))]
                               (into [[(m/* r0 (m/sin a0)) (m/* r0 (m/cos a0))]] (arc-points r1 a0 a1)))
                             [[(:x node) (:y node)] [(:x node) (:y p)] [(:x p) (:y p)]])}))]
     {:nodes nodes
      :paths (vec (for [[i node] (map-indexed vector nodes)
                        :when (some? (:parent node))
                        :let [{:keys [cluster points]} (path node)]
                        [x y] points]
                    {:id i :x x :y y :cluster cluster}))})))

;; node-link layout (d3 tree / cluster)

(defn- children-indices
  "Lists the children of every node by index.

  Parameters:

  - `nodes` (vector of maps): nodes in depth-first order, each with its `:parent` index (`nil` for the root).

  Returns a vector, per node index, of its children's indices in order (empty for a leaf)."
  [nodes]
  (reduce (fn [acc [i {:keys [parent]}]] (if (some? parent) (update acc parent conj i) acc))
          (vec (repeat (count nodes) [])) (map-indexed vector nodes)))

(defn- tidy-breadths
  "Computes the breadth positions of a tidy tree layout (d3 `tree`).

  Reingold and Tilford's tree drawing in Buchheim, Jünger and Leipert's linear-time version: every node above its children's middle, subtrees as close as the separation allows (1 between siblings, 2 between cousins), the nodes of one depth in order and never overlapping.

  Parameters:

  - `nodes` (vector of maps): nodes in depth-first order with `:parent` indices, the root first.

  Returns a vector of doubles, one breadth per node (index order), the root at 0; no nodes - an empty vector.

  See also [[node-link-layout]]."
  [nodes]
  (let [n (count nodes)
        kids (children-indices nodes)
        parent (long-array (map #(or (:parent %) -1) nodes))
        number (let [a (long-array n)]
                 (doseq [ks kids
                         [j k] (map-indexed vector ks)]
                   (Array/aset a (long k) (long j)))
                 a)
        prelim (double-array n)
        modifier (double-array n)
        change (double-array n)
        shift (double-array n)
        ancestor (long-array (range n))
        thread (long-array n -1)
        leaf-index? (fn [^long v] (empty? (kids v)))
        left-sibling (fn ^long [^long v]
                       (let [j (Array/aget number v)
                             p (Array/aget parent v)]
                         (if (or (m/neg? p) (m/zero? j)) -1 (long ((kids p) (m/long-dec j))))))
        leftmost-sibling (fn ^long [^long v]
                           (let [p (Array/aget parent v)]
                             (if (m/neg? p) v (long (first (kids p))))))
        next-left (fn ^long [^long v] (if (leaf-index? v) (Array/aget thread v) (long (first (kids v)))))
        next-right (fn ^long [^long v] (if (leaf-index? v) (Array/aget thread v) (long (peek (kids v)))))
        separation (fn ^double [^long a ^long b] (if (m/== (Array/aget parent a) (Array/aget parent b)) 1.0 2.0))
        move-subtree! (fn [^long wl ^long wr ^double sh]
                        (let [subtrees (double (m/- (Array/aget number wr) (Array/aget number wl)))
                              share (m// sh subtrees)]
                          (Array/add change wr (m/- share))
                          (Array/add shift wr sh)
                          (Array/add change wl share)
                          (Array/add prelim wr sh)
                          (Array/add modifier wr sh)))
        execute-shifts! (fn [^long v]
                          (loop [ws (rseq (kids v))
                                 sh 0.0
                                 ch 0.0]
                            (when-let [child (first ws)]
                              (let [w (long child)
                                    change-sum (m/+ ch (Array/aget change w))]
                                (Array/add prelim w sh)
                                (Array/add modifier w sh)
                                (recur (next ws) (m/+ sh (Array/aget shift w) change-sum) change-sum)))))
        ancestor-of (fn ^long [^long vil ^long v ^long default-ancestor]
                      (let [a (Array/aget ancestor vil)]
                        (if (m/== (Array/aget parent a) (Array/aget parent v)) a default-ancestor)))
        apportion (fn ^long [^long v ^long default-ancestor]
                    (let [w (long (left-sibling v))]
                      (if (m/neg? w)
                        default-ancestor
                        (let [leftmost (long (leftmost-sibling v))]
                          (loop [vir v
                                 vor v
                                 vil w
                                 vol leftmost
                                 sir (Array/aget modifier v)
                                 sor (Array/aget modifier v)
                                 sil (Array/aget modifier w)
                                 sol (Array/aget modifier leftmost)]
                            (let [right (long (next-right vil))
                                  left (long (next-left vir))]
                              (if (and (m/>= right 0) (m/>= left 0))
                                (let [vol-next (long (next-left vol))
                                      vor-next (long (next-right vor))
                                      _ (Array/aset ancestor vor-next v)
                                      sh (m/+ (m/- (m/+ (Array/aget prelim right) sil) (m/+ (Array/aget prelim left) sir))
                                              (double (separation right left)))
                                      moved? (m/pos? sh)
                                      _ (when moved? (move-subtree! (ancestor-of right v default-ancestor) v sh))
                                      sir-moved (if moved? (m/+ sir sh) sir)
                                      sor-moved (if moved? (m/+ sor sh) sor)]
                                  (recur left vor-next right vol-next
                                         (m/+ sir-moved (Array/aget modifier left)) (m/+ sor-moved (Array/aget modifier vor-next))
                                         (m/+ sil (Array/aget modifier right)) (m/+ sol (Array/aget modifier vol-next))))
                                (do (when (and (m/>= right 0) (m/neg? (long (next-right vor))))
                                      (Array/aset thread vor right)
                                      (Array/add modifier vor (m/- sil sor)))
                                    (if (and (m/>= left 0) (m/neg? (long (next-left vol))))
                                      (do (Array/aset thread vol left)
                                          (Array/add modifier vol (m/- sir sol))
                                          v)
                                      default-ancestor)))))))))
        first-walk (fn first-walk [^long v]
                     (let [w (long (left-sibling v))]
                       (if (leaf-index? v)
                         (Array/aset prelim v (if (m/neg? w) 0.0 (m/+ (Array/aget prelim w) (double (separation v w)))))
                         (let [ks (kids v)
                               first-kid (long (first ks))
                               last-kid (long (peek ks))]
                           (reduce (fn [default-ancestor k] (first-walk (long k)) (apportion (long k) (long default-ancestor))) first-kid ks)
                           (execute-shifts! v)
                           (let [mid (m/* 0.5 (m/+ (Array/aget prelim first-kid) (Array/aget prelim last-kid)))]
                             (if (m/neg? w)
                               (Array/aset prelim v mid)
                               (do (Array/aset prelim v (m/+ (Array/aget prelim w) (double (separation v w))))
                                   (Array/aset modifier v (m/- (Array/aget prelim v) mid)))))))))
        xs (double-array n)
        second-walk (fn second-walk [^long v ^double offset]
                      (Array/aset xs v (m/+ (Array/aget prelim v) offset))
                      (doseq [k (kids v)] (second-walk (long k) (m/+ offset (Array/aget modifier v)))))]
    (when (m/pos? n)
      (first-walk 0)
      (second-walk 0 (m/- (Array/aget prelim 0))))
    (vec xs)))

(defn- curve-points
  "Samples a node-link tree link in (depth, breadth) coordinates.

  Parameters:

  - `links` (keyword): `:line` or `:diagonal`.
  - `start`, `end` (`[depth breadth]`): the parent's and the node's positions.

  Returns a vector of points: `:line` - `start` and `end`; `:diagonal` - 17 points of a cubic Bezier curve with both control points at the middle depth (d3 `linkHorizontal`)."
  [links [^double s0 ^double b0 :as start] [^double s1 ^double b1 :as end]]
  (if (= :line links)
    [start end]
    (let [mid (m/* 0.5 (m/+ s0 s1))]
      (mapv (fn [^double t]
              (let [u (m/- 1.0 t)
                    a (m/* u u u)
                    b (m/* 3.0 u u t)
                    c (m/* 3.0 u t t)
                    d (m/* t t t)]
                [(m/+ (m/* a s0) (m/* b mid) (m/* c mid) (m/* d s1)) (m/+ (m/* a b0) (m/* b b0) (m/* c b1) (m/* d b1))]))
            (m/slice-range 17)))))

(def node-link-layout-entries
  "Option entries of [[node-link-layout]] (a shared group, `:tree/node-link-layout`; [[cljplot2.graphs/tree]] forwards
  them)."
  (schema/in-fragment
   :tree/node-link-layout
   [(schema/optional :layout {:description "How the function places the nodes."
                              :default :tidy
                              :cljplot2/value-meanings {:tidy "Every node at its depth, as d3 `tree`."
                                                        :cluster "Every leaf at the deepest depth, a group at the mean position of its children, as d3 `cluster`."}}
                     [:enum :tidy :cluster])
    (schema/optional :links {:description "The shape of the links." :default :diagonal
                             :cljplot2/value-meanings {:diagonal "Curves that leave and enter the nodes along the depth axis."
                                                       :line "Straight lines."}}
                     [:enum :diagonal :line])
    circular-entry
    span-entry]))

(def node-link-layout-options
  "Options schema of [[node-link-layout]]."
  (into [:map {:closed true}] node-link-layout-entries))

(defn node-link-layout
  "Lays out a tree as nodes and links (d3 `tree` / `cluster`, Vega `tree`).

  Parameters:

  - `root` (map): the tree's root (see [[from-levels]], [[from-parents]], [[from-hclust]]).
  - `options` (map, optional):
    - `:layout` - `:tidy` (default; `tidy-breadths`, every node at its depth) or `:cluster` (leaves one step apart in order and all at the deepest depth, a group above its children's middle - as [[layout]]).
    - `:links` - `:diagonal` (default; curves leaving and entering along the depth axis) or `:line`.
    - `:circular` - `true`: breadth as the angle over `:span` degrees (on a full circle the breadth range plus one step, so the ends don't meet), depth as the radius from 0 (the root) to 1 (the deepest node); diagonal links are curves in (angle, radius) - d3 `linkRadial`; points in [-1, 1]. Default false: depth along x from 0 (the root), breadth along y (the first node of a depth on top: y = -breadth).
    - `:span` - degrees a circular layout covers, clockwise from the top (default 360).

  Returns `{:nodes [node ..] :links [{:id i :x x :y y} ..]}` - nodes in depth-first order with `:depth`, `:parent` (index), `:leaf`, `:breadth`, `:depth-pos`, `:x`, `:y` (circular: also `:angle` in degrees clockwise from the top and `:radius`); one link path per non-root node, `:id` the node's index, points from the parent to the node.

  Throws `ex-info {:key k}` for an invalid option `k` (checked first, see [[node-link-layout-options]]).

  See also [[layout]], `tidy-breadths`."
  {:cljplot2/api true
   :cljplot2/arguments [{:name 'root :description "The root of a tree of `tree/from-hclust`, `tree/from-levels` or `tree/from-parents`."}]
   :cljplot2/returns "`{:nodes :links}`: the nodes with their depth, breadth and position, and a link path per node but the root as rows `{:id :x :y}`."
   :cljplot2/options node-link-layout-options}
  ([root] (node-link-layout root {}))
  ([root {:keys [links circular ^double span] layout-kind :layout :or {layout-kind :tidy links :diagonal span 360.0} :as options}]
   (schema/check-options! node-link-layout-options options)
   (let [[placed-nodes] (placed root [nil 0 nil] [[] 0] (constantly nil))
         breadths (if (= :cluster layout-kind) (mapv :x placed-nodes) (tidy-breadths placed-nodes))
         max-depth (m/max 1.0 (v/mx (map :depth placed-nodes)))
         depth-pos-of (fn ^double [node] (if (and (= :cluster layout-kind) (:leaf node)) max-depth (double (:depth node))))
         lo (v/mn breadths)
         hi (v/mx breadths)
         breadth-nodes (map (fn [node ^double b] (assoc node :breadth (m/- b lo) :depth-pos (depth-pos-of node))) placed-nodes breadths)
         span-b (m/- hi lo)
         step (m// (m/radians span) (if (m/>= span 360.0) (m/inc span-b) (m/max span-b 1.0)))
         polar (fn [[^double d ^double b]] (let [a (m/* b step)
                                                 r (m// d max-depth)]
                                             [(m/* r (m/sin a)) (m/* r (m/cos a))]))
         place (fn [[^double d ^double b :as point]] (if circular (polar point) [d (m/- b)]))
         nodes (mapv (fn [{:keys [^double breadth ^double depth-pos] :as node}]
                       (let [[x y] (place [depth-pos breadth])]
                         (cond-> (assoc node :x x :y y)
                           circular (assoc :angle (m/degrees (m/* breadth step)) :radius (m// depth-pos max-depth)))))
                     breadth-nodes)]
     {:nodes nodes
      :links (vec (for [[i {:keys [parent] :as node}] (map-indexed vector nodes)
                        :when (some? parent)
                        :let [p (nodes parent)]
                        pt (curve-points links [(:depth-pos p) (:breadth p)] [(:depth-pos node) (:breadth node)])
                        :let [[x y] (place pt)]]
                    {:id i :x x :y y}))})))

;; edge bundling

(defn- ancestors-of
  "Lists a node's ancestors.

  Parameters:

  - `nodes` (vector of maps): laid out nodes with `:parent` indices.
  - `i` (long): a node's index.

  Returns a lazy sequence of indices from `i` up to the root, both included."
  [nodes i]
  (take-while some? (iterate #(:parent (nodes %)) i)))

(defn- tree-route
  "Finds the tree path between two nodes, as d3 `node.path`.

  Parameters:

  - `nodes` (vector of maps): laid out nodes with `:parent` indices.
  - `a`, `b` (longs): the two nodes' indices.

  Returns the vector of node indices up from `a` to their lowest common ancestor, then down to `b`."
  [nodes a b]
  (let [up-a (vec (ancestors-of nodes a))
        on-b (set (ancestors-of nodes b))
        lca (first (filter on-b up-a))
        to-lca (vec (take-while #(not= lca %) up-a))
        from-lca (reverse (take-while #(not= lca %) (ancestors-of nodes b)))]
    (-> to-lca (conj lca) (into from-lca))))

(defn- straightened
  "Pulls control points towards the straight line between their ends.

  Parameters:

  - `points` (vector of `[x y]`): the control points, at least one.
  - `beta` (double): bundling strength - 1 keeps the points, 0 puts them on the line.

  Returns the vector of points `beta * p + (1 - beta) * q`, `q` the point at the same fraction of the line from the first point to the last."
  [points ^double beta]
  (let [n (count points)
        [^double x0 ^double y0] (first points)
        [^double x1 ^double y1] (peek points)
        pull (m/- 1.0 beta)]
    (vec (map-indexed (fn [i [^double x ^double y]]
                        (let [t (if (m/> n 1) (m// (double i) (double (m/long-dec n))) 0.0)
                              lx (m/+ x0 (m/* t (m/- x1 x0)))
                              ly (m/+ y0 (m/* t (m/- y1 y0)))]
                          [(m/+ (m/* beta x) (m/* pull lx)) (m/+ (m/* beta y) (m/* pull ly))]))
                      points))))

(defn- basis-spline
  "Samples the uniform cubic B-spline of control points, as d3 `curveBasis`.

  The end points are repeated, so the curve starts and ends at them.

  Parameters:

  - `points` (vector of `[x y]`): the control points.
  - `steps` (long): points per span.

  Returns a vector of `[x y]` points; fewer than three control points - the points themselves."
  [points ^long steps]
  (if (m/< (count points) 3)
    (vec points)
    (let [ps (vec (concat [(first points) (first points)] points [(peek points) (peek points)]))
          spans (m/- (count ps) 3)]
      (vec (for [i (range spans)
                 s (range (if (m/== (long i) (m/long-dec spans)) (m/long-inc steps) steps))
                 :let [t (m// (double s) (double steps))
                       u (m/- 1.0 t)
                       t2 (m/* t t)
                       t3 (m/* t2 t)
                       b0 (m// (m/* u u u) 6.0)
                       b1 (m// (m/+ (m/* 3.0 t3) (m/* -6.0 t2) 4.0) 6.0)
                       b2 (m// (m/+ (m/* -3.0 t3) (m/* 3.0 t2) (m/* 3.0 t) 1.0) 6.0)
                       b3 (m// t3 6.0)
                       [[^double ax ^double ay] [^double bx ^double by] [^double cx ^double cy] [^double dx ^double dy]]
                       (subvec ps i (m/+ (long i) 4))]]
             [(m/+ (m/* b0 ax) (m/* b1 bx) (m/* b2 cx) (m/* b3 dx))
              (m/+ (m/* b0 ay) (m/* b1 by) (m/* b2 cy) (m/* b3 dy))])))))

(def bundle-paths-entries
  "Option entries of [[bundle-paths]] (a shared group, `:tree/bundle-paths`; [[cljplot2.graphs/bundle]] forwards
  them)."
  (schema/in-fragment
   :tree/bundle-paths
   [(schema/optional :from {:description "The accessor of an edge row's source leaf key." :default :from :cljplot2/example :source}
                     schema/accessor)
    (schema/optional :to {:description "The accessor of an edge row's target leaf key." :default :to :cljplot2/example :target}
                     schema/accessor)
    (schema/optional :beta {:description "The bundling strength: 1 routes an edge along its tree path, 0 draws a straight line."
                            :default 0.85 :cljplot2/example 0.3}
                     schema/fraction)]))

(def bundle-paths-options
  "Options schema of [[bundle-paths]]."
  (into [:map {:closed true}] bundle-paths-entries))

(defn leaf-key
  "Gives the value edges name a leaf by, for edge bundling ([[bundle-paths]], `plot/bundle`).

  Not part of the documented API (no `:cljplot2/api`).

  Parameters:

  - `node` (map): a leaf.

  Returns its `:id` (leaves of [[from-parents]] trees), else its `:name` (leaves of [[from-levels]] and [[from-hclust]] trees have no id)."
  [node]
  (if (contains? node :id) (:id node) (:name node)))

(defn bundle-paths
  "Routes edges between leaves along a laid out tree (Holten's hierarchical edge bundling).

  Each edge becomes a B-spline (`basis-spline`) through the positions of the tree path between its leaves, straightened towards the straight line by `:beta`.

  Parameters:

  - `edges` (sequence of maps): edge rows naming their end leaves by [[leaf-key]] (the `:id` of a [[from-parents]] node, else the leaf's `:name`; with repeated names the first leaf is used).
  - `laid` (map): the tree's layout (see [[layout]], usually `:circular`).
  - `options` (map, optional):
    - `:from` - accessor of an edge's source leaf key (default `:from`).
    - `:to` - accessor of an edge's target leaf key (default `:to`).
    - `:beta` - bundling strength in [0 1]: 1 - along the tree path, 0 - a straight line (default 0.85).

  Returns `{:paths [{:id i :x x :y y ..} ..] :dropped n}` - path rows with the edge's index `:id` and its row's keys kept; edges with an end that isn't a leaf, or the same leaf at both ends, are dropped and counted.

  Throws `ex-info {:key k}` for an invalid option `k` (checked first, see [[bundle-paths-options]])."
  {:cljplot2/api true
   :cljplot2/arguments [{:name 'edges :description "Edge rows between leaves of the tree." :columns {:from "The source leaf: a `tree/from-parents` node's `:id`, else a leaf's `:name`; the option `:from` names another column." :to "The target leaf; the option `:to` names another column."}}
                         {:name 'laid :description "The tree's layout of `tree/layout`, usually circular."}]
   :cljplot2/returns "`{:paths :dropped}`: a path per edge as rows `{:id :x :y}` with the edge's columns; the function drops and counts edges whose end isn't a leaf or that join a leaf to itself."
   :cljplot2/options bundle-paths-options}
  ([edges laid] (bundle-paths edges laid {}))
  ([edges laid {:keys [from to ^double beta] :or {from :from to :to beta 0.85} :as options}]
   (schema/check-options! bundle-paths-options options)
   (let [nodes (:nodes laid)
         leaf-index (reduce (fn [acc [i node]] (cond-> acc (and (:leaf node) (not (contains? acc (leaf-key node))))
                                                 (assoc (leaf-key node) i)))
                            {} (map-indexed vector nodes))
         from-of (data/row-accessor :from from)
         to-of (data/row-accessor :to to)
         routed (keep-indexed (fn [i e]
                                (let [a (leaf-index (from-of e)) b (leaf-index (to-of e))]
                                  (when (and a b (not= a b)) [i e (tree-route nodes a b)])))
                              edges)]
     {:paths (vec (for [[i e route] routed
                        [x y] (basis-spline (straightened (mapv (fn [j] [(:x (nodes j)) (:y (nodes j))]) route) beta) 8)]
                    (assoc e :id i :x x :y y)))
      :dropped (m/- (count edges) (count routed))})))
