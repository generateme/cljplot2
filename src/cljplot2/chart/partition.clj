(ns cljplot2.chart.partition
  "Partition: a hierarchy as adjacent cells - an icicle in cartesian plots, a sunburst under `:coord {:kind :polar}`
  (rings around the centre; `:inner` leaves a hole, see [[cljplot2.scale.coord]]).

  Mapping: `:value` (a positive finite number; other rows are dropped and counted) and `:hierarchy` (a non-empty
  vector of accessors, outer level first) - the tree of [[cljplot2.data.hierarchy]], as in the treemap.

  Layout (computed with the stats, in data units): a node spans `x` over its fraction of the panel's total - every
  panel fills `[0 1]`, so facets and full circles are complete; a node at depth `d` (top level 1) spans `y` from
  `d - 1` to `d`, the root at the bottom (the centre in polar plots). Children lie inside their parent's `x` span in
  `:order` - `:value` (default, descending) or `:data` (first appearance); a group's unnamed leaf comes last.
  `:root true` draws the root too, on `y` `[0 1]`, every level one up. The x / y axes and grid are hidden
  (`:scale-defaults`); the plot's scales may override that. The root at the top: a reversed `:y` domain -
  `:scales {:y {:domain [n 0]}}`, `n` the number of levels (one more with `:root true`).

  Colors: without a mapped `:color` each top-level node takes the next `[:palette :color]` color, its descendants
  share it, lighter by depth (each level a quarter of the way from its CIELAB lightness L* to 90 - light colors
  stay apart from white borders); a mapped `:color` colors the leaves (legend keys), groups and the root take
  `[:palette :na]`; `:color-by :name` (default `:top` - the above) colors every node by its name through the color
  scale - a legend of the names, equal names alike at every depth (sequence sunbursts), the root and unnamed leaves
  in `[:palette :na]`; it can't be combined with a mapped `:color` (`ex-info {:key :color-by}`). Style (theme
  `[:chart :partition]`): `:border-color`, `:border-width` (the cells' outlines),
  `:pattern`, `:pattern-color`.

  Labels: `:group-labels` / `:leaf-labels` (default true - the node's name; `false` - none; a function of the node
  info `{:name :value :depth :path :share :parent-share}` (see [[cljplot2.data.hierarchy/node-info]]) returning the
  text, `nil` / `\"\"` - none; unnamed nodes get no label), text roles `:partition-group` / `:partition-leaf`
  (the text in black or white by the cell's fill), centred in the cell and drawn only when they fit: horizontal in cartesian plots (within the cell), along the
  radius in polar ones (the text's width within the ring, its height within the arc at the middle radius)."
  (:require [fastmath.core :as m]
            [cljplot2.content.label :as label]
            [cljplot2.data.hierarchy :as hierarchy]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.coord :as coord]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries
  (into [(schema/optional :border-color {:description "cell border colour"} schema/color)
         (schema/optional :border-width {:description "cell border width in pixels"} schema/non-neg)]
        draw/pattern-style-entries))

(def schemas
  "Schemas of the partition: `:style` (theme `[:chart :partition]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into theme/theme-style-entries)
                (conj (schema/optional :order {:description "order of siblings: :value (descending) or :data (first appearance)"
                                               :default :value}
                                       [:enum :value :data])
                      (schema/optional :color-by {:description (str "node colours: :top - each top-level node's palette colour, lighter by depth; "
                                                                    ":name - every node by its name through the color scale")
                                                  :default :top}
                                       [:enum :top :name])
                      (schema/nilable :root {:description "draw the root too, every level one up" :default false} schema/bool))
                (into hierarchy/label-entries))
   :mapping hierarchy/mapping-schema})

(def ^:private defaults {:order :value :root false :group-labels true :leaf-labels true})

(defn- style
  [options theme]
  (merge (theme/chart-style theme :partition options) (select-keys options [:border-color :border-width :pattern :pattern-color])))

(defn cells
  "Cells of `tree` (a root, see [[cljplot2.data.hierarchy]]) for `options` (`:order`, `:root`, `:group-labels`,
  `:leaf-labels`): `[{:x0 :x1 :y0 :y1 :depth :leaf? :color :top-index :label} ..]`, parents before their children;
  `x` - fractions of the root's value, `y` - depth bands (see the namespace doc); `:label` - the text or `nil`."
  [tree {:keys [order root group-labels leaf-labels]}]
  (let [total (double (:value tree))
        shift (if root 1.0 0.0)
        cell (fn [node depth path parent-value x0 top-index]
               (let [leaf? (nil? (:children node))
                     x1 (+ (double x0) (/ (double (:value node)) total))]
                 {:x0 (double x0) :x1 x1 :y0 (+ (dec (double depth)) shift) :y1 (+ (double depth) shift)
                  :depth depth :leaf? leaf? :color (:color node) :top-index top-index :name (:name node)
                  :label (hierarchy/label-text (if leaf? leaf-labels group-labels) node
                                               (hierarchy/node-info node depth path parent-value total))}))
        walk (fn walk [node depth path x0 top-index]
               (when (:children node)
                 (loop [[child & more] (hierarchy/ordered-children node order) x x0 out []]
                   (if-not child
                     out
                     (let [path' (cond-> path (some? (:name child)) (conj (:name child)))
                           top (if (zero? (long depth)) (:top-index child) top-index)
                           c (cell child (inc (long depth)) path' (:value node) x top)]
                       (recur more (:x1 c) (-> out (conj c) (into (walk child (inc (long depth)) path' x top)))))))))]
    (cond->> (walk tree 0 [] 0.0 nil)
      root (cons (cell tree 0 [] total 0.0 nil))
      true vec)))

(defn computed-aesthetics
  "The partition's computed color with `:color-by :name` (no title); `nil` otherwise. Throws `ex-info {:key
  :color-by}` when `:color` is mapped too."
  [options mapping]
  (when (= :name (:color-by options))
    (when (some? (:color mapping))
      (schema/fail! "Partition :color-by :name colors the nodes by name; it can't take a mapped :color too."
                    {:key :color-by :value (:color mapping)}))
    {:color nil}))

(defn- name-extents
  "`{panel {:color extent}}` of the node names of every panel's tree (`trees`) under the plot's color spec."
  [trees plot-scales]
  (into {} (keep (fn [[panel tree]]
                   (when-let [e (aesthetic/value-extent :color (get plot-scales :color {})
                                                        (keep :name (rest (tree-seq :children :children tree))))]
                     [panel {:color e}])))
        trees))

(defn compute
  "Per panel: `{:cells [..] :top-count n :style style :colored? bool :named? bool}` (see [[cells]]; `:named?` -
  `:color-by :name`); extents `:x` `[0 1]`, `:y` `[0 levels]` (the deepest panel's), aesthetic extents (the node
  names' with `:color-by :name`), no padding, `:scale-defaults` hiding both axes and grids, `:na`, `:dropped {:value
  n :color n}`."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme plot-scales] :as env}]
  (let [{:keys [trees top-count extents na dropped]} (hierarchy/panel-trees groups mapping env)
        named? (= :name (:color-by options))
        extents (if named? (name-extents trees plot-scales) extents)
        opts (merge defaults (into {} (remove (comp nil? val)) (select-keys options (keys defaults))))
        st (style options theme)
        panel-cells (update-vals trees #(cells % opts))
        levels (reduce max 1.0 (map :y1 (mapcat val panel-cells)))
        position-extents {:x (data/extent :x [0.0 1.0]) :y (data/extent :y [0.0 levels])}]
    {:data (update-vals panel-cells (fn [cs] {:cells cs :top-count top-count :style st :colored? (some? (:color mapping))
                                              :named? named?}))
     :extents (merge-with merge extents (update-vals panel-cells (constantly position-extents)))
     :padding {:x [0 0] :y [0 0]}
     :na na
     :dropped dropped
     :scale-defaults {:x {:axis false} :y {:axis false}}}))

;; drawing

(defn- line-room
  "Room for the label's height in the cell between range points `[h0 v0]` and `[h1 v1]`: the cell's height, the arc
  at the middle radius in polar plots."
  ^double [geometry [h0 v0] [h1 v1]]
  (let [h0 (double h0) v0 (double v0) h1 (double h1) v1 (double v1)]
    (if geometry
      (* (m/abs (- h1 h0)) (m/abs (* 0.5 (+ v0 v1))))
      (m/abs (- v1 v0)))))

(defn- label-fit
  "`[pixel-x pixel-y rotation]` of a label of natural (unrotated) `size` centred in the cell between range points
  `[h0 v0]` and `[h1 v1]`, or `nil` when it doesn't fit (see the namespace doc)."
  [geometry [h0 v0] [h1 v1] ^Vec2 size]
  (let [h0 (double h0) v0 (double v0) h1 (double h1) v1 (double v1)
        hm (* 0.5 (+ h0 h1)) vm (* 0.5 (+ v0 v1))]
    (if geometry
      (when (and (<= (.x size) (m/abs (- v1 v0))) (<= (.y size) (* (m/abs (- h1 h0)) (m/abs vm))))
        (let [^Vec2 p (coord/to-pixel geometry hm vm)]
          [(.x p) (.y p) (coord/radial-rotation hm)]))
      (when (and (<= (.x size) (m/abs (- h1 h0))) (<= (.y size) (m/abs (- v1 v0))))
        [hm vm 0.0]))))

(defn draw
  "Draw one panel's cells (fills, outlines), then their labels."
  [_chart {:keys [cells style] :as panel} canvas _box scales env]
  (let [geometry (scale/coord-geometry scales)
        range-point (scale/range-projection scales)
        fill-of (hierarchy/fill-fn panel scales (:theme env))
        pattern-fill (draw/pattern-fill-cache)
        corners (mapv (fn [{:keys [x0 x1 y0 y1]}] [(range-point x0 y0) (range-point x1 y1)]) cells)
        stroke (:border-color style)
        sw (double (:border-width style))]
    (doseq [[cell [a b]] (map vector cells corners)
            :let [fill (fill-of cell)]]
      (draw/range-bar! canvas geometry a b fill stroke sw (pattern-fill style fill)))
    (doseq [[{:keys [label leaf?] :as cell} [a b]] (map vector cells corners)
            :when (and label (every? #(m/valid-double? (double %)) (concat a b)))
            :let [role (if leaf? :partition-leaf :partition-group)]
            ;; a line is at least the font size tall: cells without that room aren't measured
            :when (>= (line-room geometry a b) (double (get-in env [:theme :text role :size])))
            :let [content {:kind :label :role role :text label}
                  size (measure/natural-size content {:canvas canvas :theme (:theme env)})
                  [x y rotation] (label-fit geometry a b size)]
            :when x]
      (label/draw-at! canvas (assoc content :rotation rotation :color (draw/text-color (:theme env) role (fill-of cell)))
                      [:center :middle] x y env))))

(defn legend
  "Key glyph: a filled square inset in `box`, with the chart's `:pattern`."
  [{:keys [options]} attributes canvas box env]
  (let [st (style options (:theme env))]
    (draw/bar-key! canvas box (assoc (select-keys st [:pattern :pattern-color]) :color (:color attributes) :stroke-width 0)
                   false)))
