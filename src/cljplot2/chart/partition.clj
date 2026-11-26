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
  `[:chart :partition]`): `:stroke-color`, `:stroke-width` (the cells' outlines),
  `:pattern`, `:pattern-color`.

  Labels: `:group-labels` / `:leaf-labels` (default true - the node's name; `false` - none; a function of the node
  info `{:name :value :depth :path :share :parent-share}` (see [[cljplot2.data.hierarchy/node-info]]) returning the
  text, `nil` / `\"\"` - none; unnamed nodes get no label), text roles `:partition-group` / `:partition-leaf`
  (the text in black or white by the cell's fill), centred in the cell and drawn only when they fit: horizontal in cartesian plots (within the cell), along the
  radius in polar ones (the text's width within the ring, its height within the arc at the middle radius).

  Functions: [[schemas]] - the chart's schemas; [[cells]] - the laid out cells of a tree; [[computed-aesthetics]] - the computed colour of `:color-by :name`; [[compute]] - per panel the cells; [[draw]] - one panel's cells and labels; the key glyph is [[cljplot2.chart.legend/cell-legend]]."
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
  "Map entries of the partition style (theme `[:chart :partition]`; also options)."
  (into [(schema/optional :stroke-color {:description "The outline colour of the cells." :cljplot2/value-meanings {:none "No outline."} :cljplot2/example :black} schema/color)
         (schema/optional :stroke-width {:description "The outline width of the cells in pixels; `0` draws no outline." :cljplot2/example 3} schema/non-neg)]
        draw/pattern-style-entries))

(def schemas
  "Schemas of the partition: `:style` (theme `[:chart :partition]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into theme/theme-style-entries)
                (conj (schema/optional :order {:description "The order of siblings within their parent's span."
                                               :default :value
                                               :cljplot2/value-meanings {:value "Largest first." :data "In order of first appearance; a group's unnamed leaf comes last."}}
                                       [:enum :value :data])
                      (schema/optional :color-by {:description "How the chart colours the nodes; `:name` can't go with a mapped `:color`."
                                                  :default :top
                                                  :cljplot2/value-meanings {:top "Without a mapped `:color`, each top-level node takes the next palette colour and its descendants lighter shades by depth; a mapped `:color` colours the leaves and the groups take the NA colour."
                                                                            :name "Every node takes its name's colour from the colour scale, with a legend; equal names share a colour at every depth."}}
                                       [:enum :top :name])
                      (schema/nilable :root {:description "Whether the chart draws the root too."
                                             :default false
                                             :cljplot2/value-meanings {true "Draws the root at the bottom (the centre in polar plots), every level one up." false "Starts at the top level."}}
                                      schema/bool))
                (into hierarchy/label-entries))
   :mapping hierarchy/mapping-schema})

(def ^:private defaults
  "Option defaults the layout and labels read."
  {:order :value :root false :group-labels true :leaf-labels true})

(defn cells
  "Lays out the cells of a tree in data units.

  A node spans `x` over its fraction of the root's value, inside its parent's span in `:order`; a node at depth `d` (top level 1) spans `y` from `d - 1` to `d`, every level one up with `:root` (see the namespace doc).

  Parameters:

  - `tree` (map): a root (see [[cljplot2.data.hierarchy]]).
  - `options` (map): `:order` (`:value` or `:data`), `:root` (draw the root too), `:group-labels`, `:leaf-labels` (see [[cljplot2.data.hierarchy/label-text]]).

  Returns a vector of `{:x0 :x1 :y0 :y1 :depth :leaf :color :top-index :name :label}`, parents before their children; `:label` - the text or `nil`. A root without children gives no cells (the root's own with `:root`)."
  [tree {:keys [order root group-labels leaf-labels]}]
  (let [total (double (:value tree))
        shift (if root 1.0 0.0)
        ;; more than 4 arguments: no primitive hints
        cell (fn [node depth path parent-value x0 top-index]
               (let [leaf? (nil? (:children node))
                     x1 (m/+ (double x0) (m// (double (:value node)) total))]
                 {:x0 x0 :x1 x1 :y0 (m/+ (m/dec (double depth)) shift) :y1 (m/+ (double depth) shift)
                  :depth depth :leaf leaf? :color (:color node) :top-index top-index :name (:name node)
                  :label (hierarchy/label-text (if leaf? leaf-labels group-labels) node
                                               (hierarchy/node-info node depth path parent-value total))}))
        walk (fn walk [node depth path x0 top-index]
               (when (:children node)
                 (loop [[child & more] (hierarchy/ordered-children node order)
                        x x0
                        out []]
                   (if-not child
                     out
                     (let [child-path (cond-> path (some? (:name child)) (conj (:name child)))
                           top (if (m/zero? (long depth)) (:top-index child) top-index)
                           c (cell child (m/long-inc (long depth)) child-path (:value node) x top)]
                       (recur more (:x1 c) (-> out (conj c) (into (walk child (m/long-inc (long depth)) child-path x top)))))))))]
    (cond->> (walk tree 0 [] 0.0 nil)
      root (cons (cell tree 0 [] total 0.0 nil))
      true vec)))

(defn computed-aesthetics
  "Gives the partition's computed colour aesthetic.

  Parameters:

  - `options` (map): the chart's options - `:color-by`.
  - `mapping` (map): the merged mapping.

  Returns `{:color nil}` (a computed colour without a title) with `:color-by :name`, else `nil`.

  Throws `ex-info {:key :color-by}` when `:color-by :name` meets a mapped `:color`."
  [options mapping]
  (when (= :name (:color-by options))
    (when (some? (:color mapping))
      (schema/fail! "Partition :color-by :name colors the nodes by name; it can't take a mapped :color too."
                    {:key :color-by :value (:color mapping)}))
    {:color nil}))

(defn- name-extents
  "Gives the colour extents of the node names of every panel's tree.

  Parameters:

  - `trees` (map): `{panel root}`.
  - `plot-scales` (map): the plot's `:scales` (its `:color` spec).

  Returns `{panel {:color extent}}`; a panel without named nodes is left out.

  See also [[cljplot2.scale.aesthetic/value-extent]]."
  [trees plot-scales]
  (into {} (keep (fn [[panel tree]]
                   (when-let [e (aesthetic/value-extent :color (get plot-scales :color {})
                                                        (keep :name (rest (tree-seq :children :children tree))))]
                     [panel {:color e}])))
        trees))

(defn compute
  "Computes the partition's stats: per panel the laid out cells.

  Parameters:

  - `chart` (map): the chart value - `:options` (see the namespace doc), `:kind`, `:schema` (its style keys).
  - `split` (map): `:groups` - `{panel rows}`, `:mapping` - the merged mapping.
  - `env` (map): `:theme`, `:plot-scales` and the rest of the compute environment.

  Returns `{:data {panel {:cells [..] :top-count n :style st :colored b :named b}} :extents :padding :na :dropped :scale-defaults}`: cells - see [[cells]]; `:named` - `:color-by :name`; extents `:x` `[0 1]`, `:y` `[0 levels]` (the deepest panel's, at least 1) and the aesthetic extents (the node names' with `:color-by :name`); no padding; `:scale-defaults` hiding both axes; `:dropped {:value n :color n}`."
  [{:keys [options] :as chart} {:keys [groups mapping]} {:keys [theme plot-scales] :as env}]
  (let [{:keys [trees top-count extents na dropped]} (hierarchy/panel-trees groups mapping env)
        named? (= :name (:color-by options))
        aesthetic-extents (if named? (name-extents trees plot-scales) extents)
        opts (data/options-with-defaults defaults options)
        st (theme/resolved-style theme chart)
        panel-cells (update-vals trees #(cells % opts))
        levels (reduce m/max 1.0 (map :y1 (mapcat val panel-cells)))
        position-extents {:x (data/extent :x [0.0 1.0]) :y (data/extent :y [0.0 levels])}]
    {:data (update-vals panel-cells (fn [cs] {:cells cs :top-count top-count :style st :colored (some? (:color mapping))
                                              :named named?}))
     :extents (merge-with merge aesthetic-extents (update-vals panel-cells (constantly position-extents)))
     :padding data/zero-padding
     :na na
     :dropped dropped
     :scale-defaults {:x {:axis false} :y {:axis false}}}))

;; drawing

(defn- line-room
  "Gives the room for a label's height in a cell.

  Parameters:

  - `geometry` (map or `nil`): the polar geometry; `nil` - cartesian.
  - `[h0 v0]`, `[h1 v1]`: the cell's corners in range space.

  Returns pixels: the cell's height; in polar plots the arc at the cell's middle radius."
  ^double [geometry [^double h0 ^double v0] [^double h1 ^double v1]]
  (if geometry
    (m/* (m/abs (m/- h1 h0)) (m/abs (m/* 0.5 (m/+ v0 v1))))
    (m/abs (m/- v1 v0))))

(defn- label-fit
  "Places a label centred in a cell, if it fits.

  Cartesian: the label horizontal within the cell. Polar: along the radius, its width within the ring and its height within the arc at the middle radius.

  Parameters:

  - `geometry` (map or `nil`): the polar geometry; `nil` - cartesian.
  - `[h0 v0]`, `[h1 v1]`: the cell's corners in range space.
  - `size` (`Vec2`): the label's natural (unrotated) size.

  Returns `[pixel-x pixel-y rotation]`, or `nil` when the label doesn't fit."
  [geometry [^double h0 ^double v0] [^double h1 ^double v1] ^Vec2 size]
  (let [hm (m/* 0.5 (m/+ h0 h1))
        vm (m/* 0.5 (m/+ v0 v1))]
    (if geometry
      (when (and (m/<= (.x size) (m/abs (m/- v1 v0))) (m/<= (.y size) (m/* (m/abs (m/- h1 h0)) (m/abs vm))))
        (let [^Vec2 p (coord/to-pixel geometry hm vm)]
          [(.x p) (.y p) (coord/radial-rotation hm)]))
      (when (and (m/<= (.x size) (m/abs (m/- h1 h0))) (m/<= (.y size) (m/abs (m/- v1 v0))))
        [hm vm 0.0]))))

(defn draw
  "Draws one panel's cells - fills and outlines - then their labels.

  Parameters:

  - `_chart`: the chart value (unused).
  - `computed` (map): the panel's stats - `:cells`, `:style` and the colour keys read by [[cljplot2.data.hierarchy/fill-fn]] (see [[compute]]).
  - `canvas`: the panel canvas.
  - `_box`: the panel box (unused - the scales place the cells).
  - `scales` (map): the finalized scales.
  - `env` (map): the drawing environment (`:theme`).

  Returns `nil`. A label is drawn only when the cell has room for a line of its role's font size and the label fits (see [[label-fit]])."
  [_chart {panel-cells :cells style :style :as panel} canvas _box scales env]
  (let [geometry (scale/coord-geometry scales)
        range-point (scale/range-projection scales)
        fill-of (hierarchy/fill-fn panel scales (:theme env))
        pattern-fill (draw/pattern-fill-cache)
        corners (mapv (fn [{:keys [x0 x1 y0 y1]}] [(range-point x0 y0) (range-point x1 y1)]) panel-cells)
        stroke (:stroke-color style)
        sw (double (:stroke-width style))]
    (doseq [[cell [a b]] (map vector panel-cells corners)
            :let [fill (fill-of cell)]]
      (draw/range-bar! canvas geometry a b fill stroke sw (pattern-fill style fill)))
    (doseq [[{:keys [label leaf] :as cell} [a b]] (map vector panel-cells corners)
            :when (and label (data/finite-point? a) (data/finite-point? b))
            :let [role (if leaf :partition-leaf :partition-group)]
            ;; a line is at least the font size tall: cells without that room aren't measured
            :when (m/>= (line-room geometry a b) (double (get-in env [:theme :text role :size])))
            :let [content {:kind :label :role role :text label}
                  size (measure/natural-size content {:canvas canvas :theme (:theme env)})
                  [x y rotation] (label-fit geometry a b size)]
            :when x]
      (label/draw-at! canvas (assoc content :rotation rotation :color (draw/text-color (:theme env) role (fill-of cell)))
                      [:center :middle] x y env))))

