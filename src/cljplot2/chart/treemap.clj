(ns cljplot2.chart.treemap
  "Treemap: nested rectangles from a hierarchy of columns, areas from a numeric value (a panel without axes).

  Mapping: `:value` (area, a positive finite number; other rows are dropped and counted) and `:hierarchy` (a
  non-empty vector of accessors, outer level first) - the tree of [[cljplot2.data.hierarchy]]. Optional `:color`
  aesthetic colors leaves (rows with the same path but different colors stay separate leaves); without it each
  top-level group takes the next `[:palette :color]` color, its leaves sharing it.

  Options (style defaults from theme `[:chart :treemap]`): `:layout` `:squarify` (default) or `:slice-dice`,
  `:order` of siblings - `:value` (descending) or `:data` (first appearance; a group's unnamed leaf comes last), as in
  the partition; default `:value` for `:squarify`, `:data` for `:slice-dice` (`:slice-dice` with `:value` - a mosaic
  ordered by size; `:squarify` with `:data` keeps the data order, cells less square),
  `:group-labels` / `:leaf-labels` (default true - the node's name; `false` - none; a function of the node info
  `{:name :value :depth :path :share :parent-share}` (see [[cljplot2.data.hierarchy/node-info]]) returning the text,
  `nil` / `\"\"` - none; e.g. `#(format \"%s\\n%.1f%%\" (:name %) (* 100 (:share %)))`; unnamed nodes get no label),
  `:gap` (pixels inside every group), `:stroke-color`,
  `:stroke-width`. Group headers use the `:treemap-group` text role in a strip at the group's top (reserved only when
  the header fits the group's width); facet panels lay out separately, each filling its own box (rectangle areas
  compare within a panel, not across panels); leaf labels the
  `:treemap-leaf` role, centered, drawn only when they fit; label text in black or white by the fill under it. The layout is computed when drawing, from the panel box.

  Functions: [[schemas]] - the chart's schemas; [[compute]] - per panel the tree, its style and options; [[draw]] - the layout and drawing of one panel; the key glyph is [[cljplot2.chart.legend/cell-legend]]."
  (:require [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.data.hierarchy :as hierarchy]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.theme :as theme]
            [cljplot2.layout.treemap :as layout]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries
  "Map entries of the treemap style (theme `[:chart :treemap]`; also options)."
  (into [(schema/optional :gap {:description "The room inside every group around its header and children, in pixels." :cljplot2/example 8} schema/non-neg)
         (schema/optional :stroke-color {:description "The outline colour of the cells." :cljplot2/value-meanings {:none "No outline."} :cljplot2/example :black} schema/color)
         (schema/optional :stroke-width {:description "The outline width of the cells in pixels; `0` draws no outline." :cljplot2/example 3} schema/non-neg)]
        draw/pattern-style-entries))

(def schemas
  "Schemas of the treemap: `:style` (theme `[:chart :treemap]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into theme/theme-style-entries)
                (conj (schema/optional :layout {:description "How the chart cuts each group's rectangle into its children's cells."
                                                :default :squarify
                                                :cljplot2/value-meanings {:squarify "Cells as near to squares as the values allow."
                                                                          :slice-dice "Slices across the rectangle, turning direction at every level."}}
                                       [:enum :squarify :slice-dice])
                      (schema/optional :order {:description "The order of siblings within their group's rectangle."
                                               :cljplot2/default-doc "`:value` with `:squarify`, `:data` with `:slice-dice`."
                                               :cljplot2/value-meanings {:value "Largest first." :data "In order of first appearance; a group's unnamed leaf comes last."}
                                               :cljplot2/example :data}
                                       [:enum :value :data]))
                (into hierarchy/label-entries))
   :mapping hierarchy/mapping-schema})

(def ^:private defaults
  "Option defaults the drawing reads: the layout and both label kinds on."
  {:layout :squarify :group-labels true :leaf-labels true})

(defn compute
  "Computes the treemap's stats: per panel the tree to lay out when drawing.

  Parameters:

  - `chart` (map): the chart value - `:options` (see the namespace doc), `:kind`, `:schema` (its style keys).
  - `split` (map): `:groups` - `{panel rows}`, `:mapping` - the merged mapping (`:value`, `:hierarchy`, optional `:color`).
  - `env` (map): `:theme` and the rest of the compute environment (see [[cljplot2.data.hierarchy/panel-trees]]).

  Returns `{:data {panel computed} :extents :na :dropped}`: `computed` - `{:tree root :top-count n :style st :options o :colored b}` (`root` - see [[cljplot2.data.hierarchy]], top-level children with `:top-index`; `o` - `:layout`, `:order`, `:group-labels`, `:leaf-labels` with their defaults; `:colored` - a mapped `:color`); aesthetic extents only (no x / y); `:dropped {:value n :color n}`.

  Throws `ex-info {:key :mapping}` when rows exist but none has a finite `:value` or, with a mapped `:color`, none a valid colour value (see [[cljplot2.data.hierarchy/panel-trees]]).

  See also [[draw]]."
  [{:keys [options] :as chart} {:keys [groups mapping]} {:keys [theme] :as env}]
  (let [{:keys [trees top-count extents na dropped]} (hierarchy/panel-trees groups mapping env)
        st (theme/resolved-style theme chart)
        with-defaults (data/options-with-defaults defaults options)
        opts (assoc with-defaults :order (data/option options :order (if (= :slice-dice (:layout with-defaults)) :data :value)))]
    {:data (update-vals trees (fn [tree] {:tree tree :top-count top-count :style st :options opts
                                          :colored (some? (:color mapping))}))
     :extents extents
     :na na
     :dropped dropped}))

;; drawing

(defn- pixel-box
  "Gives the integer pixel box of a rectangle, its edges rounded.

  Parameters:

  - `rect` (map): `:x`, `:y`, `:w`, `:h` in pixels.

  Returns `{:x :y :w :h}` of longs: the rounded left and top edges, the width and height between the rounded edges."
  [{:keys [^double x ^double y ^double w ^double h]}]
  (let [x0 (m/round x)
        y0 (m/round y)
        x1 (m/round (m/+ x w))
        y1 (m/round (m/+ y h))]
    {:x x0 :y y0 :w (m/long-sub x1 x0) :h (m/long-sub y1 y0)}))

(defn- draw-label!
  "Draws a label inside a box.

  Parameters:

  - `c`: the canvas.
  - `role` (keyword): the text role.
  - `text` (string): the label text.
  - `fill`: the colour under the label - the role's `:contrast` colour is black or white by it (see [[cljplot2.graphics.draw/text-color]]).
  - `box` (map): `:x`, `:y`, `:w`, `:h` in pixels.
  - `env` (map): the drawing environment.

  Returns `nil`; the canvas graphics state is kept (isolated graphics)."
  [c role text fill box env]
  (c2d/with-isolated-canvas [isolated c]
    (draw/draw-content {:kind :label :role role :text text :color (draw/text-color (:theme env) role fill)} isolated box env)))

(defn- fill-box!
  "Fills a box and draws its inner border.

  Parameters:

  - `c`: the canvas.
  - `color`: the fill colour.
  - `box` (map): integer `:x`, `:y`, `:w`, `:h` (see [[pixel-box]]).
  - `st` (map): the style - `:stroke-color`, `:stroke-width`, `:pattern`, `:pattern-color`.
  - `pattern-fill` (function): of a style and a fill, the pattern fill (see [[cljplot2.graphics.draw/pattern-fill-cache]]).

  Returns `nil`."
  [c color {:keys [x y w h] :as box} {:keys [stroke-color stroke-width] :as st} pattern-fill]
  (draw/fill-patterned! c color (pattern-fill st color) #(c2d/rect % (long x) (long y) (long w) (long h)))
  (draw/inner-border! c box stroke-color stroke-width))

(defn- children-rects
  "Lays out the children of a group node in its content box.

  Parameters:

  - `node` (map): a group node (with `:children`).
  - `content` (map): `:x`, `:y`, `:w`, `:h` - the room inside the group.
  - `depth` (long): the node's depth (root 0); slice-and-dice turns its slices by it.
  - `options` (map): `:layout` - `:squarify` or `:slice-dice`; `:order` - `:value` (descending) or `:data` (see [[cljplot2.data.hierarchy/ordered-children]]).

  Returns a sequence of `[child rect]` pairs, children in `:order`.

  See also [[cljplot2.layout.treemap/squarify]], [[cljplot2.layout.treemap/slice-dice]]."
  [node content depth {layout-kind :layout order :order}]
  (let [children (hierarchy/ordered-children node order)
        values (map :value children)]
    (map vector children (if (= layout-kind :slice-dice)
                           (layout/slice-dice values content depth)
                           (layout/squarify values content)))))

(defn- label-text
  "Gives the label text of a node.

  Parameters:

  - `labels`: the labels option - `true` (the name), `false` / `nil` (none) or a function of the node info.
  - `node` (map): the tree node.
  - `at` (map): `:depth`, `:path`, `:parent-value` - where the node is.
  - `total` (number): the root's value.

  Returns the text, or `nil` for none.

  See also [[cljplot2.data.hierarchy/label-text]], [[cljplot2.data.hierarchy/node-info]]."
  [labels node {:keys [depth path parent-value]} total]
  (when labels
    (hierarchy/label-text labels node (hierarchy/node-info node depth path parent-value total))))

(defn- draw-node!
  "Draws a node of the tree in its rectangle, then its children inside it.

  A group gets a brightened fill, a header strip with its label (reserved only when the label fits the group's width) and its children laid out in the rest, `:gap` pixels inside; a leaf its fill and a centred label when it fits. A node smaller than a pixel either way isn't drawn.

  Parameters:

  - `c`: the canvas.
  - `node` (map): the tree node.
  - `rect` (map): `:x`, `:y`, `:w`, `:h` in pixels.
  - `at` (map): `:depth` (root 0), `:path` (names from the top), `:parent-value`.
  - `color`: the node's fill; `nil` for the root (no fill, no header).
  - `ctx` (map): `:style` (`:gap`, outlines, pattern), `:options` (`:layout`, `:order`, `:group-labels`, `:leaf-labels`), `:leaf-color` (function of a leaf - its mapped colour or `nil`), `:top-color` (function of a top-level node), `:pattern-fill`, `:total`.
  - `env` (map): the drawing environment.

  Returns `nil`."
  [c node rect at color {:keys [style options leaf-color top-color pattern-fill total] :as ctx} env]
  (let [{^long box-x :x ^long box-y :y ^long box-w :w ^long box-h :h :as box} (pixel-box rect)
        depth (long (:depth at))
        root? (m/zero? depth)]
    (when (or root? (and (m/>= box-w 1) (m/>= box-h 1)))
      (if (:children node)
        (let [gap (double (:gap style))
              {:keys [^double x ^double y ^double w ^double h]} rect
              header-text (when-not root? (label-text (:group-labels options) node at total))
              ^Vec2 header-size (when header-text (measure/label-size :treemap-group header-text env))
              ;; the header strip is reserved only when the header text fits the group's width
              header? (and header-size (m/<= (.x header-size) (m/- w (m/* 2.0 gap))))
              header-h (if header? (.y header-size) 0.0)
              content {:x (m/+ x gap)
                       :y (m/+ y gap header-h)
                       :w (m/- w (m/* 2.0 gap))
                       :h (m/- h (m/* 2.0 gap) header-h)}
              gap-px (m/round gap)]
          (when-not root?
            (fill-box! c (c/brighten color) box (dissoc style :pattern) pattern-fill))
          (when header?
            (draw-label! c :treemap-group header-text (c/brighten color)
                         {:x (m/long-add box-x gap-px)
                          :y (m/long-add box-y gap-px)
                          :w (m/long-max 0 (m/long-sub box-w (m/long-mult 2 gap-px)))
                          :h (long (m/ceil header-h))}
                         env))
          (when (and (m/pos? (double (:w content))) (m/pos? (double (:h content))))
            (doseq [[child child-rect] (children-rects node content depth options)]
              (draw-node! c child child-rect
                          {:depth (m/long-inc depth)
                           :path (cond-> (:path at) (some? (:name child)) (conj (:name child)))
                           :parent-value (:value node)}
                          (if root? (top-color child) color)
                          ctx env))))
        (let [fill (or (leaf-color node) color)]
          (when-not root?
            (fill-box! c fill box style pattern-fill))
          (when-let [text (label-text (:leaf-labels options) node at total)]
            (let [^Vec2 size (measure/label-size :treemap-leaf text env)]
              (when (and (m/<= (.x size) box-w) (m/<= (.y size) box-h))
                (draw-label! c :treemap-leaf text fill box env)))))))))

(defn draw
  "Lays out and draws one panel's treemap in the panel box.

  Parameters:

  - `_chart`: the chart value (unused).
  - `computed` (map): the panel's stats - `:tree`, `:top-count`, `:style`, `:options`, `:colored` (see [[compute]]).
  - `canvas`: the panel canvas.
  - `box` (map): `:w`, `:h` - the panel size in pixels.
  - `scales` (map): the finalized scales (`:color` for mapped leaf colours).
  - `env` (map): the drawing environment (`:theme`).

  Returns `nil`; nothing is drawn without a tree.

  See also [[draw-node!]]."
  [_chart {:keys [tree top-count style options colored]} canvas {:keys [w h]} scales env]
  (when tree
    (let [canvas-env (assoc env :canvas canvas)
          theme (:theme env)
          na (get-in theme [:palette :na])
          color-fn (when colored (aesthetic/attribute-fn :color (:color scales) na))
          palette-fn (hierarchy/top-color-fn theme top-count)
          ctx {:style style :options options :pattern-fill (draw/pattern-fill-cache) :total (:value tree)
               :leaf-color (fn [leaf] (when color-fn (color-fn (:color leaf))))
               :top-color (fn [child] (if colored na (palette-fn (:top-index child))))}]
      (draw-node! canvas tree {:x 0.0 :y 0.0 :w (double w) :h (double h)} {:depth 0 :path [] :parent-value (:value tree)}
                  nil ctx canvas-env))))

