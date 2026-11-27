(ns cljplot2.chart.pack
  "Circle packing: a hierarchy as nested circles, leaf areas from a numeric value (a panel without axes, cartesian
  plots only).

  Mapping: `:value` (a positive finite number; other rows are dropped and counted) and `:hierarchy` (a non-empty
  vector of accessors, outer level first) - the tree of [[cljplot2.data.hierarchy]], as in the treemap. One level
  (`:hierarchy [:name]`) packs plain circles, one per row.

  Layout (when drawing, from the panel box - see [[cljplot2.layout.pack]]): leaf areas proportional to the value;
  each group the smallest circle around its packed children - a group's area is not proportional to its value
  (packing leaves gaps), compare leaves within a group. Children are packed in `:order` - `:value` (default,
  descending: the largest near the centre) or `:data` (first appearance). The whole packing fits the panel's
  shorter side, centred; circles stay round in any panel shape. Facet panels pack separately, each filling its own
  box: circle sizes compare within a panel, not across panels. `:gap` (pixels, as d3's padding) - the gap between
  siblings and between a child and its parent's edge; a panel too small for such gaps gets the widest it holds.

  Colors (as the partition): without a mapped `:color` each top-level node takes the next `[:palette :color]` color,
  the nodes below it lighter by depth (see [[cljplot2.data.hierarchy/depth-shade]]); a mapped `:color` colors the
  leaves (legend keys), groups take `[:palette :na]`. `:root true` draws the root circle too (in `[:palette :na]`);
  `:hidden-levels` - a set of depths (root 0, top level 1) whose circles and labels aren't drawn, the layout
  unchanged. Style (theme `[:chart :pack]`): `:gap`, `:stroke-color`, `:stroke-width` (the circles' outlines),
  `:pattern`, `:pattern-color`.

  Labels: `:group-labels` / `:leaf-labels` (default true - the node's name; `false` - none; a function of the node
  info `{:name :value :depth :path :share :parent-share}` (see [[cljplot2.data.hierarchy/node-info]]) returning the
  text, `nil` / `\"\"` - none; unnamed nodes get no label), text roles `:pack-group` / `:pack-leaf`, in black or white
  by the circle painted under the label's centre, drawn after all circles and only when they fit: a leaf label
  centred (its box's diagonal within the diameter); a group label inside the circle under its top (its box's top
  corners on the circle at most, the box above the centre) - it may cover child circles. A label overlapping one
  placed before it (parents first: an ancestor's) isn't drawn.

  Functions: [[schemas]] - the chart's schemas; [[compute]] - per panel the ordered tree; [[draw]] - the packing and drawing of one panel; [[legend]] - the key glyph."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.content.label :as label]
            [cljplot2.data.hierarchy :as hierarchy]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.layout.pack :as layout]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries
  "Map entries of the pack style (theme `[:chart :pack]`; also options)."
  (into [(schema/optional :gap {:description "The room between siblings and between a child and its parent's edge, in pixels." :cljplot2/example 10}
                          schema/non-neg)
         (schema/optional :stroke-color {:description "The outline colour of the circles." :cljplot2/value-meanings {:none "No outline."} :cljplot2/example :black} schema/color)
         (schema/optional :stroke-width {:description "The outline width of the circles in pixels; `0` draws no outline." :cljplot2/example 3} schema/non-neg)]
        draw/pattern-style-entries))

(def schemas
  "Schemas of the circle packing: `:style` (theme `[:chart :pack]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into theme/theme-style-entries)
                (conj (schema/optional :order {:description "The order in which the chart packs siblings."
                                               :default :value
                                               :cljplot2/value-meanings {:value "Largest first, near the centre." :data "In order of first appearance; a group's unnamed leaf comes last."}}
                                       [:enum :value :data])
                      (schema/nilable :root {:description "Whether the chart draws the root circle too."
                                             :default false
                                             :cljplot2/value-meanings {true "Draws the circle around everything." false "Starts at the top level."}}
                                      schema/bool)
                      (schema/nilable :hidden-levels {:description "The depths the chart leaves out, counting the root as `0` and the top level as `1`; the other circles keep their places."
                                                      :default #{} :cljplot2/example #{1}}
                                      [:set [:int {:min 0}]]))
                (into hierarchy/label-entries))
   :mapping hierarchy/mapping-schema})

(def ^:private defaults
  "Option defaults the packing and drawing read."
  {:order :value :root false :hidden-levels #{} :group-labels true :leaf-labels true})

(def ^{:private true :const true :tag 'double} min-drawn-radius
  "Smallest radius (pixels) of a drawn circle: smaller ones would be single anti-aliased dots (internal, not an option)."
  0.5)

(defn compute
  "Computes the circle packing's stats: per panel the tree, its children in `:order`.

  Parameters:

  - `chart` (map): the chart value - `:options` (see the namespace doc), `:kind`, `:schema` (its style keys).
  - `split` (map): `:groups` - `{panel rows}`, `:mapping` - the merged mapping.
  - `env` (map): `:theme` and the rest of the compute environment.

  Returns `{:data {panel {:tree root :top-count n :style st :options o :colored b}} :extents :na :dropped}`: `root` - see [[cljplot2.data.hierarchy]]; `o` - the options of `defaults` with their defaults; aesthetic extents only (no x / y); `:dropped {:value n :color n}`.

  Throws `ex-info {:key :mapping}` when rows exist but none has a finite `:value` or, with a mapped `:color`, none a valid colour value (see [[cljplot2.data.hierarchy/panel-trees]]).

  See also [[draw]]."
  [{:keys [options] :as chart} {:keys [groups mapping]} {:keys [theme] :as env}]
  (let [{:keys [trees top-count extents na dropped]} (hierarchy/panel-trees groups mapping env)
        opts (data/options-with-defaults defaults options)
        st (theme/resolved-style theme chart)]
    {:data (update-vals trees (fn [tree] {:tree (hierarchy/ordered-tree tree (:order opts)) :top-count top-count :style st
                                          :options opts :colored (some? (:color mapping))}))
     :extents extents
     :na na
     :dropped dropped}))

;; drawing

(defn- drawn-nodes
  "Gives the nodes of a packed tree to draw, parents first.

  Parameters:

  - `tree` (map): the packed root (nodes with `:x`, `:y`, `:r`, see [[cljplot2.layout.pack]]).
  - `options` (map): `:root` (the root too), `:hidden-levels` (depths not drawn), `:group-labels`, `:leaf-labels`.

  Returns a vector of `{:x :y :r :depth :leaf :color :top-index :label}` (see [[cljplot2.data.hierarchy/label-text]]); the root only with `:root`, none at `:hidden-levels`."
  [tree {:keys [root hidden-levels group-labels leaf-labels]}]
  (let [total (:value tree)
        node-of (fn [node depth path parent-value top-index]
                  (let [leaf? (nil? (:children node))]
                    {:x (:x node) :y (:y node) :r (:r node) :depth depth :leaf leaf? :color (:color node)
                     :top-index top-index
                     :label (hierarchy/label-text (if leaf? leaf-labels group-labels) node
                                                  (hierarchy/node-info node depth path parent-value total))}))
        walk (fn walk [node depth path parent-value top-index]
               (let [top (if (m/one? (long depth)) (:top-index node) top-index)
                     drawn? (and (or root (m/pos? (long depth))) (not (contains? hidden-levels depth)))]
                 (cond-> (if drawn? [(node-of node depth path parent-value top)] [])
                   (:children node) (into (mapcat #(walk % (m/long-inc (long depth)) (cond-> path (some? (:name %)) (conj (:name %)))
                                                         (:value node) top)
                                                  (:children node))))))]
    (walk tree 0 [] total nil)))

(defn- circle!
  "Draws a filled and outlined circle.

  Parameters:

  - `canvas`: the canvas.
  - `node` (map): `:x`, `:y` - the centre, `:r` - the radius, in pixels.
  - `fill`: the fill colour.
  - `stroke`: the outline colour (`:none` - no outline).
  - `stroke-width` (number): the outline width in pixels (0 - no outline).
  - `pattern-fill`: the pattern drawn over the fill (see [[cljplot2.graphics.draw/fill-with-pattern!]]).

  Returns `nil`."
  [canvas {:keys [^double x ^double y ^double r]} fill stroke stroke-width pattern-fill]
  ;; more than 4 arguments: `stroke-width` unhinted
  (let [d (m/* 2.0 r)]
    (draw/fill-patterned! canvas fill pattern-fill #(c2d/ellipse % x y d d))
    (draw/outline! canvas stroke stroke-width #(c2d/ellipse % x y d d true))))

(defn- label-position
  "Places a label in a circle, if it fits.

  A leaf label is centred (its box's diagonal within the diameter); a group label sits under the circle's top (its box's top corners on the circle at most, the whole box above the centre).

  Parameters:

  - `node` (map): `:x`, `:y`, `:r` in pixels, `:leaf`.
  - `size` (`Vec2`): the label's natural size.

  Returns the label's pixel centre `[x y]`, or `nil` when it doesn't fit."
  [{:keys [^double x ^double y ^double r leaf]} ^Vec2 size]
  (let [w (.x size)
        h (.y size)]
    (if leaf
      (when (m/<= (m/hypot w h) (m/* 2.0 r))
        [x y])
      (when (m/<= w (m/* 2.0 r))
        ;; the box's top edge at distance `t` above the centre, where the circle's chord is the box's width
        (let [t (m/sqrt (m/- (m/* r r) (m/* 0.25 w w)))]
          (when (m/>= t h)
            [x (m/+ (m/- y t) (m/* 0.5 h))]))))))

(defn- covering-node
  "Gives the circle painted at a pixel.

  Parameters:

  - `nodes` (sequence): the drawn nodes, parents first (`:x`, `:y`, `:r` in pixels).
  - `x`, `y` (doubles): the pixel.

  Returns the last (the deepest) node containing the pixel, or `nil`."
  [nodes ^double x ^double y]
  (last (filter (fn [{^double nx :x ^double ny :y ^double r :r}] (m/<= (m/hypot (m/- x nx) (m/- y ny)) r)) nodes)))

(defn- label-placements
  "Gives the labels of the drawn nodes to draw, each fitting its circle and overlapping no label placed before it.

  Nodes come parents first, so an ancestor's label wins a collision.

  Parameters:

  - `nodes` (sequence): the drawn nodes, parents first (see [[drawn-nodes]]).
  - `size-of` (function): of a label content, its natural size (`Vec2`).
  - `fill-of` (function): of a node, its fill.
  - `line-size` (function): of a text role, its font size - circles narrower than one line aren't measured.

  Returns a vector of `{:content c :x px :y py :box b :backdrop f}`: `:backdrop` - the fill of the circle painted under the label's centre (for the role's `:contrast` colour).

  See also [[label-position]], [[covering-node]]."
  [nodes size-of fill-of line-size]
  (reduce (fn [placed {:keys [label leaf ^double r] :as node}]
            (let [role (if leaf :pack-leaf :pack-group)]
              (if-not (and label (m/>= (m/* 2.0 r) (double (line-size role))))
                placed
                (let [content {:kind :label :role role :text label}
                      ^Vec2 size (size-of content)
                      [px py] (label-position node size)
                      box (when px {:x (m/- (double px) (m/* 0.5 (.x size))) :y (m/- (double py) (m/* 0.5 (.y size)))
                                    :w (.x size) :h (.y size)})]
                  (if (or (nil? box) (some #(measure/boxes-overlap? box (:box %)) placed))
                    placed
                    (conj placed {:content content :x px :y py :box box
                                  :backdrop (fill-of (covering-node nodes px py))}))))))
          []
          nodes))

(defn draw
  "Packs and draws one panel's tree in the panel box: circles parents first, then their labels.

  Parameters:

  - `_chart`: the chart value (unused).
  - `computed` (map): the panel's stats - `:tree`, `:style`, `:options` and the colour keys read by [[cljplot2.data.hierarchy/fill-fn]] (see [[compute]]).
  - `canvas`: the panel canvas.
  - `box` (map): `:w`, `:h` - the panel size in pixels.
  - `scales` (map): the finalized scales.
  - `env` (map): the drawing environment (`:theme`).

  Returns `nil`; nothing is drawn without a tree, circles smaller than [[min-drawn-radius]] aren't drawn.

  See also [[label-placements]]."
  [_chart {:keys [tree style options] :as panel} canvas {:keys [w h]} scales env]
  (when tree
    (let [theme (:theme env)
          packed (layout/pack tree {:x 0.0 :y 0.0 :w (double w) :h (double h)} (:gap style))
          nodes (filterv #(m/>= (double (:r %)) min-drawn-radius) (drawn-nodes packed options))
          fill-of (hierarchy/fill-fn panel scales theme)
          pattern-fill (draw/pattern-fill-cache)
          stroke (:stroke-color style)
          sw (double (:stroke-width style))]
      (doseq [node nodes
              :let [fill (fill-of node)]]
        (circle! canvas node fill stroke sw (pattern-fill style fill)))
      (doseq [{:keys [content x y backdrop]} (label-placements nodes #(measure/natural-size % {:canvas canvas :theme theme})
                                                               fill-of #(get-in theme [:text % :size]))]
        (label/draw-at! canvas (assoc content :color (draw/text-color theme (:role content) backdrop))
                        [:center :middle] x y env)))))

(defn legend
  "Draws the key glyph: a filled circle inset in the key box, with the chart's pattern.

  Parameters:

  - `chart` (map): the chart value (its style options).
  - `attributes` (map): the key's attributes - `:color`.
  - `canvas`: the legend canvas.
  - `box` (map): `:x`, `:y`, `:w`, `:h` - the key box.
  - `env` (map): the legend environment (`:theme`).

  Returns `nil`."
  [chart attributes canvas box env]
  (let [fill (:color attributes)]
    (draw/circle-key! canvas box fill (draw/pattern-fill (theme/resolved-style (:theme env) chart) fill))))
