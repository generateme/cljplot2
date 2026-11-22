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
  box: circle sizes compare within a panel, not across panels. `:padding` (pixels, as d3's) - the gap between
  siblings and between a child and its parent's edge; a panel too small for such gaps gets the widest it holds.

  Colors (as the partition): without a mapped `:color` each top-level node takes the next `[:palette :color]` color,
  the nodes below it lighter by depth (see [[cljplot2.data.hierarchy/depth-shade]]); a mapped `:color` colors the
  leaves (legend keys), groups take `[:palette :na]`. `:root true` draws the root circle too (in `[:palette :na]`);
  `:hidden-levels` - a set of depths (root 0, top level 1) whose circles and labels aren't drawn, the layout
  unchanged. Style (theme `[:chart :pack]`): `:padding`, `:border-color`, `:border-width` (the circles' outlines),
  `:pattern`, `:pattern-color`.

  Labels: `:group-labels` / `:leaf-labels` (default true - the node's name; `false` - none; a function of the node
  info `{:name :value :depth :path :share :parent-share}` (see [[cljplot2.data.hierarchy/node-info]]) returning the
  text, `nil` / `\"\"` - none; unnamed nodes get no label), text roles `:pack-group` / `:pack-leaf`, in black or white
  by the circle painted under the label's centre, drawn after all circles and only when they fit: a leaf label
  centred (its box's diagonal within the diameter); a group label inside the circle under its top (its box's top
  corners on the circle at most, the box above the centre) - it may cover child circles. A label overlapping one
  placed before it (parents first: an ancestor's) isn't drawn."
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
  (into [(schema/optional :padding {:description "gap between siblings and between a child and its parent's edge, in pixels"}
                          schema/non-neg)
         (schema/optional :border-color {:description "circle border colour"} schema/color)
         (schema/optional :border-width {:description "circle border width in pixels"} schema/non-neg)]
        draw/pattern-style-entries))

(def schemas
  "Schemas of the circle packing: `:style` (theme `[:chart :pack]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into theme/theme-style-entries)
                (conj (schema/optional :order {:description "packing order of siblings: :value (descending, largest near the centre) or :data"
                                               :default :value}
                                       [:enum :value :data])
                      (schema/nilable :root {:description "draw the root circle too" :default false} schema/bool)
                      (schema/nilable :hidden-levels {:description "depths (root 0, top level 1) not drawn, the layout unchanged"
                                                      :default #{}}
                                      [:set [:int {:min 0}]]))
                (into hierarchy/label-entries))
   :mapping hierarchy/mapping-schema})

(def ^:private defaults {:order :value :root false :hidden-levels #{} :group-labels true :leaf-labels true})

(def ^:private ^:const min-drawn-radius
  "Circles with a smaller radius (pixels) aren't drawn."
  0.5)

(defn- style
  [options theme]
  (merge (theme/chart-style theme :pack options) (select-keys options [:padding :border-color :border-width :pattern :pattern-color])))

(defn compute
  "Per panel: `{:tree root :top-count n :style style :options {...} :colored? bool}` (`root` - see
  [[cljplot2.data.hierarchy]], children in `:order`); aesthetic extents only (no x / y), `:na`, `:dropped {:value n
  :color n}`."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme] :as env}]
  (let [{:keys [trees top-count extents na dropped]} (hierarchy/panel-trees groups mapping env)
        opts (merge defaults (into {} (remove (comp nil? val)) (select-keys options (keys defaults))))
        st (style options theme)]
    {:data (update-vals trees (fn [tree] {:tree (hierarchy/ordered-tree tree (:order opts)) :top-count top-count :style st
                                          :options opts :colored? (some? (:color mapping))}))
     :extents extents
     :na na
     :dropped dropped}))

;; drawing

(defn- drawn-nodes
  "Nodes of the packed `tree` to draw, parents first: `{:x :y :r :depth :leaf? :color :top-index :label}` (see
  [[cljplot2.data.hierarchy/label-text]]); the root only with `:root`, none at `:hidden-levels`."
  [tree {:keys [root hidden-levels group-labels leaf-labels]}]
  (let [total (:value tree)
        node-of (fn [node depth path parent-value top-index]
                  (let [leaf? (nil? (:children node))]
                    {:x (:x node) :y (:y node) :r (:r node) :depth depth :leaf? leaf? :color (:color node)
                     :top-index top-index
                     :label (hierarchy/label-text (if leaf? leaf-labels group-labels) node
                                                  (hierarchy/node-info node depth path parent-value total))}))
        walk (fn walk [node depth path parent-value top-index]
               (let [top (if (== 1 (long depth)) (:top-index node) top-index)
                     drawn? (and (or root (pos? (long depth))) (not (contains? hidden-levels depth)))]
                 (cond-> (if drawn? [(node-of node depth path parent-value top)] [])
                   (:children node) (into (mapcat #(walk % (inc (long depth)) (cond-> path (some? (:name %)) (conj (:name %)))
                                                         (:value node) top)
                                                  (:children node))))))]
    (walk tree 0 [] total nil)))

(defn- circle!
  "Fill circle `node` with `fill` and the pattern of `pattern-fill`, outline it with `stroke` (`stroke-width` > 0)."
  [canvas {:keys [x y r]} fill stroke stroke-width pattern-fill]
  (let [x (double x) y (double y) d (* 2.0 (double r)) stroke-width (double stroke-width)]
    (c2d/set-color canvas fill)
    (c2d/ellipse canvas x y d d)
    (draw/fill-with-pattern! canvas pattern-fill #(c2d/ellipse canvas x y d d))
    (when (and (pos? stroke-width) (not (theme/none? stroke)))
      (c2d/set-stroke-custom canvas {:size stroke-width})
      (c2d/set-color canvas stroke)
      (c2d/ellipse canvas x y d d true))))

(defn- label-position
  "Pixel centre of a label of natural `size` in circle `{:x :y :r}`, or `nil` when it doesn't fit: leaves centred
  (the box's diagonal within the diameter), groups under the circle's top (the box's top corners on the circle at
  most, the whole box above the centre)."
  [{:keys [x y r leaf?]} ^Vec2 size]
  (let [x (double x) y (double y) r (double r)
        w (.x size) h (.y size)]
    (if leaf?
      (when (<= (m/hypot w h) (* 2.0 r))
        [x y])
      (when (<= w (* 2.0 r))
        ;; the box's top edge at distance `t` above the centre, where the circle's chord is the box's width
        (let [t (m/sqrt (- (* r r) (* 0.25 w w)))]
          (when (>= t h)
            [x (+ (- y t) (* 0.5 h))]))))))

(defn- covering-node
  "The last of the drawn `nodes` (parents first: the deepest) containing pixel `(x, y)` - the circle painted there."
  [nodes ^double x ^double y]
  (last (filter #(<= (m/hypot (- x (double (:x %))) (- y (double (:y %)))) (double (:r %))) nodes)))

(defn- boxes-overlap?
  [a b]
  (and (< (double (:x a)) (+ (double (:x b)) (double (:w b)))) (< (double (:x b)) (+ (double (:x a)) (double (:w a))))
       (< (double (:y a)) (+ (double (:y b)) (double (:h b)))) (< (double (:y b)) (+ (double (:y a)) (double (:h a))))))

(defn- label-placements
  "Labels of the drawn `nodes` (parents first) to draw: `[{:content c :x px :y py :box b :backdrop f} ..]` - each
  fitting its circle (see [[label-position]]) and overlapping no label placed before it (an ancestor's label wins);
  `:backdrop` - the fill of the circle painted under its centre (for the role's `:contrast` colour). `size-of` - natural size of a label content, `fill-of` - a node's
  fill, `line-size` - a role's font size (circles narrower than one line aren't measured)."
  [nodes size-of fill-of line-size]
  (reduce (fn [placed {:keys [label leaf? r] :as node}]
            (let [role (if leaf? :pack-leaf :pack-group)]
              (if-not (and label (>= (* 2.0 (double r)) (double (line-size role))))
                placed
                (let [content {:kind :label :role role :text label}
                      ^Vec2 size (size-of content)
                      [px py] (label-position node size)
                      box (when px {:x (- (double px) (* 0.5 (.x size))) :y (- (double py) (* 0.5 (.y size)))
                                    :w (.x size) :h (.y size)})]
                  (if (or (nil? box) (some #(boxes-overlap? box (:box %)) placed))
                    placed
                    (conj placed {:content content :x px :y py :box box
                                  :backdrop (fill-of (covering-node nodes px py))}))))))
          []
          nodes))

(defn draw
  "Pack and draw one panel's tree in the panel `box`: circles parents first, then their labels."
  [_chart {:keys [tree style options] :as panel} canvas {:keys [w h]} scales env]
  (when tree
    (let [theme (:theme env)
          packed (layout/pack tree {:x 0.0 :y 0.0 :w (double w) :h (double h)} (:padding style))
          nodes (filterv #(>= (double (:r %)) min-drawn-radius) (drawn-nodes packed options))
          fill-of (hierarchy/fill-fn panel scales theme)
          pattern-fill (draw/pattern-fill-cache)
          stroke (:border-color style)
          sw (double (:border-width style))]
      (doseq [node nodes
              :let [fill (fill-of node)]]
        (circle! canvas node fill stroke sw (pattern-fill style fill)))
      (doseq [{:keys [content x y backdrop]} (label-placements nodes #(measure/natural-size % {:canvas canvas :theme theme})
                                                               fill-of #(get-in theme [:text % :size]))]
        (label/draw-at! canvas (assoc content :color (draw/text-color theme (:role content) backdrop))
                        [:center :middle] x y env)))))

(defn legend
  "Key glyph: a filled circle inset in `box`, with the chart's `:pattern`."
  [{:keys [options]} attributes canvas {:keys [x y w h]} env]
  (let [st (style options (:theme env))
        d (* 0.7 (m/min (double w) (double h)))
        cx (+ (double x) (* 0.5 (double w))) cy (+ (double y) (* 0.5 (double h)))
        fill (:color attributes)]
    (c2d/set-color canvas fill)
    (c2d/ellipse canvas cx cy d d)
    (draw/fill-with-pattern! canvas (draw/pattern-fill st fill) #(c2d/ellipse canvas cx cy d d))))
