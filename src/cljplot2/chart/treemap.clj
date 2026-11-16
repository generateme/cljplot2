(ns cljplot2.chart.treemap
  "Treemap: nested rectangles from a hierarchy of columns, areas from a numeric value (a panel without axes).

  Mapping: `:value` (area, a positive finite number; other rows are dropped and counted) and `:hierarchy` (a
  non-empty vector of accessors, outer level first) - the tree of [[cljplot2.data.hierarchy]]. Optional `:color`
  aesthetic colors leaves (rows with the same path but different colors stay separate leaves); without it each
  top-level group takes the next `[:palette :color]` color, its leaves sharing it.

  Options (style defaults from theme `[:chart :treemap]`): `:layout` `:squarify` (default) or `:slice-dice`,
  `:group-labels` / `:leaf-labels` (default true - the node's name; `false` - none; a function of the node info
  `{:name :value :depth :path :share :parent-share}` (see [[cljplot2.data.hierarchy/node-info]]) returning the text,
  `nil` / `\"\"` - none; e.g. `#(format \"%s\\n%.1f%%\" (:name %) (* 100 (:share %)))`; unnamed nodes get no label),
  `:padding` (pixels inside every group), `:border-color`,
  `:border-width`. Group headers use the `:treemap-group` text role in a strip at the group's top (reserved only when
  the header fits the group's width); leaf labels the
  `:treemap-leaf` role, centered, drawn only when they fit. The layout is computed when drawing, from the panel box."
  (:require [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.graphics.canvas :as canvas]
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
  (into [(schema/optional :padding schema/non-neg)
         (schema/optional :border-color schema/color)
         (schema/optional :border-width schema/non-neg)]
        draw/pattern-style-entries))

(def schemas
  "Schemas of the treemap: `:style` (theme `[:chart :treemap]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (conj (schema/optional :layout [:enum :squarify :slice-dice])
                      (schema/nilable :group-labels hierarchy/label-option)
                      (schema/nilable :leaf-labels hierarchy/label-option)))
   :mapping hierarchy/mapping-schema})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :treemap]) (select-keys options [:padding :border-color :border-width :pattern :pattern-color])))

(defn compute
  "Per panel: `{:tree root :top-count n :style style :options {...} :colored? bool}`, `root` - see
  [[cljplot2.data.hierarchy]] (top-level children with `:top-index`, `:top-count` of them); aesthetic extents only
  (no x / y), `:na`, `:dropped {:value n :color n}`."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme] :as env}]
  (let [{:keys [trees top-count extents na dropped]} (hierarchy/panel-trees groups mapping env)
        st (style options theme)
        opts (merge {:layout :squarify :group-labels true :leaf-labels true}
                    (into {} (remove (comp nil? val)) (select-keys options [:layout :group-labels :leaf-labels])))]
    {:data (update-vals trees (fn [tree] {:tree tree :top-count top-count :style st :options opts
                                          :colored? (some? (:color mapping))}))
     :extents extents
     :na na
     :dropped dropped}))

;; drawing

(defn- pixel-box
  "Integer box of `rect` (rounded edges)."
  [{:keys [x y w h]}]
  (let [x0 (m/round (double x)) y0 (m/round (double y))
        x1 (m/round (+ (double x) (double w))) y1 (m/round (+ (double y) (double h)))]
    {:x x0 :y y0 :w (- x1 x0) :h (- y1 y0)}))

(defn- label-size
  ^Vec2 [role text env]
  (measure/natural-size {:kind :label :role role :text text} env))

(defn- draw-label!
  [c role text box env]
  (canvas/call-with-isolated-graphics c #(draw/draw-content {:kind :label :role role :text text} % box env)))

(defn- fill-box!
  "Fill `box` with `color` and the pattern of style `st` (`pattern-fill` - see
  [[cljplot2.graphics.draw/pattern-fill-cache]]), then its inner border."
  [c color {:keys [x y w h] :as box} {:keys [border-color border-width] :as st} pattern-fill]
  (c2d/set-color c color)
  (c2d/rect c (long x) (long y) (long w) (long h))
  (draw/fill-with-pattern! c (pattern-fill st color) #(c2d/rect c (long x) (long y) (long w) (long h)))
  (draw/inner-border! c box border-color border-width))

(defn- children-rects
  "`[child rect]` pairs of group `node`'s children laid out in `content` at `depth`."
  [node content depth layout]
  (if (= layout :slice-dice)
    (let [children (hierarchy/ordered-children node :data)]
      (mapv vector children (layout/slice-dice (map :value children) content depth)))
    (let [sorted (hierarchy/ordered-children node :value)]
      (mapv vector sorted (layout/squarify (map :value sorted) content)))))

(defn- label-text
  "Label of `node` at `at` (`{:depth :path :parent-value}`) for the labels option `labels`, see
  [[cljplot2.data.hierarchy/label-text]]."
  [labels node {:keys [depth path parent-value]} total]
  (when labels
    (hierarchy/label-text labels node (hierarchy/node-info node depth path parent-value total))))

(defn- draw-node!
  "Draw `node` in `rect` at `at` (`{:depth :path :parent-value}`, root depth 0) with fill `color` (`nil` for the
  root: no fill, no header)."
  [c node rect at color {:keys [style options leaf-color top-color pattern-fill total] :as ctx} env]
  (let [box (pixel-box rect)
        depth (long (:depth at))
        root? (zero? depth)]
    (when (and (or root? (and (>= (long (:w box)) 1) (>= (long (:h box)) 1))))
      (if (:children node)
        (let [padding (double (:padding style))
              {:keys [x y w h]} rect
              header-text (when-not root? (label-text (:group-labels options) node at total))
              ^Vec2 header-size (when header-text (label-size :treemap-group header-text env))
              ;; the header strip is reserved only when the header text fits the group's width
              header? (and header-size (<= (.x header-size) (- (double w) (* 2.0 padding))))
              header-h (if header? (.y header-size) 0.0)
              content {:x (+ (double x) padding) :y (+ (double y) padding header-h)
                       :w (- (double w) (* 2.0 padding)) :h (- (double h) (* 2.0 padding) header-h)}]
          (when-not root?
            (fill-box! c (c/brighten color) box (dissoc style :pattern) pattern-fill))
          (when header?
            (draw-label! c :treemap-group header-text
                         {:x (+ (long (:x box)) (long (m/round padding))) :y (+ (long (:y box)) (long (m/round padding)))
                          :w (m/max 0 (- (long (:w box)) (* 2 (long (m/round padding))))) :h (long (m/ceil header-h))}
                         env))
          (when (and (pos? (double (:w content))) (pos? (double (:h content))))
            (doseq [[child child-rect] (children-rects node content depth (:layout options))]
              (draw-node! c child child-rect
                          {:depth (inc depth) :path (cond-> (:path at) (some? (:name child)) (conj (:name child)))
                           :parent-value (:value node)}
                          (if root? (top-color child) color)
                          ctx env))))
        (let [fill (or (leaf-color node) color)]
          (when-not root?
            (fill-box! c fill box style pattern-fill))
          (when-let [text (label-text (:leaf-labels options) node at total)]
            (let [^Vec2 size (label-size :treemap-leaf text env)]
              (when (and (<= (.x size) (double (:w box))) (<= (.y size) (double (:h box))))
                (draw-label! c :treemap-leaf text box env)))))))))

(defn draw
  "Lay out and draw one panel's treemap in the panel `box`."
  [_chart {:keys [tree top-count style options colored?]} canvas {:keys [w h]} scales env]
  (when tree
    (let [env (assoc env :canvas canvas)
          theme (:theme env)
          na (get-in theme [:palette :na])
          color-fn (when colored? (aesthetic/attribute-fn :color (:color scales) na))
          palette-fn (hierarchy/top-color-fn theme top-count)
          ctx {:style style :options options :pattern-fill (draw/pattern-fill-cache) :total (:value tree)
               :leaf-color (fn [leaf] (when color-fn (color-fn (:color leaf))))
               :top-color (fn [child] (if colored? na (palette-fn (:top-index child))))}]
      (draw-node! canvas tree {:x 0.0 :y 0.0 :w (double w) :h (double h)} {:depth 0 :path [] :parent-value (:value tree)}
                  nil ctx env))))

(defn legend
  "Key glyph: a filled square inset in `box`, with the chart's `:pattern`."
  [{:keys [options]} attributes canvas {:keys [x y w h]} env]
  (let [side (* 0.7 (m/min (double w) (double h)))
        cx (+ (double x) (* 0.5 (double w))) cy (+ (double y) (* 0.5 (double h)))]
    (c2d/set-color canvas (:color attributes))
    (c2d/crect canvas cx cy side side)
    (draw/fill-with-pattern! canvas (draw/pattern-fill (style options (:theme env)) (:color attributes))
                             #(c2d/crect canvas cx cy side side))))
