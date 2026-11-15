(ns cljplot2.chart.treemap
  "Treemap: nested rectangles from a hierarchy of columns, areas from a numeric value (a panel without axes).

  Mapping: `:value` (area, a positive finite number; other rows are dropped and counted) and `:hierarchy` (a
  non-empty vector of accessors, outer level first). A `nil` or `\"\"` level value ends the row's path at its parent;
  rows with an identical path merge (values summed). A row whose path stops at a group that also has deeper rows
  becomes an unnamed leaf of that group. Optional `:color` aesthetic colors leaves (rows with the same path but
  different colors stay separate leaves); without it each top-level group takes the next `[:palette :color]` color,
  its leaves sharing it.

  Options (style defaults from theme `[:chart :treemap]`): `:layout` `:squarify` (default) or `:slice-dice`,
  `:group-labels` / `:leaf-labels` (default true), `:padding` (pixels inside every group), `:border-color`,
  `:border-width`. Group headers use the `:treemap-group` text role in a strip at the group's top (reserved only when
  the header fits the group's width); leaf labels the
  `:treemap-leaf` role, centered, drawn only when they fit. The layout is computed when drawing, from the panel box."
  (:require [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.graphics.canvas :as canvas]
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
                      (schema/nilable :group-labels schema/bool)
                      (schema/nilable :leaf-labels schema/bool)))
   :mapping [:map [:value schema/accessor] [:hierarchy [:vector {:min 1} schema/accessor]]]})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :treemap]) (select-keys options [:padding :border-color :border-width :pattern :pattern-color])))

(defn- hierarchy-accessors
  [hierarchy]
  (mapv #(data/row-accessor :hierarchy %) hierarchy))

(defn- path-of
  "Level values of `row` up to the first `nil` / `\"\"`."
  [accessors row]
  (vec (take-while #(not (or (nil? %) (= "" %))) (map #(% row) accessors))))

(defn- leaves
  "Leaves named `name` from `entries` ending here: one per distinct color value (first-appearance order), values
  summed - rows differing only in color stay separate rectangles."
  [name entries]
  (let [by-color (group-by :color entries)]
    (mapv (fn [col] {:name name :value (reduce + 0.0 (map :value (by-color col))) :color col})
          (distinct (map :color entries)))))

(defn- nodes
  "Nodes named `name` from `entries` (`{:path rest-of-path :value v :color c}`): the leaves of `name` when no entry
  goes deeper, otherwise one group whose children are the deeper groups (first-appearance order) followed by unnamed
  leaves for the entries ending at the group."
  [name entries]
  (let [ends (filter #(empty? (:path %)) entries)
        deeper (remove #(empty? (:path %)) entries)]
    (if (empty? deeper)
      (leaves name ends)
      (let [by-first (group-by #(first (:path %)) deeper)
            children (-> (into [] (mapcat (fn [k] (nodes k (map #(update % :path subvec 1) (by-first k)))))
                               (distinct (map #(first (:path %)) deeper)))
                         (into (leaves nil ends)))]
        [{:name name :value (reduce + 0.0 (map :value children)) :children children}]))))

(defn- root
  "Root group of `entries`: its children are the top-level nodes (always a group, also for a single leaf)."
  [entries]
  (let [[n & more] (nodes nil entries)]
    (if (and (:children n) (nil? more))
      n
      {:name nil :value (reduce + 0.0 (map :value (cons n more))) :children (vec (cons n more))})))

(defn compute
  "Per panel: `{:tree root :top-count n :style style :options {...} :colored? bool}`, `root` -
  `{:name nil :value :children}` (always a group), top-level children with `:top-index`
  (first-appearance order over all panels, `:top-count` of them), leaves `{:name :value :color}`; aesthetic extents
  only (no x / y), `:na`, `:dropped {:value n :color n}`."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme na-level? plot-scales] :or {na-level? true}}]
  (let [st (style options theme)
        value-of (data/row-accessor :value (:value mapping))
        accessors (hierarchy-accessors (:hierarchy mapping))
        color-of (when (some? (:color mapping)) (data/row-accessor :color (:color mapping)))
        color-spec (get plot-scales :color {})
        value-ok? #(let [v (value-of %)] (and (number? v) (m/valid-double? (double v)) (pos? (double v))))
        color-ok? (fn [row] (or (nil? color-of)
                                (let [cv (color-of row)]
                                  (if (nil? cv)
                                    na-level?
                                    (aesthetic/kept-value? :color color-spec cv)))))
        all-rows (mapcat val groups)
        trees (update-vals groups
                           (fn [rows]
                             (let [kept (filter #(and (value-ok? %) (color-ok? %)) rows)]
                               (when (seq kept)
                                 (root (map (fn [r] {:path (path-of accessors r) :value (double (value-of r))
                                                         :color (when color-of (color-of r))})
                                                kept))))))
        colors (fn [tree] (when color-of (keep :color (tree-seq :children :children tree))))
        ;; top-level values in first-appearance order over all panels: default colors agree across panels
        top-index (zipmap (distinct (map #(first (path-of accessors %)) (filter #(and (value-ok? %) (color-ok? %)) all-rows)))
                          (range))
        index-tops (fn [tree] (update tree :children (fn [cs] (mapv #(assoc % :top-index (top-index (:name %) 0)) cs))))]
    {:data (into {} (keep (fn [[panel tree]]
                            (when tree [panel {:tree (index-tops tree) :top-count (count top-index) :style st
                                               :options (merge {:layout :squarify :group-labels true :leaf-labels true}
                                                               (into {} (remove (comp nil? val))
                                                                     (select-keys options [:layout :group-labels :leaf-labels])))
                                               :colored? (some? color-of)}])))
                 trees)
     :extents (into {} (keep (fn [[panel tree]]
                               (when-let [e (some->> tree colors seq (aesthetic/value-extent :color color-spec))]
                                 [panel {:color e}])))
                    trees)
     :na (if (and color-of (some (fn [[_ tree]] (some #(and (contains? % :color) (nil? (:color %)) (not (:children %)))
                                                       (tree-seq :children :children tree)))
                                 (filter val trees)))
           #{:color} #{})
     :dropped (cond-> {:value (count (remove value-ok? all-rows))}
                color-of (assoc :color (count (remove color-ok? (filter value-ok? all-rows)))))}))

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
  [children content depth layout]
  (if (= layout :slice-dice)
    (mapv vector children (layout/slice-dice (map :value children) content depth))
    (let [sorted (vec (sort-by (comp - double :value) children))]
      (mapv vector sorted (layout/squarify (map :value sorted) content)))))

(defn- draw-node!
  "Draw `node` in `rect` at `depth` with fill `color` (`nil` for the root: no fill, no header)."
  [c node rect depth color {:keys [style options leaf-color top-color pattern-fill] :as ctx} env]
  (let [box (pixel-box rect)
        depth (long depth)
        root? (zero? depth)]
    (when (and (or root? (and (>= (long (:w box)) 1) (>= (long (:h box)) 1))))
      (if-let [children (:children node)]
        (let [padding (double (:padding style))
              {:keys [x y w h]} rect
              ^Vec2 header-size (when (and (not root?) (:group-labels options) (some? (:name node)))
                                  (label-size :treemap-group (str (:name node)) env))
              ;; the header strip is reserved only when the header text fits the group's width
              header? (and header-size (<= (.x header-size) (- (double w) (* 2.0 padding))))
              header-h (if header? (.y header-size) 0.0)
              content {:x (+ (double x) padding) :y (+ (double y) padding header-h)
                       :w (- (double w) (* 2.0 padding)) :h (- (double h) (* 2.0 padding) header-h)}]
          (when-not root?
            (fill-box! c (c/brighten color) box (dissoc style :pattern) pattern-fill))
          (when header?
            (draw-label! c :treemap-group (str (:name node))
                         {:x (+ (long (:x box)) (long (m/round padding))) :y (+ (long (:y box)) (long (m/round padding)))
                          :w (m/max 0 (- (long (:w box)) (* 2 (long (m/round padding))))) :h (long (m/ceil header-h))}
                         env))
          (when (and (pos? (double (:w content))) (pos? (double (:h content))))
            (doseq [[child child-rect] (children-rects children content depth (:layout options))]
              (draw-node! c child child-rect (inc depth)
                          (if root? (top-color child) color)
                          ctx env))))
        (let [fill (or (leaf-color node) color)]
          (when-not root?
            (fill-box! c fill box style pattern-fill))
          (when (and (:leaf-labels options) (some? (:name node)))
            (let [^Vec2 size (label-size :treemap-leaf (str (:name node)) env)]
              (when (and (<= (.x size) (double (:w box))) (<= (.y size) (double (:h box))))
                (draw-label! c :treemap-leaf (str (:name node)) box env)))))))))

(defn draw
  "Lay out and draw one panel's treemap in the panel `box`."
  [_chart {:keys [tree top-count style options colored?]} canvas {:keys [w h]} scales env]
  (when tree
    (let [env (assoc env :canvas canvas)
          theme (:theme env)
          na (get-in theme [:palette :na])
          color-fn (when colored? (aesthetic/attribute-fn :color (:color scales) na))
          palette-fn (aesthetic/attribute-fn :color (aesthetic/scale-spec :color {:kind :ordinal :domain (vec (range (max 1 (long top-count))))}
                                                                          nil false (:palette theme))
                                             na)
          ctx {:style style :options options :pattern-fill (draw/pattern-fill-cache)
               :leaf-color (fn [leaf] (when color-fn (color-fn (:color leaf))))
               :top-color (fn [child] (if colored? na (palette-fn (:top-index child))))}]
      (draw-node! canvas tree {:x 0.0 :y 0.0 :w (double w) :h (double h)} 0 nil ctx env))))

(defn legend
  "Key glyph: a filled square inset in `box`, with the chart's `:pattern`."
  [{:keys [options]} attributes canvas {:keys [x y w h]} env]
  (let [side (* 0.7 (m/min (double w) (double h)))
        cx (+ (double x) (* 0.5 (double w))) cy (+ (double y) (* 0.5 (double h)))]
    (c2d/set-color canvas (:color attributes))
    (c2d/crect canvas cx cy side side)
    (draw/fill-with-pattern! canvas (draw/pattern-fill (style options (:theme env)) (:color attributes))
                             #(c2d/crect canvas cx cy side side))))
