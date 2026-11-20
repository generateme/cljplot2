(ns cljplot2.chart.venn
  "Venn diagram of 2 or 3 sets with the item count of every region (a panel without axes, cartesian plots only).
  data-to-viz: at most 3 sets, numbers in every region - for more sets use the UpSet recipe of
  [[cljplot2.data.sets]].

  Mapping: `:set` and `:item` - one row per item in a set (repeated rows count once; rows with a `nil` set or item
  are dropped and counted). Sets in first-appearance order over all panels, or `:sets` (a vector) choosing and
  ordering them; 1 or more than 3 sets throw `ex-info {:key :sets}`. Facets: a diagram per panel, the same sets.

  Layout (when drawing, fitted to the panel with room for the set names, centred; see [[cljplot2.layout.venn]]):
  2 sets - circle areas proportional to the set sizes and the overlap to the intersection (`:proportional false` -
  two equal circles); 3 sets - the classic layout of equal circles (not proportional: `:proportional` doesn't apply).

  Drawing (style defaults from theme `[:chart :venn]`): each set the next `[:palette :color]` color, filled with
  `:alpha` (overlaps mix), outlined with `:border-color` (a color, `:none` or `:darker` - the darkened set color) and
  `:border-width`; no legend - `:set-labels` (default true) writes the set names outside their circles (text role
  `:venn-set`; left out when their room would leave the diagram less than half the panel). `:region-labels`
  (default true - the count; `false` - none; a function of `{:sets [s ..] :size n :share f}` - `:share` of the
  panel's distinct items - returning the text) labels every region of the layout (text role `:venn-count`), in black
  or white by the color painted under it - always drawn, as data-to-viz asks: a label wider than a small region spills
  over its edges."
  (:require [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.content.label :as label]
            [cljplot2.data.rows :as data]
            [cljplot2.data.sets :as sets]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.layout.venn :as layout]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.coord :as coord]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries
  [(schema/optional :alpha {:description "fill opacity"} schema/fraction)
   (schema/optional :border-color {:description "circle outline colour: a colour, :none or :darker (the darkened set colour)"} draw/stroke-color)
   (schema/optional :border-width {:description "circle outline width in pixels"} schema/non-neg)])

(def schemas
  "Schemas of the Venn diagram: `:style` (theme `[:chart :venn]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (conj (schema/optional :sets {:description "the sets shown (2 or 3), in order"
                                              :cljplot2/default-doc "every set, in first-appearance order"}
                                       [:vector {:min 1} any?])
                      (schema/nilable :proportional {:description "2 sets: circle and overlap areas proportional to the counts (false - equal circles)"
                                                     :default true}
                                      schema/bool)
                      (schema/nilable :set-labels {:description "write the set names outside their circles" :default true} schema/bool)
                      (schema/nilable :region-labels {:description (str "region labels: true - the item count, false - none, or a function of "
                                                                        "{:sets :size :share} returning the text")
                                                      :default true}
                                      [:or schema/bool schema/function])))
   :mapping [:map
             [:set {:description "the set a row's item belongs to"} schema/accessor]
             [:item {:description "the item (repeated rows count once)"} schema/accessor]]})

(def ^:private ^:const label-gap
  "Pixels between a circle and its set name."
  4.0)

(defn- style
  [options theme]
  (merge (get-in theme [:chart :venn]) (select-keys options [:alpha :border-color :border-width])))

(defn compute
  "Per panel: `{:sets [s ..] :sizes [n ..] :regions {#{i ..} n} :total n :style st :options o}` (regions keyed by set
  indices, see [[cljplot2.layout.venn]]); `:dropped {:set n}` (rows with a `nil` set or item). Throws `ex-info {:key
  :sets}` for 1 or more than 3 sets."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme]}]
  (let [set-of (data/row-accessor :set (:set mapping))
        order (sets/set-order (mapcat val groups) set-of (:sets options))
        n (count order)]
    (when (and (pos? n) (not (<= 2 n 3)))
      (schema/fail! (str "A Venn diagram shows 2 or 3 sets, got " n " - choose them with :sets, or use the UpSet recipe of cljplot2.data.sets.")
                    {:key :sets :value order}))
    (let [index (zipmap order (range))
          st (style options theme)
          opts (merge {:proportional true :set-labels true :region-labels true}
                      (into {} (remove (comp nil? val)) (select-keys options [:proportional :set-labels :region-labels])))
          per-panel (update-vals groups (fn [rows] (sets/memberships rows mapping order)))]
      {:data (into {} (keep (fn [[panel {:keys [members]}]]
                              (when (and (pos? n) (seq members))
                                [panel {:sets order
                                        :sizes (mapv (frequencies (mapcat identity (vals members))) order)
                                        :regions (frequencies (map (fn [ss] (set (map index ss))) (vals members)))
                                        :total (count members)
                                        :style st :options opts}])))
                   per-panel)
       :dropped {:set (reduce + 0 (map :dropped (vals per-panel)))}})))

;; drawing

(defn- layout-circles
  "Circles (layout units) of a panel: see [[cljplot2.layout.venn]]."
  [{:keys [sizes regions options]}]
  (if (= 3 (count sizes))
    (layout/three-sets)
    (layout/two-sets (or (first sizes) 0) (or (second sizes) 0) (get regions #{0 1} 0) (:proportional options))))

(defn- name-direction
  "Unit direction `[dx dy]` from the layout's centre in which circle `i` of `circles` gets its set name."
  [circles i]
  (if (= 2 (count circles))
    (if (zero? (long i)) [-1.0 0.0] [1.0 0.0])
    (let [{:keys [x y]} (circles i)
          l (m/hypot (double x) (double y))]
      [(/ (double x) l) (/ (double y) l)])))

(defn- fit
  "Function of a layout point `[x y]` to the panel pixel, scaling `circles` to the `w` x `h` panel less `margin-x` /
  `margin-y` on every side, centred; with the scale as `:scale` meta."
  [circles w h margin-x margin-y]
  (let [w (double w) h (double h) margin-x (double margin-x) margin-y (double margin-y)
        x0 (double (reduce min (map #(- (double (:x %)) (double (:r %))) circles)))
        x1 (double (reduce max (map #(+ (double (:x %)) (double (:r %))) circles)))
        y0 (double (reduce min (map #(- (double (:y %)) (double (:r %))) circles)))
        y1 (double (reduce max (map #(+ (double (:y %)) (double (:r %))) circles)))
        scale (m/max 0.0 (m/min (/ (- w (* 2.0 margin-x)) (m/max 1e-12 (- x1 x0)))
                                (/ (- h (* 2.0 margin-y)) (m/max 1e-12 (- y1 y0)))))
        cx (* 0.5 (+ x0 x1)) cy (* 0.5 (+ y0 y1))]
    (with-meta (fn [[x y]] [(+ (* 0.5 w) (* scale (- (double x) cx))) (+ (* 0.5 h) (* scale (- (double y) cy)))])
      {:scale scale})))

(defn draw
  "Draw one panel's diagram: circles, set names, region labels."
  [_chart {:keys [sets sizes regions total style options] :as panel} canvas {:keys [w h]} _scales env]
  (let [theme (:theme env)
        circles (layout-circles panel)
        n (count circles)
        palette (aesthetic/palette-color-fn theme n)
        fills (mapv #(c/set-alpha (c/to-color (palette %)) (* 255.0 (double (:alpha style)))) (range n))
        name-contents (mapv (fn [s] {:kind :label :role :venn-set :text (str s)}) sets)
        name-sizes (mapv #(measure/natural-size % {:canvas canvas :theme theme}) name-contents)
        mx (+ label-gap (double (reduce max 0.0 (map #(.x ^Vec2 %) name-sizes))))
        my (+ label-gap (double (reduce max 0.0 (map #(.y ^Vec2 %) name-sizes))))
        ;; set names only while their room leaves the diagram at least half the panel each way
        names? (and (:set-labels options) (< (* 4.0 mx) (double w)) (< (* 4.0 my) (double h)))
        to-pixel (if names? (fit circles w h mx my) (fit circles w h 0.0 0.0))
        scale (double (:scale (meta to-pixel)))
        sw (double (:border-width style))]
    (doseq [[i {:keys [x y r]}] (map-indexed vector circles)
            :let [[px py] (to-pixel [x y]) d (* 2.0 scale (double r))]
            :when (pos? d)]
      (c2d/set-color canvas (fills i))
      (c2d/ellipse canvas px py d d)
      (let [stroke (draw/outline-color (:border-color style) (palette i))]
        (when (and (pos? sw) (not (theme/none? stroke)))
          (c2d/set-stroke-custom canvas {:size sw})
          (c2d/set-color canvas stroke)
          (c2d/ellipse canvas px py d d true))))
    (when names?
      (doseq [[i {:keys [x y r]}] (map-indexed vector circles)
              :when (pos? (double r))
              :let [[dx dy] (name-direction circles i)
                    [px py] (to-pixel [x y])
                    reach (+ (* scale (double r)) label-gap)
                    theta (m/atan2 (double dx) (- (double dy)))]]
        (label/draw-at! canvas (name-contents i) (coord/outward-anchor theta)
                        (+ (double px) (* reach (double dx))) (+ (double py) (* reach (double dy))) env)))
    (when-let [labels (:region-labels options)]
      (doseq [[region point] (layout/region-points circles)
              :let [size (get regions region 0)
                    text (if (fn? labels)
                           (labels {:sets (mapv sets (sort region)) :size size
                                    :share (if (pos? (long total)) (/ (double size) (double total)) 0.0)})
                           (str size))]
              :when (and (some? text) (not= "" text))
              :let [[px py] (to-pixel point)]]
        (label/draw-at! canvas {:kind :label :role :venn-count :text (str text)
                                :color (draw/text-color (:theme env) :venn-count (or (draw/painted-color canvas px py) :white))}
                        [:center :middle] px py env)))))

(defn legend
  "No legend: the set names label the circles."
  [_ _ _ _ _]
  nil)
