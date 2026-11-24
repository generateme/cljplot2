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
  `:border-width`; no legend - `:set-labels` (default true) writes the set names outside their circles, theme `:label-gap` pixels away (text role
  `:venn-set`; left out when their room would leave the diagram less than half the panel). `:region-labels`
  (default true - the count; `false` - none; a function of `{:sets [s ..] :size n :share f}` - `:share` of the
  panel's distinct items - returning the text) labels every region of the layout (text role `:venn-count`), in black
  or white by the color painted under it - always drawn, as data-to-viz asks: a label wider than a small region spills
  over its edges.

  Functions: [[schemas]] - the chart's schemas; [[compute]] - per panel the sets and region counts; [[draw]] - one panel's diagram; [[legend]] - none."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
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
  "Map entries of the venn style (theme `[:chart :venn]`; also options)."
  [(schema/optional :alpha {:description "fill opacity"} schema/fraction)
   (schema/optional :border-color {:description "circle outline colour: a colour, :none or :darker (the darkened set colour)"} draw/stroke-color)
   (schema/optional :border-width {:description "circle outline width in pixels"} schema/non-neg)])

(def schemas
  "Schemas of the Venn diagram: `:style` (theme `[:chart :venn]`), `:options`, `:mapping` (merged)."
  {:style (-> [:map {:closed true}]
             (into style-entries)
             (conj (schema/optional :label-gap {:description "pixels between a circle and its set name (theme only, not an option)"} schema/non-neg)))
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into theme/theme-style-entries)
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

(def ^:private defaults
  "Option defaults the drawing reads."
  {:proportional true :set-labels true :region-labels true})

(defn compute
  "Computes the Venn diagram's stats: per panel the sets, their sizes and the item count of every region.

  Parameters:

  - `chart` (map): the chart value - `:options` (see the namespace doc), `:kind`, `:schema` (its style keys).
  - `split` (map): `:groups` - `{panel rows}`, `:mapping` - `:set`, `:item`.
  - `env` (map): `:theme`.

  Returns `{:data {panel {:sets [s ..] :sizes [n ..] :regions {#{i ..} n} :total n :style st :options o}} :dropped {:set n}}`: regions keyed by set indices (see [[cljplot2.layout.venn]]); a panel without members is left out; `:dropped` - rows with a `nil` set or item.

  Throws `ex-info {:key :sets}` for 1 or more than 3 sets."
  [{:keys [options] :as chart} {:keys [groups mapping]} {:keys [theme]}]
  (let [set-of (data/row-accessor :set (:set mapping))
        order (sets/set-order (mapcat val groups) set-of (:sets options))
        n (count order)]
    (when (and (m/pos? n) (not (m/<= 2 n 3)))
      (schema/fail! (str "A Venn diagram shows 2 or 3 sets, got " n " - choose them with :sets, or use the UpSet recipe of cljplot2.data.sets.")
                    {:key :sets :value order}))
    (let [index (zipmap order (range))
          st (theme/resolved-style theme chart)
          opts (data/options-with-defaults defaults options)
          per-panel (update-vals groups (fn [rows] (sets/memberships rows mapping order)))]
      {:data (into {} (keep (fn [[panel {:keys [members]}]]
                              (when (and (m/pos? n) (seq members))
                                [panel {:sets order
                                        :sizes (mapv (frequencies (mapcat identity (vals members))) order)
                                        :regions (frequencies (map (fn [ss] (set (map index ss))) (vals members)))
                                        :total (count members)
                                        :style st :options opts}])))
                   per-panel)
       :dropped {:set (reduce m/long-add 0 (map :dropped (vals per-panel)))}})))

;; drawing

(defn- layout-circles
  "Gives the circles of a panel's diagram in layout units.

  Parameters:

  - `computed` (map): the panel's stats - `:sizes`, `:regions`, `:options` (`:proportional`).

  Returns a vector of `{:x :y :r}`: the classic layout for 3 sets, else two circles (proportional unless `:proportional false`).

  See also [[cljplot2.layout.venn/two-sets]], [[cljplot2.layout.venn/three-sets]]."
  [{:keys [sizes regions options]}]
  (if (m/== 3 (count sizes))
    (layout/three-sets)
    (layout/two-sets (or (first sizes) 0) (or (second sizes) 0) (get regions #{0 1} 0) (:proportional options))))

(defn- name-direction
  "Gives the direction in which a circle gets its set name.

  Parameters:

  - `circles` (vector): the layout's circles `{:x :y :r}`, centred at the origin.
  - `i` (long): the circle's index.

  Returns the unit vector `[dx dy]` from the layout's centre: left / right for 2 sets, away from the centre for 3."
  [circles ^long i]
  (if (m/== 2 (count circles))
    (if (m/zero? i) [-1.0 0.0] [1.0 0.0])
    (let [{:keys [^double x ^double y]} (circles i)
          l (m/hypot x y)]
      [(m// x l) (m// y l)])))

(defn- fit
  "Gives the mapping of layout points to panel pixels fitting the circles in the panel.

  The circles' bounding box is scaled to the panel less the margins on every side, centred.

  Parameters:

  - `circles` (sequence): the layout's circles `{:x :y :r}` (at least one).
  - `w`, `h` (numbers): the panel size in pixels.
  - `margin-x`, `margin-y` (numbers): pixels kept free on the left and right, top and bottom.

  Returns a function of a layout point `[x y]` giving its pixel `[px py]`, with the scale (pixels per layout unit, at least 0) as its `:scale` meta."
  [circles w h margin-x margin-y]
  ;; more than 4 arguments: no primitive hints
  (let [panel-w (double w)
        panel-h (double h)
        mx (double margin-x)
        my (double margin-y)
        x0 (v/mn (map (fn [{^double x :x ^double r :r}] (m/- x r)) circles))
        x1 (v/mx (map (fn [{^double x :x ^double r :r}] (m/+ x r)) circles))
        y0 (v/mn (map (fn [{^double y :y ^double r :r}] (m/- y r)) circles))
        y1 (v/mx (map (fn [{^double y :y ^double r :r}] (m/+ y r)) circles))
        scale (m/max 0.0 (m/min (m// (m/- panel-w (m/* 2.0 mx)) (m/max 1e-12 (m/- x1 x0)))
                                (m// (m/- panel-h (m/* 2.0 my)) (m/max 1e-12 (m/- y1 y0)))))
        cx (m/* 0.5 (m/+ x0 x1))
        cy (m/* 0.5 (m/+ y0 y1))]
    (with-meta (fn [[^double x ^double y]] [(m/+ (m/* 0.5 panel-w) (m/* scale (m/- x cx))) (m/+ (m/* 0.5 panel-h) (m/* scale (m/- y cy)))])
      {:scale scale})))

(defn draw
  "Draws one panel's diagram: the circles, the set names, the region labels.

  Set names are written only while their room (and the theme's `:label-gap`) leaves the diagram at least half the panel each way.

  Parameters:

  - `_chart`: the chart value (unused).
  - `computed` (map): the panel's stats - `:sets`, `:sizes`, `:regions`, `:total`, `:style`, `:options` (see [[compute]]).
  - `canvas`: the panel canvas.
  - `box` (map): `:w`, `:h` - the panel size in pixels.
  - `_scales`: the finalized scales (unused - colours come from the theme palette).
  - `env` (map): the drawing environment (`:theme`).

  Returns `nil`.

  See also [[fit]], [[name-direction]]."
  [_chart {:keys [sets regions total style options] :as panel} canvas {:keys [^double w ^double h]} _scales env]
  (let [theme (:theme env)
        circles (layout-circles panel)
        n (count circles)
        palette (aesthetic/palette-color-fn theme n)
        fills (mapv #(draw/with-alpha (palette %) (:alpha style)) (range n))
        name-contents (mapv (fn [set-name] {:kind :label :role :venn-set :text (str set-name)}) sets)
        name-sizes (mapv #(measure/natural-size % {:canvas canvas :theme theme}) name-contents)
        label-gap (double (:label-gap style))
        ^Vec2 largest (measure/max-size name-sizes)
        mx (m/+ label-gap (.x largest))
        my (m/+ label-gap (.y largest))
        ;; set names only while their room leaves the diagram at least half the panel each way
        names? (and (:set-labels options) (m/< (m/* 4.0 mx) w) (m/< (m/* 4.0 my) h))
        to-pixel (if names? (fit circles w h mx my) (fit circles w h 0.0 0.0))
        scale (double (:scale (meta to-pixel)))
        sw (double (:border-width style))]
    (doseq [[i {:keys [x y ^double r]}] (map-indexed vector circles)
            :let [[px py] (to-pixel [x y])
                  d (m/* 2.0 scale r)]
            :when (m/pos? d)]
      (c2d/set-color canvas (fills i))
      (c2d/ellipse canvas px py d d)
      (let [stroke (draw/outline-color (:border-color style) (palette i))]
        (when (and (m/pos? sw) (not (theme/none? stroke)))
          (c2d/set-stroke-custom canvas {:size sw})
          (c2d/set-color canvas stroke)
          (c2d/ellipse canvas px py d d true))))
    (when names?
      (doseq [[i {:keys [x y ^double r]}] (map-indexed vector circles)
              :when (m/pos? r)
              :let [[^double dx ^double dy] (name-direction circles i)
                    [^double px ^double py] (to-pixel [x y])
                    reach (m/+ (m/* scale r) label-gap)
                    theta (m/atan2 dx (m/- dy))]]
        (label/draw-at! canvas (name-contents i) (coord/outward-anchor theta)
                        (m/+ px (m/* reach dx)) (m/+ py (m/* reach dy)) env)))
    (when-let [labels (:region-labels options)]
      (doseq [[region point] (layout/region-points circles)
              :let [size (get regions region 0)
                    text (if (fn? labels)
                           (labels {:sets (mapv sets (sort region)) :size size
                                    :share (if (m/pos? (long total)) (m// (double size) (double total)) 0.0)})
                           (str size))]
              :when (and (some? text) (not= "" text))
              :let [[px py] (to-pixel point)]]
        (label/draw-at! canvas {:kind :label :role :venn-count :text (str text)
                                :color (draw/text-color (:theme env) :venn-count (or (draw/painted-color canvas px py) (draw/plot-backdrop (:theme env))))}
                        [:center :middle] px py env)))))

(defn legend
  "Draws no key glyph: the set names label the circles.

  Parameters: the legend arguments (chart, attributes, canvas, box, environment), unused.

  Returns `nil`."
  [_ _ _ _ _]
  nil)
