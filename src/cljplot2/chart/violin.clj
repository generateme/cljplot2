(ns cljplot2.chart.violin
  "Violin plot: a kernel density estimate (see [[cljplot2.data.kde]]) of the values per level (the categorical axis, see
  `:level-axis` in [[cljplot2.data.strip]]) and group,
  mirrored around the item's centre line - a strip chart (see [[cljplot2.data.strip]]: placement `:place`, `:width`,
  `:thickness`, groups `:position`; `:mirror` with two groups draws a split violin, one half per group).

  Options (style defaults from theme `[:chart :violin]`): `:color` (fill), `:stroke-color` (outline: a color, `:none`
  or `:darker` - the darkened fill, default), `:stroke-width`,
  `:inner-color` (inner marks; `:none` - black or white, whichever reads better on the fill);
  `:kernel`, `:bandwidth`, `:cut` (default 0: the violin spans the data range), `:n` (evaluation points, default 100);
  `:scale` - widths: `:width` (default; every violin as wide as its slot), `:area` (equal areas: densities on one
  scale), `:count` (areas proportional to the counts); `:varwidth` (width factor `sqrt(n / max n)`, default false);
  `:inner` - `:box` (default; a thin IQR bar, a whisker line and a median dot), `:quartiles` (Q1 / Q3 dashed and the
  median across the violin), `:rug` (a tick per value), `:points` (values on the centre line), `:none`;
  `:padding {:x [lo hi]}` (default `[0.05 0.05]`). Items with fewer than 2 distinct values have no violin and are
  counted in `:dropped {:group n}`.

  Functions: [[legend]] - the chart contract's key glyph (the chart computes and draws through [[cljplot2.data.strip/compute]] and [[cljplot2.data.strip/draw]]); [[schemas]] - its style, options and mapping."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.chart.box :as box]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.data.group :as group]
            [cljplot2.data.kde :as kde]
            [cljplot2.data.rows :as data]
            [cljplot2.data.strip :as strip]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries
  "Map entries of the violin style (theme `[:chart :violin]`), also its options."
  (into [(schema/optional :color {:description "violin fill colour (a mapped :color wins)"} schema/solid-color)
         (schema/optional :stroke-color {:description "outline colour: a colour, :none or :darker (the darkened fill)"} draw/stroke-color)
         (schema/optional :stroke-width {:description "outline width in pixels"} schema/non-neg)
         (schema/optional :inner-color {:description "colour of the inner marks (:none - black or white, whichever reads better on the fill)"}
                          schema/color)]
        draw/pattern-style-entries))

(def schemas
  "Schemas of the violin chart: `:style` (theme `[:chart :violin]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into theme/theme-style-entries)
                (into strip/options)
                (conj (schema/optional :kernel {:description "density kernel (a fastmath kernel keyword)" :default :gaussian} keyword?)
                      (schema/optional :bandwidth {:description "kernel bandwidth in data units, or a fastmath bandwidth selector keyword"
                                                   :default :nrd0}
                                       [:or schema/pos keyword?])
                      (schema/nilable :n {:description "number of density evaluation points per violin" :default 100} [:and :int [:>= 2]])
                      (schema/nilable :cut {:description "how far the violin reaches past the data range, in bandwidths (0 - the data range)"
                                            :default 0.0}
                                      schema/non-neg)
                      (schema/optional :scale {:description "violin widths: :width (each as wide as its slot), :area (equal areas - densities on one scale) or :count (areas proportional to the counts)"
                                               :default :width}
                                       [:enum :width :area :count])
                      (schema/nilable :varwidth {:description "multiply each violin's width by the square root of its count relative to the largest"
                                                 :default false}
                                      schema/bool)
                      (schema/optional :inner {:description "inner marks: :box (IQR bar, whisker line, median dot), :quartiles (quartile lines across), :rug (a tick per value), :points (values on the centre line) or :none"
                                               :default :box}
                                       [:enum :box :quartiles :rug :points :none])
                      (schema/optional :padding {:description "padding of the data span per position axis, {:x [lo hi] :y [lo hi]} fractions of the span"
                                                 :cljplot2/default-doc "[0.05 0.05] along the value axis; with :place :bottom also :bottom-share below the data"}
                                       data/position-padding)))
   :mapping (strip/values-mapping
              [(schema/nilable :x {:description "values (data units) on the value axis, or levels for vertical violins"} schema/accessor)
               (schema/nilable :y {:description "levels on the categorical axis (absent - one violin), or values for vertical violins"}
                               schema/accessor)
               (schema/nilable :color {:description "groups, each a violin in its own fill colour"} schema/accessor)
               (schema/nilable :pattern {:description "groups, each a violin with its own fill pattern"} schema/accessor)
               (schema/nilable :group {:description "groups without a visual aesthetic, each its own violin"} schema/accessor)])})

(def ^{:const true :private true :tag 'long} default-points
  "Density evaluation points per violin when `:n` isn't given."
  100)

(defn- profile
  "Gives the kernel density profile of an item's values.

  Parameters:

  - `values` (sequence of numbers): the item's values, at least 2 distinct.
  - `options` (map): the chart's options - `:kernel`, `:bandwidth`, `:cut` (default 0 - the data range), `:n` (default 100 points).

  Returns `{:xs [..] :ds [..]}`: the evaluation points and the raw densities there.

  See also [[cljplot2.data.kde/estimate]], [[cljplot2.data.kde/curve]]."
  [values options]
  (let [e (kde/estimate values (assoc (select-keys options [:kernel :bandwidth]) :cut (data/option options :cut 0.0)))
        {:keys [x y]} (kde/curve e (kde/grid-points (:lo e) (:hi e) (long (or (:n options) default-points))))]
    {:xs x :ds y}))

(defn- compute-in-strip-frame
  "Computes the violin chart's stats in the strip frame (levels on `:y`).

  Parameters:

  - `chart` (map): the chart value - `:options` (the density options, `:scale` default `:width`, `:varwidth`, the strip options, style options).
  - `chart-split` (map): the chart's rows split per panel.
  - `env` (map): the compute environment - `:theme`, `:plot-scales` ...

  Returns `{:data :extents :padding :padding-explicit :padding-px :na :dropped}`: per panel `{:items [..] :style st :place p :options o :mirror? b}`; an item has `:xs`, `:widths` (densities scaled to `[0 1]` by `:scale` and `:varwidth`) and the [[cljplot2.chart.box/box-stats]] of its values; x extents - the density ranges; `:dropped` `:group` - items with fewer than 2 distinct values (no violin)."
  [{:keys [options] :as chart} chart-split {:keys [theme] :as env}]
  (let [{:keys [place position items panels dropped]} (strip/split chart-split options :center env)
        violin? #(m/>= (count (distinct (:values %))) 2)
        no-violin (count (remove violin? (mapcat val items)))
        profiled (update-vals items (fn [its] (mapv #(merge % (box/box-stats (:values %)) (profile (:values %) options))
                                                   (filter violin? its))))
        all (mapcat val profiled)
        scale-by (get options :scale :width)
        global-max (double (reduce m/max 0.0 (mapcat :ds all)))
        count-max (double (reduce m/max 0.0 (map #(m/* (double (:n %)) (double (reduce m/max 0.0 (:ds %)))) all)))
        factor (box/width-factors all (:varwidth options))
        widths (fn [{:keys [ds n] :as item}]
                 (let [own-max (double (reduce m/max 0.0 ds))
                       k (m/* (double (factor item))
                              (double (case scale-by
                                        :width (if (m/pos? own-max) (m// own-max) 0.0)
                                        :area (if (m/pos? global-max) (m// global-max) 0.0)
                                        :count (if (m/pos? count-max) (m// (double n) count-max) 0.0))))]
                   (mapv #(m/* k (double %)) ds)))
        sized (update-vals profiled (fn [its] (mapv #(-> % (assoc :widths (widths %)) (dissoc :ds)) its)))
        violin-style (theme/resolved-style theme chart)]
    {:data (update-vals sized (fn [its] {:items its :style violin-style :place place :options options
                                         :mirror? (= :mirror position)}))
     :extents (merge-with merge (group/aesthetic-extents panels (:plot-scales env))
                          (strip/extents place sized (fn [{:keys [xs]}] [(first xs) (peek xs)])))
     :padding (strip/padding place options)
     :padding-explicit (strip/explicit-padding place options)
     :padding-px (strip/n-label-room options sized env)
     :na (group/na-dims panels)
     :dropped (assoc dropped :group no-violin)}))

(defn- width-at
  "Gives the width of a violin at a value, linearly interpolated on its grid.

  Parameters:

  - `xs` (vector of doubles): the violin's evaluation points, increasing.
  - `widths` (vector of doubles): its widths in `[0 1]` at them.
  - `v` (double): the value.

  Returns the width; the first width below the grid, the last above it."
  ^double [xs widths ^double v]
  (let [n (count xs)
        found (long (java.util.Collections/binarySearch ^java.util.List xs v compare))
        i (if (m/neg? found) (m/long-dec (m/long-sub 0 found)) found)]
    (cond
      (m/<= i 0) (double (first widths))
      (m/>= i n) (double (peek widths))
      :else (let [x0 (double (xs (m/long-dec i)))
                  x1 (double (xs i))
                  w0 (double (widths (m/long-dec i)))
                  w1 (double (widths i))]
              (if (m/== x0 x1) w1 (m/+ w0 (m/* (m/- w1 w0) (m// (m/- v x0) (m/- x1 x0)))))))))

(defn- draw-in-strip-frame
  "Draws the violins of one panel in the strip frame: the outline polygon (filled), then the inner marks.

  Parameters:

  - `_chart` (map): the chart value (unused).
  - `computed` (map): the panel's stats - `:items` (with `:xs`, `:widths` and box statistics), `:style`, `:place`, `:options` (`:inner` default `:box`), `:mirror?` (split violins - one half per group).
  - `canvas`: the clojure2d canvas.
  - `box` (map): the panel box.
  - `scales` (map): finalized scales (in the strip frame).
  - `env` (map): the draw environment - `:theme`.

  A violin whose outline the scales can't place entirely is skipped, as is a group outside its scale's domain; the n labels sit past the upper end of the density.

  See also [[width-at]], [[cljplot2.data.strip/placement]]."
  [_chart {:keys [items style place options mirror?]} canvas box scales env]
  (let [{:keys [point] :as pl} (strip/placement box scales options place)
        inner (get options :inner :box)
        pattern-fill (draw/pattern-fill-cache)]
    (doseq [{:keys [level slot xs widths q1 median q3 lav uav values aesthetics]} items
            :let [st (group/style style aesthetics scales (:theme env))]
            :when st]
      (let [[^double lo ^double hi] slot
            ;; base line and outward extent: whole violin - centre ± half the slot; split half - from 0 outward
            base (if mirror? 0.0 (strip/in-slot slot 0.0))
            extent (if mirror? (m/+ lo hi) (strip/slot-half slot))
            outline (fn [^double side] (mapv (fn [x w] (point x level (m/+ base (m/* side extent (double w))))) xs widths))
            upper (outline 1.0)
            lower (if mirror? (mapv #(point % level base) xs) (outline -1.0))
            polygon (into upper (rseq lower))
            fill (:color st)
            stroke (draw/outline-color (:stroke-color st) fill)
            inner-color (if (theme/none? (:inner-color st)) (draw/contrast-color fill) (:inner-color st))
            sw (double (:stroke-width st))
            p (fn [v cross] (point v level cross))
            across (fn [v] (let [w (m/* extent (width-at xs widths (double v)))]
                             (if mirror? [(p v base) (p v (m/+ base w))] [(p v (m/- base w)) (p v (m/+ base w))])))]
        (when (every? measure/finite-point? polygon)
          (let [pixels (mapv (fn [^Vec2 q] [(.x q) (.y q)]) polygon)]
            (c2d/set-color canvas fill)
            (c2d/path canvas pixels true false)
            (draw/fill-with-pattern! canvas (pattern-fill st fill) #(c2d/path % pixels true false))
            (when-not (theme/none? stroke)
              (c2d/set-stroke-custom canvas {:size sw :join :round})
              (c2d/set-color canvas stroke)
              (c2d/path canvas pixels true true)))
          (c2d/set-color canvas inner-color)
          (case inner
            :box (do (c2d/set-stroke-custom canvas {:size (m/max 1.0 sw) :cap :butt})
                     (c2d/line canvas (p lav base) (p uav base))
                     (c2d/set-stroke-custom canvas {:size 5.0 :cap :butt})
                     (c2d/line canvas (p q1 base) (p q3 base))
                     (c2d/set-color canvas fill)
                     (let [^Vec2 median-pixel (p median base)] (c2d/ellipse canvas (.x median-pixel) (.y median-pixel) 4 4)))
            :quartiles (do (c2d/set-stroke-custom canvas {:size (m/max 1.0 sw) :cap :butt :dash [4.0 3.0]})
                           (doseq [q [q1 q3]] (apply c2d/line canvas (across q)))
                           (c2d/set-stroke-custom canvas {:size (m/max 1.0 sw) :cap :butt})
                           (apply c2d/line canvas (across median)))
            :rug (let [t (m/* 0.15 extent)]
                   (c2d/set-stroke-custom canvas {:size 1.0 :cap :butt})
                   (doseq [v values]
                     (if mirror?
                       (c2d/line canvas (p v base) (p v (m/+ base t)))
                       (c2d/line canvas (p v (m/- base t)) (p v (m/+ base t))))))
            :points (doseq [v values :let [^Vec2 q (p v base)]] (c2d/ellipse canvas (.x q) (.y q) 3 3))
            :none nil))))
    (strip/draw-n-labels! canvas pl items (fn [{:keys [xs]}] (peek xs)) options scales env)))

(def ^{:const true :private true :tag 'long} glyph-points
  "Segments along each side of the legend's violin silhouette."
  24)

(defmethod strip/in-frame-compute :violin [chart chart-split env] (compute-in-strip-frame chart chart-split env))

(defmethod strip/in-frame-draw :violin [chart computed canvas box scales env]
  (draw-in-strip-frame chart computed canvas box scales env))

(defn legend
  "Draws the violin chart's key glyph: a small horizontal violin silhouette centred in a legend key.

  The silhouette is a symmetric lens with pointed ends - half-width `(1 - t^2)` along `t` in `[-1 1]` - filled with `:color` (and the fill pattern) and outlined with `:stroke-color`.

  Parameters:

  - `chart` (map): the chart value.
  - `attributes` (map): the key's mapped style attributes - `:color` and `:pattern` are used.
  - `canvas`: the clojure2d canvas.
  - `box` (map): the key box `{:x :y :w :h}` in pixels.
  - `env` (map): the legend environment - `:theme`."
  [chart attributes canvas {:keys [^double x ^double y ^double w ^double h]} env]
  (let [st (merge (theme/resolved-style (:theme env) chart) (select-keys attributes [:color :pattern]))
        cx (m/+ x (m/* 0.5 w))
        cy (m/+ y (m/* 0.5 h))
        half-length (m/* 0.42 w)
        half-width (m/* 0.3 h)
        ts (m/slice-range -1.0 1.0 (m/long-inc glyph-points))
        side (fn [^double sign] (mapv (fn [^double t] [(m/+ cx (m/* t half-length)) (m/+ cy (m/* sign half-width (m/- 1.0 (m/* t t))))]) ts))
        outline (into (side 1.0) (rseq (side -1.0)))
        stroke (draw/outline-color (:stroke-color st) (:color st))]
    (c2d/set-color canvas (:color st))
    (c2d/path canvas outline true false)
    (draw/fill-with-pattern! canvas (draw/pattern-fill st (:color st)) #(c2d/path % outline true false))
    (when-not (theme/none? stroke)
      (c2d/set-stroke-custom canvas {:size (double (:stroke-width st)) :join :round})
      (c2d/set-color canvas stroke)
      (c2d/path canvas outline true true))))
