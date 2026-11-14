(ns cljplot2.chart.violin
  "Violin plot: a kernel density estimate (see [[cljplot2.data.kde]]) of the `:x` values per `:y` level and group,
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
  counted in `:dropped {:group n}`."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.chart.box :as box]
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
  [(schema/optional :color schema/solid-color)
   (schema/optional :stroke-color draw/stroke-color)
   (schema/optional :stroke-width schema/non-neg)
   (schema/optional :inner-color schema/color)])

(def schemas
  "Schemas of the violin chart: `:style` (theme `[:chart :violin]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into strip/options)
                (conj (schema/optional :kernel keyword?)
                      (schema/optional :bandwidth [:or schema/pos keyword?])
                      (schema/nilable :n [:and :int [:>= 2]])
                      (schema/nilable :cut schema/non-neg)
                      (schema/optional :scale [:enum :width :area :count])
                      (schema/nilable :varwidth schema/bool)
                      (schema/optional :inner [:enum :box :quartiles :rug :points :none])
                      (schema/optional :padding data/position-padding)))
   :mapping [:map [:x schema/accessor]]})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :violin]) (select-keys options [:color :stroke-color :stroke-width :inner-color])))

(defn- profile
  "`{:xs [..] :ds [..] :n}` - the KDE of `values` on its grid (raw densities)."
  [values options]
  (let [e (kde/estimate values (assoc (select-keys options [:kernel :bandwidth]) :cut (data/option options :cut 0.0)))
        {:keys [x y]} (kde/curve e (kde/grid-points (:lo e) (:hi e) (long (or (:n options) 100))))]
    {:xs x :ds y}))

(defn compute
  "Per panel: `{:items [..] :style st :place p :options o :mirror? b}`; an item has `:xs`, `:widths` (densities scaled to
  `[0 1]` by `:scale` and `:varwidth`) and the [[cljplot2.chart.box/box-stats]] of its values; extents (x - the KDE
  ranges), padding, `:na`, `:dropped` (`:group` - items without a violin)."
  [{:keys [options]} chart-split {:keys [theme] :as env}]
  (let [{:keys [place position items panels dropped]} (strip/split chart-split options :center env)
        violin? #(>= (count (distinct (:values %))) 2)
        no-violin (count (remove violin? (mapcat val items)))
        items (update-vals items (fn [its] (mapv #(merge % (box/box-stats (:values %)) (profile (:values %) options))
                                                (filter violin? its))))
        all (mapcat val items)
        scale-by (get options :scale :width)
        global-max (double (reduce max 0.0 (mapcat :ds all)))
        count-max (double (reduce max 0.0 (map #(* (double (:n %)) (double (reduce max 0.0 (:ds %)))) all)))
        factor (box/width-factors all (:varwidth options))
        widths (fn [{:keys [ds n] :as item}]
                 (let [own-max (double (reduce max 0.0 ds))
                       k (* (double (factor item))
                            (double (case scale-by
                                      :width (if (pos? own-max) (/ 1.0 own-max) 0.0)
                                      :area (if (pos? global-max) (/ 1.0 global-max) 0.0)
                                      :count (if (pos? count-max) (/ (double n) count-max) 0.0))))]
                   (mapv #(* k (double %)) ds)))
        items (update-vals items (fn [its] (mapv #(-> % (assoc :widths (widths %)) (dissoc :ds)) its)))]
    {:data (update-vals items (fn [its] {:items its :style (style options theme) :place place :options options
                                         :mirror? (= :mirror position)}))
     :extents (merge-with merge (group/aesthetic-extents panels)
                          (strip/extents place items (fn [{:keys [xs]}] [(first xs) (peek xs)])))
     :padding (strip/padding place options)
     :padding-px (strip/n-label-room options items env)
     :na (group/na-dims panels)
     :dropped (assoc dropped :group no-violin)}))

(defn- line! [canvas ^Vec2 a ^Vec2 b] (c2d/line canvas (.x a) (.y a) (.x b) (.y b)))

(defn- width-at
  "Width (`[0 1]`) of the violin at `v`, linearly interpolated on its grid `xs` / `widths`."
  ^double [xs widths ^double v]
  (let [n (count xs)
        i (long (java.util.Collections/binarySearch ^java.util.List xs v compare))
        i (if (neg? i) (- (- i) 1) i)]
    (cond
      (<= i 0) (double (first widths))
      (>= i n) (double (peek widths))
      :else (let [x0 (double (xs (dec i))) x1 (double (xs i))
                  w0 (double (widths (dec i))) w1 (double (widths i))]
              (if (== x0 x1) w1 (+ w0 (* (- w1 w0) (/ (- v x0) (- x1 x0)))))))))

(defn draw
  "Draw the violins of one panel: the outline polygon (filled), then the inner marks."
  [_chart {:keys [items style place options mirror?]} canvas box scales env]
  (let [{:keys [point] :as pl} (strip/placement box scales options place)
        inner (get options :inner :box)]
    (doseq [{:keys [level slot xs widths q1 median q3 lav uav values aesthetics]} items
            :let [st (group/style style aesthetics scales (:theme env))]
            :when st]
      (let [[lo hi] slot
            mirror-half? (boolean mirror?)
            ;; base line and outward extent: whole violin - centre ± half the slot; split half - from 0 outward
            base (if mirror-half? 0.0 (strip/in-slot slot 0.0))
            extent (if mirror-half? (+ (double lo) (double hi)) (strip/slot-half slot))
            outline (fn [^double side] (mapv (fn [x w] (point x level (+ base (* side extent (double w))))) xs widths))
            upper (outline 1.0)
            lower (if mirror-half? (mapv #(point % level base) xs) (outline -1.0))
            polygon (into upper (rseq lower))
            fill (:color st)
            stroke (draw/outline-color (:stroke-color st) fill)
            inner-color (if (theme/none? (:inner-color st)) (box/contrast-color fill) (:inner-color st))
            sw (double (:stroke-width st))
            p (fn [v cross] (point v level cross))
            across (fn [v] (let [w (* extent (width-at xs widths (double v)))]
                             (if mirror-half? [(p v base) (p v (+ base w))] [(p v (- base w)) (p v (+ base w))])))]
        (when (every? #(and (m/valid-double? (.x ^Vec2 %)) (m/valid-double? (.y ^Vec2 %))) polygon)
          (c2d/set-color canvas fill)
          (c2d/path canvas (mapv (fn [^Vec2 v] [(.x v) (.y v)]) polygon) true false)
          (when-not (theme/none? stroke)
            (c2d/set-stroke-custom canvas {:size sw :join :round})
            (c2d/set-color canvas stroke)
            (c2d/path canvas (mapv (fn [^Vec2 v] [(.x v) (.y v)]) polygon) true true))
          (c2d/set-color canvas inner-color)
          (case inner
            :box (do (c2d/set-stroke-custom canvas {:size (m/max 1.0 sw) :cap :butt})
                     (line! canvas (p lav base) (p uav base))
                     (c2d/set-stroke-custom canvas {:size 5.0 :cap :butt})
                     (line! canvas (p q1 base) (p q3 base))
                     (c2d/set-color canvas fill)
                     (let [^Vec2 m (p median base)] (c2d/ellipse canvas (.x m) (.y m) 4 4)))
            :quartiles (do (c2d/set-stroke-custom canvas {:size (m/max 1.0 sw) :cap :butt :dash [4.0 3.0]})
                           (doseq [q [q1 q3]] (apply line! canvas (across q)))
                           (c2d/set-stroke-custom canvas {:size (m/max 1.0 sw) :cap :butt})
                           (apply line! canvas (across median)))
            :rug (do (c2d/set-stroke-custom canvas {:size 1.0 :cap :butt})
                     (doseq [v values
                             :let [t (* 0.15 extent)]]
                       (if mirror-half?
                         (line! canvas (p v base) (p v (+ base t)))
                         (line! canvas (p v (- base t)) (p v (+ base t))))))
            :points (doseq [v values :let [^Vec2 q (p v base)]] (c2d/ellipse canvas (.x q) (.y q) 3 3))
            :none nil))))
    (strip/draw-n-labels! canvas pl items (fn [{:keys [xs]}] (peek xs)) options scales env)))

(def ^:private ^:const glyph-points 24)

(defn legend
  "Key glyph: a small horizontal violin silhouette - a symmetric lens with pointed ends, half-width `(1 - t^2)` along
  `t` in `[-1 1]` - filled with `:color` and outlined with `:stroke-color`, centred in `box`."
  [{:keys [options]} attributes canvas {:keys [x y w h]} env]
  (let [st (merge (style options (:theme env)) (select-keys attributes [:color]))
        cx (+ (double x) (* 0.5 (double w))) cy (+ (double y) (* 0.5 (double h)))
        half-length (* 0.42 (double w)) half-width (* 0.3 (double h))
        ts (map #(- (* 2.0 (/ (double %) glyph-points)) 1.0) (range (inc glyph-points)))
        side (fn [^double sign] (mapv (fn [^double t] [(+ cx (* t half-length)) (+ cy (* sign half-width (- 1.0 (* t t))))]) ts))
        outline (into (side 1.0) (rseq (side -1.0)))
        stroke (draw/outline-color (:stroke-color st) (:color st))]
    (c2d/set-color canvas (:color st))
    (c2d/path canvas outline true false)
    (when-not (theme/none? stroke)
      (c2d/set-stroke-custom canvas {:size (double (:stroke-width st)) :join :round})
      (c2d/set-color canvas stroke)
      (c2d/path canvas outline true true))))
