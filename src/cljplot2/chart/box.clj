(ns cljplot2.chart.box
  "Box plot: a Tukey box per `:y` level and group of the `:x` values - a strip chart (see [[cljplot2.data.strip]]:
  placement `:place`, `:width`, `:thickness`, groups `:position`).

  Per item: quartiles Q1, median, Q3 (fastmath `percentiles`, legacy estimation), whiskers to the adjacent values
  (the most extreme values within 1.5 IQR of the box, fastmath `adjacent-values`), outliers beyond them.
  Drawn as the old cljplot box: the IQR rectangle filled with `:color` and stroked darker, the median line black or
  white by the fill's luma, whiskers with caps, outliers as `:shape` points of `:size`.

  Options (style defaults from theme `[:chart :box]`): `:color`, `:stroke-width`, `:shape`, `:size` (outliers);
  `:outliers` (default true), `:mean` (a mean marker, default false), `:varwidth` (box width proportional to the
  square root of its count, relative to the largest, default false), `:padding {:x [lo hi]}` (default
  `[0.05 0.05]`), plus the strip options and `:data` / `:mapping` (`:x` values, optional `:y` levels)."
  (:require [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.stats :as stats]
            [cljplot2.data.group :as group]
            [cljplot2.data.rows :as data]
            [cljplot2.data.strip :as strip]
            [cljplot2.graphics.shape :as shape]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries
  [(schema/optional :color schema/solid-color)
   (schema/optional :stroke-width schema/non-neg)
   (schema/optional :shape schema/shape)
   (schema/optional :size schema/non-neg)])

(def Schema
  "Schemas of the box chart: `:style` (theme `[:chart :box]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into strip/options)
                (conj (schema/nilable :outliers schema/bool)
                      (schema/nilable :mean schema/bool)
                      (schema/nilable :varwidth schema/bool)
                      (schema/optional :padding data/position-padding)))
   :mapping [:map [:x schema/accessor]]})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :box]) (select-keys options [:color :stroke-width :shape :size])))

(defn box-stats
  "Tukey box statistics of `values` (non-empty): `{:q1 :median :q3 :lav :uav :outliers [..] :mean :n}`."
  [values]
  (let [[q1 median q3] (stats/percentiles values [25.0 50.0 75.0])
        [lav uav] (stats/adjacent-values values q1 q3 median)]
    {:q1 q1 :median median :q3 q3 :lav lav :uav uav
     :outliers (vec (stats/outliers values q1 q3))
     :mean (stats/mean values)
     :n (count values)}))

(defn width-factors
  "Item -> width factor: `sqrt(n / max n)` over all `items` with `varwidth?`, else 1."
  [items varwidth?]
  (let [nmax (double (reduce max 1 (map :n items)))]
    (fn ^double [item] (if varwidth? (m/sqrt (/ (double (:n item)) nmax)) 1.0))))

(defn compute
  "Per panel: `{:items [item ...] :style st :place p :options o}`, items with [[box-stats]] and `:width-factor`;
  extents (x - whiskers and, with `:outliers`, outliers; y - see [[cljplot2.data.strip/extents]]), padding, `:na`,
  `:dropped`."
  [{:keys [options]} chart-split {:keys [theme] :as env}]
  (let [{:keys [place items panels dropped]} (strip/split chart-split options :center env)
        items (update-vals items (fn [its] (mapv #(merge % (box-stats (:values %))) its)))
        factor (width-factors (mapcat val items) (:varwidth options))
        items (update-vals items (fn [its] (mapv #(assoc % :width-factor (factor %)) its)))
        outliers? (data/option options :outliers true)
        x-range (fn [{:keys [lav uav outliers]}]
                  (let [vs (cond-> [lav uav] outliers? (into outliers))]
                    [(reduce min vs) (reduce max vs)]))]
    {:data (update-vals items (fn [its] {:items its :style (style options theme) :place place :options options}))
     :extents (merge-with merge (group/aesthetic-extents panels) (strip/extents place items x-range))
     :padding (strip/padding place options)
     :padding-px (strip/n-label-room options items env)
     :na (group/na-dims panels)
     :dropped dropped}))

(defn- line! [canvas ^Vec2 a ^Vec2 b] (c2d/line canvas (.x a) (.y a) (.x b) (.y b)))

(defn- finite? [^Vec2 p] (and (m/valid-double? (.x p)) (m/valid-double? (.y p))))

(defn contrast-color
  "Black or white, whichever reads better on `color` (by luma)."
  [color]
  (if (> 127.5 (double (c/luma color))) :white :black))

(defn draw
  "Draw the boxes of one panel."
  [_chart {:keys [items style place options]} canvas box scales env]
  (let [{:keys [point] :as pl} (strip/placement box scales options place)
        outliers? (data/option options :outliers true)]
    (doseq [{:keys [level slot q1 median q3 lav uav outliers mean width-factor aesthetics]} items
            :let [st (group/style style aesthetics scales (:theme env))]
            :when st]
      (let [c0 (strip/in-slot slot 0.0)
            hw (* (strip/slot-half slot) (double width-factor))
            p (fn [v cross] (point v level cross))
            a (p q1 (- c0 hw)) b (p q3 (+ c0 hw))]
        (when (and (finite? a) (finite? b))
          (let [fill (:color st)
                stroke (c/darken fill)
                sw (double (:stroke-width st))]
            (c2d/set-stroke-custom canvas {:size sw :cap :butt})
            (c2d/set-color canvas stroke)
            (line! canvas (p lav c0) (p q1 c0))
            (line! canvas (p q3 c0) (p uav c0))
            (line! canvas (p lav (- c0 (* 0.5 hw))) (p lav (+ c0 (* 0.5 hw))))
            (line! canvas (p uav (- c0 (* 0.5 hw))) (p uav (+ c0 (* 0.5 hw))))
            (let [x (m/min (.x ^Vec2 a) (.x ^Vec2 b)) y (m/min (.y ^Vec2 a) (.y ^Vec2 b))
                  w (m/abs (- (.x ^Vec2 b) (.x ^Vec2 a))) h (m/abs (- (.y ^Vec2 b) (.y ^Vec2 a)))]
              (c2d/set-color canvas fill)
              (c2d/rect canvas x y w h)
              (c2d/set-color canvas stroke)
              (c2d/rect canvas x y w h true))
            (c2d/set-color canvas (contrast-color fill))
            (line! canvas (p median (- c0 hw)) (p median (+ c0 hw)))
            (when outliers?
              (doseq [o outliers]
                (shape/draw! canvas (:shape st) (p o c0) (assoc st :stroke-color stroke))))
            (when (:mean options)
              (shape/draw! canvas \O (p mean c0) {:size (m/max 3.0 (* 0.5 (double (:size st))))
                                                   :color (contrast-color fill) :stroke-color stroke
                                                   :stroke-width sw}))))))
    (strip/draw-n-labels! canvas pl items
                          (fn [{:keys [uav outliers]}] (if (and outliers? (seq outliers)) (reduce max uav outliers) uav))
                          options scales env)))

(defn legend
  "Key glyph: a small box with a median line, centred in `box`."
  [{:keys [options]} attributes canvas {:keys [x y w h]} env]
  (let [st (merge (style options (:theme env)) (select-keys attributes [:color]))
        bw (* 0.7 (double w)) bh (* 0.5 (double h))
        bx (+ (double x) (* 0.5 (- (double w) bw))) by (+ (double y) (* 0.5 (- (double h) bh)))]
    (c2d/set-stroke-custom canvas {:size (double (:stroke-width st))})
    (c2d/set-color canvas (:color st))
    (c2d/rect canvas bx by bw bh)
    (c2d/set-color canvas (c/darken (:color st)))
    (c2d/rect canvas bx by bw bh true)
    (c2d/set-color canvas (contrast-color (:color st)))
    (c2d/line canvas (+ bx (* 0.5 bw)) by (+ bx (* 0.5 bw)) (+ by bh))))
