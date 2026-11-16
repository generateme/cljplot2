(ns cljplot2.chart.rug
  "Rug: a short line per `:x` value across its strip - a strip chart (see [[cljplot2.data.strip]]); without a `:y`
  mapping placed `:edge` (along the panel's low y edge, `:thickness` pixels, default 8).

  Options (style defaults from theme `[:chart :rug]`): `:color`, `:line-width` (pixels; `:width` is the strip option); `:padding {:x [lo hi]}`
  (default `[0.05 0.05]`), plus the strip options and `:data` / `:mapping`."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.data.group :as group]
            [cljplot2.data.rows :as data]
            [cljplot2.data.strip :as strip]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries
  [(schema/optional :color schema/color)
   (schema/optional :line-width schema/non-neg)])

(def schemas
  "Schemas of the rug chart: `:style` (theme `[:chart :rug]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into strip/options)
                (conj (schema/optional :padding data/position-padding)))
   :mapping strip/values-mapping})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :rug]) (select-keys options [:color :line-width])))

(defn- compute-in-strip-frame
  "Per panel: `{:items [..] :style st :place p :options o}`; extents (x - the values), padding, `:na`, `:dropped`."
  [{:keys [options]} chart-split {:keys [theme] :as env}]
  (let [{:keys [place items panels dropped]} (strip/split chart-split options :edge env)]
    {:data (update-vals items (fn [its] {:items its :style (style options theme) :place place :options options}))
     :extents (merge-with merge (group/aesthetic-extents panels (:plot-scales env))
                          (strip/extents place items (fn [{:keys [values]}] [(reduce min values) (reduce max values)])))
     :padding (strip/padding place options)
     :padding-explicit (strip/explicit-padding place options)
     :padding-px (strip/n-label-room options items env)
     :na (group/na-dims panels)
     :dropped dropped}))

(defn- draw-in-strip-frame
  "Draw the rug lines of one panel."
  [_chart {:keys [items style place options]} canvas box scales env]
  (let [{:keys [point] :as pl} (strip/placement box scales options place)]
    (doseq [{:keys [level slot values aesthetics]} items
            :let [st (group/style style aesthetics scales (:theme env))]
            :when (and st (pos? (double (:line-width st))))]
      (c2d/set-color canvas (:color st))
      (c2d/set-stroke-custom canvas {:size (double (:line-width st)) :cap :butt})
      (doseq [v values
              :let [[lo hi] slot
                    ^Vec2 a (point v level (double lo))
                    ^Vec2 b (point v level (double hi))]
              :when (and (m/valid-double? (.x a)) (m/valid-double? (.y a)) (m/valid-double? (.y b)))]
        (c2d/line canvas (.x a) (.y a) (.x b) (.y b))))
    (strip/draw-n-labels! canvas pl items (fn [{:keys [values]}] (reduce max values)) options scales env)))


(defn compute
  "Stats of the chart with its levels on the categorical axis (see [[cljplot2.data.strip/compute-oriented]]):
  [[compute-in-strip-frame]] in the strip frame."
  [chart chart-split env]
  (strip/compute-oriented compute-in-strip-frame chart chart-split env))

(defn draw
  "Draw one panel (see [[cljplot2.data.strip/draw-oriented]]): [[draw-in-strip-frame]] in the strip frame."
  [chart computed canvas box scales env]
  (strip/draw-oriented draw-in-strip-frame chart computed canvas box scales env))
(defn legend
  "Key glyph: a short vertical line in the middle of `box`."
  [{:keys [options]} attributes canvas {:keys [x y w h]} env]
  (let [st (merge (style options (:theme env)) (select-keys attributes [:color]))
        cx (+ (double x) (* 0.5 (double w)))]
    (c2d/set-color canvas (:color st))
    (c2d/set-stroke-custom canvas {:size (m/max 1.0 (double (:line-width st))) :cap :butt})
    (c2d/line canvas cx (+ (double y) (* 0.2 (double h))) cx (+ (double y) (* 0.8 (double h))))))
