(ns cljplot2.pipeline
  "Plot pipeline: a plot map passes through fixed stages, each a pure function adding one key.

  Stages, in order: `:split` -> `:stats` -> `:extents` -> `:scale-specs` -> `:layout` -> `:scales`.

  Plot keys read so far:

  * `:grid` - layout grid (see [[cljplot2.grid]]); a single flexible `:panel` cell when absent
  * `:options` - render options (see [[default-options]]), merged under `build`'s options

  Step 1: every stage except `:layout` is a stub producing an empty value."
  (:require [clojure2d.core :as c2d]
            [cljplot2.grid :as grid]
            [cljplot2.measure :as measure]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def default-options
  "`:width`, `:height` - target size in pixels; `:hint` - rendering hint, see [[cljplot2.measure/rendering-hints]]."
  {:width 600 :height 600 :hint :high})

(def ^:const max-layout-rounds
  "Upper bound on measure -> solve -> finalize-scales rounds."
  3)

(def default-grid
  "Grid used when a plot has no `:grid`: one flexible cell with item `:panel`."
  {:columns [{:id :panel :size [:flexible]}]
   :rows [{:id :panel :size [:flexible]}]
   :items [{:id :panel :column :panel :row :panel}]})

;; stages

(defn- split-stage [plot] (assoc plot :split {}))
(defn- stats-stage [plot] (assoc plot :stats {}))
(defn- extents-stage [plot] (assoc plot :extents {}))
(defn- scale-specs-stage [plot] (assoc plot :scale-specs {}))

(defn finalize-scales
  "Scales finalized for `layout` (`nil` before the first layout round). Stub: `{}`."
  [_plot _layout]
  {})

(defn- layout-stage
  "Repeat measure -> solve -> finalize scales until item measurements stop changing,
  at most [[max-layout-rounds]] times. The number of rounds is stored under `:rounds`."
  [plot]
  (let [{:keys [width height hint]} (:options plot)
        prepared (grid/prepare (or (:grid plot) default-grid))
        box {:x 0 :y 0 :w width :h height}]
    (c2d/with-canvas [canvas (measure/scratch-canvas hint)]
      (let [measure-for (fn [layout]
                          (grid/measure-items prepared
                                              {:canvas canvas :scales (finalize-scales plot layout)}
                                              layout))]
        (loop [round 1
               measurements (measure-for nil)]
          (let [layout (grid/solve prepared box measurements)
                remeasured (measure-for layout)]
            (if (or (= measurements remeasured) (>= round max-layout-rounds))
              (assoc plot :layout (assoc layout :rounds round))
              (recur (inc round) remeasured))))))))

(defn- scales-stage [plot] (assoc plot :scales (finalize-scales plot (:layout plot))))

(def stage-order
  "Pipeline stages in execution order."
  [:split :stats :extents :scale-specs :layout :scales])

(def ^:private stage-fns
  {:split split-stage
   :stats stats-stage
   :extents extents-stage
   :scale-specs scale-specs-stage
   :layout layout-stage
   :scales scales-stage})

;; build

(defn- validate-options!
  [{:keys [width height hint]}]
  (doseq [[k value] [[:width width] [:height height]]]
    (when-not (and (integer? value) (pos? (long value)))
      (throw (ex-info "Plot size must be a positive integer." {:key k :value value}))))
  (when-not (measure/rendering-hints hint)
    (throw (ex-info "Unknown rendering hint." {:key :hint :value hint :allowed measure/rendering-hints}))))

(defn build
  "Run `plot` through the pipeline stages and return the plot map with each stage's key added.

  `options` (merged over `(:options plot)` and [[default-options]], stored under `:options`):
  `:width`, `:height`, `:hint`, and `:until` - last stage to run (default: the last one)."
  ([plot] (build plot {}))
  ([plot options]
   (let [until (get options :until (peek stage-order))
         stage-count (inc (.indexOf ^java.util.List stage-order until))
         options (merge default-options (:options plot) (dissoc options :until))]
     (when (zero? stage-count)
       (throw (ex-info "Unknown stage." {:key :until :value until :allowed stage-order})))
     (validate-options! options)
     (reduce (fn [plot stage] ((stage-fns stage) plot))
             (assoc plot :options options)
             (take stage-count stage-order)))))
