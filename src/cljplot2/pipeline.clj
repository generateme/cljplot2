(ns cljplot2.pipeline
  "Plot pipeline: a plot map passes through fixed stages, each a pure function adding one key.

  Stages, in order: `:split` -> `:stats` -> `:extents` -> `:scale-specs` -> `:grid` -> `:layout` -> `:finalized-scales`.

  Plot keys read so far:

  * `:labels` - plot titles, see [[cljplot2.plot-grid]]
  * `:scales` - `{:x spec :y spec}`, position scales, see [[cljplot2.scale/scale-spec]]
  * `:custom-grid` - a hand-written grid (see [[cljplot2.grid]]) used instead of the generated one
  * `:options` - render options (see [[default-options]]), merged under `build`'s options; stored resolved
  * `:theme` - deep-merged over [[cljplot2.theme/default-theme]]; stored resolved

  Stages `:split`, `:stats`, `:extents` are still stubs producing an empty value."
  (:require [cljplot2.grid :as grid]
            [cljplot2.measure :as measure]
            [cljplot2.plot-grid :as plot-grid]
            [cljplot2.scale :as scale]
            [cljplot2.theme :as theme]
            ;; built-in content kinds (natural-size / draw-content methods)
            [cljplot2.label]
            [cljplot2.panel]
            [cljplot2.axis]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def default-options
  "`:width`, `:height` - target size in pixels; `:hint` - rendering hint, see [[cljplot2.measure/rendering-hints]]."
  {:width 600 :height 600 :hint :high})

(def ^:const max-layout-rounds
  "Upper bound on measure -> solve -> finalize-scales rounds."
  3)

;; stages

(defn- split-stage [plot] (assoc plot :split {}))
(defn- stats-stage [plot] (assoc plot :stats {}))
(defn- extents-stage [plot] (assoc plot :extents {}))
(def ^:private position-scales #{:x :y})

(defn- scale-specs-stage
  "Phase-1 specs of the plot `:scales` (`{}` when absent)."
  [{:keys [scales] :as plot}]
  (when-not (or (nil? scales) (map? scales))
    (throw (ex-info "Plot :scales must be a map." {:key :scales :value scales})))
  (doseq [k (keys scales)]
    (when-not (position-scales k)
      (throw (ex-info "Unknown plot scale." {:key :scales :value k :allowed position-scales}))))
  (assoc plot :scale-specs (into {} (map (fn [[k spec]] [k (scale/scale-spec k spec)])) scales)))

(defn- grid-stage
  "The plot's grid: `:custom-grid` when given, otherwise generated from plot keys.
  A custom grid can't be combined with `:labels` (they would be ignored)."
  [{:keys [custom-grid labels] :as plot}]
  (when (and custom-grid labels)
    (throw (ex-info "Plot :labels can't be combined with :custom-grid; put labels into the custom grid."
                    {:key :labels})))
  (assoc plot :grid (or custom-grid (plot-grid/single-panel-grid plot))))

(def ^:private ^:const provisional-tick-hint 10)

(defn finalize-scales
  "Scales of `:scale-specs` finalized for the `:panel` box of `layout`: x range `[left right]`, y range
  `[bottom top]`, tick hint from the axis length and theme `[:axis k :tick-spacing]`.
  Before the first layout round (`layout` is `nil`) a provisional finalization is used: range `[0 1]`, 10 ticks.
  Throws `ex-info` with `{:key :scales}` when the layout has no `:panel` item."
  [plot layout]
  (let [specs (:scale-specs plot)
        panel (when (and layout (seq specs))
                (or (grid/item-box layout :panel)
                    (throw (ex-info "Plot :scales need a :panel item in the grid." {:key :scales}))))]
    (into {} (map (fn [[k spec]]
                    [k (if-not panel
                         (scale/finalize spec [0.0 1.0] provisional-tick-hint)
                         (let [{:keys [x y w h]} panel
                               x (long x) y (long y) w (long w) h (long h)
                               spacing (double (get-in plot [:theme :axis k :tick-spacing]))]
                           (if (= k :x)
                             (scale/finalize spec [(double x) (double (+ x w))] (scale/tick-hint w spacing))
                             (scale/finalize spec [(double (+ y h)) (double y)] (scale/tick-hint h spacing)))))]))
          specs)))

(defn- layout-stage
  "Repeat measure -> solve -> finalize scales until item measurements stop changing,
  at most [[max-layout-rounds]] times. The number of rounds is stored under `:rounds`."
  [plot]
  (let [{:keys [width height hint]} (:options plot)
        prepared (grid/prepare (:grid plot))
        box {:x 0 :y 0 :w width :h height}]
    (measure/call-with-scratch-canvas
     hint
     (fn [canvas]
       (let [measure-for (fn [layout]
                           (grid/measure-items prepared
                                               {:canvas canvas
                                                :theme (:theme plot)
                                                :scales (finalize-scales plot layout)}
                                               layout))]
         (loop [round 1
                measurements (measure-for nil)]
           (let [layout (grid/solve prepared box measurements)
                 remeasured (measure-for layout)]
             (if (or (= measurements remeasured) (>= round max-layout-rounds))
               (assoc plot :layout (assoc layout :rounds round))
               (recur (inc round) remeasured)))))))))

(defn- finalized-scales-stage
  "Scales finalized for the final layout (stored under `:finalized-scales`; the plot's `:scales` key is user input)."
  [plot]
  (assoc plot :finalized-scales (finalize-scales plot (:layout plot))))

(def stage-order
  "Pipeline stages in execution order."
  [:split :stats :extents :scale-specs :grid :layout :finalized-scales])

(def ^:private stage-fns
  {:split split-stage
   :stats stats-stage
   :extents extents-stage
   :scale-specs scale-specs-stage
   :grid grid-stage
   :layout layout-stage
   :finalized-scales finalized-scales-stage})

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
  `:width`, `:height`, `:hint`, and `:until` - last stage to run (default: the last one).
  `:theme` is stored merged with the default theme.
  Stage keys already present in `plot` (e.g. from an earlier `build`) are dropped first."
  ([plot] (build plot {}))
  ([plot options]
   (let [until (get options :until (peek stage-order))
         stage-count (inc (.indexOf ^java.util.List stage-order until))
         options (merge default-options (:options plot) (dissoc options :until))]
     (when (zero? stage-count)
       (throw (ex-info "Unknown stage." {:key :until :value until :allowed stage-order})))
     (validate-options! options)
     (reduce (fn [plot stage] ((stage-fns stage) plot))
             (-> (apply dissoc plot stage-order)
                 (assoc :options options)
                 (update :theme theme/plot-theme))
             (take stage-count stage-order)))))
