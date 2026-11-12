(ns cljplot2.pipeline
  "Plot pipeline: a plot map passes through fixed stages, each a pure function adding one key.

  Stages, in order: `:split` -> `:stats` -> `:extents` -> `:scale-specs` -> `:grid` -> `:layout` -> `:finalized-scales`.

  Plot keys read so far:

  * `:labels` - plot titles, see [[cljplot2.plot-grid]]
  * `:scales` - `{:x spec :y spec}`, position scales, see [[cljplot2.scale/data-scale-spec]]
  * `:data`, `:mapping` - plot data and mapping, see [[cljplot2.data]]
  * `:charts` - vector of chart values, see [[cljplot2.chart]]
  * `:custom-grid` - a hand-written grid (see [[cljplot2.grid]]) used instead of the generated one
  * `:options` - render options (see [[default-options]]), merged under `build`'s options; stored resolved
  * `:theme` - deep-merged over [[cljplot2.theme/default-theme]]; stored resolved

  One panel (`:panel`) until facets."
  (:require [fastmath.core :as m]
            [cljplot2.data :as data]
            [cljplot2.grid :as grid]
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

(def ^:private position-scales #{:x :y})

(def ^:private panel-key
  "Split group key of the single panel (facets come with step 6)."
  :panel)

(defn- validate-mapping!
  [mapping]
  (when-not (or (nil? mapping) (map? mapping))
    (throw (ex-info ":mapping must be a map." {:key :mapping :value mapping}))))

(defn- validate-charts!
  [charts]
  (when-not (or (nil? charts) (vector? charts))
    (throw (ex-info "Plot :charts must be a vector of charts." {:key :charts})))
  (doseq [chart charts]
    (when-not (and (map? chart) (fn? (:compute chart)) (fn? (:draw chart)))
      (throw (ex-info "Chart must be a chart value (see cljplot2.chart)." {:key :charts})))))

(defn- split-stage
  "Per chart: its rows (own non-nil `:data` or the plot's; `:data nil` counts as absent) grouped by panel, and its
  mapping (plot mapping with the chart's `:mapping` merged over)."
  [{:keys [charts data mapping] :as plot}]
  (validate-charts! charts)
  (validate-mapping! mapping)
  (let [plot-rows (delay (data/rows data))]
    (assoc plot :split (mapv (fn [{:keys [options]}]
                               (validate-mapping! (:mapping options))
                               {:mapping (merge mapping (:mapping options))
                                :groups {panel-key (if (some? (:data options)) (data/rows (:data options)) @plot-rows)}})
                             charts))))

(defn- stats-stage
  "Per chart: the data its `:compute` returns from its split groups."
  [{:keys [charts split theme] :as plot}]
  (assoc plot :stats (mapv (fn [chart chart-split] ((:compute chart) chart chart-split {:theme theme})) charts split)))

(defn- extents-stage
  "Per position scale: extents merged over charts and panels, padding as the largest per side among the charts
  with an extent there."
  [{:keys [stats] :as plot}]
  (assoc plot :extents
         (into {} (keep (fn [k]
                          (let [with-extent (keep (fn [st]
                                                    (when-let [es (seq (keep #(get % k) (vals (:extents st))))]
                                                      {:extent (data/merge-extents k es) :padding (get-in st [:padding k] [0 0])}))
                                                  stats)]
                            (when (seq with-extent)
                              [k {:extent (data/merge-extents k (map :extent with-extent))
                                  :padding (reduce (fn [[a b] [c d]] [(m/max (double a) (double c)) (m/max (double b) (double d))])
                                                [0.0 0.0] (map :padding with-extent))}]))))
               position-scales)))

(defn- scale-specs-stage
  "Phase-1 specs of the position scales with a plot `:scales` entry or data extents."
  [{:keys [scales extents] :as plot}]
  (when-not (or (nil? scales) (map? scales))
    (throw (ex-info "Plot :scales must be a map." {:key :scales :value scales})))
  (doseq [k (keys scales)]
    (when-not (position-scales k)
      (throw (ex-info "Unknown plot scale." {:key :scales :value k :allowed position-scales}))))
  (assoc plot :scale-specs
         (into {} (map (fn [k] [k (scale/data-scale-spec k (get scales k {}) (get-in extents [k :extent])
                                                         (get-in extents [k :padding]))]))
               (distinct (concat (keys scales) (keys extents))))))

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
  The `:panel` item may sit in a nested grid (first found depth-first).
  Throws `ex-info` with `{:key :scales}` when the layout has no `:panel` item."
  [plot layout]
  (let [specs (:scale-specs plot)
        panel (when (and layout (seq specs))
                (or (grid/find-item-box layout :panel)
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
