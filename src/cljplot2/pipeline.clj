(ns cljplot2.pipeline
  "Plot pipeline: a plot map passes through fixed stages, each a pure function adding one key.

  Stages, in order: `:split` -> `:stats` -> `:extents` -> `:scale-specs` -> `:grid` -> `:layout` -> `:finalized-scales`.

  Plot keys read so far:

  * `:labels` - plot titles, see [[cljplot2.plot-grid]]
  * `:scales` - `{:x spec :y spec}`, position scales, see [[cljplot2.scale/data-scale-spec]]
  * `:data`, `:mapping` - plot data and mapping, see [[cljplot2.data]]
  * `:charts` - vector of chart values, see [[cljplot2.chart]]
  * `:facets` - `{:columns :rows :order :na-panel}` facet layout options, see [[cljplot2.facet]]
  * `:custom-grid` - a hand-written grid (see [[cljplot2.grid]]) used instead of the generated one
  * `:options` - render options (see [[default-options]]), merged under `build`'s options; stored resolved
  * `:theme` - deep-merged over [[cljplot2.theme/default-theme]]; stored resolved

  Facet mapping keys (`:facet`, `:facet-col`, `:facet-row`) split rows into panels, see [[cljplot2.facet]]."
  (:require [fastmath.core :as m]
            [cljplot2.data :as data]
            [cljplot2.facet :as facet]
            [cljplot2.grid :as grid]
            [cljplot2.measure :as measure]
            [cljplot2.plot-grid :as plot-grid]
            [cljplot2.scale :as scale]
            [cljplot2.theme :as theme]
            ;; built-in content kinds (natural-size / draw-content methods)
            [cljplot2.label]
            [cljplot2.panel]
            [cljplot2.axis]
            [cljplot2.strip]))

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

(defn- facet-split
  "Facet dimensions, levels and per-chart groups for charts `chart-inputs` (`{:rows :mapping}`)."
  [chart-inputs scales na-panel?]
  (let [dim-sets (distinct (remove empty? (map (comp facet/facet-dims :mapping) chart-inputs)))
        _ (when (> (count dim-sets) 1)
            (throw (ex-info "Charts use different facet mappings." {:key :mapping :value (vec dim-sets)})))
        dims (vec (first dim-sets))
        faceted (filter #(seq (facet/facet-dims (:mapping %))) chart-inputs)
        accessors (fn [mapping] (into {} (map (fn [d] [d (data/row-accessor d (mapping d))])) dims))
        levels (into {} (map (fn [d]
                               (let [values (mapcat (fn [{:keys [rows mapping]}] (map (get (accessors mapping) d) rows)) faceted)]
                                 [d (facet/levels d values (get scales d) na-panel?)])))
                     dims)
        active? (and (seq dims) (every? seq (vals levels)))]
    {:facets {:dims (if active? dims []) :levels (if active? levels {})}
     :charts (mapv (fn [{:keys [rows mapping]}]
                     (cond
                       (not active?) {:mapping mapping :groups {facet/single-panel rows} :dropped 0}
                       (empty? (facet/facet-dims mapping)) {:mapping mapping :groups {facet/all-panels rows} :dropped 0}
                       :else (let [{:keys [groups dropped]} (facet/group-rows rows dims (accessors mapping) levels)]
                               {:mapping mapping :groups groups :dropped dropped})))
                   chart-inputs)}))

(defn- split-stage
  "Rows of every chart (own non-nil `:data` or the plot's) grouped by facet key, with the chart's mapping (plot
  mapping with the chart's `:mapping` merged over). Stores `{:facets {:dims :levels :dropped {:facet n}}
  :charts [{:mapping :groups} ...]}`; see [[cljplot2.facet]]."
  [{:keys [charts data mapping scales facets] :as plot}]
  (validate-charts! charts)
  (validate-mapping! mapping)
  (facet/validate-options! facets)
  (let [plot-rows (delay (data/rows data))
        inputs (mapv (fn [{:keys [options]}]
                       (validate-mapping! (:mapping options))
                       {:mapping (merge mapping (:mapping options))
                        :rows (if (some? (:data options)) (data/rows (:data options)) @plot-rows)})
                     charts)
        {split-facets :facets chart-splits :charts} (facet-split inputs scales (get facets :na-panel true))]
    (assoc plot :split {:facets (assoc split-facets :dropped {:facet (reduce + (map :dropped chart-splits))})
                        :charts (mapv #(dissoc % :dropped) chart-splits)})))

(defn- stats-stage
  "Per chart: the data its `:compute` returns from its split groups."
  [{:keys [charts split theme] :as plot}]
  (assoc plot :stats (mapv (fn [chart chart-split] ((:compute chart) chart chart-split {:theme theme})) charts (:charts split))))

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
    (when-not (or (position-scales k) (facet/facet-keys k))
      (throw (ex-info "Unknown plot scale." {:key :scales :value k :allowed (into position-scales facet/facet-keys)}))))
  (assoc plot :scale-specs
         (into {} (map (fn [k] [k (scale/data-scale-spec k (get scales k {}) (get-in extents [k :extent])
                                                         (get-in extents [k :padding]))]))
               (distinct (concat (filter position-scales (keys scales)) (keys extents))))))

(defn- grid-stage
  "The plot's grid: `:custom-grid` when given, otherwise generated from plot keys.
  A custom grid can't be combined with `:labels` (they would be ignored)."
  [{:keys [custom-grid labels] :as plot}]
  (when (and custom-grid labels)
    (throw (ex-info "Plot :labels can't be combined with :custom-grid; put labels into the custom grid."
                    {:key :labels})))
  (assoc plot :grid (or custom-grid (plot-grid/plot-grid plot))))

(def ^:private ^:const provisional-tick-hint 10)

(defn- panel-content? [content] (= :panel (:kind content)))

(defn finalize-scales
  "Scales of `:scale-specs` finalized in panel-local coordinates for the reference panel of `layout` - the first
  `:panel` content of the grid (depth-first): x range `[0 w]`, y range `[h 0]`, tick hint from the panel size
  and theme `[:axis k :tick-spacing]`. All panels share these scales (they differ by at most 1 px in size).
  Before the first layout round (`layout` is `nil`) a provisional finalization is used: range `[0 1]`, 10 ticks.
  Throws `ex-info` with `{:key :scales}` when the grid has no `:panel` content."
  [plot layout]
  (let [specs (:scale-specs plot)
        panel (when (and layout (seq specs))
                (or (grid/find-content-box (:grid plot) layout panel-content?)
                    (throw (ex-info "Plot :scales need a :panel item in the grid." {:key :scales}))))]
    (into {} (map (fn [[k spec]]
                    [k (if-not panel
                         (scale/finalize spec [0.0 1.0] provisional-tick-hint)
                         (let [{:keys [w h]} panel
                               w (long w) h (long h)
                               spacing (double (get-in plot [:theme :axis k :tick-spacing]))]
                           (if (= k :x)
                             (scale/finalize spec [0.0 (double w)] (scale/tick-hint w spacing))
                             (scale/finalize spec [(double h) 0.0] (scale/tick-hint h spacing)))))]))
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
