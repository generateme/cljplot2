(ns cljplot2.chart
  "Charts. A chart value is a plain map `{:kind :options :compute :draw}` made by a constructor; `:compute` and
  `:draw` are top-level functions, so chart values print and compare as data.

  * `(compute chart {:groups {panel rows} :mapping mapping} env)` -> data
    `{:extents {panel {dim extent}} :padding {dim [lo hi]} :data {panel computed} :dropped {dim n}}`
    (`env` - `{:theme}`); stored per chart in the plot's `:stats`
  * `(draw chart computed canvas panel-box scales env)` - draws one panel's `computed` data; called by the panel
    with isolated graphics clipped to the panel box

  Options common to all charts: `:data` (own data, see [[cljplot2.data/rows]]) and `:mapping` (merged over the
  plot's mapping)."
  (:require [cljplot2.chart.scatter :as scatter]))

(defn- chart-value
  [kind compute draw options]
  (when-not (or (nil? options) (map? options))
    (throw (ex-info "Chart options must be a map." {:key :options :kind kind})))
  {:kind kind :options (or options {}) :compute compute :draw draw})

(defn scatter
  "Scatter chart, see [[cljplot2.chart.scatter]]."
  ([] (scatter {}))
  ([options] (chart-value :scatter scatter/compute scatter/draw options)))
