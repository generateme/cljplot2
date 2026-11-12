(ns cljplot2.chart
  "Charts. A chart value is a plain map `{:kind :options :compute :draw :legend}` made by a constructor; `:compute` and
  `:draw` are top-level functions, so chart values print and compare as data.

  * `(compute chart {:groups {panel rows} :mapping mapping} env)` -> data
    `{:extents {panel {dim extent}} :padding {dim [lo hi]} :data {panel computed} :na #{dim} :dropped {dim n}}`
    (`env` - `{:theme :na-level?}`); stored per chart in the plot's `:stats`. Aesthetic extents (`:color`,
    `:shape`, `:size`) are [[cljplot2.data/value-extent]]s; `:na` lists aesthetics with NA (`nil`) values
  * `(draw chart computed canvas panel-box scales env)` - draws one panel's `computed` data; called by the panel
    with isolated graphics clipped to the panel box. Positions come from `cljplot2.scale/forward-fn`, which gives
    `##NaN` for values the scale can't place; such values are skipped.
  * `(legend chart attributes canvas box env)` - optional; draws the chart's key glyph in a legend key `box`,
    `attributes` - the entry's mapped style attributes (`{:color c :shape s :size z}`, NA ones absent except color).
    Every chart mapping a legend's dimension draws into its keys, in chart order.

  Options common to all charts: `:data` (own data, see [[cljplot2.data/rows]]) and `:mapping` (merged over the
  plot's mapping)."
  (:require [cljplot2.chart.scatter :as scatter]))

(defn- chart-value
  [kind compute draw legend options]
  (when-not (or (nil? options) (map? options))
    (throw (ex-info "Chart options must be a map." {:key :options :kind kind})))
  {:kind kind :options (or options {}) :compute compute :draw draw :legend legend})

(defn scatter
  "Scatter chart, see [[cljplot2.chart.scatter]]."
  ([] (scatter {}))
  ([options] (chart-value :scatter scatter/compute scatter/draw scatter/legend options)))
