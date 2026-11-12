(ns cljplot2.chart
  "Charts. A chart value is a plain map `{:kind :options :compute :draw :legend}` made by a constructor; `:compute` and
  `:draw` are top-level functions, so chart values print and compare as data.

  * `(compute chart {:groups {panel rows} :mapping mapping} env)` -> data
    `{:extents {panel {dim extent}} :padding {dim [lo hi]} :data {panel computed} :na #{dim} :dropped {dim n}}`
    and optionally `:axis-titles {:x t :y t}` (suggested axis titles; the first chart's win, plot `:labels` win over
    all) and `:tick-format {:x f :y f}` (suggested tick label format, `:absolute` - magnitudes; used unless the plot's
    scale has a `:formatter`) (`env` - `{:theme :na-level? :plot-scales}`, `:plot-scales` - the plot's `:scales`); stored per chart in the
    plot's `:stats`. Aesthetic extents (`:color`,
    `:shape`, `:size`) are [[cljplot2.data.rows/value-extent]]s; `:na` lists aesthetics with NA (`nil`) values
  * `(draw chart computed canvas panel-box scales env)` - draws one panel's `computed` data; called by the panel
    with isolated graphics clipped to the panel box. Positions come from `cljplot2.scale.position/forward-fn`, which gives
    `##NaN` for values the scale can't place; such values are skipped.
  * `(legend chart attributes canvas box env)` - optional; draws the chart's key glyph in a legend key `box`,
    `attributes` - the entry's mapped style attributes (`{:color c :shape s :size z}`, NA ones absent except color).
    Every chart mapping a legend's dimension draws into its keys, in chart order.

  Options common to all charts: `:data` (own data, see [[cljplot2.data.rows/rows]]) and `:mapping` (merged over the
  plot's mapping)."
  (:require [cljplot2.chart.density :as density]
            [cljplot2.chart.ecdf :as ecdf]
            [cljplot2.chart.histogram :as histogram]
            [cljplot2.chart.line :as line]
            [cljplot2.chart.scatter :as scatter]
            [cljplot2.chart.treemap :as treemap]))

(defn- chart-value
  [kind compute draw legend options]
  (when-not (or (nil? options) (map? options))
    (throw (ex-info "Chart options must be a map." {:key :options :kind kind})))
  {:kind kind :options (or options {}) :compute compute :draw draw :legend legend})

(defn scatter
  "Scatter chart, see [[cljplot2.chart.scatter]]."
  ([] (scatter {}))
  ([options] (chart-value :scatter scatter/compute scatter/draw scatter/legend options)))

(defn histogram
  "Histogram of `:x`, see [[cljplot2.chart.histogram]]."
  ([] (histogram {}))
  ([options] (chart-value :histogram histogram/compute histogram/draw histogram/legend options)))

(defn density
  "Kernel density estimate of `:x`, see [[cljplot2.chart.density]]."
  ([] (density {}))
  ([options] (chart-value :density density/compute density/draw density/legend options)))

(defn line
  "Line through `(:x, :y)` points, see [[cljplot2.chart.line]]."
  ([] (line {}))
  ([options] (chart-value :line line/compute line/draw line/legend options)))

(defn treemap
  "Treemap of `:value` over the `:hierarchy` levels, see [[cljplot2.chart.treemap]]."
  ([] (treemap {}))
  ([options] (chart-value :treemap treemap/compute treemap/draw treemap/legend options)))

(defn ecdf
  "Empirical CDF of `:x`, see [[cljplot2.chart.ecdf]]."
  ([] (ecdf {}))
  ([options] (chart-value :ecdf ecdf/compute ecdf/draw ecdf/legend options)))
