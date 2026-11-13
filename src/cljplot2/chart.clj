(ns cljplot2.chart
  "Charts. A chart value is a plain map `{:kind :options :compute :draw :legend :schema}` made by a constructor;
  `:compute` and `:draw` are top-level functions, so chart values print and compare as data.

  `:schema` (optional for user charts) - `{:options s :style s :mapping s}` malli schemas, checked by `build` before
  any stage runs: `:options` against the chart's options, `:mapping` against the plot mapping merged with the chart's,
  `:style` against theme `[:chart kind]` (built-in kinds: [[builtin-styles]], always). Charts don't validate their
  input themselves.

  * `(compute chart {:groups {panel rows} :mapping mapping} env)` -> data
    `{:extents {panel {dim extent}} :padding {dim [lo hi]} :data {panel computed} :na #{dim} :dropped {dim n}}`
    and optionally `:axis-titles {:x t :y t}` (suggested axis titles; the first chart's win, plot `:labels` win over
    all) and `:tick-format {:x f :y f}` (suggested tick label format, `:absolute` - magnitudes; used unless the plot's
    scale has a `:formatter`) (`env` - `{:theme :na-level? :plot-scales}`, `:plot-scales` - the plot's `:scales`); stored per chart in the
    plot's `:stats`. Aesthetic extents (`:color`,
    `:shape`, `:size`) are [[cljplot2.data.rows/value-extent]]s; `:na` lists aesthetics with NA (`nil`) values
  * `(draw chart computed canvas panel-box scales env)` - draws one panel's `computed` data; called by the panel
    with isolated graphics clipped to the panel box. Pixel positions come only from
    `cljplot2.scale.position/projection` (data `x`, `y` -> pixel point; `:x` is vertical in a plot with `:flip true`),
    never from assuming `:x` is horizontal; it gives `##NaN` for values the scale can't place, and such values are
    skipped. Charts compute in their own frame (`:x`, `:y`); flip is a drawing concern only.
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

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- chart-value
  [kind compute draw legend schema options]
  {:kind kind :options (if (nil? options) {} options) :compute compute :draw draw :legend legend :schema schema})

(def builtin-styles
  "Style schema of every built-in chart kind: `build` checks theme `[:chart kind]` against it."
  {:scatter (:style scatter/Schema)
   :histogram (:style histogram/Schema)
   :density (:style density/Schema)
   :line (:style line/Schema)
   :treemap (:style treemap/Schema)
   :ecdf (:style ecdf/Schema)})

(defn scatter
  "Scatter chart, see [[cljplot2.chart.scatter]]."
  ([] (scatter {}))
  ([options] (chart-value :scatter scatter/compute scatter/draw scatter/legend scatter/Schema options)))

(defn histogram
  "Histogram of `:x`, see [[cljplot2.chart.histogram]]."
  ([] (histogram {}))
  ([options] (chart-value :histogram histogram/compute histogram/draw histogram/legend histogram/Schema options)))

(defn density
  "Kernel density estimate of `:x`, see [[cljplot2.chart.density]]."
  ([] (density {}))
  ([options] (chart-value :density density/compute density/draw density/legend density/Schema options)))

(defn line
  "Line through `(:x, :y)` points, see [[cljplot2.chart.line]]."
  ([] (line {}))
  ([options] (chart-value :line line/compute line/draw line/legend line/Schema options)))

(defn treemap
  "Treemap of `:value` over the `:hierarchy` levels, see [[cljplot2.chart.treemap]]."
  ([] (treemap {}))
  ([options] (chart-value :treemap treemap/compute treemap/draw treemap/legend treemap/Schema options)))

(defn ecdf
  "Empirical CDF of `:x`, see [[cljplot2.chart.ecdf]]."
  ([] (ecdf {}))
  ([options] (chart-value :ecdf ecdf/compute ecdf/draw ecdf/legend ecdf/Schema options)))
