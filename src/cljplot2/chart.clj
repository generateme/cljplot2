(ns cljplot2.chart
  "Charts. A chart value is a plain map `{:kind :options :compute :draw :legend :schema}` made by a constructor;
  `:compute` and `:draw` are top-level functions, so chart values print and compare as data.

  `:schema` (optional for user charts) - `{:options s :style s :mapping s}` malli schemas, checked by `build` before
  any stage runs: `:options` against the chart's options, `:mapping` against the plot mapping merged with the chart's,
  `:style` against theme `[:chart kind]` (built-in kinds: [[builtin-styles]], always). Charts don't validate their
  input themselves.

  * `(compute chart {:groups {panel rows} :mapping mapping} env)` -> data
    `{:extents {panel {dim extent}} :padding {dim [lo hi]} :data {panel computed} :na #{dim} :dropped {dim n}}`
    (an extent with `:companion? true` counts only when another chart has extents of its dimension)
    and optionally `:axis-titles {:x t :y t}` (suggested axis titles; the first chart's win, plot `:labels` win over
    all) and `:tick-format {:x f :y f}` (suggested tick label format, `:absolute` - magnitudes; used unless the plot's
    scale has a `:formatter`) and `:scale-defaults {:x attrs :y attrs}` (suggested position scale attributes, e.g.
    bands `:padding-out`; dropped when the plot's own `:scales` entry sets any of them) and `:padding-px {dim [lo hi]}`
    (pixels kept free beyond the data on each side of a continuous position scale, e.g. for labels past the data)
    (`env` - `{:theme :na-level? :plot-scales :flip? :hint}`, `:plot-scales` - the plot's `:scales`, `:flip?` - the
    plot's `:flip`, `:hint` - the rendering hint, for measuring text); stored per chart in the
    plot's `:stats`. Aesthetic extents (`:color`,
    `:shape`, `:size`, `:dash`) are [[cljplot2.data.rows/value-extent]]s; `:na` lists aesthetics with NA (`nil`) values
  * `(draw chart computed canvas panel-box scales env)` - draws one panel's `computed` data; called by the panel
    with isolated graphics clipped to the panel box. Pixel positions come only from
    `cljplot2.scale.position/projection` (data `x`, `y` -> pixel point; `:x` is vertical in a plot with `:flip true`),
    never from assuming `:x` is horizontal; it gives `##NaN` for values the scale can't place, and such values are
    skipped. Charts compute in their own frame (`:x`, `:y`); flip is a drawing concern only.
  * `(legend chart attributes canvas box env)` - optional; draws the chart's key glyph in a legend key `box`,
    `attributes` - the entry's mapped style attributes (`{:color c :shape s :size z :dash d}`, NA ones absent except color);
    `env` also has the chart's computed data (`:chart-stats`, see compute).
    Every chart mapping a legend's dimension draws into its keys, in chart order.

  Options common to all charts: `:data` (own data, see [[cljplot2.data.rows/rows]]) and `:mapping` (merged over the
  plot's mapping; a chart value with `:inherit-mapping :facets` - the annotation charts - takes only the plot
  mapping's facet keys)."
  (:require [cljplot2.chart.beeswarm :as beeswarm]
            [cljplot2.chart.box :as box]
            [cljplot2.chart.density :as density]
            [cljplot2.chart.ecdf :as ecdf]
            [cljplot2.chart.extent :as extent]
            [cljplot2.chart.function :as function]
            [cljplot2.chart.histogram :as histogram]
            [cljplot2.chart.line :as line]
            [cljplot2.chart.rect :as rect]
            [cljplot2.chart.rug :as rug]
            [cljplot2.chart.rule :as rule]
            [cljplot2.chart.scatter :as scatter]
            [cljplot2.chart.stripplot :as stripplot]
            [cljplot2.chart.text :as text]
            [cljplot2.chart.treemap :as treemap]
            [cljplot2.chart.violin :as violin]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- chart-value
  [kind compute draw legend schema options]
  {:kind kind :options (if (nil? options) {} options) :compute compute :draw draw :legend legend :schema schema})

(def builtin-styles
  "Style schema of every built-in chart kind: `build` checks theme `[:chart kind]` against it."
  {:scatter (:style scatter/schemas)
   :histogram (:style histogram/schemas)
   :density (:style density/schemas)
   :line (:style line/schemas)
   :treemap (:style treemap/schemas)
   :ecdf (:style ecdf/schemas)
   :box (:style box/schemas)
   :violin (:style violin/schemas)
   :stripplot (:style stripplot/schemas)
   :beeswarm (:style beeswarm/schemas)
   :rug (:style rug/schemas)
   :extent (:style extent/schemas)
   :rule (:style rule/schemas)
   :rect (:style rect/schemas)
   :function (:style function/schemas)})

(defn- annotation-value
  "Chart value of an annotation chart: it inherits only the facet keys of the plot mapping."
  [kind compute draw legend schema options]
  (assoc (chart-value kind compute draw legend schema options) :inherit-mapping :facets))

(defn scatter
  "Scatter chart, see [[cljplot2.chart.scatter]]."
  ([] (scatter {}))
  ([options] (chart-value :scatter scatter/compute scatter/draw scatter/legend scatter/schemas options)))

(defn histogram
  "Histogram of `:x`, see [[cljplot2.chart.histogram]]."
  ([] (histogram {}))
  ([options] (chart-value :histogram histogram/compute histogram/draw histogram/legend histogram/schemas options)))

(defn density
  "Kernel density estimate of `:x`, see [[cljplot2.chart.density]]."
  ([] (density {}))
  ([options] (chart-value :density density/compute density/draw density/legend density/schemas options)))

(defn line
  "Line through `(:x, :y)` points, see [[cljplot2.chart.line]]."
  ([] (line {}))
  ([options] (chart-value :line line/compute line/draw line/legend line/schemas options)))

(defn treemap
  "Treemap of `:value` over the `:hierarchy` levels, see [[cljplot2.chart.treemap]]."
  ([] (treemap {}))
  ([options] (chart-value :treemap treemap/compute treemap/draw treemap/legend treemap/schemas options)))

(defn ecdf
  "Empirical CDF of `:x`, see [[cljplot2.chart.ecdf]]."
  ([] (ecdf {}))
  ([options] (chart-value :ecdf ecdf/compute ecdf/draw ecdf/legend ecdf/schemas options)))

(defn box
  "Box plot of `:x` per `:y` level and group, see [[cljplot2.chart.box]] and [[cljplot2.data.strip]]."
  ([] (box {}))
  ([options] (chart-value :box box/compute box/draw box/legend box/schemas options)))

(defn violin
  "Violin plot of `:x` per `:y` level and group, see [[cljplot2.chart.violin]] and [[cljplot2.data.strip]]."
  ([] (violin {}))
  ([options] (chart-value :violin violin/compute violin/draw violin/legend violin/schemas options)))

(defn stripplot
  "Jittered strip plot of `:x` per `:y` level and group, see [[cljplot2.chart.stripplot]] and [[cljplot2.data.strip]]."
  ([] (stripplot {}))
  ([options] (chart-value :stripplot stripplot/compute stripplot/draw stripplot/legend stripplot/schemas options)))

(defn beeswarm
  "Beeswarm of `:x` per `:y` level and group (points packed without overlap), see [[cljplot2.chart.beeswarm]] and
  [[cljplot2.data.strip]]."
  ([] (beeswarm {}))
  ([options] (chart-value :beeswarm beeswarm/compute beeswarm/draw beeswarm/legend beeswarm/schemas options)))

(defn rug
  "Rug of `:x` values, see [[cljplot2.chart.rug]] and [[cljplot2.data.strip]]."
  ([] (rug {}))
  ([options] (chart-value :rug rug/compute rug/draw rug/legend rug/schemas options)))

(defn extent
  "Interval of `:x` per `:y` level and group (fastmath.stats extents), see [[cljplot2.chart.extent]] and
  [[cljplot2.data.strip]]."
  ([] (extent {}))
  ([options] (chart-value :extent extent/compute extent/draw extent/legend extent/schemas options)))

(defn rule
  "Reference lines (`:x`, `:y`, `:slope` / `:intercept`), see [[cljplot2.chart.rule]] and
  [[cljplot2.data.annotation]]."
  ([] (rule {}))
  ([options] (annotation-value :rule rule/compute rule/draw rule/legend rule/schemas options)))

(defn rect
  "Rectangles and bands (`:x [a b]`, `:y [a b]`), see [[cljplot2.chart.rect]] and [[cljplot2.data.annotation]]."
  ([] (rect {}))
  ([options] (annotation-value :rect rect/compute rect/draw rect/legend rect/schemas options)))

(defn function
  "Curves of functions (`:f` - y = f(x), `:parametric` - [x y] = f(t)) sampled across the panel, see
  [[cljplot2.chart.function]]."
  ([] (function {}))
  ([options] (annotation-value :function function/compute function/draw function/legend function/schemas
                               (function/categorical-labels options))))

(defn text
  "Text labels at positions, see [[cljplot2.chart.text]] and [[cljplot2.data.annotation]]."
  ([] (text {}))
  ([options] (annotation-value :text text/compute text/draw text/legend text/schemas options)))
