(ns cljplot2.chart
  "Chart constructors (one per chart kind) and the chart contract.

  A chart value is a plain map `{:kind :options :compute :draw :legend :schema}` made by a constructor; `:compute` and `:draw` are top-level functions, so chart values print and compare as data. A chart's style is the theme `[:chart kind]` with the named style of its `:theme-style` option merged over it and its own style options over both (see [[cljplot2.theme/chart-style]], [[cljplot2.theme/named-styles]]).

  Optional keys of a chart value:

  - `:computed-aesthetics` (optional) - `{dim title}`, or a function of the chart's options and merged mapping returning one: aesthetics the chart computes itself instead of mapping (e.g. [[bin2d]]'s color, titled by its measure): a plot `:scales` entry of such an aesthetic is valid without a chart mapping it, another chart can't map it, and `title` (a string or `nil`) is its legend title unless the plot's scale has a `:title` (the first chart's title wins); see [[cljplot2.scale.aesthetic/computed-aesthetics]].
  - `:coords` (optional, default `#{:cartesian}`) - the coordinate systems the chart draws in (plot `:coord`, see [[cljplot2.scale.coord]]); polar charts: [[bar]], [[segment]], [[scatter]], [[line]], [[text]], [[rule]], [[rect]], [[parallel]] (radar), [[partition]] (sunburst); `#{:three-d}` - 3D charts ([[cloud]], [[surface]]): only with other 3D charts, each with an explicit `:domain` when there are several, see [[cljplot2.data.space3d]]; a 3D chart value also has `:primitives` - `(fn [chart computed point scales env])` giving its primitives for one panel, which the panel draws with every 3D chart's in one scene ([[cljplot2.graphics.space3d/draw-scene!]]).
  - `:schema` (optional for user charts) - `{:options s :style s :mapping s}` malli schemas, checked by `build` before any stage runs: `:options` against the chart's options, `:mapping` against the plot mapping merged with the chart's, `:style` against theme `[:chart kind]` (built-in kinds: [[builtin-styles]], always). Charts don't validate their input themselves.

  The functions of a chart value:

  - `(compute chart {:groups {panel rows} :mapping mapping} env)` -> data `{:extents {panel {dim extent}} :padding {dim [lo hi]} :data {panel computed} :na #{dim} :dropped {dim n}}` (`:padding` - the chart's padding, fractions of the span; optional `:padding-explicit {dim [lo hi]}` - the sides that are requests, `nil` - default: per side the plot takes the largest request of the charts reaching that end of the merged extent, the largest padding only where none does, see [[cljplot2.data.rows/explicit-padding]]) (an extent with `:companion? true` counts only when another chart has extents of its dimension) and optionally `:axis-titles {:x t :y t}` (suggested axis titles; the first chart's win, plot `:labels` win over all) and `:tick-format {:x f :y f}` (suggested tick label format, `:absolute` - magnitudes; used unless the plot's scale has a `:formatter`) and `:scale-defaults {:x attrs :y attrs}` (suggested position scale attributes, e.g. bands `:padding-out`; dropped when the plot's own `:scales` entry sets any of them) and `:padding-px {dim [lo hi]}` (pixels kept free beyond the data on each side of a continuous position scale, e.g. for labels past the data) (`env` - `{:theme :na-level? :plot-scales :flip? :hint :coord :aspect :panel-size}`, `:plot-scales` - the plot's `:scales`, `:flip?` - the plot's `:flip`, `:hint` - the rendering hint, for measuring text, `:aspect` - the plot's `:aspect`, `:panel-size` - a delay of one panel's estimated `[w h]` in pixels (see [[cljplot2.layout.plot-grid/estimated-panel-size]]; deref only when needed - it measures text)); stored per chart in the plot's `:stats`. Aesthetic extents (`:color`, `:shape`, `:size`, `:dash`) are [[cljplot2.data.rows/value-extent]]s; `:na` lists aesthetics with NA (`nil`) values
  - `(draw chart computed canvas panel-box scales env)` - draws one panel's `computed` data (`env` also has the panel's `:panel-key` - its facet level, `[col row]` for a grid, `cljplot2.data.facet/single-panel` without facets); called by the panel with isolated graphics clipped to the panel box. Pixel positions come from `cljplot2.scale.position/projection` (data `x`, `y` -> pixel point; `:x` is vertical in a plot with `:flip true`), never from assuming `:x` is horizontal; it gives `##NaN` for values the scale can't place, and such values are skipped. Shapes that must follow a polar plot's circle are built in range space - `cljplot2.scale.position/range-projection` (or the scales' forward functions, `cljplot2.data.strip/placement` `:range-point`) - and turned into pixels by `cljplot2.scale.coord` (`to-pixel`, `line-path`, `rect-path`, `circle-path`; the identity in cartesian plots). Charts compute in their own frame (`:x`, `:y`); flip and polar coordinates are drawing concerns only.
  - `(legend chart attributes canvas box env)` - optional; draws the chart's key glyph in a legend key `box`, `attributes` - the entry's mapped style attributes (`{:color c :shape s :size z :dash d}`, NA ones absent except color); `env` also has the chart's computed data (`:chart-stats`, see compute). Every chart mapping a legend's dimension draws into its keys, in chart order.

  Options common to all charts: `:data` (own data, see [[cljplot2.data.rows/rows]]) and `:mapping` (merged over the plot's mapping; a chart value with `:inherit-mapping :facets` - the annotation charts - takes only the plot mapping's facet keys).

  Context in every facet panel: a chart whose mapping sets the facet keys to `nil` (`{:facet nil}`, see [[cljplot2.data.facet/facet-dims]]) is drawn in every panel - e.g. all lines greyed behind the faceted ones: `(line {:mapping {:facet nil :color nil :group :g} :color [200 200 200]})` listed before `(line)`.

  Functions: a constructor per built-in kind, each taking an optional options map - [[scatter]], [[line]], [[segment]], [[bar]], [[histogram]], [[density]], [[ecdf]], [[qq]], [[pp]], [[ribbon]], [[smooth]], [[candlestick]], [[parallel]], [[polygon]], [[treemap]], [[partition]], [[pack]], [[waffle]], [[venn]], [[tile]], [[bin2d]], [[field]], [[contour]], [[correlation]], [[cloud]], [[surface]], [[box]], [[bullet]], [[violin]], [[stripplot]], [[beeswarm]], [[dotplot]], [[rug]], [[extent]], [[rule]], [[rect]], [[function]], [[text]], [[image]]; [[builtin-styles]] - the style schema of every built-in kind; [[chart-value?]] - tests for a chart value."
  (:refer-clojure :exclude [partition])
  (:require [cljplot2.chart.bar :as bar]
            [cljplot2.chart.beeswarm :as beeswarm]
            [cljplot2.chart.dotplot :as dotplot]
            [cljplot2.chart.bin2d :as bin2d]
            [cljplot2.chart.box :as box]
            [cljplot2.chart.bullet :as bullet]
            [cljplot2.chart.candlestick :as candlestick]
            [cljplot2.chart.contour :as contour]
            [cljplot2.chart.correlation :as correlation]
            [cljplot2.chart.density :as density]
            [cljplot2.chart.ecdf :as ecdf]
            [cljplot2.chart.extent :as extent]
            [cljplot2.chart.field :as field]
            [cljplot2.chart.function :as function]
            [cljplot2.chart.histogram :as histogram]
            [cljplot2.chart.image :as image]
            [cljplot2.chart.line :as line]
            [cljplot2.chart.pack :as pack]
            [cljplot2.chart.cloud :as cloud]
            [cljplot2.chart.parallel :as parallel]
            [cljplot2.chart.partition :as partition]
            [cljplot2.chart.polygon :as polygon]
            [cljplot2.chart.probability :as probability]
            [cljplot2.chart.rect :as rect]
            [cljplot2.chart.ribbon :as ribbon]
            [cljplot2.chart.rug :as rug]
            [cljplot2.chart.rule :as rule]
            [cljplot2.chart.scatter :as scatter]
            [cljplot2.chart.segment :as segment]
            [cljplot2.chart.smooth :as smooth]
            [cljplot2.chart.stripplot :as stripplot]
            [cljplot2.chart.surface :as surface]
            [cljplot2.chart.text :as text]
            [cljplot2.chart.tile :as tile]
            [cljplot2.chart.treemap :as treemap]
            [cljplot2.chart.venn :as venn]
            [cljplot2.chart.violin :as violin]
            [cljplot2.chart.waffle :as waffle]
            [cljplot2.data.measure :as measure]
            [cljplot2.data.strip :as strip]
            [cljplot2.graphics.space3d :as space-draw]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn chart-value?
  "Tests whether a value is a chart value: a map with a `:compute` function key (see the namespace doc).

  Parameters:

  - `v`: any value (a margin entry, a chart, a plot ...).

  Returns `true` for a chart value, else `false`."
  [v]
  (and (map? v) (contains? v :compute)))

(defn- chart-value
  "A chart value - the plain map of the chart contract (see the namespace doc).

  Parameters:

  - `kind` (keyword): the chart kind, the key of its theme style `[:chart kind]`.
  - `compute`, `draw`, `legend` (functions): the chart's top-level functions; `legend` may be `nil` (no key glyph).
  - `schema` (map): `{:options :style :mapping}` malli schemas.
  - `options` (map or `nil`): the chart's options; `nil` is `{}`.

  Returns `{:kind :options :compute :draw :legend :schema}`."
  [kind compute draw legend schema options]
  {:kind kind :options (if (nil? options) {} options) :compute compute :draw draw :legend legend :schema schema})

(def builtin-styles
  "Style schema of every built-in chart kind: `build` checks theme `[:chart kind]` against it (and every named style of [[cljplot2.theme/named-styles]] merged over its kind)."
  {:scatter (:style scatter/schemas)
   :histogram (:style histogram/schemas)
   :density (:style density/schemas)
   :line (:style line/schemas)
   :treemap (:style treemap/schemas)
   :partition (:style partition/schemas)
   :pack (:style pack/schemas)
   :cloud (:style cloud/schemas)
   :surface (:style surface/schemas)
   :waffle (:style waffle/schemas)
   :venn (:style venn/schemas)
   :tile (:style tile/schemas)
   :bin2d (:style bin2d/schemas)
   :field (:style field/schemas)
   :contour (:style contour/schemas)
   :correlation (:style correlation/schemas)
   :ecdf (:style ecdf/schemas)
   :box (:style box/schemas)
   :bullet (:style bullet/schemas)
   :violin (:style violin/schemas)
   :stripplot (:style stripplot/schemas)
   :beeswarm (:style beeswarm/schemas)
   :dotplot (:style dotplot/schemas)
   :rug (:style rug/schemas)
   :extent (:style extent/schemas)
   :rule (:style rule/schemas)
   :rect (:style rect/schemas)
   :function (:style function/schemas)
   :ribbon (:style ribbon/schemas)
   :smooth (:style smooth/schemas)
   :segment (:style segment/schemas)
   :candlestick (:style candlestick/schemas)
   :bar (:style bar/schemas)
   :parallel (:style parallel/schemas)
   :polygon (:style polygon/schemas)
   :image (:style image/schemas)
   :qq (:style probability/qq-schemas)
   :pp (:style probability/pp-schemas)})

(defn- polar
  "A chart value drawing in polar coordinates too.

  Parameters:

  - `v` (map): a chart value.

  Returns `v` with `:coords #{:cartesian :polar}`."
  [v]
  (assoc v :coords #{:cartesian :polar}))

(defn- annotation-value
  "A chart value of an annotation chart, which inherits only the facet keys of the plot mapping.

  Parameters as [[chart-value]]. Returns the chart value with `:inherit-mapping :facets`."
  [kind compute draw legend schema options]
  (assoc (chart-value kind compute draw legend schema options) :inherit-mapping :facets))

(defn scatter
  "Scatter chart.

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.scatter]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :scatter]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:scatter` - drawn in cartesian and polar coordinates. Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.scatter]]."
  ([] (scatter {}))
  ([options] (polar (chart-value :scatter scatter/compute scatter/draw scatter/legend scatter/schemas options))))

(defn histogram
  "Histogram of `:x`.

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.histogram]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :histogram]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:histogram`. Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.histogram]]."
  ([] (histogram {}))
  ([options] (chart-value :histogram histogram/compute histogram/draw histogram/legend histogram/schemas options)))

(defn density
  "Kernel density estimate of `:x`.

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.density]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :density]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:density`. Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.density]]."
  ([] (density {}))
  ([options] (chart-value :density density/compute density/draw density/legend density/schemas options)))

(defn line
  "Line through `(:x, :y)` points.

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.line]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :line]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:line` - drawn in cartesian and polar coordinates. Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.line]]."
  ([] (line {}))
  ([options] (polar (chart-value :line line/compute line/draw line/legend line/schemas options))))

(defn ribbon
  "Band between `:ymin` and `:ymax` along `:x`.

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.ribbon]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :ribbon]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:ribbon`. Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.ribbon]]."
  ([] (ribbon {}))
  ([options] (chart-value :ribbon ribbon/compute ribbon/draw ribbon/legend ribbon/schemas options)))

(defn smooth
  "Fitted curve of `:y` on `:x` with confidence, prediction or bootstrap bands (`:method` loess, lm, glm, gp, interpolators).

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.smooth]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :smooth]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:smooth`. Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.smooth]]."
  ([] (smooth {}))
  ([options] (chart-value :smooth smooth/compute smooth/draw smooth/legend smooth/schemas options)))

(defn segment
  "Segments from `(:x, :y)` to `(:xend, :yend)` (an end literal, mapped or the start coordinate - lollipops with `{:yend 0}`).

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.segment]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :segment]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:segment` - drawn in cartesian and polar coordinates. Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.segment]]."
  ([] (segment {}))
  ([options] (polar (chart-value :segment segment/compute segment/draw segment/legend segment/schemas options))))

(defn candlestick
  "Candlesticks (or OHLC bars) of `:open :high :low :close` per `:x`.

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.candlestick]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :candlestick]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:candlestick`. Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.candlestick]]."
  ([] (candlestick {}))
  ([options] (chart-value :candlestick candlestick/compute candlestick/draw candlestick/legend candlestick/schemas options)))

(defn bar
  "Bars per level of the categorical axis and group (`{:x level :y value}` vertical, `{:x value :y level}` horizontal, `:level-axis`; `:stat` sum, mean, median, a function or a count measure; dodged, stacked, normalized, mirrored).

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.bar]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :bar]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:bar` - drawn in cartesian and polar coordinates. Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.bar]], [[cljplot2.data.strip]]."
  ([] (bar {}))
  ([options] (polar (chart-value :bar strip/compute strip/draw bar/legend bar/schemas options))))

(defn parallel
  "Parallel coordinates of wide rows over `:variables` (one axis per variable, affine `:scaling`); a radar chart under a polar `:coord`.

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.parallel]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :parallel]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:parallel` - drawn in cartesian and polar coordinates. Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.parallel]]."
  ([] (parallel {}))
  ([options] (polar (chart-value :parallel parallel/compute parallel/draw parallel/legend parallel/schemas options))))

(defn polygon
  "Closed filled polygons through the `(:x, :y)` points of each group in row order.

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.polygon]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :polygon]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:polygon`. Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.polygon]]."
  ([] (polygon {}))
  ([options] (chart-value :polygon polygon/compute polygon/draw polygon/legend polygon/schemas options)))

(defn treemap
  "Treemap of `:value` over the `:hierarchy` levels.

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.treemap]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :treemap]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:treemap`. Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.treemap]]."
  ([] (treemap {}))
  ([options] (chart-value :treemap treemap/compute treemap/draw treemap/legend treemap/schemas options)))

(defn partition
  "Partition of `:value` over the `:hierarchy` levels: an icicle, a sunburst under a polar `:coord`.

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.partition]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :partition]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:partition` - drawn in cartesian and polar coordinates; it computes its own colour aesthetic (a colour scale without a mapping). Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.partition]]."
  ([] (partition {}))
  ([options] (polar (assoc (chart-value :partition partition/compute partition/draw partition/legend partition/schemas options)
                           :computed-aesthetics partition/computed-aesthetics))))

(defn cloud
  "3D scatter (points, spikes, bars; side projections; a 2D histogram with `:bins`) of `:x`, `:y`, `:z` in a projected box.

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.cloud]]), `:data` and `:mapping` (taken by every chart), and the point style keys over the theme style `[:chart :cloud]` (`:projection-alpha` and `:band-alpha` are theme only, not options), and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:cloud` - a 3D chart, drawn only with other 3D charts. Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.cloud]]."
  ([] (cloud {}))
  ([options] (assoc (chart-value :cloud cloud/compute space-draw/draw-chart cloud/legend cloud/schemas options)
                    :coords #{:three-d} :primitives cloud/chart-primitives)))

(defn surface
  "3D surface (gridded `:x :y :z`, a function `:f`, a density, a parametric `:fu`) of shaded quads or a wireframe in a projected box.

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.surface]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :surface]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:surface` - a 3D chart, drawn only with other 3D charts; it computes its own colour aesthetic (a colour scale without a mapping). Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.surface]]."
  ([] (surface {}))
  ([options] (assoc (chart-value :surface surface/compute space-draw/draw-chart nil surface/schemas options)
                    :coords #{:three-d} :primitives surface/chart-primitives
                    :computed-aesthetics surface/computed-aesthetics)))

(defn pack
  "Circle packing of `:value` over the `:hierarchy` levels.

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.pack]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :pack]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:pack`. Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.pack]]."
  ([] (pack {}))
  ([options] (chart-value :pack pack/compute pack/draw pack/legend pack/schemas options)))

(defn waffle
  "Waffle of the `:color` categories' shares (or counts with `:unit`) as grid cells.

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.waffle]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :waffle]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:waffle`. Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.waffle]]."
  ([] (waffle {}))
  ([options] (chart-value :waffle waffle/compute waffle/draw waffle/legend waffle/schemas options)))

(defn venn
  "Venn diagram of 2 or 3 sets from `:set` / `:item` membership rows.

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.venn]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :venn]` (`:label-gap` is theme only, not an option), and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:venn`. Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.venn]]."
  ([] (venn {}))
  ([options] (chart-value :venn venn/compute venn/draw venn/legend venn/schemas options)))

(defn tile
  "Heatmap cells of any `fastmath.grid` type at (`:x`, `:y`), filled by `:color`.

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.tile]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :tile]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:tile`. Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.tile]]."
  ([] (tile {}))
  ([options] (chart-value :tile tile/compute tile/draw tile/legend tile/schemas options)))

(defn bin2d
  "Points counted in grid cells (hexbin, 2D histogram).

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.bin2d]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :bin2d]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:bin2d` - it computes its own colour aesthetic (a colour scale without a mapping). Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.bin2d]]."
  ([] (bin2d {}))
  ([options] (assoc (chart-value :bin2d bin2d/compute bin2d/draw nil bin2d/schemas options)
                     :computed-aesthetics {:color (measure/titles (get options :measure :count))})))

(defn ecdf
  "Empirical CDF of `:x`.

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.ecdf]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :ecdf]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:ecdf`. Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.ecdf]]."
  ([] (ecdf {}))
  ([options] (chart-value :ecdf ecdf/compute ecdf/draw ecdf/legend ecdf/schemas options)))

(defn- probability-value
  "A chart value of a probability plot (Q-Q or P-P).

  Parameters:

  - `kind` (keyword): `:qq` or `:pp`.
  - `compute` (function): the plot's compute function.
  - `schema` (map): its schemas.
  - `options` (map or `nil`): the chart's options.

  Returns an annotation chart value (every panel, facet keys only, see [[annotation-value]]) when both sides are distributions, else a plain one (see [[chart-value]])."
  [kind compute schema options]
  (let [given (if (nil? options) {} options)]
    ((if (probability/distributions-only? given) annotation-value chart-value)
     kind compute probability/draw probability/legend schema given)))

(defn qq
  "Q-Q plot: quantiles of `:x` / `:x-distribution` against `:y` / `:y-distribution`.

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.probability]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :qq]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:qq` - an annotation chart (facet keys only, drawn in every panel) when both sides are distributions. Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.probability]]."
  ([] (qq {}))
  ([options] (probability-value :qq probability/qq-compute probability/qq-schemas options)))

(defn pp
  "P-P plot: probabilities of `:x` / `:x-distribution` against `:y` / `:y-distribution`.

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.probability]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :pp]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:pp` - an annotation chart (facet keys only, drawn in every panel) when both sides are distributions. Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.probability]]."
  ([] (pp {}))
  ([options] (probability-value :pp probability/pp-compute probability/pp-schemas options)))

(defn box
  "Box plot of the values per level (the categorical axis, `:level-axis`) and group.

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.box]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :box]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:box`. Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.box]], [[cljplot2.data.strip]]."
  ([] (box {}))
  ([options] (chart-value :box strip/compute strip/draw box/legend box/schemas options)))

(defn bullet
  "Bullet chart of ranges, measures and markers per level (the categorical axis, `:level-axis`) and group.

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.bullet]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :bullet]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:bullet`. Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.bullet]], [[cljplot2.data.strip]]."
  ([] (bullet {}))
  ([options] (chart-value :bullet strip/compute strip/draw nil bullet/schemas options)))

(defn violin
  "Violin plot of the values per level (the categorical axis, `:level-axis`) and group.

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.violin]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :violin]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:violin`. Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.violin]], [[cljplot2.data.strip]]."
  ([] (violin {}))
  ([options] (chart-value :violin strip/compute strip/draw violin/legend violin/schemas options)))

(defn stripplot
  "Jittered strip plot of the values per level (the categorical axis, `:level-axis`) and group.

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.stripplot]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :stripplot]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:stripplot`. Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.stripplot]], [[cljplot2.data.strip]]."
  ([] (stripplot {}))
  ([options] (chart-value :stripplot strip/compute strip/draw stripplot/legend stripplot/schemas options)))

(defn beeswarm
  "Beeswarm of the values per level (the categorical axis, `:level-axis`) and group (points packed without overlap).

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.beeswarm]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :beeswarm]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:beeswarm`. Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.beeswarm]], [[cljplot2.data.strip]]."
  ([] (beeswarm {}))
  ([options] (chart-value :beeswarm strip/compute strip/draw beeswarm/legend beeswarm/schemas options)))

(defn dotplot
  "Dot plot of the values per level (the categorical axis, `:level-axis`): binned values as stacks of dots (Wilkinson dot density or fixed bins).

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.dotplot]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :dotplot]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:dotplot`. Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.dotplot]], [[cljplot2.data.strip]]."
  ([] (dotplot {}))
  ([options] (chart-value :dotplot strip/compute strip/draw dotplot/legend dotplot/schemas options)))

(defn rug
  "Rug of the values (per level of the categorical axis when one is mapped, `:level-axis`).

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.rug]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :rug]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:rug`. Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.rug]], [[cljplot2.data.strip]]."
  ([] (rug {}))
  ([options] (chart-value :rug strip/compute strip/draw rug/legend rug/schemas options)))

(defn extent
  "Interval of the values per level (the categorical axis, `:level-axis`) and group (fastmath.stats extents).

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.extent]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :extent]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:extent`. Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.extent]], [[cljplot2.data.strip]]."
  ([] (extent {}))
  ([options] (chart-value :extent strip/compute strip/draw extent/legend extent/schemas options)))

(defn rule
  "Reference lines (`:x`, `:y`, `:slope` / `:intercept`).

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.rule]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :rule]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:rule` - drawn in cartesian and polar coordinates; an annotation chart, taking only the facet keys of the plot mapping. Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.rule]], [[cljplot2.data.annotation]]."
  ([] (rule {}))
  ([options] (polar (annotation-value :rule rule/compute rule/draw rule/legend rule/schemas options))))

(defn rect
  "Rectangles and bands (`:x [a b]`, `:y [a b]`).

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.rect]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :rect]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:rect` - drawn in cartesian and polar coordinates; an annotation chart, taking only the facet keys of the plot mapping. Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.rect]], [[cljplot2.data.annotation]]."
  ([] (rect {}))
  ([options] (polar (annotation-value :rect rect/compute rect/draw rect/legend rect/schemas options))))

(defn function
  "Curves of functions (`:f` - y = f(x), `:parametric` - [x y] = f(t)) sampled across the panel.

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.function]]), its own `:mapping` (no `:data` - it samples its functions), and the style keys over the theme style `[:chart :function]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:function` - an annotation chart, taking only the facet keys of the plot mapping. Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.function]]."
  ([] (function {}))
  ([options] (annotation-value :function function/compute function/draw function/legend function/schemas
                               (function/categorical-labels options))))

(defn text
  "Text labels at positions.

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.text]]), `:data` and `:mapping` (taken by every chart), and the text style keys over the theme text role of `:role` (default `:annotation`).

  Returns a chart value of kind `:text` - drawn in cartesian and polar coordinates; an annotation chart, taking only the facet keys of the plot mapping. Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.text]], [[cljplot2.data.annotation]]."
  ([] (text {}))
  ([options] (polar (annotation-value :text text/compute text/draw text/legend text/schemas options))))

(defn field
  "Raster of a 2D field - the density of points, interpolated gridded `:value`s or a function of a point `[x y]` - colored by value.

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.field]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :field]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:field` - it computes its own colour aesthetic (a colour scale without a mapping). Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.field]]."
  ([] (field {}))
  ([options] (assoc (chart-value :field field/compute field/draw nil field/schemas options)
                    :computed-aesthetics field/computed-aesthetics)))

(defn image
  "A free chart: an image of each panel drawn by a callback from the finalized position scales.

  Parameters:

  - `options` (map, optional): the chart's options (see [[cljplot2.chart.image]]); the chart has no theme style.

  Returns a chart value of kind `:image` - an annotation chart, taking only the facet keys of the plot mapping. Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here. `:f` is required (the schema rejects a chart without it).

  See also [[cljplot2.chart.image]]."
  ([] (image {}))
  ([options] (annotation-value :image image/compute image/draw nil image/schemas options)))

(defn contour
  "Contour lines or filled regions of a 2D field (density, gridded `:value`s, a function).

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.contour]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :contour]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:contour` - it computes its own colour aesthetic (a colour scale without a mapping). Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.contour]]."
  ([] (contour {}))
  ([options] (assoc (chart-value :contour contour/compute contour/draw contour/legend contour/schemas options)
                    :computed-aesthetics contour/computed-aesthetics)))

(defn correlation
  "Correlation coefficient of the panel's `:x` / `:y` drawn as a glyph (number, shade, square, circle, ellipse, pie) - a correlogram cell.

  Parameters:

  - `options` (map, optional): the chart's options - its own options (see [[cljplot2.chart.correlation]]), `:data` and `:mapping` (taken by every chart), and the style keys over the theme style `[:chart :correlation]`, and `:theme-style` - a named theme style merged over it (see [[cljplot2.theme/chart-style]]).

  Returns a chart value of kind `:correlation` - it computes its own colour aesthetic (a colour scale without a mapping). Options are validated by `build` (see [[cljplot2.pipeline/build]]), not here.

  See also [[cljplot2.chart.correlation]], [[cljplot2.multiplot/pairs]]."
  ([] (correlation {}))
  ([options] (assoc (chart-value :correlation correlation/compute correlation/draw correlation/legend correlation/schemas options)
                    :computed-aesthetics correlation/computed-aesthetics)))
