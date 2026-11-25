(ns cljplot2.chart.common
  "Option schema pieces several chart kinds share. Chart namespaces don't require each other (CLAUDE.md, Layers), so a schema entry or rule two charts take lives here; shared statistics, geometry and drawing live in the lower layers, shared key glyphs in [[cljplot2.chart.legend]].

  Functions and vars: [[fill-gradient-rule]] (line, ribbon, density), [[point-style-entries]] (scatter, cloud, qq, pp),
  [[stat-space-option]] (histogram, density, smooth).

  Stat space: a chart whose statistic depends on the coordinates it runs in (bins, a kernel estimate, a fit) takes
  `:stat-space` - `:data` (data units) or `:scale` (the coordinates of the axis's position scale, see
  [[cljplot2.scale.position/stat-transform-for]]: log10 on a `:log` axis, data units on every other kind). Its
  default is the chart's behaviour before the option existed: `:data` for histogram and density, `:scale` for smooth;
  charts without the option (bin2d, contour, field, scatter jitter) always run in `:scale` space. In `:scale` space
  values the transform can't place (0 and negatives on `:log`) are dropped and counted (see
  [[cljplot2.data.rows/transformed-numbers]]), and the results are taken back to data units for drawing; a parameter
  in x units (a density's numeric bandwidth) is in the transformed units then, explicit histogram breaks stay in data
  units. Values are numbers in either space: histogram and density take no dates (`:datetime` x values are not
  numbers - an all-date x throws `{:key :mapping}`); smooth fits dates (as epoch milliseconds) in both."
  (:require [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def fill-gradient-rule
  "Rule (see [[cljplot2.schema/with-rules]]) of a chart's options: `:fill-gradient-axis` needs `:fill-gradient`; an error at `[:fill-gradient-axis]`."
  [#(when (and (:fill-gradient-axis %) (nil? (:fill-gradient %))) ":fill-gradient-axis needs :fill-gradient") [:fill-gradient-axis]])

(def point-style-entries
  "Map entries of a point style (all optional; defaults from the theme; `:alpha` has none - `nil` keeps the colours'
  own alpha, applied by the charts and by [[cljplot2.chart.legend/stroked-point-legend]])."
  [(schema/optional :size {:description "point size in pixels (a mapped :size wins)"} schema/non-neg)
   (schema/optional :color {:description "point colour (a mapped :color wins)"} schema/color)
   (schema/optional :stroke-color {:description "outline colour: a colour or :none"} schema/color)
   (schema/optional :stroke-width {:description "outline width in pixels"} schema/non-neg)
   (schema/optional :shape {:description "point shape (a mapped :shape wins)"} schema/shape)
   (schema/nilable :alpha {:description "opacity of every point colour, mapped, NA or not, and of the legend keys (the outline keeps its own)"
                           :cljplot2/default-doc "the colours' own alpha"}
                   schema/fraction)])

(defn stat-space-option
  "Gives the `:stat-space` option entry of a chart: where its statistic runs - `:data` (data units) or `:scale` (the position scale's stat coordinates, see [[cljplot2.scale.position/stat-transform-for]]).

  Parameters:

  - `default` (keyword): the chart's default, `:data` or `:scale`.
  - `description` (string): the entry's description (what runs in that space).

  Returns a nilable schema entry `:stat-space`."
  [default description]
  (schema/nilable :stat-space {:description description :default default} [:enum :data :scale]))
