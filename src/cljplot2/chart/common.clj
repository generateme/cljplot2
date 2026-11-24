(ns cljplot2.chart.common
  "Option schema pieces several chart kinds share. Chart namespaces don't require each other (CLAUDE.md, Layers), so a schema entry or rule two charts take lives here; shared statistics, geometry and drawing live in the lower layers, shared key glyphs in [[cljplot2.chart.legend]].

  Functions and vars: [[fill-gradient-rule]] (line, ribbon, density), [[point-style-entries]] (scatter, ribbon, cloud, qq, pp)."
  (:require [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def fill-gradient-rule
  "Rule (see [[cljplot2.schema/with-rules]]) of a chart's options: `:fill-gradient-axis` needs `:fill-gradient`; an error at `[:fill-gradient-axis]`."
  [#(when (and (:fill-gradient-axis %) (nil? (:fill-gradient %))) ":fill-gradient-axis needs :fill-gradient") [:fill-gradient-axis]])

(def point-style-entries
  "Map entries of a point style (all optional; defaults from the theme)."
  [(schema/optional :size {:description "point size in pixels (a mapped :size wins)"} schema/non-neg)
   (schema/optional :color {:description "point colour (a mapped :color wins)"} schema/color)
   (schema/optional :stroke-color {:description "outline colour: a colour or :none"} schema/color)
   (schema/optional :stroke-width {:description "outline width in pixels"} schema/non-neg)
   (schema/optional :shape {:description "point shape (a mapped :shape wins)"} schema/shape)])

