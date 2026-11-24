(ns cljplot2.chart.legend
  "Legend key glyphs shared by several chart kinds - the chart contract's `:legend` function (see [[cljplot2.chart]]).

  Functions: [[stroked-point-legend]] (scatter, qq, pp), [[point-legend]] (stripplot, beeswarm), [[line-legend]] (rule, ecdf), [[cell-legend]] (partition, treemap, waffle)."
  (:require [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.graphics.shape :as shape]
            [cljplot2.theme :as theme]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn stroked-point-legend
  "Draws a point key glyph stroked at the chart's width: its shape fitted and centred in the key box, line shapes and outlines at the chart's `:stroke-width`, its colour at the style's `:alpha` (scatter, cloud points, probability plots; see [[cljplot2.chart.common/point-style-entries]]).

  Parameters:

  - `chart` (map): the chart value.
  - `attributes` (map): the key's mapped attributes (`:color`, `:shape`, `:size`), merged over the chart's resolved style.
  - `canvas`: the clojure2d canvas.
  - `box` (map): the key box `{:x :y :w :h}` in pixels.
  - `env` (map): the legend environment - `:theme`.

  Drawn for its effect on the canvas; a `nil` `:alpha` keeps the colour's own alpha (the outline always does). See
  also [[cljplot2.graphics.shape/styled-key!]], [[point-legend]]."
  [chart attributes canvas box env]
  (let [st (theme/key-style env chart attributes nil)]
    (shape/styled-key! canvas box (update st :color draw/with-alpha (:alpha st)) true)))

(defn point-legend
  "Draws a point key glyph with the legend's stroke: its shape fitted and centred in the key box, the canvas stroke kept (stripplot, beeswarm).

  Parameters:

  - `chart` (map): the chart value.
  - `attributes` (map): the key's mapped attributes, merged over the chart's resolved style.
  - `canvas`: the clojure2d canvas.
  - `box` (map): the key box `{:x :y :w :h}` in pixels.
  - `env` (map): the legend environment - `:theme`.

  Drawn for its effect on the canvas. See also [[stroked-point-legend]] (stroked at the chart's width)."
  [chart attributes canvas box env]
  (shape/styled-key! canvas box (theme/key-style env chart attributes nil) false))

(defn line-legend
  "Draws a line chart's key glyph: a horizontal line through the middle of the key box.

  Parameters:

  - `chart` (map): the chart value.
  - `attributes` (map): the key's mapped attributes - `:color` and `:dash` are used.
  - `canvas`: the clojure2d canvas.
  - `box` (map): the key box `{:x :y :w :h}` in pixels.
  - `env` (map): the legend environment - `:theme`.

  Drawn for its effect on the canvas. See also [[cljplot2.graphics.polyline/key-glyph!]]."
  [chart attributes canvas box env]
  (polyline/key-glyph! canvas box (theme/key-style env chart attributes [:color :dash]) :none))

(defn cell-legend
  "Draws a space-filling chart's key glyph: a square of the key's colour with the chart's fill pattern, without an outline.

  Parameters:

  - `chart` (map): the chart value.
  - `attributes` (map): the key's mapped attributes - `:color` is used.
  - `canvas`: the clojure2d canvas.
  - `box` (map): the key box `{:x :y :w :h}` in pixels.
  - `env` (map): the legend environment - `:theme`.

  The pattern and its colour come from the chart's resolved style (`:pattern`, `:pattern-color`). Drawn for its effect on the canvas. See also [[cljplot2.graphics.draw/bar-key!]]."
  [chart attributes canvas box env]
  (draw/bar-key! canvas box (assoc (select-keys (theme/resolved-style (:theme env) chart) [:pattern :pattern-color])
                                   :color (:color attributes) :stroke-width 0)
                 false))
