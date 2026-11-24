(ns cljplot2.graphics.cells
  "Cell styles and drawing of the cell charts ([[cljplot2.chart.tile]], [[cljplot2.chart.bin2d]]); the cell geometry is in [[cljplot2.data.cells]].

  [[draw-cell!]] draws a cell polygon through a projection, shrunk by `:cell-scale` toward its centre; a chart draws its cells inside [[call-with-cell-rendering]] (abutting cells without seams).

  Functions: options - [[style-entries]]; drawing - [[call-with-cell-rendering]], [[draw-cell!]]."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.data.cells :as cells]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def style-entries
  "Style map entries of a cell chart: `:cell` (type), `:cell-scale` (polygon shrink factor, `(0 1]`), `:stroke-color`
  (a color, `:none` or `:darker`), `:stroke-width`, `:alpha-factor` (fill alpha multiplier, a fraction; theme default 1.0 - colors as they are)."
  (schema/in-fragment
   :cell
   [(schema/optional :cell {:description "cell shape of the grid (a fastmath grid cell type)"} cells/cell-schema)
    (schema/optional :cell-scale {:description "cell polygon size relative to its grid cell, in (0 1]"} [:and schema/finite [:> 0] [:<= 1]])
    (schema/optional :stroke-color {:description "cell outline: a colour, :none or :darker (the darkened fill)"} draw/stroke-color)
    (schema/optional :stroke-width {:description "cell outline width in pixels"} schema/non-neg)
    (schema/optional :alpha-factor {:description "factor multiplying the fill colours' alpha"} schema/fraction)]))

(defn- shrunk
  "Gives the cell corners shrunk toward their mean.

  Parameters:

  - `corners` (sequence of `[x y]`): the corners, not empty.
  - `k` (double): the scale factor, 1 - unchanged.

  Returns the corners scaled by `k` about their mean (`corners` itself for `k` 1)."
  [corners ^double k]
  (if (m/one? k)
    corners
    (let [n (count corners)
          cx (m// (v/sum (map first corners)) n)
          cy (m// (v/sum (map second corners)) n)]
      (mapv (fn [[^double x ^double y]] [(m/+ cx (m/* k (m/- x cx))) (m/+ cy (m/* k (m/- y cy)))]) corners))))

(defn call-with-cell-rendering
  "Calls a drawing function with rendering suited to a chart's cells.

  Abutting cells are filled without antialiasing, so neighbours share their edges - no seams of the panel between them; spaced cells keep smooth edges.

  Parameters:

  - `canvas`: the clojure2d canvas.
  - `style` (map): the chart's style, `:cell-scale` read (default 1; at least 1 - abutting).
  - `f` (function): of the canvas to draw on.

  Returns the result of `f`.

  See also [[draw-cell!]]."
  [canvas {:keys [cell-scale]} f]
  (if (m/>= (double (or cell-scale 1.0)) 1.0)
    ;; antialiasing off: cells side by side share their edges exactly (no seams of the background between them)
    (c2d/with-isolated-canvas [unaliased canvas]
      (c2d/set-rendering-hint unaliased :antialiasing :off)
      (f unaliased))
    (f canvas)))

(defn draw-cell!
  "Draws one cell polygon through a projection.

  Parameters:

  - `canvas`: the clojure2d canvas.
  - `to-pixel` (function): of a corner `[a b]` (any 2d units) giving its pixel `Vec2`.
  - `corners` (sequence of `[a b]`): the cell's corners, shrunk toward their mean by `:cell-scale`.
  - `fill`: the fill colour.
  - `style` (map): `:cell-scale` (default 1), `:alpha-factor` - fill alpha multiplier (none when `nil`), `:stroke-color` - a colour, `:none` or `:darker` (see [[cljplot2.graphics.draw/outline-color]]), `:stroke-width` - outline width in pixels (0 - none).

  Returns `nil`. A corner the projection can't place (`##NaN`) skips the cell.

  See also [[call-with-cell-rendering]]."
  [canvas to-pixel corners fill {:keys [cell-scale stroke-color stroke-width alpha-factor]}]
  (let [points (mapv (fn [p] (to-pixel p)) (shrunk corners (or cell-scale 1.0)))]
    (when (every? data/finite-point? points)
      (let [cell-fill (if alpha-factor (draw/faded fill alpha-factor) fill)
            stroke (draw/outline-color stroke-color cell-fill)]
        (c2d/set-color canvas cell-fill)
        (c2d/path canvas points true false)
        (draw/outline! canvas stroke stroke-width #(c2d/path % points true true))))))
