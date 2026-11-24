(ns cljplot2.chart.statistic-cell
  "Statistic cells: the parts the [[cljplot2.chart.correlation]] and [[cljplot2.chart.pairwise]] charts share - one statistic of the panel's `:x` / `:y` drawn as a cell filling the panel, a line per group under it.

  Functions: [[diverging-range]] - the colour range of a signed statistic; [[grouping]] - the panel groups of a mapping; [[colored-by-value?]] - whether the cell takes the value's colour; [[value-fill]] - a cell's fill; [[value-lines]] - the text lines of a cell; [[dropped]] - the non-finite counts; [[legend]] - the key glyph."
  (:require [fastmath.core :as m]
            [cljplot2.data.group :as group]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def diverging-range
  "The colour range of a signed statistic (zero neutral - white in the middle): the computed colour's `:range` of correlations and signed pairwise statistics; `:scales {:color {:range ..}}` changes it."
  :RColorBrewer/RdBu)

(defn- grouped?
  "Tests whether a mapping groups a cell chart: an ordinal aesthetic or `:group` (see [[cljplot2.data.group/group-dims]]).

  Parameters:

  - `mapping` (map): the merged mapping.

  Returns the grouping dimensions (truthy), else `nil`."
  [mapping]
  (seq (group/group-dims mapping)))

(defn colored-by-value?
  "Tests whether a cell chart takes the value's colour (a computed colour).

  Parameters:

  - `options` (map): the chart's options - `:color`, `:glyph`.
  - `mapping` (map): the merged mapping.

  Returns true unless a fixed `:color`, the `:number` glyph (the default) or a mapped aesthetic colours the cell (the plot's colour then belongs to the mapping, also a continuous colour of the other charts); else `false`."
  [options mapping]
  (not (or (:color options) (= :number (data/option options :glyph :number)) (grouped? mapping))))

(defn grouping
  "Gives the panel groups of a cell chart.

  Ordinal aesthetics and `:group` group the chart; a continuous colour colours the other charts' marks and doesn't.

  Parameters:

  - `groups` (map): `{panel rows}`.
  - `mapping` (map): the merged mapping.
  - `env` (map): the compute environment (grouping settings).

  Returns `{:grouped g :split s}`: `g` - the grouping dimensions or `nil`; `s` - the split groups (see [[cljplot2.data.group/split]]) when grouped."
  [groups mapping env]
  (let [continuous (group/continuous-color groups mapping env)
        grouping-mapping (if continuous (:mapping continuous) mapping)
        grouped (grouped? grouping-mapping)]
    {:grouped grouped
     :split (when grouped (group/split groups grouping-mapping env))}))

(defn value-fill
  "Gives the fill of a cell.

  Parameters:

  - `v` (double): the cell's value.
  - `by-value?`: the cell takes the value's colour (see [[colored-by-value?]]).
  - `fixed`: the fixed colour (the style's `:color`).
  - `scales` (map): the panel's scales (`:color` when coloured by value).
  - `env` (map): the drawing environment (the theme's NA colour).

  Returns the value's colour through the colour scale when coloured by value (`nil` for `##NaN` or a value outside the scale - nothing drawn), else `fixed`."
  [v by-value? fixed scales env]
  (if by-value?
    (when-not (m/nan? (double v))
      ((aesthetic/attribute-fn :color (:color scales) (get-in env [:theme :palette :na])) v))
    fixed))

(defn value-lines
  "Gives the text lines of a cell: a first line, and a line per group in the group's colour.

  Parameters:

  - `first-line` (map): `{:text :color}` of the panel's line (`:color` `nil` - the text role's).
  - `groups` (sequence): `[{:aesthetics a :value v} ..]` of the panel, `nil` ungrouped.
  - `text-of` (function): the text of a value.
  - `base-color`: the colour of a group line without a colour aesthetic (e.g. grouped by `:group`).
  - `scales` (map): the panel's scales (the groups' styles).
  - `env` (map): the drawing environment.

  Returns a vector of `{:text :color}`; a group line reads `<level>: value`; a group whose aesthetics fall outside their scales gets no line."
  [first-line groups text-of base-color scales env]
  (into [first-line]
        (keep (fn [{:keys [aesthetics value]}]
                (when-let [st (group/style {:color base-color} aesthetics scales (:theme env))]
                  {:text (str (or (some-> (first (vals aesthetics)) str) "NA") ": " (text-of value)) :color (:color st)})))
        groups))

(defn dropped
  "Counts the rows a cell chart drops for a non-finite `:x` or `:y`.

  Parameters:

  - `rows` (sequence): every panel's rows.
  - `mapping` (map): the merged mapping (`:x`, `:y`).

  Returns `{:x n :y n}`."
  [rows mapping]
  (let [x-of (data/row-accessor :x (:x mapping))
        y-of (data/row-accessor :y (:y mapping))]
    {:x (count (remove (comp schema/finite? x-of) rows))
     :y (count (remove (comp schema/finite? y-of) rows))}))

(defn legend
  "Draws the key glyph of a grouped cell chart: a filled square in the key's colour.

  Parameters:

  - `_chart`: the chart value (unused).
  - `attributes` (map): the legend entry's attributes (`:color`).
  - `canvas`: the legend's canvas.
  - `box` (map): the key box.
  - `_env`: unused.

  Returns `nil`."
  [_chart attributes canvas box _env]
  (draw/bar-key! canvas box {:color (:color attributes) :stroke-width 0} false))
