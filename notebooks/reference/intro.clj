;; # Reference

;; The reference lists every option of cljplot2: the plot map, render options, scales, facets, labels, the theme,
;; every chart, builder and user-facing data function. The tables are generated from the library's validating
;; schemas, so they name exactly what `plot/build` accepts. Use cases and galleries show the options at work; this
;; part says what exists.

(ns reference.intro
  (:require [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [cljplot2.pipeline :as pipeline]
            [notebook-utils :as nu]))

;; ## How the part is organised

;; * **Concepts** (this chapter and the next eleven) - rendering, data and mapping, aesthetics and legends, scales
;;   and coordinates, layering, labels and text, facets and multiplots, the theme, the shared option groups, errors
;;   and what the library doesn't do.
;; * **Charts**, **builders** and **data functions** - one table per chart kind, builder and user-facing function,
;;   with a few images each.

;; ## How to read a table

;; * **Option** - the key; a nested key is listed after its map, with its full path (`:scales :x`). `(required)`
;;   marks a key that must be given.
;; * **Type** - the values the schema accepts.
;; * **Default** - the value used when the key is absent (style defaults come from the theme), or how a computed
;;   default is found; empty when absent means "off" or "not drawn".
;; * **Description** - what the key does.
;; * **Group** - shown when some keys belong to a shared group (data and mapping, curves, strips ...); a group is
;;   described once, in the Shared options chapter.

;; ## A plot is a map

;; `plot/render-image` takes a plot map and render options and returns a `java.awt.image.BufferedImage`. The plot
;; keys (nested maps have their own chapters):

(nu/options-table (filter #(= 1 (count (:path %))) (describe/options pipeline/plot-schema)))

;; The smallest plot: data, a mapping and one chart.

(plot/render-image {:data [{:x 1 :y 2} {:x 2 :y 3} {:x 3 :y 1} {:x 4 :y 4}]
                    :mapping {:x :x :y :y}
                    :charts [(chart/line) (chart/scatter)]}
                   {:width 500 :height 350})

;; ## The pipeline

;; `plot/build` validates the whole plot first (render options, plot keys, the merged theme, every chart's options
;; and mapping) and throws an error listing the problems (Errors chapter). Then the plot passes through
;; pure stages, each adding one key: `:split` (rows split per facet panel and group) → `:stats` (each chart's
;; computed data) → `:extents` → `:scale-specs` → `:grid` → `:layout` → `:finalized-scales` → `:subplots`.
;; `plot/render` draws the result.

;; ## The tables as data

;; The tables come from `cljplot2.describe`, usable at the REPL too: `describe/options` takes a chart value, a
;; builder or data function var or a schema; `describe/mapping` the mapping keys a chart reads; `describe/theme-rows`
;; the theme; `describe/fragment-rows` a shared group; `describe/api-vars` lists the user-facing data functions. A
;; row is a plain map:

(first (describe/options (chart/scatter)))
