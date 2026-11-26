(ns cljplot2.chart.rug
  "Rug: a short line per `:x` value across its strip - a strip chart (see [[cljplot2.data.strip]]); without a `:y`
  mapping placed `:edge` (along the panel's low y edge, `:thickness` pixels, default 8).

  Options (style defaults from theme `[:chart :rug]`): `:color`, `:line-width` (pixels; `:width` is the strip option); `:padding {:x [lo hi]}`
  (default `[0.05 0.05]`), plus the strip options and `:data` / `:mapping`.

  Functions: [[legend]] - its key glyph (the chart computes and draws through [[cljplot2.chart.strip/compute]] and [[cljplot2.chart.strip/draw]]); [[schemas]] - its style, options and mapping."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.data.group :as group]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.data.rows :as data]
            [cljplot2.chart.strip :as strip-chart]
            [cljplot2.data.strip :as strip]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries
  "Map entries of the rug's style (theme `[:chart :rug]`), also its options."
  [(schema/optional :color {:description "The colour of the rug lines; a mapped `:color` wins." :cljplot2/example :orange} schema/solid-color)
   (schema/optional :line-width {:description "The width of the rug lines in pixels; `0` draws nothing." :cljplot2/example 3} schema/non-neg)])

(def schemas
  "Schemas of the rug chart: `:style` (theme `[:chart :rug]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into theme/theme-style-entries)
                (into strip/options)
                (conj strip/padding-option))
   :mapping (strip/values-mapping
              [(schema/nilable :x {:description "The values of horizontal rugs, or the levels of vertical ones."} schema/accessor)
               (schema/nilable :y {:description "The levels of horizontal rugs, or the values of vertical ones; with no levels the chart draws one rug along the panel edge."}
                               schema/accessor)
               (schema/nilable :color {:description "Splits the rows into groups, each in its own line colour."} schema/accessor)
               (schema/nilable :group {:description "Splits the rows into groups that look the same, dodged as other groups."} schema/accessor)])})

(defn- compute-in-strip-frame
  "Computes the rug's stats in the strip frame (levels on `:y`).

  Parameters:

  - `chart` (map): the chart value - `:options` (the strip options, `:padding`, style options).
  - `chart-split` (map): the chart's rows split per panel (`:groups`, `:mapping`).
  - `env` (map): the compute environment - `:theme`, `:plot-scales` ...

  Returns `{:data :extents :padding :padding-explicit :padding-px :na :dropped}`: per panel `{:items [..] :style st :place p :options o}`; x extents - the items' values (each item holds at least one value).

  See also [[cljplot2.data.strip/split]]."
  [{:keys [options] :as chart} chart-split {:keys [theme] :as env}]
  (let [{:keys [place items panels dropped]} (strip/split chart-split options :edge env)
        rug-style (theme/resolved-style theme chart)]
    {:data (update-vals items (fn [its] {:items its :style rug-style :place place :options options}))
     :extents (merge-with merge (group/aesthetic-extents panels (:plot-scales env))
                          (strip/extents place items (fn [{:keys [values]}] [(v/mn values) (v/mx values)])))
     :padding (strip/padding place options)
     :padding-explicit (strip/explicit-padding place options)
     :padding-px (strip-chart/n-label-room options items env)
     :na (group/na-dims panels)
     :dropped dropped}))

(defn- draw-in-strip-frame
  "Draws the rug lines of one panel in the strip frame.

  Parameters:

  - `_chart` (map): the chart value (unused).
  - `computed` (map): the panel's stats - `:items`, `:style`, `:place`, `:options`.
  - `canvas`: the clojure2d canvas.
  - `box` (map): the panel box `{:w :h}`.
  - `scales` (map): finalized scales (in the strip frame).
  - `env` (map): the draw environment - `:theme` ...

  Draws a line across its slot at each value of every item whose group style has a positive `:line-width` (a group outside its scale's domain is skipped, as are values the scale can't place), then the n labels.

  See also [[cljplot2.chart.strip/placement]], [[cljplot2.chart.strip/draw-n-labels!]]."
  [_chart {:keys [items style place options]} canvas box scales env]
  (let [{:keys [point] :as pl} (strip-chart/placement box scales options place)]
    (doseq [{:keys [level slot values aesthetics]} items
            :let [st (group/style style aesthetics scales (:theme env))]
            :when (and st (m/pos? (double (:line-width st))))]
      (c2d/set-color canvas (:color st))
      (c2d/set-stroke-custom canvas {:size (:line-width st) :cap :butt})
      (doseq [v values
              :let [[^double lo ^double hi] slot
                    ^Vec2 a (point v level lo)
                    ^Vec2 b (point v level hi)]
              :when (and (data/finite-point? a) (data/finite-point? b))]
        (c2d/line canvas a b)))
    (strip-chart/draw-n-labels! canvas pl items (fn [{:keys [values]}] (v/mx values)) options scales env)))

(defmethod strip-chart/in-frame-compute :rug [chart chart-split env] (compute-in-strip-frame chart chart-split env))

(defmethod strip-chart/in-frame-draw :rug [chart computed canvas box scales env]
  (draw-in-strip-frame chart computed canvas box scales env))

(defn legend
  "Draws the rug's key glyph: a short vertical line in the middle of a legend key.

  Parameters:

  - `chart` (map): the chart value.
  - `attributes` (map): the key's mapped style attributes - `:color` is used.
  - `canvas`: the clojure2d canvas.
  - `box` (map): the key box `{:x :y :w :h}` in pixels.
  - `env` (map): the legend environment - `:theme`.

  The line spans the middle 60% of the box height, at least 1 pixel wide."
  [chart attributes canvas box env]
  (let [st (theme/key-style env chart attributes [:color])]
    (draw/spike-key! canvas box {:color (:color st) :line-width (m/max 1.0 (double (:line-width st))) :span [0.2 0.8]})))
