(ns cljplot2.content.invalid-band
  "Invalid-value band content: a band beside a position axis's start drawing what charts count as invalid along it
  (rows whose value there is missing, NaN, non-numeric or not placeable), on the panel's other scale.

  A chart with `:invalid :show` (histogram) returns stats `:invalid-bands {dim {:label text :data {panel computed}}}`:
  per panel, the data its own `:draw` draws in the band of position dimension `dim` (`:x` or `:y`), in band
  coordinates - `[0 1]` across the band (see [[band-scale]]) - and the panel's other dimension as usual. The layout
  (see [[cljplot2.layout.plot-grid]]) adds the band's tracks beside every panel, outside the axis line: a horizontal
  dimension's band is a column left of the panel (right of the vertical axis), a vertical dimension's a row under it
  (above the horizontal axis) - the low end of an axis that isn't reversed. With a plot margin on that side the band
  lies outside the margin: columns vertical axis, band, gap, inner track, left margin, panel (rows likewise: panel,
  bottom margin, inner track, gap, band, horizontal axis); the inner track then holds nothing.

  Contract of the band data:

  - Bars and marks take the chart's own style (its colour, or the mapped one) - the band is drawn by the chart's
    `:draw`.
  - One label per dimension: of several charts with a band of one dimension the first chart's `:label` is drawn.
  - A row invalid in two band dimensions (a later two-dimensional chart) has no cell: it is dropped, counted in
    `:dropped` under the first dimension that drops it (each row counted once); there is no corner band.
  - Only the main charts of generated grids draw bands: `:invalid :show` in a margin chart or a `:custom-grid` plot is
    a rule error of `build` (see [[cljplot2.pipeline]]).

  Why separate content kinds rather than a `:panel` / `:axis` modifier (as plot margins, `{:kind :panel :margin
  side}`): a band's scale across it is fixed (`[0 1]`, see [[band-scale]]) and built here at draw time from the
  band's pixel size, so no pipeline stage makes or finalizes it; margins share a pipeline-made scale (the margin env).
  Both kinds wrap the `:panel` and `:axis` contents, so no panel or axis drawing is duplicated.

  Content kinds:

  - `{:kind :invalid-band :dim k :direction d :label text :panel p :alternate b}` - the band's panel: drawn as a
    `:panel` (fill, `:alternate` shading, grid lines of the other scale, border; see [[cljplot2.content.panel]]) of
    the charts' band data of panel `p` (default the single panel), with the panel's scales and `k` replaced by the band scale. Its natural size across
    the band is theme `[:panel :invalid-band-size]`, at least the label plus theme `[:axis k :tick-label-gap]`.
  - `{:kind :invalid-band-axis :dim k :direction d :label text}` - the band's axis: an axis of the band scale (theme
    `[:axis k]`), one tick at the band's centre labelled `text`, in the main axis's row or column.

  `d` is `:horizontal` or `:vertical` - the direction dimension `k` is drawn in.

  Functions: [[band-scale]]; methods of [[cljplot2.graphics.measure/natural-size]],
  [[cljplot2.graphics.measure/validate-content]] and [[cljplot2.graphics.draw/draw-content]] for both kinds."
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.content.axis :as axis]
            [cljplot2.content.panel]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private band-padding
  "Fractions of the band coordinates `[0 1]` left free at each end of the band."
  [0.1 0.1])

(def ^:private content-schema
  "Schema of both band content kinds: `:dim`, `:direction`, `:label`; optional `:panel`, `:alternate`."
  [:map [:dim [:enum :x :y]] [:direction [:enum :horizontal :vertical]] [:label :string] (schema/optional :panel any?)
   (schema/optional :alternate :boolean)])

(defmethod measure/validate-content :invalid-band [content] (schema/explain! [[content-schema content [:content]]]))
(defmethod measure/validate-content :invalid-band-axis [content] (schema/explain! [[content-schema content [:content]]]))

(defn band-scale
  "Gives the finalized scale across an invalid-value band.

  Parameters:

  - `content` (map): band content - `:dim`, `:direction`, `:label`.
  - `length` (number): the band's thickness in pixels.

  Returns a linear scale of domain `[0 1]` padded by a tenth at each end over `[0 length]` (horizontal) or
  `[length 0]` (vertical) with `:direction`, one tick at 0.5 labelled `:label` and no grid lines (`:axis :no-grid`)."
  [{:keys [dim direction label]} length]
  (let [l (double length)]
    (assoc (scale/finalize (scale/scale-spec dim {:kind :linear :domain [0.0 1.0] :nice false :padding band-padding
                                                  :ticks [0.5] :formatter (constantly label) :axis :no-grid})
                           (if (= :horizontal direction) [0.0 l] [l 0.0])
                           1)
           :direction direction)))

(defn- thickness
  "Gives the natural thickness of a band.

  Parameters:

  - `content` (map): band content - `:dim`, `:direction`, `:label`.
  - `env` (map): the measuring environment - `:theme`, `:canvas`.

  Returns theme `[:panel :invalid-band-size]`, at least the label's extent across the band plus theme
  `[:axis dim :tick-label-gap]`, whole pixels (a double)."
  ^double [{:keys [dim direction label]} {:keys [theme] :as env}]
  (let [^Vec2 size (measure/natural-size (axis/tick-label theme dim label) env)
        extent (if (= :horizontal direction) (.x size) (.y size))]
    (m/ceil (m/max (double (get-in theme [:panel :invalid-band-size]))
                   (m/+ extent (double (get-in theme [:axis dim :tick-label-gap] 0.0)))))))

(defmethod measure/natural-size :invalid-band
  [{:keys [direction] :as content} env]
  (let [t (thickness content env)]
    (if (= :horizontal direction) (v/vec2 t 0.0) (v/vec2 0.0 t))))

(defn- axis-env
  "Gives the environment the band's axis is measured and drawn in.

  Parameters:

  - `content` (map): band axis content - `:dim`.
  - `env` (map): the content environment.
  - `length` (number): the band's thickness in pixels.

  Returns `env` whose only scale is the band scale (see [[band-scale]]), no panel-local scales."
  [{:keys [dim] :as content} env length]
  (assoc env :scales {dim (band-scale content length)} :panel-scales nil))

(defmethod measure/natural-size :invalid-band-axis
  [{:keys [dim] :as content} env]
  (measure/natural-size {:kind :axis :scale dim} (axis-env content env (thickness content env))))

(defmethod draw/draw-content :invalid-band-axis
  [{:keys [dim direction] :as content} canvas {:keys [w h] :as box} env]
  (draw/draw-content {:kind :axis :scale dim} canvas box (axis-env content env (if (= :horizontal direction) w h))))

(defmethod draw/draw-content :invalid-band
  [{:keys [dim direction] :as content} canvas {:keys [w h] :as box} env]
  (let [scales (assoc (scale/panel-scales-of env content) dim (band-scale content (if (= :horizontal direction) w h)))
        ;; each chart's band data takes the place of its panel data
        band-stats (mapv (fn [stats] {:data (get-in stats [:invalid-bands dim :data])}) (:stats env))]
    (draw/draw-content (cond-> {:kind :panel}
                         (contains? content :panel) (assoc :panel (:panel content))
                         (:alternate content) (assoc :alternate true))
                       canvas box (assoc env :scales scales :panel-scales nil :stats band-stats))))
