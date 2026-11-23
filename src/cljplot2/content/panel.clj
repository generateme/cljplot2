(ns cljplot2.content.panel
  "Panel content: the box charts are drawn in.

  Content `{:kind :panel}` has natural size 0x0 (it takes what its tracks give). It is drawn from the theme's
  `:panel` section (validated against [[cljplot2.theme/theme-schema]] by `build`): fill, then a border inside the
  box; `:none` colors draw nothing, `:border-width` is rounded to whole pixels. `{:kind :panel :alternate true}` also
  draws theme `[:panel :alternate-fill]` over the fill (facet shading).

  Finalized scales in `env` `:scales` are panel-local (range `[0 w]` horizontal / `[h 0]` vertical): grid lines (theme
  `[:panel :grid]`, `:bands` for bands scales) and charts (`env` `:charts`, `:stats`) are drawn translated to the
  panel's top-left corner, between the fill and the border; each chart with isolated graphics, it and the cartesian
  grid and band fills clipped to the panel box (whole pixels from the layout; a grid line at a tick on the panel's
  edge falls outside) - polar guides stay inside the circle unclipped. `{:kind :panel :panel k}` draws the charts'
  data of split group `k` (default `:panel`) and of charts drawn in all panels; several panel items with the same
  key draw the same data. `{:kind :panel :margin side}` - a plot margin's panel: its charts, stats and scales come
  from `env` `[:margins side]` (the shared axis scale and the margin's magnitude scale); only the shared axis draws
  grid lines.

  Functions: methods of [[cljplot2.graphics.measure/natural-size]] and [[cljplot2.graphics.draw/draw-content]] for
  `:panel`."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.content.label :as label]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.data.facet :as facet]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.graphics.space3d :as space3d]
            [cljplot2.scale.coord :as coord]
            [cljplot2.scale.position :as scale]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defmethod measure/natural-size :panel [_ _] (v/vec2 0.0 0.0))

(defn- call-clipped-to-panel
  "Calls `f` on an isolated copy of a canvas clipped to the panel box at the origin.

  Charts and the cartesian grid stay inside the panel (a grid line at a tick on the panel's edge would fall beside
  it); the box is used as given (whole pixels from the layout, no rounding should a size ever be fractional), within
  any clip the canvas already has.

  Parameters:

  - `c`: the canvas in graphics context, translated to the panel's corner.
  - `w`, `h`: the panel size in pixels.
  - `f` (fn of a canvas): draws on the clipped copy.

  Returns the result of `f`; every state change `f` makes is discarded."
  [c w h f]
  (c2d/with-isolated-canvas [clipped c]
    (c2d/add-clip clipped 0.0 0.0 (double w) (double h))
    (f clipped)))

(defn- draw-charts!
  "Draws every chart's data of one panel in panel-local coordinates.

  Each chart draws on its own copy of the graphics, clipped to the panel, with the default stroke (1 px, solid,
  square caps, miter joins) - no chart inherits the grid's or another chart's stroke (a dashed grid made stripplot
  ticks dashed). 3D charts (`:coords #{:three-d}`) of the panel share one scene: one box frame and one depth order of
  all their primitives (see [[cljplot2.graphics.space3d/draw-scene!]]).

  Parameters:

  - `c`: the canvas in graphics context, translated to the panel's corner.
  - `panel-key`: the split group drawn; data of charts drawn in all panels is drawn too.
  - `w`, `h`: the panel size in pixels.
  - `env` (map): `:charts`, `:stats`, `:scales` (panel-local) and the rest of the draw environment; the charts get it
    with `:panel-key`.

  Returns `nil`."
  [c panel-key w h env]
  (let [panel-env (assoc env :panel-key panel-key)
        layers (for [[chart stats] (map vector (:charts env) (:stats env))
                     computed [(get-in stats [:data panel-key]) (get-in stats [:data facet/all-panels])]
                     :when (some? computed)]
                 [chart computed])
        three-d? #(contains? (:coords (first %)) :three-d)
        clipped (fn [f] (call-clipped-to-panel c w h (fn [chart-canvas]
                                                        (c2d/set-stroke chart-canvas 1.0 :square :miter 10.0)
                                                        (f chart-canvas))))
        three-d (filter three-d? layers)]
    (doseq [[chart computed] (remove three-d? layers)]
      (clipped #((:draw chart) chart computed % {:x 0 :y 0 :w w :h h} (:scales panel-env) panel-env)))
    (when (seq three-d)
      (clipped #(space3d/draw-scene! % three-d [w h] (:scales panel-env) panel-env)))))

(defn- band-span
  "Gives the whole pixels a band's step covers along its scale.

  Parameters:

  - `centre` (double): the band's centre in pixels.
  - `half` (double): half the scale's step (band plus padding) in pixels.

  Returns `[lo len]` - the first pixel and the length, both rounded ends."
  [^double centre ^double half]
  (let [a (m/round (m/- centre half))
        b (m/round (m/+ centre half))]
    [(m/min a b) (m/abs (m/long-sub b a))]))

(defn- draw-band-fills!
  "Fills every second band (from the second level) of each bands scale across a cartesian panel.

  A band spans its step (band plus padding, so shaded bands touch their neighbours), whole pixels, along the scale's
  direction, filled with theme `[:panel :grid :bands-fill]`; `:none` draws nothing, a scale with `:axis false` has
  no fills (they belong to its grid).

  Parameters:

  - `c`: the canvas in graphics context, at the panel's corner.
  - `w`, `h` (longs): the panel size in pixels.
  - `env` (map): `:theme`, `:scales` (panel-local, finalized).

  Returns `nil`."
  [c ^long w ^long h env]
  (let [fill (get-in env [:theme :panel :grid :bands-fill])]
    (when-not (or (nil? fill) (theme/none? fill))
      (doseq [k [:x :y]
              :let [finalized (get-in env [:scales k])]
              :when (and (= :bands (:kind finalized)) (scale/grid-drawn? finalized))
              :let [forward (scale/forward-fn finalized)
                    half (m/* 0.5 (double (:step (scale/band-geometry finalized))))
                    horizontal? (scale/horizontal? k finalized)]
              [i level] (map-indexed vector (:domain finalized))
              :when (odd? (long i))
              :let [[lo len] (band-span (forward level) half)]]
        (c2d/set-color c fill)
        (if horizontal?
          (c2d/rect c lo 0 len h)
          (c2d/rect c 0 lo w len))))))

(defn- grid-line-style
  "Gives the grid line style of a finalized scale.

  Parameters:

  - `k` (keyword): the scale key, `:x` or `:y`.
  - `finalized` (map): the finalized scale, `nil` - none.
  - `env` (map): `:theme`.

  Returns theme `[:panel :grid k]` (a bands scale: `[:panel :grid :bands]`, unless `k`'s line is `:none`); `nil` when
  it draws nothing - no scale, theme `:none` line or colour, the scale's `:axis false`, zero width."
  [k finalized env]
  (let [axis-line (get-in env [:theme :panel :grid k])
        {:keys [color width] :as line} (if (and (= :bands (:kind finalized)) (not (theme/none? axis-line)))
                                         (get-in env [:theme :panel :grid :bands])
                                         axis-line)]
    (when (and finalized (scale/grid-drawn? finalized)
               (not (theme/none? line)) (not (theme/none? color)) (m/pos? (double width)))
      line)))

(defn- draw-tick-lines!
  "Strokes a line across a cartesian panel at each tick, in the canvas's current stroke and colour.

  Parameters:

  - `c`: the canvas in graphics context, at the panel's corner.
  - `finalized` (map): the finalized scale of `k`.
  - `ticks` (sequence): the tick values.
  - `[k w h]`: the scale key and the panel size in pixels - a horizontal scale (by its direction) draws vertical
    lines, a vertical one horizontal lines.

  Returns `nil`. Each line runs through the centre of the tick's pixel (see [[cljplot2.graphics.draw/pixel-center]])."
  [c finalized ticks [k ^long w ^long h]]
  (let [forward (scale/forward-fn finalized)
        horizontal? (scale/horizontal? k finalized)]
    (doseq [tick ticks
            :let [p (draw/pixel-center (forward tick))]]
      (if horizontal?
        (c2d/line c p 0 p h)
        (c2d/line c 0 p w p)))))

(defn- draw-grid-lines!
  "Draws the grid lines at the tick positions of a cartesian panel's scales.

  A scale with an `:axis-domain` draws at its display scale's ticks (see [[cljplot2.scale.position/display-scale]]);
  each scale's line is [[grid-line-style]].

  Parameters:

  - `c`: the canvas in graphics context, at the panel's corner.
  - `w`, `h` (longs): the panel size in pixels.
  - `env` (map): `:theme`, `:scales` (panel-local, finalized).

  Returns `nil`."
  [c ^long w ^long h env]
  (doseq [k [:x :y]
          :let [finalized (some-> (get-in env [:scales k]) scale/display-scale)
                line (grid-line-style k finalized env)]
          :when (and line (polyline/set-style! c line))]
    (draw-tick-lines! c finalized (:ticks finalized) [k w h])))

(defn- draw-minor-grid-lines!
  "Draws the grid lines at the minor ticks of a cartesian panel's scales.

  Scales with a `:minor-ticks` spec whose grid is drawn get lines at their `:minor-ticks`, styled by theme
  `[:panel :grid :minor]` (default `:none` - none).

  Parameters:

  - `c`: the canvas in graphics context, at the panel's corner.
  - `w`, `h` (longs): the panel size in pixels.
  - `env` (map): `:theme`, `:scales` (panel-local, finalized).

  Returns `nil`."
  [c ^long w ^long h env]
  (let [line (get-in env [:theme :panel :grid :minor])]
    (when-not (or (nil? line) (theme/none? line))
      (doseq [k [:x :y]
              :let [finalized (some-> (get-in env [:scales k]) scale/display-scale)]
              :when (and (seq (:minor-ticks finalized)) (scale/grid-drawn? finalized) (polyline/set-style! c line))]
        (draw-tick-lines! c finalized (:minor-ticks finalized) [k w h])))))

(defn- angular-key
  "Gives the key of the angular (horizontal) scale among finalized scales.

  Parameters:

  - `scales` (map): the finalized position scales.

  Returns `:y` for flipped scales, else `:x`."
  [scales]
  (if (scale/flipped? scales) :y :x))

(defn- draw-polar-band-fills!
  "Fills every second band (from the second level) of each bands scale of a polar panel.

  The polar counterpart of [[draw-band-fills!]]: a band is a sector (angular scale) or a ring (radial scale), filled
  with theme `[:panel :grid :bands-fill]`.

  Parameters:

  - `c`: the canvas in graphics context, at the panel's corner.
  - `geometry` (map): the polar geometry (see [[cljplot2.scale.coord/geometry]]).
  - `env` (map): `:theme`, `:scales` (panel-local, finalized).

  Returns `nil`."
  [c geometry env]
  (let [fill (get-in env [:theme :panel :grid :bands-fill])
        akey (angular-key (:scales env))
        [t0 t1] (:theta geometry)
        rin (:inner-radius geometry) radius (:radius geometry)]
    (when-not (or (nil? fill) (theme/none? fill))
      (doseq [k [:x :y]
              :let [finalized (get-in env [:scales k])]
              :when (and (= :bands (:kind finalized)) (scale/grid-drawn? finalized))
              :let [forward (scale/forward-fn finalized)
                    half (m/* 0.5 (double (:step (scale/band-geometry finalized))))
                    angular? (= k akey)]
              [i level] (map-indexed vector (:domain finalized))
              :when (odd? (long i))
              :let [centre (double (forward level))
                    polygon (if angular?
                              (coord/rect-path geometry (m/- centre half) rin (m/+ centre half) radius)
                              (coord/rect-path geometry t0 (m/- centre half) t1 (m/+ centre half)))]]
        (c2d/set-color c fill)
        (c2d/path c polygon true false)))))

(defn- draw-polar-grid-lines!
  "Draws the grid lines of a polar panel: spokes at the angular scale's ticks, circles at the radial scale's.

  The polar counterpart of [[draw-grid-lines!]]; each scale's line is [[grid-line-style]], ticks the scale can't
  place are skipped.

  Parameters:

  - `c`: the canvas in graphics context, at the panel's corner.
  - `geometry` (map): the polar geometry (see [[cljplot2.scale.coord/geometry]]).
  - `env` (map): `:theme`, `:scales` (panel-local, finalized).

  Returns `nil`."
  [c geometry env]
  (let [akey (angular-key (:scales env))]
    (doseq [k [:x :y]
            :let [finalized (get-in env [:scales k])
                  line (grid-line-style k finalized env)]
            :when (and line (polyline/set-style! c line))]
      (let [forward (scale/forward-fn finalized)]
        (doseq [tick (:ticks finalized)
                :let [p (double (forward tick))]
                :when (m/valid-double? p)]
          (c2d/path c (if (= k akey)
                        (coord/line-path geometry [[p (:inner-radius geometry)] [p (:radius geometry)]] false)
                        (coord/circle-path geometry p))
                    false true))))))

(defn- polar-label-placements
  "Gives the tick labels of a polar scale placed without collisions.

  All labels are kept for a scale keeping its labels (`:overlapping-labels :keep`), else each whose box overlaps no
  label kept before it ([[cljplot2.graphics.measure/kept-box-indices]]).

  Parameters:

  - `c`: the canvas in graphics context (measuring).
  - `finalized` (map): the finalized scale.
  - `place` (fn of a double): gives `[x y anchor]` of a tick at that pixel coordinate along the scale.
  - `env` (map): `:theme`.

  Returns a vector of `[content anchor x y]`; ticks the scale can't place are left out."
  [c finalized place env]
  (let [forward (scale/forward-fn finalized)
        ;; indexed by kept-box-indices
        placed (vec (for [[tick text] (map vector (:ticks finalized) (:tick-labels finalized))
                          :let [p (double (forward tick))]
                          :when (m/valid-double? p)
                          :let [[x y anchor] (place p)]]
                      [{:kind :label :role :tick-label :text text} anchor x y]))]
    (if (= :keep (:overlapping-labels finalized))
      placed
      (mapv placed (measure/kept-box-indices
                    (mapv (fn [[content anchor x y]] (label/box-at c content anchor x y env)) placed))))))

(defn- draw-polar-labels!
  "Draws the tick labels (role `:tick-label`) of a polar panel.

  The angular scale's labels go around the circle, just outside the outer radius (theme `[:axis k :tick-length]` +
  `:label-gap`), anchored away from the centre; the radial scale's along the start spoke, beside it. Scales with
  `:axis false` or `:axis :grid` have none; colliding labels are dropped (see [[polar-label-placements]]).

  Parameters:

  - `c`: the canvas in graphics context, at the panel's corner.
  - `geometry` (map): the polar geometry (see [[cljplot2.scale.coord/geometry]]).
  - `env` (map): `:theme`, `:scales` (panel-local, finalized).

  Returns `nil`."
  [c geometry env]
  (let [scales (:scales env)
        akey (angular-key scales)
        rkey (if (= akey :x) :y :x)
        t0 (double (first (:theta geometry)))
        radius (double (:radius geometry))
        drawn (fn [k] (let [s (get scales k)] (when (and s (scale/axis-drawn? s)) s)))
        agap (m/+ (double (get-in env [:theme :axis akey :tick-length])) (double (get-in env [:theme :axis akey :label-gap])))
        rgap (double (get-in env [:theme :axis rkey :label-gap]))
        ;; beside the start spoke: a small step perpendicular to it, toward decreasing angles
        side (m/- t0 m/HALF_PI)
        dx (m/* rgap (m/sin side))
        dy (m/- (m/* rgap (m/cos side)))]
    (when-let [angular (drawn akey)]
      (doseq [[content anchor x y] (polar-label-placements
                                    c angular
                                    (fn [^double theta] (let [^Vec2 p (coord/to-pixel geometry theta (m/+ radius agap))]
                                                          [(.x p) (.y p) (coord/outward-anchor theta)]))
                                    env)]
        (label/draw-at! c content anchor x y env)))
    (when-let [radial (drawn rkey)]
      (doseq [[content anchor x y] (polar-label-placements
                                    c radial
                                    (fn [^double r] (let [^Vec2 p (coord/to-pixel geometry t0 r)]
                                                      [(m/+ (.x p) dx) (m/+ (.y p) dy) (coord/outward-anchor side)]))
                                    env)]
        (label/draw-at! c content anchor x y env)))))

(defn- margin-env
  "Gives the draw environment of a plot margin's panel.

  Parameters:

  - `env` (map): the plot's draw environment, `[:margins side]` read.
  - `side` (keyword): the margin's side.

  Returns `env` with the margin's charts, stats and scales merged over it and no grid lines across the shared axis
  (theme `[:panel :grid :y]` `:none` - the magnitude scale)."
  [env side]
  (-> (merge env (get-in env [:margins side]))
      (assoc-in [:theme :panel :grid :y] :none)))

(defn- draw-panel!
  "Draws a panel: fill, alternate fill, then in panel-local coordinates the grid (or polar guides) and the charts,
  then the border.

  Parameters:

  - `content` (map): the panel content - `:alternate`, `:panel` (see the namespace doc).
  - `canvas`: the canvas in graphics context.
  - `box` (map): `{:x :y :w :h}` - the panel box in whole pixels (fractions are dropped).
  - `env` (map): the draw environment with panel-local `:scales`.

  Returns `nil`."
  [content canvas {:keys [^long x ^long y ^long w ^long h]} env]
  (let [{:keys [fill alternate-fill border-color border-width]} (:panel (:theme env))
        panel-key (get content :panel facet/single-panel)]
    (when-not (theme/none? fill)
      (c2d/set-color canvas fill)
      (c2d/rect canvas x y w h))
    (when (and (:alternate content) (not (theme/none? alternate-fill)))
      (c2d/set-color canvas alternate-fill)
      (c2d/rect canvas x y w h))
    ;; panel-local coordinates: scales map to [0 w] x [h 0]
    (c2d/with-isolated-canvas [local canvas]
      (c2d/translate local x y)
      (if-let [geometry (scale/coord-geometry (:scales env))]
        (do (draw-polar-band-fills! local geometry env)
            (draw-polar-grid-lines! local geometry env)
            (draw-charts! local panel-key w h env)
            (draw-polar-labels! local geometry env))
        (do (call-clipped-to-panel local w h (fn [clipped]
                                               (draw-band-fills! clipped w h env)
                                               (draw-minor-grid-lines! clipped w h env)
                                               (draw-grid-lines! clipped w h env)))
            (draw-charts! local panel-key w h env))))
    (draw/inner-border! canvas {:x x :y y :w w :h h} border-color border-width)))

(defmethod draw/draw-content :panel
  [content canvas box env]
  (draw-panel! content canvas box (if-let [side (:margin content)]
                                    (margin-env env side)
                                    (assoc env :scales (scale/panel-scales-of env content)))))
