(ns cljplot2.content.panel
  "Panel content `{:kind :panel}`: the box charts are drawn in. Natural size 0x0 (it takes what its tracks give).
  Drawn from the theme's `:panel` section (validated against [[cljplot2.theme/theme-schema]] by `build`): fill, then a border
  inside the box; `:none` colors draw nothing, `:border-width` is rounded to whole pixels.
  `{:kind :panel :alternate true}` also draws theme `[:panel :alternate-fill]` over the fill (facet shading).
  Finalized scales in `env` `:scales` are panel-local (range `[0 w]` horizontal / `[h 0]` vertical): grid lines (theme
  `[:panel :grid]`, `:bands` for bands scales) and charts (`env` `:charts`, `:stats`) are drawn translated to the panel's top-left corner,
  between the fill and the border; each chart with isolated graphics clipped to the panel. `{:kind :panel
  :panel k}` draws the charts' data of split group `k` (default `:panel`) and of charts drawn in all panels;
  several panel items with the same key draw the same data.
  `{:kind :panel :margin side}` - a plot margin's panel: its charts, stats and scales come from `env`
  `[:margins side]` (the shared axis scale and the margin's magnitude scale); only the shared axis draws grid lines."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.content.label :as label]
            [cljplot2.graphics.canvas :as canvas]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.data.facet :as facet]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.scale.coord :as coord]
            [cljplot2.scale.position :as scale]
            [cljplot2.theme :as theme])
  (:import [java.awt Graphics2D Rectangle]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defmethod measure/natural-size :panel [_ _] (v/vec2 0.0 0.0))

(defn- draw-charts!
  "Draw every chart's data for `panel-key` (and data of charts drawn in all panels) in panel-local coordinates."
  [c panel-key w h env]
  (doseq [[chart stats] (map vector (:charts env) (:stats env))
          computed [(get-in stats [:data panel-key]) (get-in stats [:data facet/all-panels])]
          :when (some? computed)]
    (canvas/call-with-isolated-graphics
     c (fn [c']
         (.clip ^Graphics2D (:graphics c') (Rectangle. 0 0 (int w) (int h)))
         ((:draw chart) chart computed c' {:x 0 :y 0 :w w :h h} (:scales env) env)))))

(defn- draw-band-fills!
  "Every second band (from the second level) of each bands scale filled with theme `[:panel :grid :bands-fill]`
  across the `w` x `h` panel at the origin: a band spans its step (band plus padding, so shaded bands touch their
  neighbours), whole pixels, along the scale's direction. `:none` draws nothing; a scale with `:axis false` has no
  fills (they belong to its grid)."
  [c ^long w ^long h env]
  (let [fill (get-in env [:theme :panel :grid :bands-fill])]
    (when-not (or (nil? fill) (theme/none? fill))
      (doseq [k [:x :y]
              :let [finalized (get-in env [:scales k])]
              :when (and (= :bands (:kind finalized)) (not (false? (:axis finalized))))
              :let [forward (scale/forward-fn finalized)
                    half (* 0.5 (double (:step (scale/band-geometry finalized))))
                    horizontal? (scale/horizontal? k finalized)]
              [i level] (map-indexed vector (:domain finalized))
              :when (odd? (long i))
              :let [centre (double (forward level))
                    a (m/round (- centre half)) b (m/round (+ centre half))
                    lo (m/min a b) len (m/abs (- b a))]]
        (c2d/set-color c fill)
        (if horizontal?
          (c2d/rect c lo 0 len h)
          (c2d/rect c 0 lo w len))))))

(defn- draw-grid-lines!
  "Grid lines at the tick positions of the panel-local scales, `w` x `h` panel at the origin: a horizontal scale
  (by its direction) draws vertical lines, a vertical one horizontal lines. `[:panel :grid k]` (by scale key)
  `:none` turns the lines of scale `k` off, as does the scale's `:axis false`; otherwise a bands scale draws the
  `[:panel :grid :bands]` line."
  [c ^long w ^long h env]
  (doseq [k [:x :y]
          :let [finalized (get-in env [:scales k])
                axis-line (get-in env [:theme :panel :grid k])
                {:keys [color width dash] :as line} (if (and (= :bands (:kind finalized)) (not (theme/none? axis-line)))
                                                      (get-in env [:theme :panel :grid :bands])
                                                      axis-line)]
          :when (and finalized (not (false? (:axis finalized)))
                     (not (theme/none? line)) (not (theme/none? color)) (pos? (double width)))]
    (c2d/set-color c color)
    (c2d/set-stroke-custom c (cond-> {:size (double width) :cap :butt} (polyline/dash-pattern dash) (assoc :dash (polyline/dash-pattern dash))))
    (let [forward (scale/forward-fn finalized)
          horizontal? (scale/horizontal? k finalized)]
      (doseq [tick (:ticks finalized)
              :let [p (draw/pixel-center (forward tick))]]
        (if horizontal?
          (c2d/line c p 0 p h)
          (c2d/line c 0 p w p))))))

(defn- grid-line-style
  "Line style of the grid of finalized scale `finalized` (key `k`) in `env`, `nil` when it draws nothing (theme `:none`,
  the scale's `:axis false`, zero width); bands scales draw the `[:panel :grid :bands]` line."
  [k finalized env]
  (let [axis-line (get-in env [:theme :panel :grid k])
        {:keys [color width] :as line} (if (and (= :bands (:kind finalized)) (not (theme/none? axis-line)))
                                         (get-in env [:theme :panel :grid :bands])
                                         axis-line)]
    (when (and finalized (not (false? (:axis finalized)))
               (not (theme/none? line)) (not (theme/none? color)) (pos? (double width)))
      line)))

(defn- angular-key
  "Key of the angular (horizontal) scale among the finalized `scales`."
  [scales]
  (if (scale/flipped? scales) :y :x))

(defn- draw-polar-band-fills!
  "Polar counterpart of [[draw-band-fills!]]: every second band (from the second level) of each bands scale as a
  sector (angular scale) or a ring (radial scale), filled with theme `[:panel :grid :bands-fill]`."
  [c geometry env]
  (let [fill (get-in env [:theme :panel :grid :bands-fill])
        akey (angular-key (:scales env))
        [t0 t1] (:theta geometry)
        rin (:inner-radius geometry) radius (:radius geometry)]
    (when-not (or (nil? fill) (theme/none? fill))
      (doseq [k [:x :y]
              :let [finalized (get-in env [:scales k])]
              :when (and (= :bands (:kind finalized)) (not (false? (:axis finalized))))
              :let [forward (scale/forward-fn finalized)
                    half (* 0.5 (double (:step (scale/band-geometry finalized))))
                    angular? (= k akey)]
              [i level] (map-indexed vector (:domain finalized))
              :when (odd? (long i))
              :let [centre (double (forward level))
                    polygon (if angular?
                              (coord/rect-path geometry (- centre half) rin (+ centre half) radius)
                              (coord/rect-path geometry t0 (- centre half) t1 (+ centre half)))]]
        (c2d/set-color c fill)
        (c2d/path c polygon true false)))))

(defn- draw-polar-grid-lines!
  "Polar counterpart of [[draw-grid-lines!]]: spokes at the angular scale's ticks, circles at the radial scale's."
  [c geometry env]
  (let [akey (angular-key (:scales env))]
    (doseq [k [:x :y]
            :let [finalized (get-in env [:scales k])
                  {:keys [color width dash] :as line} (grid-line-style k finalized env)]
            :when line]
      (c2d/set-color c color)
      (c2d/set-stroke-custom c (cond-> {:size (double width) :cap :butt} (polyline/dash-pattern dash) (assoc :dash (polyline/dash-pattern dash))))
      (let [forward (scale/forward-fn finalized)]
        (doseq [tick (:ticks finalized)
                :let [p (double (forward tick))]
                :when (m/valid-double? p)]
          (c2d/path c (if (= k akey)
                        (coord/line-path geometry [[p (:inner-radius geometry)] [p (:radius geometry)]] false)
                        (coord/circle-path geometry p))
                    false true))))))

(defn- draw-polar-labels!
  "Tick labels of a polar panel (role `:tick-label`): the angular scale's around the circle, just outside the outer
  radius (theme `[:axis k :tick-length]` + `:label-gap`), anchored away from the centre; the radial scale's along the
  start spoke, beside it. Scales with `:axis false` have none."
  [c geometry env]
  (let [scales (:scales env)
        akey (angular-key scales)
        rkey (if (= akey :x) :y :x)
        t0 (double (first (:theta geometry)))
        radius (double (:radius geometry))
        labels (fn [k] (let [finalized (get scales k)]
                         (when (and finalized (not (false? (:axis finalized))))
                           (map vector (:ticks finalized) (:tick-labels finalized) (repeat (scale/forward-fn finalized))))))]
    (doseq [[tick text forward] (labels akey)
            :let [theta (double (forward tick))
                  gap (+ (double (get-in env [:theme :axis akey :tick-length])) (double (get-in env [:theme :axis akey :label-gap])))
                  ^fastmath.vector.Vec2 p (coord/to-pixel geometry theta (+ radius gap))]
            :when (m/valid-double? theta)]
      (label/draw-at! c {:kind :label :role :tick-label :text text} (coord/outward-anchor theta) (.x p) (.y p) env))
    ;; beside the start spoke: a small step perpendicular to it, toward decreasing angles
    (let [gap (double (get-in env [:theme :axis rkey :label-gap]))
          side (- t0 m/HALF_PI)]
      (doseq [[tick text forward] (labels rkey)
              :let [r (double (forward tick))
                    ^fastmath.vector.Vec2 p (coord/to-pixel geometry t0 r)
                    dx (* gap (m/sin side)) dy (- (* gap (m/cos side)))]
              :when (m/valid-double? r)]
        (label/draw-at! c {:kind :label :role :tick-label :text text} (coord/outward-anchor side)
                        (+ (.x p) dx) (+ (.y p) dy) env)))))

(defn- margin-env
  "`env` for the panel of plot margin `side`: the margin's charts, stats and scales, no grid lines across the shared
  axis (the magnitude scale `:y`)."
  [env side]
  (-> (merge env (get-in env [:margins side]))
      (assoc-in [:theme :panel :grid :y] :none)))

(defn- draw-panel!
  [content canvas {:keys [x y w h]} env]
  (let [{:keys [fill alternate-fill border-color border-width]} (:panel (:theme env))
        x (long x) y (long y) w (long w) h (long h)]
    (when-not (theme/none? fill)
      (c2d/set-color canvas fill)
      (c2d/rect canvas x y w h))
    (when (and (:alternate content) (not (theme/none? alternate-fill)))
      (c2d/set-color canvas alternate-fill)
      (c2d/rect canvas x y w h))
    ;; panel-local coordinates: scales map to [0 w] x [h 0]
    (canvas/call-with-isolated-graphics
     canvas (fn [c']
              (c2d/translate c' x y)
              (if-let [geometry (scale/coord-geometry (:scales env))]
                (do (draw-polar-band-fills! c' geometry env)
                    (draw-polar-grid-lines! c' geometry env)
                    (draw-charts! c' (get content :panel facet/single-panel) w h env)
                    (draw-polar-labels! c' geometry env))
                (do (draw-band-fills! c' w h env)
                    (draw-grid-lines! c' w h env)
                    (draw-charts! c' (get content :panel facet/single-panel) w h env)))))
    (draw/inner-border! canvas {:x x :y y :w w :h h} border-color border-width)))

(defmethod draw/draw-content :panel
  [content canvas box env]
  (let [env (if-let [side (:margin content)] (margin-env env side) env)]
    (draw-panel! content canvas box env)))
