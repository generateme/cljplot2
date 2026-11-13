(ns cljplot2.content.panel
  "Panel content `{:kind :panel}`: the box charts are drawn in. Natural size 0x0 (it takes what its tracks give).
  Drawn from the theme's `:panel` section (validated against [[cljplot2.theme/theme-schema]] by `build`): fill, then a border
  inside the box; `:none` colors draw nothing, `:border-width` is rounded to whole pixels.
  `{:kind :panel :alternate true}` also draws theme `[:panel :alternate-fill]` over the fill (facet shading).
  Finalized scales in `env` `:scales` are panel-local (range `[0 w]` horizontal / `[h 0]` vertical): grid lines (theme
  `[:panel :grid]`, `:bands` for bands scales) and charts (`env` `:charts`, `:stats`) are drawn translated to the panel's top-left corner,
  between the fill and the border; each chart with isolated graphics clipped to the panel. `{:kind :panel
  :panel k}` draws the charts' data of split group `k` (default `:panel`) and of charts drawn in all panels;
  several panel items with the same key draw the same data."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.graphics.canvas :as canvas]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.data.facet :as facet]
            [cljplot2.graphics.measure :as measure]
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
  neighbours), whole pixels, along the scale's direction. `:none` draws nothing."
  [c ^long w ^long h env]
  (let [fill (get-in env [:theme :panel :grid :bands-fill])]
    (when-not (or (nil? fill) (theme/none? fill))
      (doseq [k [:x :y]
              :let [finalized (get-in env [:scales k])]
              :when (= :bands (:kind finalized))
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
  `:none` turns the lines of scale `k` off; otherwise a bands scale draws the `[:panel :grid :bands]` line."
  [c ^long w ^long h env]
  (doseq [k [:x :y]
          :let [finalized (get-in env [:scales k])
                axis-line (get-in env [:theme :panel :grid k])
                {:keys [color width dash] :as line} (if (and (= :bands (:kind finalized)) (not (theme/none? axis-line)))
                                                      (get-in env [:theme :panel :grid :bands])
                                                      axis-line)]
          :when (and finalized (not (theme/none? line)) (not (theme/none? color)) (pos? (double width)))]
    (c2d/set-color c color)
    (c2d/set-stroke-custom c (cond-> {:size (double width) :cap :butt} (not (theme/none? dash)) (assoc :dash (vec dash))))
    (let [forward (scale/forward-fn finalized)
          horizontal? (scale/horizontal? k finalized)]
      (doseq [tick (:ticks finalized)
              :let [p (draw/pixel-center (forward tick))]]
        (if horizontal?
          (c2d/line c p 0 p h)
          (c2d/line c 0 p w p))))))

(defmethod draw/draw-content :panel
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
              (draw-band-fills! c' w h env)
              (draw-grid-lines! c' w h env)
              (draw-charts! c' (get content :panel facet/single-panel) w h env)))
    (draw/inner-border! canvas {:x x :y y :w w :h h} border-color border-width)))
