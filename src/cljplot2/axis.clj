(ns cljplot2.axis
  "Axis content `{:kind :axis :scale :x|:y}`: line on the panel edge, outward ticks and tick labels
  (`:tick-label` text role) of the finalized scale in `env` `:scales`. Styled by theme `[:axis :x|:y]`.

  The x axis sits below the panel (line on its box's top edge), the y axis left of it (line on its box's right
  edge). Natural size: x - height = tick length + label gap + tallest label; y - width = tick length + label gap
  + widest label. Without a finalized scale in `env` the axis is empty."
  (:require [clojure2d.core :as c2d]
            [fastmath.vector :as v]
            [cljplot2.canvas :as canvas]
            [cljplot2.draw :as draw]
            [cljplot2.measure :as measure]
            [cljplot2.scale :as scale]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private axis-scales #{:x :y})

(defmethod measure/validate-content :axis
  [{k :scale}]
  (when-not (axis-scales k)
    (throw (ex-info "Axis :scale must be :x or :y." {:key :scale :value k}))))

(defn- tick-label [text] {:kind :label :role :tick-label :text text})

(defn- label-sizes
  "Natural sizes of the tick labels of `finalized`."
  [finalized env]
  (mapv #(measure/natural-size (tick-label %) env) (:tick-labels finalized)))

(defmethod measure/natural-size :axis
  [{k :scale} env]
  (let [finalized (get-in env [:scales k])
        {:keys [tick-length label-gap]} (get-in env [:theme :axis k])
        sizes (when finalized (label-sizes finalized env))
        widest (double (reduce max 0.0 (map #(.x ^Vec2 %) sizes)))
        tallest (double (reduce max 0.0 (map #(.y ^Vec2 %) sizes)))]
    (cond
      (nil? finalized) (v/vec2 0.0 0.0)
      (= k :x) (v/vec2 0.0 (+ (double tick-length) (double label-gap) tallest))
      :else (v/vec2 (+ (double tick-length) (double label-gap) widest) 0.0))))

(defn- set-line-style!
  [c color width]
  (c2d/set-color c color)
  (c2d/set-stroke-custom c {:size (double width) :cap :butt}))

(defn- draws? [color width] (and (not (theme/none? color)) (pos? (double width))))

(defmethod draw/draw-content :axis
  [{k :scale} c {:keys [x y w h]} env]
  (when-let [finalized (get-in env [:scales k])]
    (let [{:keys [line-color line-width tick-color tick-length tick-width label-gap]} (get-in env [:theme :axis k])
          x (double x) y (double y) w (double w) h (double h)
          tick-length (double tick-length)
          label-gap (double label-gap)
          positions (mapv #(scale/forward finalized %) (:ticks finalized))
          sizes (label-sizes finalized (assoc env :canvas c)) ;; target canvas: same rendering hint as layout
          draw-label! (fn [text box] (canvas/call-with-isolated-graphics c #(draw/draw-content (tick-label text) % box env)))]
      (when (draws? line-color line-width)
        (set-line-style! c line-color line-width)
        (if (= k :x)
          (c2d/line c x y (+ x w) y)
          (c2d/line c (+ x w) y (+ x w) (+ y h))))
      (when (draws? tick-color tick-width)
        (set-line-style! c tick-color tick-width)
        (doseq [^double p positions]
          (if (= k :x)
            (c2d/line c p y p (+ y tick-length))
            (c2d/line c (+ x w) p (- (+ x w) tick-length) p))))
      (doseq [[^double p text ^Vec2 size] (map vector positions (:tick-labels finalized) sizes)
              :let [lw (.x size) lh (.y size)]]
        (if (= k :x)
          (draw-label! text {:x (- p (* 0.5 lw)) :y (+ y tick-length label-gap) :w lw :h lh})
          (draw-label! text {:x (- (+ x w) tick-length label-gap lw) :y (- p (* 0.5 lh)) :w lw :h lh}))))))
