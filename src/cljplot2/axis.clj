(ns cljplot2.axis
  "Axis content `{:kind :axis :scale :x|:y}`: line on the panel edge, outward ticks and tick labels
  (`:tick-label` text role) of the panel-local finalized scale in `env` `:scales` (the axis box spans its panel
  column for x, its panel row for y, so positions are offset by the box's x / y). Styled by theme `[:axis :x|:y]`.

  The x axis sits below the panel (line in its box's top pixel row), the y axis left of it (line in its box's
  rightmost pixel column). Natural size: x - height = tick length + label gap + tallest label; y - width = tick
  length + label gap + widest label. Without a finalized scale in `env` the axis is empty.

  Tick labels at the axis ends stick out past the axis box. `{:kind :axis-overhang :scale :x|:y :end :start|:end}`
  reserves that room: its natural size along the axis is how far the label of the tick nearest that end of the
  range sticks out beyond it (0 when it doesn't)."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
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

(defmethod measure/validate-content :axis-overhang
  [{k :scale end :end}]
  (when-not (axis-scales k)
    (throw (ex-info "Axis overhang :scale must be :x or :y." {:key :scale :value k})))
  (when-not (#{:start :end} end)
    (throw (ex-info "Axis overhang :end must be :start or :end." {:key :end :value end}))))

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

(defmethod measure/natural-size :axis-overhang
  [{k :scale end :end} env]
  (let [finalized (get-in env [:scales k])
        ticks (:ticks finalized)]
    (if (empty? ticks)
      (v/vec2 0.0 0.0)
      (let [forward (scale/forward-fn finalized)
            [a b] (:range finalized)
            edge (double (if (= end :start) a b))
            nearest (apply min-key #(m/abs (- edge (double (forward %)))) ticks)
            ^Vec2 size (measure/natural-size (tick-label ((:tick-labels finalized) (.indexOf ^java.util.List ticks nearest))) env)
            half (* 0.5 (if (= k :x) (.x size) (.y size)))
            ;; rounded up like label sizes, so integer track edges never clip the label
            overhang (m/ceil (m/max 0.0 (- half (m/abs (- edge (double (forward nearest)))))))]
        (if (= k :x) (v/vec2 overhang 0.0) (v/vec2 0.0 overhang))))))

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
          ;; panel-local positions; the axis box spans its panel column (x) / row (y)
          offset (if (= k :x) x y)
          forward (scale/forward-fn finalized)
          positions (mapv #(+ offset (double (forward %))) (:ticks finalized))
          line-x (draw/pixel-center (dec (+ x w))) ;; y axis: rightmost column of its box
          line-y (draw/pixel-center y)              ;; x axis: top row of its box
          sizes (label-sizes finalized (assoc env :canvas c)) ;; target canvas: same rendering hint as layout
          draw-label! (fn [text box] (canvas/call-with-isolated-graphics c #(draw/draw-content (tick-label text) % box env)))]
      (when (draws? line-color line-width)
        (set-line-style! c line-color line-width)
        (if (= k :x)
          (c2d/line c x line-y (+ x w) line-y)
          (c2d/line c line-x y line-x (+ y h))))
      (when (draws? tick-color tick-width)
        (set-line-style! c tick-color tick-width)
        (doseq [^double p positions
                :let [p (draw/pixel-center p)]]
          (if (= k :x)
            (c2d/line c p y p (+ y tick-length))
            (c2d/line c (+ x w) p (- (+ x w) tick-length) p))))
      (doseq [[^double p text ^Vec2 size] (map vector positions (:tick-labels finalized) sizes)
              :let [lw (.x size) lh (.y size)]]
        (if (= k :x)
          (draw-label! text {:x (- p (* 0.5 lw)) :y (+ y tick-length label-gap) :w lw :h lh})
          (draw-label! text {:x (- (+ x w) tick-length label-gap lw) :y (- p (* 0.5 lh)) :w lw :h lh}))))))
