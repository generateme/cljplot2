(ns cljplot2.content.axis
  "Axis content `{:kind :axis :scale :x|:y}`: line on the panel edge, outward ticks and tick labels
  (`:tick-label` text role) of the panel-local finalized scale in `env` `:scales` (the axis box spans its panel
  column for x, its panel row for y, so positions are offset by the box's x / y). Styled by theme `[:axis :x|:y]`.

  The x axis sits below the panel (line in its box's top pixel row), the y axis left of it (line in its box's
  rightmost pixel column). Natural size: x - height = tick length + label gap + tallest label; y - width = tick
  length + label gap + widest label. Without a finalized scale in `env` the axis is empty.

  Tick labels at the axis ends stick out past the axis box. `{:kind :axis-overhang :scale :x|:y :end :start|:end}`
  reserves that room: its natural size along the axis is how far the label of the tick nearest that end of the
  range sticks out beyond it (0 when it doesn't).

  Bands axes tick every band; when tick labels would overlap, only every `s`-th label is drawn (`s` the smallest step
  without overlap, starting at the first band) - see [[label-step]]."
  (:require [cljplot2.schema :as schema]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.graphics.canvas :as canvas]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.scale.position :as scale]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def Axis
  "`{:kind :axis}` content: `:scale` `:x` or `:y`."
  [:map [:scale [:enum :x :y]]])

(def Overhang
  "`{:kind :axis-overhang}` content: `:scale` `:x` or `:y`, `:end` `:start` or `:end`."
  [:map [:scale [:enum :x :y]] [:end [:enum :start :end]]])

(defmethod measure/validate-content :axis [content] (schema/explain! [[Axis content [:content]]]))
(defmethod measure/validate-content :axis-overhang [content] (schema/explain! [[Overhang content [:content]]]))

(defn- tick-label [text] {:kind :label :role :tick-label :text text})

(defn- label-sizes
  "Natural sizes of the tick labels of `finalized`."
  [finalized env]
  (mapv #(measure/natural-size (tick-label %) env) (:tick-labels finalized)))

(defn- label-padding
  "Padding of tick labels in the `env` theme."
  [env]
  (:padding (theme/text-style (:theme env) (tick-label nil))))

(defn label-step
  "Step `s` between drawn tick labels of axis `k`: 1 unless `finalized` is a bands scale whose labels would overlap;
  then the smallest `s` such that labels `0, s, 2s, ...` don't overlap along the axis (at most the label count).
  Labels overlap when their text blocks - natural `sizes` (`Vec2`) without the label `padding` on both sides - at
  their tick positions overlap by more than 1 px (natural sizes are rounded up)."
  ^long [k finalized sizes padding]
  (let [n (count sizes)]
    (if (or (not= :bands (:kind finalized)) (< n 2))
      1
      (let [forward (scale/forward-fn finalized)
            positions (mapv #(double (forward %)) (:ticks finalized))
            inset (* 2.0 (double padding))
            extents (mapv (fn [^Vec2 size] (- (if (= k :x) (.x size) (.y size)) inset)) sizes)
            fits? (fn [^long step]
                    (every? (fn [i] (let [i (long i) j (+ i step)]
                                      (>= (m/abs (- (double (positions j)) (double (positions i))))
                                          (- (* 0.5 (+ (double (extents i)) (double (extents j)))) 1.0))))
                            (range 0 (- n step) step)))]
        (or (first (filter fits? (range 1 n))) n)))))

(defn- drawn?
  "Is label `i` drawn with label step `step`?"
  [^long step i]
  (zero? (rem (long i) step)))

(defmethod measure/natural-size :axis
  [{k :scale} env]
  (let [finalized (get-in env [:scales k])
        {:keys [tick-length label-gap]} (get-in env [:theme :axis k])
        all-sizes (when finalized (label-sizes finalized env))
        step (if finalized (label-step k finalized all-sizes (label-padding env)) 1)
        sizes (keep-indexed (fn [i size] (when (drawn? step i) size)) all-sizes)
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
          step (label-step k finalized sizes (label-padding env))
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
      (doseq [[i ^double p text ^Vec2 size] (map vector (range) positions (:tick-labels finalized) sizes)
              :when (drawn? step i)
              :let [lw (.x size) lh (.y size)]]
        (if (= k :x)
          (draw-label! text {:x (- p (* 0.5 lw)) :y (+ y tick-length label-gap) :w lw :h lh})
          (draw-label! text {:x (- (+ x w) tick-length label-gap lw) :y (- p (* 0.5 lh)) :w lw :h lh}))))))
