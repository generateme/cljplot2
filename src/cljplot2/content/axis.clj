(ns cljplot2.content.axis
  "Axis content `{:kind :axis :scale :x|:y}`: line on the panel edge, outward ticks and tick labels
  (`:tick-label` text role) of the panel-local finalized scale in `env` `:scales` (the axis box spans its panel
  column for a horizontal scale, its panel row for a vertical one, so positions are offset by the box's x / y).
  Styled by theme `[:axis :x|:y]` - by scale key, also when a flipped plot draws `:x` vertically.

  Geometry follows the scale's direction ([[cljplot2.scale.position/direction]]): a horizontal axis sits below the
  panel (line in its box's top pixel row), a vertical one left of it (line in its box's rightmost pixel column).
  `{:kind :axis ... :margin side}` - the magnitude axis of a plot margin: its scale from `env` `[:margins side :scales]`.
  Natural size: horizontal - height = tick length + label gap + tallest label; vertical - width = tick length +
  label gap + widest label. Without a finalized scale in `env` the axis is empty.

  Tick labels at the axis ends stick out past the axis box. `{:kind :axis-overhang :scale :x|:y :end :start|:end}`
  reserves that room: its natural size along the axis is how far the label of the tick nearest that end of the
  range sticks out beyond it (0 when it doesn't).

  Bands axes tick every band; when tick labels would overlap, only every `s`-th label is drawn (`s` the smallest step
  without overlap, starting at the first band) - see [[label-step]].

  Tick labels rotate by theme `[:axis k :label-rotation]` (clockwise degrees, -90..90; e.g. -90 or -45 for long
  bands labels): a horizontal axis puts the end of each rotated text nearest the axis at its tick (see
  [[label-offset-fn]]), a vertical axis keeps labels centred on their ticks; natural sizes, thinning and overhangs use
  the rotated label boxes. A horizontal axis followed by another panel's axis (`:next-panel-gap`, set by the facet
  grid for every column but the last) skips its labels that would overlap the next panel's labels."
  (:require [cljplot2.schema :as schema]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.content.label :as label]
            [cljplot2.graphics.canvas :as canvas]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.scale.position :as scale]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def axis-schema
  "`{:kind :axis}` content: `:scale` `:x` or `:y`; optional `:next-panel-gap` - pixels to the next panel's identical
  axis along a horizontal axis (facet columns): labels overlapping that axis's labels aren't drawn."
  [:map [:scale [:enum :x :y]] (schema/optional :next-panel-gap schema/non-neg)
   (schema/optional :margin [:enum :top :right])])

(defn- axis-scale
  "Finalized scale of axis `content`: `:scale` of `env` `:scales`, or of the plot margin's scales (`env`
  `[:margins side :scales]`) for an axis with `:margin side`."
  [{k :scale margin :margin} env]
  (if margin (get-in env [:margins margin :scales k]) (get-in env [:scales k])))

(def overhang-schema
  "`{:kind :axis-overhang}` content: `:scale` `:x` or `:y`, `:end` `:start` or `:end`."
  [:map [:scale [:enum :x :y]] [:end [:enum :start :end]]])

(defmethod measure/validate-content :axis [content] (schema/explain! [[axis-schema content [:content]]]))
(defmethod measure/validate-content :axis-overhang [content] (schema/explain! [[overhang-schema content [:content]]]))

(defn- tick-label
  ([text] (tick-label text 0))
  ([text rotation] {:kind :label :role :tick-label :text text :rotation rotation}))

(defn- label-rotation
  "Tick label rotation of the axis of scale `k` (theme `[:axis k :label-rotation]`, clockwise degrees, default 0)."
  [k env]
  (get-in env [:theme :axis k :label-rotation] 0))

(defn- label-sizes
  "Natural sizes of the (rotated) tick labels of `finalized`, the axis of scale `k`."
  [k finalized env]
  (let [rotation (label-rotation k env)]
    (mapv #(measure/natural-size (tick-label % rotation) env) (:tick-labels finalized))))

(defn- label-offset-fn
  "Function of a tick label text to the offset of its box centre from the tick along the axis of scale `k`. Labels
  of a horizontal axis rotated by `r` (not 0) put the end of the text nearest the axis at the tick: the centre
  moves by `sign(r) * w/2 * cos(r)` (`w` - the unrotated text frame width); vertical axes keep labels centred."
  [k finalized env]
  (let [rotation (double (label-rotation k env))]
    (if (or (zero? rotation) (not (scale/horizontal? k finalized)))
      (constantly 0.0)
      (let [factor (* 0.5 (m/signum rotation) (m/cos (m/radians rotation)))]
        (fn [text] (* factor (double (first (label/text-frame (tick-label text) (:canvas env) (:theme env))))))))))

(defn- label-offsets
  "Per tick label of the axis of scale `k`: offset of its box centre from the tick along the axis, see
  [[label-offset-fn]]."
  [k finalized env]
  (mapv (label-offset-fn k finalized env) (:tick-labels finalized)))

(defn label-spans
  "Per tick label of the axis of scale `k` with label `sizes`: the length along the axis a label needs so it doesn't
  overlap its neighbour, or `nil` when unrotated (the size along the axis). Rotated labels are parallel, so a label
  needs the smaller of its box length along the axis and its text frame height over the sine of the angle between
  the text and the axis."
  [k finalized env sizes]
  (let [rotation (double (label-rotation k env))]
    (when-not (zero? rotation)
      (let [horizontal? (scale/horizontal? k finalized)
            a (m/radians rotation)
            sine (m/abs (if horizontal? (m/sin a) (m/cos a)))]
        (mapv (fn [text ^Vec2 size]
                (let [along (if horizontal? (.x size) (.y size))
                      frame-h (double (second (label/text-frame (tick-label text) (:canvas env) (:theme env))))]
                  (if (pos? sine) (m/min along (/ frame-h sine)) along)))
              (:tick-labels finalized) sizes)))))

(defn- label-padding
  "Padding of tick labels in the `env` theme."
  [env]
  (:padding (theme/text-style (:theme env) (tick-label nil))))

(defn- label-intervals
  "`[centres extents]` of the tick labels of the axis of scale `k` along the axis: centre - the tick position moved
  by its offset (`offsets`, see [[label-offsets]]), extent - the label's text block (natural `sizes` (`Vec2`) along
  the axis, or `spans` (see [[label-spans]]) when given, without the label `padding` on both sides)."
  [k finalized sizes padding offsets spans]
  (let [forward (scale/forward-fn finalized)
        inset (* 2.0 (double padding))
        horizontal? (scale/horizontal? k finalized)]
    [(mapv (fn [tick off] (+ (double (forward tick)) (double off))) (:ticks finalized) (or offsets (repeat 0.0)))
     (if spans
       (mapv #(- (double %) inset) spans)
       (mapv (fn [^Vec2 size] (- (if horizontal? (.x size) (.y size)) inset)) sizes))]))

(defn- overlap?
  "Do labels with centres `ci`, `cj` and extents `ei`, `ej` overlap by more than 1 px (natural sizes are rounded up)?"
  [^double ci ^double ei ^double cj ^double ej]
  (< (m/abs (- cj ci)) (- (* 0.5 (+ ei ej)) 1.0)))

(defn label-step
  "Step `s` between drawn tick labels of the axis of scale `k`: 1 unless `finalized` is a bands scale whose labels would overlap;
  then the smallest `s` such that labels `0, s, 2s, ...` don't overlap along the axis (at most the label count).
  Labels overlap when their text blocks overlap by more than 1 px, see [[label-intervals]] (`sizes`, `padding`,
  `offsets`, `spans`)."
  ([k finalized sizes padding] (label-step k finalized sizes padding nil nil))
  ([k finalized sizes padding offsets spans]
   (let [n (count sizes)]
     (if (or (not= :bands (:kind finalized)) (< n 2))
       1
       (let [[centres extents] (label-intervals k finalized sizes padding offsets spans)
             fits? (fn [^long step]
                     (every? (fn [i] (let [i (long i) j (+ i step)]
                                       (not (overlap? (centres i) (extents i) (centres j) (extents j)))))
                             (range 0 (- n step) step)))]
         (or (first (filter fits? (range 1 n))) n))))))

(defn crossing-labels
  "Indices of the `drawn` labels (with `[centres extents]` along the axis) that overlap a drawn label of the next
  panel's identical axis, `shift` further along (panel length plus gap); these are skipped so labels never cross
  into a neighbouring facet panel's labels (the next panel keeps its own)."
  [[centres extents] drawn ^double shift]
  (set (filter (fn [i]
                 (some (fn [j] (overlap? (centres i) (extents i) (+ shift (double (centres j))) (extents j))) drawn))
               drawn)))

(defn- drawn?
  "Is label `i` drawn with label step `step`?"
  [^long step i]
  (zero? (rem (long i) step)))

(defmethod measure/natural-size :axis
  [{k :scale :as content} env]
  (let [finalized (axis-scale content env)
        {:keys [tick-length label-gap]} (get-in env [:theme :axis k])
        all-sizes (when finalized (label-sizes k finalized env))
        step (if finalized
               (label-step k finalized all-sizes (label-padding env) (label-offsets k finalized env)
                           (label-spans k finalized env all-sizes))
               1)
        sizes (keep-indexed (fn [i size] (when (drawn? step i) size)) all-sizes)
        widest (double (reduce max 0.0 (map #(.x ^Vec2 %) sizes)))
        tallest (double (reduce max 0.0 (map #(.y ^Vec2 %) sizes)))]
    (cond
      (nil? finalized) (v/vec2 0.0 0.0)
      (scale/horizontal? k finalized) (v/vec2 0.0 (+ (double tick-length) (double label-gap) tallest))
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
            i (.indexOf ^java.util.List ticks nearest)
            ^Vec2 size (measure/natural-size (tick-label ((:tick-labels finalized) i) (label-rotation k env)) env)
            horizontal? (scale/horizontal? k finalized)
            half (* 0.5 (if horizontal? (.x size) (.y size)))
            label-offset (double ((label-offset-fn k finalized env) ((:tick-labels finalized) i)))
            p (double (forward nearest))
            ;; rounded up like label sizes, so integer track edges never clip the label
            overhang (m/ceil (m/max 0.0 (if (zero? label-offset)
                                          (- half (m/abs (- edge p)))
                                          ;; a moved label box: how far it reaches past the edge on the edge's side
                                          (let [centre (+ p label-offset)
                                                other (double (if (= end :start) b a))]
                                            (if (>= edge other)
                                              (- (+ centre half) edge)
                                              (- edge (- centre half)))))))]
        (if horizontal? (v/vec2 overhang 0.0) (v/vec2 0.0 overhang))))))

(defn- set-line-style!
  [c color width]
  (c2d/set-color c color)
  (c2d/set-stroke-custom c {:size (double width) :cap :butt}))

(defn- draws? [color width] (and (not (theme/none? color)) (pos? (double width))))

(defmethod draw/draw-content :axis
  [{k :scale :as content} c {:keys [x y w h]} env]
  (when-let [finalized (axis-scale content env)]
    (let [{:keys [line-color line-width tick-color tick-length tick-width label-gap]} (get-in env [:theme :axis k])
          x (double x) y (double y) w (double w) h (double h)
          tick-length (double tick-length)
          label-gap (double label-gap)
          horizontal? (scale/horizontal? k finalized)
          ;; panel-local positions; the axis box spans its panel column (horizontal) / row (vertical)
          offset (if horizontal? x y)
          forward (scale/forward-fn finalized)
          positions (mapv #(+ offset (double (forward %))) (:ticks finalized))
          line-x (draw/pixel-center (dec (+ x w))) ;; vertical axis: rightmost column of its box
          line-y (draw/pixel-center y)              ;; horizontal axis: top row of its box
          env (assoc env :canvas c) ;; target canvas: same rendering hint as layout
          rotation (label-rotation k env)
          sizes (label-sizes k finalized env)
          offsets (label-offsets k finalized env)
          spans (label-spans k finalized env sizes)
          step (label-step k finalized sizes (label-padding env) offsets spans)
          drawn (filterv #(drawn? step %) (range (count sizes)))
          next-gap (:next-panel-gap content)
          skipped (if (and horizontal? next-gap)
                    (crossing-labels (label-intervals k finalized sizes (label-padding env) offsets spans) drawn
                                     (+ w (double next-gap)))
                    #{})
          draw-label! (fn [text box] (canvas/call-with-isolated-graphics c #(draw/draw-content (tick-label text rotation) % box env)))]
      (when (draws? line-color line-width)
        (set-line-style! c line-color line-width)
        (if horizontal?
          (c2d/line c x line-y (+ x w) line-y)
          (c2d/line c line-x y line-x (+ y h))))
      (when (draws? tick-color tick-width)
        (set-line-style! c tick-color tick-width)
        (doseq [^double p positions
                :let [p (draw/pixel-center p)]]
          (if horizontal?
            (c2d/line c p y p (+ y tick-length))
            (c2d/line c (+ x w) p (- (+ x w) tick-length) p))))
      (doseq [[i ^double p text ^Vec2 size ^double off] (map vector (range) positions (:tick-labels finalized) sizes offsets)
              :when (and (drawn? step i) (not (skipped i)))
              :let [lw (.x size) lh (.y size)]]
        (if horizontal?
          (draw-label! text {:x (- (+ p off) (* 0.5 lw)) :y (+ y tick-length label-gap) :w lw :h lh})
          (draw-label! text {:x (- (+ x w) tick-length label-gap lw) :y (- p (* 0.5 lh)) :w lw :h lh}))))))
