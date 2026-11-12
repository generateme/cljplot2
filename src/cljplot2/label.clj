(ns cljplot2.label
  "Label content: plain text, possibly multi-line, styled by a theme role.

  `{:kind :label :text x :role r}`, optional:

  * `:rotation` - clockwise degrees: `0` (default), `90` or `270` (reads bottom-to-top)
  * `:align` - `[h v]`, `h` one of `:left :center :right`, `v` one of `:top :middle :bottom`,
    relative to the unrotated text (default `[:center :middle]`); `h` also aligns lines
  * style keys overriding the role's theme style, see [[cljplot2.theme/style-keys]]

  `:text` is converted with `str` and split into lines on `\\n`; `nil` or `\"\"` is empty (zero size).
  Natural size is the text block rounded up plus padding on every side; rotation `90`/`270` swaps width and height.
  Drawing clips the text to the item box."
  (:require [clojure.string :as str]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.draw :as draw]
            [cljplot2.font :as font]
            [cljplot2.measure :as measure]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private rotations #{0 90 270})
(def ^:private h-aligns #{:left :center :right})
(def ^:private v-aligns #{:top :middle :bottom})

(defn- lines
  "Lines of the label text; empty text has no lines, empty inner or trailing lines are kept."
  [text]
  (let [s (str text)]
    (if (= "" s) [] (str/split s #"\n" -1))))

(defn- label-spec
  "Validated drawing parameters of label `content` in `theme`."
  [theme content]
  (let [rotation (get content :rotation 0)
        align (get content :align [:center :middle])
        style (theme/text-style theme content)
        padding (:padding style)]
    (when-not (rotations rotation)
      (throw (ex-info "Label rotation must be 0, 90 or 270." {:key :rotation :value rotation})))
    (when-not (and (vector? align) (= 2 (count align)) (h-aligns (align 0)) (v-aligns (align 1)))
      (throw (ex-info "Label :align must be [h v], h in #{:left :center :right}, v in #{:top :middle :bottom}."
                      {:key :align :value align})))
    (when-not (and (number? padding) (>= (double padding) 0.0))
      (throw (ex-info "Text padding must be a non-negative number." {:key :padding :value padding})))
    {:lines (lines (:text content))
     :rotation rotation
     :h-align (align 0)
     :v-align (align 1)
     :padding (double padding)
     :color (:color style)
     :font (font/resolve-font (:font style) (:font-style style) (:size style))}))

(defn- text-metrics
  "Set the label font on `canvas` and measure its lines: line widths, line height,
  ascent (baseline offset from the line top) and the unrotated text block size."
  [canvas {:keys [lines font]}]
  (c2d/set-font canvas font)
  (let [boxes (mapv #(c2d/text-bounding-box canvas %) lines)
        [_ ^double y _ ^double line-height] (or (first boxes) [0.0 0.0 0.0 0.0])
        widths (mapv #(double (nth % 2)) boxes)]
    {:widths widths
     :line-height line-height
     :ascent (- y)
     :block-width (double (reduce max 0.0 widths))
     :block-height (* line-height (count lines))}))

(defmethod measure/natural-size :label
  [content {:keys [canvas theme]}]
  (when-not canvas
    (throw (ex-info "Measuring a label needs a canvas." {:key :canvas})))
  (let [spec (label-spec theme content)]
    (if (empty? (:lines spec))
      (v/vec2 0.0 0.0)
      (let [{:keys [^double block-width ^double block-height]} (text-metrics canvas spec)
            padding2 (* 2.0 (double (:padding spec)))
            w (+ (m/ceil block-width) padding2)
            h (+ (m/ceil block-height) padding2)]
        (if (= 0 (:rotation spec)) (v/vec2 w h) (v/vec2 h w))))))

(defn- align-offset
  "Offset of a `size` long block inside `available` space for alignment `align` (start/center/end)."
  ^double [align ^double available ^double size]
  (case align
    (:left :top) 0.0
    (:center :middle) (* 0.5 (- available size))
    (:right :bottom) (- available size)))

(defmethod draw/draw-content :label
  [content canvas {:keys [x y w h]} {:keys [theme]}]
  (let [spec (label-spec theme content)]
    (when (seq (:lines spec))
      (let [x (long x) y (long y) w (long w) h (long h)
            {:keys [widths ^double line-height ^double ascent ^double block-width ^double block-height]}
            (text-metrics canvas spec)
            rotation (long (:rotation spec))
            padding (double (:padding spec))
            ;; text frame: the box as seen by unrotated text, origin at its top-left corner
            [frame-w frame-h] (if (zero? rotation) [(double w) (double h)] [(double h) (double w)])
            block-x (+ padding (align-offset (:h-align spec) (- ^double frame-w (* 2.0 padding)) block-width))
            block-y (+ padding (align-offset (:v-align spec) (- ^double frame-h (* 2.0 padding)) block-height))]
        (c2d/clip canvas x y w h)
        (c2d/push-matrix canvas)
        (try
          (c2d/translate canvas (+ x (* 0.5 w)) (+ y (* 0.5 h)))
          (c2d/rotate canvas (m/radians rotation))
          (c2d/translate canvas (* -0.5 ^double frame-w) (* -0.5 ^double frame-h))
          (c2d/set-color canvas (:color spec))
          (dorun (map-indexed (fn [^long idx ^double line-width]
                                (c2d/text canvas (nth (:lines spec) idx)
                                          (+ block-x (align-offset (:h-align spec) block-width line-width))
                                          (+ block-y (* idx line-height) ascent)))
                              widths))
          (finally
            (c2d/pop-matrix canvas)
            (c2d/reset-clip canvas)))))))
