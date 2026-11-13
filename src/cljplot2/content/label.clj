(ns cljplot2.content.label
  "Label content: plain text, possibly multi-line, styled by a theme role.

  `{:kind :label :text x :role r}`, optional:

  * `:rotation` - clockwise degrees: `0` (default), `90` or `270` (reads bottom-to-top)
  * `:align` - `[h v]`, `h` one of `:left :center :right`, `v` one of `:top :middle :bottom`,
    relative to the unrotated text (default: the role's theme `:align`, else `[:center :middle]`); `h` also aligns lines
  * style keys overriding the role's theme style, see [[cljplot2.theme/style-keys]]

  `:text` is converted with `str` and split into lines; `nil` or `\"\"` is empty (zero size).
  Natural size is the text block rounded up plus padding on every side; rotation `90`/`270` swaps width and height.
  Internally (axis tick labels, theme `[:axis k :label-rotation]`) any angle is drawn: the natural size is then the
  rounded-up bounding box of the rotated text frame, the text rotated about the box centre.
  Drawing clips the text to the item box (intersected with any outer clip).
  Content is validated against [[Options]] by [[cljplot2.graphics.measure/validate-content]] (called from
  `grid/prepare`).

  `:text` line breaks: `\\n` or `\\r\\n`."
  (:require [clojure.string :as str]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.font :as font]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [java.awt Graphics2D Rectangle]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def Options
  "Label options given by users (plot `:labels` maps, label content in custom grids): `:text`, `:role`, `:rotation`,
  `:align` and the style overrides of [[cljplot2.theme/style-keys]]."
  (-> [:map {:closed true}
       (schema/optional :kind [:= :label])
       (schema/optional :text any?)
       (schema/optional :role keyword?)
       (schema/optional :rotation [:enum 0 90 270])]
      (into theme/text-style-overrides)))

(defn- lines
  "Lines of the label text; empty text has no lines, empty inner or trailing lines are kept."
  [text]
  (let [s (str text)]
    (if (= "" s) [] (str/split s #"\r?\n" -1))))

(def ^:private default-align [:center :middle])

(defmethod measure/validate-content :label
  [content]
  (schema/explain! [[Options content [:content]]]))

(defn- label-spec
  "Drawing parameters of validated label `content` in `theme`; validates the style with inline overrides."
  [theme content]
  (let [rotation (get content :rotation 0)
        style (theme/text-style theme content)
        align (or (:align style) default-align)
        padding (:padding style)]
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

(defn quarter-turn
  "`rotation` (degrees) normalized to `0`, `90`, `180` or `270` when it is a multiple of 90, else `nil`."
  [rotation]
  (let [r (double rotation)
        r (- r (* 360.0 (m/floor (/ r 360.0))))]
    (when (== r (* 90.0 (m/floor (/ r 90.0)))) (long r))))

(defn text-frame
  "Unrotated size `[w h]` of the text frame of label `content` (text block rounded up plus padding), measured on
  `canvas` with `theme` - the label before rotation."
  [content canvas theme]
  (let [spec (label-spec theme content)]
    (if (empty? (:lines spec))
      [0.0 0.0]
      (let [{:keys [^double block-width ^double block-height]} (text-metrics canvas spec)
            padding2 (* 2.0 (double (:padding spec)))]
        [(+ (m/ceil block-width) padding2) (+ (m/ceil block-height) padding2)]))))

(defmethod measure/natural-size :label
  [content {:keys [canvas theme]}]
  (when-not canvas
    (schema/fail! "Measuring a label needs a canvas." {:key :canvas}))
  (let [spec (label-spec theme content)]
    (if (empty? (:lines spec))
      (v/vec2 0.0 0.0)
      (let [{:keys [^double block-width ^double block-height]} (text-metrics canvas spec)
            padding2 (* 2.0 (double (:padding spec)))
            w (+ (m/ceil block-width) padding2)
            h (+ (m/ceil block-height) padding2)]
        (condp contains? (quarter-turn (:rotation spec))
          #{0 180} (v/vec2 w h)
          #{90 270} (v/vec2 h w)
          ;; any other angle: the rounded-up bounding box of the rotated text frame
          (let [a (m/radians (double (:rotation spec)))
                c (m/abs (m/cos a)) s (m/abs (m/sin a))]
            (v/vec2 (m/ceil (+ (* w c) (* h s))) (m/ceil (+ (* w s) (* h c))))))))))

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
            rotation (double (:rotation spec))
            padding (double (:padding spec))
            ;; text frame: the box as seen by unrotated text, origin at its top-left corner; for an angle that isn't a
            ;; multiple of 90 the box is the rotated frame's bounding box, the frame its unrotated size
            [frame-w frame-h] (condp contains? (quarter-turn rotation)
                                #{0 180} [(double w) (double h)]
                                #{90 270} [(double h) (double w)]
                                [(+ (m/ceil block-width) (* 2.0 padding)) (+ (m/ceil block-height) (* 2.0 padding))])
            block-x (+ padding (align-offset (:h-align spec) (- ^double frame-w (* 2.0 padding)) block-width))
            block-y (+ padding (align-offset (:v-align spec) (- ^double frame-h (* 2.0 padding)) block-height))]
        ;; graphics state is not restored here: callers isolate draw-content calls
        (.clip ^Graphics2D (:graphics canvas) (Rectangle. x y w h))
        (c2d/translate canvas (+ x (* 0.5 w)) (+ y (* 0.5 h)))
        (c2d/rotate canvas (m/radians rotation))
        (c2d/translate canvas (* -0.5 ^double frame-w) (* -0.5 ^double frame-h))
        (c2d/set-color canvas (:color spec))
        (dorun (map-indexed (fn [^long idx ^double line-width]
                              (c2d/text canvas (nth (:lines spec) idx)
                                        (+ block-x (align-offset (:h-align spec) block-width line-width))
                                        (+ block-y (* idx line-height) ascent)))
                            widths))))))
