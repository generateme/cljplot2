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
  Drawing clips the text to the item box (intersected with any outer clip). A style `:background` fills the text
  frame under the text: the item box (a label placed at a point by [[draw-at!]] gets its natural size - text plus
  padding; a layout item may be larger), or for an angle that isn't a multiple of 90 the rotated natural frame.
  Content is validated against [[label-options]] by [[cljplot2.graphics.measure/validate-content]] (called from
  `grid/prepare`).

  `:text` line breaks: `\\n` or `\\r\\n`."
  (:require [clojure.string :as str]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [malli.util :as mu]
            [cljplot2.graphics.canvas :as canvas]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.font :as font]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [java.awt Graphics2D Rectangle]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def label-options
  "Label options given by users (plot `:labels` maps, label content in custom grids): `:text`, `:role`, `:rotation`,
  `:align` and the style overrides of [[cljplot2.theme/style-keys]]."
  (-> [:map {:closed true :cljplot2/type-doc "a label options map (:text, :role, :rotation, text style keys)"}
       (schema/optional :kind {:cljplot2/section :label :description "content kind (labels only)"} [:= :label])
       (schema/optional :text {:cljplot2/section :label :description "the text (converted with str; \\n breaks lines)"} any?)
       (schema/optional :role {:cljplot2/section :label :description "theme text role styling the label (:title, :axis-title, ... or a role of your theme)"}
                        keyword?)
       (schema/optional :rotation {:cljplot2/section :label :description "clockwise degrees (270 reads bottom to top)" :default 0} [:enum 0 90 270])]
      (into (map (fn [[k props s]] [k (assoc props :cljplot2/section :text-style) s])) theme/text-style-overrides)))

(defn- lines
  "Lines of the label text; empty text has no lines, empty inner or trailing lines are kept."
  [text]
  (let [s (str text)]
    (if (= "" s) [] (str/split s #"\r?\n" -1))))

(def ^:private default-align [:center :middle])

(def ^:private content-schema
  "Label content (custom grid items, labels the layout builds): [[label-options]] with `:role` required - plot
  `:labels` get their role from the layout, content has no default."
  (mu/required-keys label-options [:role]))

(defmethod measure/validate-content :label
  [content]
  (schema/explain! [[content-schema content [:content]]]))

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
     :background (:background style)
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
        (when-let [background (when-not (= :none (:background spec)) (:background spec))]
          (c2d/set-color canvas background)
          (c2d/rect canvas 0.0 0.0 frame-w frame-h))
        (c2d/set-color canvas (:color spec))
        (dorun (map-indexed (fn [^long idx ^double line-width]
                              (c2d/text canvas (nth (:lines spec) idx)
                                        (+ block-x (align-offset (:h-align spec) block-width line-width))
                                        (+ block-y (* idx line-height) ascent)))
                            widths))))))

(defn value-text
  "Text of a data value in a label (tile values, contour levels): a number to at most 2 decimals (whole numbers
  without them), a keyword's name, other values as `str`; `nil` - none."
  [value]
  (cond
    (nil? value) nil
    (number? value) (let [r (m/approx (double value) 2)]
                      (if (== r (m/round r)) (str (m/round r)) (str r)))
    (keyword? value) (name value)
    :else (str value)))

(def ^:private anchor-fraction
  {:left 0.0 :center 0.5 :right 1.0 :top 0.0 :middle 0.5 :bottom 1.0})

(defn box-at
  "Pixel box `{:x :y :w :h}` [[draw-at!]] gives label `content` with its `anchor` at `x`, `y` (`env` - `:theme`)."
  [c content [h v] x y env]
  (let [^fastmath.vector.Vec2 size (measure/natural-size content {:canvas c :theme (:theme env)})]
    {:x (m/round (- (double x) (* (double (anchor-fraction h)) (.x size))))
     :y (m/round (- (double y) (* (double (anchor-fraction v)) (.y size))))
     :w (.x size) :h (.y size)}))

(defn draw-at-frame!
  "Draw label `content` on canvas `c` with the `anchor` point of its unrotated text frame (`[h v]` as [[draw-at!]] -
  e.g. `[:left :middle]` - the start of the text) at pixel `x`, `y`, the text rotated about that point by its
  `:rotation`: radial labels start at their anchor whatever the angle. (For a rotation of 0 the same as
  [[draw-at!]].) `env` - `:theme`. Isolates the canvas state."
  [c content [h v] x y env]
  (let [theme (:theme env)
        [fw fh] (text-frame content c theme)
        ^fastmath.vector.Vec2 size (measure/natural-size content {:canvas c :theme theme})
        a (m/radians (double (get content :rotation 0)))
        ;; from the anchor to the frame's centre, in frame units, then rotated clockwise (y down)
        dx (* (- 0.5 (double (anchor-fraction h))) (double fw))
        dy (* (- 0.5 (double (anchor-fraction v))) (double fh))
        cx (+ (double x) (- (* dx (m/cos a)) (* dy (m/sin a))))
        cy (+ (double y) (+ (* dx (m/sin a)) (* dy (m/cos a))))
        left (m/round (- cx (* 0.5 (.x size))))
        top (m/round (- cy (* 0.5 (.y size))))]
    (canvas/call-with-isolated-graphics
     c #(draw/draw-content content % {:x left :y top :w (.x size) :h (.y size)} env))))

(defn draw-at!
  "Draw label `content` on canvas `c` with the `anchor` point of its box (`[h v]`, `h` one of `:left :center :right`,
  `v` one of `:top :middle :bottom`; the box is the natural size - for rotated text the rotated frame's bounding box)
  at pixel `x`, `y`; the box starts at whole pixels. `env` - `:theme`. Isolates the canvas state."
  [c content [h v] x y env]
  (let [x (double x) y (double y)
        ^fastmath.vector.Vec2 size (measure/natural-size content {:canvas c :theme (:theme env)})
        left (m/round (- x (* (double (anchor-fraction h)) (.x size))))
        top (m/round (- y (* (double (anchor-fraction v)) (.y size))))]
    (canvas/call-with-isolated-graphics
     c #(draw/draw-content content % {:x left :y top :w (.x size) :h (.y size)} env))))
