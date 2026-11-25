(ns cljplot2.content.label
  "Label content: plain text, possibly multi-line, styled by a theme role.

  `{:kind :label :text x :role r}`, optional:

  * `:rotation` - clockwise degrees: `0` (default), `90` or `270` (reads bottom-to-top)
  * `:align` - `[h v]`, `h` one of `:left :center :right`, `v` one of `:top :middle :bottom`,
    relative to the unrotated text (default: the role's theme `:align`, else `[:center :middle]`); `h` also aligns lines
  * style keys overriding the role's theme style, see [[cljplot2.theme/style-keys]]

  `:text` is converted with `str` and split into lines at line breaks (LF or CRLF); `nil` or the empty string is empty
  (zero size).
  Natural size is the text block rounded up plus the `:inset` on every side; rotation `90`/`270` swaps width and height.
  Internally (axis tick labels, theme `[:axis k :label-rotation]`) any angle is drawn: the natural size is then the
  rounded-up bounding box of the rotated text frame, the text rotated about the box centre.
  Drawing clips the text to the item box (intersected with any outer clip). A style `:background` fills the text
  frame under the text: the item box (a label placed at a point by [[draw-at!]] gets its natural size - text plus
  its inset; a layout item may be larger), or for an angle that isn't a multiple of 90 the rotated natural frame.
  Content is validated against [[label-options]] by [[cljplot2.graphics.measure/validate-content]] (called from
  `grid/prepare`).

  Functions: [[quarter-turn]], [[text-frame]], [[name-text]], [[value-text]], [[box-at]], [[draw-at!]], [[draw-at-frame!]]; schema
  [[label-options]]; methods of [[cljplot2.graphics.measure/validate-content]],
  [[cljplot2.graphics.measure/natural-size]] and [[cljplot2.graphics.draw/draw-content]] for `:label`.

  Fitted text: [[fitted-label]] (a label wrapped, shrunk, then cut to a length along its text, with [[along-text]]) is
  the one shrinking rule of texts too long for their room (axis titles, facet strips, text charts with `:x-span`):
  the least size is the text role's `:min-size` (a role without it doesn't shrink). Content with the internal key
  `:fit true` (axis titles) is fitted to its item box when measured (from the second layout round, once the box is
  known) and drawn: wrapped at word boundaries, then shrunk, then cut with an ellipsis; text that fits is unchanged.
  A label rotated across its own track (a y title with `:rotation 0`, an x title with `:rotation 90`) sizes that
  track in the first round, before any box is known, so it isn't fitted: a track too small for it is a layout error.
  [[draw-fitted-lines!]] (text lines fitted to a box)."
  (:require [clojure.string :as str]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [malli.util :as mu]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.font :as font]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def label-options
  "Schema of the label options given by users (plot `:labels` maps, label content in custom grids): `:text`, `:role`, `:rotation`,
  `:align` and the style overrides of [[cljplot2.theme/style-keys]]."
  (-> [:map {:closed true :cljplot2/type-doc "a label options map (:text, :role, :rotation, text style keys)"}
       (schema/optional :kind {:cljplot2/section :label :description "content kind (labels only)"} [:= :label])
       (schema/optional :text {:cljplot2/section :label :description "the text (converted with str; \\n breaks lines)"} any?)
       (schema/optional :role {:cljplot2/section :label :description "theme text role styling the label (:title, :axis-title, ... or a role of your theme)"}
                        keyword?)
       (schema/optional :rotation {:cljplot2/section :label :description "clockwise degrees (270 reads bottom to top)" :default 0} [:enum 0 90 270])]
      (into (map (fn [[k props s]] [k (assoc props :cljplot2/section :text-style) s])) theme/text-style-overrides)))

(defn- text-lines
  "Gives the lines of a label text.

  Parameters:

  - `text`: any value, converted with `str` (`nil` - empty).

  Returns a vector of strings split at LF or CRLF; empty text has no lines, empty inner or trailing lines are kept."
  [text]
  (let [s (str text)]
    (if (= "" s) [] (str/split s #"\r?\n" -1))))

(def ^:private default-align
  "Placement `[h v]` of a label's text in its box when neither the label nor its role gives `:align`."
  [:center :middle])

(def ^:private content-schema
  "Schema of label content (custom grid items, labels the layout builds): [[label-options]] with `:role` required - plot
  `:labels` get their role from the layout, content has no default."
  (-> (mu/required-keys label-options [:role])
      (mu/merge [:map (schema/optional :fit {:cljplot2/internal true
                                              :description "fit the text to its box: wrapped, shrunk down to its role's :min-size, then cut (axis titles)"}
                                       schema/bool)])))

(defmethod measure/validate-content :label
  [content]
  (schema/explain! [[content-schema content [:content]]]))

(defn- label-spec
  "Gives the drawing parameters of a validated label.

  Parameters:

  - `theme` (map): the merged theme.
  - `content` (map): the label content.

  Returns `{:lines :rotation :h-align :v-align :inset :color :background :font}` - the role's style with the
  label's inline overrides, the font resolved (see [[cljplot2.graphics.font/resolve-font]]), the colour resolved
  against the plot backdrop."
  [theme content]
  (let [rotation (get content :rotation 0)
        style (theme/text-style theme content)
        align (or (:align style) default-align)
        inset (:inset style)]
    {:lines (text-lines (:text content))
     :rotation rotation
     :h-align (align 0)
     :v-align (align 1)
     :inset (double inset)
     :color (draw/resolved-text-color (:color style) (draw/plot-backdrop theme))
     :background (:background style)
     :font (font/resolve-font (:font style) (:font-style style) (:size style))}))

(defn- text-metrics
  "Sets the label font on a canvas and measures the label's lines.

  Parameters:

  - `canvas`: the canvas in graphics context (its font changes).
  - `spec` (map): the label's drawing parameters (see [[label-spec]]) - `:lines`, `:font`.

  Returns `{:widths :line-height :ascent :block-width :block-height}`: line widths (a vector, indexed), the line
  height, the ascent (baseline offset from the line top) and the unrotated text block size; no lines - zeros."
  [canvas {text :lines font :font}]
  (c2d/set-font canvas font)
  (let [boxes (mapv #(c2d/text-bounding-box canvas %) text)
        [_ ^double top _ ^double line-height] (or (first boxes) [0.0 0.0 0.0 0.0])
        widths (mapv #(double (nth % 2)) boxes)]
    {:widths widths
     :line-height line-height
     :ascent (m/- top)
     :block-width (double (reduce m/max 0.0 widths))
     :block-height (m/* line-height (count text))}))

(defn quarter-turn
  "Gives a rotation normalized to a quarter turn.

  Parameters:

  - `rotation` (number): degrees, any sign or size.

  Returns `0`, `90`, `180` or `270` (a long) when `rotation` is a multiple of 90 (e.g. -90 gives 270, 720 gives 0),
  else `nil`."
  [rotation]
  (let [r (m/wrap 0.0 360.0 (double rotation))]
    (when (m/== r (m/* 90.0 (m/floor (m// r 90.0)))) (long r))))

(defn text-frame
  "Gives the unrotated size of a label's text frame: the text block rounded up plus the `:inset` on every side.

  Parameters:

  - `content` (map): the label content.
  - `canvas`: a canvas in graphics context, measuring (its font changes).
  - `theme` (map): the merged theme.

  Returns `[w h]` (doubles) - the label before rotation; `[0.0 0.0]` for empty text.

  See also [[cljplot2.graphics.measure/natural-size]] (the rotated box)."
  [content canvas theme]
  (let [spec (label-spec theme content)]
    (if (empty? (:lines spec))
      [0.0 0.0]
      (let [{:keys [^double block-width ^double block-height]} (text-metrics canvas spec)
            inset2 (m/* 2.0 (double (:inset spec)))]
        [(m/+ (m/ceil block-width) inset2) (m/+ (m/ceil block-height) inset2)]))))

;; fitted text

(def ^:private ellipsis
  "The mark ending a cut text line."
  "…")

(def ^{:private true :const true :tag 'long} max-wrapped-lines
  "Most lines one text line wraps into (see [[fitted-label]]); the last takes the rest of the words (text geometry, not a style)."
  3)

(defn along-text
  "Gives the length of a label along its text direction.

  Parameters:

  - `content` (map): a label content, optional `:rotation`.
  - `env` (map): `:canvas` (a canvas in graphics context; its font changes), `:theme`.

  Returns the natural width of the unrotated label (its height for a rotation of 90 or 270), a double."
  ^double [content env]
  (let [^Vec2 size (measure/natural-size content env)]
    (if (#{90 270} (quarter-turn (get content :rotation 0))) (.y size) (.x size))))

(defn- cut-line
  "Gives the longest start of a text line that, ended with an ellipsis, fits a length.

  Parameters:

  - `fits?` (function): tests whether a text fits.
  - `line` (string): the line.

  Returns `line` when it fits, else its longest start (whitespace trimmed) with the ellipsis that fits; the ellipsis
  alone when no start fits."
  [fits? line]
  (if (fits? line)
    line
    (loop [lo 0 hi (count line)]
      ;; invariant: a start of lo characters fits (0 - the ellipsis alone, taken as the fallback), hi doesn't
      (if (m/<= (m/long-sub hi lo) 1)
        (str (str/trimr (subs line 0 lo)) ellipsis)
        (let [mid (m/long-quot (m/long-add lo hi) 2)]
          (if (fits? (str (str/trimr (subs line 0 mid)) ellipsis)) (recur mid hi) (recur lo mid)))))))

(defn- wrapped-line
  "Gives a text line wrapped at word boundaries into lines that fit a length.

  Parameters:

  - `fits?` (function): tests whether a text fits.
  - `line` (string): the line.

  Returns a vector of at most [[max-wrapped-lines]] lines: words joined by single spaces, each line as long as fits,
  the last one holding the words left (it may not fit); a word longer than the length takes a line of its own; a
  blank line stays one empty line."
  [fits? line]
  (let [words (remove str/blank? (str/split line #"\s+"))]
    (if (empty? words)
      [""]
      (let [lines (reduce (fn [acc word]
                            (let [current (peek acc)
                                  joined (str current " " word)]
                              (if (and current (fits? joined)) (conj (pop acc) joined) (conj acc word))))
                          [] words)]
        (if (m/> (count lines) max-wrapped-lines)
          (conj (subvec lines 0 (m/long-dec max-wrapped-lines))
                (str/join " " (subvec lines (m/long-dec max-wrapped-lines))))
          lines)))))

(defn fitted-label
  "Gives a label fitted to a length along its text: wrapped, then shrunk, then cut.

  Text longer than the length is first wrapped at word boundaries (when `:wrap` is true; each line into at most 3
  lines, given line breaks kept), then drawn smaller - its size scaled down to fit, rounded down, down to its text
  role's `:min-size` (a role without one, or a size below it, isn't shrunk) - and when that is still too long each
  line too long is cut, ending with an ellipsis (unless `:cut` is false: the text kept whole, longer than the length
  at the least size). Used by facet strips (no wrapping), axis titles (see [[cljplot2.layout.plot-grid]]) and text
  charts with `:x-span` (no cutting, see [[cljplot2.chart.text]]).

  Parameters:

  - `content` (map): the label content (its `:size` over its role's).
  - `fit` (map): `:length` - pixels along the text; `:wrap` - true wraps first (default false); `:cut` - false keeps
    the text whole (default true).
  - `env` (map): `:canvas` (a canvas in graphics context; its font changes), `:theme`.

  Returns `content` when it fits, else with a wrapped or cut `:text` (a string) and possibly a `:size`; a length too
  short for any text leaves the ellipsis."
  [content {:keys [^double length wrap cut] :or {cut true}} env]
  (let [natural (along-text content env)]
    (if (m/<= natural length)
      content
      (let [fits-for (fn [c] (fn [text] (m/<= (along-text (assoc c :text text) env) length)))
            wrapped (when wrap
                      (assoc content :text (str/join "\n" (mapcat (partial wrapped-line (fits-for content))
                                                                  (text-lines (:text content))))))]
        (if (and wrapped ((fits-for wrapped) (:text wrapped)))
          wrapped
          (let [base (or wrapped content)
                base-natural (if wrapped (along-text wrapped env) natural)
                {:keys [^double size min-size]} (theme/text-style (:theme env) base)
                least (m/min size (double (or min-size size)))
                sized (assoc base :size (m/max least (m/floor (m// (m/* size length) base-natural))))
                fits? (fits-for sized)]
            (if (or (not cut) (fits? (:text sized)))
              sized
              (assoc sized :text (str/join "\n" (map #(cut-line fits? %) (str/split-lines (str (:text sized)))))))))))))

(defn- box-fitted
  "Gives a label with `:fit` fitted to a box (see [[fitted-label]]).

  Parameters:

  - `content` (map): the label content; `:fit` - true fits it, `nil` or false - not fitted.
  - `box` (map or `nil`): `{:w :h}` in pixels, `nil` - not known yet (the first layout round).
  - `env` (map): `:canvas` (a canvas in graphics context; its font changes), `:theme`.

  Returns `content` without `:fit`, wrapped, shrunk or cut to the box's length along its text (the height for a
  rotation of 90 or 270); unchanged (without `:fit`) without a box or `:fit`."
  [content box env]
  (let [plain (dissoc content :fit)]
    (if (and box (:fit content))
      (fitted-label plain {:length (double (if (#{90 270} (quarter-turn (get content :rotation 0))) (:h box) (:w box)))
                           :wrap true}
                    env)
      plain)))

(defmethod measure/natural-size :label
  [content {:keys [canvas theme box] :as env}]
  (when-not canvas
    (schema/fail! "Measuring a label needs a canvas." {:key :canvas}))
  (let [spec (label-spec theme (box-fitted content box env))]
    (if (empty? (:lines spec))
      (v/vec2 0.0 0.0)
      (let [{:keys [^double block-width ^double block-height]} (text-metrics canvas spec)
            inset2 (m/* 2.0 (double (:inset spec)))
            w (m/+ (m/ceil block-width) inset2)
            h (m/+ (m/ceil block-height) inset2)]
        (condp contains? (quarter-turn (:rotation spec))
          #{0 180} (v/vec2 w h)
          #{90 270} (v/vec2 h w)
          ;; any other angle: the rounded-up bounding box of the rotated text frame
          (let [a (m/radians (double (:rotation spec)))
                cosine (m/abs (m/cos a)) sine (m/abs (m/sin a))]
            (v/vec2 (m/ceil (m/+ (m/* w cosine) (m/* h sine))) (m/ceil (m/+ (m/* w sine) (m/* h cosine))))))))))

(defn- align-offset
  "Gives the offset of a block inside the space available for it.

  Parameters:

  - `align` (keyword): `:left` / `:top` (start), `:center` / `:middle`, `:right` / `:bottom` (end).
  - `available` (double): the space's length.
  - `size` (double): the block's length.

  Returns the offset of the block's start (negative when the block is longer than the space)."
  ^double [align ^double available ^double size]
  (case align
    (:left :top) 0.0
    (:center :middle) (m/* 0.5 (m/- available size))
    (:right :bottom) (m/- available size)))

(defmethod draw/draw-content :label
  [content canvas {:keys [^long x ^long y ^long w ^long h] :as box} {:keys [theme] :as env}]
  (let [spec (label-spec theme (box-fitted content box (assoc env :canvas canvas)))]
    (when (seq (:lines spec))
      (let [{:keys [widths ^double line-height ^double ascent ^double block-width ^double block-height]}
            (text-metrics canvas spec)
            rotation (double (:rotation spec))
            inset (double (:inset spec))
            inset2 (m/* 2.0 inset)
            ;; text frame: the box as seen by unrotated text, origin at its top-left corner; for an angle that isn't a
            ;; multiple of 90 the box is the rotated frame's bounding box, the frame its unrotated size
            [^double frame-w ^double frame-h] (condp contains? (quarter-turn rotation)
                                                #{0 180} [(double w) (double h)]
                                                #{90 270} [(double h) (double w)]
                                                [(m/+ (m/ceil block-width) inset2) (m/+ (m/ceil block-height) inset2)])
            block-x (m/+ inset (align-offset (:h-align spec) (m/- frame-w inset2) block-width))
            block-y (m/+ inset (align-offset (:v-align spec) (m/- frame-h inset2) block-height))]
        ;; graphics state is not restored here: callers isolate draw-content calls
        (c2d/add-clip canvas x y w h)
        (c2d/translate canvas (m/+ x (m/* 0.5 w)) (m/+ y (m/* 0.5 h)))
        (c2d/rotate canvas (m/radians rotation))
        (c2d/translate canvas (m/* -0.5 frame-w) (m/* -0.5 frame-h))
        (when-let [background (when-not (theme/none? (:background spec)) (:background spec))]
          (c2d/set-color canvas background)
          (c2d/rect canvas 0.0 0.0 frame-w frame-h))
        (c2d/set-color canvas (:color spec))
        (doseq [[^long idx text ^double line-width] (map vector (range) (:lines spec) widths)]
          (c2d/text canvas text
                    (m/+ block-x (align-offset (:h-align spec) block-width line-width))
                    (m/+ block-y (m/* idx line-height) ascent)))))))

(defn name-text
  "Gives the text of a name: a level, an accessor, a set name.

  Parameters:

  - `v`: any value.

  Returns a keyword's name, else `(str v)` (`nil` gives an empty string).

  See also [[value-text]] (numbers rounded)."
  [v]
  (if (keyword? v) (name v) (str v)))

(defn value-text
  "Gives the text of a data value in a label (tile values, contour levels).

  Parameters:

  - `value`: any value.

  Returns a number to at most 2 decimals (whole numbers without them), a keyword's name, other values as `str`;
  `nil` for `nil`."
  [value]
  (cond
    (nil? value) nil
    (number? value) (let [r (m/approx (double value) 2)]
                      (if (m/== r (m/round r)) (str (m/round r)) (str r)))
    :else (name-text value)))

(def ^:private anchor-fraction
  "Fraction of a box's width (`:left :center :right`) or height (`:top :middle :bottom`) an anchor sits at."
  {:left 0.0 :center 0.5 :right 1.0 :top 0.0 :middle 0.5 :bottom 1.0})

(defn box-at
  "Gives the pixel box [[draw-at!]] gives a label with its anchor at a point.

  Parameters:

  - `c`: a canvas in graphics context, measuring.
  - `content` (map): the label content.
  - `[h v]`: the anchor - `h` one of `:left :center :right`, `v` one of `:top :middle :bottom`.
  - `x`, `y` (numbers): the anchor's pixel.
  - `env` (map): `:theme`.

  Returns `{:x :y :w :h}` - the natural size, the corner at whole pixels."
  [c content [h v] x y env]
  (let [^Vec2 size (measure/natural-size content {:canvas c :theme (:theme env)})]
    {:x (m/round (m/- (double x) (m/* (double (anchor-fraction h)) (.x size))))
     :y (m/round (m/- (double y) (m/* (double (anchor-fraction v)) (.y size))))
     :w (.x size) :h (.y size)}))

(defn draw-at-frame!
  "Draws a label with an anchor of its unrotated text frame at a point, the text rotated about that point.

  Radial labels start at their anchor whatever the angle; for a rotation of 0 the same as [[draw-at!]].

  Parameters:

  - `c`: the canvas in graphics context.
  - `content` (map): the label content, its `:rotation` (clockwise degrees, default 0) turns the text.
  - `[h v]`: the anchor of the text frame as [[draw-at!]] - e.g. `[:left :middle]`, the start of the text.
  - `x`, `y` (numbers): the anchor's pixel.
  - `env` (map): `:theme`.

  Returns `nil`; the canvas state is isolated (nothing changes, measuring included)."
  [c content [h v] x y env]
  (c2d/with-isolated-canvas [isolated c]
    (let [theme (:theme env)
          [^double fw ^double fh] (text-frame content isolated theme)
          ^Vec2 size (measure/natural-size content {:canvas isolated :theme theme})
          a (m/radians (double (get content :rotation 0)))
          ;; from the anchor to the frame's centre, in frame units, then rotated clockwise (y down)
          dx (m/* (m/- 0.5 (double (anchor-fraction h))) fw)
          dy (m/* (m/- 0.5 (double (anchor-fraction v))) fh)
          cx (m/+ (double x) (m/- (m/* dx (m/cos a)) (m/* dy (m/sin a))))
          cy (m/+ (double y) (m/+ (m/* dx (m/sin a)) (m/* dy (m/cos a))))
          left (m/round (m/- cx (m/* 0.5 (.x size))))
          top (m/round (m/- cy (m/* 0.5 (.y size))))]
      (draw/draw-content content isolated {:x left :y top :w (.x size) :h (.y size)} env)))
  nil)

(defn draw-at!
  "Draws a label with an anchor of its box at a point.

  The box is the natural size - for rotated text the rotated frame's bounding box - and starts at whole pixels.

  Parameters:

  - `c`: the canvas in graphics context.
  - `content` (map): the label content.
  - `[h v]`: the anchor - `h` one of `:left :center :right`, `v` one of `:top :middle :bottom`.
  - `x`, `y` (numbers): the anchor's pixel.
  - `env` (map): `:theme`.

  Returns `nil`; the canvas state is isolated (nothing changes, measuring included).

  See also [[box-at]], [[draw-at-frame!]]."
  [c content anchor x y env]
  (c2d/with-isolated-canvas [isolated c]
    (draw/draw-content content isolated (box-at isolated content anchor x y env) env))
  nil)

(def ^{:private true :const true :tag 'double} min-fitted-size
  "Smallest font size fitted text lines shrink to in a narrow box."
  6.0)

(def ^{:private true :const true :tag 'double} fitted-share
  "Share of the box's width the widest fitted line may take before the font shrinks (text geometry, not a style)."
  0.92)

(def ^{:private true :const true :tag 'double} fitted-line-spacing
  "Line height of fitted text as a multiple of the font size (text geometry, not a style)."
  1.3)

(defn draw-fitted-lines!
  "Draws text lines centred in a box, one under another, the font shrunk to fit the box's width.

  The font shrinks (to 6 points at least) so the widest line - and the `:template` text, the widest a line can be, so equal boxes get one size - fits 92 % of the box's width; lines are 1.3 font sizes apart. Used by the cells of correlograms and pairwise statistics.

  Parameters:

  - `canvas`: the canvas in graphics context, the box at its origin.
  - `text` (map):
    - `:lines` (vector) - `[{:text :color} ..]`, `:color` optional (the role's colour);
    - `:template` (string or `nil`) - the widest text a line can hold (`nil` - the lines alone);
    - `:role` (keyword) - the text role.
  - `box` (map): `:w` and `:h` in pixels.
  - `env` (map): `:theme`.

  Returns `nil`.

  See also [[draw-at!]]."
  [canvas {:keys [lines template role]} {:keys [^double w ^double h]} env]
  (let [role-size (double (get-in env [:theme :text role :size]))
        content (fn [{:keys [text color]} size] (cond-> {:kind :label :role role :text text :size size} color (assoc :color color)))
        widest (m/max 0.0 (v/mx (map #(.x ^Vec2 (measure/natural-size (content % role-size) {:canvas canvas :theme (:theme env)})) (cond-> lines template (conj {:text template})))))
        size (if (m/> widest (m/* fitted-share w)) (m/max min-fitted-size (m/floor (m// (m/* role-size fitted-share w) widest))) role-size)
        line-h (m/* fitted-line-spacing size)
        top (m/- (m/* 0.5 h) (m/* 0.5 line-h (m/long-dec (count lines))))]
    (doseq [[^long i line] (map-indexed vector lines)]
      (draw-at! canvas (content line size) [:center :middle] (m/* 0.5 w) (m/+ top (m/* line-h i)) env))
    nil))
