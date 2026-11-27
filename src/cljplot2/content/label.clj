(ns cljplot2.content.label
  "Label content: plain text, possibly multi-line, styled by a theme role.

  `{:kind :label :text x :role r}`, optional:

  * `:rotation` - clockwise degrees: `0` (default), `90` or `270` (reads bottom-to-top)
  * `:align` - `[h v]`, `h` one of `:left :center :right`, `v` one of `:top :middle :bottom`,
    relative to the unrotated text (default: the role's theme `:align`, else `[:center :middle]`); `h` also aligns lines
  * style keys overriding the role's theme style, see [[cljplot2.theme/style-keys]]

  `:text` is any value: a formula value ([[cljplot2.data.text/math]]) or a runs value ([[cljplot2.data.text/runs]])
  is kept, any other value is converted with `str`; it is split into lines at line breaks (LF or CRLF); `nil` or the
  empty string is empty (zero size). Formulas - formula values and, with the style's `:dollar-math` on (the
  default), dollar pairs in the text (see [[cljplot2.data.text]]) - are typeset at the style's size in its colour,
  bold for a bold font style (see [[cljplot2.graphics.latex]]); a line with formulas has its baseline at the largest
  ascent of its runs and its height from the largest ascent plus the largest descent (the font's line height at
  least). A formula that doesn't parse is an error keyed `:text`.
  Natural size is the text block rounded up plus the `:inset` on every side; rotation `90`/`270` swaps width and height.
  Internally (axis tick labels, theme `[:axis k :label-rotation]`) any angle is drawn: the natural size is then the
  rounded-up bounding box of the rotated text frame, the text rotated about the box centre.
  Drawing clips the text to the item box (intersected with any outer clip). A style `:background` fills the text
  frame under the text: the item box (a label placed at a point by [[draw-at!]] gets its natural size - text plus
  its inset; a layout item may be larger), or for an angle that isn't a multiple of 90 the rotated natural frame.
  Content is validated against [[label-options]] by [[cljplot2.graphics.measure/validate-content]] (called from
  `grid/prepare`).

  Functions: [[quarter-turn]], [[text-frame]], [[name-text]], [[value-text]], [[box-at]], [[frame-corners]], [[draw-at!]],
  [[draw-at-frame!]], [[label-options?]], [[formulas?]]; style [[drawn-style]]; leader lines [[draw-leader!]]; text along a path [[draw-along-path!]]; schema
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
            [cljplot2.data.text :as text]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.font :as font]
            [cljplot2.graphics.latex :as latex]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.graphics.text-path :as text-path]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]
           [java.awt Font]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def label-options
  "Schema of the label options given by users (plot `:labels` maps, label content in custom grids): `:text`, `:role`, `:rotation`,
  `:align` and the style overrides of [[cljplot2.theme/style-keys]]."
  (-> [:map {:closed true :cljplot2/type-doc "a label options map (:text, :role, :rotation, text style keys)" :cljplot2/type-name "label map"}
       (schema/optional :kind {:cljplot2/section :label :description "The content kind of a custom grid's label item; a plot label needs none."
                               :cljplot2/internal true}
                        [:= :label])
       (schema/optional :text {:cljplot2/section :label :description "The text: any value as `str`, or a `plot/math` or `plot/runs` value; a newline breaks lines."
                               :cljplot2/default-doc "None: an empty label." :cljplot2/example "Walks"}
                        any?)
       (schema/optional :role {:cljplot2/section :label :description "The theme text role whose style the label takes: `:title`, `:axis-title` ... or a role of your theme."
                               :cljplot2/default-doc "`:title` for the plot title, `:axis-title` for axis titles, `:legend-title` for legend titles; a custom grid's label has none and must give it." :cljplot2/example :tick-label
                               :cljplot2/example-context {:text "Walks"}}
                        keyword?)
       (schema/optional :rotation {:cljplot2/section :label :description "The label's clockwise turn in degrees." :cljplot2/default-doc "`0`; `270` for the y axis title; along its edge for a 3D frame's axis title." :cljplot2/example 90
                                   :cljplot2/example-context {:text "Walks"}
                                   :cljplot2/value-meanings {0 "Horizontal." 90 "Turned clockwise, reading top to bottom." 270 "Turned counter-clockwise, reading bottom to top."}}
                        [:enum 0 90 270])]
      (into (map (fn [[k props s]] [k (assoc props :cljplot2/section :text-style) s])) theme/text-style-overrides)))

(defn label-options?
  "Tests whether a label value is a map of [[label-options]] rather than a text.

  Parameters:

  - `x`: a label value - a text (any value, formula and runs values of [[cljplot2.data.text]] included) or a map of
    [[label-options]].

  Returns truthy for a map that isn't a formula or runs value, else `nil` or false."
  [x]
  (and (map? x) (not (text/formula-text? x))))

(def ^:private default-align
  "Placement `[h v]` of a label's text in its box when neither the label nor its role gives `:align`."
  [:center :middle])

(def ^:private content-schema
  "Schema of label content (custom grid items, labels the layout builds): [[label-options]] with `:role` required - plot
  `:labels` get their role from the layout, content has no default."
  (-> (mu/required-keys label-options [:role])
      (mu/merge [:map (schema/optional :fit {:cljplot2/internal true
                                              :description "Fits the text to its box: wrapped, shrunk down to its role's `:min-size`, then cut (axis titles)."}
                                       schema/bool)])))

(defmethod measure/validate-content :label
  [content]
  (schema/explain! [[content-schema content [:content]]]))

(defn- label-spec
  "Gives the drawing parameters of a validated label.

  Parameters:

  - `theme` (map): the merged theme.
  - `content` (map): the label content.

  Returns `{:lines :formulas :bold :rotation :h-align :v-align :inset :color :background :font}` - the role's style
  with the label's inline overrides, the font resolved (see [[cljplot2.graphics.font/resolve-font]]), the colour
  resolved against the label's own `:background` box, or the plot backdrop without one; `:lines` - vectors of runs (see [[cljplot2.data.text/text-lines]]),
  `:formulas` - truthy when a run is a formula, `:bold` - true for a bold font style."
  [theme content]
  (let [rotation (get content :rotation 0)
        style (theme/text-style theme content)
        align (or (:align style) default-align)
        inset (:inset style)
        lines (text/text-lines (:text content) (not (false? (:dollar-math style))))]
    {:lines lines
     :formulas (text/formulas? lines)
     :bold (contains? #{:bold :bold-italic} (:font-style style))
     :rotation rotation
     :h-align (align 0)
     :v-align (align 1)
     :inset (double inset)
     :color (draw/resolved-text-color (:color style) (if (or (nil? (:background style)) (theme/none? (:background style)))
                                                       (draw/plot-backdrop theme)
                                                       (:background style)))
     :background (:background style)
     :font (font/resolve-font (:font style) (:font-style style) (:size style))}))

(defn drawn-style
  "Gives the style a label is drawn with - its role's style with its inline keys over it, resolved.

  Parameters:

  - `theme` (map): the merged theme.
  - `content` (map): the label content (`:role`, inline style keys).

  Returns `{:font :color :background :inset}` - the font resolved (see [[cljplot2.graphics.font/resolve-font]]), the
  colour resolved against the label's `:background` box, or the plot backdrop without one (`:contrast` - black or white), the role's or the inline `:background`
  (`:none` or `nil` - none) and `:inset` in pixels.

  Throws as [[cljplot2.theme/text-style]] for an unknown role."
  [theme content]
  (select-keys (label-spec theme content) [:font :color :background :inset]))

(def ^{:private true :const true :tag 'double} leader-width
  "Width of a leader line joining a label to what it labels, pixels (one rule for every chart's leaders)."
  1.0)

(defn draw-leader!
  "Draws a leader line joining a label to what it labels: thin, in the label's drawn colour (see [[drawn-style]]).

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `content` (map): the label content (`:role`, `:color` ...).
  - `[[x0 y0] [x1 y1]]`: the line's ends in pixels.
  - `env` (map): `:theme`.

  Returns `nil`; the canvas state is isolated."
  [canvas content [[^double x0 ^double y0] [^double x1 ^double y1]] env]
  (c2d/with-isolated-canvas [c canvas]
    (c2d/set-color c (:color (drawn-style (:theme env) content)))
    (c2d/set-stroke-custom c {:size leader-width})
    (c2d/line c x0 y0 x1 y1))
  nil)

(defn- typeset-run
  "Gives a formula run typeset in a label's font.

  Parameters:

  - `run` (map): a formula value (see [[cljplot2.data.text/math]]).
  - `size` (double): the label's font size.
  - `bold` (boolean): true for a bold font style.

  Returns the typeset formula (see [[cljplot2.graphics.latex/typeset]])."
  [run size bold]
  (latex/typeset (:cljplot2/math run) size bold))

(defn- plain-metrics
  "Measures the lines of a label without formulas (see [[text-metrics]]).

  Parameters:

  - `canvas`: the canvas in graphics context, its font set to the label's.
  - `lines` (vector): the lines, vectors of string runs.

  Returns the metrics of [[text-metrics]]: every line the font's height, its runs measured as one string."
  [canvas lines]
  (let [boxes (mapv #(c2d/text-bounding-box canvas (apply str %)) lines)
        [_ ^double top _ ^double line-height] (or (first boxes) [0.0 0.0 0.0 0.0])
        widths (mapv #(double (nth % 2)) boxes)]
    {:lines (mapv (fn [^long idx ^double w] {:width w :top (m/* idx line-height) :ascent (m/- top) :run-widths [w]})
                  (range) widths)
     :block-width (reduce m/max 0.0 widths)
     :block-height (m/* line-height (count lines))}))

(defn- formula-metrics
  "Measures the lines of a label with formulas (see [[text-metrics]]).

  Parameters:

  - `canvas`: the canvas in graphics context, its font set to the label's.
  - `spec` (map): `:lines` (vectors of runs), `:font`, `:bold`.

  Returns the metrics of [[text-metrics]]: a line's ascent and descent are the largest of the font's and its
  formulas'."
  [canvas {lines :lines ^Font font :font bold :bold}]
  (let [size (double (.getSize2D font))
        [_ ^double font-top _ ^double font-height] (c2d/text-bounding-box canvas " ")
        font-ascent (m/- font-top)
        font-descent (m/- font-height font-ascent)
        measured (mapv (fn [runs]
                         (let [typeset (mapv #(when (text/formula? %) (typeset-run % size bold)) runs)
                               run-widths (mapv (fn [run t] (if t (:width t) (double (nth (c2d/text-bounding-box canvas run) 2))))
                                                runs typeset)
                               ascent (double (reduce m/max font-ascent (keep :height typeset)))
                               descent (double (reduce m/max font-descent (keep :depth typeset)))]
                           {:width (reduce m/+ 0.0 run-widths) :ascent ascent :height (m/+ ascent descent) :run-widths run-widths}))
                       lines)
        tops (reductions m/+ 0.0 (map :height measured))]
    {:lines (mapv (fn [line top] (-> line (assoc :top top) (dissoc :height))) measured tops)
     :block-width (reduce m/max 0.0 (map :width measured))
     :block-height (reduce m/+ 0.0 (map :height measured))}))

(defn- text-metrics
  "Sets the label font on a canvas and measures the label's lines.

  Parameters:

  - `canvas`: the canvas in graphics context (its font changes).
  - `spec` (map): the label's drawing parameters (see [[label-spec]]) - `:lines`, `:formulas`, `:font`, `:bold`.

  Returns `{:lines :block-width :block-height}`: per line `{:width :top :ascent :run-widths}` (the line's top and
  baseline below the text block's top, each run's width) and the unrotated text block size; no lines - zeros."
  [canvas spec]
  (c2d/set-font canvas (:font spec))
  (if (:formulas spec)
    (formula-metrics canvas spec)
    (plain-metrics canvas (:lines spec))))

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

(defn- run-words
  "Gives the words of a line of runs: pieces between white space, a formula part of the word it touches.

  Parameters:

  - `runs` (vector): a line's runs - strings and formula values.

  Returns a vector of words, each a non-empty vector of runs."
  [runs]
  (let [close (fn [acc word] (cond-> acc (seq word) (conj word)))
        [words word] (reduce (fn [[acc word] run]
                               (if (text/formula? run)
                                 [acc (conj word run)]
                                 (let [[head & tail] (str/split run #"\s+" -1)
                                       continued (cond-> word (seq head) (conj head))]
                                   (reduce (fn [[a w] part] [(close a w) (if (seq part) [part] [])])
                                           [acc continued] tail))))
                             [[] []] runs)]
    (close words word)))

(defn- words-line
  "Joins words into one line of runs, single spaces between them.

  Parameters:

  - `words` (sequence): words, vectors of runs.

  Returns a vector of runs."
  [words]
  (vec (apply concat (interpose [" "] words))))

(defn- wrapped-runs
  "Gives a line of runs wrapped at white space into lines that fit a length (see [[wrapped-line]]).

  Parameters:

  - `fits?` (function): tests whether a line of runs fits.
  - `runs` (vector): the line.

  Returns a vector of at most [[max-wrapped-lines]] lines of runs, the last one holding the words left; an empty line
  stays one empty line."
  [fits? runs]
  (let [words (run-words runs)]
    (if (empty? words)
      [[]]
      (let [lines (reduce (fn [acc word]
                            (let [current (peek acc)
                                  joined (when current (words-line [current word]))]
                              (if (and current (fits? joined)) (conj (pop acc) joined) (conj acc word))))
                          [] words)]
        (if (m/> (count lines) max-wrapped-lines)
          (conj (subvec lines 0 (m/long-dec max-wrapped-lines))
                (words-line (subvec lines (m/long-dec max-wrapped-lines))))
          lines)))))

(defn- cut-runs
  "Gives the longest start of a line of runs that, ended with an ellipsis, fits a length; formulas are never cut.

  Parameters:

  - `fits?` (function): tests whether a line of runs fits.
  - `runs` (vector): the line.

  Returns `runs` when it fits, else its longest start - whole runs, then a start of a string run (whitespace trimmed)
  - with the ellipsis; the ellipsis alone when no start fits."
  [fits? runs]
  (if (fits? runs)
    runs
    (or (some (fn [^long i]
                (let [before (subvec runs 0 i)
                      run (nth runs i)]
                  (if (text/formula? run)
                    (let [line (conj before ellipsis)] (when (fits? line) line))
                    (let [cut (cut-line #(fits? (conj before %)) (str run))]
                      (when (and (not= ellipsis cut) (fits? (conj before cut))) (conj before cut))))))
              (range (m/long-dec (count runs)) -1 -1))
        [ellipsis])))

(defn- runs-content
  "Gives label content holding lines of runs as its text.

  Parameters:

  - `content` (map): the label content.
  - `lines` (sequence): lines, vectors of runs (strings and formula values).

  Returns `content` with a runs value `:text` (see [[cljplot2.data.text/runs]]) and `:dollar-math false` - its
  strings are already parsed."
  [content lines]
  (assoc content :text (apply text/runs (interpose "\n" (map #(apply text/runs %) lines))) :dollar-math false))

(defn- shrunk
  "Gives label content drawn smaller to fit a length, down to its text role's `:min-size`.

  Parameters:

  - `content` (map): the label content.
  - `natural` (double): its length along the text.
  - `length` (double): pixels along the text.
  - `env` (map): `:theme`.

  Returns `content` with `:size` - its size scaled down to fit, rounded down, at least the least size."
  [content ^double natural ^double length env]
  (let [{:keys [^double size min-size]} (theme/text-style (:theme env) content)
        least (m/min size (double (or min-size size)))]
    (assoc content :size (m/max least (m/floor (m// (m/* size length) natural))))))

(defn fitted-label
  "Gives a label fitted to a length along its text: wrapped, then shrunk, then cut.

  Text longer than the length is first wrapped at word boundaries (when `:wrap` is true; each line into at most 3
  lines, given line breaks kept), then drawn smaller - its size scaled down to fit, rounded down, down to its text
  role's `:min-size` (a role without one, or a size below it, isn't shrunk) - and when that is still too long each
  line too long is cut, ending with an ellipsis (unless `:cut` is false: the text kept whole, longer than the length
  at the least size). Formulas are words of their own: never split or cut (a text with formulas is cut in its plain
  pieces). Used by facet strips (no wrapping), axis titles (see [[cljplot2.layout.plot-grid]]) and text charts with
  `:x-span` (no cutting, see [[cljplot2.chart.text]]).

  Parameters:

  - `content` (map): the label content (its `:size` over its role's).
  - `fit` (map): `:length` - pixels along the text; `:wrap` - true wraps first (default false); `:cut` - false keeps
    the text whole (default true).
  - `env` (map): `:canvas` (a canvas in graphics context; its font changes), `:theme`.

  Returns `content` when it fits, else with a wrapped or cut `:text` (a string, with formulas a runs value; its
  dollars already read, so with `:dollar-math false`) and possibly a `:size`; a length too short for any text leaves the ellipsis."
  [content {:keys [^double length wrap cut] :or {cut true}} env]
  (let [natural (along-text content env)]
    (if (m/<= natural length)
      content
      (let [{:keys [lines formulas]} (label-spec (:theme env) content)
            fits-in (fn [c] (fn [t] (m/<= (along-text (assoc c :text t) env) length)))
            runs-fit (fn [c] (fn [line] (m/<= (along-text (runs-content c [line]) env) length)))
            wrapped (when wrap
                      (if formulas
                        (runs-content content (mapcat (partial wrapped-runs (runs-fit content)) lines))
                        (assoc content
                               :text (str/join "\n" (mapcat (partial wrapped-line (fits-in content)) (text/plain-lines lines)))
                               :dollar-math false)))]
        (if (and wrapped (m/<= (along-text wrapped env) length))
          wrapped
          (let [base (or wrapped content)
                sized (shrunk base (if wrapped (along-text wrapped env) natural) length env)]
            (cond
              (or (not cut) (m/<= (along-text sized env) length)) sized
              formulas (let [fits? (runs-fit sized)]
                         (runs-content sized (map #(cut-runs fits? %) (:lines (label-spec (:theme env) sized)))))
              :else (let [fits? (fits-in sized)]
                      (assoc sized
                             :text (str/join "\n" (map #(cut-line fits? %) (text/plain-lines (:lines (label-spec (:theme env) sized)))))
                             :dollar-math false)))))))))

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

(defn- draw-lines!
  "Draws a label's lines: each run after the previous one on its line's baseline.

  Parameters:

  - `canvas`: the canvas in graphics context, its font and colour set to the label's, the origin at the text frame.
  - `spec` (map): `:lines` (vectors of runs), `:h-align`, `:color`, `:font`, `:bold` (see [[label-spec]]).
  - `metrics` (map): the lines' metrics of [[text-metrics]].
  - `[block-x block-y]` (doubles): the text block's top-left corner.

  Returns `nil`."
  [canvas {:keys [lines h-align color ^Font font bold]} {line-metrics :lines ^double block-width :block-width} [^double block-x ^double block-y]]
  (let [size (double (.getSize2D font))]
    (doseq [[runs {:keys [^double width ^double top ^double ascent run-widths]}] (map vector lines line-metrics)]
      (let [baseline (m/+ block-y top ascent)]
        (reduce (fn [^double x [run ^double run-width]]
                  (if (text/formula? run)
                    (latex/draw-typeset! canvas (typeset-run run size bold) x baseline color)
                    (c2d/text canvas run x baseline))
                  (m/+ x run-width))
                (m/+ block-x (align-offset h-align block-width width))
                (map vector runs run-widths))))
    nil))

(defmethod draw/draw-content :label
  [content canvas {:keys [^long x ^long y ^long w ^long h] :as box} {:keys [theme] :as env}]
  (let [spec (label-spec theme (box-fitted content box (assoc env :canvas canvas)))]
    (when (seq (:lines spec))
      (let [{:keys [^double block-width ^double block-height] :as metrics} (text-metrics canvas spec)
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
        (draw-lines! canvas spec metrics [block-x block-y])))))

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

(defn frame-corners
  "Gives the corners of the text frame [[draw-at!]] draws for a label with its anchor at a point.

  For a rotation that is a multiple of 90 the frame is the box itself ([[box-at]]); else the unrotated text frame
  ([[text-frame]]) turned about the box's centre - inside the box, which is its rounded-up bounding box.

  Parameters:

  - `c`: a canvas in graphics context, measuring.
  - `content` (map): the label content (`:rotation` - clockwise degrees, default 0).
  - `anchor` (`[h v]`): the anchor of the box (see [[box-at]]).
  - `x`, `y` (numbers): the anchor's pixel.
  - `env` (map): `:theme`.

  Returns a vector of four `[x y]` in order around the frame."
  [c content anchor x y env]
  (let [{bx :x by :y bw :w bh :h} (box-at c content anchor x y env)
        rotation (double (get content :rotation 0))]
    (if (quarter-turn rotation)
      (let [x0 (double bx) y0 (double by) x1 (m/+ x0 (double bw)) y1 (m/+ y0 (double bh))]
        [[x0 y0] [x1 y0] [x1 y1] [x0 y1]])
      (let [[^double fw ^double fh] (text-frame content c (:theme env))
            cx (m/+ (double bx) (m/* 0.5 (double bw)))
            cy (m/+ (double by) (m/* 0.5 (double bh)))
            a (m/radians rotation)
            cosine (m/cos a) sine (m/sin a)]
        (mapv (fn [[^double u ^double v]]
                (let [fu (m/* u 0.5 fw) fv (m/* v 0.5 fh)]
                  [(m/+ cx (m/- (m/* fu cosine) (m/* fv sine))) (m/+ cy (m/+ (m/* fu sine) (m/* fv cosine)))]))
              [[-1.0 -1.0] [1.0 -1.0] [1.0 1.0] [-1.0 1.0]])))))

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

(defn draw-along-path!
  "Draws a label's text along a pixel polyline, glyph by glyph, in the label's drawn style (see [[drawn-style]]).

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `content` (map): the label content - `:text` (converted with `str`; one line), `:role`, inline style keys; its
    `:background` (the role's or inline; `:none` / `nil` - none) fills a band under the glyphs, `:inset` pixels past
    the text on each side of it.
  - `points` (sequence of `[x y]`): the pixel polyline.
  - `placement` (map): `:position`, `:anchor`, `:offset` (see [[cljplot2.graphics.text-path/draw!]]).
  - `env` (map): `:theme`.

  Returns a truthy value when the text is drawn, `nil` when it isn't (see [[cljplot2.graphics.text-path/draw!]]).

  Throws `ex-info {:key :text}` for a text with formulas (a formula isn't laid along a path; `:dollar-math false`
  keeps dollars literal)."
  [canvas content points placement env]
  (let [{:keys [lines formulas]} (label-spec (:theme env) content)]
    (when formulas
      (schema/fail! "A text along a path can't hold LaTeX formulas (`:dollar-math false` keeps dollars literal)."
                    {:key :text :value (:text content)}))
    (text-path/draw! canvas (str/join "\n" (text/plain-lines lines)) points (drawn-style (:theme env) content) placement)))

(defn formulas?
  "Tests whether a label's text holds formulas.

  Parameters:

  - `theme` (map): the merged theme.
  - `content` (map): the label content (`:text`, `:role`, inline `:dollar-math`).

  Returns truthy when its text holds a formula (a formula value, or a dollar pair with dollar math on - see
  [[cljplot2.data.text]]), else `nil`."
  [theme content]
  (:formulas (label-spec theme content)))

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
