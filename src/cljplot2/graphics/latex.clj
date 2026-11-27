(ns cljplot2.graphics.latex
  "LaTeX formulas typeset by JLaTeXMath: measured and drawn on a canvas at a font size.

  Formulas use JLaTeXMath's own fonts (Computer Modern, in the library jar). Its GPL-only Greek and Cyrillic font jars
  are excluded: Greek letters work in math (commands such as alpha after a backslash, or Unicode letters), not in text
  mode (the text command); Cyrillic letters are rejected. Formulas are set in TeX's text style (as an inline formula in
  a paragraph) at the label's font size and in its colour, bold (the boldsymbol command) for a bold font style;
  colour commands inside a formula (textcolor, fgcolor) keep theirs. At the same size the capital height of a formula
  is 0.94 of the default font's (DejaVu Sans Mono) and 0.96 of Roboto's, so formulas take no scale of their own.

  This namespace draws through the library's Java2D boxes: clojure2d has no function for them (the one drawing
  interop kept outside clojure2d on purpose).

  Functions: [[typeset]], [[draw-typeset!]], [[problem]]."
  (:require [clojure2d.core :as c2d]
            [clojure2d.protocols :as pr]
            [fastmath.core :as m]
            [cljplot2.cache :as cache]
            [cljplot2.schema :as schema])
  (:import [java.awt Graphics2D]
           [org.scilab.forge.jlatexmath Box TeXConstants TeXFormula TeXIcon]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^{:private true :const true :tag 'long} typeset-cache-limit
  "Most typeset formulas kept (a formula at one size and weight is one entry)."
  512)

(defonce ^{:private true :doc "Typeset formulas: `[tex size bold]` -> the formula of [[typeset]]."}
  typeset-cache
  (cache/lru-cache typeset-cache-limit))

(def ^:private cyrillic
  "Cyrillic letters (the Cyrillic and Cyrillic Supplement blocks), which need JLaTeXMath's excluded font jar."
  #"[Ѐ-ԯ]")

(defn- typeset-uncached
  "Parses and typesets a formula.

  Parameters:

  - `[tex size bold]`: the LaTeX source (math mode), the font size in points and true for bold.

  Returns `{:box :size :width :height :depth}`.

  Throws `ex-info {:key :text :value tex}` when the formula doesn't parse or holds Cyrillic letters."
  [[^String tex ^double size bold]]
  (when (re-find cyrillic tex)
    (schema/fail! "Cyrillic letters can't be typeset in formulas (their font isn't included)." {:key :text :value tex}))
  (let [^TeXIcon icon (try
                        (.createTeXIcon (TeXFormula. (if bold (str "\\boldsymbol{" tex "}") tex)) TeXConstants/STYLE_TEXT (float size))
                        (catch Exception e
                          (schema/fail! (str "Invalid LaTeX formula: " (ex-message e)) {:key :text :value tex})))
        ^Box box (.getBox icon)]
    {:box box
     :size size
     :width (double (.getTrueIconWidth icon))
     :height (m/* (.getHeight box) size)
     :depth (m/* (.getDepth box) size)}))

(defn typeset
  "Gives a LaTeX formula typeset at a font size, with its metrics.

  Parameters:

  - `tex` (string): the formula in LaTeX math mode, e.g. `x_i^2`.
  - `size` (number): the font size in points (pixels at scale 1).
  - `bold` (boolean): true typesets it bold.

  Returns `{:box :size :width :height :depth}` - the typeset box (drawn by [[draw-typeset!]]) and, in pixels, its width, its height above the baseline and its depth below it. A formula asked for again (same source, size and weight) is not typeset again.

  Throws `ex-info {:key :text :value tex}` when the formula doesn't parse (an unknown command; JLaTeXMath completes unbalanced braces) or holds Cyrillic letters."
  [tex size bold]
  (cache/lookup-or-compute typeset-cache [(str tex) (double size) (true? bold)] typeset-uncached))

(defn problem
  "Gives why a formula can't be typeset.

  Parameters:

  - `tex` (string): the formula in LaTeX math mode.

  Returns the reason as one message (e.g. `Invalid LaTeX formula: ...`, without the `Invalid input:` prefix of the thrown error), or `nil` when the formula typesets (see [[typeset]])."
  [tex]
  (try (typeset tex 10.0 false) nil
       (catch clojure.lang.ExceptionInfo e (or (:message (first (:errors (ex-data e)))) (ex-message e)))))

(defn draw-typeset!
  "Draws a typeset formula with its baseline start at a point, in a colour.

  Parameters:

  - `canvas`: the canvas in graphics context (its transform applies: rotated text draws rotated formulas).
  - `formula` (map): a formula of [[typeset]].
  - `x`, `y` (numbers): the pixel of the baseline's start.
  - `color`: any clojure2d colour.

  Returns `nil`; the canvas state is isolated. Safe from several threads: one formula is drawn by one thread at a time."
  [canvas {:keys [^Box box ^double size]} x y color]
  (c2d/with-isolated-canvas [c canvas]
    (c2d/set-color c color)
    (c2d/translate c (double x) (double y))
    (c2d/scale c size)
    ;; a JLaTeXMath box draws itself on a Graphics2D (clojure2d has no function for it); it keeps drawing state in
    ;; its fields, so a cached box is drawn by one thread at a time
    (locking box
      (.draw box ^Graphics2D (pr/graphics2d c) (float 0.0) (float 0.0))))
  nil)
