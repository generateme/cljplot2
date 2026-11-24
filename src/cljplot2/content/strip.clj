(ns cljplot2.content.strip
  "Strip content: a facet label over a strip box.

  Content `{:kind :strip :text t}` (optional `:rotation`) is a `:strip` role label over theme
  `[:strip {:fill :border-color :border-width}]` (`:none` colors draw nothing); its natural size is the label's.
  With `:interval {:lo :hi :min :max}` (a shingle level) a bar in theme `[:strip :shingle-fill]` shades the part of
  the strip that `[lo hi]` covers within `[min max]` - along the text direction (top-down for `:rotation 90`).
  Text longer than its strip is drawn smaller, down to theme `[:strip :min-text-size]` points, then each line too
  long is cut with an ellipsis.

  Functions: methods of [[cljplot2.graphics.measure/validate-content]], [[cljplot2.graphics.measure/natural-size]]
  and [[cljplot2.graphics.draw/draw-content]] for `:strip`."
  (:require [clojure.string :as str]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- strip-label
  "Gives the `:strip` role label content of strip `content`.

  Parameters:

  - `content` (map): the strip content - `:text`, optional `:rotation`.

  Returns `{:kind :label :role :strip :text t}`, with the rotation when given."
  [{:keys [text rotation]}]
  (cond-> {:kind :label :role :strip :text text} rotation (assoc :rotation rotation)))

(defn- interval-fractions
  "Gives the fractions of a strip a shingle interval covers.

  Parameters:

  - `interval` (map): `:lo`, `:hi` - the level's range; `:min`, `:max` - the range of all levels.

  Returns `[a b]`, the fractions of `lo` and `hi` within `[min max]`; `[0.0 1.0]` when `min` equals `max` (or exceeds it)."
  [{:keys [^double lo ^double hi] ^double start :min ^double end :max}]
  (if (m/> end start)
    [(m/norm lo start end) (m/norm hi start end)]
    [0.0 1.0]))

(def ^:private ellipsis
  "The mark ending a cut strip text."
  "\u2026")

(defn- rotated?
  "Tests whether a content's `:rotation` is a number equal to one of some angles.

  Parameters:

  - `content` (map): a content, optional `:rotation` (degrees).
  - `angles` (sequence of numbers): the angles.

  Returns truthy when `:rotation` is a number numerically equal (`m/==`, so `90.0` matches `90`) to one of `angles`, else `nil`."
  [{:keys [rotation]} angles]
  (when (number? rotation) (some #(m/== (double rotation) (double %)) angles)))

(defn- along-text
  "Gives the length of a label along its text direction.

  Parameters:

  - `label-content` (map): a label content, optional `:rotation`.
  - `env` (map): `:canvas` (a canvas in graphics context; its font changes), `:theme`.

  Returns the natural width of the unrotated label (its height for a rotation of 90 or 270), a double."
  ^double [label-content env]
  (let [^Vec2 size (measure/natural-size label-content env)]
    (if (rotated? label-content [90 270]) (.y size) (.x size))))

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

(defn- fitted-label
  "Gives the strip label fitted to the strip's length along its text.

  Text longer than the strip is drawn smaller - its role's size scaled down to fit, down to theme
  `[:strip :min-text-size]` - and when that is still too long each line too long is cut, ending with an ellipsis.

  Parameters:

  - `label-content` (map): the `:strip` role label (see [[strip-label]]).
  - `available` (double): the strip's length along the text, in pixels.
  - `env` (map): `:canvas` (a canvas in graphics context; its font changes), `:theme`.

  Returns `label-content` when it fits, else with `:size` and possibly a cut `:text`."
  [label-content ^double available env]
  (let [natural (along-text label-content env)]
    (if (m/<= natural available)
      label-content
      (let [theme (:theme env)
            role-size (double (:size (theme/text-style theme label-content)))
            min-size (m/min role-size (double (get-in theme [:strip :min-text-size])))
            size (m/max min-size (m/floor (m// (m/* role-size available) natural)))
            sized (assoc label-content :size size)
            fits? (fn [text] (m/<= (along-text (assoc sized :text text) env) available))]
        (if (fits? (:text sized))
          sized
          (assoc sized :text (str/join "\n" (map #(cut-line fits? %) (str/split-lines (str (:text sized)))))))))))

(defmethod measure/validate-content :strip
  [content]
  (measure/validate-content (strip-label content)))

(defmethod measure/natural-size :strip
  [content env]
  (measure/natural-size (strip-label content) env))

(defmethod draw/draw-content :strip
  [content c {:keys [^long x ^long y ^long w ^long h] :as box} env]
  (let [{:keys [fill border-color border-width shingle-fill]} (get-in env [:theme :strip])]
    (when-not (theme/none? fill)
      (c2d/set-color c fill)
      (c2d/rect c x y w h))
    (when-let [interval (:interval content)]
      (when-not (theme/none? shingle-fill)
        (let [[^double a ^double b] (interval-fractions interval)]
          (c2d/set-color c shingle-fill)
          (if (rotated? content [90])
            (let [y0 (m/long-add y (m/round (m/* a h))) y1 (m/long-add y (m/round (m/* b h)))]
              (c2d/rect c x y0 w (m/long-sub y1 y0)))
            (let [x0 (m/long-add x (m/round (m/* a w))) x1 (m/long-add x (m/round (m/* b w)))]
              (c2d/rect c x0 y (m/long-sub x1 x0) h))))))
    (draw/inner-border! c box border-color border-width)
    (c2d/with-isolated-canvas [isolated c]
      (let [available (double (if (rotated? content [90 270]) h w))]
        (draw/draw-content (fitted-label (strip-label content) available (assoc env :canvas isolated)) isolated box env)))))
