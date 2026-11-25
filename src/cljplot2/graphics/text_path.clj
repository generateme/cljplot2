(ns cljplot2.graphics.text-path
  "Text laid along a polyline in pixels, glyph by glyph, each glyph turned to the line where it sits.

  The text is laid on its baseline line: the path shifted sideways (a parallel offset line, mitred at the corners, its
  backward steps on the inner side of tight bends dropped) so the text sits on its side of the path - the anchor's
  `v`: `:middle` - the text centred on the path, `:bottom` - its box's bottom on the path, the text above it, `:top` -
  below it; `:offset [dx dy]` moves it `dy` pixels further up (up as the text reads). Glyphs follow one another by
  arc length along that baseline line, each from its start point to its end point on it (the glyph turned to that
  chord), so they neither pile up on the inner side of a bend nor spread on the outer one; glyph advances are the
  widths of the text's prefixes (kerning kept).

  Placement by arc length along the baseline line: the anchor's `h` puts the text at a fraction `:position` of its
  length (`:left` - the text starts there, `:center` - centred there, `:right` - ends there), `dx` moves it along.
  Text longer than the line isn't drawn; text running past an end is moved back onto it.

  Read upright by the whole path's direction: a path whose last point lies left of its first one is laid reversed
  (the fraction then from the other end); an exactly vertical one (first and last points with the same x) reads
  bottom to top, as a y axis title; a closed path (last point on the first) is laid as given.

  Functions: [[baseline-path]], [[glyph-placements]], [[draw!]]."
  (:require [clojure2d.core :as c2d]
            [clojure2d.core.shape :as sh]
            [fastmath.core :as m]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.theme :as theme]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^{:private true :const true :tag 'double} vertical-tolerance
  "Largest x difference in pixels between a path's first and last points for which the path counts as vertical."
  1.0e-6)

(def ^{:private true :const true :tag 'double} min-miter-cosine
  "Smallest cosine of half the turn at a corner the baseline line is mitred at; sharper corners are bevelled (geometry)."
  0.25)

(defn- upright-path
  "Gives a path oriented to read upright.

  Parameters:

  - `path` (vector of `[x y]`): the pixel polyline, at least 2 points.

  Returns `path` or its reverse (a vector): reversed when its last point lies left of its first one, or - exactly
  vertical (x difference within [[vertical-tolerance]]) - below it (so it reads bottom to top); a closed path as given."
  [path]
  (let [[^double x0 ^double y0] (first path)
        [^double x1 ^double y1] (peek path)
        dx (m/- x1 x0)]
    (if (or (m/< dx (m/- vertical-tolerance))
            (and (m/<= (m/abs dx) vertical-tolerance) (m/> y1 y0)))
      (vec (rseq path))
      path)))

(defn- unit-normal
  "Gives the unit normal of a segment, to its right as drawn (y down) - down as text along it reads.

  Parameters:

  - `[[x0 y0] [x1 y1]]`: the segment, of positive length.

  Returns `[nx ny]`."
  [[[^double x0 ^double y0] [^double x1 ^double y1]]]
  (let [dx (m/- x1 x0) dy (m/- y1 y0) l (m/hypot dx dy)]
    [(m// (m/- dy) l) (m// dx l)]))

(defn baseline-path
  "Gives a polyline shifted sideways by a distance - the line text laid along it sits on.

  Parameters:

  - `path` (vector of `[x y]`): the polyline, at least 2 points, no two consecutive ones equal.
  - `distance` (double): pixels to the right of the path as drawn (y down) - below it as text along it reads; negative
    - to the left.

  Returns a vector of `[x y]`: every point moved along the normal of its segments - mitred at a corner (the offset
  lines meet), bevelled at one turning more than about 150 degrees (two points); points stepping backward along their
  segment (the inner side of a bend tighter than `distance`) are dropped. The path itself for `distance` 0."
  [path ^double distance]
  (if (m/zero? distance)
    path
    (let [segments (mapv vector path (rest path))
          normals (mapv unit-normal segments)
          last-index (m/long-dec (count path))
          shifted (fn [[^double x ^double y] [^double nx ^double ny] ^double k]
                    [(m/+ x (m/* k nx)) (m/+ y (m/* k ny))])
          ;; [point segment] - each shifted point with the index of the segment it comes along
          offset (into []
                       (mapcat (fn [^long i]
                                 (let [p (path i)]
                                   (cond
                                     (m/zero? i) [[(shifted p (normals 0) distance) 0]]
                                     (m/== i last-index) [[(shifted p (peek normals) distance) (m/long-dec i)]]
                                     :else (let [[^double ax ^double ay :as a] (normals (m/long-dec i))
                                                 [^double bx ^double by :as b] (normals i)
                                                 mx (m/+ ax bx) my (m/+ ay by)
                                                 ml (m/hypot mx my)
                                                 cosine (if (m/pos? ml) (m/+ (m/* ax (m// mx ml)) (m/* ay (m// my ml))) 0.0)]
                                             (if (m/< cosine min-miter-cosine)
                                               [[(shifted p a distance) (m/long-dec i)] [(shifted p b distance) i]]
                                               [[(shifted p [(m// mx ml) (m// my ml)] (m// distance cosine)) (m/long-dec i)]]))))))
                       (range (count path)))]
      ;; keep the line moving forward: drop a point stepping back along the segment it comes along
      (reduce (fn [kept [[^double x ^double y :as q] ^long k]]
                (let [[^double lx ^double ly] (peek kept)
                      [[^double sx0 ^double sy0] [^double sx1 ^double sy1]] (segments k)]
                  (if (m/neg? (m/+ (m/* (m/- x lx) (m/- sx1 sx0)) (m/* (m/- y ly) (m/- sy1 sy0))))
                    kept
                    (conj kept q))))
              [(ffirst offset)] (rest offset)))))

(defn glyph-placements
  "Gives where the glyphs of a text sit along a polyline, the text turned to read upright.

  Parameters:

  - `points` (sequence of `[x y]`): the pixel polyline; consecutive equal points are merged.
  - `advances` (vector of doubles): the glyphs' edges along the text - `n + 1` non-decreasing values from 0.0, glyph
    `i` from `advances i` to `advances i + 1`, the last one the text's length.
  - `placement` (map): `:baseline` - the baseline's distance below the path as the text reads, pixels (default 0);
    `:position` - a fraction of the baseline line's length (default 0.5); `:anchor` `[h v]` - `h` places the text at
    the position (`:left` starts there, `:center` - default, `:right` ends there); `:offset [dx dy]` - `dx` pixels
    along the line (default 0).

  Returns a vector of `[x y angle]`, one per glyph: the glyph's start on the baseline line (see [[baseline-path]]) of
  the upright path (see the namespace doc) and its angle - radians, clockwise as drawn - of the chord to its end
  there; `nil` for fewer than 2 distinct points, no glyphs or a text longer than the baseline line."
  [points advances {:keys [baseline position anchor offset]}]
  (let [path (vec (dedupe (map (fn [[x y]] [(double x) (double y)]) points)))]
    (when (and (m/< 1 (count path)) (m/< 1 (count advances)))
      (let [upright (upright-path path)
            reversed? (not (identical? upright path))
            line (baseline-path upright (double (or baseline 0.0)))
            lengths (polyline/cumulative-lengths line)
            total (double (peek lengths))
            text-length (double (peek advances))
            fraction (double (or position 0.5))
            [h] (or anchor [:center :middle])
            [^double dx] (or offset [0 0])]
        (when (and (m/pos? total) (m/<= text-length total))
          (let [at (m/+ (m/* (if reversed? (m/- 1.0 fraction) fraction) total) dx)
                start (m/constrain (case h
                                     :left at
                                     :center (m/- at (m/* 0.5 text-length))
                                     :right (m/- at text-length))
                                   0.0 (m/- total text-length))
                at-length (fn [^double s] (polyline/point-at-length line lengths (m/constrain s 0.0 total)))]
            (mapv (fn [^double a ^double b]
                    (let [[[^double x0 ^double y0] _ ^double angle] (at-length (m/+ start a))
                          [[^double x1 ^double y1]] (at-length (m/+ start b))
                          cx (m/- x1 x0) cy (m/- y1 y0)]
                      [x0 y0 (if (m/> (m/hypot cx cy) 1.0e-9) (m/atan2 cy cx) angle)]))
                  advances (rest advances))))))))

(defn- baseline-shift
  "Gives the baseline of a glyph below the path, in the glyph's turned frame.

  Parameters:

  - `v` (keyword): the side - `:middle`, `:bottom` (text above the path), `:top` (below it).
  - `top` (double): the line box's top relative to the baseline (negative).
  - `line-height` (double): the line box's height.

  Returns the baseline's distance below the path in pixels."
  ^double [v ^double top ^double line-height]
  (case v
    :middle (m/- (m/+ top (m/* 0.5 line-height)))
    :bottom (m/- (m/+ top line-height))
    :top (m/- top)))

(defn- band-shape
  "Gives the area under a text's glyphs along a path - the background of a label laid along it.

  Parameters:

  - `placed` (sequence of `[x y angle]`): the glyphs' starts (see [[glyph-placements]]).
  - `widths` (sequence of doubles): the glyphs' advances.
  - `[top bottom]`: the band's top and bottom relative to the baseline in the glyphs' frame (pixels, y down).
  - `inset` (double): pixels the band reaches past the text's first and last glyphs.

  Returns a shape - the union of the glyphs' turned boxes and of the joints between neighbouring boxes (filling the
  wedges a bend opens between them)."
  [placed widths [^double top ^double bottom] ^double inset]
  (let [n (count widths)
        corners (mapv (fn [^long i [^double x ^double y ^double angle] ^double w]
                        (let [u0 (if (m/zero? i) (m/- inset) 0.0)
                              u1 (if (m/== i (m/long-dec n)) (m/+ w inset) w)
                              cosine (m/cos angle) sine (m/sin angle)]
                          (mapv (fn [[^double u ^double v]]
                                  [(m/+ x (m/- (m/* u cosine) (m/* v sine))) (m/+ y (m/+ (m/* u sine) (m/* v cosine)))])
                                [[u0 top] [u1 top] [u1 bottom] [u0 bottom]])))
                      (range) placed widths)
        joints (map (fn [[_ top-right bottom-right _] [top-left _ _ bottom-left]] [top-right top-left bottom-left bottom-right])
                    corners (rest corners))]
    (reduce sh/add-shape (map sh/path (concat corners joints)))))

(defn draw!
  "Draws a text along a polyline, glyph by glyph (see [[glyph-placements]]).

  Parameters:

  - `canvas`: the canvas in graphics context (its state is restored afterwards).
  - `text` (string): the text, one line (a line break is drawn as a glyph of its own).
  - `points` (sequence of `[x y]`): the pixel polyline.
  - `style` (map): `:font` - a `java.awt.Font`, `:color` - the text colour; `:background` - a colour filling a band
    under the glyphs (`nil` / `:none` - none) from the line box's top to its bottom plus `:inset` pixels (default 0)
    on every side.
  - `placement` (map): `:position`, `:anchor` `[h v]` (default `[:center :middle]`), `:offset [dx dy]` - `dy` pixels
    up from the path (see the namespace doc).

  Returns a truthy value when the text is drawn; `nil` for an empty text, fewer than 2 distinct points or a text
  longer than its baseline line."
  [canvas text points {:keys [font color background inset]} {:keys [anchor offset] :as placement}]
  (when (seq text)
    (c2d/with-isolated-canvas [c canvas]
      (c2d/set-font c font)
      (let [glyphs (vec (re-seq #"(?s)." text))
            advances (mapv (fn [^long k] (if (m/zero? k) 0.0 (double (nth (c2d/text-bounding-box c (apply str (subvec glyphs 0 k))) 2))))
                           (range (m/long-inc (count glyphs))))
            [_ ^double top _ ^double line-height] (c2d/text-bounding-box c text)
            [_ v] (or anchor [:center :middle])
            [_ ^double dy] (or offset [0 0])
            baseline (m/- (baseline-shift v top line-height) dy)]
        (when-let [placed (glyph-placements points advances (assoc placement :baseline baseline))]
          (when (and (some? background) (not (theme/none? background)))
            (let [pad (double (or inset 0.0))]
              (c2d/set-color c background)
              (c2d/shape c (band-shape placed (map m/- (rest advances) advances)
                                       [(m/- top pad) (m/+ top line-height pad)] pad))))
          (c2d/set-color c color)
          (doseq [[glyph [^double x ^double y ^double angle]] (map vector glyphs placed)]
            (c2d/push-matrix c)
            (c2d/translate c x y)
            (c2d/rotate c angle)
            (c2d/text c glyph 0.0 0.0)
            (c2d/pop-matrix c))
          true)))))
