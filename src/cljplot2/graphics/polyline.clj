(ns cljplot2.graphics.polyline
  "Polylines in pixel coordinates: stroked runs of points, broken where a coordinate is NaN, and areas between them
  and a baseline.

  Line style: `{:color c :width w :dash d}` - `:color` a clojure2d color or `:none`, `:width` pixels (0 draws
  nothing), `:dash` `:none` / absent, a name of [[cljplot2.schema/dash-styles]] or a sequence of positive dash
  lengths (resolved by [[dash-pattern]] - every stroke with a dash goes through it).

  Functions: runs [[finite-runs]], [[runs]], [[path-length]]; strokes [[set-style!]], [[stroke-runs!]], [[draw!]],
  [[draw-segments!]], [[bridge-style]], [[key-glyph!]]; shapes [[trail-shape]], [[rings-path]]; areas
  [[fill-polygons!]], [[fill-under!]], [[fill-runs-to-baseline!]], [[fill-closed-runs!]], [[fill-between!]],
  [[fill-area!]], [[area-color]], gradients [[gradient-fn]], [[fill-gradient-paint!]]."
  (:require [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [clojure2d.core.shape :as sh]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [java.awt Shape]
           [java.awt.geom Path2D]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def style-entries
  "Map entries of a line style (all optional; defaults from the theme)."
  (schema/in-fragment
   :line-style
   [(schema/optional :color {:description "line colour (a mapped :color wins)"} schema/color)
    (schema/optional :width {:description "line width in pixels"} schema/non-neg)
    (schema/nilable :dash {:description "dash style of the line (a mapped :dash wins)"} schema/dash)]))

(defn finite-runs
  "Gives the runs of consecutive indices whose coordinates are finite.

  Parameters:

  - `xs`, `ys` (indexed sequences of numbers): the coordinates, equal counts.

  Returns a vector of index vectors; a non-finite coordinate ends a run. Empty for no finite point."
  [xs ys]
  (let [finite-at? (fn [i] (and (m/valid-double? (double (nth xs i))) (m/valid-double? (double (nth ys i)))))]
    ;; runs are indexed (first / peek) and stored by callers
    (->> (range (count xs))
         (partition-by finite-at?)
         (filter (comp finite-at? first))
         (mapv vec))))

(defn runs
  "Gives the runs of consecutive points with finite coordinates.

  Parameters:

  - `xs`, `ys` (indexed sequences of numbers): the coordinates, equal counts.

  Returns a vector of runs, each a vector of `[x y]` (see [[finite-runs]])."
  [xs ys]
  ;; drawn runs are vectors: callers take their last point with peek
  (mapv (fn [run] (mapv (fn [i] [(nth xs i) (nth ys i)]) run)) (finite-runs xs ys)))

(defn dash-pattern
  "Gives the dash lengths of a validated dash.

  Parameters:

  - `dash`: `nil`, `:none`, `:solid`, a name of [[cljplot2.schema/dash-styles]] or a sequence of positive lengths.

  Returns a vector of doubles; `nil` for a solid line (`nil`, `:none`, `:solid`)."
  [dash]
  (let [lengths (get schema/dash-styles dash dash)]
    (when (and (some? lengths) (not (theme/none? lengths)))
      (mapv double lengths))))

(defn set-style!
  "Sets the canvas color and stroke of a line style (butt caps, round joins).

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `style` (map): `:color`, `:width`, `:dash` (see the namespace doc).

  Returns true when the style draws; `nil` for a `:none` colour or width 0 (nothing set)."
  [canvas {:keys [color width dash]}]
  (when (and (not (theme/none? color)) (m/pos? (double width)))
    (let [pattern (dash-pattern dash)]
      (c2d/set-color canvas color)
      (c2d/set-stroke-custom canvas (cond-> {:size (double width) :cap :butt :join :round}
                                      pattern (assoc :dash pattern)))
      true)))

(defn stroke-runs!
  "Strokes runs of pixel points with a line style.

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `point-runs` (sequence of `[x y]` sequences): the runs; a run of fewer than 2 points draws nothing.
  - `style` (map): the line style (see [[set-style!]]).

  Returns `nil`."
  [canvas point-runs style]
  (when (set-style! canvas style)
    (doseq [run point-runs :when (m/> (count run) 1)]
      (c2d/path canvas run false true))))

(defn draw!
  "Strokes the finite runs of pixel coordinates with a line style.

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `xs`, `ys` (indexed sequences of numbers): the coordinates (see [[runs]]).
  - `style` (map): the line style.

  Returns `nil`; a single-point run draws nothing."
  [canvas xs ys style]
  (stroke-runs! canvas (runs xs ys) style))

(defn bridge-style
  "Gives the line style of a bridge over a gap of a line.

  Parameters:

  - `style` (map): the line's style.
  - `bridge` (map): `:alpha` (a fraction multiplying the line colour's alpha), optional `:width`, `:dash` (over the line's).

  Returns the bridge's line style; a `:none` colour stays."
  [style {:keys [^double alpha] :as bridge}]
  (let [color (:color style)]
    (cond-> (merge style (select-keys bridge [:width :dash]))
      (not (theme/none? color)) (assoc :color (draw/faded color alpha)))))

(def bridge-schema
  "Theme `:bridge` of a line style: `:alpha` (fraction of the line alpha), optional `:width`, `:dash`."
  [:map {:closed true}
   [:alpha {:description "bridge opacity, a fraction of the line's alpha"
            :cljplot2/default-doc "0.3 in the theme's :bridge (a given :bridge replaces it whole)"}
    schema/fraction]
   (schema/optional :width {:description "bridge width in pixels" :cljplot2/default-doc "the line's width"} schema/non-neg)
   (schema/nilable :dash {:description "dash style of the bridge" :cljplot2/default-doc "the line's dash"} schema/dash)])

(defn path-length
  "Gives the length of a polyline in pixels.

  Parameters:

  - `points` (sequence of `[x y]`): the polyline.

  Returns the summed segment lengths; 0.0 for fewer than 2 points."
  ^double [points]
  (reduce m/+ 0.0 (map (fn [[^double x0 ^double y0] [^double x1 ^double y1]]
                         (m/hypot (m/- x1 x0) (m/- y1 y0)))
                       points (rest points))))

(defn draw-segments!
  "Strokes the segments between consecutive finite points, each in its own colour.

  Consecutive segments of one colour form one path (round joins, round caps at its ends, so colour changes show no gaps); the dash runs on along each run of finite points, across colour changes and skipped segments.

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `xs`, `ys` (indexed sequences of numbers): pixel coordinates; segment `i` runs from point `i` to `i + 1`.
  - `segment-colors` (fn of `i`): the colour of segment `i`; `nil` or `:none` skips it.
  - `style` (map): `:width` (0 draws nothing), `:dash`.

  Returns `nil`."
  [canvas xs ys segment-colors {:keys [width dash]}]
  (when (m/pos? (double width))
    (let [pattern (dash-pattern dash)
          stroke (cond-> {:size (double width) :cap :round :join :round} pattern (assoc :dash pattern))]
      (doseq [run (finite-runs xs ys)]
        (reduce (fn [^double phase segments]
                  (let [color (segment-colors (ffirst segments))
                        ;; drawn by c2d/path and measured by path-length
                        points (mapv (fn [i] [(double (nth xs i)) (double (nth ys i))])
                                     (cons (ffirst segments) (map second segments)))]
                    (when (and (some? color) (not (theme/none? color)))
                      (c2d/set-stroke-custom canvas (cond-> stroke pattern (assoc :dash-phase phase)))
                      (c2d/set-color canvas color)
                      (c2d/path canvas points false true))
                    (m/+ phase (path-length points))))
                0.0
                (partition-by (fn [[i _]] (segment-colors i)) (partition 2 1 run)))))))

(defn- signed-area
  "Gives the shoelace area of a closed polygon; its sign is the winding direction.

  Parameters:

  - `points` (sequence of `[x y]`): the polygon's corners.

  Returns the signed area; 0.0 for no points."
  ^double [points]
  (m/* 0.5 (double (reduce m/+ 0.0 (map (fn [[^double x0 ^double y0] [^double x1 ^double y1]] (m/- (m/* x0 y1) (m/* x1 y0)))
                                points (concat (rest points) [(first points)]))))))

(def ^{:private true :tag 'double} disc-winding
  "Winding direction (sign of [[signed-area]]) of a java.awt ellipse's path, flattened."
  (m/signum (signed-area (c2d/shape->points (sh/ellipse 1.0 1.0 2.0 2.0) 0.01))))

(defn- hull-points
  "Gives the corners of the quadrilateral between two discs bounded by their outer tangents.

  Parameters:

  - `a`, `b` (`[x y r]`): the discs (numbers).

  Returns the 4 corners (the discs themselves not included), wound as [[disc-winding]]; `nil` when one disc holds the other."
  [[^double ax ^double ay ^double ar] [^double bx ^double by ^double br]]
  (let [dx (m/- bx ax) dy (m/- by ay)
        d (m/hypot dx dy)]
    (when (m/> d (m/abs (m/- ar br)))
      (let [ux (m// dx d) uy (m// dy d)
            cos-t (m// (m/- ar br) d)
            sin-t (m/sqrt (m/- 1.0 (m/* cos-t cos-t)))
            ;; unit normals of the two tangent lines
            px (m/- (m/* cos-t ux) (m/* sin-t uy)) py (m/+ (m/* cos-t uy) (m/* sin-t ux))
            qx (m/+ (m/* cos-t ux) (m/* sin-t uy)) qy (m/- (m/* cos-t uy) (m/* sin-t ux))
            pts [[(m/+ ax (m/* ar px)) (m/+ ay (m/* ar py))] [(m/+ bx (m/* br px)) (m/+ by (m/* br py))]
                 [(m/+ bx (m/* br qx)) (m/+ by (m/* br qy))] [(m/+ ax (m/* ar qx)) (m/+ ay (m/* ar qy))]]]
        (if (m/== (m/signum (signed-area pts)) disc-winding) pts (rseq pts))))))

(defn- ring-path-def
  "Gives the path definition entries of a closed ring: a move to its first point, lines to the rest, a close.

  Parameters:

  - `ring` (sequence of `[x y]`): the points, at least 1.
  - `winding`: the path's winding rule on every entry (`:even-odd`, `:non-zero`) or `nil` (the default non-zero).

  Returns a sequence of `c2d/path-def->shape` entries."
  [[start & more] winding]
  (concat [[:move start winding]] (map (fn [p] [:line p winding]) more) [[:close nil winding]]))

(defn trail-shape
  "Gives the shape of a trail through pixel points with a width at each.

  The width changes linearly between points, with round ends and joins - a disc at every point and the hull between consecutive discs, all wound one way in one non-zero path (filled once: no seams, translucent colours even).

  Parameters:

  - `xs`, `ys` (sequences of numbers): the points in pixels.
  - `widths` (sequence of numbers): the width at each point in pixels.

  Returns the shape: one point - its disc; zero widths - an empty shape."
  ^Shape [xs ys widths]
  ;; discs are paired by partition and walked again
  (let [discs (mapv (fn [x y w] [(double x) (double y) (m/* 0.5 (double w))]) xs ys widths)]
    (c2d/path-def->shape
     (concat (for [[^double x ^double y ^double r] discs :when (m/pos? r)]
               [:shape [(sh/ellipse x y (m/* 2.0 r) (m/* 2.0 r)) false]])
             (for [[a b] (partition 2 1 discs)
                   :let [pts (hull-points a b)]
                   :when pts
                   entry (ring-path-def pts nil)]
               entry)))))

(defn key-glyph!
  "Draws the legend key glyph of a line.

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `box` (map): `{:x :y :w :h}` in pixels.
  - `style` (map): the line style.
  - `area`: the area colour; `:none` - a horizontal line through the middle in `style`, else a filled square ([[cljplot2.graphics.draw/key-glyph-share]] of the smaller side) outlined in `style`.
  - `pattern-fill`: the square's pattern (see [[cljplot2.graphics.draw/pattern-fill]]); `nil` (default) - none.

  Returns `nil`."
  ([canvas box style area] (key-glyph! canvas box style area nil))
  ([canvas {:keys [^double x ^double y ^double w ^double h]} style area pattern-fill]
   (let [cx (m/+ x (m/* 0.5 w))
         cy (m/+ y (m/* 0.5 h))]
     (if (theme/none? area)
       (draw! canvas [x (m/+ x w)] [cy cy] style)
       (let [side (m/* draw/key-glyph-share (m/min w h))]
         (c2d/set-color canvas area)
         (c2d/crect canvas cx cy side side)
         (draw/fill-with-pattern! canvas pattern-fill #(c2d/crect % cx cy side side))
         (when (set-style! canvas style)
           (c2d/crect canvas cx cy side side true)))))))

(defn rings-path
  "Gives the even-odd path of pixel rings: a region with its holes, filled by `c2d/shape`.

  Parameters:

  - `rings` (sequence of `[[x y] ..]`): the rings; rings of fewer than 3 points left out.

  Returns a `Path2D` (empty for no ring)."
  ^Path2D [rings]
  (c2d/path-def->shape (mapcat #(ring-path-def % :even-odd) (filter #(m/> (count %) 2) rings))))

(defn fill-polygons!
  "Fills polygons in a colour, then a pattern over them.

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `polygons` (sequence of `[x y]` sequences): the polygons in pixels.
  - `color`: the fill; `:none` fills nothing.
  - `pattern-fill`: the pattern (see [[cljplot2.graphics.draw/pattern-fill]]); `nil` - none.

  Returns `nil`."
  [canvas polygons color pattern-fill]
  (when-not (theme/none? color)
    ;; filled twice (colour, pattern)
    (let [polygon-vec (vec polygons)
          fill! (fn [c] (doseq [polygon polygon-vec] (c2d/path c polygon true false)))]
      (c2d/set-color canvas color)
      (fill! canvas)
      (draw/fill-with-pattern! canvas pattern-fill fill!))))

(defn fill-under!
  "Fills the area between each finite run of a curve and its base points.

  The polygon runs from the base point of the run's first point through the run to the base point of its last one.

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `xs`, `ys` (indexed sequences of numbers): the curve in pixels.
  - `bxs`, `bys` (indexed sequences of numbers): the base point of every point, e.g. the projection of `(x, 0)` (a horizontal baseline for unflipped charts, vertical for flipped).
  - `color`: the fill; `:none` fills nothing.
  - `pattern-fill`: the pattern over the fill (see [[fill-polygons!]]); `nil` (default) - none.

  Returns `nil`."
  ([canvas xs ys bxs bys color] (fill-under! canvas xs ys bxs bys color nil))
  ([canvas xs ys bxs bys color pattern-fill]
   (fill-polygons! canvas (for [run (finite-runs xs ys) :when (m/> (count run) 1)
                                :let [i0 (first run) i1 (peek run)]]
                            (-> [[(nth bxs i0) (nth bys i0)]]
                                (into (map (fn [i] [(nth xs i) (nth ys i)])) run)
                                (conj [(nth bxs i1) (nth bys i1)])))
                   color pattern-fill)))

(defn fill-runs-to-baseline!
  "Fills the area between each run of pixel points and a baseline.

  The polygon runs from the base point of the run's first point through the run to the base point of its last.

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `point-runs` (sequence of `[x y]` vectors): the runs; a run of fewer than 2 points fills nothing.
  - `baseline` (number): the baseline's pixel position - a y position when `horizontal?` (unflipped charts), else an x position.
  - `horizontal?`: truthy for a horizontal baseline.
  - `color`: the fill; `:none` fills nothing.
  - `pattern-fill`: the pattern over the fill (see [[fill-polygons!]]); `nil` (default) - none.

  Returns `nil`."
  ([canvas point-runs baseline horizontal? color]
   (fill-runs-to-baseline! canvas point-runs baseline horizontal? color nil))
  ([canvas point-runs baseline horizontal? color pattern-fill]
   (let [base-px (double baseline)
         base (if horizontal? (fn [[x _]] [x base-px]) (fn [[_ y]] [base-px y]))]
     (fill-polygons! canvas (for [run point-runs :when (m/> (count run) 1)]
                              (-> [(base (first run))] (into run) (conj (base (peek run)))))
                     color pattern-fill))))

(defn fill-closed-runs!
  "Fills every run of pixel points as the polygon it closes.

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `point-runs` (sequence of `[x y]` sequences): the runs; a run of fewer than 3 points fills nothing.
  - `color`: the fill; `:none` fills nothing.
  - `pattern-fill`: the pattern over the fill (see [[fill-polygons!]]); `nil` (default) - none.

  Returns `nil`."
  ([canvas point-runs color] (fill-closed-runs! canvas point-runs color nil))
  ([canvas point-runs color pattern-fill]
   (fill-polygons! canvas (filter #(m/> (count %) 2) point-runs) color pattern-fill)))

(defn fill-between!
  "Fills the area between an upper and a lower pixel curve, point `i` of one paired with point `i` of the other.

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `xs`, `ys` (indexed sequences of numbers): the upper curve.
  - `bxs`, `bys` (indexed sequences of numbers): the lower curve.
  - `color`: the fill; `:none` fills nothing.
  - `pattern-fill`: the pattern over the fill (see [[fill-polygons!]]); `nil` (default) - none.

  Returns `nil`; one polygon per run of indices where both points are finite (runs of fewer than 2 fill nothing)."
  ([canvas xs ys bxs bys color] (fill-between! canvas xs ys bxs bys color nil))
  ([canvas xs ys bxs bys color pattern-fill]
   ;; masked coordinates are indexed by finite-runs
   (fill-polygons! canvas (for [run (finite-runs (mapv (fn [a b] (if (m/valid-double? (double b)) a ##NaN)) xs bxs)
                                                 (mapv (fn [a b] (if (m/valid-double? (double b)) a ##NaN)) ys bys))
                                :when (m/> (count run) 1)]
                            (-> []
                                (into (map (fn [i] [(nth xs i) (nth ys i)])) run)
                                (into (map (fn [i] [(nth bxs i) (nth bys i)])) (rseq run))))
                   color pattern-fill)))

;; areas

(defn area-color
  "Gives the fill of a group's area.

  Parameters:

  - `st` (map): the line style with the group's attributes merged - `:color`, `:alpha` (0..1), `:fill`.
  - `stacked?`: truthy for stacked areas.

  Returns the group colour with opacity `:alpha` for `:fill :color` or for stacked areas without a fill, else `:fill` (`:none` - no area)."
  [st stacked?]
  (if (or (= :color (:fill st)) (and stacked? (theme/none? (:fill st :none))))
    (draw/with-alpha (:color st) (:alpha st))
    (:fill st :none)))

(def fill-gradient-schema
  "Schema of `:fill-gradient`: a clojure2d gradient - a preset keyword, a colour list or a function of `t`."
  (schema/named-type ::fill-gradient #(or (keyword? %) (sequential? %) (fn? %))
                     "should be a clojure2d gradient: a preset keyword, a colour list or a function"))

(defn gradient-fn
  "Gives the clojure2d gradient of a `:fill-gradient` option.

  Parameters:

  - `g`: a preset keyword, a colour list or a function of `t` (taken as is).

  Returns a function of `t` in [0, 1]; an unknown gradient throws `ex-info {:key :fill-gradient}`."
  [g]
  (if (fn? g)
    g
    (try (c/gradient g)
         (catch Exception e
           (schema/fail! "Unknown :fill-gradient." {:key :fill-gradient :value g :cause (ex-message e)})))))

(def ^{:private true :const true :tag 'long} gradient-stops
  "Stops of a linear gradient paint sampled from a gradient function."
  32)

(def ^{:private true :const true :tag 'double} last-gradient-stop
  "Index of the last gradient stop, as a double divisor."
  (double (m/long-dec gradient-stops)))

(defn fill-gradient-paint!
  "Fills polygons with a linear gradient paint between two pixel points.

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `polygons` (sequence of `[x y]` sequences): the polygons; fewer than 3 points fill nothing.
  - `gradient` (fn of `t`): the gradient, sampled at [[gradient-stops]] stops.
  - `[fx fy]`, `[tx ty]`: the pixel points of `t` 0 and 1.

  Returns `nil`; the paint is the gradient's last colour afterwards (callers set their colour again). Equal points fill nothing."
  [canvas polygons gradient [^double fx ^double fy] [^double tx ^double ty]]
  (when-not (and (m/== fx tx) (m/== fy ty))
    ;; float stops, the gradient sampled at each (as the paint stores them); consumed twice
    (let [fractions (mapv (fn [^long i] (float (m// (double i) last-gradient-stop))) (range gradient-stops))]
      (c2d/linear-gradient-mode canvas fx fy tx ty fractions (map gradient fractions))
      (doseq [polygon polygons :when (m/> (count polygon) 2)]
        (c2d/path canvas polygon true false))
      (c2d/set-color canvas (gradient 1.0)))))

(defn- gradient-axis-points
  "Gives the pixel ends of a position scale's range along its panel direction.

  Parameters:

  - `scales` (map): the panel's finalized scales.
  - `axis` (keyword): `:x` or `:y`.

  Returns `[from to]`, each `[x y]` (the other coordinate 0)."
  [scales axis]
  (let [sc (get scales axis)
        [^double r0 ^double r1] (:range sc)]
    (if (scale/horizontal? axis sc) [[r0 0.0] [r1 0.0]] [[0.0 r0] [0.0 r1]])))

(defn fill-area!
  "Fills the area between paired point runs, in a gradient or a colour, then a pattern.

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `upper`, `lower` (sequences of `[x y]`): the pixel curves, point `i` of one paired with point `i` of the other.
  - `color`: the fill without a gradient; `:none` fills nothing.
  - `gradient` (fn of `t` or `nil`): a linear gradient paint over the range of the `axis` scale.
  - `axis` (keyword): `:x` or `:y`.
  - `scales` (map): the panel's finalized scales.
  - `pattern-fill`: the pattern over the fill (see [[fill-polygons!]]; over a gradient its ink is `:pattern-color` against the gradient's middle colour); `nil` (default) - none.

  Returns `nil`."
  ([canvas upper lower color gradient axis scales] (fill-area! canvas upper lower color gradient axis scales nil))
  ([canvas upper lower color gradient axis scales pattern-fill]
   (if gradient
     (let [[from to] (gradient-axis-points scales axis)
           ;; filled twice (gradient, pattern)
           polygon (into (vec upper) (rseq (vec lower)))]
       (fill-gradient-paint! canvas [polygon] gradient from to)
       (when (m/> (count polygon) 2)
         (draw/fill-with-pattern! canvas pattern-fill #(c2d/path % polygon true false))))
     ;; coordinates indexed by fill-between!
     (fill-between! canvas (mapv first upper) (mapv second upper) (mapv first lower) (mapv second lower) color
                    pattern-fill))))
