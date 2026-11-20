(ns cljplot2.graphics.draw
  "Drawing grid item content - the counterpart of [[cljplot2.graphics.measure/natural-size]]."
  (:require [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.graphics.pattern :as pattern]
            [cljplot2.scale.coord :as coord]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]
           [java.awt Graphics2D Shape TexturePaint]
           [java.awt.geom Path2D$Double]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def stroke-color
  "Schema of a filled mark's `:stroke-color`: a color, `:none` (no outline) or `:darker` (the darkened fill)."
  [:or [:= :darker] schema/color])

(def pattern-style-entries
  "Style map entries of a filled chart's pattern: `:pattern` (see [[cljplot2.schema/pattern]]) and `:pattern-color`
  (the ink: a color or `:darker`, see [[pattern-ink]])."
  (schema/in-fragment
   :pattern
   [(schema/optional :pattern {:description "pattern drawn over the fill (a mapped :pattern wins)"} schema/pattern)
    (schema/optional :pattern-color {:description "pattern ink: a colour, :none or :darker (the darkened fill)"} stroke-color)]))

(defn contrast-color
  "Black or white, whichever reads better on `color` (by luma): text and lines drawn over a fill."
  [color]
  (if (> 127.5 (double (c/luma color))) :white :black))

(defn painted-color
  "Color already painted on `canvas` at user-space point `(x, y)` (through the canvas transform, e.g. a panel's
  translation); `nil` outside the image."
  [canvas ^double x ^double y]
  (let [^Graphics2D g (:graphics canvas)
        ^java.awt.image.BufferedImage img (c2d/get-image canvas)
        p (.transform (.getTransform g) (java.awt.geom.Point2D$Double. x y) nil)
        px (long (m/floor (.getX p))) py (long (m/floor (.getY p)))]
    (when (and (< -1 px (.getWidth img)) (< -1 py (.getHeight img)))
      (c/to-color (.getRGB img px py)))))

(defn outline-color
  "Outline color of a mark filled with `fill` for `:stroke-color` `stroke` (see [[stroke-color]]); `:none` stays `:none`."
  [stroke fill]
  (if (= :darker stroke) (c/darken fill) stroke))

(defn pattern-ink
  "Ink of a fill pattern over `fill` for `:pattern-color` `ink` (a color or `:darker` - the darkened fill), no more
  opaque than the fill: a translucent fill (e.g. overlaid groups) gets translucent ink."
  [ink fill]
  (let [ink (outline-color ink fill)
        alpha (m/min (c/alpha ink) (c/alpha fill))]
    (if (< alpha (c/alpha ink)) (c/set-alpha ink alpha) ink)))

(defn pattern-fill
  "Pattern fill `{:pattern p :ink c :paint texture}` of a mark filled with `fill` in style `st` (`:pattern`,
  `:pattern-color` - see [[pattern-ink]]; `:paint` - the tile's `TexturePaint`, anchored at the user origin); `nil`
  without a pattern (`:solid`, `nil`), an ink (`:pattern-color :none`) or a fill (`:none`, `nil`). Many marks of one
  style share it: build it once per group, or use [[pattern-fill-cache]]."
  [st fill]
  (let [p (:pattern st)
        ink (:pattern-color st :darker)]
    (when-let [spec (pattern/spec-of p)]
      (when (and (some? fill) (not (theme/none? fill)) (not (theme/none? ink)))
        (let [ink (pattern-ink ink fill)
              period (double (pattern/period spec))]
          {:pattern p :ink ink
           :paint (TexturePaint. (pattern/tile spec ink) (java.awt.geom.Rectangle2D$Double. 0.0 0.0 period period))})))))

(defn pattern-fill-cache
  "A function of `st` and `fill` giving [[pattern-fill]], built once per pattern, ink and fill - for one draw call
  over many marks (bars, boxes, rects)."
  []
  (let [cached (memoize (fn [p ink fill] (pattern-fill {:pattern p :pattern-color ink} fill)))]
    (fn [st fill] (cached (:pattern st) (:pattern-color st :darker) fill))))

(defn fill-with-pattern!
  "Draw the pattern of `pattern-fill` (see [[pattern-fill]]; `nil` - nothing) over a fill: `draw-fn` (no arguments)
  fills the same shapes again while the canvas paints the pattern's tile; the previous paint is restored. `draw-fn`
  must not set a colour."
  [canvas pattern-fill draw-fn]
  (when-let [^TexturePaint texture (:paint pattern-fill)]
    (let [^Graphics2D g (:graphics canvas)
          paint (.getPaint g)]
      (.setPaint g texture)
      (try (draw-fn) (finally (.setPaint g paint))))))

(declare pixel-center)

(defn zero-line!
  "The line at 0 of value scale `k` (default `:y`) across the panel `box` (`{:w :h}`; flip-aware) in style
  `:zero-line-color` / `:zero-line-width` (mirror densities, histograms and bars); nothing when 0 isn't in the scale
  or the line draws nothing."
  ([canvas box scales style] (zero-line! canvas box scales style :y))
  ([canvas {:keys [w h]} scales {:keys [zero-line-color zero-line-width]} k]
   (let [p (if (k scales) (scale/forward (k scales) 0.0) ##NaN)
         width (double zero-line-width)]
     (when (and (not (m/nan? p)) (not (theme/none? zero-line-color)) (pos? width))
       (let [p (pixel-center p)]
         (c2d/set-color canvas zero-line-color)
         (c2d/set-stroke-custom canvas {:size width :cap :butt})
         (if (scale/horizontal? k (k scales))
           (c2d/line canvas p 0 p h)
           (c2d/line canvas 0 p w p)))))))

(defn pixel-center
  "Center of the pixel containing coordinate `p`; thin lines drawn there cover whole pixels (no antialiasing smear)."
  ^double [^double p]
  (+ (m/floor p) 0.5))

(defmulti draw-content
  "Draw item `content` onto `canvas` (in graphics context) inside `box` (`{:x :y :w :h}`),
  dispatched like [[cljplot2.graphics.measure/natural-size]].

  `env` keys: `:theme` - the plot theme, `:scales` - finalized scales (see [[cljplot2.scale.position/finalize]]).

  A method may change any graphics state (clip, transform, font, color, ...) without restoring it;
  callers isolate each call (see [[cljplot2.graphics.canvas/call-with-isolated-graphics]]). To restrict drawing
  to `box`, intersect the clip (`Graphics2D.clip`) rather than replacing it, so an outer clip still applies.

  Content kinds without a method draw nothing."
  (fn [content _canvas _box _env] (measure/content-kind content)))

(defmethod draw-content :default [_ _ _ _] nil)

(defn inner-border!
  "Draw a border of `width` (rounded, at most half the box) and `color` inside box `{:x :y :w :h}` as four filled
  bands - exact pixels, nothing outside the box. `:none` color or width 0 draws nothing."
  [canvas {:keys [x y w h]} color width]
  (let [x (long x) y (long y) w (long w) h (long h)
        bw (m/min (m/round (double width)) (quot w 2) (quot h 2))]
    (when (and (not= :none color) (pos? bw))
      (c2d/set-color canvas color)
      (c2d/rect canvas x y w bw)
      (c2d/rect canvas x (- (+ y h) bw) w bw)
      (c2d/rect canvas x y bw h)
      (c2d/rect canvas (- (+ x w) bw) y bw h))))

(defn rect-of
  "Pixel rectangle `{:x :y :w :h}` (whole pixels) spanned by the data corners `[x0 y0]` and `[x1 y1]` placed with
  `projection` (see [[cljplot2.scale.position/projection]]): both corners rounded, then normalized (non-negative
  width and height), so it works for flipped scales too."
  [projection x0 y0 x1 y1]
  (let [^Vec2 a (projection x0 y0)
        ^Vec2 b (projection x1 y1)
        ax (m/round (.x a)) ay (m/round (.y a))
        bx (m/round (.x b)) by (m/round (.y b))]
    {:x (m/min ax bx) :y (m/min ay by) :w (m/abs (- bx ax)) :h (m/abs (- by ay))}))

(def ^:private ^:const min-bar-pixels
  "Bars thinner than this (pixels, along either side) aren't drawn: sub-pixel slivers only smear the baseline."
  0.5)

(defn visible-bar?
  "Is a bar `w` x `h` pixels drawn (both sides at least half a pixel)?"
  [^double w ^double h]
  (and (>= w min-bar-pixels) (>= h min-bar-pixels)))

(def corner-radius-option
  "Option entry `:corner-radius`: pixels, every corner of a bar rounded (default 0 - square)."
  (first (schema/in-fragment
          :corner-radius
          [(schema/optional :corner-radius {:description "radius in pixels of every bar corner" :default 0} schema/non-neg)])))

(def corner-radius-end-option
  "Option entry `:corner-radius-end`: pixels, only the corners at a bar's value end rounded (Vega-Lite
  `cornerRadiusEnd`); wins over `:corner-radius`."
  (first (schema/in-fragment
          :corner-radius
          [(schema/optional :corner-radius-end {:description "radius in pixels of the corners at the bar's value end only (wins over :corner-radius)"}
                            schema/non-neg)])))

(defn corner-rounding
  "Corner rounding `{:radius r :end? e}` of a bar chart's `options` (see [[corner-radius-option]],
  [[corner-radius-end-option]]): `:corner-radius-end` - only the value-end corners - wins over `:corner-radius`;
  `nil` for square corners (both absent or 0). A drawn bar adds `:axis` (see [[value-axis]]) for end rounding."
  [options]
  (let [end (:corner-radius-end options)
        r (double (or end (:corner-radius options) 0.0))]
    (when (pos? r) {:radius r :end? (some? end)})))

(defn value-axis
  "Range axis (`:h` or `:v`) a bar's value runs along: the axis where range points `base` (at the baseline) and
  `end` (at the value end, the same bar edge) differ (`[h v]` or `Vec2`)."
  [[bh _] [eh _]]
  (if (== (double bh) (double eh)) :v :h))

(defn value-ends
  "`[base end]` of a bar spanning values `y0..y1` (any order): the value end is the one farther from 0 - stacked
  segments `[below above]`, negative ones and a mirror's downward bars `[far near]` (see
  [[cljplot2.data.position/stack-values]])."
  [y0 y1]
  (if (> (m/abs (double y0)) (m/abs (double y1))) [y1 y0] [y0 y1]))

(defn bar-rounding
  "`rounding` (see [[corner-rounding]], may be `nil`) of one bar with its `:axis` (see [[value-axis]] of `base` and
  `end`)."
  [rounding base end]
  (some-> rounding (assoc :axis (value-axis base end))))

(defn whole-pixel
  "`p` (`Vec2`) with both coordinates rounded to whole pixels."
  ^Vec2 [^Vec2 p]
  (Vec2. (m/round (.x p)) (m/round (.y p))))

(defn- rounded-corners
  "Rounded corners of the range rectangle `[h0 v0]`-`[h1 v1]` (the second corner at the value end) under `rounding`
  (see [[corner-rounding]]): a set of `[i j]` - the corner at `hi` and `vj` (0 or 1); every corner, or with `:end?`
  the two at the value end along `:axis`."
  [{:keys [end? axis]}]
  (cond
    (not end?) #{[0 0] [1 0] [1 1] [0 1]}
    (= :h axis) #{[1 0] [1 1]}
    :else #{[0 1] [1 1]}))

(def ^:private ^:const kappa
  "Control point distance of a cubic Bezier quarter circle, as a fraction of the radius."
  0.5522847498307936)

(defn rounded-rect
  "Shape of the pixel rectangle `{:x :y :w :h}` with corner radii `[top-left top-right bottom-right bottom-left]`
  (pixels, each clamped to half the shorter side; 0 - square), the corners quarter circles."
  ^Shape [{:keys [x y w h]} radii]
  (let [x (double x) y (double y) w (double w) h (double h)
        limit (* 0.5 (m/min w h))
        [tl tr br bl] (mapv #(m/min limit (double %)) radii)
        tl (double tl) tr (double tr) br (double br) bl (double bl)
        x1 (+ x w) y1 (+ y h)
        path (Path2D$Double.)]
    (.moveTo path (+ x tl) y)
    (.lineTo path (- x1 tr) y)
    (.curveTo path (+ (- x1 tr) (* kappa tr)) y x1 (- (+ y tr) (* kappa tr)) x1 (+ y tr))
    (.lineTo path x1 (- y1 br))
    (.curveTo path x1 (+ (- y1 br) (* kappa br)) (+ (- x1 br) (* kappa br)) y1 (- x1 br) y1)
    (.lineTo path (+ x bl) y1)
    (.curveTo path (- (+ x bl) (* kappa bl)) y1 x (+ (- y1 bl) (* kappa bl)) x (- y1 bl))
    (.lineTo path x (+ y tl))
    (.curveTo path x (- (+ y tl) (* kappa tl)) (- (+ x tl) (* kappa tl)) y (+ x tl) y)
    (.closePath path)
    path))

(defn- pixel-rect-radii
  "Corner radii `[top-left top-right bottom-right bottom-left]` of the pixel rectangle spanned by `a` and `b` (`Vec2`,
  `b` at the value end) rounded with `rounding` (see [[rounded-corners]])."
  [^Vec2 a ^Vec2 b rounding]
  (let [corners (rounded-corners rounding)
        r (double (:radius rounding))
        left (if (<= (.x a) (.x b)) 0 1) top (if (<= (.y a) (.y b)) 0 1)
        radius (fn [i j] (if (corners [i j]) r 0.0))]
    [(radius left top) (radius (- 1 left) top) (radius (- 1 left) (- 1 top)) (radius left (- 1 top))]))

(defn shape-bar!
  "Fill the java.awt `shape` with `fill` and the pattern of `pattern-fill` (see [[pattern-fill]], `nil` - none),
  outline it with `stroke` (a color or `:none`, `stroke-width` pixels, mitred) when `stroke-width` is positive."
  [canvas ^Shape shape fill stroke stroke-width pattern-fill]
  (let [stroke-width (double stroke-width)]
    (c2d/set-color canvas fill)
    (c2d/shape canvas shape false)
    (fill-with-pattern! canvas pattern-fill #(c2d/shape canvas shape false))
    (when (and (pos? stroke-width) (not (theme/none? stroke)))
      (c2d/set-stroke-custom canvas {:size stroke-width :join :miter})
      (c2d/set-color canvas stroke)
      (c2d/shape canvas shape true))))

(defn bar!
  "Fill the rectangle with pixel corners `a` and `b` (`Vec2`, any order) with `fill` and the pattern of
  `pattern-fill` (see [[pattern-fill]], `nil` - none), outline it with `stroke` (a color or `:none`, `stroke-width`
  pixels, mitred) when `stroke-width` is positive; nothing for a bar that isn't [[visible-bar?]]. `rounding` (see
  [[corner-rounding]], `nil` - square) rounds its corners; end rounding needs `b` at the value end."
  ([canvas a b fill stroke stroke-width] (bar! canvas a b fill stroke stroke-width nil nil))
  ([canvas a b fill stroke stroke-width pattern-fill] (bar! canvas a b fill stroke stroke-width pattern-fill nil))
  ([canvas ^Vec2 a ^Vec2 b fill stroke stroke-width pattern-fill rounding]
   (let [stroke-width (double stroke-width)
         x (m/min (.x a) (.x b)) y (m/min (.y a) (.y b))
         w (m/abs (- (.x b) (.x a))) h (m/abs (- (.y b) (.y a)))]
     (when (visible-bar? w h)
       (if rounding
         (shape-bar! canvas (rounded-rect {:x x :y y :w w :h h} (pixel-rect-radii a b rounding))
                     fill stroke stroke-width pattern-fill)
         (do (c2d/set-color canvas fill)
             (c2d/rect canvas x y w h)
             (fill-with-pattern! canvas pattern-fill #(c2d/rect canvas x y w h))
             (when (and (pos? stroke-width) (not (theme/none? stroke)))
               (c2d/set-stroke-custom canvas {:size stroke-width :join :miter})
               (c2d/set-color canvas stroke)
               (c2d/rect canvas x y w h true))))))))

(def ^:private ^:const corner-step-angle
  "Largest angle (radians) between points of a rounded corner's arc."
  (m/radians 10.0))

(defn- corner-arc
  "Pixel points of the arc of radius `r` around pixel `c` from pixel `p` to pixel `q` (both `r` from `c`), the
  shorter way, densified."
  [^Vec2 c ^double r ^Vec2 p ^Vec2 q]
  (let [a0 (m/atan2 (- (.y p) (.y c)) (- (.x p) (.x c)))
        a1 (m/atan2 (- (.y q) (.y c)) (- (.x q) (.x c)))
        d (- (double (mod (+ (- a1 a0) m/PI) m/TWO_PI)) m/PI)
        step (if (pos? r) (m/min corner-step-angle (/ 2.0 r)) corner-step-angle)
        n (m/max 1 (long (m/ceil (/ (m/abs d) step))))]
    (mapv (fn [^long i] (let [a (+ a0 (* d (/ (double i) n)))]
                          [(+ (.x c) (* r (m/cos a))) (+ (.y c) (* r (m/sin a)))]))
          (range (inc n)))))

(defn- sector-corner
  "Rounded corner of radius `r` where the radial edge at angle `a` (radians) meets the arc of radius `arc`; `inward` -
  the angular direction (+1 / -1) into the sector, `outer?` - the outer arc (the corner circle inside it), else the
  inner one (outside it). `{:delta d :edge points}`: `d` - the angle the arc gives up, `points` - the corner from the
  arc to the edge (pixels; one point - square)."
  [geometry {:keys [a inward arc outer? r]}]
  (let [a (double a) arc (double arc) r (double r) inward (double inward)]
    (if (pos? r)
      (let [c (if outer? (- arc r) (+ arc r))
            delta (m/asin (/ r c))
            at (+ a (* inward delta))
            tangent-arc (coord/to-pixel geometry at arc)
            tangent-edge (coord/to-pixel geometry a (m/sqrt (- (* c c) (* r r))))]
        {:delta delta :edge (corner-arc (coord/to-pixel geometry at c) r tangent-arc tangent-edge)})
      (let [^Vec2 p (coord/to-pixel geometry a arc)]
        {:delta 0.0 :edge [[(.x p) (.y p)]]}))))

(defn- sector-radius-limit
  "Largest corner radius of the annular sector spanning `span` radians between radii `lo` and `hi`: half the radial
  thickness, and the corners of one arc not overlapping (outer arc; inner arc when `lo` > 0)."
  ^double [^double span ^double lo ^double hi]
  (let [s (m/sin (m/min (* 0.5 span) m/HALF_PI))]
    (cond-> (m/min (* 0.5 (- hi lo)) (/ (* hi s) (+ 1.0 s)))
      (and (pos? lo) (< s 1.0)) (m/min (/ (* lo s) (- 1.0 s))))))

(defn rounded-sector-path
  "Closed polygon (pixel `[x y]` points) of the range rectangle `[h0 v0]`-`[h1 v1]` (the second corner at the value
  end) in polar `geometry`, rounded with `rounding` (see [[corner-rounding]]): an annular sector with circular corners,
  the radius clamped by [[sector-radius-limit]]; no inner corners at a zero inner radius (a wedge); a full ring has no
  corners ([[cljplot2.scale.coord/rect-path]])."
  [geometry [h0 v0] [h1 v1] rounding]
  (let [h0 (double h0) h1 (double h1) v0 (double v0) v1 (double v1)
        lo (m/min v0 v1) hi (m/max v0 v1)
        span (m/abs (- h1 h0))]
    (if (>= span (- m/TWO_PI 1.0e-9))
      (coord/rect-path geometry h0 v0 h1 v1)
      (let [corners (rounded-corners rounding)
            r (m/min (double (:radius rounding)) (sector-radius-limit span lo hi))
            sigma (if (<= h0 h1) 1.0 -1.0)
            outer-j (if (>= v1 v0) 1 0)
            corner (fn [i outer?]
                     (sector-corner geometry {:a (if (zero? (long i)) h0 h1) :inward (if (zero? (long i)) sigma (- sigma))
                                              :arc (if outer? hi lo) :outer? outer?
                                              :r (if (corners [i (if outer? outer-j (- 1 outer-j))]) r 0.0)}))
            o0 (corner 0 true) o1 (corner 1 true)
            outer (coord/line-path geometry [[(+ h0 (* sigma (double (:delta o0)))) hi]
                                             [(- h1 (* sigma (double (:delta o1)))) hi]] true)
            inner (if (pos? lo)
                    (let [i1 (corner 1 false) i0 (corner 0 false)]
                      (concat (rseq (:edge i1))
                              (coord/line-path geometry [[(- h1 (* sigma (double (:delta i1)))) lo]
                                                         [(+ h0 (* sigma (double (:delta i0)))) lo]] true)
                              (:edge i0)))
                    (coord/line-path geometry [[h1 0.0]] false))]
        (vec (concat outer (:edge o1) inner (rseq (:edge o0))))))))

(defn- polygon-shape
  "java.awt shape of the closed polygon `points` (pixel `[x y]`)."
  ^Shape [points]
  (let [path (Path2D$Double.)
        [[x y] & more] points]
    (.moveTo path (double x) (double y))
    (doseq [[x y] more] (.lineTo path (double x) (double y)))
    (.closePath path)
    path))

(defn range-bar!
  "[[bar!]] of the range rectangle with corners `[h0 v0]` and `[h1 v1]` (see [[cljplot2.scale.coord]]): in cartesian
  plots (`geometry` `nil`) the pixel rectangle itself; in polar ones (`geometry` - the panel's polar geometry) the
  annular sector, filled with `fill` and the pattern of `pattern-fill`, outlined with `stroke` when `stroke-width`
  is positive. `rounding` (see [[corner-rounding]], `nil` - square) rounds the corners (polar - see
  [[rounded-sector-path]]); end rounding needs the second corner at the value end."
  ([canvas geometry a b fill stroke stroke-width pattern-fill]
   (range-bar! canvas geometry a b fill stroke stroke-width pattern-fill nil))
  ([canvas geometry [h0 v0 :as a] [h1 v1 :as b] fill stroke stroke-width pattern-fill rounding]
   (if-not geometry
     (bar! canvas (Vec2. (double h0) (double v0)) (Vec2. (double h1) (double v1)) fill stroke stroke-width pattern-fill
           rounding)
     (let [polygon (if rounding
                     (rounded-sector-path geometry a b rounding)
                     (coord/rect-path geometry h0 v0 h1 v1))
           stroke-width (double stroke-width)]
       (when (every? (fn [[x y]] (and (m/valid-double? (double x)) (m/valid-double? (double y)))) polygon)
         (if rounding
           (shape-bar! canvas (polygon-shape polygon) fill stroke stroke-width pattern-fill)
           (do (c2d/set-color canvas fill)
               (c2d/path canvas polygon true false)
               (fill-with-pattern! canvas pattern-fill #(c2d/path canvas polygon true false))
               (when (and (pos? stroke-width) (not (theme/none? stroke)))
                 (c2d/set-stroke-custom canvas {:size stroke-width :join :miter})
                 (c2d/set-color canvas stroke)
                 (c2d/path canvas polygon true true)))))))))

(def ^:private ^:const key-radius-fraction
  "Largest corner radius of a legend key square, as a fraction of its side: the key reads as a rounded square whatever
  the bars' radius (pixels of a large bar would make a small key a circle)."
  0.25)

(defn key-rounding
  "`rounding` (see [[corner-rounding]], may be `nil`) of a legend key square of `side` pixels: the radius at most
  [[key-radius-fraction]] of the side, end rounding at its top (an upright bar)."
  [rounding side]
  (some-> rounding
          (assoc :axis :v)
          (update :radius #(m/min (double %) (* key-radius-fraction (double side))))))

(defn bar-key!
  "Legend key glyph of a bar chart (histogram, bar): a filled square inset in `box` with its pattern (see
  [[pattern-fill]]), outlined by `:stroke-color` (see [[outline-color]]) when `:stroke-width` is positive;
  `translucent?` - the fill gets `:alpha` (grouped overlay bars); `rounding` (see [[corner-rounding]], `nil` -
  square) rounds its corners as an upright bar's (see [[key-rounding]]). `st` - `{:color :stroke-color :stroke-width
  :alpha :pattern :pattern-color}`."
  ([canvas box st translucent?] (bar-key! canvas box st translucent? nil))
  ([canvas {:keys [x y w h]} st translucent? rounding]
   (let [side (* 0.7 (m/min (double w) (double h)))
         cx (+ (double x) (* 0.5 (double w)))
         cy (+ (double y) (* 0.5 (double h)))
         fill (if translucent? (c/set-alpha (:color st) (* 255.0 (double (:alpha st)))) (:color st))
         stroke (outline-color (:stroke-color st) (:color st))
         stroke-width (double (:stroke-width st))]
     (if rounding
       (let [half (* 0.5 side)]
         (bar! canvas (Vec2. (- cx half) (+ cy half)) (Vec2. (+ cx half) (- cy half)) fill stroke stroke-width
               (pattern-fill st fill) (key-rounding rounding side)))
       (do (c2d/set-color canvas fill)
           (c2d/crect canvas cx cy side side)
           (fill-with-pattern! canvas (pattern-fill st fill) #(c2d/crect canvas cx cy side side))
           (when (and (pos? stroke-width) (not (theme/none? stroke)))
             (c2d/set-stroke-custom canvas {:size stroke-width})
             (c2d/set-color canvas stroke)
             (c2d/crect canvas cx cy side side true)))))))

(defn cvd-filter-fn
  "Color function simulating the color vision deficiency `deficiency` - a clojure2d `cvd-filter` keyword
  (`:protanopia`, `:deuteranopia`, `:tritanopia`, `:achromatopsia`) or `[keyword severity]` (`:protanomaly`,
  `:deuteranomaly`, `:tritanomaly`, `:achromatomaly`; severity in `[0 1]`). Throws `ex-info` (clojure2d's) for an
  unknown deficiency or severity."
  [deficiency]
  (if (vector? deficiency) (apply c/cvd-filter deficiency) (c/cvd-filter deficiency)))
