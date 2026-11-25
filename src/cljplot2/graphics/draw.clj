(ns cljplot2.graphics.draw
  "Drawing grid item content - the counterpart of [[cljplot2.graphics.measure/natural-size]] - and the marks and
  colours charts share.

  Functions: content [[draw-content]], [[inner-border!]], [[zero-line!]], [[pixel-center]]; colours
  [[contrast-color]], [[plot-backdrop]], [[resolved-text-color]], [[text-color]], [[painted-color]],
  [[outline-color]], [[with-alpha]], [[faded]], [[translucent-fill]], [[visible-stroke?]], [[outline!]]; patterns [[pattern-ink]], [[pattern-fill]], [[pattern-fill-cache]],
  [[fill-with-pattern!]], [[fill-patterned!]]; bars [[rect-of]], [[visible-bar?]], [[corner-rounding]], [[value-axis]], [[value-ends]],
  [[bar-rounding]], [[whole-pixel]], [[bar!]], [[rounded-sector-path]], [[range-bar!]]; legend keys
  [[key-rounding]], [[key-glyph-share]], [[bar-key!]], [[circle-key!]], [[spike-key!]]; [[cvd-filter-fn]].

  Schemas: [[stroke-color]], [[pattern-style-entries]], [[corner-radius-option]], [[corner-radius-end-option]]."
  (:require [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [clojure2d.core.shape :as sh]
            [clojure2d.pixels :as p]
            [fastmath.core :as m]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.graphics.pattern :as pattern]
            [cljplot2.scale.coord :as coord]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]
           [java.awt Shape]))

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

(def ^:private white
  "Opaque white: the contrast colour on dark fills and the backdrop without a plot background."
  (c/gray 255))

(def ^:private black
  "Opaque black: the contrast colour on light fills."
  (c/gray 0))

(defn contrast-color
  "Gives black or white, whichever reads better on a colour: text and lines drawn over a fill.

  Parameters:

  - `color`: the colour under the text (any clojure2d representation).

  Returns opaque white when its WCAG contrast ratio with `color` is larger than black's, else opaque black (black on a tie); e.g. black on mid greens and oranges, where white reads worse."
  [color]
  (if (m/> (double (c/contrast-ratio white color)) (double (c/contrast-ratio black color))) white black))

(defn plot-backdrop
  "Gives what text outside any chart fill is drawn over.

  Parameters:

  - `theme` (map): the plot theme.

  Returns the theme's plot background; opaque white when it is `:none`."
  [theme]
  (let [background (get-in theme [:plot :background])]
    (if (theme/none? background) white background)))

(defn resolved-text-color
  "Gives a text colour as drawn over a backdrop.

  Parameters:

  - `color`: the text colour (see [[cljplot2.schema/text-color]]) - a colour or `:contrast`.
  - `backdrop`: the colour under the text.

  Returns `color` itself; for `:contrast` black or white by `backdrop` (see [[contrast-color]])."
  [color backdrop]
  (if (= :contrast color) (contrast-color backdrop) color))

(defn text-color
  "Gives the colour of text in a theme role drawn over a backdrop.

  Parameters:

  - `theme` (map): the plot theme.
  - `role` (keyword): the text role.
  - `backdrop`: the colour under the text (e.g. the fill under a label).

  Returns the role's `:color` resolved by [[resolved-text-color]]."
  [theme role backdrop]
  (resolved-text-color (get-in theme [:text role :color]) backdrop))

(defn painted-color
  "Gives the colour already painted on a canvas at a user-space point.

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `x`, `y` (doubles): the point in user space (through the canvas transform, e.g. a panel's translation).

  Returns the pixel's colour (a `Vec4`); `nil` outside the image."
  [canvas ^double x ^double y]
  (let [^Vec2 at (c2d/transform canvas x y)
        px (long (m/floor (.x at))) py (long (m/floor (.y at)))]
    (when (and (m/< -1 px (c2d/width canvas)) (m/< -1 py (c2d/height canvas)))
      (p/get-color canvas px py))))

(defn outline-color
  "Gives the outline colour of a mark.

  Parameters:

  - `stroke`: the `:stroke-color` (see [[stroke-color]]) - a colour, `:none` or `:darker`.
  - `fill`: the mark's fill.

  Returns the darkened fill for `:darker`, else `stroke` (`:none` stays `:none`)."
  [stroke fill]
  (if (= :darker stroke) (c/darken fill) stroke))

(defn visible-stroke?
  "Tests whether a stroke draws anything: its colour isn't `:none` and its width is positive.

  Parameters:

  - `color`: the stroke colour or `:none`.
  - `width` (number): the stroke width in pixels.

  Returns a truthy value for a visible stroke."
  [color width]
  (and (not (theme/none? color)) (m/pos? (double width))))

(defn outline!
  "Strokes an outline when it is visible (see [[visible-stroke?]]).

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `color`: the outline colour or `:none`.
  - `width` (number): the outline width in pixels.
  - `join` (keyword, optional): the line join (`:miter`, `:round`, `:bevel`); the canvas default without it.
  - `stroke-shape!` (function): of the canvas, drawing the outlined shape stroked.

  Returns `nil`; nothing drawn for an invisible stroke."
  ([canvas color width stroke-shape!] (outline! canvas color width nil stroke-shape!))
  ([canvas color width join stroke-shape!]
   (when (visible-stroke? color width)
     (c2d/set-stroke-custom canvas (cond-> {:size width} join (assoc :join join)))
     (c2d/set-color canvas color)
     (stroke-shape! canvas)
     nil)))

(defn pattern-ink
  "Gives the ink of a fill pattern, no more opaque than the fill: a translucent fill (e.g. overlaid groups) gets translucent ink.

  Parameters:

  - `ink`: the `:pattern-color` - a colour or `:darker` (the darkened fill).
  - `fill`: the mark's fill.

  Returns the ink colour with alpha at most the fill's."
  [ink fill]
  (let [resolved (outline-color ink fill)
        alpha (m/min (c/alpha resolved) (c/alpha fill))]
    (if (m/< alpha (c/alpha resolved)) (c/set-alpha resolved alpha) resolved)))

(defn pattern-fill
  "Gives the pattern fill of a mark.

  Many marks of one style share it: build it once per group, or use [[pattern-fill-cache]].

  Parameters:

  - `st` (map): the style - `:pattern` (see [[cljplot2.schema/pattern]]), `:pattern-color` (see [[pattern-ink]], default `:darker`).
  - `fill`: the mark's fill.

  Returns `{:pattern p :ink c :tile img :period n}` - `img` the tile image (see [[cljplot2.graphics.pattern/tile]]), `n` its side in pixels, repeated from the user origin; `nil` without a pattern (`:solid`, `nil`), an ink (`:pattern-color :none`) or a fill (`:none`, `nil`)."
  [st fill]
  (let [pattern-name (:pattern st)
        ink (:pattern-color st :darker)]
    (when-let [spec (pattern/spec-of pattern-name)]
      (when (and (some? fill) (not (theme/none? fill)) (not (theme/none? ink)))
        (let [pattern-color (pattern-ink ink fill)]
          {:pattern pattern-name :ink pattern-color
           :tile (pattern/tile spec pattern-color) :period (pattern/period spec)})))))

(defn pattern-fill-cache
  "Gives a function of a style and a fill giving [[pattern-fill]], built once per pattern, ink and fill - for one draw
  call over many marks (bars, boxes, rects).

  Returns a function `(fn [st fill] pattern-fill)`; its cache lives as long as the function."
  []
  (let [cached (memoize (fn [pattern-name ink fill] (pattern-fill {:pattern pattern-name :pattern-color ink} fill)))]
    (fn [st fill] (cached (:pattern st) (:pattern-color st :darker) fill))))

(defn fill-with-pattern!
  "Draws a pattern over a fill: the same shapes filled again with the pattern's tile as the paint.

  Parameters:

  - `canvas`: the canvas in graphics context (its paint is kept).
  - `fill-pattern`: see [[pattern-fill]]; `nil` draws nothing.
  - `draw-fn` (fn of a canvas): fills the shapes on the canvas it gets; it must not set a colour.

  Returns `nil`."
  [canvas fill-pattern draw-fn]
  (when-let [{:keys [tile ^long period]} fill-pattern]
    (c2d/with-isolated-canvas [patterned canvas]
      (c2d/pattern-mode patterned tile 0 0 period period)
      (draw-fn patterned)
      nil)))

(defn fill-patterned!
  "Fills shapes in a colour, then draws a pattern over them.

  Parameters:

  - `canvas`: the canvas in graphics context; its colour is `color` afterwards.
  - `color`: the fill colour (any clojure2d representation).
  - `fill-pattern`: see [[pattern-fill]]; `nil` - the fill alone.
  - `draw-fn` (fn of a canvas): fills the shapes on the canvas it gets, called twice (the fill, then the pattern); it must not set a colour.

  Returns `nil`.

  See also [[fill-with-pattern!]] (a pattern over a fill drawn otherwise, e.g. a gradient)."
  [canvas color fill-pattern draw-fn]
  (c2d/set-color canvas color)
  (draw-fn canvas)
  (fill-with-pattern! canvas fill-pattern draw-fn))

(defn pixel-center
  "Gives the centre of the pixel containing a coordinate: thin lines drawn there cover whole pixels (no antialiasing smear).

  Parameters:

  - `p` (double): the coordinate in pixels.

  Returns `floor(p) + 0.5`."
  ^double [^double p]
  (m/+ (m/floor p) 0.5))

(defn zero-line!
  "Draws the line at 0 of a value scale across a panel (mirror densities, histograms and bars).

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `box` (map): the panel's `{:w :h}` in pixels (flip-aware).
  - `scales` (map): the panel's finalized scales.
  - `style` (map): `:zero-line-color`, `:zero-line-width`.
  - `k` (keyword): the value scale, default `:y`.

  Returns `nil`; nothing is drawn when 0 isn't in the scale, the colour is `:none` or the width 0."
  ([canvas box scales style] (zero-line! canvas box scales style :y))
  ([canvas {:keys [w h]} scales {:keys [zero-line-color zero-line-width]} k]
   (let [at (if (k scales) (scale/forward (k scales) 0.0) ##NaN)
         width (double zero-line-width)]
     (when (and (not (m/nan? at)) (not (theme/none? zero-line-color)) (m/pos? width))
       (let [px (pixel-center at)]
         (c2d/set-color canvas zero-line-color)
         (c2d/set-stroke-custom canvas {:size width :cap :butt})
         (if (scale/horizontal? k (k scales))
           (c2d/line canvas px 0 px h)
           (c2d/line canvas 0 px w px)))))))

(defmulti draw-content
  "Draws item content onto a canvas inside a box, dispatched like [[cljplot2.graphics.measure/natural-size]].

  A method may change any graphics state (clip, transform, font, colour, ...) without restoring it; callers isolate each call (`c2d/with-isolated-canvas`). To restrict drawing to the box a method intersects the clip (`c2d/add-clip`) rather than replacing it, so an outer clip still applies.

  Parameters:

  - `content` (map): the item content.
  - `canvas`: the canvas in graphics context.
  - `box` (map): `{:x :y :w :h}` in pixels.
  - `env` (map): `:theme` - the plot theme, `:scales` - finalized scales (see [[cljplot2.scale.position/finalize]]).

  Content kinds without a method draw nothing."
  (fn [content _canvas _box _env] (measure/content-kind content)))

(defmethod draw-content :default [_ _ _ _] nil)

(defn inner-border!
  "Draws a border inside a box as four filled bands - exact pixels, nothing outside the box.

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `box` (map): `{:x :y :w :h}` in whole pixels.
  - `color`: the border colour; `:none` draws nothing.
  - `width` (number): the width in pixels, rounded, at most half the box; 0 draws nothing.

  Returns `nil`."
  [canvas {:keys [^long x ^long y ^long w ^long h]} color width]
  (let [bw (m/min (m/round (double width)) (m/long-quot w 2) (m/long-quot h 2))]
    (when (and (not (theme/none? color)) (m/pos? bw))
      (c2d/set-color canvas color)
      (c2d/rect canvas x y w bw)
      (c2d/rect canvas x (m/long-sub (m/long-add y h) bw) w bw)
      (c2d/rect canvas x y bw h)
      (c2d/rect canvas (m/long-sub (m/long-add x w) bw) y bw h))))

(defn rect-of
  "Gives the whole-pixel rectangle spanned by two data corners.

  Parameters:

  - `projection` (fn of x y): places a data point as a `Vec2` (see [[cljplot2.scale.position/projection]]).
  - `x0`, `y0`, `x1`, `y1`: the corners `[x0 y0]` and `[x1 y1]` in data units.

  Returns `{:x :y :w :h}`: both corners rounded, then normalized (non-negative width and height), so flipped scales work too."
  [projection x0 y0 x1 y1]
  (let [^Vec2 a (projection x0 y0)
        ^Vec2 b (projection x1 y1)
        ax (m/round (.x a)) ay (m/round (.y a))
        bx (m/round (.x b)) by (m/round (.y b))]
    {:x (m/min ax bx) :y (m/min ay by) :w (m/abs (m/long-sub bx ax)) :h (m/abs (m/long-sub by ay))}))

(def ^{:private true :const true :tag 'double} min-bar-pixels
  "Bars thinner than this (pixels, along either side) aren't drawn: sub-pixel slivers only smear the baseline."
  0.5)

(defn visible-bar?
  "Tests whether a bar is drawn: both sides at least [[min-bar-pixels]].

  Parameters:

  - `w`, `h` (doubles): the bar's sides in pixels.

  Returns true or false."
  [^double w ^double h]
  (and (m/>= w min-bar-pixels) (m/>= h min-bar-pixels)))

(def corner-radius-option
  "Option entry `:corner-radius`: pixels, every corner of a bar or rect rounded (default 0 - square)."
  (first (schema/in-fragment
          :corner-radius
          [(schema/optional :corner-radius {:description "radius in pixels of every corner of a bar or rect" :default 0} schema/non-neg)])))

(def corner-radius-end-option
  "Option entry `:corner-radius-end`: pixels, only the corners at a bar's value end rounded (Vega-Lite
  `cornerRadiusEnd`); wins over `:corner-radius`."
  (first (schema/in-fragment
          :corner-radius
          [(schema/optional :corner-radius-end {:description "radius in pixels of the corners at the bar's value end only (wins over :corner-radius)"}
                            schema/non-neg)])))

(defn corner-rounding
  "Gives the corner rounding of a bar chart.

  Parameters:

  - `options` (map): the chart options - `:corner-radius`, `:corner-radius-end` (see [[corner-radius-option]], [[corner-radius-end-option]]).

  Returns `{:radius r :value-end e}` - `:corner-radius-end` (only the value-end corners) wins over `:corner-radius`; `nil` for square corners (both absent or 0). A drawn bar adds `:axis` (see [[bar-rounding]])."
  [options]
  (let [end (:corner-radius-end options)
        r (double (or end (:corner-radius options) 0.0))]
    (when (m/pos? r) {:radius r :value-end (some? end)})))

(defn value-axis
  "Gives the range axis a bar's value runs along.

  Parameters:

  - `base`, `end` (`[h v]` or `Vec2`): range points at the baseline and at the value end, on the same bar edge.

  Returns `:v` when their `h` coordinates are equal, else `:h`."
  [[^double bh _] [^double eh _]]
  (if (m/== bh eh) :v :h))

(defn value-ends
  "Gives the base and the value end of a bar spanning two values.

  Parameters:

  - `y0`, `y1` (numbers): the bar's values, any order.

  Returns `[base end]`: the value end is the one farther from 0 - stacked segments `[below above]`, negative ones and a mirror's downward bars `[far near]` (see [[cljplot2.data.position/stack-values]])."
  [y0 y1]
  (if (m/> (m/abs (double y0)) (m/abs (double y1))) [y1 y0] [y0 y1]))

(defn bar-rounding
  "Gives the rounding of one bar.

  Parameters:

  - `rounding`: see [[corner-rounding]], may be `nil`.
  - `base`, `end`: the bar's range points (see [[value-axis]]).

  Returns `rounding` with its `:axis`; `nil` for `nil`."
  [rounding base end]
  (some-> rounding (assoc :axis (value-axis base end))))

(defn whole-pixel
  "Gives a point with both coordinates rounded to whole pixels.

  Parameters:

  - `p` (`Vec2`): the point.

  Returns a `Vec2`."
  ^Vec2 [^Vec2 p]
  (Vec2. (m/round (.x p)) (m/round (.y p))))

(defn- rounded-corners
  "Gives the rounded corners of a range rectangle `[h0 v0]`-`[h1 v1]` (the second corner at the value end).

  Parameters:

  - `rounding` (map): `:value-end`, `:axis` (see [[corner-rounding]], [[bar-rounding]]).

  Returns a set of `[i j]` - the corner at `hi` and `vj` (0 or 1): every corner, or with `:value-end` the two at the value end along `:axis`."
  [{:keys [value-end axis]}]
  (cond
    (not value-end) #{[0 0] [1 0] [1 1] [0 1]}
    (= :h axis) #{[1 0] [1 1]}
    :else #{[0 1] [1 1]}))

(defn- pixel-rect-radii
  "Gives the corner radii of the pixel rectangle spanned by two corners.

  Parameters:

  - `a`, `b` (`Vec2`): the corners, `b` at the value end.
  - `rounding` (map): see [[rounded-corners]], with `:radius`.

  Returns `[top-left top-right bottom-right bottom-left]`, the radius at rounded corners, 0.0 elsewhere."
  [^Vec2 a ^Vec2 b rounding]
  (let [corners (rounded-corners rounding)
        r (double (:radius rounding))
        left (if (m/<= (.x a) (.x b)) 0 1) top (if (m/<= (.y a) (.y b)) 0 1)
        radius (fn [i j] (if (corners [i j]) r 0.0))]
    [(radius left top) (radius (m/long-sub 1 left) top) (radius (m/long-sub 1 left) (m/long-sub 1 top)) (radius left (m/long-sub 1 top))]))

(defn- shape-bar!
  "Fills a shape with a colour and a pattern and outlines it.

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `shape` (java.awt `Shape`): the bar.
  - `fill`: the fill colour.
  - `stroke`: the outline colour or `:none`.
  - `stroke-width` (number): the outline width in pixels (mitred); 0 - no outline.
  - `fill-pattern`: see [[pattern-fill]], `nil` - none.

  Returns `nil`."
  [canvas ^Shape shape fill stroke stroke-width fill-pattern]
  (let [width (double stroke-width)]
    (fill-patterned! canvas fill fill-pattern #(c2d/shape % shape false))
    (outline! canvas stroke width :miter #(c2d/shape % shape true))))

(defn bar!
  "Fills the rectangle between two pixel corners with a colour and a pattern and outlines it.

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `a`, `b` (`Vec2`): the corners, any order (`b` at the value end for end rounding).
  - `fill`: the fill colour.
  - `stroke`: the outline colour or `:none`.
  - `stroke-width` (number): the outline width in pixels (mitred); 0 - no outline.
  - `fill-pattern`: see [[pattern-fill]], `nil` (default) - none.
  - `rounding`: see [[corner-rounding]], `nil` (default) - square corners.

  Returns `nil`; nothing for a bar that isn't [[visible-bar?]]."
  ([canvas a b fill stroke stroke-width] (bar! canvas a b fill stroke stroke-width nil nil))
  ([canvas a b fill stroke stroke-width fill-pattern] (bar! canvas a b fill stroke stroke-width fill-pattern nil))
  ([canvas ^Vec2 a ^Vec2 b fill stroke stroke-width fill-pattern rounding]
   (let [width (double stroke-width)
         x (m/min (.x a) (.x b)) y (m/min (.y a) (.y b))
         w (m/abs (m/- (.x b) (.x a))) h (m/abs (m/- (.y b) (.y a)))]
     (when (visible-bar? w h)
       (if rounding
         (shape-bar! canvas (sh/rounded-rect x y w h (pixel-rect-radii a b rounding))
                     fill stroke width fill-pattern)
         (do (fill-patterned! canvas fill fill-pattern #(c2d/rect % x y w h))
             (outline! canvas stroke width :miter #(c2d/rect % x y w h true))))))))

(def ^{:private true :const true :tag 'double} corner-step-angle
  "Largest angle (radians) between points of a rounded corner's arc."
  (m/radians 10.0))

(defn- corner-arc
  "Gives the pixel points of an arc around a centre, the shorter way, densified.

  Parameters:

  - `c` (`Vec2`): the centre.
  - `r` (double): the radius.
  - `p`, `q` (`Vec2`): the arc's ends, both `r` from `c`.

  Returns a vector of `[x y]` from `p` to `q`, at most [[corner-step-angle]] (and 2 / r) radians apart."
  [^Vec2 c ^double r ^Vec2 p ^Vec2 q]
  (let [a0 (m/atan2 (m/- (.y p) (.y c)) (m/- (.x p) (.x c)))
        a1 (m/atan2 (m/- (.y q) (.y c)) (m/- (.x q) (.x c)))
        d (m/- (m/mod (m/+ (m/- a1 a0) m/PI) m/TWO_PI) m/PI)
        step (if (m/pos? r) (m/min corner-step-angle (m// 2.0 r)) corner-step-angle)
        n (m/max 1 (long (m/ceil (m// (m/abs d) step))))]
    ;; the corner's points are reversed with rseq by rounded-sector-path
    (mapv (fn [^long i] (let [a (m/+ a0 (m/* d (m// (double i) (double n))))]
                          [(m/+ (.x c) (m/* r (m/cos a))) (m/+ (.y c) (m/* r (m/sin a)))]))
          (range (m/long-inc n)))))

(defn- sector-corner
  "Gives a rounded corner where a radial edge meets an arc of an annular sector.

  Parameters:

  - `geometry` (map): the panel's polar geometry.
  - `corner` (map): `:a` - the edge's angle (radians), `:inward` - the angular direction (+1 / -1) into the sector, `:arc` - the arc's radius, `:outer` - the outer arc (the corner circle inside it), else the inner one (outside it), `:r` - the corner radius.

  Returns `{:delta d :edge points}`: `d` - the angle the arc gives up, `points` - the corner from the arc to the edge (pixels; one point - a square corner)."
  [geometry {:keys [^double a ^double inward ^double arc outer ^double r]}]
  (if (m/pos? r)
    (let [centre-r (if outer (m/- arc r) (m/+ arc r))
          delta (m/asin (m// r centre-r))
          at (m/+ a (m/* inward delta))
          tangent-arc (coord/to-pixel geometry at arc)
          tangent-edge (coord/to-pixel geometry a (m/sqrt (m/- (m/* centre-r centre-r) (m/* r r))))]
      {:delta delta :edge (corner-arc (coord/to-pixel geometry at centre-r) r tangent-arc tangent-edge)})
    (let [^Vec2 corner (coord/to-pixel geometry a arc)]
      {:delta 0.0 :edge [[(.x corner) (.y corner)]]})))

(defn- sector-radius-limit
  "Gives the largest corner radius of an annular sector: half the radial thickness, and the corners of one arc not overlapping (outer arc; inner arc when the inner radius is positive).

  Parameters:

  - `span` (double): the sector's angle in radians.
  - `lo`, `hi` (doubles): the inner and outer radius.

  Returns the radius limit."
  ^double [^double span ^double lo ^double hi]
  (let [s (m/sin (m/min (m/* 0.5 span) m/HALF_PI))]
    (cond-> (m/min (m/* 0.5 (m/- hi lo)) (m// (m/* hi s) (m/inc s)))
      (and (m/pos? lo) (m/< s 1.0)) (m/min (m// (m/* lo s) (m/- 1.0 s))))))

(def ^{:private true :const true :tag 'double} full-turn
  "Angular span (radians) treated as a whole circle: 2π less a tolerance."
  (m/- m/TWO_PI 1.0e-9))

(defn rounded-sector-path
  "Gives the closed polygon of a range rectangle in polar geometry with rounded corners: an annular sector with circular corners.

  Parameters:

  - `geometry` (map): the panel's polar geometry.
  - `[h0 v0]`, `[h1 v1]`: the range rectangle's corners (numbers), the second at the value end.
  - `rounding` (map): see [[corner-rounding]], [[bar-rounding]].

  Returns a vector of pixel `[x y]` points; the radius clamped by [[sector-radius-limit]]; no inner corners at a zero inner radius (a wedge); a full ring has no corners (see [[cljplot2.scale.coord/rect-path]])."
  [geometry [^double h0 ^double v0] [^double h1 ^double v1] rounding]
  (let [lo (m/min v0 v1) hi (m/max v0 v1)
        span (m/abs (m/- h1 h0))]
    (if (m/>= span full-turn)
      (coord/rect-path geometry h0 v0 h1 v1)
      (let [corners (rounded-corners rounding)
            r (m/min (double (:radius rounding)) (sector-radius-limit span lo hi))
            sigma (if (m/<= h0 h1) 1.0 -1.0)
            outer-j (if (m/>= v1 v0) 1 0)
            corner (fn [^long i outer?]
                     (sector-corner geometry {:a (if (m/zero? i) h0 h1) :inward (if (m/zero? i) sigma (m/- sigma))
                                              :arc (if outer? hi lo) :outer outer?
                                              :r (if (corners [i (if outer? outer-j (m/long-sub 1 outer-j))]) r 0.0)}))
            o0 (corner 0 true) o1 (corner 1 true)
            outer (coord/line-path geometry [[(m/+ h0 (m/* sigma (double (:delta o0)))) hi]
                                             [(m/- h1 (m/* sigma (double (:delta o1)))) hi]] true)
            inner (if (m/pos? lo)
                    (let [i1 (corner 1 false) i0 (corner 0 false)]
                      (concat (rseq (:edge i1))
                              (coord/line-path geometry [[(m/- h1 (m/* sigma (double (:delta i1)))) lo]
                                                         [(m/+ h0 (m/* sigma (double (:delta i0)))) lo]] true)
                              (:edge i0)))
                    (coord/line-path geometry [[h1 0.0]] false))]
        ;; the polygon is checked, filled and stroked by range-bar!
        (vec (concat outer (:edge o1) inner (rseq (:edge o0))))))))

(defn range-bar!
  "Draws [[bar!]] of a range rectangle: the pixel rectangle in cartesian plots, the annular sector in polar ones.

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `geometry`: the panel's polar geometry; `nil` - cartesian.
  - `a`, `b` (`[h v]`): the range rectangle's corners (see [[cljplot2.scale.coord]]), `b` at the value end for end rounding.
  - `fill`, `stroke`, `stroke-width`, `fill-pattern`: as [[bar!]].
  - `rounding`: see [[corner-rounding]], `nil` (default) - square corners (polar - see [[rounded-sector-path]]).

  Returns `nil`. A sector going once around the whole circle - a disc or ring - is outlined without the seam along its start spoke (see [[cljplot2.scale.coord/full-turn-circles]]); a sector with a non-finite point draws nothing."
  ([canvas geometry a b fill stroke stroke-width fill-pattern]
   (range-bar! canvas geometry a b fill stroke stroke-width fill-pattern nil))
  ([canvas geometry [^double h0 ^double v0 :as a] [^double h1 ^double v1 :as b] fill stroke stroke-width fill-pattern rounding]
   (if-not geometry
     (bar! canvas (Vec2. h0 v0) (Vec2. h1 v1) fill stroke stroke-width fill-pattern rounding)
     (let [polygon (if rounding
                     (rounded-sector-path geometry a b rounding)
                     (coord/rect-path geometry h0 v0 h1 v1))
           width (double stroke-width)]
       (when (every? data/finite-point? polygon)
         (if rounding
           (shape-bar! canvas (sh/path polygon true) fill stroke width fill-pattern)
           (do (fill-patterned! canvas fill fill-pattern #(c2d/path % polygon true false))
               (outline! canvas stroke width :miter
                         (fn [c]
                           (if (coord/full-turn? geometry h0 h1)
                             (doseq [circle (coord/full-turn-circles geometry h0 v0 h1 v1)] (c2d/path c circle false true))
                             (c2d/path c polygon true true)))))))))))

(def ^{:private true :const true :tag 'double} key-radius-fraction
  "Largest corner radius of a legend key square, as a fraction of its side: the key reads as a rounded square whatever
  the bars' radius (pixels of a large bar would make a small key a circle)."
  0.25)

(defn key-rounding
  "Gives the rounding of a legend key square.

  Parameters:

  - `rounding`: see [[corner-rounding]], may be `nil`.
  - `side` (number): the key square's side in pixels.

  Returns `rounding` with the radius at most [[key-radius-fraction]] of the side and end rounding at its top (an upright bar); `nil` for `nil`."
  [rounding side]
  (some-> rounding
          (assoc :axis :v)
          (update :radius #(m/min (double %) (m/* key-radius-fraction (double side))))))

(def ^{:const true :tag 'double} key-glyph-share
  "Share of a legend key box's shorter side a filled key glyph (square, circle, dot) takes."
  0.7)

(defn circle-key!
  "Draws a filled circle key glyph centred in a legend key box, with a pattern over it.

  Parameters:

  - `canvas`: the clojure2d canvas.
  - `box` (map): the key box `{:x :y :w :h}` (pixels).
  - `fill`: the circle's colour.
  - `fill-pattern`: the pattern over the fill (see [[pattern-fill]]), `nil` - none.

  The circle's diameter is [[key-glyph-share]] of the box's shorter side.

  Returns `nil`.

  See also [[bar-key!]] (a square glyph)."
  [canvas {:keys [^double x ^double y ^double w ^double h]} fill fill-pattern]
  (let [d (m/* key-glyph-share (m/min w h))
        cx (m/+ x (m/* 0.5 w))
        cy (m/+ y (m/* 0.5 h))]
    (fill-patterned! canvas fill fill-pattern #(c2d/ellipse % cx cy d d))))

(defn spike-key!
  "Draws a vertical line key glyph through the middle of a legend key box.

  Parameters:

  - `canvas`: the clojure2d canvas.
  - `box` (map): the key box `{:x :y :w :h}` (pixels).
  - `line` (map): `:color`, `:line-width` (pixels, butt caps) and `:span` - `[from to]`, the line's ends as fractions of the box height from its top.

  Returns the canvas.

  See also [[circle-key!]], [[bar-key!]]."
  [canvas {:keys [^double x ^double y ^double w ^double h]} {:keys [color line-width span]}]
  (let [[^double from ^double to] span
        cx (m/+ x (m/* 0.5 w))]
    (c2d/set-color canvas color)
    (c2d/set-stroke-custom canvas {:size line-width :cap :butt})
    (c2d/line canvas cx (m/+ y (m/* from h)) cx (m/+ y (m/* to h)))))

(defn with-alpha
  "Gives a colour with its alpha set to a fraction of full opacity.

  Parameters:

  - `color`: a colour (any clojure2d representation) or `:none`.
  - `alpha` (fraction or `nil`): the opacity, 0 - transparent, 1 - opaque.

  Returns `color` with alpha `255 alpha`; `:none` and a `nil` alpha give `color` unchanged.

  See also [[faded]] (an existing alpha scaled)."
  [color alpha]
  (if (or (nil? alpha) (theme/none? color)) color (c/set-alpha color (m/* 255.0 (double alpha)))))

(defn faded
  "Gives a colour with its alpha scaled.

  Parameters:

  - `color`: a colour (any clojure2d representation), `:none` or `nil`.
  - `k` (number): the factor of the colour's alpha, e.g. 0.5 halves its opacity.

  Returns `color` with alpha `k` times its own (not clamped: a factor in `[0, 1]` keeps it in range, see `clojure2d.color/scale-alpha`); `:none` and `nil` unchanged.

  See also [[with-alpha]] (alpha set to a fraction)."
  [color ^double k]
  (if (or (nil? color) (theme/none? color)) color (c/scale-alpha color k)))

(defn translucent-fill
  "Gives the fill colour of a mark drawn translucent or opaque (grouped overlay bars are translucent).

  Parameters:

  - `color`: the mark colour or `:none`.
  - `alpha` (fraction or `nil`): the opacity of a translucent fill.
  - `translucent?`: truthy - the fill gets `alpha` (see [[with-alpha]]).

  Returns `color` with alpha `alpha` when `translucent?`, else `color`."
  [color alpha translucent?]
  (if translucent? (with-alpha color alpha) color))

(defn bar-key!
  "Draws the legend key glyph of a bar chart (histogram, bar): a filled square inset in a key box with its pattern, outlined.

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `box` (map): the key box `{:x :y :w :h}` in pixels; the square's side is [[key-glyph-share]] of its shorter side.
  - `st` (map): `:color`, `:stroke-color` (see [[outline-color]]), `:stroke-width` (0 - no outline), `:alpha`, `:pattern`, `:pattern-color` (see [[pattern-fill]]).
  - `translucent?`: truthy - the fill gets `:alpha` (grouped overlay bars).
  - `rounding`: see [[corner-rounding]], `nil` (default) - square; rounds the corners as an upright bar's (see [[key-rounding]]).

  Returns `nil`."
  ([canvas box st translucent?] (bar-key! canvas box st translucent? nil))
  ([canvas {:keys [^double x ^double y ^double w ^double h]} st translucent? rounding]
   (let [side (m/* key-glyph-share (m/min w h))
         cx (m/+ x (m/* 0.5 w))
         cy (m/+ y (m/* 0.5 h))
         fill (translucent-fill (:color st) (:alpha st) translucent?)
         stroke (outline-color (:stroke-color st) (:color st))
         stroke-width (double (:stroke-width st))]
     (if rounding
       (let [half (m/* 0.5 side)]
         (bar! canvas (Vec2. (m/- cx half) (m/+ cy half)) (Vec2. (m/+ cx half) (m/- cy half)) fill stroke stroke-width
               (pattern-fill st fill) (key-rounding rounding side)))
       (do (fill-patterned! canvas fill (pattern-fill st fill) #(c2d/crect % cx cy side side))
           (outline! canvas stroke stroke-width #(c2d/crect % cx cy side side true)))))))

(defn cvd-filter-fn
  "Gives the colour function simulating a colour vision deficiency.

  Parameters:

  - `deficiency`: a clojure2d `cvd-filter` keyword (`:protanopia`, `:deuteranopia`, `:tritanopia`, `:achromatopsia`) or `[keyword severity]` (`:protanomaly`, `:deuteranomaly`, `:tritanomaly`, `:achromatomaly`; severity in `[0 1]`).

  Returns a function of a colour. An unknown deficiency or severity throws clojure2d's `ex-info`."
  [deficiency]
  (if (vector? deficiency) (apply c/cvd-filter deficiency) (c/cvd-filter deficiency)))
