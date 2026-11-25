(ns cljplot2.graphics.measure
  "Natural sizes of grid item content, and pixel geometry shared by layout and drawing.

  Text metrics depend on the rendering hint (fractional metrics), not on canvas size,
  so content is measured on a 1x1 scratch canvas created with the target canvas's hint.

  Functions: measuring [[natural-size]], [[label-size]], [[validate-content]], [[content-kind]], [[scratch-canvas]],
  [[call-with-scratch-canvas]]; label thinning [[spans-overlap?]], [[kept-label-indices]],
  [[kept-box-indices]], [[box-crosses-path?]]; geometry [[boxes-overlap?]], [[max-size]].

  Axis tick labels lying end to end (text blocks without their inset) are kept at least theme
  `[:axis k :tick-label-gap]` pixels apart, so they never touch; labels stacked across their text (vertical axes,
  colour bars) are measured by their line height, which holds the font's leading; label boxes ([[kept-box-indices]]:
  polar guides, text `:overlapping-labels :drop`) include their insets.

  Options: [[rendering-hints]]."
  (:require [cljplot2.schema :as schema]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def rendering-hints
  "Rendering hints accepted for plots."
  #{:low :mid :high :highest})

(defn scratch-canvas
  "Gives a 1x1 canvas for measuring; draw or measure on it within a graphics context (see [[call-with-scratch-canvas]]).

  Parameters:

  - `hint` (keyword): the rendering hint, one of [[rendering-hints]].

  Returns a clojure2d canvas."
  [hint]
  (c2d/canvas 1 1 hint))

(defn call-with-scratch-canvas
  "Calls a function with a [[scratch-canvas]] in graphics context; the graphics are released even when it throws.

  Parameters:

  - `hint` (keyword): the rendering hint.
  - `f` (fn of a canvas): the measuring.

  Returns the result of `f`."
  [hint f]
  (c2d/with-canvas [c (scratch-canvas hint)] (f c)))

(defn spans-overlap?
  "Tests whether two labels along one axis, their centres some distance apart, are closer than a gap.

  Natural sizes are rounded up, so 1 px less than `gap` is allowed: with `gap` 0 labels may overlap by up to 1 px.

  Parameters:

  - `distance` (double): the distance between the labels' centres (any sign).
  - `ei`, `ej` (doubles): the labels' extents.
  - `gap` (double): the least pixels between the two labels (e.g. theme `[:axis k :tick-label-gap]`; 0 - up to 1 px
    overlap).

  Returns true or false."
  [^double distance ^double ei ^double ej ^double gap]
  (m/< (m/abs distance) (m/+ (m/* 0.5 (m/+ ei ej)) (m/dec gap))))

(defn kept-label-indices
  "Gives the indices of labels along one axis to draw without collisions: the first, then each label at least a gap from the last kept one (see [[spans-overlap?]]).

  Parameters:

  - `centres`, `extents` (vectors of numbers): the labels' centres and extents, in tick order.
  - `gap` (number): the least pixels between neighbouring labels (see [[spans-overlap?]]); default 0 - labels may
    touch.

  Returns a vector of indices; empty for no labels."
  ([centres extents] (kept-label-indices centres extents 0.0))
  ([centres extents gap]
   (let [g (double gap)]
     (loop [i (long 0) last-kept nil kept []]
       (if (m/>= i (count centres))
         kept
         (if (and last-kept (spans-overlap? (m/- (double (centres i)) (double (centres last-kept)))
                                            (extents last-kept) (extents i) g))
           (recur (m/long-inc i) last-kept kept)
           (recur (m/long-inc i) i (conj kept i))))))))

(defn max-size
  "Gives the largest width and the largest height of a collection of sizes, each taken separately.

  Parameters:

  - `sizes` (sequence): sizes as `Vec2` `[w h]` (e.g. natural sizes of labels).

  Returns a `Vec2` `[max-w max-h]`; `[0 0]` for no sizes."
  ^Vec2 [sizes]
  (reduce v/emx (Vec2. 0.0 0.0) sizes))

(defn boxes-overlap?
  "Tests whether two pixel boxes overlap; touching edges don't.

  Parameters:

  - `a`, `b` (maps): boxes `{:x :y :w :h}` (pixels, top-left corner and size).

  Returns `true` when their interiors intersect, else `false`.

  See also [[label-boxes-overlap?]] (overlaps of more than 1 px, for measured labels)."
  [{^double ax :x ^double ay :y ^double aw :w ^double ah :h} {^double bx :x ^double by :y ^double bw :w ^double bh :h}]
  (and (m/< ax (m/+ bx bw)) (m/< bx (m/+ ax aw))
       (m/< ay (m/+ by bh)) (m/< by (m/+ ay ah))))

(defn- label-boxes-overlap?
  "Tests whether two label boxes overlap by more than 1 px both across and along (natural sizes are rounded up).

  Parameters:

  - `a`, `b` (maps): pixel boxes `{:x :y :w :h}`.

  Returns true or false.

  See also [[boxes-overlap?]] (any overlap)."
  [a b]
  (let [over (fn ^double [^double a0 ^double aw ^double b0 ^double bw]
               (m/- (m/min (m/+ a0 aw) (m/+ b0 bw)) (m/max a0 b0)))]
    (and (m/> (double (over (:x a) (:w a) (:x b) (:w b))) 1.0)
         (m/> (double (over (:y a) (:h a) (:y b) (:h b))) 1.0))))

(defn kept-box-indices
  "Gives the indices of label boxes to draw without collisions: each box overlapping no box kept before it (see [[label-boxes-overlap?]]) - around a circle the last label is checked against the first too.

  Parameters:

  - `boxes` (vector of maps): pixel boxes `{:x :y :w :h}`, in tick order.

  Returns a vector of indices; empty for no boxes."
  [boxes]
  (reduce (fn [kept i]
            (if (some #(label-boxes-overlap? (boxes i) (boxes %)) kept) kept (conj kept i)))
          [] (range (count boxes))))

(defn box-crosses-path?
  "Tests whether a polyline crosses a label box by more than 1 px (natural sizes are rounded up).

  Parameters:

  - `box` (map): a pixel box `{:x :y :w :h}`.
  - `points` (sequence): the polyline's pixel points `[x y]`.

  Returns truthy when a segment of `points` meets the box shrunk by 1 px on every side, else `nil`; never for fewer
  than two points or a box of at most 2 px across.

  See also [[kept-box-indices]] (boxes against boxes)."
  [{:keys [^double x ^double y ^double w ^double h]} points]
  (when (and (m/> w 2.0) (m/> h 2.0))
    ;; a geometry query (no drawing): segment against rectangle
    (let [inner (java.awt.geom.Rectangle2D$Double. (m/inc x) (m/inc y) (m/- w 2.0) (m/- h 2.0))]
      (some (fn [[[^double x1 ^double y1] [^double x2 ^double y2]]] (.intersectsLine inner x1 y1 x2 y2))
            (partition 2 1 points)))))

(defn content-kind
  "Gives the dispatch value of item content.

  Parameters:

  - `content`: the item content (a map) or `nil` (an empty box).

  Returns `::empty` for `nil`, otherwise its `:kind`."
  [content]
  (if (nil? content) ::empty (:kind content)))

(defmulti validate-content
  "Checks item content once, before layout, dispatched on [[content-kind]].

  Parameters:

  - `content` (map): the item content.

  Returns `nil`; throws `ex-info` naming the offending key. Kinds without a method aren't checked here; [[natural-size]] and [[cljplot2.graphics.draw/draw-content]] may assume validated content."
  content-kind)

(defmethod validate-content :default [_] nil)

(defmulti natural-size
  "Gives the natural size of item content, dispatched on [[content-kind]].

  Parameters:

  - `content`: the item content; `nil` - an empty box.
  - `env` (map): `:canvas` - a scratch canvas in graphics context (absent when measuring without one), `:box` - the item's box from the previous layout round (`nil` in the first round), `:scales` - scales finalized for the previous layout round, `:theme` - the plot theme (see [[cljplot2.theme]]).

  Returns a `Vec2` `[width height]`; zero for `nil` content. Content without a known `:kind` is an error keyed `:kind`."
  (fn [content _env] (content-kind content)))

(defmethod natural-size ::empty [_ _] (v/vec2 0.0 0.0))

(def ^:private placeholder-schema
  "Schema of placeholder content: a fixed `:size` `[w h]`."
  [:map [:size [:tuple number? number?]]])

(defmethod natural-size :placeholder
  [{:keys [size] :as content} _]
  (schema/explain! [[placeholder-schema content [:content]]])
  (v/vec2 (size 0) (size 1)))

(defmethod natural-size :default
  [content _]
  (schema/fail! "Unknown content kind." {:key :kind :value (:kind content)}))

(defn label-size
  "Gives the natural size of a text in a text role.

  Parameters:

  - `role` (keyword): the text role (see [[cljplot2.theme/text-roles]]).
  - `text`: the label text.
  - `env` (map): `:canvas` - a scratch canvas in graphics context, `:theme`.

  Returns the `Vec2` width and height in pixels of the label `{:kind :label :role role :text text}` (see [[natural-size]])."
  ^Vec2 [role text env]
  (natural-size {:kind :label :role role :text text} env))
