(ns cljplot2.graphics.measure
  "Natural sizes of grid item content, and pixel geometry shared by layout and drawing.

  Text metrics depend on the rendering hint (fractional metrics), not on canvas size,
  so content is measured on a 1x1 scratch canvas created with the target canvas's hint.

  Functions: measuring [[natural-size]], [[label-size]], [[validate-content]], [[content-kind]], [[scratch-canvas]],
  [[call-with-scratch-canvas]]; label thinning [[spans-overlap?]], [[kept-label-indices]],
  [[kept-box-indices]], [[box-crosses-path?]]; label repel [[repelled-placements]]; geometry [[boxes-overlap?]], [[max-size]].

  Axis tick labels lying end to end (text blocks without their inset) are kept at least theme
  `[:axis k :tick-label-gap]` pixels apart, so they never touch; labels stacked across their text (vertical axes,
  colour bars) are measured by their line height, which holds the font's leading; label boxes ([[kept-box-indices]]:
  polar guides, text `:overlapping-labels :drop` / `:repel`) include their insets.

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

(def ^{:private true :const true :tag 'double} repel-step
  "Pixels between neighbouring candidate shifts of a repelled label box (layout geometry)."
  4.0)

(def ^{:private true :const true :tag 'double} obstacle-radius
  "Half the side of the square around a label's point that other repelled labels keep clear of (pixels; layout geometry)."
  3.0)

(def ^{:private true :const true :tag 'double} min-leader-move
  "Shortest move of a repelled label that gets a leader line, pixels: a label moved less far keeps none (layout geometry)."
  6.0)

(defn- candidate-shifts
  "Gives the shifts a repelled box tries, nearest first.

  Parameters:

  - `distance` (double): the largest shift in pixels.
  - `w`, `h` (doubles): the panel size - no shift is longer along x than `w` nor along y than `h` (a box moved farther
    leaves the panel), so the candidates are bounded by the panel whatever the distance.

  Returns a vector of `[dx dy]` - `[0 0]` first, then the points of a grid of [[repel-step]] pixels within `distance`,
  by distance, ties by angle counterclockwise from the right (as drawn)."
  [^double distance ^double w ^double h]
  (let [nx (long (m/floor (m// (m/min distance (m/max 0.0 w)) repel-step)))
        ny (long (m/floor (m// (m/min distance (m/max 0.0 h)) repel-step)))]
    (->> (for [i (range (m/long-sub 0 nx) (m/long-inc nx))
               j (range (m/long-sub 0 ny) (m/long-inc ny))
               :let [dx (m/* repel-step (double i)) dy (m/* repel-step (double j))
                     d (m/hypot dx dy)]
               :when (m/<= d distance)]
           [d (m/mod (m/atan2 (m/- dy) dx) m/TWO_PI) [dx dy]])
         (sort-by (fn [[d a]] [d a]))
         (mapv peek))))

(defn- shifted-box
  "Gives a box moved by a shift.

  Parameters:

  - `box` (map): a pixel box `{:x :y :w :h}`.
  - `[dx dy]`: the shift in pixels.

  Returns the moved box."
  [{:keys [^double x ^double y] :as box} [^double dx ^double dy]]
  (assoc box :x (m/+ x dx) :y (m/+ y dy)))

(defn- box-corners
  "Gives the corners of a box, clockwise as drawn from its top-left one.

  Parameters:

  - `box` (map): a pixel box `{:x :y :w :h}`.

  Returns a vector of four `[x y]`."
  [{:keys [^double x ^double y ^double w ^double h]}]
  [[x y] [(m/+ x w) y] [(m/+ x w) (m/+ y h)] [x (m/+ y h)]])

(defn- nearest-edge-point
  "Gives the point of a convex polygon's edge nearest to a point outside it.

  Parameters:

  - `corners` (vector of `[x y]`): the polygon's corners in order (either way round).
  - `[px py]`: the point.

  Returns `[x y]` on the edge; `nil` when the point lies inside the polygon or on its edge."
  [corners [^double px ^double py]]
  (let [edges (map vector corners (concat (rest corners) [(first corners)]))
        sides (map (fn [[[^double x0 ^double y0] [^double x1 ^double y1]]]
                     (m/signum (m/- (m/* (m/- x1 x0) (m/- py y0)) (m/* (m/- y1 y0) (m/- px x0)))))
                   edges)]
    (when-not (or (every? #(m/>= (double %) 0.0) sides) (every? #(m/<= (double %) 0.0) sides))
      (peek (reduce (fn [[^double best :as acc] [[^double x0 ^double y0] [^double x1 ^double y1]]]
                      (let [dx (m/- x1 x0) dy (m/- y1 y0)
                            l2 (m/+ (m/* dx dx) (m/* dy dy))
                            t (if (m/pos? l2) (m/constrain (m// (m/+ (m/* (m/- px x0) dx) (m/* (m/- py y0) dy)) l2) 0.0 1.0) 0.0)
                            qx (m/+ x0 (m/* t dx)) qy (m/+ y0 (m/* t dy))
                            d (m/hypot (m/- qx px) (m/- qy py))]
                        (if (m/< d best) [d [qx qy]] acc)))
                    [##Inf nil] edges)))))

(defn- leader-of
  "Gives the leader line of a repelled label.

  Parameters:

  - `point` (`[x y]`): the label's point.
  - `frame` (vector of `[x y]`): the corners of the label's drawn frame where it would sit unmoved.
  - `[dx dy]`: the label's shift.

  Returns `[point end]` - from the point to the nearest point of the moved frame's edge - when the label moved more
  than [[min-leader-move]] pixels and its point lies outside the moved frame; else `nil`."
  [point frame [^double dx ^double dy]]
  (when (m/> (m/hypot dx dy) min-leader-move)
    (when-let [end (nearest-edge-point (mapv (fn [[^double x ^double y]] [(m/+ x dx) (m/+ y dy)]) frame) point)]
      [point end])))

(defn- point-in-box?
  "Tests whether a point lies in a box (its edge included).

  Parameters:

  - `[x y]`: the point.
  - `box` (map): a pixel box `{:x :y :w :h}`.

  Returns true or false."
  [[^double x ^double y] {bx :x by :y bw :w bh :h}]
  (and (m/<= (double bx) x (m/+ (double bx) (double bw)))
       (m/<= (double by) y (m/+ (double by) (double bh)))))

(defn- slab-exit
  "Gives where a segment leaves a slab along one coordinate.

  Parameters:

  - `p` (double): the segment's start along the coordinate, inside the slab.
  - `d` (double): the segment's extent along it (end less start).
  - `lo`, `hi` (doubles): the slab's ends.

  Returns the segment's parameter (0 - its start, 1 - its end) at the slab's edge; `##Inf` for `d` 0 (never leaves)."
  ^double [^double p ^double d ^double lo ^double hi]
  (cond (m/pos? d) (m// (m/- hi p) d)
        (m/neg? d) (m// (m/- lo p) d)
        :else ##Inf))

(defn- leaving-box
  "Gives a segment starting inside boxes shortened to where it leaves the last of them.

  Parameters:

  - `[[x0 y0] [x1 y1]]`: the segment, its start inside every box of `covers`.
  - `covers` (sequence of maps): pixel boxes `{:x :y :w :h}`.

  Returns the segment from its farthest exit point (it ends outside the boxes); itself without `covers`; `nil` when
  less than [[min-leader-move]] pixels of it are left."
  [[[^double x0 ^double y0] [^double x1 ^double y1] :as segment] covers]
  (if (empty? covers)
    segment
    (let [dx (m/- x1 x0) dy (m/- y1 y0)
          t (double (reduce (fn [^double acc {bx :x by :y bw :w bh :h}]
                              (m/max acc (m/min (slab-exit x0 dx (double bx) (m/+ (double bx) (double bw)))
                                                (slab-exit y0 dy (double by) (m/+ (double by) (double bh))))))
                            0.0 covers))
          sx (m/+ x0 (m/* t dx)) sy (m/+ y0 (m/* t dy))]
      (when (and (m/< t 1.0) (m/> (m/hypot (m/- x1 sx) (m/- y1 sy)) min-leader-move))
        [[sx sy] [x1 y1]]))))

(defn- segment-box
  "Gives the bounding box of a segment.

  Parameters:

  - `[[x0 y0] [x1 y1]]`: the segment.

  Returns a pixel box `{:x :y :w :h}`."
  [[[^double x0 ^double y0] [^double x1 ^double y1]]]
  {:x (m/min x0 x1) :y (m/min y0 y1) :w (m/abs (m/- x1 x0)) :h (m/abs (m/- y1 y0))})

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

(defn repelled-placements
  "Gives where label boxes go to stay off one another and off the labels' points, inside a panel - deterministic label repel.

  Boxes are placed in order (the order is the priority, as in [[kept-box-indices]]): each stays where it is when it
  lies inside the panel and overlaps (by more than 1 px, see [[label-boxes-overlap?]]) no box placed before it; else
  it takes the nearest free shift of a 4 px grid within `distance` (ties by angle, counterclockwise from the right) -
  free: inside the panel, overlapping no box placed before it and no other label's point (a square of 6 px around
  it). With `:leaders` a label moved more than 6 px gets a leader line from its point to the nearest point of its
  drawn frame's edge (none when the point lies in the frame; starting where it leaves the boxes placed over the
  point, none when less than 6 px are left); then also no leader crosses a box (by more than 1 px, see
  [[box-crosses-path?]]): a position whose box crosses a leader placed before it, or whose leader crosses a box
  placed before it, isn't free. A box larger than the panel (no position inside it) stays where it is, unmoved.
  Candidate shifts are bounded by the panel's size, so the time doesn't grow with `distance` past it.

  Parameters:

  - `boxes` (vector of maps): pixel boxes `{:x :y :w :h}`, in priority order.
  - `points` (vector of `[x y]`): each box's label point (pixels; its own point is no obstacle to it).
  - `area` (map): `:w`, `:h` - the panel size in pixels (boxes stay within `[0 w]` x `[0 h]`); `:distance` - the
    largest shift in pixels; `:leaders` - give leader lines and keep them off the boxes (default `nil` - none);
    `:frames` - per box the corners of its drawn frame (a rotated label's turned text frame; default - the box's).

  Returns a vector, per box, of `{:shift [dx dy] :leader l}` (shift `[0.0 0.0]` - not moved; `:leader` `[point end]`
  or `nil`) or `nil` - no free position within `distance` (the box isn't drawn); empty for no boxes. Empty boxes
  (zero size) never overlap."
  [boxes points {:keys [^double w ^double h ^double distance leaders frames]}]
  (let [obstacle-side (m/* 2.0 obstacle-radius)
        obstacles (mapv (fn [[^double x ^double y]]
                          {:x (m/- x obstacle-radius) :y (m/- y obstacle-radius) :w obstacle-side :h obstacle-side})
                        points)
        candidates (candidate-shifts distance w h)
        inside? (fn [{bx :x by :y bw :w bh :h}]
                  (and (m/>= (double bx) 0.0) (m/>= (double by) 0.0)
                       (m/<= (m/+ (double bx) (double bw)) w) (m/<= (m/+ (double by) (double bh)) h)))]
    (:placements
     (reduce (fn [{:keys [kept kept-leaders] :as acc} i]
               (let [{bx :x by :y bw :w bh :h :as box} (boxes i)
                     [^double px ^double py :as point] (points i)
                     frame (if frames (frames i) (box-corners box))
                     ;; only what a shifted box or its leader can reach
                     x0 (m/min px (m/- (double bx) distance)) y0 (m/min py (m/- (double by) distance))
                     reach {:x x0 :y y0
                            :w (m/- (m/max px (m/+ (double bx) (double bw) distance)) x0)
                            :h (m/- (m/max py (m/+ (double by) (double bh) distance)) y0)}
                     near-kept (filterv #(boxes-overlap? reach %) kept)
                     near-leaders (when leaders (filterv #(boxes-overlap? reach (segment-box %)) kept-leaders))
                     near (into near-kept
                                (keep-indexed (fn [j o] (when (and (not= i j) (boxes-overlap? reach o)) o)))
                                obstacles)
                     ;; boxes over the label's own point: its leader starts where it leaves them
                     covers (when leaders (filterv #(point-in-box? point %) near-kept))
                     placement (fn [s] {:shift s :leader (when leaders (some-> (leader-of point frame s) (leaving-box covers)))})
                     clear? (fn [b {:keys [leader]}]
                              (and (not-any? #(box-crosses-path? b %) near-leaders)
                                   (not (and leader (some #(box-crosses-path? % leader) near-kept)))))
                     placed (cond
                              (or (m/> (double bw) w) (m/> (double bh) h)) (placement [0.0 0.0])
                              (and (inside? box) (not-any? #(label-boxes-overlap? box %) near-kept)
                                   (clear? box nil))
                              (placement (first candidates))
                              :else (some (fn [s] (let [b (shifted-box box s)]
                                                    (when (and (inside? b) (not-any? #(label-boxes-overlap? b %) near))
                                                      (let [pl (placement s)] (when (clear? b pl) pl)))))
                                          (rest candidates)))]
                 (if placed
                   (cond-> (-> acc
                               (update :placements conj placed)
                               (update :kept conj (shifted-box box (:shift placed))))
                     (:leader placed) (update :kept-leaders conj (:leader placed)))
                   (update acc :placements conj nil))))
             {:placements [] :kept [] :kept-leaders []} (range (count boxes))))))

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
