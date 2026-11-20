(ns cljplot2.data.space3d
  "3D space of the 3D charts (`chart/cloud`, `chart/surface`): orthographic projection of a data box onto a panel.

  Data coordinates are normalized per axis into a box of side `aspect` (`[ax ay az]`, default `[1 1 1]`) centred at the
  origin, then rotated to the view `{:azimuth a :elevation e}` (degrees; default 40 / 30): screen right `u`, screen up
  `v`, depth `d` (away from the viewer). Azimuth 0, elevation 0 looks along `+y`: x to the right, z up; azimuth turns
  the box about the vertical z axis (90 - y to the right), elevation tilts the view from above (positive) or below.

  Perspective: the view's `:distance` δ (default 0 - orthographic) is lattice's `distance` - measured, as lattice
  does, in a box scaled to a longest side of 1: the eye sits `L/δ` box units (`L` - the longest side of `aspect`) in
  front of the box centre, a point at depth `d` is scaled by `1 / (1 + δ d / L)` toward the centre ([[perspective]];
  lattice's look is 0.2), so only the box's proportions matter, as orthographically. The eye must stay outside the
  sphere around the box ([[distance-problem]]); faces turn toward or away from the eye by their position too
  ([[face-orientation]], [[visible-edges]]).

  [[projector]] fits the projected box (plus a margin for axes) in a panel and gives pixel positions with depths;
  primitives are drawn far to near by depth (painter's algorithm, see [[cljplot2.graphics.space3d]]). Faces are
  shaded by Lambert lighting ([[shade]]); [[visible-edges]] - box edges not hidden behind it, [[axis-edges]] - the box
  edge each axis is drawn on.

  The 3D charts' shared frame options - [[frame-options]] (schema), [[frame-settings]] (a chart's frame, the options'
  defaults applied) and [[frame-overrides]] (the frame options a chart sets explicitly) - live here too."
  (:require [clojure2d.color :as c]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.data.accessor :as accessor]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec2 Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def default-view {:azimuth 40.0 :elevation 30.0})

(def view-schema
  "`:view {:azimuth deg :elevation deg :distance δ}` (elevation in [-90 90]; δ >= 0, see the namespace doc)."
  [:map {:closed true}
   (schema/optional :azimuth {:description "rotation around the vertical axis, degrees" :default 40.0} schema/finite)
   (schema/optional :elevation {:description "angle of the eye above the floor, degrees" :default 30.0}
                    [:and schema/finite [:>= -90] [:<= 90]])
   (schema/optional :distance {:description "perspective: 0 - parallel projection, larger - stronger perspective (lattice distance)"
                               :default 0.0}
                    schema/finite-non-neg)])

(def aspect-schema
  "`:aspect [ax ay az]` - positive box proportions."
  [:tuple schema/pos schema/pos schema/pos])

(def domains-schema
  "`:domain {:x [a b] :y [a b] :z [a b]}` - explicit data domains (each optional; `a > b` reverses the axis, e.g. depth
  growing downward)."
  (into [:map {:closed true}]
        (map #(schema/optional % {:description (str "explicit " (name %) " domain [a b] (a > b reverses the axis)")}
                               (schema/with-rules [:tuple schema/finite schema/finite]
                                   [(fn [[a b]] (when (== (double a) (double b)) "domain should be two different numbers"))])))
        [:x :y :z]))

(def frame-options
  "Option entries shared by the 3D charts: view, box proportions, frame, axis titles, explicit domains."
  (schema/in-fragment
   :frame-3d
   [(schema/optional :view {:description "the eye's position: azimuth, elevation and perspective distance"} view-schema)
    (schema/optional :aspect {:description "box proportions [ax ay az]" :default [1 1 1]} aspect-schema)
    (schema/nilable :box {:description "draw the box: hidden edges dashed and a floor grid behind the data, visible edges over it"
                          :default true}
                    schema/bool)
    (schema/nilable :axes {:description "draw ticks, tick labels and axis titles" :default true} schema/bool)
    (schema/nilable :axis-titles {:description "axis titles (nil - none)" :cljplot2/default-doc "the mapped column names"}
                    [:map {:closed true}
                     (schema/optional :x {:description "x axis title"} any?)
                     (schema/optional :y {:description "y axis title"} any?)
                     (schema/optional :z {:description "z axis title"} any?)])
    (schema/optional :domain {:description "explicit data domains (needed for each of several 3D charts in one plot)"
                              :cljplot2/default-doc "the data's domains"}
                     domains-schema)
    (schema/optional :reverse {:description "axes running the other way (mirrors the scene)"} [:set [:enum :x :y :z]])]))

(def axes [:x :y :z])

(defn box-aspect
  "Box proportions `aspect` as a vector; `nil` - the default cube `[1 1 1]`."
  [aspect]
  (vec (or aspect [1.0 1.0 1.0])))

(defn frame-settings
  "Frame of a 3D chart from its `options` and merged `mapping`: `{:view :aspect :box? :axes? :titles}` - titles from
  `:axis-titles`, else the names of keyword accessors."
  [options mapping]
  {:view (merge default-view (:view options))
   :aspect (box-aspect (:aspect options))
   :box? (not (false? (:box options)))
   :axes? (not (false? (:axes options)))
   :titles (if (contains? options :axis-titles)
             (:axis-titles options)
             (into {} (keep (fn [k] (let [a (accessor/unwrap (get mapping k))] (when (keyword? a) [k (name a)])))) axes))})

(defn frame-overrides
  "The frame settings a 3D chart's `options` set explicitly (keys of [[frame-settings]]; `:view` as given, `:titles`
  from `:axis-titles`): a later chart's are merged over the first chart's frame of a scene, see
  [[cljplot2.graphics.space3d/scene-frame]]."
  [options]
  (cond-> {}
    (:view options) (assoc :view (:view options))
    (:aspect options) (assoc :aspect (:aspect options))
    (some? (:box options)) (assoc :box? (:box options))
    (some? (:axes options)) (assoc :axes? (:axes options))
    (contains? options :axis-titles) (assoc :titles (:axis-titles options))))

(defn axis-index
  "Position of axis `k` (`:x` / `:y` / `:z`) in a point or a corner."
  ^long [k]
  (case k :x 0 :y 1 :z 2))

;; normalization and rotation

(defn widen
  "Domain `[lo hi]` of values; a single value `v` widens to `v ± 0.5` (`[v v]` can't be normalized)."
  [[lo hi]]
  (let [lo (double lo) hi (double hi)]
    (if (< lo hi) [lo hi] [(- lo 0.5) (+ hi 0.5)])))

(def ^:const nice-ticks
  "Tick count a data-derived domain is made nice for (see [[data-domain]])."
  5)

(defn nice-domain
  "`[lo hi]` (`lo` < `hi`) extended to the nearest ticks of about [[nice-ticks]] ticks over it (no extension limit),
  so its ends can be labelled."
  [[lo hi]]
  (:domain (scale/finalize (scale/scale-spec :x {:domain [lo hi] :nice 1.0}) [0.0 1.0] nice-ticks)))

(defn data-domain
  "Domain of the numbers `vs` widened by `reach` on both sides (a single value widened, see [[widen]]), made nice
  ([[nice-domain]]); `[0 1]` without values."
  [vs ^double reach]
  (if (seq vs)
    (let [[lo hi] (widen [(reduce min vs) (reduce max vs)])]
      (nice-domain [(- (double lo) reach) (+ (double hi) reach)]))
    [0.0 1.0]))

(defn reverse-domains
  "`domains` (`{:x [a b] ..}`) with the domain of every axis in `reversed` (a set of `:x` / `:y` / `:z`, `nil` -
  none) turned the other way: `[b a]` - the axis runs from its high end (a reversed domain, see [[normalizer]])."
  [domains reversed]
  (reduce (fn [ds k] (update ds k (comp vec reverse))) domains reversed))

(defn in-domain?
  "Is `v` within the explicit `domain` (`[a b]`, either direction; `nil` - none: everything is)?"
  [domain v]
  (or (nil? domain) (let [[a b] domain] (<= (m/min (double a) (double b)) (double v) (m/max (double a) (double b))))))

(defn normalizer
  "Function of a data point `[x y z]` giving its position in the box of side `aspect` centred at the origin
  (`domains` - `{:x [a b] ..}`, `a` at the low box side, `b` at the high one; `a` > `b` reverses the axis)."
  [domains aspect]
  (let [f (fn [k ^double a]
            (let [[lo hi] (domains k) lo (double lo) span (- (double hi) lo)]
              (fn ^double [^double value] (* a (- (/ (- value lo) span) 0.5)))))
        [fx fy fz] (map f axes aspect)]
    (fn [[x y z]] (Vec3. (fx (double x)) (fy (double y)) (fz (double z))))))

(defn rotation
  "Function of a normalized point (`Vec3`) giving its view coordinates `Vec3 (u v d)` for `view` (see the namespace
  doc): `u` right, `v` up, `d` depth (larger - farther)."
  [{:keys [azimuth elevation]}]
  (let [a (m/radians (double (or azimuth (:azimuth default-view))))
        e (m/radians (double (or elevation (:elevation default-view))))
        ca (m/cos a) sa (m/sin a) ce (m/cos e) se (m/sin e)]
    (fn [^Vec3 p]
      (let [x (.x p) y (.y p) z (.z p)
            u (+ (* x ca) (* y sa))
            d0 (- (* y ca) (* x sa))]
        (Vec3. u (+ (* z ce) (* d0 se)) (- (* d0 ce) (* z se)))))))

;; perspective

(defn- view-distance
  "Perspective `:distance` δ of `view` (0 - orthographic)."
  ^double [view]
  (double (or (:distance view) 0.0)))

(defn box-distance
  "Perspective `:distance` of `view` in the units of a box of side `aspect` (`nil` - a cube): δ over the box's
  longest side - lattice's `distance` applies to its box scaled to a longest side of 1. The eye sits at `1 /
  box-distance` from the box centre; 0 - orthographic."
  ^double [view aspect]
  (/ (view-distance view) (double (reduce max (box-aspect aspect)))))

(defn- half-diagonal
  "Half the diagonal of a box of side `aspect` - the radius of its circumscribed sphere."
  ^double [[ax ay az]]
  (* 0.5 (m/sqrt (+ (m/sq (double ax)) (m/sq (double ay)) (m/sq (double az))))))

(defn distance-problem
  "`nil` when the eye of `view` is outside the sphere circumscribing a box of side `aspect` (`nil` - a cube) - every
  point of the box, in any view, is then in front of the eye; else the error message (the largest `:distance` the box
  takes: its longest side over its half-diagonal, 2/√3 for a cube - any box takes a `:distance` below that)."
  [view aspect]
  (let [aspect (box-aspect aspect)]
    (when (>= (* (box-distance view aspect) (half-diagonal aspect)) 1.0)
      (format ":distance %s puts the eye within the sphere around the box of :aspect %s - it should be below %.4f"
              (:distance view) aspect (/ (double (reduce max aspect)) (half-diagonal aspect))))))

(defn check-distance!
  "Throw `ex-info {:key :distance}` for the eye of `view` within the sphere around a box of side `aspect` (see
  [[distance-problem]]; a chart's own options are checked by its schema - this checks the frame merged from several
  charts). Returns `nil`."
  [view aspect]
  (when-let [problem (distance-problem view aspect)]
    (schema/fail! (str "The 3D box: " problem ".") {:key :distance :in [:view :distance] :value (:distance view)})))

(defn perspective
  "Function of view coordinates `Vec3 (u v d)` giving the screen position `Vec2` (box units): `(u, v)` scaled by
  `1 / (1 + δ d)` (δ - [[box-distance]] of `view` and `aspect`); `(u, v)` as they are for δ = 0. Points must be in
  front of the eye (`1 + δ d > 0`, see [[distance-problem]])."
  [view aspect]
  (let [delta (box-distance view aspect)]
    (if (zero? delta)
      (fn [^Vec3 q] (v/vec2 (.x q) (.y q)))
      (fn [^Vec3 q] (let [s (/ 1.0 (+ 1.0 (* delta (.z q))))] (v/vec2 (* s (.x q)) (* s (.y q))))))))

;; box geometry - corners as [ix iy iz] bits, 0 low / 1 high

(def corners (vec (for [ix [0 1] iy [0 1] iz [0 1]] [ix iy iz])))

(defn corner-point
  "Normalized position of box corner `[ix iy iz]` in a box of side `aspect`."
  ^Vec3 [[ix iy iz] [ax ay az]]
  (Vec3. (* (double ax) (- (double ix) 0.5)) (* (double ay) (- (double iy) 0.5)) (* (double az) (- (double iz) 0.5))))

(def faces
  "The six box faces: `{:axis k :side 0|1 :corners [4 corners, in order around the face]}`."
  (vec (for [[i k] (map-indexed vector axes) side [0 1]]
         {:axis k :side side
          :corners (let [others (vec (remove #{i} [0 1 2]))
                         cycle-of [[0 0] [1 0] [1 1] [0 1]]]
                     (mapv (fn [[a b]] (assoc (assoc (assoc [0 0 0] i side) (others 0) a) (others 1) b)) cycle-of))})))

(def edges
  "The twelve box edges: `{:axis k :from corner :to corner}` (along axis `k`)."
  (vec (for [[i k] (map-indexed vector axes)
             a [0 1] b [0 1]
             :let [others (vec (remove #{i} [0 1 2]))
                   base (assoc (assoc [0 0 0] (others 0) a) (others 1) b)]]
         {:axis k :from (assoc base i 0) :to (assoc base i 1)})))

(defn- outward-normal
  "Rotated (by `rotate`) outward unit normal of the box face of `axis` / `side`."
  ^Vec3 [rotate axis side]
  (let [s (- (* 2.0 (double side)) 1.0)]
    (rotate (Vec3. (if (= axis :x) s 0.0) (if (= axis :y) s 0.0) (if (= axis :z) s 0.0)))))

(def ^:const ^:private facing-tolerance
  "A face turns toward (away from) the eye beyond this; within it - edge-on, neither."
  1.0e-9)

(defn- turn-away
  "How far a face of view-space outward unit normal `n` through the view-space point `c` turns from the eye at
  distance `delta` (see [[perspective]]): `n.z + δ (n · c)` - `-δ (eye - c) · n`, so negative toward the eye, positive
  away; `n.z` alone for δ = 0 (orthographic: the direction only)."
  ^double [^double delta ^Vec3 n ^Vec3 c]
  (+ (.z n) (* delta (v/dot n c))))

(defn- box-face-turn
  "Function of a box face `axis` / `side` of a box of side `aspect` (`nil` - a cube) giving how far it turns from the eye of `view`
  ([[turn-away]]; its centre is its outward normal times half the box side across it)."
  [view aspect]
  (let [rotate (rotation view)
        sides (box-aspect aspect)
        delta (box-distance view sides)]
    (fn ^double [axis side]
      (let [^Vec3 n (outward-normal rotate axis side)]
        (turn-away delta n (v/mult n (* 0.5 (double (nth sides (axis-index axis))))))))))

(defn face-orientation
  "Function of an axis-aligned face (of `axis`, data-low `side` 0 / data-high 1) through the data point `at` giving
  `[normal front?]` - its view-space outward unit normal and whether it faces the eye of the `frame` (`:domains`,
  `:aspect`, `:view`; a reversed domain turns its low side to the box's high side) - for boxes drawn inside the data
  box (bars)."
  [{:keys [domains aspect view]}]
  (let [rotate (rotation view)
        normalize (normalizer domains (box-aspect aspect))
        delta (box-distance view aspect)
        reversed (into {} (map (fn [k] (let [[a b] (domains k)] [k (> (double a) (double b))]))) axes)]
    (fn [axis side at]
      (let [^Vec3 n (outward-normal rotate axis (if (reversed axis) (- 1 (long side)) side))]
        [n (< (turn-away delta n (rotate (normalize at))) (- facing-tolerance))]))))

(defn far-side
  "Side (0 / 1) of the box face of `axis` turned away from the eye of `view` (box of side `aspect`) - the far wall
  across that axis; the farther-turned one when the eye is between both (perspective), `nil` when neither turns
  away (edge-on)."
  [view aspect axis]
  (let [turn (box-face-turn view aspect)
        low (double (turn axis 0)) high (double (turn axis 1))
        [side t] (if (>= high low) [1 high] [0 low])]
    (when (> (double t) facing-tolerance) side)))

(defn- edge-faces
  "The two faces `[axis side]` an `edge` belongs to."
  [{:keys [axis from]}]
  (vec (for [[i k] (map-indexed vector axes) :when (not= k axis)] [k (from i)])))

(defn- front-faces
  "Predicate of a box face `[axis side]` facing the eye of `view` (box of side `aspect`; edge-on faces count as back)."
  [view aspect]
  (let [turn (box-face-turn view aspect)]
    (fn [[k side]] (< (double (turn k side)) (- facing-tolerance)))))

(defn visible-edges
  "Box edges not hidden behind the box (side `aspect`) in `view`: those with a front-facing face (see [[faces]])."
  [view aspect]
  (let [front? (front-faces view aspect)]
    (filterv (fn [e] (some front? (edge-faces e))) edges)))

(defn hidden-edges
  "Box edges behind the box (side `aspect`) in `view` (both faces turned away): the complement of [[visible-edges]]."
  [view aspect]
  (let [shown (set (visible-edges view aspect))]
    (filterv (complement shown) edges)))

(defn axis-edges
  "Per axis the box edge `{:axis :from :to}` its ticks are drawn on in `view` (box of side `aspect`), given `screen` -
  a function of a corner giving its pixel `Vec2` (y down): x / y - the outline edge along that axis lowest on screen,
  z - the leftmost outline edge along z (any edge along the axis when none is on the outline, e.g. a face-on view).
  An outline edge has one face toward the eye, the other not."
  [view aspect screen]
  (let [front? (front-faces view aspect)
        silhouette? (fn [e] (let [[a b] (map front? (edge-faces e))] (not= a b)))
        mid (fn [{:keys [from to]}] (v/mult (v/add (screen from) (screen to)) 0.5))
        pick (fn [k better]
               (let [along (filter #(= k (:axis %)) edges)
                     outline (filter silhouette? along)]
                 (first (sort-by better (if (seq outline) outline along)))))]
    {:x (pick :x #(- (.y ^fastmath.vector.Vec2 (mid %))))
     :y (pick :y #(- (.y ^fastmath.vector.Vec2 (mid %))))
     :z (pick :z #(.x ^fastmath.vector.Vec2 (mid %)))}))

;; projection onto a panel

(defn turning-extent
  "`[width height]` (box units) of the largest projection of a box of side `aspect` over every azimuth at `elevation`
  (degrees): the floor diagonal `f` across, `az |cos e| + f |sin e|` up."
  [aspect elevation]
  (let [[ax ay az] aspect
        e (m/radians (double elevation))
        f (m/hypot (double ax) (double ay))]
    [f (+ (* (double az) (m/abs (m/cos e))) (* f (m/abs (m/sin e))))]))

(defn- corner-bounds
  "`[u0 u1 v0 v1]` (box units) of the screen positions of the corners of a box of side `aspect` in `view`."
  [aspect view]
  (let [rotate (rotation view)
        project (perspective view aspect)
        ps (map #(project (rotate (corner-point % aspect))) corners)
        us (map #(.x ^Vec2 %) ps) vs (map #(.y ^Vec2 %) ps)]
    [(reduce min us) (reduce max us) (reduce min vs) (reduce max vs)]))

(def ^:private turning-azimuths
  "Azimuths a perspective box's turning bounds are sampled at (every half degree)."
  (range 0.0 360.0 0.5))

(def ^:const ^:private turning-pad
  "Share a perspective box's sampled turning extent is widened by: more than the extent can grow between samples (a
  corner's screen position is smooth in the azimuth: about `h²/8` of the extent at a step `h` - 1e-5 at half a degree,
  more near the eye; the tests turn the box at fractional azimuths up to a `:distance` of 1.1)."
  1.0e-3)

(defn turning-fit
  "Extent and centre of a box of side `aspect` turned about its vertical axis at `view`'s elevation and distance:
  `{:width :height :u-mid :v-mid}` (box units) - a panel fitting `width` x `height` centred at `(u-mid, v-mid)`
  holds the box in every azimuth. Orthographic (δ = 0): [[turning-extent]], centred on `view`'s own corners (the box
  centre); perspective: the corners projected every half degree of azimuth - the largest `|u|` both ways, the `v`
  range (near corners below grow more than far ones above shrink) - widened by [[turning-pad]] for the azimuths
  between the samples."
  [aspect view]
  (if (zero? (view-distance view))
    (let [[tw th] (turning-extent aspect (get view :elevation (:elevation default-view)))
          [u0 u1 v0 v1] (corner-bounds aspect view)]
      {:width tw :height th
       :u-mid (* 0.5 (+ (double u0) (double u1))) :v-mid (* 0.5 (+ (double v0) (double v1)))})
    (let [bounds (map #(corner-bounds aspect (assoc view :azimuth %)) turning-azimuths)
          u (double (reduce max (mapcat (fn [[u0 u1]] [(m/abs (double u0)) (m/abs (double u1))]) bounds)))
          v0 (double (reduce min (map #(nth % 2) bounds)))
          v1 (double (reduce max (map #(nth % 3) bounds)))]
      {:width (* 2.0 u (+ 1.0 turning-pad)) :height (* (- v1 v0) (+ 1.0 turning-pad)) :u-mid 0.0 :v-mid (* 0.5 (+ v0 v1))})))

(defn projector
  "Projection of the data box (`domains`, `aspect`, `view`) fitted in a `w` x `h` panel with `margin` pixels on every
  side for axes: `{:point (fn [[x y z]] [Vec2 depth]) :corner (fn [corner] Vec2) :centre Vec2 :scale k}` - pixels
  per box unit `k` fitting the box's largest projection over all azimuths at the view's elevation (and distance, see
  [[turning-fit]]) in the panel less the margins, so a box turned about its vertical axis keeps its size and place
  (views of different elevations may differ); `:centre` - the box centre's pixel; pixel y grows downward; `depth` -
  the view depth (larger - farther). A corner may have fractional bits (a point on the box surface). A panel too
  small for the margins gets a scale of 0. Throws `{:key :distance}` for an eye within the sphere around the box
  ([[check-distance!]])."
  [{:keys [domains aspect view]} {:keys [w h margin]}]
  (let [aspect (box-aspect aspect)
        _ (check-distance! view aspect)
        normalize (normalizer domains aspect)
        rotate (rotation view)
        project (perspective view aspect)
        margin (double (or margin 0.0))
        free-w (- (double w) (* 2.0 margin)) free-h (- (double h) (* 2.0 margin))
        {:keys [width height u-mid v-mid]} (turning-fit aspect view)
        k (m/max 0.0 (m/min (/ free-w (double width)) (/ free-h (double height))))
        cx (* 0.5 (double w)) cy (* 0.5 (double h))
        um (double u-mid) vm (double v-mid)
        to-pixel (fn [^Vec2 q] (v/vec2 (+ cx (* k (- (.x q) um))) (- cy (* k (- (.y q) vm)))))]
    {:point (fn [p] (let [^Vec3 q (rotate (normalize p))] [(to-pixel (project q)) (.z q)]))
     :corner (fn [corner] (to-pixel (project (rotate (corner-point corner aspect)))))
     :centre (to-pixel (v/vec2 0.0 0.0))
     :scale k}))

;; shading

(def ^:private light
  "Direction toward the light in view coordinates: from the viewer's upper left."
  (v/normalize (Vec3. -0.5 0.6 -1.0)))

(defn view-normal
  "Unit normal (view coordinates) of the plane through normalized points `a`, `b`, `c` rotated by `view`; a zero
  vector for collinear points."
  [view a b c]
  (let [rotate (rotation view)
        ^Vec3 n (v/cross (v/sub (rotate b) (rotate a)) (v/sub (rotate c) (rotate a)))
        len (v/mag n)]
    (if (pos? len) (v/div n len) (Vec3. 0.0 0.0 0.0))))

(def ^:const ambient
  "Share of a shaded face's color that doesn't depend on the light."
  0.4)

(defn shade
  "`color` lit by Lambert's law for a face of unit `normal` (view coordinates; two-sided): its channels times
  `ambient + (1 - ambient) |n·l|`, alpha kept."
  [color normal]
  (let [k (+ ambient (* (- 1.0 ambient) (m/abs (double (v/dot normal light)))))
        col (c/to-color color)]
    (c/color (* k (c/red col)) (* k (c/green col)) (* k (c/blue col)) (c/alpha col))))
