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
  shaded by Lambert lighting ([[shader]], options [[shading-options]]); [[visible-edges]] - box edges not hidden behind it, [[axis-edges]] - the box
  edge each axis is drawn on.

  The 3D charts' shared frame options - [[frame-options]] (schema), [[frame-settings]] (a chart's frame, the options'
  defaults applied) and [[frame-overrides]] (the frame options a chart sets explicitly) - live here too, as do the
  shading options [[shading-options]] of the shaded charts.

  Functions: frame - [[frame-options]], [[frame-settings]], [[frame-overrides]], [[box-aspect]], [[axis-index]]; domains - [[data-domain]], [[nice-domain]], [[widen]], [[reverse-domains]], [[in-domain?]]; projection - [[normalizer]], [[view-matrix]], [[rotation]], [[perspective]], [[box-distance]], [[distance-problem]], [[check-distance!]], [[turning-extent]], [[turning-fit]], [[projector]]; box geometry - [[corners]], [[corner-point]], [[faces]], [[edges]], [[face-orientation]], [[far-side]], [[visible-edges]], [[hidden-edges]], [[axis-edges]]; shading - [[shading-options]], [[shader]]."
  (:require [clojure2d.color :as c]
            [fastmath.core :as m]
            [fastmath.matrix :as mat]
            [fastmath.vector :as v]
            [cljplot2.data.accessor :as accessor]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec2 Vec3 Vec4]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def default-view
  "The view of a 3D chart without `:view` keys: azimuth 40 and elevation 30 degrees, orthographic."
  {:azimuth 40.0 :elevation 30.0})

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
                                 [(fn [[^double a ^double b]] (when (m/== a b) "domain should be two different numbers"))])))
        [:x :y :z]))

(def frame-options
  "Option entries shared by the 3D charts: view, box proportions, frame, axis titles, explicit domains."
  (schema/in-fragment
   :frame-3d
   [(schema/optional :view {:cljplot2/section :view :description "the eye's position: azimuth, elevation and perspective distance"} view-schema)
    (schema/optional :aspect {:cljplot2/section :view :description "box proportions [ax ay az]" :default [1 1 1]} aspect-schema)
    (schema/nilable :box {:cljplot2/section :axes :description "draw the box: hidden edges dashed and a floor grid behind the data, visible edges over it"
                          :default true}
                    schema/bool)
    (schema/nilable :axes {:cljplot2/section :axes :description "draw ticks, tick labels and axis titles" :default true} schema/bool)
    (schema/nilable :axis-titles {:cljplot2/section :axes :description "axis titles (nil - none)" :cljplot2/default-doc "the mapped column names"}
                    [:map {:closed true}
                     (schema/optional :x {:description "x axis title"} any?)
                     (schema/optional :y {:description "y axis title"} any?)
                     (schema/optional :z {:description "z axis title"} any?)])
    (schema/optional :domain {:cljplot2/section :domain :description "explicit data domains (needed for each of several 3D charts in one plot)"
                              :cljplot2/default-doc "the data's domains"}
                     domains-schema)
    (schema/optional :reverse {:cljplot2/section :view :description "axes running the other way (mirrors the scene)"} [:set [:enum :x :y :z]])]))

(def axes
  "The 3D axes in point order."
  [:x :y :z])

(defn box-aspect
  "Gives the box proportions as a vector.

  Parameters:

  - `aspect` (sequence of three positive numbers or `nil`): `[ax ay az]`.

  Returns `aspect` as a vector; `nil` - the default cube `[1.0 1.0 1.0]`."
  [aspect]
  (vec (or aspect [1.0 1.0 1.0])))

(defn frame-settings
  "Gives the frame of a 3D chart, the frame options' defaults applied.

  Parameters:

  - `options` (map): the chart's options - `:view`, `:aspect`, `:box`, `:axes`, `:axis-titles` (see [[frame-options]]).
  - `mapping` (map): the merged mapping, `:x` `:y` `:z` read for default titles.

  Returns `{:view :aspect :box? :axes? :titles}`: `:view` merged over [[default-view]], `:aspect` of [[box-aspect]], `:box?` and `:axes?` true unless false, `:titles` - `:axis-titles` when given (even `nil`), else the names of keyword accessors.

  See also [[frame-overrides]]."
  [options mapping]
  {:view (merge default-view (:view options))
   :aspect (box-aspect (:aspect options))
   :box? (not (false? (:box options)))
   :axes? (not (false? (:axes options)))
   :titles (if (contains? options :axis-titles)
             (:axis-titles options)
             (into {} (keep (fn [k] (let [a (accessor/unwrap (get mapping k))] (when (keyword? a) [k (name a)])))) axes))})

(defn frame-overrides
  "Gives the frame settings a 3D chart sets explicitly.

  A later chart's overrides are merged over the first chart's frame of a scene, see [[cljplot2.graphics.space3d/scene-frame]].

  Parameters:

  - `options` (map): the chart's options.

  Returns a map with the keys of [[frame-settings]] the options set: `:view` as given, `:aspect`, `:box?`, `:axes?`, `:titles` from `:axis-titles`."
  [options]
  (cond-> {}
    (:view options) (assoc :view (:view options))
    (:aspect options) (assoc :aspect (:aspect options))
    (some? (:box options)) (assoc :box? (:box options))
    (some? (:axes options)) (assoc :axes? (:axes options))
    (contains? options :axis-titles) (assoc :titles (:axis-titles options))))

(defn axis-index
  "Gives the position of an axis in a point or a corner.

  Parameters:

  - `k` (keyword): `:x`, `:y` or `:z`.

  Returns 0, 1 or 2; throws `IllegalArgumentException` for another key."
  ^long [k]
  (case k :x 0 :y 1 :z 2))

;; normalization and rotation

(defn widen
  "Gives the domain of values made usable for normalization.

  Parameters:

  - `[lo hi]` (doubles): the values' range, `lo` <= `hi`.

  Returns `[lo hi]` when `lo` < `hi`; a single value `v` widens to `[v - 0.5, v + 0.5]` (`[v v]` can't be normalized)."
  [[^double lo ^double hi]]
  (if (m/< lo hi) [lo hi] [(m/- lo 0.5) (m/+ hi 0.5)]))

(def ^{:const true :tag 'long} nice-ticks
  "Tick count a data-derived domain is made nice for (see [[data-domain]])."
  5)

(defn nice-domain
  "Gives the domain extended to nice tick values.

  Parameters:

  - `[lo hi]`: the domain, `lo` < `hi`.

  Returns the domain extended to the nearest ticks of about [[nice-ticks]] ticks over it (no extension limit), so its ends can be labelled.

  See also [[data-domain]]."
  [[lo hi]]
  (:domain (scale/finalize (scale/scale-spec :x {:domain [lo hi] :nice 1.0}) [0.0 1.0] nice-ticks)))

(defn data-domain
  "Gives the nice domain of a 3D axis from its values.

  Parameters:

  - `vs` (sequence of numbers): the axis's values.
  - `reach` (double): data units the domain extends past the values on both sides (e.g. half a bar).

  Returns `[lo hi]`: the values' range (a single value widened, see [[widen]]) widened by `reach` and made nice ([[nice-domain]]); `[0.0 1.0]` without values."
  [vs ^double reach]
  (if (seq vs)
    (let [[^double lo ^double hi] (widen [(reduce m/min vs) (reduce m/max vs)])]
      (nice-domain [(m/- lo reach) (m/+ hi reach)]))
    [0.0 1.0]))

(defn reverse-domains
  "Gives the domains with some axes turned the other way.

  Parameters:

  - `domains` (map): `{:x [a b] :y [a b] :z [a b]}`.
  - `reversed` (set or `nil`): the axes to reverse, of `:x` `:y` `:z`; `nil` - none.

  Returns `domains` with each reversed axis's domain as `[b a]` - the axis runs from its high end (see [[normalizer]])."
  [domains reversed]
  (reduce (fn [ds k] (update ds k (comp vec reverse))) domains reversed))

(defn in-domain?
  "Tests whether a value lies within an explicit domain.

  Parameters:

  - `domain` (`[a b]` or `nil`): the domain, either direction; `nil` - none.
  - `v` (double): the value.

  Returns true when `v` is between `a` and `b` (ends included) or there is no domain, else false."
  [domain ^double v]
  (or (nil? domain) (let [[^double a ^double b] domain] (m/<= (m/min a b) v (m/max a b)))))

(defn normalizer
  "Gives the function placing data points in the normalized box.

  Parameters:

  - `domains` (map): `{:x [a b] :y [a b] :z [a b]}` - `a` at the low box side, `b` at the high one (`a` > `b` reverses the axis).
  - `aspect` (vector of three doubles): the box proportions.

  Returns a function of a data point `[x y z]` giving its position `Vec3` in the box of side `aspect` centred at the origin.

  See also [[rotation]]."
  [domains aspect]
  (let [f (fn [k ^double a]
            (let [[^double lo ^double hi] (domains k)
                  span (m/- hi lo)]
              (fn ^double [^double value] (m/* a (m/- (m// (m/- value lo) span) 0.5)))))
        [fx fy fz] (map f axes aspect)]
    (fn [[x y z]] (Vec3. (fx x) (fy y) (fz z)))))

(def ^:private depth-last
  "Permutation of the view rotation's `(u d v)` into `(u v d)` - screen right, screen up, depth."
  (mat/mat3x3 1.0 0.0 0.0 0.0 0.0 1.0 0.0 1.0 0.0))

(defn view-matrix
  "Gives the rotation matrix of `view` taking a normalized point to its view coordinates `(u v d)`.

  Parameters:

  - `view` (map): `:azimuth` - degrees about the vertical z axis (default 40), `:elevation` - degrees of the eye above the floor (default 30); other keys ignored.

  Returns a `Mat3x3`: the box turned by minus the azimuth about z, tilted by the elevation about the screen's right axis, then reordered to `(u v d)` - `u` right, `v` up, `d` depth (larger - farther).

  See also [[rotation]]."
  [view]
  (let [{:keys [^double azimuth ^double elevation]} (merge default-view view)]
    (mat/mulm depth-last
              (mat/mulm (mat/rotation-matrix-3d-x (m/radians elevation))
                        (mat/rotation-matrix-3d-z (m/- (m/radians azimuth)))))))

(defn rotation
  "Gives the function giving the view coordinates of a normalized point for `view` (see the namespace doc).

  Parameters:

  - `view` (map): `:azimuth`, `:elevation` in degrees (defaults 40 / 30), see [[view-matrix]].

  Returns a function of a `Vec3` point giving its view coordinates `Vec3 (u v d)`: `u` right, `v` up, `d` depth (larger - farther). The map is linear, so it also turns directions (normals)."
  [view]
  (let [rotation-matrix (view-matrix view)]
    (fn [p] (mat/mulv rotation-matrix p))))

;; perspective

(defn- view-distance
  "Gives the perspective distance of a view.

  Parameters:

  - `view` (map): `:distance` read.

  Returns δ as a double, 0 (orthographic) when absent."
  ^double [view]
  (or (:distance view) 0.0))

(defn box-distance
  "Gives the perspective distance of a view in the units of a box.

  Lattice's `distance` applies to its box scaled to a longest side of 1, so δ is divided by the box's longest side. The eye sits at `1 / box-distance` from the box centre.

  Parameters:

  - `view` (map): `:distance` read (default 0).
  - `aspect` (sequence of three positive numbers or `nil`): the box proportions; `nil` - a cube.

  Returns the distance as a double; 0 - orthographic."
  ^double [view aspect]
  (m// (view-distance view) (v/mx (box-aspect aspect))))

(defn- half-diagonal
  "Gives half the diagonal of a box - the radius of its circumscribed sphere.

  Parameters:

  - `aspect` (vector of three doubles): the box proportions.

  Returns the half-diagonal as a double."
  ^double [aspect]
  (m/* 0.5 (v/mag aspect)))

(defn distance-problem
  "Checks that the eye of a view stays outside the sphere circumscribing a box.

  Outside that sphere every point of the box, in any view, is in front of the eye. A box takes a `:distance` below its longest side over its half-diagonal (2/√3 for a cube).

  Parameters:

  - `view` (map): `:distance` read.
  - `aspect` (sequence of three positive numbers or `nil`): the box proportions; `nil` - a cube.

  Returns `nil` when the eye is outside, else the error message naming the largest distance the box takes.

  See also [[check-distance!]]."
  [view aspect]
  (let [sides (box-aspect aspect)]
    (when (m/>= (m/* (box-distance view sides) (half-diagonal sides)) 1.0)
      (format ":distance %s puts the eye within the sphere around the box of :aspect %s - it should be below %.4f"
              (:distance view) sides (m// (v/mx sides) (half-diagonal sides))))))

(defn check-distance!
  "Checks the perspective distance of a merged 3D frame.

  A chart's own options are checked by its schema; this checks the frame merged from several charts.

  Parameters:

  - `view` (map): `:distance` read.
  - `aspect` (sequence of three positive numbers or `nil`): the box proportions.

  Returns `nil`.

  Throws `ex-info {:key :distance}` for an eye within the sphere around the box (see [[distance-problem]])."
  [view aspect]
  (when-let [problem (distance-problem view aspect)]
    (schema/fail! (str "The 3D box: " problem ".") {:key :distance :in [:view :distance] :value (:distance view)})))

(defn perspective
  "Gives the function projecting view coordinates onto the screen plane.

  Parameters:

  - `view` (map): `:distance` read.
  - `aspect` (sequence of three positive numbers or `nil`): the box proportions.

  Returns a function of view coordinates `Vec3 (u v d)` giving the screen position `Vec2` in box units: `(u, v)` scaled by `1 / (1 + δ d)` (δ - [[box-distance]]), `(u, v)` as they are for δ = 0. Points must be in front of the eye (`1 + δ d > 0`, see [[distance-problem]])."
  [view aspect]
  (let [delta (box-distance view aspect)]
    (if (zero? delta)
      (fn [^Vec3 q] (v/vec2 (.x q) (.y q)))
      (fn [^Vec3 q] (let [s (m// (m/inc (m/* delta (.z q))))]
                     (v/vec2 (m/* s (.x q)) (m/* s (.y q))))))))

;; box geometry - corners as [ix iy iz] bits, 0 low / 1 high

(def corners
  "The eight box corners as `[ix iy iz]` bits, 0 low and 1 high."
  [[0 0 0] [0 0 1] [0 1 0] [0 1 1] [1 0 0] [1 0 1] [1 1 0] [1 1 1]])

(defn corner-point
  "Gives the normalized position of a box corner.

  Parameters:

  - `[ix iy iz]`: the corner's bits, 0 low and 1 high (fractions give points on the box surface).
  - `[ax ay az]`: the box proportions.

  Returns the corner's position `Vec3` in the box centred at the origin.

  See also [[corners]]."
  ^Vec3 [[^double ix ^double iy ^double iz] [^double ax ^double ay ^double az]]
  (Vec3. (m/* ax (m/- ix 0.5))
         (m/* ay (m/- iy 0.5))
         (m/* az (m/- iz 0.5))))

(def faces
  "The six box faces: `{:axis k :side 0|1 :corners [4 corners, in order around the face]}`."
  (vec (for [[i k] (map-indexed vector axes) side [0 1]]
         {:axis k :side side
          :corners (let [others (vec (remove #{i} [0 1 2]))
                         cycle-of [[0 0] [1 0] [1 1] [0 1]]]
                     (mapv (fn [[a b]] (-> [0 0 0]
                                          (assoc i side)
                                          (assoc (others 0) a)
                                          (assoc (others 1) b))) cycle-of))})))

(def edges
  "The twelve box edges: `{:axis k :from corner :to corner}` (along axis `k`)."
  (vec (for [[i k] (map-indexed vector axes)
             a [0 1] b [0 1]
             :let [others (vec (remove #{i} [0 1 2]))
                   base (-> [0 0 0]
                            (assoc (others 0) a)
                            (assoc (others 1) b))]]
         {:axis k :from (assoc base i 0) :to (assoc base i 1)})))

(defn- outward-normal
  "Gives the rotated outward unit normal of a box face.

  Parameters:

  - `rotate` (function): the view rotation of [[rotation]].
  - `axis` (keyword): the face's axis, `:x` `:y` or `:z`.
  - `side` (double): 0 - the low face, 1 - the high one.

  Returns the normal `Vec3` in view coordinates."
  ^Vec3 [rotate axis ^double side]
  (let [s (m/dec (m/* 2.0 side))]
    (rotate (Vec3. (if (= axis :x) s 0.0)
                   (if (= axis :y) s 0.0)
                   (if (= axis :z) s 0.0)))))

(def ^{:const true :private true :tag 'double} facing-tolerance
  "A face turns toward (away from) the eye beyond this; within it - edge-on, neither."
  1.0e-9)
(def ^{:const true :private true :tag 'double} -facing-tolerance (m/- facing-tolerance))

(defn- turn-away
  "Gives how far a face turns away from the eye.

  Parameters:

  - `delta` (double): the perspective distance in box units (see [[box-distance]]), 0 - orthographic.
  - `n` (`Vec3`): the face's outward unit normal in view coordinates.
  - `c` (`Vec3`): a point of the face in view coordinates.

  Returns `n.z + δ (n · c)` - proportional to `-(eye - c) · n`: negative toward the eye, positive away; `n.z` alone for δ = 0 (orthographic: the direction only)."
  ^double [^double delta ^Vec3 n ^Vec3 c]
  (m/+ (.z n) (m/* delta (v/dot n c))))

(defn- box-face-turn
  "Gives the function of the faces of a box giving how far each turns from the eye.

  Parameters:

  - `view` (map): the view.
  - `aspect` (sequence of three positive numbers or `nil`): the box proportions; `nil` - a cube.

  Returns a function of a face `axis` and `side` (0 or 1) giving [[turn-away]] of the face through its centre (its outward normal times half the box side across it)."
  [view aspect]
  (let [rotate (rotation view)
        sides (box-aspect aspect)
        delta (box-distance view sides)]
    (fn ^double [axis side]
      (let [^Vec3 n (outward-normal rotate axis side)]
        (turn-away delta n (v/mult n (m/* 0.5 (double (sides (axis-index axis))))))))))

(defn face-orientation
  "Gives the function orienting axis-aligned faces drawn inside the data box (bars).

  Parameters:

  - `frame` (map): `:domains`, `:aspect`, `:view`; a reversed domain turns its low side to the box's high side.

  Returns a function of `axis`, `side` (data-low 0, data-high 1) and a data point `at` on the face giving `[normal front?]` - the face's outward unit normal in view coordinates and whether it faces the eye."
  [{:keys [domains aspect view]}]
  (let [rotate (rotation view)
        normalize (normalizer domains (box-aspect aspect))
        delta (box-distance view aspect)
        reversed (into {} (map (fn [k] (let [[^double a ^double b] (domains k)] [k (m/> a b)]))) axes)]
    (fn [axis ^long side at]
      (let [^Vec3 n (outward-normal rotate axis (if (reversed axis) (m/long-sub 1 side) side))]
        [n (m/< (turn-away delta n (rotate (normalize at))) -facing-tolerance)]))))

(defn far-side
  "Gives the side of the box face across an axis turned away from the eye - the far wall.

  Parameters:

  - `view` (map): the view.
  - `aspect` (sequence of three positive numbers or `nil`): the box proportions.
  - `axis` (keyword): `:x`, `:y` or `:z`.

  Returns 0 or 1 (the farther-turned face when the eye is between both, in perspective); `nil` when neither turns away (edge-on)."
  [view aspect axis]
  (let [turn (box-face-turn view aspect)
        low (double (turn axis 0))
        high (double (turn axis 1))
        [side ^double t] (if (m/>= high low) [1 high] [0 low])]
    (when (m/> t facing-tolerance) side)))

(defn- edge-faces
  "Gives the two faces a box edge belongs to.

  Parameters:

  - `edge` (map): an edge of [[edges]], `:axis` and `:from` read.

  Returns a vector of two faces `[axis side]`."
  [{:keys [axis from]}]
  (vec (for [[i k] (map-indexed vector axes) :when (not= k axis)] [k (from i)])))

(defn- front-faces
  "Gives the predicate of the box faces facing the eye.

  Parameters:

  - `view` (map): the view.
  - `aspect` (sequence of three positive numbers or `nil`): the box proportions.

  Returns a function of a face `[axis side]`, truthy when it faces the eye; edge-on faces count as back."
  [view aspect]
  (let [turn (box-face-turn view aspect)]
    (fn [[k side]] (m/< (double (turn k side)) -facing-tolerance))))

(defn visible-edges
  "Gives the box edges not hidden behind the box.

  Parameters:

  - `view` (map): the view.
  - `aspect` (sequence of three positive numbers or `nil`): the box proportions.

  Returns a vector of the edges (see [[edges]]) with a face toward the eye.

  See also [[hidden-edges]]."
  [view aspect]
  (let [front? (front-faces view aspect)]
    (filterv (fn [e] (some front? (edge-faces e))) edges)))

(defn hidden-edges
  "Gives the box edges behind the box (both their faces turned away).

  Parameters:

  - `view` (map): the view.
  - `aspect` (sequence of three positive numbers or `nil`): the box proportions.

  Returns a vector of edges, the complement of [[visible-edges]]."
  [view aspect]
  (let [shown (set (visible-edges view aspect))]
    (filterv (complement shown) edges)))

(defn axis-edges
  "Gives the box edge each axis's ticks are drawn on.

  An outline edge has one face toward the eye, the other not.

  Parameters:

  - `view` (map): the view.
  - `aspect` (sequence of three positive numbers or `nil`): the box proportions.
  - `screen` (function): of a corner giving its pixel `Vec2` (y down).

  Returns `{:x edge :y edge :z edge}` (edges `{:axis :from :to}`): x and y - the outline edge along that axis lowest on screen, z - the leftmost outline edge along z; any edge along the axis when none is on the outline (e.g. a face-on view)."
  [view aspect screen]
  (let [front? (front-faces view aspect)
        silhouette? (fn [e] (let [[a b] (map front? (edge-faces e))] (not= a b)))
        mid (fn [{:keys [from to]}] (v/mult (v/add (screen from) (screen to)) 0.5))
        pick (fn [k better]
               (let [along (filter #(= k (:axis %)) edges)
                     outline (filter silhouette? along)]
                 (first (sort-by better (if (seq outline) outline along)))))]
    {:x (pick :x #(m/- (.y ^Vec2 (mid %))))
     :y (pick :y #(m/- (.y ^Vec2 (mid %))))
     :z (pick :z #(.x ^Vec2 (mid %)))}))

;; projection onto a panel

(defn turning-extent
  "Gives the largest orthographic projection of a box over every azimuth.

  Parameters:

  - `[ax ay az]` (doubles): the box proportions.
  - `elevation` (double): the view's elevation, degrees.

  Returns `[width height]` in box units: the floor diagonal `f` across, `az |cos e| + f |sin e|` up.

  See also [[turning-fit]]."
  [[^double ax ^double ay ^double az] ^double elevation]
  (let [e (m/radians elevation)
        f (m/hypot ax ay)]
    [f (m/+ (m/* az (m/abs (m/cos e))) (m/* f (m/abs (m/sin e))))]))

(defn- corner-bounds
  "Gives the screen bounds of the corners of a box in a view.

  Parameters:

  - `aspect` (vector of three doubles): the box proportions.
  - `view` (map): the view.

  Returns `[u0 u1 v0 v1]` in box units - the lowest and highest screen positions of the projected corners."
  [aspect view]
  (let [rotate (rotation view)
        project (perspective view aspect)
        ps (map #(project (rotate (corner-point % aspect))) corners)
        us (map #(.x ^Vec2 %) ps) vs (map #(.y ^Vec2 %) ps)]
    [(v/mn us) (v/mx us) (v/mn vs) (v/mx vs)]))

(def ^:private turning-azimuths
  "Azimuths a perspective box's turning bounds are sampled at (every half degree)."
  (range 0.0 360.0 0.5))

(def ^{:const true :private true :tag 'double} turning-pad
  "Share a perspective box's sampled turning extent is widened by: more than the extent can grow between samples (a corner's screen position is smooth in the azimuth: about `h²/8` of the extent at a step `h` - 1e-5 at half a degree, more near the eye; the tests turn the box at fractional azimuths up to a `:distance` of 1.1)."
  1.0e-3)
(def ^{:const true :private true :tag 'double} turning-pad+ (m/inc turning-pad))


(defn turning-fit
  "Gives the extent and centre of a box turned about its vertical axis at a view's elevation and distance.

  A panel fitting the extent, centred at the centre, holds the box in every azimuth. Orthographic (δ = 0): [[turning-extent]], centred on the view's own corners (the box centre). Perspective: the corners projected every half degree of azimuth - the largest `|u|` both ways, the `v` range (near corners below grow more than far ones above shrink) - widened by [[turning-pad]] for the azimuths between the samples.

  Parameters:

  - `aspect` (vector of three doubles): the box proportions.
  - `view` (map): `:elevation` (default 30), `:distance`; `:azimuth` used for the orthographic centre.

  Returns `{:width :height :u-mid :v-mid}` in box units."
  [aspect view]
  (if (zero? (view-distance view))
    (let [[tw th] (turning-extent aspect (get view :elevation (:elevation default-view)))
          [^double u0 ^double u1 ^double v0 ^double v1] (corner-bounds aspect view)]
      {:width tw :height th
       :u-mid (m/* 0.5 (m/+ u0 u1))
       :v-mid (m/* 0.5 (m/+ v0 v1))})
    (let [bounds (map #(corner-bounds aspect (assoc view :azimuth %)) turning-azimuths)
          u (v/mx (mapcat (fn [[^double lu0 ^double lu1]] [(m/abs lu0) (m/abs lu1)]) bounds))
          v0 (v/mn (map #(nth % 2) bounds))
          v1 (v/mx (map #(nth % 3) bounds))]
      {:width (m/* 2.0 u turning-pad+)
       :height (m/* (m/- v1 v0) turning-pad+)
       :u-mid 0.0
       :v-mid (m/* 0.5 (m/+ v0 v1))})))

(defn- side-margins
  "Gives the margin of each side of a panel.

  Parameters:

  - `margin`: pixels for every side (a number), `{:top :right :bottom :left}` (each in pixels, a missing side 0) or `nil` (none).

  Returns `{:top :right :bottom :left}` of doubles."
  [margin]
  (if (map? margin)
    (into {} (map (fn [k] [k (double (get margin k 0.0))])) [:top :right :bottom :left])
    (let [pad (double (or margin 0.0))] {:top pad :right pad :bottom pad :left pad})))

(defn projector
  "Gives the projection of the data box fitted in a panel.

  The scale fits the box's largest projection over all azimuths at the view's elevation (and distance, see [[turning-fit]]) in the panel less the margins, so a box turned about its vertical axis keeps its size and place (views of different elevations may differ).

  Parameters:

  - `frame` (map): `:domains` (see [[normalizer]]), `:aspect`, `:view`.
  - `panel` (map): `:w`, `:h` - the panel size in pixels; `:margin` - pixels kept free around the box for axes: a number for every side or `{:top :right :bottom :left}` (default 0).

  Returns `{:point :corner :centre :scale}`: `:point` - a function of a data point `[x y z]` giving `[pixel depth]` (`pixel` a `Vec2`, y down; `depth` - the view depth, larger - farther); `:corner` - a function of a corner (fractional bits - a point on the box surface) giving its pixel; `:centre` - the box centre's pixel; `:scale` - pixels per box unit, 0 for a panel too small for the margins.

  Throws `ex-info {:key :distance}` for an eye within the sphere around the box ([[check-distance!]])."
  [{:keys [domains aspect view]} {:keys [^double w ^double h margin]}]
  (let [sides (box-aspect aspect)
        _ (check-distance! view sides)
        normalize (normalizer domains sides)
        rotate (rotation view)
        project (perspective view sides)
        {:keys [^double top ^double right ^double bottom ^double left]} (side-margins margin)
        free-w (m/- w left right)
        free-h (m/- h top bottom)
        {:keys [^double width ^double height ^double u-mid ^double v-mid]} (turning-fit sides view)
        k (m/max 0.0 (m/min (m// free-w width) (m// free-h height)))
        cx (m/+ left (m/* 0.5 free-w))
        cy (m/+ top (m/* 0.5 free-h))
        to-pixel (fn [^Vec2 q] (v/vec2 (m/+ cx (m/* k (m/- (.x q) u-mid)))
                                      (m/- cy (m/* k (m/- (.y q) v-mid)))))]
    {:point (fn [p] (let [^Vec3 q (rotate (normalize p))] [(to-pixel (project q)) (.z q)]))
     :corner (fn [corner] (to-pixel (project (rotate (corner-point corner sides)))))
     :centre (to-pixel (v/vec2 0.0 0.0))
     :scale k}))

;; shading

(def ^:private default-light
  "Direction toward the light of [[shading-options]] by default (view coordinates): from the viewer's upper left."
  [-0.5 0.6 -1.0])

(def ^{:const true :private true :tag 'double} default-ambient
  "Share of a shaded face's colour independent of the light, by default."
  0.4)

(def light-schema
  "Schema of the `:light` direction `[x y z]` (view coordinates): finite, not all zero."
  (schema/with-rules [:tuple schema/finite schema/finite schema/finite]
    [(fn [l] (when (every? zero? l) "the light direction should not be a zero vector"))]))

(def shading-options
  "Option entries shared by the shaded 3D charts (`surface`, `cloud` bars): the ambient share and the light direction of Lambert shading (see [[shader]])."
  (schema/in-fragment
   :shading
   [(schema/optional :ambient {:description "share of a shaded face's colour independent of the light (0 - light only, 1 - no shading)"
                               :default default-ambient}
                     schema/fraction)
    (schema/optional :light {:description "direction toward the light [x y z] in view coordinates (x right, y up, z away from the eye), normalized"
                             :default default-light}
                     light-schema)]))

(defn- light-direction
  "Gives the unit direction toward the light from `light` (`[x y z]`, view coordinates).

  Throws `ex-info {:key :light}` for a zero vector."
  ^Vec3 [[^double x ^double y ^double z :as light]]
  (let [l (Vec3. x y z)]
    (when (v/is-zero? l)
      (schema/fail! "The light direction should not be a zero vector." {:key :light :value light}))
    (v/normalize l)))

(defn shader
  "Gives the Lambert shading function of a 3D chart's shading options.

  Parameters:

  - `options` (map): `:ambient` - share of a face's colour independent of the light, in `[0 1]` (default 0.4); `:light` - direction toward the light `[x y z]` in view coordinates (default `[-0.5 0.6 -1.0]`, from the viewer's upper left), normalized.

  Returns a function of `color` and a face's unit `normal` (view coordinates) giving the colour lit two-sided: its RGB channels times `ambient + (1 - ambient) |n·l|`, alpha kept. A zero normal gives the ambient share alone; `:ambient` 1 - the colour itself.

  Throws `ex-info {:key :light}` for a zero `:light`.

  See also [[shading-options]]."
  [{:keys [^double ambient light] :or {ambient default-ambient light default-light}}]
  (let [diffuse (m/- 1.0 ambient)
        l (light-direction light)]
    (fn [color normal]
      (let [k (m/+ ambient (m/* diffuse (m/abs (v/dot normal l))))
            ^Vec4 col (c/to-color color)]
        (c/color (m/* k (.x col)) (m/* k (.y col)) (m/* k (.z col)) (.w col))))))
