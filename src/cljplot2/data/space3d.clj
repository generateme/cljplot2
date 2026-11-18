(ns cljplot2.data.space3d
  "3D space of the 3D charts (`chart/cloud`, `chart/surface`): orthographic projection of a data box onto a panel.

  Data coordinates are normalized per axis into a box of side `aspect` (`[ax ay az]`, default `[1 1 1]`) centred at the
  origin, then rotated to the view `{:azimuth a :elevation e}` (degrees; default 40 / 30): screen right `u`, screen up
  `v`, depth `d` (away from the viewer). Azimuth 0, elevation 0 looks along `+y`: x to the right, z up; azimuth turns
  the box about the vertical z axis (90 - y to the right), elevation tilts the view from above (positive) or below.

  [[projector]] fits the projected box (plus a margin for axes) in a panel and gives pixel positions with depths;
  primitives are drawn far to near by depth (painter's algorithm, see [[cljplot2.graphics.space3d]]). Faces are
  shaded by Lambert lighting ([[shade]]); [[visible-edges]] - box edges not hidden behind it, [[axis-edges]] - the box
  edge each axis is drawn on."
  (:require [clojure2d.color :as c]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def default-view {:azimuth 40.0 :elevation 30.0})

(def view-schema
  "`:view {:azimuth deg :elevation deg}` (elevation in [-90 90])."
  [:map {:closed true}
   (schema/optional :azimuth schema/finite)
   (schema/optional :elevation [:and schema/finite [:>= -90] [:<= 90]])])

(def aspect-schema
  "`:aspect [ax ay az]` - positive box proportions."
  [:tuple schema/pos schema/pos schema/pos])

(def domains-schema
  "`:domain {:x [a b] :y [a b] :z [a b]}` - explicit data domains (each optional; `a > b` reverses the axis, e.g. depth
  growing downward)."
  (into [:map {:closed true}]
        (map #(schema/optional % (schema/with-rules [:tuple schema/finite schema/finite]
                                   [(fn [[a b]] (when (== (double a) (double b)) "domain should be two different numbers"))])))
        [:x :y :z]))

(def axes [:x :y :z])

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

(defn- front-facing?
  "Does the face of `axis` / `side` look toward the viewer (its outward normal has a negative depth, beyond a
  tolerance - edge-on faces count as back)?"
  [rotate axis side]
  (< (.z (outward-normal rotate axis side)) -1.0e-9))

(defn face-normal
  "View-space outward unit normal of an axis-aligned face (of `axis`, low `side` 0 / high 1) in `view`, and whether
  it faces the viewer: `[normal front?]` - for boxes drawn inside the data box (bars)."
  [view axis side]
  (let [n (outward-normal (rotation view) axis side)]
    [n (< (.z n) -1.0e-9)]))

(defn far-side
  "Side (0 / 1) of the box face of `axis` turned away from the viewer in `view` - the far wall across that axis, `nil`
  when both are edge-on."
  [view axis]
  (first (filter #(> (.z ^Vec3 (first (face-normal view axis %))) 1.0e-9) [0 1])))

(defn- edge-faces
  "The two faces `[axis side]` an `edge` belongs to."
  [{:keys [axis from]}]
  (vec (for [[i k] (map-indexed vector axes) :when (not= k axis)] [k (from i)])))

(defn visible-edges
  "Box edges not hidden behind the box in `view`: those with a front-facing face (see [[faces]])."
  [view]
  (let [rotate (rotation view)]
    (filterv (fn [e] (some (fn [[k side]] (front-facing? rotate k side)) (edge-faces e))) edges)))

(defn hidden-edges
  "Box edges behind the box in `view` (both faces turned away): the complement of [[visible-edges]]."
  [view]
  (let [shown (set (visible-edges view))]
    (filterv (complement shown) edges)))

(defn- silhouette?
  "Is `edge` on the box outline in `view`: one of its faces front-facing, the other not?"
  [rotate edge]
  (let [[a b] (map (fn [[k side]] (front-facing? rotate k side)) (edge-faces edge))]
    (not= a b)))

(defn axis-edges
  "Per axis the box edge `{:axis :from :to}` its ticks are drawn on, given `screen` - a function of a corner giving
  its pixel `Vec2` (y down): x / y - the outline edge along that axis lowest on screen, z - the leftmost outline edge
  along z (any edge along the axis when none is on the outline, e.g. a face-on view)."
  [view screen]
  (let [rotate (rotation view)
        mid (fn [{:keys [from to]}] (v/mult (v/add (screen from) (screen to)) 0.5))
        pick (fn [k better]
               (let [along (filter #(= k (:axis %)) edges)
                     outline (filter #(silhouette? rotate %) along)]
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

(defn projector
  "Projection of the data box (`domains`, `aspect`, `view`) fitted in a `w` x `h` panel with `margin` pixels on every
  side for axes: `{:point (fn [[x y z]] [Vec2 depth]) :corner (fn [corner] Vec2) :centre Vec2 :scale k}` - pixels
  per box unit `k` fitting the box's largest projection over all azimuths at the view's elevation (see
  [[turning-extent]]) in the panel less the margins, so a box turned about its vertical axis keeps its size (views of
  different elevations may differ); the projected box centred in the panel; pixel y grows downward. A panel too
  small for the margins gets a scale of 0."
  [{:keys [domains aspect view]} {:keys [w h margin]}]
  (let [aspect (vec (or aspect [1.0 1.0 1.0]))
        normalize (normalizer domains aspect)
        rotate (rotation view)
        projected (map #(rotate (corner-point % aspect)) corners)
        us (map #(.x ^Vec3 %) projected) vs (map #(.y ^Vec3 %) projected)
        u0 (double (reduce min us)) u1 (double (reduce max us))
        v0 (double (reduce min vs)) v1 (double (reduce max vs))
        margin (double (or margin 0.0))
        free-w (- (double w) (* 2.0 margin)) free-h (- (double h) (* 2.0 margin))
        [tw th] (turning-extent aspect (get view :elevation (:elevation default-view)))
        k (m/max 0.0 (m/min (/ free-w (double tw)) (/ free-h (double th))))
        cx (* 0.5 (double w)) cy (* 0.5 (double h))
        um (* 0.5 (+ u0 u1)) vm (* 0.5 (+ v0 v1))
        to-pixel (fn [^Vec3 q] (v/vec2 (+ cx (* k (- (.x q) um))) (- cy (* k (- (.y q) vm)))))]
    {:point (fn [p] (let [^Vec3 q (rotate (normalize p))] [(to-pixel q) (.z q)]))
     :corner (fn [corner] (to-pixel (rotate (corner-point corner aspect))))
     :centre (v/vec2 cx cy)
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
