(ns cljplot2.layout.venn
  "Venn diagram geometry in layout units (y down, as pixels): circles of 2 or 3 sets and a label point per region.

  * 2 sets, proportional ([[two-sets]]): circle areas equal the set sizes, the centre distance makes the lens area
    equal the intersection (solved with `fastmath.solver/find-root`); a subset touches its superset from inside,
    disjoint sets keep a small gap. Not proportional: two unit circles, each centre on the other's edge.
  * 3 sets ([[three-sets]]): the classic layout - unit circles around a common centre (upper left, upper right,
    bottom), not proportional.

  Regions are keyed by the set of circle indices they lie in (`#{0}`, `#{0 1}`, ...).

  Functions: layouts [[two-sets]], [[three-sets]]; geometry [[lens-area]], [[region-points]]."
  (:require [fastmath.core :as m]
            [fastmath.solver :as solver]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn lens-area
  "Gives the area of the intersection of two circles.

  Parameters:

  - `d` (double): the centre distance.
  - `r1`, `r2` (doubles): the radii.

  Returns the lens area: 0 for circles apart (`d >= r1 + r2`), the smaller circle's area when one holds the other."
  ^double [^double d ^double r1 ^double r2]
  (cond
    (m/>= d (m/+ r1 r2)) 0.0
    (m/<= d (m/abs (m/- r1 r2))) (m/* m/PI (m/sq (m/min r1 r2)))
    :else (let [a1 (m/acos (m/constrain (m// (m/+ (m/* d d) (m/* r1 r1) (m/- (m/* r2 r2))) (m/* 2.0 d r1)) -1.0 1.0))
                a2 (m/acos (m/constrain (m// (m/+ (m/* d d) (m/* r2 r2) (m/- (m/* r1 r1))) (m/* 2.0 d r2)) -1.0 1.0))
                k (m/* (m/+ (m/- d) r1 r2) (m/+ d r1 (m/- r2)) (m/+ d (m/- r1) r2) (m/+ d r1 r2))]
            (m/- (m/+ (m/* r1 r1 a1) (m/* r2 r2 a2)) (m/* 0.5 (m/sqrt (m/max 0.0 k)))))))

(def ^{:private true :const true :tag 'double} disjoint-gap
  "Gap between disjoint circles, a fraction of the smaller radius."
  0.1)

(defn- centre-distance
  "Gives the centre distance of two circles whose lens area is `overlap` (see the namespace doc).

  Parameters:

  - `r1`, `r2` (doubles): the radii.
  - `overlap` (double): the wanted lens area.

  Returns the distance: `r1 + r2` for a zero radius; disjoint circles a small gap apart (see [[disjoint-gap]]) for no overlap; `|r1 - r2|` (the smaller touching the larger from inside) for an overlap of the smaller circle's area or more."
  ^double [^double r1 ^double r2 ^double overlap]
  (cond
    (or (m/zero? r1) (m/zero? r2)) (m/+ r1 r2)
    (m/<= overlap 0.0) (m/* (m/+ r1 r2) (m/+ 1.0 (m/* disjoint-gap (m// (m/min r1 r2) (m/+ r1 r2)))))
    (m/>= overlap (m/* m/PI (m/sq (m/min r1 r2)))) (m/abs (m/- r1 r2))
    :else (solver/find-root #(m/- (lens-area (double %) r1 r2) overlap) (m/abs (m/- r1 r2)) (m/+ r1 r2))))

(defn two-sets
  "Gives the circles of two sets.

  Parameters:

  - `size-a`, `size-b` (numbers): the sets' sizes.
  - `overlap` (number): the items in both, non-negative, at most the smaller size.
  - `proportional?`: truthy - circle areas and the lens area proportional to the sizes; falsy - unit circles one radius apart.

  Returns `[{:x :y :r} {:x :y :r}]`: the first circle on the left, both on y = 0."
  [size-a size-b overlap proportional?]
  (if proportional?
    (let [r1 (m/sqrt (m// (double size-a) m/PI)) r2 (m/sqrt (m// (double size-b) m/PI))
          d (centre-distance r1 r2 (double overlap))]
      [{:x 0.0 :y 0.0 :r r1} {:x d :y 0.0 :r r2}])
    [{:x 0.0 :y 0.0 :r 1.0} {:x 1.0 :y 0.0 :r 1.0}]))

(def ^{:private true :const true :tag 'double} three-set-offset
  "Distance of the three unit circles' centres from the common centre."
  0.65)

(def ^:private three-set-directions
  "Unit directions of the three circles from the common centre: upper left, upper right, bottom."
  [[(m/- (m/cos (m/radians 30.0))) -0.5] [(m/cos (m/radians 30.0)) -0.5] [0.0 1.0]])

(def ^{:private true :const true :tag 'double} single-label-distance
  "Distance of a three-set layout's single-set label points from the common centre."
  1.1)

(def ^{:private true :const true :tag 'double} pair-label-distance
  "Distance of a three-set layout's pair label points from the common centre (opposite the third circle)."
  0.75)

(def ^{:private true :const true :tag 'double} containment-tolerance
  "Relative slack of [[contains-circle?]] for rounding."
  1e-9)

(defn three-sets
  "Gives the circles `[{:x :y :r} ..]` of the classic three-set layout (see the namespace doc), around the origin."
  []
  (mapv (fn [[^double dx ^double dy]] {:x (m/* three-set-offset dx) :y (m/* three-set-offset dy) :r 1.0}) three-set-directions))

(defn- contains-circle?
  "Tests whether a circle holds another, touching from inside included (up to [[containment-tolerance]]).

  Parameters:

  - `a`, `b` (maps): circles `{:x :y :r}`.

  Returns true when `a` holds `b`, else false."
  [{^double ax :x ^double ay :y ^double ar :r} {^double bx :x ^double by :y ^double br :r}]
  (m/<= (m/+ (m/hypot (m/- bx ax) (m/- by ay)) br) (m/* ar (m/+ 1.0 containment-tolerance))))

(defn region-points
  "Gives a label point for every region of a Venn layout that exists in it.

  Parameters:

  - `circles` (vector): 2 or 3 circles, as [[two-sets]] / [[three-sets]] give them.

  Returns `{region [x y]}` (regions keyed by sets of circle indices): 2 sets - midpoints of the region's stretch of the centre line (the widest part), a region missing when a circle is empty or held by the other, the intersection when the circles overlap; 3 sets - fixed points of the classic layout."
  [circles]
  (if (m/== 3 (count circles))
    (into {#{0 1 2} [0.0 0.0]}
          (concat (for [i (range 3) :let [[^double dx ^double dy] (three-set-directions i)]]
                    [#{i} [(m/* single-label-distance dx) (m/* single-label-distance dy)]])
                  (for [i (range 3) j (range (m/long-inc (long i)) 3)
                        :let [[^double dx ^double dy] (three-set-directions (first (remove #{i j} (range 3))))]]
                    [#{i j} [(m/* pair-label-distance -1.0 dx) (m/* pair-label-distance -1.0 dy)]])))
    (let [[{^double x1 :x ^double r1 :r :as a} {^double x2 :x ^double r2 :r :as b}] circles
          mid (fn [^double p ^double q] [(m/* 0.5 (m/+ p q)) 0.0])
          a-left (m/- x1 r1) a-right (m/+ x1 r1) b-left (m/- x2 r2) b-right (m/+ x2 r2)]
      (cond-> {}
        (and (m/pos? r1) (not (contains-circle? b a))) (assoc #{0} (mid a-left (m/min b-left a-right)))
        (and (m/pos? r2) (not (contains-circle? a b))) (assoc #{1} (mid (m/max a-right b-left) b-right))
        (and (m/pos? r1) (m/pos? r2) (m/< (m/- x2 x1) (m/+ r1 r2))) (assoc #{0 1} (mid (m/max a-left b-left) (m/min a-right b-right)))))))
