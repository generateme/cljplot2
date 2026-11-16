(ns cljplot2.layout.venn
  "Venn diagram geometry in layout units (y down, as pixels): circles of 2 or 3 sets and a label point per region.

  * 2 sets, proportional ([[two-sets]]): circle areas equal the set sizes, the centre distance makes the lens area
    equal the intersection (solved with `fastmath.solver/find-root`); a subset touches its superset from inside,
    disjoint sets keep a small gap. Not proportional: two unit circles, each centre on the other's edge.
  * 3 sets ([[three-sets]]): the classic layout - unit circles around a common centre (upper left, upper right,
    bottom), not proportional.

  Regions are keyed by the set of circle indices they lie in (`#{0}`, `#{0 1}`, ...)."
  (:require [fastmath.core :as m]
            [fastmath.solver :as solver]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn lens-area
  "Area of the intersection of circles with radii `r1`, `r2` at centre distance `d`."
  ^double [^double d ^double r1 ^double r2]
  (cond
    (>= d (+ r1 r2)) 0.0
    (<= d (m/abs (- r1 r2))) (* m/PI (m/sq (m/min r1 r2)))
    :else (let [a1 (m/acos (m/constrain (/ (+ (* d d) (* r1 r1) (- (* r2 r2))) (* 2.0 d r1)) -1.0 1.0))
                a2 (m/acos (m/constrain (/ (+ (* d d) (* r2 r2) (- (* r1 r1))) (* 2.0 d r2)) -1.0 1.0))
                k (* (+ (- d) r1 r2) (+ d r1 (- r2)) (+ d (- r1) r2) (+ d r1 r2))]
            (- (+ (* r1 r1 a1) (* r2 r2 a2)) (* 0.5 (m/sqrt (m/max 0.0 k)))))))

(def ^:private ^:const disjoint-gap
  "Gap between disjoint circles, a fraction of the smaller radius."
  0.1)

(defn- centre-distance
  "Centre distance of circles `r1`, `r2` whose lens area is `overlap` (see the namespace doc)."
  ^double [^double r1 ^double r2 ^double overlap]
  (cond
    (or (zero? r1) (zero? r2)) (+ r1 r2)
    (<= overlap 0.0) (* (+ r1 r2) (+ 1.0 (* disjoint-gap (/ (m/min r1 r2) (+ r1 r2)))))
    (>= overlap (* m/PI (m/sq (m/min r1 r2)))) (m/abs (- r1 r2))
    :else (solver/find-root #(- (lens-area (double %) r1 r2) overlap) (m/abs (- r1 r2)) (+ r1 r2))))

(defn two-sets
  "Circles `[{:x :y :r} {:x :y :r}]` of sets with `size-a`, `size-b` items, `overlap` of them in both (non-negative,
  at most the smaller size): areas and lens area proportional, or (`proportional?` false) unit circles one radius
  apart. The first circle on the left, both on y = 0."
  [size-a size-b overlap proportional?]
  (if proportional?
    (let [r1 (m/sqrt (/ (double size-a) m/PI)) r2 (m/sqrt (/ (double size-b) m/PI))
          d (centre-distance r1 r2 (double overlap))]
      [{:x 0.0 :y 0.0 :r r1} {:x d :y 0.0 :r r2}])
    [{:x 0.0 :y 0.0 :r 1.0} {:x 1.0 :y 0.0 :r 1.0}]))

(def ^:private ^:const three-set-offset
  "Distance of the three unit circles' centres from the common centre."
  0.65)

(def ^:private three-set-directions
  "Unit directions of the three circles from the common centre: upper left, upper right, bottom."
  [[(- (m/cos (m/radians 30.0))) -0.5] [(m/cos (m/radians 30.0)) -0.5] [0.0 1.0]])

(defn three-sets
  "Circles of the classic three-set layout (see the namespace doc), around the origin."
  []
  (mapv (fn [[dx dy]] {:x (* three-set-offset (double dx)) :y (* three-set-offset (double dy)) :r 1.0}) three-set-directions))

(defn- contains-circle?
  [{ax :x ay :y ar :r} {bx :x by :y br :r}]
  (<= (+ (m/hypot (- (double bx) (double ax)) (- (double by) (double ay))) (double br)) (* (double ar) (+ 1.0 1e-9))))

(defn region-points
  "`{region [x y]}` - a label point for every region of `circles` (2 or 3, as [[two-sets]] / [[three-sets]] give
  them) that exists in the layout: 2 sets - midpoints of the region's stretch of the centre line (the widest part);
  3 sets - fixed points of the classic layout."
  [circles]
  (if (= 3 (count circles))
    (let [single 1.1 pair 0.75
          dir (fn [i] (three-set-directions i))]
      (into {#{0 1 2} [0.0 0.0]}
            (concat (for [i (range 3) :let [[dx dy] (dir i)]] [#{i} [(* single (double dx)) (* single (double dy))]])
                    (for [i (range 3) j (range (inc (long i)) 3)
                          :let [[dx dy] (dir (first (remove #{i j} (range 3))))]]
                      [#{i j} [(* pair -1.0 (double dx)) (* pair -1.0 (double dy))]]))))
    (let [[{x1 :x r1 :r :as a} {x2 :x r2 :r :as b}] circles
          x1 (double x1) r1 (double r1) x2 (double x2) r2 (double r2)
          mid (fn [^double p ^double q] [(* 0.5 (+ p q)) 0.0])
          a-left (- x1 r1) a-right (+ x1 r1) b-left (- x2 r2) b-right (+ x2 r2)]
      (cond-> {}
        (and (pos? r1) (not (contains-circle? b a))) (assoc #{0} (mid a-left (m/min b-left a-right)))
        (and (pos? r2) (not (contains-circle? a b))) (assoc #{1} (mid (m/max a-right b-left) b-right))
        (and (pos? r1) (pos? r2) (< (- x2 x1) (+ r1 r2))) (assoc #{0 1} (mid (m/max a-left b-left) (m/min a-right b-right)))))))
