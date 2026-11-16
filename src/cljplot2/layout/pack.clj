(ns cljplot2.layout.pack
  "Circle packing: sibling circles packed without overlap, the smallest circle enclosing them, and a hierarchy packed
  into a pixel box. Deterministic - no random order, the same input always gives the same layout. The algorithms
  follow d3-hierarchy's `pack` (ISC license):

  * [[pack-siblings]] - front-chain packing (Wang, Wang, Dai, Wang 2006): each circle is placed tangent to two
    neighbours on the front chain (the pair whose weighted midpoint is closest to the origin); chain circles it
    would overlap are dropped from the chain and the placement retried.
  * [[enclose]] - the smallest enclosing circle of circles: Welzl's move-to-front scheme over a basis of up to three
    circles (correct in any input order; a random order only speeds it up).
  * [[pack]] - a tree of `{:value :children}` nodes: leaf radii from the square root of the value (area proportional
    to value), each group the enclosing circle of its packed children, the root scaled to fit the box; gaps of
    `padding` pixels."
  (:require [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private ^:const tangency-tolerance
  "Relative slack: tangent circles (centre distance = sum of radii, up to rounding) don't overlap."
  1e-9)

;; siblings

(defn- place!
  "Set circle `c`'s centre tangent to circles `b` and `a` (indices; d3's `place(b, a, c)` orientation)."
  [^doubles xs ^doubles ys ^doubles rs b a c]
  (let [a (long a) b (long b) c (long c)
        ax (aget xs a) ay (aget ys a) ar (aget rs a)
        bx (aget xs b) by (aget ys b) br (aget rs b)
        cr (aget rs c)
        dx (- bx ax) dy (- by ay)
        d2 (+ (* dx dx) (* dy dy))]
    (if (pos? d2)
      (let [a2 (m/sq (+ ar cr)) b2 (m/sq (+ br cr))]
        (if (> a2 b2)
          (let [x (/ (- (+ d2 b2) a2) (* 2.0 d2))
                y (m/sqrt (m/max 0.0 (- (/ b2 d2) (* x x))))]
            (aset xs c (- bx (* x dx) (* y dy)))
            (aset ys c (+ (- by (* x dy)) (* y dx))))
          (let [x (/ (- (+ d2 a2) b2) (* 2.0 d2))
                y (m/sqrt (m/max 0.0 (- (/ a2 d2) (* x x))))]
            (aset xs c (- (+ ax (* x dx)) (* y dy)))
            (aset ys c (+ ay (* x dy) (* y dx))))))
      (do (aset xs c (+ ax cr))
          (aset ys c ay)))))

(defn- overlap?
  "Do circles `i` and `j` overlap (more than touch)?"
  [^doubles xs ^doubles ys ^doubles rs i j]
  (let [i (long i) j (long j)
        dr (* (+ (aget rs i) (aget rs j)) (- 1.0 tangency-tolerance))
        dx (- (aget xs j) (aget xs i)) dy (- (aget ys j) (aget ys i))]
    (and (pos? dr) (> (* dr dr) (+ (* dx dx) (* dy dy))))))

(defn- score
  "Squared distance from the origin of the weighted midpoint of chain circle `i` and its successor."
  [^doubles xs ^doubles ys ^doubles rs ^longs nxt i]
  (let [a (long i) b (aget nxt a)
        ar (aget rs a) br (aget rs b)
        ab (+ ar br)
        dx (/ (+ (* (aget xs a) br) (* (aget xs b) ar)) ab)
        dy (/ (+ (* (aget ys a) br) (* (aget ys b) ar)) ab)]
    (+ (* dx dx) (* dy dy))))

(defn- chain-conflict
  "First chain circle that circle `c` (placed between `a` and `b`) overlaps, searching forward from `b`'s successor and
  backward from `a`'s predecessor (the shorter side first): `[:forward j]`, `[:backward k]` or `nil`."
  [^doubles xs ^doubles ys ^doubles rs ^longs nxt ^longs prv a b c]
  (loop [j (aget nxt (long b)) k (aget prv (long a)) sj (aget rs (long b)) sk (aget rs (long a))]
    (if (<= sj sk)
      (if (overlap? xs ys rs j c)
        [:forward j]
        (let [j' (aget nxt j)]
          (when-not (== j' (aget nxt k)) (recur j' k (+ sj (aget rs j)) sk))))
      (if (overlap? xs ys rs k c)
        [:backward k]
        (let [k' (aget prv k)]
          (when-not (== j (aget nxt k')) (recur j k' sj (+ sk (aget rs k)))))))))

(defn- closest-pair-start
  "Chain circle whose pair with its successor scores lowest (see [[score]]): `a` - the circle before the newly
  inserted `c` - and every chain circle after `c` (not `c` itself)."
  [^doubles xs ^doubles ys ^doubles rs ^longs nxt a c]
  (let [a (long a) c (long c)]
    (loop [i (aget nxt c) best a best-score (double (score xs ys rs nxt a))]
      (if (== i c)
        best
        (let [s (double (score xs ys rs nxt i))]
          (if (< s best-score) (recur (aget nxt i) i s) (recur (aget nxt i) best best-score)))))))

(defn pack-siblings
  "Centres `[x y]` of circles with `radii` (positive numbers) packed without overlap, in input order: the first at the
  origin; two touch on the x axis (the contact at the origin); from the third on each circle touches two placed
  ones. `[]` for no radii."
  [radii]
  (let [n (count radii)
        rs (double-array radii)
        xs (double-array n)
        ys (double-array n)
        nxt (long-array n)
        prv (long-array n)]
    (when (> n 1)
      (aset xs 0 (- (aget rs 1)))
      (aset xs 1 (aget rs 0)))
    (when (> n 2)
      (place! xs ys rs 1 0 2)
      ;; front chain 0 -> 1 -> 2 -> 0
      (aset nxt 0 1) (aset nxt 1 2) (aset nxt 2 0)
      (aset prv 1 0) (aset prv 2 1) (aset prv 0 2)
      (loop [i 3 a 0 b 1]
        (when (< i n)
          (place! xs ys rs a b i)
          (let [[side idx] (chain-conflict xs ys rs nxt prv a b i)]
            (case side
              ;; drop the chain between the pair and the overlapped circle, retry the same circle
              :forward (do (aset nxt a (long idx)) (aset prv (long idx) a) (recur i a (long idx)))
              :backward (do (aset nxt (long idx) b) (aset prv b (long idx)) (recur i (long idx) b))
              (do (aset prv i a) (aset nxt i b) (aset nxt a i) (aset prv b i)
                  (let [a' (long (closest-pair-start xs ys rs nxt a i))]
                    (recur (inc i) a' (aget nxt a')))))))))
    (mapv (fn [i] [(aget xs i) (aget ys i)]) (range n))))

;; enclosing circle

(defn- encloses-not?
  "Does circle `a` fail to contain circle `b`?"
  [{ax :x ay :y ar :r} {bx :x by :y br :r}]
  (let [dr (- (double ar) (double br)) dx (- (double bx) (double ax)) dy (- (double by) (double ay))]
    (or (neg? dr) (< (* dr dr) (+ (* dx dx) (* dy dy))))))

(defn- encloses-weak?
  "Does circle `a` contain circle `b`, up to rounding?"
  [{ax :x ay :y ar :r} {bx :x by :y br :r}]
  (let [ar (double ar) br (double br)
        dr (+ (- ar br) (* (m/max ar br 1.0) 1e-9))
        dx (- (double bx) (double ax)) dy (- (double by) (double ay))]
    (and (pos? dr) (> (* dr dr) (+ (* dx dx) (* dy dy))))))

(defn- encloses-weak-all? [a bs] (every? #(encloses-weak? a %) bs))

(defn- enclosing-2
  "Smallest circle containing circles `a` and `b`."
  [{x1 :x y1 :y r1 :r :as a} {x2 :x y2 :y r2 :r}]
  (let [x1 (double x1) y1 (double y1) r1 (double r1) x2 (double x2) y2 (double y2) r2 (double r2)
        x21 (- x2 x1) y21 (- y2 y1) r21 (- r2 r1)
        l (m/sqrt (+ (* x21 x21) (* y21 y21)))]
    (if (zero? l)
      (if (>= r1 r2) a {:x x2 :y y2 :r r2})
      {:x (* 0.5 (+ x1 x2 (* (/ x21 l) r21)))
       :y (* 0.5 (+ y1 y2 (* (/ y21 l) r21)))
       :r (* 0.5 (+ l r1 r2))})))

(defn- enclosing-3
  "Circle touching circles `a`, `b` and `c` from outside, containing them (Apollonius)."
  [{x1 :x y1 :y r1 :r} {x2 :x y2 :y r2 :r} {x3 :x y3 :y r3 :r}]
  (let [x1 (double x1) y1 (double y1) r1 (double r1)
        x2 (double x2) y2 (double y2) r2 (double r2)
        x3 (double x3) y3 (double y3) r3 (double r3)
        a2 (- x1 x2) a3 (- x1 x3) b2 (- y1 y2) b3 (- y1 y3)
        c2 (- r2 r1) c3 (- r3 r1)
        d1 (- (+ (* x1 x1) (* y1 y1)) (* r1 r1))
        d2 (+ (- d1 (* x2 x2) (* y2 y2)) (* r2 r2))
        d3 (+ (- d1 (* x3 x3) (* y3 y3)) (* r3 r3))
        ab (- (* a3 b2) (* a2 b3))
        xa (- (/ (- (* b2 d3) (* b3 d2)) (* ab 2.0)) x1)
        xb (/ (- (* b3 c2) (* b2 c3)) ab)
        ya (- (/ (- (* a3 d2) (* a2 d3)) (* ab 2.0)) y1)
        yb (/ (- (* a2 c3) (* a3 c2)) ab)
        qa (- (+ (* xb xb) (* yb yb)) 1.0)
        qb (* 2.0 (+ r1 (* xa xb) (* ya yb)))
        qc (- (+ (* xa xa) (* ya ya)) (* r1 r1))
        r (- (if (> (m/abs qa) 1e-6)
               (/ (+ qb (m/sqrt (- (* qb qb) (* 4.0 qa qc)))) (* 2.0 qa))
               (/ qc qb)))]
    {:x (+ x1 xa (* xb r)) :y (+ y1 ya (* yb r)) :r r}))

(defn- enclosing-of-basis
  [basis]
  (case (count basis)
    1 (first basis)
    2 (enclosing-2 (basis 0) (basis 1))
    3 (enclosing-3 (basis 0) (basis 1) (basis 2))))

(defn- extend-basis
  "The basis of the smallest circle containing basis `basis` (1-3 circles) and circle `p`, which it misses."
  [basis p]
  (or (when (encloses-weak-all? p basis) [p])
      (some (fn [b] (when (and (encloses-not? p b) (encloses-weak-all? (enclosing-2 b p) basis)) [b p])) basis)
      (first (for [i (range (count basis)) j (range (inc (long i)) (count basis))
                   :let [bi (basis i) bj (basis j)]
                   :when (and (encloses-not? (enclosing-2 bi bj) p)
                              (encloses-not? (enclosing-2 bi p) bj)
                              (encloses-not? (enclosing-2 bj p) bi)
                              (encloses-weak-all? (enclosing-3 bi bj p) basis))]
               [bi bj p]))
      (throw (ex-info "No enclosing basis (degenerate circles)." {:basis basis :circle p}))))

(defn enclose
  "Smallest circle `{:x :y :r}` containing every circle of `circles` (`{:x :y :r}`, at least one, non-negative radii)."
  [circles]
  (let [cs (vec circles)
        n (count cs)]
    (loop [i 0 basis [] e nil]
      (if (< i n)
        (let [p (cs i)]
          (if (and e (encloses-weak? e p))
            (recur (inc i) basis e)
            (let [basis' (extend-basis basis p)]
              (recur 0 basis' (enclosing-of-basis basis')))))
        e))))

;; hierarchy

(defn- layout-units
  "`node` with its radius `:r` and its children's centres `:x :y` relative to its own (layout units): leaves -
  `sqrt(value)`; groups - children packed with `half-gap` added to their radii, centred on the enclosing circle,
  radius the enclosing one plus `half-gap` (gaps of `2 half-gap` between siblings and to the parent's edge)."
  [node ^double half-gap]
  (if-let [cs (seq (:children node))]
    (let [cs (mapv #(layout-units % half-gap) cs)
          centres (pack-siblings (map #(+ (double (:r %)) half-gap) cs))
          {ex :x ey :y er :r} (enclose (map (fn [c [x y]] {:x x :y y :r (+ (double (:r c)) half-gap)}) cs centres))]
      (assoc node
             :r (+ (double er) half-gap)
             :children (mapv (fn [c [x y]] (assoc c :x (- (double x) (double ex)) :y (- (double y) (double ey)))) cs centres)))
    (assoc node :r (m/sqrt (double (:value node))))))

(defn- absolute
  "`node` (relative layout, see [[layout-units]]) with absolute pixel `:x :y :r`: centre `(cx, cy)`, `scale` pixels per unit."
  [node ^double cx ^double cy ^double scale]
  (cond-> (assoc node :x cx :y cy :r (* scale (double (:r node))))
    (:children node) (update :children
                             (fn [cs] (mapv #(absolute % (+ cx (* scale (double (:x %)))) (+ cy (* scale (double (:y %)))) scale) cs)))))

(def ^:private ^:const gap-tolerance
  "Pixels: a fitted gap this close to the asked one is exact enough."
  0.01)

(def ^:private ^:const max-fit-rounds
  "Layout passes searching the padding in layout units (bracketing, then false position)."
  60)

(defn- laid-out
  "Layout of `tree` in layout units with `half-gap` units added to every child's and group's radius (see
  [[layout-units]]): `{:half-gap u :laid tree :scale px-per-unit :gap px}` - the scale fitting `side` pixels, `:gap` -
  the resulting gap in pixels."
  [tree ^double half-gap ^double side]
  (let [laid (layout-units tree half-gap)
        scale (/ side (* 2.0 (double (:r laid))))]
    {:half-gap half-gap :laid laid :scale scale :gap (* 2.0 half-gap scale)}))

(defn- fit-gap
  "Layout of `tree` (see [[laid-out]]) whose gaps are `gap` pixels at `side` pixels: the half gap in layout units
  bracketed by doubling, then refined by false position (Illinois) to [[gap-tolerance]]. When the box can't hold such
  gaps (they stop growing as the leaves shrink towards points) - the largest gaps found."
  [tree ^double gap ^double side]
  (let [at (fn [^double u] (laid-out tree u side))
        close? (fn [e] (< (m/abs (- (double (:gap e)) gap)) gap-tolerance))
        lo (at 0.0)
        first-guess (at (/ (* 0.5 gap) (double (:scale lo))))]
    (if (close? first-guess)
      first-guess
      ;; bracket: `hi` with a gap at least the asked one; stop when doubling stops widening the gaps (infeasible)
      (let [[lo hi feasible?] (loop [lo lo hi first-guess round 0]
                                (cond
                                  (>= (double (:gap hi)) gap) [lo hi true]
                                  (>= round max-fit-rounds) [lo hi false]
                                  :else (let [next (at (* 2.0 (double (:half-gap hi))))]
                                          (if (< (- (double (:gap next)) (double (:gap hi))) (* 1e-3 gap))
                                            [hi next false]
                                            (recur hi next (inc round))))))]
        (if-not feasible?
          (max-key :gap lo hi)
          (loop [lo lo hi hi lo-w 1.0 hi-w 1.0 side-kept nil round 0]
            (let [flo (- (double (:gap lo)) gap) fhi (- (double (:gap hi)) gap)
                  ulo (double (:half-gap lo)) uhi (double (:half-gap hi))
                  u (/ (- (* ulo (* hi-w fhi)) (* uhi (* lo-w flo))) (- (* hi-w fhi) (* lo-w flo)))
                  e (at u)
                  fe (- (double (:gap e)) gap)]
              (cond
                (or (close? e) (>= round max-fit-rounds)) e
                (neg? fe) (recur e hi 1.0 (if (= side-kept :hi) (* 0.5 hi-w) 1.0) :hi (inc round))
                :else (recur lo e (if (= side-kept :lo) (* 0.5 lo-w) 1.0) 1.0 :lo (inc round))))))))))

(defn pack
  "Tree `tree` (`{:value v :children [..]}` nodes: positive values, groups with at least one child, children in
  packing order - the first ones near the centre) packed into the pixel box `{:x :y :w :h}`: every node gets
  `:x :y :r` (pixels, absolute). The root circle fits the box's shorter side, centred. `padding` (pixels, as d3's):
  the gap between siblings and between a child and its parent's edge - half of it added to every child's radius
  before packing and to every group's enclosing radius, the size in layout units searched for the scale it gives
  (within 0.01 px); a box too small for such gaps gets the widest gaps it holds (the leaves shrink towards points)."
  [tree {:keys [x y w h]} padding]
  (let [side (m/min (double w) (double h))
        padding (double padding)
        cx (+ (double x) (* 0.5 (double w)))
        cy (+ (double y) (* 0.5 (double h)))
        {:keys [laid scale]} (if (and (pos? side) (pos? padding))
                               (fit-gap tree padding side)
                               (laid-out tree 0.0 (m/max side 0.0)))]
    (absolute laid cx cy scale)))
