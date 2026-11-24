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
  (:require [fastmath.core :as m])
  (:import [fastmath.java Array]))

;; Circles of the sibling packing live in primitive arrays (centres `xs` `ys`, radii `rs`) and the front chain in
;; `nxt` / `prv` (successor / predecessor indices), as in d3.

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^{:private true :const true :tag 'double} tangency-tolerance
  "Relative slack: tangent circles (centre distance = sum of radii, up to rounding) don't overlap."
  1e-9)

(def ^{:private true :const true :tag 'double} weak-tolerance
  "Relative slack of [[encloses-weak?]] (of the larger radius, at least 1)."
  1e-9)

(def ^{:private true :const true :tag 'double} collinear-tolerance
  "Below this the quadratic of [[enclosing-3]] degenerates to a linear equation."
  1e-6)

;; siblings

(defn- place!
  "Sets the centre of circle `ci` tangent to circles `bi` and `ai` (indices; d3's `place(b, a, c)` orientation).

  Parameters:

  - `xs`, `ys`, `rs` (double arrays): the circles' centres and radii; `xs` and `ys` are written.
  - `bi`, `ai`, `ci` (longs): the circles' indices.

  Coincident centres of `ai` and `bi` put `ci` to the right of `ai`. Returns the written `ys`."
  [^doubles xs ^doubles ys ^doubles rs bi ai ci]
  (let [a (long ai) b (long bi) c (long ci)
        ax (Array/aget xs a) ay (Array/aget ys a) ar (Array/aget rs a)
        bx (Array/aget xs b) by (Array/aget ys b) br (Array/aget rs b)
        cr (Array/aget rs c)
        dx (m/- bx ax) dy (m/- by ay)
        d2 (m/+ (m/* dx dx) (m/* dy dy))]
    (if (m/pos? d2)
      (let [a2 (m/sq (m/+ ar cr)) b2 (m/sq (m/+ br cr))]
        (if (m/> a2 b2)
          (let [x (m// (m/- (m/+ d2 b2) a2) (m/* 2.0 d2))
                y (m/sqrt (m/max 0.0 (m/- (m// b2 d2) (m/* x x))))]
            (Array/aset xs c (m/- bx (m/* x dx) (m/* y dy)))
            (Array/aset ys c (m/+ (m/- by (m/* x dy)) (m/* y dx))))
          (let [x (m// (m/- (m/+ d2 a2) b2) (m/* 2.0 d2))
                y (m/sqrt (m/max 0.0 (m/- (m// a2 d2) (m/* x x))))]
            (Array/aset xs c (m/- (m/+ ax (m/* x dx)) (m/* y dy)))
            (Array/aset ys c (m/+ ay (m/* x dy) (m/* y dx))))))
      (do (Array/aset xs c (m/+ ax cr))
          (Array/aset ys c ay)))))

(defn- overlap?
  "Tests whether two placed circles overlap, more than touching (see [[tangency-tolerance]]).

  Parameters:

  - `xs`, `ys`, `rs` (double arrays): the centres and radii of the circles being placed.
  - `ci`, `cj` (longs): the indices of the two circles.

  Returns true when they overlap, else false; a pair with zero total radius never overlaps."
  [^doubles xs ^doubles ys ^doubles rs ci cj]
  (let [i (long ci) j (long cj)
        dr (m/* (m/+ (Array/aget rs i) (Array/aget rs j)) (m/- 1.0 tangency-tolerance))
        dx (m/- (Array/aget xs j) (Array/aget xs i)) dy (m/- (Array/aget ys j) (Array/aget ys i))]
    (and (m/pos? dr) (m/> (m/* dr dr) (m/+ (m/* dx dx) (m/* dy dy))))))

(defn- score
  "Gives the squared distance from the origin of the weighted midpoint of a front-chain circle and its successor.

  The front chain places the next circle beside the pair closest to the origin.

  Parameters:

  - `xs`, `ys`, `rs` (double arrays): the centres and radii of the circles being placed.
  - `nxt` (long array): the chain's successor index of each circle.
  - `ci` (long): the index of the chain circle.

  Returns the squared distance as a double."
  [^doubles xs ^doubles ys ^doubles rs ^longs nxt ci]
  (let [a (long ci) b (Array/aget nxt a)
        ar (Array/aget rs a) br (Array/aget rs b)
        ab (m/+ ar br)
        dx (m// (m/+ (m/* (Array/aget xs a) br) (m/* (Array/aget xs b) ar)) ab)
        dy (m// (m/+ (m/* (Array/aget ys a) br) (m/* (Array/aget ys b) ar)) ab)]
    (m/+ (m/* dx dx) (m/* dy dy))))

(defn- chain-conflict
  "Gives the first chain circle that circle `c` (placed between `a` and `b`) overlaps.

  The search runs forward from `b`'s successor and backward from `a`'s predecessor, the shorter side (by summed radii) first.

  Parameters:

  - `xs`, `ys`, `rs` (double arrays): the circles.
  - `nxt`, `prv` (long arrays): the front chain.
  - `a`, `b`, `c` (longs): the pair and the placed circle.

  Returns `[:forward j]`, `[:backward k]` or `nil` when `c` overlaps no chain circle."
  [^doubles xs ^doubles ys ^doubles rs ^longs nxt ^longs prv a b c]
  (loop [j (Array/aget nxt (long b)) k (Array/aget prv (long a)) sj (Array/aget rs (long b)) sk (Array/aget rs (long a))]
    (if (m/<= sj sk)
      (if (overlap? xs ys rs j c)
        [:forward j]
        (let [j' (Array/aget nxt j)]
          (when-not (m/== j' (Array/aget nxt k)) (recur j' k (m/+ sj (Array/aget rs j)) sk))))
      (if (overlap? xs ys rs k c)
        [:backward k]
        (let [k' (Array/aget prv k)]
          (when-not (m/== j (Array/aget nxt k')) (recur j k' sj (m/+ sk (Array/aget rs k)))))))))

(defn- closest-pair-start
  "Gives the chain circle whose pair with its successor scores lowest (see [[score]]).

  The candidates are `ai` - the circle before the newly inserted `ci` - and every chain circle after `ci` (not `ci` itself); ties keep the earlier one.

  Parameters:

  - `xs`, `ys`, `rs` (double arrays): the circles.
  - `nxt` (long array): the chain successors.
  - `ai`, `ci` (longs): the circle before the inserted one and the inserted one.

  Returns the index (a long)."
  [^doubles xs ^doubles ys ^doubles rs ^longs nxt ai ci]
  (let [a (long ai) c (long ci)]
    (loop [i (Array/aget nxt c) best a best-score (double (score xs ys rs nxt a))]
      (if (m/== i c)
        best
        (let [s (double (score xs ys rs nxt i))]
          (if (m/< s best-score) (recur (Array/aget nxt i) i s) (recur (Array/aget nxt i) best best-score)))))))

(defn pack-siblings
  "Gives the centres of circles packed without overlap, in input order.

  The first circle sits at the origin; two touch on the x axis (the contact at the origin); from the third on each circle touches two placed ones.

  Parameters:

  - `radii` (sequence of positive numbers): the circles' radii.

  Returns a vector of `[x y]` (doubles), one per radius; `[]` for no radii."
  [radii]
  (let [n (count radii)
        rs (double-array radii)
        xs (double-array n)
        ys (double-array n)
        nxt (long-array n)
        prv (long-array n)]
    (when (m/> n 1)
      (Array/aset xs 0 (m/- (Array/aget rs 1)))
      (Array/aset xs 1 (Array/aget rs 0)))
    (when (m/> n 2)
      (place! xs ys rs 1 0 2)
      ;; front chain 0 -> 1 -> 2 -> 0
      (Array/aset nxt 0 1) (Array/aset nxt 1 2) (Array/aset nxt 2 0)
      (Array/aset prv 1 0) (Array/aset prv 2 1) (Array/aset prv 0 2)
      (loop [i 3 a 0 b 1]
        (when (m/< i n)
          (place! xs ys rs a b i)
          (let [[side idx] (chain-conflict xs ys rs nxt prv a b i)]
            (case side
              ;; drop the chain between the pair and the overlapped circle, retry the same circle
              :forward (do (Array/aset nxt a (long idx)) (Array/aset prv (long idx) a) (recur i a (long idx)))
              :backward (do (Array/aset nxt (long idx) b) (Array/aset prv b (long idx)) (recur i (long idx) b))
              (do (Array/aset prv i a) (Array/aset nxt i b) (Array/aset nxt a i) (Array/aset prv b i)
                  (let [a' (long (closest-pair-start xs ys rs nxt a i))]
                    (recur (m/long-inc i) a' (Array/aget nxt a')))))))))
    (mapv (fn [^long i] [(Array/aget xs i) (Array/aget ys i)]) (range n))))

;; enclosing circle

(defn- encloses-not?
  "Tests whether a circle fails to contain another, exactly (no slack).

  Parameters:

  - `a`, `b` (maps): circles `{:x :y :r}`.

  Returns true when `a` doesn't contain `b` (a smaller `a` never does), else false. See also [[encloses-weak?]]."
  [{^double ax :x ^double ay :y ^double ar :r} {^double bx :x ^double by :y ^double br :r}]
  (let [dr (m/- ar br) dx (m/- bx ax) dy (m/- by ay)]
    (or (m/neg? dr) (m/< (m/* dr dr) (m/+ (m/* dx dx) (m/* dy dy))))))

(defn- encloses-weak?
  "Tests whether a circle contains another, up to rounding (see [[weak-tolerance]]).

  Parameters:

  - `a`, `b` (maps): circles `{:x :y :r}`.

  Returns true when `a` contains `b` with the tolerance, else false. See also [[encloses-not?]]."
  [{^double ax :x ^double ay :y ^double ar :r} {^double bx :x ^double by :y ^double br :r}]
  (let [dr (m/+ (m/- ar br) (m/* (m/max ar br 1.0) weak-tolerance))
        dx (m/- bx ax) dy (m/- by ay)]
    (and (m/pos? dr) (m/> (m/* dr dr) (m/+ (m/* dx dx) (m/* dy dy))))))

(defn- encloses-weak-all?
  "Tests whether a circle contains every circle of a collection, up to rounding (see [[encloses-weak?]]).

  Parameters:

  - `a` (map): a circle `{:x :y :r}`.
  - `bs` (sequence): circles `{:x :y :r}`.

  Returns true when `a` contains them all; true for no circles."
  [a bs]
  (every? #(encloses-weak? a %) bs))

(defn- enclosing-2
  "Gives the smallest circle containing two circles.

  Parameters:

  - `a`, `b` (maps): circles `{:x :y :r}`.

  Returns the circle `{:x :y :r}`; with a shared centre the larger of the two."
  [{^double x1 :x ^double y1 :y ^double r1 :r :as a} {^double x2 :x ^double y2 :y ^double r2 :r}]
  (let [x21 (m/- x2 x1) y21 (m/- y2 y1) r21 (m/- r2 r1)
        l (m/sqrt (m/+ (m/* x21 x21) (m/* y21 y21)))]
    (if (m/zero? l)
      (if (m/>= r1 r2) a {:x x2 :y y2 :r r2})
      {:x (m/* 0.5 (m/+ x1 x2 (m/* (m// x21 l) r21)))
       :y (m/* 0.5 (m/+ y1 y2 (m/* (m// y21 l) r21)))
       :r (m/* 0.5 (m/+ l r1 r2))})))

(defn- enclosing-3
  "Gives the circle touching three circles from outside and containing them (Apollonius).

  Parameters:

  - `a`, `b`, `c` (maps): circles `{:x :y :r}`.

  Returns the circle `{:x :y :r}`; for nearly collinear centres the linear solution (see [[collinear-tolerance]])."
  [{^double x1 :x ^double y1 :y ^double r1 :r} {^double x2 :x ^double y2 :y ^double r2 :r} {^double x3 :x ^double y3 :y ^double r3 :r}]
  (let [a2 (m/- x1 x2) a3 (m/- x1 x3) b2 (m/- y1 y2) b3 (m/- y1 y3)
        c2 (m/- r2 r1) c3 (m/- r3 r1)
        d1 (m/- (m/+ (m/* x1 x1) (m/* y1 y1)) (m/* r1 r1))
        d2 (m/+ (m/- d1 (m/* x2 x2) (m/* y2 y2)) (m/* r2 r2))
        d3 (m/+ (m/- d1 (m/* x3 x3) (m/* y3 y3)) (m/* r3 r3))
        ab (m/- (m/* a3 b2) (m/* a2 b3))
        xa (m/- (m// (m/- (m/* b2 d3) (m/* b3 d2)) (m/* ab 2.0)) x1)
        xb (m// (m/- (m/* b3 c2) (m/* b2 c3)) ab)
        ya (m/- (m// (m/- (m/* a3 d2) (m/* a2 d3)) (m/* ab 2.0)) y1)
        yb (m// (m/- (m/* a2 c3) (m/* a3 c2)) ab)
        qa (m/- (m/+ (m/* xb xb) (m/* yb yb)) 1.0)
        qb (m/* 2.0 (m/+ r1 (m/* xa xb) (m/* ya yb)))
        qc (m/- (m/+ (m/* xa xa) (m/* ya ya)) (m/* r1 r1))
        r (m/- (if (m/> (m/abs qa) collinear-tolerance)
                 (m// (m/+ qb (m/sqrt (m/- (m/* qb qb) (m/* 4.0 qa qc)))) (m/* 2.0 qa))
                 (m// qc qb)))]
    {:x (m/+ x1 xa (m/* xb r)) :y (m/+ y1 ya (m/* yb r)) :r r}))

(defn- enclosing-of-basis
  "Gives the smallest circle containing the circles of a basis.

  Parameters:

  - `basis` (vector): 1 to 3 circles `{:x :y :r}`.

  Returns the single circle itself, [[enclosing-2]] of two or [[enclosing-3]] of three; another count throws (a basis never has one)."
  [basis]
  (case (count basis)
    1 (first basis)
    2 (enclosing-2 (basis 0) (basis 1))
    3 (enclosing-3 (basis 0) (basis 1) (basis 2))))

(defn- extend-basis
  "Gives the basis of the smallest circle containing a basis and a circle it misses.

  Parameters:

  - `basis` (vector): 0 to 3 circles `{:x :y :r}`.
  - `p` (map): the circle the basis's enclosing circle misses.

  Returns a vector of 1 to 3 circles, `p` last; throws `ex-info` when no basis fits (degenerate circles)."
  [basis p]
  (or (when (encloses-weak-all? p basis) [p])
      (some (fn [b] (when (and (encloses-not? p b) (encloses-weak-all? (enclosing-2 b p) basis)) [b p])) basis)
      (first (for [i (range (count basis)) j (range (m/long-inc (long i)) (count basis))
                   :let [bi (basis i) bj (basis j)]
                   :when (and (encloses-not? (enclosing-2 bi bj) p)
                              (encloses-not? (enclosing-2 bi p) bj)
                              (encloses-not? (enclosing-2 bj p) bi)
                              (encloses-weak-all? (enclosing-3 bi bj p) basis))]
               [bi bj p]))
      (throw (ex-info "No enclosing basis (degenerate circles)." {:basis basis :circle p}))))

(defn enclose
  "Gives the smallest circle containing circles.

  Parameters:

  - `circles` (sequence): `{:x :y :r}`, at least one, non-negative radii.

  Returns `{:x :y :r}`; a single circle as given. Throws `ex-info` for degenerate circles no basis fits (see [[extend-basis]])."
  [circles]
  (let [cs (vec circles)
        n (count cs)]
    (loop [i 0 basis [] e nil]
      (if (m/< i n)
        (let [p (cs i)]
          (if (and e (encloses-weak? e p))
            (recur (m/long-inc i) basis e)
            (let [basis' (extend-basis basis p)]
              (recur 0 basis' (enclosing-of-basis basis')))))
        e))))

;; hierarchy

(defn- layout-units
  "Gives a node laid out in layout units: its radius `:r` and its children's centres `:x :y` relative to its own.

  Leaves get `sqrt(value)`; groups - children packed with `half-gap` added to their radii, centred on the enclosing circle, radius the enclosing one plus `half-gap` (gaps of `2 half-gap` between siblings and to the parent's edge).

  Parameters:

  - `node` (map): `{:value :children}`.
  - `half-gap` (double): layout units added to every radius.

  Returns the node with `:r` (and children with `:x :y :r`, recursively)."
  [node ^double half-gap]
  (if-let [children (seq (:children node))]
    (let [laid (mapv #(layout-units % half-gap) children)
          centres (pack-siblings (map #(m/+ (double (:r %)) half-gap) laid))
          {^double ex :x ^double ey :y ^double er :r} (enclose (map (fn [c [x y]] {:x x :y y :r (m/+ (double (:r c)) half-gap)}) laid centres))]
      (assoc node
             :r (m/+ er half-gap)
             :children (mapv (fn [c [^double x ^double y]] (assoc c :x (m/- x ex) :y (m/- y ey))) laid centres)))
    (assoc node :r (m/sqrt (double (:value node))))))

(defn- absolute
  "Gives a node laid out in layout units (see [[layout-units]]) with absolute pixel `:x :y :r`, recursively.

  Parameters:

  - `node` (map): the relative layout.
  - `cx`, `cy` (doubles): its centre in pixels.
  - `scale` (double): pixels per layout unit."
  [node ^double cx ^double cy ^double scale]
  (cond-> (assoc node :x cx :y cy :r (m/* scale (double (:r node))))
    (:children node) (update :children
                             (fn [cs] (mapv #(absolute % (m/+ cx (m/* scale (double (:x %)))) (m/+ cy (m/* scale (double (:y %)))) scale) cs)))))

(def ^{:private true :const true :tag 'double} gap-tolerance
  "Pixels: a fitted gap this close to the asked one is exact enough."
  0.01)

(def ^{:private true :const true :tag 'long} max-fit-rounds
  "Layout passes searching the padding in layout units (bracketing, then false position)."
  60)

(def ^{:private true :const true :tag 'double} min-gap-growth
  "Fraction of the asked gap a doubled padding must widen the gaps by; less - the box can't hold the gaps."
  1e-3)

(defn- laid-out
  "Gives the layout of a tree in layout units with `half-gap` units added to every child's and group's radius (see [[layout-units]]).

  Parameters:

  - `tree` (map): the root node.
  - `half-gap` (double): layout units.
  - `side` (double): the pixels the root circle's diameter fits.

  Returns `{:half-gap u :laid tree :scale px-per-unit :gap px}` - the scale fitting `side` pixels, `:gap` the resulting gap in pixels."
  [tree ^double half-gap ^double side]
  (let [laid (layout-units tree half-gap)
        scale (m// side (m/* 2.0 (double (:r laid))))]
    {:half-gap half-gap :laid laid :scale scale :gap (m/* 2.0 half-gap scale)}))

(defn- fit-gap
  "Gives the layout of a tree (see [[laid-out]]) whose gaps are `gap` pixels at `side` pixels.

  The half gap in layout units is bracketed by doubling, then refined by false position (Illinois) to [[gap-tolerance]], at most [[max-fit-rounds]] passes each.

  Parameters:

  - `tree` (map): the root node.
  - `gap` (double): the wanted gap in pixels, positive.
  - `side` (double): the pixels the root circle fits, positive.

  Returns the layout; when the box can't hold such gaps (they stop growing as the leaves shrink towards points) - the one with the largest gaps found."
  [tree ^double gap ^double side]
  (let [at (fn [^double u] (laid-out tree u side))
        close? (fn [e] (m/< (m/abs (m/- (double (:gap e)) gap)) gap-tolerance))
        zero-gap (at 0.0)
        first-guess (at (m// (m/* 0.5 gap) (double (:scale zero-gap))))]
    (if (close? first-guess)
      first-guess
      ;; bracket: `hi` with a gap at least the asked one; stop when doubling stops widening the gaps (infeasible)
      (let [[bracket-lo bracket-hi feasible?]
            (loop [lo zero-gap hi first-guess round 0]
              (cond
                (m/>= (double (:gap hi)) gap) [lo hi true]
                (m/>= round max-fit-rounds) [lo hi false]
                :else (let [doubled (at (m/* 2.0 (double (:half-gap hi))))]
                        (if (m/< (m/- (double (:gap doubled)) (double (:gap hi))) (m/* min-gap-growth gap))
                          [hi doubled false]
                          (recur hi doubled (m/long-inc round))))))]
        (if-not feasible?
          (max-key :gap bracket-lo bracket-hi)
          (loop [lo bracket-lo hi bracket-hi lo-w 1.0 hi-w 1.0 side-kept nil round 0]
            (let [flo (m/- (double (:gap lo)) gap) fhi (m/- (double (:gap hi)) gap)
                  ulo (double (:half-gap lo)) uhi (double (:half-gap hi))
                  u (m// (m/- (m/* ulo (m/* hi-w fhi)) (m/* uhi (m/* lo-w flo))) (m/- (m/* hi-w fhi) (m/* lo-w flo)))
                  e (at u)
                  fe (m/- (double (:gap e)) gap)]
              (cond
                (or (close? e) (m/>= round max-fit-rounds)) e
                (m/neg? fe) (recur e hi 1.0 (if (= side-kept :hi) (m/* 0.5 hi-w) 1.0) :hi (m/long-inc round))
                :else (recur lo e (if (= side-kept :lo) (m/* 0.5 lo-w) 1.0) 1.0 :lo (m/long-inc round))))))))))

(defn pack
  "Gives a tree packed into a pixel box: every node gets absolute pixel `:x :y :r`.

  The root circle fits the box's shorter side, centred. Half of `padding` is added to every child's radius before packing and to every group's enclosing radius; its size in layout units is searched for the scale it gives (within 0.01 px).

  Parameters:

  - `tree` (map): `{:value v :children [..]}` nodes - positive values, groups with at least one child, children in packing order (the first ones near the centre).
  - `box` (map): `{:x :y :w :h}` pixels.
  - `padding` (number): pixels, as d3's - the gap between siblings and between a child and its parent's edge.

  Returns the tree with `:x :y :r` on every node. A box too small for such gaps gets the widest gaps it holds (the leaves shrink towards points); a zero-size box or no padding - no gap search."
  [tree {^double x :x ^double y :y ^double w :w ^double h :h} padding]
  (let [side (m/min w h)
        gap (double padding)
        cx (m/+ x (m/* 0.5 w))
        cy (m/+ y (m/* 0.5 h))
        {:keys [laid scale]} (if (and (m/pos? side) (m/pos? gap))
                               (fit-gap tree gap side)
                               (laid-out tree 0.0 (m/max side 0.0)))]
    (absolute laid cx cy scale)))
