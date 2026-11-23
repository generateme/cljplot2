(ns cljplot2.layout.treemap
  "Treemap layouts: rectangles with areas proportional to positive values, filling a rectangle `{:x :y :w :h}`
  (doubles). Both functions return one rectangle per value, in the order of `values`.

  * [[squarify]] - Bruls, Huizing, van Wijk (2000): values (callers sort them descending) are laid out in rows along
    the shorter side of the free rectangle; a value joins the current row while that doesn't worsen the row's worst
    aspect ratio.
  * [[slice-dice]] - the rectangle is cut along x at even depths and along y at odd depths, in value order."
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- worst-ratio
  "Gives the worst aspect ratio of a row of rectangles laid along a side.

  Parameters:

  - `areas` (non-empty sequence of doubles): the row's areas.
  - `total` (double): their sum.
  - `side` (double): the length of the side the row lies along.

  Returns the largest of `side^2 max / total^2` and `total^2 / (side^2 min)`; `##Inf` for a zero area or total."
  ^double [areas ^double total ^double side]
  (let [s2 (m/* side side)
        t2 (m/* total total)
        amax (v/mx areas)
        amin (v/mn areas)]
    (if (or (m/zero? amin) (m/zero? t2))
      ##Inf
      (m/max (m// (m/* s2 amax) t2) (m// t2 (m/* s2 amin))))))

(defn- place-row
  "Gives the rectangles of a row in a free rectangle and the free rectangle left.

  A wide free rectangle gets a column at its left, a tall one a row at its top.

  Parameters:

  - `areas` (non-empty sequence of doubles): the row's areas.
  - `free` (map): the free rectangle `{:x :y :w :h}`.

  Returns `[rects free']`: a vector of `{:x :y :w :h}` per area, in order, and the remaining free rectangle; a zero-size free rectangle gives zero-size rectangles."
  [areas {^double x :x ^double y :y ^double w :w ^double h :h}]
  (let [total (v/sum areas)]
    (if (m/>= w h)
      (let [cw (if (m/pos? h) (m// total h) 0.0)]
        [(first (reduce (fn [[rects ^double cy] ^double a]
                          (let [ah (if (m/pos? cw) (m// a cw) 0.0)]
                            [(conj rects {:x x :y cy :w cw :h ah}) (m/+ cy ah)]))
                        [[] y] areas))
         {:x (m/+ x cw) :y y :w (m/max 0.0 (m/- w cw)) :h h}])
      (let [rh (if (m/pos? w) (m// total w) 0.0)]
        [(first (reduce (fn [[rects ^double cx] ^double a]
                          (let [aw (if (m/pos? rh) (m// a rh) 0.0)]
                            [(conj rects {:x cx :y y :w aw :h rh}) (m/+ cx aw)]))
                        [[] x] areas))
         {:x x :y (m/+ y rh) :w w :h (m/max 0.0 (m/- h rh))}]))))

(defn squarify
  "Gives squarified rectangles for positive values inside a rectangle.

  Parameters:

  - `values` (sequence of positive numbers): in the given order, normally descending.
  - `rect` (map): `{:x :y :w :h}` (doubles).

  Returns a vector of `{:x :y :w :h}`, one per value in order; `[]` for empty `values`; a zero-size `rect` gives zero-size rectangles.

  See also [[slice-dice]]."
  [values {^double w :w ^double h :h :as rect}]
  (if (empty? values)
    []
    (let [scale (m// (m/* w h) (v/sum values))]
      (loop [remaining (map #(m/* scale (double %)) values)
             row []
             free rect
             placed []]
        (let [side (m/min (double (:w free)) (double (:h free)))]
          (cond
            (and (empty? remaining) (empty? row)) placed
            (empty? remaining) (into placed (first (place-row row free)))
            :else
            (let [candidate (conj row (first remaining))]
              (if (or (empty? row)
                      (m/<= (worst-ratio candidate (v/sum candidate) side)
                            (worst-ratio row (v/sum row) side)))
                (recur (rest remaining) candidate free placed)
                (let [[rects free-left] (place-row row free)]
                  (recur remaining [] free-left (into placed rects)))))))))))

(defn slice-dice
  "Gives slice-and-dice rectangles for positive values inside a rectangle at a tree depth.

  Parameters:

  - `values` (sequence of positive numbers): in the given order.
  - `rect` (map): `{:x :y :w :h}` (doubles).
  - `depth` (long): even depths cut along x (full height slices), odd depths along y (full width slices).

  Returns a vector of `{:x :y :w :h}`, one per value in order; values summing to 0 give zero-size rectangles.

  See also [[squarify]]."
  [values {^double x :x ^double y :y ^double w :w ^double h :h} depth]
  (let [total (v/sum values)
        along-x? (even? (long depth))]
    (first (reduce (fn [[rects ^double offset] value]
                     (let [share (if (m/pos? total) (m// (double value) total) 0.0)]
                       (if along-x?
                         (let [sw (m/* share w)] [(conj rects {:x (m/+ x offset) :y y :w sw :h h}) (m/+ offset sw)])
                         (let [sh (m/* share h)] [(conj rects {:x x :y (m/+ y offset) :w w :h sh}) (m/+ offset sh)]))))
                   [[] 0.0] values))))
