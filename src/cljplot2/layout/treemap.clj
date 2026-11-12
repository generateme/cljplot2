(ns cljplot2.layout.treemap
  "Treemap layouts: rectangles with areas proportional to positive values, filling a rectangle `{:x :y :w :h}`
  (doubles). Both functions return one rectangle per value, in the order of `values`.

  * [[squarify]] - Bruls, Huizing, van Wijk (2000): values (callers sort them descending) are laid out in rows along
    the shorter side of the free rectangle; a value joins the current row while that doesn't worsen the row's worst
    aspect ratio.
  * [[slice-dice]] - the rectangle is cut along x at even depths and along y at odd depths, in value order."
  (:require [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- worst-ratio
  "Worst aspect ratio of a row with `areas` (sum `total`) laid along a side of length `side`."
  ^double [areas ^double total ^double side]
  (let [s2 (* side side)
        t2 (* total total)
        amax (double (reduce max areas))
        amin (double (reduce min areas))]
    (if (or (zero? amin) (zero? t2))
      ##Inf
      (m/max (/ (* s2 amax) t2) (/ t2 (* s2 amin))))))

(defn- place-row
  "Rectangles of a row with `areas` in free rectangle `free`, and the remaining free rectangle.
  A wide free rectangle gets a column at its left, a tall one a row at its top."
  [areas {:keys [x y w h] :as free}]
  (let [x (double x) y (double y) w (double w) h (double h)
        total (double (reduce + areas))]
    (if (>= w h)
      (let [cw (if (pos? h) (/ total h) 0.0)]
        [(first (reduce (fn [[rects ^double cy] ^double a]
                          (let [ah (if (pos? cw) (/ a cw) 0.0)]
                            [(conj rects {:x x :y cy :w cw :h ah}) (+ cy ah)]))
                        [[] y] areas))
         {:x (+ x cw) :y y :w (m/max 0.0 (- w cw)) :h h}])
      (let [rh (if (pos? w) (/ total w) 0.0)]
        [(first (reduce (fn [[rects ^double cx] ^double a]
                          (let [aw (if (pos? rh) (/ a rh) 0.0)]
                            [(conj rects {:x cx :y y :w aw :h rh}) (+ cx aw)]))
                        [[] x] areas))
         {:x x :y (+ y rh) :w w :h (m/max 0.0 (- h rh))}]))))

(defn squarify
  "Squarified rectangles for positive `values` (in the given order, normally descending) inside `rect`.
  An empty `values` gives `[]`; a zero-size `rect` gives zero-size rectangles."
  [values {:keys [w h] :as rect}]
  (if (empty? values)
    []
    (let [total (double (reduce + values))
          scale (/ (* (double w) (double h)) total)
          areas (mapv #(* scale (double %)) values)]
      (loop [remaining areas
             row []
             free rect
             placed []]
        (let [side (m/min (double (:w free)) (double (:h free)))]
          (cond
            (and (empty? remaining) (empty? row)) placed
            (empty? remaining) (into placed (first (place-row row free)))
            :else
            (let [a (double (first remaining))
                  candidate (conj row a)]
              (if (or (empty? row)
                      (<= (worst-ratio candidate (double (reduce + candidate)) side)
                          (worst-ratio row (double (reduce + row)) side)))
                (recur (rest remaining) candidate free placed)
                (let [[rects free'] (place-row row free)]
                  (recur remaining [] free' (into placed rects)))))))))))

(defn slice-dice
  "Slice-and-dice rectangles for positive `values` inside `rect` at tree `depth`: even depths cut along x (full
  height slices), odd depths along y (full width slices)."
  [values {:keys [x y w h]} depth]
  (let [x (double x) y (double y) w (double w) h (double h)
        total (double (reduce + 0.0 values))
        along-x? (even? (long depth))]
    (first (reduce (fn [[rects ^double offset] v]
                     (let [share (if (pos? total) (/ (double v) total) 0.0)]
                       (if along-x?
                         (let [sw (* share w)] [(conj rects {:x (+ x offset) :y y :w sw :h h}) (+ offset sw)])
                         (let [sh (* share h)] [(conj rects {:x x :y (+ y offset) :w w :h sh}) (+ offset sh)]))))
                   [[] 0.0] values))))
