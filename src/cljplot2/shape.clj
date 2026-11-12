(ns cljplot2.shape
  "Point shapes (old cljplot set), drawn centered at a position with a size (bounding box side, pixels).

  * `\\O` / `\\o` - filled / outlined circle
  * `\\S` / `\\s` - filled / outlined square
  * `\\A \\V \\{ \\}` - filled triangle pointing up / down / left / right
  * `\\^ \\v \\< \\>` - outlined triangle pointing up / down / left / right
  * `\\-` `\\|` `\\\\` `\\/` - horizontal, vertical and diagonal strokes
  * `\\+` - plus, `\\x` - cross

  Filled shapes use the fill color and an optional outline; outlined shapes and strokes use the line color."
  (:require [clojure2d.core :as c2d]
            [fastmath.vector :as v])
  (:import [fastmath.vector Vec2 Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def filled-shapes #{\O \S \A \V \{ \}})
(def line-shapes #{\o \s \^ \v \< \> \- \| \\ \/ \+ \x})
(def shapes (into filled-shapes line-shapes))

(defn- triangle
  "Corners of a triangle inscribed in the square of half side `r` around (x, y), pointing `dir`."
  [^double x ^double y ^double r dir]
  (case dir
    :up [x (- y r) (+ x r) (+ y r) (- x r) (+ y r)]
    :down [x (+ y r) (+ x r) (- y r) (- x r) (- y r)]
    :left [(- x r) y (+ x r) (- y r) (+ x r) (+ y r)]
    :right [(+ x r) y (- x r) (- y r) (- x r) (+ y r)]))

(def ^:private triangle-dirs {\A :up \V :down \{ :left \} :right \^ :up \v :down \< :left \> :right})

(defn- outline!
  "Draw `shape` outline (or stroke) around center `c` - `Vec3` (x, y, half size)."
  [canvas shape ^Vec3 c]
  (let [x (.x c) y (.y c) r (.z c) d (* 2.0 r)]
    (case shape
      (\O \o) (c2d/ellipse canvas x y d d true)
      (\S \s) (c2d/crect canvas x y d d true)
      (\A \V \{ \} \^ \v \< \>) (let [[x1 y1 x2 y2 x3 y3] (triangle x y r (triangle-dirs shape))]
                                  (c2d/triangle canvas x1 y1 x2 y2 x3 y3 true))
      \- (c2d/line canvas (- x r) y (+ x r) y)
      \| (c2d/line canvas x (- y r) x (+ y r))
      \\ (c2d/line canvas (- x r) (- y r) (+ x r) (+ y r))
      \/ (c2d/line canvas (- x r) (+ y r) (+ x r) (- y r))
      \+ (do (c2d/line canvas (- x r) y (+ x r) y) (c2d/line canvas x (- y r) x (+ y r)))
      \x (do (c2d/line canvas (- x r) (- y r) (+ x r) (+ y r)) (c2d/line canvas (- x r) (+ y r) (+ x r) (- y r))))))

(defn- fill!
  [canvas shape ^Vec3 c]
  (let [x (.x c) y (.y c) r (.z c) d (* 2.0 r)]
    (case shape
      \O (c2d/ellipse canvas x y d d false)
      \S (c2d/crect canvas x y d d false)
      (let [[x1 y1 x2 y2 x3 y3] (triangle x y r (triangle-dirs shape))]
        (c2d/triangle canvas x1 y1 x2 y2 x3 y3 false)))))

(defn draw!
  "Draw `shape` centered at `center` (`Vec2`). `style` - `{:size :color :stroke-color :stroke-width}`, `:size` is
  the bounding box side: filled shapes are filled with `:color` and outlined with `:stroke-color` (`:none` or
  `:stroke-width` 0 skips the outline); outlined shapes and strokes are drawn with `:color`. Lines use the canvas
  stroke - the caller sets it once (width `:stroke-width`) for many points."
  [canvas shape ^Vec2 center {:keys [size color stroke-color stroke-width]}]
  (let [c (v/vec3 (.x center) (.y center) (* 0.5 (double size)))]
    (c2d/set-color canvas color)
    (if (filled-shapes shape)
      (do (fill! canvas shape c)
          (when (and (not= :none stroke-color) (pos? (double stroke-width)))
            (c2d/set-color canvas stroke-color)
            (outline! canvas shape c)))
      (outline! canvas shape c))))
