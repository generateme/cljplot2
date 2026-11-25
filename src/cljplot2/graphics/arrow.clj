(ns cljplot2.graphics.arrow
  "Arrowheads and end gaps of stroked runs (vectors of `[x y]` pixel points), for the `:arrow`, `:start-gap` and
  `:end-gap` options of lines and segments.

  `:arrow` - `{:end true :start false :size 8 :angle 20 :shape :closed}`: heads at the run's end and / or start,
  `:size` pixels long, `:angle` - degrees between each side and the line (ggplot2 `arrow(angle)`; 20 - a narrow head
  about 0.73 x `:size` wide, 45 - as wide as twice its length), `:closed` - a filled triangle (the line stops at its
  base), `:open` - two strokes.
  A head points along the run's last stretch of at least its length (the tangent of a curve). Gaps (pixels) shorten
  the run along its length first - an arrow pointing at a node of that radius; a run no longer than its gaps isn't
  drawn.

  Functions: [[schema]], [[draw-run!]]."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def schema
  "Schema of the `:arrow` option."
  [:map {:closed true}
   (schema/optional :end {:description "Whether the end gets an arrowhead." :default true
                          :cljplot2/value-meanings {true "The end gets a head." false "No head at the end."}
                          :cljplot2/example-context {:arrow {:start true}}}
                    schema/bool)
   (schema/optional :start {:description "Whether the start gets an arrowhead." :default false
                            :cljplot2/value-meanings {true "The start gets a head." false "No head at the start."}}
                    schema/bool)
   (schema/optional :size {:description "The head's length in pixels." :default 8.0 :cljplot2/example 12} schema/pos)
   (schema/optional :angle {:description "The angle in degrees between each side of the head and the line." :default 20.0
                            :cljplot2/example 30}
                    [:and number? [:> 0] [:< 90]])
   (schema/optional :shape {:description "The head's shape." :default :closed
                            :cljplot2/value-meanings {:closed "A filled triangle." :open "Two strokes."}}
                    [:enum :closed :open])])

(def ^:private defaults
  "The `:arrow` option's defaults (as in [[schema]])."
  {:end true :start false :size 8.0 :angle 20.0 :shape :closed})

(def ^{:private true :const true :tag 'double} closed-head-trim
  "Share of a closed head's length the line stops short of the tip: the line ends inside the filled head, not at its point."
  0.9)

(defn- point-at
  "Gives the point at a distance from a run's start along its length.

  Parameters:

  - `run` (vector of `[x y]`): the pixel points.
  - `d` (double): the distance in pixels.

  Returns `[point index]` - `index` of the segment holding it; `nil` past the end (or for fewer than 2 points), see
  [[cljplot2.graphics.polyline/point-at-length]]."
  [run ^double d]
  (some-> (polyline/point-at-length run (polyline/cumulative-lengths run) d) (subvec 0 2)))

(defn- trim-start
  "Gives a run without its first pixels along its length.

  Parameters:

  - `run` (vector of `[x y]`): the pixel points.
  - `d` (double): the pixels to cut.

  Returns the shortened run (a vector); the run itself for `d` 0 or less; `nil` when it isn't longer than `d`."
  [run ^double d]
  (if (m/not-pos? d)
    run
    (when-let [[p ^long i] (point-at run d)]
      (if (m/< (m/+ d 1.0e-9) (polyline/path-length run))
        (into [p] (subvec run (m/long-inc i)))
        nil))))

(defn- trimmed
  "Gives a run shortened at both ends along its length.

  Parameters:

  - `run` (sequence of `[x y]`): the pixel points.
  - `start-gap`, `end-gap` (doubles): the pixels cut at the start and at the end.

  Returns the shortened run (a vector); `nil` when nothing is left."
  [run ^double start-gap ^double end-gap]
  (some-> (trim-start (vec run) start-gap)
          rseq vec (trim-start end-gap) rseq vec))

(defn- head-points
  "Gives the corners of a head at a run's end, pointing from the point `size` pixels back.

  Parameters:

  - `run` (vector of `[x y]`): the pixel points.
  - `size` (double): the head's length in pixels.
  - `angle` (double): degrees between each side and the line.

  Returns `[tip left right]`; `nil` when the tip and the point back coincide."
  [run ^double size ^double angle]
  (let [tip (peek run)
        back (or (first (point-at (vec (rseq run)) size)) (first run))
        [^double tx ^double ty] tip
        [^double bx ^double by] back
        dx (m/- tx bx)
        dy (m/- ty by)
        l (m/hypot dx dy)]
    (when (m/pos? l)
      (let [ux (m// dx l)
            uy (m// dy l)
            half (m/* size (m/tan (m/radians angle)))
            base-x (m/- tx (m/* ux size))
            base-y (m/- ty (m/* uy size))]
        [tip
         [(m/- base-x (m/* uy half)) (m/+ base-y (m/* ux half))]
         [(m/+ base-x (m/* uy half)) (m/- base-y (m/* ux half))]]))))

(defn- draw-head!
  "Draws a head in a line style.

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `head` (`[tip left right]`): the corners (see [[head-points]]).
  - `shape` (keyword): `:closed` - filled in the style's colour, else two strokes.
  - `style` (map): the line style.

  Returns `nil`."
  [canvas [tip left right] shape style]
  (if (= :closed shape)
    (do (c2d/set-color canvas (:color style))
        (c2d/path canvas [tip left right] true false))
    (polyline/stroke-runs! canvas [[left tip right]] style)))

(defn draw-run!
  "Strokes a run with its arrowheads and end gaps: trimmed by the gaps, shortened under closed heads, then the heads drawn.

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `run` (sequence of `[x y]`): the pixel points.
  - `style` (map): the line style (heads take its colour; `nil` or `:none` - no heads).
  - `[arrow start-gap end-gap]`: the `:arrow` option (see [[schema]]) or `nil`, the gaps in pixels (`nil` - 0).

  Returns `nil`; a run no longer than its gaps draws nothing."
  [canvas run style [arrow start-gap end-gap]]
  (when-let [kept (trimmed run (or start-gap 0.0) (or end-gap 0.0))]
    (let [{:keys [end start ^double size ^double angle shape]
           :or {size 0.0 angle 20.0}} (when arrow (merge defaults arrow))
          head-end (when end (head-points kept size angle))
          head-start (when start (head-points (vec (rseq kept)) size angle))
          closed? (= :closed shape)
          stroked (if closed?
                    (trimmed kept (if head-start (m/* closed-head-trim size) 0.0) (if head-end (m/* closed-head-trim size) 0.0))
                    kept)]
      (when stroked (polyline/stroke-runs! canvas [stroked] style))
      (when (and (some? (:color style)) (not (theme/none? (:color style))))
        (doseq [h [head-end head-start] :when h]
          (draw-head! canvas h shape style))))))
