(ns cljplot2.graphics.arrow
  "Arrowheads and end gaps of stroked runs (vectors of `[x y]` pixel points), for the `:arrow`, `:start-gap` and
  `:end-gap` options of lines and segments.

  `:arrow` - `{:end true :start false :size 8 :angle 20 :shape :closed}`: heads at the run's end and / or start,
  `:size` pixels long, `:angle` - degrees between each side and the line (ggplot2 `arrow(angle)`; 20 - a narrow head
  about 0.73 x `:size` wide, 45 - as wide as twice its length), `:closed` - a filled triangle (the line stops at its
  base), `:open` - two strokes.
  A head points along the run's last stretch of at least its length (the tangent of a curve). Gaps (pixels) shorten
  the run along its length first - an arrow pointing at a node of that radius; a run no longer than its gaps isn't
  drawn."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def schema
  "Schema of the `:arrow` option."
  [:map {:closed true}
   (schema/optional :end {:description "a head at the run's end" :default true} schema/bool)
   (schema/optional :start {:description "a head at the run's start" :default false} schema/bool)
   (schema/optional :size {:description "head length in pixels" :default 8.0} schema/pos)
   (schema/optional :angle {:description "angle in degrees between each side of the head and the line" :default 20.0}
                    [:and number? [:> 0] [:< 90]])
   (schema/optional :shape {:description ":closed (a filled triangle) or :open (two strokes)" :default :closed}
                    [:enum :closed :open])])

(def ^:private defaults {:end true :start false :size 8.0 :angle 20.0 :shape :closed})

(defn- point-at
  "Point at `d` pixels from the start of the run `[p0 p1 ..]` along its length; `[point index]` - `index` of the
  segment holding it; `nil` past the end."
  [run ^double d]
  (loop [i (long 0)
         acc 0.0]
    (when (m/< i (m/long-dec (count run)))
      (let [[^double x0 ^double y0] (run i)
            [^double x1 ^double y1] (run (m/long-inc i))
            dx (- x1 x0)
            dy (- y1 y0)
            l (m/hypot dx dy)]
        (if (and (m/pos? l) (m/<= d (m/+ acc l)))
          (let [t (m// (m/- d acc) l)]
            [[(m/+ x0 (m/* t dx))
              (m/+ y0 (m/* t dy))] i])
          (recur (m/long-inc i) (m/+ acc l)))))))

(defn- trim-start
  "The run without its first `d` pixels (the run itself for `d` 0; `nil` when not longer than `d`)."
  [run ^double d]
  (if (m/not-pos? d)
    run
    (when-let [[p ^long i] (point-at run d)]
      (if (m/< (m/+ d 1.0e-9) (polyline/path-length run))
        (into [p] (subvec run (m/long-inc i)))
        nil))))

(defn trimmed
  "`run` shortened by `start-gap` / `end-gap` pixels along its length; `nil` when nothing is left."
  [run ^double start-gap ^double end-gap]
  (some-> (trim-start (vec run) start-gap)
          rseq vec (trim-start end-gap) rseq vec))

(defn- head-points
  "`[tip left right]` of a head of `size` and half-angle `angle` (degrees) at the end of `run` (pointing from the point
  `size` pixels back)."
  [run ^double size ^double angle]
  (let [tip (peek run)
        back (or (first (point-at (vec (rseq run)) size)) (first run))
        [^double tx ^double ty] tip
        [^double bx ^double by] back
        dx (- tx bx)
        dy (- ty by)
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
  "Draw the head `[tip left right]` in line `style` (filled for `:closed`)."
  [canvas [tip left right] shape style]
  (if (= :closed shape)
    (do (c2d/set-color canvas (:color style))
        (c2d/path canvas [tip left right] true false))
    (polyline/stroke-runs! canvas [[left tip right]] style)))

(defn draw-run!
  "Stroke `run` in line `style` with `arrow` (an `:arrow` option or `nil`) and the gaps `[start end]` (pixels): trimmed,
  shortened under closed heads, then the heads drawn."
  [canvas run style [arrow start-gap end-gap]]
  (when-let [run (trimmed run (or start-gap 0.0) (or end-gap 0.0))]
    (let [{:keys [end start ^double size ^double angle shape]
           :or {size 0.0 angle 20.0}} (when arrow (merge defaults arrow))
          head-end (when end (head-points run size angle))
          head-start (when start (head-points (vec (rseq run)) size angle))
          closed? (= :closed shape)
          stroked (if closed?
                    (trimmed run (if head-start (m/* 0.9 size) 0.0) (if head-end (m/* 0.9 size) 0.0))
                    run)]
      (when stroked (polyline/stroke-runs! canvas [stroked] style))
      (when (and (some? (:color style)) (not= :none (:color style)))
        (doseq [h [head-end head-start] :when h]
          (draw-head! canvas h shape style))))))
