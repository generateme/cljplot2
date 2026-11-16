(ns cljplot2.graphics.arrow
  "Arrowheads and end gaps of stroked runs (vectors of `[x y]` pixel points), for the `:arrow`, `:start-gap` and
  `:end-gap` options of lines and segments.

  `:arrow` - `{:end true :start false :size 8 :shape :closed}`: heads at the run's end and / or start, `:size` pixels
  long (half as wide on each side), `:closed` - a filled triangle (the line stops at its base), `:open` - two strokes.
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
   (schema/optional :end schema/bool)
   (schema/optional :start schema/bool)
   (schema/optional :size schema/pos)
   (schema/optional :shape [:enum :closed :open])])

(def ^:private defaults {:end true :start false :size 8.0 :shape :closed})

(defn- point-at
  "Point at `d` pixels from the start of the run `[p0 p1 ..]` along its length; `[point index]` - `index` of the
  segment holding it; `nil` past the end."
  [run ^double d]
  (loop [i 0 acc 0.0]
    (when (< i (dec (count run)))
      (let [[x0 y0] (run i) [x1 y1] (run (inc i))
            l (m/hypot (- (double x1) (double x0)) (- (double y1) (double y0)))]
        (if (and (pos? l) (<= d (+ acc l)))
          (let [t (/ (- d acc) l)]
            [[(+ (double x0) (* t (- (double x1) (double x0)))) (+ (double y0) (* t (- (double y1) (double y0))))] i])
          (recur (inc i) (+ acc l)))))))

(defn- trim-start
  "The run without its first `d` pixels (the run itself for `d` 0; `nil` when not longer than `d`)."
  [run ^double d]
  (if (<= d 0.0)
    run
    (when-let [[p i] (point-at run d)]
      (if (< (+ d 1.0e-9) (polyline/path-length run))
        (into [p] (subvec run (inc (long i))))
        nil))))

(defn trimmed
  "`run` shortened by `start-gap` / `end-gap` pixels along its length; `nil` when nothing is left."
  [run start-gap end-gap]
  (some-> (trim-start (vec run) (double start-gap))
          rseq vec (trim-start (double end-gap)) rseq vec))

(defn- head-points
  "`[tip left right]` of a head of `size` at the end of `run` (pointing from the point `size` pixels back)."
  [run ^double size]
  (let [tip (peek run)
        back (or (first (point-at (vec (rseq run)) size)) (first run))
        [tx ty] tip [bx by] back
        dx (- (double tx) (double bx)) dy (- (double ty) (double by))
        l (m/hypot dx dy)]
    (when (pos? l)
      (let [ux (/ dx l) uy (/ dy l)
            half (* 0.5 size)
            base-x (- (double tx) (* ux size)) base-y (- (double ty) (* uy size))]
        [tip [(- base-x (* uy half)) (+ base-y (* ux half))] [(+ base-x (* uy half)) (- base-y (* ux half))]]))))

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
    (let [{:keys [end start size shape]} (when arrow (merge defaults arrow))
          size (double (or size 0.0))
          head-end (when end (head-points run size))
          head-start (when start (head-points (vec (rseq run)) size))
          closed? (= :closed shape)
          stroked (if closed?
                    (trimmed run (if head-start (* 0.9 size) 0.0) (if head-end (* 0.9 size) 0.0))
                    run)]
      (when stroked (polyline/stroke-runs! canvas [stroked] style))
      (when (and (some? (:color style)) (not= :none (:color style)))
        (doseq [h [head-end head-start] :when h]
          (draw-head! canvas h shape style))))))
