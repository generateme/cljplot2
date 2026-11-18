(ns cljplot2.graphics.space3d
  "Drawing of 3D charts' primitives and frame (see [[cljplot2.data.space3d]] for the projection).

  Primitives - maps with a `:depth` (view depth, larger - farther), drawn far to near by [[draw-primitives!]]:

  * `{:kind :point :at Vec2 :style st}` - a shape (`st` - `:shape :size :color :stroke-color :stroke-width`)
  * `{:kind :line :from Vec2 :to Vec2 :color c :width w}`
  * `{:kind :polygon :points [Vec2 ..] :fill c :stroke c :stroke-width w}` (`:stroke :none` - no outline)

  [[frame-margin]] - the pixels the frame needs around the box; [[draw-frame!]] - the visible box edges and, per axis,
  ticks, tick labels and a title on its edge ([[cljplot2.data.space3d/axis-edges]]), styled by theme `[:axis :x]`
  (lines, tick length, label gap, tick spacing) and the `:tick-label` / `:axis-title` text roles."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.content.label :as label]
            [cljplot2.data.space3d :as space]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.graphics.shape :as shape]
            [cljplot2.scale.position :as scale]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- draw-primitive!
  [canvas {:keys [kind] :as p}]
  (case kind
    :point (let [st (:style p)]
             (c2d/set-stroke-custom canvas {:size (double (or (:stroke-width st) 1.0))})
             (shape/draw! canvas (:shape st) (:at p) st))
    :line (let [^Vec2 a (:from p) ^Vec2 b (:to p)]
            (c2d/set-stroke-custom canvas {:size (double (:width p)) :cap :round})
            (c2d/set-color canvas (:color p))
            (c2d/line canvas (.x a) (.y a) (.x b) (.y b)))
    :polygon (let [pts (mapv (fn [^Vec2 q] [(.x q) (.y q)]) (:points p))]
               (c2d/set-color canvas (:fill p))
               (c2d/path canvas pts true false)
               (when (and (not (theme/none? (:stroke p))) (pos? (double (or (:stroke-width p) 0.0))))
                 (c2d/set-stroke-custom canvas {:size (double (:stroke-width p))})
                 (c2d/set-color canvas (:stroke p))
                 (c2d/path canvas pts true true)))))

(defn draw-primitives!
  "Draw `primitives` far to near (descending `:depth`; ties keep their order)."
  [canvas primitives]
  (doseq [p (sort-by #(- (double (:depth %))) primitives)]
    (draw-primitive! canvas p)))

;; frame

(defn axis-ticks
  "`{:ticks [..] :labels [..]}` of an axis over `domain` with about `n` ticks (a linear scale, no nice extension)."
  [k domain n]
  (let [f (scale/finalize (scale/scale-spec k {:domain domain :nice false}) [0.0 1.0] (long (m/max 2 (long n))))]
    {:ticks (:ticks f) :labels (:tick-labels f)}))

(defn- text-size
  ^Vec2 [role text env]
  (measure/natural-size {:kind :label :role role :text text} env))

(defn frame-margin
  "Pixels to keep around the box for the axes of `domains` with `titles` (`{:x t ..}`, `nil` - none) when `axes?`:
  tick length, label gap, the largest tick label (about 5 ticks) and a title line. `env` - `:canvas`, `:theme`."
  [domains titles axes? env]
  (if-not axes?
    4.0
    (let [{:keys [tick-length label-gap]} (get-in env [:theme :axis :x])
          labels (mapcat #(:labels (axis-ticks % (domains %) 5)) space/axes)
          widest (double (reduce max 0.0 (map #(let [^Vec2 s (text-size :tick-label % env)] (m/max (.x s) (.y s))) labels)))
          title (double (reduce max 0.0 (keep (fn [k] (when-some [t (get titles k)]
                                                         (.y (text-size :axis-title (str t) env))))
                                               space/axes)))]
      (+ (double tick-length) (* 2.0 (double label-gap)) widest title 2.0))))

(defn- outward
  "Unit pixel direction away from the box at `edge-mid` (pixel `Vec2`) across the edge direction `along`: from the
  box `centre`, without the component along the edge; screen down when undefined."
  ^Vec2 [^Vec2 edge-mid ^Vec2 along ^Vec2 centre]
  (let [d (v/sub edge-mid centre)
        a (if (pos? (v/mag along)) (v/normalize along) along)
        o (v/sub d (v/mult a (v/dot d a)))]
    (if (> (v/mag o) 1.0e-6) (v/normalize o) (v/vec2 0.0 1.0))))

(defn- extent-along
  "Half the length of a `size` box along unit direction `o`."
  ^double [^Vec2 size ^Vec2 o]
  (* 0.5 (+ (* (m/abs (.x o)) (.x size)) (* (m/abs (.y o)) (.y size)))))

(defn- draw-axis!
  [canvas k {:keys [from to]} {:keys [corner centre]} domain title env]
  (let [{:keys [line-color tick-color tick-length tick-width label-gap]} (get-in env [:theme :axis :x])
        ;; the target tick distance of the 2D axis drawn the same way: z (vertical) as y, x and y as x
        tick-spacing (get-in env [:theme :axis (if (= k :z) :y :x) :tick-spacing])
        ^Vec2 a (corner from) ^Vec2 b (corner to)
        along (v/sub b a)
        o (outward (v/mult (v/add a b) 0.5) along centre)
        {:keys [ticks labels]} (axis-ticks k domain (m/max 3.0 (m/floor (/ (v/mag along) (double tick-spacing)))))
        [lo hi] domain
        at (fn [t] (v/add a (v/mult along (/ (- (double t) (double lo)) (- (double hi) (double lo))))))
        tick-length (double tick-length) gap (double label-gap)
        widest (atom 0.0)]
    (when-not (theme/none? line-color)
      (c2d/set-color canvas line-color)
      (c2d/set-stroke-custom canvas {:size 1.0})
      (c2d/line canvas (.x a) (.y a) (.x b) (.y b)))
    (doseq [[t text] (map vector ticks labels)
            :let [^Vec2 p (at t)
                  ^Vec2 q (v/add p (v/mult o tick-length))
                  size (text-size :tick-label text env)
                  e (extent-along size o)
                  ^Vec2 c (v/add q (v/mult o (+ gap e)))]]
      (swap! widest max (* 2.0 e))
      (when-not (theme/none? tick-color)
        (c2d/set-color canvas tick-color)
        (c2d/set-stroke-custom canvas {:size (double tick-width)})
        (c2d/line canvas (.x p) (.y p) (.x q) (.y q)))
      (label/draw-at! canvas {:kind :label :role :tick-label :text text} [:center :middle] (.x c) (.y c) env))
    (when (some? title)
      ;; a title beside a (mostly) vertical edge runs along it, as a 2D y axis title
      (let [content (cond-> {:kind :label :role :axis-title :text (str title)}
                      (> (m/abs (.x o)) (m/abs (.y o))) (assoc :rotation 270))
            ^Vec2 size (measure/natural-size content env)
            ^Vec2 c (v/add (v/mult (v/add a b) 0.5)
                           (v/mult o (+ tick-length gap (double @widest) gap (extent-along size o))))]
        (label/draw-at! canvas content [:center :middle] (.x c) (.y c) env)))))

(defn draw-frame!
  "Draw the box frame of `proj` (see [[cljplot2.data.space3d/projector]]) in `view`: the visible box edges (`box?`)
  and the axes (`axes?`) of `domains` with `titles`."
  [canvas proj {:keys [view domains titles box? axes?]} env]
  (let [env (assoc env :canvas canvas)
        corner (:corner proj)]
    (when box?
      (c2d/set-color canvas (get-in env [:theme :axis :x :line-color]))
      (c2d/set-stroke-custom canvas {:size 1.0})
      (doseq [{:keys [from to]} (space/visible-edges view)
              :let [^Vec2 a (corner from) ^Vec2 b (corner to)]]
        (c2d/line canvas (.x a) (.y a) (.x b) (.y b))))
    (when axes?
      (doseq [[k edge] (space/axis-edges view corner)]
        (draw-axis! canvas k edge proj (domains k) (get titles k) env)))))
