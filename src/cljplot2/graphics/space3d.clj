(ns cljplot2.graphics.space3d
  "Drawing of 3D charts' primitives and frame (see [[cljplot2.data.space3d]] for the projection).

  Primitives - maps with a `:depth` (view depth, larger - farther), drawn far to near by [[draw-primitives!]]:

  * `{:kind :point :at Vec2 :style st}` - a shape (`st` - `:shape :size :color :stroke-color :stroke-width`)
  * `{:kind :line :from Vec2 :to Vec2 :color c :width w}`
  * `{:kind :polygon :points [Vec2 ..] :fill c :stroke c :stroke-width w}` (`:stroke :none` - no outline)

  [[frame-margin]] - the pixels the frame needs around the box. The frame is drawn in two parts around the data:
  [[draw-frame-back!]] - grids on the floor and the back walls (theme `[:panel :grid :x]`, z lines `[:panel :grid :y]`)
  and the hidden box edges (dashed);
  [[draw-frame-front!]] - the visible box edges and, per axis, ticks, horizontal tick labels and a title along its
  edge ([[cljplot2.data.space3d/axis-edges]]), styled by theme `[:axis :x]` (lines, tick length, label gap; tick
  spacing of `[:axis :x]` / `[:axis :y]` for z) and the `:tick-label` / `:axis-title` text roles."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.content.label :as label]
            [cljplot2.data.space3d :as space]
            [cljplot2.graphics.measure :as measure]
            [cljplot2.graphics.polyline :as polyline]
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

(defn- tick-count
  "Ticks asked for an axis of `k` along an edge `length` pixels long: the edge over the theme's tick spacing of the 2D
  axis drawn the same way (z - vertical - as y, x and y as x), at least 3."
  ^long [env k ^double length]
  (let [spacing (double (get-in env [:theme :axis (if (= k :z) :y :x) :tick-spacing]))]
    (long (m/max 3.0 (m/floor (/ length spacing))))))

(defn frame-margin
  "Pixels to keep around the box for the axes of `domains` with `titles` (`{:x t ..}`, `nil` - none) when `axes?`:
  tick length, label gaps, the largest tick label (of the most ticks an edge in a `w` x `h` panel can ask for, see
  [[tick-count]]) and a title line. `env` - `:canvas`, `:theme`."
  [domains titles axes? [w h] env]
  (if-not axes?
    4.0
    (let [{:keys [tick-length label-gap]} (get-in env [:theme :axis :x])
          longest (m/min (double w) (double h))
          labels (mapcat #(:labels (axis-ticks % (domains %) (tick-count env % longest))) space/axes)
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

(defn edge-angle
  "Clockwise rotation (degrees, in `(-90 90]`) of text running along the pixel direction `along` (y down): readable
  left to right, a vertical edge bottom to top (-90 is drawn as 270)."
  ^double [^Vec2 along]
  (let [deg (m/degrees (m/atan2 (.y along) (.x along)))
        deg (double (cond (> deg 90.0) (- deg 180.0) (<= deg -90.0) (+ deg 180.0) :else deg))]
    (if (== deg 90.0) 270.0 (if (neg? deg) (+ deg 360.0) deg))))

(defn title-distance
  "Pixels from an axis edge to the centre of its title: past the ticks (`tick-length`), the tick labels (`widest` -
  their largest extent away from the edge) with a `gap` on each side, and half the title's thickness - its
  unrotated text height (`size`), as the title runs along the edge whatever its length."
  ^double [tick-length gap widest ^Vec2 size]
  (+ (double tick-length) (* 2.0 (double gap)) (double widest) (* 0.5 (.y size))))

(defn- draw-axis!
  [canvas k {:keys [from to]} {:keys [corner centre]} domain title env]
  (let [{:keys [line-color tick-color tick-length tick-width label-gap]} (get-in env [:theme :axis :x])
        ^Vec2 a (corner from) ^Vec2 b (corner to)
        along (v/sub b a)
        o (outward (v/mult (v/add a b) 0.5) along centre)
        {:keys [ticks labels]} (axis-ticks k domain (tick-count env k (v/mag along)))
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
      ;; the title runs along its edge, turned to read left to right (or bottom to top)
      (let [content {:kind :label :role :axis-title :text (str title) :rotation (edge-angle along)}
            ^Vec2 c (v/add (v/mult (v/add a b) 0.5)
                           (v/mult o (title-distance tick-length gap @widest (text-size :axis-title (str title) env))))]
        (label/draw-at! canvas content [:center :middle] (.x c) (.y c) env)))))

(defn- line!
  [canvas ^Vec2 a ^Vec2 b]
  (c2d/line canvas (.x a) (.y a) (.x b) (.y b)))

(def hidden-edge-dash
  "Dash pattern (pixels) of the hidden box edges."
  [3.0 3.0])

(defn- set-grid-line!
  "Set the canvas stroke and color of grid line style `line` (a theme `[:panel :grid k]` value); `false` for `:none`."
  [canvas {:keys [color width dash] :as line}]
  (if (or (nil? line) (theme/none? line) (theme/none? color))
    false
    (do (c2d/set-color canvas color)
        (c2d/set-stroke-custom canvas (cond-> {:size (double width) :cap :butt}
                                        (polyline/dash-pattern dash) (assoc :dash (polyline/dash-pattern dash))))
        true)))

(defn- face-grid-lines
  "Grid lines `[[from to] ..]` (pixel `Vec2`s) on the box face of `axis` at `side` (0 low / 1 high): for each other
  axis `k` a line across the face at each tick of `k`'s domain, paired with `k` - `[k [from to]]`."
  [{:keys [corner]} axis side domains env]
  (let [i (.indexOf ^java.util.List space/axes axis)
        others (remove #{i} [0 1 2])]
    (for [a others
          :let [b (first (remove #{a} others))
                k (space/axes a)
                [lo hi] (domains k)
                ^Vec2 a0 (corner (assoc [0 0 0] i side))
                edge-px (v/mag (v/sub (corner (assoc (assoc [0 0 0] i side) a 1)) a0))]
          t (:ticks (axis-ticks k (domains k) (tick-count env k edge-px)))
          :let [f (/ (- (double t) (double lo)) (- (double hi) (double lo)))
                at (fn [fb] (corner (-> [0 0 0] (assoc i side) (assoc a f) (assoc b fb))))]]
      [k [(at 0.0) (at 1.0)]])))

(defn- back-wall-side
  "Side (0 / 1) of the box face of `axis` turned away from the viewer in `view`, `nil` when neither is (edge-on)."
  [view axis]
  (first (filter #(> (.z ^fastmath.vector.Vec3 (first (space/face-normal view axis %))) 1.0e-9) [0 1])))

(defn- draw-box-grid!
  "Grid lines behind the data at the ticks: on the floor (low z; x and y ticks) and on the two side walls turned away
  from the viewer (the x wall at y and z ticks, the y wall at x and z ticks). Lines at x / y ticks use theme
  `[:panel :grid :x]`, lines at z ticks `[:panel :grid :y]` (`:none` - none)."
  [canvas proj view domains env]
  (let [faces (cons [:z 0] (keep (fn [axis] (some->> (back-wall-side view axis) (vector axis))) [:x :y]))]
    (doseq [[axis side] faces
            [k [from to]] (face-grid-lines proj axis side domains env)
            :when (set-grid-line! canvas (get-in env [:theme :panel :grid (if (= k :z) :y :x)]))]
      (line! canvas from to))))

(defn draw-frame-back!
  "Draw the box frame behind the data of `proj` (see [[cljplot2.data.space3d/projector]]) in `view`: with `box?` the
  grids of the floor and the back walls (see [[draw-box-grid!]]) and the hidden box edges (dashed,
  [[hidden-edge-dash]]), so back and front read apart."
  [canvas proj {:keys [view domains box?]} env]
  (when box?
    (let [env (assoc env :canvas canvas)
          corner (:corner proj)]
      (draw-box-grid! canvas proj view domains env)
      (c2d/set-color canvas (get-in env [:theme :axis :x :line-color]))
      (c2d/set-stroke-custom canvas {:size 1.0 :cap :butt :dash (polyline/dash-pattern hidden-edge-dash)})
      (doseq [{:keys [from to]} (space/hidden-edges view)]
        (line! canvas (corner from) (corner to))))))

(def ^:const min-axis-length
  "Pixels an axis edge needs to carry ticks; shorter (end-on) edges get no axis."
  4.0)

(declare draw-frame-front!)

(defn draw-scene!
  "Draw the 3D charts `layers` - `[[chart computed] ..]`, one panel's computed data of each (`:domains`, `:frame` -
  the box, see [[cljplot2.chart.cloud/frame-settings]]) - in one box in a `w` x `h` panel: the frame of the first
  layer (several 3D charts share their explicit domains), the back of the frame, the primitives of every chart
  (`((:primitives chart) chart computed point scales env)`, `point` - data -> `[pixel depth]`) sorted far to near
  together, then the front of the frame."
  [canvas layers [w h] scales env]
  (let [[_ {:keys [domains frame]}] (first layers)
        env (assoc env :canvas canvas)
        frame (assoc frame :domains domains)
        margin (frame-margin domains (:titles frame) (:axes? frame) [w h] env)
        proj (space/projector frame {:w w :h h :margin margin})]
    (when (pos? (double (:scale proj)))
      (draw-frame-back! canvas proj frame env)
      (draw-primitives! canvas (mapcat (fn [[chart computed]] ((:primitives chart) chart computed (:point proj) scales env)) layers))
      (draw-frame-front! canvas proj frame env))))

(defn draw-frame-front!
  "Draw the box frame in front of the data of `proj`: with `box?` the visible box edges, with `axes?` the axes of
  `domains` with `titles`."
  [canvas proj {:keys [view domains titles box? axes?]} env]
  (let [env (assoc env :canvas canvas)
        corner (:corner proj)]
    (when box?
      (c2d/set-color canvas (get-in env [:theme :axis :x :line-color]))
      (c2d/set-stroke-custom canvas {:size 1.0})
      (doseq [{:keys [from to]} (space/visible-edges view)]
        (line! canvas (corner from) (corner to))))
    (when axes?
      (doseq [[k {:keys [from to] :as edge}] (space/axis-edges view corner)
              ;; an edge seen end-on (z from straight above) has no room for an axis
              :when (>= (v/mag (v/sub (corner to) (corner from))) min-axis-length)]
        (draw-axis! canvas k edge proj (domains k) (get titles k) env)))))
