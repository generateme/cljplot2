(ns cljplot2.graphics.space3d
  "Drawing of 3D charts' primitives and frame (see [[cljplot2.data.space3d]] for the projection).

  Primitives - maps with a `:depth` (view depth, larger - farther), drawn far to near by [[draw-primitives!]]:

  * `{:kind :point :at Vec2 :style st}` - a shape (`st` - `:shape :size :color :stroke-color :stroke-width`)
  * `{:kind :line :from Vec2 :to Vec2 :color c :width w}`
  * `{:kind :polygon :points [Vec2 ..] :fill c :stroke c :stroke-width w}` (`:stroke :none` - no outline)
  * `{:kind :path :points [Vec2 ..] :closed? b :color c :width w}` - a polyline
  * `{:kind :region :rings [[Vec2 ..] ..] :fill c}` - an even-odd filled region (holes open)

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
    :path (do (c2d/set-stroke-custom canvas {:size (double (:width p)) :cap :round :join :round})
              (c2d/set-color canvas (:color p))
              (c2d/path canvas (mapv (fn [^Vec2 q] [(.x q) (.y q)]) (:points p)) (boolean (:closed? p)) true))
    :region (do (c2d/set-color canvas (:fill p))
                (c2d/shape canvas (polyline/rings-path (map (fn [ring] (mapv (fn [^Vec2 q] [(.x q) (.y q)]) ring)) (:rings p))) false))
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

(defn level-text
  "Tick label of a categorical level `v`: a keyword's name, else `(str v)`."
  [v]
  (if (keyword? v) (name v) (str v)))

(defn- ends-ticked?
  "Do the `ticks` include both ends of `domain` (within a billionth of its span)?"
  [{:keys [ticks]} [lo hi]]
  (let [tolerance (* 1.0e-9 (m/abs (- (double hi) (double lo))))
        on? (fn [end] (some #(<= (m/abs (- (double %) (double end))) tolerance) ticks))]
    (boolean (and (on? lo) (on? hi)))))

(defn edge-ticks
  "`{:ticks [..] :labels [..]}` of the axis `k` over `domain` for about `n` ticks: with `levels` (a categorical axis,
  level i at i + 1) a tick per level, labelled by [[level-text]] - every level when they're at most `n`, else every
  `ceil(levels / n)`-th from the first (the others `nil`); else the most ticks, at most `n`, that include both
  domain ends (a nice domain, see [[cljplot2.data.space3d/nice-domain]]), or `n` ticks when no count does."
  [k domain levels n]
  (if levels
    (let [every (long (m/ceil (/ (double (count levels)) (double (m/max 1 (long n))))))]
      {:ticks (mapv double (range 1 (inc (count levels))))
       :labels (vec (map-indexed (fn [i l] (when (zero? (long (mod (long i) every))) (level-text l))) levels))})
    (or (first (filter #(ends-ticked? % domain) (map #(axis-ticks k domain %) (range (long n) 1 -1))))
        (axis-ticks k domain n))))

(defn frame-margin
  "Pixels to keep around the box for the axes of a `frame` (`:domains`, `:levels`, `:titles` - `{:x t ..}`, `nil` -
  none) when `:axes?`: tick length, label gaps, the largest tick label (of the most ticks an edge in a `w` x `h` panel
  can ask for, see [[tick-count]], [[edge-ticks]]; of every level of a categorical axis) and a title line. `env` - `:canvas`, `:theme`."
  [{:keys [domains levels titles axes?]} [w h] env]
  (if-not axes?
    4.0
    (let [{:keys [tick-length label-gap]} (get-in env [:theme :axis :x])
          longest (m/min (double w) (double h))
          ;; every level label: an edge thins its own (see edge-ticks), any of them may be drawn
          labels (mapcat #(if-let [ls (get levels %)]
                            (map level-text ls)
                            (:labels (edge-ticks % (domains %) nil (tick-count env % longest))))
                         space/axes)
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

(defn- label-box
  "Box `[x0 y0 x1 y1]` of a label of `size` centred at `c`."
  [^Vec2 c ^Vec2 size]
  (let [hw (* 0.5 (.x size)) hh (* 0.5 (.y size))]
    [(- (.x c) hw) (- (.y c) hh) (+ (.x c) hw) (+ (.y c) hh)]))

(defn- boxes-overlap?
  [[ax0 ay0 ax1 ay1] [bx0 by0 bx1 by1]]
  (and (< (double ax0) (double bx1)) (< (double bx0) (double ax1)) (< (double ay0) (double by1)) (< (double by0) (double ay1))))

(defn edge-point
  "Pixel of the point at fraction `f` along the box `edge` (`{:axis :from}`) - the projected surface point (`corner`
  of fractional bits, see [[cljplot2.data.space3d/projector]]): under perspective equal steps along an edge shrink
  with depth, not equal on screen."
  ^Vec2 [corner {:keys [axis from]} ^double f]
  (corner (assoc from (space/axis-index axis) f)))

(defn- draw-axis!
  "Draw the axis of `k` on its `edge` (ticks, labels, title; see the namespace doc); a tick label overlapping one of
  the label boxes `taken` (`[x0 y0 x1 y1]`, drawn by the axes before, e.g. at a shared corner) is left out. Returns
  the boxes of the labels this axis drew."
  [canvas k {:keys [from to] :as edge} {:keys [corner centre]} {:keys [domains levels titles]} taken env]
  (let [{:keys [line-color tick-color tick-length tick-width label-gap]} (get-in env [:theme :axis :x])
        ^Vec2 a (corner from) ^Vec2 b (corner to)
        along (v/sub b a)
        o (outward (v/mult (v/add a b) 0.5) along centre)
        domain (domains k)
        title (get titles k)
        {:keys [ticks labels]} (edge-ticks k domain (get levels k) (tick-count env k (v/mag along)))
        [lo hi] domain
        labelled (atom [])
        at (fn [t] (edge-point corner edge (/ (- (double t) (double lo)) (- (double hi) (double lo)))))
        tick-length (double tick-length) gap (double label-gap)
        widest (atom 0.0)]
    (when-not (theme/none? line-color)
      (c2d/set-color canvas line-color)
      (c2d/set-stroke-custom canvas {:size 1.0})
      (c2d/line canvas (.x a) (.y a) (.x b) (.y b)))
    (doseq [[t text] (map vector ticks labels)
            :let [^Vec2 p (at t)
                  ^Vec2 q (v/add p (v/mult o tick-length))]]
      (when-not (theme/none? tick-color)
        (c2d/set-color canvas tick-color)
        (c2d/set-stroke-custom canvas {:size (double tick-width)})
        (c2d/line canvas (.x p) (.y p) (.x q) (.y q)))
      ;; a thinned categorical axis has ticks without labels
      (when (some? text)
        (let [^Vec2 size (text-size :tick-label text env)
              e (extent-along size o)
              ^Vec2 c (v/add q (v/mult o (+ gap e)))
              box (label-box c size)]
          (swap! widest max (* 2.0 e))
          (when-not (some #(boxes-overlap? box %) taken)
            (swap! labelled conj box)
            (label/draw-at! canvas {:kind :label :role :tick-label :text text} [:center :middle] (.x c) (.y c) env)))))
    (when (some? title)
      ;; the title runs along its edge, turned to read left to right (or bottom to top)
      (let [content {:kind :label :role :axis-title :text (str title) :rotation (edge-angle along)}
            ^Vec2 c (v/add (v/mult (v/add a b) 0.5)
                           (v/mult o (title-distance tick-length gap @widest (text-size :axis-title (str title) env))))]
        (label/draw-at! canvas content [:center :middle] (.x c) (.y c) env)))
    @labelled))

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
  [{:keys [corner]} axis side {:keys [domains levels]} env]
  (let [i (space/axis-index axis)
        others (remove #{i} [0 1 2])]
    (for [a others
          :let [b (first (remove #{a} others))
                k (space/axes a)
                [lo hi] (domains k)
                ^Vec2 a0 (corner (assoc [0 0 0] i side))
                edge-px (v/mag (v/sub (corner (assoc (assoc [0 0 0] i side) a 1)) a0))]
          t (:ticks (edge-ticks k (domains k) (get levels k) (tick-count env k edge-px)))
          :let [f (/ (- (double t) (double lo)) (- (double hi) (double lo)))
                at (fn [fb] (corner (-> [0 0 0] (assoc i side) (assoc a f) (assoc b fb))))]]
      [k [(at 0.0) (at 1.0)]])))

(defn- draw-box-grid!
  "Grid lines behind the data at the ticks: on the floor (low z; x and y ticks) and on the two side walls turned away
  from the viewer (the x wall at y and z ticks, the y wall at x and z ticks). Lines at x / y ticks use theme
  `[:panel :grid :x]`, lines at z ticks `[:panel :grid :y]` (`:none` - none)."
  [canvas proj {:keys [view aspect] :as frame} env]
  (let [faces (cons [:z 0] (keep (fn [axis] (some->> (space/far-side view aspect axis) (vector axis))) [:x :y]))]
    (doseq [[axis side] faces
            [k [from to]] (face-grid-lines proj axis side frame env)
            :when (set-grid-line! canvas (get-in env [:theme :panel :grid (if (= k :z) :y :x)]))]
      (line! canvas from to))))

(defn draw-frame-back!
  "Draw the box frame behind the data of `proj` (see [[cljplot2.data.space3d/projector]]) in `view`: with `box?` the
  grids of the floor and the back walls (see [[draw-box-grid!]]) and the hidden box edges (dashed,
  [[hidden-edge-dash]]), so back and front read apart."
  [canvas proj {:keys [view aspect box?] :as frame} env]
  (when box?
    (let [env (assoc env :canvas canvas)
          corner (:corner proj)]
      (draw-box-grid! canvas proj frame env)
      (c2d/set-color canvas (get-in env [:theme :axis :x :line-color]))
      (c2d/set-stroke-custom canvas {:size 1.0 :cap :butt :dash (polyline/dash-pattern hidden-edge-dash)})
      (doseq [{:keys [from to]} (space/hidden-edges view aspect)]
        (line! canvas (corner from) (corner to))))))

(def ^:const min-axis-length
  "Pixels an axis edge needs to carry ticks; shorter (end-on) edges get no axis."
  4.0)

(declare draw-frame-front!)

(defn scene-frame
  "The box of one panel's `computeds` (the computed data of each 3D chart, in chart order): the first's `:frame` with
  its `:domains` (several 3D charts share their explicit domains), the later charts' `:frame-overrides` (their
  explicitly set frame options, see [[cljplot2.chart.cloud/frame-overrides]]) merged over it in order - `:view` and
  `:titles` key by key, `:titles nil` clearing them."
  [computeds]
  (let [{:keys [frame domains]} (first computeds)]
    (reduce (fn [f {o :frame-overrides}]
              (cond-> (merge f (dissoc o :view :titles))
                (:view o) (update :view merge (:view o))
                (contains? o :titles) (assoc :titles (when-let [t (:titles o)] (merge (:titles f) t)))))
            (assoc frame :domains domains)
            (rest computeds))))

(defn draw-scene!
  "Draw the 3D charts `layers` - `[[chart computed] ..]`, one panel's computed data of each (`:domains`, `:frame`,
  `:frame-overrides`) - in one box ([[scene-frame]]) in a `w` x `h` panel: the back of the frame, the primitives of
  every chart (`((:primitives chart) chart computed point scales env)`, `point` - data -> `[pixel depth]`; the
  computed `:frame` replaced by the scene's, so faces and walls turn by the box's view) sorted far to near together,
  then the front of the frame."
  [canvas layers [w h] scales env]
  (let [env (assoc env :canvas canvas)
        frame (scene-frame (map second layers))
        margin (frame-margin frame [w h] env)
        proj (space/projector frame {:w w :h h :margin margin})]
    (when (pos? (double (:scale proj)))
      (draw-frame-back! canvas proj frame env)
      (draw-primitives! canvas (mapcat (fn [[chart computed]]
                                         ((:primitives chart) chart (assoc computed :frame frame) (:point proj) scales env))
                                       layers))
      (draw-frame-front! canvas proj frame env))))

(defn draw-chart
  "Draw function of the 3D charts' contract (see [[cljplot2.chart]]): one panel's box with this chart alone (a plot's
  panel draws all its 3D charts in one scene instead, see [[draw-scene!]])."
  [chart computed canvas {:keys [w h]} scales env]
  (draw-scene! canvas [[chart computed]] [w h] scales env))

(defn draw-frame-front!
  "Draw the box frame in front of the data of `proj`: with `box?` the visible box edges, with `axes?` the axes of the
  `frame` (`:domains`, `:levels`, `:titles`), x, y, z in turn - a tick label overlapping an earlier axis's label
  left out."
  [canvas proj {:keys [view aspect box? axes?] :as frame} env]
  (let [env (assoc env :canvas canvas)
        corner (:corner proj)]
    (when box?
      (c2d/set-color canvas (get-in env [:theme :axis :x :line-color]))
      (c2d/set-stroke-custom canvas {:size 1.0})
      (doseq [{:keys [from to]} (space/visible-edges view aspect)]
        (line! canvas (corner from) (corner to))))
    (when axes?
      (reduce (fn [taken [k {:keys [from to] :as edge}]]
                ;; an edge seen end-on (z from straight above) has no room for an axis
                (if (>= (v/mag (v/sub (corner to) (corner from))) min-axis-length)
                  (into taken (draw-axis! canvas k edge proj frame taken env))
                  taken))
              [] (space/axis-edges view aspect corner)))))
