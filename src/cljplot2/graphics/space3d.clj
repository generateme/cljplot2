(ns cljplot2.graphics.space3d
  "Drawing of 3D charts' primitives and frame (see [[cljplot2.data.space3d]] for the projection).

  Primitives - maps with a `:depth` (view depth, larger - farther), drawn far to near by [[draw-primitives!]]:

  * `{:kind :point :at Vec2 :style st}` - a shape (`st` - `:shape :size :color :stroke-color :stroke-width`)
  * `{:kind :line :from Vec2 :to Vec2 :color c :width w}`
  * `{:kind :polygon :points [Vec2 ..] :fill c :stroke c :stroke-width w}` (`:stroke :none` - no outline)
  * `{:kind :path :points [Vec2 ..] :closed? b :color c :width w}` - a polyline
  * `{:kind :region :rings [[Vec2 ..] ..] :fill c}` - an even-odd filled region (holes open)

  [[frame-margin]], [[frame-margins]] - the pixels the frame needs around the box. The frame is drawn in two parts around the data:
  [[draw-frame-back!]] - grids on the floor and the back walls (theme `[:panel :grid :x]`, z lines `[:panel :grid :y]`)
  and the hidden box edges (dashed);
  [[draw-frame-front!]] - the visible box edges and, per axis, ticks, horizontal tick labels and a title along its
  edge ([[cljplot2.data.space3d/axis-edges]]), styled by theme `[:axis :x]` (lines, tick length, label gap; tick
  spacing of `[:axis :x]` / `[:axis :y]` for z) and the `:tick-label` / `:axis-title` text roles.

  Functions: [[draw-primitives!]], [[draw-scene!]], [[draw-chart]], [[scene-frame]], frame [[frame-margin]],
  [[frame-margins]], [[draw-frame-back!]], [[draw-frame-front!]], axes [[axis-ticks]], [[edge-ticks]],
  [[edge-point]], [[edge-angle]], [[title-distance]]."
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
  "Draws one primitive (see the namespace doc).

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `p` (map): the primitive - `:kind` `:point`, `:line`, `:path`, `:region` or `:polygon` with its keys; points are pixel `Vec2`s.

  Returns the canvas or `nil`."
  [canvas {:keys [kind] :as p}]
  (case kind
    :point (let [st (:style p)]
             (c2d/set-stroke-custom canvas {:size (or (:stroke-width st) 1.0)})
             (shape/draw! canvas (:shape st) (:at p) st))
    :line (do (c2d/set-stroke-custom canvas {:size (:width p) :cap :round})
              (c2d/set-color canvas (:color p))
              (c2d/line canvas (:from p) (:to p)))
    :path (do (c2d/set-stroke-custom canvas {:size (:width p) :cap :round :join :round})
              (c2d/set-color canvas (:color p))
              (c2d/path canvas (:points p) (:closed? p) true))
    :region (do (c2d/set-color canvas (:fill p))
                (c2d/shape canvas (polyline/rings-path (:rings p)) false))
    :polygon (let [pts (:points p)]
               (c2d/set-color canvas (:fill p))
               (c2d/path canvas pts true false)
               (when (and (not (theme/none? (:stroke p))) (m/pos? (double (or (:stroke-width p) 0.0))))
                 (c2d/set-stroke-custom canvas {:size (:stroke-width p)})
                 (c2d/set-color canvas (:stroke p))
                 (c2d/path canvas pts true true)))))

(defn draw-primitives!
  "Draws primitives far to near.

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `primitives` (sequence of maps): the primitives (see the namespace doc), each with a `:depth`.

  Returns `nil`; descending `:depth`, ties keep their order."
  [canvas primitives]
  (doseq [p (sort-by #(m/- (double (:depth %))) primitives)]
    (draw-primitive! canvas p)))

;; frame

(defn axis-ticks
  "Gives the ticks of an axis over a domain (a linear scale, no nice extension).

  Parameters:

  - `k` (keyword): the axis.
  - `domain` (`[lo hi]`): the domain.
  - `n` (number): about how many ticks, at least 2.

  Returns `{:ticks [..] :labels [..]}`."
  [k domain n]
  (let [f (scale/finalize (scale/scale-spec k {:domain domain :nice false}) [0.0 1.0] (long (m/max 2 (long n))))]
    {:ticks (:ticks f) :labels (:tick-labels f)}))

(defn- text-size
  "Gives the natural size of a label text in a text role (a `Vec2`).

  Parameters:

  - `role` (keyword): the text role.
  - `text` (string): the text.
  - `env` (map): `:canvas`, `:theme`."
  ^Vec2 [role text env]
  (measure/natural-size {:kind :label :role role :text text} env))

(defn- tick-count
  "Gives the ticks asked for an axis along an edge: the edge over the theme's tick spacing of the 2D axis drawn the same way (z - vertical - as y, x and y as x).

  Parameters:

  - `env` (map): `:theme`.
  - `k` (keyword): the axis.
  - `length` (double): the edge's length in pixels.

  Returns the count, at least 3."
  ^long [env k ^double length]
  (let [spacing (double (get-in env [:theme :axis (if (= k :z) :y :x) :tick-spacing]))]
    (long (m/max 3.0 (m/floor (m// length spacing))))))

(def ^{:private true :const true :tag 'double} end-tolerance
  "Distance from a domain end, as a fraction of the span, within which a tick counts as on the end."
  1.0e-9)

(defn- ends-ticked?
  "Tests whether ticks include both ends of a domain (within [[end-tolerance]] of its span).

  Parameters:

  - `axis-ticks` (map): `:ticks` - the tick values.
  - `[lo hi]`: the domain.

  Returns truthy when both ends are ticked, else `nil`."
  [{:keys [ticks]} [^double lo ^double hi]]
  (let [tolerance (m/* end-tolerance (m/abs (m/- hi lo)))
        on? (fn [^double end] (some #(m/<= (m/abs (m/- (double %) end)) tolerance) ticks))]
    (and (on? lo) (on? hi))))

(defn edge-ticks
  "Gives the ticks of an axis along a box edge.

  Parameters:

  - `k` (keyword): the axis.
  - `domain` (`[lo hi]`): the domain.
  - `levels` (sequence or `nil`): the levels of a categorical axis (level i at i + 1).
  - `n` (number): about how many ticks.

  Returns `{:ticks [..] :labels [..]}`: with `levels` a tick per level, labelled by [[cljplot2.content.label/name-text]] - every level when they're at most `n`, else every `ceil(levels / n)`-th from the first (the others `nil`); else the most ticks, at most `n`, that include both domain ends (a nice domain, see [[cljplot2.data.space3d/nice-domain]]), or `n` ticks when no count does."
  [k domain levels n]
  (if levels
    (let [every-nth (long (m/ceil (m// (double (count levels)) (double (m/max 1 (long n))))))]
      {:ticks (mapv double (range 1 (m/long-inc (count levels))))
       :labels (vec (map-indexed (fn [^long i l] (when (m/zero? (m/long-mod i every-nth)) (label/name-text l))) levels))})
    (or (first (filter #(ends-ticked? % domain) (map #(axis-ticks k domain %) (range (long n) 1 -1))))
        (axis-ticks k domain n))))

(defn- tick-label-sizes
  "Gives the sizes of every tick label the axes of a frame may draw: of the most ticks an edge can ask for (see [[tick-count]], [[edge-ticks]]), of every level of a categorical axis (an edge thins its own, any of them may be drawn).

  Parameters:

  - `frame` (map): `:domains`, `:levels`.
  - `[w h]`: the panel size in pixels.
  - `env` (map): `:canvas`, `:theme`.

  Returns a sequence of `Vec2` sizes."
  [{:keys [domains levels]} [w h] env]
  (let [longest (m/min (double w) (double h))]
    (map #(text-size :tick-label % env)
         (mapcat #(if-let [ls (get levels %)]
                    (map label/name-text ls)
                    (:labels (edge-ticks % (domains %) nil (tick-count env % longest))))
                 space/axes))))

(def ^{:private true :const true :tag 'double} bare-frame-margin
  "Pixels kept around a box without axes."
  4.0)

(def ^{:private true :const true :tag 'double} frame-padding
  "Pixels added to a frame margin past its labels and title."
  2.0)

(defn frame-margin
  "Gives the pixels to keep around the box for the axes of a frame: tick length, label gaps, the largest tick label (see [[tick-label-sizes]]) and a title line.

  Parameters:

  - `frame` (map): `:axes?`, `:domains`, `:levels`, `:titles` (`{:x t ..}`, `nil` - none).
  - `[w h]`: the panel size in pixels.
  - `env` (map): `:canvas`, `:theme`.

  Returns the margin; [[bare-frame-margin]] without axes."
  [{:keys [titles axes?] :as frame} [w h] env]
  (if-not axes?
    bare-frame-margin
    (let [{:keys [tick-length label-gap]} (get-in env [:theme :axis :x])
          widest (double (reduce m/max 0.0 (map (fn [^Vec2 s] (m/max (.x s) (.y s))) (tick-label-sizes frame [w h] env))))
          title (double (reduce m/max 0.0 (keep (fn [k] (when-some [t (get titles k)]
                                                           (.y (text-size :axis-title (str t) env))))
                                                 space/axes)))]
      (m/+ (double tick-length) (m/* 2.0 (double label-gap)) widest title frame-padding))))

(defn frame-margins
  "Gives the pixels to keep on each side of the box for the axes of a frame: [[frame-margin]] left, right and below; above only half the tallest tick label and a label gap (the z axis's top tick label - x / y ticks and the titles go on edges low on screen or beside the box, see [[cljplot2.data.space3d/axis-edges]]).

  The same for every azimuth, so a turned box keeps its place (see [[cljplot2.data.space3d/projector]]).

  Parameters:

  - `frame` (map): see [[frame-margin]].
  - `[w h]`: the panel size in pixels.
  - `env` (map): `:canvas`, `:theme`.

  Returns `{:top :right :bottom :left}`; [[bare-frame-margin]] every side without axes."
  [{:keys [axes?] :as frame} [w h] env]
  (let [full (frame-margin frame [w h] env)]
    (if-not axes?
      {:top full :right full :bottom full :left full}
      (let [label-gap (double (get-in env [:theme :axis :x :label-gap]))
            tallest (.y (measure/max-size (tick-label-sizes frame [w h] env)))]
        {:top (m/+ (m/* 0.5 tallest) label-gap frame-padding) :right full :bottom full :left full}))))

(def ^{:private true :const true :tag 'double} min-outward-length
  "Length below which the outward direction is undefined (the edge passes through the box centre)."
  1.0e-6)

(defn- outward
  "Gives the unit pixel direction away from the box across an edge: from the box centre, without the component along the edge.

  Parameters:

  - `edge-mid` (`Vec2`): the edge's middle in pixels.
  - `along` (`Vec2`): the edge's direction.
  - `centre` (`Vec2`): the box centre in pixels.

  Returns a unit `Vec2`; screen down when undefined."
  ^Vec2 [^Vec2 edge-mid ^Vec2 along ^Vec2 centre]
  (let [d (v/sub edge-mid centre)
        a (if (m/pos? (v/mag along)) (v/normalize along) along)
        o (v/sub d (v/mult a (v/dot d a)))]
    (if (m/> (v/mag o) min-outward-length) (v/normalize o) (v/vec2 0.0 1.0))))

(defn- extent-along
  "Gives half the length of a box along a unit direction.

  Parameters:

  - `size` (`Vec2`): the box size.
  - `o` (`Vec2`): the unit direction.

  Returns the half length in pixels."
  ^double [^Vec2 size ^Vec2 o]
  (m/* 0.5 (m/+ (m/* (m/abs (.x o)) (.x size)) (m/* (m/abs (.y o)) (.y size)))))

(defn edge-angle
  "Gives the clockwise rotation of text running along a pixel direction (y down): readable left to right, a vertical edge bottom to top.

  Parameters:

  - `along` (`Vec2`): the direction.

  Returns degrees in `[0 90)` or `(270 360)`, -90 drawn as 270."
  ^double [^Vec2 along]
  (let [raw (m/degrees (m/atan2 (.y along) (.x along)))
        deg (double (cond (m/> raw 90.0) (m/- raw 180.0) (m/<= raw -90.0) (m/+ raw 180.0) :else raw))]
    (if (m/== deg 90.0) 270.0 (if (m/neg? deg) (m/+ deg 360.0) deg))))

(defn title-distance
  "Gives the pixels from an axis edge to the centre of its title: past the ticks, the tick labels with a gap on each side, and half the title's thickness - its unrotated text height, as the title runs along the edge whatever its length.

  Parameters:

  - `tick-length` (number): the tick length.
  - `gap` (number): the label gap.
  - `widest` (number): the tick labels' largest extent away from the edge.
  - `size` (`Vec2`): the title's natural size.

  Returns the distance in pixels."
  ^double [tick-length gap widest ^Vec2 size]
  (m/+ (double tick-length) (m/* 2.0 (double gap)) (double widest) (m/* 0.5 (.y size))))

(defn- label-box
  "Gives the pixel box of a label centred at a point.

  Parameters:

  - `c` (`Vec2`): the label's centre in pixels.
  - `size` (`Vec2`): its natural size.

  Returns `{:x :y :w :h}` (see [[cljplot2.graphics.measure/boxes-overlap?]])."
  [^Vec2 c ^Vec2 size]
  {:x (m/- (.x c) (m/* 0.5 (.x size))) :y (m/- (.y c) (m/* 0.5 (.y size))) :w (.x size) :h (.y size)})

(defn edge-point
  "Gives the pixel of the point at a fraction along a box edge - the projected surface point: under perspective equal steps along an edge shrink with depth, not equal on screen.

  Parameters:

  - `corner` (fn): the projector's corner function of fractional bits (see [[cljplot2.data.space3d/projector]]).
  - `edge` (map): `:axis`, `:from` - the edge.
  - `f` (double): the fraction along the edge.

  Returns a `Vec2`."
  ^Vec2 [corner {:keys [axis from]} ^double f]
  (corner (assoc from (space/axis-index axis) f)))

(defn- draw-axis!
  "Draws an axis on its edge: line, ticks, labels, title (see the namespace doc).

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `k` (keyword): the axis.
  - `edge` (map): `:from`, `:to` - the edge's corner bits.
  - `proj` (map): the projector's `:corner` and `:centre`.
  - `frame` (map): `:domains`, `:levels`, `:titles`.
  - `taken` (sequence of maps): label boxes `{:x :y :w :h}` drawn by the axes before (e.g. at a shared corner); a tick label overlapping one is left out.
  - `env` (map): `:canvas`, `:theme`.

  Returns the boxes of the labels this axis drew (a vector)."
  [canvas k {:keys [from to] :as edge} {:keys [corner centre]} {:keys [domains levels titles]} taken env]
  (let [{:keys [line-color line-width tick-color ^double tick-length tick-width ^double label-gap]} (get-in env [:theme :axis :x])
        ^Vec2 a (corner from) ^Vec2 b (corner to)
        along (v/sub b a)
        o (outward (v/mult (v/add a b) 0.5) along centre)
        domain (domains k)
        title (get titles k)
        {:keys [ticks labels]} (edge-ticks k domain (get levels k) (tick-count env k (v/mag along)))
        [^double lo ^double hi] domain
        labelled (atom [])
        at (fn [t] (edge-point corner edge (m// (m/- (double t) lo) (m/- hi lo))))
        widest (atom 0.0)]
    (when (and (not (theme/none? line-color)) (m/pos? (double line-width)))
      (c2d/set-color canvas line-color)
      (c2d/set-stroke-custom canvas {:size line-width})
      (c2d/line canvas a b))
    (doseq [[t text] (map vector ticks labels)
            :let [^Vec2 p (at t)
                  ^Vec2 q (v/add p (v/mult o tick-length))]]
      (when (and (not (theme/none? tick-color)) (m/pos? (double tick-width)))
        (c2d/set-color canvas tick-color)
        (c2d/set-stroke-custom canvas {:size tick-width})
        (c2d/line canvas p q))
      ;; a thinned categorical axis has ticks without labels
      (when (some? text)
        (let [^Vec2 size (text-size :tick-label text env)
              e (extent-along size o)
              ^Vec2 c (v/add q (v/mult o (m/+ label-gap e)))
              box (label-box c size)]
          (swap! widest m/max (m/* 2.0 e))
          ;; skipped when it collides with another axis's label or one drawn before it on this axis
          (when-not (or (some #(measure/boxes-overlap? box %) taken) (some #(measure/boxes-overlap? box %) @labelled))
            (swap! labelled conj box)
            (label/draw-at! canvas {:kind :label :role :tick-label :text text} [:center :middle] (.x c) (.y c) env)))))
    (when (some? title)
      ;; the title runs along its edge, turned to read left to right (or bottom to top)
      (let [content {:kind :label :role :axis-title :text (str title) :rotation (edge-angle along)}
            ^Vec2 c (v/add (v/mult (v/add a b) 0.5)
                           (v/mult o (title-distance tick-length label-gap @widest (text-size :axis-title (str title) env))))]
        (label/draw-at! canvas content [:center :middle] (.x c) (.y c) env)))
    @labelled))

(defn- box-edge-stroke!
  "Sets the canvas stroke and color of the box edges: theme `[:axis :x]` `:line-color` and `:line-width`, dashed by `dash` (pixels, `nil` - solid).

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `dash`: dash lengths or `nil`.
  - `env` (map): `:theme`.

  Returns truthy when the edges draw, `nil` for a `:none` colour or width 0."
  [canvas dash env]
  (let [{:keys [line-color line-width]} (get-in env [:theme :axis :x])]
    (when (and (not (theme/none? line-color)) (m/pos? (double line-width)))
      (c2d/set-color canvas line-color)
      (c2d/set-stroke-custom canvas (cond-> {:size line-width :cap :butt} dash (assoc :dash dash)))
      true)))

(defn- face-grid-lines
  "Gives the grid lines on a box face: for each other axis a line across the face at each tick of its domain.

  Parameters:

  - `proj` (map): the projector's `:corner`.
  - `axis` (keyword): the face's normal axis.
  - `side` (long): 0 - the low face, 1 - the high one.
  - `frame` (map): `:domains`, `:levels`.
  - `env` (map): `:theme`.

  Returns a sequence of `[k [from to]]` - the other axis and the line's pixel `Vec2` ends."
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
          :let [f (m// (m/- (double t) (double lo)) (m/- (double hi) (double lo)))
                at (fn [fb] (corner (-> [0 0 0] (assoc i side) (assoc a f) (assoc b fb))))]]
      [k [(at 0.0) (at 1.0)]])))

(defn- draw-box-grid!
  "Draws the grid lines behind the data at the ticks: on the floor (low z; x and y ticks) and on the two side walls turned away from the viewer (the x wall at y and z ticks, the y wall at x and z ticks).

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `proj` (map): the projector.
  - `frame` (map): `:view`, `:aspect`, `:domains`, `:levels`.
  - `env` (map): `:theme` - lines at x / y ticks use `[:panel :grid :x]`, lines at z ticks `[:panel :grid :y]` (`:none` - none).

  Returns `nil`."
  [canvas proj {:keys [view aspect] :as frame} env]
  (let [faces (cons [:z 0] (keep (fn [axis] (some->> (space/far-side view aspect axis) (vector axis))) [:x :y]))]
    (doseq [[axis side] faces
            [k [from to]] (face-grid-lines proj axis side frame env)
            :when (polyline/set-style! canvas (get-in env [:theme :panel :grid (if (= k :z) :y :x)]))]
      (c2d/line canvas from to))))

(defn draw-frame-back!
  "Draws the box frame behind the data: the grids of the floor and the back walls (see [[draw-box-grid!]]) and the hidden box edges (dashed by theme `[:frame-3d :hidden-edge-dash]`), so back and front read apart.

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `proj` (map): the projector (see [[cljplot2.data.space3d/projector]]).
  - `frame` (map): `:view`, `:aspect`, `:box?` (nothing without it), `:domains`, `:levels`.
  - `env` (map): `:theme`.

  Returns `nil`."
  [canvas proj {:keys [view aspect box?] :as frame} env]
  (when box?
    (let [canvas-env (assoc env :canvas canvas)
          corner (:corner proj)]
      (draw-box-grid! canvas proj frame canvas-env)
      (when (box-edge-stroke! canvas (polyline/dash-pattern (get-in env [:theme :frame-3d :hidden-edge-dash])) canvas-env)
        (doseq [{:keys [from to]} (space/hidden-edges view aspect)]
          (c2d/line canvas (corner from) (corner to)))))))

(def ^{:const true :tag 'double} min-axis-length
  "Pixels an axis edge needs to carry ticks; shorter (end-on) edges get no axis."
  4.0)

(declare draw-frame-front!)

(defn scene-frame
  "Gives the box of one panel's 3D charts.

  Parameters:

  - `computeds` (sequence of maps): the computed data of each 3D chart, in chart order - `:frame`, `:domains`, `:frame-overrides`.

  Returns the first's `:frame` with its `:domains` (several 3D charts share their explicit domains), the later charts' `:frame-overrides` (their explicitly set frame options, see [[cljplot2.data.space3d/frame-overrides]]) merged over it in order - `:view` and `:titles` key by key, `:titles nil` clearing them."
  [computeds]
  (let [{:keys [frame domains]} (first computeds)]
    (reduce (fn [f {o :frame-overrides}]
              (cond-> (merge f (dissoc o :view :titles))
                (:view o) (update :view merge (:view o))
                (contains? o :titles) (assoc :titles (when-let [t (:titles o)] (merge (:titles f) t)))))
            (assoc frame :domains domains)
            (rest computeds))))

(defn draw-scene!
  "Draws one panel's 3D charts in one box (see [[scene-frame]]): the back of the frame, the primitives of every chart sorted far to near together, then the front of the frame.

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `layers` (sequence): `[[chart computed] ..]` - one panel's computed data of each 3D chart (`:domains`, `:frame`, `:frame-overrides`); primitives come from `((:primitives chart) chart computed point scales env)`, `point` - data -> `[pixel depth]`, the computed `:frame` replaced by the scene's, so faces and walls turn by the box's view.
  - `[w h]`: the panel size in pixels.
  - `scales` (map): the panel's scales.
  - `env` (map): the drawing environment.

  Returns `nil`; nothing for a panel too small for the box."
  [canvas layers [w h] scales env]
  (let [canvas-env (assoc env :canvas canvas)
        frame (scene-frame (map second layers))
        proj (space/projector frame {:w w :h h :margin (frame-margins frame [w h] canvas-env)})]
    (when (m/pos? (double (:scale proj)))
      (draw-frame-back! canvas proj frame canvas-env)
      (draw-primitives! canvas (mapcat (fn [[chart computed]]
                                         ((:primitives chart) chart (assoc computed :frame frame) (:point proj) scales canvas-env))
                                       layers))
      (draw-frame-front! canvas proj frame canvas-env))))

(defn draw-chart
  "Draws a 3D chart alone in one panel's box - the draw function of the 3D charts' contract (see [[cljplot2.chart]]); a plot's panel draws all its 3D charts in one scene instead (see [[draw-scene!]]).

  Parameters:

  - `chart` (map): the chart value.
  - `computed` (map): the panel's computed data.
  - `canvas`: the canvas in graphics context.
  - `box` (map): `:w`, `:h` - the panel size.
  - `scales` (map): the panel's scales.
  - `env` (map): the drawing environment.

  Returns `nil`."
  [chart computed canvas {:keys [w h]} scales env]
  (draw-scene! canvas [[chart computed]] [w h] scales env))

(defn draw-frame-front!
  "Draws the box frame in front of the data: the visible box edges, then the axes x, y, z in turn - a tick label overlapping an earlier axis's label left out.

  Parameters:

  - `canvas`: the canvas in graphics context.
  - `proj` (map): the projector.
  - `frame` (map): `:view`, `:aspect`, `:box?` (the edges), `:axes?` (the axes), `:domains`, `:levels`, `:titles`.
  - `env` (map): `:theme`.

  Returns the label boxes drawn (a vector) with axes, else `nil`."
  [canvas proj {:keys [view aspect box? axes?] :as frame} env]
  (let [canvas-env (assoc env :canvas canvas)
        corner (:corner proj)]
    (when (and box? (box-edge-stroke! canvas nil canvas-env))
      (doseq [{:keys [from to]} (space/visible-edges view aspect)]
        (c2d/line canvas (corner from) (corner to))))
    (when axes?
      (reduce (fn [taken [k {:keys [from to] :as edge}]]
                ;; an edge seen end-on (z from straight above) has no room for an axis
                (if (m/>= (v/mag (v/sub (corner to) (corner from))) min-axis-length)
                  (into taken (draw-axis! canvas k edge proj frame taken canvas-env))
                  taken))
              [] (space/axis-edges view aspect corner)))))
