(ns cljplot2.chart.cloud
  "Cloud: a 3D scatter of rows at their mapped (x, y, z) in a box projected onto the panel (see
  [[cljplot2.data.space3d]]; a chart laying out its own panel: no x / y axes - the box carries three axes).

  Mapping: `:x`, `:y`, `:z` (numbers); aesthetics `:color`, `:size`, `:shape` as in [[cljplot2.chart.scatter]]
  (scales, legends, NA). Rows with a non-finite coordinate, or a coordinate outside an explicit `:domain`, are
  dropped and counted (`:dropped {:x n :y n :z n ..}`).

  Options (point style defaults from theme `[:chart :cloud]`: `:size`, `:color`, `:stroke-color`, `:stroke-width`,
  `:shape`):

  * `:type` - `:points` (default), `:spikes` (a line from z = `:base` - default the low z limit - to each point, width
    `:stroke-width`, lattice `type = \"h\"`) or `:bars` (a box per row from `:base` to z, `:bar-size [fx fy]` (default
    `[0.8 0.8]`) of the x / y resolution - the smallest gap between distinct values; faces shaded, outlined by
    `:stroke-color`)
  * `:view {:azimuth deg :elevation deg}` (default 40 / 30), `:aspect [ax ay az]` (box proportions, default `[1 1 1]`)
  * `:box` (default true - the visible box edges), `:axes` (default true - ticks, labels and titles), `:axis-titles
    {:x t :y t :z t}` (default the mapped column names; `nil` - none)
  * `:domain {:x [lo hi] :y .. :z ..}` - explicit data domains (each optional); otherwise the data's over all panels
    (bars widen x / y by half a bar). Several 3D charts in one plot need every domain explicit, so their boxes match.

  Facets: a box per panel, domains shared by all panels. Points, spikes and bar faces are drawn far to near."
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.chart.scatter :as scatter]
            [cljplot2.chart.tile :as tile]
            [cljplot2.data.rows :as data]
            [cljplot2.data.space3d :as space]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.shape :as shape]
            [cljplot2.graphics.space3d :as space-draw]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def frame-options
  "Option entries shared by the 3D charts: view, box proportions, frame, axis titles, explicit domains."
  [(schema/optional :view space/view-schema)
   (schema/optional :aspect space/aspect-schema)
   (schema/nilable :box schema/bool)
   (schema/nilable :axes schema/bool)
   (schema/nilable :axis-titles [:map {:closed true} (schema/optional :x any?) (schema/optional :y any?) (schema/optional :z any?)])
   (schema/optional :domain space/domains-schema)])

(def schemas
  "Schemas of the cloud chart: `:style` (theme `[:chart :cloud]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] scatter/style-entries)
   :options (-> [:map {:closed true}]
                (into scatter/style-entries)
                (into data/common-options)
                (into frame-options)
                (conj (schema/optional :type [:enum :points :spikes :bars])
                      (schema/optional :base schema/finite)
                      (schema/optional :bar-size [:tuple schema/fraction schema/fraction])))
   :mapping [:map [:x schema/accessor] [:y schema/accessor] [:z schema/accessor]]})

(def ^:private position-dims [:x :y :z])

(defn- style
  [options theme]
  (merge (get-in theme [:chart :cloud]) (select-keys options [:size :color :stroke-color :stroke-width :shape])))

(defn frame-settings
  "Frame of a 3D chart from its `options` and merged `mapping`: `{:view :aspect :box? :axes? :titles}` - titles from
  `:axis-titles`, else the names of keyword accessors."
  [options mapping]
  {:view (merge space/default-view (:view options))
   :aspect (or (:aspect options) [1.0 1.0 1.0])
   :box? (not (false? (:box options)))
   :axes? (not (false? (:axes options)))
   :titles (or (:axis-titles options)
               (into {} (keep (fn [k] (let [a (mapping k)] (when (keyword? a) [k (name a)])))) position-dims))})

(defn- in-domain?
  "Is `v` within the explicit `domain` (`nil` - none; either direction)?"
  [domain v]
  (or (nil? domain) (let [[a b] domain] (<= (m/min (double a) (double b)) (double v) (m/max (double a) (double b))))))

(defn- data-domain
  "Domain of the numbers `vs` widened by `reach` on both sides (a single value widened, see
  [[cljplot2.data.space3d/widen]]); `[0 1]` without values."
  [vs ^double reach]
  (if (seq vs)
    (let [[lo hi] (space/widen [(reduce min vs) (reduce max vs)])]
      [(- (double lo) reach) (+ (double hi) reach)])
    [0.0 1.0]))

(defn compute
  "Per panel: `{:points {:x [..] :y [..] :z [..] aesthetic [..]} :domains {:x :y :z} :frame f :style st :type t :base b
  :half [hx hy]}` (`f` - see [[frame-settings]], `hx` / `hy` - half a bar in data units); aesthetic extents only (no
  position extents: the chart lays out its panel), `:na`, `:dropped` per dimension."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme na-level? plot-scales] :or {na-level? true}}]
  (let [st (style options theme)
        explicit (:domain options)
        dims (into position-dims (aesthetic/mapped-dims mapping))
        accessors (mapv #(data/row-accessor % (mapping %)) dims)
        values (update-vals groups (fn [rows] (mapv (fn [row] (mapv #(% row) accessors)) rows)))
        all-values (apply concat (vals values))
        kept-at? (fn [i k v] (and (scatter/kept? k na-level? plot-scales v)
                                  (or (>= (long i) 3) (in-domain? (get explicit k) v))))
        keep-row? (fn [vs] (every? true? (map-indexed (fn [i k] (kept-at? i k (vs i))) dims)))
        dropped (into {} (map-indexed (fn [i k] [k (count (remove #(kept-at? i k (% i)) all-values))]) dims))
        points (update-vals values (fn [vs] (let [kept (filterv keep-row? vs)]
                                              (into {} (map-indexed (fn [i k] [k (mapv #(% i) kept)])) dims))))
        type (get options :type :points)
        bars? (= type :bars)
        [fx fy] (get options :bar-size [0.8 0.8])
        coords (fn [k] (mapcat k (vals points)))
        half {:x (if bars? (* 0.5 (double fx) (tile/resolution (coords :x))) 0.0)
              :y (if bars? (* 0.5 (double fy) (tile/resolution (coords :y))) 0.0)}
        z-values (coords :z)
        base (double (or (:base options) (if-let [[a b] (:z explicit)] (m/min (double a) (double b))
                                                 (if (seq z-values) (reduce min z-values) 0.0))))
        domains {:x (or (:x explicit) (data-domain (coords :x) (double (half :x))))
                 :y (or (:y explicit) (data-domain (coords :y) (double (half :y))))
                 :z (or (:z explicit) (data-domain (cond-> (vec z-values) (and (seq z-values) (not= type :points)) (conj base)) 0.0))}
        frame (frame-settings options mapping)]
    {:data (update-vals points (fn [p] {:points p :domains domains :frame frame :style st :type type :base base
                                        :half [(half :x) (half :y)]}))
     :extents (update-vals points (fn [p] (into {} (map (fn [k] [k (aesthetic/value-extent k (get plot-scales k) (remove nil? (p k)))]))
                                                (drop 3 dims))))
     :na (set (filter (fn [k] (some #(some nil? (% k)) (vals points))) (drop 3 dims)))
     :dropped dropped}))

;; drawing

(defn- point-styles
  "Per point index its style (`nil` - an aesthetic value outside its scale: skipped) from the panel's `points`."
  [points style scales env]
  (let [na {:color (get-in env [:theme :palette :na]) :shape (:shape style) :size (:size style)}
        mapped (vec (for [k aesthetic/dims :when (contains? points k)]
                      [k (aesthetic/attribute-fn k (get scales k) (na k)) (points k)]))]
    (if (seq mapped)
      (fn [^long i] (reduce (fn [st [k f vs]] (if-some [a (f (nth vs i))] (assoc st k a) (reduced nil))) style mapped))
      (constantly style))))

(defn- bar-faces
  "Visible faces of the bar at `x`, `y` from `base` to `z` (half sizes `hx`, `hy`) as polygon primitives: the box's
  front-facing faces, each shaded by its normal."
  [point view [x y z] base [hx hy] st]
  (let [x (double x) y (double y) hx (double hx) hy (double hy)
        lo (m/min (double base) (double z)) hi (m/max (double base) (double z))
        at (fn [[ix iy iz]] [(if (zero? (long ix)) (- x hx) (+ x hx)) (if (zero? (long iy)) (- y hy) (+ y hy)) (if (zero? (long iz)) lo hi)])
        stroke (draw/outline-color (:stroke-color st) (:color st))]
    (keep (fn [{:keys [axis side corners]}]
            (let [[normal front?] (space/face-normal view axis side)]
              (when front?
                (let [projected (map #(point (at %)) corners)]
                  {:kind :polygon :points (mapv first projected) :depth (/ (double (reduce + (map second projected))) 4.0)
                   :fill (space/shade (:color st) normal) :stroke stroke :stroke-width (:stroke-width st)}))))
          space/faces)))

(defn primitives
  "Primitives of one panel's points (see [[cljplot2.graphics.space3d]]) for the projection `point` (data -> `[pixel
  depth]`), by the chart `type`."
  [{:keys [points type base half frame]} point styles]
  (let [{:keys [x y z]} points]
    (mapcat (fn [i]
              (when-let [st (styles i)]
                (let [p [(x i) (y i) (z i)]
                      [px depth] (point p)]
                  (case type
                    :points [{:kind :point :at px :depth depth :style st}]
                    :spikes (let [[b bd] (point [(x i) (y i) base])]
                              [{:kind :line :from b :to px :depth (* 0.5 (+ (double depth) (double bd)))
                                :color (:color st) :width (:stroke-width st)}])
                    :bars (bar-faces point (:view frame) p base half st)))))
            (range (count x)))))

(defn draw
  "Draw one panel's box: frame (visible edges, axes), then the points / spikes / bars far to near."
  [_chart {:keys [domains frame style] :as computed} canvas {:keys [w h]} scales env]
  (let [env (assoc env :canvas canvas)
        margin (space-draw/frame-margin domains (:titles frame) (:axes? frame) env)
        proj (space/projector (assoc frame :domains domains) {:w w :h h :margin margin})]
    (when (pos? (double (:scale proj)))
      (space-draw/draw-frame! canvas proj (assoc frame :domains domains) env)
      (space-draw/draw-primitives! canvas (primitives computed (:point proj)
                                                      (point-styles (:points computed) style scales env))))))

(defn legend
  "Key glyph: the point (points, spikes) or a filled square (bars), styled by the key's attributes."
  [{:keys [options]} attributes canvas {:keys [x y w h] :as box} env]
  (let [st (merge (style options (:theme env)) attributes)]
    (if (= :bars (:type options))
      (draw/bar-key! canvas box {:color (:color st) :stroke-color (:stroke-color st) :stroke-width (:stroke-width st)} false)
      (shape/draw! canvas (:shape st) (v/vec2 (+ (double x) (* 0.5 (double w))) (+ (double y) (* 0.5 (double h)))) st))))
