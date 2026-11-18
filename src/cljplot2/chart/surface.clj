(ns cljplot2.chart.surface
  "Surface: a mesh of quads in a 3D box (see [[cljplot2.data.space3d]]; a chart laying out its own panel, as
  [[cljplot2.chart.cloud]], whose frame options it shares: `:view`, `:aspect`, `:box`, `:axes`, `:axis-titles`,
  `:domain`).

  Sources (one per chart):

  * gridded (default) - mapped `:x`, `:y`, `:z` on a full lattice (every distinct x with every distinct y, once; see
    [[cljplot2.data.field/lattice-values]], errors keyed `:z`): a node per row
  * `:f` - a function of `x`, `y` returning `z`, or `[z c]` (`c` - the node's color value), sampled on a regular grid
    of `:n` (default 30, or `[nx ny]`) points per axis over `:domain` `:x` / `:y` (required)
  * `:density true` - the 2D kernel density of mapped `:x`, `:y` (see [[cljplot2.data.field/density]], default
    bandwidths), sampled as `:f` over the panels' density domains (or the explicit x / y domain)
  * `:fu` - a parametric surface: a function of `u`, `v` returning `[x y z]`, sampled on `:n` points over the ranges
    `:u [lo hi]`, `:v [lo hi]` (both required with `:fu`, refused without it)

  Nodes with a non-finite coordinate, or outside an explicit `:domain`, are left out with every quad they touch.
  Domains: explicit or the nodes' over all panels. Facets: a mesh per panel (gridded / density - the panel's rows; a
  function - the same in every panel), domains shared.

  Colors: by default each quad's mean height (or `c` of `[z c]`) through the plot's color scale - a computed color
  with a color bar titled by the `:z` accessor's name (\"Density\" for a density, none for a function); option
  `:color` - one color for all quads (no color scale). A quad whose value is outside an explicit color domain isn't
  drawn.

  Options (style defaults from theme `[:chart :surface]`: `:stroke-color`, `:stroke-width`):

  * `:style` - `:surface` (default; filled quads), `:wireframe` (quad outlines in their color, `:stroke-width` wide,
    over the panel's background - hidden lines covered) or `:both` (filled quads outlined by `:stroke-color`)
  * `:shade` - Lambert shading of filled quads (default true; see [[cljplot2.data.space3d/shade]])

  Quads are drawn far to near by their centroid depth, with the other 3D charts of the panel (a cloud over a surface:
  both with every `:domain` explicit)."
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.chart.cloud :as cloud]
            [cljplot2.data.field :as field]
            [cljplot2.data.rows :as data]
            [cljplot2.data.space3d :as space]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries
  [(schema/optional :stroke-color schema/color)
   (schema/optional :stroke-width schema/non-neg)])

(def ^:private range-schema [:tuple schema/finite schema/finite])

(def schemas
  "Schemas of the surface chart: `:style` (theme `[:chart :surface]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (schema/with-rules
             (-> [:map {:closed true}]
                 (into style-entries)
                 (into data/common-options)
                 (into cloud/frame-options)
                 (conj (schema/optional :style [:enum :surface :wireframe :both])
                       (schema/optional :color schema/color)
                       (schema/nilable :shade schema/bool)
                       (schema/optional :f schema/function)
                       (schema/nilable :density schema/bool)
                       (schema/optional :fu schema/function)
                       (schema/optional :u range-schema)
                       (schema/optional :v range-schema)
                       (schema/optional :n field/n-schema)))
             [#(when (> (count (filter % [:f :fu :density])) 1) "a surface takes one source: :f, :fu or :density true") nil]
             [#(when (and (:fu %) (not (and (:u %) (:v %)))) "a parametric surface needs :u and :v ranges") [:fu]]
             [#(when (and (or (:u %) (:v %)) (not (:fu %))) ":u and :v are the ranges of a parametric :fu") [:u]])
   :mapping [:maybe [:map]]})

(defn source-kind
  "The source of a surface chart's `options`: `:function` (`:f`), `:parametric` (`:fu`), `:density`, else `:gridded`."
  [options]
  (cond (:f options) :function (:fu options) :parametric (:density options) :density :else :gridded))

(defn- value-title
  [options mapping]
  (case (source-kind options)
    :gridded (let [a (:z mapping)] (when (keyword? a) (name a)))
    :density "Density"
    nil))

(defn computed-aesthetics
  "The surface chart's computed color (titled by the `:z` accessor's name, \"Density\", or none) unless a fixed
  `:color` option colors it."
  [options mapping]
  (if (:color options) {} {:color (value-title options mapping)}))

;; meshes

(defn- finite-number? [v] (and (number? v) (m/valid-double? (double v))))

(defn mesh
  "`{:nu :nv :nodes [..] :tones [..]}` of `nu` x `nv` nodes, row by row (`i + j nu`), `node-at` - a function of `i`,
  `j` giving `[x y z]` or `[x y z tone]`: `:nodes` - `[x y z]` (doubles) or `nil` for a non-finite coordinate,
  `:tones` - each node's color value (`tone`, else `z`; `##NaN` when missing or not finite)."
  [nu nv node-at]
  (let [raw (vec (for [j (range nv) i (range nu)] (node-at i j)))
        ok? (fn [n] (and (sequential? n) (every? finite-number? (take 3 n))))]
    {:nu (long nu) :nv (long nv)
     :nodes (mapv (fn [n] (when (ok? n) (mapv double (take 3 n)))) raw)
     :tones (mapv (fn [n] (let [t (if (> (count n) 3) (nth n 3) (nth n 2 nil))]
                            (if (and (ok? n) (finite-number? t)) (double t) ##NaN)))
                  raw)}))

(defn- axis-values
  [[lo hi] n]
  (vec (m/slice-range (double lo) (double hi) (long n))))

(defn- height-node
  "Node of a height function's result `r` at `x`, `y`: `z` or `[z c]`."
  [x y r]
  (if (sequential? r) (let [[z c] r] [x y z c]) [x y r]))

(defn function-mesh
  "Mesh (see [[mesh]]) of the height function `f` (`z` or `[z c]`) over `[[x0 x1] [y0 y1]]` at `[nx ny]` points."
  [f [xd yd] [nx ny]]
  (let [xs (axis-values xd nx) ys (axis-values yd ny)]
    (mesh nx ny (fn [i j] (let [x (xs i) y (ys j)] (height-node x y (f x y)))))))

(defn parametric-mesh
  "Mesh of the parametric function `fu` (`[x y z]` of `u`, `v`) over the ranges `u`, `v` at `[nu nv]` points."
  [fu u v [nu nv]]
  (let [us (axis-values u nu) vs (axis-values v nv)]
    (mesh nu nv (fn [i j] (let [r (fu (us i) (vs j))] (when (sequential? r) r))))))

(defn gridded-mesh
  "Mesh of the gridded `rows` (accessors of `mapping` `:x`, `:y`, `:z`): a node per lattice point."
  [rows mapping]
  (let [{:keys [xs ys vss]} (field/lattice-values rows (data/row-accessor :x (:x mapping)) (data/row-accessor :y (:y mapping))
                                                  (data/row-accessor :z (:z mapping)) :z)]
    (mesh (count xs) (count ys) (fn [i j] [(xs i) (ys j) (get-in vss [i j])]))))

(defn- points-density
  "`{:f :domain}` - the 2D KDE of the finite `:x`, `:y` of `rows` (see [[cljplot2.data.field/density]]); `nil` without
  points."
  [rows mapping]
  (let [x-of (data/row-accessor :x (:x mapping)) y-of (data/row-accessor :y (:y mapping))
        pts (filterv (fn [[x y]] (and (finite-number? x) (finite-number? y))) (map (juxt x-of y-of) rows))]
    (when (seq pts)
      (field/density (map first pts) (map second pts) {} [(scale/stat-transform nil) (scale/stat-transform nil)]))))

(defn- grid-size [n] (field/grid-size (or n 30)))

(defn- explicit-xy
  "The sampling domain `[[x0 x1] [y0 y1]]`: the explicit `:x` / `:y` of `domain`, else `fallback`'s."
  [domain [fx fy]]
  [(or (:x domain) fx) (or (:y domain) fy)])

(defn panel-meshes
  "`{panel mesh}` of the split `groups` (`{panel rows}`) by the source `kind` (see the namespace doc); panels without
  rows (gridded, density) left out."
  [kind options mapping groups]
  (let [size (grid-size (:n options))
        domain (:domain options)]
    (case kind
      :function (let [{:keys [x y]} domain]
                  (when-not (and x y)
                    (schema/fail! "A surface of :f needs a :domain with :x and :y." {:key :domain}))
                  (let [m (function-mesh (:f options) [x y] size)] (update-vals groups (constantly m))))
      :parametric (let [m (parametric-mesh (:fu options) (:u options) (:v options) size)] (update-vals groups (constantly m)))
      :gridded (into {} (keep (fn [[panel rows]] (when (seq rows) [panel (gridded-mesh rows mapping)]))) groups)
      :density (let [fields (into {} (keep (fn [[panel rows]] (some->> (points-density rows mapping) (vector panel)))) groups)
                     xy (explicit-xy domain (field/union-domain (map :domain (vals fields))))]
                 (update-vals fields (fn [{:keys [f]}] (function-mesh f xy size)))))))

(defn clip-mesh
  "`mesh` without the nodes outside the explicit `domains` (`{:x [a b] ..}`, each optional): those nodes `nil`, their
  tones `##NaN`."
  [mesh domains]
  (let [inside? (fn [n] (and n (every? true? (map-indexed (fn [i k] (space/in-domain? (get domains k) (n i))) space/axes))))
        kept (mapv inside? (:nodes mesh))]
    (-> mesh
        (update :nodes (fn [ns] (mapv (fn [n k] (when k n)) ns kept)))
        (update :tones (fn [ts] (mapv (fn [t k] (if k t ##NaN)) ts kept))))))

(defn- check-mapping!
  [kind mapping]
  (doseq [k (case kind :gridded [:x :y :z] :density [:x :y] [])
          :when (nil? (get mapping k))]
    (schema/fail! (str "A " (name kind) " surface needs a mapped " k ".") {:key :mapping :in [:mapping k]})))

(defn- style
  [options theme]
  (merge (get-in theme [:chart :surface])
         (select-keys options [:stroke-color :stroke-width :color])
         {:surface-style (get options :style :surface)
          :shade? (not (false? (:shade options)))}))

(defn compute
  "Per panel: `{:mesh m :domains {:x :y :z} :frame f :style st}` (see [[mesh]], [[cljplot2.chart.cloud/frame-settings]]);
  no position extents (the chart lays out its panel), color extents - the finite tones (computed color only),
  `:dropped` - gridded / density rows without a finite x / y."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme]}]
  (let [kind (source-kind options)
        _ (check-mapping! kind mapping)
        explicit (:domain options)
        meshes (update-vals (panel-meshes kind options mapping groups) #(clip-mesh % explicit))
        nodes (mapcat #(remove nil? (:nodes %)) (vals meshes))
        domains (into {} (map-indexed (fn [i k] [k (or (get explicit k) (space/data-domain (map #(% i) nodes) 0.0))]) space/axes))
        frame (cond-> (cloud/frame-settings options mapping)
                (and (= kind :density) (nil? (:axis-titles options))) (assoc-in [:titles :z] "Density"))
        st (style options theme)
        colored? (nil? (:color options))]
    {:data (update-vals meshes (fn [mesh] {:mesh mesh :domains domains :frame frame :style st}))
     :extents (update-vals meshes (fn [{:keys [tones]}]
                                    (let [ts (remove m/nan? tones)]
                                      (if (and colored? (seq ts))
                                        {:color {:kind :numeric :min (reduce min ts) :max (reduce max ts)}}
                                        {}))))
     :dropped (if (#{:gridded :density} kind) (field/dropped-positions :gridded mapping groups) {})}))

;; drawing

(defn- quad-normal
  "View-space unit normal of the quad of normalized corners `[a b c d]` (in order around it): the cross product of its
  diagonals rotated by `rotate` (a linear map, see [[cljplot2.data.space3d/rotation]]); zero for a degenerate quad."
  ^Vec3 [rotate [a b c d]]
  (let [^Vec3 n (rotate (v/cross (v/sub c a) (v/sub d b)))
        len (v/mag n)]
    (if (pos? len) (v/div n len) n)))

(defn- tone-color
  "Function of a tone giving a quad's color: the fixed `color`, else the color `scale`'s (`na` for `##NaN` or no
  scale; `nil` outside the scale's domain)."
  [color scale na]
  (cond color (constantly color)
        scale (let [f (aesthetic/attribute-fn :color scale na)] (fn [^double t] (if (m/nan? t) na (f t))))
        :else (constantly na)))

(defn- background
  "The panel's fill, else the plot's background: a wireframe's quads cover the lines behind them with it."
  [env]
  (let [fill (get-in env [:theme :panel :fill])]
    (if (theme/none? fill) (get-in env [:theme :plot :background]) fill)))

(def ^:const seam-width
  "Pixels of the outline of a `:surface` quad in its own fill, covering the anti-aliased seams between quads."
  1.0)

(defn quad-corners
  "Node indices of the quad at `i`, `j` of a mesh `nu` nodes wide, in order around it."
  [^long nu ^long i ^long j]
  (let [a (+ i (* j nu)) b (+ a nu)]
    [a (inc a) (inc b) b]))

(defn chart-primitives
  "The surface's quads for one panel's `computed` data and the projection `point` (data -> `[pixel depth]`; the 3D
  chart contract, see [[cljplot2.graphics.space3d/draw-scene!]]): a `:polygon` per quad with four nodes, at its corners'
  mean depth, colored by its corners' mean tone, styled by `:style` (see the namespace doc)."
  [_chart {:keys [mesh domains frame style]} point scales env]
  (let [{:keys [nu nv nodes tones]} mesh
        {:keys [view aspect]} frame
        normalize (space/normalizer domains aspect)
        rotate (space/rotation view)
        color-of (tone-color (:color style) (:color scales) (get-in env [:theme :palette :na]))
        look (:surface-style style)
        width (:stroke-width style)
        back (background env)]
    (for [j (range (dec (long nv))) i (range (dec (long nu)))
          :let [ids (quad-corners nu i j)
                ps (mapv nodes ids)]
          :when (every? some? ps)
          :let [col (color-of (/ (double (reduce + (map tones ids))) 4.0))]
          :when (some? col)
          :let [projected (mapv point ps)
                depth (/ (double (reduce + (map second projected))) 4.0)
                pixels (mapv first projected)]]
      (if (= look :wireframe)
        {:kind :polygon :points pixels :depth depth :fill back :stroke col :stroke-width width}
        (let [fill (if (:shade? style) (space/shade col (quad-normal rotate (mapv normalize ps))) col)]
          (if (= look :both)
            {:kind :polygon :points pixels :depth depth :fill fill :stroke (:stroke-color style) :stroke-width width}
            ;; outlined in its own fill: no anti-aliased seams between neighbours
            {:kind :polygon :points pixels :depth depth :fill fill :stroke fill :stroke-width seam-width}))))))
