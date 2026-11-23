(ns cljplot2.chart.surface
  "Surface: a mesh of quads in a 3D box (see [[cljplot2.data.space3d]]; a chart laying out its own panel, as
  [[cljplot2.chart.cloud]], whose frame options it shares: `:view` (with `:distance` - perspective), `:aspect`,
  `:box`, `:axes`, `:axis-titles`, `:domain`, `:reverse`).

  Sources (one per chart):

  * gridded (default) - mapped `:x`, `:y`, `:z` on a full lattice (every distinct x with every distinct y, once; see
    [[cljplot2.data.field/lattice-values]], errors keyed `:z`): a node per row
  * `:f` - a function of a point `[x y]` (a `fastmath.vector.Vec2`, see Points in [[cljplot2.data.field]]) returning
    its height `z`, or `[z c]` (any sequential value or array) (height and color value `c`), sampled on a
    regular grid of `:n` (default 30, or `[nx ny]`) points per axis over `:domain` `:x` / `:y` (required)
  * `:density true` - the 2D kernel density of mapped `:x`, `:y` (see [[cljplot2.data.field/density]], default
    bandwidths) over the points' range widened by `:cut` bandwidths (default 3; 0 - the range), sampled as `:f` over
    the panels' density domains (or the explicit x / y domain)
  * `:fu` - a parametric surface: a function of `[u v]` (a `Vec2`) returning `[x y z]` (any sequential value or array), sampled on `:n` points over the ranges
    `:u [lo hi]`, `:v [lo hi]` (both required with `:fu`, refused without it)

  Nodes with a non-finite coordinate, or outside an explicit `:domain`, are left out with every quad they touch.
  Domains: explicit or the nodes' over all panels, made nice ([[cljplot2.data.space3d/nice-domain]]). Facets: a mesh per panel (gridded / density - the panel's rows; a
  function - the same in every panel), domains shared.

  Colors: by default each quad's mean height (or `c` of `[z c]`) through the plot's color scale - a computed color
  with a color bar titled by the `:z` accessor's name (\"Density\" for a density, none for a function); option
  `:color` - one color for all quads (no color scale). A quad whose value is outside an explicit color domain isn't
  drawn. The computed color is the plot's color scale: another chart of the plot (e.g. a cloud over the surface)
  can't map `:color` then - give the surface a fixed `:color`.

  Options (style defaults from theme `[:chart :surface]`: `:stroke-color`, `:stroke-width`):

  * `:style` - `:surface` (default; filled quads), `:wireframe` (quad outlines in their color, `:stroke-width` wide,
    over the panel's background - hidden lines covered) or `:both` (filled quads outlined by `:stroke-color`)
  * `:shade` - Lambert shading of filled quads (default true); `:ambient` (default 0.4) and `:light` (default `[-0.5 0.6
    -1.0]`) - the shading's ambient share and light direction (see [[cljplot2.data.space3d/shader]])

  Quads are drawn far to near by their centroid depth, with the other 3D charts of the panel (a cloud over a surface:
  both with every `:domain` explicit)."
  (:require [clojure2d.color :as c]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.chart.cloud :as cloud]
            [cljplot2.data.field :as field]
            [cljplot2.data.rows :as data]
            [cljplot2.data.space3d :as space]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2 Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries
  [(schema/optional :stroke-color {:description "quad outline colour of :style :both"} schema/color)
   (schema/optional :stroke-width {:description "quad outline width in pixels (:style :both and :wireframe)"} schema/non-neg)])

(def ^:private range-schema [:tuple schema/finite schema/finite])

(def schemas
  "Schemas of the surface chart: `:style` (theme `[:chart :surface]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (schema/with-rules
             (-> [:map {:closed true}]
                 (into style-entries)
                 (into data/common-options)
                 (into theme/theme-style-entries)
                 (into space/frame-options)
                 (into space/shading-options)
                 (conj (schema/optional :style {:description "mesh drawing: :surface (filled quads), :wireframe (quad outlines) or :both"
                                                :default :surface}
                                        [:enum :surface :wireframe :both])
                       (schema/optional :color {:description "one colour for all quads (no color scale)"
                                                :cljplot2/default-doc "each quad's mean height (or c of [z c]) through the plot's color scale"}
                                        schema/color)
                       (schema/nilable :shade {:description "shading of filled quads by their angle to the light (Lambert)" :default true} schema/bool)
                       (schema/optional :f {:description "source: a function of [x y] returning z or [z c], sampled over :domain :x / :y"}
                                        schema/function)
                       (schema/nilable :density {:description "source: the 2D kernel density of the mapped :x, :y" :default false} schema/bool)
                       (schema/optional :fu {:description "source: a parametric surface, a function of [u v] returning [x y z]"} schema/function)
                       (schema/optional :u {:description "the u range [lo hi] of :fu"} range-schema)
                       (schema/optional :v {:description "the v range [lo hi] of :fu"} range-schema)
                       (schema/optional :n {:description "grid points per axis of :f, :fu, :density: n or [nx ny]" :default 30}
                                        field/n-schema)
                       (schema/optional :cut {:description "how many bandwidths the density domain extends past the points on each side"
                                              :cljplot2/default-doc "3 (only with :density)"}
                                        schema/finite-non-neg)))
             [#(when (> (count (filter % [:f :fu :density])) 1) "a surface takes one source: :f, :fu or :density true") nil]
             [#(when (and (:fu %) (not (and (:u %) (:v %)))) "a parametric surface needs :u and :v ranges") [:fu]]
             [#(when (and (or (:u %) (:v %)) (not (:fu %))) ":u and :v are the ranges of a parametric :fu") [:u]]
             [#(when (and (some? (:cut %)) (not (:density %))) ":cut widens a :density surface") [:cut]]
             cloud/distance-rule)
   :mapping [:maybe [:map
                     (schema/nilable :x {:description "x of a lattice node (gridded) or of a point (:density)"} schema/accessor)
                     (schema/nilable :y {:description "y of a lattice node (gridded) or of a point (:density)"} schema/accessor)
                     (schema/nilable :z {:description "height of a lattice node (gridded source)"} schema/accessor)]]})

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
  "Node of a height function's result `r` at `x`, `y`: `z`, or `[z c]` as any sequential value or array (see
  [[cljplot2.data.rows/as-coordinates]])."
  [x y r]
  (if-let [[z c] (data/as-coordinates r)] [x y z c] [x y r]))

(defn function-mesh
  "Mesh (see [[mesh]]) of the height function `f` of a point `[x y]` (a `Vec2`; `z` or `[z c]`) over `[[x0 x1] [y0 y1]]` at
  `[nx ny]` points."
  [f [xd yd] [nx ny]]
  (let [xs (axis-values xd nx) ys (axis-values yd ny)]
    (mesh nx ny (fn [i j] (let [x (xs i) y (ys j)] (height-node x y (f (Vec2. x y))))))))

(defn parametric-mesh
  "Mesh of the parametric function `fu` (`[x y z]` - any sequential value or array - of a point `[u v]`, a `Vec2`) over the ranges `u`, `v` at `[nu nv]` points."
  [fu u v [nu nv]]
  (let [us (axis-values u nu) vs (axis-values v nv)]
    (mesh nu nv (fn [i j] (data/as-coordinates (fu (Vec2. (us i) (vs j))))))))

(defn gridded-mesh
  "Mesh of the gridded `rows` (accessors of `mapping` `:x`, `:y`, `:z`): a node per lattice point."
  [rows mapping]
  (let [{:keys [xs ys vss]} (field/lattice-values rows (data/row-accessor :x (:x mapping)) (data/row-accessor :y (:y mapping))
                                                  (data/row-accessor :z (:z mapping)) :z)]
    (mesh (count xs) (count ys) (fn [i j] [(xs i) (ys j) (get-in vss [i j])]))))

(defn- points-density
  "`{:f :domain}` - the 2D KDE of the finite `:x`, `:y` of `rows` (see [[cljplot2.data.field/density]], widened by
  `cut` bandwidths), `f` a function of `[x y]`; `nil` without points."
  [rows mapping cut]
  (let [x-of (data/row-accessor :x (:x mapping)) y-of (data/row-accessor :y (:y mapping))
        pts (filterv (fn [[x y]] (and (finite-number? x) (finite-number? y))) (map (juxt x-of y-of) rows))]
    (when (seq pts)
      (field/density (map first pts) (map second pts) {:cut cut} [(scale/stat-transform nil) (scale/stat-transform nil)]))))

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
      :density (let [fields (into {} (keep (fn [[panel rows]] (some->> (points-density rows mapping (:cut options)) (vector panel)))) groups)
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
  (merge (theme/chart-style theme :surface options)
         (select-keys options [:stroke-color :stroke-width :color])
         {:surface-style (get options :style :surface)
          :shade? (not (false? (:shade options)))}))

(defn compute
  "Per panel: `{:mesh m :domains {:x :y :z} :frame f :style st :shading sh}` (`sh` - the chart's `:ambient` / `:light`; see [[mesh]], [[cljplot2.data.space3d/frame-settings]]);
  no position extents (the chart lays out its panel), color extents - the finite tones (computed color only),
  `:dropped` - gridded / density rows without a finite x / y."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme]}]
  (let [kind (source-kind options)
        _ (check-mapping! kind mapping)
        explicit (:domain options)
        meshes (update-vals (panel-meshes kind options mapping groups) #(clip-mesh % explicit))
        nodes (mapcat #(remove nil? (:nodes %)) (vals meshes))
        domains (space/reverse-domains
                 (into {} (map-indexed (fn [i k] [k (or (get explicit k) (space/data-domain (map #(% i) nodes) 0.0))]) space/axes))
                 (:reverse options))
        frame (cond-> (space/frame-settings options mapping)
                (and (= kind :density) (not (contains? options :axis-titles))) (assoc-in [:titles :z] "Density"))
        st (style options theme)
        colored? (nil? (:color options))]
    {:data (update-vals meshes (fn [mesh] {:mesh mesh :domains domains :frame frame
                                           :frame-overrides (space/frame-overrides options) :style st
                                           :shading (select-keys options [:ambient :light])}))
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
  "Pixels of the outline of an opaque `:surface` quad in its own fill, covering the anti-aliased seams between quads."
  1.0)

(defn- opaque?
  [color]
  (>= (double (c/alpha (c/to-color color))) 255.0))

(defn quad-corners
  "Node indices of the quad at `i`, `j` of a mesh `nu` nodes wide, in order around it."
  [^long nu ^long i ^long j]
  (let [a (+ i (* j nu)) b (+ a nu)]
    [a (inc a) (inc b) b]))

(defn chart-primitives
  "The surface's quads for one panel's `computed` data and the projection `point` (data -> `[pixel depth]`; the 3D
  chart contract, see [[cljplot2.graphics.space3d/draw-scene!]]): a `:polygon` per quad with four nodes, at its corners'
  mean depth, colored by its corners' mean tone, styled by `:style` (see the namespace doc)."
  [_chart {:keys [mesh domains frame style shading]} point scales env]
  (let [{:keys [nu nv nodes tones]} mesh
        shade (space/shader shading)
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
        (let [fill (if (:shade? style) (shade col (quad-normal rotate (mapv normalize ps))) col)]
          (cond
            (= look :both) {:kind :polygon :points pixels :depth depth :fill fill :stroke (:stroke-color style) :stroke-width width}
            ;; outlined in its own fill: no anti-aliased seams between neighbours (an opaque fill only - a translucent
            ;; outline would darken every edge where it overlaps the fill and the neighbour's)
            (opaque? fill) {:kind :polygon :points pixels :depth depth :fill fill :stroke fill :stroke-width seam-width}
            :else {:kind :polygon :points pixels :depth depth :fill fill :stroke :none}))))))
