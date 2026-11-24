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
  with a color bar titled by the `:z` accessor's name (Density for a density, none for a function); option
  `:color` - one color for all quads (no color scale). A quad whose value is outside an explicit color domain isn't
  drawn. The computed color is the plot's color scale: another chart of the plot (e.g. a cloud over the surface)
  can't map `:color` then - give the surface a fixed `:color`.

  Options (style defaults from theme `[:chart :surface]`: `:stroke-color`, `:stroke-width`):

  * `:style` - `:surface` (default; filled quads), `:wireframe` (quad outlines in their color, `:stroke-width` wide,
    over the panel's background - hidden lines covered) or `:both` (filled quads outlined by `:stroke-color`)
  * `:shade` - Lambert shading of filled quads (default true; false - the colour itself, as `:ambient` 1); `:ambient` (default 0.4) and `:light` (default `[-0.5 0.6
    -1.0]`) - the shading's ambient share and light direction (see [[cljplot2.data.space3d/shader]])

  Quads are drawn far to near by their centroid depth, with the other 3D charts of the panel (a cloud over a surface:
  both with every `:domain` explicit).

  Functions: [[compute]], [[chart-primitives]] and [[computed-aesthetics]] - the chart contract; [[source-kind]] - the source of a chart; meshes - [[mesh]], [[function-mesh]], [[parametric-mesh]], [[gridded-mesh]], [[panel-meshes]], [[clip-mesh]]; [[quad-corners]] - the nodes of a quad; [[schemas]], [[default-n]], [[seam-width]]."
  (:require [clojure2d.color :as c]
            [fastmath.core :as m]
            [fastmath.vector :as v]
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
  "Style entries of the surface (theme `[:chart :surface]`): the quad outline."
  [(schema/optional :stroke-color {:description "quad outline colour of :style :both"} schema/color)
   (schema/optional :stroke-width {:description "quad outline width in pixels (:style :both and :wireframe)"} schema/non-neg)])

(def ^:private range-schema
  "Schema of a parameter range `[lo hi]`."
  [:tuple schema/finite schema/finite])

(def ^{:private true :const true :tag 'long} default-n
  "Grid points per axis of a sampled surface, unless `:n` says otherwise."
  30)

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
                       (schema/nilable :shade {:description "shading of filled quads by their angle to the light (Lambert; false - the colour itself, as :ambient 1)" :default true} schema/bool)
                       (schema/optional :f {:description "source: a function of [x y] returning z or [z c], sampled over :domain :x / :y"}
                                        schema/function)
                       (schema/nilable :density {:description "source: the 2D kernel density of the mapped :x, :y" :default false} schema/bool)
                       (schema/optional :fu {:description "source: a parametric surface, a function of [u v] returning [x y z]"} schema/function)
                       (schema/optional :u {:description "the u range [lo hi] of :fu"} range-schema)
                       (schema/optional :v {:description "the v range [lo hi] of :fu"} range-schema)
                       (schema/optional :n {:description "grid points per axis of :f, :fu, :density: n or [nx ny]" :default default-n}
                                        field/n-schema)
                       (schema/optional :cut {:description "how many bandwidths the density domain extends past the points on each side"
                                              :cljplot2/default-doc "3 (only with :density)"}
                                        schema/finite-non-neg)))
             [#(when (m/> (count (filter % [:f :fu :density])) 1) "a surface takes one source: :f, :fu or :density true") nil]
             [#(when (and (:fu %) (not (and (:u %) (:v %)))) "a parametric surface needs :u and :v ranges") [:fu]]
             [#(when (and (or (:u %) (:v %)) (not (:fu %))) ":u and :v are the ranges of a parametric :fu") [:u]]
             [#(when (and (some? (:cut %)) (not (:density %))) ":cut widens a :density surface") [:cut]]
             space/distance-rule)
   :mapping [:maybe [:map
                     (schema/nilable :x {:description "x of a lattice node (gridded) or of a point (:density)"} schema/accessor)
                     (schema/nilable :y {:description "y of a lattice node (gridded) or of a point (:density)"} schema/accessor)
                     (schema/nilable :z {:description "height of a lattice node (gridded source)"} schema/accessor)]]})

(defn- source-kind
  "Gives the source of a surface chart.

  Parameters:

  - `options` (map): the chart's options.

  Returns `:function` (`:f`), `:parametric` (`:fu`), `:density` (`:density true`), else `:gridded`."
  [options]
  (cond (:f options) :function (:fu options) :parametric (:density options) :density :else :gridded))

(defn- value-title
  "Gives the title of the surface's computed colour.

  Parameters:

  - `options` (map): the chart's options.
  - `mapping` (map): the merged mapping.

  Returns the `:z` keyword's name for a gridded surface (`nil` for a function accessor), `Density` for a density, `nil` for a function or a parametric surface."
  [options mapping]
  (case (source-kind options)
    :gridded (let [a (:z mapping)] (when (keyword? a) (name a)))
    :density "Density"
    nil))

(defn computed-aesthetics
  "Gives the surface's computed aesthetics.

  Parameters:

  - `options` (map): the chart's options.
  - `mapping` (map): the merged mapping.

  Returns `{:color title}` - the quads coloured by value, titled by the `:z` accessor's name, Density, or none (see [[value-title]]); `{}` when a fixed `:color` option colours them."
  [options mapping]
  (if (:color options) {} {:color (value-title options mapping)}))

;; meshes

(defn- mesh
  "Builds a mesh of `nu` x `nv` nodes, row by row.

  Parameters:

  - `nu`, `nv` (longs): nodes per row and rows.
  - `node-at` (function): of `i`, `j` giving the node at `i + j nu` - `[x y z]` or `[x y z tone]`.

  Returns `{:nu :nv :nodes [..] :tones [..]}`: `:nodes` - `[x y z]` (doubles), `nil` for a non-finite coordinate; `:tones` - each node's colour value (`tone`, else `z`), `##NaN` when missing or not finite."
  [nu nv node-at]
  (let [raw (vec (for [j (range nv) i (range nu)] (node-at i j)))
        ok? (fn [n] (and (sequential? n) (every? schema/finite? (take 3 n))))]
    {:nu (long nu) :nv (long nv)
     :nodes (mapv (fn [n] (when (ok? n) (mapv double (take 3 n)))) raw)
     :tones (mapv (fn [n] (let [t (if (m/> (count n) 3) (nth n 3) (nth n 2 nil))]
                            (if (and (ok? n) (schema/finite? t)) (double t) ##NaN)))
                  raw)}))

(defn- axis-values
  "Gives evenly spaced values over a range, ends included.

  Parameters:

  - `[lo hi]` (doubles): the range.
  - `n` (long): the number of values.

  Returns a vector of `n` values (one value - the midpoint; zero - empty)."
  [[^double lo ^double hi] ^long n]
  (vec (m/slice-range lo hi n)))

(defn- height-node
  "Builds the node of a height function's result at a point.

  Parameters:

  - `x`, `y`: the point.
  - `r`: the function's result - `z`, or `[z c]` as any sequential value or array (see [[cljplot2.data.rows/as-coordinates]]).

  Returns `[x y z c]` for a pair, else `[x y r]`."
  [x y r]
  (if-let [[z c] (data/as-coordinates r)] [x y z c] [x y r]))

(defn- function-mesh
  "Samples a height function into a mesh (see [[mesh]]).

  Parameters:

  - `f` (function): of a point `[x y]` (a `Vec2`) giving `z` or `[z c]`.
  - `[xd yd]`: the x and y ranges `[lo hi]`.
  - `[nx ny]` (longs): grid points per axis.

  Returns the mesh of `f` at `nx` x `ny` evenly spaced points, ends included."
  [f [xd yd] [nx ny]]
  (let [xs (axis-values xd nx)
        ys (axis-values yd ny)]
    (mesh nx ny (fn [i j] (let [x (xs i) y (ys j)] (height-node x y (f (Vec2. x y))))))))

(defn- parametric-mesh
  "Samples a parametric surface into a mesh (see [[mesh]]).

  Parameters:

  - `fu` (function): of a point `[u v]` (a `Vec2`) giving `[x y z]` - any sequential value or array.
  - `u`, `v`: the parameter ranges `[lo hi]`.
  - `[nu nv]` (longs): grid points per parameter.

  Returns the mesh of `fu` at `nu` x `nv` evenly spaced parameter points, ends included."
  [fu u v [nu nv]]
  (let [us (axis-values u nu)
        vs (axis-values v nv)]
    (mesh nu nv (fn [i j] (data/as-coordinates (fu (Vec2. (us i) (vs j))))))))

(defn- gridded-mesh
  "Builds the mesh of gridded rows: a node per lattice point.

  Parameters:

  - `rows` (sequence of maps): one row per lattice node.
  - `mapping` (map): `:x`, `:y`, `:z` accessors.

  Returns the mesh (see [[mesh]]) over every distinct x with every distinct y (see [[cljplot2.data.field/lattice-values]], errors keyed `:z`)."
  [rows mapping]
  (let [{:keys [xs ys vss]} (field/lattice-values rows (data/row-accessor :x (:x mapping)) (data/row-accessor :y (:y mapping))
                                                  (data/row-accessor :z (:z mapping)) :z)]
    (mesh (count xs) (count ys) (fn [i j] [(xs i) (ys j) (get-in vss [i j])]))))

(defn- points-density
  "Estimates the 2D kernel density of the finite points of rows.

  Parameters:

  - `rows` (sequence of maps): the panel's rows.
  - `mapping` (map): `:x`, `:y` accessors.
  - `cut`: bandwidths the density's domain extends past the points (`nil` - the default, see [[cljplot2.data.field/density]]).

  Returns `{:f :domain}` - `f` a function of `[x y]`; `nil` without finite points."
  [rows mapping cut]
  (let [x-of (data/row-accessor :x (:x mapping))
        y-of (data/row-accessor :y (:y mapping))
        pts (filterv (fn [[x y]] (and (schema/finite? x) (schema/finite? y))) (map (juxt x-of y-of) rows))]
    (when (seq pts)
      (field/density (map first pts) (map second pts) {:cut cut} [(scale/stat-transform nil) (scale/stat-transform nil)]))))

(defn- grid-size
  "Gives the grid size of the `:n` option.

  Parameters:

  - `n` (integer, `[nx ny]` or `nil`): the option; `nil` - [[default-n]].

  Returns `[nx ny]` (see [[cljplot2.data.field/grid-size]])."
  [n]
  (field/grid-size (or n default-n)))

(defn- explicit-xy
  "Gives the sampling ranges of a surface.

  Parameters:

  - `domain` (map or `nil`): the chart's `:domain` - optional `:x`, `:y` ranges.
  - `[fx fy]`: the fallback x and y ranges.

  Returns `[[x0 x1] [y0 y1]]`: the explicit `:x` / `:y` of `domain`, else the fallback."
  [domain [fx fy]]
  [(or (:x domain) fx) (or (:y domain) fy)])

(defn- panel-meshes
  "Builds the mesh of every panel by the chart's source.

  Parameters:

  - `kind` (keyword): the source (see [[source-kind]]).
  - `options` (map): the chart's options - `:f`, `:fu`, `:u`, `:v`, `:n`, `:cut`, `:domain`.
  - `mapping` (map): the merged mapping.
  - `groups` (map): panel -> rows.

  Returns panel -> mesh: a function or a parametric surface - the same mesh in every panel; gridded and density - from the panel's rows, panels without rows (or without a density) left out; a density's domain is the union of the panels' (unless explicit).

  Throws `ex-info {:key :domain}` for a function surface without `:domain` `:x` and `:y`."
  [kind options mapping groups]
  (let [size (grid-size (:n options))
        domain (:domain options)]
    (case kind
      :function (let [{:keys [x y]} domain]
                  (when-not (and x y)
                    (schema/fail! "A surface of :f needs a :domain with :x and :y." {:key :domain}))
                  (let [shared (function-mesh (:f options) [x y] size)] (update-vals groups (constantly shared))))
      :parametric (let [shared (parametric-mesh (:fu options) (:u options) (:v options) size)] (update-vals groups (constantly shared)))
      :gridded (into {} (keep (fn [[panel rows]] (when (seq rows) [panel (gridded-mesh rows mapping)]))) groups)
      :density (let [fields (into {} (keep (fn [[panel rows]] (some->> (points-density rows mapping (:cut options)) (vector panel)))) groups)
                     xy (explicit-xy domain (field/union-domain (map :domain (vals fields))))]
                 (update-vals fields (fn [{:keys [f]}] (function-mesh f xy size)))))))

(defn- clip-mesh
  "Leaves out the nodes of a mesh outside explicit domains.

  Parameters:

  - `node-mesh` (map): a mesh (see [[mesh]]).
  - `domains` (map): `{:x [a b] ..}`, each optional (see [[cljplot2.data.space3d/in-domain?]]).

  Returns the mesh with the nodes outside the domains `nil` and their tones `##NaN`."
  [node-mesh domains]
  (let [inside? (fn [n] (and n (every? identity (map-indexed (fn [i k] (space/in-domain? (get domains k) (n i))) space/axes))))
        kept (mapv inside? (:nodes node-mesh))]
    (-> node-mesh
        (update :nodes (fn [nodes] (mapv (fn [n k] (when k n)) nodes kept)))
        (update :tones (fn [tones] (mapv (fn [t k] (if k t ##NaN)) tones kept))))))

(defn- position-keys
  "Gives the positions a surface source reads from rows.

  Parameters:

  - `kind` (keyword): the source kind (see [[source-kind]]).

  Returns `[:x :y :z]` for `:gridded`, `[:x :y]` for `:density`, `[]` for the others."
  [kind]
  (case kind :gridded [:x :y :z] :density [:x :y] []))

(defn- check-mapping!
  "Checks a surface's mapping has the positions its source reads.

  Parameters:

  - `kind` (keyword): the source - gridded needs `:x`, `:y`, `:z`; density `:x`, `:y`; the others none.
  - `mapping` (map): the merged mapping.

  Returns `nil`.

  Throws `ex-info {:key :mapping}` for a missing position."
  [kind mapping]
  (doseq [k (position-keys kind)
          :when (nil? (get mapping k))]
    (schema/fail! (str "A " (name kind) " surface needs a mapped " k ".") {:key :mapping :in [:mapping k]})))

(defn compute
  "Computes the surface's stats: a mesh per panel in a box (the chart contract, see [[cljplot2.chart]]).

  Parameters:

  - `chart` (map): the chart value - `:kind`, `:options` (see the namespace doc), `:schema` (its style keys, see [[cljplot2.theme/resolved-style]]).
  - `split` (map): `:groups` - panel -> rows; `:mapping` - the merged mapping.
  - `env` (map): `:theme`.

  Returns `{:data {panel computed} :extents :dropped}`; per panel `{:mesh m :domains {:x :y :z} :frame f :frame-overrides o :style st :shading sh}` - see [[mesh]], [[cljplot2.data.space3d/frame-settings]]; `st` - the resolved style ([[cljplot2.theme/resolved-style]]) with the `:color` option, `:surface-style` (the `:style` option) and `:shade`; `sh` - the chart's `:ambient` / `:light`. No position extents (the chart lays out its panel); colour extents - the finite tones (computed colour only); `:dropped` - gridded or density rows without a finite x / y, `:field` - panels with rows but no mesh (see [[cljplot2.data.field/panels-without-field]]).

  Throws `ex-info {:key :mapping}` for a missing position ([[check-mapping!]]) or one without a finite value in non-empty rows, `{:key :domain}` for a function without a domain."
  [{:keys [options] :as chart} {:keys [groups mapping]} {:keys [theme]}]
  (let [kind (source-kind options)
        _ (check-mapping! kind mapping)
        _ (data/check-mapped! (mapcat val groups) (map (fn [k] [k (data/row-accessor k (mapping k))]) (position-keys kind))
                              (fn [_ v] (schema/finite? v)))
        explicit (:domain options)
        meshes (update-vals (panel-meshes kind options mapping groups) #(clip-mesh % explicit))
        nodes (mapcat #(remove nil? (:nodes %)) (vals meshes))
        domains (space/reverse-domains
                 (into {} (map-indexed (fn [i k] [k (or (get explicit k) (space/data-domain (map #(% i) nodes) 0.0))]) space/axes))
                 (:reverse options))
        frame (cond-> (space/frame-settings options mapping)
                (and (= kind :density) (not (contains? options :axis-titles))) (assoc-in [:titles :z] "Density"))
        st (merge (theme/resolved-style theme chart)
                  (select-keys options [:color])
                  {:surface-style (data/option options :style :surface)
                   :shade (not (false? (:shade options)))})
        colored? (nil? (:color options))]
    {:data (update-vals meshes (fn [panel-mesh] {:mesh panel-mesh :domains domains :frame frame
                                                 :frame-overrides (space/frame-overrides options) :style st
                                                 :shading (select-keys options [:ambient :light])}))
     :extents (update-vals meshes (fn [{:keys [tones]}]
                                    (let [ts (remove m/nan? tones)]
                                      (if (and colored? (seq ts))
                                        {:color {:kind :numeric :min (v/mn ts) :max (v/mx ts)}}
                                        {}))))
     :dropped (if (#{:gridded :density} kind)
                (assoc (field/dropped-positions :gridded mapping groups) :field (field/panels-without-field groups (set (keys meshes))))
                {})}))

;; drawing

(defn- quad-normal
  "Gives the view-space unit normal of a quad.

  Parameters:

  - `rotate` (function): the view rotation, a linear map (see [[cljplot2.data.space3d/rotation]]).
  - `[a b c d]`: the quad's normalized corners, in order around it.

  Returns the cross product of its diagonals rotated, normalized; a zero vector for a degenerate quad."
  ^Vec3 [rotate [a b c d]]
  (let [^Vec3 n (rotate (v/cross (v/sub c a) (v/sub d b)))
        len (v/mag n)]
    (if (m/pos? len) (v/div n len) n)))

(defn- tone-color
  "Gives the colour function of the quads.

  Parameters:

  - `color`: the fixed `:color` option, `nil` - none.
  - `scale` (map): the plot's colour scale, `nil` - none.
  - `na`: the NA colour.

  Returns a function of a tone giving a quad's colour: `color` when fixed; else the scale's, `na` for `##NaN`, `nil` outside the scale's domain; `na` without a scale."
  [color scale na]
  (cond color (constantly color)
        scale (let [f (aesthetic/attribute-fn :color scale na)] (fn [^double t] (if (m/nan? t) na (f t))))
        :else (constantly na)))

(defn- background
  "Gives the colour a wireframe's quads are filled with, covering the lines behind them.

  Parameters:

  - `env` (map): the drawing environment - `:theme`.

  Returns the theme's panel fill, or the plot background when the panel fill is `:none`."
  [env]
  (let [fill (get-in env [:theme :panel :fill])]
    (if (theme/none? fill) (get-in env [:theme :plot :background]) fill)))

(def ^{:private true :const true :tag 'double} seam-width
  "Pixels of the outline of an opaque `:surface` quad in its own fill, covering the anti-aliased seams between quads (an artefact fix, not an option)."
  1.0)

(defn- opaque?
  "Tests whether a colour is fully opaque.

  Parameters:

  - `color`: a colour (any clojure2d representation).

  Returns true for alpha 255, else false."
  [color]
  (m/>= (c/alpha color) 255.0))

(defn- quad-corners
  "Gives the node indices of a quad, in order around it.

  Parameters:

  - `nu` (long): nodes per mesh row.
  - `i`, `j` (longs): the quad's column and row.

  Returns `[a (a + 1) (b + 1) b]` with `a = i + j nu`, `b = a + nu`."
  [^long nu ^long i ^long j]
  (let [a (m/+ i (m/* j nu))
        b (m/+ a nu)]
    [a (m/long-inc a) (m/long-inc b) b]))

(defn chart-primitives
  "Gives the surface's quads for one panel (the 3D chart contract, see [[cljplot2.content.space3d/draw-scene!]]).

  Parameters:

  - `chart` (map): the chart value (unused).
  - `computed` (map): the panel's computed data - `:mesh`, `:domains`, `:frame`, `:style`, `:shading` (see [[compute]]).
  - `point` (function): the projection, data point -> `[pixel depth]`.
  - `scales` (map): the plot's scales (`:color`).
  - `env` (map): the drawing environment (`:theme`).

  Returns a `:polygon` per quad whose four nodes are present, at its corners' mean depth, coloured by its corners' mean tone (a quad whose colour is outside the scale is left out): `:wireframe` - outlined in its colour over the background; `:both` - filled, shaded and outlined by `:stroke-color`; `:surface` - filled and shaded, an opaque fill outlined in itself ([[seam-width]])."
  [_chart {:keys [domains frame style shading] panel-mesh :mesh} point scales env]
  (let [{:keys [nu nv nodes tones]} panel-mesh
        shade (space/shader shading)
        {:keys [view aspect]} frame
        normalize (space/normalizer domains aspect)
        rotate (space/rotation view)
        color-of (tone-color (:color style) (:color scales) (get-in env [:theme :palette :na]))
        look (:surface-style style)
        width (:stroke-width style)
        back (background env)]
    (for [j (range (m/long-dec nv)) i (range (m/long-dec nu))
          :let [ids (quad-corners nu i j)
                ps (mapv nodes ids)]
          :when (every? some? ps)
          :let [col (color-of (m// (v/sum (map tones ids)) 4.0))]
          :when (some? col)
          :let [projected (mapv point ps)
                depth (m// (v/sum (map second projected)) 4.0)
                pixels (mapv first projected)]]
      (if (= look :wireframe)
        {:kind :polygon :points pixels :depth depth :fill back :stroke-color col :stroke-width width}
        (let [fill (if (:shade style) (shade col (quad-normal rotate (mapv normalize ps))) col)]
          (cond
            (= look :both) {:kind :polygon :points pixels :depth depth :fill fill :stroke-color (:stroke-color style) :stroke-width width}
            ;; outlined in its own fill: no anti-aliased seams between neighbours (an opaque fill only - a translucent
            ;; outline would darken every edge where it overlaps the fill and the neighbour's)
            (opaque? fill) {:kind :polygon :points pixels :depth depth :fill fill :stroke-color fill :stroke-width seam-width}
            :else {:kind :polygon :points pixels :depth depth :fill fill :stroke-color :none}))))))
