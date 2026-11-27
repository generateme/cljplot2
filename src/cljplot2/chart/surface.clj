(ns cljplot2.chart.surface
  "Surface: a mesh of quads in a 3D box (see [[cljplot2.data.space3d]]; a chart laying out its own panel, as
  [[cljplot2.chart.cloud]], whose frame options it shares: `:view` (with `:distance` - perspective), `:aspect`,
  `:box`, `:axes`, `:domain`, `:reverse`; axis titles from the plot's `:labels`, else the mapped column names (a
  density surface's z reads `Density`)).

  Sources (one per chart):

  * gridded (default) - mapped `:x`, `:y`, `:z` on a full lattice (every distinct x with every distinct y, once; see
    [[cljplot2.data.field/lattice-values]], errors keyed `:z`): a node per row, with its colour value when `:color` is
    mapped
  * `:f` - a function of a point `[x y]` (a `fastmath.vector.Vec2`, see Points in [[cljplot2.data.field]]) returning
    its height `z`, or `[z c]` (any sequential value or array) (height and color value `c`), sampled on a
    regular grid of `:n` (default 30, or `[nx ny]`) points per axis over `:domain` `:x` / `:y` (required)
  * `:density true` - the 2D kernel density of mapped `:x`, `:y` (see [[cljplot2.data.field/density]], default
    bandwidths) over the points' range widened by `:cut` bandwidths (default 3; 0 - the range), sampled as `:f` over
    the panels' density domains (or the explicit x / y domain)
  * `:fu` - a parametric surface: a function of `[u v]` (a `Vec2`) returning `[x y z]` or `[x y z c]` (any sequential value or array; `c` the colour value), sampled on `:n` points over the ranges
    `:u [lo hi]`, `:v [lo hi]` (both required with `:fu`, refused without it)

  Nodes with a non-finite coordinate, or outside an explicit `:domain`, are left out with every quad they touch.
  Domains: explicit or the nodes' over all panels, made nice ([[cljplot2.data.space3d/nice-domain]]). Facets: a mesh per panel (gridded / density - the panel's rows; a
  function - the same in every panel), domains shared.

  Colors, first that applies:

  * option `:color` - one color for all quads (no color scale; a `:color` mapping inherited from the plot is unused)
  * a mapped `:color` (gridded data only) - through the plot's color scale: a quad takes its corners' mean value on
    a linear or binned scale (dates too), else their most common value (a tie - the first corner, in [[quad-corners]]
    order); a quad with a `nil` corner is NA (with `:na-level false` such a node is left out, as below); a node whose
    value the scale doesn't keep (invalid, or outside its domain - see the scale's `:out-of-domain`) is left out with
    its quads, counted in `:dropped :color`; under `:out-of-domain :as-na` a quad with a corner outside the domain is
    NA, under `:clamp` its corners are clamped to the domain before the mean; the legend is titled by the accessor's
    name; a column without a valid value is an error
  * computed - each quad's mean height (or `c` of `[z c]` / `[x y z c]`) through the plot's color scale, with a color
    bar titled by the `:z` accessor's name (Density for a density, none for a function); a quad whose value is
    outside an explicit color domain isn't drawn. The computed color is the plot's color scale: another chart of the
    plot (e.g. a cloud over the surface) can't map `:color` then - map it on a gridded surface too, or give the surface
    a fixed `:color`.

  A mapped `:color` with `:f`, `:fu` or `:density` is an error, unless a fixed `:color` makes an inherited one unused.

  Options (style defaults from theme `[:chart :surface]`: `:stroke-color`, `:stroke-width`):

  * `:style` - `:surface` (default; filled quads), `:wireframe` (quad outlines in their color, `:stroke-width` wide,
    over the panel's background - hidden lines covered) or `:both` (filled quads outlined by `:stroke-color`)
  * `:shade` - Lambert shading of filled quads (default true; false - the colour itself, as `:ambient` 1); `:ambient` (default 0.4) and `:light` (default `[-0.5 0.6
    -1.0]`) - the shading's ambient share and light direction (see [[cljplot2.data.space3d/shader]])

  Quads are drawn far to near by their centroid depth, with the other 3D charts of the panel (a cloud over a surface:
  both with every `:domain` explicit).

  Functions: [[compute]], [[chart-primitives]], [[legend]] and [[computed-aesthetics]] - the chart contract; [[source-kind]] - the source of a chart, [[mapped-color?]] - whether a mapped `:color` colours it; meshes - [[mesh]], [[function-mesh]], [[parametric-mesh]], [[gridded-mesh]], [[panel-meshes]], [[clip-mesh]]; [[quad-corners]] - the nodes of a quad; [[schemas]], [[default-n]], [[seam-width]]."
  (:require [clojure2d.color :as c]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.data.field :as field]
            [cljplot2.data.group :as group]
            [cljplot2.data.rows :as data]
            [cljplot2.data.space3d :as space]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2 Vec3]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries
  "Style entries of the surface (theme `[:chart :surface]`): the quad outline."
  [(schema/optional :stroke-color {:description "The outline colour of the quads with `:style :both`."
                                    :cljplot2/value-meanings {:none "No outline."} :cljplot2/example :white :cljplot2/example-context {:style :both}}
                    schema/color)
   (schema/optional :stroke-width {:description "The outline width of the quads in pixels, with `:style :both` or `:wireframe`."
                                   :cljplot2/example 3 :cljplot2/example-context {:style :wireframe}}
                    schema/non-neg)])

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
                 (conj (schema/optional :style {:description "How the chart draws the mesh."
                                                :default :surface
                                                :cljplot2/value-meanings {:surface "Filled quads." :wireframe "Quad outlines, each in its quad's colour; nearer quads hide the lines behind them."
                                                                          :both "Filled quads outlined by `:stroke-color`."}}
                                        [:enum :surface :wireframe :both])
                       (schema/optional :color {:description "One colour for every quad, instead of the colour scale; a `:color` mapping inherited from the plot is then unused."
                                                :cljplot2/default-doc "Gridded data with a mapped `:color` takes it through the colour scale; otherwise each quad takes its mean height, or the `c` of `[z c]` or `[x y z c]`, through the plot's colour scale."
                                                :cljplot2/example :steelblue}
                                        schema/solid-color)
                       (schema/nilable :shade {:description "Whether the chart shades filled quads by their angle to the light."
                                               :default true
                                               :cljplot2/value-meanings {true "Lambert shading." false "The colour itself, as `:ambient 1`."}}
                                       schema/bool)
                       (schema/optional :f {:description "The surface as a function of `[x y]` that returns z, or `[z c]` with `c` the colour value, sampled over `:domain :x` and `:y`."
                                            :cljplot2/default-doc "None: a gridded `:z` mapping, `:fu` or `:density` gives the surface."
                                            :cljplot2/example '(fn [[x y]] (m/* x y)) :cljplot2/example-context {:domain {:x [-1 1] :y [-1 1]}}}
                                        schema/function)
                       (schema/nilable :density {:description "Whether the surface is the 2D kernel density of the mapped `:x` and `:y`."
                                                 :default false
                                                 :cljplot2/value-meanings {true "The density of the points." false "Another source gives the surface."}}
                                       schema/bool)
                       (schema/optional :fu {:description "A parametric surface: a function of `[u v]` that returns `[x y z]`, or `[x y z c]` with `c` the colour value, over `:u` and `:v`."
                                             :cljplot2/default-doc "None: another source gives the surface."
                                             :cljplot2/example '(fn [[u v]] [(m/* u (m/cos v)) (m/* u (m/sin v)) u])
                                             :cljplot2/example-context {:u [0 1] :v [0 6.28]}}
                                        schema/function)
                       (schema/optional :u {:description "The range `[lo hi]` of `:fu`'s u parameter." :cljplot2/default-doc "None; `:fu` needs it."
                                            :cljplot2/example [0 0.5] :cljplot2/example-context {:fu '(fn [[u v]] [(m/* u (m/cos v)) (m/* u (m/sin v)) u]) :v [0 6.28]}}
                                        range-schema)
                       (schema/optional :v {:description "The range `[lo hi]` of `:fu`'s v parameter." :cljplot2/default-doc "None; `:fu` needs it."
                                            :cljplot2/example [0 3.14] :cljplot2/example-context {:fu '(fn [[u v]] [(m/* u (m/cos v)) (m/* u (m/sin v)) u]) :u [0 1]}}
                                        range-schema)
                       (schema/optional :n {:description "The number of grid points per axis at which the chart samples `:f`, `:fu` or `:density`."
                                            :default default-n :cljplot2/example 6
                                            :cljplot2/example-context {:f '(fn [[x y]] (m/* x y)) :domain {:x [-1 1] :y [-1 1]}}
                                            :cljplot2/form-meanings {:int "The same count on both axes." :tuple "`[nx ny]`, a count per axis."}}
                                        field/n-schema)
                       (schema/optional :cut {:description "How far the density reaches past the points' range on each side, in bandwidths; `0` stops at the range. It needs `:density true`."
                                              :cljplot2/default-doc "`3`." :cljplot2/example 0 :cljplot2/example-context {:density true}}
                                        schema/finite-non-neg)))
             [#(when (m/> (count (filter % [:f :fu :density])) 1) "a surface takes one source: :f, :fu or :density true") nil]
             [#(when (and (:fu %) (not (and (:u %) (:v %)))) "a parametric surface needs :u and :v ranges") [:fu]]
             [#(when (and (or (:u %) (:v %)) (not (:fu %))) ":u and :v are the ranges of a parametric :fu") [:u]]
             [#(when (and (some? (:cut %)) (not (:density %))) ":cut widens a :density surface") [:cut]]
             [#(when (and (some? (get-in % [:mapping :color])) (some? (:color %)))
                 "a surface takes a fixed :color or a mapped :color, not both") [:mapping :color]]
             [#(when (and (some? (get-in % [:mapping :color])) (some % [:f :fu :density]))
                 "a mapped :color colours gridded data; :f, :fu and :density surfaces compute their colour - drop the mapped :color or use gridded data") [:mapping :color]]
             space/distance-rule)
   :mapping [:maybe [:map
                     (schema/nilable :x {:description "The x of the gridded heights, or of the points whose density the chart estimates with `:density`."} schema/accessor)
                     (schema/nilable :y {:description "The y of the gridded heights, or of the points whose density the chart estimates with `:density`."} schema/accessor)
                     (schema/nilable :z {:description "The height of a lattice node of gridded data."} schema/accessor)
                     (schema/nilable :color {:description "The colour value of a lattice node of gridded data. A quad takes its corners' mean on a linear or binned scale, else their most common value; a `nil` corner makes it NA."} schema/accessor)]]})

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

(defn mapped-color?
  "Tests whether a mapped `:color` colours a surface's quads.

  Parameters:

  - `options` (map): the chart's options.
  - `mapping` (map): the merged mapping.

  Returns true for a gridded surface without a fixed `:color` option and with a mapped `:color`, else false."
  [options mapping]
  (and (= :gridded (source-kind options)) (nil? (:color options)) (some? (:color mapping))))

(defn computed-aesthetics
  "Gives the surface's computed aesthetics.

  Parameters:

  - `options` (map): the chart's options.
  - `mapping` (map): the merged mapping.

  Returns `{:color title}` - the quads coloured by value, titled by the `:z` accessor's name, Density, or none (see [[value-title]]); `{}` when a fixed `:color` option or a mapped `:color` ([[mapped-color?]]) colours them."
  [options mapping]
  (if (or (:color options) (mapped-color? options mapping)) {} {:color (value-title options mapping)}))

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
  - `mapping` (map): `:x`, `:y`, `:z` accessors, and `:color` when `colored?`.
  - `colored?`: truthy - read each node's mapped colour value.

  Returns the mesh (see [[mesh]]) over every distinct x with every distinct y (see [[cljplot2.data.field/lattice-values]], errors keyed `:z`); when `colored?` also `:colors` - each node's colour value as in its row, in the order of `:nodes`."
  [rows mapping colored?]
  (let [{:keys [xs ys vss extras]} (field/lattice-values rows (data/row-accessor :x (:x mapping)) (data/row-accessor :y (:y mapping))
                                                         (data/row-accessor :z (:z mapping)) :z
                                                         (when colored? (data/row-accessor :color (:color mapping))))
        node-mesh (mesh (count xs) (count ys) (fn [i j] [(xs i) (ys j) (get-in vss [i j])]))]
    (cond-> node-mesh
      colored? (assoc :colors (vec (for [j (range (count ys)) i (range (count xs))] (get-in extras [i j])))))))

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

  Returns panel -> mesh (gridded with `:colors` under a mapped `:color`, see [[mapped-color?]]): a function or a parametric surface - the same mesh in every panel; gridded and density - from the panel's rows, panels without rows (or without a density) left out; a density's domain is the union of the panels' (unless explicit).

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
      :gridded (let [colored? (mapped-color? options mapping)]
                 (into {} (keep (fn [[panel rows]] (when (seq rows) [panel (gridded-mesh rows mapping colored?)]))) groups))
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
  "Checks a surface's mapping has the positions its source reads, and no `:color` it can't use.

  Parameters:

  - `kind` (keyword): the source - gridded needs `:x`, `:y`, `:z`; density `:x`, `:y`; the others none.
  - `options` (map): the chart's options - `:color`.
  - `mapping` (map): the merged mapping.

  Returns `nil`.

  Throws `ex-info {:key :mapping}` for a missing position, or for a mapped `:color` (inherited from the plot - the chart's own is a schema rule) on a surface that isn't gridded and has no fixed `:color`."
  [kind options mapping]
  (doseq [k (position-keys kind)
          :when (nil? (get mapping k))]
    (schema/fail! (str "A " (name kind) " surface needs a mapped " k ".") {:key :mapping :in [:mapping k]}))
  (when (and (not= :gridded kind) (nil? (:color options)) (some? (:color mapping)))
    (schema/fail! (str "A mapped :color colours gridded data; a " (name kind) " surface computes its colour - set :color nil in the chart's :mapping, or give the chart a fixed :color.")
                  {:key :mapping :in [:mapping :color] :value (:color mapping)})))

(defn- present-colors
  "Gives the colour values of a mesh's present nodes.

  Parameters:

  - `node-mesh` (map): a mesh with `:colors` (see [[gridded-mesh]]).

  Returns the colour values of the present nodes (those not left out), in node order; a `nil` value is NA."
  [{:keys [nodes colors]}]
  (map second (filter first (map vector nodes colors))))

(defn- keep-colors
  "Leaves out the nodes of a mesh whose colour value the colour scale doesn't keep.

  Parameters:

  - `node-mesh` (map): a mesh with `:colors` (see [[gridded-mesh]]).
  - `kept?` (function): of a colour value, truthy when it is drawn.

  Returns `{:mesh m :dropped n}` - the mesh with those nodes `nil` (their quads left out) and the number of present nodes left out."
  [{:keys [nodes colors] :as node-mesh} kept?]
  (let [left-out (mapv (fn [n c] (and (some? n) (not (kept? c)))) nodes colors)]
    {:mesh (assoc node-mesh :nodes (mapv (fn [n out] (when-not out n)) nodes left-out))
     :dropped (count (filter true? left-out))}))

(defn compute
  "Computes the surface's stats: a mesh per panel in a box (the chart contract, see [[cljplot2.chart]]).

  Parameters:

  - `chart` (map): the chart value - `:kind`, `:options` (see the namespace doc), `:schema` (its style keys, see [[cljplot2.theme/resolved-style]]).
  - `split` (map): `:groups` - panel -> rows; `:mapping` - the merged mapping.
  - `env` (map): `:theme`, `:na-level` (default true - a `nil` colour value is NA, else its node is left out), `:plot-scales`.

  Returns `{:data {panel computed} :extents :dropped}` (with a mapped `:color` also `:na`); per panel `{:mesh m :domains {:x :y :z} :frame f :frame-overrides o :style st :shading sh}` - see [[mesh]], [[cljplot2.data.space3d/frame-settings]]; `st` - the resolved style ([[cljplot2.theme/resolved-style]]) with the `:color` option, `:surface-style` (the `:style` option) and `:shade`; `sh` - the chart's `:ambient` / `:light`. No position extents (the chart lays out its panel); colour extents - the finite tones (computed colour), the kept nodes' colour values (a mapped `:color`, see [[cljplot2.scale.aesthetic/value-extent]]); `:na` - `#{:color}` when a kept node's colour value is `nil`; `:dropped` - gridded or density rows without a finite x, y or (gridded) `:z`, each counted once (see [[cljplot2.data.field/non-finite-counts]]), `:color` - nodes whose colour value the scale doesn't keep (a mapped `:color`, see [[cljplot2.data.group/kept-value?]]), `:field` - panels with rows but no mesh (see [[cljplot2.data.field/panels-without-field]]).

  Throws `ex-info {:key :mapping}` for a missing position or an unusable `:color` ([[check-mapping!]]), a position without a finite value or a mapped `:color` without a valid value in non-empty rows, `{:key :domain}` for a function without a domain."
  [{:keys [options] :as chart} {:keys [groups mapping]} {:keys [theme na-level plot-scales] :or {na-level true}}]
  (let [kind (source-kind options)
        _ (check-mapping! kind options mapping)
        colored? (mapped-color? options mapping)
        _ (data/check-mapped! (mapcat val groups) (map (fn [k] [k (data/row-accessor k (mapping k))])
                                                       (cond-> (position-keys kind) colored? (conj :color)))
                              (fn [k v] (if (= k :color) (aesthetic/valid-value? k (:color plot-scales) v) (schema/finite? v))))
        explicit (:domain options)
        clipped (update-vals (panel-meshes kind options mapping groups) #(clip-mesh % explicit))
        color-kept (when colored? (update-vals clipped #(keep-colors % (fn [c] (group/kept-value? :color na-level plot-scales c)))))
        meshes (if colored? (update-vals color-kept :mesh) clipped)
        nodes (mapcat #(remove nil? (:nodes %)) (vals meshes))
        domains (space/reverse-domains
                 (into {} (map-indexed (fn [i k] [k (or (get explicit k) (space/data-domain (map #(% i) nodes) 0.0))]) space/axes))
                 (:reverse options))
        frame (cond-> (space/frame-settings options mapping)
                (= kind :density) (assoc-in [:titles :z] "Density"))
        st (merge (theme/resolved-style theme chart)
                  (select-keys options [:color])
                  {:surface-style (data/option options :style :surface)
                   :shade (not (false? (:shade options)))})
        computed? (and (nil? (:color options)) (not colored?))]
    (cond-> {:data (update-vals meshes (fn [panel-mesh] {:mesh panel-mesh :domains domains :frame frame
                                                         :frame-overrides (space/frame-overrides options) :style st
                                                         :shading (select-keys options [:ambient :light])}))
             :extents (update-vals meshes (fn [{:keys [tones] :as panel-mesh}]
                                            (if colored?
                                              (if-let [e (aesthetic/value-extent :color (:color plot-scales) (remove nil? (present-colors panel-mesh)))]
                                                {:color e}
                                                {})
                                              (let [ts (remove m/nan? tones)]
                                                (if (and computed? (seq ts))
                                                  {:color {:kind :numeric :min (v/mn ts) :max (v/mx ts)}}
                                                  {})))))
             :dropped (cond-> (if (#{:gridded :density} kind)
                                (assoc (field/non-finite-counts (position-keys kind) mapping groups) :field (field/panels-without-field groups (set (keys meshes))))
                                {})
                        colored? (assoc :color (reduce m/long-add 0 (map :dropped (vals color-kept)))))}
      colored? (assoc :na (if (some #(some nil? (present-colors %)) (vals meshes)) #{:color} #{})))))

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

(defn- most-common
  "Gives the most common of a quad's corner values.

  Parameters:

  - `vs` (sequence): the corner values, in [[quad-corners]] order.

  Returns the value occurring most often (levels compared by [[cljplot2.scale.aesthetic/level-key]]); a tie - the first of the tied values in `vs`."
  [vs]
  (let [counts (frequencies (map aesthetic/level-key vs))
        top (long (v/mx (vals counts)))]
    (first (filter #(m/== top (long (counts (aesthetic/level-key %)))) vs))))

(defn- value-range
  "Gives the range of values a linear or binned colour scale colours.

  Parameters:

  - `scale` (map): the colour scale spec.

  Returns `[lo hi]`: a linear scale's domain, a binned scale's first and last break; `nil` for other kinds."
  [scale]
  (case (:kind scale)
    :linear (:domain scale)
    :binned (let [breaks (:breaks scale)] [(first breaks) (last breaks)])
    nil))

(defn- quad-value
  "Gives a quad's colour value from its corners' mapped values.

  Parameters:

  - `scale` (map): the colour scale spec - `:kind`, `:out-of-domain` and the range of [[value-range]].
  - `vs` (sequence): the four corner values, in [[quad-corners]] order.

  Returns `nil` (NA) when a corner is `nil`; on a linear or binned scale with numbers or dates the mean of their numbers ([[cljplot2.data.rows/x-key]]) - under `:out-of-domain :as-na` `nil` when a corner lies outside the range, under `:clamp` the mean of the corners clamped to it; otherwise the most common value ([[most-common]])."
  [scale vs]
  (let [bounds (value-range scale)
        xs (when bounds (map data/x-key vs))]
    (cond (some nil? vs) nil
          (and xs (every? some? xs))
          (let [[^double lo ^double hi] bounds
                inside? (fn [x] (m/<= lo (double x) hi))
                mean (fn [ys] (m// (v/sum ys) 4.0))]
            (case (:out-of-domain scale)
              :as-na (when (every? inside? xs) (mean xs))
              :clamp (mean (map #(m/constrain (double %) lo hi) xs))
              (mean xs)))
          :else (most-common vs))))

(defn- quad-color-fn
  "Gives the function that colours a quad.

  Parameters:

  - `panel-mesh` (map): the mesh - `:tones`, and `:colors` under a mapped `:color` (see [[gridded-mesh]]).
  - `color`: the fixed `:color` option, `nil` - none.
  - `scale` (map): the plot's colour scale, `nil` - none.
  - `na`: the NA colour.

  Returns a function of a quad's node indices that gives its colour, or `nil` for a quad left out: with `:colors`, the scale's attribute of [[quad-value]] (`na` without a scale); else [[tone-color]] of the corners' mean tone."
  [{:keys [tones colors]} color scale na]
  (cond
    (and colors scale) (let [attribute (aesthetic/attribute-fn :color scale na)]
                         (fn [ids] (attribute (quad-value scale (map colors ids)))))
    colors (constantly na)
    :else (let [tone-of (tone-color color scale na)]
            (fn [ids] (tone-of (m// (v/sum (map tones ids)) 4.0))))))

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

  Returns a `:polygon` per quad whose four nodes are present, at its corners' mean depth, coloured by its corners' mean tone or mapped values ([[quad-color-fn]]; a quad whose colour is outside the scale is left out): `:wireframe` - outlined in its colour over the background; `:both` - filled, shaded and outlined by `:stroke-color`; `:surface` - filled and shaded, an opaque fill outlined in itself ([[seam-width]])."
  [_chart {:keys [domains frame style shading] panel-mesh :mesh} point scales env]
  (let [{:keys [nu nv nodes]} panel-mesh
        shade (space/shader shading)
        {:keys [view aspect]} frame
        normalize (space/normalizer domains aspect)
        rotate (space/rotation view)
        color-of (quad-color-fn panel-mesh (:color style) (:color scales) (get-in env [:theme :palette :na]))
        look (:surface-style style)
        width (:stroke-width style)
        back (background env)]
    (for [j (range (m/long-dec nv)) i (range (m/long-dec nu))
          :let [ids (quad-corners nu i j)
                ps (mapv nodes ids)]
          :when (every? some? ps)
          :let [col (color-of ids)]
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

(defn legend
  "Draws the surface's key glyph in a legend key box.

  Parameters:

  - `chart` (map): the chart value - `:kind` and `:schema` give its style (see [[cljplot2.theme/resolved-style]]).
  - `attributes` (map): the key's mapped style attributes (`:color`).
  - `canvas`: the clojure2d canvas.
  - `box` (map): the key box `{:x :y :w :h}` in pixels.
  - `env` (map): the drawing environment (`:theme`).

  Returns `nil`; draws a filled square in the key's colour, without an outline (see [[cljplot2.graphics.draw/bar-key!]])."
  [chart attributes canvas box env]
  (let [st (theme/key-style env chart attributes [:color])]
    (draw/bar-key! canvas box {:color (:color st) :stroke-width 0} false)))
