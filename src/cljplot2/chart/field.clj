(ns cljplot2.chart.field
  "Field: a raster of a 2D field - a kernel density of points, an interpolated gridded `:value` or a function of a
  point `[x y]` (see [[cljplot2.data.field]] for the sources, `:n`, `:bandwidth`, `:cut`, `:interpolation`, `:domain`) - colored
  through the plot's color scale (cartesian plots only).

  The field is sampled on a grid over the domain of every panel together; each panel pixel is inverse-projected to
  `(x, y)` and colored by the grid's bilinear value (smooth at any grid size; `##NaN` and points outside the domain
  stay transparent, as values outside an explicit color domain). Facets: a field per panel (points / gridded data of the panel). The color is computed, so `:color`
  can't be mapped (set it to `nil` in the chart's mapping when the plot maps it); its legend title is \"Density\" for
  points, the `:value` accessor's name for gridded data, none for a function (a plot `:scales {:color {:title t}}`
  wins). Position scales keep their data domains (no nice extension): the raster fills the panel.

  The whole domain is painted, low values included (as ggplot2's raster): with sparse data most of a panel takes the
  low end of the gradient - pick a palette or `:alpha` accordingly. On a `:log` axis the field is sampled (and a
  density estimated) in log10 coordinates, see [[cljplot2.data.field]].

  `:pixel-color` - a function of a point `[x y]` (a `Vec2`, see [[cljplot2.data.field]]) returning a clojure2d color (`nil` - transparent), called at every pixel's
  data point (the pixel centre): a raster without a grid, a color scale or a legend (e.g. complex domain colouring,
  see [[cljplot2.data.field/domain-color]]); the domain as for `:f` (option `:domain`, else the plot's explicit x / y
  domains); not with `:f` (`ex-info {:key :pixel-color}`); the mapping and the other field options are ignored (as
  with `:f`). It's called w x h times per panel - keep it cheap.

  Options (style defaults from theme `[:chart :field]`): `:alpha` (multiplies the colors' alpha, a fraction)."
  (:require [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [clojure2d.pixels :as p]
            [fastmath.core :as m]
            [cljplot2.data.field :as field]
            [cljplot2.data.rows :as data]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries [(schema/optional :alpha {:description "opacity multiplying the colours' alpha"} schema/fraction)])

(def schemas
  "Schemas of the field chart: `:style` (theme `[:chart :field]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into field/options)
                (into data/common-options)
                (conj (schema/optional :pixel-color {:description (str "a function of a point [x y] returning each pixel's colour (nil - transparent): a raster over "
                                                                     ":domain, no grid, colour scale or legend; not with :f")}
                                       schema/function)))
   :mapping [:maybe [:map
                     (schema/nilable :x {:description "x of the points (density) or of the gridded data"} schema/accessor)
                     (schema/nilable :y {:description "y of the points (density) or of the gridded data"} schema/accessor)
                     (schema/nilable :value {:description "the field's value at gridded (x, y) on a full lattice, interpolated (absent - a density of the points)"}
                                     schema/accessor)
                     [:color {:optional true :description "must be nil (the colour is the field's value): set it so in the chart's mapping when the plot maps :color"}
                      :nil]]]})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :field]) (select-keys options [:alpha])))

(defn value-title
  "Legend title of a field's computed value from the chart `options` and merged `mapping`: \"Density\" for points, the
  `:value` accessor's name (a keyword) for gridded data, `nil` for a function."
  [options mapping]
  (case (field/source-kind options mapping)
    :points "Density"
    :gridded (let [acc (:value mapping)] (when (keyword? acc) (name acc)))
    :function nil))

(defn computed-aesthetics
  "The field chart's computed color, titled by [[value-title]]; none with `:pixel-color`."
  [options mapping]
  (if (:pixel-color options) {} {:color (value-title options mapping)}))

(defn panel-grids
  "`{:grids {panel grid} :domain d :dropped {..}}` of a field chart's split `groups` (`{panel rows}`): each panel's
  field (see [[cljplot2.data.field/source-field]]) sampled on one grid over the union of the panels' domains; panels
  without a field left out; `:dropped` - rows without finite x / y (points and gridded sources)."
  [options mapping groups plot-scales]
  (let [kind (field/source-kind options mapping)
        fields (into {} (keep (fn [[panel rows]] (some->> (field/source-field kind options mapping rows plot-scales) (vector panel))))
                     groups)
        domain (field/union-domain (map :domain (vals fields)))
        size (field/grid-size (:n options))]
    {:grids (update-vals fields (fn [{:keys [f]}] (field/sample f domain size (field/axis-transforms plot-scales))))
     :domain domain
     :dropped (field/dropped-positions kind mapping groups)}))

(defn- domain-extents
  [[[x0 x1] [y0 y1]]]
  {:x {:kind :numeric :min (double x0) :max (double x1)}
   :y {:kind :numeric :min (double y0) :max (double y1)}})

(defn- pixel-color-stats
  "Stats of a `:pixel-color` field: the function in every panel of `groups`, extents of its domain."
  [options groups plot-scales st]
  (when (:f options)
    (schema/fail! "A field takes :pixel-color or :f, not both." {:key :pixel-color}))
  (let [domain (field/function-domain (:domain options) plot-scales)]
    {:data (update-vals groups (fn [_] {:pixel-color (:pixel-color options) :style st}))
     :extents (update-vals groups (fn [_] (domain-extents domain)))
     :padding {:x [0.0 0.0] :y [0.0 0.0]}
     :scale-defaults {:x {:nice false} :y {:nice false}}
     :dropped {:x 0 :y 0}}))

(defn compute
  "Per panel: `{:grid grid :style st}` (see [[cljplot2.data.field/sample]]); extents x / y - the sampled domain, color -
  the finite values of every panel; no padding, `:scale-defaults` without nice domains, `:dropped`.
  With `:pixel-color`: `{:pixel-color f :style st}` per panel, extents x / y - the function domain, no color."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme plot-scales]}]
  (let [st (style options theme)]
    (if (:pixel-color options)
      (pixel-color-stats options groups plot-scales st)
      (let [{:keys [grids domain dropped]} (panel-grids options mapping groups plot-scales)
            [lo hi] (field/value-range (vals grids))]
        {:data (update-vals grids (fn [g] {:grid g :style st}))
         :extents (update-vals grids (fn [_] (cond-> (domain-extents domain)
                                               lo (assoc :color {:kind :numeric :min (double lo) :max (double hi)}))))
         :padding {:x [0.0 0.0] :y [0.0 0.0]}
         :scale-defaults (when domain {:x {:nice false} :y {:nice false}})
         :dropped dropped}))))

(defn- grid-color-fn
  "Color of the grid's value at data `(x, y)` through the plot's color scale, `nil` for `##NaN` or a value outside
  the scale's domain."
  [grid scales env]
  (let [color-of (aesthetic/attribute-fn :color (:color scales) (get-in env [:theme :palette :na]))]
    (fn [[x y]] (let [v (field/grid-value grid (double x) (double y))] (when-not (m/nan? v) (color-of v))))))

(defn draw
  "Draw one panel's raster: a pixel image of the panel box (see the namespace doc); `:pixel-color` - its color at
  every pixel's data point."
  [_chart {:keys [grid pixel-color style]} canvas {:keys [w h]} scales env]
  (let [w (long (m/round (double w))) h (long (m/round (double h)))
        color-at (cond pixel-color pixel-color
                       (and grid (:color scales)) (grid-color-fn grid scales env))]
    (when (and color-at (pos? w) (pos? h))
      (let [flipped? (scale/flipped? scales)
            inverse-h (scale/inverse-fn (if flipped? (:y scales) (:x scales)))
            inverse-v (scale/inverse-fn (if flipped? (:x scales) (:y scales)))
            alpha (:alpha style)
            transparent (c/color 0 0 0 0)
            pixel-color (fn [_ x y]
                          (let [a (double (inverse-h (+ 0.5 (double x))))
                                b (double (inverse-v (+ 0.5 (double y))))]
                            (if-let [col (color-at (if flipped? (Vec2. b a) (Vec2. a b)))]
                              (let [col (c/to-color col)]
                                (if alpha (c/set-alpha col (* (double alpha) (double (c/alpha col)))) col))
                              transparent)))
            pixels (p/filter-colors-xy pixel-color (p/pixels w h))]
        (c2d/image canvas pixels 0 0 w h)))))
