(ns cljplot2.chart.field
  "Field: a raster of a 2D field - a kernel density of points, an interpolated gridded `:value` or a function of `x`,
  `y` (see [[cljplot2.data.field]] for the sources, `:n`, `:bandwidth`, `:cut`, `:interpolation`, `:domain`) - colored
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

  Options (style defaults from theme `[:chart :field]`): `:alpha` (multiplies the colors' alpha, a fraction)."
  (:require [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [clojure2d.pixels :as p]
            [fastmath.core :as m]
            [cljplot2.data.field :as field]
            [cljplot2.data.rows :as data]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries [(schema/optional :alpha schema/fraction)])

(def schemas
  "Schemas of the field chart: `:style` (theme `[:chart :field]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into field/options)
                (into data/common-options))
   :mapping [:maybe [:map [:color {:optional true} :nil]]]})

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
  "The field chart's computed color, titled by [[value-title]]."
  [options mapping]
  {:color (value-title options mapping)})

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

(defn compute
  "Per panel: `{:grid grid :style st}` (see [[cljplot2.data.field/sample]]); extents x / y - the sampled domain, color -
  the finite values of every panel; no padding, `:scale-defaults` without nice domains, `:dropped`."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme plot-scales]}]
  (let [{:keys [grids domain dropped]} (panel-grids options mapping groups plot-scales)
        st (style options theme)
        [lo hi] (field/value-range (vals grids))
        [[x0 x1] [y0 y1]] domain]
    {:data (update-vals grids (fn [g] {:grid g :style st}))
     :extents (update-vals grids (fn [_] (cond-> {:x {:kind :numeric :min (double x0) :max (double x1)}
                                                  :y {:kind :numeric :min (double y0) :max (double y1)}}
                                           lo (assoc :color {:kind :numeric :min (double lo) :max (double hi)}))))
     :padding {:x [0.0 0.0] :y [0.0 0.0]}
     :scale-defaults (when domain {:x {:nice false} :y {:nice false}})
     :dropped dropped}))

(defn draw
  "Draw one panel's raster: a pixel image of the panel box (see the namespace doc)."
  [_chart {:keys [grid style]} canvas {:keys [w h]} scales env]
  (let [w (long (m/round (double w))) h (long (m/round (double h)))]
    (when (and grid (pos? w) (pos? h) (:color scales))
      (let [flipped? (scale/flipped? scales)
            inverse-h (scale/inverse-fn (if flipped? (:y scales) (:x scales)))
            inverse-v (scale/inverse-fn (if flipped? (:x scales) (:y scales)))
            color-of (aesthetic/attribute-fn :color (:color scales) (get-in env [:theme :palette :na]))
            alpha (:alpha style)
            transparent (c/color 0 0 0 0)
            pixel-color (fn [_ x y]
                          (let [a (double (inverse-h (+ 0.5 (double x))))
                                b (double (inverse-v (+ 0.5 (double y))))
                                v (if flipped? (field/grid-value grid b a) (field/grid-value grid a b))]
                            (if-let [col (when-not (m/nan? v) (color-of v))]
                              (let [col (c/to-color col)]
                                (if alpha (c/set-alpha col (* (double alpha) (double (c/alpha col)))) col))
                              transparent)))
            pixels (p/filter-colors-xy pixel-color (p/pixels w h))]
        (c2d/image canvas pixels 0 0 w h)))))
