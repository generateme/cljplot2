(ns cljplot2.chart.field
  "Field: a raster of a 2D field - a kernel density of points, an interpolated gridded `:value` or a function of a
  point `[x y]` (see [[cljplot2.data.field]] for the sources, `:n`, `:bandwidth`, `:cut`, `:interpolation`, `:domain`) - colored
  through the plot's color scale (cartesian plots only).

  The field is sampled on a grid over the domain of every panel together; each panel pixel is inverse-projected to
  `(x, y)` and colored by the grid's bilinear value (smooth at any grid size; `##NaN` and points outside the domain
  stay transparent, as values outside an explicit color domain). Facets: a field per panel (points / gridded data of the panel). The color is computed, so `:color`
  can't be mapped (set it to `nil` in the chart's mapping when the plot maps it); its legend title is `Density` for
  points, the `:value` accessor's name for gridded data, none for a function (a plot `:scales {:color {:title t}}`
  wins). Position scales keep their data domains (no nice extension): the raster fills the panel.

  The whole domain is painted, low values included (as ggplot2's raster): with sparse data most of a panel takes the
  low end of the gradient - pick a palette or `:alpha-factor` accordingly. On a `:log` axis the field is sampled (and a
  density estimated) in log10 coordinates, see [[cljplot2.data.field]].

  `:pixel-color` - a function of a point `[x y]` (a `Vec2`, see [[cljplot2.data.field]]) returning a clojure2d color (`nil` - transparent), called at every pixel's
  data point (the pixel centre): a raster without a grid, a color scale or a legend (e.g. complex domain colouring,
  see [[cljplot2.data.field/domain-color]]); the domain as for `:f` (option `:domain`, else the plot's explicit x / y
  domains); not with `:f` (`ex-info {:key :pixel-color}`); the mapping and the other field options are ignored (as
  with `:f`). It's called w x h times per panel - keep it cheap.

  Options (style defaults from theme `[:chart :field]`): `:alpha-factor` (multiplies the colours' alpha, a fraction).

  Functions: [[cljplot2.data.field/value-title]], [[computed-aesthetics]] - the computed color and its title; [[panel-grids]] - the panels' sampled grids; [[compute]], [[draw]] - the chart contract."
  (:require [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [clojure2d.pixels :as p]
            [fastmath.core :as m]
            [cljplot2.chart.common :as common]
            [cljplot2.data.field :as field]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.data.rows :as data]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries
  "Style entries of the field chart (theme `[:chart :field]`)."
  [(schema/optional :alpha-factor {:description "factor multiplying the colours' alpha"} schema/fraction)])

(def ^:private transparent
  "Colour of a pixel without a value."
  (c/color 0 0 0 0))

(def schemas
  "Schemas of the field chart: `:style` (theme `[:chart :field]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into field/source-options)
                (into data/common-options)
                (into theme/theme-style-entries)
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

(defn computed-aesthetics
  "Gives the field chart's computed aesthetics.

  Parameters:

  - `options` (map): the chart's options.
  - `mapping` (map): the merged mapping.

  Returns `{:color title}` - the field's value colours the raster, titled by [[cljplot2.data.field/value-title]]; `{}` with `:pixel-color` (no colour scale)."
  [options mapping]
  (if (:pixel-color options) {} {:color (field/value-title options mapping)}))

(defn- panel-grids
  "Samples every panel's field on one grid.

  Each panel's field (see [[cljplot2.data.field/source-field]]) is sampled over the union of the panels' domains, so the panels share one grid.

  Parameters:

  - `options` (map): the chart's options - the source and `:n` (see [[cljplot2.data.field]]).
  - `mapping` (map): the merged mapping.
  - `groups` (map): `{panel rows}`.
  - `plot-scales` (map): the plot's `:scales` (axis transforms, explicit domains).

  Returns `{:grids {panel grid} :domain d :dropped {..}}`: panels without a field left out; `:dropped` - rows without finite x / y (points and gridded sources), `:field` - panels with rows but no field (see [[cljplot2.data.field/panels-without-field]]).

  Throws `ex-info {:key :mapping}` when rows exist but none has a finite `:x`, `:y` (or gridded `:value`), see [[cljplot2.data.field/check-mapped!]].

  See also [[cljplot2.data.field/sample]]."
  [options mapping groups plot-scales]
  (let [kind (field/source-kind options mapping)
        _ (field/check-mapped! kind mapping groups)
        fields (into {} (keep (fn [[panel rows]] (some->> (field/source-field kind options mapping rows plot-scales) (vector panel))))
                     groups)
        domain (field/union-domain (map :domain (vals fields)))
        size (field/grid-size (:n options))]
    {:grids (update-vals fields (fn [{:keys [f]}] (field/sample f domain size (field/axis-transforms plot-scales))))
     :domain domain
     :dropped (assoc (field/dropped-positions kind mapping groups) :field (field/panels-without-field groups (set (keys fields))))}))

(defn- domain-extents
  "Gives the x / y extents of a field's domain.

  Parameters:

  - `domain`: `[[x0 x1] [y0 y1]]`.

  Returns `{:x extent :y extent}`, numeric extents of the two ranges."
  [[[x0 x1] [y0 y1]]]
  {:x {:kind :numeric :min (double x0) :max (double x1)}
   :y {:kind :numeric :min (double y0) :max (double y1)}})

(defn- pixel-color-stats
  "Computes the stats of a `:pixel-color` field.

  Parameters:

  - `options` (map): the chart's options - `:pixel-color`, `:domain`.
  - `groups` (map): `{panel rows}` - only the panels are read.
  - `plot-scales` (map): the plot's `:scales` (explicit x / y domains when `:domain` is absent).
  - `st` (map): the chart's style.

  Returns the chart stats: the function in every panel, extents of its domain (see [[cljplot2.data.field/function-domain]]), no padding, no nice domains, nothing dropped.

  Throws `ex-info {:key :pixel-color}` with `:f` too."
  [options groups plot-scales st]
  (when (:f options)
    (schema/fail! "A field takes :pixel-color or :f, not both." {:key :pixel-color}))
  (let [domain (field/function-domain (:domain options) plot-scales)]
    {:data (update-vals groups (fn [_] {:pixel-color (:pixel-color options) :style st}))
     :extents (update-vals groups (fn [_] (domain-extents domain)))
     :padding data/zero-padding
     :scale-defaults {:x {:nice false} :y {:nice false}}
     :dropped {:x 0 :y 0}}))

(defn compute
  "Computes the field chart's stats.

  Parameters:

  - `chart` (map): the chart value - `:options` (source, `:n`, `:alpha-factor`, `:pixel-color` ...).
  - `split` (map): `:groups` (`{panel rows}`), `:mapping` (merged).
  - `env` (map): `:theme`, `:plot-scales`.

  Returns per panel `{:grid grid :style st}` (see [[cljplot2.data.field/sample]]); extents x / y - the sampled domain, color - the finite values of every panel; no padding, `:scale-defaults` without nice domains, `:dropped`. With `:pixel-color` per panel `{:pixel-color f :style st}`, extents x / y - the function domain, no color (see [[pixel-color-stats]])."
  [{:keys [options] :as chart} {:keys [groups mapping]} {:keys [theme plot-scales]}]
  (let [st (theme/resolved-style theme chart)]
    (if (:pixel-color options)
      (pixel-color-stats options groups plot-scales st)
      (let [{:keys [grids domain dropped]} (panel-grids options mapping groups plot-scales)
            [lo hi] (field/value-range (vals grids))]
        {:data (update-vals grids (fn [g] {:grid g :style st}))
         :extents (update-vals grids (fn [_] (cond-> (domain-extents domain)
                                               lo (assoc :color {:kind :numeric :min (double lo) :max (double hi)}))))
         :padding data/zero-padding
         :scale-defaults (when domain {:x {:nice false} :y {:nice false}})
         :dropped dropped}))))

(defn- grid-color-fn
  "Gives the colour function of a sampled grid.

  Parameters:

  - `grid` (map): the panel's sampled grid.
  - `scales` (map): the panel's scales - `:color` colours the values.
  - `env` (map): the drawing environment (the theme's NA colour).

  Returns a function of a data point `[x y]` giving the colour of the grid's bilinear value there; `nil` for `##NaN` or a value outside the colour scale's domain."
  [grid scales env]
  (let [color-of (aesthetic/attribute-fn :color (:color scales) (get-in env [:theme :palette :na]))]
    (fn [[^double x ^double y]] (let [v (field/grid-value grid x y)] (when-not (m/nan? v) (color-of v))))))

(defn draw
  "Draws one panel's raster: a pixel image of the panel box.

  Every pixel is inverse-projected to its data point (the pixel centre) and coloured there (see the namespace doc).

  Parameters:

  - `_chart`: the chart value (unused).
  - `computed` (map): the panel's stats - `:grid` (or `:pixel-color`, a function of a point giving a colour), `:style` (`:alpha-factor`).
  - `canvas`: the panel's canvas.
  - `box` (map): the panel box, `:w` and `:h` in pixels (rounded to whole pixels).
  - `scales` (map): the panel's finalized scales.
  - `env` (map): the drawing environment.

  Draws nothing for an empty panel, or a grid without a colour scale."
  [_chart {:keys [grid pixel-color style]} canvas {:keys [^double w ^double h]} scales env]
  (let [width (m/round w)
        height (m/round h)
        color-at (cond pixel-color pixel-color
                       (and grid (:color scales)) (grid-color-fn grid scales env))]
    (when (and color-at (m/pos? width) (m/pos? height))
      (let [flipped? (scale/flipped? scales)
            inverse-h (scale/inverse-fn (if flipped? (:y scales) (:x scales)))
            inverse-v (scale/inverse-fn (if flipped? (:x scales) (:y scales)))
            alpha-factor (:alpha-factor style)
            pixel-at (fn [_ ^long x ^long y]
                       (let [a (double (inverse-h (m/+ 0.5 x)))
                             b (double (inverse-v (m/+ 0.5 y)))]
                         (if-let [col (color-at (if flipped? (Vec2. b a) (Vec2. a b)))]
                           (let [rgba (c/to-color col)]
                             (if alpha-factor (draw/faded rgba alpha-factor) rgba))
                           transparent)))
            pixels (p/filter-colors-xy pixel-at (p/pixels width height))]
        (c2d/image canvas pixels 0 0 width height)))))
