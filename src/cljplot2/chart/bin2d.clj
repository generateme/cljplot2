(ns cljplot2.chart.bin2d
  "Bin2d: points counted in grid cells - a hexbin (default) or a 2D histogram (`:cell :square`), cartesian plots only.

  Mapping: `:x`, `:y` - numbers (rows with another value are dropped and counted). The cell color is the count
  measure, so `:color` can't be mapped (set it to `nil` in the chart's mapping when the plot maps it); groups -
  facets only. All panels share one grid (equal cells), binned in the normalized space of
  [[cljplot2.data.cells]]: cells are regular in pixels with plot `:aspect :iso` or a `:shape` matching the panel
  (height / width), close to regular otherwise (the shape is estimated from the plot size and the facet layout).

  Options (style defaults from theme `[:chart :bin2d]`):

  * `:cell` - a `fastmath.grid` cell type (default `:pointy-hex`)
  * `:bins` - a positive integer: cells across the x range (20-30 is a common choice), or a fastmath
    `estimate-grid-size` method (a cell area, the same for every cell type): `:scott-correlated` (default - the
    normal-reference rule taking the axes' correlation into account; about 8 cells across Old Faithful's 272
    eruptions), `:scott`, `:freedman-diaconis` (normal-reference, coarse for multimodal data), `:cross-validation`,
    `:shimazaki-shinomoto` (data-driven: adapt to the data's shape, cost a pass over the points per candidate size -
    noticeable for tens of thousands of points, and overfit coordinates recorded at a coarse precision, e.g. 80-90
    cells across the quakes' longitudes and latitudes)
  * `:binwidth` - cell size in x data units (wins over `:bins`)
  * `:shape` - the normalized space's height / width ratio (default: estimated, see above)
  * `:measure` - the color: `:count` (default), `:percent` (of the panel's points), `:pmf` (share of the panel's
    points), `:density` (count / (n * cell area in data units)); the legend title names it (a plot `:scales {:color
    {:title t}}` wins)
  * `:cell-scale` (shrink factor, `(0 1]`), `:stroke-color` (a color, `:none` or `:darker`), `:stroke-width`,
    `:alpha` (fill alpha, a fraction)

  Only occupied cells are drawn. On a `:log` axis the points are binned in log10 coordinates (cells regular on
  screen; `:binwidth` and the `:density` cell area in log10 units of that axis)."
  (:require [cljplot2.data.cells :as cells]
            [cljplot2.data.measure :as measure]
            [cljplot2.data.rows :as data]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [fastmath.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def schemas
  "Schemas of the bin2d chart: `:style` (theme `[:chart :bin2d]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] cells/style-entries)
   :options (-> [:map {:closed true}]
                (into cells/style-entries)
                (into data/common-options)
                (conj (schema/optional :bins [:or (into [:enum] cells/size-methods) schema/pos-int])
                      (schema/optional :binwidth [:and schema/finite [:> 0]])
                      (schema/optional :shape [:and schema/finite [:> 0]])
                      (schema/optional :measure [:enum :count :percent :pmf :density])))
   :mapping [:map [:x schema/accessor] [:y schema/accessor] [:color {:optional true} :nil]]})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :bin2d]) (select-keys options cells/style-keys)))

(defn- finite? [v] (and (number? v) (m/valid-double? (double v))))

(defn compute
  "Per panel: `{:cells [{:corners [[x y] ..] :value v} ..] :style st}` - occupied cells; extents (panels with
  cells): x / y - the cells' box in the normalized space's proportions (see [[cljplot2.data.cells/bin-points]]),
  color - the values over all panels; no padding, `:scale-defaults` without nice domains (keeps the
  cells' shape), `:dropped` (`:x`, `:y` - rows without a finite number, or one the axis can't place - e.g. 0 on a log
  axis)."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme plot-scales] :as env}]
  (let [st (style options theme)
        measure (get options :measure :count)
        x-of (data/row-accessor :x (:x mapping))
        y-of (data/row-accessor :y (:y mapping))
        [tx ty] [(scale/stat-transform (:x plot-scales)) (scale/stat-transform (:y plot-scales))]
        fx (:forward tx) fy (:forward ty) ix (:inverse tx) iy (:inverse ty)
        ;; positions in the axes' screen-linear coordinates (log10 on a log axis)
        transformed (fn [v f] (when (finite? v) (let [u (double (f (double v)))] (when (m/valid-double? u) u))))
        pairs (update-vals groups (fn [rows] (mapv (fn [row] [(transformed (x-of row) fx) (transformed (y-of row) fy)]) rows)))
        all (apply concat (vals pairs))
        points (update-vals pairs (fn [ps] (filterv (fn [[x y]] (and x y)) ps)))
        to-data (fn [[u v]] [(ix (double u)) (iy (double v))])
        {:keys [panels area extent]} (cells/bin-points points (merge {:cell (:cell st) :bins cells/default-size-method}
                                                              (select-keys options [:bins :binwidth :shape]))
                                                env)
        valued (update-vals panels (fn [cs]
                                     (let [counts (mapv :count cs)
                                           values (measure/heights measure counts (repeat (count cs) area))]
                                       (mapv (fn [c value] {:corners (mapv to-data (:corners c)) :value value}) cs values))))
        color-extent (aesthetic/value-extent :color (get plot-scales :color) (map :value (mapcat val valued)))
        position-extent (fn [[lo hi] inverse] {:kind :numeric :min (double (inverse lo)) :max (double (inverse hi))})]
    {:data (update-vals valued (fn [cs] {:cells cs :style st}))
     :extents (update-vals valued (fn [cs]
                                    (when (seq cs)
                                      {:x (position-extent (:x extent) ix) :y (position-extent (:y extent) iy) :color color-extent})))
     :padding {:x [0.0 0.0] :y [0.0 0.0]}
     :scale-defaults (when extent {:x {:nice false} :y {:nice false}})
     :dropped {:x (count (remove first all))
               :y (count (remove second all))}}))

(defn draw
  "Draw one panel's occupied cells."
  [_chart {:keys [cells style]} canvas _box scales env]
  (when (seq cells)
    (let [projection (scale/projection scales)
          to-pixel (fn [[x y]] (projection x y))
          color-of (aesthetic/attribute-fn :color (:color scales) (get-in env [:theme :palette :na]))]
      (doseq [{:keys [corners value]} cells
              :let [fill (color-of value)]
              :when fill]
        (cells/draw-cell! canvas to-pixel corners fill style)))))
