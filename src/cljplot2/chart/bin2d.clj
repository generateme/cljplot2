(ns cljplot2.chart.bin2d
  "Bin2d: points counted in grid cells - a hexbin (default) or a 2D histogram (`:cell :square`), cartesian plots only.

  Mapping: `:x`, `:y` - numbers (rows with another value are dropped and counted). The cell color is the count
  measure, so `:color` can't be mapped (set it to `nil` in the chart's mapping when the plot maps it); groups -
  facets only. All panels share one grid (equal cells), binned in the normalized space of
  [[cljplot2.data.cells]]: cells are regular in pixels with plot `:aspect :iso` or a `:shape` matching the panel
  (height / width), close to regular otherwise (the shape is estimated from the plot size and the facet layout).

  Options (style defaults from theme `[:chart :bin2d]`):

  * `:cell` - a `fastmath.grid` cell type (default `:pointy-hex`)
  * `:bins` - `:scott` (default, fastmath's normal-reference estimate - coarse for a few hundred points or
    multimodal data, e.g. about 5 cells across Old Faithful's 272 eruptions) or a positive integer: cells across the
    x range (20-30 is a common choice)
  * `:binwidth` - cell size in x data units (wins over `:bins`)
  * `:shape` - the normalized space's height / width ratio (default: estimated, see above)
  * `:measure` - the color: `:count` (default), `:percent` (of the panel's points), `:pmf` (share of the panel's
    points), `:density` (count / (n * cell area in data units)); the legend title names it (a plot `:scales {:color
    {:title t}}` wins)
  * `:cell-scale` (shrink factor, `(0 1]`), `:stroke-color` (a color, `:none` or `:darker`), `:stroke-width`,
    `:alpha` (fill alpha, a fraction)

  Only occupied cells are drawn."
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
                (conj (schema/optional :bins [:or [:= :scott] schema/pos-int])
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
  cells): x / y - the cells' box in the normalized space's proportions (see [[cljplot2.data.cells/bin-points]]; the
  plain box when either plot scale isn't linear, e.g. `:log` - there cells aren't regular anyway),
  color - the values over all panels; no padding, `:scale-defaults` without nice domains (keeps the
  cells' shape), `:dropped` (`:x`, `:y` - rows without a finite number)."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme plot-scales] :as env}]
  (let [st (style options theme)
        measure (get options :measure :count)
        x-of (data/row-accessor :x (:x mapping))
        y-of (data/row-accessor :y (:y mapping))
        pairs (update-vals groups (fn [rows] (mapv (fn [row] [(x-of row) (y-of row)]) rows)))
        all (apply concat (vals pairs))
        points (update-vals pairs (fn [ps] (filterv (fn [[x y]] (and (finite? x) (finite? y))) ps)))
        {:keys [panels area extent cells-extent]} (cells/bin-points points (merge {:cell (:cell st) :bins :scott}
                                                              (select-keys options [:bins :binwidth :shape]))
                                                env)
        valued (update-vals panels (fn [cs]
                                     (let [counts (mapv :count cs)
                                           values (measure/heights measure counts (repeat (count cs) area))]
                                       (mapv (fn [c value] {:corners (:corners c) :value value}) cs values))))
        color-extent (aesthetic/value-extent :color (get plot-scales :color) (map :value (mapcat val valued)))
        linear? (every? #(contains? #{nil :linear} (get-in plot-scales [% :kind])) [:x :y])
        extent (if linear? extent cells-extent)
        position-extent (fn [[lo hi]] {:kind :numeric :min (double lo) :max (double hi)})]
    {:data (update-vals valued (fn [cs] {:cells cs :style st}))
     :extents (update-vals valued (fn [cs]
                                    (when (seq cs)
                                      {:x (position-extent (:x extent)) :y (position-extent (:y extent)) :color color-extent})))
     :padding {:x [0.0 0.0] :y [0.0 0.0]}
     :scale-defaults (when extent {:x {:nice false} :y {:nice false}})
     :dropped {:x (count (remove (comp finite? first) all))
               :y (count (remove (comp finite? second) all))}}))

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
