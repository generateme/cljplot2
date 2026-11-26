(ns cljplot2.chart.waffle
  "Waffle: a grid of cells split among categories by their share of a total (a panel without axes, cartesian plots
  only).

  Mapping: `:color` - the category (an ordinal aesthetic: one color per category, legend keys; other grouping
  aesthetics such as `:pattern` split categories too); optional `:value` - a weight per row (a non-negative finite
  number; other rows are dropped and counted), without it every row counts 1. Groups and subgroups: facets, a waffle
  per panel.

  Cells: by default a `:rows` x `:columns` grid (10 x 10 - a percent waffle, a cell per percent); each category gets
  its share of the panel's total, rounded by largest remainder so the cells always fill the grid. With `:unit` (the
  value of one cell) each category gets `value / unit` cells (largest remainder of the total over the unit), the grid
  `:columns` wide and as many rows as needed (`:rows` ignored); more than 10000 cells in a panel throw `ex-info` (`{:key :unit}`, `{:key :rows}` for a
  larger grid). Categories fill the grid in their level order, from
  the bottom-left cell: `:fill-order` `:rows` (default; along the bottom row, then up) or `:columns` (up the first
  column, then right).

  Drawing (style defaults from theme `[:chart :waffle]`): square cells as large as the panel allows, the grid centred;
  `:gap` - the space between cells, a fraction of a cell; `:cell-shape` `:square` or `:circle`; `:stroke-color`,
  `:stroke-width`, `:pattern`, `:pattern-color`. The legend key takes the cell shape.

  Functions: [[compute]], [[draw]], [[legend]] - the chart contract; [[largest-remainder]] - whole counts in proportion to weights; [[cell-position]] - a cell's place in the grid."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.chart.legend :as chart-legend]
            [cljplot2.chart.common :as common]
            [cljplot2.data.group :as group]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries
  "Map entries of the waffle style (theme `[:chart :waffle]`, also options)."
  (into [(schema/optional :gap {:description "The room between cells, as a fraction of a cell." :cljplot2/example 0.4} [:and schema/finite [:>= 0] [:< 1]])
         (schema/optional :cell-shape {:description "The shape of every cell." :cljplot2/value-meanings {:square "A square." :circle "A circle."}} [:enum :square :circle])
         (schema/optional :stroke-color {:description "The outline colour of the cells." :cljplot2/value-meanings {:none "No outline."} :cljplot2/example :black} schema/color)
         (schema/optional :stroke-width {:description "The outline width of the cells in pixels; `0` draws no outline." :cljplot2/example 3
                                         :cljplot2/example-context {:stroke-color :black}}
                          schema/non-neg)]
        draw/pattern-style-entries))

(def ^{:private true :const true :tag 'long} max-cells
  "Most cells a waffle panel draws; more (a small `:unit`, a large grid) throw - cells would shrink below a pixel."
  10000)
(def schemas
  "Schemas of the waffle: `:style` (theme `[:chart :waffle]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into theme/theme-style-entries)
                (conj (schema/optional :rows {:description "The number of rows of the grid; `:unit` ignores it." :default 10 :cljplot2/example 5} [:int {:min 1 :max max-cells}])
                      (schema/optional :columns {:description "The number of columns of the grid." :default 10 :cljplot2/example 5} [:int {:min 1 :max max-cells}])
                      (schema/optional :unit {:description "The value one cell stands for: a category gets about its value divided by `:unit` cells, on a grid `:columns` wide; a unit above every value draws no cells."
                                              :cljplot2/default-doc "None: the categories share the `:rows` by `:columns` grid." :cljplot2/example 2}
                                       schema/finite-pos)
                      (schema/optional :fill-order {:description "How the chart fills the grid from the bottom-left cell."
                                                    :default :rows
                                                    :cljplot2/value-meanings {:rows "Row by row, upwards." :columns "Column by column."}}
                                       [:enum :rows :columns])))
   :mapping [:map
             [:color {:description "Splits the rows into categories, each with its own colour and its share of the cells."} schema/accessor]
             (schema/nilable :value {:description "The weight of each row, a non-negative finite number; the chart drops other rows."
                                     :cljplot2/default-doc "Every row counts 1."}
                             schema/accessor)
             (schema/nilable :pattern {:description "Splits the rows into groups, each with its own fill pattern; a category with two patterns takes two shares of the cells."} schema/accessor)]})

(defn largest-remainder
  "Gives whole counts summing to `n` in proportion to weights, by largest-remainder rounding.

  The floors of the exact shares come first, then one more for the largest remainders, earlier weights first on ties.

  Parameters:

  - `weights` (vector of non-negative numbers): the weights.
  - `n` (long): the total, a non-negative integer.

  Returns a vector of longs, one per weight; all zeros when the weights sum to 0."
  [weights ^long n]
  (let [total (v/sum weights)]
    (if-not (m/pos? total)
      (vec (repeat (count weights) 0))
      (let [exact (mapv #(m// (m/* (double %) n) total) weights)
            floors (mapv #(long (m/floor (double %))) exact)
            missing (m/long-sub n (long (reduce m/long-add 0 floors)))
            extra (set (take missing (sort-by (fn [i] (m/- (double (exact i)) (double (floors i)))) #(compare %2 %1)
                                              (range (count weights)))))]
        (vec (map-indexed (fn [i ^long f] (if (extra i) (m/long-inc f) f)) floors))))))

(defn- cell-counts
  "Gives a panel's cell counts and grid size.

  Parameters:

  - `weights` (vector of numbers): the categories' weights.
  - `options` (map): `:rows`, `:columns` - the grid; `:unit` - the value of one cell (`nil` - the categories share the grid).

  Returns `[counts columns rows]`: with `:unit` each category `weight / unit` cells (largest remainder of the total over the unit), `:columns` wide and as many rows as needed; else the categories' shares of `rows x columns` (see [[largest-remainder]]).

  Throws `ex-info {:key :unit}` (`{:key :rows}` for a grid) for more than [[max-cells]] cells."
  [weights {:keys [rows columns unit]}]
  (if unit
    (let [cells (m/round (m// (v/sum weights) (double unit)))]
      (when (m/> cells max-cells)
        (schema/fail! (str "A waffle of " cells " cells (at most " max-cells "): raise :unit.") {:key :unit :value unit}))
      [(largest-remainder weights cells) columns (m/long-max 1 (m/ceil (m// (double cells) (double columns))))])
    (let [cells (m/* (long rows) (long columns))]
      (when (m/> cells max-cells)
        (schema/fail! (str "A waffle of " cells " cells (at most " max-cells "): use fewer :rows / :columns.")
                      {:key :rows :value rows}))
      [(largest-remainder weights cells) columns rows])))

(defn- value-ok?
  "Tests whether a row weight is valid.

  Parameters:

  - `v`: the weight.

  Returns a truthy value for a non-negative finite number."
  [v]
  (and (schema/finite? v) (m/>= (double v) 0.0)))

(defn compute
  "Computes the waffle's cells per panel.

  Parameters:

  - `chart` (map): the chart value (`:options` - `:rows`, `:columns`, `:unit`, `:fill-order`).
  - `chart-split` (map): `:groups` - panel -> rows, `:mapping`.
  - `env` (map): the compute environment (`:theme`, `:plot-scales` ...).

  Returns per panel `{:categories [{:aesthetics a :cells n} ..] :columns c :rows r :fill-order o :style st}`; aesthetic extents, `:na`, `:dropped` (`:value` - rows with an invalid weight).

  Throws as [[cell-counts]]; `ex-info {:key :mapping}` when rows exist but none has a finite mapped `:value`."
  [{:keys [options] :as chart} {:keys [groups mapping]} {:keys [theme plot-scales] :as env}]
  (let [value-of (when (some? (:value mapping)) (data/row-accessor :value (:value mapping)))
        _ (when value-of (data/check-mapped! (mapcat val groups) [[:value value-of]] (fn [_ v] (schema/finite? v))))
        kept (if value-of (update-vals groups #(filterv (comp value-ok? value-of) %)) groups)
        {:keys [panels dropped]} (group/split kept mapping env)
        opts (data/options-with-defaults {:rows 10 :columns 10 :unit nil :fill-order :rows} options)
        st (theme/resolved-style theme chart)
        weight (fn [group-rows] (if value-of (v/sum (map value-of group-rows)) (double (count group-rows))))]
    {:data (update-vals panels (fn [gs]
                                 (let [[counts columns rows] (cell-counts (mapv (comp weight :rows) gs) opts)]
                                   {:categories (mapv (fn [g n] {:aesthetics (:aesthetics g) :cells n}) gs counts)
                                    :columns columns :rows rows :fill-order (:fill-order opts) :style st})))
     :extents (group/aesthetic-extents panels plot-scales)
     :na (group/na-dims panels)
     :dropped (cond-> dropped
                value-of (assoc :value (m/long-sub (count (mapcat val groups)) (count (mapcat val kept)))))}))

;; drawing

(defn cell-position
  "Gives the grid position of a cell.

  Parameters:

  - `k` (long): the cell's index, from 0.
  - `columns`, `rows` (longs): the grid.
  - `fill-order`: `:columns` (up the first column, then right) or else row by row from the bottom.

  Returns `[column row]`, row 0 at the bottom."
  [^long k ^long columns ^long rows fill-order]
  (if (= fill-order :columns)
    [(m/long-quot k rows) (m/long-rem k rows)]
    [(m/long-rem k columns) (m/long-quot k columns)]))

(defn- cell!
  "Draws one cell over a pixel square.

  Parameters:

  - `canvas`: the clojure2d canvas.
  - `[x0 y0 x1 y1]` (longs): the square's whole-pixel corners.
  - `fill`: the cell colour.
  - `st` (map): the cell style - `:cell-shape`, `:stroke-color`, `:stroke-width`.
  - `pattern-fill`: the cell's pattern fill (see [[cljplot2.graphics.draw/pattern-fill]]).

  Nothing is drawn for an empty square; the border only for a positive width and a colour."
  [canvas [^long x0 ^long y0 ^long x1 ^long y1] fill st pattern-fill]
  (let [w (m/long-sub x1 x0)
        h (m/long-sub y1 y0)
        circle? (= :circle (:cell-shape st))
        shape! (if circle?
                 (fn [c stroke?] (c2d/ellipse c (m/+ x0 (m/* 0.5 w)) (m/+ y0 (m/* 0.5 h)) w h stroke?))
                 (fn [c stroke?] (c2d/rect c x0 y0 w h stroke?)))
        sw (double (:stroke-width st))]
    (when (and (m/pos? w) (m/pos? h))
      (draw/fill-patterned! canvas fill pattern-fill #(shape! % false))
      (draw/outline! canvas (:stroke-color st) sw #(shape! % true)))))

(defn draw
  "Draws one panel's grid: the categories in order, cell by cell from the bottom-left.

  Parameters:

  - `_chart` (map): the chart value.
  - `computed` (map): the panel's `:categories`, `:columns`, `:rows`, `:fill-order`, `:style`.
  - `canvas`: the clojure2d canvas.
  - `box` (map): the panel's `:w`, `:h`.
  - `scales` (map): the plot's scales (aesthetics).
  - `env` (map): the drawing environment (`:theme`).

  Cells are square, as large as the panel allows, the grid centred, `:gap` (a fraction of a cell) between cells; a category outside its scale isn't drawn."
  [_chart {:keys [categories ^long columns ^long rows fill-order style]} canvas {:keys [^double w ^double h]} scales env]
  (let [side (m/min (m// w columns) (m// h rows))
        left (m/* 0.5 (m/- w (m/* side columns)))
        top (m/* 0.5 (m/- h (m/* side rows)))
        inset (m/* 0.5 side (double (:gap style)))
        pattern-fill (draw/pattern-fill-cache)
        starts (reductions m/long-add 0 (map :cells categories))]
    (doseq [[{:keys [aesthetics ^long cells]} ^long start] (map vector categories starts)
            :let [st (group/style style aesthetics scales (:theme env))]
            :when st
            k (range start (m/long-add start cells))
            :let [[^long col ^long row] (cell-position k columns rows fill-order)
                  x (m/+ left (m/* side col))
                  y (m/+ top (m/* side (m/long-sub (m/long-dec rows) row)))]]
      (cell! canvas [(m/round (m/+ x inset)) (m/round (m/+ y inset)) (m/round (m/- (m/+ x side) inset)) (m/round (m/- (m/+ y side) inset))]
             (:color st) st (pattern-fill st (:color st))))))

(defn legend
  "Draws the key glyph: a filled square or circle (`:cell-shape`) inset in the key box, with the chart's `:pattern`.

  Parameters:

  - `chart` (map): the chart value.
  - `attributes` (map): the key's attributes (`:color`).
  - `canvas`: the clojure2d canvas.
  - `box` (map): the key box `:x`, `:y`, `:w`, `:h`.
  - `env` (map): the drawing environment (`:theme`)."
  [chart attributes canvas box env]
  (let [st (theme/resolved-style (:theme env) chart)]
    (if (= :circle (:cell-shape st))
      (draw/circle-key! canvas box (:color attributes) (draw/pattern-fill st (:color attributes)))
      (chart-legend/cell-legend chart attributes canvas box env))))
