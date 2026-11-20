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
  `:gap` - the space between cells, a fraction of a cell; `:shape` `:square` or `:circle`; `:border-color`,
  `:border-width`, `:pattern`, `:pattern-color`. The legend key takes the cell shape."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [cljplot2.data.group :as group]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries
  (into [(schema/optional :gap {:description "space between cells, a fraction of a cell"} [:and schema/finite [:>= 0] [:< 1]])
         (schema/optional :shape {:description "cell shape: :square or :circle"} [:enum :square :circle])
         (schema/optional :border-color {:description "cell border colour"} schema/color)
         (schema/optional :border-width {:description "cell border width in pixels"} schema/non-neg)]
        draw/pattern-style-entries))

(def ^:const max-cells
  "Most cells a waffle panel draws; more (a small `:unit`, a large grid) throw - cells would shrink below a pixel."
  10000)

(def schemas
  "Schemas of the waffle: `:style` (theme `[:chart :waffle]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (conj (schema/optional :rows {:description "grid rows (ignored with :unit)" :default 10} [:int {:min 1 :max max-cells}])
                      (schema/optional :columns {:description "grid columns" :default 10} [:int {:min 1 :max max-cells}])
                      (schema/optional :unit {:description "the value of one cell: a category gets value / unit cells, the grid :columns wide"
                                              :cljplot2/default-doc "none - the categories share the :rows x :columns grid"}
                                       [:and schema/finite [:> 0]])
                      (schema/optional :fill-order {:description "fill from the bottom-left cell: :rows (row by row, upwards) or :columns (column by column)"
                                                    :default :rows}
                                       [:enum :rows :columns])))
   :mapping [:map
             [:color {:description "the category: one colour and a share of the cells per category"} schema/accessor]
             (schema/nilable :value {:description "weight per row, a non-negative finite number (other rows are dropped)"
                                     :cljplot2/default-doc "every row counts 1"}
                             schema/accessor)
             (schema/nilable :pattern {:description "fill pattern, splitting categories too"} schema/accessor)]})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :waffle]) (select-keys options [:gap :shape :border-color :border-width :pattern :pattern-color])))

(defn largest-remainder
  "Whole counts summing to `n` (a non-negative integer) in proportion to the non-negative `weights` (largest-remainder
  rounding: the floors of the exact shares, then one more for the largest remainders, earlier weights first on
  ties). All zeros when the weights sum to 0."
  [weights n]
  (let [total (double (reduce + 0.0 weights))
        n (long n)]
    (if-not (pos? total)
      (vec (repeat (count weights) 0))
      (let [exact (mapv #(/ (* (double %) n) total) weights)
            floors (mapv #(long (m/floor (double %))) exact)
            missing (- n (long (reduce + 0 floors)))
            extra (set (take missing (sort-by (fn [i] (- (double (exact i)) (double (floors i)))) #(compare %2 %1)
                                              (range (count weights)))))]
        (vec (map-indexed (fn [i f] (if (extra i) (inc (long f)) f)) floors))))))

(defn- cell-counts
  "`[counts columns rows]` of a panel's category `weights` for the options (see the namespace doc). Throws
  `ex-info {:key :unit}` (`{:key :rows}` for a grid) for more than [[max-cells]] cells."
  [weights {:keys [rows columns unit]}]
  (if unit
    (let [cells (m/round (/ (double (reduce + 0.0 weights)) (double unit)))]
      (when (> cells max-cells)
        (schema/fail! (str "A waffle of " cells " cells (at most " max-cells "): raise :unit.") {:key :unit :value unit}))
      [(largest-remainder weights cells) columns (max 1 (long (m/ceil (/ (double cells) (double columns)))))])
    (let [cells (* (long rows) (long columns))]
      (when (> cells max-cells)
        (schema/fail! (str "A waffle of " cells " cells (at most " max-cells "): use fewer :rows / :columns.")
                      {:key :rows :value rows}))
      [(largest-remainder weights cells) columns rows])))

(defn- value-ok? [v] (and (number? v) (m/valid-double? (double v)) (>= (double v) 0.0)))

(defn compute
  "Per panel: `{:categories [{:aesthetics a :cells n} ..] :columns c :rows r :fill-order o :style st}`; aesthetic
  extents, `:na`, `:dropped` (`:value` - rows with an invalid weight)."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme plot-scales] :as env}]
  (let [value-of (when (some? (:value mapping)) (data/row-accessor :value (:value mapping)))
        kept (if value-of (update-vals groups #(filterv (comp value-ok? value-of) %)) groups)
        {:keys [panels dropped]} (group/split kept mapping env)
        opts (merge {:rows 10 :columns 10 :fill-order :rows} (select-keys options [:rows :columns :unit :fill-order]))
        st (style options theme)
        weight (fn [rows] (if value-of (reduce + 0.0 (map #(double (value-of %)) rows)) (double (count rows))))]
    {:data (update-vals panels (fn [gs]
                                 (let [[counts columns rows] (cell-counts (mapv (comp weight :rows) gs) opts)]
                                   {:categories (mapv (fn [g n] {:aesthetics (:aesthetics g) :cells n}) gs counts)
                                    :columns columns :rows rows :fill-order (:fill-order opts) :style st})))
     :extents (group/aesthetic-extents panels plot-scales)
     :na (group/na-dims panels)
     :dropped (cond-> dropped
                value-of (assoc :value (- (count (mapcat val groups)) (count (mapcat val kept)))))}))

;; drawing

(defn cell-position
  "`[column row]` of the `k`-th cell (0-based, row 0 at the bottom) in a `columns` x `rows` grid filled in
  `fill-order`."
  [k columns rows fill-order]
  (let [k (long k)]
    (if (= fill-order :columns)
      [(quot k (long rows)) (rem k (long rows))]
      [(rem k (long columns)) (quot k (long columns))])))

(defn- cell!
  "Draw one cell over the pixel square `x0..x1` x `y0..y1` (whole pixels) in style `st` with `fill`."
  [canvas x0 y0 x1 y1 fill st pattern-fill]
  (let [x0 (long x0) y0 (long y0) w (- (long x1) x0) h (- (long y1) y0)
        circle? (= :circle (:shape st))
        shape! (if circle?
                 #(c2d/ellipse canvas (+ x0 (* 0.5 w)) (+ y0 (* 0.5 h)) w h %)
                 #(c2d/rect canvas x0 y0 w h %))
        sw (double (:border-width st))]
    (when (and (pos? w) (pos? h))
      (c2d/set-color canvas fill)
      (shape! false)
      (draw/fill-with-pattern! canvas pattern-fill #(shape! false))
      (when (and (pos? sw) (not (theme/none? (:border-color st))))
        (c2d/set-stroke-custom canvas {:size sw})
        (c2d/set-color canvas (:border-color st))
        (shape! true)))))

(defn draw
  "Draw one panel's grid: categories in order, cell by cell from the bottom-left."
  [_chart {:keys [categories columns rows fill-order style]} canvas {:keys [w h]} scales env]
  (let [columns (long columns) rows (long rows)
        side (m/min (/ (double w) columns) (/ (double h) rows))
        left (* 0.5 (- (double w) (* side columns)))
        top (* 0.5 (- (double h) (* side rows)))
        inset (* 0.5 side (double (:gap style)))
        pattern-fill (draw/pattern-fill-cache)
        starts (reductions + 0 (map :cells categories))]
    (doseq [[{:keys [aesthetics cells]} start] (map vector categories starts)
            :let [st (group/style style aesthetics scales (:theme env))]
            :when st
            k (range start (+ (long start) (long cells)))
            :let [[col row] (cell-position k columns rows fill-order)
                  x (+ left (* side (double col)))
                  y (+ top (* side (double (- rows 1 (long row)))))]]
      (cell! canvas (m/round (+ x inset)) (m/round (+ y inset)) (m/round (- (+ x side) inset)) (m/round (- (+ y side) inset))
             (:color st) st (pattern-fill st (:color st))))))

(defn legend
  "Key glyph: a filled square or circle (the cell `:shape`) inset in `box`, with the chart's `:pattern`."
  [{:keys [options]} attributes canvas {:keys [x y w h] :as box} env]
  (let [st (style options (:theme env))]
    (if (= :circle (:shape st))
      (let [d (* 0.7 (m/min (double w) (double h)))
            cx (+ (double x) (* 0.5 (double w))) cy (+ (double y) (* 0.5 (double h)))]
        (c2d/set-color canvas (:color attributes))
        (c2d/ellipse canvas cx cy d d)
        (draw/fill-with-pattern! canvas (draw/pattern-fill st (:color attributes)) #(c2d/ellipse canvas cx cy d d)))
      (draw/bar-key! canvas box (assoc (select-keys st [:pattern :pattern-color]) :color (:color attributes) :stroke-width 0)
                     false))))
