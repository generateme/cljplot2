(ns cljplot2.multiplot
  "Multiplots: plots arranged in a grid, each drawn as a whole plot in its cell (see [[cljplot2.subplot]]).

  Panel alignment ([[align]], on by default in [[arrange]], `:align false` turns it off): sub-plots in one row (cells with equal box `y` and `h`)
  share their panel area's top and bottom edges; sub-plots in one column (equal box `x` and `w`) share its left and
  right edges. The panel area of a sub-plot is the bounding box of its `:panel` items; sub-plots without panels (e.g.
  nested multiplots) take no part."
  (:require [cljplot2.grid :as grid]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn arrange
  "Plot map arranging `plots` in a grid of cells filled row by row: `{:arrange {:plots plots ...}}`.

  `options`:

  * `:columns`, `:rows` - grid shape (default one column; one given, the other computed)
  * `:widths`, `:heights` - relative column / row weights (default equal)
  * `:align` - aligns the panels of sub-plots in rows and columns (default true; `false` - each sub-plot laid out
    on its own), see [[align]]
  * `:title` - overall title above the cells (`:labels {:title t}`)
  * `:theme` - the multiplot's theme: background, title style, `[:multiplot :gap]` between cells, `[:plot :margin]`
    around them; sub-plots keep their own themes

  The grid is generated at build time from the merged theme ([[cljplot2.plot-grid/arrange-grid]], cells `[:plot i]`),
  where the spec is validated (`ex-info {:key :arrange}`)."
  ([plots] (arrange plots {}))
  ([plots {:keys [title theme] :as options}]
   (cond-> {:arrange (assoc (select-keys options [:columns :rows :widths :heights :align]) :plots plots)}
     theme (assoc :theme theme)
     (some? title) (assoc :labels {:title title}))))

(def ^:const max-alignment-rounds
  "Upper bound on rebuilds while aligning (a changed panel size may change tick labels and axis sizes)."
  4)

(def ^:private sides [:left :top :right :bottom])

(defn- panel-insets
  "Distances `{:left :top :right :bottom}` from the edges of built plot `built` to its panel area, or `nil` without
  `:panel` items."
  [built]
  (when-let [boxes (seq (map :box (grid/content-items (:grid built) (:layout built) #(= :panel (:kind %)))))]
    (let [{:keys [width height]} (:options built)
          x0 (reduce min (map :x boxes)) y0 (reduce min (map :y boxes))
          x1 (reduce max (map #(+ (long (:x %)) (long (:w %))) boxes))
          y1 (reduce max (map #(+ (long (:y %)) (long (:h %))) boxes))]
      {:left x0 :top y0 :right (- (long width) (long x1)) :bottom (- (long height) (long y1))})))

(defn- paddings
  "Padding per cell id making panel insets equal within rows / columns: per side, the largest unpadded inset of the
  cell's group minus the cell's own. `cells` - `{:id :box :built :padding}` with a panel."
  [cells]
  (let [natural (into {} (map (fn [{:keys [id built padding]}]
                                [id (merge-with - (panel-insets built) (merge (zipmap sides (repeat 0)) padding))]))
                      cells)
        group-of {:left (fn [{:keys [box]}] [(:x box) (:w box)]) :right (fn [{:keys [box]}] [(:x box) (:w box)])
                  :top (fn [{:keys [box]}] [(:y box) (:h box)]) :bottom (fn [{:keys [box]}] [(:y box) (:h box)])}
        target (fn [side cell]
                 (let [k ((group-of side) cell)]
                   (reduce max (map #(get-in natural [(:id %) side]) (filter #(= k ((group-of side) %)) cells)))))]
    (into {} (map (fn [{:keys [id] :as cell}]
                    [id (into {} (map (fn [side] [side (- (long (target side cell)) (long (get-in natural [id side])))]))
                              sides)])
                  cells))))

(defn align
  "Align the panels of built sub-plots `cells` (`[{:id :box :built}]`, boxes in the outer layout) in rows and
  columns. `rebuild` - `(fn [cell padding] built)` builds the cell's plot with `:alignment-padding padding` (pixels
  added to its margins per side). Repeats until paddings stop changing, at most [[max-alignment-rounds]] times.
  Returns `{id built}` for all `cells`."
  [cells rebuild]
  (let [panelled (filterv #(panel-insets (:built %)) cells)]
    (loop [round 1
           current (mapv #(assoc % :padding {}) panelled)]
      (let [wanted (paddings current)
            next-cells (mapv (fn [{:keys [id padding] :as cell}]
                               (let [p (wanted id)]
                                 (if (= p (merge (zipmap sides (repeat 0)) padding))
                                   cell
                                   (assoc cell :padding p :built (rebuild cell p)))))
                             current)]
        (if (or (= (map :padding current) (map :padding next-cells)) (>= round max-alignment-rounds))
          (merge (into {} (map (juxt :id :built)) cells) (into {} (map (juxt :id :built)) next-cells))
          (recur (inc round) next-cells))))))
