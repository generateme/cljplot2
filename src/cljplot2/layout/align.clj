(ns cljplot2.layout.align
  "Panel alignment of multiplot cells: sub-plots in one row (cells with equal box `y` and `h`) share their panel
  area's top and bottom edges; sub-plots in one column (equal box `x` and `w`) share its left and right edges.
  The panel area of a sub-plot is the bounding box of its main `:panel` items (plot margin panels left out, so plots
  with and without margins align on their main panels); sub-plots without panels (e.g. nested
  multiplots) and iso sub-plots (they centre their panels themselves) take no part. Used by the pipeline's `:subplots` stage for `:arrange` (on unless `:align false`)."
  (:require [cljplot2.layout.grid :as grid]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:const max-alignment-rounds
  "Upper bound on rebuilds while aligning (a changed panel size may change tick labels and axis sizes)."
  4)

(def ^:private sides [:left :top :right :bottom])

(defn panel-insets
  "Distances `{:left :top :right :bottom}` from the edges of built plot `built` to its panel area, or `nil` without
  `:panel` items."
  [built]
  ;; main panels only: plot margins sit outside the aligned panel area
  (when-let [boxes (seq (map :box (grid/content-items (:grid built) (:layout built)
                                                      #(and (= :panel (:kind %)) (nil? (:margin %))))))]
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

(defn align-panels
  "Align the panels of built sub-plots `cells` (`[{:id :box :built}]`, boxes in the outer layout) in rows and
  columns. `rebuild` - `(fn [cell padding] built)` builds the cell's plot with `:alignment-padding padding` (pixels
  added to its margins per side). Repeats until paddings stop changing, at most [[max-alignment-rounds]] times.
  Returns `{id built}` for all `cells`."
  [cells rebuild]
  (let [panelled (filterv #(and (panel-insets (:built %)) (not= :iso (get-in % [:built :aspect]))) cells)]
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
