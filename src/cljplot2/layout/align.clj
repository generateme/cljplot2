(ns cljplot2.layout.align
  "Panel alignment of multiplot cells: sub-plots in one row (cells with equal box `y` and `h`) share their panel
  area's top and bottom edges; sub-plots in one column (equal box `x` and `w`) share its left and right edges.
  The panel area of a sub-plot is the bounding box of its main `:panel` items (plot margin panels left out, so plots
  with and without margins align on their main panels); sub-plots without panels (e.g. nested
  multiplots) and iso sub-plots (they centre their panels themselves) take no part. Used by the pipeline's `:subplots` stage for `:arrange` (on unless `:align false`).

  Functions: [[align-panels]], [[panel-insets]]."
  (:require [fastmath.core :as m]
            [cljplot2.layout.grid :as grid]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^{:private true :const true :tag 'long} max-alignment-rounds
  "Upper bound on rebuilds while aligning (a changed panel size may change tick labels and axis sizes)."
  4)

(def ^:private sides
  "The sides of a box, in inset order."
  [:left :top :right :bottom])

(defn panel-insets
  "Gives the distances from the edges of a built plot to its panel area.

  Parameters:

  - `built` (map): a built plot (`:grid`, `:layout`, `:options` `:width` `:height`).

  Returns `{:left :top :right :bottom}` (pixels) to the bounding box of the main `:panel` items (plot margin panels left out); `nil` without `:panel` items."
  [built]
  ;; main panels only: plot margins sit outside the aligned panel area
  (when-let [boxes (seq (map :box (grid/content-items (:grid built) (:layout built)
                                                      #(and (= :panel (:kind %)) (nil? (:margin %))))))]
    (let [{:keys [width height]} (:options built)
          x0 (reduce m/long-min (map :x boxes)) y0 (reduce m/long-min (map :y boxes))
          x1 (reduce m/long-max (map #(m/long-add (long (:x %)) (long (:w %))) boxes))
          y1 (reduce m/long-max (map #(m/long-add (long (:y %)) (long (:h %))) boxes))]
      {:left x0 :top y0 :right (m/long-sub (long width) (long x1)) :bottom (m/long-sub (long height) (long y1))})))

(defn- paddings
  "Gives the padding per cell making panel insets equal within rows and columns.

  Parameters:

  - `cells` (sequence): `{:id :box :built :padding}`, each with a panel.

  Returns a map cell id -> `{:left :top :right :bottom}`: per side, the largest unpadded inset of the cell's row (top, bottom) or column (left, right) minus the cell's own."
  [cells]
  (let [natural (into {} (map (fn [{:keys [id built padding]}]
                                [id (merge-with m/long-sub (panel-insets built) (merge (zipmap sides (repeat 0)) padding))]))
                      cells)
        group-of {:left (fn [{:keys [box]}] [(:x box) (:w box)]) :right (fn [{:keys [box]}] [(:x box) (:w box)])
                  :top (fn [{:keys [box]}] [(:y box) (:h box)]) :bottom (fn [{:keys [box]}] [(:y box) (:h box)])}
        target (fn [side cell]
                 (let [k ((group-of side) cell)]
                   (reduce m/long-max (map #(get-in natural [(:id %) side]) (filter #(= k ((group-of side) %)) cells)))))]
    (into {} (map (fn [{:keys [id] :as cell}]
                    [id (into {} (map (fn [side] [side (m/long-sub (long (target side cell)) (long (get-in natural [id side])))]))
                              sides)])
                  cells))))

(defn align-panels
  "Gives the built sub-plots with their panels aligned in rows and columns.

  Parameters:

  - `cells` (sequence): `[{:id :box :built}]`, boxes in the outer layout.
  - `rebuild` (fn): `(fn [cell padding] built)` builds the cell's plot with `:alignment-padding padding` (pixels added to its margins per side).

  Repeats until paddings stop changing, at most [[max-alignment-rounds]] times; cells without panels and iso cells take no part.

  Returns `{id built}` for all `cells`."
  [cells rebuild]
  (let [panelled (filter #(and (panel-insets (:built %)) (not= :iso (get-in % [:built :aspect]))) cells)]
    (loop [round 1
           current (mapv #(assoc % :padding {}) panelled)]
      (let [wanted (paddings current)
            next-cells (mapv (fn [{:keys [id padding] :as cell}]
                               (let [p (wanted id)]
                                 (if (= p (merge (zipmap sides (repeat 0)) padding))
                                   cell
                                   (assoc cell :padding p :built (rebuild cell p)))))
                             current)]
        (if (or (= (map :padding current) (map :padding next-cells)) (m/>= round max-alignment-rounds))
          (merge (into {} (map (juxt :id :built)) cells) (into {} (map (juxt :id :built)) next-cells))
          (recur (m/long-inc round) next-cells))))))
