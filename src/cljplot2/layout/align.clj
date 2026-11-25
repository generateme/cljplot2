(ns cljplot2.layout.align
  "Panel alignment of multiplot cells: sub-plots in one row (cells with equal box `y` and `h`) share their panel
  area's top and bottom edges; sub-plots in one column (equal box `x` and `w`) share its left and right edges.
  The panel area of a sub-plot is the bounding box of its main `:panel` items (plot margin panels left out, so plots
  with and without margins align on their main panels); sub-plots without panels (e.g. nested
  multiplots) and iso sub-plots (they centre their panels themselves) take no part. Used by the pipeline's `:subplots` stage for `:arrange` (on unless `:align false`).

  Even gaps (the last step of an `:equal-panels` arrange, see [[gap-paddings]]): the space between the panels of
  neighbouring columns - the left column's right inset plus the right column's left inset - made the same at every
  column boundary, the largest one; likewise between rows. The smaller boundaries are padded on the earlier column's
  right (row's bottom), so labels keep the room they need; [[align-panels]] adds such fixed paddings.

  Functions: [[align-panels]], [[panel-insets]], [[gap-paddings]]."
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

(def ^:private axes
  "Per direction, the box keys of a cell's place and size and its inset sides before and after its panel."
  [{:start :x :size :w :before :left :after :right}
   {:start :y :size :h :before :top :after :bottom}])

(defn- even-gap-extras
  "Gives the padding evening the space between the panels of neighbouring columns (or rows).

  Parameters:

  - `cells` (sequence): `{:id :box}` cells with a panel.
  - `inset` (fn): `(inset cell side)` - the cell's aligned inset of a side, pixels.
  - `axis` (map): `:start`, `:size` (box keys), `:before`, `:after` (inset sides), see [[axes]].

  Returns a map cell id -> pixels added on its `:after` side: columns are the cells of one box place and size, in
  order; a column's boundary is with the first later column starting at or after its end (overlapping columns -
  cells spanning rows - have none); every boundary's space is raised to the largest. Empty for fewer than two
  columns."
  [cells inset {:keys [start size before after]}]
  (let [columns (vec (sort-by key (group-by (fn [{:keys [box]}] [(long (get box start)) (long (get box size))]) cells)))
        widest (fn [side column-cells] (reduce m/long-max 0 (map #(long (inset % side)) column-cells)))
        boundaries (keep-indexed (fn [k [[^long s ^long z] column-cells]]
                                   (when-let [[_ next-cells] (first (filter (fn [[[^long s2]]] (m/>= s2 (m/long-add s z)))
                                                                            (subvec columns (m/long-inc k))))]
                                     [column-cells (m/long-add (long (widest after column-cells)) (long (widest before next-cells)))]))
                                 columns)
        space (reduce m/long-max 0 (map second boundaries))]
    (into {} (for [[column-cells ^long boundary] boundaries
                   {:keys [id]} column-cells]
               [id (m/long-sub space boundary)]))))

(defn gap-paddings
  "Gives the paddings making the space between the panels of neighbouring columns and rows of built cells even.

  Parameters:

  - `cells` (sequence): `{:id :box :built}` - boxes in the outer layout, built sub-plots (cells without a panel take
    no part).

  Returns a map cell id -> `{:right :bottom}` pixels: each column's (row's) boundary raised to the largest space
  between neighbouring panels (see [[even-gap-extras]]); empty when the gaps are already even or there are fewer than
  two columns and rows."
  [cells]
  (let [panelled (filter #(panel-insets (:built %)) cells)
        inset (fn [{:keys [built]} side] (get (panel-insets built) side))
        [columns rows] (map #(even-gap-extras panelled inset %) axes)]
    (into {} (for [id (distinct (concat (keys columns) (keys rows)))
                   :let [right (get columns id 0) bottom (get rows id 0)]
                   :when (or (m/pos? (long right)) (m/pos? (long bottom)))]
               [id {:right right :bottom bottom}]))))

(defn- paddings
  "Gives the padding per cell making panel insets equal within rows and columns.

  Parameters:

  - `cells` (sequence): `{:id :box :built :padding}`, each with a panel.
  - `fixed` (map): cell id -> `{side px}` added to the cell's padding (see [[gap-paddings]]); `nil` - none.

  Returns a map cell id -> `{:left :top :right :bottom}`: per side, the largest unpadded inset of the cell's row (top, bottom) or column (left, right) minus the cell's own, plus the cell's `fixed` padding."
  [cells fixed]
  (let [natural (into {} (map (fn [{:keys [id built padding]}]
                                [id (merge-with m/long-sub (panel-insets built) (merge (zipmap sides (repeat 0)) padding))]))
                      cells)
        group-of {:left (fn [{:keys [box]}] [(:x box) (:w box)]) :right (fn [{:keys [box]}] [(:x box) (:w box)])
                  :top (fn [{:keys [box]}] [(:y box) (:h box)]) :bottom (fn [{:keys [box]}] [(:y box) (:h box)])}
        target (fn [side cell]
                 (let [k ((group-of side) cell)]
                   (reduce m/long-max (map #(get-in natural [(:id %) side]) (filter #(= k ((group-of side) %)) cells)))))]
    (let [aligned (into {} (map (fn [{:keys [id] :as cell}]
                                  [id (into {} (map (fn [side] [side (m/long-sub (long (target side cell)) (long (get-in natural [id side])))]))
                                            sides)])
                                cells))]
      (reduce-kv (fn [acc id extra] (if (contains? acc id) (update acc id #(merge-with m/long-add % extra)) acc))
                 aligned (or fixed {})))))

(defn align-panels
  "Gives the built sub-plots with their panels aligned in rows and columns.

  Parameters:

  - `cells` (sequence): `[{:id :box :built}]`, boxes in the outer layout.
  - `rebuild` (fn): `(fn [cell padding] built)` builds the cell's plot with `:alignment-padding padding` (pixels added to its margins per side).
  - `fixed` (map): cell id -> `{side px}` padding added to the cell's alignment (even gaps, see [[gap-paddings]]); `nil` - none.

  Repeats until paddings stop changing, at most [[max-alignment-rounds]] times; cells without panels and iso cells take no part.

  Returns `{id built}` for all `cells`."
  [cells rebuild fixed]
  (let [panelled (filter #(and (panel-insets (:built %)) (not= :iso (get-in % [:built :aspect]))) cells)]
    (loop [round 1
           current (mapv #(assoc % :padding {}) panelled)]
      (let [wanted (paddings current fixed)
            next-cells (mapv (fn [{:keys [id padding] :as cell}]
                               (let [p (wanted id)]
                                 (if (= p (merge (zipmap sides (repeat 0)) padding))
                                   cell
                                   (assoc cell :padding p :built (rebuild cell p)))))
                             current)]
        (if (or (= (map :padding current) (map :padding next-cells)) (m/>= round max-alignment-rounds))
          (merge (into {} (map (juxt :id :built)) cells) (into {} (map (juxt :id :built)) next-cells))
          (recur (m/long-inc round) next-cells))))))
