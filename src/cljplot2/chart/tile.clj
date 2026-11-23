(ns cljplot2.chart.tile
  "Tile: one grid cell per row at its (x, y), filled by its `:color` - heatmaps (cartesian plots only).

  Mapping: `:x`, `:y` - each axis categorical (strings / keywords: a cell per level, the band scale without padding)
  or numeric (cells centred on the values, sized by the axis's resolution - the smallest gap between distinct values
  over all panels: values on a regular lattice tile the panel, scattered values give cells as narrow as their
  closest pair - bin them with [[cljplot2.chart.bin2d]], or round them to a lattice, instead); `:color` - numeric (a gradient, color bar) or categorical (palette, legend keys). Rows with an
  invalid x / y or a color outside the color scale are dropped and counted; a `nil` color is NA (theme
  `[:palette :na]`). Several rows at one (x, y) in a panel: the last one is drawn, the others counted in `:dropped
  {:duplicate n}`. Facets: a heatmap per panel.

  Cells (`:cell`, default `:square`): any `fastmath.grid` type, laid out as in [[cljplot2.data.cells]] - on
  categorical axes `:pointy-hex` gives a honeycomb (odd rows shifted half a cell right), `:flat-hex` shifts odd
  columns half a cell up, `:triangle` alternates up / down; on numeric axes a cell stays centred on its value along
  that axis (a honeycomb needs categorical axes, or [[cljplot2.chart.bin2d]] for points). Band scales keep room for
  cells reaching past their band (`:padding-out`).

  Options (style defaults from theme `[:chart :tile]`): `:cell`, `:cell-scale` (shrink factor, `(0 1]`; gaps between
  cells), `:stroke-color` (a color, `:none` or `:darker`), `:stroke-width`, `:alpha` (fill alpha, a fraction);
  `:value-label` - `true` (the color value: numbers to 2 decimals) or a function of `{:value :x :y}` returning the
  text (`nil` - none), drawn centred in black or white by the color painted under it (translucent fills included;
  text role `:tile-value`).

  Cell size (`:size` mapping, optional): each cell shrinks by `:cell-scale` × `size / s-max` - `size` the value's size
  through the plot's size scale, `s-max` the largest size of that scale's `:range` (1 for an identity scale, whose
  values are the fractions themselves, larger ones clamped to 1); e.g. `:scales {:size {:range [7 10]}}` gives cells from 70 % to 100 % (lattice
  `shrink`). A size of 0 draws no cell; rows with a `nil` size or one outside the size scale are dropped (`:dropped
  {:size n}`). Sized cells are drawn antialiased (they don't abut); legend keys are cells scaled the same way.

  Colour bar beside a heatmap (pheatmap-style annotation): a one-row tile (`:y` constant, `:color` the annotation)
  above the heatmap in `(plot/arrange [bar heatmap] {:columns 1 :heights [1 6]})`, both plots with the same explicit
  x `:domain` (the bar's x axis hidden with `:axis false`) - the arranged panels align, so its cells sit over the
  heatmap's columns; a column of cells beside it works the same way with a shared y `:domain`.

  Functions: [[compute]], [[draw]], [[legend]] - the chart contract; [[resolution]] - the cell size of numeric positions; [[largest-size]], [[size-factor]] - cell sizes from a size scale."
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.content.label :as label]
            [cljplot2.data.cells :as cells]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def schemas
  "Schemas of the tile chart: `:style` (theme `[:chart :tile]`), `:options`, `:mapping` (merged)."
  {:style (-> [:map {:closed true}]
             (into cells/style-entries)
             (conj (schema/optional :size-key-fill {:description "fill of a size legend key without a colour (theme only, not an option)"}
                                    schema/color)))
   :options (-> [:map {:closed true}]
                (into cells/style-entries)
                (into data/common-options)
                (into theme/theme-style-entries)
                (conj (schema/optional :value-label {:description "label in each cell: true (the colour value, numbers to 2 decimals) or a function of {:value :x :y} returning the text (nil - none), drawn in black or white by the fill under it"
                                                     :default false}
                                       [:or :boolean schema/function])))
   :mapping [:map
             [:x {:description "cell x position: categorical (a cell per level) or numeric (cells centred on the values, sized by the smallest gap)"}
              schema/accessor]
             [:y {:description "cell y position: categorical (a cell per level) or numeric (cells centred on the values, sized by the smallest gap)"}
              schema/accessor]
             [:color {:description "cell fill: numeric (a gradient and colour bar) or categorical (a palette and legend); nil - the NA colour"}
              schema/accessor]
             (schema/nilable :size {:description "cell size: each cell shrinks by :cell-scale times its size over the size scale's largest"}
                             schema/accessor)]})

(defn- position-ok?
  "Tests whether `v` is a valid position value (dates too - [[axis-info]] rejects them with a message)."
  [v]
  (data/valid-value? v))

(defn- color-kept?
  "Tests whether a colour value is kept.

  Parameters:

  - `na-level?`: `nil` values form the NA level.
  - `plot-scales` (map): the plot's `:scales`.
  - `v`: the colour value.

  Returns truthy for a value the plot's colour scale keeps (see [[cljplot2.scale.aesthetic/kept-value?]]) or, with `na-level?`, `nil`."
  [na-level? plot-scales v]
  (or (aesthetic/kept-value? :color (get plot-scales :color {}) v) (and na-level? (nil? v))))

(defn- size-kept?
  "Tests whether `v` is a kept size: not `nil` and valid under the plot's size scale (no NA level for cell sizes)."
  [plot-scales v]
  (and (some? v) (aesthetic/kept-value? :size (get plot-scales :size {}) v)))

(defn largest-size
  "Gives the largest size of a size scale.

  Parameters:

  - `spec` (map): a finalized or phase-1 size scale - `:kind`, `:range` (sizes, or a level -> size map); `nil` - none.

  Returns the maximum of its `:range` as a double; 1.0 for an identity scale, no scale or an empty range.

  See also [[size-factor]]."
  ^double [spec]
  (let [size-range (:range spec)
        sizes (if (map? size-range) (vals size-range) size-range)]
    (if (and (not= :identity (:kind spec)) (seq sizes))
      (v/mx sizes)
      1.0)))

(defn size-factor
  "Gives the shrink factor of a cell of a given size.

  Parameters:

  - `size` (number): an attribute of the size scale.
  - `largest` (double): the scale's largest size (see [[largest-size]]).

  Returns `size / largest` clamped to `[0 1]` (identity sizes above 1 give 1); 0 for a non-positive largest size."
  ^double [^double size ^double largest]
  (if (m/pos? largest) (m/constrain (m// size largest) 0.0 1.0) 0.0))

(defn resolution
  "Gives the smallest positive gap between distinct numbers.

  Parameters:

  - `vs` (sequence of numbers): the values.

  Returns the smallest gap between consecutive distinct values; 1.0 for fewer than two distinct values."
  ^double [vs]
  (let [ds (sort (distinct (map double vs)))]
    (if (m/< (count ds) 2)
      1.0
      (v/mn (map (fn [^double a ^double b] (m/- b a)) ds (rest ds))))))

(defn- axis-info
  "Gives the layout of a position axis from its values over all panels.

  Parameters:

  - `k` (keyword): `:x` or `:y`.
  - `vs` (sequence): the axis's kept values.
  - `plot-scales` (map): the plot's `:scales` (a `:kind :bands` scale is categorical).

  Returns `{:kind :categorical}` (category values, or a plot scale declared categorical) or `{:kind :numeric :min m :resolution r}` (no values: min 0, resolution 1).

  Throws `ex-info {:key :mapping}` for temporal values."
  [k vs plot-scales]
  (let [e (data/extent k vs)]
    (case (if (= :bands (get-in plot-scales [k :kind])) :categorical (:kind e))
      nil {:kind :numeric :min 0.0 :resolution 1.0}
      :temporal (schema/fail! (str "Tile " k " values must be numbers or categories (strings, keywords), not dates.")
                              {:key :mapping :in [:mapping k]})
      :numeric {:kind :numeric :min (:min e) :resolution (resolution vs)}
      :categorical {:kind :categorical})))

(defn- last-per-position
  "Keeps one cell per position of a panel - the last.

  Parameters:

  - `rows` (sequence): the panel's cells `[x y color]` (`[x y color size]` with a size mapping).

  Returns `[cells duplicates]`: one cell per (x, y) - the last - in the first-appearance order of the positions (numbers compared as doubles), and the count of the others."
  [rows]
  (let [key-of (fn [[x y]] [(aesthetic/level-key x) (aesthetic/level-key y)])
        last-row (reduce (fn [acc row] (assoc acc (key-of row) row)) {} rows)
        order (distinct (map key-of rows))]
    [(mapv last-row order) (m/- (count rows) (count last-row))]))

(def ^{:private true :const true :tag 'double} half-band+
  "Reach past which a cell leaves its band: half a band plus a rounding tolerance."
  (m/+ 0.5 1.0e-9))

(defn- band-room
  "Gives the bands attributes keeping room for cells reaching past their bands.

  Parameters:

  - `reach` (`[lo hi]`): how far cells reach from their band centres, in index units (see [[cljplot2.data.cells/lattice-extent]]).
  - `vertical?`: the axis is vertical (its range runs upward).

  Returns `{:padding-in 0 :padding-out p}` - outer padding (in steps, each side - total twice it) for the reach past the outer half bands - with `:align-outer` (the share of the outer space on the low-pixel side: the first band's side of a horizontal axis, the last band's of a vertical one) when there is any."
  [[^double lo ^double hi] vertical?]
  (let [beyond (fn ^double [^double reach] (if (m/> reach half-band+) (m/- reach 0.5) 0.0))
        first-side (double (beyond (m/- lo)))
        last-side (double (beyond hi))
        total (m/+ first-side last-side)]
    (cond-> {:padding-in 0.0 :padding-out (m/* 0.5 total)}
      (m/pos? total) (assoc :align-outer (m// (if vertical? last-side first-side) total)))))

(defn compute
  "Computes the tile chart's stats: one cell per position, per panel.

  Parameters:

  - `chart` (map): the chart value - `:options` (the cell style, `:value-label`).
  - `split` (map): `:groups` (`{panel rows}`), `:mapping` (merged; `:x`, `:y`, `:color`, optional `:size`).
  - `env` (map): `:theme`, `:na-level?` (default true), `:plot-scales`, `:flip?`.

  Returns per panel `{:cells [[x y color size?] ..] :axes {:x info :y info} :style st :value-label f :sized? b}` (axis info - see [[axis-info]]); extents (numeric axes: the values widened by the cells' reach; categorical: the levels), color value extents (and size values with a `:size` mapping), no padding, `:scale-defaults` (categorical axes: see [[band-room]]; numeric axes: no nice domain - cells fill the panel), `:na`, `:dropped` (`:x`, `:y`, `:color`, `:duplicate`; `:size` with a `:size` mapping).

  Throws `ex-info {:key :mapping}` when no row has a valid colour, or for temporal positions."
  [{:keys [options] :as chart} {:keys [groups mapping]} {:keys [theme na-level? plot-scales flip?] :or {na-level? true}}]
  (let [st (theme/resolved-style theme chart)
        sized? (some? (:size mapping))
        accessors (mapv #(data/row-accessor % (mapping %)) (cond-> [:x :y :color] sized? (conj :size)))
        values (update-vals groups (fn [rows] (mapv (fn [row] (mapv #(% row) accessors)) rows)))
        all (apply concat (vals values))
        size-ok? (fn [cell] (or (not sized?) (size-kept? plot-scales (nth cell 3))))
        keep? (fn [[x y c :as cell]] (and (position-ok? x) (position-ok? y) (color-kept? na-level? plot-scales c) (size-ok? cell)))
        kept (update-vals values #(last-per-position (filterv keep? %)))
        cells-of (update-vals kept first)
        kept-all (apply concat (vals cells-of))
        axes {:x (axis-info :x (map first kept-all) plot-scales) :y (axis-info :y (map second kept-all) plot-scales)}
        numeric? (fn [k] (= :numeric (:kind (axes k))))
        [rx ry] (cells/lattice-reach (:cell st) (numeric? :x) (numeric? :y))
        reach {:x rx :y ry}
        [x-extent y-extent] (cells/lattice-extent (:cell st) (numeric? :x) (numeric? :y))
        overhang {:x x-extent :y y-extent}
        axis-extent (fn [k vs]
                      (when (seq vs)
                        (if (numeric? k)
                          (let [cell-reach (m/* (double (reach k)) (double (:resolution (axes k))))]
                            {:kind :numeric :min (m/- (v/mn vs) cell-reach) :max (m/+ (v/mx vs) cell-reach)})
                          (if (every? number? vs) {:kind :categorical :values (vec (distinct vs))} (data/extent k vs)))))]
    (when (and (seq all) (not-any? #(aesthetic/valid-value? :color (get plot-scales :color) (nth % 2)) all))
      (schema/fail! data/no-valid-values {:key :mapping :value :color :rows (count all)}))
    {:data (update-vals cells-of (fn [cs] {:cells cs :axes axes :style st :value-label (:value-label options) :sized? sized?}))
     :extents (update-vals cells-of (fn [cs] (into {} (filter val)
                                                   {:x (axis-extent :x (map first cs))
                                                    :y (axis-extent :y (map second cs))
                                                    :color (aesthetic/value-extent :color (get plot-scales :color)
                                                                                   (remove nil? (map #(nth % 2) cs)))
                                                    :size (when sized?
                                                            (aesthetic/value-extent :size (get plot-scales :size)
                                                                                    (map #(nth % 3) cs)))})))
     :padding {:x [0.0 0.0] :y [0.0 0.0]}
     :scale-defaults (into {} (for [k [:x :y]]
                                [k (if (numeric? k) {:nice false} (band-room (overhang k) (= k (if flip? :x :y))))]))
     :na (if (some #(nil? (nth % 2)) kept-all) #{:color} #{})
     :dropped (cond-> {:x (count (remove #(position-ok? (first %)) all))
                       :y (count (remove #(position-ok? (second %)) all))
                       :color (count (remove #(color-kept? na-level? plot-scales (nth % 2)) all))
                       :duplicate (reduce m/long-add 0 (map second (vals kept)))}
                sized? (assoc :size (count (remove size-ok? all))))}))

;; drawing

(defn- axis-placement
  "Gives the pixel placement of cells along a position axis.

  Parameters:

  - `info` (map): the axis's layout (see [[axis-info]]) - `:kind`, `:min`, `:resolution`.
  - `finalized` (map): the axis's finalized position scale.

  Returns `{:pixel (fn [value offset] px) :index (fn [value] i)}`: categorical - the band centre plus `offset` band steps (signed by the range's direction), the level's index in the scale domain; numeric - the pixel of `value + offset * resolution`, the value's step from the axis minimum. `##NaN` pixels for values the scale can't place."
  [{:keys [kind] axis-min :min step-size :resolution} finalized]
  (let [f (scale/forward-fn finalized)]
    (if (= kind :categorical)
      (let [index (zipmap (map aesthetic/level-key (:domain finalized)) (range))
            {:keys [^double step]} (scale/band-geometry finalized)
            [^double start ^double end] (:range finalized)
            signed-step (m/* step (m/signum (m/- end start)))]
        {:pixel (fn [value ^double offset] (m/+ (double (f value)) (m/* offset signed-step)))
         :index (fn [value] (get index (aesthetic/level-key value) 0))})
      (let [lo (double axis-min)
            r (double step-size)]
        {:pixel (fn [^double value ^double offset] (double (f (m/+ value (m/* offset r)))))
         :index (fn [^double value] (m/round (m// (m/- value lo) r)))}))))

(defn- label-text
  "Gives the value label text of a cell.

  Parameters:

  - `value-label`: the `:value-label` option - `true` or a function of `{:value :x :y}`.
  - `value`: the cell's colour value.
  - `x`, `y`: the cell's position values.

  Returns the function's result as text (`nil` - no label); for `true` a number to at most 2 decimals (whole numbers without them), a keyword's name, other values as text (see [[cljplot2.content.label/value-text]])."
  [value-label value x y]
  (if (fn? value-label)
    (some-> (value-label {:value value :x x :y y}) str)
    (label/value-text value)))

(defn draw
  "Draws one panel's cells: fills, then the value labels on top.

  Parameters:

  - `_chart`: the chart value (unused).
  - `computed` (map): the panel's stats (see [[compute]]).
  - `canvas`: the panel's canvas.
  - `_box`: the panel box (unused).
  - `scales` (map): the panel's finalized scales.
  - `env` (map): the drawing environment.

  A cell whose colour is outside the colour scale, or whose size factor is 0, isn't drawn; labels take black or white by the colour painted under them (text role `:tile-value`)."
  [_chart {:keys [cells axes style value-label sized?]} canvas _box scales env]
  (when (seq cells)
    (let [px (axis-placement (:x axes) (:x scales))
          py (axis-placement (:y axes) (:y scales))
          flipped? (scale/flipped? scales)
          centre-x? (= :numeric (:kind (:x axes)))
          centre-y? (= :numeric (:kind (:y axes)))
          color-of (aesthetic/attribute-fn :color (:color scales) (get-in env [:theme :palette :na]))
          factor-of (if sized?
                      (let [size-of (aesthetic/attribute-fn :size (:size scales) nil)
                            largest (largest-size (:size scales))]
                        (fn [cell] (some-> (size-of (nth cell 3)) (size-factor largest))))
                      (constantly 1.0))
          placed (keep (fn [[x y value :as cell]]
                         (when-let [fill (when (m/pos? (double (or (factor-of cell) 0.0))) (color-of value))]
                           (let [to-pixel (fn [[ox oy]]
                                            (let [a ((:pixel px) x ox) b ((:pixel py) y oy)]
                                              (if flipped? (v/vec2 b a) (v/vec2 a b))))]
                             {:x x :y y :value value :fill fill :to-pixel to-pixel
                              :cell-scale (m/* (double (or (:cell-scale style) 1.0)) (double (factor-of cell)))
                              :offsets (cells/lattice-offsets (:cell style) ((:index px) x) ((:index py) y) centre-x? centre-y?)})))
                       cells)]
      (let [draw-cells! (fn [c] (doseq [{:keys [to-pixel offsets fill cell-scale]} placed]
                                  (cells/draw-cell! c to-pixel offsets fill (assoc style :cell-scale cell-scale))))]
        ;; sized cells don't abut: antialiased edges
        (if sized? (draw-cells! canvas) (cells/call-with-cell-rendering canvas style draw-cells!)))
      (when value-label
        (doseq [{:keys [x y value fill to-pixel offsets]} placed
                :let [text (label-text value-label value x y)
                      ^fastmath.vector.Vec2 centre (to-pixel (let [n (double (count offsets))]
                                                               [(m// (v/sum (map first offsets)) n)
                                                                (m// (v/sum (map second offsets)) n)]))]
                :when (and text (not (m/nan? (.x centre))) (not (m/nan? (.y centre))))]
          (label/draw-at! canvas {:kind :label :role :tile-value :text text
                                  :color (draw/text-color (:theme env) :tile-value
                                                          (or (draw/painted-color canvas (.x centre) (.y centre)) fill))}
                          [:center :middle] (.x centre) (.y centre) env))))))

(defn legend
  "Draws the tile chart's key glyph: the cell shape fitted in the key box.

  Parameters:

  - `chart` (map): the chart value.
  - `attributes` (map): the legend entry's mapped attributes (`:color`, `:size`).
  - `canvas`: the legend's canvas.
  - `box` (map): the key box `{:x :y :w :h}`.
  - `env` (map): the drawing environment (`:theme`, `:scales` - the plot's size scale).

  The cell is filled with the key's colour (the style's `:size-key-fill`, theme `[:chart :tile]`, for a size key without one); a size key's cell is scaled by its size factor (see [[size-factor]]); a factor of 0 draws nothing."
  [chart attributes canvas {:keys [^double x ^double y ^double w ^double h]} env]
  (let [st (theme/resolved-style (:theme env) chart)
        offsets (cells/lattice-offsets (:cell st) 0 0 true true)
        [^double rx ^double ry] (cells/lattice-reach (:cell st) true true)
        k (m// (m/* 0.4 (m/min w h)) (m/max rx ry))
        cx (m/+ x (m/* 0.5 w))
        cy (m/+ y (m/* 0.5 h))
        factor (if-let [size (:size attributes)]
                 (size-factor size (largest-size (get-in env [:scales :size])))
                 1.0)]
    (when (m/pos? factor)
      (cells/draw-cell! canvas (fn [[^double ox ^double oy]] (v/vec2 (m/+ cx (m/* k ox)) (m/- cy (m/* k oy))))
                        offsets (or (:color attributes) (:size-key-fill st)) (assoc st :cell-scale factor)))))
