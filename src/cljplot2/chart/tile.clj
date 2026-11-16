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

  Colour bar beside a heatmap (pheatmap-style annotation): a one-row tile (`:y` constant, `:color` the annotation)
  above the heatmap in `(plot/arrange [bar heatmap] {:columns 1 :heights [1 6]})`, both plots with the same explicit
  x `:domain` (the bar's x axis hidden with `:axis false`) - the arranged panels align, so its cells sit over the
  heatmap's columns; a column of cells beside it works the same way with a shared y `:domain`."
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.content.label :as label]
            [cljplot2.data.cells :as cells]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def schemas
  "Schemas of the tile chart: `:style` (theme `[:chart :tile]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] cells/style-entries)
   :options (-> [:map {:closed true}]
                (into cells/style-entries)
                (into data/common-options)
                (conj (schema/optional :value-label [:or :boolean schema/function])))
   :mapping [:map [:x schema/accessor] [:y schema/accessor] [:color schema/accessor]]})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :tile]) (select-keys options cells/style-keys)))

(defn- position-ok?
  "Is `v` a valid position value (dates too - [[axis-info]] rejects them with a message)?"
  [v]
  (data/valid-value? v))

(defn- color-kept?
  [na-level? plot-scales v]
  (or (aesthetic/kept-value? :color (get plot-scales :color {}) v) (and na-level? (nil? v))))

(defn resolution
  "Smallest positive gap between the distinct numbers `vs` (1.0 for fewer than two)."
  ^double [vs]
  (let [ds (vec (sort (distinct (map double vs))))]
    (if (< (count ds) 2)
      1.0
      (double (reduce min (map (fn [a b] (- (double b) (double a))) ds (rest ds)))))))

(defn- axis-info
  "Layout of position axis `k` from its values `vs` over all panels: `{:kind :categorical}` (category values, or a
  plot scale declared categorical - `:kind :bands` in `plot-scales`) or `{:kind :numeric :min m :resolution r}`.
  Temporal values throw `ex-info {:key :mapping}`."
  [k vs plot-scales]
  (let [e (data/extent k vs)]
    (case (if (= :bands (get-in plot-scales [k :kind])) :categorical (:kind e))
      nil {:kind :numeric :min 0.0 :resolution 1.0}
      :temporal (schema/fail! (str "Tile " k " values must be numbers or categories (strings, keywords), not dates.")
                              {:key :mapping :in [:mapping k]})
      :numeric {:kind :numeric :min (:min e) :resolution (resolution vs)}
      :categorical {:kind :categorical})))

(defn- last-per-position
  "Rows `[x y color]` of a panel, one per (x, y) - the last - in first-appearance order of the positions; and the
  count of the others."
  [rows]
  (let [key-of (fn [[x y]] [(if (number? x) (double x) x) (if (number? y) (double y) y)])
        last-row (reduce (fn [acc r] (assoc acc (key-of r) r)) {} rows)
        order (distinct (map key-of rows))]
    [(mapv last-row order) (- (count rows) (count last-row))]))

(defn- band-room
  "Bands attributes keeping room for cells reaching `[lo hi]` (index units) from their band centres: no inner
  padding; outer padding (in steps, each side - total twice it) for the reach past the outer half bands, put on the
  side that needs it. `:align-outer` is the share of the outer space on the low-pixel side: the first band's side of
  a horizontal axis, the last band's of a `vertical?` one (its range runs upward)."
  [[lo hi] vertical?]
  (let [beyond (fn ^double [^double reach] (if (> reach (+ 0.5 1.0e-9)) (- reach 0.5) 0.0))
        first-side (double (beyond (- (double lo))))
        last-side (double (beyond (double hi)))
        total (+ first-side last-side)]
    (cond-> {:padding-in 0.0 :padding-out (* 0.5 total)}
      (pos? total) (assoc :align-outer (/ (if vertical? last-side first-side) total)))))

(defn compute
  "Per panel: `{:cells [[x y color] ..] :axes {:x info :y info} :style st :value-label f}` (axis info - see
  [[axis-info]]); extents (numeric axes: the values widened by the cells' reach; categorical: the levels), color
  value extents, no padding, `:scale-defaults` (categorical axes: see [[band-room]]; numeric axes: no nice domain - cells fill the panel), `:na`, `:dropped` (`:x`, `:y`, `:color`, `:duplicate`)."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme na-level? plot-scales flip?] :or {na-level? true}}]
  (let [st (style options theme)
        accessors (mapv #(data/row-accessor % (mapping %)) [:x :y :color])
        values (update-vals groups (fn [rows] (mapv (fn [row] (mapv #(% row) accessors)) rows)))
        all (apply concat (vals values))
        keep? (fn [[x y c]] (and (position-ok? x) (position-ok? y) (color-kept? na-level? plot-scales c)))
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
                          (let [r (* (double (reach k)) (double (:resolution (axes k))))]
                            {:kind :numeric :min (- (double (reduce min vs)) r) :max (+ (double (reduce max vs)) r)})
                          (if (every? number? vs) {:kind :categorical :values (vec (distinct vs))} (data/extent k vs)))))]
    (when (and (seq all) (not-any? #(aesthetic/valid-value? :color (get plot-scales :color) (nth % 2)) all))
      (schema/fail! "No valid values for a mapped dimension - check the mapping." {:key :mapping :value :color :rows (count all)}))
    {:data (update-vals cells-of (fn [cs] {:cells cs :axes axes :style st :value-label (:value-label options)}))
     :extents (update-vals cells-of (fn [cs] (into {} (filter val)
                                                   {:x (axis-extent :x (map first cs))
                                                    :y (axis-extent :y (map second cs))
                                                    :color (aesthetic/value-extent :color (get plot-scales :color)
                                                                                   (remove nil? (map #(nth % 2) cs)))})))
     :padding {:x [0.0 0.0] :y [0.0 0.0]}
     :scale-defaults (into {} (for [k [:x :y]]
                                [k (if (numeric? k) {:nice false} (band-room (overhang k) (= k (if flip? :x :y))))]))
     :na (if (some #(nil? (nth % 2)) kept-all) #{:color} #{})
     :dropped {:x (count (remove #(position-ok? (first %)) all))
               :y (count (remove #(position-ok? (second %)) all))
               :color (count (remove #(color-kept? na-level? plot-scales (nth % 2)) all))
               :duplicate (reduce + 0 (map second (vals kept)))}}))

;; drawing

(defn- axis-placement
  "`{:pixel (fn [value offset] px) :index (fn [value] i)}` of a position axis: categorical - the band centre plus
  `offset` band steps, the level's index in the scale domain; numeric - the pixel of `value + offset * resolution`,
  the value's step from the axis minimum. `##NaN` pixels for values the scale can't place."
  [{:keys [kind min resolution]} finalized]
  (let [f (scale/forward-fn finalized)]
    (if (= kind :categorical)
      (let [domain (vec (:domain finalized))
            index (zipmap (map #(if (number? %) (double %) %) domain) (range))
            {:keys [step]} (scale/band-geometry finalized)
            [start end] (:range finalized)
            step (* (double step) (m/signum (- (double end) (double start))))]
        {:pixel (fn [value ^double offset] (+ (double (f value)) (* offset step)))
         :index (fn [value] (get index (if (number? value) (double value) value) 0))})
      (let [lo (double min) r (double resolution)]
        {:pixel (fn [value ^double offset] (double (f (+ (double value) (* offset r)))))
         :index (fn [value] (m/round (/ (- (double value) lo) r)))}))))

(defn- label-text
  "Text of the value label of a cell (`value-label` - the option), `nil` - none: a function's result as text; for
  `true` a number to at most 2 decimals (whole numbers without them), a keyword's name, other values as text."
  [value-label value x y]
  (cond
    (fn? value-label) (some-> (value-label {:value value :x x :y y}) str)
    (nil? value) nil
    (number? value) (let [r (m/approx (double value) 2)]
                      (if (== r (m/round r)) (str (m/round r)) (str r)))
    (keyword? value) (name value)
    :else (str value)))

(defn draw
  "Draw one panel's cells (fills, then value labels on top)."
  [_chart {:keys [cells axes style value-label]} canvas _box scales env]
  (when (seq cells)
    (let [px (axis-placement (:x axes) (:x scales))
          py (axis-placement (:y axes) (:y scales))
          flipped? (scale/flipped? scales)
          centre-x? (= :numeric (:kind (:x axes)))
          centre-y? (= :numeric (:kind (:y axes)))
          color-of (aesthetic/attribute-fn :color (:color scales) (get-in env [:theme :palette :na]))
          placed (keep (fn [[x y value]]
                         (when-let [fill (color-of value)]
                           (let [to-pixel (fn [[ox oy]]
                                            (let [a ((:pixel px) x ox) b ((:pixel py) y oy)]
                                              (if flipped? (v/vec2 b a) (v/vec2 a b))))]
                             {:x x :y y :value value :fill fill :to-pixel to-pixel
                              :offsets (cells/lattice-offsets (:cell style) ((:index px) x) ((:index py) y) centre-x? centre-y?)})))
                       cells)]
      (doseq [{:keys [to-pixel offsets fill]} placed]
        (cells/draw-cell! canvas to-pixel offsets fill style))
      (when value-label
        (doseq [{:keys [x y value fill to-pixel offsets]} placed
                :let [text (label-text value-label value x y)
                      ^fastmath.vector.Vec2 centre (to-pixel (let [n (double (count offsets))]
                                                               [(/ (double (reduce + (map first offsets))) n)
                                                                (/ (double (reduce + (map second offsets))) n)]))]
                :when (and text (not (m/nan? (.x centre))) (not (m/nan? (.y centre))))]
          (label/draw-at! canvas {:kind :label :role :tile-value :text text
                                  :color (draw/contrast-color (or (draw/painted-color canvas (.x centre) (.y centre)) fill))}
                          [:center :middle] (.x centre) (.y centre) env))))))

(defn legend
  "Key glyph: the cell shape filled with the key's color, fitted in `box`."
  [{:keys [options]} attributes canvas {:keys [x y w h]} env]
  (let [st (style options (:theme env))
        offsets (cells/lattice-offsets (:cell st) 0 0 true true)
        [rx ry] (cells/lattice-reach (:cell st) true true)
        k (/ (* 0.4 (m/min (double w) (double h))) (m/max (double rx) (double ry)))
        cx (+ (double x) (* 0.5 (double w))) cy (+ (double y) (* 0.5 (double h)))]
    (cells/draw-cell! canvas (fn [[ox oy]] (v/vec2 (+ cx (* k (double ox))) (- cy (* k (double oy)))))
                      offsets (:color attributes) (assoc st :cell-scale 1.0))))
