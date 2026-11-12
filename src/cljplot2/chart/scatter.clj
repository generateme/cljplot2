(ns cljplot2.chart.scatter
  "Scatter chart: one shape per row at its mapped (x, y).

  Options (defaults from theme `[:chart :scatter]`): `:size` (pixels), `:color`, `:stroke-color`, `:stroke-width`,
  `:shape` (see [[cljplot2.shape]]), `:padding {:x [lo hi] :y [lo hi]}` (fractions of the data span),
  plus `:data` and `:mapping` (see [[cljplot2.chart]])."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.data :as data]
            [cljplot2.scale :as scale]
            [cljplot2.shape :as shape]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private dims [:x :y])

(defn- style
  "Chart options merged over the theme defaults, validated."
  [options theme]
  (let [{:keys [size shape stroke-width color stroke-color] :as merged}
        (merge (get-in theme [:chart :scatter]) (select-keys options [:size :color :stroke-color :stroke-width :shape]))]
    (when-not (and (number? size) (>= (double size) 0.0))
      (throw (ex-info "Scatter :size must be a non-negative number." {:key :size :value size})))
    (when-not (and (number? stroke-width) (>= (double stroke-width) 0.0))
      (throw (ex-info "Scatter :stroke-width must be a non-negative number." {:key :stroke-width :value stroke-width})))
    (when-not (shape/shapes shape)
      (throw (ex-info "Unknown scatter :shape." {:key :shape :value shape})))
    (when (or (nil? color) (nil? stroke-color))
      (throw (ex-info "Scatter colors must be colors or :none." {:key :color})))
    merged))

(defn- validate-padding!
  [padding]
  (doseq [[k p] padding]
    (when-not (and (vector? p) (= 2 (count p)) (every? #(and (number? %) (>= (double %) 0.0)) p))
      (throw (ex-info "Scatter :padding must map dimensions to [lo hi] non-negative fractions."
                      {:key :padding :scale k :value p})))))

(defn compute
  "Per panel: points (rows with valid mapped x and y) as `{:x [..] :y [..] :style style}`, `style` being the
  validated options merged over the theme; extents per panel, padding and per-dimension counts of dropped rows
  (zero when nothing is dropped).
  Throws `ex-info` with `{:key :mapping}` when a mapped dimension has no valid value in a non-empty data set
  (usually a misspelt mapping)."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme]}]
  (let [st (style options theme)
        padding (merge {:x [0.05 0.05] :y [0.05 0.05]} (:padding options))
        accessors (into {} (map (fn [k]
                                  (when-not (contains? mapping k)
                                    (throw (ex-info "Scatter needs :x and :y in its mapping." {:key :mapping :value k})))
                                  [k (data/row-accessor k (mapping k))]))
                        dims)
        values (update-vals groups (fn [rows] (mapv (fn [row] (mapv #((accessors %) row) dims)) rows)))
        row-count (reduce + (map count (vals values)))
        invalid-counts (into {} (map-indexed (fn [i k] [k (count (remove #(data/valid-value? (% i)) (apply concat (vals values))))]) dims))
        points (update-vals values (fn [vs] (let [valid (filterv #(every? data/valid-value? %) vs)]
                                              {:x (mapv first valid) :y (mapv second valid) :style st})))]
    (validate-padding! padding)
    (doseq [[k n] invalid-counts]
      (when (and (pos? (long row-count)) (= (long n) (long row-count)))
        (throw (ex-info "No valid values for a mapped dimension - check the mapping." {:key :mapping :value k :rows row-count}))))
    {:data points
     :extents (update-vals points (fn [{:keys [x y]}] {:x (data/extent :x x) :y (data/extent :y y)}))
     :padding padding
     :dropped invalid-counts}))

(defn draw
  "Draw the points of one panel (`computed` - `{:x [..] :y [..] :style}`) on `canvas` with the finalized `scales`.
  No points draw nothing (there may be no scales then); points the scales can't place (`##NaN`, e.g. a value
  outside an explicit bands domain) are skipped."
  [_chart {:keys [x y style]} canvas _box scales _env]
  (when (seq x)
    (let [{:keys [shape stroke-width]} style
          fx (scale/forward-fn (:x scales))
          fy (scale/forward-fn (:y scales))]
      (c2d/set-stroke-custom canvas {:size (double stroke-width)})
      (dorun (map (fn [vx vy]
                    (let [px (double (fx vx)) py (double (fy vy))]
                      (when-not (or (m/nan? px) (m/nan? py))
                        (shape/draw! canvas shape (v/vec2 px py) style))))
                  x y)))))
