(ns cljplot2.chart.scatter
  "Scatter chart: one shape per row at its mapped (x, y).

  Options (defaults from theme `[:chart :scatter]`): `:size` (pixels), `:color`, `:stroke-color`, `:stroke-width`,
  `:shape` (see [[cljplot2.shape]]), `:padding {:x [lo hi] :y [lo hi]}` (fractions of the data span),
  plus `:data` and `:mapping` (see [[cljplot2.chart]])."
  (:require [clojure2d.core :as c2d]
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

(defn compute
  "Points per panel: rows with valid mapped x and y, as `{:x [..] :y [..]}`; extents per panel, padding and
  per-dimension counts of dropped rows."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme]}]
  (style options theme)
  (let [accessors (into {} (map (fn [k]
                                  (when-not (contains? mapping k)
                                    (throw (ex-info "Scatter needs :x and :y in its mapping." {:key :mapping :value k})))
                                  [k (data/row-accessor k (mapping k))]))
                        dims)
        per-panel (into {} (map (fn [[panel rows]]
                                  (let [values (mapv (fn [row] (mapv #((accessors %) row) dims)) rows)
                                        valid (filterv #(every? data/valid-value? %) values)]
                                    [panel {:points {:x (mapv first valid) :y (mapv second valid)}
                                            :dropped (into {} (map-indexed (fn [i k] [k (count (remove #(data/valid-value? (% i)) values))]) dims))}])))
                        groups)]
    {:data (update-vals per-panel :points)
     :extents (update-vals per-panel (fn [{{:keys [x y]} :points}] {:x (data/extent :x x) :y (data/extent :y y)}))
     :padding (let [padding (merge {:x [0.05 0.05] :y [0.05 0.05]} (:padding options))]
                (doseq [[k p] padding]
                  (when-not (and (vector? p) (= 2 (count p)) (every? #(and (number? %) (>= (double %) 0.0)) p))
                    (throw (ex-info "Scatter :padding must map dimensions to [lo hi] non-negative fractions."
                                    {:key :padding :scale k :value p}))))
                padding)
     :dropped (apply merge-with + {:x 0 :y 0} (map :dropped (vals per-panel)))}))

(defn draw
  "Draw the points of one panel (`computed` - `{:x [..] :y [..]}`) on `canvas` with the finalized `scales`.
  No points draw nothing (there may be no scales then)."
  [{:keys [options]} {:keys [x y]} canvas _box scales {:keys [theme]}]
  (when (seq x)
   (let [{:keys [shape stroke-width] :as st} (style options theme)
        fx (scale/forward-fn (:x scales))
        fy (scale/forward-fn (:y scales))]
    (c2d/set-stroke-custom canvas {:size (double stroke-width)})
    (dorun (map (fn [vx vy] (shape/draw! canvas shape (v/vec2 (fx vx) (fy vy)) st)) x y)))))
