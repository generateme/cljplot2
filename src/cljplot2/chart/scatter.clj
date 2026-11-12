(ns cljplot2.chart.scatter
  "Scatter chart: one shape per row at its mapped (x, y).

  Options (defaults from theme `[:chart :scatter]`): `:size` (pixels), `:color`, `:stroke-color`, `:stroke-width`,
  `:shape` (see [[cljplot2.shape]]); mapped `:color`, `:shape`, `:size` override them per point
  (see [[cljplot2.aesthetic]]); `:padding {:x [lo hi] :y [lo hi]}` (fractions of the data span),
  plus `:data` and `:mapping` (see [[cljplot2.chart]])."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.aesthetic :as aesthetic]
            [cljplot2.data :as data]
            [cljplot2.scale :as scale]
            [cljplot2.shape :as shape]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private position-dims [:x :y])

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

(defn- kept?
  "Is `v` of dimension `k` kept: valid, or `nil` (NA) for an aesthetic when `na-level?`."
  [k na-level? v]
  (boolean (or (data/valid-value? v) (and na-level? (nil? v) (aesthetic/dim? k)))))

(defn compute
  "Per panel: points (rows with valid mapped x and y and kept aesthetic values, see [[cljplot2.aesthetic]]) as
  `{:x [..] :y [..] :style style}` plus a vector per mapped aesthetic (`:color`, `:shape`, `:size`; `nil` = NA),
  `style` being the validated options merged over the theme; extents per panel (aesthetics: value extents of
  non-NA values), padding, `:na` (set of aesthetics with NA values) and per-dimension counts of dropped rows (zero
  when nothing is dropped). `env` `:na-level?` (default true) keeps `nil` aesthetic values as NA.
  Throws `ex-info` with `{:key :mapping}` when a mapped dimension has no valid value in a non-empty data set
  (usually a misspelt mapping)."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme na-level?] :or {na-level? true}}]
  (let [st (style options theme)
        padding (merge {:x [0.05 0.05] :y [0.05 0.05]} (:padding options))
        _ (doseq [k position-dims]
            (when-not (contains? mapping k)
              (throw (ex-info "Scatter needs :x and :y in its mapping." {:key :mapping :value k}))))
        dims (into position-dims (aesthetic/mapped-dims mapping))
        accessors (mapv #(data/row-accessor % (mapping %)) dims)
        values (update-vals groups (fn [rows] (mapv (fn [row] (mapv #(% row) accessors)) rows)))
        all-values (apply concat (vals values))
        row-count (count all-values)
        dropped (into {} (map-indexed (fn [i k] [k (count (remove #(kept? k na-level? (% i)) all-values))]) dims))
        keep-row? (fn [vs] (every? true? (map-indexed (fn [i k] (kept? k na-level? (vs i))) dims)))
        points (update-vals values (fn [vs] (let [kept (filterv keep-row? vs)]
                                              (into {:style st}
                                                    (map-indexed (fn [i k] [k (mapv #(% i) kept)]))
                                                    dims))))]
    (validate-padding! padding)
    (doseq [[i k] (map-indexed vector dims)]
      (when (and (pos? row-count) (not-any? #(data/valid-value? (% i)) all-values))
        (throw (ex-info "No valid values for a mapped dimension - check the mapping." {:key :mapping :value k :rows row-count}))))
    {:data points
     :extents (update-vals points (fn [p] (into {:x (data/extent :x (:x p)) :y (data/extent :y (:y p))}
                                                (map (fn [k] [k (data/value-extent k (remove nil? (p k)))]))
                                                (drop 2 dims))))
     :padding padding
     :na (set (filter (fn [k] (some #(some nil? (% k)) (vals points))) (drop 2 dims)))
     :dropped dropped}))

(defn- na-attributes
  "Attribute used for NA values per aesthetic: theme `[:palette :na]` color; the chart's own shape and size."
  [style theme]
  {:color (get-in theme [:palette :na]) :shape (:shape style) :size (:size style)})

(defn draw
  "Draw the points of one panel (`computed` - `{:x [..] :y [..] :style}` and aesthetic vectors) on `canvas` with the
  finalized `scales` (aesthetic scales map values to the point's color, shape, size). No points draw nothing
  (there may be no scales then); points the scales can't place - a position `##NaN` (e.g. outside an explicit bands
  domain) or an aesthetic value outside its domain (`nil` attribute) - are skipped."
  [_chart {:keys [x y style] :as computed} canvas _box scales env]
  (when (seq x)
    (let [{:keys [stroke-width]} style
          fx (scale/forward-fn (:x scales))
          fy (scale/forward-fn (:y scales))
          na (na-attributes style (:theme env))
          mapped (vec (for [k aesthetic/dims :when (contains? computed k)]
                        [k (aesthetic/attribute-fn k (get scales k) (na k)) (computed k)]))
          point-style (if (seq mapped)
                        (fn [^long i] (reduce (fn [st [k f vs]]
                                                (if-some [a (f (nth vs i))] (assoc st k a) (reduced nil)))
                                              style mapped))
                        (constantly style))]
      (c2d/set-stroke-custom canvas {:size (double stroke-width)})
      (dorun (map-indexed (fn [^long i vx]
                            (let [px (double (fx vx)) py (double (fy (nth y i)))]
                              (when-not (or (m/nan? px) (m/nan? py))
                                (when-let [st (point-style i)]
                                  (shape/draw! canvas (:shape st) (v/vec2 px py) st)))))
                          x)))))

(defn legend
  "Draw the key glyph: the chart's shape (style from options and theme, `attributes` - mapped color, shape, size -
  merged over) centered in `box`."
  [{:keys [options]} attributes canvas {:keys [x y w h]} env]
  (let [st (merge (style options (:theme env)) attributes)]
    (c2d/set-stroke-custom canvas {:size (double (:stroke-width st))})
    (shape/draw! canvas (:shape st) (v/vec2 (+ (double x) (* 0.5 (double w))) (+ (double y) (* 0.5 (double h)))) st)))
