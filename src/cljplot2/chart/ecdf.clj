(ns cljplot2.chart.ecdf
  "ECDF: the empirical cumulative distribution of `:x` per panel and group, drawn as a step line from 0 to 1.

  Uses fastmath's `:real-discrete-distribution` `cdf`; the value at the largest data value is exactly 1.0.
  Options (style defaults from theme `[:chart :ecdf]`): `:color`, `:width`, `:dash`,
  `:padding {:x [lo hi] :y [lo hi]}` (default 5% each side).
  Groups (see [[cljplot2.data.group]]) come from ordinal aesthetics (`:color` colors the line) and `:group`.

  Functions: [[steps]] - the step-line vertices of a sample; [[compute]], [[draw]], [[legend]] - the chart contract; [[schemas]] - its style, options and mapping."
  (:require [fastmath.core :as m]
            [fastmath.random :as r]
            [cljplot2.data.rows :as data]
            [cljplot2.data.group :as group]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def schemas
  "Schemas of the ECDF chart: `:style` (theme `[:chart :ecdf]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] polyline/style-entries)
   :options (-> [:map {:closed true}]
                (into polyline/style-entries)
                (into data/common-options)
                (into theme/theme-style-entries)
                (conj (schema/optional :padding {:description "padding of the data span per position axis, {:x [lo hi] :y [lo hi]} fractions of the span"
                                                 :cljplot2/default-doc "{:x [0.05 0.05] :y [0.05 0.05]}"}
                                       data/position-padding)))
   :mapping [:map
             [:x {:description "values whose empirical distribution is drawn (data units)"} schema/accessor]
             (schema/nilable :color {:description "groups, each a step line in its own colour"} schema/accessor)
             (schema/nilable :dash {:description "groups, each a step line with its own dash style"} schema/accessor)
             (schema/nilable :group {:description "groups without a visual aesthetic, each its own step line"} schema/accessor)]})

(defn steps
  "Gives the step-line vertices of the empirical cumulative distribution of a sample.

  Parameters:

  - `values` (sequence of doubles): finite values, not empty.

  Returns `{:x [..] :y [..]}`: two vertices per distinct value - at the value, the previous level and then its cumulative share - starting at `(min, 0)` and ending at `(max, 1.0)` exactly.

  See also [[compute]]."
  [values]
  (let [d (r/distribution :real-discrete-distribution {:data values})
        xs (vec (sort (distinct values)))
        last-x (double (peek xs))
        fs (mapv (fn [^double x] (if (m/== x last-x) 1.0 (r/cdf d x))) xs)]
    (reduce (fn [{:keys [x y]} [xi fi prev]]
              {:x (conj x xi xi) :y (conj y prev fi)})
            {:x [] :y []}
            (map vector xs fs (cons 0.0 fs)))))

(defn compute
  "Computes the ECDF chart's stats.

  Parameters:

  - `chart` (map): the chart value - `:options` (`:padding`, style options).
  - `chart-split` (map): `:groups` (`{panel rows}`) and the merged `:mapping`.
  - `env` (map): the compute environment - `:theme`, `:plot-scales` ...

  Returns `{:data :extents :padding :padding-explicit :na :dropped :axis-titles}`: per panel `{:steps [{:x [..] :y [..] :aesthetics}] :style st}` (a group without valid values has no steps); x extents - the step vertices, y `[0 1]`; `:dropped` `:x` - rows without a finite x; the y axis titled `Empirical CDF`.

  Throws `ex-info {:key :mapping}` when rows exist but none has a finite x.

  See also [[steps]]."
  [{:keys [options] :as chart} {:keys [groups mapping]} {:keys [theme] :as env}]
  (let [st (theme/resolved-style theme chart)
        {:keys [panels dropped]} (group/split groups mapping env)
        x-of (data/row-accessor :x (:x mapping))
        ;; per group: valid x values and the count of invalid ones
        xs (update-vals panels (fn [gs] (mapv #(data/finite-numbers (map x-of (:rows %))) gs)))
        row-count (long (reduce m/long-add (map count (mapcat #(map :rows %) (vals panels)))))
        x-dropped (long (reduce m/long-add (map :dropped (mapcat identity (vals xs)))))
        data (into {} (map (fn [[panel gs]]
                             [panel {:steps (vec (keep (fn [[g {:keys [values]}]]
                                                         (when (seq values)
                                                           (assoc (steps values) :aesthetics (:aesthetics g))))
                                                       (map vector gs (xs panel))))
                                     :style st}]))
                   panels)]
    (when (and (m/pos? row-count) (m/== row-count x-dropped))
      (schema/fail! data/no-valid-values {:key :mapping :value :x :rows row-count}))
    {:data data
     :extents (merge-with merge (group/aesthetic-extents panels (:plot-scales env))
                          (into {} (keep (fn [[panel {panel-steps :steps}]]
                                           (when (seq panel-steps)
                                             [panel {:x (data/extent :x (mapcat :x panel-steps))
                                                     :y {:kind :numeric :min 0.0 :max 1.0}}])))
                                data))
     :padding (merge data/default-padding (:padding options))
     :padding-explicit (data/explicit-padding {} options)
     :na (group/na-dims panels)
     :dropped (assoc dropped :x x-dropped)
     :axis-titles {:y "Empirical CDF"}}))

(defn draw
  "Draws the step lines of one panel.

  Parameters:

  - `_chart` (map): the chart value (unused).
  - `computed` (map): the panel's stats - `:steps`, `:style`.
  - `canvas`: the clojure2d canvas.
  - `_box` (map): the panel box (unused).
  - `scales` (map): finalized scales.
  - `env` (map): the draw environment - `:theme`.

  Draws each group's steps in its group style (see [[cljplot2.data.group/style]]); a group outside its scale's domain is skipped, a non-finite projected vertex breaks the line."
  [_chart {group-steps :steps style :style} canvas _box scales env]
  (when (seq group-steps)
    (let [projection (scale/projection scales)]
      (doseq [{:keys [x y aesthetics]} group-steps
              :let [st (group/style style aesthetics scales (:theme env))]
              :when st]
        (let [points (mapv projection x y)]
          (polyline/draw! canvas (mapv #(.x ^Vec2 %) points) (mapv #(.y ^Vec2 %) points) st))))))

(defn legend
  "Draws the ECDF's key glyph: a horizontal line through the middle of a legend key.

  Parameters:

  - `chart` (map): the chart value.
  - `attributes` (map): the key's mapped style attributes - `:color` and `:dash` are used.
  - `canvas`: the clojure2d canvas.
  - `box` (map): the key box in pixels.
  - `env` (map): the legend environment - `:theme`.

  See also [[cljplot2.graphics.polyline/key-glyph!]]."
  [chart attributes canvas box env]
  (polyline/key-glyph! canvas box (merge (theme/resolved-style (:theme env) chart) (select-keys attributes [:color :dash])) :none))
