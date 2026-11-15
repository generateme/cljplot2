(ns cljplot2.chart.ecdf
  "ECDF: the empirical cumulative distribution of `:x` per panel and group, drawn as a step line from 0 to 1.

  Uses fastmath's `:real-discrete-distribution` `cdf`; the value at the largest data value is exactly 1.0.
  Options (style defaults from theme `[:chart :ecdf]`): `:color`, `:width`, `:dash`,
  `:padding {:x [lo hi] :y [lo hi]}` (default 5% each side).
  Groups (see [[cljplot2.data.group]]) come from ordinal aesthetics (`:color` colors the line) and `:group`."
  (:require [fastmath.random :as r]
            [cljplot2.data.rows :as data]
            [cljplot2.data.group :as group]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def schemas
  "Schemas of the ECDF chart: `:style` (theme `[:chart :ecdf]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] polyline/style-entries)
   :options (-> [:map {:closed true}]
                (into polyline/style-entries)
                (into data/common-options)
                (conj (schema/optional :padding data/position-padding)))
   :mapping [:map [:x schema/accessor]]})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :ecdf]) (select-keys options [:color :width :dash])))

(defn steps
  "Step-line vertices of the ECDF of `values` (finite doubles, not empty): starting at `(min, 0)`, rising at every
  distinct value, ending at `(max, 1.0)`."
  [values]
  (let [d (r/distribution :real-discrete-distribution {:data values})
        xs (vec (sort (distinct values)))
        last-x (peek xs)
        fs (mapv #(if (= % last-x) 1.0 (double (r/cdf d %))) xs)]
    (reduce (fn [{:keys [x y]} [xi fi prev]]
              {:x (conj x xi xi) :y (conj y prev fi)})
            {:x [] :y []}
            (map vector xs fs (cons 0.0 fs)))))

(defn compute
  "Per panel: `{:steps [{:x [..] :y [..] :aesthetics}] :style style}`; extents (y `[0 1]`), padding, `:na`, `:dropped`,
  `:axis-titles`."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme] :as env}]
  (let [st (style options theme)
        {:keys [panels dropped]} (group/split groups mapping env)
        x-of (data/row-accessor :x (:x mapping))
        ;; per group: valid x values and the count of invalid ones
        xs (update-vals panels (fn [gs] (mapv #(data/finite-numbers (map x-of (:rows %))) gs)))
        row-count (long (reduce + 0 (map count (mapcat #(map :rows %) (vals panels)))))
        x-dropped (long (reduce + 0 (map :dropped (mapcat identity (vals xs)))))
        data (into {} (map (fn [[panel gs]]
                             [panel {:steps (vec (keep (fn [[g {:keys [values]}]]
                                                         (when (seq values)
                                                           (assoc (steps values) :aesthetics (:aesthetics g))))
                                                       (map vector gs (xs panel))))
                                     :style st}]))
                   panels)]
    (when (and (pos? row-count) (= row-count x-dropped))
      (schema/fail! "No valid values for a mapped dimension - check the mapping." {:key :mapping :value :x :rows row-count}))
    {:data data
     :extents (merge-with merge (group/aesthetic-extents panels (:plot-scales env))
                          (into {} (keep (fn [[panel {:keys [steps]}]]
                                           (when (seq steps)
                                             [panel {:x (data/extent :x (mapcat :x steps))
                                                     :y {:kind :numeric :min 0.0 :max 1.0}}])))
                                data))
     :padding (merge {:x [0.05 0.05] :y [0.05 0.05]} (:padding options))
     :padding-explicit (data/explicit-padding {} options)
     :na (group/na-dims panels)
     :dropped (assoc dropped :x x-dropped)
     :axis-titles {:y "Empirical CDF"}}))

(defn draw
  "Draw the step lines of one panel."
  [_chart {:keys [steps style]} canvas _box scales env]
  (when (seq steps)
    (let [projection (scale/projection scales)]
      (doseq [{:keys [x y aesthetics]} steps
              :let [st (group/style style aesthetics scales (:theme env))]
              :when st]
        (let [points (mapv projection x y)]
          (polyline/draw! canvas (mapv #(.x ^Vec2 %) points) (mapv #(.y ^Vec2 %) points) st))))))

(defn legend
  "Key glyph: a horizontal line through the middle of `box`."
  [{:keys [options]} attributes canvas box env]
  (polyline/key-glyph! canvas box (merge (style options (:theme env)) (select-keys attributes [:color :dash])) :none))
