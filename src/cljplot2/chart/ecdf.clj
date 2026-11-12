(ns cljplot2.chart.ecdf
  "ECDF: the empirical cumulative distribution of `:x` per panel and group, drawn as a step line from 0 to 1.

  Uses fastmath's `:real-discrete-distribution` `cdf`; the value at the largest data value is exactly 1.0.
  Options (style defaults from theme `[:chart :ecdf]`): `:color`, `:width`, `:dash`,
  `:padding {:x [lo hi] :y [lo hi]}` (default 5% each side).
  Groups (see [[cljplot2.group]]) come from ordinal aesthetics (`:color` colors the line) and `:group`."
  (:require [fastmath.random :as r]
            [cljplot2.data :as data]
            [cljplot2.group :as group]
            [cljplot2.polyline :as polyline]
            [cljplot2.scale :as scale]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- invalid [message data] (throw (ex-info message data)))

(defn- style
  [options theme]
  (doto (merge (get-in theme [:chart :ecdf]) (select-keys options [:color :width :dash]))
    (->> (polyline/validate-style! :ecdf))))

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
  (when-not (contains? mapping :x)
    (invalid "ECDF needs :x in its mapping." {:key :mapping :value :x}))
  (doseq [[d p] (:padding options)]
    (when-not (and (vector? p) (= 2 (count p)) (every? #(and (number? %) (>= (double %) 0.0)) p))
      (invalid "ECDF :padding must map dimensions to [lo hi] non-negative fractions." {:key :padding :scale d :value p})))
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
      (invalid "No valid values for a mapped dimension - check the mapping." {:key :mapping :value :x :rows row-count}))
    {:data data
     :extents (merge-with merge (group/aesthetic-extents panels)
                          (into {} (keep (fn [[panel {:keys [steps]}]]
                                           (when (seq steps)
                                             [panel {:x (data/extent :x (mapcat :x steps))
                                                     :y {:kind :numeric :min 0.0 :max 1.0}}])))
                                data))
     :padding (merge {:x [0.05 0.05] :y [0.05 0.05]} (:padding options))
     :na (group/na-dims panels)
     :dropped (assoc dropped :x x-dropped)
     :axis-titles {:y "Empirical CDF"}}))

(defn draw
  "Draw the step lines of one panel."
  [_chart {:keys [steps style]} canvas _box scales env]
  (when (seq steps)
    (let [fx (scale/forward-fn (:x scales))
          fy (scale/forward-fn (:y scales))]
      (doseq [{:keys [x y aesthetics]} steps
              :let [st (group/style style aesthetics scales (:theme env))]
              :when st]
        (polyline/draw! canvas (mapv fx x) (mapv fy y) st)))))

(defn legend
  "Key glyph: a horizontal line through the middle of `box`."
  [{:keys [options]} attributes canvas {:keys [x y w h]} env]
  (let [st (merge (style options (:theme env)) (select-keys attributes [:color]))
        cy (+ (double y) (* 0.5 (double h)))]
    (polyline/draw! canvas [(double x) (+ (double x) (double w))] [cy cy] st)))
