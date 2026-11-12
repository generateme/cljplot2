(ns cljplot2.chart.density
  "Density: a kernel density estimate (fastmath) of `:x` per panel and group, drawn as a line.

  Options (style defaults from theme `[:chart :density]`):

  * `:kernel` - fastmath KDE kernel keyword (default `:gaussian`)
  * `:bandwidth` - a number or a fastmath selector keyword (default `:nrd0`, as R)
  * `:n` - evaluation points (default 512), `:cut` - the curve spans `[min - cut*h, max + cut*h]` (default 3, as R)
  * `:color`, `:width`, `:dash` (line), `:fill` (area under the curve, default `:none`)
  * `:padding {:x [lo hi] :y [lo hi]}` - default `{:x [0.05 0.05] :y [0 0.05]}`

  Groups (see [[cljplot2.group]]) come from ordinal aesthetics (`:color` colors the line) and `:group`; a group with
  fewer than 2 distinct values has no curve and is counted in `:dropped {:group n}`."
  (:require [fastmath.core :as m]
            [fastmath.kernel :as k]
            [cljplot2.data :as data]
            [cljplot2.group :as group]
            [cljplot2.polyline :as polyline]
            [cljplot2.scale :as scale]
            [cljplot2.theme :as theme]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- invalid [message data] (throw (ex-info message data)))

(defn- style
  [options theme]
  (let [st (merge (get-in theme [:chart :density]) (select-keys options [:color :width :dash :fill]))]
    (polyline/validate-style! :density st)
    (when (nil? (:fill st)) (invalid "Density :fill must be a color or :none." {:key :fill}))
    st))

(defn- validate-options!
  [{:keys [n cut padding] :as options}]
  (when-not (or (nil? n) (and (integer? n) (>= (long n) 2)))
    (invalid "Density :n must be an integer >= 2." {:key :n :value n}))
  (when-not (or (nil? cut) (and (number? cut) (>= (double cut) 0.0)))
    (invalid "Density :cut must be a non-negative number." {:key :cut :value cut}))
  (doseq [[d p] padding]
    (when-not (and (vector? p) (= 2 (count p)) (every? #(and (number? %) (>= (double %) 0.0)) p))
      (invalid "Density :padding must map dimensions to [lo hi] non-negative fractions." {:key :padding :scale d :value p}))))

(defn- curve
  "`{:x [..] :y [..]}` of the KDE of `values` (at least 2 distinct)."
  [values {:keys [kernel bandwidth n cut] :or {kernel :gaussian bandwidth :nrd0 n 512 cut 3}}]
  (let [{:keys [kde h]} (try (k/kernel-density kernel values {:bandwidth bandwidth} true)
                             (catch clojure.lang.ExceptionInfo e
                               (invalid (str "Density estimation failed: " (ex-message e)) {:key :density :cause (ex-data e)})))
        h (double h)
        lo (- (double (reduce min values)) (* (double cut) h))
        hi (+ (double (reduce max values)) (* (double cut) h))
        step (/ (- hi lo) (dec (long n)))
        xs (mapv #(+ lo (* step (double %))) (range n))]
    {:x xs :y (mapv #(double (kde %)) xs)}))

(defn compute
  "Per panel: `{:curves [{:x [..] :y [..] :aesthetics}] :style style}`; extents, padding, `:na`, `:dropped`
  (`:x` invalid values, `:group` groups without a curve), `:axis-titles`."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme] :as env}]
  (validate-options! options)
  (when-not (contains? mapping :x)
    (invalid "Density needs :x in its mapping." {:key :mapping :value :x}))
  (let [st (style options theme)
        {:keys [panels dropped]} (group/split groups mapping env)
        x-of (data/row-accessor :x (:x mapping))
        ;; per group: valid x values and the count of invalid ones
        xs (update-vals panels (fn [gs] (mapv #(data/finite-numbers (map x-of (:rows %))) gs)))
        x-results (mapcat identity (vals xs))
        row-count (long (reduce + 0 (map count (mapcat #(map :rows %) (vals panels)))))
        x-dropped (long (reduce + 0 (map :dropped x-results)))
        no-curve (count (filter #(< (count (distinct (:values %))) 2) x-results))
        data (into {} (map (fn [[panel gs]]
                             [panel {:curves (vec (keep (fn [[g {:keys [values]}]]
                                                          (when (>= (count (distinct values)) 2)
                                                            (assoc (curve values options) :aesthetics (:aesthetics g))))
                                                        (map vector gs (xs panel))))
                                     :style st}]))
                   panels)]
    (when (and (pos? row-count) (= row-count x-dropped))
      (invalid "No valid values for a mapped dimension - check the mapping." {:key :mapping :value :x :rows row-count}))
    {:data data
     :extents (merge-with merge (group/aesthetic-extents panels)
                          (into {} (keep (fn [[panel {:keys [curves]}]]
                                           (when (seq curves)
                                             [panel {:x (data/extent :x (mapcat :x curves))
                                                     :y {:kind :numeric :min 0.0 :max (double (reduce max 0.0 (mapcat :y curves)))}}])))
                                data))
     :padding (merge {:x [0.05 0.05] :y [0.0 0.05]} (:padding options))
     :na (group/na-dims panels)
     :dropped (assoc dropped :x x-dropped :group no-curve)
     :axis-titles {:y "Density"}}))

(defn draw
  "Draw the curves of one panel (fill under, then line)."
  [_chart {:keys [curves style]} canvas _box scales env]
  (when (seq curves)
    (let [fx (scale/forward-fn (:x scales))
          fy (scale/forward-fn (:y scales))
          zero (fy 0.0)]
      (doseq [{:keys [x y aesthetics]} curves
              :let [st (group/style style aesthetics scales (:theme env))]
              :when st]
        (let [px (mapv fx x) py (mapv fy y)]
          (polyline/fill-under! canvas px py zero (:fill st))
          (polyline/draw! canvas px py st))))))

(defn legend
  "Key glyph: a horizontal line through the middle of `box`."
  [{:keys [options]} attributes canvas {:keys [x y w h]} env]
  (let [st (merge (style options (:theme env)) (select-keys attributes [:color]))
        cy (+ (double y) (* 0.5 (double h)))]
    (polyline/draw! canvas [(double x) (+ (double x) (double w))] [cy cy] st)))
