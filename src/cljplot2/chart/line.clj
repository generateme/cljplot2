(ns cljplot2.chart.line
  "Line: points `(x, y)` joined per panel and group.

  Options (style defaults from theme `[:chart :line]`): `:sort` (default true - points ordered by numeric or temporal
  x, ties in data order; `false` keeps data order), `:color`, `:width`, `:dash`, `:padding {:x [lo hi] :y [lo hi]}`
  (default 5% each side).
  A row with an invalid x is dropped (counted); an invalid y breaks the line there (counted).
  Groups (see [[cljplot2.group]]) come from ordinal aesthetics (`:color` colors the line) and `:group`."
  (:require [cljplot2.data :as data]
            [cljplot2.group :as group]
            [cljplot2.polyline :as polyline]
            [cljplot2.scale :as scale]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- invalid [message data] (throw (ex-info message data)))

(defn- style
  [options theme]
  (doto (merge (get-in theme [:chart :line]) (select-keys options [:color :width :dash]))
    (->> (polyline/validate-style! :line))))

(defn- validate-options!
  [{:keys [padding] :as options}]
  (when-not (contains? #{nil true false} (:sort options))
    (invalid "Line :sort must be a boolean." {:key :sort :value (:sort options)}))
  (doseq [[d p] padding]
    (when-not (and (vector? p) (= 2 (count p)) (every? #(and (number? %) (>= (double %) 0.0)) p))
      (invalid "Line :padding must map dimensions to [lo hi] non-negative fractions." {:key :padding :scale d :value p}))))

(defn- sortable? [xs] (or (every? number? xs) (every? data/temporal? xs)))

(defn compute
  "Per panel: `{:lines [{:x [..] :y [..] :aesthetics}] :style style}` (`y` `nil` where the line breaks); extents,
  padding, `:na`, `:dropped`."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme] :as env}]
  (validate-options! options)
  (doseq [k [:x :y]]
    (when-not (contains? mapping k)
      (invalid "Line needs :x and :y in its mapping." {:key :mapping :value k})))
  (let [st (style options theme)
        {:keys [panels dropped]} (group/split groups mapping env)
        x-of (data/row-accessor :x (:x mapping))
        y-of (data/row-accessor :y (:y mapping))
        sort? (get options :sort true)
        rows (mapcat (fn [gs] (mapcat :rows gs)) (vals panels))
        row-count (count rows)
        _ (doseq [[k f] [[:x x-of] [:y y-of]]]
            (when (and (pos? row-count) (not-any? #(data/valid-value? (f %)) rows))
              (invalid "No valid values for a mapped dimension - check the mapping." {:key :mapping :value k :rows row-count})))
        data (update-vals panels
                          (fn [gs]
                            {:lines (mapv (fn [g]
                                            (let [pts (->> (:rows g)
                                                           (map (fn [r] [(x-of r) (y-of r)]))
                                                           (filter #(data/valid-value? (first %))))
                                                  pts (if (and sort? (sortable? (map first pts))) (sort-by first pts) pts)]
                                              {:x (mapv first pts)
                                               :y (mapv #(let [v (second %)] (when (data/valid-value? v) v)) pts)
                                               :aesthetics (:aesthetics g)}))
                                          gs)
                             :style st}))]
    {:data data
     :extents (merge-with merge (group/aesthetic-extents panels)
                          (into {} (keep (fn [[panel {:keys [lines]}]]
                                           (let [xs (mapcat :x lines) ys (remove nil? (mapcat :y lines))]
                                             (when (seq ys)
                                               [panel {:x (data/extent :x xs) :y (data/extent :y ys)}]))))
                                data))
     :padding (merge {:x [0.05 0.05] :y [0.05 0.05]} (:padding options))
     :na (group/na-dims panels)
     :dropped (assoc dropped
                     :x (count (remove #(data/valid-value? (x-of %)) rows))
                     :y (count (remove #(data/valid-value? (y-of %)) rows)))}))

(defn draw
  "Draw the lines of one panel; `nil` y values and values the scales can't place break a line."
  [_chart {:keys [lines style]} canvas _box scales env]
  (when (seq lines)
    (let [fx (scale/forward-fn (:x scales))
          fy (scale/forward-fn (:y scales))]
      (doseq [{:keys [x y aesthetics]} lines
              :let [st (group/style style aesthetics scales (:theme env))]
              :when st]
        (polyline/draw! canvas (mapv fx x) (mapv #(if (nil? %) ##NaN (fy %)) y) st)))))

(defn legend
  "Key glyph: a horizontal line through the middle of `box`."
  [{:keys [options]} attributes canvas {:keys [x y w h]} env]
  (let [st (merge (style options (:theme env)) (select-keys attributes [:color]))
        cy (+ (double y) (* 0.5 (double h)))]
    (polyline/draw! canvas [(double x) (+ (double x) (double w))] [cy cy] st)))
