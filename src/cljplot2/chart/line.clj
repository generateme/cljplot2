(ns cljplot2.chart.line
  "Line: points `(x, y)` joined per panel and group.

  Options (style defaults from theme `[:chart :line]`): `:sort` (default true - points ordered by numeric or temporal
  x, ties in data order; `false` keeps data order), `:color`, `:width`, `:dash`, `:fill` (area chart: the area
  between the line and y = 0 - or the nearer panel edge when 0 is outside the y range - default `:none`),
  `:padding {:x [lo hi] :y [lo hi]}` (default 5% each side).
  A row with an invalid x is dropped (counted); an invalid y breaks the line there (counted).
  Groups (see [[cljplot2.data.group]]) come from ordinal aesthetics (`:color` colors the line) and `:group`."
  (:require [fastmath.core :as m]
            [cljplot2.data.rows :as data]
            [cljplot2.data.group :as group]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private style-entries
  (conj polyline/style-entries (schema/optional :fill schema/color)))

(def Schema
  "Schemas of the line chart: `:style` (theme `[:chart :line]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (conj (schema/nilable :sort schema/bool)
                      (schema/optional :padding data/position-padding)))
   :mapping [:map [:x schema/accessor] [:y schema/accessor]]})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :line]) (select-keys options [:color :width :dash :fill])))

(defn- sortable? [xs] (or (every? number? xs) (every? data/temporal? xs)))

(defn compute
  "Per panel: `{:lines [{:x [..] :y [..] :aesthetics}] :style style}` (`y` `nil` where the line breaks); extents,
  padding, `:na`, `:dropped`."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme] :as env}]
  (let [st (style options theme)
        {:keys [panels dropped]} (group/split groups mapping env)
        x-of (data/row-accessor :x (:x mapping))
        y-of (data/row-accessor :y (:y mapping))
        sort? (get options :sort true)
        rows (mapcat (fn [gs] (mapcat :rows gs)) (vals panels))
        row-count (count rows)
        _ (doseq [[k f] [[:x x-of] [:y y-of]]]
            (when (and (pos? row-count) (not-any? #(data/valid-value? (f %)) rows))
              (schema/fail! "No valid values for a mapped dimension - check the mapping." {:key :mapping :value k :rows row-count})))
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
  "Draw the lines of one panel (area fill first, if any); `nil` y values and values the scales can't place break a
  line. The fill baseline is y = 0 clamped into the panel (`box`) along the y scale's direction."
  [_chart {:keys [lines style]} canvas {:keys [w h]} scales env]
  (when (seq lines)
    (let [projection (scale/projection scales)
          y-scale (:y scales)
          y-horizontal? (scale/horizontal? :y y-scale)
          zero (scale/forward y-scale 0.0)
          ;; y = 0 clamped into the panel along the y scale's direction; a scale without 0 - its range start
          baseline (if (m/nan? zero)
                     (double (first (:range y-scale)))
                     (m/constrain zero 0.0 (double (if y-horizontal? w h))))]
      (doseq [{:keys [x y aesthetics]} lines
              :let [st (group/style style aesthetics scales (:theme env))]
              :when st]
        (let [points (mapv (fn [vx vy] (if (nil? vy) (Vec2. ##NaN ##NaN) (projection vx vy))) x y)
              px (mapv #(.x ^Vec2 %) points)
              py (mapv #(.y ^Vec2 %) points)]
          (polyline/fill-under! canvas px py
                                (if y-horizontal? (vec (repeat (count px) baseline)) px)
                                (if y-horizontal? py (vec (repeat (count py) baseline)))
                                (:fill st))
          (polyline/draw! canvas px py st))))))

(defn legend
  "Key glyph: a horizontal line through the middle of `box`."
  [{:keys [options]} attributes canvas {:keys [x y w h]} env]
  (let [st (merge (style options (:theme env)) (select-keys attributes [:color]))
        cy (+ (double y) (* 0.5 (double h)))]
    (polyline/draw! canvas [(double x) (+ (double x) (double w))] [cy cy] st)))
