(ns cljplot2.chart.histogram
  "Histogram: counts of `:x` values in bins shared by all panels and groups (lattice-style common breaks).

  Options (style defaults from theme `[:chart :histogram]`):

  * `:bins` - a fastmath bin-estimation method keyword (`:sturges` default, `:sqrt`, `:rice`, `:doane`, `:scott`,
    `:freedman-diaconis`; computed on all data), a positive integer (equal-width bins over the data range), or a
    vector of at least 2 increasing breaks (values outside are dropped and counted)
  * `:y` - bar height: `:percent` (default, of the panel's total, or the group's when grouped), `:count`, `:pmf`
    (share of the total, sums to 1), `:density` (count / (n * width), area 1)
  * `:position` - grouped bars: `:overlay` (default; full-width, fill alpha `:alpha`), `:dodge` (bins split among the
    groups), `:mirror` (exactly two groups, the second drawn downward; y tick labels show magnitudes)
  * `:symmetric` - mirror only: `true` ties both halves to one y range `[-M M]`, `M` the tallest bar of either group
    over all panels, so the halves compare visually (default false)
  * `:color` (fill; the stroke is the darkened fill), `:stroke-width`, `:alpha` (overlay fill opacity, 0..1)
  * `:padding {:x [lo hi] :y [lo hi]}` - default `{:x [0.05 0.05] :y [0 0.05]}` (mirror: y `[0.05 0.05]`); bars
    stand on the panel's bottom edge, x padding keeps end ticks off facet panel joints

  Groups (see [[cljplot2.data.group]]) come from ordinal aesthetics (`:color` fills the bars) and `:group`.
  A single distinct value gets one bin of width 1 centred on it."
  (:require [clojure2d.color :as c]
            [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.stats :as stats]
            [cljplot2.data.rows :as data]
            [cljplot2.data.group :as group]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private measures {:percent "Percent of Total" :count "Count" :pmf "Probability" :density "Density"})
(def ^:private positions #{:overlay :dodge :mirror})

(def ^:private style-entries
  [(schema/optional :color schema/solid-color)
   (schema/optional :stroke-width schema/non-neg)
   (schema/optional :alpha schema/fraction)])

(def ^:private breaks-schema
  [:and [:vector {:min 2} schema/finite]
   (schema/rule #(when-not (apply < %) "breaks should be increasing"))])

(def Schema
  "Schemas of the histogram: `:style` (theme `[:chart :histogram]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (conj (schema/optional :bins [:multi {:dispatch #(cond (keyword? %) :method (vector? %) :breaks :else :count)}
                                              [:method keyword?] [:breaks breaks-schema] [:count schema/pos-int]])
                      (schema/optional :y (into [:enum] (keys measures)))
                      (schema/optional :position (into [:enum] (sort positions)))
                      (schema/nilable :symmetric schema/bool)
                      (schema/optional :padding data/position-padding)))
   :mapping [:map [:x schema/accessor]]})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :histogram]) (select-keys options [:color :stroke-width :alpha])))

(defn- equal-breaks
  [^double lo ^double hi ^long n]
  (let [step (/ (- hi lo) n)]
    (conj (mapv #(+ lo (* step (double %))) (range n)) hi)))

(defn breaks
  "Bin breaks for all `values` (doubles) and the `:bins` option; `nil` for no values."
  [values bins]
  (cond
    (vector? bins) (mapv double bins)
    (empty? values) nil
    :else (let [lo (double (reduce min values))
                hi (double (reduce max values))]
            (if (== lo hi)
              [(- lo 0.5) (+ lo 0.5)]
              (let [n (long (if (keyword? bins)
                              (try (long (stats/estimate-bins values bins))
                                   (catch IllegalArgumentException _
                                     (schema/fail! "Unknown histogram :bins method." {:key :bins :value bins})))
                              (long bins)))]
                (equal-breaks lo hi (m/max 1 n)))))))

(defn- bin-counts
  "Counts of `values` per bin of `brks` (last bin closed) and the number outside."
  [brks values]
  (let [arr (double-array brks)
        n (dec (alength arr))
        lo (aget arr 0)
        hi (aget arr n)
        counts (long-array n)]
    (doseq [v values
            :let [v (double v)]]
      (when (<= lo v hi)
        (let [i (java.util.Arrays/binarySearch arr v)
              bin (if (neg? i) (- (- i) 2) (m/min i (dec n)))]
          (aset counts bin (inc (aget counts bin))))))
    {:counts (vec counts) :outside (- (count values) (long (reduce + 0 (vec counts))))}))

(defn- heights
  "Bar heights of `counts` (bins `brks`) for measure `y`."
  [y brks counts]
  (let [total (double (reduce + 0 counts))]
    (mapv (fn [^long cnt [^double a ^double b]]
            (if (zero? total)
              0.0
              (case y
                :count (double cnt)
                :percent (/ (* 100.0 cnt) total)
                :pmf (/ cnt total)
                :density (/ cnt (* total (- b a))))))
          counts (partition 2 1 brks))))

(defn compute
  "Per panel: `{:bars [{:x0 :x1 :y0 :y1 :aesthetics}] :style style :translucent? bool}` in data units (bars in group
  order; `:translucent?` - grouped overlay, fill drawn with `:alpha`); `:tick-format {:y :absolute}` for mirror; extents
  (x - outer breaks, y - from 0, or `[-max2 max1]` for mirror), padding, `:na`, `:dropped`, `:axis-titles`."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme] :as env}]
  (let [st (style options theme)
        y (get options :y :percent)
        position (get options :position :overlay)
        {:keys [panels dims dropped]} (group/split groups (dissoc mapping :y) env)
        x-of (data/row-accessor :x (:x mapping))
        xs (update-vals panels (fn [gs] (mapv #(data/finite-numbers (map x-of (:rows %))) gs)))
        all-values (into [] (comp (mapcat identity) (mapcat :values)) (vals xs))
        all-count (long (reduce + 0 (map count (mapcat #(map :rows %) (vals panels)))))
        _ (when (and (pos? all-count) (empty? all-values))
            (schema/fail! "No valid values for a mapped dimension - check the mapping." {:key :mapping :value :x :rows all-count}))
        brks (breaks all-values (get options :bins :sturges))
        group-keys (group/ordered-keys (mapcat #(map :key %) (vals panels)) dims (:plot-scales env))
        slot (zipmap group-keys (range))
        n-groups (count group-keys)
        _ (when (and (= position :mirror) (not= 2 n-groups) (seq all-values))
            (schema/fail! "Histogram :position :mirror needs exactly two groups." {:key :position :groups n-groups}))
        outside (if brks
                  (let [lo (double (first brks)) hi (double (peek brks))]
                    (count (remove #(<= lo (double %) hi) all-values)))
                  0)
        bars-of (fn [g {:keys [values]}]
                  (let [{:keys [counts]} (bin-counts brks values)
                        hs (heights y brks counts)
                        i (long (slot (:key g)))
                        down? (and (= position :mirror) (= i 1))]
                    (mapv (fn [[^double a ^double b] ^double h]
                            (let [[a b] (if (= position :dodge)
                                          (let [w (/ (- b a) n-groups)] [(+ a (* i w)) (+ a (* (inc i) w))])
                                          [a b])]
                              {:x0 a :x1 b :y0 0.0 :y1 (if down? (- h) h) :aesthetics (:aesthetics g)}))
                          (partition 2 1 brks) hs)))
        data (if brks
               (into {} (map (fn [[panel gs]]
                               [panel {:bars (vec (mapcat bars-of gs (xs panel))) :style st
                                       :translucent? (and (> n-groups 1) (= position :overlay))}]))
                     panels)
               {})
        bar-extents (update-vals data (fn [{:keys [bars]}]
                                        (when (seq bars)
                                          {:x {:kind :numeric :min (first brks) :max (peek brks)}
                                           :y {:kind :numeric
                                               :min (reduce min 0.0 (map :y1 bars))
                                               :max (reduce max 0.0 (map :y1 bars))}})))
        tallest (double (reduce max 0.0 (map #(m/abs (double (:y1 %))) (mapcat :bars (vals data)))))
        bar-extents (if (and (= position :mirror) (:symmetric options))
                      (update-vals bar-extents #(some-> % (assoc :y {:kind :numeric :min (- tallest) :max tallest})))
                      bar-extents)
        aesthetic-extents (group/aesthetic-extents panels)]
    {:data data
     :extents (merge-with merge aesthetic-extents (into {} (filter val) bar-extents))
     :padding (merge {:x [0.05 0.05] :y (if (= position :mirror) [0.05 0.05] [0.0 0.05])} (:padding options))
     :na (group/na-dims panels)
     :dropped (assoc dropped :x (+ (long (reduce + 0 (map :dropped (mapcat identity (vals xs))))) (long outside)))
     :axis-titles {:y (measures y)}
     :tick-format (when (= position :mirror) {:y :absolute})}))

(defn draw
  "Draw the bars of one panel; grouped overlay bars get fill alpha `:alpha`; empty bins and bars of groups outside
  an aesthetic domain are skipped."
  [_chart {:keys [bars style translucent?]} canvas _box scales env]
  (when (seq bars)
    (let [fx (scale/forward-fn (:x scales))
          fy (scale/forward-fn (:y scales))
          stroke-width (double (:stroke-width style))]
      (c2d/set-stroke-custom canvas {:size stroke-width :join :miter})
      (doseq [{:keys [x0 x1 y0 y1 aesthetics]} bars
              :when (not (== (double y0) (double y1)))
              :let [st (group/style style aesthetics scales (:theme env))]
              :when st]
        (let [px0 (m/round (fx x0)) px1 (m/round (fx x1))
              py0 (m/round (fy y0)) py1 (m/round (fy y1))
              left (m/min px0 px1) top (m/min py0 py1)
              w (m/abs (- px1 px0)) h (m/abs (- py1 py0))
              fill (:color st)]
          (c2d/set-color canvas (if translucent?
                                  (c/set-alpha fill (* 255.0 (double (:alpha style))))
                                  fill))
          (c2d/rect canvas left top w h)
          (when (pos? stroke-width)
            (c2d/set-color canvas (c/darken fill))
            (c2d/rect canvas left top w h true)))))))

(defn legend
  "Key glyph: a filled square (darkened stroke) inset in `box`; with a mapped `:color` and `:overlay` position (the
  groups' bars are translucent) the fill gets `:alpha` too."
  [{:keys [options]} attributes canvas {:keys [x y w h]} env]
  (let [st (merge (style options (:theme env)) (select-keys attributes [:color]))
        translucent? (and (contains? attributes :color) (= :overlay (get options :position :overlay)))
        side (* 0.7 (m/min (double w) (double h)))
        cx (+ (double x) (* 0.5 (double w)))
        cy (+ (double y) (* 0.5 (double h)))]
    (c2d/set-color canvas (if translucent? (c/set-alpha (:color st) (* 255.0 (double (:alpha st)))) (:color st)))
    (c2d/crect canvas cx cy side side)
    (when (pos? (double (:stroke-width st)))
      (c2d/set-stroke-custom canvas {:size (double (:stroke-width st))})
      (c2d/set-color canvas (c/darken (:color st)))
      (c2d/crect canvas cx cy side side true))))
