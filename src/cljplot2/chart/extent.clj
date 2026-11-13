(ns cljplot2.chart.extent
  "Extent: an interval of the `:x` values per `:y` level and group, drawn as a line with end caps and a centre mark -
  a strip chart (see [[cljplot2.data.strip]]). Intervals come from the fastmath.stats extents.

  Options (style defaults from theme `[:chart :extent]`): `:color`, `:line-width` (`:width` is the strip option), `:shape`, `:size` (centre
  mark); `:method` - a keyword of [[extent-methods]] or a function of the values returning `[lo hi]` or `[lo hi centre]`
  (default `:min-max`); `:method-args` - extra arguments passed after the values (e.g. `[10.0 90.0]` for
  `:percentile`, `[0.01]` for `:ci`; default none); `:center` - `:method` (the method's centre, default) or `:none`;
  `:padding {:x [lo hi]}` (default `[0.05 0.05]`), plus the strip options and `:data` / `:mapping`."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.stats :as stats]
            [cljplot2.data.group :as group]
            [cljplot2.data.rows :as data]
            [cljplot2.data.strip :as strip]
            [cljplot2.graphics.shape :as shape]
            [cljplot2.schema :as schema])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def extent-methods
  "Extent methods: keyword -> fastmath.stats function of the values (and `:method-args`) returning
  `[lo hi centre]`."
  {:min-max stats/extent
   :stddev stats/stddev-extent
   :mad stats/mad-extent
   :sem stats/sem-extent
   :percentile stats/percentile-extent
   :quantile stats/quantile-extent
   :pi stats/pi-extent
   :hpdi stats/hpdi-extent
   :adjacent stats/adjacent-values
   :inner-fence stats/inner-fence-extent
   :outer-fence stats/outer-fence-extent
   :ci stats/ci
   :bootstrap-ci stats/bootstrap-ci
   :percentile-bc stats/percentile-bc-extent
   :percentile-bca stats/percentile-bca-extent})

(def ^:private style-entries
  [(schema/optional :color schema/color)
   (schema/optional :line-width schema/non-neg)
   (schema/optional :shape schema/shape)
   (schema/optional :size schema/non-neg)])

(def Schema
  "Schemas of the extent chart: `:style` (theme `[:chart :extent]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into strip/options)
                (conj (schema/optional :method [:or (into [:enum] (sort (keys extent-methods))) schema/function])
                      (schema/nilable :method-args [:sequential any?])
                      (schema/optional :center [:enum :method :none])
                      (schema/optional :padding data/position-padding)))
   :mapping [:map [:x schema/accessor]]})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :extent]) (select-keys options [:color :line-width :shape :size])))

(defn interval
  "`{:lo :hi :center}` of `values` by `method` (keyword of [[extent-methods]] or a function) with `method-args`; `:center`
  `nil` when the method gives none. Throws `ex-info {:key :method}` for a result that isn't 2 or 3 numbers."
  [values method method-args]
  (let [f (if (keyword? method) (extent-methods method) method)
        [lo hi c :as result] (apply f values method-args)]
    (when-not (and (sequential? result) (<= 2 (count result) 3) (every? number? result))
      (schema/fail! "Extent :method should return [lo hi] or [lo hi centre]." {:key :method :value result}))
    {:lo (double lo) :hi (double hi) :center (some-> c double)}))

(defn compute
  "Per panel: `{:items [..] :style st :place p :options o}`, items with [[interval]]; extents (x - the intervals and
  centres), padding, `:na`, `:dropped`."
  [{:keys [options]} chart-split {:keys [theme] :as env}]
  (let [{:keys [place items panels dropped]} (strip/split chart-split options :center env)
        method (get options :method :min-max)
        items (update-vals items (fn [its] (mapv #(merge % (interval (:values %) method (:method-args options))) its)))]
    {:data (update-vals items (fn [its] {:items its :style (style options theme) :place place :options options}))
     :extents (merge-with merge (group/aesthetic-extents panels)
                          (strip/extents place items (fn [{:keys [lo hi center]}]
                                                       [(m/min (double lo) (double (or center lo)))
                                                        (m/max (double hi) (double (or center hi)))])))
     :padding (merge {:x [0.05 0.05]} (:padding options))
     :na (group/na-dims panels)
     :dropped dropped}))

(defn- line! [canvas ^Vec2 a ^Vec2 b] (c2d/line canvas (.x a) (.y a) (.x b) (.y b)))

(defn draw
  "Draw the intervals of one panel: the line, end caps (half the item's cross range) and the centre mark."
  [_chart {:keys [items style place options]} canvas box scales env]
  (let [{:keys [point]} (strip/placement box scales options place)
        center? (not= :none (get options :center :method))]
    (doseq [{:keys [level slot lo hi center aesthetics]} items
            :let [st (group/style style aesthetics scales (:theme env))
                  c0 (strip/in-slot slot 0.0)
                  cap (* 0.5 (strip/slot-half slot))
                  p (fn [v cross] (point v level cross))
                  ^Vec2 a (p lo c0)]
            :when (and st (m/valid-double? (.x a)) (m/valid-double? (.y a)))]
      (c2d/set-color canvas (:color st))
      (c2d/set-stroke-custom canvas {:size (double (:line-width st)) :cap :butt})
      (line! canvas a (p hi c0))
      (line! canvas (p lo (- c0 cap)) (p lo (+ c0 cap)))
      (line! canvas (p hi (- c0 cap)) (p hi (+ c0 cap)))
      (when (and center? center)
        (shape/draw! canvas (:shape st) (p center c0) (assoc st :stroke-color :none))))))

(defn legend
  "Key glyph: a horizontal capped line with the centre mark."
  [{:keys [options]} attributes canvas {:keys [x y w h]} env]
  (let [st (merge (style options (:theme env)) (select-keys attributes [:color]))
        x0 (+ (double x) (* 0.15 (double w))) x1 (+ (double x) (* 0.85 (double w)))
        cy (+ (double y) (* 0.5 (double h))) cap (* 0.25 (double h))]
    (c2d/set-color canvas (:color st))
    (c2d/set-stroke-custom canvas {:size (m/max 1.0 (double (:line-width st))) :cap :butt})
    (c2d/line canvas x0 cy x1 cy)
    (c2d/line canvas x0 (- cy cap) x0 (+ cy cap))
    (c2d/line canvas x1 (- cy cap) x1 (+ cy cap))
    (shape/draw! canvas (:shape st) (Vec2. (* 0.5 (+ x0 x1)) cy) (assoc st :size (m/min (double (:size st)) (* 0.6 (double h)))
                                                                        :stroke-color :none))))
