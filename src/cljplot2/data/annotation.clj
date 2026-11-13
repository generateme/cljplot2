(ns cljplot2.data.annotation
  "Annotation charts (`chart/rule`, `chart/rect`, `chart/text`): marks at given positions.

  Two input modes:

  * **literal** - positions as chart options (a position or a vector of them); drawn in every panel
    (`facet/all-panels`), rows are ignored
  * **data** - positions from rows (the chart's own `:data` or the plot's) through the chart's **own** `:mapping`:
    annotation charts inherit only the facet keys of the plot mapping (chart value `:inherit-mapping :facets`), so a
    plot mapping `{:x :yield}` never turns a literal rule into data rules; rows follow facets as for other charts.
    Literal options win over the mapping.

  A position is a number or a level (string / keyword) of the position scale - on a `:bands` scale a level stands for
  its band (a point at the band centre, an interval over its step). Positions enter the scale extents unless the
  option `:extent` is false."
  (:require [fastmath.core :as m]
            [cljplot2.data.facet :as facet]
            [cljplot2.data.group :as group]
            [cljplot2.data.rows :as data]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn position?
  "Is `v` a position: a finite number, a string or a keyword?"
  [v]
  (or (and (number? v) (m/valid-double? (double v))) (string? v) (keyword? v)))

(def Position
  "A position: a finite number or a level (string / keyword)."
  (schema/named-type ::position position? "should be a finite number or a level (string / keyword)"))

(defn positions-schema
  "Schema of a literal option: `item` or a non-empty vector of them."
  [item]
  [:or item [:vector {:min 1} item]])

(def options
  "Map entries of the options every annotation chart takes (besides [[cljplot2.data.rows/common-options]])."
  [(schema/nilable :extent schema/bool)])

(defn as-vector
  "Literal option `v` as a vector of items; `item?` - is `v` itself one item (else a vector of items)."
  [v item?]
  (cond (nil? v) [] (item? v) [v] :else (vec v)))

(defn broadcast
  "Vectors `vs` (each of 1 or `n` items, `n` the longest) as `n` tuples; `keys` - the option of each vector, for the
  error `ex-info {:key k}` of a length that is neither 1 nor `n`."
  [keys vs]
  (let [n (long (reduce max 0 (map count vs)))]
    (doseq [[k v] (map vector keys vs)
            :when (not (or (= 1 (count v)) (= n (count v))))]
      (schema/fail! (str "Annotation " k " should have 1 or " n " items.") {:key k :value v}))
    (vec (for [i (range n)] (mapv #(if (= 1 (count %)) (first %) (nth % i)) vs)))))

(defn literal?
  "Is the chart literal: some of its position `keys` set in `options`?"
  [options keys]
  (boolean (some #(some? (get options %)) keys)))

(defn extents
  "Extents `{dim extent}` of the positions `{dim [v ..]}` (non-positions such as `nil` or `[:panel f]` skipped),
  `{}` with `:extent false`. Mixed numbers and levels throw `ex-info {:key :extents}`."
  [options dim-values]
  (if (false? (:extent options))
    {}
    (into {} (keep (fn [[k vs]] (when-let [e (data/extent k (filter position? vs))] [k e]))) dim-values)))

(defn literal-stats
  "Stats of a literal annotation: `computed` drawn in every panel, `dim-values` - its positions (see [[extents]])."
  [computed options dim-values]
  (let [e (extents options dim-values)]
    {:data {facet/all-panels computed}
     :extents (if (seq e) {facet/all-panels e} {})
     :na #{}
     :dropped {}}))

(defn data-stats
  "Stats of a data annotation from `chart-split`: per panel `(panel-fn groups)` (groups of [[group/split]] by the
  chart's aesthetics), extents of `(dims-fn groups)` - `{dim [v ..]}` - plus the aesthetic extents."
  [chart-split options env panel-fn dims-fn]
  (let [{:keys [panels dropped]} (group/split (:groups chart-split) (:mapping chart-split) env)]
    {:data (update-vals panels panel-fn)
     :extents (merge-with merge
                          (group/aesthetic-extents panels)
                          (into {} (keep (fn [[panel gs]] (let [e (extents options (dims-fn gs))] (when (seq e) [panel e]))))
                                panels))
     :na (group/na-dims panels)
     :dropped dropped}))

(defn require-mapping!
  "Throw `ex-info {:key :mapping}` unless the chart `mapping` maps one of the key sets `alternatives`
  (`[[:x] [:y] ...]`, every key of a set mapped); `what` names the chart."
  [what mapping alternatives]
  (when-not (some (fn [ks] (every? #(some? (get mapping %)) ks)) alternatives)
    (schema/fail! (str what " needs literal positions in its options or position keys in its own :mapping "
                       (pr-str alternatives) ".")
                  {:key :mapping :value (select-keys mapping (distinct (apply concat alternatives)))})))

;; pixels

(defn- step
  ^double [finalized]
  (double (or (:step (scale/band-geometry finalized)) 0.0)))

(defn pixel
  "Pixel of position `v` on the finalized scale (a level - its band centre); `##NaN` when the scale can't place it
  or is missing."
  ^double [finalized v]
  (if finalized (scale/forward finalized v) ##NaN))

(defn interval-pixels
  "Pixel interval `[lo hi]` (lo <= hi) covering positions `a` and `b` on the finalized scale: a level covers its band
  step; `nil` - the scale's range end on that side (`a` - the low-value end, `b` - the high-value end). `nil` when
  an end can't be placed or the scale is missing."
  [finalized a b]
  (when finalized
   (let [[r0 r1] (:range finalized)
        half (* 0.5 (step finalized))
        ends (fn [v range-end]
               (cond
                 (nil? v) [(double range-end) (double range-end)]
                 (number? v) (let [p (pixel finalized v)] [p p])
                 :else (let [p (pixel finalized v)] [(- p half) (+ p half)])))
        ps (concat (ends a r0) (ends b r1))]
    (when (every? #(m/valid-double? (double %)) ps)
      [(double (reduce min ps)) (double (reduce max ps))]))))

(defn axis-pixel
  "Pixel along `finalized`'s axis of a text position: `[:panel f]` - fraction `f` of the range from its low-value
  end, else [[pixel]]; `##NaN` without a scale."
  ^double [finalized v]
  (cond
    (nil? finalized) ##NaN
    (and (vector? v) (= :panel (first v)))
    (let [[r0 r1] (:range finalized)]
      (+ (double r0) (* (double (second v)) (- (double r1) (double r0)))))
    :else (pixel finalized v)))
