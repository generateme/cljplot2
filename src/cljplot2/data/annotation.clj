(ns cljplot2.data.annotation
  "Positions, extents and pixels of the annotation charts (`chart/rule`, `chart/rect`, `chart/text`) - marks at given positions.

  Two input modes:

  * **literal** - positions as chart options (a position or a vector of them); drawn in every panel
    (`facet/all-panels`), rows are ignored
  * **data** - positions from rows (the chart's own `:data` or the plot's) through the chart's **own** `:mapping`:
    annotation charts inherit only the facet keys of the plot mapping (chart value `:inherit-mapping :facets`), so a
    plot mapping `{:x :yield}` never turns a literal rule into data rules; rows follow facets as for other charts.
    Literal options win over the mapping.

  A position is a number, a `java.time` value (on a datetime scale) or a level (string / keyword) of the position
  scale - on a `:bands` scale a level stands for
  its band (a point at the band centre, an interval over its step). Positions enter the scale extents unless the
  option `:extent` is false.

  Functions: positions - [[position?]], [[position]], [[positions-schema]], [[as-vector]], [[broadcast]], [[literal?]]; stats - [[extents]], [[literal-stats]], [[data-stats]], [[require-mapping!]]; pixels - [[pixel-fn]], [[interval-fn]], [[axis-pixel-fn]]; the shared option group [[options]]."
  (:require [fastmath.core :as m]
            [fastmath.vector :as v]
            [cljplot2.data.facet :as facet]
            [cljplot2.data.group :as group]
            [cljplot2.data.rows :as data]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn position?
  "Tests whether a value is an annotation position.

  Parameters:

  - `v`: any value.

  Returns true for a finite number, a `java.time` value, a string or a keyword (a level), else false.

  See also [[position]]."
  [v]
  (or (and (number? v) (m/valid-double? (double v)))
      (data/temporal? v)
      (string? v)
      (keyword? v)))

(def position
  "A position: a finite number, a `java.time` value or a level (string / keyword)."
  (schema/named-type ::position position? "should be a finite number, a java.time value or a level (string / keyword)"))

(defn positions-schema
  "Schema of a literal position option: one item or a non-empty vector of them.

  Parameters:

  - `item`: the schema of one item, e.g. [[position]].

  Returns a malli schema."
  [item]
  [:or item [:vector {:min 1} item]])

(def options
  "Map entries of the options every annotation chart takes (besides [[cljplot2.data.rows/common-options]])."
  (schema/in-fragment
   :annotation
   [(schema/nilable :extent {:description "include the positions in the scale extents"
                             :default true}
                    schema/bool)]))

(defn as-vector
  "Literal option value as a vector of items.

  Parameters:

  - `v`: the option value - one item, a sequence of items or `nil`.
  - `item?` (function): predicate of a single item.

  Returns `[]` for `nil`, `[v]` for one item, else `v` as a vector.

  See also [[broadcast]]."
  [v item?]
  (cond (nil? v) [] (item? v) [v] :else (vec v)))

(defn broadcast
  "Zips literal option vectors into tuples, repeating single items.

  Parameters:

  - `keys` (sequence of keywords): the option of each vector, for errors.
  - `vs` (sequence of vectors): each of 1 or `n` items, `n` the longest length.

  Returns a vector of `n` tuples, the i-th taking the i-th item of every vector (a single item - in every tuple); `[]` when every vector is empty.

  Throws `ex-info {:key k}` for a vector whose length is neither 1 nor `n`.

  See also [[as-vector]]."
  [keys vs]
  (let [n (long (reduce max 0 (map count vs)))]
    (doseq [[k v] (map vector keys vs)
            :when (not (or (m/one? (count v)) (m/== n (count v))))]
      (schema/fail! (str "Annotation " k " should have 1 or " n " items.") {:key k :value v}))
    (vec (for [i (range n)] (mapv #(if (m/one? (count %)) (first %) (nth % i)) vs)))))

(defn literal?
  "Tests whether an annotation chart is literal (positions in its options rather than rows).

  Parameters:

  - `options` (map): the chart's options.
  - `keys` (sequence of keywords): its position options.

  Returns a truthy value when some of `keys` is set (not `nil`) in `options`, else `nil`."
  [options keys]
  (some #(some? (get options %)) keys))

(defn extents
  "Scale extents of an annotation's positions.

  Parameters:

  - `options` (map): the chart's options, `:extent` read (false - no extents).
  - `dim-values` (map): position dimension -> its values, `{dim [v ...]}`; non-positions such as `nil` or `[:panel f]` are skipped.

  Returns `{dim extent}` of the dimensions with positions (see [[cljplot2.data.rows/extent]]); `{}` with `:extent false`.

  Throws `ex-info {:key :extents}` for numbers mixed with levels in one dimension.

  See also [[literal-stats]], [[data-stats]]."
  [options dim-values]
  (if (false? (:extent options))
    {}
    (into {} (keep (fn [[k vs]] (when-let [e (data/extent k (filter position? vs))] [k e]))) dim-values)))

(defn literal-stats
  "Chart stats of a literal annotation, drawn in every panel.

  Parameters:

  - `computed`: the draw data.
  - `options` (map): the chart's options (`:extent`).
  - `dim-values` (map): its positions `{dim [v ...]}`, see [[extents]].

  Returns `{:data :extents :na :dropped}` with `computed` and the extents under `cljplot2.data.facet/all-panels`, no NA dimensions, nothing dropped.

  See also [[data-stats]]."
  [computed options dim-values]
  (let [e (extents options dim-values)]
    {:data {facet/all-panels computed}
     :extents (if (seq e) {facet/all-panels e} {})
     :na #{}
     :dropped {}}))

(defn data-stats
  "Chart stats of a data annotation, per panel from its rows.

  Parameters:

  - `chart-split` (map): the chart's split, `:groups` and `:mapping`.
  - `options` (map): the chart's options (`:extent`).
  - `env` (map): the compute environment (`:plot-scales` ...).
  - `panel-fn` (function): of a panel's groups (of [[cljplot2.data.group/split]] by the chart's aesthetics) returning `[computed positions]` - the panel's draw data and its positions `{dim [v ...]}`.

  Returns `{:data :extents :na :dropped}`: the draw data per panel; per panel the aesthetic extents merged with the position extents (see [[extents]]); NA dimensions and dropped counts of the split.

  See also [[literal-stats]]."
  [chart-split options env panel-fn]
  (let [{:keys [panels dropped]} (group/split (:groups chart-split) (:mapping chart-split) env)
        per-panel (update-vals panels panel-fn)]
    {:data (update-vals per-panel first)
     :extents (merge-with merge
                          (group/aesthetic-extents panels (:plot-scales env))
                          (into {} (keep (fn [[panel [_ positions]]]
                                           (let [e (extents options positions)] (when (seq e) [panel e]))))
                                per-panel))
     :na (group/na-dims panels)
     :dropped dropped}))

(defn require-mapping!
  "Checks that a data annotation maps its positions.

  Parameters:

  - `what` (string): the chart's name for the message.
  - `mapping` (map): the chart's own mapping.
  - `alternatives` (sequence of key vectors): e.g. `[[:x] [:y]]` - one of them must have every key mapped.

  Returns `nil`.

  Throws `ex-info {:key :mapping}` when no alternative is fully mapped."
  [what mapping alternatives]
  (when-not (some (fn [ks] (every? #(some? (get mapping %)) ks)) alternatives)
    (schema/fail! (str what " needs literal positions in its options or position keys in its own :mapping "
                       (pr-str alternatives) ".")
                  {:key :mapping :value (select-keys mapping (distinct (apply concat alternatives)))})))

;; pixels

(defn pixel-fn
  "Pixel function of positions on a finalized position scale.

  Parameters:

  - `finalized` (map): a finalized scale, or `nil`.

  Returns a function of a position giving its pixel (a level - its band centre); `##NaN` when the scale can't place it, always `##NaN` without a scale.

  See also [[interval-fn]], [[axis-pixel-fn]]."
  [finalized]
  (if finalized (scale/forward-fn finalized) (fn ^double [_] ##NaN)))

(defn interval-fn
  "Pixel interval function of position pairs on a finalized position scale.

  Parameters:

  - `finalized` (map): a finalized scale, or `nil`.
  - `band-width` (double): fraction of a band's step a level covers, centred (default 1).

  Returns a function of positions `a` and `b` giving `[lo hi]` (lo <= hi) - the pixels covering both: a number or a `java.time` value - its pixel, a level - its band step (scaled by `band-width`), `nil` - the scale's range end on that side (`a` the low-value end, `b` the high-value end). The function returns `nil` when an end can't be placed; without a scale it always returns `nil`.

  See also [[pixel-fn]]."
  ([finalized] (interval-fn finalized 1.0))
  ([finalized ^double band-width]
   (if-not finalized
     (constantly nil)
     (let [[r0 r1] (:range finalized)
           half (m/* 0.5 band-width (double (or (:step (scale/band-geometry finalized)) 0.0)))
           pixel (pixel-fn finalized)
           ends (fn [v range-end]
                  (cond
                    (nil? v) [range-end range-end]
                    (or (number? v) (data/temporal? v)) (let [p (pixel v)] [p p])
                    :else (let [p (double (pixel v))] [(m/- p half) (m/+ p half)])))]
       (fn [a b]
         (let [ps (concat (ends a r0) (ends b r1))]
           (when (every? #(m/valid-double? (double %)) ps)
             [(v/mn ps) (v/mx ps)])))))))

(defn axis-pixel-fn
  "Pixel function of text positions along a finalized scale's axis.

  Parameters:

  - `finalized` (map): a finalized scale, or `nil`.

  Returns a function of a position giving its pixel: `[:panel f]` - fraction `f` of the range from its low-value end, other positions as [[pixel-fn]]; always `##NaN` without a scale."
  [finalized]
  (if-not finalized
    (fn ^double [_] ##NaN)
    (let [[^double r0 ^double r1] (:range finalized)
          pixel (pixel-fn finalized)]
      (fn ^double [v]
        (if (and (vector? v) (= :panel (first v)))
          (m/+ r0 (m/* (double (second v)) (m/- r1 r0)))
          (pixel v))))))
