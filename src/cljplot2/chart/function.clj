(ns cljplot2.chart.function
  "Function: curves of functions sampled across the panel - `y = f(x)` (`:f`) or parametric `[x y] = f(t)`
  (`:parametric`). No data: the chart draws in every panel and inherits only the plot's facet keys (as annotations).

  * `:f` / `:parametric` (exactly one) - a function, a vector of functions, a vector of `[label function]` pairs or
    a map label -> function. Each function is a row `{:function label}` (label - the pair's or map's key, or the
    index in a vector, 0 for a single function), grouped by the chart's own `:mapping` (`{:color :function}`,
    `:dash`, `:group`); labels are levels in the given order (vector order, pair order, a map's iteration order - a
    hash map of more than 8 entries has none, use pairs), not sorted, unless the plot's scale has an explicit
    `:domain`: a mapped `:function` is declared categorical ([[categorical-labels]]), so numeric labels get ordinal
    scales - the plot's scale of that aesthetic, shared with the other charts (numeric data of another chart on it
    then needs few distinct values).
    Without a mapping every function takes the theme style.
  * `:domain [lo hi]` (`:f`) - numbers or `java.time` values; without it the plot's explicit x domain, else the x
    scale's domain at drawing (the chart then adds no extents). On a temporal domain or a `:datetime` x scale `f`
    receives a `LocalDateTime`.
  * `:t [lo hi]` (`:parametric`, required) - numbers or `java.time` values (then `f` receives a `LocalDateTime`,
    sampled evenly in elapsed time).
  * `:sample-step` - `:f`: pixels between samples along x (default 1) or `{:count n}`; `:parametric`: only
    `{:count n}` (default 500).
  * `:y-limits [lo hi]` - samples outside don't count for the y extent (the curve is only clipped).
  * `:y-extent` - y extent of the samples without `:y-limits` or an explicit plot y domain: `:quantiles` (default,
    the 1% and 99% sample quantiles), `{:quantiles [p q]}`, `:min-max` or `:none` (no y extent).
  * `:gaps` - `nil` / non-finite results break the curve: `:break` (default) or `:bridge` (see
    [[cljplot2.chart.line]]); consecutive samples beyond opposite panel edges (asymptotes) cut the curve, never
    bridged.
  * `:fill` - `:f`: the area to y = 0 (clamped into the panel); `:parametric`: the polygon the curve closes.
  * `:color`, `:line-width`, `:dash`, `:bridge` - style, defaults from theme `[:chart :function]`; `:padding` as line.

  Exceptions thrown by a function propagate.

  Functions: [[schemas]] - the chart's schemas, with the named types the book documents [[functions-schema]],
  [[span-schema]], [[quantile-range-schema]]; [[categorical-labels]] - options with function labels as levels;
  [[compute]] - the functions and their sampled extents; [[draw]] - one panel's curves; [[legend]] - the key glyph."
  (:require [fastmath.core :as m]
            [fastmath.stats :as stats]
            [wadogo.scale :as s]
            [cljplot2.data.accessor :as accessor]
            [cljplot2.data.facet :as facet]
            [cljplot2.data.group :as group]
            [cljplot2.data.rows :as data]
            [cljplot2.graphics.curve :as curve]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.scale.aesthetic :as aesthetic]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^{:private true :const true :tag 'long} extent-samples
  "Samples of an `:f` function over its domain for the y extent."
  1000)

(def ^{:private true :const true :tag 'long} parametric-samples
  "Samples of a `:parametric` function over `:t`, unless `:sample-step {:count n}` says otherwise."
  500)

(def ^:private default-quantiles
  "Sample quantiles `[p q]` making the y extent of `:y-extent :quantiles`."
  [0.01 0.99])

;; schema

(defn- labelled-pair?
  "Tests whether a value is a `[label function]` pair.

  Parameters:

  - `p`: any value.

  Returns a truthy value for a two-element vector whose second element is a function."
  [p]
  (and (vector? p) (m/== 2 (count p)) (fn? (second p))))

(def functions-schema
  "Schema of `:f` / `:parametric`: a function, a non-empty vector of functions or of `[label function]` pairs, or a non-empty map label -> function."
  (schema/named-type ::functions
                     #(or (fn? %) (and (vector? %) (seq %) (or (every? fn? %) (every? labelled-pair? %)))
                          (and (map? %) (seq %) (every? fn? (vals %))))
                     "should be a function, a non-empty vector of functions or of [label function] pairs, or a non-empty map label -> function"
                     {:cljplot2/type-name "functions"
                      :cljplot2/type-plural "functions"
                      :cljplot2/type-meaning "One function, or several with labels: a vector of functions (labelled 0, 1, ...), a vector of `[label function]` pairs, or a map from label to function, e.g. `[[\"sin\" m/sin] [\"cos\" m/cos]]`."}))

(defn- span-value?
  "Tests whether a value can end a sampled span.

  Parameters:

  - `v`: any value.

  Returns a truthy value for a finite number or a `java.time` value."
  [v]
  (or (schema/finite? v) (data/temporal? v)))

(def span-schema
  "Schema of `:domain` / `:t`: `[lo hi]` - finite numbers `lo < hi`, or two different `java.time` values."
  (schema/named-type ::span
                     #(and (vector? %) (m/== 2 (count %)) (every? span-value? %)
                           (or (every? number? %) (every? data/temporal? %))
                           (if (every? number? %) (m/< (double (% 0)) (double (% 1))) (not= (% 0) (% 1))))
                     "should be [lo hi]: finite numbers lo < hi, or two different java.time values"
                     {:cljplot2/type-name "span"
                      :cljplot2/type-meaning "The range a function is sampled over, `[lo hi]`: two finite numbers with `lo` below `hi`, e.g. `[0 10]`, or two different `java.time` values."}))

(defn- quantile-range?
  "Tests whether a value is a quantile range `{:quantiles [p q]}`, `0 <= p < q <= 1`.

  Parameters:

  - `v`: any value.

  Returns a truthy value for such a map."
  [v]
  (and (map? v) (= [:quantiles] (keys v))
       (let [q (:quantiles v)]
         (and (vector? q) (m/== 2 (count q)) (every? number? q)
              (m/<= 0.0 (double (q 0))) (m/< (double (q 0)) (double (q 1))) (m/<= (double (q 1)) 1.0)))))

(def quantile-range-schema
  "Schema of a quantile range `{:quantiles [p q]}`, `0 <= p < q <= 1` - the form of `:y-extent` other than its keywords."
  (schema/named-type ::quantile-range quantile-range? "should be {:quantiles [p q]}, 0 <= p < q <= 1"
                     {:cljplot2/type-name "quantile range"
                      :cljplot2/type-meaning "Two sample quantiles `{:quantiles [p q]}`, fractions from 0 to 1 with `p` below `q`, e.g. `{:quantiles [0.05 0.95]}`."}))

(def ^:private y-extent-schema
  "Schema of `:y-extent`: `:quantiles`, `:min-max`, `:none` or `{:quantiles [p q]}`, `0 <= p < q <= 1`."
  (schema/named-type ::y-extent
                     #(or (contains? #{:quantiles :min-max :none} %) (quantile-range? %))
                     "should be :quantiles, :min-max, :none or {:quantiles [p q]}, 0 <= p < q <= 1"
                     {:cljplot2/value-set [:quantiles :min-max :none]
                      :cljplot2/other-types [::quantile-range]}))

(def ^:private style-entries
  "Map entries of the function style: the line style, `:fill` and `:bridge`."
  (conj polyline/style-entries
        (schema/optional :fill {:cljplot2/section :curve :description "The fill of the area between the curve and y = 0, or of the shape a parametric curve closes."
                                 :cljplot2/value-meanings {:none "No fill."} :cljplot2/example :lightblue :cljplot2/example-context {:f '(fn [x] (m/sin x))}}
                         schema/color)
        (schema/optional :bridge {:cljplot2/section :gaps :description "The style of the segments that bridge gaps with `:gaps :bridge`."
                                   :cljplot2/example-context {:gaps :bridge :f '(fn [x] (when-not (< 2 x 3) (m/sin x)))}}
                         polyline/bridge-schema)))

(defn- function-option-problem
  "Gives the problem of the function options, if any.

  Parameters:

  - `options` (map): `:f`, `:parametric`, `:t`, `:domain`, `:sample-step`.

  Returns `[message key]` for no function or both forms (`:f`), a parametric chart without `:t` or `:t` on `:f` (`:t`), a pixel step on a parametric chart (`:sample-step`), `:domain` on a parametric chart (`:domain`); `nil` for valid options."
  [{:keys [f parametric t domain sample-step]}]
  (cond
    (and (nil? f) (nil? parametric)) ["a function chart needs :f or :parametric" :f]
    (and f parametric) ["a function chart takes :f or :parametric, not both" :f]
    (and parametric (nil? t)) ["a :parametric function chart needs :t [lo hi]" :t]
    (and parametric (some? sample-step) (not (map? sample-step))) ["a :parametric function chart samples by count: :sample-step {:count n}" :sample-step]
    (and parametric domain) [":domain is for :f; a :parametric chart takes :t" :domain]
    (and f t) [":t is for :parametric; an :f chart takes :domain" :t]))

(def ^:private option-rules
  "Rules (see [[cljplot2.schema/with-rules]]) of the options: [[function-option-problem]], each at its key."
  (for [k [:f :t :sample-step :domain]]
    [#(let [[message problem-key] (function-option-problem %)] (when (= k problem-key) message)) [k]]))

(def schemas
  "Schemas of the function chart: `:style` (theme `[:chart :function]`), `:options`, `:mapping` (the chart's own)."
  {:style (into [:map {:closed true}] style-entries)
   :options (apply schema/with-rules
             (-> [:map {:closed true}]
                 (into style-entries)
                 (into theme/theme-style-entries)
                 (into data/legend-option)
                 (conj (schema/optional :data (schema/forbidden "isn't used: a function chart samples its functions"))
                       (schema/optional :mapping {:cljplot2/section :curve :description "The chart's own mapping of the functions, each a row `{:function label}`, e.g. `{:color :function}`. Of the plot's mapping only the facet keys apply."
                                                  :cljplot2/default-doc "No mapping: every function in the theme's style." :cljplot2/example {:color :function}
                                                  :cljplot2/example-context {:f '{"sin" (fn [x] (m/sin x)) "cos" (fn [x] (m/cos x))}}}
                                        data/mapping-schema)
                       (schema/optional :f {:cljplot2/section :curve :description "The functions y = f(x) to draw."
                                            :cljplot2/default-doc "None; give `:f` or `:parametric`." :cljplot2/example '(fn [x] (m/sin x))}
                                        functions-schema)
                       (schema/optional :parametric {:cljplot2/section :curve :description "The functions [x y] = f(t) to draw, each returning a point."
                                                     :cljplot2/default-doc "None; give `:f` or `:parametric`." :cljplot2/example '(fn [t] [(m/cos t) (m/sin t)])
                                                     :cljplot2/example-context {:t [0 6.3]}}
                                        functions-schema)
                       (schema/optional :domain {:cljplot2/section :curve :description "The x range the `:f` functions are sampled over."
                                                 :cljplot2/default-doc "The plot's fixed x domain, else the panel's x range."
                                                 :cljplot2/example [0 3] :cljplot2/example-context {:f '(fn [x] (m/sin x))}}
                                        span-schema)
                       (schema/optional :t {:cljplot2/section :curve :description "The t range the `:parametric` functions are sampled over."
                                            :cljplot2/default-doc "None; `:parametric` needs it." :cljplot2/example [0 3.2]
                                            :cljplot2/example-context {:parametric '(fn [t] [(m/cos t) (m/sin t)])}}
                                        span-schema)
                       (schema/optional :sample-step {:cljplot2/section :curve :description "How densely the functions are sampled: pixels between samples along x, or `{:count n}` samples. `:parametric` takes only a count."
                                                      :cljplot2/default-doc "One pixel for `:f`, `{:count 500}` for `:parametric`."
                                                      :cljplot2/example 25 :cljplot2/example-context {:f '(fn [x] (m/sin x))}}
                                        curve/sample-step-schema)
                       (schema/optional :y-limits {:cljplot2/section :extent :description "Only samples within these limits count for the y extent; the panel's edge clips the rest of the curve."
                                                   :cljplot2/default-doc "No limits: `:y-extent` picks the samples." :cljplot2/example [-0.5 0.5]
                                                   :cljplot2/example-context {:f '(fn [x] (m/sin x))}}
                                        schema/finite-interval)
                       (schema/optional :y-extent {:cljplot2/section :extent :description "Which samples set the y extent when neither `:y-limits` nor a fixed y domain does."
                                                   :default :quantiles
                                                   :cljplot2/value-meanings {:quantiles "The samples between the 1% and 99% quantiles, so a pole doesn't stretch the axis." :min-max "All samples." :none "None; other charts or the scale set the y range."}
                                                   :cljplot2/form-meanings {::quantile-range "The samples between these quantiles."}
                                                   :cljplot2/example {:quantiles [0.2 0.8]} :cljplot2/example-context {:f '(fn [x] (m/sin x))}}
                                        y-extent-schema)
                       (schema/optional :gaps {:cljplot2/section :gaps :description "How the curve handles a `nil` or non-finite result."
                                               :default :break
                                               :cljplot2/value-meanings {:break "Leaves empty space." :bridge "Joins the samples on both sides of the gap with a straight segment in the `:bridge` style."}
                                               :cljplot2/example-context {:f '(fn [x] (when-not (< 2 x 3) (m/sin x)))}}
                                        [:enum :break :bridge])
                       (schema/optional :padding {:cljplot2/section :extent :description "Room around the sampled curve on x and y, as fractions of its span."
                                                  :cljplot2/default-doc "`0.05` on every side." :cljplot2/example {:x [0 0]}
                                                  :cljplot2/example-context {:f '(fn [x] (m/sin x)) :domain [0 6]}}
                                        data/position-padding)))
             option-rules)
   :mapping [:map
             (schema/nilable :color {:description "Splits the rows, one per function, into groups, each with its own colour."} schema/accessor)
             (schema/nilable :dash {:description "Splits the rows, one per function, into groups, each with its own dash style."} schema/accessor)
             (schema/nilable :group {:description "Splits the rows into groups that look the same, one row per function."} schema/accessor)]})

(defn categorical-labels
  "Marks the function labels of a function chart's own mapping categorical.

  Function labels are levels, also when they're numbers (vector indices).

  Parameters:

  - `options` (map): the chart's options - `:mapping`.

  Returns `options` with every accessor `:function` of `:mapping` marked categorical (see [[cljplot2.data.accessor]]); `options` as they are without a mapping."
  [options]
  (if-let [mapping (:mapping options)]
    (assoc options :mapping (update-vals mapping #(if (= :function %) (accessor/->CategoricalAccessor :function) %)))
    options))

;; input

(defn- labelled
  "Labels the functions of `:f` / `:parametric`.

  Parameters:

  - `fs`: a function, a vector of functions or of `[label f]` pairs, or a map label -> function.

  Returns a vector of `[label f]`: a single function labelled 0, vector functions by their index, pairs and map entries as given."
  [fs]
  (cond
    (fn? fs) [[0 fs]]
    (map? fs) (vec fs)
    (every? labelled-pair? fs) (vec fs)
    :else (vec (map-indexed vector fs))))

(defn- label-ordered-extents
  "Orders the aesthetic extents of function labels as the labels are given.

  Parameters:

  - `extents` (map): aesthetic extents `{panel {dim extent}}`.
  - `mapping` (map): the chart's own mapping - the dims mapped to `:function`.
  - `labels` (sequence): the function labels in their given order.

  Returns `extents` with every dim mapped to `:function` listing its values in the labels' order, marked `:ordered` (ordinal scales keep it, see [[cljplot2.scale.aesthetic]])."
  [extents mapping labels]
  (let [dims (filter #(= :function (some-> (get mapping %) accessor/unwrap)) aesthetic/dims)
        order (vec (distinct labels))]
    (update-vals extents (fn [es] (reduce (fn [dim-extents k] (if-let [e (get dim-extents k)]
                                                                (assoc dim-extents k (assoc e :values (filterv (set (:values e)) order) :ordered true))
                                                                dim-extents))
                                          es dims)))))

(defn- functions-of
  "Gives the functions of a function chart (its options valid, see [[option-rules]]).

  Parameters:

  - `options` (map): `:f`, `:parametric`.

  Returns `[parametric labelled-functions]` (see [[labelled]])."
  [{:keys [f parametric]}]
  [(some? parametric) (labelled (or f parametric))])

;; sampling

(def ^:private sampled-kinds
  "Continuous position scale kinds samples follow (evenly in the scale's transform)."
  #{:linear :log :pow :symlog})

(defn- span-samples
  "Gives values evenly spread over a span.

  Parameters:

  - `[lo hi]`: the span - numbers or `java.time` values.
  - `n` (long): the sample count, at least 2.
  - `spec` (map, optional): a position scale spec - numbers spread evenly in its transform (`:log`, `:pow`, `:symlog` with `:base` / `:exponent`; else linear).

  Returns a sequence of `n` values from `lo` to `hi`: numbers, or `LocalDateTime`s evenly in elapsed time."
  ([interval n] (span-samples interval n nil))
  ([[lo hi] ^long n spec]
   (let [ws (if (number? lo)
              (s/scale (get sampled-kinds (:kind spec) :linear)
                       (merge (select-keys spec [:base :exponent]) {:domain [(double lo) (double hi)] :range [0.0 1.0]}))
              (s/scale :datetime {:domain [lo hi] :range [0.0 1.0]}))]
     (map #(s/inverse ws %) (m/slice-range n)))))

(defn- sample-count
  "Gives the sample count of a `:sample-step` option.

  Parameters:

  - `sample-step`: the option - `{:count n}`, a number (a step) or `nil`.
  - `default` (long): the count without `{:count n}`.

  Returns `n` of `{:count n}`, else `default`."
  ^long [sample-step default]
  (long (if (map? sample-step) (:count sample-step) default)))

(defn- plottable
  "Gives a value when it can be plotted.

  Parameters:

  - `v`: any value.

  Returns `v` for a finite number or a `java.time` value; else `nil`."
  [v]
  (when (or (schema/finite? v) (data/temporal? v)) v))

(defn- point-of
  "Gives the point of a parametric function's result.

  Parameters:

  - `v`: the result - a sequential `[x y]`.

  Returns `[x y]` when both coordinates are plottable (see [[plottable]]), else `nil`."
  [v]
  (when (and (sequential? v) (m/== 2 (count v)))
    (let [[x y] v] (when (and (plottable x) (plottable y)) [x y]))))

(defn- explicit-span
  "Gives the plot's explicit domain of a position scale when it can be sampled.

  Parameters:

  - `plot-scales` (map): the plot's `:scales`.
  - `k` (keyword): the position scale, `:x` or `:y`.

  Returns the domain as a vector when it is two numbers or `java.time` values ([[cljplot2.data.rows/full-domain]]), else `nil` - also for `[nil nil]` (no domain) and a partial domain (its open end comes from the scale: the chart then samples over the x scale's domain at drawing, and its y extent completes a partial y domain)."
  [plot-scales k]
  (let [d (data/full-domain (get-in plot-scales [k :domain]))]
    (when (and (m/== 2 (count d)) (every? span-value? d)) (vec d))))

(defn- y-extent-of
  "Gives the y extent of sampled function values.

  Parameters:

  - `ys` (sequence): the plottable y samples.
  - `options` (map): `:y-limits [lo hi]` (only samples within count, by their min / max), `:y-extent` - the mode (default `:quantiles`, see the namespace doc).
  - `explicit-y?` (truthy): does the plot have an explicit y domain with both ends (a domain with a `nil` end - a partial domain - takes its open end from this extent)?

  Returns a y extent (temporal samples - their min / max); `nil` with an explicit y domain, for `:none`, or without samples."
  [ys {:keys [y-limits] extent-mode :y-extent} explicit-y?]
  (when-not explicit-y?
    (let [mode (or extent-mode :quantiles)
          numeric? (every? number? ys)]
      (cond
        (empty? ys) nil
        y-limits (data/extent :y (filter #(m/<= (double (y-limits 0)) (double %) (double (y-limits 1))) ys))
        (= :none mode) nil
        (or (= :min-max mode) (not numeric?)) (data/extent :y ys)
        :else (let [[p q] (if (map? mode) (:quantiles mode) default-quantiles)
                    [lo hi] (stats/quantiles ys [p q])]
                (data/extent :y [lo hi]))))))

(defn- sampled-extents
  "Gives the position extents of functions from their samples.

  Parameters:

  - `parametric` (boolean): `:parametric` (over `:t`) rather than `:f` (over the domain)?
  - `fs` (sequence): the labelled functions `[[label f] ..]`.
  - `options` (map): `:t`, `:domain`, `:sample-step`, `:y-limits`, `:y-extent`.
  - `plot-scales` (map): the plot's `:scales` - explicit x / y domains, the x scale spec.

  Returns `{:x extent :y extent}` (each when there is one); `{}` for an `:f` chart without a domain to sample yet."
  [parametric fs {:keys [t domain sample-step] :as options} plot-scales]
  (let [explicit-y? (explicit-span plot-scales :y)]
    (if parametric
      (let [points (for [[_ f] fs v (span-samples t (sample-count sample-step parametric-samples))
                         :let [p (point-of (f v))] :when p]
                     p)
            xs (map first points)
            ey (y-extent-of (map second points) options explicit-y?)]
        (cond-> {}
          (seq xs) (assoc :x (data/extent :x xs))
          ey (assoc :y ey)))
      (if-let [d (or domain (explicit-span plot-scales :x))]
        (let [ys (for [[_ f] fs v (span-samples d (sample-count sample-step extent-samples) (:x plot-scales))
                       :let [y (plottable (f v))] :when y]
                   y)
              e (y-extent-of ys options explicit-y?)]
          (cond-> {:x (data/extent :x d)} e (assoc :y e)))
        {}))))

(defn compute
  "Computes the functions of the chart, drawn in every panel.

  Parameters:

  - `chart` (map): the chart value - `:options` (see the namespace doc).
  - `split` (map): `:mapping` - the chart's own mapping (facet keys of the plot).
  - `env` (map): `:theme`, `:plot-scales` (and the grouping keys of [[cljplot2.data.group/split]]).

  Returns `{:data :extents :padding :padding-explicit :na :dropped}`: one panel (`facet/all-panels`) `{:functions [{:f :aesthetics}] :style st :parametric :domain :t :sample-step :gaps}` - the functions grouped by the chart's own mapping, `st` the resolved style; position extents from samples (see [[sampled-extents]]), aesthetic extents of the labels in their given order.

  Exceptions of a function propagate (option conflicts fail the options schema, see [[option-rules]])."
  [{:keys [options] :as chart} {:keys [mapping]} {:keys [theme plot-scales] :as env}]
  (let [[parametric fs] (functions-of options)
        rows (mapv (fn [[label f]] {:function label ::f f}) fs)
        {:keys [panels dropped]} (group/split {facet/all-panels rows} (select-keys mapping (conj aesthetic/dims :group)) env)
        functions (vec (for [g (get panels facet/all-panels) row (:rows g)] {:f (::f row) :aesthetics (:aesthetics g)}))
        extents (sampled-extents parametric fs options plot-scales)]
    (into (data/padding-stats data/default-padding {} options)
          {:data {facet/all-panels (merge {:functions functions :style (theme/resolved-style theme chart) :parametric parametric}
                                          (select-keys options [:domain :t :sample-step :gaps]))}
           :extents (merge-with merge (label-ordered-extents (group/aesthetic-extents panels plot-scales) mapping (map first fs))
                                (if (seq extents) {facet/all-panels extents} {}))
           :na (group/na-dims panels)
           :dropped dropped})))

;; drawing

(defn- jump?
  "Tests whether consecutive pixel positions jump farther than the panel.

  A sampled continuous curve moves less between two samples, so a jump is an asymptote (this includes samples beyond opposite panel edges).

  Parameters:

  - `extent` (double): the panel's size along the coordinate.
  - `a`, `b` (doubles): the consecutive positions."
  [^double extent ^double a ^double b]
  (m/> (m/abs (m/- b a)) extent))

(defn- stub
  "Gives the point continuing a segment past its end - a piece's end running out of the panel at a cut.

  Parameters:

  - `[x0 y0]`, `[x1 y1]`: the segment from its start to its end.
  - `reach` (double): pixels beyond the end.

  Returns `[x y]`; `nil` when the segment has no direction or isn't finite."
  [[^double x0 ^double y0] [^double x1 ^double y1] ^double reach]
  (let [dx (m/- x1 x0)
        dy (m/- y1 y0)
        len (m/hypot dx dy)]
    (when (and (m/valid-double? len) (m/pos? len))
      [(m/+ x1 (m/* reach (m// dx len))) (m/+ y1 (m/* reach (m// dy len)))])))

(defn- cut-pieces
  "Splits sampled points where the curve jumps across the panel (asymptotes, see [[jump?]]).

  Parameters:

  - `points` (vector): pixel points `[x y]` (NaN coordinates break the curve later).
  - `w`, `h` (doubles): the panel size.

  Returns a vector of pieces (vectors of points), split where consecutive points jump farther than the panel in x or y; at a cut each piece runs on out of the panel along its end segment (see [[stub]])."
  [points ^double w ^double h]
  (let [n (count points)
        reach (m/+ w h)
        cut? (fn [[x0 y0] [x1 y1]] (or (jump? w x0 x1) (jump? h y0 y1)))
        point (fn [^long i] (when (m/< -1 i n) (points i)))]
    (loop [i 0
           piece []
           pieces []]
      (if (m/== i n)
        (conj pieces piece)
        (let [p (points i)]
          (if (and (m/pos? i) (cut? (points (m/long-dec i)) p))
            (let [before (some-> (point (m/long-sub i 2)) (stub (points (m/long-dec i)) reach))
                  after (some-> (point (m/long-inc i)) (stub p reach))]
              (recur (m/long-inc i) (cond-> [] after (conj after) true (conj p)) (conj pieces (cond-> piece before (conj before)))))
            (recur (m/long-inc i) (conj piece p) pieces)))))))

(defn- piece-runs
  "Gives the drawn runs of a piece.

  Parameters:

  - `piece` (vector): pixel points `[x y]`.

  Returns a vector of runs - the piece's finite runs (see [[cljplot2.graphics.polyline/finite-runs]]), points as drawn (see [[cljplot2.graphics.curve/drawn-point]])."
  [piece]
  (let [xs (mapv first piece)
        ys (mapv second piece)]
    (mapv (fn [run] (mapv (fn [i] (curve/drawn-point [(xs i) (ys i)])) run)) (polyline/finite-runs xs ys))))

(defn- require-scales!
  "Checks that a panel can draw an `:f` function chart.

  Parameters:

  - `scales` (map): the panel's finalized scales.

  Returns `nil`.

  Throws `ex-info {:key :domain}` without an x scale (no `:domain`, no explicit x domain, no x data), `{:key :y-extent}` without a y scale (no y samples, no y data), `{:key :f}` for a categorical x scale."
  [scales]
  (cond
    (nil? (:x scales))
    (schema/fail! "A function chart needs an x domain: its :domain, an explicit {:x {:domain ..}} or another chart's x data."
                  {:key :domain :value nil})
    (nil? (:y scales))
    (schema/fail! "A function chart needs a y range: samples counted by :y-extent, an explicit {:y {:domain ..}} or another chart's y data."
                  {:key :y-extent :value nil})
    (not (scale/continuous-kinds (:kind (:x scales))))
    (schema/fail! (str "A function chart samples a continuous x scale; the plot's x scale is " (:kind (:x scales))
                       " (categorical data).")
                  {:key :f :value (:kind (:x scales))})))

(defn- f-points
  "Samples an `:f` function in pixels along the x scale.

  Parameters:

  - `f` (function): y = f(x).
  - `domain` (`[lo hi]` or `nil`): the span sampled; `nil` - the x scale's domain.
  - `sample-step`: pixels between samples or `{:count n}` (default [[cljplot2.graphics.curve/default-sample-step]]).
  - `scales` (map): the finalized scales.

  Returns a vector of pixel points `[x y]` (swapped in a flipped plot), y `##NaN` where `f` gives no plottable number; `nil` when the domain's ends aren't placed apart in the panel."
  [f domain sample-step scales]
  (let [x-scale (:x scales)
        flipped? (scale/flipped? scales)
        fx (scale/forward-fn x-scale)
        fy (scale/forward-fn (:y scales))
        inv (scale/inverse-fn x-scale)
        r0 (double (first (:range x-scale)))
        r1 (double (second (:range x-scale)))
        [lo hi] (or domain (:domain x-scale))
        clamp (fn [^double p] (m/constrain p (m/min r0 r1) (m/max r0 r1)))
        p0 (double (clamp (fx lo)))
        p1 (double (clamp (fx hi)))]
    (when (and (m/valid-double? p0) (m/valid-double? p1) (m/not== p0 p1))
      (mapv (fn [p] (let [y (plottable (f (inv p)))
                          q (if (and y (not (data/temporal? y))) (double (fy y)) ##NaN)]
                      (if flipped? [q p] [p q])))
            (curve/sample-grid p0 p1 (or sample-step curve/default-sample-step))))))

(defn- parametric-points
  "Samples a parametric function in pixels.

  Parameters:

  - `f` (function): `[x y] = f(t)`.
  - `t` (`[lo hi]`): the span of t.
  - `sample-step` (`{:count n}` or `nil`): the sample count (default 500).
  - `scales` (map): the finalized scales.

  Returns a vector of pixel points `[x y]`; `[##NaN ##NaN]` where `f` gives no plottable point."
  [f t sample-step scales]
  (let [projection (scale/projection scales)]
    (mapv (fn [v] (if-let [[x y] (point-of (f v))]
                    (let [^Vec2 p (projection x y)] [(.x p) (.y p)])
                    [##NaN ##NaN]))
          (span-samples t (sample-count sample-step parametric-samples)))))

(defn draw
  "Draws every function of a panel: sampled, cut at asymptotes (see [[cut-pieces]]), each piece filled (`:fill`), stroked and bridged (`:gaps :bridge`) as a line (see [[cljplot2.graphics.curve/draw-runs!]]).

  Parameters:

  - `computed` (map): `:functions`, `:style`, `:parametric`, `:domain`, `:t`, `:sample-step`, `:gaps` (see [[compute]]).
  - `canvas`: the clojure2d canvas.
  - `box` (map): the panel's `:w`, `:h`.
  - `scales` (map): the finalized scales.
  - `env` (map): `:theme`.

  Throws the errors of [[require-scales!]] for an `:f` chart; exceptions of a function propagate."
  [_chart {:keys [functions style parametric domain t sample-step gaps]} canvas {:keys [^double w ^double h]} scales env]
  (when-not parametric (require-scales! scales))
  (let [y-scale (:y scales)
        area (if parametric
               :closed
               {:baseline (curve/area-baseline y-scale w h) :horizontal (not (scale/horizontal? :y y-scale))})]
    (doseq [{:keys [f aesthetics]} functions
            :let [st (group/style style aesthetics scales (:theme env))]
            :when st
            piece (cut-pieces (if parametric
                                (parametric-points f t sample-step scales)
                                (f-points f domain sample-step scales))
                              w h)]
      (curve/draw-runs! canvas (piece-runs piece) st area (or gaps :break)))))

(defn legend
  "Draws the chart's key glyph: a horizontal line through the middle of the key's box; with `:fill` a filled square (the fill) outlined in the line style (see [[cljplot2.graphics.polyline/key-glyph!]]).

  Parameters:

  - `chart` (map): the chart value.
  - `attributes` (map): the key's mapped attributes - `:color`, `:dash`.
  - `canvas`: the clojure2d canvas.
  - `box` (map): the key's box.
  - `env` (map): `:theme`."
  [chart attributes canvas box env]
  (let [st (theme/key-style env chart attributes [:color :dash])]
    (polyline/key-glyph! canvas box st (data/option st :fill :none))))
