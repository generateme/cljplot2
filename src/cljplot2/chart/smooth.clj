(ns cljplot2.chart.smooth
  "Smooth: a fitted curve of `:y` on `:x` per panel and group, with bands (see [[cljplot2.data.smooth]]).

  Options (style defaults from theme `[:chart :smooth]`):

  * `:method` - `:loess` (default), `:lm`, `:glm`, `:gp`, any other fastmath 1D interpolation method or a function
    `(fn [xs ys] f)`; `:method-params` - a map: the options of fastmath `loess` (over the ggplot2 defaults
    `{:bandwidth 0.75 :degree 2}` - see [[cljplot2.data.smooth/loess-defaults]]; `:iters` robustness iterations,
    fastmath's default 2, `0` - ggplot2's plain fit), `lm`
    (`:degree` - polynomial degree, default 1 - and lm options), `glm` (`:family`, ...), `gaussian-process` (`:kernel`,
    `:noise`, `:kscale`, `:normalize?`), else the extra argument of the method (or function)
  * `:band` - `:confidence` (default for `:loess`, `:lm`, `:glm`, `:gp`), `:prediction` (`:lm`), `:bootstrap` (any
    method; `:bootstrap {:samples :rng :ci :estimation}` - see `fastmath.interpolation/bootstrap`; `:rng` is copied
    per build, see [[cljplot2.data.rng/for-build]]) or `:none`
    (default for the other methods; their `:confidence` throws `ex-info {:key :band}`); `:level` - a number in (0, 1)
    or a vector of them (nested bands, the widest drawn first; repeated levels drawn once), default 0.95
  * `:n` - grid points (default 200) over each group's x range; `:full-range true` - over the plot's x `:domain`
    when it is given (a partial `[lo nil]` / `[nil hi]` - its open end from that range), else over the x range of all groups of the panel
  * `:color`, `:line-width`, `:dash` (line), `:fill` (bands: `:color` - default, the group colour faded by `:alpha`
    (its own alpha times `:alpha`, default 0.3) - a colour or `:none`: no band, as `:band :none`)
  * `:padding {:x [lo hi] :y [lo hi]}` - default 5% each side

  * `:stat-space` - `:scale` (default): on a `:log` x or y axis the fit runs in log10 coordinates of that axis (see
    [[cljplot2.scale.position/stat-transform-for]]; lattice's `type = \"smooth\"` on log scales) and the curve and
    bands are taken back to data units; values the axis can't place (0, negatives) are left out of the fit (counted);
    `:data` - the fit in data units on every axis

  x is numeric or temporal (`java.time`, fitted in epoch milliseconds - see [[cljplot2.data.rows/x-key]]; parameters
  in x units, such as a `:gp` kernel's `:sigma`, are in milliseconds then); rows with an
  invalid x or y are left out (counted). Groups (see [[cljplot2.data.group]]) come from ordinal aesthetics (`:color`)
  and `:group`, one fit per group; a group with fewer distinct x or rows than the method needs (see
  [[cljplot2.data.smooth/fit-requirements]]) has no curve and is counted in `:dropped {:group n}`; other fit errors
  throw `ex-info {:key :method}` naming the group. A band undefined at a point (loess outside the data) breaks there.

  Functions: [[schemas]] - the chart's schemas; [[compute]] - the fitted curves and bands of every panel; [[draw]] -
  one panel's bands and curves; [[legend]] - the key glyph."
  (:require [cljplot2.chart.common :as common]
            [cljplot2.data.rows :as data]
            [cljplot2.data.group :as group]
            [cljplot2.data.rng :as rng]
            [cljplot2.data.smooth :as smooth]
            [cljplot2.graphics.curve :as curve]
            [cljplot2.graphics.draw :as draw]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme]
            [fastmath.core :as m])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^{:private true :const true :tag 'long} default-n
  "Grid points a curve is evaluated at, unless `:n` says otherwise."
  200)

(def ^:private style-entries
  "Map entries of the smooth style: the line style, the band `:fill` and `:alpha`, the band pattern."
  (into (conj polyline/style-entries
              (schema/optional :fill {:cljplot2/section :band :description "How the band is filled."
                                      :cljplot2/value-meanings {:color "The curve's colour, faded by `:alpha`."
                                                        :none "No band, as with `:band :none`."}}
                               [:or [:= :color] schema/color])
              (schema/optional :alpha {:cljplot2/section :band :description "How opaque the band is with `:fill :color`: a factor on the curve colour's alpha."
                                       :cljplot2/example 0.5}
                               schema/fraction))
        draw/pattern-style-entries))

(def ^:private method-schema
  "Schema of `:method`: a function `(fn [xs ys] f)` or a fit method name of [[cljplot2.data.smooth/smooth-methods]]."
  (schema/named-type ::method #(or (fn? %) (contains? smooth/smooth-methods %))
                     (str "should be a function (fn [xs ys] f) or one of " (vec (sort smooth/smooth-methods)))
                     {:cljplot2/value-set smooth/smooth-methods :cljplot2/other-types [::schema/function]}))

(defn- method-params-problem
  "Gives the problem of the `:method-params` of the options, if any.

  Parameters:

  - `options` (map): the chart's options - `:method` (default `:loess`), `:method-params`.

  Returns the error message for a loess `:bandwidth` outside (0, 1], a `:degree` with a method other than `:lm` (a positive integer) or `:loess` (1 or 2); `nil` when they fit the method."
  [options]
  (let [method (data/option options :method :loess)
        params (:method-params options)]
    (cond
      (and (= :loess method) (contains? params :bandwidth)
           (not (and (number? (:bandwidth params)) (m/< 0.0 (double (:bandwidth params))) (m/<= (double (:bandwidth params)) 1.0))))
      ":bandwidth of loess should be a number in (0 1]"
      (and (contains? params :degree) (not (#{:lm :loess} method))) ":degree is for :method :lm or :loess"
      (and (= :loess method) (contains? params :degree) (not (let [degree (:degree params)] (and (number? degree) (or (m/== (double degree) 1.0) (m/== (double degree) 2.0)))))) ":degree of loess should be 1 or 2"
      (and (= :lm method) (contains? params :degree) (not (and (integer? (:degree params)) (m/pos? (long (:degree params))))))
      ":degree should be a positive integer")))

(defn- band-problem
  "Gives the problem of the `:band` of the options, if any.

  Parameters:

  - `options` (map): the chart's options - `:method` (default `:loess`), `:band` (default by the method).

  Returns the error message for a band the method has none of; `nil` otherwise."
  [options]
  (let [method (data/option options :method :loess)
        band (data/option options :band (smooth/default-band method))]
    (cond
      (and (= :confidence band) (not (contains? smooth/error-methods method)))
      (str "the method " (pr-str method) " has no confidence band; use :band :bootstrap (or :none)")
      (and (= :prediction band) (not= :lm method)) ":band :prediction is for :method :lm")))

(def ^:private option-rules
  "Rules (see [[cljplot2.schema/with-rules]]) of the options: [[method-params-problem]] at `:method-params`, [[band-problem]] at `:band`."
  [[method-params-problem [:method-params]]
   [band-problem [:band]]])

(def schemas
  "Schemas of the smooth chart: `:style` (theme `[:chart :smooth]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (apply schema/with-rules
             (-> [:map {:closed true}]
                 (into style-entries)
                 (into data/common-options)
                 (into theme/theme-style-entries)
                 (conj (schema/optional :method {:cljplot2/section :fit :description "How the curve is fitted to each group's points."
                                                 :default :loess
                                                 :cljplot2/value-meanings {:loess "Local regression: a quadratic fit around each x over the nearest 75% of the points (ggplot2's span and degree)."
                                                                   :lm "A least-squares polynomial: a straight line unless `:method-params` sets a `:degree`."
                                                                   :glm "A generalized linear model; `:method-params` sets its `:family`."
                                                                   :gp "A Gaussian process: its mean, with a band from its standard deviation."}
                                                 :cljplot2/other-values "Other fastmath 1D interpolation methods. They have no band of their own; `:band :bootstrap` adds one."
                                                 :cljplot2/form-meanings {::schema/function "Your own fit: a function of the points' `xs` and `ys` (and of `:method-params` as a third argument, when given) that returns the curve as a function from x to y. It has no band of its own."}
                                                 :cljplot2/example :lm}
                                        method-schema)
                       (schema/nilable :method-params {:cljplot2/section :fit :description "Options of the fit method, e.g. the loess `:bandwidth` and `:iters` (robustness iterations, `0` for ggplot2's plain fit) or the `:lm` `:degree`. Other methods get it as an extra argument."
                                                       :cljplot2/default-doc "The method's own defaults; `:loess` uses `{:bandwidth 0.75 :degree 2}`."
                                                       :cljplot2/example {:bandwidth 0.3}}
                                       [:map-of keyword? any?])
                       (schema/optional :band {:cljplot2/section :band :description "The band drawn around the curve."
                                               :cljplot2/default-doc "`:confidence` for `:loess`, `:lm`, `:glm` and `:gp`; `:none` for the other methods."
                                               :cljplot2/value-meanings {:confidence "Where the fitted curve may lie, at the confidence `:level`."
                                                                 :prediction "Where new points may fall, at the confidence `:level` (only with `:method :lm`)."
                                                                 :bootstrap "A band from refitting resampled points, for any method (see `:bootstrap`)."
                                                                 :none "No band."}
                                               :cljplot2/example :bootstrap}
                                        (into [:enum] smooth/band-kinds))
                       (schema/optional :level {:cljplot2/section :band :description "The band's level."
                                                :default 0.95
                                                :cljplot2/form-meanings {::schema/open-fraction "One band at this level."
                                                                         :vector "Nested bands, one per level; the widest is drawn first."}
                                                :cljplot2/example [0.5 0.95]}
                                        [:or schema/open-fraction [:vector {:min 1} schema/open-fraction]])
                       (schema/nilable :bootstrap {:cljplot2/section :band :description "Options of the bootstrap band; they apply with `:band :bootstrap`."}
                                       [:map {:closed true}
                                        (schema/optional :samples {:description "How many resampled fits make the band." :default 200 :cljplot2/example 500
                                                                   :cljplot2/example-context {:band :bootstrap}} schema/pos-int)
                                        (schema/optional :rng {:description "The random generator that draws the samples. A seeded one gives the same band on every build."
                                                               :cljplot2/default-doc "An unseeded generator, so each build draws a different band."
                                                               :cljplot2/example '(r/rng :mersenne 7)
                                                               :cljplot2/example-context {:band :bootstrap}}
                                                         rng/schema)
                                        (schema/optional :ci {:description "How the interval at each point of the band is computed from the samples."
                                                            :default :percentile
                                                            :cljplot2/example-context {:band :bootstrap}
                                                            :cljplot2/value-meanings {:percentile "The samples' percentiles."
                                                                              :basic "The percentiles reflected around the fitted value."
                                                                              :bc "Percentiles corrected for the samples' bias."
                                                                              :normal "A normal interval from the samples' standard deviation."}}
                                                         (into [:enum] (sort smooth/bootstrap-intervals)))
                                        (schema/optional :estimation {:description "How the samples' percentiles are estimated: `:legacy` or one of R's quantile types `:r1` to `:r9`. `:ci :normal` doesn't use it."
                                                                      :default :legacy
                                                                      :cljplot2/example :r7
                                                                      :cljplot2/example-context {:band :bootstrap}}
                                                         keyword?)])
                       (schema/nilable :n {:cljplot2/section :fit :description "How many points along x the curve is evaluated at." :default 200 :cljplot2/example 50} [:and :int [:>= 2]])
                       (schema/nilable :full-range {:cljplot2/section :fit :description "Whether each group's curve spans a common x range instead of the group's own."
                                                    :default false
                                                    :cljplot2/value-meanings {true "The plot's x `:domain` when it's given, else the x range of all groups in the panel."
                                                                      false "The group's own x range."}}
                                       schema/bool)
                       (first (schema/in-section :fit [(common/stat-space-option :scale "The space the curve is fitted in."
                                                                                           {:cljplot2/value-meanings {:scale "The axes' coordinates: on a `:log` axis the fit uses log10 values and leaves out zeros and negatives."
                                                                                                              :data "Data units, whatever the axes."}})]))
                       (schema/optional :padding {:cljplot2/section :fit :description "Room around the data on x and y, as fractions of the data span."
                                                  :cljplot2/default-doc "`0.05` on every side."
                                                  :cljplot2/example {:x [0 0]}}
                                        data/position-padding)))
             option-rules)
   :mapping [:map
             [:x {:description "The x of the points, a number or a date."} schema/accessor]
             [:y {:description "The values the curve fits."} schema/accessor]
             (schema/nilable :color {:description "Splits the rows into groups, each with its own curve, band and colour."} schema/accessor)
             (schema/nilable :dash {:description "Splits the rows into groups, each curve with its own dash style."} schema/accessor)
             (schema/nilable :pattern {:description "Splits the rows into groups, each band with its own fill pattern."} schema/accessor)
             (schema/nilable :group {:description "Splits the rows into groups that look the same, each with its own fit."} schema/accessor)]})

(defn- fit-spec
  "Gives the fit of a smooth chart (its options valid, see [[option-rules]]).

  Parameters:

  - `options` (map): the chart's options - `:method` (default `:loess`), `:method-params`, `:band` (default by the method), `:level` (default 0.95), `:bootstrap`.
  - `st` (map): the resolved style - `:fill`.

  Returns `{:method :params :band :levels :bootstrap}`: `:band :none` for a `:fill :none`; `:levels` a vector; the bootstrap `:rng` copied for this build."
  [options st]
  (let [method (data/option options :method :loess)
        band (data/option options :band (smooth/default-band method))
        level (data/option options :level 0.95)]
    {:method method :params (:method-params options) :band (if (theme/none? (:fill st)) :none band) :levels (if (number? level) [level] level)
     :bootstrap (when (= :bootstrap band) (update (:bootstrap options) :rng rng/for-build))}))

(defn- group-points
  "Gives the fit points of one group.

  Parameters:

  - `g` (map): the group - `:rows`.
  - `x-of`, `y-of` (functions): the x and y accessors.
  - `[tx ty]`: the axes' stat transforms (see [[cljplot2.scale.position/stat-transform]]).

  Returns a vector of `[key x y]` of the rows with a valid x and a finite y: `key` ([[cljplot2.data.rows/x-key]]) and `y` in the fit coordinates; points a transform can't place (e.g. 0 on a log axis) are left out.

  Throws `ex-info {:key :mapping :in [:mapping :x]}` for a valid x without a key (not numeric, not temporal)."
  [g x-of y-of [tx ty]]
  (let [fx (:forward tx) fy (:forward ty)]
    (into [] (keep (fn [r]
                     (let [x (x-of r) y (y-of r)]
                       (when (and (data/valid-value? x) (schema/finite? y))
                         (let [k (fx (double (or (data/x-key x)
                                                 (schema/fail! "Smooth needs numeric or temporal (java.time) x." {:key :mapping :in [:mapping :x] :value x}))))
                               v (fy (double y))]
                           (when (and (m/valid-double? k) (m/valid-double? v))
                             [k x v]))))))
          (:rows g))))

(defn- unplaced-counts
  "Counts the rows a stat transform can't place.

  Parameters:

  - `rows` (sequence of maps): every row.
  - `x-of`, `y-of` (functions): the x and y accessors.
  - `[tx ty]`: the axes' stat transforms.

  Returns `{:x n :y n}`: rows with a valid x and a finite y whose x the x transform can't place (e.g. 0 on a log axis), and of the others those whose y the y transform can't place - each row counted once."
  [rows x-of y-of [tx ty]]
  (let [fx (:forward tx) fy (:forward ty)]
    (reduce (fn [acc r]
              (let [x (x-of r) y (y-of r)
                    k (when (and (data/valid-value? x) (schema/finite? y)) (data/x-key x))]
                (cond
                  (nil? k) acc
                  (not (m/valid-double? (fx (double k)))) (update acc :x m/long-inc)
                  (not (m/valid-double? (fy (double y)))) (update acc :y m/long-inc)
                  :else acc)))
            {:x 0 :y 0} rows)))

(defn- ends
  "Gives the ends of fit points.

  Parameters:

  - `points` (non-empty sequence): `[key x ..]`.

  Returns `[[lo-key lo-x] [hi-key hi-x]]` - the points of the smallest and the largest key."
  [points]
  [(let [p (apply min-key first points)] [(first p) (second p)])
   (let [p (apply max-key first points)] [(first p) (second p)])])

(defn- domain-ends
  "Gives the ends of the plot's x domain, a partial one completed by the data.

  Parameters:

  - `plot-scales` (map): the plot's `:scales` - `[:x :domain]` (numbers or `java.time` values; read through [[cljplot2.data.rows/given-domain]]).
  - `tx` (map): the x stat transform - `:forward`.
  - `data-ends` (`[[lo-key lo-x] [hi-key hi-x]]` or `nil`): the ends of the fit points (see [[ends]]).

  Returns `[[lo-key lo-x] [hi-key hi-x]]`, keys in the transform's coordinates: a full domain's ends; a partial `[lo nil]` / `[nil hi]` - its given end and the data's other one (`nil` without data, or when the data lie beyond the given end - the x scale reports that); `nil` without a keyed domain (also `[nil nil]`)."
  [plot-scales tx data-ends]
  (let [domain (data/given-domain (get-in plot-scales [:x :domain]))
        keyed (fn [x] [((:forward tx) (double (data/x-key x))) x])]
    (cond
      (data/partial-domain? domain)
      (when data-ends
        (let [[lo hi] domain
              [[^double a] [^double b] :as completed] [(if (some? lo) (keyed lo) (first data-ends))
                                                       (if (some? hi) (keyed hi) (second data-ends))]]
          (when (m/< a b) completed)))
      (and (seq domain) (every? data/x-key domain)) (ends (map keyed domain)))))

(defn- fitted-curve
  "Fits the curve of one group.

  Parameters:

  - `points` (vector): the group's fit points (see [[group-points]]).
  - `[[k0 x0] [k1 x1]]`: the range the curve spans, keys and x values.
  - `spec` (map): the fit (see [[fit-spec]]).
  - `n` (long): grid points over the range.
  - `aesthetics` (map): the group's aesthetic values, for errors.
  - `[tx ty]`: the axes' stat transforms - the curve taken back to data units.

  Returns `{:x [..] :y [..] :bands [{:level :lo :hi}] :ends [x0 x1] :aesthetics}`.

  Throws `ex-info {:key :method}` naming the group for a failed fit."
  [points [[^double k0 x0] [^double k1 x1]] spec n aesthetics [tx ty]]
  (let [like (second (first points))
        ix (:inverse tx)
        iy (:inverse ty)
        grid-keys (curve/sample-grid k0 k1 {:count n})
        {:keys [fit bands]} (try (smooth/fit-curve spec (mapv first points) (mapv #(nth % 2) points) grid-keys)
                                 (catch Exception e
                                   (schema/fail! (str "Smooth fit failed: " (ex-message e)
                                                      (when (seq aesthetics) (str " Group: " (pr-str aesthetics) ".")))
                                                 {:key :method :value (:method spec) :group aesthetics :cause (ex-message e)})))]
    {:x (mapv #(data/key->x (ix (double %)) like) grid-keys)
     :y (mapv #(iy (double %)) fit)
     :bands (mapv (fn [band] (-> band (update :lo #(mapv (fn [v] (iy (double v))) %)) (update :hi #(mapv (fn [v] (iy (double v))) %))))
                  bands)
     :ends [x0 x1] :aesthetics aesthetics}))

(defn- finite
  "Picks the finite values of numbers.

  Parameters:

  - `vs` (sequence of numbers): the values.

  Returns a lazy sequence of the finite ones."
  [vs]
  (filter schema/finite? vs))

(defn compute
  "Computes the fitted curves of every panel.

  Parameters:

  - `chart` (map): the chart value - `:options` (see the namespace doc).
  - `split` (map): `:groups` - panel -> rows, `:mapping` - the merged mapping.
  - `env` (map): `:theme`, `:plot-scales` - the plot's `:scales` (axis transforms, the x `:domain`).

  Returns `{:data :extents :padding :padding-explicit :na :dropped}`: per panel `{:curves [{:x [..] :y [..] :bands [{:level :lo :hi}] :ends [x0 x1] :aesthetics}] :style st}` (see [[fitted-curve]]), `st` the resolved style; extents - x over the curves' ends, y over the fits and finite band bounds; `:dropped` - `:x`, `:y` (also values the stat transform can't place, see [[unplaced-counts]]), `:group` (groups too small for the method).

  Throws `ex-info {:key :mapping}` when `:x` or `:y` has no valid value in non-empty data, and the errors of [[group-points]], [[fitted-curve]]."
  [{:keys [options] :as chart} {:keys [groups mapping]} {:keys [theme plot-scales] :as env}]
  (let [st (theme/resolved-style theme chart)
        spec (fit-spec options st)
        {required-x :distinct-x required-rows :rows} (smooth/fit-requirements (:method spec) (:params spec))
        n (long (data/option options :n default-n))
        full-range? (data/option options :full-range false)
        {:keys [panels dropped]} (group/split groups mapping env)
        x-of (data/row-accessor :x (:x mapping))
        y-of (data/row-accessor :y (:y mapping))
        rows (mapcat (fn [gs] (mapcat :rows gs)) (vals panels))
        _ (data/check-mapped! rows [[:x x-of] [:y y-of]])
        space (data/option options :stat-space :scale)
        transforms [(scale/stat-transform-for space (:x plot-scales)) (scale/stat-transform-for space (:y plot-scales))]
        points (update-vals panels (fn [gs] (mapv #(group-points % x-of y-of transforms) gs)))
        unplaced (unplaced-counts rows x-of y-of transforms)
        fittable? (fn [pts] (and (m/>= (count pts) (long required-rows)) (m/>= (count (distinct (map first pts))) (long required-x))))
        data (into {} (map (fn [[panel gs]]
                             (let [pts (points panel)
                                   panel-range (when full-range?
                                                 (let [data-ends (some->> (seq (apply concat (filter fittable? pts))) ends)]
                                                   (or (domain-ends plot-scales (first transforms) data-ends) data-ends)))]
                               [panel {:curves (vec (keep (fn [[g p]]
                                                            (when (fittable? p)
                                                              (fitted-curve p (or panel-range (ends p)) spec n (:aesthetics g) transforms)))
                                                          (map vector gs pts)))
                                       :style st}])))
              panels)]
    (into (data/padding-stats data/default-padding {} options)
          {:data data
           :extents (merge-with merge (group/aesthetic-extents panels plot-scales)
                                (into {} (keep (fn [[panel {:keys [curves]}]]
                                                 (when (seq curves)
                                                   [panel {:x (data/extent :x (mapcat :ends curves))
                                                           :y (data/extent :y (concat (finite (mapcat :y curves))
                                                                                      (finite (mapcat (fn [c] (mapcat #(concat (:lo %) (:hi %)) (:bands c)))
                                                                                                      curves))))}])))
                                      data))
           :na (group/na-dims panels)
           :dropped (assoc dropped
                           :x (m/long-add (data/invalid-count x-of rows) (long (:x unplaced)))
                           :y (m/long-add (count (remove #(schema/finite? (y-of %)) rows)) (long (:y unplaced)))
                           :group (count (remove fittable? (mapcat val points))))})))

(defn draw
  "Draws the curves of one panel: the bands of every group (widest first, see [[cljplot2.graphics.curve/draw-bands!]]), then every line.

  Parameters:

  - `computed` (map): `:curves`, `:style` (see [[compute]]).
  - `canvas`: the clojure2d canvas.
  - `scales` (map): the finalized scales.
  - `env` (map): `:theme`.

  A group whose aesthetic value is outside its scale isn't drawn."
  [_chart {:keys [curves style]} canvas _box scales env]
  (when (seq curves)
    (let [projection (scale/projection scales)
          drawn (vec (keep (fn [c] (when-let [st (group/style style (:aesthetics c) scales (:theme env))] [c st])) curves))]
      (curve/draw-bands! canvas
                          (vec (for [[{:keys [x bands]} st] drawn
                                     {:keys [lo hi]} bands]
                                 {:runs (curve/band-runs x hi lo projection {}) :fill (polyline/area-color st false)
                                  :pattern-fill (draw/pattern-fill st (polyline/area-color st false))}))
                          {:scales scales})
      (doseq [[{:keys [x y]} st] drawn
              :let [points (mapv (fn [vx vy] (let [^Vec2 p (projection vx vy)] (curve/drawn-point [(.x p) (.y p)]))) x y)]]
        (polyline/draw! canvas (mapv first points) (mapv second points) st)))))

(defn legend
  "Draws the chart's key glyph: a filled square in the band colour (without a band - none) under a horizontal line in the line style.

  Parameters:

  - `chart` (map): the chart value - `:options` `:band`, `:method`.
  - `attributes` (map): the key's mapped attributes - `:color`, `:dash`, `:pattern`.
  - `canvas`: the clojure2d canvas.
  - `box` (map): the key's box.
  - `env` (map): `:theme`."
  [{:keys [options] :as chart} attributes canvas box env]
  (let [st (theme/key-style env chart attributes [:color :dash :pattern])
        band (data/option options :band (smooth/default-band (data/option options :method :loess)))]
    (when-not (= :none band)
      (polyline/key-glyph! canvas box {:color :none :line-width 0} (polyline/area-color st false) (draw/pattern-fill st (polyline/area-color st false))))
    (polyline/key-glyph! canvas box st :none)))
