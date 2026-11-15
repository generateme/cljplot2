(ns cljplot2.chart.smooth
  "Smooth: a fitted curve of `:y` on `:x` per panel and group, with bands (see [[cljplot2.data.smooth]]).

  Options (style defaults from theme `[:chart :smooth]`):

  * `:method` - `:loess` (default), `:lm`, `:glm`, `:gp`, any other fastmath 1D interpolation method or a function
    `(fn [xs ys] f)`; `:method-params` - a map: the options of fastmath `loess` (`:bandwidth`, `:iters`, ...), `lm`
    (`:degree` - polynomial degree, default 1 - and lm options), `glm` (`:family`, ...), `gaussian-process` (`:kernel`,
    `:noise`, `:kscale`, `:normalize?`), else the extra argument of the method (or function)
  * `:band` - `:confidence` (default for `:loess`, `:lm`, `:glm`, `:gp`), `:prediction` (`:lm`), `:bootstrap` (any
    method; `:bootstrap {:samples :rng :ci :estimation}` - see `fastmath.interpolation/bootstrap`; `:rng` is copied
    per build, see [[cljplot2.data.rng/for-build]]) or `:none`
    (default for the other methods; their `:confidence` throws `ex-info {:key :band}`); `:level` - a number in (0, 1)
    or a vector of them (nested bands, the widest drawn first; repeated levels drawn once), default 0.95
  * `:n` - grid points (default 200) over each group's x range; `:full-range true` - over the plot's x `:domain`
    when it is given, else over the x range of all groups of the panel
  * `:color`, `:width`, `:dash` (line), `:fill` (bands: `:color` - default, the group colour at `:alpha`, default
    0.3 - a colour or `:none`: no band, as `:band :none`)
  * `:padding {:x [lo hi] :y [lo hi]}` - default 5% each side

  x is numeric or temporal (`java.time`, fitted in epoch milliseconds - see [[cljplot2.data.rows/x-key]]; parameters
  in x units, such as a `:gp` kernel's `:sigma`, are in milliseconds then); rows with an
  invalid x or y are left out (counted). Groups (see [[cljplot2.data.group]]) come from ordinal aesthetics (`:color`)
  and `:group`, one fit per group; a group with fewer distinct x than the method needs (see
  [[cljplot2.data.smooth/required-points]]) has no curve and is counted in `:dropped {:group n}`; other fit errors
  throw `ex-info {:key :method}` naming the group. A band undefined at a point (loess outside the data) breaks there."
  (:require [cljplot2.chart.ribbon :as ribbon]
            [cljplot2.data.rows :as data]
            [cljplot2.data.group :as group]
            [cljplot2.data.rng :as rng]
            [cljplot2.data.smooth :as smooth]
            [cljplot2.graphics.curve :as curve]
            [cljplot2.graphics.polyline :as polyline]
            [cljplot2.scale.position :as scale]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme]
            [fastmath.core :as m])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private ^:const default-n 200)

(def ^:private style-entries
  (conj polyline/style-entries
        (schema/optional :fill [:or [:= :color] schema/color])
        (schema/optional :alpha schema/fraction)))

(def ^:private method-schema
  (schema/named-type ::method #(or (fn? %) (contains? smooth/smooth-methods %))
                     (str "should be a function (fn [xs ys] f) or one of " (vec (sort smooth/smooth-methods)))))

(def ^:private level-schema
  (schema/named-type ::level #(and (number? %) (< 0.0 (double %) 1.0)) "should be a number in (0 1)"))

(def schemas
  "Schemas of the smooth chart: `:style` (theme `[:chart :smooth]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (conj (schema/optional :method method-schema)
                      (schema/nilable :method-params [:map-of keyword? any?])
                      (schema/optional :band (into [:enum] smooth/band-kinds))
                      (schema/optional :level [:or level-schema [:vector {:min 1} level-schema]])
                      (schema/nilable :bootstrap [:map {:closed true}
                                                  (schema/optional :samples schema/pos-int)
                                                  (schema/optional :rng rng/schema)
                                                  (schema/optional :ci (into [:enum] (sort smooth/bootstrap-intervals)))
                                                  (schema/optional :estimation keyword?)])
                      (schema/nilable :n [:and :int [:>= 2]])
                      (schema/nilable :full-range schema/bool)
                      (schema/optional :padding data/position-padding)))
   :mapping [:map [:x schema/accessor] [:y schema/accessor]]})

(defn- style
  [options theme]
  (merge (get-in theme [:chart :smooth]) (select-keys options [:color :width :dash :fill :alpha])))

(defn- fit-spec
  "The fit of the chart `options` with style `st`: `{:method :params :band :levels :bootstrap}` (`:band :none` for a
  `:fill :none`; the bootstrap `:rng` copied for this build). Throws `ex-info` for `:degree` without `:lm` or not a
  positive integer, a loess `:bandwidth` outside (0, 1] (`{:key :method-params}`), a band the method has none of
  (`{:key :band}`)."
  [options st]
  (let [method (get options :method :loess)
        params (:method-params options)
        band (get options :band (smooth/default-band method))
        level (get options :level 0.95)]
    (when (and (= :loess method) (contains? params :bandwidth)
               (not (and (number? (:bandwidth params)) (< 0.0 (double (:bandwidth params))) (<= (double (:bandwidth params)) 1.0))))
      (schema/fail! ":method-params :bandwidth of loess should be a number in (0 1]." {:key :method-params :value (:bandwidth params)}))
    (when (and (contains? params :degree) (not= :lm method))
      (schema/fail! ":method-params :degree is for :method :lm." {:key :method-params :value params}))
    (when (and (= :lm method) (contains? params :degree) (not (and (integer? (:degree params)) (pos? (long (:degree params))))))
      (schema/fail! ":method-params :degree should be a positive integer." {:key :method-params :value (:degree params)}))
    (when (and (= :confidence band) (not (contains? smooth/error-methods method)))
      (schema/fail! (str "The method " (pr-str method) " has no confidence band; use :band :bootstrap (or :none).")
                    {:key :band :value band}))
    (when (and (= :prediction band) (not= :lm method))
      (schema/fail! ":band :prediction is for :method :lm." {:key :band :value band}))
    {:method method :params params :band (if (theme/none? (:fill st)) :none band) :levels (if (number? level) [level] level)
     :bootstrap (when (= :bootstrap band) (update (:bootstrap options) :rng rng/for-build))}))

(defn- group-points
  "`[[key x y] ..]` of the rows of group `g` with a valid x and a finite y (`key` - [[cljplot2.data.rows/x-key]]); a
  valid x without a key (not numeric, not temporal) throws `ex-info {:key :x}`."
  [g x-of y-of]
  (into [] (keep (fn [r]
                   (let [x (x-of r) y (y-of r)]
                     (when (and (data/valid-value? x) (number? y) (m/valid-double? (double y)))
                       [(or (data/x-key x)
                            (schema/fail! "Smooth needs numeric or temporal (java.time) x." {:key :x :value x}))
                        x (double y)]))))
        (:rows g)))

(defn- ends
  "`[[lo-key lo-x] [hi-key hi-x]]` of `points` (`[key x ..]`)."
  [points]
  [(let [p (apply min-key first points)] [(first p) (second p)])
   (let [p (apply max-key first points)] [(first p) (second p)])])

(defn- domain-ends
  "`[[lo-key lo-x] [hi-key hi-x]]` of the plot's x `:domain` in `plot-scales` (numbers or `java.time` values), else
  `nil`."
  [plot-scales]
  (let [domain (get-in plot-scales [:x :domain])]
    (when (and (sequential? domain) (seq domain) (every? data/x-key domain))
      (ends (map (fn [x] [(data/x-key x) x]) domain)))))

(defn- fitted-curve
  "Curve of a group's `points` over the range `[[k0 x0] [k1 x1]]` for the fit `spec`: `{:x [..] :y [..] :bands
  [..] :ends [x0 x1] :aesthetics}`; fit errors throw `ex-info {:key :method}` naming the group."
  [points [[k0 x0] [k1 x1]] spec n aesthetics]
  (let [like (second (first points))
        keys (curve/sample-grid (double k0) (double k1) {:count n})
        {:keys [fit bands]} (try (smooth/fit-curve spec (mapv first points) (mapv #(nth % 2) points) keys)
                                 (catch Exception e
                                   (schema/fail! (str "Smooth fit failed: " (ex-message e)
                                                      (when (seq aesthetics) (str " Group: " (pr-str aesthetics) ".")))
                                                 {:key :method :value (:method spec) :group aesthetics :cause (ex-message e)})))]
    {:x (mapv #(data/key->x % like) keys) :y fit :bands bands :ends [x0 x1] :aesthetics aesthetics}))

(defn- finite [vs] (filter #(m/valid-double? (double %)) vs))

(defn compute
  "Per panel: `{:curves [{:x [..] :y [..] :bands [{:level :lo :hi}] :ends [x0 x1] :aesthetics}] :style style}`;
  extents (x over the curves' ends, y over the fits and finite band bounds), padding, `:na`, `:dropped` (`:x`, `:y`,
  `:group`)."
  [{:keys [options]} {:keys [groups mapping]} {:keys [theme plot-scales] :as env}]
  (let [st (style options theme)
        spec (fit-spec options st)
        required (smooth/required-points (:method spec) (:params spec))
        n (long (or (:n options) default-n))
        full-range? (data/option options :full-range false)
        {:keys [panels dropped]} (group/split groups mapping env)
        x-of (data/row-accessor :x (:x mapping))
        y-of (data/row-accessor :y (:y mapping))
        rows (mapcat (fn [gs] (mapcat :rows gs)) (vals panels))
        _ (doseq [[k f] [[:x x-of] [:y y-of]]]
            (when (and (seq rows) (not-any? #(data/valid-value? (f %)) rows))
              (schema/fail! "No valid values for a mapped dimension - check the mapping." {:key :mapping :value k :rows (count rows)})))
        points (update-vals panels (fn [gs] (mapv #(group-points % x-of y-of) gs)))
        fittable? (fn [pts] (>= (count (distinct (map first pts))) required))
        data (into {} (map (fn [[panel gs]]
                             (let [pts (points panel)
                                   panel-range (when full-range?
                                                 (or (domain-ends plot-scales)
                                                     (some->> (seq (apply concat (filter fittable? pts))) ends)))]
                               [panel {:curves (vec (keep (fn [[g p]]
                                                            (when (fittable? p)
                                                              (fitted-curve p (or panel-range (ends p)) spec n (:aesthetics g))))
                                                          (map vector gs pts)))
                                       :style st}])))
              panels)]
    {:data data
     :extents (merge-with merge (group/aesthetic-extents panels plot-scales)
                          (into {} (keep (fn [[panel {:keys [curves]}]]
                                           (when (seq curves)
                                             [panel {:x (data/extent :x (mapcat :ends curves))
                                                     :y (data/extent :y (concat (finite (mapcat :y curves))
                                                                                (finite (mapcat (fn [c] (mapcat #(concat (:lo %) (:hi %)) (:bands c)))
                                                                                                curves))))}])))
                                data))
     :padding (merge {:x [0.05 0.05] :y [0.05 0.05]} (:padding options))
     :na (group/na-dims panels)
     :dropped (assoc dropped
                     :x (count (remove #(data/valid-value? (x-of %)) rows))
                     :y (count (remove #(let [y (y-of %)] (and (number? y) (m/valid-double? (double y)))) rows))
                     :group (count (remove fittable? (mapcat val points))))}))

(defn draw
  "Draw the curves of one panel: the bands of every group (widest first, see
  [[cljplot2.chart.ribbon/draw-bands!]]), then every line."
  [_chart {:keys [curves style]} canvas _box scales env]
  (when (seq curves)
    (let [projection (scale/projection scales)
          drawn (vec (keep (fn [c] (when-let [st (group/style style (:aesthetics c) scales (:theme env))] [c st])) curves))]
      (ribbon/draw-bands! canvas
                          (vec (for [[{:keys [x bands]} st] drawn
                                     {:keys [lo hi]} bands]
                                 {:runs (ribbon/band-runs x lo hi projection {}) :fill (ribbon/band-fill st)}))
                          nil nil scales)
      (doseq [[{:keys [x y]} st] drawn
              :let [points (mapv (fn [vx vy] (let [^Vec2 p (projection vx vy)] (curve/drawn-point [(.x p) (.y p)]))) x y)]]
        (polyline/draw! canvas (mapv first points) (mapv second points) st)))))

(defn legend
  "Key glyph: a filled square in the band colour (without a band - none) under a horizontal line in the line style."
  [{:keys [options]} attributes canvas box env]
  (let [st (merge (style options (:theme env)) (select-keys attributes [:color :dash]))
        band (get options :band (smooth/default-band (get options :method :loess)))]
    (when-not (= :none band)
      (polyline/key-glyph! canvas box {:color :none :width 0} (ribbon/band-fill st)))
    (polyline/key-glyph! canvas box st :none)))
