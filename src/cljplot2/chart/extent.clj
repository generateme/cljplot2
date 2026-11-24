(ns cljplot2.chart.extent
  "Extent: an interval of the values per level (the categorical axis, see `:level-axis` in [[cljplot2.data.strip]])
  and group, drawn as a line with end caps and a centre mark -
  a strip chart (see [[cljplot2.data.strip]]). Intervals come from the fastmath.stats extents.

  Options (style defaults from theme `[:chart :extent]`): `:color`, `:line-width` (`:width` is the strip option), `:shape`, `:size` (centre
  mark); `:method` - a keyword of [[extent-methods]] or a function of the values returning `[lo hi]` or `[lo hi centre]`
  (default `:min-max`); `:method-args` - extra arguments passed after the values (e.g. `[10.0 90.0]` for
  `:percentile`, `[0.01]` for `:ci`, `[alpha samples]` for the bootstrap methods; default none); bootstrap methods
  (`:bootstrap-normal`, `-basic`, `-percentile`, `-mad-median`, `-bc`, `-bca`, `-studentized`, `-t`, see
  [[bootstrap-intervals]]) - the interval of `:statistic` (a function of the values, default fastmath `mean`)
  resampled with `:rng` (a fastmath random generator, copied per build - every build draws the same samples, see
  [[cljplot2.data.rng/for-build]]; without it every build creates a fresh one); `:center` - `:method` (the method's centre, default) or `:none`;
  `:mirror` - a grouping dimension with two levels: the second level's intervals on the negative side (negated, as
  mirrored bars - error bars over `chart/bar` with the same `:mirror`; value tick labels show magnitudes, see
  [[cljplot2.data.strip]]);
  `:padding {:x [lo hi]}` (default `[0.05 0.05]`), plus the strip options and `:data` / `:mapping`.

  Functions: [[legend]] - the chart contract's key glyph (the chart computes and draws through [[cljplot2.chart.strip/compute]] and [[cljplot2.chart.strip/draw]]); [[interval]] - an interval of values by a method; [[extent-methods]], [[bootstrap-intervals]] - the methods."
  (:require [clojure2d.core :as c2d]
            [fastmath.core :as m]
            [fastmath.stats :as stats]
            [fastmath.stats.bootstrap :as boot]
            [cljplot2.data.group :as group]
            [cljplot2.data.rng :as rng]
            [cljplot2.data.rows :as data]
            [cljplot2.chart.strip :as strip-chart]
            [cljplot2.data.strip :as strip]
            [cljplot2.graphics.shape :as shape]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme])
  (:import [fastmath.vector Vec2]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def bootstrap-intervals
  "Bootstrap interval methods: keyword -> fastmath.stats.bootstrap interval of bootstrap results and `alpha`
  (`:bootstrap-mad-median` ignores `alpha`), each `[lo hi centre]`."
  {:bootstrap-normal boot/ci-normal
   :bootstrap-basic boot/ci-basic
   :bootstrap-percentile boot/ci-percentile
   :bootstrap-mad-median (fn [boot-data _alpha] (boot/ci-mad-median boot-data))
   :bootstrap-bc boot/ci-bc
   :bootstrap-bca boot/ci-bca
   :bootstrap-studentized boot/ci-studentized
   :bootstrap-t boot/ci-t})

(def extent-methods
  "Extent methods: keyword -> fastmath.stats function of the values (and `:method-args`) returning
  `[lo hi centre]`, plus the [[bootstrap-intervals]] (see [[bootstrap-interval]])."
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
   :percentile-bc stats/percentile-bc-extent
   :percentile-bca stats/percentile-bca-extent})

(def ^:private style-entries
  "Map entries of the extent style (theme `[:chart :extent]`, also options)."
  [(schema/optional :color {:description "interval colour (a mapped :color wins)"} schema/color)
   (schema/optional :line-width {:description "interval line width in pixels"} schema/non-neg)
   (schema/optional :shape {:description "shape of the centre mark (a mapped :shape wins)"} schema/shape)
   (schema/optional :size {:description "size of the centre mark in pixels (a mapped :size wins)"} schema/non-neg)])

(def schemas
  "Schemas of the extent chart: `:style` (theme `[:chart :extent]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into theme/theme-style-entries)
                (into strip/options)
                (conj (schema/optional :method {:description "interval of the values: a fastmath extent or bootstrap method keyword, or a function of the values returning [lo hi] or [lo hi centre]"
                                                :default :min-max}
                                       [:or (into [:enum] (sort (concat (keys extent-methods) (keys bootstrap-intervals))))
                                        schema/function])
                      (schema/nilable :statistic {:description "statistic of the values resampled by the bootstrap methods"
                                                  :cljplot2/default-doc "fastmath mean"}
                                      schema/function)
                      (schema/nilable :method-args {:description "extra arguments passed to the method after the values (e.g. [10.0 90.0] for :percentile)"}
                                      [:sequential any?])
                      (schema/nilable :rng {:description "fastmath random generator of the bootstrap methods, copied per build (every build draws the same samples)"
                                            :cljplot2/default-doc "a fresh generator every build"}
                                      rng/schema)
                      (schema/optional :center {:description "centre mark: :method (the method's centre) or :none"
                                                :default :method}
                                       [:enum :method :none])
                      strip/mirror-option
                      (schema/optional :padding {:description "padding of the data span per position axis, {:x [lo hi] :y [lo hi]} fractions of the span"
                                                 :cljplot2/default-doc "[0.05 0.05] along the value axis; with :place :bottom also :bottom-share below the data"}
                                       data/position-padding)))
   :mapping (strip/values-mapping
              [(schema/nilable :x {:description "values (data units) on the value axis, or levels for vertical intervals"} schema/accessor)
               (schema/nilable :y {:description "levels on the categorical axis (absent - one interval), or values for vertical intervals"}
                               schema/accessor)
               (schema/nilable :color {:description "interval colour per group (gives legend keys)"} schema/accessor)
               (schema/nilable :shape {:description "centre mark shape per group"} schema/accessor)
               (schema/nilable :size {:description "centre mark size per group"} schema/accessor)
               (schema/nilable :group {:description "groups without a visual aesthetic (dodged as other groups)"} schema/accessor)])})

(defn- bootstrap-interval
  "Gives a bootstrap interval of a statistic of values.

  Parameters:

  - `ci` (keyword): a key of [[bootstrap-intervals]].
  - `rng`: the fastmath random generator resampling the values.
  - `statistic` (function or `nil`): a function of the values; `nil` - fastmath `mean`.
  - `values` (sequence of numbers): the values.
  - `alpha` (double, optional): the interval's significance level (default 0.05).
  - `samples` (long, optional): the number of bootstrap samples (default 500).

  Returns `[lo hi centre]` (see fastmath.stats.bootstrap `bootstrap`)."
  ([ci rng statistic values] (bootstrap-interval ci rng statistic values 0.05))
  ([ci rng statistic values alpha] (bootstrap-interval ci rng statistic values alpha 500))
  ([ci rng statistic values alpha samples]
   ((bootstrap-intervals ci) (boot/bootstrap values (or statistic stats/mean) {:samples samples :rng rng}) alpha)))

(defn interval
  "Gives the interval of values by an extent method.

  Parameters:

  - `values` (sequence of numbers): the values.
  - `method`: a keyword of [[extent-methods]] or [[bootstrap-intervals]], or a function of the values (and the arguments) returning `[lo hi]` or `[lo hi centre]`.
  - `options` (map): `:method-args` - extra arguments passed after the values; `:rng`, `:statistic` - of the bootstrap methods (see [[bootstrap-interval]]).

  Returns `{:lo :hi :center}` as doubles; `:center` `nil` when the method gives none.

  Throws `ex-info {:key :method-args}` when the method rejects the arguments, `{:key :method}` for a result that isn't 2 or 3 numbers."
  [values method {:keys [method-args rng statistic]}]
  (let [f (cond
            (contains? bootstrap-intervals method) (partial bootstrap-interval method rng statistic)
            (keyword? method) (extent-methods method)
            :else method)
        [lo hi c :as result] (try (apply f values method-args)
                                  (catch clojure.lang.ArityException e
                                    (schema/fail! (str "Extent :method " (pr-str method) " doesn't take :method-args "
                                                       (pr-str method-args) ".")
                                                  {:key :method-args :value method-args :cause (ex-message e)}))
                                  (catch ClassCastException e
                                    (schema/fail! (str "Extent :method " (pr-str method) " can't use :method-args "
                                                       (pr-str method-args) ".")
                                                  {:key :method-args :value method-args :cause (ex-message e)})))]
    (when-not (and (sequential? result) (m/<= 2 (count result) 3) (every? number? result))
      (schema/fail! "Extent :method should return [lo hi] or [lo hi centre]." {:key :method :value result}))
    {:lo (double lo) :hi (double hi) :center (some-> c double)}))

(defn- mirrored
  "Gives an interval on the negative side.

  Parameters:

  - `interval` (map): `:lo`, `:hi` (doubles), `:center` (or `nil`).

  Returns `{:lo -hi :hi -lo :center -center}` (`:center` `nil` stays `nil`)."
  [{:keys [^double lo ^double hi center]}]
  {:lo (m/- hi) :hi (m/- lo) :center (when center (m/- (double center)))})

(defn- compute-in-strip-frame
  "Computes the chart's stats in the strip frame.

  Parameters:

  - `chart` (map): the chart value (`:options` - `:method`, `:method-args`, `:rng`, `:statistic`, `:mirror`, `:padding` ...).
  - `chart-split` (map): `:groups`, `:mapping`.
  - `env` (map): the compute environment (`:theme`, `:plot-scales` ...).

  Returns per panel `{:items [..] :style st :place p :options o}`, items with their [[interval]] (negated for `:mirrored?` items); extents (x - the intervals and centres), padding, `:na`, `:dropped`, `:tick-format` (`:absolute` with a mirror).

  Throws as [[interval]]."
  [{:keys [options] :as chart} chart-split {:keys [theme] :as env}]
  (let [{:keys [place panels dropped mirror?] split-items :items} (strip/split chart-split (cond-> options (:mirror options) (assoc ::strip/mirror (:mirror options)))
                                                                               :center env)
        method (data/option options :method :min-max)
        rng (rng/for-build (:rng options))
        side (fn [it iv] (if (:mirrored? it) (mirrored iv) iv))
        items (update-vals split-items (fn [its] (mapv #(merge % (side % (interval (:values %) method {:method-args (:method-args options) :rng rng
                                                                                              :statistic (:statistic options)}))) its)))
        st (theme/resolved-style theme chart)]
    {:data (update-vals items (fn [its] {:items its :style st :place place :options options}))
     :extents (merge-with merge (group/aesthetic-extents panels (:plot-scales env))
                          (strip/extents place items (fn [{:keys [^double lo ^double hi center]}]
                                                       [(m/min lo (double (or center lo)))
                                                        (m/max hi (double (or center hi)))])))
     :padding (strip/padding place options)
     :padding-explicit (strip/explicit-padding place options)
     :padding-px (strip-chart/n-label-room options items env)
     :na (group/na-dims panels)
     :dropped dropped
     :tick-format (when mirror? {:x :absolute})}))

(defn- draw-in-strip-frame
  "Draws the intervals of one panel in the strip frame: the line, end caps (half the item's cross range) and the centre mark.

  Parameters:

  - `_chart` (map): the chart value.
  - `computed` (map): the panel's `:items`, `:style`, `:place`, `:options` (`:center` - `:none` leaves the centre mark out).
  - `canvas`: the clojure2d canvas.
  - `box` (map): the panel box.
  - `scales` (map): the finalized scales (strip frame).
  - `env` (map): the drawing environment.

  An item whose low end can't be placed, or whose aesthetics are outside their scales, isn't drawn; n labels go past the far end (the near one of a mirrored item)."
  [_chart {:keys [items style place options]} canvas box scales env]
  (let [{:keys [point] :as pl} (strip-chart/placement box scales options place)
        center? (not= :none (data/option options :center :method))]
    (strip-chart/draw-n-labels! canvas pl items (fn [{:keys [^double lo ^double hi center mirrored?]}]
                                            (if mirrored?
                                              (m/min lo (double (or center lo)))
                                              (m/max hi (double (or center hi)))))
                          options scales env)
    (doseq [{:keys [level slot lo hi center aesthetics]} items
            :let [st (group/style style aesthetics scales (:theme env))
                  c0 (strip/in-slot slot 0.0)
                  cap (m/* 0.5 (strip/slot-half slot))
                  p (fn [v cross] (point v level cross))
                  ^Vec2 a (p lo c0)]
            :when (and st (data/finite-point? a))]
      (c2d/set-color canvas (:color st))
      (c2d/set-stroke-custom canvas {:size (:line-width st) :cap :butt})
      (c2d/line canvas a (p hi c0))
      (c2d/line canvas (p lo (m/- c0 cap)) (p lo (m/+ c0 cap)))
      (c2d/line canvas (p hi (m/- c0 cap)) (p hi (m/+ c0 cap)))
      (when (and center? center)
        (shape/draw! canvas (:shape st) (p center c0) (assoc st :stroke-color :none))))))

(defmethod strip-chart/in-frame-compute :extent [chart chart-split env] (compute-in-strip-frame chart chart-split env))

(defmethod strip-chart/in-frame-draw :extent [chart computed canvas box scales env]
  (draw-in-strip-frame chart computed canvas box scales env))

(defn legend
  "Draws the key glyph: a horizontal capped line with the centre mark.

  Parameters:

  - `chart` (map): the chart value.
  - `attributes` (map): the key's attributes (`:color`).
  - `canvas`: the clojure2d canvas.
  - `box` (map): the key box `:x`, `:y`, `:w`, `:h`.
  - `env` (map): the drawing environment (`:theme`)."
  [chart attributes canvas {:keys [^double x ^double y ^double w ^double h]} env]
  (let [st (theme/key-style env chart attributes [:color])
        x0 (m/+ x (m/* 0.15 w))
        x1 (m/+ x (m/* 0.85 w))
        cy (m/+ y (m/* 0.5 h))
        cap (m/* 0.25 h)]
    (c2d/set-color canvas (:color st))
    (c2d/set-stroke-custom canvas {:size (m/max 1.0 (double (:line-width st))) :cap :butt})
    (c2d/line canvas x0 cy x1 cy)
    (c2d/line canvas x0 (m/- cy cap) x0 (m/+ cy cap))
    (c2d/line canvas x1 (m/- cy cap) x1 (m/+ cy cap))
    (shape/draw! canvas (:shape st) (Vec2. (m/* 0.5 (m/+ x0 x1)) cy) (assoc st :size (m/min (double (:size st)) (m/* 0.6 h))
                                                                          :stroke-color :none))))
