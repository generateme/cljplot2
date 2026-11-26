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

(def method-descriptions
  "What each method of [[extent-methods]] and [[bootstrap-intervals]] gives, as sentences for option cards: keyword -> sentence."
  {:min-max "The smallest and the largest value."
   :stddev "The mean plus and minus one standard deviation."
   :mad "The median plus and minus the median absolute deviation."
   :sem "The mean plus and minus its standard error."
   :percentile "Two percentiles around the median, by default the 25th and 75th."
   :quantile "Two quantiles around the median, by default the 0.25 and 0.75 quantiles."
   :pi "The central interval holding a share of the values, with equal tails, by default half."
   :hpdi "The narrowest interval holding a share of the values, by default 95%."
   :adjacent "The box plot's whisker ends: the most extreme values within 1.5 IQR of the quartiles."
   :inner-fence "The quartiles widened by 1.5 IQR."
   :outer-fence "The quartiles widened by 3 IQR."
   :ci "The t-based confidence interval of the mean, by default 95%."
   :percentile-bc "A bias-corrected percentile interval, by default from the 2.5th to the 97.5th percentile."
   :percentile-bca "A bias-corrected and accelerated percentile interval, by default from the 2.5th to the 97.5th percentile."
   :bootstrap-normal "A bootstrap interval of `:statistic` by the normal approximation."
   :bootstrap-basic "A basic bootstrap interval of `:statistic`."
   :bootstrap-percentile "The percentile bootstrap interval of `:statistic`, by default 95% of 500 resamples."
   :bootstrap-mad-median "The median of the resampled `:statistic` plus and minus their median absolute deviation: a spread band, not a confidence interval."
   :bootstrap-bc "A bias-corrected bootstrap interval of `:statistic`."
   :bootstrap-bca "A bias-corrected and accelerated bootstrap interval of `:statistic`."
   :bootstrap-studentized "A studentized bootstrap interval of `:statistic`."
   :bootstrap-t "A bootstrap-t interval of `:statistic`."})

(def ^:private style-entries
  "Map entries of the extent style (theme `[:chart :extent]`, also options)."
  [(schema/optional :color {:description "The colour of the intervals; a mapped `:color` wins." :cljplot2/example :orange} schema/solid-color)
   (schema/optional :line-width {:description "The width of the interval lines in pixels." :cljplot2/example 5} schema/non-neg)
   (schema/optional :shape {:description "The shape of the centre mark; a mapped `:shape` wins." :cljplot2/example \s} schema/shape)
   (schema/optional :size {:description "The size of the centre mark in pixels; a mapped `:size` wins." :cljplot2/example 14} schema/non-neg)])

(def schemas
  "Schemas of the extent chart: `:style` (theme `[:chart :extent]`), `:options`, `:mapping` (merged)."
  {:style (into [:map {:closed true}] style-entries)
   :options (-> [:map {:closed true}]
                (into style-entries)
                (into data/common-options)
                (into theme/theme-style-entries)
                (into strip/options)
                (conj (schema/optional :method {:description "How the chart computes each interval from the values."
                                                :default :min-max
                                                :cljplot2/value-meanings (select-keys method-descriptions [:min-max :stddev :sem :ci :percentile :bootstrap-percentile])
                                                :cljplot2/other-values "Other fastmath extents and bootstrap intervals; the bootstrap ones resample `:statistic`."
                                                :cljplot2/form-meanings {::schema/function "A function of the values that returns `[lo hi]` or `[lo hi centre]`."}
                                                :cljplot2/example :stddev}
                                       [:or (into [:enum] (sort (concat (keys extent-methods) (keys bootstrap-intervals))))
                                        schema/function])
                      (schema/nilable :statistic {:description "The statistic the bootstrap methods compute on each resample, a function of the values."
                                                  :cljplot2/default-doc "The mean."
                                                  :cljplot2/example '(fn [vs] (stats/median vs))
                                                  :cljplot2/example-context {:method :bootstrap-percentile :rng '(r/rng :mersenne 7)}}
                                      schema/function)
                      (schema/nilable :method-args {:description "Extra arguments the method takes after the values, e.g. `[10.0 90.0]` for `:percentile`, `[0.01]` (alpha) for a 99% `:ci`, `[alpha samples]` for a bootstrap method."
                                                    :cljplot2/default-doc "None: the method's defaults."
                                                    :cljplot2/example [10.0 90.0] :cljplot2/example-context {:method :percentile}}
                                      [:sequential any?])
                      (schema/nilable :rng {:description "The random generator the bootstrap methods resample with. A seeded one gives the same intervals on every build."
                                            :cljplot2/default-doc "An unseeded generator, so each build resamples differently."
                                            :cljplot2/example '(r/rng :mersenne 7) :cljplot2/example-context {:method :bootstrap-percentile}}
                                      rng/schema)
                      (schema/optional :center {:description "What the chart marks at each interval's centre."
                                                :default :method
                                                :cljplot2/value-meanings {:method "The method's centre, e.g. the mean or median." :none "No centre mark."}}
                                       [:enum :method :none])
                      strip/mirror-option
                      strip/padding-option))
   :mapping (strip/values-mapping
              [(schema/nilable :x {:description "The values of horizontal intervals, or the levels of vertical ones."} schema/accessor)
               (schema/nilable :y {:description "The levels of horizontal intervals, or the values of vertical ones; with no levels the chart draws one interval."}
                               schema/accessor)
               (schema/nilable :color {:description "Splits the rows into groups, each with its own interval colour."} schema/accessor)
               (schema/nilable :shape {:description "Splits the rows into groups, each with its own centre mark shape."} schema/accessor)
               (schema/nilable :size {:description "Splits the rows into groups, each with its own centre mark size."} schema/accessor)
               (schema/nilable :group {:description "Splits the rows into groups that look the same, each its own interval beside the others of its level."} schema/accessor)])})

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

  Returns per panel `{:items [..] :style st :place p :options o}`, items with their [[interval]] (negated for `:mirrored` items); extents (x - the intervals and centres), padding, `:na`, `:dropped`, `:tick-format` (`:absolute` with a mirror).

  Throws as [[interval]]."
  [{:keys [options] :as chart} chart-split {:keys [theme] :as env}]
  (let [{:keys [place panels dropped mirror] split-items :items} (strip/split chart-split (cond-> options (:mirror options) (assoc ::strip/mirror (:mirror options)))
                                                                               :center env)
        method (data/option options :method :min-max)
        rng (rng/for-build (:rng options))
        side (fn [it iv] (if (:mirrored it) (mirrored iv) iv))
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
     :tick-format (when mirror {:x :absolute})}))

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
    (strip-chart/draw-n-labels! canvas pl items (fn [{:keys [^double lo ^double hi center] mirrored? :mirrored}]
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
