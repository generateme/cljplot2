(ns cljplot2.series-plots
  "Plot builders of time series: rows from [[cljplot2.data.series]] drawn with ordinary charts, returned as a plot map
  (edit it, add charts, or build the plot from the rows yourself).

  A builder's `:scales` option is merged into the builder's own scales key by key (see
  [[cljplot2.theme/deep-merge]])."
  (:require [clojure2d.color :as c]
            [cljplot2.chart :as chart]
            [cljplot2.data.series :as series]
            [cljplot2.multiplot :as multiplot]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private default-colors
  {:stem [69 117 180] :band [69 117 180] :significant [215 48 39]})

(def ^:private kinds-of
  "The ACF kinds (`acf-rows` `:partial?`) drawn for a builder `:partial?` value."
  {false [false] :acf [false] true [true] :pacf [true] :both [false true]})

(def ^:private kind-names {false "ACF" true "PACF"})

(def acf-options
  "Options schema of [[acf]]: its own entries, `:lags` / `:alpha` forwarded to
  [[cljplot2.data.series/acf-rows]] and the builder entries."
  (schema/options-map
   [(schema/nilable :partial? {:description "the correlograms drawn: :both (ACF above PACF), :acf or false (ACF), :pacf or true (PACF)"
                               :default :both}
                    [:enum :both :acf :pacf false true])
    (schema/nilable :band {:description "significance band: :smooth (a ribbon through the lags) or :stepped (a block per lag)"
                           :default :smooth}
                    [:enum :smooth :stepped])
    (schema/nilable :colors {:description "colours of the stems and points, the band and the significant lags' points"}
                    [:map {:closed true}
                     (schema/optional :stem {:description "colour of the stems and points" :default (:stem default-colors)}
                                      schema/color)
                     (schema/optional :band {:description "colour of the significance band" :default (:band default-colors)}
                                      schema/color)
                     (schema/optional :significant {:description "colour of the points of lags outside the band"
                                                    :default (:significant default-colors)}
                                      schema/color)])]
   series/acf-rows-entries
   multiplot/builder-entries))

(defn- stepped-band
  "Rows `{:x :lo :hi :lag :kind}` of a stepped band: each lag's band from `lag - 0.5` to `lag + 0.5` (a ribbon group
  per lag and kind)."
  [rows]
  (vec (mapcat (fn [{:keys [lag] :as row}]
                 (let [lag (double lag) base (select-keys row [:lo :hi :kind :lag])]
                   [(assoc base :x (- lag 0.5)) (assoc base :x (+ lag 0.5))]))
               rows)))

(defn- band-chart
  "The significance band of `rows`: a ribbon through the lags (`:smooth`) or a block per lag (`:stepped`)."
  [rows band color]
  (if (= band :stepped)
    (chart/ribbon {:data (stepped-band rows) :mapping {:x :x :ymin :lo :ymax :hi :group :lag} :color color})
    (chart/ribbon {:data rows :mapping {:x :lag :ymin :lo :ymax :hi} :color color})))

(defn acf
  "Correlogram of the series `values` as a plot map: ACF and PACF (see [[cljplot2.data.series/acf-rows]]) - stems
  from 0 (`chart/segment`) with points (`chart/scatter`), lags beyond the significance band highlighted, the band
  (Bartlett's per lag for ACF, constant for PACF), a rule at 0.

  `options`: `:lags`, `:alpha` (see `acf-rows`); `:partial?` - `:both` (default; ACF above PACF, facet rows by
  `:kind`, each row its own y scale - `:facets {:scales {:y :free}}` - unless `:scales {:y {:domain ..}}` sets one), `:acf` or `false` (ACF), `:pacf` or `true`
  (PACF); `:band` - `:smooth` (default, a ribbon through the lags, as R's `ci.type \"ma\"` line) or `:stepped` (a block per lag); `:colors {:stem :band
  :significant}`; `:scales`, `:title`, `:theme` (see [[acf-options]]). Throws `ex-info {:key k}` for an invalid option
  (checked first; `nil` options - the defaults).

  Equivalents: R `acf` / `pacf` plots, `forecast::ggtsdisplay` (lower panels)."
  {:cljplot2/options acf-options}
  ([values] (acf values {}))
  ([values {:keys [scales title] user-theme :theme :as options}]
   (schema/check-options! acf-options options)
   (let [{:keys [partial? band colors] :as options} (merge {:partial? :both :band :smooth :colors {}} (into {} (remove (comp nil? val)) options))
         {:keys [stem significant] band-color :band} (merge default-colors colors)
         kinds (kinds-of partial?)
         rows (vec (mapcat (fn [p?] (map #(assoc % :kind (kind-names p?))
                                         (series/acf-rows values (assoc (select-keys options [:lags :alpha]) :partial? p?))))
                           kinds))
         both? (= 2 (count kinds))]
     (cond-> {:data rows
              :mapping (cond-> {:x :lag :y :value} both? (assoc :facet-row :kind))
              :charts [(band-chart rows band band-color)
                       (chart/rule {:y 0.0})
                       (chart/segment {:yend 0.0 :color stem})
                       (chart/scatter {:color stem :size 5})
                       (chart/scatter {:data (filterv :significant? rows) :color significant :size 5})]
              :scales (theme/deep-merge {} scales)
              :labels {:title title :x "Lag" :y (when-not both? (kind-names (first kinds)))}}
       ;; ACF and PACF differ in range (lag 0 is 1 in the ACF): each panel row its own y domain, unless the caller
       ;; fixes one
       (and both? (nil? (get-in scales [:y :domain]))) (assoc :facets {:scales {:y :free}})
       user-theme (assoc :theme user-theme)))))

(def ^:private horizon-ends
  "Lightest and darkest colour of the default horizon band palettes: blues for positive, reds for negative values."
  {:positive [[158 202 225] [8 69 148]] :negative [[252 174 145] [165 15 21]]})

(defn- band-colors
  "`n` colours from the lightest to the darkest of `[light dark]` (one band - the darkest)."
  [[light dark] ^long n]
  (if (= 1 n)
    [(c/to-color dark)]
    (mapv #(c/lerp light dark (/ (double %) (double (dec n)))) (range n))))

(defn- horizon-colors
  "Colours of the bands `1..n`, `-1..-n` (see [[cljplot2.data.series/horizon-bands]]): `colors` `:positive` /
  `:negative` (at least `n` colours each, lightest first) or the default palettes. Throws `ex-info {:key :colors}`
  for fewer than `n` colours."
  [colors ^long n]
  (when-let [sign (some (fn [[k cs]] (when (< (count cs) n) k)) colors)]
    (schema/fail! (str "horizon :colors " sign " should hold at least " n " colors (:bands).")
                  {:key :colors :value colors}))
  (into (vec (take n (or (:positive colors) (band-colors (:positive horizon-ends) n))))
        (take n (or (:negative colors) (band-colors (:negative horizon-ends) n)))))

(def horizon-options
  "Options schema of [[horizon]]: its own entries, those forwarded to [[cljplot2.data.series/horizon-rows]] and the
  builder entries."
  (schema/options-map
   [(schema/nilable :colors {:description "band colours per sign, lightest first, at least :bands colours each"}
                    [:map {:closed true}
                     (schema/optional :positive {:description "colours of the positive bands, lightest first"
                                                 :cljplot2/default-doc ":bands blues from [158 202 225] to [8 69 148]"}
                                      [:sequential schema/color])
                     (schema/optional :negative {:description "colours of the negative (mirrored) bands, lightest first"
                                                 :cljplot2/default-doc ":bands reds from [252 174 145] to [165 15 21]"}
                                      [:sequential schema/color])])
    (schema/optional :alpha {:description "fill alpha of the bands (translucent bands show the ones beneath)" :default 1.0}
                     schema/fraction)]
   series/horizon-rows-entries
   multiplot/builder-entries))

(defn horizon
  "Horizon chart of `rows` as a plot map (see [[cljplot2.data.series/horizon-rows]]): values `:y` over `:x` folded
  into `:bands` (default 2) bands of one height, negatives mirrored upward in a second hue; every band an area
  (`chart/line {:fill :color}`) from 0 across the whole x range (no x padding, `:x {:nice false}`), the darker bands over the lighter
  ones; the y scale `[0 band-height]` without an axis; with `:series` a panel per series in one column, in their
  first-appearance order (the series in the strip; one band height for all; a `nil` series - the NA panel, last).

  `options`: `:x`, `:y`, `:series` (row keys), `:bands`, `:band-height`, `:interpolator`, `:steps-per-gap` (see
  `horizon-rows` - a curve through the values, e.g. `:monotone`); `:colors {:positive [..] :negative [..]}` (at least
  `:bands` colours each, lightest first; default blues / reds); `:alpha` - the bands' fill alpha (default 1, opaque;
  translucent bands show the ones beneath); `:scales`, `:title`, `:theme` (see [[horizon-options]]). Throws `ex-info
  {:key k}` for an invalid option `k` (checked first), fewer than `:bands` colours (`:colors`), x or y values.

  Equivalents: Vega-Lite `area_horizon`, Vega `horizon-graph`, latticeExtra `horizonplot`."
  {:cljplot2/options horizon-options}
  ([rows] (horizon rows {}))
  ([rows {:keys [x series bands colors scales title alpha] user-theme :theme :or {x :x bands 2 alpha 1.0} :as options}]
   (schema/check-options! horizon-options options)
   (let [{band-rows :rows h :band-height} (series/horizon-rows rows (select-keys options [:x :y :series :bands :band-height
                                                                                         :interpolator :steps-per-gap]))
         n (long bands)
         palette (horizon-colors (or colors {}) n)]
     (cond-> {:data band-rows
              :mapping (cond-> {:x :x :y :y :color :band} series (assoc :facet :series))
              :charts [(chart/line {:fill :color :alpha alpha :width 0 :padding {:x [0.0 0.0] :y [0.0 0.0]}})]
              :scales (theme/deep-merge (cond-> {:x {:nice false}
                                                 :y {:domain [0.0 h] :nice false :axis false}
                                                 :color {:kind :ordinal :domain (series/horizon-bands n) :range palette
                                                         :legend false}}
                                          ;; panels in the series' first-appearance order (facets would sort them)
                                          series (assoc :facet {:domain (vec (distinct (keep :series band-rows)))}))
                                        scales)
              :labels {:title title :x (name x) :y nil}}
       series (assoc :facets {:columns 1})
       user-theme (assoc :theme user-theme)))))
