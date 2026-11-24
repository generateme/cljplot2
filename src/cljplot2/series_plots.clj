(ns cljplot2.series-plots
  "Plot builders of time series: rows from [[cljplot2.data.series]] drawn with ordinary charts and returned as a plot map.

  The returned map can be edited, extended with charts, or rebuilt from the rows by hand. A builder's `:scales` option is merged into the builder's own scales key by key (see [[cljplot2.theme/deep-merge]]); its options are validated first against its options schema.

  Builders: [[acf]] (correlogram - ACF and PACF) with [[acf-options]], [[horizon]] (horizon chart) with [[horizon-options]]."
  (:require [fastmath.core :as m]
            [clojure2d.color :as c]
            [cljplot2.chart :as chart]
            [cljplot2.data.series :as series]
            [cljplot2.multiplot :as multiplot]
            [cljplot2.schema :as schema]
            [cljplot2.theme :as theme]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private default-colors
  "Default colours of a correlogram: the stems and points, the significance band, the points of significant lags."
  {:stem (c/color 69 117 180)
   :band (c/color 69 117 180)
   :significant (c/color 215 48 39)})

(def ^:private kinds-of
  "The ACF kinds (`acf-rows` `:partial` values) drawn for each builder `:correlograms` value."
  {:acf [false] :pacf [true] :both [false true]})

(def ^:private kind-names
  "Display name of an ACF kind (`:partial` value): panel strip and y axis title."
  {false "ACF" true "PACF"})

(def acf-options
  "Options schema of [[acf]]: its own entries, `:lags` / `:level` forwarded to [[cljplot2.data.series/acf-rows]] and the builder entries."
  (schema/options-map
   [(schema/nilable :correlograms {:description "the correlograms drawn: :both (ACF above PACF), :acf or :pacf"
                                   :default :both}
                    [:enum :both :acf :pacf])
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
   (remove #(= :partial (first %)) series/acf-rows-entries)
   multiplot/builder-entries))

(defn- stepped-band
  "Rows of a stepped significance band: a block per lag.

  Parameters:

  - `rows` (sequence of maps): ACF rows with `:lag`, `:lo`, `:hi` and `:kind`.

  Returns two rows `{:x :lo :hi :lag :kind}` per input row, at `lag - 0.5` and `lag + 0.5` (a ribbon group per lag and kind)."
  [rows]
  (vec (mapcat (fn [{:keys [^double lag] :as row}]
                 (let [base (select-keys row [:lo :hi :kind :lag])]
                   [(assoc base :x (m/- lag 0.5)) (assoc base :x (m/+ lag 0.5))]))
               rows)))

(defn- band-chart
  "The significance band chart of a correlogram.

  Parameters:

  - `rows` (sequence of maps): ACF rows with `:lag`, `:lo`, `:hi`.
  - `band` (keyword): `:smooth` - a ribbon through the lags, `:stepped` - a block per lag (see [[stepped-band]]).
  - `color` (colour): the band colour.

  Returns a ribbon chart."
  [rows band color]
  (if (= band :stepped)
    (chart/ribbon {:data (stepped-band rows) :mapping {:x :x :ymin :lo :ymax :hi :group :lag} :color color})
    (chart/ribbon {:data rows :mapping {:x :lag :ymin :lo :ymax :hi} :color color})))

(defn acf
  "Builds a correlogram of a series as a plot map: ACF and PACF by lag (see [[cljplot2.data.series/acf-rows]]).

  Stems from 0 (`chart/segment`) with points (`chart/scatter` in the theme style `[:chart :acf-point]`), the lags beyond the significance band highlighted, the band (Bartlett's per lag for ACF, constant for PACF) and a rule at 0.

  Parameters:

  - `values` (sequence of numbers): the series.
  - `options` (map, optional; `nil` values - the defaults):
    - `:lags`, `:level` - the lags and the band's confidence level, see [[cljplot2.data.series/acf-rows]];
    - `:correlograms` - `:both` (default; ACF above PACF, facet rows by `:kind`, each row its own y scale unless `:scales {:y {:domain ..}}` sets one), `:acf` (ACF) or `:pacf` (PACF);
    - `:band` - `:smooth` (default, a ribbon through the lags, as the R `ci.type ma` line) or `:stepped` (a block per lag);
    - `:colors` - `{:stem :band :significant}` colours (defaults blue, blue, red);
    - `:scales`, `:title`, `:theme` - the builder entries.

  Returns a plot map. Throws `ex-info {:key k}` for an invalid option `k` (checked first, see [[acf-options]]).

  Equivalents: R `acf` / `pacf` plots, `forecast::ggtsdisplay` (lower panels)."
  {:cljplot2/options acf-options}
  ([values] (acf values {}))
  ([values {:keys [scales title] user-theme :theme :as options}]
   (schema/check-options! acf-options options)
   (let [{:keys [correlograms band colors] :as settings} (merge {:correlograms :both :band :smooth :colors {}} (into {} (remove (comp nil? val)) options))
         {:keys [stem significant] band-color :band} (merge default-colors colors)
         kinds (kinds-of correlograms)
         rows (vec (mapcat (fn [p?] (map #(assoc % :kind (kind-names p?))
                                         (series/acf-rows values (assoc (select-keys settings [:lags :level]) :partial p?))))
                           kinds))
         both? (m/== 2 (count kinds))]
     (cond-> {:data rows
              :mapping (cond-> {:x :lag :y :value} both? (assoc :facet-row :kind))
              :charts [(band-chart rows band band-color)
                       (chart/rule {:y 0.0})
                       (chart/segment {:yend 0.0 :color stem})
                       (chart/scatter {:theme-style :acf-point :color stem})
                       (chart/scatter {:theme-style :acf-point :data (filterv :significant? rows) :color significant})]
              :scales (theme/deep-merge {} scales)
              :labels {:title title :x "Lag" :y (when-not both? (kind-names (first kinds)))}}
       ;; ACF and PACF differ in range (lag 0 is 1 in the ACF): each panel row its own y domain, unless the caller
       ;; fixes one
       (and both? (nil? (get-in scales [:y :domain]))) (assoc :facets {:scales {:y :free}})
       user-theme (assoc :theme user-theme)))))

(def ^:private horizon-ends
  "Lightest and darkest colour of the default horizon band palettes: blues for positive, reds for negative values."
  {:positive [(c/color 158 202 225) (c/color 8 69 148)]
   :negative [(c/color 252 174 145) (c/color 165 15 21)]})

(defn- band-colors
  "Band colours evenly from the lightest to the darkest colour.

  Parameters:

  - `[light dark]` (two colours): the ends.
  - `n` (long): the number of bands, positive.

  Returns `n` colours, lightest first; one band gets the darkest."
  [[light dark] ^long n]
  (if (m/one? n)
    [dark]
    (mapv (fn [^double t] (c/lerp light dark t)) (m/slice-range n))))

(defn- horizon-colors
  "Colours of the horizon bands `1..n` then `-1..-n` (see [[cljplot2.data.series/horizon-bands]]).

  Parameters:

  - `colors` (map): `:positive` / `:negative` colours (at least `n` each, lightest first); a missing sign takes the default palette of [[horizon-ends]] (see [[band-colors]]).
  - `n` (long): the number of bands per sign.

  Returns `2n` colours. Throws `ex-info {:key :colors}` when a sign has fewer than `n` colours."
  [colors ^long n]
  (when-let [sign (some (fn [[k cs]] (when (m/< (count cs) n) k)) colors)]
    (schema/fail! (str "horizon :colors " sign " should hold at least " n " colors (:bands).")
                  {:key :colors :value colors}))
  (into (vec (take n (or (:positive colors) (band-colors (:positive horizon-ends) n))))
        (take n (or (:negative colors) (band-colors (:negative horizon-ends) n)))))

(def horizon-options
  "Options schema of [[horizon]]: its own entries, those forwarded to [[cljplot2.data.series/horizon-rows]] and the builder entries."
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
  "Builds a horizon chart of rows as a plot map (see [[cljplot2.data.series/horizon-rows]]).

  Values `:y` over `:x` are folded into bands of one height, negatives mirrored upward in a second hue; every band is an area (`chart/line {:fill :color}`) from 0 across the whole x range (no x padding, `:x {:nice false}`), the darker bands over the lighter ones; the y scale is `[0 band-height]` without an axis. With `:series` there is a panel per series in one column, in first-appearance order (the series in the strip, one band height for all, a `nil` series - the NA panel, last).

  Parameters:

  - `rows` (rows): the series points.
  - `options` (map, optional):
    - `:x` (default `:x`), `:y`, `:series` - row keys;
    - `:bands` (default 2), `:band-height`, `:interpolator`, `:steps-per-gap` - see [[cljplot2.data.series/horizon-rows]] (a curve through the values, e.g. `:monotone`);
    - `:colors` - `{:positive [..] :negative [..]}`, at least `:bands` colours each, lightest first (default blues / reds, see `horizon-colors`);
    - `:alpha` - the bands' fill alpha (default 1, opaque; translucent bands show the ones beneath);
    - `:scales`, `:title`, `:theme` - the builder entries.

  Returns a plot map. Throws `ex-info {:key k}` for an invalid option `k` (checked first, see [[horizon-options]]), fewer than `:bands` colours (`:colors`), or invalid x or y values.

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
              :charts [(chart/line {:fill :color :alpha alpha :line-width 0 :padding {:x [0.0 0.0] :y [0.0 0.0]}})]
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
