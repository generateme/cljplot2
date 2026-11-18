(ns cljplot2.series-plots
  "Plot builders of time series: rows from [[cljplot2.data.series]] drawn with ordinary charts, returned as a plot map
  (edit it, add charts, or build the plot from the rows yourself).

  A builder's `:scales` option is merged into the builder's own scales key by key (see
  [[cljplot2.theme/deep-merge]])."
  (:require [cljplot2.chart :as chart]
            [cljplot2.data.series :as series]
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

(defn- check-options!
  "Throw `ex-info {:key k}` for an invalid `:partial?`, `:band` or `:colors`."
  [{:keys [partial? band colors]}]
  (when-not (contains? kinds-of partial?)
    (schema/fail! "acf :partial? should be :both, :acf (or false) or :pacf (or true)." {:key :partial? :value partial?}))
  (when-not (#{:smooth :stepped} band)
    (schema/fail! "acf :band should be :smooth or :stepped." {:key :band :value band}))
  (when-not (and (map? colors) (every? (set (keys default-colors)) (keys colors)) (every? schema/color? (vals colors)))
    (schema/fail! "acf :colors should be a map of :stem, :band, :significant to colors." {:key :colors :value colors})))

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
  :significant}`; `:scales`, `:title`, `:theme`. Throws `ex-info {:key k}` for an invalid `:partial?`, `:band`,
  `:colors` (`nil` options - the defaults).

  Equivalents: R `acf` / `pacf` plots, `forecast::ggtsdisplay` (lower panels)."
  ([values] (acf values {}))
  ([values {:keys [scales title] user-theme :theme :as options}]
   (let [{:keys [partial? band colors] :as options} (merge {:partial? :both :band :smooth :colors {}} (into {} (remove (comp nil? val)) options))
         _ (check-options! options)
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
