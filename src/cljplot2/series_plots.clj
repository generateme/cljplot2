(ns cljplot2.series-plots
  "Plot builders of time series: rows from [[cljplot2.data.series]] drawn with ordinary charts, returned as a plot map
  (edit it, add charts, or build the plot from the rows yourself).

  A builder's `:scales` option is merged into the builder's own scales key by key (see
  [[cljplot2.theme/deep-merge]])."
  (:require [cljplot2.chart :as chart]
            [cljplot2.data.series :as series]
            [cljplot2.theme :as theme]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private default-colors
  {:stem [69 117 180] :band [69 117 180] :significant [215 48 39]})

(def ^:private kind-names {false "ACF" true "PACF"})

(defn- band-rows
  "Rows `{:x :lo :hi :lag}` of a stepped band: each lag's band from `lag - 0.5` to `lag + 0.5` (a ribbon group per
  lag)."
  [rows]
  (vec (mapcat (fn [{:keys [lag lo hi] :as row}]
                 (let [lag (double lag) base (select-keys row [:lo :hi :kind :lag])]
                   [(assoc base :x (- lag 0.5)) (assoc base :x (+ lag 0.5))]))
               rows)))

(defn acf
  "Correlogram of the series `values` as a plot map: ACF and PACF (see [[cljplot2.data.series/acf-rows]]) - stems
  from 0 (`chart/segment`) with points (`chart/scatter`), lags beyond the significance band highlighted, the band
  as a stepped ribbon (Bartlett's per lag for ACF, constant for PACF), a rule at 0.

  `options`: `:lags`, `:alpha` (see `acf-rows`); `:partial?` - `:both` (default; ACF above PACF, facet rows by
  `:kind`), `false` (ACF) or `true` (PACF); `:colors {:stem :band :significant}`; `:scales`, `:title`, `:theme`.

  Equivalents: R `acf` / `pacf` plots, `forecast::ggtsdisplay` (lower panels)."
  [values {:keys [partial? colors scales title] user-theme :theme :or {partial? :both} :as options}]
  (let [{:keys [stem band significant]} (merge default-colors colors)
        kinds (if (= :both partial?) [false true] [(boolean partial?)])
        rows (vec (mapcat (fn [p?] (map #(assoc % :kind (kind-names p?))
                                        (series/acf-rows values (assoc (select-keys options [:lags :alpha]) :partial? p?))))
                          kinds))
        both? (= 2 (count kinds))]
    (cond-> {:data rows
             :mapping (cond-> {:x :lag :y :value} both? (assoc :facet-row :kind))
             :charts [(chart/ribbon {:data (band-rows rows) :mapping {:x :x :ymin :lo :ymax :hi :group :lag} :color band})
                      (chart/rule {:y 0.0})
                      (chart/segment {:yend 0.0 :color stem})
                      (chart/scatter {:color stem :size 5})
                      (chart/scatter {:data (filterv :significant? rows) :color significant :size 5})]
             :scales (theme/deep-merge {} scales)
             :labels {:title title :x "Lag" :y (when-not both? (kind-names (first kinds)))}}
      user-theme (assoc :theme user-theme))))
