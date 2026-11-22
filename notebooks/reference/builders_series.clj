;; # Builders: series

;; Builders of time series plots: the autocorrelation correlogram of a series of values, and horizon charts of one or
;; more series. Each takes its data and an options map and returns an ordinary plot map; the series functions it
;; forwards to are shared groups (Shared options).

(ns reference.builders-series
  (:require [fastmath.random :as r]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [notebook-utils :as nu]))

;; An autoregressive series AR(1) with coefficient 0.7, 200 values from a seeded generator:

(def ar1 (let [g (r/rng :mersenne 11)]
           (vec (reductions (fn [x _] (+ (* 0.7 x) (r/grandom g))) 0.0 (range 199)))))

(nu/data-table {:t (range (count ar1)) :value ar1})

;; ## acf

;; The autocorrelation (ACF) and partial autocorrelation (PACF) of a series of numbers by lag: a stem per lag, a
;; significance band around zero, the lags outside it marked.

(nu/options-table (describe/own-rows (describe/options #'plot/acf)))

(plot/render-image (plot/acf ar1) {:width 700 :height 500})

;; One correlogram - the ACF (`:acf` or `false`) or the PACF (`:pacf` or `true`) - and the band as a block per lag
;; (`:stepped`); the default `:both` draws ACF above PACF, the band `:smooth`:

(plot/render-image (plot/arrange [(plot/acf ar1 {:partial? :acf :band :stepped :title ":partial? :acf, :band :stepped"})
                                  (plot/acf ar1 {:partial? :pacf :band :smooth :title ":partial? :pacf"})
                                  (plot/acf ar1 {:partial? false :title ":partial? false (ACF)"})
                                  (plot/acf ar1 {:partial? true :title ":partial? true (PACF)"})]
                                 {:columns 2})
                   {:width 1000 :height 600})

;; The colours of the stems, the band and the significant lags:

(plot/render-image (plot/acf ar1 {:partial? :both :lags 30
                                  :colors {:stem [90 90 90] :band [230 150 40] :significant [40 150 60]}
                                  :title ":colors"})
                   {:width 700 :height 500})

;; The lag points draw in the theme style `[:chart :acf-point]` (a scatter style over `[:chart :scatter]`; their
;; colours are the `:colors` option):

(nu/options-table (describe/style-rows :acf-point))

;; Larger triangle points through the theme:

(plot/render-image (plot/acf ar1 {:partial? :acf :theme {:chart {:acf-point {:size 8 :shape \A}}}})
                   {:width 700 :height 360})

(nu/shared-groups (describe/options #'plot/acf))

;; ## horizon

;; Horizon charts: each series folded into bands of equal height - values above the first band drawn over it in a
;; darker colour, negative values mirrored up in reds - so a series takes a fraction of its height. The data are rows
;; with x, y and (several series) a series column.

(nu/options-table (describe/own-rows (describe/options #'plot/horizon)))

(def waves (for [[i series] (map-indexed vector ["north" "south" "east"]) t (range 0 100 2)]
             {:t t :series series
              :v (* (+ 1.0 i) (Math/sin (+ (* (+ 0.05 (* 0.03 i)) t) (* 1.3 i))) (+ 1 (* 0.01 t)))}))

(nu/data-table waves)

(plot/render-image (plot/horizon waves {:x :t :y :v :series :series :bands 3}) {:width 700 :height 300})

;; Two bands (the default; left - blues and reds), own band colours per sign, lightest first (middle), and
;; translucent bands showing the ones beneath (right):

(plot/render-image (plot/arrange [(plot/horizon waves {:x :t :y :v :series :series :title "defaults"})
                                  (plot/horizon waves {:x :t :y :v :series :series
                                                       :colors {:positive [[199 233 192] [35 139 69]]
                                                                :negative [[218 218 235] [84 39 143]]}
                                                       :title ":colors"})
                                  (plot/horizon waves {:x :t :y :v :series :series :alpha 0.5 :title ":alpha 0.5"})]
                                 {:columns 3})
                   {:width 1000 :height 320})

(nu/shared-groups (describe/options #'plot/horizon))
