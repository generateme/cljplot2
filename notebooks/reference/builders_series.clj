;; # Series

;; These builders draw time series: the autocorrelation correlogram of a series of values, and horizon charts of one
;; or more series. Use them to look at a series' structure over time. The Shared options chapter documents the series
;; options they pass on.

(ns reference.builders-series
  (:require [fastmath.core :as m]
            [fastmath.random :as r]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [notebook-utils :as nu]))

;; The values below form an AR(1) series with coefficient 0.7: fastmath `r/ar` draws 200 values over seeded white
;; noise:

(def ar1 (vec (take 200 (r/ar 0.7 (r/white-noise (r/rng :mersenne 11) 1.0)))))

(nu/data-table ar1)

;; The table below holds three made-up series of growing waves, `:t` and `:v` per series; the horizon charts use it:

(def waves (for [[i series] (map-indexed vector ["north" "south" "east"]) t (range 0 100 2)]
             {:t t :series series
              :v (m/* (m/inc i) (m/sin (m/+ (m/* (m/+ 0.05 (m/* 0.03 i)) t) (m/* 1.3 i))) (m/inc (m/* 0.01 t)))}))

(nu/data-table waves)

;; ## Autocorrelation plots

;; `plot/acf` draws the autocorrelation (ACF) and partial autocorrelation (PACF) of a series of numbers by lag. It
;; draws a stem per lag and a significance band around zero, and marks the lags outside the band (but not lag 0, which
;; is always 1). Use it to find the lags a series depends on.

(nu/argument-list #'plot/acf)

(nu/shared-groups (describe/options #'plot/acf))

;; These options choose the correlograms, the band's shape and the colours. The plot below draws both correlograms of
;; the AR(1) series:

(nu/option-cards '(plot/acf values) (describe/own-rows (describe/options #'plot/acf)))

(plot/render-image (plot/acf ar1) {:width 700 :height 500})

;; The next panels draw only the ACF with the band as a block per lag, then only the PACF:

(plot/render-image (plot/arrange [(plot/acf ar1 {:correlograms :acf :band :stepped :title ":correlograms :acf, :band :stepped"})
                                  (plot/acf ar1 {:correlograms :pacf :title ":correlograms :pacf"})]
                                 {:columns 2})
                   {:width 1000 :height 300})

;; The next plot sets the colours of the stems, the band and the significant lags:

(plot/render-image (plot/acf ar1 {:correlograms :both :lags 30
                                  :colors {:stem [90 90 90] :band [230 150 40] :significant [40 150 60]}
                                  :title ":colors"})
                   {:width 700 :height 500})

;; The lag points draw in the theme style `[:chart :acf-point]` (a scatter style; the `:colors` option sets their
;; colours):

(nu/theme-key-list :acf-point (describe/style-rows :acf-point))

;; The plot below draws larger triangle points through the theme:

(plot/render-image (plot/acf ar1 {:correlograms :acf :theme {:chart {:acf-point {:size 8 :shape \A}}}})
                   {:width 700 :height 360})

;; ## Horizon charts

;; `plot/horizon` folds each series of rows into bands of equal height: values above the first band draw over it in a
;; darker colour, and negative values mirror up in reds. Use it to compare many series in little height.

(nu/argument-list #'plot/horizon)

(nu/shared-groups (describe/options #'plot/horizon))

;; These options set the band colours of each sign and the bands' opacity. The plot below folds the waves into 3
;; bands:

(nu/option-cards '(plot/horizon rows) (describe/own-rows (describe/options #'plot/horizon)))

(plot/render-image (plot/horizon waves {:x :t :y :v :series :series :bands 3}) {:width 700 :height 300})

;; The left panel below draws two bands, the default, in blues and reds; the middle one sets its own band colours per
;; sign, lightest first; the right one makes the bands translucent, so they show the ones beneath:

(plot/render-image (plot/arrange [(plot/horizon waves {:x :t :y :v :series :series :title "defaults"})
                                  (plot/horizon waves {:x :t :y :v :series :series
                                                       :colors {:positive [[199 233 192] [35 139 69]]
                                                                :negative [[218 218 235] [84 39 143]]}
                                                       :title ":colors"})
                                  (plot/horizon waves {:x :t :y :v :series :series :alpha 0.5 :title ":alpha 0.5"})]
                                 {:columns 3})
                   {:width 1000 :height 320})

