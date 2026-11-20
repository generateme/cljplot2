;; # Limits

;; What cljplot2 doesn't do, and the nearest way around it.

(ns reference.limits
  (:require [cljplot2.chart :as chart]
            [cljplot2.core :as plot]))

;; ## Output

;; * Static raster images only (`BufferedImage`, a clojure2d canvas): no interactivity, no SVG or PDF.
;; * No save function - `clojure2d.core/save` writes an image (Render chapter).

;; ## Layout

;; * Legends sit right of the panels; there is no placement option. A legend can go elsewhere as its own plot:
;;   `:legend-only true` in an `:arrange` cell (and `:legend false` on the plot beside it).
;; * No subtitle or caption: write them with the `text` chart in panel units, or as a title of an `:arrange`.
;; * `:margins` need a single panel (no facets, `:arrange` or `:custom-grid`); `:aspect :iso` and `:flip` need the
;;   generated grid; free or sliced facet scales don't combine with explicit domains or `:aspect :iso`.

;; A caption written by a `text` chart, a legend moved below by `:legend-only`:

(def rows (for [[i g] (map-indexed vector ["a" "b" "c"]) x (range 12)]
            {:g g :x x :y (+ (* x (inc i)) (mod (* x 7) 5))}))

(plot/render-image (plot/arrange [{:data rows
                                   :mapping {:x :x :y :y :color :g}
                                   :charts [(chart/line)
                                            (chart/text {:x [:panel 1.0] :y [:panel 0.02] :anchor [:right :bottom]
                                                         :text "caption: synthetic data" :size 9 :extent false})]
                                   :legend false}
                                  {:data rows
                                   :mapping {:x :x :y :y :color :g}
                                   :charts [(chart/line)]
                                   :legend-only true}]
                                 {:rows 2 :heights [6 1]})
                   {:width 600 :height 450})

;; ## Coordinates

;; * Polar coordinates only with the charts declaring them (Scales, axes and coordinates chapter); no polar
;;   `:aspect :iso`, `:margins` or `:custom-grid`.
;; * 3D charts (`cloud`, `surface`) only with other 3D charts, without `:flip`, `:aspect :iso`, `:margins`, polar
;;   coordinates or free facet scales.
;; * No map projections or geographic shapes.

;; ## Data

;; * Data is rows (maps) or columns (equal-length sequences); other tables are converted by the caller.
;; * Dates are `java.time` values; date strings stay levels.
