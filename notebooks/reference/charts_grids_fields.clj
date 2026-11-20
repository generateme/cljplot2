;; # Charts: grids and fields

;; Charts colouring a plane: a cell per row (`tile`), a 2D histogram (`bin2d`), a scalar field drawn as a raster
;; (`field`) or as level lines (`contour`), and the correlation of two variables as one glyph (`correlation`).

(ns reference.charts-grids-fields
  (:require [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [notebook-utils :as nu]))

(nu/source-note {:dataset :faithful})

(def faithful (nu/dataset :faithful))

(nu/data-table faithful)

(nu/source-note {:dataset :volcano})

(def volcano (nu/dataset :volcano))

(nu/data-table volcano)

(nu/source-note {:dataset :iris})

(def iris (nu/dataset :iris))

(nu/data-table iris)

;; ## tile

;; A cell per row at its x and y, filled by a mapped colour: numeric (a gradient) or categorical (a palette).

(nu/options-table (describe/mapping (chart/tile)))

(def week-hours (for [[i day] (map-indexed vector ["Mon" "Tue" "Wed" "Thu" "Fri"]) hour (range 8 18)]
                  {:day day :hour hour
                   :visits (+ 10 (* 3 (mod (* (inc i) hour) 7)) (if (< 11 hour 14) 15 0))}))

(nu/data-table week-hours)

(plot/render-image {:data week-hours
                    :mapping {:x :hour :y :day :color :visits}
                    :charts [(chart/tile)]
                    :scales {:y {:reverse true}}}
                   {:width 600 :height 300})

;; Its own option writes the value in each cell (black or white by the fill, the `:tile-value` text role); the cell
;; shape and outline are the Cell style group:

(nu/options-table (describe/own-rows (describe/options (chart/tile))))

(plot/render-image (plot/arrange [{:data week-hours
                                   :mapping {:x :hour :y :day :color :visits}
                                   :charts [(chart/tile {:value-label true})]
                                   :scales {:y {:reverse true}}
                                   :labels {:title ":value-label true"}
                                   :legend false}
                                  {:data week-hours
                                   :mapping {:x :hour :y :day :color :visits}
                                   :charts [(chart/tile {:value-label #(when (< 30 (:value %)) "busy")})]
                                   :scales {:y {:reverse true}}
                                   :labels {:title "a function: \"busy\" above 30"}}]
                                 {:columns 2 :widths [1 1.15]})
                   {:width 1000 :height 300})

(nu/shared-groups (describe/options (chart/tile)))

;; ## bin2d

;; A 2D histogram: points counted in rectangular cells, the count (or another measure) as the cell colour.

(nu/options-table (describe/mapping (chart/bin2d)))

(plot/render-image {:data faithful
                    :mapping {:x :eruptions :y :waiting}
                    :charts [(chart/bin2d)]
                    :labels {:x "eruption [min]" :y "waiting [min]"}}
                   {:width 600 :height 400})

;; Its options: the number of cells (or a fastmath estimate), a cell width in data units, the cells' height / width
;; ratio and what the colour measures:

(nu/options-table (describe/own-rows (describe/options (chart/bin2d))))

(plot/render-image (plot/arrange [{:data faithful
                                   :mapping {:x :eruptions :y :waiting}
                                   :charts [(chart/bin2d {:bins 10})]
                                   :labels {:title ":bins 10"}
                                   :legend false}
                                  {:data faithful
                                   :mapping {:x :eruptions :y :waiting}
                                   :charts [(chart/bin2d {:binwidth 0.25 :shape 0.5})]
                                   :labels {:title ":binwidth 0.25 :shape 0.5"}
                                   :legend false}
                                  {:data faithful
                                   :mapping {:x :eruptions :y :waiting}
                                   :charts [(chart/bin2d {:measure :percent})]
                                   :labels {:title ":measure :percent"}}]
                                 {:columns 3 :widths [1 1 1.3]})
                   {:width 1000 :height 320})

(nu/shared-groups (describe/options (chart/bin2d)))

;; ## field

;; A raster of a scalar field: the density of mapped points, gridded `:value`s interpolated, or a function of a
;; point (the Fields group).

(nu/options-table (describe/mapping (chart/field)))

(plot/render-image (plot/arrange [{:data faithful
                                   :mapping {:x :eruptions :y :waiting}
                                   :charts [(chart/field)]
                                   :labels {:title "density of the points"}
                                   :legend false}
                                  {:data volcano
                                   :mapping {:x :column :y :row :value :height}
                                   :charts [(chart/field)]
                                   :labels {:title "gridded :value"}}]
                                 {:columns 2 :widths [1 1.2]})
                   {:width 1000 :height 380})

;; Its options: the raster's opacity, or a colour per pixel computed by a function (no field, no colour scale):

(nu/options-table (describe/own-rows (describe/options (chart/field))))

(plot/render-image (plot/arrange [{:data faithful
                                   :mapping {:x :eruptions :y :waiting}
                                   :charts [(chart/field {:alpha 0.5}) (chart/scatter {:size 3 :color :black})]
                                   :labels {:title ":alpha 0.5, points over it"}
                                   :legend false}
                                  {:charts [(chart/field {:domain {:x [-2 2] :y [-2 2]}
                                                          :pixel-color (fn [[x y]]
                                                                         (when (< (+ (* x x) (* y y)) 3)
                                                                           [(int (* 60 (+ x 2))) 80 (int (* 60 (+ y 2)))]))})]
                                   :labels {:title ":pixel-color - a colour per point"}}]
                                 {:columns 2})
                   {:width 900 :height 380})

(nu/shared-groups (describe/options (chart/field)))

;; ## contour

;; Level lines (or filled bands) of a scalar field - the same sources as `field`; categorical groups give a field
;; each.

(nu/options-table (describe/mapping (chart/contour)))

(plot/render-image (plot/arrange [{:data volcano
                                   :mapping {:x :column :y :row :value :height}
                                   :charts [(chart/contour)]
                                   :labels {:title "gridded :value"}
                                   :legend false}
                                  {:data iris
                                   :mapping {:x :sepal-length :y :sepal-width :color :species}
                                   :charts [(chart/contour {:levels 6})]
                                   :labels {:title "a density per :color group"}}]
                                 {:columns 2 :widths [1 1.25]})
                   {:width 1000 :height 380})

;; Its options: the levels (a count or the values), filled bands instead of lines, labels on the lines, one line
;; colour and the opacity:

(nu/options-table (describe/own-rows (describe/options (chart/contour))))

(plot/render-image (plot/arrange [{:data volcano
                                   :mapping {:x :column :y :row :value :height}
                                   :charts [(chart/contour {:levels [100 120 140 160 180] :labels true
                                                            :color [40 40 40]})]
                                   :labels {:title ":levels vector, :labels true"}}
                                  {:data volcano
                                   :mapping {:x :column :y :row :value :height}
                                   :charts [(chart/contour {:fill true :levels 8 :alpha 0.8})]
                                   :labels {:title ":fill true :levels 8"}}]
                                 {:columns 2 :widths [1 1.25]})
                   {:width 1000 :height 380})

(nu/shared-groups (describe/options (chart/contour)))

;; ## correlation

;; The correlation coefficient r of two variables drawn as one glyph filling the panel - a cell of a correlogram
;; (`plot/pairs`). Alone it writes r, one line per group.

(nu/options-table (describe/mapping (chart/correlation)))

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :petal-length :color :species}
                    :charts [(chart/correlation)]}
                   {:width 450 :height 300})

;; Its options: the coefficient, the glyph and its colours (by default r on a diverging scale):

(nu/options-table (describe/own-rows (describe/options (chart/correlation))))

(plot/render-image (plot/pairs iris {:variables [:sepal-length :sepal-width :petal-length :petal-width]
                                     :lower (chart/correlation {:glyph :ellipse :stroke-color :none})
                                     :upper (chart/correlation {:glyph :square :method :spearman})
                                     :diagonal :names
                                     :title "ellipses (Pearson) below, squares (Spearman) above"})
                   {:width 700 :height 620})

(nu/shared-groups (describe/options (chart/correlation)))
