;; # 2D bins and fields

;; Charts that colour a plane, for dense points or a surface: a cell per row (`tile`), counts in 2D bins (`bin2d`),
;; a field as a raster (`field`) or as level lines (`contour`).

(ns reference.charts-bins-fields
  (:require [fastmath.core :as m]
            [cljplot2.chart :as chart]
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
;; shape and outline are the [Cell style](reference.shared_options.html#cell-style) group:

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

;; A mapped `:size` shrinks each cell by its value; with the colour mapped to another variable the size legend's keys
;; have no colour of their own - their fill is a theme key of `[:chart :tile]`, not an option:

(nu/options-table (describe/style-rows :tile [:size-key-fill]))

(plot/render-image {:data week-hours
                    :mapping {:x :hour :y :day :color :day :size :visits}
                    :charts [(chart/tile)]
                    :scales {:y {:reverse true}}
                    :theme {:chart {:tile {:size-key-fill [200 120 60]}}}}
                   {:width 600 :height 300})

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
                                   :charts [(chart/bin2d {:binwidth 0.25 :aspect 0.5})]
                                   :labels {:title ":binwidth 0.25 :aspect 0.5"}
                                   :legend false}
                                  {:data faithful
                                   :mapping {:x :eruptions :y :waiting}
                                   :charts [(chart/bin2d {:measure :percent})]
                                   :labels {:title ":measure :percent"}}]
                                 {:columns 3 :widths [1 1 1.3]})
                   {:width 1000 :height 320})

;; The fastmath estimates of the grid (the default `:scott-correlated` above) - each picks its own cell count:

(plot/render-image (plot/arrange [{:data faithful
                                   :mapping {:x :eruptions :y :waiting}
                                   :charts [(chart/bin2d {:bins :scott})]
                                   :labels {:title ":bins :scott"}
                                   :legend false}
                                  {:data faithful
                                   :mapping {:x :eruptions :y :waiting}
                                   :charts [(chart/bin2d {:bins :freedman-diaconis})]
                                   :labels {:title ":bins :freedman-diaconis"}
                                   :legend false}
                                  {:data faithful
                                   :mapping {:x :eruptions :y :waiting}
                                   :charts [(chart/bin2d {:bins :cross-validation})]
                                   :labels {:title ":bins :cross-validation"}
                                   :legend false}
                                  {:data faithful
                                   :mapping {:x :eruptions :y :waiting}
                                   :charts [(chart/bin2d {:bins :shimazaki-shinomoto})]
                                   :labels {:title ":bins :shimazaki-shinomoto"}
                                   :legend false}]
                                 {:columns 2})
                   {:width 900 :height 600})

;; The other measures: the share of the panel's points (`:pmf`) and the density - the count over n times the cell
;; area (the legend shows the scale):

(plot/render-image (plot/arrange [{:data faithful
                                   :mapping {:x :eruptions :y :waiting}
                                   :charts [(chart/bin2d {:measure :pmf})]
                                   :labels {:title ":measure :pmf"}}
                                  {:data faithful
                                   :mapping {:x :eruptions :y :waiting}
                                   :charts [(chart/bin2d {:measure :density})]
                                   :labels {:title ":measure :density"}}]
                                 {:columns 2})
                   {:width 900 :height 340})

(nu/shared-groups (describe/options (chart/bin2d)))

;; ## field

;; A raster of a scalar field: the density of mapped points, gridded `:value`s interpolated, or a function of a
;; point (the [Fields](reference.shared_options.html#fields) group).

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
                                   :charts [(chart/field {:alpha-factor 0.5}) (chart/scatter {:size 3 :color :black})]
                                   :labels {:title ":alpha-factor 0.5, points over it"}
                                   :legend false}
                                  {:charts [(chart/field {:domain {:x [-2 2] :y [-2 2]}
                                                          :pixel-color (fn [[x y]]
                                                                         (when (< (+ (* x x) (* y y)) 3)
                                                                           [(int (* 60 (+ x 2))) 80 (int (* 60 (+ y 2)))]))})]
                                   :labels {:title ":pixel-color - a colour per point"}}]
                                 {:columns 2})
                   {:width 900 :height 380})

;; ### Field sources

;; The [Fields](reference.shared_options.html#fields) group gives the field. A function of a point `[x y]` sampled
;; over a `:domain` on a grid of `:n` points per axis (a coarse grid shows its cells):

(plot/render-image (plot/arrange [{:charts [(chart/field {:f (fn [[x y]] (m/sin (+ (* x x) (* y y))))
                                                          :domain {:x [-2 2] :y [-2 2]}})]
                                   :labels {:title ":f over :domain"}
                                   :legend false}
                                  {:charts [(chart/field {:f (fn [[x y]] (m/sin (+ (* x x) (* y y))))
                                                          :domain {:x [-2 2] :y [-2 2]} :n 12})]
                                   :labels {:title ":n 12"}}]
                                 {:columns 2 :widths [1 1.2]})
                   {:width 900 :height 380})

;; Gridded values are interpolated between the lattice points - here a 5 x 5 lattice:

(def lattice (for [x (range 5) y (range 5)] {:x x :y y :z (* (m/sin x) (m/cos (* 0.8 y)))}))

(nu/data-table lattice)

(plot/render-image (plot/arrange [{:data lattice
                                   :mapping {:x :x :y :y :value :z}
                                   :charts [(chart/field {:interpolation :bilinear})]
                                   :labels {:title ":interpolation :bilinear"}
                                   :legend false}
                                  {:data lattice
                                   :mapping {:x :x :y :y :value :z}
                                   :charts [(chart/field {:interpolation :bicubic})]
                                   :labels {:title ":interpolation :bicubic"}
                                   :legend false}
                                  {:data lattice
                                   :mapping {:x :x :y :y :value :z}
                                   :charts [(chart/field {:interpolation :cubic-2d})]
                                   :labels {:title ":interpolation :cubic-2d"}}]
                                 {:columns 3 :widths [1 1 1.25]})
                   {:width 1000 :height 320})

;; The density of mapped points: its kernel and bandwidth, a binned estimate (faster on many points), and how far
;; the domain reaches past the points (`:cut`, in bandwidths):

(plot/render-image (plot/arrange [{:data faithful
                                   :mapping {:x :eruptions :y :waiting}
                                   :charts [(chart/field {:kernel :epanechnikov :bandwidth [0.2 4]})]
                                   :labels {:title "epanechnikov, bandwidth [0.2 4]"}
                                   :legend false}
                                  {:data faithful
                                   :mapping {:x :eruptions :y :waiting}
                                   :charts [(chart/field {:binned true})]
                                   :labels {:title ":binned true"}
                                   :legend false}
                                  {:data faithful
                                   :mapping {:x :eruptions :y :waiting}
                                   :charts [(chart/field {:cut 0})]
                                   :labels {:title ":cut 0"}
                                   :legend false}]
                                 {:columns 3})
                   {:width 1000 :height 320})

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
                                   :charts [(chart/contour {:filled true :levels 8 :alpha-factor 0.8})]
                                   :labels {:title ":filled true :levels 8"}}]
                                 {:columns 2 :widths [1 1.25]})
                   {:width 1000 :height 380})

;; ### Contour sources

;; The same sources as `field`: a function over a domain (lines on a coarse grid are angular), interpolated gridded
;; values and a density with its own bandwidth:

(plot/render-image (plot/arrange [{:charts [(chart/contour {:f (fn [[x y]] (+ (* x x) (* 2 y y) (* x y)))
                                                            :domain {:x [-2 2] :y [-2 2]} :labels true})]
                                   :labels {:title ":f over :domain"}
                                   :legend false}
                                  {:charts [(chart/contour {:f (fn [[x y]] (+ (* x x) (* 2 y y) (* x y)))
                                                            :domain {:x [-2 2] :y [-2 2]} :n 6})]
                                   :labels {:title ":n 6"}
                                   :legend false}
                                  {:data lattice
                                   :mapping {:x :x :y :y :value :z}
                                   :charts [(chart/contour {:interpolation :bicubic :levels 8})]
                                   :labels {:title "5 x 5 lattice, :bicubic"}
                                   :legend false}
                                  {:data faithful
                                   :mapping {:x :eruptions :y :waiting}
                                   :charts [(chart/scatter {:size 3 :color [0 0 0 80]})
                                            (chart/contour {:bandwidth [0.15 3] :cut 1})]
                                   :labels {:title ":bandwidth [0.15 3] :cut 1"}}]
                                 {:columns 2 :widths [1 1.25]})
                   {:width 900 :height 640})

(nu/shared-groups (describe/options (chart/contour)))
