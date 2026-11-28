;; # 2D bins and fields

;; These charts colour a plane: a cell per row (`tile`), counts in 2D bins (`bin2d`), and a field as a raster
;; (`field`) or as level lines (`contour`). Use them for points too dense to draw one by one, or for a value over two
;; variables.

(ns reference.charts-bins-fields
  (:require [fastmath.core :as m]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [cljplot2.graphics.raster :as raster]
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

;; The table below holds made-up visits per hour of a working week; the heatmaps use it:

(def week-hours (for [[i day] (map-indexed vector ["Mon" "Tue" "Wed" "Thu" "Fri"]) hour (range 8 18)]
                  {:day day :hour hour
                   :visits (+ 10 (* 3 (mod (* (inc i) hour) 7)) (if (< 11 hour 14) 15 0))}))

(nu/data-table week-hours)

;; ## Heatmaps

;; `chart/tile` draws a cell per row at its x and y, filled by the mapped colour: numbers through a gradient,
;; categories through a palette. Use it for a value over two variables, e.g. a matrix or a week of hours.

(nu/shared-groups (describe/options (chart/tile)))

(nu/mapping-list (describe/mapping (chart/tile)))

(plot/render-image {:data week-hours
                    :mapping {:x :hour :y :day :color :visits}
                    :charts [(chart/tile)]
                    :scales {:y {:reverse true}}}
                   {:width 600 :height 300})

;; This option writes the value in each cell, in the `:tile-value` text role; the cell shape and outline are the [Cell
;; style](reference.shared_options.html#cell-style) group. The panels below write the values, then a word above 30:

(nu/option-cards 'chart/tile (describe/own-rows (describe/options (chart/tile))))

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

;; A mapped `:size` shrinks each cell by its value. With the colour mapped to another variable, the size legend's keys
;; have no colour of their own: a theme key fills them. The plot below fills them orange:

(nu/theme-key-list :tile (describe/style-rows :tile [:size-key-fill]))

(plot/render-image {:data week-hours
                    :mapping {:x :hour :y :day :color :day :size :visits}
                    :charts [(chart/tile)]
                    :scales {:y {:reverse true}}
                    :theme {:chart {:tile {:size-key-fill [200 120 60]}}}}
                   {:width 600 :height 300})

;; ## 2D bins

;; `chart/bin2d` counts points in cells, hexagons by default, and colours each cell by its count or another measure.
;; Use it when points are too many to draw: the colour shows where they crowd.

(nu/shared-groups (describe/options (chart/bin2d)))

(nu/mapping-list (describe/mapping (chart/bin2d)))

(plot/render-image {:data faithful
                    :mapping {:x :eruptions :y :waiting}
                    :charts [(chart/bin2d)]
                    :labels {:x "eruption [min]" :y "waiting [min]"}}
                   {:width 600 :height 400})

;; These options set the cell size (by a rule, a count or a width), the shape of the binning space and what the colour
;; measures. The panels below use 10 cells across, a cell width in a flatter space, and percentages:

(nu/option-cards 'chart/bin2d (describe/own-rows (describe/options (chart/bin2d))))

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

;; The next panels compare the other rules (the default `:scott-correlated` is above); each picks its own cell size:

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

;; The last panels colour by the share of the panel's points (`:pmf`) and by the density; their legends show the
;; scales:

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

;; ## Raster fields

;; `chart/field` draws a scalar field as a raster: the density of mapped points, gridded values interpolated, or a
;; function of a point (the [Fields](reference.shared_options.html#fields) group). Use it to show a smooth value over
;; a plane, coloured through the colour scale.

(nu/shared-groups (describe/options (chart/field)))

(nu/mapping-list (describe/mapping (chart/field)))

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

;; These options set the raster's opacity, colour each pixel by a function instead of a field, or antialias the
;; raster ([Antialiasing](reference.charts_bins_fields.html#antialiasing) explains its settings). The panels below
;; draw a translucent density under the points, then a colour per point of a disc:

(nu/option-cards 'chart/field (describe/own-rows (describe/options (chart/field))))

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

;; ### Antialiasing

;; With `:antialias` each pixel takes several samples, so band edges and the edges of uncoloured regions come out smooth
;; instead of stepped. `true` takes the defaults below; a map of these settings overrides some of them:

(nu/option-cards 'chart/field (mapv #(update % :path (partial into [:antialias])) (describe/schema-rows raster/antialias-settings-schema)))

;; The panels below draw bands of a disc with one sample per pixel, with the default 2 x 2 samples, then with 3 x 3
;; samples and a Gaussian filter of radius 1.5:

(let [banded-disc (fn [[x y]]
                    (let [r (m/hypot x y)]
                      (when (< r 1)
                        ([:steelblue :orange :seagreen :crimson] (mod (long (m/floor (* 6 (+ r (* 0.3 x))))) 4)))))]
  (plot/render-image (plot/arrange [{:charts [(chart/field {:domain {:x [-1.1 1.1] :y [-1.1 1.1]} :pixel-color banded-disc})]
                                     :aspect :iso
                                     :labels {:title "one sample per pixel"}}
                                    {:charts [(chart/field {:domain {:x [-1.1 1.1] :y [-1.1 1.1]} :pixel-color banded-disc
                                                            :antialias true})]
                                     :aspect :iso
                                     :labels {:title ":antialias true"}}
                                    {:charts [(chart/field {:domain {:x [-1.1 1.1] :y [-1.1 1.1]} :pixel-color banded-disc
                                                            :antialias {:splats 3 :filter :gaussian :radius 1.5}})]
                                     :aspect :iso
                                     :labels {:title "3 x 3, :gaussian"}}]
                                   {:columns 3})
                     {:width 900 :height 330}))

;; ### Field sources

;; The [Fields](reference.shared_options.html#fields) group gives the field. The panels below sample a function of a
;; point `[x y]` over a `:domain`, then on a coarse grid of 12 points per axis, which shows its cells:

(plot/render-image (plot/arrange [{:charts [(chart/field {:f (fn [[x y]] (m/sin (+ (* x x) (* y y))))
                                                          :domain {:x [-2 2] :y [-2 2]}})]
                                   :labels {:title ":f over :domain"}
                                   :legend false}
                                  {:charts [(chart/field {:f (fn [[x y]] (m/sin (+ (* x x) (* y y))))
                                                          :domain {:x [-2 2] :y [-2 2]} :n 12})]
                                   :labels {:title ":n 12"}}]
                                 {:columns 2 :widths [1 1.2]})
                   {:width 900 :height 380})

;; The chart interpolates gridded values between the lattice points, here of a 5 x 5 lattice:

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

;; The next panels change the density estimate of mapped points: its kernel and bandwidth, a binned estimate (faster on many
;; points), and how far the domain reaches past the points (`:cut`, in bandwidths):

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

;; ## Contour lines

;; `chart/contour` draws the level lines, or filled bands, of a scalar field from the same sources as `chart/field`;
;; categorical groups give a field each. Use it to read heights off a field, or to compare the densities of groups.

(nu/shared-groups (describe/options (chart/contour)))

(nu/mapping-list (describe/mapping (chart/contour)))

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

;; These options set the levels, filled bands instead of lines, labels on the lines, one line colour and the opacity.
;; The panels below label lines at given heights, then fill 8 bands:

(nu/option-cards 'chart/contour (describe/own-rows (describe/options (chart/contour))))

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

;; The panels below take the sources of `chart/field`: a function over a domain (its lines angular on a coarse grid),
;; interpolated gridded values and a density with its own bandwidth:

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
