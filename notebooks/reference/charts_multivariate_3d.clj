;; # Charts: multivariate and 3D

;; Many variables at once: a line per row across one axis per variable (`parallel`, a radar in polar
;; coordinates), points in a 3D box (`cloud`) and surfaces over a grid (`surface`). Each chart lists the mapping keys
;; it reads, its own options and the shared option groups it takes (documented once in the Shared options chapter).

(ns reference.charts-multivariate-3d
  (:require [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [notebook-utils :as nu]))

(nu/source-note {:dataset :iris})

(def iris (nu/dataset :iris))

(nu/data-table iris)

(nu/source-note {:dataset :quakes})

(def quakes (nu/dataset :quakes))

(nu/data-table quakes)

;; ## parallel

;; A line per row across one vertical axis per variable, each axis scaled on its own; the variables come from the
;; `:variables` option, the mapping only groups the rows.

(nu/options-table (describe/mapping (chart/parallel)))

(plot/render-image {:data iris
                    :mapping {:color :species}
                    :charts [(chart/parallel {:variables [:sepal-length :sepal-width :petal-length :petal-width]})]
                    :labels {:x nil :y nil}}
                   {:width 700 :height 400})

;; ### Axes

;; Which variables and in what order (a `[label accessor]` pair renames one), how each is scaled - every axis to
;; 0..1 (`:uniminmax`), to standard scores, robustly, centred, or all on one common range - explicit limits, the
;; axes themselves and what a missing value does:

(nu/options-table (describe/own-rows (describe/options (chart/parallel))) :axes)

(plot/render-image (plot/arrange [{:data iris
                                   :mapping {:color :species}
                                   :charts [(chart/parallel {:variables [["petal L" :petal-length] ["petal W" :petal-width]
                                                                         ["sepal L" :sepal-length]]
                                                             :scaling :std})]
                                   :labels {:title ":scaling :std, labelled variables" :x nil :y nil}
                                   :legend false}
                                  {:data iris
                                   :mapping {:color :species}
                                   :charts [(chart/parallel {:variables [:petal-length :petal-width :sepal-length]
                                                             :scaling :globalminmax :limits {:petal-width [0 8]}})]
                                   :labels {:title ":globalminmax, :limits {:petal-width [0 8]}" :x nil :y nil}}]
                                 {:columns 2 :widths [1 1.25]})
                   {:width 1000 :height 380})

;; ### Style

;; Smooth curves between the axes, and - in polar coordinates, where the axes become spokes of a radar - a fill of
;; each row's polygon:

(nu/options-table (describe/own-rows (describe/options (chart/parallel))) :style)

(def athletes [{:name "Ann" :speed 8 :strength 5 :stamina 9 :skill 6 :reach 4}
               {:name "Bo" :speed 5 :strength 9 :stamina 6 :skill 4 :reach 8}
               {:name "Cy" :speed 6 :strength 6 :stamina 5 :skill 9 :reach 6}])

(nu/data-table athletes)

(plot/render-image (plot/arrange [{:data iris
                                   :mapping {:color :species}
                                   :charts [(chart/parallel {:variables [:sepal-length :sepal-width :petal-length :petal-width]
                                                             :curve :smooth :smoothness 0.6})]
                                   :labels {:title ":curve :smooth" :x nil :y nil}
                                   :legend false}
                                  {:data athletes
                                   :mapping {:color :name}
                                   :charts [(chart/parallel {:variables [:speed :strength :stamina :skill :reach]
                                                             :scaling :globalminmax :limits {:speed [0 10] :strength [0 10]
                                                                                             :stamina [0 10] :skill [0 10]
                                                                                             :reach [0 10]}
                                                             :fill :color :alpha 0.2})]
                                   :coord {:kind :polar}
                                   :labels {:title "polar, :fill :color" :x nil :y nil}}]
                                 {:columns 2 :widths [1.2 1]})
                   {:width 1000 :height 420})

(nu/shared-groups (describe/options (chart/parallel)))

;; ## cloud

;; Points in a 3D box at their x, y and z, seen in perspective; colour, size and shape may come from the data.

(nu/options-table (describe/mapping (chart/cloud)))

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :sepal-width :z :petal-length :color :species}
                    :charts [(chart/cloud)]}
                   {:width 600 :height 480})

;; ### Style

;; The point style used when nothing is mapped:

(nu/options-table (describe/own-rows (describe/options (chart/cloud))) :style)

(plot/render-image {:data quakes
                    :mapping {:x :long :y :lat :z :depth}
                    :charts [(chart/cloud {:size 5 :shape \s :color [200 80 40 220] :stroke-color :black
                                           :stroke-width 0.3 :domain {:z [690 40]}
                                           :axis-titles {:x "longitude" :y "latitude" :z "depth"}})]}
                   {:width 600 :height 480})

;; ### Marks

;; Points, spikes from a base, bars, or nothing but the side projections of the points on the far walls; with
;; `:bins` the rows are counted on an x / y grid and the counts are the heights - a 2D histogram:

(nu/options-table (describe/own-rows (describe/options (chart/cloud))) :marks)

(plot/render-image (plot/arrange [{:data quakes
                                   :mapping {:x :long :y :lat :z :mag}
                                   :charts [(chart/cloud {:type :spikes :base 4 :size 3})]
                                   :labels {:title ":type :spikes, :base 4"}}
                                  {:data quakes
                                   :mapping {:x :long :y :lat :z :depth}
                                   :charts [(chart/cloud {:size 2 :domain {:z [690 40]}
                                                          :projections {:xy :density :xz :contour :yz :points}})]
                                   :labels {:title ":projections"}}
                                  {:data quakes
                                   :mapping {:x :long :y :lat}
                                   :charts [(chart/cloud {:bins [10 10] :type :bars :bar-size [0.9 0.9]})]
                                   :labels {:title ":bins [10 10], :type :bars"}}]
                                 {:columns 3})
                   {:width 1000 :height 380})

(nu/shared-groups (describe/options (chart/cloud)))

;; ## surface

;; A mesh of quads over a grid: gridded data (x, y, z per lattice node), a function of [x y] sampled over a domain,
;; a parametric surface, or the 2D density of mapped points; heights coloured through the colour scale.

(nu/options-table (describe/mapping (chart/surface)))

(def lattice (for [x (range -2 2.01 0.25) y (range -2 2.01 0.25)]
               {:x x :y y :z (* x (Math/exp (- (+ (* x x) (* y y)))))}))

(nu/data-table lattice)

(plot/render-image {:data lattice
                    :mapping {:x :x :y :y :z :z}
                    :charts [(chart/surface)]}
                   {:width 600 :height 480})

;; Its options: the source (`:f` with the 3D frame's `:domain`, `:fu` over `:u` / `:v`, or `:density`), the grid
;; size, and the drawing - filled, wireframe or both, shaded, in one colour:

(nu/options-table (describe/own-rows (describe/options (chart/surface))))

(plot/render-image (plot/arrange [{:charts [(chart/surface {:f (fn [[x y]] (Math/sin (Math/hypot x y)))
                                                            :domain {:x [-6 6] :y [-6 6]} :n 25 :style :both})]
                                   :labels {:title ":f, :style :both"}
                                   :legend false}
                                  {:charts [(chart/surface {:fu (fn [[u v]] [(* (+ 2.0 (Math/cos v)) (Math/cos u))
                                                                             (* (+ 2.0 (Math/cos v)) (Math/sin u))
                                                                             (Math/sin v)])
                                                            :u [0.0 (* 2 Math/PI)] :v [0.0 (* 2 Math/PI)] :n [36 18]
                                                            :aspect [1 1 0.35] :color [120 170 210] :shade true})]
                                   :labels {:title ":fu (a torus), one colour"}}
                                  {:data iris
                                   :mapping {:x :sepal-length :y :sepal-width}
                                   :charts [(chart/surface {:density true :n 30 :style :wireframe
                                                            :stroke-color [40 40 40]})]
                                   :labels {:title ":density, :wireframe"}
                                   :legend false}]
                                 {:columns 3})
                   {:width 1000 :height 380})

(nu/shared-groups (describe/options (chart/surface)))
