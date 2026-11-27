;; # Multivariate and 3D

;; These charts draw more variables than two axes hold: a line per row across one axis per variable (`parallel`, a
;; radar in polar coordinates), points in a 3D box (`cloud`) and surfaces over a grid (`surface`). Use them to see
;; rows across many variables at once; for a matrix of plots of every pair of variables see
;; [`plot/pairs`](reference.facets_arrange.html#scatter-plot-matrix).

(ns reference.charts-multivariate-3d
  (:require [fastmath.core :as m]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.describe :as describe]
            [notebook-utils :as nu]))

(nu/source-note {:dataset :iris})

(def iris (nu/dataset :iris))

(nu/data-table iris)

(nu/source-note {:dataset :quakes})

(def quakes (nu/dataset :quakes))

(nu/data-table quakes)

;; The lattice below holds x, y and a function's value z per node, with the node's distance from the centre and its
;; quadrant; the surfaces use it:

(def lattice (for [x (range -2 2.01 0.25) y (range -2 2.01 0.25)]
               {:x x :y y :z (* x (m/exp (- (+ (* x x) (* y y)))))
                :distance (m/hypot x y)
                :quadrant (str (if (neg? y) "S" "N") (if (neg? x) "W" "E"))}))

(nu/data-table lattice)

;; ## Parallel coordinates

;; `chart/parallel` draws a line per row across one vertical axis per variable, each axis scaled on its own. Use it to
;; compare rows over many variables. The `:variables` option names the axes; the mapping only groups the rows.

(nu/shared-groups (describe/options (chart/parallel)))

(nu/mapping-list (describe/mapping (chart/parallel)))

(plot/render-image {:data iris
                    :mapping {:color :species}
                    :charts [(chart/parallel {:variables [:sepal-length :sepal-width :petal-length :petal-width]})]
                    :labels {:x :none :y :none}}
                   {:width 700 :height 400})

;; ### Axes

;; These options set the variables and their order, how each is scaled, fixed limits, the axes themselves and what a
;; missing value does. The panels below draw labelled variables as standard scores, then all on one range with a
;; fixed limit:

(nu/option-cards 'chart/parallel (describe/own-rows (describe/options (chart/parallel))) :axes)

(plot/render-image (plot/arrange [{:data iris
                                   :mapping {:color :species}
                                   :charts [(chart/parallel {:variables [["petal L" :petal-length] ["petal W" :petal-width]
                                                                         ["sepal L" :sepal-length]]
                                                             :scaling :std})]
                                   :labels {:title ":scaling :std, labelled variables" :x :none :y :none}
                                   :legend false}
                                  {:data iris
                                   :mapping {:color :species}
                                   :charts [(chart/parallel {:variables [:petal-length :petal-width :sepal-length]
                                                             :scaling :globalminmax :limits {:petal-width [0 8]}})]
                                   :labels {:title ":globalminmax, :limits {:petal-width [0 8]}" :x :none :y :none}}]
                                 {:columns 2 :widths [1 1.25]})
                   {:width 1000 :height 380})

;; The next panels compare centred values (`:center`) with the robust scaling, which divides each value's distance
;; from a centre by a spread: the median and the median absolute deviation, then the mean and a quantile spread
;; (`:robust-spread 0.25`). The chart draws every variable name, also when names collide; the theme's
;; `[:axis :x :label-rotation]` turns long names in narrow plots:

(plot/render-image (plot/arrange [{:data iris
                                   :mapping {:color :species}
                                   :charts [(chart/parallel {:variables [:sepal-length :sepal-width :petal-length :petal-width]
                                                             :scaling :center})]
                                   :labels {:title ":scaling :center" :x :none :y :none}
                                   :theme {:axis {:x {:label-rotation -30}}}
                                   :legend false}
                                  {:data iris
                                   :mapping {:color :species}
                                   :charts [(chart/parallel {:variables [:sepal-length :sepal-width :petal-length :petal-width]
                                                             :scaling :robust :robust-center :median :robust-spread :mad})]
                                   :labels {:title ":robust, :median / :mad" :x :none :y :none}
                                   :theme {:axis {:x {:label-rotation -30}}}
                                   :legend false}
                                  {:data iris
                                   :mapping {:color :species}
                                   :charts [(chart/parallel {:variables [:sepal-length :sepal-width :petal-length :petal-width]
                                                             :scaling :robust :robust-center :mean :robust-spread 0.25})]
                                   :labels {:title ":robust, :mean / 0.25" :x :none :y :none}
                                   :theme {:axis {:x {:label-rotation -30}}}}]
                                 {:columns 3 :widths [1 1 1.3]})
                   {:width 1000 :height 360})

;; The next panels break the lines at missing values, then drop those rows, then draw only the lines, without axes or
;; padding:

(def readings [{:id "a" :t1 3 :t2 5 :t3 4 :t4 6} {:id "b" :t1 4 :t2 nil :t3 6 :t4 5}
               {:id "c" :t1 6 :t2 4 :t3 nil :t4 3} {:id "d" :t1 5 :t2 6 :t3 5 :t4 4}])

(nu/data-table readings)

(plot/render-image (plot/arrange [{:data readings
                                   :mapping {:color :id}
                                   :charts [(chart/parallel {:variables [:t1 :t2 :t3 :t4] :missing :break :line-width 2})]
                                   :labels {:title ":missing :break" :x :none :y :none}
                                   :legend false}
                                  {:data readings
                                   :mapping {:color :id}
                                   :charts [(chart/parallel {:variables [:t1 :t2 :t3 :t4] :missing :drop :line-width 2})]
                                   :labels {:title ":missing :drop" :x :none :y :none}
                                   :legend false}
                                  {:data readings
                                   :mapping {:color :id}
                                   :charts [(chart/parallel {:variables [:t1 :t2 :t3 :t4] :axes false :padding {:y [0 0]}
                                                             :line-width 2})]
                                   :labels {:title ":axes false, :padding {:y [0 0]}" :x :none :y :none}}]
                                 {:columns 3 :widths [1 1 1.2]})
                   {:width 1000 :height 320})

;; ### Style

;; These options join the axes with smooth curves and fill each row's polygon in polar coordinates, where the axes
;; become the spokes of a radar. The panels below draw smooth curves, then a filled radar; the chart labels a value
;; every spoke starts at (here 0 at the centre) only once:

(nu/option-cards 'chart/parallel (describe/own-rows (describe/options (chart/parallel))) :style)

(def athletes [{:name "Ann" :speed 8 :strength 5 :stamina 9 :skill 6 :reach 4}
               {:name "Bo" :speed 5 :strength 9 :stamina 6 :skill 4 :reach 8}
               {:name "Cy" :speed 6 :strength 6 :stamina 5 :skill 9 :reach 6}])

(nu/data-table athletes)

(plot/render-image (plot/arrange [{:data iris
                                   :mapping {:color :species}
                                   :charts [(chart/parallel {:variables [:sepal-length :sepal-width :petal-length :petal-width]
                                                             :curve :smooth :smoothness 0.6})]
                                   :labels {:title ":curve :smooth" :x :none :y :none}
                                   :legend false}
                                  {:data athletes
                                   :mapping {:color :name}
                                   :charts [(chart/parallel {:variables [:speed :strength :stamina :skill :reach]
                                                             :scaling :globalminmax :limits {:speed [0 10] :strength [0 10]
                                                                                             :stamina [0 10] :skill [0 10]
                                                                                             :reach [0 10]}
                                                             :fill :color :alpha 0.2})]
                                   :coord {:kind :polar}
                                   :labels {:title "polar, :fill :color" :x :none :y :none}}]
                                 {:columns 2 :widths [1.2 1]})
                   {:width 1000 :height 420})

;; Spokes on their own ranges (the default `:uniminmax`) start at different values, with their first ticks near the
;; centre where the spokes meet. The chart drops a tick label there that crosses another spoke (below, 3.0 of the
;; drat axis):

(nu/source-note {:dataset :mtcars})

(def mtcars (nu/dataset :mtcars))

(nu/data-table mtcars)

(plot/render-image {:data (filter #(#{"Mazda RX4" "Merc 240D" "Lotus Europa" "Cadillac Fleetwood"} (:model %)) mtcars)
                    :mapping {:color :model}
                    :charts [(chart/parallel {:variables [:mpg :hp :wt :qsec :drat :disp] :fill :color})]
                    :coord {:kind :polar}
                    :labels {:x :none :y :none}}
                   {:width 640 :height 420})

;; ## 3D clouds

;; `chart/cloud` draws a mark per row in a 3D box at its x, y and z, seen in perspective: points, spikes, lines or
;; bars. Use it to look at three numeric variables together; colour, size and shape may come from the data.

(nu/shared-groups (describe/options (chart/cloud)))

(nu/mapping-list (describe/mapping (chart/cloud)))

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :sepal-width :z :petal-length :color :species}
                    :charts [(chart/cloud)]}
                   {:width 600 :height 480})

;; ### Style

;; These options style the marks; a mapped colour, size or shape wins over its option. The example below draws
;; translucent outlined squares:

(nu/option-cards 'chart/cloud (describe/own-rows (describe/options (chart/cloud))) :style)

(plot/render-image {:data quakes
                    :mapping {:x :long :y :lat :z :depth}
                    :charts [(chart/cloud {:size 5 :shape \S :color [200 80 40] :alpha 0.85 :stroke-color :black
                                           :stroke-width 0.3 :domain {:z [690 40]}})]
                    :labels {:x "longitude" :y "latitude" :z "depth"}}
                   {:width 600 :height 480})

;; ### Marks

;; These options set the marks, the side views of the rows on the far walls and a 2D histogram of the rows. The
;; panels below draw spikes from a base, the three kinds of projection and counted bars:

(nu/option-cards 'chart/cloud (describe/own-rows (describe/options (chart/cloud))) :marks)

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

;; `:type :lines` joins a panel's points in data order, a line per value of the mapped `:group`. Each segment takes
;; its first point's colour and lies at its own depth among the other marks. The panels below draw three spirals,
;; then profiles of a surface, a line along y for every x, coloured by height:

(def spirals (for [g ["a" "b" "c"] t (range 0 12.6 0.3)
                   :let [phase (* 2.0944 (count (take-while #(not= g %) ["a" "b" "c"])))]]
               {:spiral g :x (* (+ 1.0 (* 0.1 t)) (m/cos (+ t phase))) :y (* (+ 1.0 (* 0.1 t)) (m/sin (+ t phase))) :z t}))

(nu/data-table spirals)

(def profiles (for [x (range -2 2.01 0.4) y (range -2 2.01 0.1)]
                {:x x :y y :z (* x (m/exp (- (+ (* x x) (* y y)))))}))

(nu/data-table profiles)

(plot/render-image (plot/arrange [{:data spirals
                                   :mapping {:x :x :y :y :z :z :color :spiral :group :spiral}
                                   :charts [(chart/cloud {:type :lines :line-width 2})]
                                   :labels {:title ":type :lines, :group :spiral"}}
                                  {:data profiles
                                   :mapping {:x :x :y :y :z :z :color :z :group :x}
                                   :charts [(chart/cloud {:type :lines :line-width 1.5 :view {:azimuth 30 :elevation 25}})]
                                   :labels {:title "profiles, :group :x"}}]
                                 {:columns 2})
                   {:width 1000 :height 420})

;; With `:type :none` only the projections remain, the point cloud's shadows on the three walls:

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :sepal-width :z :petal-length :color :species}
                    :charts [(chart/cloud {:type :none :projections {:xy :points :xz :points :yz :points}})]}
                   {:width 600 :height 480})

;; Theme keys fade the projected points and density bands. The plot below uses a coarser density grid with fewer
;; levels and changes both fades:

(nu/theme-key-list :cloud (describe/style-rows :cloud [:projection-alpha-factor :band-alpha-factor]))

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :sepal-width :z :petal-length}
                    :charts [(chart/cloud {:type :none :projections {:xy :density :xz :contour :yz :points}
                                           :projection-grid 20 :projection-levels 3})]
                    :theme {:chart {:cloud {:projection-alpha-factor 0.8 :band-alpha-factor 0.3}}}}
                   {:width 600 :height 480})

;; The panels below apply the [3D frame](reference.shared_options.html#d-frame) options to the same points: they move
;; the eye round and up, flatten the box and reverse the x axis, then leave out the box and axes:

(plot/render-image (plot/arrange [{:data iris
                                   :mapping {:x :sepal-length :y :sepal-width :z :petal-length :color :species}
                                   :charts [(chart/cloud {:view {:azimuth 120 :elevation 45}})]
                                   :labels {:title ":azimuth 120 :elevation 45"}
                                   :legend false}
                                  {:data iris
                                   :mapping {:x :sepal-length :y :sepal-width :z :petal-length :color :species}
                                   :charts [(chart/cloud {:aspect [1 1 0.4] :reverse #{:x}})]
                                   :labels {:title ":aspect, :reverse #{:x}"}
                                   :legend false}
                                  {:data iris
                                   :mapping {:x :sepal-length :y :sepal-width :z :petal-length :color :species}
                                   :charts [(chart/cloud {:box false :axes false})]
                                   :labels {:title ":box false, :axes false"}}]
                                 {:columns 3 :widths [1 1 1.3]})
                   {:width 1000 :height 360})

;; ## 3D surfaces

;; `chart/surface` draws a mesh of quads from gridded data, a function of `[x y]` over a domain, a parametric surface
;; or the 2D density of mapped points, coloured by height, by a function's colour value or by a mapped column. Use it
;; to see the shape of a function or density of two variables.

(nu/shared-groups (describe/options (chart/surface)))

(nu/mapping-list (describe/mapping (chart/surface)))

(plot/render-image {:data lattice
                    :mapping {:x :x :y :y :z :z}
                    :charts [(chart/surface)]}
                   {:width 600 :height 480})

;; These options set the source (`:f` over the 3D frame's `:domain`, `:fu` over `:u` and `:v`, or `:density`), the
;; grid size and the drawing: filled quads, a wireframe or both, shading and one colour. The panels below draw a
;; function as outlined quads, a one-colour torus and a density wireframe:

(nu/option-cards 'chart/surface (describe/own-rows (describe/options (chart/surface))))

(plot/render-image (plot/arrange [{:charts [(chart/surface {:f (fn [[x y]] (m/sin (m/hypot x y)))
                                                            :domain {:x [-6 6] :y [-6 6]} :n 25 :style :both})]
                                   :labels {:title ":f, :style :both"}
                                   :legend false}
                                  {:charts [(chart/surface {:fu (fn [[u v]] [(* (+ 2.0 (m/cos v)) (m/cos u))
                                                                             (* (+ 2.0 (m/cos v)) (m/sin u))
                                                                             (m/sin v)])
                                                            :u [0.0 m/TWO_PI] :v [0.0 m/TWO_PI] :n [36 18]
                                                            :aspect [1 1 0.35] :color [120 170 210] :shade true})]
                                   :labels {:title ":fu (a torus), one colour"}}
                                  {:data iris
                                   :mapping {:x :sepal-length :y :sepal-width}
                                   :charts [(chart/surface {:density true :n 30 :style :wireframe})]
                                   :labels {:title ":density, :wireframe"}
                                   :legend false}]
                                 {:columns 3})
                   {:width 1000 :height 380})

;; The next panels stop a density surface at the points' range (`:cut 0`), where its edges end in steep walls, then
;; reach 3 bandwidths past them (the default), then outline the quads of `:style :both`:

(plot/render-image (plot/arrange [{:data iris
                                   :mapping {:x :sepal-length :y :sepal-width}
                                   :charts [(chart/surface {:density true :cut 0})]
                                   :labels {:title ":density, :cut 0"}
                                   :legend false}
                                  {:data iris
                                   :mapping {:x :sepal-length :y :sepal-width}
                                   :charts [(chart/surface {:density true :cut 3})]
                                   :labels {:title ":cut 3 (default)"}
                                   :legend false}
                                  {:data lattice
                                   :mapping {:x :x :y :y :z :z}
                                   :charts [(chart/surface {:style :both :stroke-color [40 40 40] :stroke-width 2})]
                                   :labels {:title ":stroke-color, :stroke-width 2"}}]
                                 {:columns 3 :widths [1 1 1.25]})
                   {:width 1000 :height 360})

;; The panels below apply the [3D frame](reference.shared_options.html#d-frame) options to a surface: they view it
;; from another side and from above, then reverse y:

(plot/render-image (plot/arrange [{:data lattice
                                   :mapping {:x :x :y :y :z :z}
                                   :charts [(chart/surface {:view {:azimuth -60 :elevation 60}})]
                                   :labels {:title ":view {:azimuth -60 :elevation 60}"}
                                   :legend false}
                                  {:data lattice
                                   :mapping {:x :x :y :y :z :z}
                                   :charts [(chart/surface {:reverse #{:y}})]
                                   :labels {:x "x" :y "y (reversed)" :z "height" :title ":reverse #{:y}"}
                                   :legend false}]
                                 {:columns 2})
                   {:width 900 :height 380})

;; A mapped `:color` colours gridded data by another column instead of the height. On a numeric column a quad takes
;; the mean of its corners' values; on a categorical column it takes the most common one. The left panel colours the
;; lattice by distance from the centre, the right one by quadrant:

(plot/render-image (plot/arrange [{:data lattice
                                   :mapping {:x :x :y :y :z :z :color :distance}
                                   :charts [(chart/surface)]}
                                  {:data lattice
                                   :mapping {:x :x :y :y :z :z :color :quadrant}
                                   :charts [(chart/surface)]}]
                                 {:columns 2})
                   {:width 1000 :height 400})
