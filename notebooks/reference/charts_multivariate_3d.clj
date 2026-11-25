;; # Charts: multivariate and 3D

;; Many variables at once: a line per row across one axis per variable (`parallel`, a radar in polar
;; coordinates), points in a 3D box (`cloud`), surfaces over a grid (`surface`), and a matrix of plots of every pair of
;; variables (scatter-plot matrix, at the end). Each chart lists the mapping keys
;; it reads, its own options and the shared option groups it takes (documented once in the Shared options chapter).

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

;; The scalings side by side: centred values (`:center`, original units less each variable's mean) and the robust
;; scaling - values less a centre over a spread, the median over the median absolute deviation by default, here also
;; the mean over a quantile spread (`:robust-spread 0.25` - the interquartile half-range). Every variable name is
;; drawn, also when the names collide; long names in narrow plots turn with the theme's
;; `[:axis :x :label-rotation]`:

(plot/render-image (plot/arrange [{:data iris
                                   :mapping {:color :species}
                                   :charts [(chart/parallel {:variables [:sepal-length :sepal-width :petal-length :petal-width]
                                                             :scaling :center})]
                                   :labels {:title ":scaling :center" :x nil :y nil}
                                   :theme {:axis {:x {:label-rotation -30}}}
                                   :legend false}
                                  {:data iris
                                   :mapping {:color :species}
                                   :charts [(chart/parallel {:variables [:sepal-length :sepal-width :petal-length :petal-width]
                                                             :scaling :robust :robust-center :median :robust-spread :mad})]
                                   :labels {:title ":robust, :median / :mad" :x nil :y nil}
                                   :theme {:axis {:x {:label-rotation -30}}}
                                   :legend false}
                                  {:data iris
                                   :mapping {:color :species}
                                   :charts [(chart/parallel {:variables [:sepal-length :sepal-width :petal-length :petal-width]
                                                             :scaling :robust :robust-center :mean :robust-spread 0.25})]
                                   :labels {:title ":robust, :mean / 0.25" :x nil :y nil}
                                   :theme {:axis {:x {:label-rotation -30}}}}]
                                 {:columns 3 :widths [1 1 1.3]})
                   {:width 1000 :height 360})

;; A missing value breaks a row's line there (`:break`) or drops the row (`:drop`); without `:axes` only the lines
;; are drawn, here also without padding:

(def readings [{:id "a" :t1 3 :t2 5 :t3 4 :t4 6} {:id "b" :t1 4 :t2 nil :t3 6 :t4 5}
               {:id "c" :t1 6 :t2 4 :t3 nil :t4 3} {:id "d" :t1 5 :t2 6 :t3 5 :t4 4}])

(nu/data-table readings)

(plot/render-image (plot/arrange [{:data readings
                                   :mapping {:color :id}
                                   :charts [(chart/parallel {:variables [:t1 :t2 :t3 :t4] :missing :break :line-width 2})]
                                   :labels {:title ":missing :break" :x nil :y nil}
                                   :legend false}
                                  {:data readings
                                   :mapping {:color :id}
                                   :charts [(chart/parallel {:variables [:t1 :t2 :t3 :t4] :missing :drop :line-width 2})]
                                   :labels {:title ":missing :drop" :x nil :y nil}
                                   :legend false}
                                  {:data readings
                                   :mapping {:color :id}
                                   :charts [(chart/parallel {:variables [:t1 :t2 :t3 :t4] :axes false :padding {:y [0 0]}
                                                             :line-width 2})]
                                   :labels {:title ":axes false, :padding {:y [0 0]}" :x nil :y nil}}]
                                 {:columns 3 :widths [1 1 1.2]})
                   {:width 1000 :height 320})

;; ### Style

;; Smooth curves between the axes, and - in polar coordinates, where the axes become spokes of a radar - a fill of
;; each row's polygon (a value every spoke starts at, here 0 at the centre, is labelled once):

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

;; Spokes on their own ranges (the default `:uniminmax`) start at different values, their first ticks close to the
;; centre where the spokes meet; a tick label there crossing another spoke is dropped (here 3.0 of the drat axis):

(nu/source-note {:dataset :mtcars})

(def mtcars (nu/dataset :mtcars))

(nu/data-table mtcars)

(plot/render-image {:data (filter #(#{"Mazda RX4" "Merc 240D" "Lotus Europa" "Cadillac Fleetwood"} (:model %)) mtcars)
                    :mapping {:color :model}
                    :charts [(chart/parallel {:variables [:mpg :hp :wt :qsec :drat :disp] :fill :color})]
                    :coord {:kind :polar}
                    :labels {:x nil :y nil}}
                   {:width 640 :height 420})

(nu/shared-groups (describe/options (chart/parallel)))

;; ## cloud

;; Points in a 3D box at their x, y and z, seen in perspective; colour, size and shape may come from the data.

(nu/options-table (describe/mapping (chart/cloud)))

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :sepal-width :z :petal-length :color :species}
                    :charts [(chart/cloud)]}
                   {:width 600 :height 480})

;; ### Style

;; The point style used when nothing is mapped (`:alpha` - the opacity of every mark's colour, mapped or not;
;; `:line-width` - of spikes and lines):

(nu/options-table (describe/own-rows (describe/options (chart/cloud))) :style)

(plot/render-image {:data quakes
                    :mapping {:x :long :y :lat :z :depth}
                    :charts [(chart/cloud {:size 5 :shape \S :color [200 80 40] :alpha 0.85 :stroke-color :black
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

;; `:type :lines` joins a panel's points in data order - one line per value of the mapped `:group` (one line
;; without it); each segment takes its first point's colour and is drawn at its own depth among the other marks.
;; Three spirals, and profiles of a surface - a line along y for every x, coloured by height:

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

;; `:type :none` leaves only the projections - the point cloud's shadows on the three walls:

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :sepal-width :z :petal-length :color :species}
                    :charts [(chart/cloud {:type :none :projections {:xy :points :xz :points :yz :points}})]}
                   {:width 600 :height 480})

;; A coarser density grid with fewer levels (`:projection-grid`, `:projection-levels`); how strongly projected points
;; and density bands are faded is a theme key of `[:chart :cloud]`, not an option:

(nu/options-table (describe/style-rows :cloud [:projection-alpha-factor :band-alpha-factor]))

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :sepal-width :z :petal-length}
                    :charts [(chart/cloud {:type :none :projections {:xy :density :xz :contour :yz :points}
                                           :projection-grid 20 :projection-levels 3})]
                    :theme {:chart {:cloud {:projection-alpha-factor 0.8 :band-alpha-factor 0.3}}}}
                   {:width 600 :height 480})

;; The 3D frame (Shared options) on the same points: the eye moved round and up, the box flattened and the x axis
;; reversed, the box and axes left out:

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

(nu/shared-groups (describe/options (chart/cloud)))

;; ## surface

;; A mesh of quads over a grid: gridded data (x, y, z per lattice node), a function of [x y] sampled over a domain,
;; a parametric surface, or the 2D density of mapped points; heights coloured through the colour scale.

(nu/options-table (describe/mapping (chart/surface)))

(def lattice (for [x (range -2 2.01 0.25) y (range -2 2.01 0.25)]
               {:x x :y y :z (* x (m/exp (- (+ (* x x) (* y y)))))}))

(nu/data-table lattice)

(plot/render-image {:data lattice
                    :mapping {:x :x :y :y :z :z}
                    :charts [(chart/surface)]}
                   {:width 600 :height 480})

;; Its options: the source (`:f` with the 3D frame's `:domain`, `:fu` over `:u` / `:v`, or `:density`), the grid
;; size, and the drawing - filled, wireframe (value-coloured lines) or both, shaded, in one colour:

(nu/options-table (describe/own-rows (describe/options (chart/surface))))

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

;; A density surface reaching past the points by `:cut` bandwidths (0 - the points' range only, the edges cut off
;; high), and the quad outlines' colour and width (outlines of `:style :both`):

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

;; The [3D frame](reference.shared_options.html#d-frame) (Shared options) on a surface: seen from another side and from above, and with y reversed:

(plot/render-image (plot/arrange [{:data lattice
                                   :mapping {:x :x :y :y :z :z}
                                   :charts [(chart/surface {:view {:azimuth -60 :elevation 60}})]
                                   :labels {:title ":view {:azimuth -60 :elevation 60}"}
                                   :legend false}
                                  {:data lattice
                                   :mapping {:x :x :y :y :z :z}
                                   :charts [(chart/surface {:reverse #{:y} :axis-titles {:x "x" :y "y (reversed)" :z "height"}})]
                                   :labels {:title ":reverse #{:y}"}
                                   :legend false}]
                                 {:columns 2})
                   {:width 900 :height 380})

(nu/shared-groups (describe/options (chart/surface)))

;; ## Scatter-plot matrix

;; Not a chart but a matrix of plots of the charts above and on the other pages:
;; [`plot/pairs`](reference.facets_arrange.html#scatter-plot-matrix) gives every pair of `:variables` a cell. By
;; default a scatter plot below the diagonal (`:lower`), the correlation as text above it (`:upper`) and a density on
;; it (`:diagonal`):

(plot/render-image (plot/pairs iris {:variables [:sepal-length :sepal-width :petal-length :petal-width]
                                     :mapping {:color :species}})
                   {:width 700 :height 620})

;; A slot takes one chart, a vector of charts (layered in each cell) or `:none` (empty cells; `nil` - the default); `:diagonal :names` writes
;; the variable's name in the diagonal cell instead:

(plot/render-image (plot/pairs iris {:variables [:sepal-length :sepal-width :petal-length]
                                     :lower [(chart/scatter {:size 4}) (chart/smooth {:method :lm})]
                                     :upper :none
                                     :diagonal :names
                                     :title "scatter and a linear fit below, empty above"})
                   {:width 600 :height 560})
