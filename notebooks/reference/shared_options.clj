;; # Shared options

;; Option groups taken by several charts, builders or data functions, documented once here. A chart's page lists the
;; groups it takes and which of their keys (a chart may take only part of a group); builder tables mark the rows of a
;; group with its title.

(ns reference.shared-options
  (:require [fastmath.core :as m]
            [fastmath.kernel :as k]
            [fastmath.random :as r]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.levels :as levels]
            [cljplot2.data.tree :as tree]
            [cljplot2.describe :as describe]
            [cljplot2.schema :as schema]
            [notebook-utils :as nu]))

(def wave (for [t (range 30)] {:t t :v (m/sin (m/* 0.3 t)) :g (if (< t 15) "early" "late")}))

(nu/data-table wave)

(def counts [{:k "a" :g "x" :v 3} {:k "a" :g "y" :v 2} {:k "b" :g "x" :v 5}
             {:k "b" :g "y" :v 4} {:k "c" :g "x" :v 4} {:k "c" :g "y" :v 1}])

(nu/data-table counts)

(nu/source-note {:dataset :iris})

(def iris (nu/dataset :iris))

(nu/data-table iris)

(nu/source-note {:dataset :faithful})

(def faithful (nu/dataset :faithful))

(nu/data-table faithful)

;; ## Charts

;; ### Common options

;; Every chart may read its own rows and merge its own mapping over the plot's - e.g. a reference curve beside the
;; data (Data and mapping chapter) - and leave its key glyph out of the legends.

(nu/options-table (describe/fragment-rows :common))

(plot/render-image {:data wave
                    :mapping {:x :t :y :v}
                    :charts [(chart/scatter)
                             (chart/line {:data (for [t (range 0 30 0.5)] {:t t :v (m/sin (m/* 0.3 t))})})]}
                   {:width 500 :height 250})

;; `:legend false` keeps a chart's glyph out of the legend while its colours still follow the scale: the rug's
;; ticks would otherwise be drawn over the scatter's dots in every key (left). A legend no chart draws a key into is
;; dropped.

(plot/render-image (plot/arrange (for [legend [true false]]
                                   {:data wave
                                    :mapping {:x :t :y :v :color #(if (m/pos? (double (:v %))) "up" "down")}
                                    :charts [(chart/scatter) (chart/rug {:mapping {:y nil} :legend legend})]
                                    :labels {:title (str "rug :legend " legend)}})
                                 {:columns 2})
                   {:width 800 :height 250})

;; ### Annotation options

;; The rule, rect and text charts mark literal positions. By default their positions count for the scales (the axis
;; reaches them); `:extent false` leaves them out - the rule at y = 2 is then off the panel (right).

(nu/options-table (describe/fragment-rows :annotation))

(plot/render-image (plot/arrange (for [extent [true false]]
                                   {:data wave
                                    :mapping {:x :t :y :v}
                                    :charts [(chart/line) (chart/rule {:y 2 :extent extent :dash :dashed})]
                                    :labels {:title (str ":extent " extent)}})
                                 {:columns 2})
                   {:width 800 :height 250})

;; ### Line style

;; Colour, width and dash of the lines of line-like charts (line, rule, segment, smooth, density, ecdf ...); a
;; mapped aesthetic wins over the fixed value.

(nu/options-table (describe/fragment-rows :line-style))

(plot/render-image {:data wave
                    :mapping {:x :t :y :v}
                    :charts [(chart/line {:color [178 34 34] :line-width 3 :dash [6 2]})]}
                   {:width 500 :height 250})

;; Every built-in dash style (a dash may also be a vector of dash and gap lengths, as above):

(plot/render-image {:charts (vec (mapcat (fn [i d]
                                           [(chart/rule {:y i :dash d :line-width 2})
                                            (chart/text {:x 0 :y i :text (str d) :anchor [:left :bottom]})])
                                         (range 1 100) (:dash describe/attribute-values)))
                    :scales {:x {:domain [0 1] :axis false} :y {:domain [0 9] :reverse true :axis false}}}
                   {:width 500 :height 320})

;; ### Curves

;; The line between data points of lines and ribbons: straight, a fastmath interpolation or a step, sampled for
;; drawing.

(nu/options-table (describe/fragment-rows :curve))

(def points (for [x (range 8)] {:x x :y (nth [1 3 2 5 4 4.5 2 3] x)}))

(nu/data-table points)

(plot/render-image (plot/arrange (for [interpolator [:linear :monotone :cubic :step]]
                                   {:data points
                                    :mapping {:x :x :y :y}
                                    :charts [(chart/line {:interpolator interpolator}) (chart/scatter)]
                                    :labels {:title (str ":interpolator " interpolator)}})
                                 {:columns 4})
                   {:width 1000 :height 260})

;; Every interpolation method (`schema/interpolation-methods`) through the same points, with default parameters -
;; most pass through them or smooth them; `:pava` and `:cir` fit a monotone curve - `:pava` flat runs joined by a
;; step, `:cir` flat, then sloping; `:kriging` stays near a constant (its variogram is fitted to the points, by a
;; seeded search, so every render draws the same curve), and `:gp` / `:rbf` use a kernel too narrow for points this
;; far apart:

(plot/render-image (plot/arrange (for [interpolator (sort schema/interpolation-methods)]
                                   {:data points
                                    :mapping {:x :x :y :y}
                                    :charts [(chart/line {:interpolator interpolator}) (chart/scatter {:size 4})]
                                    :labels {:title (str interpolator)}})
                                 {:columns 4})
                   {:width 1000 :height 800})

;; `:interpolator-params` are the method's arguments after the points - for `:gp` and `:rbf` a fastmath kernel. A
;; curve is fitted in pixels along the line's horizontal axis, so the kernel's width is in pixels: here 60, a bit
;; more than the gap between the points:

(plot/render-image (plot/arrange [{:data points
                                   :mapping {:x :x :y :y}
                                   :charts [(chart/line {:interpolator :gp :interpolator-params [(k/kernel :gaussian {:sigma 60.0})]})
                                            (chart/scatter {:size 4})]
                                   :labels {:title ":gp, gaussian kernel sigma 60"}}
                                  {:data points
                                   :mapping {:x :x :y :y}
                                   :charts [(chart/line {:interpolator :rbf :interpolator-params [(k/rbf :gaussian {:scale 60.0})]})
                                            (chart/scatter {:size 4})]
                                   :labels {:title ":rbf, gaussian scale 60"}}]
                                 {:columns 2})
                   {:width 800 :height 280})

;; Without an interpolator, `:curve` draws a smooth path through the points in their order - also a closed or
;; looping one (`:parametric`, a Catmull-Rom curve; `:bezier`, bezier pieces); `:sample-step` sets how densely a
;; curve is sampled (here coarse, to show the samples):

(plot/render-image (plot/arrange (for [[title opts] [[":curve :parametric" {:curve :parametric}]
                                                     [":curve :bezier" {:curve :bezier}]
                                                     [":sample-step 25 (cubic)" {:interpolator :cubic :sample-step 25}]]]
                                   {:data points
                                    :mapping {:x :x :y :y}
                                    :charts [(chart/line opts) (chart/scatter {:size 4})]
                                    :labels {:title title}})
                                 {:columns 3})
                   {:width 1000 :height 260})

;; ### Fill patterns

;; A pattern drawn over filled marks (bars, areas, boxes, tiles of hierarchies ...), in its own colour.

(nu/options-table (describe/fragment-rows :pattern))

(plot/render-image {:data (for [[i p] (map-indexed vector [:hatch :back-hatch :cross :dots])] {:p (name p) :v (+ 3 i)})
                    :mapping {:x :p :y :v :pattern :p}
                    :charts [(chart/bar {:color :white :stroke-color :black :pattern-color :black})]
                    :legend false}
                   {:width 500 :height 260})

;; ### Rounded corners

;; Rounded bar corners: all of them, or only the end away from the baseline.

(nu/options-table (describe/fragment-rows :corner-radius))

(plot/render-image {:data counts
                    :mapping {:x :k :y :v :color :g}
                    :charts [(chart/bar {:corner-radius-end 8})]}
                   {:width 500 :height 260})

;; ### Strip options

;; Strip charts (bar, box, violin, stripplot, beeswarm, dotplot, rug, bullet, extent) draw one item per level of a
;; categorical axis.

;; #### Placement

;; Where the items sit across the value axis, how wide, and which axis holds the levels:

(nu/options-table (describe/fragment-rows :strip) :placement)

(plot/render-image {:data iris
                    :mapping {:x :species :y :sepal-length}
                    :charts [(chart/violin {:width 0.9}) (chart/box {:width 0.15})]}
                   {:width 500 :height 280})

;; Without levels an item sits by `:place`: across the panel's middle (`:width` a fraction of the panel), below the
;; data (`:bottom` - the y axis reaches below zero, `:width` a fraction of the room under zero), at the panel's edge
;; or at a value of the other axis (`{:at v}`) - those two `:thickness` pixels thick. Here a box of the eruption
;; times over the scatter:

(plot/render-image (plot/arrange (for [[place opts] [[:center {:width 0.15}] [:bottom {:width 0.8}]
                                                     [:edge {:thickness 12}] [{:at 60} {:thickness 20}]]]
                                   {:data faithful
                                    :mapping {:x :eruptions :y :waiting}
                                    :charts [(chart/scatter {:size 4}) (chart/box (merge {:place place :mapping {:y nil}} opts))]
                                    :labels {:title (str ":place " place " " opts)}})
                                 {:columns 2})
                   {:width 900 :height 600})

;; `:level-axis` names the axis of the levels - needed when both axes or neither are categorical; here the rounded
;; petal widths on y (0 to 3), declared categorical, are the levels, a strip of sepal lengths at each (points spread
;; across the band by the [stripplot](reference.charts_per_level.html#stripplot)'s seeded `:jitter`):

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y (levels/categorical #(m/round (:petal-width %)))}
                    :charts [(chart/stripplot {:level-axis :y :jitter 0.3 :rng (r/rng :mersenne 5)})]
                    :labels {:x "sepal length" :y "petal width, rounded"}}
                   {:width 500 :height 280})

;; #### Groups

;; Groups of one level share its band: side by side (`:dodge`, with a gap), on top of each other, or one half each
;; (`:mirror`).

(nu/options-table (describe/fragment-rows :strip) :groups)

(plot/render-image {:data counts
                    :mapping {:x :k :y :v :color :g}
                    :charts [(chart/bar {:position :dodge :dodge-gap 0.3})]}
                   {:width 500 :height 260})

;; #### Counts

;; Each item's row count written past its largest value.

(nu/options-table (describe/fragment-rows :strip) :n-label)

(plot/render-image {:data iris
                    :mapping {:x :species :y :petal-length}
                    :charts [(chart/box {:n-label true :n-format "%d flowers"})]}
                   {:width 500 :height 280})

;; ### Mirrored strips

;; Two groups on the two sides of the value axis, e.g. a population pyramid.

(nu/options-table (describe/fragment-rows :mirror))

(plot/render-image {:data counts
                    :mapping {:x :k :y :v :color :g}
                    :charts [(chart/bar {:mirror :color})]
                    :flip true
                    :scales {:x {:reverse true}}}
                   {:width 500 :height 260})

;; The same mirroring upright, and on a range chart (the second group's intervals below the axis):

(plot/render-image (plot/arrange [{:data counts
                                   :mapping {:x :k :y :v :color :g}
                                   :charts [(chart/bar {:mirror :color})]
                                   :labels {:title "bar, :mirror :color"}}
                                  {:data iris
                                   :mapping {:x :species :y :sepal-length :color #(if (< (:petal-width %) 1.3) "narrow" "wide")}
                                   :charts [(chart/extent {:mirror :color})]
                                   :labels {:title "extent, :mirror :color"}}]
                                 {:columns 2})
                   {:width 900 :height 300})

;; ### Ridgelines

;; Histograms and densities over a categorical axis - one row per level, overlapping by `:overlap` rows.

(nu/options-table (describe/fragment-rows :ridge))

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :species :color :species}
                    :charts [(chart/density {:overlap 1.8 :fill :color :alpha 0.7})]
                    :scales {:y {:reverse true}}
                    :legend false}
                   {:width 500 :height 300})

;; ### Cell style

;; The cell shape and outline of grid-cell charts (tile, bin2d): any fastmath grid type, scaled. Every cell type
;; binning the same points:

(nu/options-table (describe/fragment-rows :cell))

(plot/render-image (plot/arrange (for [cell (:cell describe/attribute-values)]
                                   {:data iris
                                    :mapping {:x :sepal-length :y :sepal-width}
                                    :charts [(chart/bin2d {:cell cell :stroke-color :white})]
                                    :labels {:title (str ":cell " cell)}
                                    :legend false})
                                 {:columns 3})
                   {:width 900 :height 560})

;; ### Fields

;; Raster and contour charts draw a scalar field.

;; #### A function

;; The field as a function of a point over a domain ...

(nu/options-table (describe/fragment-rows :field) :function)

(plot/render-image {:charts [(chart/contour {:f (fn [[x y]] (+ (* x x) (* 2 y y))) :domain {:x [-2 2] :y [-2 2]}})]}
                   {:width 450 :height 350})

;; #### A density estimate

;; ... or the kernel density estimate of mapped points ...

(nu/options-table (describe/fragment-rows :field) :density)

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :sepal-width}
                    :charts [(chart/field {:kernel :epanechnikov :bandwidth 0.4}) (chart/scatter {:size 3})]}
                   {:width 450 :height 350})

;; #### The grid

;; ... sampled on a grid of `:n` points per axis (interpolating gridded data with `:interpolation`).

(nu/options-table (describe/fragment-rows :field) :grid)

(plot/render-image {:charts [(chart/field {:f (fn [[x y]] (m/sin (m/* x y))) :domain {:x [0 3] :y [0 3]} :n 12})]}
                   {:width 450 :height 350})

;; ### Hierarchy labels

;; Group and leaf labels of treemaps, partitions and circle packings: off, on, or a function writing them.

(nu/options-table (describe/fragment-rows :hierarchy-labels))

(def budget [{:dept "science" :team "physics" :cost 40} {:dept "science" :team "biology" :cost 25}
             {:dept "arts" :team "music" :cost 15} {:dept "arts" :team "theatre" :cost 10}
             {:dept "sport" :team "football" :cost 20}])

(nu/data-table budget)

(plot/render-image {:data budget
                    :mapping {:hierarchy [:dept :team] :value :cost}
                    :charts [(chart/treemap {:group-labels false :leaf-labels #(str (:name %) " " (:value %))})]}
                   {:width 500 :height 300})

;; ### 3D frame

;; 3D charts (cloud, surface) draw their data in a box seen in perspective.

;; #### View

;; The eye sits at an azimuth and elevation, at a perspective distance; the box has proportions and may mirror axes.

(nu/options-table (describe/fragment-rows :frame-3d) :view)

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :sepal-width :z :petal-length :color :species}
                    :charts [(chart/cloud {:view {:azimuth 60 :elevation 20 :distance 0.3} :aspect [1 1 0.6]})]}
                   {:width 500 :height 400})

;; #### Axes

;; The box, the ticks and the axis titles.

(nu/options-table (describe/fragment-rows :frame-3d) :axes)

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :sepal-width :z :petal-length}
                    :charts [(chart/cloud {:box false :axis-titles {:x "sepal L" :y "sepal W" :z "petal L"}})]}
                   {:width 500 :height 400})

;; #### Domains

;; Explicit domains - needed when several 3D charts share a plot.

(nu/options-table (describe/fragment-rows :frame-3d) :domain)

(plot/render-image {:data iris
                    :mapping {:x :sepal-length :y :sepal-width :z :petal-length}
                    :charts [(chart/cloud {:domain {:x [4 8] :y [2 4.5] :z [0 7]}})]}
                   {:width 500 :height 400})

;; ### 3D shading

;; Shaded 3D faces (surface quads, cloud bars) are lit by Lambert's law: an ambient share of the colour plus the rest
;; by the face's angle to the light, whose direction is given in view coordinates (x right, y up, z away from the eye).

(nu/options-table (describe/fragment-rows :shading))

;; The default shading beside a darker ambient share lit from the right, on the surface $z = \sin(xy)$.

(plot/render-image (plot/arrange [{:charts [(chart/surface {:f (fn [[x y]] (m/sin (m/* x y)))
                                                            :domain {:x [-2 2] :y [-2 2]} :color [120 160 220]})]
                                   :labels {:title "default shading"}}
                                  {:charts [(chart/surface {:f (fn [[x y]] (m/sin (m/* x y)))
                                                            :domain {:x [-2 2] :y [-2 2]} :color [120 160 220]
                                                            :ambient 0.2 :light [1.0 1.0 -0.5]})]
                                   :labels {:title ":ambient 0.2 :light [1 1 -0.5]"}}]
                                 {:columns 2 :title "$z = \\sin(xy)$"})
                   {:width 800 :height 400})

;; ### Text style

;; Font, size, colour, padding, alignment and background of the `text` chart - also the style of every theme text
;; role (Theme chapter).

(nu/options-table (describe/fragment-rows :text-style))

(plot/render-image {:data wave
                    :mapping {:x :t :y :v}
                    :charts [(chart/line)
                             (chart/text {:x 15 :y 0 :text "a styled note" :size 14 :font-style :bold
                                          :color :white :background [70 90 140] :inset 6})]}
                   {:width 500 :height 250})

;; ## Builders

;; ### Builder options

;; Title, scales and theme of the plot a builder returns.

(nu/options-table (describe/fragment-rows :builder))

(def flows [{:from "coal" :to "power" :value 30} {:from "gas" :to "power" :value 20} {:from "gas" :to "homes" :value 15}
            {:from "power" :to "homes" :value 25} {:from "power" :to "industry" :value 25}])

(nu/data-table flows)

(plot/render-image (plot/sankey flows {:title "Energy flows" :theme {:plot {:background :white}}})
                   {:width 500 :height 300})

;; ### Multiplot layout

;; The grid of `plot/arrange` (and of a plot's `:arrange`): shape, weights, panel alignment.

(nu/options-table (describe/fragment-rows :arrange))

(plot/render-image (plot/arrange [{:data wave :mapping {:x :t :y :v} :charts [(chart/line)]}
                                  {:data wave :mapping {:x :g :y :v} :charts [(chart/box)]}]
                                 {:columns 2 :widths [3 1]})
                   {:width 700 :height 250})

;; ## Builders and data functions

;; The options a builder forwards to the data function computing its layout; the function takes them directly too.

;; ### Graph

;; The edge rows' ends, weight and node ids, optional node rows, directedness. A directed
;; [network](reference.builders_graphs.html#network) with node labels (`:labels`) and a seeded layout (`:rng`):

(nu/options-table (describe/fragment-rows :graph/graph))

(def edges [{:a "x" :b "y" :w 3} {:a "y" :b "z" :w 1} {:a "z" :b "x" :w 2} {:a "z" :b "u" :w 2}])

(nu/data-table edges)

(plot/render-image (plot/network edges {:from :a :to :b :value :w :directed true :labels true :rng (r/rng :mersenne 1)})
                   {:width 450 :height 300})

;; ### Graph layout

;; Node positions of a network: force-directed, a circle, or random.

(nu/options-table (describe/fragment-rows :graph/layout))

(plot/render-image (plot/network edges {:from :a :to :b :layout :circle :labels true}) {:width 450 :height 300})

;; ### Chord layout

;; Node order, gaps between sectors, the ring's inner radius and the smallest drawn flow.

(nu/options-table (describe/fragment-rows :graph/chord-layout))

(plot/render-image (plot/chord flows {:gap-angle 6 :inner-radius 0.8}) {:width 450 :height 450})

;; ### Arc layout

;; Node order along the line.

(nu/options-table (describe/fragment-rows :graph/arc-layout))

(plot/render-image (plot/arc edges {:from :a :to :b :order :cluster}) {:width 450 :height 250})

;; ### Sankey layout

;; Columns, node width, gaps and relaxation of a sankey diagram.

(nu/options-table (describe/fragment-rows :graph/sankey-layout))

(plot/render-image (plot/sankey flows {:align :left :node-width 0.06 :gap 0.08}) {:width 500 :height 300})

;; ### Alluvial graph

;; The axes (categorical columns) the strands cross, their weights and which axis colours them.

(nu/options-table (describe/fragment-rows :graph/alluvial-graph))

(plot/render-image (plot/alluvial [{:class "1st" :sex "f" :n 140} {:class "1st" :sex "m" :n 180}
                                   {:class "3rd" :sex "f" :n 190} {:class "3rd" :sex "m" :n 510}]
                                  {:variables [:class :sex] :value :n :strand-color :sex})
                   {:width 500 :height 300})

;; ### Dendrogram layout

;; A cut into `:k` clusters and a circular layout.

(nu/options-table (describe/fragment-rows :tree/layout))

(def org (tree/from-levels [{:div "north" :team "a"} {:div "north" :team "b"} {:div "south" :team "c"}
                            {:div "south" :team "d"} {:div "south" :team "e"}]
                           {:hierarchy [:div :team]}))

(plot/render-image (plot/dendrogram org {:k 2}) {:width 450 :height 300})

;; ### Node-link tree layout

;; Tidy or cluster placement, diagonal or straight (`:line`) links, circular.

(nu/options-table (describe/fragment-rows :tree/node-link-layout))

(plot/render-image (plot/tree org {:layout :cluster :links :line}) {:width 450 :height 300})

;; ### Edge bundling

;; Edges between the tree's leaves, bent along the tree with strength `:beta`.

(nu/options-table (describe/fragment-rows :tree/bundle-paths))

(plot/render-image (plot/bundle [{:from "a" :to "d"} {:from "b" :to "e"} {:from "a" :to "c"}] org {:beta 0.9})
                   {:width 400 :height 400})

;; ### Sequences

;; Step sequences as text (`"home-search-end"`) split into rings of a flow sunburst.

(nu/options-table (describe/fragment-rows :hierarchy/sequence-levels))

(plot/render-image (plot/sequences [{:sequence "home-search-product" :n 5} {:sequence "home-product" :n 3}
                                    {:sequence "search-product-cart" :n 2}]
                                   {:value :n})
                   {:width 400 :height 400})

;; ### Autocorrelation rows

;; The lags of the correlogram, the confidence level of its bands and whether the rows are the partial autocorrelation (`plot/acf` forwards `:lags` and `:level`; it picks the correlograms with its own `:correlograms`).

(nu/options-table (describe/fragment-rows :series/acf-rows))

(plot/render-image (plot/acf (map #(m/sin (m/* 0.5 %)) (range 100)) {:lags 30 :correlograms :pacf})
                   {:width 600 :height 300})

;; ### Horizon rows

;; The keys of the series rows, the number and height of the bands and resampling between points.

(nu/options-table (describe/fragment-rows :series/horizon-rows))

(plot/render-image (plot/horizon (for [s ["a" "b"] t (range 60)]
                                   {:x t :y (* (if (= s "a") 1 -1) (m/sin (m/* 0.2 t)) t) :series s})
                                 {:series :series :bands 3})
                   {:width 600 :height 250})

;; ### Named theme styles

;; A chart's style comes from the theme `[:chart kind]`; `:theme-style` names a `[:chart name]` entry merged over
;; it - a partial style a theme defines once and charts pick by name (builders draw with their own, see each
;; builder's page). `build` checks the merged style against the chart's style keys; a name the theme hasn't is an
;; error.

(nu/options-table (describe/fragment-rows :theme-style))

;; Highlighted points in a style named `:highlight`, the rest in the scatter style:

(plot/render-image {:data points
                    :mapping {:x :x :y :y}
                    :charts [(chart/scatter)
                             (chart/scatter {:data (filter #(> (:y %) 4) points) :theme-style :highlight})]
                    :theme {:chart {:highlight {:size 14 :color [215 48 39] :shape \s}}}}
                   {:width 500 :height 300})
