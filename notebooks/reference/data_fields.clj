;; # Fields

;; These functions turn functions of the plane into something to draw: a colour per pixel of a complex function, the
;; level lines of a gridded field, and the rows of a vector field (arrows, traces and points mapped through it). Use
;; them with ordinary charts: `field`, `line`, `segment` and `scatter`.

(ns reference.data-fields
  (:require [fastmath.complex :as cx]
            [fastmath.core :as m]
            [fastmath.fields :as f]
            [fastmath.random :as r]
            [fastmath.vector :as v]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.contour :as contour]
            [cljplot2.data.field :as field]
            [cljplot2.data.vector-field :as vf]
            [cljplot2.describe :as describe]
            [notebook-utils :as nu]))

;; The grid below holds a field `sin x + cos 1.3y` over [-3 3]²: axis coordinates `:xs` and `:ys`, their counts `:nx`
;; and `:ny`, and `:values` row by row, x fastest; the isolines section uses it. The table below holds its axis
;; coordinates, the same along x and y:

(def grid-xs (range -3.0 3.01 0.25))

(nu/data-table grid-xs)

(def grid {:xs (double-array grid-xs) :ys (double-array grid-xs) :nx (count grid-xs) :ny (count grid-xs)
           :values (double-array (for [y grid-xs x grid-xs] (m/+ (m/sin x) (m/cos (m/* 1.3 y)))))})

;; The points below form a grid of seeds over [-2 2]²; the trace and mapped point sections use them:

(def seeds (for [x (range -2.0 2.01 0.5) y (range -2.0 2.01 0.5)] [x y]))

(nu/data-table seeds)

;; ## Domain colouring

;; `field/domain-color` returns a `:pixel-color` function for `chart/field` that colours a complex function by domain
;; colouring: the hue is the argument of f(x + iy), the brightness its magnitude wrapped into [0 1], in a hue-first
;; colour space. Use it to see the zeros, poles and phase of a complex function.

(nu/argument-list #'field/domain-color)

;; These options set how the magnitude wraps and the colour space. The panels below draw log(z) / sin(z) with the
;; defaults, then with every other `:wrap`:

(nu/option-cards '(field/domain-color f) (describe/options #'field/domain-color))

(plot/render-image (plot/arrange [{:charts [(chart/field {:pixel-color (field/domain-color (fn [z] (cx/div (cx/log z) (cx/sin z))))
                                                          :domain {:x [-6 6] :y [-6 6]}})]
                                   :labels {:title "defaults (:log2, :HSI)"}}
                                  {:charts [(chart/field {:pixel-color (field/domain-color (fn [z] (cx/div (cx/log z) (cx/sin z)))
                                                                                           {:wrap :log10})
                                                          :domain {:x [-6 6] :y [-6 6]}})]
                                   :labels {:title ":wrap :log10"}}
                                  {:charts [(chart/field {:pixel-color (field/domain-color (fn [z] (cx/div (cx/log z) (cx/sin z)))
                                                                                           {:wrap :exp})
                                                          :domain {:x [-6 6] :y [-6 6]}})]
                                   :labels {:title ":wrap :exp"}}
                                  {:charts [(chart/field {:pixel-color (field/domain-color (fn [z] (cx/div (cx/log z) (cx/sin z)))
                                                                                           {:wrap :frac})
                                                          :domain {:x [-6 6] :y [-6 6]}})]
                                   :labels {:title ":wrap :frac"}}
                                  {:charts [(chart/field {:pixel-color (field/domain-color (fn [z] (cx/div (cx/log z) (cx/sin z)))
                                                                                           {:wrap :sigmoid})
                                                          :domain {:x [-6 6] :y [-6 6]}})]
                                   :labels {:title ":wrap :sigmoid"}}
                                  {:charts [(chart/field {:pixel-color (field/domain-color (fn [z] (cx/div (cx/log z) (cx/sin z)))
                                                                                           {:wrap :sin})
                                                          :domain {:x [-6 6] :y [-6 6]}})]
                                   :labels {:title ":wrap :sin"}}]
                                 {:columns 3})
                   {:width 1000 :height 680})

;; The panels below draw z³ - 1 in each colour space; all hues meet at its three zeros:

(plot/render-image (plot/arrange [{:charts [(chart/field {:pixel-color (field/domain-color (fn [z] (cx/sub (cx/mult z (cx/mult z z)) cx/ONE))
                                                                                           {:colorspace :HCL})
                                                          :domain {:x [-2 2] :y [-2 2]}})]
                                   :labels {:title ":colorspace :HCL"}}
                                  {:charts [(chart/field {:pixel-color (field/domain-color (fn [z] (cx/sub (cx/mult z (cx/mult z z)) cx/ONE))
                                                                                           {:colorspace :HSB})
                                                          :domain {:x [-2 2] :y [-2 2]}})]
                                   :labels {:title ":colorspace :HSB"}}
                                  {:charts [(chart/field {:pixel-color (field/domain-color (fn [z] (cx/sub (cx/mult z (cx/mult z z)) cx/ONE))
                                                                                           {:colorspace :HSI :wrap :log2})
                                                          :domain {:x [-2 2] :y [-2 2]}})]
                                   :labels {:title ":colorspace :HSI"}}
                                  {:charts [(chart/field {:pixel-color (field/domain-color (fn [z] (cx/sub (cx/mult z (cx/mult z z)) cx/ONE))
                                                                                           {:colorspace :HSL})
                                                          :domain {:x [-2 2] :y [-2 2]}})]
                                   :labels {:title ":colorspace :HSL"}}
                                  {:charts [(chart/field {:pixel-color (field/domain-color (fn [z] (cx/sub (cx/mult z (cx/mult z z)) cx/ONE))
                                                                                           {:colorspace :HSV})
                                                          :domain {:x [-2 2] :y [-2 2]}})]
                                   :labels {:title ":colorspace :HSV"}}
                                  {:charts [(chart/field {:pixel-color (field/domain-color (fn [z] (cx/sub (cx/mult z (cx/mult z z)) cx/ONE))
                                                                                           {:colorspace :HWB})
                                                          :domain {:x [-2 2] :y [-2 2]}})]
                                   :labels {:title ":colorspace :HWB"}}]
                                 {:columns 3})
                   {:width 1000 :height 680})

;; ## Isolines

;; `contour/isolines` traces the lines of a gridded field at one level by marching squares and returns them as point
;; vectors, each open or closed; `chart/contour` draws them. Use it for level lines of your own.

(nu/argument-list #'contour/isolines)

;; The table below counts the points of the lines at levels 1 and 1.5: an open line that leaves the grid and a closed
;; ring around the field's peak:

(nu/result-table (for [level [1.0 1.5] line (contour/isolines grid level)]
                   (assoc (update line :points count) :level level)))

;; The plot below draws the lines of six levels from a row per point, with its level and line number; a closed line
;; repeats its first point at its end:

(plot/render-image {:data (for [level [-1.0 -0.5 0.0 0.5 1.0 1.5]
                                [i {:keys [points closed]}] (map-indexed vector (contour/isolines grid level))
                                [x y] (cond-> points closed (conj (first points)))]
                            {:level level :line (str level "/" i) :x x :y y})
                    :mapping {:x :x :y :y :color #(str (:level %)) :group :line}
                    :charts [(chart/line {:order :data :line-width 2})]
                    :scales {:color {:title "level" :domain ["-1.0" "-0.5" "0.0" "0.5" "1.0" "1.5"]}}
                    :aspect :iso}
                   {:width 600 :height 480})

;; ## Vector field arrows

;; `vector-field/arrows` returns the arrows of a vector field on a grid over `:domain`. Use it with `chart/segment` and
;; arrowheads.

(nu/argument-list #'vf/arrows)

;; These options set the domain, the grid and the arrow length. The table below holds the arrows of a rotation on a
;; 4 × 4 grid:

(nu/option-cards '(vf/arrows f) (describe/options #'vf/arrows))

(nu/result-table (vf/arrows (fn [[x y]] (v/vec2 (- y) x)) {:domain {:x [-2 2] :y [-2 2]} :n 4}))

;; The left panel below draws the rotation on the default square grid, the middle one on a shifted grid with longer
;; arrows. The right one draws a fastmath field (`fastmath.fields`) and colours the arrows by magnitude:

(plot/render-image (plot/arrange [{:data (vf/arrows (fn [[x y]] (v/vec2 (- y) x)) {:domain {:x [-2 2] :y [-2 2]} :n 12 :grid :square})
                                   :mapping {:x :x :y :y :xend :xend :yend :yend}
                                   :charts [(chart/segment {:arrow {:end true :size 5}})]
                                   :aspect :iso
                                   :labels {:title "rotation, :n 12"}}
                                  {:data (vf/arrows (fn [[x y]] (v/vec2 (- y) x)) {:domain {:x [-2 2] :y [-2 2]} :n 12 :grid :shifted
                                                                                   :length-scale 1.6})
                                   :mapping {:x :x :y :y :xend :xend :yend :yend}
                                   :charts [(chart/segment {:arrow {:end true :size 5}})]
                                   :aspect :iso
                                   :labels {:title ":grid :shifted, :length-scale 1.6"}}
                                  {:data (vf/arrows (f/field :hyperbolic) {:domain {:x [-3 3] :y [-3 3]} :n [16 12]})
                                   :mapping {:x :x :y :y :xend :xend :yend :yend :color :magnitude}
                                   :charts [(chart/segment {:line-width 1.5 :arrow {:end true :size 5}})]
                                   :scales {:color {:title "|f|" :range [[160 200 235] [8 48 107]]}}
                                   :aspect :iso
                                   :labels {:title "hyperbolic, :n [16 12]"}}]
                                 {:columns 3 :widths [1 1 1.25]})
                   {:width 1000 :height 360})

;; ## Streamlines

;; `vector-field/traces` returns paths that follow the field from seed points. Use it for streamlines.

(nu/argument-list #'vf/traces)

;; These options set the number of steps and their length. The table below holds the first rows of five steps from
;; every seed:

(nu/option-cards '(vf/traces f seeds) (describe/options #'vf/traces))

(nu/result-table (vf/traces (fn [[x y]] (v/vec2 (- y (* 0.3 x)) (- (- x) (* 0.3 y)))) seeds {:steps 5 :step 0.1}))

;; The left and middle panels below draw streamlines of a damped rotation from the seed grid, short and long; after
;; 400 steps the spirals stop short of the centre. The right one draws traces of a cellular flow `[sin y, sin x]` from
;; 300 seeded random points as faint lines: the cells turn in alternate directions, and the panel cuts the traces that
;; leave it:

(plot/render-image (plot/arrange [{:data (vf/traces (fn [[x y]] (v/vec2 (- y (* 0.3 x)) (- (- x) (* 0.3 y)))) seeds
                                                    {:steps 20 :step 0.05})
                                   :mapping {:x :x :y :y :group :id}
                                   :charts [(chart/line {:order :data :arrow {:end true :size 5}})]
                                   :aspect :iso
                                   :labels {:title ":steps 20, :step 0.05"}}
                                  {:data (vf/traces (fn [[x y]] (v/vec2 (- y (* 0.3 x)) (- (- x) (* 0.3 y)))) seeds
                                                    {:steps 400 :step 0.05})
                                   :mapping {:x :x :y :y :group :id :color :i}
                                   :charts [(chart/line {:order :data})]
                                   :scales {:color {:title "step" :range [[160 200 235] [8 48 107]]}}
                                   :aspect :iso
                                   :labels {:title ":steps 400"}}
                                  {:data (let [g (r/rng :mersenne 12)]
                                           (vf/traces (fn [[x y]] (v/vec2 (m/sin y) (m/sin x)))
                                                      (repeatedly 300 #(vector (r/drandom g -4 4) (r/drandom g -4 4)))
                                                      {:steps 60 :step 0.05}))
                                   :mapping {:x :x :y :y :group :id}
                                   :charts [(chart/line {:order :data :color [20 70 140 70]})]
                                   :scales {:x {:domain [-4 4] :nice false} :y {:domain [-4 4] :nice false}}
                                   :aspect :iso
                                   :labels {:title "[sin y, sin x], 300 seeds"}}]
                                 {:columns 3 :widths [1 1.2 1]})
                   {:width 1000 :height 360})

;; ## Mapped points

;; `vector-field/mapped-points` maps seed points once through the field. Use it to draw the image of a region.

(nu/argument-list #'vf/mapped-points)

;; The table below maps the first 20 seeds through a sinusoidal field:

(nu/result-table (vf/mapped-points (f/field :sinusoidal) (take 20 seeds)))

;; The plots below map 100 000 seeded random points of the square [-3 3]² through two fastmath fields and colour them
;; by the seed's x:

(plot/render-image (plot/arrange [{:data (let [g (r/rng :mersenne 9)]
                                           (vf/mapped-points (f/field :swirl)
                                                             (repeatedly 100000 #(vector (r/drandom g -3 3) (r/drandom g -3 3)))))
                                   :mapping {:x :x :y :y :color :x0}
                                   :charts [(chart/scatter {:size 1})]
                                   :scales {:color {:range [[250 190 120] [120 30 10]]}}
                                   :legend false
                                   :aspect :iso
                                   :labels {:title "swirl"}}
                                  {:data (let [g (r/rng :mersenne 10)]
                                           (vf/mapped-points (f/field :horseshoe)
                                                             (repeatedly 100000 #(vector (r/drandom g -3 3) (r/drandom g -3 3)))))
                                   :mapping {:x :x :y :y :color :x0}
                                   :charts [(chart/scatter {:size 1})]
                                   :scales {:color {:title "x0" :range [[250 190 120] [120 30 10]]}}
                                   :aspect :iso
                                   :labels {:title "horseshoe"}}]
                                 {:columns 2 :widths [1 1.2]})
                   {:width 900 :height 420})
