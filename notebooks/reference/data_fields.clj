;; # Data functions: fields

;; Functions turning functions of the plane into something to draw: a colour per pixel of a complex function, the
;; level lines of a gridded field, and the rows of a vector field - arrows, traces (streamlines) and points mapped
;; through it - drawn with ordinary charts (`field`, `line`, `segment`, `scatter`).

(ns reference.data-fields
  (:require [fastmath.complex :as cx]
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

;; ## field/domain-color

;; A `:pixel-color` function for `chart/field` colouring a complex function by domain colouring: the hue is the
;; argument of f(x + iy), the brightness its magnitude wrapped into [0 1] by `:wrap`, in a hue-first `:colorspace`.

(nu/options-table (describe/options #'field/domain-color))

;; log(z) / sin(z) with the defaults (`:log2` wrap, `:HSI`), then every other `:wrap`:

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

;; The colour spaces (here with z³ - 1, its three zeros where all hues meet):

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

;; ## contour/isolines

;; The lines of a gridded field at one level (marching squares) - what `chart/contour` draws - as point vectors, each
;; open or closed. The grid: `:xs` / `:ys` (axis coordinates), `:nx` / `:ny` and `:values` (row by row, x fastest):

(def grid-xs (range -3.0 3.01 0.25))

(def grid {:xs (double-array grid-xs) :ys (double-array grid-xs) :nx (count grid-xs) :ny (count grid-xs)
           :values (double-array (for [y grid-xs x grid-xs] (+ (Math/sin x) (Math/cos (* 1.3 y)))))})

;; The lines at level 1 - a closed ring and open lines leaving the grid:

(nu/data-table (map #(update % :points count) (contour/isolines grid 1.0)))

;; Drawn as lines - every point row with its level and line number - for five levels:

(plot/render-image {:data (for [level [-1.0 -0.5 0.0 0.5 1.0]
                                [i {:keys [points closed?]}] (map-indexed vector (contour/isolines grid level))
                                [x y] (cond-> points closed? (conj (first points)))]
                            {:level level :line (str level "/" i) :x x :y y})
                    :mapping {:x :x :y :y :color #(str (:level %)) :group :line}
                    :charts [(chart/line {:sort false :width 2})]
                    :scales {:color {:title "level" :domain ["-1.0" "-0.5" "0.0" "0.5" "1.0"]}}
                    :aspect :iso}
                   {:width 600 :height 480})

;; ## vector-field/arrows

;; Arrows of a vector field `f` (a function of a point `[x y]` returning a vector) on a grid over `:domain` - rows
;; `{:x :y :xend :yend :magnitude}` for `chart/segment` with arrowheads.

(nu/options-table (describe/options #'vf/arrows))

(nu/data-table (vf/arrows (fn [[x y]] (v/vec2 (- y) x)) {:domain {:x [-2 2] :y [-2 2]} :n 4}))

;; A rotation on the default square grid (left), on a shifted grid with longer arrows (middle), and a fastmath field
;; (`fastmath.fields`) with the arrows coloured by magnitude (right):

(plot/render-image (plot/arrange [{:data (vf/arrows (fn [[x y]] (v/vec2 (- y) x)) {:domain {:x [-2 2] :y [-2 2]} :n 12 :grid :square})
                                   :mapping {:x :x :y :y :xend :xend :yend :yend}
                                   :charts [(chart/segment {:arrow {:end true :size 5}})]
                                   :aspect :iso
                                   :labels {:title "rotation, :n 12"}}
                                  {:data (vf/arrows (fn [[x y]] (v/vec2 (- y) x)) {:domain {:x [-2 2] :y [-2 2]} :n 12 :grid :shifted
                                                                                   :scale 1.6})
                                   :mapping {:x :x :y :y :xend :xend :yend :yend}
                                   :charts [(chart/segment {:arrow {:end true :size 5}})]
                                   :aspect :iso
                                   :labels {:title ":grid :shifted, :scale 1.6"}}
                                  {:data (vf/arrows (f/field :hyperbolic) {:domain {:x [-3 3] :y [-3 3]} :n [16 12]})
                                   :mapping {:x :x :y :y :xend :xend :yend :yend :color :magnitude}
                                   :charts [(chart/segment {:width 1.5 :arrow {:end true :size 5}})]
                                   :scales {:color {:title "|f|" :range [[160 200 235] [8 48 107]]}}
                                   :aspect :iso
                                   :labels {:title "hyperbolic, :n [16 12]"}}]
                                 {:columns 3 :widths [1 1 1.25]})
                   {:width 1000 :height 360})

;; ## vector-field/traces

;; Paths following the field from seed points - rows `{:id :i :x :y}` (seed index, step); each step moves a point
;; along f, `:steps` times by a factor `:step`.

(nu/options-table (describe/options #'vf/traces))

(def seeds (for [x (range -2.0 2.01 0.5) y (range -2.0 2.01 0.5)] [x y]))

(nu/data-table (map (fn [[x y]] {:x x :y y}) seeds))

(nu/data-table (vf/traces (fn [[x y]] (v/vec2 (- y (* 0.3 x)) (- (- x) (* 0.3 y)))) seeds {:steps 5 :step 0.1}))

;; Streamlines of a damped rotation from the seed grid - short (left) and long (middle; the spirals stop short of
;; the centre after 400 steps) - and traces of a cellular flow `[sin y, sin x]` from 300 seeded random points as faint
;; lines (right; cells turning in alternate directions, traces leaving the panel are cut):

(plot/render-image (plot/arrange [{:data (vf/traces (fn [[x y]] (v/vec2 (- y (* 0.3 x)) (- (- x) (* 0.3 y)))) seeds
                                                    {:steps 20 :step 0.05})
                                   :mapping {:x :x :y :y :group :id}
                                   :charts [(chart/line {:sort false :arrow {:end true :size 5}})]
                                   :aspect :iso
                                   :labels {:title ":steps 20, :step 0.05"}}
                                  {:data (vf/traces (fn [[x y]] (v/vec2 (- y (* 0.3 x)) (- (- x) (* 0.3 y)))) seeds
                                                    {:steps 400 :step 0.05})
                                   :mapping {:x :x :y :y :group :id :color :i}
                                   :charts [(chart/line {:sort false})]
                                   :scales {:color {:title "step" :range [[160 200 235] [8 48 107]]}}
                                   :aspect :iso
                                   :labels {:title ":steps 400"}}
                                  {:data (let [g (r/rng :mersenne 12)]
                                           (vf/traces (fn [[x y]] (v/vec2 (Math/sin y) (Math/sin x)))
                                                      (repeatedly 300 #(vector (r/drandom g -4 4) (r/drandom g -4 4)))
                                                      {:steps 60 :step 0.05}))
                                   :mapping {:x :x :y :y :group :id}
                                   :charts [(chart/line {:sort false :color [20 70 140 70]})]
                                   :scales {:x {:domain [-4 4] :nice false} :y {:domain [-4 4] :nice false}}
                                   :aspect :iso
                                   :labels {:title "[sin y, sin x], 300 seeds"}}]
                                 {:columns 3 :widths [1 1.2 1]})
                   {:width 1000 :height 360})

;; ## vector-field/mapped-points

;; Seed points mapped once through the field: rows `{:x :y :x0 :y0}` - the image f(x0, y0) and its seed.

(nu/data-table (vf/mapped-points (f/field :sinusoidal) (take 20 seeds)))

;; 100 000 seeded random points through two fastmath fields, coloured by the seed's x (the field's image of the
;; square [-3 3]²):

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
