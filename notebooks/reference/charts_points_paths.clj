;; # Charts: points and paths

;; Marks placed at data positions: points (`scatter`), lines through them (`line`), straight pieces between two
;; positions (`segment`) and filled shapes (`polygon`). Each chart lists the mapping keys it reads, its own options
;; and the shared option groups it takes (documented once in the Shared options chapter).

(ns reference.charts-points-paths
  (:require [fastmath.random :as r]
            [cljplot2.chart :as chart]
            [cljplot2.core :as plot]
            [cljplot2.data.levels :as levels]
            [cljplot2.describe :as describe]
            [notebook-utils :as nu]))

(nu/source-note {:dataset :mtcars})

(def mtcars (nu/dataset :mtcars))

(nu/data-table mtcars)

(nu/source-note {:dataset :stocks})

(def stocks (nu/dataset :stocks))

(nu/data-table stocks)

;; ## scatter

;; A point per row at its x and y; colour, shape and size may come from the data, and `:order` sets which points
;; are drawn on top.

(nu/options-table (describe/mapping (chart/scatter)))

(plot/render-image {:data mtcars
                    :mapping {:x :wt :y :mpg :color (levels/categorical :cyl) :size :hp}
                    :charts [(chart/scatter)]
                    :scales {:color {:title "cylinders"} :size {:range [5 18]}}
                    :labels {:x "weight [1000 lbs]" :y "miles per gallon"}}
                   {:width 600 :height 380})

;; Its options: the point style used when nothing is mapped, the draw order of a mapped `:order`, and jitter -
;; random offsets separating points on a grid of values (seeded here, so the image is the same on every build):

(nu/options-table (describe/own-rows (describe/options (chart/scatter))))

(plot/render-image (plot/arrange [{:data mtcars
                                   :mapping {:x :cyl :y :gear}
                                   :charts [(chart/scatter {:size 10 :shape \s :color [200 80 40 150]})]
                                   :labels {:title "overplotted"}}
                                  {:data mtcars
                                   :mapping {:x :cyl :y :gear}
                                   :charts [(chart/scatter {:size 10 :shape \s :color [200 80 40 150]
                                                           :stroke-color :black :stroke-width 0.5
                                                           :jitter [0.3 0.2] :rng (r/rng :mersenne 7)})]
                                   :labels {:title ":jitter [0.3 0.2]"}}]
                                 {:columns 2})
                   {:width 900 :height 330})

;; A mapped `:order` draws points by its values, later ones on top: the most powerful cars over the others
;; (ascending) or under them (descending); `:padding` is the room around the data, here none on x:

(plot/render-image (plot/arrange [{:data mtcars
                                   :mapping {:x :wt :y :mpg :color :hp :order :hp}
                                   :charts [(chart/scatter {:size 26 :stroke-color :white})]
                                   :labels {:title ":order :hp, :order-direction :ascending"}
                                   :legend false}
                                  {:data mtcars
                                   :mapping {:x :wt :y :mpg :color :hp :order :hp}
                                   :charts [(chart/scatter {:size 26 :stroke-color :white :order-direction :descending
                                                           :padding {:x [0 0] :y [0.05 0.05]}})]
                                   :labels {:title ":order-direction :descending, :padding x 0"}}]
                                 {:columns 2 :widths [1 1.2]})
                   {:width 1000 :height 360})

(nu/shared-groups (describe/options (chart/scatter)))

;; ## line

;; A polyline per group through the rows ordered by x; it may fill the area below, stack with other groups, or
;; carry a colour or width along its length. (The dataset's dates are text: the mapping parses them.)

(nu/options-table (describe/mapping (chart/line)))

(plot/render-image {:data stocks
                    :mapping {:x #(java.time.LocalDate/parse (:date %)) :y :price :color :symbol}
                    :charts [(chart/line)]
                    :labels {:x "date" :y "price [USD]"}}
                   {:width 700 :height 380})

;; ### Style

;; The area under a line (`:fill`), its gradient, the stroke's opacity and the trail - a filled band whose width
;; follows a mapped `:size`:

(nu/options-table (describe/own-rows (describe/options (chart/line))) :style)

(def wave (for [t (range 40)] {:t t :v (+ 1.5 (Math/sin (* 0.25 t))) :w (+ 1 (mod t 10))}))

(nu/data-table wave)

(plot/render-image (plot/arrange [{:data wave :mapping {:x :t :y :v}
                                   :charts [(chart/line {:fill :color :alpha 0.3})]
                                   :labels {:title ":fill :color"}}
                                  {:data wave :mapping {:x :t :y :v}
                                   :charts [(chart/line {:fill-gradient :mpl/viridis :fill-gradient-axis :y})]
                                   :labels {:title ":fill-gradient"}}
                                  {:data wave :mapping {:x :t :y :v :size :w}
                                   :charts [(chart/line {:trail true})]
                                   :labels {:title ":trail true"}
                                   :legend false}]
                                 {:columns 3})
                   {:width 1000 :height 300})

;; The gradient along x instead, and `:stroke-alpha` fading the strokes of overlapping lines:

(plot/render-image (plot/arrange [{:data wave :mapping {:x :t :y :v}
                                   :charts [(chart/line {:fill-gradient :mpl/viridis :fill-gradient-axis :x})]
                                   :labels {:title ":fill-gradient-axis :x"}}
                                  {:data stocks
                                   :mapping {:x #(java.time.LocalDate/parse (:date %)) :y :price :color :symbol}
                                   :charts [(chart/line {:width 3 :stroke-alpha 0.4})]
                                   :labels {:title ":stroke-alpha 0.4"}}]
                                 {:columns 2 :widths [1 1.4]})
                   {:width 1000 :height 300})

;; ### Path

;; How points are joined: sorted by x or in data order, what a missing value does (a break or a bridge), how
;; groups combine (the Layering and position chapter shows `:stack` and the streamgraphs), and the padding:

(nu/options-table (describe/own-rows (describe/options (chart/line))) :path)

(def spiral (for [t (range 0 12 0.25)] {:x (* t (Math/cos t)) :y (* t (Math/sin t)) :v (when-not (< 5 t 6) t)}))

(nu/data-table spiral)

(plot/render-image (plot/arrange [{:data spiral :mapping {:x :x :y :y}
                                   :charts [(chart/line)]
                                   :labels {:title ":sort true (default)"}}
                                  {:data spiral :mapping {:x :x :y :y}
                                   :charts [(chart/line {:sort false})]
                                   :labels {:title ":sort false"}}
                                  {:data spiral :mapping {:x :x :y :v}
                                   :charts [(chart/line {:sort false :gaps :break :padding {:x [0 0] :y [0 0]}})]
                                   :labels {:title ":gaps :break, no padding"}}
                                  {:data spiral :mapping {:x :x :y :v}
                                   :charts [(chart/line {:sort false :gaps :bridge :bridge {:alpha 0.5 :dash :dotted}})]
                                   :labels {:title ":gaps :bridge"}}]
                                 {:columns 2})
                   {:width 900 :height 600})

;; Groups combined by `:position` (`:overlay`, the default, draws each line on its own):

(def streams (for [series ["a" "b" "c"] t (range 25)]
               {:series series :t t :v (+ 1.5 (Math/sin (+ (* 0.3 t) (* 2 (count series)) (int (first series)))))}))

(nu/data-table streams)

(plot/render-image (plot/arrange [{:data streams :mapping {:x :t :y :v :color :series}
                                   :charts [(chart/line {:position :stack})] :labels {:title ":stack"} :legend false}
                                  {:data streams :mapping {:x :t :y :v :color :series}
                                   :charts [(chart/line {:position :normalize})] :labels {:title ":normalize"} :legend false}
                                  {:data streams :mapping {:x :t :y :v :color :series}
                                   :charts [(chart/line {:position :silhouette})] :labels {:title ":silhouette"} :legend false}
                                  {:data streams :mapping {:x :t :y :v :color :series}
                                   :charts [(chart/line {:position :wiggle})] :labels {:title ":wiggle"}}]
                                 {:columns 4 :widths [1 1 1 1.3]})
                   {:width 1000 :height 280})

;; ### Ends

;; Arrowheads at the ends of each run and gaps shortening it - e.g. to stop short of a marker:

(nu/options-table (describe/own-rows (describe/options (chart/line))) :ends)

(def path-points [{:x 0 :y 0} {:x 1 :y 2} {:x 2 :y 1} {:x 3 :y 3}])

(nu/data-table path-points)

(plot/render-image (plot/arrange [{:data path-points :mapping {:x :x :y :y}
                                   :charts [(chart/line {:arrow {:end true}})]
                                   :labels {:title ":arrow {:end true}"}}
                                  {:data path-points :mapping {:x :x :y :y}
                                   :charts [(chart/scatter {:size 14})
                                            (chart/line {:arrow {:start true :end true :shape :open :size 12}
                                                         :start-gap 9 :end-gap 9})]
                                   :labels {:title "open heads, gaps 9 px"}}]
                                 {:columns 2})
                   {:width 800 :height 300})

(nu/shared-groups (describe/options (chart/line)))

;; ## segment

;; A straight piece per row from (x, y) to (xend, yend); the ends may be mapped or given as literals, with
;; arrowheads and gaps as on lines.

(nu/options-table (describe/mapping (chart/segment)))

(def moves [{:from "a" :x 1 :y 1 :x2 3 :y2 2} {:from "b" :x 2 :y 4 :x2 4 :y2 3} {:from "c" :x 3 :y 0.5 :x2 1.5 :y2 3}])

(nu/data-table moves)

(plot/render-image {:data moves
                    :mapping {:x :x :y :y :xend :x2 :yend :y2 :color :from}
                    :charts [(chart/segment {:width 2})]}
                   {:width 500 :height 320})

;; Its options: literal ends (here every segment starts at x = 0, a lollipop stem), arrows and gaps:

(nu/options-table (describe/own-rows (describe/options (chart/segment))))

(plot/render-image (plot/arrange [{:data mtcars
                                   :mapping {:x :mpg :y :model}
                                   :charts [(chart/segment {:xend 0}) (chart/scatter)]
                                   :scales {:y {:reverse true}}
                                   :labels {:title ":xend 0"}}
                                  {:data moves
                                   :mapping {:x :x :y :y :xend :x2 :yend :y2}
                                   :charts [(chart/scatter {:size 10})
                                            (chart/segment {:arrow {:end true :size 10} :end-gap 4})]
                                   :labels {:title ":arrow, :end-gap 4"}}]
                                 {:columns 2 :widths [1.3 1]})
                   {:width 1000 :height 620})

(plot/render-image (plot/arrange [{:data moves
                                   :mapping {:x :x :y :y :xend :x2}
                                   :charts [(chart/segment {:yend 4.5 :dash :dashed}) (chart/scatter)]
                                   :labels {:title ":yend 4.5 (one end for all)"}}
                                  {:data moves
                                   :mapping {:x :x :y :y :xend :x2 :yend :y2}
                                   :charts [(chart/scatter {:size 14})
                                            (chart/segment {:start-gap 10 :end-gap 10 :padding {:x [0.2 0.2] :y [0.2 0.2]}})]
                                   :labels {:title ":start-gap 10, :padding 0.2"}}]
                                 {:columns 2})
                   {:width 800 :height 320})

(nu/shared-groups (describe/options (chart/segment)))

;; ## polygon

;; A filled shape per group through its rows in data order.

(nu/options-table (describe/mapping (chart/polygon)))

(def shapes (concat (for [a (range 0 360 60)]
                      {:shape "hexagon" :x (Math/cos (Math/toRadians a)) :y (Math/sin (Math/toRadians a))})
                    (for [[x y] [[1.5 -1] [3 -1] [2.25 1]]] {:shape "triangle" :x x :y y})))

(nu/data-table shapes)

(plot/render-image {:data shapes
                    :mapping {:x :x :y :y :color :shape}
                    :charts [(chart/polygon)]}
                   {:width 500 :height 260})

;; Its options: the fill, its opacity and the outline (`:color` - the group's colour, opaque):

(nu/options-table (describe/own-rows (describe/options (chart/polygon))))

(plot/render-image {:data shapes
                    :mapping {:x :x :y :y :color :shape}
                    :charts [(chart/polygon {:alpha 0.35 :stroke-color :color :stroke-width 2})]}
                   {:width 500 :height 260})

;; Without a mapped colour every polygon takes `:color`; `:padding` 0 lets the shapes touch the panel's edges:

(plot/render-image {:data shapes
                    :mapping {:x :x :y :y :group :shape}
                    :charts [(chart/polygon {:color [120 160 90] :stroke-color :black :padding {:x [0 0] :y [0 0]}})]}
                   {:width 500 :height 260})

(nu/shared-groups (describe/options (chart/polygon)))
