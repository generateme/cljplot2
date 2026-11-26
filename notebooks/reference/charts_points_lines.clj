;; # Points, lines and areas

;; These charts draw the data where it lies: points (`scatter`), lines through them (`line`), line segments between
;; two points (`segment`), filled shapes (`polygon`), bands between two bounds (`ribbon`) and price candles
;; (`candlestick`). Start here for raw data in two dimensions.

(ns reference.charts-points-lines
  (:require [fastmath.core :as m]
            [fastmath.random :as r]
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

(nu/source-note {:dataset :ohlc})

(def ohlc (nu/dataset :ohlc))

(nu/data-table ohlc)

;; ## Scatter plots

;; `chart/scatter` draws a point per row at its x and y. Use it to show how two variables relate. Colour, shape, size
;; and drawing order can come from the data.

(nu/shared-groups (describe/options (chart/scatter)))

(nu/mapping-list (describe/mapping (chart/scatter)))

(plot/render-image {:data mtcars
                    :mapping {:x :wt :y :mpg :color (levels/categorical :cyl) :size :hp}
                    :charts [(chart/scatter)]
                    :scales {:color {:title "cylinders"} :size {:range [5 18]}}
                    :labels {:x "weight [1000 lbs]" :y "miles per gallon"}}
                   {:width 600 :height 380})

;; Its options style the points when nothing is mapped, set the draw order and spread points that overlap:

(nu/option-cards 'chart/scatter (describe/own-rows (describe/options (chart/scatter))))

(plot/render-image (plot/arrange [{:data mtcars
                                   :mapping {:x :cyl :y :gear}
                                   :charts [(chart/scatter {:size 10 :shape \S :color [200 80 40 150]})]
                                   :labels {:title "overplotted"}}
                                  {:data mtcars
                                   :mapping {:x :cyl :y :gear}
                                   :charts [(chart/scatter {:size 10 :shape \S :color [200 80 40 150]
                                                           :stroke-color :black :stroke-width 0.5
                                                           :jitter [0.3 0.2] :rng (r/rng :mersenne 7)})]
                                   :labels {:title ":jitter [0.3 0.2]"}}]
                                 {:columns 2})
                   {:width 900 :height 330})

;; The most powerful cars drawn over the others (ascending) or under them (descending); no padding on x:

(plot/render-image (plot/arrange [{:data mtcars
                                   :mapping {:x :wt :y :mpg :color :hp :order :hp}
                                   :charts [(chart/scatter {:size 26 :stroke-color :white})]
                                   :labels {:title ":order :hp, ascending (default)"}
                                   :legend false}
                                  {:data mtcars
                                   :mapping {:x :wt :y :mpg :color :hp :order :hp}
                                   :charts [(chart/scatter {:size 26 :stroke-color :white :descending true
                                                           :padding {:x [0 0] :y [0.05 0.05]}})]
                                   :labels {:title ":descending true, :padding x 0"}}]
                                 {:columns 2 :widths [1 1.2]})
                   {:width 1000 :height 360})

;; Half-transparent bubbles keep the ones beneath visible; the white outline stays opaque:

(plot/render-image {:data mtcars
                    :mapping {:x :wt :y :mpg :color (levels/categorical :cyl) :size :hp}
                    :charts [(chart/scatter {:alpha 0.45 :stroke-color :white :stroke-width 1})]
                    :scales {:color {:title "cylinders"} :size {:range [8 30]}}
                    :labels {:title ":alpha 0.45" :x "weight [1000 lbs]" :y "miles per gallon"}}
                   {:width 600 :height 380})

;; ## Line charts

;; `chart/line` joins each group's points into a line, ordered by x. Use it for values that change along x, such as
;; a time series; it can also fill the area below, stack groups, or vary colour and width along its length. (The
;; dataset's dates are text: the mapping parses them.)

(nu/shared-groups (describe/options (chart/line)))

(nu/mapping-list (describe/mapping (chart/line)))

(plot/render-image {:data stocks
                    :mapping {:x #(java.time.LocalDate/parse (:date %)) :y :price :color :symbol}
                    :charts [(chart/line)]
                    :labels {:x "date" :y "price [USD]"}}
                   {:width 700 :height 380})

;; ### Style

;; These options decide how the line and the area below it look, including a trail whose width follows a mapped
;; `:size`:

(nu/option-cards 'chart/line (describe/own-rows (describe/options (chart/line))) :style)

(def wave (for [t (range 40)] {:t t :v (+ 1.5 (m/sin (* 0.25 t))) :w (+ 1 (mod t 10))}))

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

;; A gradient along x, and faded strokes where lines overlap:

(plot/render-image (plot/arrange [{:data wave :mapping {:x :t :y :v}
                                   :charts [(chart/line {:fill-gradient :mpl/viridis :fill-gradient-axis :x})]
                                   :labels {:title ":fill-gradient-axis :x"}}
                                  {:data stocks
                                   :mapping {:x #(java.time.LocalDate/parse (:date %)) :y :price :color :symbol}
                                   :charts [(chart/line {:line-width 3 :stroke-alpha-factor 0.4})]
                                   :labels {:title ":stroke-alpha-factor 0.4"}}]
                                 {:columns 2 :widths [1 1.4]})
                   {:width 1000 :height 300})

;; `:fill :color` keeps the line colour's own opacity: a dimmed line (alpha 60 of 255) fills dimmed too, here beside
;; the same line at full opacity:

(plot/render-image (plot/arrange [{:data wave :mapping {:x :t :y :v}
                                   :charts [(chart/line {:color [200 80 40] :fill :color :alpha 0.5})]
                                   :labels {:title "opaque colour, :alpha 0.5"}}
                                  {:data wave :mapping {:x :t :y :v}
                                   :charts [(chart/line {:color [200 80 40 60] :fill :color :alpha 0.5})]
                                   :labels {:title "colour alpha 60, :alpha 0.5"}}]
                                 {:columns 2})
                   {:width 1000 :height 300})

;; ### Path

;; These options decide how the points become a path: their order, what happens at a missing y and how groups
;; combine (the Layering and position chapter shows `:stack` and the streamgraphs):

(nu/option-cards 'chart/line (describe/own-rows (describe/options (chart/line))) :path)

(def spiral (for [t (range 0 12 0.25)] {:x (* t (m/cos t)) :y (* t (m/sin t)) :v (when-not (< 5 t 6) t)}))

(nu/data-table spiral)

(plot/render-image (plot/arrange [{:data spiral :mapping {:x :x :y :y}
                                   :charts [(chart/line)]
                                   :labels {:title ":order :x (default)"}}
                                  {:data spiral :mapping {:x :x :y :y}
                                   :charts [(chart/line {:order :data})]
                                   :labels {:title ":order :data"}}
                                  {:data spiral :mapping {:x :x :y :v}
                                   :charts [(chart/line {:order :data :gaps :break :padding {:x [0 0] :y [0 0]}})]
                                   :labels {:title ":gaps :break, no padding"}}
                                  {:data spiral :mapping {:x :x :y :v}
                                   :charts [(chart/line {:order :data :gaps :bridge :bridge {:alpha-factor 0.5 :dash :dotted}})]
                                   :labels {:title ":gaps :bridge"}}]
                                 {:columns 2})
                   {:width 900 :height 600})

;; The four stacked positions, side by side:

(def streams (for [series ["a" "b" "c"] t (range 25)]
               {:series series :t t :v (+ 1.5 (m/sin (+ (* 0.3 t) (* 2 (count series)) (int (first series)))))}))

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

;; These options add arrowheads, or stop each line short of its points, e.g. before a marker:

(nu/option-cards 'chart/line (describe/own-rows (describe/options (chart/line))) :ends)

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

;; ## Line segments

;; `chart/segment` draws a straight line per row from (x, y) to (xend, yend). Use it for arrows, links between two
;; points or lollipop stems; the ends can be mapped or fixed for all rows.

(nu/shared-groups (describe/options (chart/segment)))

(nu/mapping-list (describe/mapping (chart/segment)))

(def moves [{:from "a" :x 1 :y 1 :x2 3 :y2 2} {:from "b" :x 2 :y 4 :x2 4 :y2 3} {:from "c" :x 3 :y 0.5 :x2 1.5 :y2 3}])

(nu/data-table moves)

(plot/render-image {:data moves
                    :mapping {:x :x :y :y :xend :x2 :yend :y2 :color :from}
                    :charts [(chart/segment {:line-width 2})]}
                   {:width 500 :height 320})

;; Its options give fixed ends, arrowheads and gaps (on the left, `:xend 0` ends every segment at x = 0, as
;; lollipop stems):

(nu/option-cards 'chart/segment (describe/own-rows (describe/options (chart/segment))))

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

;; ## Polygons

;; `chart/polygon` fills one shape per group, its rows the vertices in data order. Use it for regions, outlines or
;; any custom shape.

(nu/shared-groups (describe/options (chart/polygon)))

(nu/mapping-list (describe/mapping (chart/polygon)))

(def shapes (concat (for [a (range 0 360 60)]
                      {:shape "hexagon" :x (m/cos (m/radians a)) :y (m/sin (m/radians a))})
                    (for [[x y] [[1.5 -1] [3 -1] [2.25 1]]] {:shape "triangle" :x x :y y})))

(nu/data-table shapes)

(plot/render-image {:data shapes
                    :mapping {:x :x :y :y :color :shape}
                    :charts [(chart/polygon)]}
                   {:width 500 :height 260})

;; Its options decide how the shapes are filled and outlined:

(nu/option-cards 'chart/polygon (describe/own-rows (describe/options (chart/polygon))))

(plot/render-image {:data shapes
                    :mapping {:x :x :y :y :color :shape}
                    :charts [(chart/polygon {:alpha 0.35 :stroke-color :color :stroke-width 2})]}
                   {:width 500 :height 260})

;; With `:padding` 0 the shapes touch the panel's edges:

(plot/render-image {:data shapes
                    :mapping {:x :x :y :y :group :shape}
                    :charts [(chart/polygon {:color [120 160 90] :stroke-color :black :padding {:x [0 0] :y [0 0]}})]}
                   {:width 500 :height 260})

;; ## Bands

;; `chart/ribbon` fills a band per group between a lower and an upper bound at each x. Use it for an interval, a
;; range of values or an envelope around a line.

(nu/shared-groups (describe/options (chart/ribbon)))

(nu/mapping-list (describe/mapping (chart/ribbon)))

(def bands (for [g ["low" "high"] t (range 0 25)]
             (let [c (+ (if (= g "low") 1.0 4.0) (m/sin (* 0.3 t)))
                   w (+ 0.4 (* 0.04 t))]
               {:group g :t t :lo (- c w) :mid c :hi (+ c w)})))

(nu/data-table bands)

(plot/render-image {:data bands
                    :mapping {:x :t :ymin :lo :ymax :hi :color :group}
                    :charts [(chart/ribbon) (chart/line {:mapping {:y :mid}})]}
                   {:width 600 :height 340})

;; Its options decide how the band's fill and edges look, or put a gradient in place of the fill:

(nu/option-cards 'chart/ribbon (describe/own-rows (describe/options (chart/ribbon))))

(def band (filter #(= "high" (:group %)) bands))

(nu/data-table band)

(plot/render-image (plot/arrange [{:data band :mapping {:x :t :ymin :lo :ymax :hi}
                                   :charts [(chart/ribbon {:color [200 80 40] :alpha 0.5})]
                                   :labels {:title ":color, :alpha 0.5"}}
                                  {:data band :mapping {:x :t :ymin :lo :ymax :hi}
                                   :charts [(chart/ribbon {:fill :none :stroke-color :color :stroke-width 2 :dash :dashed})]
                                   :labels {:title ":fill :none, dashed edges"}}
                                  {:data band :mapping {:x :t :ymin :lo :ymax :hi}
                                   :charts [(chart/ribbon {:fill-gradient [:yellow :purple] :fill-gradient-axis :y})]
                                   :labels {:title ":fill-gradient along y"}}]
                                 {:columns 3})
                   {:width 1000 :height 300})


;; A gradient along x; no padding on x, a third of the span on y:

(plot/render-image (plot/arrange [{:data band :mapping {:x :t :ymin :lo :ymax :hi}
                                   :charts [(chart/ribbon {:fill-gradient [:yellow :purple] :fill-gradient-axis :x})]
                                   :labels {:title ":fill-gradient-axis :x"}}
                                  {:data band :mapping {:x :t :ymin :lo :ymax :hi}
                                   :charts [(chart/ribbon {:padding {:x [0 0] :y [0.33 0.33]}})]
                                   :labels {:title ":padding {:x [0 0] :y [0.33 0.33]}"}}]
                                 {:columns 2})
                   {:width 800 :height 300})

;; ## Candlestick charts

;; `chart/candlestick` draws a candle per row: a wick from low to high and a body from open to close, coloured by
;; whether the price rose or fell. Use it for prices over time. The dates here are text, so each trading day gets its
;; own slot, with no weekend gaps.

(nu/shared-groups (describe/options (chart/candlestick)))

(nu/mapping-list (describe/mapping (chart/candlestick)))

(plot/render-image {:data ohlc
                    :mapping {:x :date :open :open :high :high :low :low :close :close}
                    :charts [(chart/candlestick)]
                    :labels {:y "VIX"}}
                   {:width 700 :height 360})

;; Its options decide how the candles look, from their colours and patterns to the OHLC bar style:

(nu/option-cards 'chart/candlestick (describe/own-rows (describe/options (chart/candlestick))))

(plot/render-image (plot/arrange [{:data ohlc :mapping {:x :date :open :open :high :high :low :low :close :close}
                                   :charts [(chart/candlestick {:up-color [33 102 172] :down-color [240 140 30]
                                                                :width 0.4 :stroke-color :black :stroke-width 1.5})]
                                   :labels {:title "colours, :width 0.4, outlined"}}
                                  {:data ohlc :mapping {:x :date :open :open :high :high :low :low :close :close}
                                   :charts [(chart/candlestick {:down-pattern :hatch :pattern-color :black})]
                                   :labels {:title ":down-pattern :hatch"}}
                                  {:data ohlc :mapping {:x :date :open :open :high :high :low :low :close :close}
                                   :charts [(chart/candlestick {:style :ohlc :line-width 2})]
                                   :labels {:title ":style :ohlc"}}]
                                 {:columns 3})
                   {:width 1000 :height 320})


;; Here a pattern marks the up candles, and `:padding` adds room above and below the prices:

(plot/render-image {:data ohlc :mapping {:x :date :open :open :high :high :low :low :close :close}
                    :charts [(chart/candlestick {:up-pattern :dots :pattern-color :black :padding {:y [0.3 0.3]}})]
                    :labels {:title ":up-pattern :dots, :padding {:y [0.3 0.3]}"}}
                   {:width 700 :height 340})

